/*
 * File:        mredmsw.cc
 * Purpose:     MrEd Windows event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */


#include "wx_main.h"
#include "wx_media.h"
#include "scheme.h"
#include "wx_dialg.h"

#include "mred.h"

#define OS_SEMAPHORE_TYPE HANDLE

#include "../mzscheme/src/schwinfd.h"

#include <winsock.h>

extern long last_msg_time;

extern void wxDoPreGM(void);
extern void wxDoPostGM(void);
extern int wxCheckMousePosition();
extern void wxDoLeaveEvent(wxWindow *w, int x, int y, int flags);

typedef struct LeaveEvent {
  wxWindow *wnd;
  int x, y, flags;
  struct LeaveEvent *next;
} LeaveEvent;

void MrEdInitFirstContext(MrEdContext *c)
{
}

void MrEdInitNewContext(MrEdContext *c)
{
}

void MrEdDestroyContext(MrEdFinalizedContext *)
{
}

void wxCreatedWindow(wxWindow *w)
{
}

void wxDestroyedWindow(void *context, wxWindow *w)
{
}

extern wxWindow *wxHWNDtoWindow(HWND);

static MrEdContext *GetContext(HWND hwnd)
{
  HWND next = hwnd, wnd;
  do {
    do {
      wnd = next;
      next = GetParent(next);
    } while (next);
    next = GetWindow(wnd, GW_OWNER);
  } while (next);

  wxWindow *w;
  w = wxHWNDtoWindow(wnd);
  
  if (!w)
    return NULL;

  if (wxSubType(w->__type, wxTYPE_FRAME))
    return (MrEdContext *)((wxFrame *)w)->context;
  else if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    return (MrEdContext *)((wxDialogBox *)w)->context;
  else
    return NULL;
}

typedef struct {
  MrEdContext *c, *c_return;
  MSG *msg;
  int remove;
  HWND wnd;
} CheckInfo;

static BOOL CALLBACK CheckWindow(HWND wnd, LPARAM param)
{
  CheckInfo *info = (CheckInfo *)param;
  MrEdContext *c;

  c = GetContext(wnd);

  if ((!info->c && (!c || c->ready)) || (info->c == c)) {
    if (c && c->queued_leaves) {
      if (info->remove) {
	info->wnd = wnd;
	info->c_return = c;
	info->msg->message = WM_USER + 1;
	info->msg->lParam = (long)c->queued_leaves;
	c->queued_leaves = c->queued_leaves->next;
      }
      return FALSE;
    }
    
    if (PeekMessage(info->msg, wnd, NULL, NULL, 
                    info->remove ? PM_REMOVE : PM_NOREMOVE)) {
      info->wnd = wnd;
      info->c_return = c;
      return FALSE;
    }
  }

  return TRUE;
}

int FindReady(MrEdContext *c, MSG *msg, int remove, MrEdContext **c_return)
{
  MSG backup;
  CheckInfo info;

  if (!msg)
    msg = &backup;

  info.c = c;
  info.msg = msg;
  info.remove = remove;

  if (!EnumWindows((WNDENUMPROC)CheckWindow, (LPARAM)&info)) {
    if (c_return)
      *c_return = info.c_return;
    return TRUE;
  }

  return FALSE;
}

int MrEdGetNextEvent(int check_only, int current_only, 
		     MSG *event, MrEdContext **which)
{
  MrEdContext *c;

  if (which)
    *which = NULL;

  if (current_only)
    c = MrEdGetContext();
  else
    c = NULL;

  wxCheckMousePosition();

  return FindReady(c, event, !check_only, which);
}

void MrEdDispatchEvent(MSG *msg)
{
  if (msg->message == WM_USER + 1) {
    /* Queued leave event */
    LeaveEvent *e = (LeaveEvent *)msg->lParam;
    wxDoLeaveEvent(e->wnd, e->x, e->y, e->flags);
  } else if (!wxTheApp->ProcessMessage(msg)) {
    TranslateMessage(msg);
    last_msg_time = msg->time;
    DispatchMessage(msg);
  }
}

int MrEdCheckForBreak(void)
{
  HWND w = GetActiveWindow();

  if (MrEdGetContext() != GetContext(w))
    return 0;

  SHORT hit = 0x8000;
  SHORT hitnow = 0x0001;
  SHORT c = GetAsyncKeyState('C');
#if BREAKING_REQUIRES_SHIFT
  SHORT shift = GetAsyncKeyState(VK_SHIFT);
#else
  SHORT shift = hit;
#endif
  SHORT control = GetAsyncKeyState(VK_CONTROL);
  
  return ((c & hit) && (c & hitnow) && (control & hit) && (shift & hit));
}

static long signal_waitdone(void *)
{
  WaitMessage();
  return 0;
}

static long signal_fddone(void *fds)
{
  win_extended_fd_set *r = (win_extended_fd_set *)fds;
  win_extended_fd_set *w = ((win_extended_fd_set *)fds) + 1;
  win_extended_fd_set *e = ((win_extended_fd_set *)fds) + 2;
  
  select(0, &r->set, &w->set, &e->set, 0);

  return 0;
}

static HANDLE wait_msg_thread = NULL;

void MrEdMSWSleep(float secs, void *fds)
{
  win_extended_fd_set *r, *w, *e;
 
  if (wxCheckMousePosition())
    return;
 
  if (fds) {
    r = (win_extended_fd_set *)fds;
    w = ((win_extended_fd_set *)fds) + 1;
    e = ((win_extended_fd_set *)fds) + 2;
  } else
    r = w = e = NULL;
    
  /* Block: use different stratgey if there are handles or fds to watch: */
  if (fds && ((r->set.fd_count || w->set.fd_count || e->set.fd_count)
              || r->num_handles)) {
      
    int num_handles = r->num_handles, *rps, two_rps[2];
    HANDLE *handles, two_handles[2];
    SOCKET fake;

    if (num_handles) {
      /* handles has been set up with an extra couple of slots: */ 
      handles = r->handles;
      rps = r->repost_sema;
    } else {
      handles = two_handles;
      rps = two_rps;
    }

  
    HANDLE th2;
    DWORD result;
    DWORD id;

    if (r->set.fd_count || w->set.fd_count || e->set.fd_count) {
      fake = socket(PF_INET, SOCK_STREAM, 0);
      FD_SET(fake, e);

      th2 = CreateThread(NULL, 5000, 
	              (LPTHREAD_START_ROUTINE)signal_fddone,
		      fds, 0, &id);
    
      rps[num_handles] = 0;
      handles[num_handles++] = th2;
    } else
      th2 = NULL;

    if (wait_msg_thread)
      if (WaitForSingleObject(wait_msg_thread, 0) == WAIT_OBJECT_0) {
	CloseHandle(wait_msg_thread);
	wait_msg_thread = NULL;
      }

    if (!wait_msg_thread)
      wait_msg_thread = CreateThread(NULL, 5000, 
				     (LPTHREAD_START_ROUTINE)signal_waitdone,
				     NULL, 0, &id);

    rps[num_handles] = 0;
    handles[num_handles++] = wait_msg_thread;

    result = WaitForMultipleObjects(num_handles, handles, FALSE, 
                                    secs ? (DWORD)(secs * 1000) : INFINITE);

    if ((result >= WAIT_OBJECT_0) && (result < WAIT_OBJECT_0 + num_handles)) {
      result -= WAIT_OBJECT_0;
      if (rps[result])
        ReleaseSemaphore(handles[result], 1, NULL);
    }

    if (th2) {
      closesocket(fake);
      CloseHandle(th2);
    }
  } else {
    UINT id;
    
    if (secs)
      id = SetTimer(NULL, 0, secs * 1000, NULL);
    else
      id = 0;
  
    if (wxTheApp->keep_going)
      WaitMessage();

    if (secs)
      KillTimer(NULL, id);
  }
}

void wxQueueLeaveEvent(void *ctx, wxWindow *wnd, int x, int y, int flags)
{
  MrEdContext *c = (MrEdContext *)ctx;
  LeaveEvent *e = new LeaveEvent(), *prev, *n;

  e->wnd = wnd;
  e->x = x;
  e->y = y;
  e->flags = flags;
  e->next = NULL;

  prev = NULL;
  for (n = c->queued_leaves; n; n = n->next)
    prev = n;

  if (prev)
    prev->next = e;
  else
    c->queued_leaves = e;
}
