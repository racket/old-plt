
/* Copyright (c) 1997-98 PLt (Matthew Flatt)

  This file exists because of a problem in Windows: when
  a built-in dialog is used (such as the FindFile dialog),
  you have no control over the dispatching of events in
  in that thread's event queue.

  "Easy," you say, "Run it in it's own thread."

  Yes, of course. But there's more: Window assigns working
  directories BY PROCESS, not by thread! "Who could be so
  stupid?" you wonder, knowing that the standard Windows
  FindFile dialog changes the working directory. Unfortunately,
  these two horrendous design choices don't cancel each other
  out.

  So we need to make sure that the real thread and the "primitive
  dialog" thread don't run at the same time, and we'll manually
  switch the current directory as the threads take turn. Yes,
  we're trying to implement a mini operating system on top of
  Windows. Again.

  The basic idea is simple. Unfortunately, the thread switches
  are not as isolated as we'd like. Mostly, the primitive dialogs
  will run as the main thread sleeps, and this is handled by
  DoPreGM and DoPostGM. But the main thread can take naps here
  and there, so we create a low-priority thread that swaps in
  the primitive thread and then swaps them back out when they
  sleep (which we trust to be fairly soon).

  Danger! In setting up for the switch, we have to call Windows
  functions - GetCurrentDirectory and SetCurrentDirectory - that
  have process-global locks. So there's yet another, even lower-
  priority thread, that detects this and gives the old thread
  a little more time. Presumably, in this case, the old thread
  was waiting for something extenal to MrEd.

  For the final bit of complication, recall that thread priorities
  are global to the whole operating system. So if there were some
  background process running continuously, our low-priority threads 
  wouldn't get to run at all. So for the critical bits be bump
  up the process's base priority.

  Enjoy.

  */

#if defined(_MSC_VER)
# include "wx.h"
#else
# include "wx.h"
#endif

#include "wx_pdf.h"
#include "wx_timer.h"
#include <direct.h>

typedef struct {
  wxPDF f;
  void *data;
  BOOL result;
  BOOL done;
  BOOL usedir;
  HWND hwnd;
  HANDLE ready_sema;
  HANDLE go_sema;
} PrimData;

typedef struct ActiveThread {
  HANDLE th;
  struct ActiveThread *next;
} ActiveThread;

static ActiveThread *active_threads;

static int ff_dir_size;
static char *ff_dir;
static int ff_drive;
static int real_dir_size;
static char *real_dir;
static int real_drive;

static long DoPrim(void *data)
{
  PrimData *_data = (PrimData *)data;
  MSG msg;
  long old;

  // Use common dialog directory
  if (_data->usedir && ff_dir)
    SetCurrentDirectory(ff_dir);
  
  // Create a queue - yuck!
  // We have to do this so the queue can be joined to the
  // main thread.
  PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE);
  ReleaseSemaphore(_data->ready_sema, 1, &old);
  WaitForSingleObject(_data->go_sema, INFINITE);

  _data->result = _data->f(_data->data, _data->hwnd);

  _data->done = 1;

  return 0;
}

static int Check(void *d)
{
  return ((PrimData *)d)->done;
}

static char *GetCWD(char *buf, int maxl, int *size, int *drive)
{
  char *d;
  // *drive = _getdrive();
  int needed = GetCurrentDirectory(maxl, buf);
  if (needed < maxl) {
    d = buf;
    if (size)
      *size = maxl;
  } else {
    d = new char[needed + 2];
    GetCurrentDirectory(needed + 2, d);
    if (size)
      *size = needed + 2;
  }

  return d;
}

static void SetCWD(char *dir, int drive)
{
  // _chdrive(drive);
  SetCurrentDirectory(dir);
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);
extern wxWindow *wxHWNDtoWindow(HWND);
extern int mred_do_prepost_gm;

static HANDLE checker;
static HANDLE main_thread_for_checker;
static HANDLE deadlock_detector;
static HANDLE deadlock_detector2;

static int wait_at_pre = 0;
static int undeadlock_via_prims;

static int gear = 0;

static void HighGear(void)
{
	if (!gear++) {
	  /* It goes all the way to 11! */
	  SetPriorityClass(GetCurrentProcess(), 11); 
    }
}

static void LowGear(void)
{
	if (!--gear) {
		SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
	}
}

static void WaitHere(void)
{
  if (wait_at_pre) {
    /* We were running to get a lock released. Now we should wait. */
	/* We don't have to worry much about race conditions because
	   the loop below can't lock anything */
	SetThreadPriority(main_thread_for_checker, THREAD_PRIORITY_LOWEST);
    do {
		Sleep(10); /* deadlock detector should take back over */
	} while (wait_at_pre);
	SetThreadPriority(main_thread_for_checker, THREAD_PRIORITY_NORMAL);
  }
}

void wxDoPreGM(void)
{
  WaitHere();

  ActiveThread *a;

  LowGear();
  SuspendThread(checker);

  /* The checker can't have locked CWD, so no need for the deadlock checker */
  real_dir = GetCWD(real_dir, real_dir_size, &real_dir_size, &real_drive);
  if (ff_dir)
    SetCWD(ff_dir, ff_drive);

  for (a = active_threads; a; a = a->next)
    ResumeThread(a->th);
}

void wxDoPostGM(void)
{
  ActiveThread *a;

  for (a = active_threads; a; a = a->next)
    SuspendThread(a->th);

  /* Did we catch an active thread at a bad time? See
     "This is insane", below. */
  ResumeThread(deadlock_detector2);
  HighGear();
  ff_dir = GetCWD(ff_dir, ff_dir_size, &ff_dir_size, &ff_drive);
  LowGear();
  SuspendThread(deadlock_detector2);
  if (real_dir)
    SetCWD(real_dir, real_drive);

  ResumeThread(checker);
  HighGear();
}

static long DeadlockChecker(void *data)
/* See "This is insane", below */
{
	ActiveThread *a;

	while (1) {
	  SuspendThread(checker);
	  if (undeadlock_via_prims) {
		  for (a = active_threads; a; a = a->next)
            ResumeThread(a->th);
		  for (a = active_threads; a; a = a->next)
            SuspendThread(a->th);
	  } else {
	      wait_at_pre = 1;
          ResumeThread(main_thread_for_checker);
          SuspendThread(main_thread_for_checker);
		  /* in case it started waiting: */
		  SetThreadPriority(main_thread_for_checker, THREAD_PRIORITY_NORMAL);
	      wait_at_pre = 0;
	  }
	  ResumeThread(checker);

	  Sleep(0);
    }

	return 0;
}

static long DeadlockChecker2(void *data)
/* See "This is insane", below */
{
	ActiveThread *a;

	while (1) {
	  /* This is the reason that we have a Checker2 instead
	     of parameterizing DeadlockChecker: with parameterization,
		 there would be a race condition on reading the argument to
		 SuspendThread and setting the parameterized argument. */
	  SuspendThread(main_thread_for_checker);
	  for (a = active_threads; a; a = a->next)
         ResumeThread(a->th);
	  for (a = active_threads; a; a = a->next)
         SuspendThread(a->th);
	  ResumeThread(main_thread_for_checker);

	  Sleep(0);
    }

	return 0;
}

static long PrimtimeChecker(void *data)
{
  ActiveThread *a;

  /* Why doesn't this chew up cycles? First, because it runs
     with low priority. Second, because when the main
     thread goes into the central WaitMessage(), it enables
     all the prim threads and suspends this checker thread. */
  while (1) {
    /* Give the threads for prims some time, adjusting the
       directory, first. */
    SuspendThread(main_thread_for_checker);
    
	SetThreadPriority(checker, THREAD_PRIORITY_NORMAL);

	/* This is insane. It's possible now to go into deadlock
	   because the main thread may have a lock on getting/setting
	   CWD. deadlock_detector detects this, and gives the main
	   thread more time. The main thread will eventually give up
	   the lock, and we'll be on our way. */
    undeadlock_via_prims = 0;
    ResumeThread(deadlock_detector);
    real_dir = GetCWD(real_dir, real_dir_size, &real_dir_size, &real_drive);
    SuspendThread(deadlock_detector);
    if (ff_dir)
      SetCWD(ff_dir, ff_drive);
	
    for (a = active_threads; a; a = a->next)
      ResumeThread(a->th);

	SetThreadPriority(checker, THREAD_PRIORITY_BELOW_NORMAL);

	Sleep(0);
    /* Those threads have a higher priority than the checker, so they run
       until they've done as much as they can for now. Then we suspend them
       again. Kinda dangerous because the main thread will be swapped
       out indefinitely, but we trust the primitive dialogs. */

	SetThreadPriority(checker, THREAD_PRIORITY_NORMAL);

    for (a = active_threads; a; a = a->next)
      SuspendThread(a->th);

	/* See "This is insane", above. */
    undeadlock_via_prims = 1;
    ResumeThread(deadlock_detector);
    ff_dir = GetCWD(ff_dir, ff_dir_size, &ff_dir_size, &ff_drive);
    SuspendThread(deadlock_detector);
    if (real_dir)
      SetCWD(real_dir, real_drive);

    ResumeThread(main_thread_for_checker);

	SetThreadPriority(checker, THREAD_PRIORITY_BELOW_NORMAL);
	Sleep(0);
  }

  return 0;
}

BOOL wxPrimitiveDialog(wxPDF f, void *data, int strict)
{
   long old;
   DWORD id;
   HANDLE th;
   PrimData *_data = new PrimData;
   BOOL result;
   HWND top;
   wxWindow *w;

   _data->f = f;
   _data->data = data;
   _data->usedir = strict;
   _data->done = 0;
   _data->ready_sema = CreateSemaphore(NULL, 0, 1, NULL);
   _data->go_sema = CreateSemaphore(NULL, 0, 1, NULL);

   /* Make window child of frontmost if it exists in the
      same context, and disable all others in the context. */
   top = GetForegroundWindow();
   w = wxHWNDtoWindow(top);
   if (w) {
     void *wc, *tc;
     if (wxSubType(w->__type, wxTYPE_FRAME))
       wc = ((wxFrame *)w)->context;
     else if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
       wc = ((wxDialogBox *)w)->context;
     else
       wc = NULL;
     tc = wxGetContextForFrame();
     if (wc != tc)
       top = NULL;
   } else
     top = NULL;

   _data->hwnd = top;

   // Disable other windows:
   wxList disabled_windows;
   wxChildNode *cnode;   
   wxNode *node;   
   for (cnode = wxTopLevelWindows(NULL)->First(); cnode; cnode = cnode->Next()) {
     wxWindow *w = (wxWindow *)cnode->Data();
     if (w && cnode->IsShown()) {
       if (w->GetHWND() != top) {
	 disabled_windows.Append(w);
	 w->InternalEnable(FALSE);
       }
     }
   }
   
   char *orig_dir, dbuf[MAX_PATH];
   int orig_drive;
   if (strict) {
     if (checker && active_threads)
       SuspendThread(checker);

     orig_dir = GetCWD(dbuf, MAX_PATH, NULL, &orig_drive);
     if (ff_dir)
       SetCWD(ff_dir, ff_drive);
   }

   if (!(th = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)DoPrim, _data, 0, &id))) {
     if (strict && checker && active_threads)
       ResumeThread(checker);
     result = f(data, top);
   } else {
     WaitForSingleObject(_data->ready_sema, INFINITE);
     AttachThreadInput(id, GetCurrentThreadId(), TRUE);
     ReleaseSemaphore(_data->go_sema, 1, &old);

     // If it's the FindFile dialog, we want to control the thread's
     // processing so that the cwd is saved/retored. Do this by
     // suspending the thread, and resuming it from a low-priority
     // thread (checker) that sets the CWD before letting the thread
     // run.
     if (strict) {
       SuspendThread(th);
       ff_dir = GetCWD(ff_dir, ff_dir_size, &ff_dir_size, &ff_drive);
       SetCWD(orig_dir, orig_drive);
       mred_do_prepost_gm++;

       ActiveThread *a;
       a = new ActiveThread;
       a->th = th;
       a->next = active_threads;
       active_threads = a;
       
       if (!checker) {
	 DuplicateHandle(GetCurrentProcess(),
			 GetCurrentThread(),
			 GetCurrentProcess(),
			 &main_thread_for_checker,
			 DUPLICATE_SAME_ACCESS,
			 FALSE,
			 0);
	 checker = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)PrimtimeChecker, NULL, 
				CREATE_SUSPENDED, &id);
	 SetThreadPriority(checker, THREAD_PRIORITY_BELOW_NORMAL);
     deadlock_detector = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)DeadlockChecker, NULL, 
				CREATE_SUSPENDED, &id);
	 SetThreadPriority(deadlock_detector, THREAD_PRIORITY_LOWEST);
	 deadlock_detector2 = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)DeadlockChecker2, NULL, 
				CREATE_SUSPENDED, &id);
	 SetThreadPriority(deadlock_detector2, THREAD_PRIORITY_LOWEST);
       }

	   if (!active_threads->next) {
         ResumeThread(checker);
		 HighGear();
	   }
     }

     wxDispatchEventsUntil(Check, (void *)_data);

     result = _data->result;

     if (strict) {
       --mred_do_prepost_gm;
       
       ActiveThread *a, *prev = NULL;
       a = active_threads;
       while (a && (a->th != th)) {
	 prev = a;
	 a = a->next;
       }
       if (a) {
	 if (prev)
	   prev->next = a->next;
	 else
	   active_threads = a->next;
       }

       if (!active_threads) {
		WaitHere();
		LowGear();
	    SuspendThread(checker);
	   }
     }

     CloseHandle(th);
   }

   // Restore other windows:
   for (node = disabled_windows.First(); node; node = node->Next()) {
     wxWindow *w = (wxWindow *)node->Data();
     w->InternalEnable(TRUE);
   }

  if (_data->ready_sema)
    CloseHandle(_data->ready_sema);
  if (_data->go_sema)
    CloseHandle(_data->go_sema);

  return result;
}
