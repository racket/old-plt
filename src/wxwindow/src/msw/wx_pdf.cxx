
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
  *drive = _getdrive();
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
  _chdrive(drive);
  SetCurrentDirectory(dir);
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);
extern wxWindow *wxHWNDtoWindow(HWND);
extern int mred_do_prepost_gm;

static HANDLE checker;
static HANDLE main_thread_for_checker;

void wxDoPreGM(void)
{
  ActiveThread *a;

  SuspendThread(checker);

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

  ff_dir = GetCWD(ff_dir, ff_dir_size, &ff_dir_size, &ff_drive);
  if (real_dir)
    SetCWD(real_dir, real_drive);

  ResumeThread(checker);
}

static long Checker(void *data)
{
  ActiveThread *a;

  /* Why doesn't this chew up cycles? Because when the main
     thread goes into the central WaintMessage(), it enables
     all the prim threads and suspends this checker thread. */
  while (1) {
    /* Give the threads for prims some time, adjusting the
       directory, first. */
    SuspendThread(main_thread_for_checker);
    
    real_dir = GetCWD(real_dir, real_dir_size, &real_dir_size, &real_drive);
    if (ff_dir)
      SetCWD(ff_dir, ff_drive);

    for (a = active_threads; a; a = a->next)
      ResumeThread(a->th);

    /* Those threads have a higher priority, so they should run until
       they've done as much as they can for now. Then we suspend them
       again. Kinda dangerous because the main thread will be swapped
       out indefinitely, but we trust the primitive dialogs. */

    for (a = active_threads; a; a = a->next)
      SuspendThread(a->th);

    ff_dir = GetCWD(ff_dir, ff_dir_size, &ff_dir_size, &ff_drive);
    if (real_dir)
      SetCWD(real_dir, real_drive);

    ResumeThread(main_thread_for_checker);
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
	 checker = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)Checker, NULL, 
				CREATE_SUSPENDED, &id);
	 SetThreadPriority(th, THREAD_PRIORITY_BELOW_NORMAL);
       }

       ResumeThread(checker);
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

       if (!active_threads)
	 SuspendThread(checker);
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
