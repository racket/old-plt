
/* Copyright (c) 1997-02 PLT (Matthew Flatt)

  This file exists because of a problem in Windows: when
  a built-in dialog is used (such as the FindFile dialog),
  you have no control over the dispatching of events in
  in that thread's event queue. So we run each dialog in
  its own thread.

  */

#include "wx.h"

#include "wx_pdf.h"
#include "wx_timer.h"
#include <direct.h>

extern "C" {
  struct Scheme_Thread_Memory *scheme_remember_thread(void *);
  void scheme_forget_thread(struct Scheme_Thread_Memory *);
};

typedef struct {
  wxPDF f;
  void *data;
  BOOL result;
  BOOL done;
  BOOL usedir;
  HWND hwnd;
} PrimData;

static long DoPrim(void *data)
{
  PrimData *_data = (PrimData *)data;
  MSG msg;
  long old;

  _data->result = _data->f(_data->data, _data->hwnd);

  _data->done = 1;

  return 0;
}

static int Check(void *d)
{
  return ((PrimData *)d)->done;
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);
extern wxWindow *wxHWNDtoWindow(HWND);

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
  wxList *disabled_windows;
  wxChildNode *cnode;   
  wxNode *node;   
  disabled_windows = new wxList;
  for (cnode = wxTopLevelWindows(NULL)->First(); cnode; cnode = cnode->Next()) {
    wxWindow *w = (wxWindow *)cnode->Data();
    if (w && cnode->IsShown()) {
      if (w->GetHWND() != top) {
	disabled_windows->Append(w);
	w->InternalEnable(FALSE);
      }
    }
  }
   
  if (!(th = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)DoPrim, _data, 0, &id))) {
    /* Thread creation failed! We're in trouble, but do `f' anywasy. */
    result = f(data, top);
  } else {
    struct Scheme_Thread_Memory *thread_memory;

    thread_memory = scheme_remember_thread((void *)th);

    /* There used to be a call to AttachThreadInput() here.
       That was disasterous, because it interefered with the
       carefulyl tuned event dispatching of mredmsw.cxx. */

    wxDispatchEventsUntil(Check, (void *)_data);

    WaitForSingleObject(th, INFINITE);
    scheme_forget_thread(thread_memory);
    CloseHandle(th);

    result = _data->result;
  }

  /* Restore other windows: */
  for (node = disabled_windows->First(); node; node = node->Next()) {
    wxWindow *w = (wxWindow *)node->Data();
    w->InternalEnable(TRUE);
  }

  delete disabled_windows;

  return result;
}
