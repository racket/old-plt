/*
 * File:        wx_clipb.cc
 * Purpose:     Clipboard implementation.
 * Author:      Julian Smart and Matthew Flatt
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004 PLT Scheme, Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#endif

#define Uses_wxApp
#define Uses_wxFrame
#define Uses_wxClipboard
#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib
#include "wx.h"
#include <X11/Xatom.h>
#define  Uses_ShellWidget
#include "widgets.h"

wxClipboard *wxTheClipboard;

Widget wx_clipWindow;
static Widget getClipWindow;

extern void MrEdQueueBeingReplaced(wxClipboardClient *clipOwner);

#ifdef MZ_PRECISE_GC
Atom ATOM(char *atom) 
{
  Widget wgt;
  wgt = wxAPP_TOPLEVEL;
  return XInternAtom(XtDisplay(wgt), atom, FALSE);
}
#else
# define ATOM(atom) XInternAtom(XtDisplay(wxAPP_TOPLEVEL), atom, FALSE)
#endif
#define VALUE_TYPE void*

Atom xa_text, xa_targets;

static wxFrame *clipboard_frame;
static wxFrame *get_clipboard_frame;

void wxInitClipboard(void)
{
  if (!wx_clipWindow) {
    /* Hack: We need a window for clipboard stuff */
    wxWindow_Xintern *fh;
    wxREGGLOB(clipboard_frame);
    wxREGGLOB(get_clipboard_frame);
    clipboard_frame = new wxFrame(NULL, "clipboard", 0, 0, 10, 10);
    get_clipboard_frame = new wxFrame(NULL, "get clipboard", 0, 0, 10, 10);

    fh = clipboard_frame->GetHandle();
    wx_clipWindow = fh->frame;
    XtRealizeWidget(wx_clipWindow);

    fh = get_clipboard_frame->GetHandle();
    getClipWindow = fh->frame;
    XtRealizeWidget(getClipWindow);

    /* Initially not in any specific context. */
    clipboard_frame->context = NULL;
    /* Not in any specific context. */
    get_clipboard_frame->context = NULL;
  }

  if (!wxTheClipboard) {
    wxREGGLOB(wxTheClipboard);
    wxTheClipboard = new wxClipboard;
  }

  xa_text = ATOM("TEXT");
  xa_targets = ATOM("TARGETS");
}

static void AddClipboardFrame(int on)
{
  clipboard_frame->context = NULL;
}

wxClipboardClient::wxClipboardClient()
{
  formats = new wxStringList();
}

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;
}

wxClipboard::~wxClipboard()
{
}

static Boolean wxConvertClipboard(Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *target,
				  Atom *type_return, XtPointer *value_return,
				  unsigned long *length_return,
				  int *format_return)
{
  wxClipboard *cb;
  Atom xa;
  char **formats = NULL;
  int i = 0, count, extra;

  cb = wxTheClipboard;

  if (*target == xa_targets) {
    if (cb->clipOwner) {
      count = cb->clipOwner->formats->Number();
      extra = (cb->clipOwner->formats->Member("TEXT")) ? 1 : 0;
      cb->receivedTargets = new Atom[count + extra];
      formats = cb->clipOwner->formats->ListToArray(FALSE);
      for (i = 0; i < count; i++) {
	Atom atm;
	atm = ATOM(formats[i]);
	((Atom *)cb->receivedTargets)[i] = atm;
      }
      if (extra)
	((Atom *)cb->receivedTargets)[count] = XA_STRING;
    } else {
      count = 2;
      cb->receivedTargets = new Atom[2];
      ((Atom *)cb->receivedTargets)[0] = XA_STRING;
      ((Atom *)cb->receivedTargets)[1] = xa_text;
      extra = 0;
    }

    *value_return = (VALUE_TYPE)cb->receivedTargets;
    *type_return = XA_ATOM;
    *format_return = 8 * sizeof(Atom);
    *length_return = count + extra;

    cb->sentString = NULL;

    return TRUE;
  } 
  
  cb->receivedTargets = NULL;

  if (cb->clipOwner) {
    formats = cb->clipOwner->formats->ListToArray(FALSE);
    for (i = cb->clipOwner->formats->Number(); i--; ) {
      xa = ATOM(formats[i]);
      if (xa == *target)
	break;
      if (xa == xa_text && *target == XA_STRING)
	break;
    }
    if (i < 0)
      return FALSE;
  } else if (*target != xa_text && *target != XA_STRING)
    return FALSE;

  *type_return = XA_STRING;
  *format_return = 8;
  if (cb->clipOwner) {
    long sz = 0;
    char *s;
    s = cb->clipOwner->GetData(formats[i], &sz);
    cb->sentString = s;
    *length_return = sz;
    *value_return = (VALUE_TYPE)cb->sentString;
  } else {
    *value_return = (VALUE_TYPE)cb->cbString;
    *length_return = strlen(cb->cbString);
  }

  return TRUE;
}

static void wxSelectionDone(Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *WXUNUSED(target))
{
  wxClipboard *cb;

  cb = wxTheClipboard;
  cb->sentString = NULL;
  cb->receivedTargets = NULL;
}

static void wxStringSelectionDone(Widget WXUNUSED(w), Atom *WXUNUSED(selection), Atom *WXUNUSED(target))
{
  /* do nothing */
}

static void wxLoseClipboard(Widget WXUNUSED(w), Atom *WXUNUSED(selection))
{
  wxClipboard *cb;

  cb = wxTheClipboard;
  
  if (cb->clipOwner) {
    MrEdQueueBeingReplaced(cb->clipOwner);
    cb->clipOwner = NULL;
    AddClipboardFrame(0);
  }
  cb->cbString = NULL;
}

void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
    AddClipboardFrame(0);
  }
  cbString = NULL;

  clipOwner = client;
  client->context = wxGetContextForFrame();
  clipboard_frame->context = client->context;
  AddClipboardFrame(1);

  got_selection = XtOwnSelection(wx_clipWindow, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard, 
				 wxSelectionDone);

  if (!got_selection) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
    AddClipboardFrame(0);
  }
}

wxClipboardClient *wxClipboard::GetClipboardClient()
{
  return clipOwner;
}

void wxClipboard::SetClipboardString(char *str, long time)
{
  Bool got_selection;

  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
    AddClipboardFrame(0);
  }

  cbString = str;

  got_selection = XtOwnSelection(wx_clipWindow, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard, 
				 wxStringSelectionDone);
  
  if (!got_selection) {
    cbString = NULL;
  }
}

void wxClipboard::SetClipboardBitmap(wxBitmap *bm, long time)
{
  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    AddClipboardFrame(0);
    clipOwner = NULL;
  }

  cbString = NULL;

  /* Don't know how to put a bitmap into the clipboard. */
}

wxBitmap *wxClipboard::GetClipboardBitmap(long time)
{
  return NULL;
}

static void wxGetTargets(Widget WXUNUSED(w), XtPointer WXUNUSED(cbv), Atom *WXUNUSED(sel), Atom *WXUNUSED(type),
			 XtPointer value, unsigned long *len, int *WXUNUSED(format))
{
  wxClipboard *cb = wxTheClipboard;

  if (*len <= 0) {
    cb->receivedTargets = (void *)1; /* To break the waiting loop */
    cb->receivedLength = 0;
  } else {
    cb->receivedTargets = new Atom[*len];
    memcpy(cb->receivedTargets, value, *len * sizeof(Atom));
    cb->receivedLength = *len;
  }
}

static void wxGetSelection(Widget WXUNUSED(w), XtPointer WXUNUSED(cbv), Atom *WXUNUSED(sel), Atom *WXUNUSED(type),
			   XtPointer value, unsigned long *len, int *WXUNUSED(format))
{
  wxClipboard *cb = wxTheClipboard;

  cb->receivedString = new char[*len + 1];
  memcpy(cb->receivedString, value, *len);
  cb->receivedString[*len] = 0;
  cb->receivedLength = *len;
}

char *wxClipboard::GetClipboardString(long time)
{
  char *str;
  long length;

  str = GetClipboardData("TEXT", &length, time);
  if (!str)
    str = "";

  return str;
}

extern void wxBlockUntil(int (*)(void *), void *);

static int clipget_in_progress;

#if 0
static int CheckNotInProgress(void *WXUNUSED(v))
{
  return !clipget_in_progress;
}
#endif

static int CheckReadyTarget(void *WXUNUSED(v))
{
  return !!wxTheClipboard->receivedTargets;
}

static int CheckReadyString(void *WXUNUSED(v))
{
  return !!wxTheClipboard->receivedString;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time)
{
  if (clipOwner)  {
    if (clipOwner->formats->Member(format))
      return wxsGetDataInEventspace(clipOwner, format, length);
    else
      return NULL;
  } else if (cbString) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {
    Atom xa;
    long i;

    /* Need to make sure that only one thread is here at a time. */
    /* Disabled for now because we haven't handled thread kills and 
       escapes. */
#if 0
    wxBlockUntil(CheckNotInProgress, NULL);
#endif

    clipget_in_progress = 1;

    receivedString = NULL;
    receivedTargets = NULL;

    XtGetSelectionValue(getClipWindow, XA_PRIMARY,
			xa_targets, wxGetTargets, (XtPointer)NULL, time);

    wxBlockUntil(CheckReadyTarget, NULL);

    xa = ATOM(format);

    for (i = 0; i < receivedLength; i++) {
      if (((Atom *)receivedTargets)[i] == xa)
	break;
      else if (((Atom *)receivedTargets)[i] == XA_STRING
	       && xa == xa_text) {
	xa = XA_STRING;
	break;
      }
    }

    if (receivedLength)
      receivedTargets = NULL;

    if (i >= receivedLength) {
      clipget_in_progress = 0;
      return NULL;
    }

    XtGetSelectionValue(getClipWindow, XA_PRIMARY,
			xa, wxGetSelection, (XtPointer)NULL, 0);
    
    wxBlockUntil(CheckReadyString, NULL);

    *length = receivedLength;

    clipget_in_progress = 0;

    return receivedString;
  }
}
