/*
 * File:        wx_clipb.cc
 * Purpose:     Clipboard implementation. DIFFERENT FROM MSW IMPLEMENTATION.
 * Author:      Julian Smart and Matthew Flatt
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: Clipboard.cc,v 1.2 1999/11/04 17:25:35 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_clipb.cc	1.2 5/9/94";

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

static Widget clipWindow;

#define ATOM(atom) XInternAtom(XtDisplay(wxAPP_TOPLEVEL), atom, FALSE)
#define VALUE_TYPE void*

Atom xa_text, xa_targets;

void wxInitClipboard(void)
{
  if (!clipWindow) {
    /* Hack: We need a window for clipboard stuff */
    static wxFrame *frame = new wxFrame(NULL, "clipboard", 0, 0, 10, 10);
    XtRealizeWidget(frame->GetHandle()->frame);
    /* Not in any specific context! */
    frame->context = NULL;
    clipWindow = frame->GetHandle()->frame;
  }

  if (!wxTheClipboard)
    wxTheClipboard = new wxClipboard;
  xa_text = ATOM("TEXT");
  xa_targets = ATOM("TARGETS");
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
  if (cbString)
    delete[] cbString;
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
      for (i = 0; i < count; i++)
	((Atom *)cb->receivedTargets)[i] = ATOM(formats[i]);
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
    cb->sentString = cb->clipOwner->GetData(formats[i], &sz);
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
  if (cb->sentString) {
    delete[] cb->sentString;
    cb->sentString = NULL;
  }
  if (cb->receivedTargets)
    delete[]  cb->receivedTargets;
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
    cb->clipOwner->BeingReplaced();
    cb->clipOwner = NULL;
  }
  if (cb->cbString) {
    delete[] cb->cbString;
    cb->cbString = NULL;
  }
}

void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner)
    clipOwner->BeingReplaced();
  clipOwner = client;
  if (cbString) {
    delete[] cbString;
    cbString = NULL;
  }

  got_selection = XtOwnSelection(clipWindow, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard, 
				 wxSelectionDone);

  if (!got_selection) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
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
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
  if (cbString)
    delete[] cbString;

  cbString = str;

  got_selection = XtOwnSelection(clipWindow, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard, 
				 wxStringSelectionDone);
  
  if (!got_selection) {
    delete[] cbString;
    cbString = NULL;
  }
}

static void wxGetTargets(Widget WXUNUSED(w), XtPointer cbv, Atom *WXUNUSED(sel), Atom *WXUNUSED(type),
			 XtPointer value, unsigned long *len, int *WXUNUSED(format))
{
  wxClipboard *cb;

  cb = (wxClipboard *)cbv;
  if (*len <= 0) {
    cb->receivedTargets = (void *)1; /* To break the waiting loop */
    cb->receivedLength = 0;
  } else {
    cb = (wxClipboard *)cbv;
    cb->receivedTargets = new Atom[*len];
    memcpy(cb->receivedTargets, value, *len * sizeof(Atom));
    cb->receivedLength = *len;
  }
}

static void wxGetSelection(Widget WXUNUSED(w), XtPointer cbv, Atom *WXUNUSED(sel), Atom *WXUNUSED(type),
			   XtPointer value, unsigned long *len, int *WXUNUSED(format))
{
  wxClipboard *cb;

  cb = (wxClipboard *)cbv;
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

extern void wxDispatchEventsUntil(int (*)(void *), void *);

static int CheckReady(void *v)
{
  return (int)*(void **)v;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time)
{
  if (clipOwner)  {
    if (clipOwner->formats->Member(format))
      return clipOwner->GetData(format, length);
    else
      return NULL;
  } else if (cbString) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {
    receivedString = NULL;
    receivedTargets = NULL;

    XtGetSelectionValue(clipWindow, XA_PRIMARY,
			xa_targets, wxGetTargets, (XtPointer)this, time);

    wxDispatchEventsUntil(CheckReady, &receivedTargets);

    Atom xa;
    long i;

    xa = ATOM(format);

    for (i = 0; i < receivedLength; i++)
      if (((Atom *)receivedTargets)[i] == xa
	  || (((Atom *)receivedTargets)[i] == XA_STRING
	      && xa == xa_text))
	break;

    if (receivedLength)
      delete[] receivedTargets;

    if (i >= receivedLength)
      return NULL;

    XtGetSelectionValue(clipWindow, XA_PRIMARY,
			xa, wxGetSelection, (XtPointer)this, 0);
    
    wxDispatchEventsUntil(CheckReady, &receivedString);

    *length = receivedLength;

    return receivedString;
  }
}
