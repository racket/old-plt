/*
 * File:        wx_clipb.cc
 * Purpose:     Clipboard implementation. DIFFERENT FROM MSW IMPLEMENTATION.
 * Author:      Julian Smart and Matthew Flatt
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_clipb.cxx,v 1.1.1.1 1997/12/22 16:12:07 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_clipb.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_CLIPBOARD
#include "wx_clipb.h"

#include "wx_main.h"
#include "wx_clipb.h"
#include "wx_utils.h"

wxClipboard *wxTheClipboard;

#ifdef wx_xview
#include <xview/sel_pkg.h>
extern Xv_Server xview_server;

#define ATOM(atom) xv_get(xview_server, SERVER_ATOM, atom)
#define VALUE_TYPE Xv_opaque
#endif

#ifdef wx_motif
#define ATOM(atom) XInternAtom(XtDisplay(wxTheApp->topLevel), atom, FALSE)
#define VALUE_TYPE void*
#endif

Atom xa_text, xa_targets;

Bool 
wxOpenClipboard (void)
{
  return FALSE;
}

Bool 
wxCloseClipboard (void)
{
  return FALSE;
}

Bool 
wxEmptyClipboard (void)
{
  return FALSE;
}

Bool 
wxIsClipboardFormatAvailable (int dataFormat)
{
  return FALSE;
}

Bool 
wxSetClipboardData (int dataFormat, wxObject * obj, int width, int height)
{
  return FALSE;
}

wxObject *
wxGetClipboardData (int dataFormat)
{
  return NULL;
}

int 
wxEnumClipboardFormats (int dataFormat)
{
  return 0;
}

int 
wxRegisterClipboardFormat (char *formatName)
{
  return 0;
}

Bool 
wxGetClipboardFormatName (int dataFormat, char *formatName, int maxCount)
{
  formatName[0] = 0;
  return FALSE;
}

/*
 * Matthew Flatt's code
 */


void wxInitClipboard(void)
{
  if (!wxTheClipboard)
    wxTheClipboard = new wxClipboard;
  xa_text = ATOM("TEXT");
  xa_targets = ATOM("TARGETS");
}

IMPLEMENT_DYNAMIC_CLASS(wxClipboard, wxObject)
IMPLEMENT_ABSTRACT_CLASS(wxClipboardClient, wxObject)

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;
#ifdef wx_xview
  sel_owner = 0;
#endif
}

wxClipboard::~wxClipboard()
{
  if (cbString)
    delete[] cbString;
}

#ifdef wx_motif
static Boolean wxConvertClipboard(Widget w, Atom *selection, Atom *target,
				  Atom *type_return, XtPointer *value_return,
				  unsigned long *length_return,
				  int *format_return)
#endif
#ifdef wx_xview
static int wxConvertClipboard(Selection_owner sel, Atom *target,
			      Xv_opaque *value_return, long *length_return,
			      int *format_return)
#endif
{
  wxClipboard *cb;
  Atom xa;
  char **formats;
  int i, count, extra;

  cb = wxTheClipboard;

  if (*target == xa_targets) {
    if (cb->clipOwner) {
      count = cb->clipOwner->formats.Number();
      extra = (cb->clipOwner->formats.Member("TEXT")) ? 1 : 0;
      cb->receivedTargets = new Atom[count + extra];
      formats = cb->clipOwner->formats.ListToArray(FALSE);
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
#ifdef wx_motif
    *type_return = XA_ATOM;
#else
    *target = XA_ATOM;
#endif
    *format_return = 8 * sizeof(Atom);
    *length_return = count + extra;

    cb->sentString = NULL;

    return TRUE;
  } 
  
  cb->receivedTargets = NULL;

  if (cb->clipOwner) {
    formats = cb->clipOwner->formats.ListToArray(FALSE);
    for (i = cb->clipOwner->formats.Number(); i--; ) {
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

#ifdef wx_motif
  *type_return = XA_STRING;
#else
  *target = XA_STRING;
#endif
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

#ifdef wx_motif
static void wxSelectionDone(Widget w, Atom *selection, Atom *target)
#endif
#ifdef wx_xview
static void wxSelectionDone(Selection_owner sel, Xv_opaque data, Atom target)
#endif
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

#ifdef wx_motif
static void wxStringSelectionDone(Widget w, Atom *selection, Atom *target)
#endif
#ifdef wx_xview
static void wxStringSelectionDone(Selection_owner sel, Xv_opaque data, Atom target)
#endif
{
  /* do nothing */
}


#ifdef wx_motif
static void wxLoseClipboard(Widget w, Atom *selection)
#endif
#ifdef wx_xview
static void wxLoseClipboard(Selection_owner sel)
#endif
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

  got_selection = XtOwnSelection(wxTheApp->topLevel, XA_PRIMARY, time,
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

  got_selection = XtOwnSelection(wxTheApp->topLevel, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard,
				 wxStringSelectionDone);

  if (!got_selection) {
    delete[] cbString;
    cbString = NULL;
  }
}

#ifdef wx_motif
static void wxGetTargets(Widget w, XtPointer cbv, Atom *sel, Atom *type,
			 XtPointer value, unsigned long *len, int *format)
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

static void wxGetSelection(Widget w, XtPointer cbv, Atom *sel, Atom *type,
			   XtPointer value, unsigned long *len, int *format)
{
  wxClipboard *cb;

  cb = (wxClipboard *)cbv;
  cb->receivedString = new char[*len + 1];
  memcpy(cb->receivedString, value, *len);
  cb->receivedString[*len] = 0;
  cb->receivedLength = *len;
}
#endif

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
    if (clipOwner->formats.Member(format))
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

    XtGetSelectionValue(wxTheApp->topLevel, XA_PRIMARY,
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

    XtGetSelectionValue(wxTheApp->topLevel, XA_PRIMARY,
			xa, wxGetSelection, (XtPointer)this, 0);
    
    wxDispatchEventsUntil(CheckReady, &receivedString);

    *length = receivedLength;

    return receivedString;
  }
}

#endif
