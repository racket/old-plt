/*
 * File:      wx_text.cc
 * Purpose:     wxTextWindow implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_text.cxx,v 1.2 1998/04/08 00:09:16 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_text.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include <fstream.h>
#include <stdio.h>
#include <ctype.h>

#include "common.h"
#include "wx_main.h"
#include "wx_panel.h"
#include "wx_text.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wx_privt.h"

#ifdef wx_motif
#include <Xm/Text.h>
#include <sys/types.h>
#include <sys/stat.h>
static void 
wxTextWindowChangedProc (Widget w, XtPointer clientData, XtPointer ptr);
static void 
wxTextWindowModifyProc (Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs);
static void 
wxTextWindowGainFocusProc (Widget w, XtPointer clientData, XmAnyCallbackStruct *cbs);
static void 
wxTextWindowLoseFocusProc (Widget w, XtPointer clientData, XmAnyCallbackStruct *cbs);
#endif

extern void wxFrameCheckFocus(wxWindow *w);

IMPLEMENT_DYNAMIC_CLASS(wxTextWindow, wxWindow)

wxTextWindow::wxTextWindow (void)
{
  file_name = NULL;

  textPosition = 0;
  textModified = FALSE;
}

wxTextWindow::wxTextWindow (wxWindow * parent, int x, int y, int width, int height,
	      long style, char *name):
wxbTextWindow (parent, x, y, width, height, style, name)
{
  Create (parent, x, y, width, height, style, name);
}

Bool wxTextWindow::
Create (wxWindow * parent, int x, int y, int width, int height,
	long style, char *name)
{
  file_name = NULL;
  windowStyle = style;

  windowName = copystring (name);

  textPosition = 0;
  textModified = FALSE;
  tempCallbackStruct = NULL;

  Widget parentWidget = 0;
  if (wxSubType(parent->__type, wxTYPE_FRAME))
    parentWidget = ((wxFrame *)parent)->clientArea;
  else if (wxSubType(parent->__type, wxTYPE_PANEL))
    parentWidget = (Widget)parent->handle;
  else
  {
    wxError("Text subwindow must be a child of either a frame or panel!");
    return FALSE;
  }

  Widget textWidget = XmCreateScrolledText (parentWidget, windowName, NULL, 0);

  XtVaSetValues (textWidget,
		 XmNeditable, ((style & wxREADONLY) ? False : True),
		 XmNeditMode, XmMULTI_LINE_EDIT,
		 NULL);

  if (wxWidgetHashTable->Get ((long) textWidget))
    {
      wxError ("Widget table clash in wx_text.cc");
    }
  wxWidgetHashTable->Put ((long) textWidget, this);

  XtTranslations ptr;
  XtOverrideTranslations (textWidget,
		   ptr = XtParseTranslationTable ("<Configure>: resize()"));
  XtFree ((char *) ptr);

  XtAddCallback(textWidget, XmNvalueChangedCallback, (XtCallbackProc)wxTextWindowChangedProc, (XtPointer)this);

  XtAddCallback(textWidget, XmNmodifyVerifyCallback, (XtCallbackProc)wxTextWindowModifyProc, (XtPointer)this);

  //  XtAddCallback(textWidget, XmNactivateCallback, (XtCallbackProc)wxTextWindowModifyProc, (XtPointer)this);

  XtAddCallback(textWidget, XmNfocusCallback, (XtCallbackProc)wxTextWindowGainFocusProc, (XtPointer)this);

  XtAddCallback(textWidget, XmNlosingFocusCallback, (XtCallbackProc)wxTextWindowLoseFocusProc, (XtPointer)this);

  XtManageChild (textWidget);
  handle = (char *) textWidget;

  if (wxSubType(parent->__type, wxTYPE_PANEL))
    ((wxPanel *)parent)->AttachWidget(this, 0, x, y, width, height);
  else
    SetSize (x, y, width, height);
  if (parent)
    parent->AddChild (this);
  window_parent = parent;

  wxWidgetHashTable->Put((long)textWidget, this);
  AddPreHandlers(textWidget);

  return TRUE;
}

wxTextWindow::~wxTextWindow (void)
{
  wxWidgetHashTable->Delete((long)handle);
}

Bool wxTextWindow::LoadFile (char *file)
{
  if (file_name)
    delete[]file_name;

  file_name = copystring (file);

  Widget textWidget = (Widget) handle;
  FILE *fp;

  struct stat statb;
  if ((stat (file, &statb) == -1) || (statb.st_mode & S_IFMT) != S_IFREG ||
      !(fp = fopen (file, "r")))
    {
      return FALSE;
    }
  else
    {
      long len = statb.st_size;
      char *text;
      if (!(text = XtMalloc ((unsigned) (len + 1))))
	{
	  fclose (fp);
	  return FALSE;
	}
      if (fread (text, sizeof (char), len, fp) != len)
	{
	}
      fclose (fp);

      text[len] = 0;
      XmTextSetString (textWidget, text);
      textPosition = len;
      XtFree (text);
      textModified = FALSE;
      return TRUE;
    }
}

// If file is null, try saved file name first
// Returns TRUE if succeeds.
Bool wxTextWindow::SaveFile (char *file)
{
  Widget textWidget = (Widget) handle;
  FILE *fp;

  if (!(fp = fopen (file, "w")))
    {
      return FALSE;
    }
  else
    {
      char *text = XmTextGetString (textWidget);
      long len = XmTextGetLastPosition (textWidget);

      if (fwrite (text, sizeof (char), len, fp) != len)
	{
	  // Did not write whole file
	}
      // Make sure newline terminates the file
      if (text[len - 1] != '\n')
	fputc ('\n', fp);

      fclose (fp);
      XtFree (text);
      textModified = FALSE;
      return TRUE;
    }
}

void wxTextWindow::WriteText (char *text)
{
  Widget textWidget = (Widget) handle;
  XmTextInsert (textWidget, GetInsertionPoint(), text);
  textPosition += strlen (text);
  XtVaSetValues (textWidget, XmNcursorPosition, textPosition, NULL);
  XmTextShowPosition (textWidget, textPosition);
  textModified = TRUE;
}

void wxTextWindow::SetSize (int x, int y, int w, int h, int sizeFlags)
{
  Widget textWidget = (Widget) handle;
  Widget parent = XtParent (textWidget);
  Bool isShow = XtIsManaged(parent);
  if (isShow)
    XtUnmanageChild (parent);

  if (x > -1)
    XtVaSetValues (parent,
//                  XmNleftAttachment, XmATTACH_SELF,
		   XmNx, x,
		   NULL);

  if (y > -1)
    XtVaSetValues (parent,
//                  XmNtopAttachment, XmATTACH_SELF,
		   XmNy, y,
		   NULL);

  if (w > -1)
    XtVaSetValues (parent, XmNwidth, w, NULL);
  if (h > -1)
    XtVaSetValues (parent, XmNheight, h, NULL);

  if (isShow)
    XtManageChild (parent);

  sr_width = w;
  sr_height = h;
  GetEventHandler()->OnSize (w, h);
}

void wxTextWindow::SetClientSize (int w, int h)
{
  SetSize (-1, -1, w, h);
}


void wxTextWindow::Clear (void)
{
  XmTextSetString ((Widget) handle, "");
  textPosition = 0;
  textModified = FALSE;
}

Bool wxTextWindow::Modified (void)
{
  return textModified;
}

// Not clear whether Clear is required as well as DiscardEdits
void wxTextWindow::DiscardEdits (void)
{
  XmTextSetString ((Widget) handle, "");
  textPosition = 0;
  textModified = FALSE;
}

void wxTextWindow::SetEditable (Bool editable)
{
  Widget textWidget = (Widget) handle;
  XmTextSetEditable (textWidget, editable);
}

char *wxTextWindow::GetContents (void)
{
  char *s = XmTextGetString ((Widget) handle);
  if (s)
    {
      char *res = copystring (s);
      XtFree (s);
      return res;
    }
  return NULL;
}

void wxTextWindow::SetInsertionPoint (long pos)
{
  Widget textWidget = (Widget) handle;
  XmTextSetInsertionPosition (textWidget, (XmTextPosition) pos);
  textPosition = pos;
}

void wxTextWindow::SetInsertionPointEnd (void)
{
  Widget textWidget = (Widget) handle;
  XmTextPosition pos = XmTextGetLastPosition (textWidget);
  XmTextSetInsertionPosition (textWidget, (XmTextPosition) (pos + 1));
}

long wxTextWindow::GetInsertionPoint (void)
{
  Widget textWidget = (Widget) handle;
  return (long) XmTextGetInsertionPosition (textWidget);
}

long wxTextWindow::GetLastPosition (void)
{
  Widget textWidget = (Widget) handle;
  return (long) XmTextGetLastPosition (textWidget);
}

long wxTextWindow::XYToPosition (long x, long y)
{
  Widget textWidget = (Widget) handle;
  return (long) XmTextXYToPos (textWidget, (Position) x, (Position) y);
}

void wxTextWindow::PositionToXY (long pos, long *x, long *y)
{
  Position xx;
  Position yy;
  Widget textWidget = (Widget)handle;
  XmTextPosToXY(textWidget, pos, &xx, &yy);
  *x = xx;
  *y = yy;
}

void wxTextWindow::ShowPosition (long pos)
{
  Widget textWidget = (Widget) handle;
  XmTextShowPosition (textWidget, (XmTextPosition) pos);
}

int wxTextWindow::GetLineLength (long lineNo)
{
  GetLineText (lineNo, wxBuffer);
  return strlen (wxBuffer);
}

int wxTextWindow::GetNumberOfLines (void)
{
  Widget textWidget = (Widget) handle;
  // HIDEOUSLY inefficient, but we have no choice.
  char *s = XmTextGetString (textWidget);
  if (s)
    {
      long i = 0;
      int currentLine = 0;
      Bool finished = FALSE;
      while (!finished)
	{
	  int ch = s[i];
	  if (ch == '\n')
	    {
	      currentLine++;
	      i++;
	    }
	  else if (ch == 0)
	    {
	      finished = TRUE;
	    }
	  else
	    i++;
	}

      XtFree (s);
      return currentLine;
    }
  return 0;
}

int wxTextWindow::GetLineText (long lineNo, char *buf)
{
  Widget textWidget = (Widget) handle;
  // HIDEOUSLY inefficient, but we have no choice.
  char *s = XmTextGetString (textWidget);
  if (s)
    {
      long i = 0;
      int currentLine = 0;
      Bool finished = FALSE;
      while (!finished)
	{
	  int ch = s[i];
	  if (ch == '\n')
	    {
	      currentLine++;
	      i++;
	      if (currentLine == lineNo)
		finished = TRUE;
	    }
	  else if (ch == 0)
	    {
	      finished = TRUE;
	    }
	  else
	    i++;
	}
      // Now get the text
      finished = FALSE;
      int j = 0;
      while (!finished)
	{
	  int ch = s[i];
	  if (ch == '\n')
	    {
	      buf[j] = 0;
	      finished = TRUE;
	    }
	  else if (ch == 0)
	    {
	      buf[j] = 0;
	      finished = TRUE;
	    }
	  else
	    {
	      i++;
	      j++;
	    }
	}

      XtFree (s);
      return j;
    }
  return 0;
}

void wxTextWindow::Replace (long from, long to, char *value)
{
  Widget textWidget = (Widget) handle;
  XmTextReplace (textWidget, (XmTextPosition) from, (XmTextPosition) to,
		 value);
}

void wxTextWindow::Remove (long from, long to)
{
  Widget textWidget = (Widget) handle;
  XmTextSetSelection (textWidget, (XmTextPosition) from, (XmTextPosition) to,
		      (Time) 0);
  XmTextRemove (textWidget);
}

void wxTextWindow::SetSelection(long from, long to)
{
  Widget textWidget = (Widget) handle;
  XmTextSetSelection (textWidget, (XmTextPosition) from, (XmTextPosition) to,
		      (Time) 0);
}

// Copy selection to clipboard
void wxTextWindow::Copy(void)
{
  XmTextCopy((Widget)handle, CurrentTime);
}

// Paste clipboard into text window
void wxTextWindow::Paste(void)
{
  XmTextPaste((Widget)handle);
}

// Copy selection to clipboard, then remove selection.
void wxTextWindow::Cut(void)
{
  XmTextCut((Widget)handle, CurrentTime);
}

static void 
wxTextWindowChangedProc (Widget w, XtPointer clientData, XtPointer ptr)
{
  if (!wxWidgetHashTable->Get ((long) w))
    // Widget has been deleted!
    return;

  wxTextWindow *tw = (wxTextWindow *) clientData;
  tw->SetModified(TRUE);
}

static void 
wxTextWindowModifyProc (Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs)
{
//  if (!wxWidgetHashTable->Get ((long) w))
//    return;

  wxTextWindow *tw = (wxTextWindow *) clientData;

  // If we're already within an OnChar, return: probably
  // a programmatic insertion.
  if (tw->tempCallbackStruct)
    return;

  // Check for a backspace
  if (cbs->startPos == (cbs->currInsert - 1))
  {
    tw->tempCallbackStruct = (XtPointer)cbs;

    wxKeyEvent *_event = new wxKeyEvent(wxEVENT_TYPE_CHAR);
    wxKeyEvent &event = *_event;

    event.keyCode = WXK_DELETE;
    event.eventObject = tw;

    // Only if wxTextWindow::OnChar is called
    // will this be set to True (and the character
    // passed through)
    cbs->doit = False;

    tw->tempCallbackStruct = NULL;

    return;
  }

  // Pasting operation: let it through without
  // calling OnChar
  if (cbs->text->length > 1)
    return;

  // Something other than text
  if (cbs->text->ptr == NULL)
    return;

  tw->tempCallbackStruct = (XtPointer)cbs;

  wxKeyEvent *_event = new wxKeyEvent(wxEVENT_TYPE_CHAR);
  wxKeyEvent &event = *_event;

  event.keyCode = (cbs->text->ptr[0] == 10 ? 13 : cbs->text->ptr[0]);
  event.eventObject = tw;

  // Only if wxTextWindow::OnChar is called
  // will this be set to True (and the character
  // passed through)
  cbs->doit = False;

  tw->tempCallbackStruct = NULL;
}

static void 
wxTextWindowGainFocusProc (Widget w, XtPointer clientData, XmAnyCallbackStruct *cbs)
{
  if (!wxWidgetHashTable->Get ((long) w))
    return;

  wxTextWindow *tw = (wxTextWindow *) clientData;
  wxFrameCheckFocus(tw);
#if 0
  tw->GetEventHandler()->OnSetFocus();
#endif
}

static void 
wxTextWindowLoseFocusProc (Widget w, XtPointer clientData, XmAnyCallbackStruct *cbs)
{
  if (!wxWidgetHashTable->Get ((long) w))
    return;

  wxTextWindow *tw = (wxTextWindow *) clientData;
  wxFrameCheckFocus(tw);
#if 0
  tw->GetEventHandler()->OnKillFocus();
#endif
}

void wxTextWindow::OnChar(wxKeyEvent& event)
{
  if (tempCallbackStruct)
  {
    XmTextVerifyCallbackStruct *textStruct =
        (XmTextVerifyCallbackStruct *)tempCallbackStruct;
    textStruct->doit = True;
    if (isascii(event.keyCode) && (textStruct->text->length == 1))
    {
      textStruct->text->ptr[0] = ((event.keyCode == WXK_RETURN) ? 10 : event.keyCode);
    }
  }
}
