/*
 * File:      wx_text.cc
 * Purpose:     wxTextWindow implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_text.cc,v 1.3 1994/08/14 21:28:43 edz Exp $
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

#ifdef wx_xview
// Don't know why it should be necessary to define this!
#define _OTHER_TEXTSW_FUNCTIONS
#include <xview/textsw.h>
#include <xview/frame.h>

// Old notify proc
static void (*wxTextWindowDefaultNotifyProc)(Textsw, Attr_avlist);
// New notify proc
void wxTextWindowNotifyProc(Textsw textsw, Attr_avlist avlist);
#endif

IMPLEMENT_DYNAMIC_CLASS(wxTextWindow, wxWindow)

wxTextWindow::wxTextWindow (void)
{
  file_name = NULL;

#ifdef wx_motif
  textPosition = 0;
  textModified = FALSE;
#endif
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
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  if (!wxSubType(parent->__type, wxTYPE_FRAME))
  {
    wxError("A text subwindow must be a child of a frame in XView!");
    return FALSE;
  }
  wxFrame *frame = (wxFrame *)parent;

  int real_y = frame->y_offset;
  if (y > -1)
    real_y = y + frame->y_offset;	// Allow for possible menu bar

  Frame x_frame = (Frame) frame->handle;
  Textsw x_textsw = (Textsw) xv_create (x_frame, TEXTSW,
			     WIN_CLIENT_DATA, (char *) this, XV_SHOW, FALSE,
                             TEXTSW_MEMORY_MAXIMUM, 400000,
                             TEXTSW_READ_ONLY,
                                   ((style & wxREADONLY) ? TRUE : FALSE),
			     NULL);
  handle = (char *) x_textsw;
  window_parent = frame;
  file_name = NULL;

  wxTextWindowDefaultNotifyProc =
   (void (*)(Textsw, Attr_avlist)) xv_get(x_textsw, TEXTSW_NOTIFY_PROC);

  xv_set(x_textsw, TEXTSW_NOTIFY_PROC, wxTextWindowNotifyProc, NULL);

  if (x > -1)
    xv_set (x_textsw, XV_X, x, NULL);
  if (y > -1)
    xv_set (x_textsw, XV_Y, real_y, NULL);
  if (width > -1)
    xv_set (x_textsw, XV_WIDTH, width, NULL);
  if (height > -1)
    xv_set (x_textsw, XV_HEIGHT, height, NULL);

  xv_set (x_textsw, XV_SHOW, TRUE, NULL);
#endif
  if (parent)
    parent->AddChild (this);
  window_parent = parent;
  return TRUE;
}

wxTextWindow::~wxTextWindow (void)
{
#ifdef wx_motif
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;

  textsw_reset (textsw, 0, 0);
#endif
}

Bool wxTextWindow::LoadFile (char *file)
{
  if (file_name)
    delete[]file_name;

  file_name = copystring (file);
#ifdef wx_motif
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
#endif
#ifdef wx_xview

  Textsw textsw = (Textsw) handle;

  Textsw_status status;
  xv_set (textsw, TEXTSW_STATUS, &status, TEXTSW_FILE, file_name, TEXTSW_FIRST, 0, NULL);

  return (status == TEXTSW_STATUS_OKAY);
#endif
}

// If file is null, try saved file name first
// Returns TRUE if succeeds.
Bool wxTextWindow::SaveFile (char *file)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;

  char *the_file = file;
  if (!file)
    the_file = file_name;

  if (the_file)
    return !textsw_store_file (textsw, the_file, 100, 100);
  else
    return FALSE;
#endif
}

void wxTextWindow::WriteText (char *text)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextInsert (textWidget, GetInsertionPoint(), text);
  textPosition += strlen (text);
  XtVaSetValues (textWidget, XmNcursorPosition, textPosition, NULL);
  XmTextShowPosition (textWidget, textPosition);
  textModified = TRUE;
#elif defined(wx_xview)
  Textsw textsw = (Textsw) handle;

  // The old code allocated 1 byte too little for the dup and
  // used it to change \n to 10 in text but \n = 10 so it seems
  // to have been...
  xv_set (textsw, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, NULL);
  textsw_insert (textsw, text, strlen (text));
#else
#error "Not yet..."
#endif
}

void wxTextWindow::SetSize (int x, int y, int w, int h, int sizeFlags)
{
#ifdef wx_motif
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

#endif
#ifdef wx_xview
  int currentX, currentY;
  GetPosition (&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  if (w == -1 || h == -1)
    {
      int ww, hh;
      GetSize (&ww, &hh);
      if (w == -1)
	w = ww;
      if (h == -1)
	h = hh;
    }

  int real_y = y;

  if (window_parent)
    real_y += ((wxFrame *) window_parent)->y_offset;	// Allow for possible menu bar

  Xv_opaque object = (Xv_opaque) handle;

  (void) xv_set (object, XV_X, x, XV_Y, real_y, NULL);
  (void) xv_set (object, XV_WIDTH, w, XV_HEIGHT, h, NULL);
#endif
  sr_width = w;
  sr_height = h;
  GetEventHandler()->OnSize (w, h);
}

void wxTextWindow::SetClientSize (int w, int h)
{
#ifdef wx_motif
  SetSize (-1, -1, w, h);
#else
  wxWindow::SetClientSize (w, h);
#endif
}


void wxTextWindow::Clear (void)
{
#ifdef wx_motif
  XmTextSetString ((Widget) handle, "");
  textPosition = 0;
  textModified = FALSE;
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;

//  textsw_delete(textsw, 0, TEXTSW_INFINITY);
  textsw_reset (textsw, 0, 0);
#endif
}

Bool wxTextWindow::Modified (void)
{
#ifdef wx_motif
  return textModified;
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  return (xv_get (textsw, TEXTSW_MODIFIED));
#endif
}

// Not clear whether Clear is required as well as DiscardEdits
void wxTextWindow::DiscardEdits (void)
{
#ifdef wx_motif
  XmTextSetString ((Widget) handle, "");
  textPosition = 0;
  textModified = FALSE;
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;

  textsw_reset (textsw, 0, 0);
#endif
}

void wxTextWindow::SetEditable (Bool editable)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextSetEditable (textWidget, editable);
#endif
#ifdef wx_xview
  xv_set((Xv_opaque)handle, TEXTSW_READ_ONLY, !editable, NULL);
#endif
}

char *wxTextWindow::GetContents (void)
{
#ifdef wx_motif
  char *s = XmTextGetString ((Widget) handle);
  if (s)
    {
      char *res = copystring (s);
      XtFree (s);
      return res;
    }
  return NULL;
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  xv_set (textsw, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, NULL);
  int last = (int) xv_get (textsw, TEXTSW_INSERTION_POINT);
  char *buf = new char[last];
  xv_get (textsw, TEXTSW_CONTENTS, 0, buf, last);
  buf[last] = 0;

  return buf;
#endif
}

void wxTextWindow::SetInsertionPoint (long pos)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextSetInsertionPosition (textWidget, (XmTextPosition) pos);
  textPosition = pos;
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  xv_set (textsw, TEXTSW_INSERTION_POINT, (Textsw_index) pos, NULL);
#endif
}

void wxTextWindow::SetInsertionPointEnd (void)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextPosition pos = XmTextGetLastPosition (textWidget);
  XmTextSetInsertionPosition (textWidget, (XmTextPosition) (pos + 1));
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  long len = (long) xv_get (textsw, TEXTSW_LENGTH);
  xv_set (textsw, TEXTSW_INSERTION_POINT, (Textsw_index) (len + 1), NULL);
#endif
}

long wxTextWindow::GetInsertionPoint (void)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  return (long) XmTextGetInsertionPosition (textWidget);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  return (long) xv_get (textsw, TEXTSW_INSERTION_POINT);
#endif
}

long wxTextWindow::GetLastPosition (void)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  return (long) XmTextGetLastPosition (textWidget);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  long len = (long) xv_get (textsw, TEXTSW_LENGTH);
  if (len == 0)
    return 0;
  else
    return len - 1;
#endif
}

long wxTextWindow::XYToPosition (long x, long y)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  return (long) XmTextXYToPos (textWidget, (Position) x, (Position) y);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  long pos = (long) textsw_index_for_file_line (textsw, (int) y);
  return pos + x;
#endif
}

void wxTextWindow::PositionToXY (long pos, long *x, long *y)
{
#ifdef wx_motif
  Position xx;
  Position yy;
  Widget textWidget = (Widget)handle;
  XmTextPosToXY(textWidget, pos, &xx, &yy);
  *x = xx;
  *y = yy;
#endif
#ifdef wx_xview
  // NOT POSSIBLE IN XVIEW
  *x = 0;
  *y = 0;
#endif
}

void wxTextWindow::ShowPosition (long pos)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextShowPosition (textWidget, (XmTextPosition) pos);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;

  xv_set (textsw, TEXTSW_FIRST, (Textsw_index) pos, NULL);
#endif
}

int wxTextWindow::GetLineLength (long lineNo)
{
#ifdef wx_motif
  GetLineText (lineNo, wxBuffer);
  return strlen (wxBuffer);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  Textsw_index lineIndex = textsw_index_for_file_line (textsw, (int) lineNo);
  if (lineIndex > -1)		// What's the error code??

    {
      char buf[2];
      int i = 0;

      for (;;)
	{
	  Textsw_index nextPos = xv_get (textsw, TEXTSW_CONTENTS, (Textsw_index) (lineIndex + i), buf, 1);
	  if (buf[0] == '\n' || buf[0] == 0 || (nextPos != lineIndex + i + 1))
	    break;
	  i++;
	}			// for(;;)

      return i;
    }
  // Error
  return 0;
#endif
}

int wxTextWindow::GetNumberOfLines (void)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  int noLines = 0;
  char buf[2];
  int i = 0;

  for (;;)
    {
      Textsw_index nextPos = xv_get (textsw, TEXTSW_CONTENTS, (Textsw_index) i, buf, 1);
      if (buf[0] == '\n')
	noLines++;
      else if (buf[0] == 0 || (nextPos != i + 1))
	break;
      i++;
    }				// for(;;)

  return noLines;
#endif
}

int wxTextWindow::GetLineText (long lineNo, char *buf)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  Textsw_index lineIndex = textsw_index_for_file_line (textsw, (int) lineNo);
  if (lineIndex > -1)		// What's the error code??

    {
      int i = 0;
      char buf2[2];
      for (;;)
	{
	  Textsw_index nextPos = (Textsw_index) xv_get (textsw, TEXTSW_CONTENTS, (Textsw_index) (lineIndex + i), buf2, 1);
	  if (buf2[0] == '\n' || buf2[0] == 0 || (nextPos != lineIndex + i + 1))
	    {
	      buf[i] = '\0';
	      break;
	    }
	  else
	    buf[i++] = buf2[0];
	}			// for(;;)

      return i;
    }
  // Error
  return 0;
#endif
}

void wxTextWindow::Replace (long from, long to, char *value)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextReplace (textWidget, (XmTextPosition) from, (XmTextPosition) to,
		 value);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  textsw_replace_bytes (textsw, from, to + 1, value, strlen (value));
#endif
}

void wxTextWindow::Remove (long from, long to)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextSetSelection (textWidget, (XmTextPosition) from, (XmTextPosition) to,
		      (Time) 0);
  XmTextRemove (textWidget);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  textsw_delete (textsw, from, to + 1);
#endif
}

void wxTextWindow::SetSelection(long from, long to)
{
#ifdef wx_motif
  Widget textWidget = (Widget) handle;
  XmTextSetSelection (textWidget, (XmTextPosition) from, (XmTextPosition) to,
		      (Time) 0);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw) handle;
  textsw_set_selection (textsw, from, to + 1, 1);
#endif
}

// Copy selection to clipboard
void wxTextWindow::Copy(void)
{
#ifdef wx_motif
  XmTextCopy((Widget)handle, CurrentTime);
#endif
}

// Paste clipboard into text window
void wxTextWindow::Paste(void)
{
#ifdef wx_motif
  XmTextPaste((Widget)handle);
#endif
}

// Copy selection to clipboard, then remove selection.
void wxTextWindow::Cut(void)
{
#ifdef wx_motif
  XmTextCut((Widget)handle, CurrentTime);
#endif
}


#ifdef wx_xview
void wxTextWindow::DragAcceptFiles (Bool accept)
{

/* NO CUSTOM DRAG AND DROP IN TEXT SUBWINDOW! UGH!
   if (accept)
   {
   if (dropSite) xv_destroy(dropSite);

   Xv_Window pw = xv_get((Textsw)handle, TEXTSW_FIRST_PAINT_WINDOW, NULL);
   dropSite = xv_create(pw, DROP_SITE_ITEM,
   DROP_SITE_ID, NewId(),
   DROP_SITE_REGION, xv_get(pw, WIN_RECT),
   DROP_SITE_EVENT_MASK, DND_ENTERLEAVE,
   NULL);
   }
   else if (dropSite) xv_destroy(dropSite);
 */
}
#endif

#ifdef wx_motif
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

    if (!tw->CallPreOnChar(tw, &event))
      tw->GetEventHandler()->OnChar(event);

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

  if (!tw->CallPreOnChar(tw, &event))
    tw->GetEventHandler()->OnChar(event);

  tw->tempCallbackStruct = NULL;
}

static void 
wxTextWindowGainFocusProc (Widget w, XtPointer clientData, XmAnyCallbackStruct *cbs)
{
  if (!wxWidgetHashTable->Get ((long) w))
    return;

  wxTextWindow *tw = (wxTextWindow *) clientData;
  tw->GetEventHandler()->OnSetFocus();
}

static void 
wxTextWindowLoseFocusProc (Widget w, XtPointer clientData, XmAnyCallbackStruct *cbs)
{
  if (!wxWidgetHashTable->Get ((long) w))
    return;

  wxTextWindow *tw = (wxTextWindow *) clientData;
  tw->GetEventHandler()->OnKillFocus();
}

#endif

void wxTextWindow::OnChar(wxKeyEvent& event)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
#endif
}

/*
 * XView notification proc
 */

#ifdef wx_xview
void wxTextWindowNotifyProc(Textsw textsw, Attr_avlist avlist)
{
//  wxTextWindow *tw = (wxTextWindow *)xv_get(textsw, WIN_CLIENT_DATA);

//  cout << "Got a notification.\n";

  // Nothing this proc can do: no character input is
  // routed through here!! So no OnChar for XView.
  wxTextWindowDefaultNotifyProc(textsw, avlist);
}
#endif
