/*								-*- C++ -*-
 * $Id: BusyCursor.cc,v 1.1.1.1 1997/12/22 17:28:56 mflatt Exp $
 *
 * Purpose: busy cursor
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define  Uses_XLib
#define  Uses_XtIntrinsic
#define  Uses_wxCursor
#define  Uses_wxWindow
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_SimpleWidget
#include "widgets.h"

extern int wxGetBusyState();
extern void wxSetBusyState(int);

static void wxXSetNoCursor(wxWindow *win, wxCursor *cursor)
{
  Cursor c;

  if (cursor || !win->cursor)
    c = None;
  else
    c = GETCURSOR(win->cursor);

  XtVaSetValues(win->X->handle, XtNcursor, c, NULL);
  
  for(wxChildNode *node = win->GetChildren()->First(); node; node = node->Next()) {
    wxWindow *child = (wxWindow *) node->Data ();
    wxXSetNoCursor(child, cursor);
  }
}

void wxXSetBusyCursor(wxWindow *win, wxCursor *cursor)
{
  Cursor c;

  if (cursor)
    c = GETCURSOR(cursor);
  else if (win->cursor)
    c = GETCURSOR(win->cursor);
  else
    c = GETCURSOR(wxSTANDARD_CURSOR);
  
  XtVaSetValues(win->X->handle, XtNcursor, c, NULL);

  for(wxChildNode *node = win->GetChildren()->First(); node; node = node->Next()) {
    wxWindow *child = (wxWindow *) node->Data ();
    if (wxSubType(child->__type, wxTYPE_FRAME))
      wxXSetBusyCursor(child, cursor);
    else
      wxXSetNoCursor(child, cursor);
  }
}

void wxBeginBusyCursor(wxCursor * cursor)
{
  wxCursorBusy = wxGetBusyState();
  wxCursorBusy++;
  wxSetBusyState(wxCursorBusy);

  if (wxCursorBusy == 1)
    for(wxChildNode *node = wxTopLevelFrames(NULL)->First(); node; node = node->Next()) {
      wxWindow *win = (wxWindow *)node->Data();
      if (win && node->IsShown())
	wxXSetBusyCursor(win, cursor);
    }

  XFlush(wxAPP_DISPLAY);
}

// Restore cursor to normal
void 
wxEndBusyCursor (void)
{
  wxCursorBusy = wxGetBusyState();
  if (wxCursorBusy == 0)
    return;
  wxCursorBusy--;
  wxSetBusyState(wxCursorBusy);

  if (wxCursorBusy == 0)
    for(wxChildNode *node = wxTopLevelFrames(NULL)->First(); node; node = node->Next()) {
      wxWindow *win = (wxWindow *)node->Data();
      if (win && node->IsShown())
	wxXSetBusyCursor(win, NULL);
    }

  XFlush(wxAPP_DISPLAY);
}

Bool wxIsBusy (void)
{
  return !!wxGetBusyState();
}
