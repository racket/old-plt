/*								-*- C++ -*-
 * $Id: BusyCursor.cc,v 1.1 1996/01/10 14:56:49 markus Exp $
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

void wxXSetBusyCursor(wxWindow *win, wxCursor *cursor)
{
  if (cursor)
    XtVaSetValues(win->X->handle, XtNcursor, GETCURSOR(cursor), NULL);
  else if (win->cursor)
    XtVaSetValues(win->X->handle, XtNcursor, GETCURSOR(win->cursor), NULL);
  else
    XtVaSetValues(win->X->handle, XtNcursor, GETCURSOR(wxSTANDARD_CURSOR), 
		  NULL);

  for(wxChildNode *node = win->GetChildren()->First(); node; node = node->Next()) {
    wxWindow *child = (wxWindow *) node->Data ();
    if (wxSubType(child->__type, wxTYPE_FRAME) ||
	wxSubType(child->__type, wxTYPE_CANVAS) ||
	wxSubType(child->__type, wxTYPE_PANEL) ||
	wxSubType(child->__type, wxTYPE_TEXT_WINDOW))
      wxXSetBusyCursor(child, cursor);
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
