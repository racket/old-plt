/*								-*- C++ -*-
 * $Id: DialogBox.cc,v 1.1 1996/01/10 14:57:07 markus Exp $
 *
 * Purpose: dialog box
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

#ifdef __GNUG__
#pragma implementation "DialogBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxDialogBox
#define  Uses_wxList
#define  Uses_wxTypeTree
#include "wx.h"

IMPLEMENT_DYNAMIC_CLASS(wxDialogBox, wxFrame)

wxDialogBox::wxDialogBox(void) : wxFrame()
{
    __type = wxTYPE_DIALOG_BOX;

    modal = modal_showing = FALSE;
}

wxDialogBox::wxDialogBox(wxFrame *parent, char *title, Bool _modal, int x,
			 int y, int width, int height, long style, char *name)
    : wxFrame()
{
    __type = wxTYPE_DIALOG_BOX;

    modal = modal_showing = FALSE;

    Create(parent, title, _modal, x, y, width, height, style, name);
}

Bool wxDialogBox::Create(wxFrame *parent, char *title, Bool _modal, int x,
			 int y, int width, int height, long style, char *name)
{
    (void)wxFrame::Create(parent, title, x, y, width, height, style, name);

    modal = _modal; /* MATTHEW */

    return TRUE;
}

extern void wxDispatchEventsUntil(int (*)(void *), void *);

static int CheckDialogShowing(void *data)
{
  return !((wxDialogBox *)data)->ModalShowing();
}

Bool wxDialogBox::Show(Bool show)
{
    if (show == IsShown()) { // do nothing if state doesn't change
      if (show) /* Well, do move it to the front... */
	wxFrame::Show(show);
      return TRUE;
    }

    // popup/popdown frame
    wxFrame::Show(show);
    SetShown(show);

    // handle modality
    if (show) {
	if (modal) {
	    if (modal_showing)
		return TRUE;

	    modal_showing = TRUE;

#if 1
	    wxWindow *saveModal;
	    saveModal = wxGetModalWindow(this);
	    wxPutModalWindow(this, this);

	    wxList disabled_windows;
	    wxNode *node;
	    wxChildNode *cnode;

	    for (cnode = wxTopLevelFrames(this)->First(); cnode; cnode = cnode->Next()) {
	      wxWindow *w = (wxWindow *)cnode->Data();
	      if (w && w != this && cnode->IsShown()) {
		disabled_windows.Append(w);
		w->InternalEnable(FALSE);
	      }
	    }

	    wxDispatchEventsUntil(CheckDialogShowing, (void *)this);

	    wxPutModalWindow(this, saveModal);

	    for (node = disabled_windows.First(); node; node = node->Next()) {
	      wxWindow *w = (wxWindow *)node->Data();
	      w->InternalEnable(TRUE);
	    }
#else
	    wxModalShowingStack.Insert((wxObject*)TRUE);
	    wxModalFrames.Insert(this);

	    XtAddGrab(X->frame, TRUE, FALSE);

	    while ((wxModalShowingStack.Number() > 0)
	    && (Bool)wxModalShowingStack.First()->Data()) {
		XEvent event;
		XtAppNextEvent(wxAPP_CONTEXT, &event);
		XtDispatchEvent(&event);
		wxDoNextEvent();
	    }
	    // Remove modal dialog flag from stack
	    wxNode *node = wxModalShowingStack.First();
	    if (node) {
		delete node;
		delete wxModalFrames.First();
	    }
#endif
	} else
	    modal_showing = FALSE;
    } else {
#if 0
	if (modal_showing)
	    XtRemoveGrab(X->frame);
#endif

	modal_showing = FALSE;

#if 0
	wxNode *node = wxModalShowingStack.First();
	if (node)
	    node->SetData((wxObject *)FALSE);
#endif
	XFlush(XtDisplay(wxAPP_TOPLEVEL));
	XSync(XtDisplay(wxAPP_TOPLEVEL), FALSE);
    }
#if 0
    // Now process all events, to ensure widget destruction
    wxFlushEvents();
#endif

    return TRUE;
}

//-----------------------------------------------------------------------------
// compatibility, I like the typesave constructor more
//-----------------------------------------------------------------------------

wxDialogBox::wxDialogBox(wxWindow *parent, char *title, Bool modal, int x,
			 int y, int width, int height, long style, char *name)
    : wxFrame()
{
    __type = wxTYPE_DIALOG_BOX;

    /* MATTHEW: */
    modal_showing = FALSE;

    Create(parent, title, modal, x, y, width, height, style, name);
}

Bool wxDialogBox::Create(wxWindow *parent, char *title, Bool modal, int x,
			 int y, int width, int height, long style, char *name)
{
    if (!wxSubType(__type, wxTYPE_FRAME))
	wxFatalError("parent has to be a wxFrame or any subtype", "wxDialogBox");

    return Create((wxFrame*)parent, title, modal, x, y, width, height, style, name);
}
