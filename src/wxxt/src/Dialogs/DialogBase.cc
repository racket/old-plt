/*								-*- C++ -*-
 * $Id: DialogBase.cc,v 1.2 1996/01/11 10:26:54 markus Exp $
 *
 * Purpose: dialog function box (used for common dialogs)
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
#pragma implementation "DialogBase.h"
#endif

#define  Uses_wxButton
#define  Uses_wxDialogBase
#define  Uses_wxMessage
#define  Uses_wxTypeTree
#include "wx.h"

//-----------------------------------------------------------------------------
// implemetation of wxDialogBase
//-----------------------------------------------------------------------------

IMPLEMENT_CLASS(wxDialogBase, wxDialogBox)

wxDialogBase::wxDialogBase(char *message, char *caption, int style, wxWindow *parent,
			   int _x, int _y)
{
    yes = no = ok = cancel = NULL;
    dlg_style = style;
    x = _x; y = _y;
    msg = NULL; buttons = NULL;

    // busy cursor
    wxBeginBusyCursor();
    // find a parent, that is a frame => centre works better and it is more
    wxWindow *p = parent;
    for (/*wxWindow *p = parent*/; p; p = p->GetParent())
	if (wxSubType(p->__type, wxTYPE_FRAME))
	    break;
    // create dialogbox (p is now NULL or a frame)
    Create((wxFrame*)p, caption, TRUE, -1, -1, -1, -1,
	   wxDEFAULT_DIALOG_STYLE, "dialogBase");
    // create message
    msg = DEBUG_NEW wxMessage(this, message);
    if (dlg_style & wxCENTRE)
	msg->SetAlignment(wxALIGN_CENTRE);
    this->NewLine();
}

int wxDialogBase::GetInput(void)
{
    this->NewLine();

    // create buttons
    buttons = DEBUG_NEW wxPanel(this, -1, -1, -1, -1, wxNO_DC, "button_panel");

    if (dlg_style & wxYES_NO) {
	yes = DEBUG_NEW wxButton(buttons, NULL, "Yes");
	no  = DEBUG_NEW wxButton(buttons, NULL, "No");
    } else if (dlg_style & wxOK) {
	ok = DEBUG_NEW wxButton(buttons, NULL, "Ok");
    }
    if (dlg_style & wxCANCEL) {
	cancel = DEBUG_NEW wxButton(buttons, NULL, "Cancel");
    }
    buttons->Fit();

    // resize dialog box and position message and buttons properly
    Fit();

    // position dialogbox on screen
    Move(x, y);
    if (x < 0)
	Centre(wxHORIZONTAL);
    if (y < 0)
	Centre(wxVERTICAL);

    // un-busy cursor
    wxEndBusyCursor();
    // show modal dialog box and wait for input
    Show(TRUE);

    return pressed_button;
}

Bool wxDialogBase::OnClose(void)
{
    pressed_button = wxCANCEL;
    Show(FALSE);

    // forbid destruction of dialog box, Show(FALSE) exits modal event loop
    return FALSE;
}

void wxDialogBase::OnCommand(wxWindow &win, wxCommandEvent &event)
{
    switch (event.eventType) {
    case wxEVENT_TYPE_BUTTON_COMMAND:
	if      (&win == yes)  pressed_button = wxYES;
	else if (&win == no)   pressed_button = wxNO;
	else if (&win == ok)   pressed_button = wxOK;
	else                   pressed_button = wxCANCEL;
	break;
    case wxEVENT_TYPE_TEXT_ENTER_COMMAND:
    case wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND:
	pressed_button = wxOK;
	break;
    default:
	return;
    }
    Show(FALSE);
}

void wxDialogBase::OnSize(int width, int height)
{
    wxDialogBox::OnSize(width, height);

    if (msg && buttons) {
	if ( dlg_style & wxCENTRE )
	    msg->Centre(wxHORIZONTAL);
	buttons->Centre(wxHORIZONTAL);
    }
}
