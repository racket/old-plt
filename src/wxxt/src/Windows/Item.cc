/*								-*- C++ -*-
 * $Id: Item.cc,v 1.2 1998/09/06 01:54:02 mflatt Exp $
 *
 * Purpose: base class for all panel items
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
#pragma implementation "Item.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxItem
#define  Uses_wxPanel
#include "wx.h"
#include "widgets.h" // for <X11/StringDefs.h>

//-----------------------------------------------------------------------------
// wxItem constructor
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxItem, wxWindow)

wxItem::wxItem(void) : wxWindow()
{ 
    __type = wxTYPE_ITEM;
    label_fg = wxBLACK;
    label_font = wxSYSTEM_FONT;
    callback = NULL;
};

//-----------------------------------------------------------------------------
// get and set colours
//-----------------------------------------------------------------------------

void wxItem::ChainToPanel(wxPanel *panel, long _style, char *name)
{
    if (!panel)
	wxFatalError("created without a panel!", name ? name : "item");
    parent = panel;
    parent->AddChild(this);

    style       = _style;
    fg         = panel->GetButtonColour();
    bg         = panel->GetBackgroundColour();
    font       = panel->GetButtonFont();
    label_fg   = panel->GetLabelColour();
    label_font = panel->GetLabelFont();
}

void wxItem::ChangeColours(void)
{
    if (X->frame && X->frame != X->handle) {
	if (parent->GetBackgroundColour())
	    XtVaSetValues(X->frame, XtNbackground,
			  parent->GetBackgroundColour()->GetPixel(cmap), NULL);
	if (label_fg)
	    XtVaSetValues(X->frame, XtNforeground,
			  label_fg->GetPixel(cmap), NULL);
    }
    if (X->handle) {
	if (bg)
	    XtVaSetValues(X->handle, XtNbackground,
			  bg->GetPixel(cmap), NULL);
	if (fg)
	    XtVaSetValues(X->handle, XtNforeground,
			  fg->GetPixel(cmap), NULL);
    }
}

#if 0
void wxItem::SetLabelColour(wxColour *col)
{
    label_fg = col; ChangeColours();
}
#endif

//-----------------------------------------------------------------------------
// functions to execute item
//-----------------------------------------------------------------------------

void wxItem::Command(wxCommandEvent &WXUNUSED(event))
{
}

void wxItem::ProcessCommand(wxCommandEvent &event)
{
    if (callback) {
	(void)(*callback)(*this, event);
    } else {
	if (parent)
	    parent->GetEventHandler()->OnCommand(*this, event);
    }
}

char *wxGetCtlLabel(char *label)
{
  if (!label)
    return NULL;

#if 0
  wxGetLabelAndKey(label, &label, NULL);
#endif

  return label;
}
