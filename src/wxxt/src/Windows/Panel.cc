/*								-*- C++ -*-
 * $Id: Panel.cc,v 1.2 1999/03/09 14:17:52 mflatt Exp $
 *
 * Purpose: base class for all panels
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
#pragma implementation "Panel.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxButton
#define  Uses_wxPanel
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_BoardWidget
#define  Uses_EnforcerWidget
#include "widgets.h"

#define REPORT_ZERO_WIDTH_FLAG 0x20
#define REPORT_ZERO_HEIGHT_FLAG 0x40

//-----------------------------------------------------------------------------
// wxPanel create and destroy
//-----------------------------------------------------------------------------

wxPanel::wxPanel(void) : wxWindow()
{
    __type = wxTYPE_PANEL;

    default_item = NULL;
    label_font = wxSYSTEM_FONT;
    button_colour = label_colour = wxBLACK;
    label_pos = wxHORIZONTAL;
    cursor_x = PANEL_HMARGIN;
    cursor_y = PANEL_VMARGIN;
    h_space = PANEL_HSPACING;
    v_space = PANEL_VSPACING;
    v_line_extent = 0;
}

wxPanel::wxPanel(wxPanel *panel, int x, int y, int width, int height,
		 int style, char *name) : wxWindow()
{
    __type = wxTYPE_PANEL;

    default_item = NULL;
    label_font = wxSYSTEM_FONT;
    button_colour = label_colour = wxBLACK;
    label_pos = wxHORIZONTAL;
    cursor_x = PANEL_HMARGIN;
    cursor_y = PANEL_VMARGIN;
    h_space = PANEL_HSPACING;
    v_space = PANEL_VSPACING;
    v_line_extent = 0;
    Create(panel, x, y, width, height, style, name);
}

Bool wxPanel::Create(wxPanel *panel, int x, int y, int width, int height,
		     int _style, char *name)
{
    if (!panel)
	wxFatalError("wxPanel created without a parent!");
    parent = panel;
    parent->AddChild(this);

    style         = _style;
    button_colour = panel->GetButtonColour();
    label_colour  = panel->GetLabelColour();
    label_font    = panel->GetLabelFont();
    font          = panel->font;
    fg            = panel->fg;
    bg            = panel->bg;

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  fg->GetPixel(cmap),
	 XtNfont,        font->GetInternalFont(),
	 XtNhighlightThickness, 0,
	 NULL);
    // internal representation
    if (style & wxBORDER) {
	X->handle = XtVaCreateManagedWidget
	    ("panel", xfwfBoardWidgetClass, X->frame,
	     XtNbackground, bg->GetPixel(cmap),
	     XtNframeWidth, 2,
	     XtNframeType, XfwfSunken,
	     NULL);
	xoff = yoff = 4; // offset for border
    } else {
	X->handle = XtVaCreateManagedWidget
	    ("panel", xfwfBoardWidgetClass, X->frame,
	     XtNbackground, bg->GetPixel(cmap),
	     XtNhighlightThickness, 0,
	     NULL);
    }

    // Delay until it's really needed:
#if 0
    // Initialize PanelDC
    if (!(style & wxNO_DC)) {
	CreateDC();
    }
#endif

    // position in panel/frame
    panel->PositionItem(this, x, y, width, height);
    // add event handlers
    AddEventHandlers();
    // ready
    return TRUE;
}

//-----------------------------------------------------------------------------
// if setting own background, change colours of item labels
//-----------------------------------------------------------------------------

void wxPanel::SetBackgroundColour(wxColour *col)
{
    // inherit from window
    wxWindow::SetBackgroundColour(col);
    // change children's label background colours
    for (wxChildNode *node = children->First(); node; node = node->Next()) {
	wxWindow *child  = (wxWindow*)node->Data();
	if (wxSubType(child->__type, wxTYPE_ITEM))
	    child->ChangeColours();
    }
}

//-----------------------------------------------------------------------------
// resize/layout panel
//-----------------------------------------------------------------------------

void wxPanel::GetClientSize(int *width, int *height)
{
    Position  xx, yy;
    int ww, hh;

    XfwfCallComputeInside(X->handle, &xx, &yy, &ww, &hh);
    xoff = xx; yoff = yy;
    *width = ww; *height = hh;

    
    if (misc_flags & REPORT_ZERO_WIDTH_FLAG)
      *width = 0;
    if (misc_flags & REPORT_ZERO_HEIGHT_FLAG)
      *height = 0;
}

void wxPanel::Fit(void)
{
    int hsize=0, vsize=0;

    if (children) {
	for (wxChildNode *node = children->First(); node; node = node->Next()) {
	    wxWindow *child = (wxWindow*)(node->Data());
	    if (child) {
		int x, y, w, h;
		child->GetPosition(&x, &y); child->GetSize(&w, &h);
		hsize = max(hsize, x + w);
		vsize = max(vsize, y + h);
	    }
	}
    } else {
	hsize = PANEL_HMARGIN;
	vsize = PANEL_VMARGIN;
    }
    hsize += PANEL_HMARGIN + (style & wxBORDER ? 4 : 0 );
    vsize += PANEL_VMARGIN + (style & wxBORDER ? 4 : 0 );
    SetClientSize(hsize, vsize);
}

// void wxPanel::Layout(void)
// --> wxLayout.cc

void wxPanel::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  wxChildNode *cn;
  for (cn = GetChildren()->First(); cn; cn = cn->Next()) {
    wxWindow *w = (wxWindow *)cn->Data();
    w->InternalEnable(!gray, TRUE);
  }
}

//-----------------------------------------------------------------------------
// positioning of items
//-----------------------------------------------------------------------------

void wxPanel::GetCursor(int *x, int *y)
{
    *x = cursor_x; *y = cursor_y;
}

void wxPanel::SetItemCursor(int x, int y)
{
    cursor_x = x; cursor_y = y;
}

void wxPanel::NewLine(int pixels)
{
    cursor_x = PANEL_HMARGIN;
    cursor_y += v_line_extent + v_space + pixels;
    v_line_extent = 0;
}

void wxPanel::PositionItem(wxWindow *item, int x, int y, int width, int height)
{
    // position child
    item->Move((x<0 ? cursor_x : x), (y<0 ? cursor_y : y));
    // move cursor and compute height of line
    item->SetSize(width, height);
    item->GetSize(&width, &height);
    if (x < 0)	cursor_x += width + h_space;
    if (y < 0) v_line_extent = max(v_line_extent, height);

    /* MATTHEW: Non-neg also sets cursor */
    if (x > 0 && (x + width > cursor_x))
      cursor_x = x + width + h_space;
    if (y > 0 && (y > cursor_y))
      cursor_y = y;

    if (IsGray())
      item->InternalEnable(0, TRUE);
}

void wxPanel::Tab(int pixels)
{
    cursor_x += ( pixels ? pixels : h_space);
}

//-----------------------------------------------------------------------------
// virtual event functions
//-----------------------------------------------------------------------------

void wxPanel::OnDefaultAction(wxItem *WXUNUSED(item))
{
    wxButton *but = GetDefaultItem();
    if (but) {
	wxCommandEvent event(wxEVENT_TYPE_BUTTON_COMMAND);
	but->Command(event);
    }
}

//-----------------------------------------------------------------------------
// compatibility, I like the typesave constructor more
//-----------------------------------------------------------------------------

wxPanel::wxPanel(wxWindow *panel, int x, int y, int width, int height,
		 int style, char *name) : wxWindow()
{
    __type = wxTYPE_PANEL;

    default_item = NULL;
    label_font = wxSYSTEM_FONT;
    button_colour = label_colour = wxBLACK;
    label_pos = wxHORIZONTAL;
    cursor_x = PANEL_HMARGIN;
    cursor_y = PANEL_VMARGIN;
    h_space = PANEL_HSPACING;
    v_space = PANEL_VSPACING;
    v_line_extent = 0;
    Create(panel, x, y, width, height, style, name);
}

Bool wxPanel::Create(wxWindow *panel, int x, int y, int width, int height,
		     int style, char *name)
{
    if (!panel)
	wxFatalError("wxPanel created without a parent!");
    if (!wxSubType(__type, wxTYPE_PANEL))
	wxFatalError("parent has to be a wxFrame, wxPanel, or any subtype", "wxPanel");

    return Create((wxPanel*)panel, x, y, width, height, style, name);
}
