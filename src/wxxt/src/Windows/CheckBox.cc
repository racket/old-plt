/*								-*- C++ -*-
 * $Id: CheckBox.cc,v 1.5 1999/11/04 17:25:37 mflatt Exp $
 *
 * Purpose: check box panel item
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
#pragma implementation "CheckBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxCheckBox
#include "wx.h"
#define  Uses_TraversingEnforcerWidget
#define  Uses_ToggleWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy button
//-----------------------------------------------------------------------------

wxCheckBox::wxCheckBox(void) : wxItem()
{
    __type = wxTYPE_CHECK_BOX;
}

wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction function, char *label,
		       int x, int y, int width, int height,
		       long style, char *name) : wxItem()
{
    __type = wxTYPE_CHECK_BOX;

    Create(panel, function, label, x, y, width, height, style, name);
}

wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction function, wxBitmap *bitmap,
		       int x, int y, int width, int height,
		       long style, char *name) : wxItem()
{
    __type = wxTYPE_CHECK_BOX;

    Create(panel, function, bitmap, x, y, width, height, style, name);
}

Bool wxCheckBox::Create(wxPanel *panel, wxFunction function, char *label,
			int x, int y, int width, int height,
			long style, char *name)
{
    ChainToPanel(panel, style, name);

    // label = wxGetCtlLabel(label);
    bm_label = NULL;

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfTraversingEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNshrinkToFit, (width < 0 || height < 0),
	 NULL);
    // create widget
    X->handle = XtVaCreateManagedWidget
	("checkbox", xfwfToggleWidgetClass, X->frame,
	 XtNlabel,       label,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  fg->GetPixel(cmap),
	 XtNfont,        font->GetInternalFont(),
	 XtNshrinkToFit, (width < 0 || height < 0),
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    // propagate key events from frame to checkbox widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);
    // set data declared in wxItem
    callback = function;
    XtAddCallback(X->handle, XtNonCallback,  wxCheckBox::OnEventCallback,
		  (XtPointer)this);
    XtAddCallback(X->handle, XtNoffCallback, wxCheckBox::OnEventCallback,
		  (XtPointer)this);

    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    return TRUE;
}

Bool wxCheckBox::Create(wxPanel *panel, wxFunction function, wxBitmap *bitmap,
			int x, int y, int width, int height,
			long style, char *name)
{
    if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
      return Create(panel, function, "<bad-image>", x, y, width, height, style, name);

    bitmap->selectedIntoDC++;
    bm_label = bitmap;

    ChainToPanel(panel, style, name);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfTraversingEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNshrinkToFit, (width < 0 || height < 0),
	 NULL);
    // create widget
    X->handle = XtVaCreateManagedWidget
	("checkbox", xfwfToggleWidgetClass, X->frame,
	 XtNpixmap,      GETPIXMAP(bitmap),
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  fg->GetPixel(cmap),
	 XtNfont,        font->GetInternalFont(),
	 XtNshrinkToFit, (width < 0 || height < 0),
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    // propagate key events from frame to checkbox widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);
    // set data declared in wxItem
    callback = function;
    XtAddCallback(X->handle, XtNonCallback,  wxCheckBox::OnEventCallback,
		  (XtPointer)this);
    XtAddCallback(X->handle, XtNoffCallback, wxCheckBox::OnEventCallback,
		  (XtPointer)this);

    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    return TRUE;
}

 wxCheckBox::~wxCheckBox()
{
  if (bm_label) {
    --bm_label->selectedIntoDC;
    XtVaSetValues(X->handle, XtNpixmap, NULL, NULL);
  }
}

//-----------------------------------------------------------------------------
// alternate SetLabel for changing bitmap
//-----------------------------------------------------------------------------

void wxCheckBox::SetLabel(char *label)
{
  if (!bm_label) {
    label = wxGetCtlLabel(label);
    
    XtVaSetValues(X->handle, XtNlabel, label, NULL);
  }
}

void wxCheckBox::SetLabel(wxBitmap *bitmap)
{
  if (bm_label && bitmap && bitmap->Ok() && (bitmap->selectedIntoDC >= 0)
      && (bitmap->GetDepth()==1 || bitmap->GetDepth()==wxDisplayDepth())) {
    --bm_label->selectedIntoDC;
    bm_label = bitmap;
    bm_label->selectedIntoDC++;
    XtVaSetValues(X->handle, XtNpixmap, GETPIXMAP(bitmap), NULL);
  }
}


char *wxCheckBox::GetLabel(void)
{
    if (!X->handle) // forbid, if no widget associated
	return NULL;

    char *label = NULL;
    XtVaGetValues(X->handle, XtNlabel, &label, NULL);
    return label;
}

//-----------------------------------------------------------------------------
// wxCheckBox: set & query state
//-----------------------------------------------------------------------------

Bool wxCheckBox::GetValue(void)
{
    Boolean state;
    XtVaGetValues(X->handle, XtNon, &state, NULL);
    return state;
}

void wxCheckBox::SetValue(Bool state)
{
    XtVaSetValues(X->handle, XtNon, Boolean(state), NULL);
}

void wxCheckBox::Command(wxCommandEvent *event)
{
  ProcessCommand (event);
}

//-----------------------------------------------------------------------------
// callbacks for xfwfToggleWidgetClass
//-----------------------------------------------------------------------------

void wxCheckBox::OnEventCallback(Widget WXUNUSED(w),
				 XtPointer clientData, XtPointer WXUNUSED(ptr))
{
    wxCheckBox     *checkbox = (wxCheckBox*)clientData;
    wxCommandEvent *event;

    event = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);

    checkbox->ProcessCommand(event);
}
