/*								-*- C++ -*-
 * $Id: Slider.cc,v 1.4 1998/08/15 15:05:48 mflatt Exp $
 *
 * Purpose: slider panel item
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
#pragma implementation "Slider.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxSlider
#include "wx.h"
#define  Uses_TraversingEnforcerWidget
#define  Uses_SliderWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy button
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxSlider, wxItem)

wxSlider::wxSlider(void) : wxItem()
{
    __type = wxTYPE_SLIDER;

    minimum = maximum = value = 0;
}

wxSlider::wxSlider(wxPanel *panel, wxFunction func, char *label,
		   int _value, int min_value, int max_value, int width,
		   int x, int y, long style, char *name)
{
    __type = wxTYPE_SLIDER;

    minimum = maximum = value = 0;

    Create(panel, func, label, _value, min_value, max_value, width, 
	   x, y, style, name);
}

Bool wxSlider::Create(wxPanel *panel, wxFunction func, char *label,
		      int init_value, int min_value, int max_value, int length,
		      int x, int y, long style, char *name)
{
    ChainToPanel(panel, style, name);

    Bool vert = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    minimum = min_value;
    maximum = max_value;

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfTraversingEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNframeType,   XfwfSunken,
	 XtNframeWidth,  2,
	 XtNshrinkToFit, TRUE,
	 NULL);
    // compute sizes of the slider widget
    float swidth, sheight; char tempstring[80];
    sprintf(tempstring, "-%d", max(abs(max_value), abs(min_value)));
    GetTextExtent(tempstring, &swidth, &sheight);
    swidth += 8; sheight += 8; // shadows and margin
    // create the slider widget
    X->handle = XtVaCreateManagedWidget
	("slider", xfwfSlider2WidgetClass, X->frame,
	 XtNbackground,    bg->GetPixel(cmap),
	 XtNforeground,    fg->GetPixel(cmap),
	 XtNthumbColor,    bg->GetPixel(cmap),
	 XtNfont,          font->GetInternalFont(),
	 XtNwidth,         style & wxVERTICAL ? int(swidth) : length,
	 XtNheight,        style & wxVERTICAL ? length : int(sheight),
	 XtNframeType,     XfwfRaised,
	 XtNframeWidth,    0,
	 XtNhighlightThickness, 0,
	 NULL);
    if (style & wxVERTICAL) {
	XfwfResizeThumb(X->handle, 1.0, min(0.9,sheight/length));
    } else {
	XfwfResizeThumb(X->handle, min(0.9, swidth/length), 1.0);
    }
    SetValue(init_value);
    // set data declared in wxItem
    callback = func;
    XtAddCallback(X->handle, XtNscrollCallback, wxSlider::EventCallback,
		  (XtPointer)this);
    // panel positioning
    panel->PositionItem(this, x, y, -1, -1);
    AddEventHandlers();

    return TRUE;
}

void wxSlider::OnSize(int width, int height)
{
  float swidth, sheight; char tempstring[80];
  Dimension length;
  sprintf(tempstring, "-%d", max(abs(maximum), abs(minimum)));
  GetTextExtent(tempstring, &swidth, &sheight);
  swidth += 8; sheight += 8; // shadows and margin
  if (style & wxVERTICAL) {
    XtVaGetValues(X->handle, XtNheight, &length, NULL);
    if (length > height) length = height;
    XfwfResizeThumb(X->handle, 1.0, min(0.9,sheight/length));
  } else {
    XtVaGetValues(X->handle, XtNwidth, &length, NULL);
    if (length > width) length = width;
    XfwfResizeThumb(X->handle, min(0.9, swidth/length), 1.0);
  }

  wxItem::OnSize(width, height);
}

//-----------------------------------------------------------------------------
// methods to access internal data
//-----------------------------------------------------------------------------

void wxSlider::SetValue(int new_value)
{
    if (minimum <= new_value && new_value <= maximum) {
	value = new_value;
	char tempstring[80];
	sprintf(tempstring, "%d", value);
	XtVaSetValues(X->handle, XtNlabel, tempstring, NULL);
	if (style & wxVERTICAL)
	    XfwfMoveThumb(X->handle,
			  0.0, float(value-minimum)/float(maximum-minimum));
	else
	    XfwfMoveThumb(X->handle,
			  float(value-minimum)/float(maximum-minimum), 0.0);
    }
}

void wxSlider::Command(wxCommandEvent &event)
{
  ProcessCommand(event);
}

//-----------------------------------------------------------------------------
// callbacks for xfwfGroupWidgetClass
//-----------------------------------------------------------------------------

void wxSlider::EventCallback(Widget WXUNUSED(w),
			     XtPointer dclient, XtPointer dcall)
{
    wxSlider       *slider = (wxSlider*)dclient;
    XfwfScrollInfo *info   = (XfwfScrollInfo*)dcall;
    Bool           process = FALSE;
    int            new_value = 0;

    if ((slider->style & wxVERTICAL) && (info->flags & XFWF_VPOS)) {
	if (info->reason == XfwfSPageUp || info->reason == XfwfSPageDown) {
	    XfwfMoveThumb(slider->X->handle, 0.0, info->vpos);
	}
	new_value = int(slider->minimum 
			+ info->vpos * (slider->maximum-slider->minimum));
	process = TRUE;
    } else if (!(slider->style & wxVERTICAL) && (info->flags & XFWF_HPOS)) {
	if (info->reason == XfwfSPageLeft || info->reason == XfwfSPageRight) {
	    XfwfMoveThumb(slider->X->handle, info->hpos, 0.0);
	}
	new_value = int(slider->minimum
			+ info->hpos * (slider->maximum-slider->minimum));
	process = TRUE;
    }
    if (process && new_value != slider->value) {
	// set and display new value
	slider->value = new_value;
	char tempstring[80];
	sprintf(tempstring, "%d", new_value);
	XtVaSetValues(slider->X->handle, XtNlabel, tempstring, NULL);
	// process event
	wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
	slider->ProcessCommand(*event);
    }
}
