/*								-*- C++ -*-
 * $Id: RadioBox.cc,v 1.9 1999/08/09 14:51:30 mflatt Exp $
 *
 * Purpose: radio box panel item
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
#pragma implementation "RadioBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxRadioBox
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_GroupWidget
#define  Uses_ToggleWidget
#include "widgets.h"

#define TOGGLES ((Widget*)toggles)

//-----------------------------------------------------------------------------
// create and destroy button
//-----------------------------------------------------------------------------

wxRadioBox::wxRadioBox(void) : wxItem()
{
    __type = wxTYPE_RADIO_BOX;

    toggles     = NULL;
    num_toggles = 0;
}

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func, char *label,
		       int x, int y, int width, int height, int n, char **choices,
		       int num_rows, long style, char *name) : wxItem()
{
    __type = wxTYPE_RADIO_BOX;

    toggles     = NULL;
    num_toggles = 0;

    Create(panel, func, label, x, y, width, height, n, choices,
	   num_rows, style, name);
}

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func, char *label,
		       int x, int y, int width, int height, int n, wxBitmap **choices,
		       int num_rows, long style, char *name) : wxItem()
{
    __type = wxTYPE_RADIO_BOX;

    toggles     = NULL;
    num_toggles = 0;

    Create(panel, func, label, x, y, width, height, n, choices,
	   num_rows, style, name);
}

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func, char *label,
		   int x, int y, int width, int height, int n, char **choices,
		   int num_rows, long style, char *name)
{
    int i;

    if ( (num_toggles = n) <= 0 ) {
	wxDebugMsg("%s created without items (n=0)!\n", name);
	return TRUE;
    }

    bm_labels = NULL;

    ChainToPanel(panel, style, name);

    Bool vert = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    /* MATTHEW: [5] */
    if ((style & wxVERTICAL) == wxVERTICAL) {
      if (num_rows <= 0)
	num_rows = num_toggles;
      else
	num_rows = 1;
    } else {
      if (num_rows <= 0)
	num_rows = 1;
      else
	num_rows = num_toggles / num_rows;
    }

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNframeType,   (style & wxFLAT) ? XfwfChiseled : XfwfSunken,
	 XtNframeWidth,  0, /* MATTHEW: [5] No frame */
	 XtNshrinkToFit, (width < 0 || height < 0),
	 NULL);
    // create group widget, which holds the the toggles
    X->handle = XtVaCreateManagedWidget
	("radiobox", xfwfGroupWidgetClass, X->frame,
	 XtNselectionStyle, (style & wxAT_MOST_ONE) ?
	 		     XfwfSingleSelection : XfwfOneSelection,
	 XtNstoreByRow,     FALSE,
	 XtNlabel,          NULL,
	 XtNframeWidth,     0,
	 XtNbackground,  bg->GetPixel(cmap), /* MATTHEW */
	 XtNrows,           num_rows, /* MATTHEW: [5] */
	 XtNshrinkToFit,    (width < 0 || height < 0),
	 NULL);
    // create the toggles
    toggles = (void*)new Widget[num_toggles];
    enabled = new Bool[num_toggles];
    for (i=0; i < num_toggles; ++i) {
        enabled[i] = 1;
	char num_name[10]; sprintf(num_name, "%d", i);
	char *label = wxGetCtlLabel(choices[i]);
	((Widget*)toggles)[i] = XtVaCreateManagedWidget
	    (num_name, xfwfToggleWidgetClass, X->handle,
	     XtNlabel,         label,
	     XtNbackground,    bg->GetPixel(cmap),
	     XtNforeground,    fg->GetPixel(cmap),
	     XtNfont,          font->GetInternalFont(),
	     XtNshrinkToFit,   TRUE,
	     NULL);
    }
    // set data declared in wxItem
    callback = func;
    XtAddCallback(X->handle, XtNactivate, wxRadioBox::EventCallback,
		  (XtPointer)this);
    // resize enforcer
    Dimension ww, hh; float lw, lh;
    XtVaGetValues(X->handle, XtNwidth, &ww, XtNheight, &hh, NULL);
    if (label)
      GetTextExtent(label, &lw, &lh, NULL, NULL, label_font);
    else {
      lw = lh = 0;
    }
    if (vert)	hh += int(lh);
    else	ww += int(lw);
    XtVaSetValues(X->frame, XtNwidth, ww+4, XtNheight, hh+4, NULL);
    // panel positioning
    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    for (i=0; i < num_toggles; ++i)
      XtInsertEventHandler(((Widget*)toggles)[i],
			   KeyPressMask |	// for PreOnChar
			   ButtonPressMask |	// for PreOnEvent
			   ButtonReleaseMask |
			   ButtonMotionMask |
			   PointerMotionMask |
			   PointerMotionHintMask,
			   FALSE,
			   (XtEventHandler)wxWindow::WindowEventHandler,
			   (XtPointer)saferef,
			   XtListHead);

    return TRUE;
}

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func, char *label,
		   int x, int y, int width, int height, int n, wxBitmap **choices,
		   int num_rows, long style, char *name)
{
    int i;

    if ( (num_toggles = n) <= 0 ) {
	wxDebugMsg("%s created without items (n=0)!\n", name);
	return TRUE;
    }

    ChainToPanel(panel, style, name);

    label = wxGetCtlLabel(label);

    /* MATTHEW: [5] */
    Bool vert = (panel->GetLabelPosition() == wxVERTICAL);

    /* MATTHEW: [5] */
    if ((style & wxVERTICAL) == wxVERTICAL) {
      if (num_rows <= 0)
	num_rows = num_toggles;
      else
	num_rows = 1;
    } else {
      if (num_rows <= 0)
	num_rows = 1;
      else
	num_rows = num_toggles / num_rows;
    }

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNframeType,   (style & wxFLAT) ? XfwfChiseled : XfwfSunken,
	 XtNframeWidth,  0, /* MATTHEW: no frame */
	 XtNshrinkToFit, TRUE,
	 NULL);
    // create group widget, which holds the the toggles
    X->handle = XtVaCreateManagedWidget
	("radiobox", xfwfGroupWidgetClass, X->frame,
	 XtNselectionStyle, (style & wxAT_MOST_ONE) ?
	 		     XfwfSingleSelection : XfwfOneSelection,
	 XtNstoreByRow,     FALSE,
	 XtNlabel,          NULL,
	 XtNframeWidth,     0,
	 XtNbackground,     bg->GetPixel(cmap), /* MATTHEW */
	 XtNrows,           num_rows,/* MATTHEW: [5] */
	 XtNshrinkToFit,    TRUE,
	 NULL);
    // create the toggles
    toggles = (void*)new Widget[num_toggles];
    enabled = new Bool[num_toggles];
    bm_labels = new wxBitmap*[num_toggles];
    for (i=0; i < num_toggles; ++i) {
	char num_name[10]; sprintf(num_name, "%d", i);

	char *kind;
	void *label;

	enabled[i] = 1;

	if (choices[i]->Ok() && (choices[i]->selectedIntoDC >= 0)) {
	  kind = XtNpixmap;
	  label = (void *)GETPIXMAP(choices[i]);
	  bm_labels[i] = choices[i];
	  choices[i]->selectedIntoDC++;
	} else {
	  kind = XtNlabel;
	  label = (char *)"<bad-image>";
	  bm_labels[i] = NULL;
	}

	((Widget*)toggles)[i] = XtVaCreateManagedWidget
	    (num_name, xfwfToggleWidgetClass, X->handle,
	     kind,             label,
	     XtNbackground,    bg->GetPixel(cmap),
	     XtNforeground,    fg->GetPixel(cmap),
	     XtNfont,          font->GetInternalFont(),
	     XtNshrinkToFit,   TRUE,
	     NULL);
    }
    // set data declared in wxItem
    callback = func;
    XtAddCallback(X->handle, XtNactivate, wxRadioBox::EventCallback,
		  (XtPointer)this);
    // resize enforcer
    Dimension ww, hh; float lw, lh;
    XtVaGetValues(X->handle, XtNwidth, &ww, XtNheight, &hh, NULL);
    if (label)
      GetTextExtent(label, &lw, &lh, NULL, NULL, label_font);
    else {
      lw = lh = 0;
    }
    if (vert)	hh += int(lh);
    else	ww += int(lw);
    XtVaSetValues(X->frame, XtNwidth, ww+4, XtNheight, hh+4, NULL);
    // panel positioning
    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    for (i = 0; i < num_toggles; i++)
      XtInsertEventHandler(((Widget*)toggles)[i],
			   KeyPressMask |	// for PreOnChar
			   ButtonPressMask |	// for PreOnEvent
			   ButtonReleaseMask |
			   ButtonMotionMask |
			   PointerMotionMask |
			   PointerMotionHintMask,
			   FALSE,
			   (XtEventHandler)wxWindow::WindowEventHandler,
			   (XtPointer)saferef,
			   XtListHead);

    return TRUE;
}

wxRadioBox::~wxRadioBox(void)
{
  if (bm_labels) {
    int i;
    for (i = 0; i < num_toggles; i++)
      if (bm_labels[i]) {
	--bm_labels[i]->selectedIntoDC;
	XtVaSetValues(((Widget*)toggles)[i], XtNpixmap, NULL, NULL);
      }
  }

  if (num_toggles) {
    // delete toggles;
    num_toggles = 0;
  }
}

//-----------------------------------------------------------------------------
// methods to access internal data
//-----------------------------------------------------------------------------

void wxRadioBox::Enable(int item, Bool enable)
{
  if (0 <= item && item < num_toggles) {
    enabled[item] = enable;
    if (!IsGray())
      XtSetSensitive(TOGGLES[item], enable);
  }
}

void wxRadioBox::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  for (int i = 0; i < num_toggles; i++)
    XtSetSensitive(TOGGLES[i], gray ? FALSE : enabled[i]);
}

int wxRadioBox::FindString(char *s)
{
  for (int i=0; i < num_toggles; ++i) {
    char *l = GetLabel(i); /* MATTHEW: [5] */
    if (l && !strcmp(l, s))
      return i;
  }
  return -1;
}

char *wxRadioBox::GetLabel(int item)
{
    char *label = NULL;

    if (0 <= item && item < num_toggles)
	XtVaGetValues(TOGGLES[item], XtNlabel, &label, NULL);

    return label;
}

int wxRadioBox::GetSelection(void)
{
    long selection;

    if (!num_toggles)
      return -1;

    XtVaGetValues(X->handle, XtNselection, &selection, NULL);

    return int(selection);
}

char *wxRadioBox::GetStringSelection(void)
{
    char *label = NULL;
    int  item = GetSelection();

    if (0 <= item && item < num_toggles)
	XtVaGetValues(TOGGLES[item], XtNlabel, &label, NULL);
    return label;
}

char *wxRadioBox::GetString(int which)
{
    char *label = NULL;

    /* MATTHEW: [5] */
    if (0 <= which && which < num_toggles)
	XtVaGetValues(TOGGLES[which], XtNlabel, &label, NULL);
    return label;
}

void wxRadioBox::SetLabel(int item, char *label)
{
  label = wxGetCtlLabel(label);

  if (0 <= item && item < num_toggles
      && (!bm_labels || !bm_labels[item]))
    XtVaSetValues(TOGGLES[item], XtNlabel, label, NULL);
}

void wxRadioBox::SetLabel(int item, wxBitmap *bitmap)
{
  if (0 <= item && item < num_toggles
      && bm_labels && bm_labels[item]) {
    --bm_labels[item]->selectedIntoDC;
    bm_labels[item] = bitmap;
    bm_labels[item]->selectedIntoDC++;
    XtVaSetValues(TOGGLES[item], XtNlabel, NULL,
		  XtNpixmap, GETPIXMAP(bitmap), NULL);
  }
}

void wxRadioBox::SetSelection(int item)
{
  if (0 <= item && item < num_toggles)
    XtVaSetValues(X->handle, XtNselection, long(item), NULL);
}

void wxRadioBox::SetStringSelection(char *s)
{
  SetSelection(FindString(s));
}

Bool wxRadioBox::Show(int item, Bool show)
{
  if (0 <= item && item < num_toggles) {
    if (show) XtMapWidget(TOGGLES[item]);
    else      XtUnmapWidget(TOGGLES[item]);
  }
  return FALSE;
}

void wxRadioBox::Command(wxCommandEvent &event)
{
  ProcessCommand(event);
}

//-----------------------------------------------------------------------------
// override parent methods
//-----------------------------------------------------------------------------

void wxRadioBox::ChangeColours(void)
{
    wxItem::ChangeColours();
    for (int i=0; i < num_toggles; ++i) {
	if (bg)
	    XtVaSetValues(TOGGLES[i], XtNbackground,
			  bg->GetPixel(cmap), NULL);
	if (fg)
	    XtVaSetValues(TOGGLES[i], XtNforeground,
			  fg->GetPixel(cmap), NULL);
    }
}

//-----------------------------------------------------------------------------
// callbacks for xfwfGroupWidgetClass
//-----------------------------------------------------------------------------

void wxRadioBox::SetSelectedButtonFocus()
{
  ButtonFocus(GetSelection());
}

void wxRadioBox::EventCallback(Widget WXUNUSED(w), XtPointer dclient, XtPointer WXUNUSED(dcall))
{
    wxRadioBox     *radioBox = (wxRadioBox*)dclient;
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);

    radioBox->SetSelectedButtonFocus();

    radioBox->ProcessCommand(*event);
}

extern "C" Boolean has_focus_now(Widget w);

int wxRadioBox::ButtonFocus(int which)
{
  if (which > num_toggles) return -1;

  if (which > -1) {
    // Set the focus on the just-clicked button
    // find frame of this widget
    wxWindow *win = this;
    for (/*wxWindow *win = this*/; win; win = win->GetParent())
      if (wxSubType(win->__type, wxTYPE_FRAME))
	break;
    
    if (win) {
      XtSetKeyboardFocus(win->GetHandle()->frame, (Widget)TOGGLES[which]);
    }

    return -1;
  } else {
    int i;
    for (i = num_toggles; i--; ) {
      Widget w = (Widget)TOGGLES[i];
      if (has_focus_now(w))
	return i;
    }
    return -1;
  }
}
