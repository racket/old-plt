/*								-*- C++ -*-
 * $Id: Text.cc,v 1.2 1998/02/05 23:00:33 mflatt Exp $
 *
 * Purpose: text panel item
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
#pragma implementation "Text.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxText
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_ScrollTextWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy text
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxText, wxItem)

wxText::wxText(void) : wxItem()
{
    __type = wxTYPE_TEXT;
}

wxText::wxText(wxPanel *panel, wxFunction function, char *label,
	       char *value, int x, int y, int width, int height,
	       long style, char *name) : wxItem()
{
    __type = wxTYPE_TEXT;

    Create(panel, function, label, value, x, y, width, height, style, name);
}

Bool wxText::Create(wxPanel *panel, wxFunction function, char *label,
		    char *value, int x, int y, int width, int height,
		    long style, char *name)
{
    ChainToPanel(panel, style, name);

    Bool vert = (panel->GetLabelPosition() == wxVERTICAL);

    editable = TRUE;

    label = wxGetCtlLabel(label);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       label,
	 XtNalignment,   (vert ? XfwfTop : XfwfLeft),
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNframeType,   XfwfSunken,
	 XtNframeWidth,  2,
	 XtNshrinkToFit, TRUE,
	 NULL);
    // create text widget
    X->handle = XtVaCreateManagedWidget("porthole", scrollingTextWidgetClass, X->frame,
					XtNborderWidth, 0, NULL);
    porthole = (void *)X->handle;
    //  scrollingText- is a porthole containing a asciiText-Widget
    XtVaGetValues(X->handle, XtNtextWidget, &(X->handle), NULL);
    XFontStruct *oldfnt;
    int oldfht;
    XtVaGetValues(X->handle, XtNfont, &oldfnt, NULL);
    oldfht = oldfnt->ascent + oldfnt->descent;
    XFontStruct *fnt = (XFontStruct *)font->GetInternalFont();
    XtVaSetValues(X->handle,
		  XtNbackground, bg->GetPixel(cmap),
		  XtNforeground, fg->GetPixel(cmap),
		  XtNfont,       fnt,
		  XtNresize,     XawtextResizeBoth,
		  XtNstring,     value,
		  XtNeditType,   style & wxREADONLY ? XawtextRead : XawtextEdit,
		  XtNecho,       style & wxPASSWORD ? FALSE : TRUE,
		  NULL);

    // Now that font change changed in Text widget, resize porthole:
    if (fnt) {
      Dimension ht;
      XtVaGetValues((Widget)porthole, XtNheight, &ht, NULL);
      XtVaSetValues((Widget)porthole, XtNheight, ht + (fnt->ascent + fnt->descent - oldfht), NULL);
    }

    // propagate key events from frame to text widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);
    // callback
    callback = function;
    XtAddCallback(XawTextGetSource(X->handle), XtNcallback,
		  wxText::EventCallback, (XtPointer)this);
    // compute text height
    if (width < 0) {
      if (value) {
	float w, h;
	GetTextExtent(value, &w, &h, NULL, NULL, NULL);
	width = (int)w + 14;
	if (!vert && label) {
	  GetTextExtent(label, &w, &h, NULL, NULL, NULL);
	  width += (int)w + 4; /* Border */
	}
      } else
	width = wxTEXT_WIDTH;
    }

    panel->PositionItem(this, x, y, width, height);
    // redisplay textfield
    XEvent dummy;
    XtCallActionProc(X->handle, "redraw-display", &dummy, NULL, 0);
    AddEventHandlers();

    cursor = wxIBEAM_CURSOR;

    return TRUE;
}

//-----------------------------------------------------------------------------
// methods to set and get the value of wxText
//-----------------------------------------------------------------------------

char *wxText::GetValue(void)
{
    char *value;

    XtVaGetValues(X->handle, XtNstring, &value, NULL);
    return value;
}

void wxText::SetValue(char *value)
{
  if (value)
    XtVaSetValues(X->handle, XtNstring, value, NULL);
}

//-----------------------------------------------------------------------------
// Handling for ENTER inside OnChar
//-----------------------------------------------------------------------------

void wxText::OnChar(wxKeyEvent &event)
{
    if (event.keyCode == WXK_RETURN) {
	if (style & wxPROCESS_ENTER) {
	    // process enter to callback function
	    wxCommandEvent *cevent = new wxCommandEvent(wxEVENT_TYPE_TEXT_ENTER_COMMAND);
	    cevent->eventObject   = this;
	    cevent->commandString = GetValue();
	    ProcessCommand(*cevent);
	} else if (style & wxPROCESS_ENTER_TO_PANEL) {
	    // pressing enter should invoke the default action
	    parent->GetEventHandler()->OnDefaultAction(this);
	}
    }
    if (editable)
      wxItem::OnChar(event); // chain to parent method
}

void wxText::Command(wxCommandEvent &event)
{
  SetValue(event.commandString);
  ProcessCommand(event);
}

//-----------------------------------------------------------------------------
// callbacks for textWidgetClass
//-----------------------------------------------------------------------------

void wxText::EventCallback(Widget WXUNUSED(w),
			   XtPointer clientData, XtPointer WXUNUSED(ptr))
{
    wxText         *text = (wxText*)clientData;
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_TEXT_COMMAND);

    event->eventObject   = text;
    event->commandString = text->GetValue();

    text->ProcessCommand(*event);
}

/* MATTHEW: [5] */
void wxText::SetEditable(Bool on)
{
  editable = on;
}


void wxText::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  XtSetSensitive((Widget)porthole, !gray);
}
