/*								-*- C++ -*-
 * $Id: MultiText.cc,v 1.1 1996/01/10 14:57:20 markus Exp $
 *
 * Purpose: multi text panel item
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
#pragma implementation "MultiText.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxMultiText
#include "wx.h"
#define  Uses_AsciiTextWidget
#define  Uses_EnforcerWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy multiText
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxMultiText, wxText)

wxMultiText::wxMultiText(void) : wxText()
{
    __type = wxTYPE_MULTI_TEXT;
}

wxMultiText::wxMultiText(wxPanel *panel, wxFunction function, char *label,
			 char *value, int x, int y, int width, int height,
			 long style, char *name) : wxText()
{
    __type = wxTYPE_MULTI_TEXT;

    Create(panel, function, label, value, x, y, width, height, style, name);
}

Bool wxMultiText::Create(wxPanel *panel, wxFunction function, char *label,
			 char *value, int x, int y, int width, int height,
			 long style, char *name)
{
    ChainToPanel(panel, style, name);

    Bool vert   = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,      label,
	 XtNalignment,  vert ? XfwfTop : XfwfTopLeft,
	 XtNbackground, bg->GetPixel(cmap),
	 XtNforeground, label_fg->GetPixel(cmap),
	 XtNfont,       label_font->GetInternalFont(),
	 XtNframeType,  XfwfSunken,
	 XtNframeWidth, 2,
	 NULL);
    // create multiText widget
    X->handle = XtVaCreateManagedWidget
	("multitext", asciiTextWidgetClass, X->frame,
	 XtNbackground,       bg->GetPixel(cmap),
	 XtNforeground,       fg->GetPixel(cmap),
	 XtNfont,             font->GetInternalFont(),
	 XtNborderWidth,      0,
	 XtNresize,           XawtextResizeNever,
	 XtNscrollVertical,   XawtextScrollAlways,
	 XtNscrollHorizontal, (style & wxHSCROLL) ?
	 		      XawtextScrollWhenNeeded : XawtextScrollNever,
	 XtNstring,           value,
	 XtNeditType,         (style & wxREADONLY) ? XawtextRead : XawtextEdit,
	 NULL);
    // propagate key events from frame to text widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);
    // callback
    callback = function;
    XtAddCallback(XawTextGetSource(X->handle), XtNcallback,
		  wxMultiText::EventCallback, (XtPointer)this);

    panel->PositionItem(this, x, y,
			(width  > -1 ? width  : wxMULTI_TEXT_WIDTH),
			(height > -1 ? height : wxMULTI_TEXT_HEIGHT));
    AddEventHandlers();

    SetEditable(!(style & wxREADONLY));

    return TRUE;
}

//-----------------------------------------------------------------------------
// copy value to buffer
//-----------------------------------------------------------------------------

void wxMultiText::GetValue(char *buffer, int size)
{
    *buffer = 0;
    char *s = GetValue();
    if (s) {
	if (strlen(s) > (unsigned)(size-1)) {
	    strncpy(buffer, s, size-1);
	    buffer[size-1] = 0;
	} else
	    strcpy(buffer, s);
    }
}

//-----------------------------------------------------------------------------
// callback for multiTextWidgetClass
//-----------------------------------------------------------------------------

void wxMultiText::EventCallback(Widget WXUNUSED(w),
				XtPointer clientData, XtPointer WXUNUSED(ptr))
{
    wxMultiText         *multiText = (wxMultiText*)clientData;
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_MULTITEXT_COMMAND);

    event->eventObject   = multiText;
    event->commandString = multiText->GetValue();

    multiText->ProcessCommand(*event);
}
