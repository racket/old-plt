/*								-*- C++ -*-
 * $Id: Message.cc,v 1.5 1998/12/15 17:23:53 mflatt Exp $
 *
 * Purpose: message panel item
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
#pragma implementation "Message.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxMessage
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_LabelWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy message
//-----------------------------------------------------------------------------

wxMessage::wxMessage(void) : wxItem()
{
    __type = wxTYPE_MESSAGE;
}

wxMessage::wxMessage(wxPanel *panel, char *message,
		   int x, int y, long style, char *name) : wxItem()
{
    __type = wxTYPE_MESSAGE;
    Create(panel, message, x, y, style, name);
}

wxMessage::wxMessage(wxPanel *panel, wxBitmap *bitmap,
		   int x, int y, long style, char *name) : wxItem()
{
    __type = wxTYPE_MESSAGE;
    Create(panel, bitmap, x, y, style, name);
}

static void do_nothing()
{
}

Bool wxMessage::Create(wxPanel *panel, char *message,
		      int x, int y, long style, char *name)
{
  // message = wxGetCtlLabel(message);

    bm_label = NULL;

    ChainToPanel(panel, style, name);

    // create frame
    X->frame = XtVaCreateWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNshrinkToFit, TRUE,
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    XtManageChild(X->frame);
    // create widget
    X->handle = XtVaCreateManagedWidget
	("message", xfwfLabelWidgetClass, X->frame,
	 XtNlabel,       message,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNalignment,   wxALIGN_LEFT,
	 XtNshrinkToFit, TRUE,
	 XtNhighlightThickness, 0,
	 ( !(style & wxBORDER) ) ?
	     NULL :
	     XtNouterOffset, 1, XtNinnerOffset, 1,
	     XtNframeWidth,  2, XtNframeType, XfwfChiseled,
	     NULL);

    panel->PositionItem(this, x, y, -1, -1);
    AddEventHandlers();

    /* This just turns on KeyPress events in the widget so that PreOnChar() works. */
    XtAddEventHandler(X->frame, KeyPressMask, FALSE, (XtEventHandler)do_nothing, NULL);
    XtAddEventHandler(X->handle, KeyPressMask, FALSE, (XtEventHandler)do_nothing, NULL);
    
    AllowResize(FALSE);

    return TRUE;
}

Bool wxMessage::Create(wxPanel *panel, wxBitmap *bitmap,
		      int x, int y, long style, char *name)
{
    if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
      return Create(panel, "<bad-image>", x, y, style, name);

    bitmap->selectedIntoDC++;
    bm_label = bitmap;

    ChainToPanel(panel, style, name);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNshrinkToFit, TRUE,
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    // create widget
    X->handle = XtVaCreateManagedWidget
	("Message", xfwfLabelWidgetClass, X->frame,
	 XtNpixmap,      GETPIXMAP(bitmap),
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNalignment,   wxALIGN_LEFT,
	 XtNshrinkToFit, TRUE,
	 XtNhighlightThickness, 0,
	 ( !(style & wxBORDER) ) ?
	     NULL :
	     XtNouterOffset, 1, XtNinnerOffset, 1,
	     XtNframeWidth,  2, XtNframeType, XfwfChiseled,
	     NULL);

    panel->PositionItem(this, x, y, -1, -1);

    AllowResize(FALSE);

    return TRUE;
}

wxMessage::~wxMessage()
{
  if (bm_label) {
    --bm_label->selectedIntoDC;
    XtVaSetValues(X->handle, XtNpixmap, NULL, NULL);
  }
}

//-----------------------------------------------------------------------------
// alternate SetLabel for changing bitmap
//-----------------------------------------------------------------------------

void wxMessage::AllowResize(Bool allow)
{
    XtVaSetValues(X->handle, XtNshrinkToFit, allow, NULL);
}

void wxMessage::SetAlignment(long alignment)
{
    XtVaSetValues(X->handle, XtNalignment, alignment, NULL);
}

void wxMessage::SetLabel(char *message)
{
  message = wxGetCtlLabel(message);
  if (!bm_label)
    XtVaSetValues(X->handle, XtNlabel, message, XtNbitmap, None, NULL);
}

void wxMessage::SetLabel(wxBitmap *bitmap)
{
  if (bm_label && bitmap && bitmap->Ok() && (bitmap->selectedIntoDC >= 0)
      && (bitmap->GetDepth()==1 || bitmap->GetDepth()==wxDisplayDepth())) {
    --bm_label->selectedIntoDC;
    bm_label = bitmap;
    bm_label->selectedIntoDC++;
    XtVaSetValues(X->handle, XtNlabel, NULL, XtNpixmap, GETPIXMAP(bitmap), NULL);
  }
}

char *wxMessage::GetLabel(void)
{
    if (!X->handle) // forbid, if no widget associated
	return NULL;

    char *label = NULL;
    XtVaGetValues(X->handle, XtNlabel, &label, NULL);
    return label;
}
