/*								-*- C++ -*-
 * $Id: Choice.cc,v 1.1 1996/01/10 14:57:06 markus Exp $
 *
 * Purpose: choice panel item
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
#pragma implementation "Choice.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxChoice
#define  Uses_wxMenu
#include "wx.h"
#define  Uses_ArrowWidget
#define  Uses_TraversingEnforcerWidget
#define  Uses_LabelWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy button
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxChoice, wxItem)

wxChoice::wxChoice(void) : wxItem()
{
    __type = wxTYPE_CHOICE;

    choice_menu = DEBUG_NEW wxMenu(NULL, (wxFunction)&(wxChoice::MenuEventCallback));
    num_choices = 0;
    selection   = -1;
}

wxChoice::wxChoice(wxPanel *panel, wxFunction function, char *label,
		   int x, int y, int width, int height,
		   int n, char *choices[], long style, char *name) : wxItem()
{
    __type = wxTYPE_BUTTON;

    choice_menu = DEBUG_NEW wxMenu(NULL, (wxFunction)&(wxChoice::MenuEventCallback));
    num_choices = 0;
    selection   = -1;

    Create(panel, function, label, x, y, width, height, n, choices, style, name);
}

Bool wxChoice::Create(wxPanel *panel, wxFunction function, char *label,
		      int x, int y, int width, int height,
		      int n, char *choices[], long style, char *name)
{
    ChainToPanel(panel, style, name);

    Bool vert = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfTraversingEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNshrinkToFit, TRUE,
	 NULL);
    // create widget
    X->handle = XtVaCreateManagedWidget
	("choice", xfwfLabelWidgetClass, X->frame,
	 XtNlabel,       n > 0 ? choices[0] : "",
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  fg->GetPixel(cmap),
	 XtNfont,        font->GetInternalFont(),
	 XtNframeWidth,  0,
	 XtNalignment,   XfwfLeft,
	 XtNleftMargin,  16,
	 XtNshrinkToFit, (width < 0 || height < 0),
	 // XtNtraversalOn, TRUE, /* MATTHEW */
	 NULL);
    // arrow widget which pops up a menu
    Widget button = XtVaCreateManagedWidget
	("choice_button", xfwfArrowWidgetClass, X->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  bg->GetPixel(cmap),
	 XtNdirection,   XfwfBottom,
	 XtNrepeat,      FALSE,
	 XtNlocation,    "0 0 14 1.0",
	 NULL);
    // set data declared in wxItem
    callback = function;
    XtAddCallback(button, XtNcallback, wxChoice::EventCallback, (XtPointer)this);

    selection = n > 0 ? 0 : -1;
    for (int i = 0; i < n; ++i)
	Append(choices[i]);

    panel->PositionItem(this, x, y, width, height);
    AddEventHandlers();

    return TRUE;
}

wxChoice::~wxChoice (void)
{
    if (choice_menu)
	delete choice_menu;
    choice_menu = NULL;
    num_choices = 0;
}

//-----------------------------------------------------------------------------
// size
//-----------------------------------------------------------------------------

void  wxChoice::GetSize(int *width, int *height)
{
  float w, h;
  float maxw = 0, currentw = 0;
  int i;

  wxWindow::GetSize(width, height);

  if (width) {
    for (i = 0; i < num_choices; i++) {
      char *s = GetString(i);
      GetTextExtent(s, &w, &h, NULL, NULL, font);
      if (w > maxw)
	maxw = w;
      if (i == selection)
	currentw = w;
    }

    *width += (int)(maxw - currentw);
  }
}


//-----------------------------------------------------------------------------
// methods to access internal data
//-----------------------------------------------------------------------------

void wxChoice::Append(char *s)
{
    choice_menu->Append(num_choices++, s, (char *)(-1));
    if (num_choices == 1)
      XtVaSetValues(X->handle, XtNlabel, s, NULL);
}

void wxChoice::Clear(void)
{
    delete choice_menu;
    choice_menu = DEBUG_NEW wxMenu(NULL, (wxFunction)&(wxChoice::MenuEventCallback));
    num_choices = 0;
    selection = 0;
    XtVaSetValues(X->handle, XtNlabel, "", NULL);
}

int wxChoice::FindString(char *s)
{
    return choice_menu->FindItem(s);
}

char *wxChoice::GetString(int n)
{
    return choice_menu->GetLabel(n);
}

char *wxChoice::GetStringSelection(void)
{
    return choice_menu->GetLabel(selection);
}

void wxChoice::SetSelection(int n)
{
    if (0 <= n && n <= num_choices) {
	selection = n;
	char *label = GetString(selection);
	XtVaSetValues(X->handle, XtNlabel, label, NULL);
    }
}

void wxChoice::SetStringSelection(char *s)
{
    SetSelection(FindString(s));
}

//-----------------------------------------------------------------------------
// callback for commandWidgetClass
//-----------------------------------------------------------------------------

void wxChoice::EventCallback(Widget WXUNUSED(w),
			     XtPointer clientData, XtPointer WXUNUSED(ptr))
{
    wxChoice *choice = (wxChoice*)clientData;

    choice->choice_menu->SetClientData((char*)choice);
    choice->choice_menu->SetFont(choice->font);
    choice->choice_menu->SetBackgroundColour(choice->bg);
    choice->choice_menu->SetForegroundColour(choice->fg);

    // popup menu below "button"
    Dimension hh;
    XtVaGetValues(choice->X->handle, XtNheight, &hh, NULL);

    choice->PopupMenu(choice->choice_menu, 0, (int)hh);
}

void wxChoice::MenuEventCallback(wxObject& obj, wxCommandEvent& ev)
{
    wxChoice       *choice = (wxChoice*)((wxMenu&)obj).GetClientData();

    if (!choice->Number())
      return;

    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);

    event->eventObject   = choice;
    event->commandInt    = ev.commandInt;
    event->commandString = choice->GetString(ev.commandInt);

    choice->SetSelection(ev.commandInt);
    choice->ProcessCommand(*event);
}
