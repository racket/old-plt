/*								-*- C++ -*-
 * $Id: Panel.h,v 1.2 1998/09/06 01:54:02 mflatt Exp $
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

#ifndef Panel_h
#define Panel_h

#ifdef __GNUG__
#pragma interface
#endif

class wxButton;
class wxColour;
class wxCommandEvent;
class wxFont;
class wxItem;

class wxPanel : public wxWindow {
DECLARE_DYNAMIC_CLASS(wxPanel)
public:
    wxPanel(void); 
    wxPanel(wxPanel *parent, int x=-1, int y=-1, int width=-1, int height=-1,
	    int style=0, char *name="panel");
    wxPanel(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
	    int style=0, char *name="panel");
    // panel creation
    Bool  Create(wxPanel *parent, 
		 int x=-1, int y=-1, int width=-1, int height=-1,
		 int style=0, char *name="panel");
    Bool  Create(wxWindow *parent, 
		 int x=-1, int y=-1, int width=-1, int height=-1,
		 int style=0, char *name="panel");
    // resize/layout panel
    virtual void  GetClientSize(int *width, int *height);
    virtual void  Fit(void);
    virtual void  Layout(void);
    // data retrieved from wxItem and it's children
    wxColour  *GetButtonColour(void)	{ return button_colour; }
    wxFont    *GetButtonFont(void)      { return font; }
    wxColour  *GetLabelColour(void)     { return label_colour; }
    wxFont    *GetLabelFont(void)       { return label_font; }
    void      SetBackgroundColour(wxColour *col);
    void      SetButtonColour(wxColour *col)   { button_colour = col; }
    void      SetButtonFont(wxFont *fnt)      { font = fnt; }
    void      SetLabelColour(wxColour *col)    { label_colour = col; }
    void      SetLabelFont(wxFont *font)       { label_font = font; }
    // position of labels
    int   GetLabelPosition(void)           { return label_pos; }
    void  SetLabelPosition(int position)   { label_pos = position; }
    // positioning of items
    void  GetCursor(int *x, int *y);
    void  SetItemCursor(int x, int y);
    int   GetHorizontalSpacing(void)  { return h_space; }
    int   GetVerticalSpacing(void)    { return v_space; }
    void  NewLine(int pixels = 0);
    void  PositionItem(wxWindow *win, int x, int y, int width, int height);
    void  SetHorizontalSpacing(int sp)  { h_space = sp; }
    void  SetVerticalSpacing(int sp)    { v_space = sp; }
    void  Tab(int pixels = 0);
    // default item
    wxButton  *GetDefaultItem(void)  { return default_item; }
    // virtual event functions
    virtual void  OnDefaultAction(wxItem *item);
    // drawing
    wxPanelDC* GetPanelDC(void) { return dc; }
    virtual void ChangeToGray(Bool gray);
protected:
    friend class wxButton;	// allow access to default_item

    wxButton  *default_item;	// executed on default action
    wxColour  *button_colour;   // colour for buttons
    wxColour  *label_colour;    // colour for labels
    wxFont    *label_font;	// font for labels
    int       label_pos;	// where to put the label
    int       cursor_x, cursor_y,
	      h_space, v_space,
              v_line_extent; // for positioning of items
};

#endif // Panel_h
