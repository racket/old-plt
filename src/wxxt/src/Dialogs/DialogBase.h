/*								-*- C++ -*-
 * $Id: DialogBase.h,v 1.2 1996/01/11 10:26:54 markus Exp $
 *
 * Purpose: dialog function box (used for common dialogs)
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

#ifndef DialogBase_h
#define DialogBase_h

#ifdef __GNUG__
#pragma interface
#endif

class wxButton;
class wxMessage;
class wxPanel;

class wxDialogBase : public wxDialogBox {
DECLARE_DYNAMIC_CLASS(wxDialogBase)
public:
    wxDialogBase(char *message, char *caption, int style,
		 wxWindow *parent=NULL, int x=-1, int y=-1);

    int   GetInput(void);
    void  OnCommand(wxWindow &win, wxCommandEvent &event);
    Bool  OnClose(void);
    void  OnSize(int width, int height);
private:
    int  pressed_button;
    int  dlg_style;
    int  x, y;

    wxButton   *yes;
    wxButton   *no;
    wxButton   *ok;
    wxButton   *cancel;
    wxPanel    *buttons;
    wxMessage  *msg;
};

#endif // DialogBase_h
