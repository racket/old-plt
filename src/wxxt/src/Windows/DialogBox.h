/*								-*- C++ -*-
 * $Id: DialogBox.h,v 1.3 1999/11/25 16:32:23 mflatt Exp $
 *
 * Purpose: dialog box
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

#ifndef DialogBox_h
#define DialogBox_h

#ifdef __GNUG__
#pragma interface
#endif

class wxDialogBox : public wxFrame {
public:
    wxDialogBox(void);
    wxDialogBox(wxWindow *parent, char *title, Bool modal=FALSE,
		int x=-1, int y=-1, int width=500, int height=500,
		long style=wxDEFAULT_DIALOG_STYLE, char *name="dialogBox");

    Bool Create(wxFrame *parent, char *title, Bool modal=FALSE,
		int x=-1, int y=-1, int width=500, int height=500,
		long style=wxDEFAULT_DIALOG_STYLE, char *name="dialogBox");

    void SetModal(Bool mod) { modal = mod; }
    Bool Show(Bool show);

    Bool ModalShowing() { return modal_showing; }
private:
    Bool modal, modal_showing;
};

#endif // DialogBox_h

