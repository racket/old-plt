/*								-*- C++ -*-
 * $Id: EnhDialogBox.h,v 1.1 1996/01/10 14:57:08 markus Exp $
 *
 * Purpose: enhanced dialog box
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

#ifndef EnhDialogBox_h
#define EnhDialogBox_h

#ifdef __GNUG__
#pragma interface
#endif

class wxEnhDialogBox : public wxDialogBox {
DECLARE_DYNAMIC_CLASS(wxEnhDialogBox)
public:
    wxPanel *userPanel;

    wxEnhDialogBox(void);
    wxEnhDialogBox(wxFrame *frame, char *title, Bool modal = FALSE,
		   wxFunction fun = NULL, int space=-1,
		   int x=-1, int y=-1, int width=-1, int height=-1,
		   long style = wxENH_DEFAULT, char *name = "enhDialogBox");
    ~wxEnhDialogBox();

    Bool Create(wxFrame *frame, char *title, Bool modal = FALSE,
		wxFunction fun = NULL, int space=-1,
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style = wxENH_DEFAULT, char *name = "enhDialogBox");

    wxButton  *AddCmd(char *label,wxFunction fun=NULL, int tag = 0);
    wxButton  *AddCmd(wxBitmap *bitmap,wxFunction fun=NULL, int tag = 0);
    wxButton  *GetCmd(int number);
    void      Fit(void);
    void      SetPin(Bool flag);
    void      SetStatus(char *label=NULL) { SetStatusText(label); }
    Bool      Show(Bool show, Bool flag=FALSE);
};

#endif // EnhDialogBox_h
