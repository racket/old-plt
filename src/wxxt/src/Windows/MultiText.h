/*								-*- C++ -*-
 * $Id: MultiText.h,v 1.1 1996/01/10 14:57:21 markus Exp $
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

#ifndef MultiText_h
#define MultiText_h

#ifdef __GNUG__
#pragma interface
#endif

class wxPanel;

class wxMultiText : public wxText {
DECLARE_DYNAMIC_CLASS(wxMultiText)
public:
    wxMultiText(void);
    wxMultiText(wxPanel *panel, wxFunction func, char *label, char *value="",
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="multiText");
    
    Bool Create(wxPanel *panel, wxFunction func, char *label, char *value="",
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="multiText");

    char *GetValue(void) { return wxText::GetValue(); }
    void GetValue(char *buffer, int size);
private:
#   ifdef Have_Xt_Types
    static void EventCallback(Widget, XtPointer, XtPointer);
#   endif
};

#endif // MultiText_h
