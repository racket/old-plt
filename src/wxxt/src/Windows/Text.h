/*								-*- C++ -*-
 * $Id: Text.h,v 1.1 1996/01/10 14:57:25 markus Exp $
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

#ifndef Text_h
#define Text_h

#ifdef __GNUG__
#pragma interface
#endif

class wxKeyEvent;
class wxPanel;

class wxText : public wxItem {
DECLARE_DYNAMIC_CLASS(wxText)
public:
    wxText(void);
    wxText(wxPanel *panel, wxFunction func, char *label, char *value="",
	     int x=-1, int y=-1, int width=-1, int height=-1,
	     long style=0, char *name="text");
    
    Bool Create(wxPanel *panel, wxFunction func, char *label, char *value="",
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="text");

    char *GetValue(void);
    void SetValue(char *value);

    // Clipboard operations
    void Copy(void) {};
    void Cut(void) {};
    void Paste(void) {};
    // OnChar handles pressing of ENTER if wanted
    void OnChar(wxKeyEvent& event);

    void SetEditable(Bool on);

    virtual void ChangeToGray(Bool gray);
private:
#   ifdef Have_Xt_Types
    static void EventCallback(Widget, XtPointer, XtPointer);

    friend void wxTextEnter(Widget, XEvent*, String*, Cardinal*);
#   endif
    Bool editable;
    void *porthole; /* Widget */
};

#endif // Text_h
