/*								-*- C++ -*-
 * $Id: TextWindow.h,v 1.1 1996/01/10 14:57:27 markus Exp $
 *
 * Purpose: text window panel item
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

#ifndef TextWindow_h
#define TextWindow_h

#ifdef __GNUG__
#pragma interface
#endif

class wxPanel;

class wxTextWindow : public wxText {
DECLARE_DYNAMIC_CLASS(wxTextWindow)
public:
    wxTextWindow(void);
    wxTextWindow(wxPanel *panel,
		 int x=-1, int y=-1, int width=-1, int height=-1,
		 long style=0, char *name="textWindow");
    wxTextWindow(wxWindow *panel,
		 int x=-1, int y=-1, int width=-1, int height=-1,
		 long style=0, char *name="textWindow");
    /* MATTHEW: */
    wxTextWindow(wxFrame *panel,
		 int x=-1, int y=-1, int width=-1, int height=-1,
		 long style=0, char *name="textWindow");
    
    Bool Create(wxPanel *panel,
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="textWindow");
    Bool Create(wxWindow *panel,
		int x=-1, int y=-1, int width=-1, int height=-1,
		long style=0, char *name="textWindow");

    void  Clear(void);
    void  DiscardEdits(void)
	{ Clear(); }
    char  *GetContents(void);
    long  GetInsertionPoint(void);
    long  GetLastPosition(void);
    int   GetLineLength(long lineNo);
    int   GetLineText(long lineNo, char *buf);
    int   GetNumberOfLines(void);
    Bool  LoadFile(char *file);
    Bool  Modified(void)
	{ return modified; }
    void  PositionToXY(long pos, long *x, long *y);
    void  Remove(long from, long to);
    void  Replace(long from, long to, char *value);
    Bool  SaveFile(char *file);
    void  SetEditable(Bool editable);
    void  SetInsertionPoint(long pos);
    void  SetInsertionPointEnd(void);
    void  SetSelection(long from, long to);
    void  ShowPosition(long pos);
    void  WriteText(char *text);
    long  XYToPosition(long x, long y);

    wxTextWindow& operator<<(char *s);
    wxTextWindow& operator<<(int i);
    wxTextWindow& operator<<(long i);
    wxTextWindow& operator<<(float f);
    wxTextWindow& operator<<(double d);
    wxTextWindow& operator<<(char c);

private:
#   ifdef Have_Xt_Types
    static void EventCallback(Widget, XtPointer, XtPointer);
#   endif
    Bool modified;
};

#endif // TextWin_h
