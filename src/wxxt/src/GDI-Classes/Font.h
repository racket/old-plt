/*								-*- C++ -*-
 * $Id: Font.h,v 1.1 1996/01/10 14:56:14 markus Exp $
 *
 * Purpose: wxWindows font handling
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

#ifndef Font_h
#define Font_h

#ifdef __GNUG__
#pragma interface
#endif

class wxFont : public wxObject {
DECLARE_DYNAMIC_CLASS(wxFont)
public:
    wxFont(void);
    /* MATTHEW */
    wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	   Bool underlined = FALSE);
    /* MATTHEW */
    wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	   Bool underlined = FALSE);
    ~wxFont(void);

    int   GetPointSize(void)     { return point_size; }
    int   GetFamily(void)        { return family; }
    char  *GetFamilyString(void) { return wx_desc[family]; }
    /* MATTHEW: */
    int   GetFontId(void)        { return font_id; }
    /* MATTHEW: */
    char  *GetFaceString(void);
    int   GetStyle(void)         { return style; }
    char  *GetStyleString(void)  { return wx_desc[style]; }
    int   GetWeight(void)	 { return weight==wxNORMAL_WEIGHT ? wxNORMAL : weight; }
    char  *GetWeightString(void) { return wx_desc[weight]; }
    Bool  GetUnderlined(void)    { return underlined; }

    void  *GetInternalFont(float scale = 1.0); // return type XFontStruct*
//    void  GetPSFont(char **name, char **style, int *point_size, float scale = 1.0);
private:
    wxList *scaled_xfonts;
    short  point_size;
    short  family, style, weight;
    Bool   underlined;
    char   **wx_desc;
    int    font_id; /* MATTHEW */
};

class wxFontList : public wxList {
DECLARE_DYNAMIC_CLASS(wxFontList)
public:
    wxFontList(void) : wxList() { }
    ~wxFontList(void);

    void AddFont(wxFont *font)    { Append(font); }
    void RemoveFont(wxFont *font) { DeleteObject(font); }

    wxFont *FindOrCreateFont(int PointSize, int FontIdOrFamily, int Style, 
			     int Weight, Bool underline = FALSE);
    /* MATTTHEW */
    wxFont *FindOrCreateFont(int PointSize, const char *Face, int Family, 
			     int Style, int Weight,
			     Bool underline = FALSE);
};

#endif // Font_h
