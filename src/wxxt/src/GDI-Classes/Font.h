/*								-*- C++ -*-
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

extern char *wx_font_spec[];

#ifdef WX_USE_XFT
# define wxFontStruct XftFont
#else
# define wxFontStruct XFontStruct
#endif

class wxFont : public wxObject {
public:
    wxFont(void);
    wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	   Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT);
    wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	   Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT);
    ~wxFont(void);

    void InitFont(void);

    int   GetPointSize(void)     { return point_size; }
    int   GetFamily(void)        { return family; }
    char  *GetFamilyString(void) { return wx_font_spec[family]; }
    int   GetFontId(void)        { return font_id; }
    char  *GetFaceString(void);
    int   GetStyle(void)         { return style; }
    char  *GetStyleString(void)  { return wx_font_spec[style]; }
    int   GetWeight(void)	 { return weight==wxNORMAL_WEIGHT ? wxNORMAL : weight; }
    char  *GetWeightString(void) { return wx_font_spec[weight]; }
    Bool  GetUnderlined(void)    { return underlined; }
    int   GetSmoothing(void)     { return smoothing; }

    void  *GetInternalFont(float scale = 1.0); // return type XFontStruct*
    void  *GetInternalAAFont(float scale = 1.0); // return type wxFontStruct*
private:
    wxList *scaled_xfonts;
#ifdef WX_USE_XFT
    wxList *scaled_xft_fonts;
#endif
    short  point_size;
    short  family, style, weight;
    Bool   underlined;
    int    font_id;
    int    smoothing;
};

class wxFontList : public wxObject {
   wxChildList *list;
public:
    wxFontList(void);
    ~wxFontList(void);

    void AddFont(wxFont *font);

    wxFont *FindOrCreateFont(int PointSize, int FontIdOrFamily, int Style, 
			     int Weight, Bool underline = FALSE,
			     int smoothing = wxSMOOTHING_DEFAULT);
    wxFont *FindOrCreateFont(int PointSize, const char *Face, int Family, 
			     int Style, int Weight,
			     Bool underline = FALSE,
			     int smoothing = wxSMOOTHING_DEFAULT);
};

#endif // Font_h
