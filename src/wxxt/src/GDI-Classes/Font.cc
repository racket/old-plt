 /*								-*- C++ -*-
 * $Id: Font.cc,v 1.1.1.1 1997/12/22 17:28:51 mflatt Exp $
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

#ifdef __GNUG__
#pragma implementation "Font.h"
#endif

#define  Uses_XLib
#define  Uses_wxFont
#define  Uses_wxFontDirectory
#include "wx.h"

static char *wx_font_spec [] = {
    "wxDEFAULT",
    // families
    "wxDECORATIVE", "wxMODERN", "wxROMAN", "wxSCRIPT", "wxSWISS", "wxTELETYPE",
    // style
    "wxNORMAL", "wxSLANT", "wxITALIC",
    // weight
    "wxNORMAL", "wxBOLD", "wxLIGHT",
};

// local function prototypes
static XFontStruct *wxLoadQueryFont(int point_size, int fontid, int style,
				    int weight, Bool underlined);
static XFontStruct *wxLoadQueryNearestFont(int point_size, int fontid,
					   int style, int weight, 
					   Bool underlined);

IMPLEMENT_DYNAMIC_CLASS(wxFont, wxObject)
IMPLEMENT_DYNAMIC_CLASS(wxFontList, wxList)

//-----------------------------------------------------------------------------
// wxFont create and destroy
//-----------------------------------------------------------------------------

wxFont::wxFont(void)
{
    __type = wxTYPE_FONT;

    font_id       = wxDEFAULT;
    family        = wxTheFontNameDirectory.GetFamily(font_id);
    style         = wxNORMAL;
    weight        = wxNORMAL_WEIGHT;
    point_size    = 12;
    underlined    = FALSE;

    wx_desc       = wx_font_spec;
    scaled_xfonts = new wxList(wxKEY_INTEGER);

#if !WXGARBAGE_COLLECTION_ON
    wxTheFontList->Append(this);
#endif
}

wxFont::wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	       Bool Underlined)
{
    __type = wxTYPE_FONT;

    font_id       = FontIdOrFamily;
    family        = wxTheFontNameDirectory.GetFamily(FontIdOrFamily);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    wx_desc       = wx_font_spec;
    scaled_xfonts = new wxList(wxKEY_INTEGER);

#if !WXGARBAGE_COLLECTION_ON
    wxTheFontList->Append(this);
#endif
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, 
	       int Weight, Bool Underlined)
{
    __type = wxTYPE_FONT;

    font_id       = wxTheFontNameDirectory.FindOrCreateFontId(Face, Family);
    family        = wxTheFontNameDirectory.GetFamily(font_id);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    wx_desc       = wx_font_spec;
    scaled_xfonts = new wxList(wxKEY_INTEGER);

#if !WXGARBAGE_COLLECTION_ON
    wxTheFontList->Append(this);
#endif
}

wxFont::~wxFont(void)
{
    wxNode *node = scaled_xfonts->First();
    while (node) {
	XFontStruct *xfont = (XFontStruct*)node->Data();
	wxNode *next = node->Next();
	XFreeFont(wxAPP_DISPLAY, xfont);
	node = next;
    }
    delete scaled_xfonts;
#if !WXGARBAGE_COLLECTION_ON
    wxTheFontList->DeleteObject(this);
#endif
}

char *wxFont::GetFaceString(void)
{
  return wxTheFontNameDirectory.GetFontName(font_id); 
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

void *wxFont::GetInternalFont(float scale)
{
    long        int_scale = long(scale * 100.0 + 0.5); // key for fontlist
    int         point_scale = (point_size * 10 * int_scale) / 100;
    wxNode      *node=NULL;
    XFontStruct *xfont;

    if ((node = scaled_xfonts->Find(int_scale))) {
	xfont = (XFontStruct*)node->Data();
    } else {
	xfont = wxLoadQueryNearestFont(point_scale, font_id, style, weight,
				       underlined);
	scaled_xfonts->Append(int_scale, (wxObject*)xfont);
    }
    return (void*)xfont;
}

//-----------------------------------------------------------------------------
// wxFontList
//-----------------------------------------------------------------------------

wxFontList::wxFontList(void)
: wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxFontList::~wxFontList(void)
{
}

void wxFontList::AddFont(wxFont *Font) 
{ 
  list->Append(Font); 
  list->Show(Font, FALSE); /* so it can be collected */
} 

//-----------------------------------------------------------------------------
// search for font in fontlist
//-----------------------------------------------------------------------------

wxFont *wxFontList::FindOrCreateFont(int PointSize, int FontIdOrFamily, 
				     int Style, int Weight, Bool underline)
{
  wxFont *font;
  int i = 0;
  
  while (wxChildNode *node = list->NextNode(i)) {
    wxFont *each_font = (wxFont*)node->Data();
    if (each_font &&
	each_font->GetPointSize() == PointSize &&
	each_font->GetStyle() == Style &&
	each_font->GetWeight() == Weight &&
	each_font->GetFontId() == FontIdOrFamily &&
	each_font->GetUnderlined() == underline)
      return each_font;
  }
  
  font = new wxFont(PointSize, FontIdOrFamily, Style, Weight, underline);

#if WXGARBAGE_COLLECTION_ON
  AddFont(font);
#endif

  return font;
}

wxFont *wxFontList::FindOrCreateFont(int PointSize, const char *Face, 
				     int Family, int Style, int Weight, 
				     Bool underline)
{
  return FindOrCreateFont(PointSize,
			  wxTheFontNameDirectory.FindOrCreateFontId(Face, 
								    Family),
			  Style,
			  Weight,
			  underline);
}

//-----------------------------------------------------------------------------
// local utilities
//-----------------------------------------------------------------------------

static XFontStruct *wxLoadQueryFont(int point_size, int fontid, int style,
				    int weight, Bool WXUNUSED(underlined))
{
  char buffer[512];
  char *name = wxTheFontNameDirectory.GetScreenName(fontid, weight, style);

  if (!name)
    name = "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*";

  sprintf(buffer, name, point_size);
  
  return XLoadQueryFont(wxAPP_DISPLAY, buffer);
}

static XFontStruct *wxLoadQueryNearestFont(int point_size, int fontid,
					   int style, int weight,
					   Bool underlined)
{
    XFontStruct *font;

    font = wxLoadQueryFont(point_size, fontid, style, weight, underlined);

    if (!font) {
	// search up and down by stepsize 10
	int max_size = point_size + 20 * (1 + (point_size/180));
	int min_size = point_size - 20 * (1 + (point_size/180));
	int i;

	// Search for smaller size (approx.)
	for (i=point_size-10; !font && i >= 10 && i >= min_size; i -= 10)
	    font = wxLoadQueryFont(i, fontid, style, weight, underlined);
	// Search for larger size (approx.)
	for (i=point_size+10; !font && i <= max_size; i += 10)
	    font = wxLoadQueryFont(i, fontid, style, weight, underlined);
	// Try default family
	if (!font && fontid != wxDEFAULT)
	    font = wxLoadQueryFont(point_size, wxDEFAULT, style, 
				   weight, underlined);
	// Bogus font
	if (!font)
	    font = wxLoadQueryFont(120, wxDEFAULT, wxNORMAL, wxNORMAL_WEIGHT,
				    underlined);

	/* MATTHEW: [6] Last-ditch efforts */
	if (!font) {
	  char buffer[40];
	  sprintf(buffer, "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*", point_size);
	  font = XLoadQueryFont(wxAPP_DISPLAY, buffer);
	  
	  if (!font) /* really last-ditch */
	    font = XLoadQueryFont(wxAPP_DISPLAY, "-*-*-*-*-*-*-*-*-*-*-*-*-*-*");
	}
    }
    return font;
}
