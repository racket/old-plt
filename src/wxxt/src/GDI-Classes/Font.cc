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

#ifdef __GNUG__
#pragma implementation "Font.h"
#endif

#define  Uses_XLib
#define  Uses_wxFont
#define  Uses_wxFontDirectory
#define  Uses_wxWindowDC
#include "wx.h"

#ifdef WX_USE_XFT
# include <X11/Xft/Xft.h>
#endif


char *wx_font_spec [] = {
    "wxDEFAULT",
    // families
    "wxDECORATIVE", "wxMODERN", "wxROMAN", "wxSCRIPT", "wxSWISS", "wxTELETYPE",
    // style
    "wxNORMAL", "wxSLANT", "wxITALIC",
    // weight
    "wxNORMAL", "wxBOLD", "wxLIGHT",
    // More families
    "wxSYSTEM", "wxSYMBOL"
};

// local function prototypes
static XFontStruct *wxLoadQueryNearestFont(int point_size, int fontid, int family,
					   int style, int weight, 
					   Bool underlined, Bool size_in_pixels);
#ifdef WX_USE_XFT
static wxFontStruct *wxLoadQueryNearestAAFont(int point_size, int fontid, int family,
					      int style, int weight, 
					      Bool underlined, int smoothing, 
					      Bool size_in_pixels);
#endif

//-----------------------------------------------------------------------------
// wxFont create and destroy
//-----------------------------------------------------------------------------

wxFont::wxFont(void)
{
    font_id       = wxDEFAULT;
    family        = wxTheFontNameDirectory->GetFamily(font_id);
    style         = wxNORMAL;
    weight        = wxNORMAL_WEIGHT;
    point_size    = 12;
    underlined    = FALSE;

    InitFont();
}

wxFont::wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	       Bool Underlined, int Smoothing, Bool sip)
{
    font_id       = FontIdOrFamily;
    family        = wxTheFontNameDirectory->GetFamily(FontIdOrFamily);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    smoothing     = Smoothing;
    size_in_pixels = sip;

    InitFont();
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, 
	       int Weight, Bool Underlined, int Smoothing, Bool sip)
{
    font_id       = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);
    family        = wxTheFontNameDirectory->GetFamily(font_id);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    smoothing     = Smoothing;
    size_in_pixels = sip;

    InitFont();
}

void wxFont::InitFont(void)
{
  wxList *sl;
  
  __type = wxTYPE_FONT;
  
  sl            = new wxList(wxKEY_INTEGER);
  scaled_xfonts = sl;

#ifdef WX_USE_XFT
  sl            = new wxList(wxKEY_INTEGER);
  scaled_xft_fonts = sl;
#endif
}

wxFont::~wxFont(void)
{
  wxNode *node;
  node = scaled_xfonts->First();
  while (node) {
    XFontStruct *xfont;
    wxNode *next;
    xfont = (XFontStruct*)node->Data();
    next = node->Next();
    XFreeFont(wxAPP_DISPLAY, xfont);
    node = next;
  }
  DELETE_OBJ scaled_xfonts;

#ifdef  WX_USE_XFT
  node = scaled_xft_fonts->First();
  while (node) {
    wxFontStruct *xfont;
    wxNode *next;
    xfont = (wxFontStruct*)node->Data();
    next = node->Next();
    if (xfont != (wxFontStruct *)0x1)
      XftFontClose(wxAPP_DISPLAY, xfont);
    node = next;
  }
  DELETE_OBJ scaled_xft_fonts;
#endif
}

char *wxFont::GetFaceString(void)
{
  /* If it's one of the portable facelss fonts, return NULL. */
  switch (font_id) {
  case wxDEFAULT:
  case wxDECORATIVE:
  case wxMODERN:
  case wxROMAN:
  case wxSCRIPT:
  case wxSWISS:
  case wxTELETYPE:
  case wxSYSTEM:
  case wxSYMBOL:
    return NULL;
  default:
    return wxTheFontNameDirectory->GetFontName(font_id); 
  }
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

void *wxFont::GetInternalFont(float scale)
{
    long        int_scale = (long)(scale * 100.0 + 0.5); // key for fontlist
    int         point_scale = (point_size * 10 * int_scale) / 100;
    wxNode      *node=NULL;
    XFontStruct *xfont;

    if ((node = scaled_xfonts->Find(int_scale))) {
      xfont = (XFontStruct*)node->Data();
    } else {
      xfont = wxLoadQueryNearestFont(point_scale, font_id, family, style, weight,
				     underlined, size_in_pixels);
      scaled_xfonts->Append(int_scale, (wxObject*)xfont);
    }
    return (void*)xfont;
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

void *wxFont::GetInternalAAFont(float scale)
{
#ifdef WX_USE_XFT
  if (wxXRenderHere()) {
    long        int_scale = (long)(scale * 100.0 + 0.5); // key for fontlist
    int         point_scale = (point_size * int_scale) / 100;
    wxNode      *node=NULL;
    wxFontStruct *xft_font;

    if ((node = scaled_xft_fonts->Find(int_scale))) {
      xft_font = (wxFontStruct*)node->Data();
    } else {
      xft_font = wxLoadQueryNearestAAFont(point_scale, font_id, family, style, weight,
					  underlined, smoothing, size_in_pixels);

      /* Record a 0x1 to mean "no AA font": */
      if (!xft_font)
	xft_font = (wxFontStruct*)0x1;

      scaled_xft_fonts->Append(int_scale, (wxObject*)xft_font);
    }
    if (xft_font == (wxFontStruct*)0x1)
      return NULL;    
    return (void*)xft_font;
  } else
    return NULL;
#else
    return GetInternalFont(scale);
#endif
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
  list->Show(Font, -1); /* so it can be collected */
} 

//-----------------------------------------------------------------------------
// search for font in fontlist
//-----------------------------------------------------------------------------

wxFont *wxFontList::FindOrCreateFont(int PointSize, int FontIdOrFamily, 
				     int Style, int Weight, Bool underline,
				     int smoothing, Bool sip)
{
  wxFont *font;
  wxChildNode *node;
  int i = 0;
  
  while ((node = list->NextNode(i))) {
    wxFont *each_font;
    each_font = (wxFont*)node->Data();
    if (each_font &&
	each_font->GetPointSize() == PointSize &&
	each_font->GetStyle() == Style &&
	each_font->GetWeight() == Weight &&
	each_font->GetFontId() == FontIdOrFamily &&
	each_font->GetUnderlined() == underline &&
	each_font->GetSmoothing() == smoothing &&
	each_font->GetSizeInPixels() == sip)
      return each_font;
  }
  
  font = new wxFont(PointSize, FontIdOrFamily, Style, Weight, underline, smoothing, sip);

#if WXGARBAGE_COLLECTION_ON
  AddFont(font);
#endif

  return font;
}

wxFont *wxFontList::FindOrCreateFont(int PointSize, const char *Face, 
				     int Family, int Style, int Weight, 
				     Bool underline, int smoothing, Bool sip)
{
  int id;
  id = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);

  return FindOrCreateFont(PointSize,
			  id,
			  Style,
			  Weight,
			  underline,
			  smoothing,
			  sip);
}

//-----------------------------------------------------------------------------
// local utilities
//-----------------------------------------------------------------------------

#ifdef WX_USE_XFT

static wxFontStruct *wxLoadQueryNearestAAFont(int point_size, int fontid, int family,
					      int style, int weight,
					      Bool underlined, int smoothing, Bool sip)
{
  char *name;
  wxFontStruct *fs;

  name = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);

  if (name && (name[0] != ' '))
    /* Not an Xft font name */
    return NULL;
  
  {
    int sl, wt, aa;
    const char *aa_tag = XFT_ANTIALIAS;

    wt = ((weight == wxBOLD)
	  ? XFT_WEIGHT_BOLD
	  : ((weight == wxLIGHT)
	     ? XFT_WEIGHT_LIGHT
	     : XFT_WEIGHT_MEDIUM));
    sl = ((style == wxITALIC)
	  ? XFT_SLANT_ITALIC
	  : ((weight == wxSLANT)
	     ? XFT_SLANT_OBLIQUE
	     : XFT_SLANT_ROMAN));

    switch (smoothing) {
    case wxSMOOTHING_OFF:
      aa = 0;
      break;
    case wxSMOOTHING_ON:
    case wxSMOOTHING_PARTIAL:
      aa = 1;
      break;
    default:
      aa_tag = NULL;
      aa = 0;
      break;
    }
    
    if (name) {
      fs = XftFontOpen(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY),
		       XFT_FAMILY, XftTypeString, name + 1,
		       (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, point_size,
		       XFT_WEIGHT, XftTypeInteger, wt,
		       XFT_SLANT, XftTypeInteger, sl,
		       /* aa tag must be last, b/c is might be 0ed: */
		       aa_tag, XftTypeBool, aa,
		       NULL);
    } else
      fs = NULL;

    if (!fs) {
      /* accept most any default: */
      fs = XftFontOpen(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY),
		       (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, point_size,
		       XFT_WEIGHT, XftTypeInteger, wt,
		       XFT_SLANT, XftTypeInteger, sl,
		       /* aa tag must be last, b/c is might be 0ed: */
		       aa_tag, XftTypeBool, aa,
		       NULL);
    }
  }
  
  return fs;
}

#endif

static XFontStruct *wxLoadQueryFont(int point_size, int fontid, int style,
				    int weight, Bool underlined, 
				    int si_try_again)
{
  char buffer[512];
  char *name;
  XFontStruct *s;

  name = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);

  if (!name)
    name = "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*";

  sprintf(buffer, name, point_size);

  s = XLoadQueryFont(wxAPP_DISPLAY, buffer);

  if (!s && si_try_again && ((style == wxSLANT) || (style == wxITALIC))) {
    /* Try slant/italic instead of italic/slant: */
    s = wxLoadQueryFont(point_size, fontid, 
			(style == wxSLANT) ? wxITALIC : wxSLANT, 
			weight, underlined, 0);
  }
  
  return s;
}

static XFontStruct *wxLoadQueryNearestFont(int point_size, int fontid, int family,
					   int style, int weight,
					   Bool underlined, Bool sip)
{
  XFontStruct *font;
  int tried_once = 0;

  while (1) {

    font = wxLoadQueryFont(point_size, fontid, style, weight, underlined, 1);

    if (!font) {
      // search up and down by stepsize 10
      int max_size = point_size + 20 * (1 + (point_size/180));
      int min_size = point_size - 20 * (1 + (point_size/180));
      int i;

      // Try plain style
      font = wxLoadQueryFont(point_size, fontid, wxNORMAL, wxNORMAL_WEIGHT, underlined, 1);

      // Search for smaller size (approx.)
      for (i=point_size-10; !font && i >= 10 && i >= min_size; i -= 10) {
	font = wxLoadQueryFont(i, fontid, style, weight, underlined, 1);
	if (!font)
	  font = wxLoadQueryFont(i, fontid,  wxNORMAL, wxNORMAL_WEIGHT, underlined, 1);
      }
      // Search for larger size (approx.)
      for (i=point_size+10; !font && i <= max_size; i += 10) {
	font = wxLoadQueryFont(i, fontid, style, weight, underlined, 1);
	if (!font)
	  font = wxLoadQueryFont(i, fontid,  wxNORMAL, wxNORMAL_WEIGHT, underlined, 1);
      }
    }
    
    if (font || tried_once)
      break;
    else {
      tried_once = 1;
      fontid = family;
    }
  }

  /* Last-ditch efforts */
  if (!font) {
    char buffer[40];
    sprintf(buffer, "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*", point_size);
    font = XLoadQueryFont(wxAPP_DISPLAY, buffer);
    
    if (!font) /* really last-ditch */
      font = XLoadQueryFont(wxAPP_DISPLAY, "-*-*-*-*-*-*-*-*-*-*-*-*-*-*");
  }

  return font;
}
