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
					   Bool underlined, Bool size_in_pixels,
					   float angle);
#ifdef WX_USE_XFT
static wxFontStruct *wxLoadQueryNearestAAFont(int point_size, int fontid, int family,
					      int style, int weight, 
					      Bool underlined, int smoothing, 
					      Bool size_in_pixels,
					      float angle);
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
    rotation      = 0.0;

    InitFont();
}

wxFont::wxFont(int PointSize, int FontIdOrFamily, int Style, int Weight,
	       Bool Underlined, int Smoothing, Bool sip, float Rotation)
{
    font_id       = FontIdOrFamily;
    family        = wxTheFontNameDirectory->GetFamily(FontIdOrFamily);
    style         = Style;
    weight        = Weight == wxNORMAL ? wxNORMAL_WEIGHT : Weight;
    point_size    = PointSize;
    underlined    = Underlined;
    smoothing     = Smoothing;
    size_in_pixels = sip;
    rotation      = Rotation;

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
    rotation      = 0.0;

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

  if (rotated_fonts) {
    node = rotated_fonts->First();
    while (node) {
      wxFont *rot;
      wxNode *next;
      rot = (wxFont*)node->Data();
      next = node->Next();
      DELETE_OBJ rot;
      node = next;
    }
    DELETE_OBJ rotated_fonts;
  }
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
// rotation
//-----------------------------------------------------------------------------

int wxFont::CanRotate()
{
  return 1;
}

wxFont *wxFont::GetRotated(float angle)
{
  int int_angle = (int)(angle * 1000);
  wxNode *node;
  wxFont *rot;

  if (!rotated_fonts) {
    rotated_fonts = new wxList(wxKEY_INTEGER);
  }

  node = rotated_fonts->Find(int_angle);
  if (node)
    return (wxFont *)node->Data();

  rot = new wxFont(point_size, font_id, style, weight,
		   underlined, smoothing, size_in_pixels, angle);
  
  rotated_fonts->Append(int_angle, (wxObject*)rot);

  return rot;
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

void *wxFont::GetInternalFont(float scale, float angle)
{
  long        int_scale = (long)(scale * 100.0 + 0.5); // key for fontlist
  int         point_scale = (point_size * 10 * int_scale) / 100;
  wxNode      *node=NULL;
  XFontStruct *xfont;

  if (angle != rotation) {
    wxFont *rot;
    rot = GetRotated(angle);
    return rot->GetInternalFont(scale, angle);
  }

  if ((node = scaled_xfonts->Find(int_scale))) {
    xfont = (XFontStruct*)node->Data();
  } else {
    xfont = wxLoadQueryNearestFont(point_scale, font_id, family, style, weight,
				   underlined, size_in_pixels, angle);
    scaled_xfonts->Append(int_scale, (wxObject*)xfont);
  }
  return (void*)xfont;
}

//-----------------------------------------------------------------------------
// get internal representation of font
//-----------------------------------------------------------------------------

void *wxFont::GetInternalAAFont(float scale, float angle)
{
#ifdef WX_USE_XFT
  if (wxXRenderHere()) {
    long        int_scale = (long)(scale * 100.0 + 0.5); // key for fontlist
    int         point_scale = (point_size * int_scale) / 100;
    wxNode      *node=NULL;
    wxFontStruct *xft_font;

    if (angle != rotation) {
      wxFont *rot;
      rot = GetRotated(angle);
      return rot->GetInternalAAFont(scale, angle);
    }

    if ((node = scaled_xft_fonts->Find(int_scale))) {
      xft_font = (wxFontStruct*)node->Data();
    } else {
      xft_font = wxLoadQueryNearestAAFont(point_scale, font_id, family, style, weight,
					  underlined, smoothing, size_in_pixels, angle);

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
					      Bool underlined, int smoothing, Bool sip,
					      float angle)
{
  char *name;
  wxFontStruct *fs;

  name = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);

  if (name && (name[0] != ' '))
    /* Not an Xft font name */
    return NULL;
  
  {
    int sl, wt;
    const char *ex_tags[2];
    int ex_types[2];
    long ex_vals[2];
    int ex_pos = 0;
    XftMatrix rot;

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

    ex_tags[0] = NULL;
    ex_tags[1] = NULL;

    switch (smoothing) {
    case wxSMOOTHING_OFF:
      ex_vals[ex_pos] = 0;
      ex_types[ex_pos] = XftTypeBool;
      ex_tags[ex_pos++] = XFT_ANTIALIAS;
      break;
    case wxSMOOTHING_ON:
    case wxSMOOTHING_PARTIAL:
      ex_vals[ex_pos] = 1;
      ex_types[ex_pos] = XftTypeBool;
      ex_tags[ex_pos++] = XFT_ANTIALIAS;
      break;
    default:
      break;
    }

    if (angle) {
      XftMatrixInit(&rot);
      XftMatrixRotate(&rot, cos(angle), sin(angle));
      ex_vals[ex_pos] = (long)&rot;
      ex_types[ex_pos] = XftTypeMatrix;
      ex_tags[ex_pos++] = XFT_MATRIX;
    }
    
    if (name) {
      fs = XftFontOpen(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY),
		       XFT_FAMILY, XftTypeString, name + 1,
		       (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, point_size,
		       XFT_WEIGHT, XftTypeInteger, wt,
		       XFT_SLANT, XftTypeInteger, sl,
		       ex_tags[0], ex_types[0], ex_vals[0],
		       ex_tags[1], ex_types[1], ex_vals[1],
		       NULL);
    } else
      fs = NULL;

    if (!fs) {
      /* accept most any default: */
      fs = XftFontOpen(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY),
		       (sip ? XFT_PIXEL_SIZE : XFT_SIZE), XftTypeInteger, point_size,
		       XFT_WEIGHT, XftTypeInteger, wt,
		       XFT_SLANT, XftTypeInteger, sl,
		       ex_tags[0], ex_types[0], ex_vals[0],
		       ex_tags[1], ex_types[1], ex_vals[1],
		       NULL);
    }
  }
  
  return fs;
}

#endif

static XFontStruct *wxLoadQueryFont(int point_size, int fontid, int style,
				    int weight, Bool underlined, 
				    int si_try_again, Bool sip, float angle)
{
  char *buffer;
  char *name;
  long len, i, found = 0;
  XFontStruct *s;

  name = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);

  if (!name)
    name = "-*-*-*-*-*-*-*-%d-*-*-*-*-*-*";

  len = strlen(name);
  buffer = new char[len + 128];

  /* Make sure there's %d and no other format directives: */
  for (i = 0; i < len; i++) {
    if (name[i] == '%') {
      if (name[i + 1] == '%')
	i++;
      else if (name[i + 1] == 'd') {
	if (found)
	  return NULL;
	found = i + 1;
      } else
	return NULL;
    }
  }

  /* If the size is in pixels, try to change
     ...-*-%d-... to ...-%d-*-... */
  if (sip && found) {
    if ((found > 4) 
	&& (name[found+1] == '-')
	&& (name[found-2] == '-')
	&& (name[found-3] == '*')
	&& (name[found-4] == '-')) {
      char *rename;
      rename = new char[len + 1];
      memcpy(rename, name, len + 1);
      name = rename;
      name[found-3] = '%';
      name[found-2] = 'd';
      name[found-1] = '-';
      name[found] = '*';
    } else
      sip = 0;
  } else
    sip = 0;

  if (found && (angle != 0.0)) {
    /* Replace %d with %s: */
    char *rename, *matrix_str;
    double matrix[4];
    
    rename = new char[len + 1];
    memcpy(rename, name, len + 1);
    for (i = 0; i < len; i++) {
      if (rename[i] == '%') {
	if (rename[i + 1] == 'd') {
	  rename[i + 1] = 's';
	  break;
	}
	i++;
      }
    }

    matrix[0] = matrix[3] = ((double)point_size / 10) * cos(angle);
    matrix[1] = ((double)point_size / 10) * sin(angle);
    matrix[2] = -matrix[1];

    matrix_str = new char[128];
    sprintf(matrix_str, "[%g %g %g %g]", 
	    matrix[0], matrix[1],
	    matrix[2], matrix[3]);
    for (i = 0; matrix_str[i]; i++) {
      if (matrix_str[i] == '-')
	matrix_str[i] = '~';
    }
    
    sprintf(buffer, rename, matrix_str);
  } else {
    sprintf(buffer, name, sip ? point_size/10 : point_size);
  }

  s = XLoadQueryFont(wxAPP_DISPLAY, buffer);

  if (!s && si_try_again && ((style == wxSLANT) || (style == wxITALIC))) {
    /* Try slant/italic instead of italic/slant: */
    s = wxLoadQueryFont(point_size, fontid, 
			(style == wxSLANT) ? wxITALIC : wxSLANT, 
			weight, underlined, 0, sip, angle);
  }
  
  return s;
}

static XFontStruct *wxLoadQueryNearestFont(int point_size, int fontid, int family,
					   int style, int weight,
					   Bool underlined, Bool sip, float angle)
{
  XFontStruct *font;
  int tried_once = 0;

  while (1) {

    font = wxLoadQueryFont(point_size, fontid, style, weight, underlined, 1, sip, angle);

    if (!font) {
      // search up and down by stepsize 10
      int max_size = point_size + 20 * (1 + (point_size/180));
      int min_size = point_size - 20 * (1 + (point_size/180));
      int i;

      // Try plain style
      font = wxLoadQueryFont(point_size, fontid, wxNORMAL, wxNORMAL_WEIGHT, underlined, 1, sip, angle);

      // Search for smaller size (approx.)
      for (i=point_size-10; !font && i >= 10 && i >= min_size; i -= 10) {
	font = wxLoadQueryFont(i, fontid, style, weight, underlined, 1, sip, angle);
	if (!font)
	  font = wxLoadQueryFont(i, fontid,  wxNORMAL, wxNORMAL_WEIGHT, underlined, 1, sip, angle);
      }
      // Search for larger size (approx.)
      for (i=point_size+10; !font && i <= max_size; i += 10) {
	font = wxLoadQueryFont(i, fontid, style, weight, underlined, 1, sip, angle);
	if (!font)
	  font = wxLoadQueryFont(i, fontid,  wxNORMAL, wxNORMAL_WEIGHT, underlined, 1, sip, angle);
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
