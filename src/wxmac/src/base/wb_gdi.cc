/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_gdi.cc,v 1.18 1999/11/13 15:11:42 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_gdi.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#ifdef wx_x
extern Colormap wxMainColormap;
#endif
#ifdef wx_xview
extern Xv_Server xview_server;
#endif
#if USE_IMAGE_LOADING_IN_MAC
#include "wx_image.h"
#endif


wxbFont::wxbFont (void)
{
  __type = wxTYPE_FONT;
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxbFont::wxbFont (int PointSize, int Family, int Style, int Weight, Bool Underline)
{
  __type = wxTYPE_FONT;
}

wxbFont::~wxbFont ()
{
}

char *wxbFont::GetFamilyString(void)
{
  char *fam = NULL;
  switch (GetFamily())
  {
    case wxDECORATIVE:
      fam = "wxDECORATIVE";
      break;
    case wxROMAN:
      fam = "wxROMAN";
      break;
    case wxSCRIPT:
      fam = "wxSCRIPT";
      break;
    case wxSWISS:
      fam = "wxSWISS";
      break;
    case wxMODERN:
      fam = "wxMODERN";
      break;
    case wxTELETYPE:
      fam = "wxTELETYPE";
      break;
    case wxSYSTEM:
      fam = "wxSYSTEM";
      break;
    case wxSYMBOL:
      fam = "wxSYMBOL";
      break;
    default:
      fam = "wxDEFAULT";
      break;
  }
  return fam;
}

char *wxbFont::GetFaceString(void)
{
  return wxTheFontNameDirectory.GetFontName(fontid); 
}

char *wxbFont::GetStyleString(void)
{
  char *styl = NULL;
  switch (GetStyle())
  {
    case wxITALIC:
      styl = "wxITALIC";
      break;
    case wxSLANT:
      styl = "wxSLANT";
      break;
    default:
      styl = "wxNORMAL";
      break;
  }
  return styl;
}

char *wxbFont::GetWeightString(void)
{
  char *w = NULL;
  switch (GetWeight())
  {
    case wxBOLD:
      w = "wxBOLD";
      break;
    case wxLIGHT:
      w = "wxLIGHT";
      break;
    default:
      w = "wxNORMAL";
      break;
  }
  return w;
}

// Colour

wxColour::wxColour (void)
{
  __type = wxTYPE_COLOUR;
  isInit = FALSE;
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = 0;
#endif
#ifdef wx_mac
  red = 0; green = 0; blue = 0;
  pixel.red = 0;
  pixel.green = 0;
  pixel.blue = 0;
#endif
  locked = 0;
//  wxTheColourList->Append (this);
}

wxColour::wxColour (unsigned char r, unsigned char g, unsigned char b)
{
  __type = wxTYPE_COLOUR;
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
  locked = 0;
//  wxTheColourList->Append (this);
}

wxColour::wxColour (const char *col)
{
  __type = wxTYPE_COLOUR;
  wxColour *the_colour = wxTheColourDatabase->FindColour (col);
  if (the_colour)
    {
      red = the_colour->Red ();
      green = the_colour->Green ();
      blue = the_colour->Blue ();
      isInit = TRUE;
    }
  else
    {
      red = 0;
      green = 0;
      blue = 0;
      isInit = FALSE;
    }
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
//  wxTheColourList->Append (this);
  locked = 0;
}

wxColour::~wxColour (void)
{
//  wxTheColourList->DeleteObject (this);
}

wxColour *wxColour::CopyFrom(wxColour *src)
{
  red = src->red;
  green = src->green;
  blue = src->blue;
  pixel = src->pixel;
  isInit = src->isInit;

  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;

  return this;
}


wxColour &wxColour::operator =(wxColour &src)
{
  CopyFrom(&src);

  return this;
}

wxColour& wxColour::operator = (const char *col)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour (col);
  if (the_colour)
    {
      red = the_colour->Red ();
      green = the_colour->Green ();
      blue = the_colour->Blue ();
      isInit = TRUE;
    }
  else
    {
      red = 0;
      green = 0;
      blue = 0;
      isInit = FALSE;
    }

  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;

  return (*this);
}

void wxColour::Set (unsigned char r, unsigned char g, unsigned char b)
{
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;

  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
}

void wxColour::Get (unsigned char *r, unsigned char *g, unsigned char *b)
{
  *r = red;
  *g = green;
  *b = blue;
}

wxColourDatabase::wxColourDatabase(KeyType type) : wxList(type)
{
}

wxColourDatabase::~wxColourDatabase (void)
{
  // Cleanup Colour allocated in Initialize()
  wxNode *node = First ();
  while (node) {
    wxColour *col;
    wxNode *next;
    col = (wxColour *) node->Data ();
    next = node->Next ();
    delete col;
    node = next;
  }
}

// Colour database stuff
void wxColourDatabase::Initialize (void)
{
  // Don't initialize for X: colours are found
  // in FindColour below.
#if defined(wx_msw) || defined(wx_mac)
#define APPEND(name, c) tmpc = c; tmpc->Lock(1); Append(name, tmpc)
  wxColour *tmpc;
  APPEND("AQUAMARINE", new wxColour(112, 216, 144));
  APPEND("BLACK", new wxColour(0, 0, 0));
  APPEND("BLUE", new wxColour(80, 80, 248));
  APPEND("BLUE VIOLET", new wxColour(138, 43, 226));
  APPEND("BROWN", new wxColour(132, 60, 36));
  APPEND("CADET BLUE", new wxColour(96, 160, 160));
  APPEND("CORAL", new wxColour(255, 127, 80));
  APPEND("CORNFLOWER BLUE", new wxColour(68, 64, 108));
  APPEND("CYAN", new wxColour(0, 255, 255));
  APPEND("DARK GRAY", new wxColour(169, 169, 169));
  APPEND("DARK GREEN", new wxColour(0, 100, 0));
  APPEND("DARK OLIVE GREEN", new wxColour(85, 107, 47));
  APPEND("DARK ORCHID", new wxColour(153, 50, 204));
  APPEND("DARK SLATE BLUE", new wxColour(72, 61, 139));
  APPEND("DARK SLATE GRAY", new wxColour(47, 79, 79));
  APPEND("DARK TURQUOISE", new wxColour(0, 206, 209));
  APPEND("DIM GRAY", new wxColour(105, 105, 105));
  APPEND("FIREBRICK", new wxColour(178, 34, 34));
  APPEND("FOREST GREEN", new wxColour(34, 139, 34));
  APPEND("GOLD", new wxColour(255, 215, 0));
  APPEND("GOLDENROD", new wxColour(218, 165, 32));
  APPEND("GRAY", new wxColour(190, 190, 190));
  APPEND("GREEN", new wxColour(60, 248, 52));
  APPEND("GREEN YELLOW", new wxColour(173, 255, 47));
  APPEND("INDIAN RED", new wxColour(205, 92, 92));
  APPEND("KHAKI", new wxColour(240, 230, 140));
  APPEND("LIGHT BLUE", new wxColour(173, 216, 230));
  APPEND("LIGHT GRAY", new wxColour(211, 211, 211));
  APPEND("LIGHT STEEL BLUE", new wxColour(176, 196, 222));
  APPEND("LIME GREEN", new wxColour(50, 205, 50));
  APPEND("MAGENTA", new wxColour(255, 0, 255));
  APPEND("MAROON", new wxColour(176, 48, 96));
  APPEND("MEDIUM AQUAMARINE", new wxColour(102, 205, 170));
  APPEND("MEDIUM BLUE", new wxColour(0, 0, 205));
  APPEND("MEDIUM FOREST GREEN", new wxColour(107, 142, 35));
  APPEND("MEDIUM GOLDENROD", new wxColour(234, 234, 173));
  APPEND("MEDIUM ORCHID", new wxColour(186, 85, 211));
  APPEND("MEDIUM SEA GREEN", new wxColour(60, 179, 113));
  APPEND("MEDIUM SLATE BLUE", new wxColour(123, 104, 238));
  APPEND("MEDIUM SPRING GREEN", new wxColour(0, 250, 154));
  APPEND("MEDIUM TURQUOISE", new wxColour(72, 209, 204));
  APPEND("MEDIUM VIOLET RED", new wxColour(199, 21, 133));
  APPEND("MIDNIGHT BLUE", new wxColour(25, 25, 112));
  APPEND("NAVY", new wxColour(36, 36, 140));
  APPEND("ORANGE", new wxColour(255, 165, 0));
  APPEND("ORANGE RED", new wxColour(255, 69, 0));
  APPEND("ORCHID", new wxColour(218, 112, 214));
  APPEND("PALE GREEN", new wxColour(152, 251, 152));
  APPEND("PINK", new wxColour(255, 192, 203));
  APPEND("PLUM", new wxColour(221, 160, 221));
  APPEND("PURPLE", new wxColour(160, 32, 240));
  APPEND("RED", new wxColour(248, 20, 64));
  APPEND("SALMON", new wxColour(250, 128, 114));
  APPEND("SEA GREEN", new wxColour(46, 139, 87));
  APPEND("SIENNA", new wxColour(160, 82, 45));
  APPEND("SKY BLUE", new wxColour(135, 206, 235));
  APPEND("SLATE BLUE", new wxColour(106, 90, 205));
  APPEND("SPRING GREEN", new wxColour(0, 255, 127));
  APPEND("STEEL BLUE", new wxColour(70, 130, 180));
  APPEND("TAN", new wxColour(210, 180, 140));
  APPEND("THISTLE", new wxColour(216, 191, 216));
  APPEND("TURQUOISE", new wxColour(64, 224, 208));
  APPEND("VIOLET", new wxColour(238, 130, 238));
  APPEND("VIOLET RED", new wxColour(208, 32, 144));
  APPEND("WHEAT", new wxColour(245, 222, 179));
  APPEND("WHITE", new wxColour(255, 255, 255));
  APPEND("YELLOW", new wxColour(255, 255, 0));
  APPEND("YELLOW GREEN", new wxColour(154, 205, 50));
#endif
}

/*
 * Changed by Ian Brown, July 1994.
 *
 * When running under X, the Colour Database starts off empty. The X server
 * is queried for the colour first time after which it is entered into the
 * database. This allows our client to use the server colour database which
 * is hopefully gamma corrected for the display being used.
 */

wxColour *wxColourDatabase::FindColour(const char *colour)
{
  const char *p;
  
  // Insure upcased:
  for (p = colour; *p && !islower(*p); p++);
  if (*p) {
  	char *naya = new char[strlen(colour) + 1], *q;
  	for (p = colour, q = naya; *p; p++, q++)
  		*q = toupper(*p);
  	*q = 0;
  	colour = naya;
  }

  wxNode *node = Find(colour);
  if (node)
    return (wxColour *)node->Data();
  else 
    return NULL;
}

char *wxColourDatabase::FindName (wxColour *colour)
{
  unsigned char red = colour->Red();
  unsigned char green = colour->Green();
  unsigned char blue = colour->Blue();

  for (wxNode * node = First (); node; node = node->Next ())
    {
      wxColour *col = (wxColour *) node->Data ();
      if (col->Red () == red && col->Green () == green && col->Blue () == blue)
	{
	  char *found = node->key.string;
	  if (found)
	    return found;
	}
    }
  return NULL;			// Not Found

}


void 
wxInitializeStockObjects (void)
{
  wxTheBrushList = new wxBrushList;
  wxThePenList = new wxPenList;
  wxTheFontList = new wxFontList;
  wxTheBitmapList = new wxGDIList;
  wxTheCursorList = new wxGDIList;

  wxNORMAL_FONT = new wxFont (12, wxSYSTEM, wxNORMAL, wxNORMAL);
  wxSMALL_FONT = new wxFont (10, wxSWISS, wxNORMAL, wxNORMAL);
  wxITALIC_FONT = new wxFont (12, wxROMAN, wxITALIC, wxNORMAL);
  wxSWISS_FONT = new wxFont (12, wxSWISS, wxNORMAL, wxNORMAL);

  wxRED_PEN = new wxPen ("RED", 0, wxSOLID);
  wxCYAN_PEN = new wxPen ("CYAN", 0, wxSOLID);
  wxGREEN_PEN = new wxPen ("GREEN", 0, wxSOLID);
  wxBLACK_PEN = new wxPen ("BLACK", 0, wxSOLID);
  wxWHITE_PEN = new wxPen ("WHITE", 0, wxSOLID);
  wxTRANSPARENT_PEN = new wxPen ("BLACK", 0, wxTRANSPARENT);
  wxBLACK_DASHED_PEN = new wxPen ("BLACK", 0, wxSHORT_DASH);
  wxGREY_PEN = new wxPen ("GRAY", 0, wxSOLID);
  wxMEDIUM_GREY_PEN = new wxPen ("MEDIUM GRAY", 0, wxSOLID);
  wxLIGHT_GREY_PEN = new wxPen ("LIGHT GRAY", 0, wxSOLID);

  wxWHITE_PEN->Lock(1);
  wxBLACK_PEN->Lock(1);

  wxBLUE_BRUSH = new wxBrush ("BLUE", wxSOLID);
  wxGREEN_BRUSH = new wxBrush ("GREEN", wxSOLID);
  wxWHITE_BRUSH = new wxBrush ("WHITE", wxSOLID);
  wxBLACK_BRUSH = new wxBrush ("BLACK", wxSOLID);
  wxTRANSPARENT_BRUSH = new wxBrush ("BLACK", wxTRANSPARENT);
  wxCYAN_BRUSH = new wxBrush ("CYAN", wxSOLID);
  wxRED_BRUSH = new wxBrush ("RED", wxSOLID);
  wxGREY_BRUSH = new wxBrush ("GRAY", wxSOLID);
  wxMEDIUM_GREY_BRUSH = new wxBrush ("MEDIUM GRAY", wxSOLID);
  wxLIGHT_GREY_BRUSH = new wxBrush ("LIGHT GRAY", wxSOLID);
  
  wxWHITE_BRUSH->Lock(1);
  wxBLACK_BRUSH->Lock(1);
  
  wxColour ctlGray(0xE8, 0xE8, 0xE8);
  wxCONTROL_BACKGROUND_BRUSH = new wxBrush(ctlGray, wxSOLID);
  wxCONTROL_BACKGROUND_BRUSH->Lock(1);

  wxBLACK = new wxColour ("BLACK");
  wxWHITE = new wxColour ("WHITE");
  wxRED = new wxColour ("RED");
  wxBLUE = new wxColour ("BLUE");
  wxGREEN = new wxColour ("GREEN");
  wxCYAN = new wxColour ("CYAN");
  wxLIGHT_GREY = new wxColour ("LIGHT GRAY");

  wxSTANDARD_CURSOR = new wxCursor (wxCURSOR_ARROW);
  wxHOURGLASS_CURSOR = new wxCursor (wxCURSOR_WAIT);
  wxCROSS_CURSOR = new wxCursor (wxCURSOR_CROSS);
  wxIBEAM_CURSOR = new wxCursor (wxCURSOR_IBEAM);
}

void 
wxDeleteStockObjects (void)
{
  if (wxBLACK)
    delete wxBLACK;
  if (wxWHITE)
    delete wxWHITE;
  if (wxRED)
    delete wxRED;
  if (wxBLUE)
    delete wxBLUE;
  if (wxGREEN)
    delete wxGREEN;
  if (wxCYAN)
    delete wxCYAN;
  if (wxLIGHT_GREY)
    delete wxLIGHT_GREY;

  if (wxSTANDARD_CURSOR)
    delete wxSTANDARD_CURSOR;
  if (wxHOURGLASS_CURSOR)
    delete wxHOURGLASS_CURSOR;
  if (wxCROSS_CURSOR)
    delete wxCROSS_CURSOR;
}

// Pens

wxbPen::wxbPen (void)
{
  __type = wxTYPE_PEN;
  locked = 0;
}

wxbPen::~wxbPen ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbPen::wxbPen (wxColour& col, int Width, int Style)
{
  locked = 0;
  __type = wxTYPE_PEN;
}

wxbPen::wxbPen (const char *col, int Width, int Style)
{
  locked = 0;
  __type = wxTYPE_PEN;
}

int wxbPen::GetWidth (void)
{
  return width;
}

int wxbPen::GetStyle (void)
{
  return style;
}

int wxbPen::GetJoin (void)
{
  return join;
}

wxBitmap *wxbPen::GetStipple (void)
{
  return stipple;
}

int wxbPen::GetCap (void)
{
  return cap;
}

int wxbPen::GetDashes (wxDash ** ptr)
{
  *ptr = dash;
  return nb_dash;
}

wxColour *wxbPen::GetColour (void)
{
  return &colour;
}

void wxbPen::SetColour (wxColour *col)
{
  colour.CyopFrom(col);
}

void wxbPen::SetColour (const char *col)
{
  colour = col;
}

void wxbPen::SetColour (char red, char green, char blue)
{
 colour.Set(red, green, blue);
}

void wxbPen::SetWidth (int Width)
{
  width = Width;
}

void wxbPen::SetCap (int Cap)
{
  cap = Cap;
}

void wxbPen::SetJoin (int Join)
{
  join = Join;
}

void wxbPen::SetStyle (int Style)
{
  style = Style;
}

void wxbPen::SetDashes (int nbDash, wxDash * Dash)
{
  nb_dash = nbDash;
  dash = Dash;
}

void wxbPen::SetStipple (wxBitmap * Stipple)
{
  if (Stipple && (Stipple->selectedIntoDC < 0))
    return;
  if (Stipple)
    Stipple->selectedIntoDC++;
  if (stipple)
    --stipple->selectedIntoDC;

  stipple = Stipple;
}

// Brushes

wxbBrush::wxbBrush (void)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::~wxbBrush ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbBrush::wxbBrush (wxColour *col, int Style)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::wxbBrush (char *col, int Style)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

int wxbBrush::GetStyle (void)
{
  return style;
}

wxBitmap *wxbBrush::GetStipple (void)
{
  return stipple;
}

wxColour *wxbBrush::GetColour (void)
{
  return &colour;
}

void wxbBrush::SetColour (wxColour *col)
{
  colour.CopyFrom(col);
}

void wxbBrush::SetColour (const char *col)
{
  colour = col;
}

void wxbBrush::SetColour (char red, char green, char blue)
{
  colour.Set(red, green, blue);
}

void wxbBrush::SetStyle (int Style)
{
  style = Style;
}

void wxbBrush::SetStipple (wxBitmap * Stipple)
{
  if (Stipple && (Stipple->selectedIntoDC < 0))
    return;
  if (Stipple)
    Stipple->selectedIntoDC++;
  if (stipple)
    --stipple->selectedIntoDC;

  stipple = Stipple;
}

wxGDIList::wxGDIList (void)
{
}

wxGDIList::~wxGDIList (void)
{
#ifndef wx_x
  wxNode *node = First ();
  while (node)
    {
      wxObject *object = (wxObject *) node->Data ();
      wxNode *next = node->Next ();
      delete object;
      node = next;
    }
#endif
}

// Pen and Brush lists
wxPenList::wxPenList(void)
 : wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxPenList::~wxPenList(void)
{
}


void wxPenList::AddPen (wxPen * pen)
{
  list->Append(pen);
  list->Show(pen, -1);
  pen->Lock(1);
}

wxPen *wxPenList::FindOrCreatePen (wxColour * colour, int width, int style)
{
  wxPen *pen;
  wxChildNode *node;
  int i = 0;

  if (!colour)
    return NULL;

  while (node = list->NextNode(i))
    {
      wxPen *each_pen = (wxPen *) node->Data ();
      if (each_pen &&
	  each_pen->GetWidth() == width &&
	  each_pen->GetStyle() == style &&
	  each_pen->GetColour()->Red () == colour->Red () &&
	  each_pen->GetColour()->Green () == colour->Green () &&
	  each_pen->GetColour()->Blue () == colour->Blue ())
	return each_pen;
    }
  pen = new wxPen (*colour, width, style);
#if WXGARBAGE_COLLECTION_ON
  AddPen(pen);
#endif
  return pen;
}

wxPen *wxPenList::FindOrCreatePen (char *colour, int width, int style)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour(colour);
  if (the_colour)
    return FindOrCreatePen (the_colour, width, style);
  else
    return NULL;
}

wxBrushList::wxBrushList(void)
 : wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxBrushList::~wxBrushList(void)
{
}

void wxBrushList::AddBrush (wxBrush * brush)
{
  brush->Lock(1);
  list->Append(brush);
  list->Show(brush, -1);
}

wxBrush *wxBrushList::FindOrCreateBrush (wxColour * colour, int style)
{
  wxBrush *brush;
  wxChildNode *node;
  int i = 0;

  if (!colour)
    return NULL;

  while (node = list->NextNode(i))
    {
      wxBrush *each_brush = (wxBrush *) node->Data ();
      if (each_brush &&
	  each_brush->GetStyle() == style &&
	  each_brush->GetColour()->Red() == colour->Red() &&
	  each_brush->GetColour()->Green() == colour->Green() &&
	  each_brush->GetColour()->Blue() == colour->Blue())
	return each_brush;
    }
  brush = new wxBrush (*colour, style);
#if WXGARBAGE_COLLECTION_ON
  AddBrush(brush);
#endif
  return brush;
}

wxBrush *wxBrushList::FindOrCreateBrush (char *colour, int style)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour (colour);
  if (the_colour)
    return FindOrCreateBrush (the_colour, style);
  else
    return NULL;
}


wxFontList::wxFontList(void)
 : wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxFontList::~wxFontList(void)
{
}

void wxFontList::AddFont (wxFont * font)
{
  list->Append(font);
  list->Show(font, -1);
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, int FamilyOrFontId, int Style, int Weight, Bool underline)
{
  wxFont *font;
  wxChildNode *node;
  int i = 0;

  while (node = list->NextNode(i))
    {
      wxFont *each_font = (wxFont *) node->Data ();
      if (each_font &&
	  each_font->GetPointSize () == PointSize &&
	  each_font->GetStyle () == Style &&
	  each_font->GetWeight () == Weight &&
	  each_font->GetFontId () == FamilyOrFontId &&
	  each_font->GetUnderlined () == underline)
	return each_font;
    }
  font = new wxFont (PointSize, FamilyOrFontId, Style, Weight, underline);
#if WXGARBAGE_COLLECTION_ON
  AddFont(font);
#endif
  return font;
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, const char *Face, int Family, int Style, int Weight, Bool underline)
{
  return FindOrCreateFont(PointSize,
			  wxTheFontNameDirectory.FindOrCreateFontId(Face, Family),
			  Style,
			  Weight,
			  underline);
}

#if (!USE_TYPEDEFS)
wxPoint::wxPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxPoint::wxPoint (float the_x, float the_y)  : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxPoint::~wxPoint (void)
{
}
#endif

#if (!USE_TYPEDEFS)
wxIntPoint::wxIntPoint (void)  : wxObject(WXGC_NO_CLEANUP)
{
}

wxIntPoint::wxIntPoint (int the_x, int the_y)  : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxIntPoint::~wxIntPoint (void)
{
}
#endif

static WX_FAR char *font_defaults[] = {
  /* MATTHEW: [4] Family map */
  "FamilyDefault", "Default",
  "FamilyRoman", "Roman",
  "FamilyDecorative", "Decorative",
  "FamilyModern", "Modern",
  "FamilyTeletype", "Teletype",
  "FamilySwiss", "Swiss",
  "FamilyScript", "Script",
  "FamilySystem", "System",
  "FamilySymbol", "Symbol",

  "AfmMedium", "",
  "AfmBold", "Bo",
  "AfmLight", "",
  "AfmStraight", "",
  "AfmItalic", "${AfmSlant}",
  "AfmSlant", "O",
  "AfmRoman", "Ro",

  "AfmTimes", "Times",
  "AfmHelvetica", "Helv",
  "AfmCourier", "Cour",

  "Afm___", "${AfmTimes,$[weight],$[style]}",

  "AfmTimes__", "${AfmTimes}${Afm$[weight]}${Afm$[style]}",
  "AfmTimesMediumStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimesLightStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimes_Italic", "${AfmTimes}$[weight]${AfmItalic}",
  "AfmTimes_Slant", "${AfmTimes}$[weight]${AfmItalic}",

  "AfmSwiss__", "${AfmHelvetica}${Afm$[weight]}${Afm$[style]}",
  "AfmModern__", "${AfmCourier}${Afm$[weight]}${Afm$[style]}",
  "AfmSymbol__", "Sym",

  "AfmTeletype__", "${AfmModern,$[weight],$[style]}",

  "PostScriptMediumStraight", "",
  "PostScriptMediumItalic", "-Oblique",
  "PostScriptMediumSlant", "-Oblique",
  "PostScriptLightStraight", "",
  "PostScriptLightItalic", "-Oblique",
  "PostScriptLightSlant", "-Oblique",
  "PostScriptBoldStraight", "-Bold",
  "PostScriptBoldItalic", "-BoldOblique",
  "PostScriptBoldSlant", "-BoldOblique",

#if WX_NORMALIZED_PS_FONTS
  "PostScript___", "${PostScriptTimes,$[weight],$[style]}",
#else
  "PostScriptRoman__", "${PostScriptTimes,$[weight],$[style]}",
  "PostScript___", "LucidaSans${PostScript$[weight]$[style]}",
#endif

  "PostScriptTimesMedium", "",
  "PostScriptTimesLight", "",
  "PostScriptTimesBold", "Bold",

  "PostScriptTimes__", "Times${PostScript$[weight]$[style]}",
  "PostScriptTimesMediumStraight", "Times-Roman",
  "PostScriptTimesLightStraight", "Times-Roman",
  "PostScriptTimes_Slant", "Times-${PostScriptTimes$[weight]}Italic",
  "PostScriptTimes_Italic", "Times-${PostScriptTimes$[weight]}Italic",

  "PostScriptSwiss__", "Helvetica${PostScript$[weight]$[style]}",
  "PostScriptModern__", "Courier${PostScript$[weight]$[style]}",
  "PostScriptSymbol__", "Symbol",

  "PostScriptTeletype__", "${PostScriptModern,$[weight],$[style]}",

#if !WX_NORMALIZED_PS_FONTS
  "PostScriptScript__", "Zapf-Chancery-MediumItalic",
#endif

  "ScreenDefault__", "applicationfont",
  "ScreenSystem__", "systemfont",
  "ScreenRoman__", "times",
  "ScreenDecorative__", "geneva",
  "ScreenModern__", "monaco", /* "courier" is also good */
  "ScreenTeletype__", "${ScreenModern,$[weight],$[style]}",
  "ScreenSwiss__", "helvetica",
  "ScreenScript__", "geneva",
  "ScreenSymbol__", "symbol",

  NULL
};


wxFontNameDirectory wxTheFontNameDirectory;

enum {
  wxWEIGHT_NORMAL,
  wxWEIGHT_BOLD,
  wxWEIGHT_LIGHT,
  wxNUM_WEIGHTS
  };

enum {
  wxSTYLE_NORMAL,
  wxSTYLE_ITALIC,
  wxSTYLE_SLANT,
  wxNUM_STYLES
  };

class wxSuffixMap {
 public:
  char *map[wxNUM_WEIGHTS][wxNUM_STYLES];
  void Initialize(const char *, const char *);
  void Cleanup(void);
};


class wxFontNameItem : public wxObject
{
 public:
  int id;
  int family;
  char *name;
  wxSuffixMap screen, printing, afm;
  Bool isfamily;
  
  ~wxFontNameItem(void);
};

wxFontNameItem::~wxFontNameItem(void)
{
	screen.Cleanup();
	printing.Cleanup();
	afm.Cleanup();
	delete [] name;
}

static int WCoordinate(int w)
{
  switch (w) {
  case wxBOLD:
    return wxWEIGHT_BOLD;
  case wxLIGHT:
    return wxWEIGHT_LIGHT;
  case wxNORMAL:
  default:
    return wxWEIGHT_NORMAL;
  }
}

static int SCoordinate(int s)
{
  switch (s) {
  case wxITALIC:
    return wxSTYLE_ITALIC;
  case wxSLANT:
    return wxSTYLE_SLANT;
  case wxNORMAL:
  default:
    return wxSTYLE_NORMAL;
  }
}

wxFontNameDirectory::wxFontNameDirectory(void)
{
  table = new wxHashTable(wxKEY_INTEGER, 20);
  nextFontId = -1;
}

wxFontNameDirectory::~wxFontNameDirectory()
{ // The data in the hash table must be deleted as well as the table
  table->DeleteContents(1);
  delete table;
}

int wxFontNameDirectory::GetNewFontId(void)
{
  return (nextFontId--);
}

#if !USE_RESOURCES
#define wxGetResource(a, b, c) 0
#endif

static void SearchResource(const char *prefix, const char **names, int count, char **v)
{
  int k, i, j;
  char resource[1024], **defaults, *internal;

  k = 1 << count;

  *v = NULL;
  internal = NULL;

  for (i = 0; i < k; i++) {
    strcpy(resource, prefix);
    for (j = 0; j < count; j++) {
      if (!(i & (1 << j)))
		strcat(resource, names[j]);
      else
		strcat(resource, "_");
    }

    if (wxGetResource(wxTheApp->wx_class, (char *)resource, v) && **v)
      return;

    if (!internal) {
      defaults = font_defaults;
      while (*defaults) {
		if (!strcmp(*defaults, resource)) {
	  	  internal = defaults[1];
	  	  break;
		}
		defaults += 2;
      }
    }
  }

  if (internal)
    *v = copystring(internal);
}

void wxFontNameDirectory::Initialize()
{
  wxTheFontNameDirectory.Initialize(wxDEFAULT, wxDEFAULT, "Default");
  wxTheFontNameDirectory.Initialize(wxDECORATIVE, wxDECORATIVE, "Decorative");
  wxTheFontNameDirectory.Initialize(wxROMAN, wxROMAN, "Roman");
  wxTheFontNameDirectory.Initialize(wxMODERN, wxMODERN, "Modern");
  wxTheFontNameDirectory.Initialize(wxTELETYPE, wxTELETYPE, "Teletype");
  wxTheFontNameDirectory.Initialize(wxSWISS, wxSWISS, "Swiss");
  wxTheFontNameDirectory.Initialize(wxSCRIPT, wxSCRIPT, "Script");
  wxTheFontNameDirectory.Initialize(wxSYSTEM, wxSYSTEM, "System");
  wxTheFontNameDirectory.Initialize(wxSYMBOL, wxSYMBOL, "Symbol");
}

typedef char *a_charptr;
// Note from CJC - Initialize leaked like crazy because the copystring() returned by
// searchresource() was not deleted on expansions.
//
void wxSuffixMap::Initialize(const char *resname, const char *devresname)
{
  const char *weight, *style;
  char *v, *rname;
  int i, j, k;
  const char *names[3];

  for (k = 0; k < wxNUM_WEIGHTS; k++) {
    switch (k) {
    case wxWEIGHT_NORMAL:
      weight = "Medium";
      break;
    case wxWEIGHT_LIGHT:
      weight = "Light";
      break;
    case wxWEIGHT_BOLD:
      default:
      weight = "Bold";
  	}
    for (j = 0; j < wxNUM_STYLES; j++) {
      switch (j) {
      case wxSTYLE_NORMAL:
		style = "Straight";
		break;
      case wxSTYLE_ITALIC:
		style = "Italic";
		break;
      case wxSTYLE_SLANT:
	      default:
		style = "Slant";
   	  }

      names[0] = resname;
      names[1] = weight;
      names[2] = style;
      
      SearchResource(devresname, names, 3, &v);

      /* Expand macros in the found string: */
  found:
      int len, closer = 0, startpos = 0;

      len = (v ? strlen(v) : 0);
      for (i = 0; i < len; i++)
		if (v[i] == '$' && ((v[i+1] == '[') || (v[i+1] == '{'))) {
	 		startpos = i;
	  		if (v[i+1] == '[')
	   		  closer = ']';
	  		else
	    	  closer = '}';
	  		i++;
		} else if (v[i] == closer) {
	  	  int newstrlen;
	  	  const char *r = NULL;
	  	  char *naya, *name;
	  	  char *tmp; //cjc
	  
	  	  name = v + startpos + 2;
	  	  v[i] = 0;
		  tmp = NULL; //cjc
	  	  if (closer == '}') {
	    	int i, count, len;
	    	char **names;

	    	for (i = 0, count = 1; name[i]; i++)
	      		if (name[i] == ',')
				  count++;
	    
	    	len = i;

	    	names = new a_charptr[count];
	    	names[0] = name;
	    	for (i = 0, count = 1; i < len; i++)
	      		if (name[i] == ',') {
					names[count++] = name + i + 1;
					name[i] = 0;
	      		}

	    	SearchResource("", (const char **)names, count, (char **)&r);
	    	delete[] names;
			tmp = (char *)r;	// cjc
	    	if (!r) {
	      		for (i = 0; i < len; i++)
					if (!name[i])
		  				name[i] = ',';
	      		r = "";
	      		printf("Bad resource name \"%s\" in font lookup\n", name);
	    	}
	  	  } else if (!strcmp(name, "weight")) {
	    	r = weight;
	      } else if (!strcmp(name, "style")) {
	    	r = style;
	      } else if (!strcmp(name, "family")) {
	    	r = resname;
	      } else {
	    	r = "";
	    	printf("Bad font macro name \"%s\"\n", name);
	      }
	    newstrlen = strlen(r);

	    naya = new char[len + newstrlen + 1];
	    memcpy(naya, v, startpos);
	    memcpy(naya + startpos, r, newstrlen);
	    memcpy(naya + startpos + newstrlen, v + i + 1, len - i + 1);
	    delete[] v;
	    delete[] tmp; //cjc
	    v = naya;

	    goto found;
	  }

      rname = (char *)((resname[0] == '@') ? resname + 1 : resname);

#if defined(wx_msw) || defined(wx_mac)
      if (!v)
		v = copystring(rname);
#endif
      /* We have a final value: */
      map[k][j] = v;
    } // wxNUM_STYLES (j);
  } // wxNUM_WEIGHTS (k)
}

void wxSuffixMap::Cleanup(void)
{
	int i,j;
	char *v, **vec,*str;
	vec = map[0,0];
	for (i = 0; i < wxNUM_WEIGHTS; i++) {
		for (j = 0; j < wxNUM_STYLES; j++) {
			str = *vec++;
			if (*str) {
				delete   str;
			}
		}
	}
}

void wxFontNameDirectory::Initialize(int fontid, int family, const char *resname)
{
  wxFontNameItem *item = new wxFontNameItem;
  char *fam, resource[256];
  
  item->id = fontid;
  item->family = family;
  item->isfamily = (resname[0] != '@');
  
  sprintf(resource, "Family%s", resname);
  fam = NULL;
  SearchResource((const char *)resource, NULL, 0, (char **)&fam);
  if (fam) {
    if (!strcmp(fam, "Default"))
      item->family = wxDEFAULT;
    else if (!strcmp(fam, "Roman"))
      item->family = wxROMAN;
    else if (!strcmp(fam, "Decorative"))
      item->family = wxDECORATIVE;
    else if (!strcmp(fam, "Modern"))
      item->family = wxMODERN;
    else if (!strcmp(fam, "Teletype"))
      item->family = wxTELETYPE;
    else if (!strcmp(fam, "Swiss"))
      item->family = wxSWISS;
    else if (!strcmp(fam, "Script"))
      item->family = wxSCRIPT;
    else if (!strcmp(fam, "System"))
      item->family = wxSYSTEM;
    else if (!strcmp(fam, "Symbol"))
      item->family = wxSYMBOL;
    delete [] fam;
  }

  item->name = copystring(resname);
  item->screen.Initialize(resname, "Screen");
  item->printing.Initialize(resname, "PostScript");
  item->afm.Initialize(resname, "Afm");

  table->Put(fontid, item);
}

int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;

  if (id = GetFontId(name))
    return id;

  id = GetNewFontId();

  char *s;
  s = new char[strlen(name) + 2];
  strcpy(s + 1, name);
  s[0] = '@';
  Initialize(id, family, s);

  return id;
}

char *wxFontNameDirectory::GetScreenName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  return item->screen.map[WCoordinate(weight)][SCoordinate(style)];
}

void wxFontNameDirectory::SetScreenName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return;

  item->screen.map[WCoordinate(weight)][SCoordinate(style)] = s;
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  return item->printing.map[WCoordinate(weight)][SCoordinate(style)];
}

void wxFontNameDirectory::SetPostScriptName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  item->printing.map[WCoordinate(weight)][SCoordinate(style)] = s;
}

char *wxFontNameDirectory::GetAFMName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  return item->afm.map[WCoordinate(weight)][SCoordinate(style)];
}

void wxFontNameDirectory::SetAFMName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  item->afm.map[WCoordinate(weight)][SCoordinate(style)] = s;
}

char *wxFontNameDirectory::GetFontName(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  if (item->isfamily)
    return NULL;

  return item->name + 1;
}

int wxFontNameDirectory::GetFontId(const char *name)
{
  wxNode *node;

  table->BeginFind();

  while (node = table->Next()) {
    wxFontNameItem *item = (wxFontNameItem *)node->Data();
    if (!item->isfamily && !strcmp(name, item->name+1))
      return item->id;
  }

  return 0;
}

int wxFontNameDirectory::GetFamily(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return wxDEFAULT;

  return item->family;
}

#include "::::wxcommon:Region.h"
#include "::::wxcommon:Region.cxx"
