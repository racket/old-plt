/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "../../../wxcommon/Region.h"

#include <stdio.h>
#include <ctype.h>
#include <math.h>

wxbFont::wxbFont (void)
{
  __type = wxTYPE_FONT;
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxbFont::wxbFont (int WXUNUSED(PointSize), int WXUNUSED(Family), int WXUNUSED(Style), int WXUNUSED(Weight), Bool WXUNUSED(Underline))
{
  __type = wxTYPE_FONT;
}

wxbFont::~wxbFont ()
{
}

char *wxbFont::GetFaceString(void)
{
  /* If it's one of the portable faceless fonts, return NULL */
  switch (GetFamily())
  {
  case wxDECORATIVE:
  case wxROMAN:
  case wxSCRIPT:
  case wxSWISS:
  case wxMODERN:
  case wxTELETYPE:
  case wxSYSTEM:
  case wxSYMBOL:
    return NULL;
  default:
    return wxTheFontNameDirectory->GetFontName(fontid); 
  }
}

// Colour

wxColour::wxColour (void)
{
  __type = wxTYPE_COLOUR;
  isInit = FALSE;
  locked = 0;
  pixel = 0;
}

wxColour::wxColour (const unsigned char r, const unsigned char g, const unsigned char b)
{
  __type = wxTYPE_COLOUR;
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
  pixel = RGB (red, green, blue);
}

wxColour::wxColour (const wxColour *col)
{
  __type = wxTYPE_COLOUR;
  locked = 0;
  CopyFrom(col);
}

wxColour* wxColour::CopyFrom(const wxColour* col)
{
  red = col->red;
  green = col->green;
  blue = col->blue;
  isInit = col->isInit;
  pixel = col->pixel;
  return this;
}

wxColour& wxColour::operator=(const wxColour& col)
{
  return *(CopyFrom(&col));
}

wxColour::wxColour (const char *col)
{
  __type = wxTYPE_COLOUR;
  locked = 0;

  CopyFrom(col);
}

wxColour::~wxColour (void)
{
}

wxColour* wxColour::CopyFrom(const char *col)
{
  wxColour *the_colour;

  the_colour = wxTheColourDatabase->FindColour (col);

  if (the_colour) {
    red = the_colour->Red ();
    green = the_colour->Green ();
    blue = the_colour->Blue ();
    isInit = TRUE;
  } else {
    red = 0;
    green = 0;
    blue = 0;
    isInit = FALSE;
  }
  pixel = RGB (red, green, blue);

  return this;
}

void wxColour::Set (unsigned char r, unsigned char g, unsigned char b)
{
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
  pixel = RGB (red, green, blue);

}

void wxColour::Get (unsigned char *r, unsigned char *g, unsigned char *b)
{
  *r = red;
  *g = green;
  *b = blue;
}

wxColourDatabase::wxColourDatabase (int type):
wxList ((KeyType)type)
{
}

wxColourDatabase::~wxColourDatabase (void)
{
  // Cleanup Colour allocated in Initialize()
  wxNode *node = First ();
  while (node)
    {
      wxColour *col = (wxColour *) node->Data ();
      wxNode *next = node->Next ();
      delete col;
      node = next;
    }
}

// Colour database stuff
void wxColourDatabase::Initialize (void)
{
  // Don't initialize into regular map for X: colours are found
  // in FindColour below. But to ensure that all of these names
  // are present, add them to an auxiliary list:

# define APPEND_TO_DB Append

  wxColour *tmpc;
#define APPEND_C(name, c) tmpc = c; tmpc->Lock(1); APPEND_TO_DB(name, tmpc);
  APPEND_C("AQUAMARINE", new wxColour(112, 216, 144));
  APPEND_C("BLACK", new wxColour(0, 0, 0));
  APPEND_C("BLUE", new wxColour(80, 80, 248));
  APPEND_C("BLUE VIOLET", new wxColour(138, 43, 226));
  APPEND_C("BROWN", new wxColour(132, 60, 36));
  APPEND_C("CADET BLUE", new wxColour(96, 160, 160));
  APPEND_C("CORAL", new wxColour(255, 127, 80));
  APPEND_C("CORNFLOWER BLUE", new wxColour(68, 64, 108));
  APPEND_C("CYAN", new wxColour(0, 255, 255));
  APPEND_C("DARK GRAY", new wxColour(169, 169, 169));
  APPEND_C("DARK GREEN", new wxColour(0, 100, 0));
  APPEND_C("DARK OLIVE GREEN", new wxColour(85, 107, 47));
  APPEND_C("DARK ORCHID", new wxColour(153, 50, 204));
  APPEND_C("DARK SLATE BLUE", new wxColour(72, 61, 139));
  APPEND_C("DARK SLATE GRAY", new wxColour(47, 79, 79));
  APPEND_C("DARK TURQUOISE", new wxColour(0, 206, 209));
  APPEND_C("DIM GRAY", new wxColour(105, 105, 105));
  APPEND_C("FIREBRICK", new wxColour(178, 34, 34));
  APPEND_C("FOREST GREEN", new wxColour(34, 139, 34));
  APPEND_C("GOLD", new wxColour(255, 215, 0));
  APPEND_C("GOLDENROD", new wxColour(218, 165, 32));
  APPEND_C("GRAY", new wxColour(190, 190, 190));
  APPEND_C("GREEN", new wxColour(60, 248, 52));
  APPEND_C("GREEN YELLOW", new wxColour(173, 255, 47));
  APPEND_C("INDIAN RED", new wxColour(205, 92, 92));
  APPEND_C("KHAKI", new wxColour(240, 230, 140));
  APPEND_C("LIGHT BLUE", new wxColour(173, 216, 230));
  APPEND_C("LIGHT GRAY", new wxColour(211, 211, 211));
  APPEND_C("LIGHT STEEL BLUE", new wxColour(176, 196, 222));
  APPEND_C("LIME GREEN", new wxColour(50, 205, 50));
  APPEND_C("MAGENTA", new wxColour(255, 0, 255));
  APPEND_C("MAROON", new wxColour(176, 48, 96));
  APPEND_C("MEDIUM AQUAMARINE", new wxColour(102, 205, 170));
  APPEND_C("MEDIUM BLUE", new wxColour(0, 0, 205));
  APPEND_C("MEDIUM FOREST GREEN", new wxColour(107, 142, 35));
  APPEND_C("MEDIUM GOLDENROD", new wxColour(234, 234, 173));
  APPEND_C("MEDIUM ORCHID", new wxColour(186, 85, 211));
  APPEND_C("MEDIUM SEA GREEN", new wxColour(60, 179, 113));
  APPEND_C("MEDIUM SLATE BLUE", new wxColour(123, 104, 238));
  APPEND_C("MEDIUM SPRING GREEN", new wxColour(0, 250, 154));
  APPEND_C("MEDIUM TURQUOISE", new wxColour(72, 209, 204));
  APPEND_C("MEDIUM VIOLET RED", new wxColour(199, 21, 133));
  APPEND_C("MIDNIGHT BLUE", new wxColour(25, 25, 112));
  APPEND_C("NAVY", new wxColour(36, 36, 140));
  APPEND_C("ORANGE", new wxColour(255, 165, 0));
  APPEND_C("ORANGE RED", new wxColour(255, 69, 0));
  APPEND_C("ORCHID", new wxColour(218, 112, 214));
  APPEND_C("PALE GREEN", new wxColour(152, 251, 152));
  APPEND_C("PINK", new wxColour(255, 192, 203));
  APPEND_C("PLUM", new wxColour(221, 160, 221));
  APPEND_C("PURPLE", new wxColour(160, 32, 240));
  APPEND_C("RED", new wxColour(248, 20, 64));
  APPEND_C("SALMON", new wxColour(250, 128, 114));
  APPEND_C("SEA GREEN", new wxColour(46, 139, 87));
  APPEND_C("SIENNA", new wxColour(160, 82, 45));
  APPEND_C("SKY BLUE", new wxColour(135, 206, 235));
  APPEND_C("SLATE BLUE", new wxColour(106, 90, 205));
  APPEND_C("SPRING GREEN", new wxColour(0, 255, 127));
  APPEND_C("STEEL BLUE", new wxColour(70, 130, 180));
  APPEND_C("TAN", new wxColour(210, 180, 140));
  APPEND_C("THISTLE", new wxColour(216, 191, 216));
  APPEND_C("TURQUOISE", new wxColour(64, 224, 208));
  APPEND_C("VIOLET", new wxColour(238, 130, 238));
  APPEND_C("VIOLET RED", new wxColour(208, 32, 144));
  APPEND_C("WHEAT", new wxColour(245, 222, 179));
  APPEND_C("WHITE", new wxColour(255, 255, 255));
  APPEND_C("YELLOW", new wxColour(255, 255, 0));
  APPEND_C("YELLOW GREEN", new wxColour(154, 205, 50));
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
  // Force capital so lc matches as in X
  char uc_colour[256];
  int i;

  for (i = 0; colour[i] && i < 255; i++)
    uc_colour[i] = toupper(colour[i]);
  uc_colour[i] = 0;
  colour = uc_colour;

  wxNode *node = Find(colour);
  if (node)
    return (wxColour *)node->Data();

  return NULL;
}

char *wxColourDatabase::FindName (wxColour * colour)
{
  unsigned char red = colour->Red();
  unsigned char green = colour->Green();
  unsigned char blue = colour->Blue();

  for (wxNode * node = First(); node; node = node->Next ())
    {
      wxColour *col = (wxColour *) node->Data ();
      if (col->Red () == red && col->Green () == green && col->Blue () == blue)
	{
	  char *found = node->string_key;
	  if (found)
	    return found;
	}
    }
  return NULL;			// Not Found

}


void 
wxInitializeStockObjects (void)
{
  wxREGGLOB(wxTheBrushList);
  wxREGGLOB(wxThePenList);
  wxREGGLOB(wxTheFontList);

  wxREGGLOB(wxNORMAL_FONT);

  wxREGGLOB(wxBLACK_PEN);

  wxREGGLOB(wxWHITE_BRUSH);
  wxREGGLOB(wxBLACK_BRUSH);

  wxREGGLOB(wxBLACK);
  wxREGGLOB(wxWHITE);

  wxREGGLOB(wxSTANDARD_CURSOR);
  wxREGGLOB(wxHOURGLASS_CURSOR);
  wxREGGLOB(wxCROSS_CURSOR);
  wxREGGLOB(wxIBEAM_CURSOR);

  wxTheBrushList = new wxBrushList;
  wxThePenList = new wxPenList;
  wxTheFontList = new wxFontList;

  wxNORMAL_FONT = new wxFont (12, wxSYSTEM, wxNORMAL, wxNORMAL);

  wxBLACK_PEN = new wxPen ("BLACK", 0, wxSOLID);

  wxBLACK_PEN->Lock(1);

  wxWHITE_BRUSH = new wxBrush ("WHITE", wxSOLID);
  wxBLACK_BRUSH = new wxBrush ("BLACK", wxSOLID);

  wxWHITE_BRUSH->Lock(1);
  wxBLACK_BRUSH->Lock(1);

  wxBLACK = new wxColour ("BLACK");
  wxWHITE = new wxColour ("WHITE");

  wxSTANDARD_CURSOR = new wxCursor (wxCURSOR_ARROW);
  wxHOURGLASS_CURSOR = new wxCursor (wxCURSOR_WAIT);
  wxCROSS_CURSOR = new wxCursor (wxCURSOR_CROSS);
  wxIBEAM_CURSOR = new wxCursor (wxCURSOR_IBEAM);
}

void 
wxDeleteStockObjects (void)
{
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

wxbPen::wxbPen (wxColour * WXUNUSED(col), float WXUNUSED(Width), int WXUNUSED(Style))
{
  __type = wxTYPE_PEN;
  locked = 0;
}

wxbPen::wxbPen (const char *WXUNUSED(col), float WXUNUSED(Width), int WXUNUSED(Style))
{
  __type = wxTYPE_PEN;
  locked = 0;
}

int wxbPen::GetWidth (void)
{
  return (int)width;
}

float wxbPen::GetWidthF(void)
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
  return colour;
}

void wxbPen::SetColour (wxColour *col)
{
  colour->CopyFrom(col);
}

void wxbPen::SetColour (const char *col)
{
  colour->CopyFrom(col);
}

void wxbPen::SetColour (char red, char green, char blue)
{
 colour->Set(red, green, blue);
}

void wxbPen::SetWidth (float Width)
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
}

wxbBrush::~wxbBrush ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbBrush::wxbBrush (wxColour * WXUNUSED(col), int WXUNUSED(Style))
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::wxbBrush (char *WXUNUSED(col), int WXUNUSED(Style))
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
  return colour;
}

void wxbBrush::SetColour (wxColour *col)
{
  colour->CopyFrom(col);
}

void wxbBrush::SetColour (const char *col)
{
  colour->CopyFrom(col);
}

void wxbBrush::SetColour (char red, char green, char blue)
{
  colour->Set(red, green, blue);
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
  list->Show(pen, -1); /* so it can be collected */
}

wxPen *wxPenList::FindOrCreatePen (wxColour * colour, float width, int style)
{
  wxPen *pen; /* MATTHEW: [8] */
  int i = 0;
  wxChildNode *node;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    wxPen *each_pen = (wxPen *) node->Data ();
    if (each_pen &&
	each_pen->GetWidthF() == width &&
	each_pen->GetStyle() == style &&
	each_pen->GetColour()->Red () == colour->Red () &&
	each_pen->GetColour()->Green () == colour->Green () &&
	each_pen->GetColour()->Blue () == colour->Blue ())
      return each_pen;
  }
  pen = new wxPen (colour, width, style);

  pen->Lock(1);

  AddPen(pen);

  return pen;
}

wxPen *wxPenList::FindOrCreatePen (char *colour, float width, int style)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour (colour);
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

void wxBrushList::AddBrush(wxBrush *Brush) 
{ 
  list->Append(Brush); 
  list->Show(Brush, -1); /* so it can be collected */
} 


wxBrush *wxBrushList::FindOrCreateBrush (wxColour * colour, int style)
{
  wxBrush *brush; /* MATTTHEW: [8] */
  int i = 0;
  wxChildNode *node;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    wxBrush *each_brush = (wxBrush *) node->Data ();
    if (each_brush &&
	each_brush->GetStyle() == style &&
	each_brush->GetColour()->Red () == colour->Red () &&
	each_brush->GetColour()->Green () == colour->Green () &&
	each_brush->GetColour()->Blue () == colour->Blue ())
      return each_brush;
  }

  brush = new wxBrush (colour, style);

  brush->Lock(1);

  AddBrush(brush);

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


wxFontList::wxFontList (void)
: wxObject(WXGC_NO_CLEANUP)
{
  list = new wxChildList;
}

wxFontList::~wxFontList (void)
{
}

void wxFontList::AddFont (wxFont * font)
{
  list->Append(font);
  list->Show(font, -1); /* so it can be collected */
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, int FamilyOrFontId, int Style, int Weight, Bool underline)
{
  wxFont *fnt; /* MATTTHEW: [8] */
  int i = 0;
  wxChildNode *node;

  while ((node = list->NextNode(i))) {
    wxFont *each_font = (wxFont *) node->Data ();
    if (each_font &&
	each_font->GetPointSize () == PointSize &&
	each_font->GetStyle () == Style &&
	each_font->GetWeight () == Weight &&
	each_font->GetFontId () == FamilyOrFontId && /* MATTHEW: [4] New font system */
	each_font->GetUnderlined () == underline)
      return each_font;
  }

  fnt = new wxFont (PointSize, FamilyOrFontId, Style, Weight, underline);

  AddFont(fnt);

  return fnt;
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, const char *Face, int Family, int Style, int Weight, Bool underline)
{
  return FindOrCreateFont(PointSize,
			  wxTheFontNameDirectory->FindOrCreateFontId(Face, Family),
			  Style,
			  Weight,
			  underline);
}

wxPoint::wxPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxPoint::wxPoint (float the_x, float the_y) : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxPoint::~wxPoint (void)
{
}

wxIntPoint::wxIntPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxIntPoint::wxIntPoint (int the_x, int the_y) : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxIntPoint::~wxIntPoint (void)
{
}

#include "../../../wxcommon/FontDirectory.cxx"

#include "../../../wxcommon/Region.cxx"
