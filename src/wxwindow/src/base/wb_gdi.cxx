/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_gdi.cxx,v 1.17 1999/09/17 00:43:10 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_gdi.h"

#endif

#ifdef wx_x
# define UseXtRegions
#endif

#include "wx_rgn.h"

#include <stdio.h>
#include <ctype.h>
#include <math.h>

IMPLEMENT_DYNAMIC_CLASS(wxColour, wxObject)
IMPLEMENT_CLASS(wxColourDatabase, wxList)
IMPLEMENT_DYNAMIC_CLASS(wxFontList, wxList)
IMPLEMENT_DYNAMIC_CLASS(wxPenList, wxList)
IMPLEMENT_DYNAMIC_CLASS(wxBrushList, wxList)
IMPLEMENT_DYNAMIC_CLASS(wxGDIList, wxList)

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

/* MATTHEW: [4] New font system */
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
    return wxTheFontNameDirectory.GetFontName(fontid); 
  }
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
  locked = 0;
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = 0;
#endif
//  wxTheColourList->Append (this);
}

wxColour::wxColour (const unsigned char r, const unsigned char g, const unsigned char b)
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
//  wxTheColourList->Append (this);
}

wxColour::wxColour (const wxColour& col)
{
  __type = wxTYPE_COLOUR;
  red = col.red;
  green = col.green;
  blue = col.blue;
  isInit = col.isInit;
  pixel = col.pixel;
  locked = 0;
}

wxColour& wxColour::operator =(const wxColour& col)
{
  __type = wxTYPE_COLOUR;
  red = col.red;
  green = col.green;
  blue = col.blue;
  isInit = col.isInit;
  pixel = col.pixel;
  return *this;
}

wxColour::wxColour (const char *col)
{
  __type = wxTYPE_COLOUR;
  locked = 0;
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
//  wxTheColourList->Append (this);
}

wxColour::~wxColour (void)
{
//  wxTheColourList->DeleteObject (this);
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
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
  return (*this);
}

void wxColour::Set (unsigned char r, unsigned char g, unsigned char b)
{
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

#ifndef wx_msw
static wxHashTable *aux = NULL;
#endif

// Colour database stuff
void wxColourDatabase::Initialize (void)
{
  // Don't initialize into regular map for X: colours are found
  // in FindColour below. But to ensure that all of these names
  // are present, add them to an auxiliary list:

#ifdef wx_msw
# define APPEND_TO_DB Append
#else
  if (!aux)
    return;
# define APPEND_TO_DB aux->Put
#endif

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

#ifdef wx_msw
  return NULL;
#else
  XColor xcolour;
  
  Display *display = XtDisplay(wxTheApp->topLevel);
  
  wxColour *col;
  
  if (!aux) {
    aux = new wxHashTable(wxKEY_STRING, 20);
    Initialize(); /* fills in aux */
  }

  /* MATTHEW: [4] Use wxGetMainColormap */
  if (XParseColor(display, wxGetMainColormap(display), colour,&xcolour)) {
    /* But only add it if it's a standard color */
    if (aux->Get(colour)) {
      unsigned char r = (unsigned char)(xcolour.red >> 8);
      unsigned char g = (unsigned char)(xcolour.green >> 8);
      unsigned char b = (unsigned char)(xcolour.blue >> 8);
      
      col = new wxColour(r, g, b);
      col->Lock(1);
    } else
      return NULL;
  } else {  
    /* Check the standard color list: */
    col = (wxColour *)aux->Get(colour);
    if (!col)
      return NULL;
  }
  
  Append(colour, col);

  return col;
#endif
}

/* Old FindColour
wxColour *wxColourDatabase::FindColour (const char *colour)
{
  wxNode *node = Find (colour);
  return node ? (wxColour *) node->Data () : NULL ;
}
*/

char *wxColourDatabase::FindName (wxColour& colour)
{
  unsigned char red = colour.Red ();
  unsigned char green = colour.Green ();
  unsigned char blue = colour.Blue ();

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
  // wxTheColourList =  new wxGDIList;

#ifdef wx_motif
#endif
#ifdef wx_x
  wxFontPool = new XFontPool;
#endif

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

wxbPen::wxbPen (wxColour& WXUNUSED(col), int WXUNUSED(Width), int WXUNUSED(Style))
{
  __type = wxTYPE_PEN;
  locked = 0;
}

wxbPen::wxbPen (const char *WXUNUSED(col), int WXUNUSED(Width), int WXUNUSED(Style))
{
  __type = wxTYPE_PEN;
  locked = 0;
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

wxColour& wxbPen::GetColour (void)
{
  return colour;
}

void wxbPen::SetColour (wxColour& col)
{
  colour = col;
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
}

wxbBrush::~wxbBrush ()
{
  if (stipple)
    --stipple->selectedIntoDC;
}

wxbBrush::wxbBrush (wxColour& WXUNUSED(col), int WXUNUSED(Style))
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

wxColour& wxbBrush::GetColour (void)
{
  return colour;
}

void wxbBrush::SetColour (wxColour& col)
{
  colour = col;
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
  list->Show(pen, -1); /* so it can be collected */
}

wxPen *wxPenList::FindOrCreatePen (wxColour * colour, int width, int style)
{
  wxPen *pen; /* MATTHEW: [8] */
  int i = 0;
  wxChildNode *node;

  if (!colour)
    return NULL;

  while ((node = list->NextNode(i))) {
    wxPen *each_pen = (wxPen *) node->Data ();
    if (each_pen &&
	each_pen->GetWidth () == width &&
	each_pen->GetStyle () == style &&
	each_pen->GetColour ().Red () == colour->Red () &&
	each_pen->GetColour ().Green () == colour->Green () &&
	each_pen->GetColour ().Blue () == colour->Blue ())
      return each_pen;
  }
  /* MATTHEW: [8] With GC, must explicitly add: */
  pen = new wxPen (*colour, width, style);

  pen->Lock(1);

#if WXGARBAGE_COLLECTION_ON
  AddPen(pen);
#endif

  return pen;
}

wxPen *wxPenList::FindOrCreatePen (char *colour, int width, int style)
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
	each_brush->GetStyle () == style &&
	each_brush->GetColour ().Red () == colour->Red () &&
	each_brush->GetColour ().Green () == colour->Green () &&
	each_brush->GetColour ().Blue () == colour->Blue ())
      return each_brush;
  }

  /* MATTHEW: [8] With GC, must explicitly add: */
  brush = new wxBrush (*colour, style);

  brush->Lock(1);

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
  /* MATTHEW: [8] With GC, must explicitly add: */
  fnt = new wxFont (PointSize, FamilyOrFontId, Style, Weight, underline);

#if WXGARBAGE_COLLECTION_ON
  AddFont(fnt);
#endif

  return fnt;
}

/* MATTHEW: [4] New font system */
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

wxPoint::wxPoint (float the_x, float the_y) : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxPoint::~wxPoint (void)
{
}
#endif

#if (!USE_TYPEDEFS)

IMPLEMENT_DYNAMIC_CLASS(wxPoint, wxObject)
IMPLEMENT_DYNAMIC_CLASS(wxIntPoint, wxObject)

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
#endif

/* MATTHEW: [2] Font name directory */

#define wx_x_afm

char *font_defaults[] = {
  /* MATTHEW: [4] Family map */
  "FamilySystem", "System",
  "FamilyDefault", "Default",
  "FamilyRoman", "Roman",
  "FamilyDecorative", "Decorative",
  "FamilyModern", "Modern",
  "FamilyTeletype", "Teletype",
  "FamilySwiss", "Swiss",
  "FamilyScript", "Script",
  "FamilySymbol", "Symbol",

  "AfmMedium", "",
#ifdef wx_x_afm
  "AfmBold", "Bo",
#else
  "AfmBold", "bo",
#endif
  "AfmLight", "",
  "AfmStraight", "",
  "AfmItalic", "${AfmSlant}",
#ifdef wx_x_afm
  "AfmSlant", "O",
#else
  "AfmSlant", "ob",
#endif
#ifdef wx_x_afm
  "AfmRoman", "Ro",
#else
  "AfmRoman", "ro",
#endif

#ifdef wx_x_afm
  "AfmTimes", "Times",
  "AfmHelvetica", "Helv",
  "AfmCourier", "Cour",
#else
  "AfmTimes", "time",
  "AfmHelvetica", "helv",
  "AfmCourier", "cour",  
#endif

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

  "ScreenSystem__", "MS Sans Serif",
  "ScreenDefault__", "MS Sans Serif",
  "ScreenRoman__", "Times New Roman",
  "ScreenDecorative__", "Arial",
  "ScreenModern__", "Courier New",
  "ScreenTeletype__", "${ScreenModern$[weight];$[style]}",
  "ScreenSwiss__", "Arial",
  "ScreenScript__", "Arial",
  "ScreenSymbol__", "Symbol",

  NULL
};


IMPLEMENT_DYNAMIC_CLASS(wxFontNameDirectory, wxObject)

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
  void Initialize(const char *, const char *, int weight, int style); /* MATTHEW: [14] */

  /* MATTHEW: [14] zero map initially: */
  wxSuffixMap() {
    int i, j;
    for (i = 0; i < wxNUM_WEIGHTS; i++)
      for (j = 0; j < wxNUM_STYLES; j++)
	map[i][j] = NULL;
  }
};

class wxFontNameItem : public wxObject
{
 public:
  int id;
  int family; /* MATTHEW: [4] New font system */
  char *name;
  wxSuffixMap screen, printing, afm;
  Bool isfamily;
};

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
  nextFontId = 100;
}

wxFontNameDirectory::~wxFontNameDirectory()
{
  delete table;
}

int wxFontNameDirectory::GetNewFontId(void)
{
  return (nextFontId++);
}

#if !USE_RESOURCES
#define wxGetResource(a, b, c) 0
#endif

/* MATTHEW: [4] const */
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
   /* MATTHEW: [4] New font system */
  wxTheFontNameDirectory.Initialize(wxSYSTEM, wxSYSTEM, "System");
  wxTheFontNameDirectory.Initialize(wxDEFAULT, wxDEFAULT, "Default");
  wxTheFontNameDirectory.Initialize(wxDECORATIVE, wxDECORATIVE, "Decorative");
  wxTheFontNameDirectory.Initialize(wxROMAN, wxROMAN, "Roman");
  wxTheFontNameDirectory.Initialize(wxMODERN, wxMODERN, "Modern");
  wxTheFontNameDirectory.Initialize(wxTELETYPE, wxTELETYPE, "Teletype");
  wxTheFontNameDirectory.Initialize(wxSWISS, wxSWISS, "Swiss");
  wxTheFontNameDirectory.Initialize(wxSCRIPT, wxSCRIPT, "Script");
  wxTheFontNameDirectory.Initialize(wxSYMBOL, wxSYMBOL, "Symbol");
}

typedef char *a_charptr;

/* MATTHEW: [14] Pass in weight & style and just do one */
void wxSuffixMap::Initialize(const char *resname, const char *devresname,
			     int wt, int st)
{
  const char *weight, *style; /* MATTHEW: [4] const */
  char *v, *rname;
  int i; /* MATTHEW: [14] Delete k & j */
  const char *names[3]; /* MATTHEW: [4] const */

  {
    switch (wt) {
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
    {
      switch (st) {
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
      
      v = NULL;
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
	  const char *r = NULL; /* MATTHEW: [4] const */
	  char *naya, *name;
	  
	  name = v + startpos + 2;
	  v[i] = 0;

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
	  v = naya;

	  goto found;
	}

      rname = (char *)((resname[0] == '@') ? resname + 1 : resname);

#ifdef wx_msw
      /* MATTHEW: [4] For msw, there's a meaningful default */
      if (!v)
	v = copystring(rname);
#else
      if (!strcmp(devresname, "Screen")) {
	if (v && (v[0] == '+')) {
	  memmove(v, v + 1, strlen(v));
	} else {
	  int len;
	  char *src;
	  char *normalcy;
	  /* Build name using special heuristics:
	     -([^-]*) => -*-\1-<weight>-<style>-normal-*-*-%d-*-*-*-*-*-*
	     -([^-]*)-(.*) => -\1-\2-<weight>-<style>-normal-*-*-%d-*-*-*-*-*-*
	     ([^-].*[^-]) => \1
	     */
	  src = (v ? v : (char *)rname);
	  len = strlen(src);
	  if (src[0] == '-') {
	    int c = 0;
	    for (i = 0; i < len; i++)
	      if (src[i] == '-')
		c++;
	    
	    v = new char[len + 40];
	    char *prefix;
	    if (c < 2)
	      prefix = "-*";
	    else
	      prefix = "";
	    
	    if (c < 3) {
	      switch (wt) {
	      case wxWEIGHT_NORMAL:
		weight = "-medium";
		break;
	      case wxWEIGHT_LIGHT:
		weight = "-light";
		break;
	      case wxWEIGHT_BOLD:
	      default:
		weight = "-bold";
	      }
	    } else
	      weight = "";
	    
	    if (c < 4) {
	      switch (st) {
	      case wxSTYLE_NORMAL:
		style = "-r";
	      break;
	      case wxSTYLE_ITALIC:
		style = "-i";
		break;
	      case wxSTYLE_SLANT:
	      default:
		style = "-o";
	      }
	    } else
	      style = "";
	    
	    if (c < 5)
	      normalcy = "-normal";
	    else
	      normalcy = "";
	    
	    sprintf(v, "%s%s%s%s%s-*-*-%%d-*-*-*-*-*-*",
		    prefix, src, weight, style, normalcy);
	  } else
	    v = copystring(src);
	}
      }
#endif

      /* We have a final value: */
      map[wt][st] = v;
    }
  }
}

/* MATTHEW: [4] New font system */
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
    if (!strcmp(fam, "System"))
      item->family = wxSYSTEM;
    else if (!strcmp(fam, "Default"))
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
    else if (!strcmp(fam, "Symbol"))
      item->family = wxSYMBOL;
  }

  item->name = copystring(resname);

  table->Put(fontid, item);
}

/* MATTHEW: [4] Add */
int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;
  char *s;

  if ((id = GetFontId(name)))
    return id;

  id = GetNewFontId();

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

  int wt = WCoordinate(weight), st = SCoordinate(style);

  if (!item->screen.map[wt][st])
    item->screen.Initialize(item->name, "Screen", wt, st);

  return item->screen.map[wt][st];
}

void wxFontNameDirectory::SetScreenName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  item->screen.map[wt][st] = s;
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  if (!item->printing.map[wt][st])
    item->printing.Initialize(item->name, "PostScript", wt, st);

  return item->printing.map[wt][st];
}

void wxFontNameDirectory::SetPostScriptName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  item->printing.map[wt][st] = s;
}

char *wxFontNameDirectory::GetAFMName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  if (!item->afm.map[wt][st])
    item->afm.Initialize(item->name, "Afm", wt, st);

  return item->afm.map[wt][st];
}

void wxFontNameDirectory::SetAFMName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  item->afm.map[wt][st] = s;
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

int wxFontNameDirectory::GetFontId(const char *name) /* MATTHEW: [4] const */
{
  wxNode *node;

  table->BeginFind();

  while ((node = table->Next())) {
    wxFontNameItem *item = (wxFontNameItem *)node->Data();
    if (!item->isfamily && !strcmp(name, item->name+1))
      return item->id;
  }

  return 0;
}

/* MATTHEW: [4] Add */
int wxFontNameDirectory::GetFamily(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return wxDEFAULT;

  return item->family;
}


#include "wb_rgn.cxx"
