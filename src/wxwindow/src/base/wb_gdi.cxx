/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_gdi.cxx,v 1.2 1998/02/03 18:49:55 mflatt Exp $
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

#include <stdio.h>

#ifdef wx_xview
extern Xv_Server xview_server;
#endif

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
    default:
      fam = "wxDEFAULT";
      break;
  }
  return fam;
}

/* MATTHEW: [4] New font system */
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
#define APPEND_TO_DB Append
#else
  if (!aux)
    return;
#define APPEND_TO_DB aux->Put
#endif

  wxColour *tmpc;
#define APPEND_C(name, c) tmpc = c; tmpc->Lock(1); APPEND_TO_DB(name, tmpc);
  APPEND_C ("AQUAMARINE", new wxColour (112, 219, 147));
  APPEND_C ("BLACK", new wxColour (0, 0, 0));
  APPEND_C ("BLUE", new wxColour (0, 0, 255));
  APPEND_C ("BLUE VIOLET", new wxColour (159, 95, 159));
  APPEND_C ("BROWN", new wxColour (165, 42, 42));
  APPEND_C ("CADET BLUE", new wxColour (95, 159, 159));
  APPEND_C ("CORAL", new wxColour (255, 127, 0));
  APPEND_C ("CORNFLOWER BLUE", new wxColour (66, 66, 111));
  APPEND_C ("CYAN", new wxColour (0, 255, 255));
  APPEND_C ("DARK GREY", new wxColour (47, 47, 47));	// ?

  APPEND_C ("DARK GREEN", new wxColour (47, 79, 47));
  APPEND_C ("DARK OLIVE GREEN", new wxColour (79, 79, 47));
  APPEND_C ("DARK ORCHID", new wxColour (153, 50, 204));
  APPEND_C ("DARK SLATE BLUE", new wxColour (107, 35, 142));
  APPEND_C ("DARK SLATE GREY", new wxColour (47, 79, 79));
  APPEND_C ("DARK TURQUOISE", new wxColour (112, 147, 219));
  APPEND_C ("DIM GREY", new wxColour (84, 84, 84));
  APPEND_C ("FIREBRICK", new wxColour (142, 35, 35));
  APPEND_C ("FOREST GREEN", new wxColour (35, 142, 35));
  APPEND_C ("GOLD", new wxColour (204, 127, 50));
  APPEND_C ("GOLDENROD", new wxColour (219, 219, 112));
#ifdef wx_msw
  APPEND_C ("GREY", new wxColour (128, 128, 128));
#else
  APPEND_C ("GREY", new wxColour (192, 192, 192));
#endif
  APPEND_C ("GREEN", new wxColour (0, 255, 0));
  APPEND_C ("GREEN YELLOW", new wxColour (147, 219, 112));
  APPEND_C ("INDIAN RED", new wxColour (79, 47, 47));
  APPEND_C ("KHAKI", new wxColour (159, 159, 95));
  APPEND_C ("LIGHT BLUE", new wxColour (191, 216, 216));
#ifdef wx_msw
  APPEND_C ("LIGHT GREY", new wxColour (192, 192, 192));
#else
  APPEND_C ("LIGHT GREY", new wxColour (168, 168, 168));
#endif
  APPEND_C ("LIGHT STEEL BLUE", new wxColour (143, 143, 188));
  APPEND_C ("LIME GREEN", new wxColour (50, 204, 50));
  APPEND_C ("LIGHT MAGENTA", new wxColour (255, 0, 255));
  APPEND_C ("MAGENTA", new wxColour (255, 0, 255));
  APPEND_C ("MAROON", new wxColour (142, 35, 107));
  APPEND_C ("MEDIUM AQUAMARINE", new wxColour (50, 204, 153));
  APPEND_C ("MEDIUM GREY", new wxColour (100, 100, 100));
  APPEND_C ("MEDIUM BLUE", new wxColour (50, 50, 204));
  APPEND_C ("MEDIUM FOREST GREEN", new wxColour (107, 142, 35));
  APPEND_C ("MEDIUM GOLDENROD", new wxColour (234, 234, 173));
  APPEND_C ("MEDIUM ORCHID", new wxColour (147, 112, 219));
  APPEND_C ("MEDIUM SEA GREEN", new wxColour (66, 111, 66));
  APPEND_C ("MEDIUM SLATE BLUE", new wxColour (127, 0, 255));
  APPEND_C ("MEDIUM SPRING GREEN", new wxColour (127, 255, 0));
  APPEND_C ("MEDIUM TURQUOISE", new wxColour (112, 219, 219));
  APPEND_C ("MEDIUM VIOLET RED", new wxColour (219, 112, 147));
  APPEND_C ("MIDNIGHT BLUE", new wxColour (47, 47, 79));
  APPEND_C ("NAVY", new wxColour (35, 35, 142));
  APPEND_C ("ORANGE", new wxColour (204, 50, 50));
  APPEND_C ("ORANGE RED", new wxColour (255, 0, 127));
  APPEND_C ("ORCHID", new wxColour (219, 112, 219));
  APPEND_C ("PALE GREEN", new wxColour (143, 188, 143));
  APPEND_C ("PINK", new wxColour (188, 143, 234));
  APPEND_C ("PLUM", new wxColour (234, 173, 234));
  APPEND_C ("PURPLE", new wxColour (176, 0, 255));
  APPEND_C ("RED", new wxColour (255, 0, 0));
  APPEND_C ("SALMON", new wxColour (111, 66, 66));
  APPEND_C ("SEA GREEN", new wxColour (35, 142, 107));
  APPEND_C ("SIENNA", new wxColour (142, 107, 35));
  APPEND_C ("SKY BLUE", new wxColour (50, 153, 204));
  APPEND_C ("SLATE BLUE", new wxColour (0, 127, 255));
  APPEND_C ("SPRING GREEN", new wxColour (0, 255, 127));
  APPEND_C ("STEEL BLUE", new wxColour (35, 107, 142));
  APPEND_C ("TAN", new wxColour (219, 147, 112));
  APPEND_C ("THISTLE", new wxColour (216, 191, 216));
  APPEND_C ("TURQUOISE", new wxColour (173, 234, 234));
  APPEND_C ("VIOLET", new wxColour (79, 47, 79));
  APPEND_C ("VIOLET RED", new wxColour (204, 50, 153));
  APPEND_C ("WHEAT", new wxColour (216, 216, 191));
  APPEND_C ("WHITE", new wxColour (255, 255, 255));
  APPEND_C ("YELLOW", new wxColour (255, 255, 0));
  APPEND_C ("YELLOW GREEN", new wxColour (153, 204, 50));
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
  else return NULL;
#else
  else {
    XColor xcolour;

#ifdef wx_motif
    Display *display = XtDisplay(wxTheApp->topLevel) ;
#endif
#ifdef wx_xview
    Xv_Screen screen = xv_get(xview_server, SERVER_NTH_SCREEN, 0);
    Xv_opaque root_window = xv_get(screen, XV_ROOT);
    Display *display = (Display *)xv_get(root_window, XV_DISPLAY);
#endif

    wxColour *col;

    /* MATTHEW: [4] Use wxGetMainColormap */
    if (XParseColor(display, wxGetMainColormap(display), colour,&xcolour)) {
      unsigned char r = (unsigned char)(xcolour.red >> 8);
      unsigned char g = (unsigned char)(xcolour.green >> 8);
      unsigned char b = (unsigned char)(xcolour.blue >> 8);
      
      col = new wxColour(r, g, b);
      col->Lock(1);
    } else {  
      if (!aux) {
	aux = new wxHashTable(wxKEY_STRING, 20);
	Initialize(); /* fills in aux */
      }

      col = (wxColour *)aux->Get(colour);
      if (!col)
	return NULL;
    }

    Append(colour, col);

    return col;
  }
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

  wxNORMAL_FONT = new wxFont (12, wxMODERN, wxNORMAL, wxNORMAL);
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
  wxGREY_PEN = new wxPen ("GREY", 0, wxSOLID);
  wxMEDIUM_GREY_PEN = new wxPen ("MEDIUM GREY", 0, wxSOLID);
  wxLIGHT_GREY_PEN = new wxPen ("LIGHT GREY", 0, wxSOLID);

  wxWHITE_PEN->Lock(1);
  wxBLACK_PEN->Lock(1);

  wxBLUE_BRUSH = new wxBrush ("BLUE", wxSOLID);
  wxGREEN_BRUSH = new wxBrush ("GREEN", wxSOLID);
  wxWHITE_BRUSH = new wxBrush ("WHITE", wxSOLID);
  wxBLACK_BRUSH = new wxBrush ("BLACK", wxSOLID);
  wxTRANSPARENT_BRUSH = new wxBrush ("BLACK", wxTRANSPARENT);
  wxCYAN_BRUSH = new wxBrush ("CYAN", wxSOLID);
  wxRED_BRUSH = new wxBrush ("RED", wxSOLID);
  wxGREY_BRUSH = new wxBrush ("GREY", wxSOLID);
  wxMEDIUM_GREY_BRUSH = new wxBrush ("MEDIUM GREY", wxSOLID);
  wxLIGHT_GREY_BRUSH = new wxBrush ("LIGHT GREY", wxSOLID);

  wxWHITE_BRUSH->Lock(1);
  wxBLACK_BRUSH->Lock(1);

  wxBLACK = new wxColour ("BLACK");
  wxWHITE = new wxColour ("WHITE");
  wxRED = new wxColour ("RED");
  wxBLUE = new wxColour ("BLUE");
  wxGREEN = new wxColour ("GREEN");
  wxCYAN = new wxColour ("CYAN");
  wxLIGHT_GREY = new wxColour ("LIGHT GREY");

  wxSTANDARD_CURSOR = new wxCursor (wxCURSOR_ARROW);
  wxHOURGLASS_CURSOR = new wxCursor (wxCURSOR_WAIT);
  wxCROSS_CURSOR = new wxCursor (wxCURSOR_CROSS);
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

  while (node = list->NextNode(i)) {
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
  int i;
  wxChildNode *node;

  if (!colour)
    return NULL;

  while (node = list->NextNode(i)) {
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

  while (node = list->NextNode(i)) {
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

  "PostScriptTeletype__", "${PostScriptModern,$[weight],$[style]}",

#if !WX_NORMALIZED_PS_FONTS
  "PostScriptScript__", "Zapf-Chancery-MediumItalic",
#endif

#ifdef wx_x
  "ScreenMedium", "medium",
  "ScreenBold", "bold",
  "ScreenLight", "light",
  "ScreenStraight", "r",
  "ScreenItalic", "i",
  "ScreenSlant", "o",

  /* MATTHEW: [4] "Family" -> "Base" */
  "ScreenSystemBase", "misc-fixed", /* MATTHEW: [10] *-* is bad */
  "ScreenDefaultBase", "misc-fixed", /* MATTHEW: [10] *-* is bad */
  "ScreenRomanBase", "*-times",
  "ScreenDecorativeBase", "*-helvetica",
  "ScreenModernBase", "*-courier",
  "ScreenTeletypeBase", "*-lucidatypewriter", /* MATTHEW: [4] Not courier */
  "ScreenSwissBase", "*-lucida",
  "ScreenScriptBase", "*-zapfchancery",

  /* MATTHEW: [4] Use ${ScreenStdSuffix} */
  "ScreenStdSuffix", "-${Screen$[weight]}-${Screen$[style]}"
    "-normal-*-*-%d-*-*-*-*-*-*",

  "ScreenDefault__",
  "+-${ScreenDefaultBase}${ScreenStdSuffix}",
  "ScreenRoman__",
  "+-${ScreenRomanBase}${ScreenStdSuffix}",
  "ScreenDecorative__",
  "+-${ScreenDecorativeBase}${ScreenStdSuffix}",
  "ScreenModern__",
  "+-${ScreenModernBase}${ScreenStdSuffix}",
  "ScreenTeletype__",
  "+-${ScreenTeletypeBase}${ScreenStdSuffix}",
  "ScreenSwiss__",
  "+-${ScreenSwissBase}${ScreenStdSuffix}",
  "ScreenScript__",
  "+-${ScreenScriptBase}${ScreenStdSuffix}",
#else
  /* MATTHEW: [4] don't specify family, weight && style... */
  "ScreenSystem__", "MS Sans Serif",
  "ScreenDefault__", "MS Sans Serif",
  "ScreenRoman__", "Times New Roman",
  "ScreenDecorative__", "Modern",
  "ScreenModern__", "Courier New",
  "ScreenTeletype__", "${ScreenModern$[weight];$[style]}",
  "ScreenSwiss__", "Arial",
  "ScreenScript__", "Script",
#endif
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
  Bool isroman;
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
}

typedef char *a_charptr;

/* MATTHEW: [14] Pass in weight & style and just do one */
void wxSuffixMap::Initialize(const char *resname, const char *devresname,
			     int wt, int st)
{
  const char *weight, *style; /* MATTHEW: [4] const */
  char *v;
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

#ifdef wx_msw
      /* MATTHEW: [4] For msw, there's a meaningful default */
      if (!v)
	v = copystring(resname);
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
	  src = (v ? v : resname);
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
  }

  item->name = copystring(resname);

  /* MATTHEW: [14] Delay this: */
#if 0
  item->screen.Initialize(resname, "Screen");
  item->printing.Initialize(resname, "PostScript");
  item->afm.Initialize(resname, "Afm");
#endif

  table->Put(fontid, item);
}

/* MATTHEW: [4] Add */
int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;

  if ((id = GetFontId(name)))
    return id;

  id = GetNewFontId();
  Initialize(id, family, name);

  return id;
}

char *wxFontNameDirectory::GetScreenName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* MATTHEW: [14] Check for init */
  if (!item->screen.map[wt][st])
    item->screen.Initialize(item->name, "Screen", wt, st);

  return item->screen.map[wt][st];
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* MATTHEW: [14] Check for init */
  if (!item->printing.map[wt][st])
    item->printing.Initialize(item->name, "PostScript", wt, st);

  return item->printing.map[wt][st];
}

char *wxFontNameDirectory::GetAFMName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* MATTHEW: [14] Check for init */
  if (!item->afm.map[wt][st])
    item->afm.Initialize(item->name, "Afm", wt, st);

  return item->afm.map[wt][st];
}

char *wxFontNameDirectory::GetFontName(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  return item->name;
}

int wxFontNameDirectory::GetFontId(const char *name) /* MATTHEW: [4] const */
{
  wxNode *node;

  table->BeginFind();

  while ((node = table->Next())) {
    wxFontNameItem *item = (wxFontNameItem *)node->Data();
    if (!strcmp(name, item->name))
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

