/*
 * File:	wx_gdi.cc
 * Purpose:	GDI (Graphics Device Interface) objects and functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#include "wx.h"

#include "..\..\utils\dib\dib.h"

#include "xpm34.h"
#include "wximgfil.h"
#include "wximgxbm.h"

// Resource counting
#if 0
int pen_count, brush_count, font_count, bitmap_count;
# define COUNT_P(c) c++
# define COUNT_M(c) --c
#else
# define COUNT_P(c) 
# define COUNT_M(c) 
#endif

void RegisterGDIObject(HANDLE x);
void DeleteRegisteredGDIObject(HANDLE x);

Bool wxMakeBitmapAndPalette(LPBITMAPINFOHEADER lpInfo, HPALETTE * phPal, HBITMAP * phBitmap);

wxFont::wxFont(void)
{
  COUNT_P(font_count);

  Create(12, wxDEFAULT, wxNORMAL, wxNORMAL, FALSE);
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxFont::wxFont(int PointSize, int Family, int Style, int Weight, Bool Underlined):
  wxbFont(PointSize, Family, Style, Weight, Underlined)
{
  COUNT_P(font_count);

  Create(PointSize, Family, Style, Weight, Underlined);
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, Bool Underlined):
  wxbFont(PointSize, Family, Style, Weight, Underlined)
{
  COUNT_P(font_count);

  Create(PointSize, 
	 wxTheFontNameDirectory->FindOrCreateFontId(Face, Family), 
	 Style, Weight, Underlined);
}

Bool wxFont::Create(int PointSize, int FontId, int Style, int Weight, Bool Underlined)
{
  fontid = FontId;
  family = wxTheFontNameDirectory->GetFamily(fontid);
  style = Style;
  weight = Weight;
  point_size = PointSize;
  underlined = Underlined;

  temporary = FALSE;

  screen_cfont = NULL;
  general_cfont = NULL;

  return TRUE;
}

wxFont::~wxFont()
{
  if (screen_cfont)
    DeleteRegisteredGDIObject(screen_cfont);
  if (general_cfont)
    DeleteRegisteredGDIObject(general_cfont);
  
  COUNT_M(font_count);
}

HFONT wxFont::BuildInternalFont(HDC dc, Bool screenFont)
{
  int nHeight;

  if (screenFont && screen_cfont)
    return screen_cfont;
  if (!screenFont && general_cfont)
    return general_cfont;

  if (screenFont) {
    int dpi;
    HDC dc2 = ::GetDC(NULL);
    dpi = ::GetDeviceCaps(dc, LOGPIXELSY);
    ::ReleaseDC(NULL, dc2);
    nHeight = point_size*dpi/72;
  } else
    nHeight = point_size;
  
  HFONT cfont;
  BYTE ff_italic;
  int ff_weight = 0;
  int ff_family = 0;
  char *ff_face = NULL;
  
  ff_face = wxTheFontNameDirectory->GetScreenName(fontid, weight, style);
  if (!*ff_face)
    ff_face = NULL;
  
  switch (family) {
  case wxSCRIPT:
    ff_family = FF_SCRIPT;
    break;
  case wxDECORATIVE: 
    ff_family = FF_DECORATIVE;
    break;
  case wxROMAN: 
    ff_family = FF_ROMAN;
    break;
  case wxTELETYPE:
  case wxMODERN:
  case wxSYMBOL:
    ff_family = FF_MODERN;
    break;
  case wxSWISS: 
    ff_family = FF_SWISS;
    break;
  case wxDEFAULT:
  default: 
    ff_family = FF_SWISS;
  }
  
  if (style == wxITALIC || style == wxSLANT)
    ff_italic = 1;
  else
    ff_italic = 0;
  
  if (weight == wxNORMAL)
    ff_weight = FW_NORMAL;
  else if (weight == wxLIGHT)
    ff_weight = FW_LIGHT;
  else if (weight == wxBOLD)
    ff_weight = FW_BOLD;
  
  Bool ff_underline = underlined;
  
  cfont = CreateFont(-nHeight, 0, 0, 0,ff_weight,ff_italic,(BYTE)ff_underline,
		     0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		     PROOF_QUALITY, DEFAULT_PITCH | ff_family, ff_face);
  
  if (!cfont) {
    /* Try defaulting to family: */
    ff_face = wxTheFontNameDirectory->GetScreenName(family, weight, style);
    cfont = CreateFont(-nHeight, 0, 0, 0,ff_weight,ff_italic,(BYTE)ff_underline,
		       0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		       PROOF_QUALITY, DEFAULT_PITCH | ff_family, ff_face);
  }

  if (!cfont)
    cfont = CreateFont(12, 0, 0, 0,FW_NORMAL,0,(BYTE)0,
		       0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		       PROOF_QUALITY, DEFAULT_PITCH | FF_SWISS, NULL);

  RegisterGDIObject((HANDLE)cfont);

  if (screenFont)
    screen_cfont = cfont;
  else
    general_cfont = cfont;

  return cfont;
}

/*
 * Colour map
 *
 */

wxColourMap::wxColourMap(void)
{
  ms_palette = 0;
}

wxColourMap::wxColourMap(const int n, const unsigned char *red, const unsigned char *green, const unsigned char *blue)
{
  Create(n, red, green, blue);
}

wxColourMap::~wxColourMap(void)
{
  if (ms_palette)
    DeleteRegisteredGDIObject(ms_palette);
}

Bool wxColourMap::Create(const int n, const unsigned char *red, const unsigned char *green, const unsigned char *blue)
{
  if (ms_palette)
    return FALSE;
    
  NPLOGPALETTE npPal = (NPLOGPALETTE)LocalAlloc(LMEM_FIXED, sizeof(LOGPALETTE) + 
						(WORD)n * sizeof(PALETTEENTRY));
  if (!npPal)
    return(FALSE);

  npPal->palVersion = 0x300;
  npPal->palNumEntries = n;

  int i;
  for (i = 0; i < n; i ++) {
    npPal->palPalEntry[i].peRed = red[i];
    npPal->palPalEntry[i].peGreen = green[i];
    npPal->palPalEntry[i].peBlue = blue[i];
    npPal->palPalEntry[i].peFlags = 0;
  }
  ms_palette = CreatePalette((LPLOGPALETTE)npPal);
  RegisterGDIObject(ms_palette);
  LocalFree((HANDLE)npPal);
  return TRUE;
}

int wxColourMap::GetPixel(const unsigned char red, const unsigned char green, const unsigned char blue)
{
  return ::GetNearestPaletteIndex(ms_palette, RGB(red, green, blue));
}

Bool wxColourMap::GetRGB(const int index, unsigned char *red, unsigned char *green, unsigned char *blue)
{
  if (index < 0 || index > 255)
    return FALSE;
  
  PALETTEENTRY entry;
  if (::GetPaletteEntries(ms_palette, index, 1, &entry)) {
    *red = entry.peRed;
    *green = entry.peGreen;
    *blue = entry.peBlue;
    return TRUE;
  } else
    return FALSE;
}

// Pens

wxPen::wxPen(void)
{
  wxColour *c;

  COUNT_P(pen_count);

  stipple = NULL;
  style = wxSOLID;
  width = 0;
  join = wxJOIN_ROUND;
  cap = wxCAP_ROUND;
  nb_dash = 0;
  dash = NULL;
  cpen = NULL;
  my_old_cpen = NULL;
  old_width = -1;
  old_style = -1;
  old_join  = -1;
  old_cap  = -1;
  old_nb_dash  = -1;
  old_dash  = NULL;
  old_color  = 0;
  old_stipple = NULL;

  c = new wxColour(wxBLACK);
  c->Lock(1);
  colour = c;
}

wxPen::~wxPen()
{
  COUNT_M(pen_count);

  if (cpen)
    DeleteRegisteredGDIObject(cpen);

  cpen = NULL;
}

wxPen::wxPen(wxColour *col, float Width, int Style)
{
  wxColour *c;

  COUNT_P(pen_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  stipple = NULL;
  width = Width;
  style = Style;
  join = wxJOIN_ROUND;
  cap = wxCAP_ROUND;
  nb_dash = 0;
  dash = NULL;
  cpen = NULL;
  my_old_cpen = NULL;
  old_width = -1;
  old_style = -1;
  old_join  = -1;
  old_cap  = -1;
  old_nb_dash  = -1;
  old_dash  = NULL;
  old_color  = 0;
  old_stipple = NULL;

  ChangePen();
}

wxPen::wxPen(const char *col, float Width, int Style)
{
  wxColour *c;

  COUNT_P(pen_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  stipple = NULL;
  width = Width;
  style = Style;
  join = wxJOIN_ROUND;
  cap = wxCAP_ROUND;
  nb_dash = 0;
  dash = NULL;
  cpen = NULL;
  old_width = -1;
  old_style = -1;
  old_join  = -1;
  old_cap  = -1;
  old_nb_dash  = -1;
  old_dash  = NULL;
  old_color  = 0;
  old_stipple = NULL;
    
  ChangePen();
}

void wxPen::ChangePen(void)
{
  if (style==wxTRANSPARENT)
    return;

  Bool must_change = FALSE;

  COLORREF ms_colour = 0;

  ms_colour = colour->pixel;

  if (cpen==NULL)
    must_change = TRUE;
  else
    must_change = !(width==old_width     &&
                    join==old_join       &&
                    cap==old_cap         &&
                    dash==old_dash       &&
                    nb_dash==old_nb_dash &&
                    style==old_style     &&
                    stipple==old_stipple &&
                    old_color==ms_colour);

  if (!must_change)
    return;

  old_width = width;
  old_join = join;
  old_cap = cap;
  old_dash = dash;
  old_nb_dash = nb_dash;
  old_style = style;
  old_stipple = stipple;
  old_color = ms_colour;

  if (cpen) {
    /* Note: the pen can't be selected anywhere if we're changing it, so
       delete is ok */
    DeleteRegisteredGDIObject(cpen);
    cpen = NULL;
  }

  wxBitmap *bm = GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (join==wxJOIN_ROUND        &&
      cap==wxCAP_BUTT           &&
      style!=wxUSER_DASH        &&
      !bm                       &&
      (width || style == wxSOLID))
    cpen = CreatePen(wx2msPenStyle(style), width, ms_colour);
  else {
    DWORD ms_style = wx2msPenStyle(style);
    int xwidth = width;

    if (!width) {
      xwidth = 1;
      ms_style |= PS_COSMETIC;
    } else
      ms_style |= PS_GEOMETRIC;
    
    LOGBRUSH logb;

    switch(join) {
    case wxJOIN_BEVEL: ms_style |= PS_JOIN_BEVEL; break;
    case wxJOIN_MITER: ms_style |= PS_JOIN_MITER; break;
    default:
    case wxJOIN_ROUND: ms_style |= PS_JOIN_ROUND; break;
    }

    switch(cap) {
    case wxCAP_PROJECTING: ms_style |= PS_ENDCAP_SQUARE; break;
    case wxCAP_BUTT:       ms_style |= PS_ENDCAP_FLAT;   break;
    default:
    case wxCAP_ROUND:      ms_style |= PS_ENDCAP_ROUND;  break;
    }

    if (bm) {
      logb.lbStyle = BS_PATTERN;
      logb.lbHatch = (LONG)stipple->ms_bitmap;
    } else {
      switch(style) {
      case wxBDIAGONAL_HATCH:
	logb.lbStyle = BS_HATCHED;
	logb.lbHatch = HS_BDIAGONAL;
	break;
      case wxCROSSDIAG_HATCH:
	logb.lbStyle = BS_HATCHED;
	logb.lbHatch = HS_DIAGCROSS;
	break;
      case wxFDIAGONAL_HATCH:
	logb.lbStyle = BS_HATCHED;
	logb.lbHatch = HS_FDIAGONAL;
	break;
      case wxCROSS_HATCH:
	logb.lbStyle = BS_HATCHED;
	logb.lbHatch = HS_CROSS;
	break;
      case wxHORIZONTAL_HATCH:
	logb.lbStyle = BS_HATCHED;
	logb.lbHatch = HS_HORIZONTAL;
	break;
      case wxVERTICAL_HATCH:
	logb.lbStyle = BS_HATCHED;
	logb.lbHatch = HS_VERTICAL;
	break;
      default:
	logb.lbStyle = BS_SOLID;
	break;
      }
    }
    logb.lbColor = ms_colour;
    wxDash *real_dash;
    if (style==wxUSER_DASH && nb_dash && dash) {
      real_dash = new wxDash[nb_dash];
      int i;
      for (i=0;i<nb_dash;i++)
        real_dash[i] = dash[i] * xwidth;
    } else
      real_dash = NULL;
    
    cpen = ExtCreatePen(ms_style, xwidth, &logb,
			style == wxUSER_DASH ? nb_dash : 0,
			real_dash);
  }

  RegisterGDIObject(cpen);

  return;
}

HPEN wxPen::SelectPen(HDC dc)
{
  HPEN prev_pen;

  if (cpen && style!=wxTRANSPARENT)
    prev_pen = (HPEN)::SelectObject(dc,cpen);
  else {
    HPEN nullPen = (HPEN)::GetStockObject(NULL_PEN);
    prev_pen = (HPEN)::SelectObject(dc , nullPen);
  }
  
  return prev_pen;
}

int wx2msPenStyle(int wx_style)
{
  int cstyle;

  switch (wx_style) {  
  case wxXOR_DOT:
  case wxDOT:
    cstyle = PS_DOT;
    break;
  case wxSHORT_DASH:
  case wxXOR_SHORT_DASH:
  case wxLONG_DASH:
  case wxXOR_LONG_DASH:
    cstyle = PS_DASH;
    break;
  case wxDOT_DASH:
  case wxXOR_DOT_DASH:
    cstyle = PS_DASHDOT;
    break;
  case wxTRANSPARENT:
    cstyle = PS_NULL;
    break;
  case wxUSER_DASH:
    cstyle = PS_DOT;
    break;
  case wxSOLID:
  default:
    cstyle = PS_SOLID;
    break;
  }

  return cstyle;
}


// Brushes

wxBrush::wxBrush(void)
{
  wxColour *c;

  COUNT_P(brush_count);
  
  c = new wxColour(wxBLACK);
  c->Lock(1);
  colour = c;

  style = wxSOLID;
  stipple = NULL;
  cbrush = NULL;
  old_color = 0;
  old_style = -1;
  old_stipple = NULL;
}

wxBrush::~wxBrush()
{
  COUNT_M(brush_count);

  if (cbrush)
    DeleteRegisteredGDIObject(cbrush);

  cbrush = NULL;
}

wxBrush::wxBrush(wxColour *col, int Style)
{
  wxColour *c;

  COUNT_P(brush_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  style = Style;
  stipple = NULL;
  cbrush = NULL;
  old_color = 0;
  old_style = -1;
  old_stipple = NULL;

  ChangeBrush();
}

void wxBrush::ChangeBrush(void) 
{
  if (style==wxTRANSPARENT)
    return;

  Bool must_change = FALSE;

  COLORREF ms_colour = 0;

  ms_colour = colour->pixel;

  if (cbrush==NULL)
    must_change = TRUE;
  else
    must_change = ((style != old_style)
		   || (stipple != old_stipple)
		   || (old_color != ms_colour));

  if (!must_change)
    return;

  if (cbrush) {
    /* Note: brush isn't selected anywhere if we can change it. */
    DeleteRegisteredGDIObject(cbrush);
    cbrush = NULL;
  }

  wxBitmap *bm = GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (bm) {
    cbrush = CreatePatternBrush(bm->ms_bitmap);
  } else {
    switch (style) {
    case wxTRANSPARENT:
      break;
    case wxBDIAGONAL_HATCH:
      cbrush = CreateHatchBrush(HS_BDIAGONAL, ms_colour);
      break;
    case wxCROSSDIAG_HATCH:
      cbrush = CreateHatchBrush(HS_DIAGCROSS, ms_colour);
      break;
    case wxFDIAGONAL_HATCH:
      cbrush = CreateHatchBrush(HS_FDIAGONAL, ms_colour);
      break;
    case wxCROSS_HATCH:
      cbrush = CreateHatchBrush(HS_CROSS, ms_colour);
      break;
    case wxHORIZONTAL_HATCH:
      cbrush = CreateHatchBrush(HS_HORIZONTAL, ms_colour);
      break;
    case wxVERTICAL_HATCH:
      cbrush = CreateHatchBrush(HS_VERTICAL, ms_colour);
      break;
      break;
    case wxSOLID:
    default:
      cbrush = CreateSolidBrush(ms_colour);
      break;
    }
  }

  RegisterGDIObject(cbrush);

  old_style = style;
  old_stipple = stipple;
  old_color = ms_colour;
}

HBRUSH wxBrush::SelectBrush(HDC dc)
{
  HBRUSH prev_brush;

  if (cbrush && style!=wxTRANSPARENT) {
    prev_brush = (HBRUSH)::SelectObject(dc, cbrush);
  } else {
    HBRUSH nullBrush = (HBRUSH)::GetStockObject(NULL_BRUSH);
    prev_brush = (HBRUSH)::SelectObject(dc, nullBrush);
  }

  return prev_brush;
}

wxBrush::wxBrush(const char *col, int Style)
{
  wxColour *c;

  COUNT_P(brush_count);

  c = new wxColour(col);
  c->Lock(1);
  colour = c;

  style = Style;
  stipple = NULL;
  cbrush = NULL;
  old_color = 0;
  old_style = -1;
  old_stipple = NULL;
}

// Cursors

wxCursor::wxCursor(void)
{
  __type = wxTYPE_CURSOR;
  width = 32; height = 32;
  ms_cursor = NULL;
  destroyCursor = FALSE;
//  wxTheCursorList->Append(this);
}

wxCursor::wxCursor(char WXUNUSED(bits)[], int WXUNUSED(width), int WXUNUSED(height))
{
  __type = wxTYPE_CURSOR;
  ms_cursor = NULL;
  destroyCursor = FALSE;
//  wxTheCursorList->Append(this);
}

wxCursor::wxCursor(const char *cursor_file, long flags, int hotSpotX, int hotSpotY)
{
  __type = wxTYPE_CURSOR;
  destroyCursor = FALSE;
  ms_cursor = 0;
  ok = FALSE;
  if (flags & wxBITMAP_TYPE_CUR_RESOURCE)
  {
    ms_cursor = LoadCursor(wxhInstance, cursor_file);
    if (ms_cursor)
      ok = TRUE;
    else
      ok = FALSE;
  }
  else if (flags & wxBITMAP_TYPE_CUR)
  {
#if USE_RESOURCE_LOADING_IN_MSW
    ms_cursor = ReadCursorFile((char *)cursor_file, wxhInstance, &width, &height);
#endif
  }
  else if (flags & wxBITMAP_TYPE_ICO)
  {
#if USE_RESOURCE_LOADING_IN_MSW
    ms_cursor = IconToCursor((char *)cursor_file, wxhInstance, hotSpotX, hotSpotY, &width, &height);
#endif
  }
  else if (flags & wxBITMAP_TYPE_BMP)
  {
#if USE_RESOURCE_LOADING_IN_MSW
    HBITMAP hBitmap = 0;
    HPALETTE hPalette = 0;
    Bool success = ReadDIB((char *)cursor_file, &hBitmap, &hPalette);
    if (!success)
      return;
    if (hPalette)
      DeleteObject(hPalette);
    POINT pnt;
    pnt.x = hotSpotX;
    pnt.y = hotSpotY;
    ms_cursor = MakeCursorFromBitmap(wxhInstance, hBitmap, &pnt);
    DeleteObject(hBitmap);
    if (ms_cursor)
      ok = TRUE;
#endif
  }
//  wxTheCursorList->Append(this);
}

// Cursors by stock number
wxCursor::wxCursor(int cursor_type)
{
  __type = wxTYPE_CURSOR;
  switch (cursor_type)
  {
    case wxCURSOR_WAIT:
  case wxCURSOR_WATCH:
      ms_cursor = LoadCursor(NULL, IDC_WAIT);
      break;
    case wxCURSOR_IBEAM:
      ms_cursor = LoadCursor(NULL, IDC_IBEAM);
      break;
    case wxCURSOR_CROSS:
      ms_cursor = LoadCursor(NULL, IDC_CROSS);
      break;
    case wxCURSOR_SIZENWSE:
      ms_cursor = LoadCursor(NULL, IDC_SIZENWSE);
      break;
    case wxCURSOR_SIZENESW:
      ms_cursor = LoadCursor(NULL, IDC_SIZENESW);
      break;
    case wxCURSOR_SIZEWE:
      ms_cursor = LoadCursor(NULL, IDC_SIZEWE);
      break;
    case wxCURSOR_SIZENS:
      ms_cursor = LoadCursor(NULL, IDC_SIZENS);
      break;
    case wxCURSOR_CHAR:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
    case wxCURSOR_HAND:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_HAND");
      break;
    }
    case wxCURSOR_BULLSEYE:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_BULLSEYE");
      break;
    }
    case wxCURSOR_PENCIL:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PENCIL");
      break;
    }
    case wxCURSOR_MAGNIFIER:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_MAGNIFIER");
      break;
    }
    case wxCURSOR_NO_ENTRY:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_NO_ENTRY");
      break;
    }
    case wxCURSOR_LEFT_BUTTON:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
    case wxCURSOR_RIGHT_BUTTON:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
    case wxCURSOR_MIDDLE_BUTTON:
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
    case wxCURSOR_SIZING:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_SIZING");
      break;
    }
    case wxCURSOR_SPRAYCAN:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_ROLLER");
      break;
    }
    case wxCURSOR_PAINT_BRUSH:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PBRUSH");
      break;
    }
    case wxCURSOR_POINT_LEFT:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PLEFT");
      break;
    }
    case wxCURSOR_POINT_RIGHT:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_PRIGHT");
      break;
    }
    case wxCURSOR_QUESTION_ARROW:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_QARROW");
      break;
    }
    case wxCURSOR_BLANK:
    {
      ms_cursor = LoadCursor(wxhInstance, "wxCURSOR_BLANK");
      break;
    }
    default:
    case wxCURSOR_ARROW:
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
  }
//  wxTheCursorList->Append(this);
  ok = !!ms_cursor;
}

wxCursor::~wxCursor(void)
{
}

// Global cursor setting
void wxSetCursor(wxCursor *cursor)
{
  if (cursor && cursor->ms_cursor)
    ::SetCursor(cursor->ms_cursor);

  wxFlushEvents();
}

// Misc. functions

// Return TRUE if we have a colour display
Bool wxColourDisplay(void)
{
  HDC dc = ::GetDC(NULL);
  Bool flag;
  int num = GetDeviceCaps(dc, NUMCOLORS);
  if ((num < 0) || (num > 2))
    flag = TRUE;
  else
    flag = FALSE;
  ReleaseDC(NULL, dc);
  return flag;
}

// Returns depth of screen
int wxDisplayDepth(void)
{
  HDC dc = ::GetDC(NULL);
  int planes = GetDeviceCaps(dc, PLANES);
  int bitsPerPixel = GetDeviceCaps(dc, BITSPIXEL);
  int depth = planes*bitsPerPixel;
  ReleaseDC(NULL, dc);
  return depth;
}

// Get size of display
void wxDisplaySize(int *width, int *height)
{
  RECT r;

  if (SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0)) {
    *width = (r.right - r.left);
    *height = (r.bottom - r.top);
  } else {
    HDC dc = ::GetDC(NULL);
    *width = GetDeviceCaps(dc, HORZRES); *height = GetDeviceCaps(dc, VERTRES);
    ReleaseDC(NULL, dc);
  }
}

wxBitmap::wxBitmap(void)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  ms_bitmap = NULL;
  selectedInto = NULL;
  numColors = 0;
  bitmapColourMap = NULL;
  WXGC_IGNORE(selectedInto);
}

wxBitmap::wxBitmap(char bits[], int the_width, int the_height)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  width = the_width;
  height = the_height;
  depth = 1;
  numColors = 0;
  bitmapColourMap = NULL;

  int rowwidth = ((width + 7) / 8), offset;
  if (rowwidth % sizeof(WORD))
    /* byte-aligned => word aligned */
    offset = 1;
  else
    offset = 0;

  char *copy = new char[(rowwidth + offset) * height], *sp, *cp;
  sp = bits; cp = copy;
  int i, j;
  for (i = 0; i < height; i++) {
    for (j = 0; j < rowwidth; j++, sp++, cp++) {
      *cp = 0xFF ^ *sp;
    }
    cp += offset;
  }

  ms_bitmap = CreateBitmap(the_width, the_height, 1, 1, copy);

  RegisterGDIObject(ms_bitmap);

  if (ms_bitmap)
    ok = TRUE;
  else
    ok = FALSE;

  selectedInto = NULL;
  WXGC_IGNORE(selectedInto);
}

wxBitmap::wxBitmap(int w, int h, Bool b_and_w)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = w;
  height = h;
  depth = b_and_w ? 1 : -1;
  numColors = 0;
  selectedInto = NULL;
  bitmapColourMap = NULL;

  (void)Create(w, h, b_and_w ? 1 : -1);
  WXGC_IGNORE(selectedInto);
}

wxBitmap::wxBitmap(char *bitmap_file, long flags)
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  selectedInto = NULL;
  numColors = 0;
  bitmapColourMap = NULL;

  LoadFile(bitmap_file, (int)flags);
  WXGC_IGNORE(selectedInto);
}

#if USE_XPM_IN_MSW
// Create from data
wxBitmap::wxBitmap(char **data, wxItem *WXUNUSED(anItem))
{
  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  selectedInto = NULL;
  bitmapColourMap = NULL;

  XImage *ximage;
  int     ErrorStatus;
  XpmAttributes xpmAttr;
  HDC     dc;

  ok = FALSE;
  numColors = 0;

  dc = ::CreateCompatibleDC(NULL);	/* memory DC */

  if (dc)
  {
    xpmAttr.valuemask = XpmReturnInfos;	/* get infos back */
    ErrorStatus = XpmCreateImageFromData(&dc, data,
         &ximage, (XImage **) NULL, &xpmAttr);

    if (ErrorStatus == XpmSuccess)
    {
      /* ximage is malloced and contains bitmap and attributes */
      ms_bitmap = ximage->bitmap;
      RegisterGDIObject(ms_bitmap);

      BITMAP  bm;
      GetObject(ms_bitmap, sizeof(bm), (LPSTR) & bm);

      width = (bm.bmWidth);
      height = (bm.bmHeight);
      depth = (bm.bmPlanes * bm.bmBitsPixel);
      numColors = xpmAttr.npixels;
      XpmFreeAttributes(&xpmAttr);

      XImageFree(ximage);	// releases the malloc, but does not detroy
			// the bitmap
      ok = TRUE;

    } else
    {
      ok = False;
//  XpmDebugError(ErrorStatus, NULL);
    }
    DeleteDC(dc);
  }
  WXGC_IGNORE(selectedInto);
}
#endif

Bool wxBitmap::Create(int w, int h, int d)
{
  width = w;
  height = h;
  depth = d;

  if (d > 0)
    ms_bitmap = CreateBitmap(w, h, d, 1, NULL);
  else {
    HDC dc = GetDC(NULL);
    ms_bitmap = ::CreateCompatibleBitmap(dc, w, h);
    ReleaseDC(NULL, dc);
    depth = wxDisplayDepth();
  }
  RegisterGDIObject(ms_bitmap);
  if (ms_bitmap)
    ok = TRUE;
  else
    ok = FALSE;
  return ok;
}

extern int wxsGetImageType(char *fn);

Bool wxBitmap::LoadFile(char *bitmap_file, long flags)  
{
  if (selectedIntoDC)
    return FALSE;

  if (!flags)
    flags = wxsGetImageType(bitmap_file);

  /* Nevermind the palette */
  flags |= wxBITMAP_DISCARD_COLOURMAP;

  wxMemoryDC *oldSel = (wxMemoryDC *)selectedInto;

  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  
  if (oldSel)
    oldSel->SelectObject(NULL);

  if (ms_bitmap) {
    DeleteRegisteredGDIObject(ms_bitmap);
    ms_bitmap = NULL;
  }

  if (flags & wxBITMAP_TYPE_BMP_RESOURCE)
  {
    ms_bitmap = LoadBitmap(wxhInstance, bitmap_file);
    if (ms_bitmap)
    {
      RegisterGDIObject(ms_bitmap);
      ok = TRUE;
      BITMAP bm;
      GetObject(ms_bitmap, sizeof(BITMAP), (LPSTR) &bm);
      width = bm.bmWidth;
      height = bm.bmHeight;
      depth = bm.bmPlanes;
    }
  }

  else if (flags & wxBITMAP_TYPE_XBM)
  {
    char *c;
    int w, h;

    c = wxLoadXBM(bitmap_file, &w, &h);
    if (c) {
      HDC glob_dc = GetDC(NULL);
      ms_bitmap = CreateBitmap(w, h, 1, 1, NULL);
      RegisterGDIObject(ms_bitmap);
      ReleaseDC(NULL, glob_dc);
      if (ms_bitmap) {
	HDC dc = ::CreateCompatibleDC(NULL);
			
	if (dc)
	  {
	    HGDIOBJ orig = ::SelectObject(dc, ms_bitmap);
	    char *p;
	    COLORREF white = RGB(255, 255, 255);
	    COLORREF black = RGB(0, 0, 0);
	    int i, j;
				
	    for (i = 0, p = c; i < h; i++)
	      for (j = 0; j < w; j++, p++)
		::SetPixelV(dc, j, i, *p ? black : white);

	    ::SelectObject(dc, orig);
	    DeleteDC(dc);

	    ok = TRUE;
	    width = w;
	    height = h;
	    depth = 1;
	  } else {
	    DeleteRegisteredGDIObject(ms_bitmap);
	    ms_bitmap = NULL;
	  }

      }
    }
  }
#if USE_XPM_IN_MSW
  else if (flags & wxBITMAP_TYPE_XPM)
  {
    XImage *ximage;
    XpmAttributes xpmAttr;
    HDC     dc;

    ok = False;
	 dc = ::CreateCompatibleDC(NULL);
	 if (dc)
	 {
		xpmAttr.valuemask = XpmReturnPixels;
		int errorStatus = XpmReadFileToImage(&dc, bitmap_file, &ximage, (XImage **) NULL, &xpmAttr);
		DeleteDC(dc);
		if (errorStatus == XpmSuccess)
		{
	ms_bitmap = ximage->bitmap;
	RegisterGDIObject(ms_bitmap);

	BITMAP  bm;
	GetObject(ms_bitmap, sizeof(bm), (LPSTR) & bm);

	width = (bm.bmWidth);
	height = (bm.bmHeight);
	depth = (bm.bmPlanes * bm.bmBitsPixel);
	numColors = xpmAttr.npixels;
		  XpmFreeAttributes(&xpmAttr);
	XImageFree(ximage);
	
	ok = TRUE;
	}
      else
      {
		  ok = FALSE;
      }
    }
  }
#endif
#if USE_IMAGE_LOADING_IN_MSW
  else if ((flags & wxBITMAP_TYPE_BMP) 
	  || (flags & wxBITMAP_TYPE_ANY))
  {
    wxColourMap *cmap = NULL;
    Bool success = FALSE;
    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadIntoBitmap(bitmap_file, this);
    else
      success = wxLoadIntoBitmap(bitmap_file, this, &cmap);
    if (!success && cmap)
    {
      delete cmap;
		cmap = NULL;
    }
    if (cmap)
      bitmapColourMap = cmap;
  }
#endif
  else if (flags & wxBITMAP_TYPE_GIF)
  {
    wxColourMap *cmap = NULL;
    Bool success = FALSE;
    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadGifIntoBitmap(bitmap_file, this);
    else
      success = wxLoadGifIntoBitmap(bitmap_file, this, &cmap);
    if (!success && cmap)
    {
      delete cmap;
		cmap = NULL;
    }
    if (cmap)
      bitmapColourMap = cmap;
  }
  
  if (oldSel && ok)
	oldSel->SelectObject(this);

  return ok;
}

wxBitmap::~wxBitmap(void)
{
  COUNT_M(bitmap_count);

  if (selectedInto)
  {
    ((wxMemoryDC *)selectedInto)->SelectObject(NULL);
  }
  if (ms_bitmap)
  {
    DeleteRegisteredGDIObject(ms_bitmap);
  }
  ms_bitmap = NULL;

  if (bitmapColourMap)
    delete bitmapColourMap;
}

Bool wxBitmap::SaveFile(char *filename, int typ, wxColourMap *cmap)
{
  if (!ok) return FALSE;

  switch (typ)
  {
#if USE_IMAGE_LOADING_IN_MSW
    case wxBITMAP_TYPE_BMP:
    {
      wxColourMap *actualCmap = cmap;
      if (!actualCmap)
        actualCmap = bitmapColourMap;
      return wxSaveBitmap(filename, this, actualCmap);
      break;
    }
#endif
    case wxBITMAP_TYPE_XBM:
    {
	  char *c, *p;
      HGDIOBJ orig = NULL;
	  int i, j;

	  c = new char[width * height];

	  HDC dc = selectedInto 
		       ? selectedInto->cdc
		       : CreateCompatibleDC(NULL);
			
	  if (dc && !selectedInto) {
	    orig = SelectObject(dc, ms_bitmap);
		if (!orig) {
			DeleteDC(dc);
			dc = NULL;
		}
	  }

	  if (!dc) return FALSE;

	  for (i = 0, p = c; i < height; i++)
		for (j = 0; j < width; j++, p++)
		  *p = (::GetPixel(dc, j, i) ? 1 : 0);
	
	  if (!selectedInto) {
		  SelectObject(dc, orig);
		  DeleteDC(dc);
	  }

      return wxSaveXBM(filename, c, width, height);
      break;
    }
#if USE_XPM_IN_MSW
    case wxBITMAP_TYPE_XPM:
    {
      HGDIOBJ orig = NULL;

      Visual *visual = NULL;
      XImage  ximage;

	  HDC dc = selectedInto 
		       ? selectedInto->cdc
		       : CreateCompatibleDC(NULL);

	  if (dc && !selectedInto) {
	    orig = SelectObject(dc, ms_bitmap);
		if (!orig) {
			DeleteDC(dc);
			dc = NULL;
		}
	  }

	  if (!dc) return FALSE;

	  /* for following SetPixel */
      /* fill the XImage struct 'by hand' */
	  ximage.width = width; ximage.height = height;
	  ximage.depth = depth; ximage.bitmap = ms_bitmap;
	  int errorStatus = XpmWriteFileFromImage(&dc, filename,
						  &ximage, (XImage *) NULL, (XpmAttributes *) NULL);

      if (!selectedInto) {
		 SelectObject(dc, orig);
		 DeleteDC(dc);
	  }

	  if (errorStatus == XpmSuccess)
	    return TRUE;		/* no error */
	  else
	    return FALSE;

      break;
    }
#endif
    default:
      break;
  }
  return FALSE;
}

