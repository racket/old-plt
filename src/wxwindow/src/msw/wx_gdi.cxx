/*
 * File:	wx_gdi.cc
 * Purpose:	GDI (Graphics Device Interface) objects and functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
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

extern int read_JPEG_file(char *filename, wxBitmap *bm);
extern int write_JPEG_file(char *filename, wxBitmap *bm, int quality_val);

extern int wx_read_png(char *file_name, wxBitmap *bm, int w_mask, wxColour *bg);
extern int wx_write_png(char *file_name, wxBitmap *bm);

wxFont::wxFont(void)
{
  COUNT_P(font_count);

  Create(12, wxDEFAULT, wxNORMAL, wxNORMAL, FALSE, wxSMOOTHING_DEFAULT, FALSE);
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxFont::wxFont(int PointSize, int Family, int Style, int Weight, Bool Underlined, int Smoothing, Bool sip):
  wxbFont(PointSize, Family, Style, Weight, Underlined, Smoothing)
{
  COUNT_P(font_count);

  Create(PointSize, Family, Style, Weight, Underlined, Smoothing, sip);
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, Bool Underlined, int Smoothing, Bool sip):
  wxbFont(PointSize, Family, Style, Weight, Underlined, Smoothing, sip)
{
  int id;

  COUNT_P(font_count);

  id = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);

  Create(PointSize, id, Style, Weight, Underlined, Smoothing, sip);
}

Bool wxFont::Create(int PointSize, int FontId, int Style, int Weight, Bool Underlined, int Smoothing, Bool sip)
{
  fontid = FontId;
  family = wxTheFontNameDirectory->GetFamily(fontid);
  style = Style;
  weight = Weight;
  point_size = PointSize;
  underlined = Underlined;
  smoothing = Smoothing;
  size_in_pixels = sip;

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

static int CALLBACK check_font_charset(ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme,
				       DWORD FontType, LPARAM lParam)
{
  *(int *)lParam = lpelfe->elfLogFont.lfCharSet;
  return 0;
}

HFONT wxFont::BuildInternalFont(HDC dc, Bool screenFont)
{
  int nHeight;
  HFONT cfont;
  BYTE ff_italic;
  int ff_weight = 0;
  int ff_family = 0;
  char *ff_face = NULL;
  int charset = ANSI_CHARSET;
  Bool ff_underline = underlined;
  int ff_qual;
    
  if (screenFont && screen_cfont)
    return screen_cfont;
  if (!screenFont && general_cfont)
    return general_cfont;

  if (screenFont && !size_in_pixels) {
    int dpi;
    dpi = ::GetDeviceCaps(dc, LOGPIXELSY);
    nHeight = MulDiv(point_size, dpi, 72);
  } else
    nHeight = point_size;
  
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
    ff_family = FF_DONTCARE;
  }
  
  // Determine the charset:
  {
    LOGFONT lf;
    lf.lfCharSet = DEFAULT_CHARSET;
    if (strlen(ff_face) < 32)
      strcpy(lf.lfFaceName, ff_face);
    else {
      memcpy(lf.lfFaceName, ff_face, 31);
      lf.lfFaceName[32] = NULL;
    }
    lf.lfPitchAndFamily = 0;
    EnumFontFamiliesEx(dc, &lf, (FONTENUMPROC)check_font_charset, (LPARAM)&charset, 0);
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

  if (smoothing == wxSMOOTHING_DEFAULT) {
    if ((family == wxMODERN) && (nHeight > 8) && (nHeight < 14))
      ff_qual = ANTIALIASED_QUALITY;
    else
      ff_qual = PROOF_QUALITY;
  } else if (smoothing == wxSMOOTHING_PARTIAL)
    ff_qual = ANTIALIASED_QUALITY;
  else if (smoothing == wxSMOOTHING_ON) {
#ifndef CLEARTYPE_QUALITY
# define CLEARTYPE_QUALITY 5
#endif
    ff_qual = CLEARTYPE_QUALITY;
  } else
    ff_qual = NONANTIALIASED_QUALITY;
  
  cfont = CreateFont(-nHeight, 0, 0, 0,ff_weight,ff_italic,(BYTE)ff_underline,
		     0, charset, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		     ff_qual, DEFAULT_PITCH | ff_family, ff_face);
  
  if (!cfont) {
    /* Try defaulting to family: */
    ff_face = wxTheFontNameDirectory->GetScreenName(family, weight, style);
    cfont = CreateFont(-nHeight, 0, 0, 0,ff_weight,ff_italic,(BYTE)ff_underline,
		       0, charset, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		       ff_qual, DEFAULT_PITCH | ff_family, ff_face);
  }

  if (!cfont)
    cfont = CreateFont(12, 0, 0, 0,FW_NORMAL,0,(BYTE)0,
		       0, charset, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		       ff_qual, DEFAULT_PITCH | FF_SWISS, NULL);

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

wxColourMap::~wxColourMap(void)
{
  if (ms_palette)
    DeleteRegisteredGDIObject(ms_palette);
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
  Bool must_change = FALSE;
  COLORREF ms_colour = 0;
  wxBitmap *bm;

  if (style==wxTRANSPARENT)
    return;

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

  bm = GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (join==wxJOIN_ROUND        &&
      cap==wxCAP_BUTT           &&
      style!=wxUSER_DASH        &&
      !bm                       &&
      (width || style == wxSOLID)) {
    HPEN naya;
    naya = CreatePen(wx2msPenStyle(style), width, ms_colour);
    cpen = naya;
  } else {
    LOGBRUSH logb;
    int xwidth = width;
    DWORD ms_style;
    wxDash *real_dash;

    ms_style = wx2msPenStyle(style);

    if (!width) {
      xwidth = 1;
      ms_style |= PS_COSMETIC;
    } else
      ms_style |= PS_GEOMETRIC;
    
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

    if (style==wxUSER_DASH && nb_dash && dash) {
      int i;
      real_dash = new wxDash[nb_dash];
      for (i=0;i<nb_dash;i++) {
        real_dash[i] = dash[i] * xwidth;
      }
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
    HPEN nullPen;
    nullPen = (HPEN)::GetStockObject(NULL_PEN);
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
  Bool must_change = FALSE;
  COLORREF ms_colour = 0;
  wxBitmap *bm;

  if (style==wxTRANSPARENT)
    return;

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

  bm = GetStipple();
  if (bm && !bm->Ok())
    bm = NULL;

  if (bm) {
    cbrush = CreatePatternBrush(bm->ms_bitmap);
  } else {
    switch (style) {
    case wxTRANSPARENT:
      break;
    case wxBDIAGONAL_HATCH:
      {
	cbrush = CreateHatchBrush(HS_BDIAGONAL, ms_colour);
	break;
      }
    case wxCROSSDIAG_HATCH:
      {
	cbrush = CreateHatchBrush(HS_DIAGCROSS, ms_colour);
	break;
      }
    case wxFDIAGONAL_HATCH:
      {
	cbrush = CreateHatchBrush(HS_FDIAGONAL, ms_colour);
	break;
      }
    case wxCROSS_HATCH:
      {
	cbrush = CreateHatchBrush(HS_CROSS, ms_colour);
	break;
      }
    case wxHORIZONTAL_HATCH:
      {
	cbrush = CreateHatchBrush(HS_HORIZONTAL, ms_colour);
	break;
      }
    case wxVERTICAL_HATCH:
      {
	cbrush = CreateHatchBrush(HS_VERTICAL, ms_colour);
	break;
      }
    case wxSOLID:
    default:
      {
	cbrush = CreateSolidBrush(ms_colour);
	break;
      }
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
    HBRUSH nullBrush;
    nullBrush = (HBRUSH)::GetStockObject(NULL_BRUSH);
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

static wxMemoryDC *temp_mdc, *temp_mask_mdc;

wxCursor::wxCursor(wxBitmap *bm, wxBitmap *mask, int hotSpotX, int hotSpotY)
{
  int w, h, bw, bh, i, j, delta, bit, s;
  unsigned char r, g, b;
  unsigned char *ands, *xors;
  wxColour *c;
  wxMemoryDC *mask_dc;

  __type = wxTYPE_CURSOR;
  destroyCursor = FALSE;
  ms_cursor = 0;
  ok = FALSE;

  /* Get the allowed size for cursors: */
  w = GetSystemMetrics(SM_CXCURSOR);
  h = GetSystemMetrics(SM_CYCURSOR);

  bw = bm->GetWidth();
  bh = bm->GetHeight();

  /* If the given cursor doesn't fit, give up. (MrEd constrains the
     bitmap to be 16x16, which surely will fit.) */
  if ((bw > w) || (bh > h))
    return;

  /* Make read-only DCs for reading bits from the bitmaps: */
  if (!temp_mdc) {
    wxREGGLOB(temp_mdc);
    wxREGGLOB(temp_mask_mdc);
    temp_mdc = new wxMemoryDC(1);
    temp_mask_mdc = new wxMemoryDC(1);
  }

  temp_mdc->SelectObject(bm);
  /* Might fail, so we double-check: */
  if (!temp_mdc->GetObject())
    return;
  /* If bm and mask arethe same, use one DC (since re-selecting
     will fail, anyway). */
  if (mask == bm) {
    mask_dc = temp_mdc;
  } else {
    temp_mask_mdc->SelectObject(mask);
    if (!temp_mask_mdc->GetObject()) {
      temp_mdc->SelectObject(NULL);
      return;
    }
    mask_dc = temp_mask_mdc;
  }

  c = new wxColour(); /* to recieve bit values */

  /* Windows wants cursor data in terms of an "and" bit array and
     "xor" bit array. */
  s = (w * h) >> 3; /* size of arrays in bytes */
  ands = new uchar[s];
  xors = new uchar[s];

  /* Init arrays to a value that means "the screen" */
  for (i = 0; i < s; i++) {
    ands[i] = 255;
    xors[i] = 0;
  }

  /* Read bits from mask and bm and set the corresponding bits in
     `ands' and `xors' */
  bit = 128;
  delta = 0;
  for (j = 0; j < bh; j++) {
    for (i = 0; i < w; i++) {
      if (i < bw) {
	mask_dc->GetPixel(i, j, c);
	c->Get(&r, &g, &b);
	
	/* black bit in mask? */
	if (!r && !g && !b) {
	  temp_mdc->GetPixel(i, j, c);
	  c->Get(&r, &g, &b);
	  
	  if (!r && !g && !b) {
	    /* black bit for cursor */
	    ands[delta] -= bit;
	  } else {
	    /* white bit for cursor */
	    ands[delta] -= bit;	    
	    xors[delta] |= bit;	    
	  }
	} /* otherwise, leave as screen */
      }

      bit = bit >> 1;
      if (!bit) {
	delta++;
	bit = 128;
      }
    }
  }

  ms_cursor = CreateCursor(wxhInstance, hotSpotX, hotSpotY, w, h, ands, xors);

  /* Clean up */
  temp_mdc->SelectObject(NULL);
  temp_mask_mdc->SelectObject(NULL);

  ok = !!ms_cursor;
}

// Cursors by stock number
wxCursor::wxCursor(int cursor_type)
{
  __type = wxTYPE_CURSOR;
  switch (cursor_type) {
  case wxCURSOR_WAIT:
  case wxCURSOR_WATCH:
    {
      ms_cursor = LoadCursor(NULL, IDC_WAIT);
      break;
    }
  case wxCURSOR_IBEAM:
    {
      ms_cursor = LoadCursor(NULL, IDC_IBEAM);
      break;
    }
  case wxCURSOR_CROSS:
    {
      ms_cursor = LoadCursor(NULL, IDC_CROSS);
      break;
    }
  case wxCURSOR_SIZENWSE:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZENWSE);
      break;
    }
  case wxCURSOR_SIZENESW:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZENESW);
      break;
    }
  case wxCURSOR_SIZEWE:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZEWE);
      break;
    }
  case wxCURSOR_SIZENS:
    {
      ms_cursor = LoadCursor(NULL, IDC_SIZENS);
      break;
    }
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
    {
      ms_cursor = LoadCursor(NULL, IDC_ARROW);
      break;
    }
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
  HDC dc;
  Bool flag;
  int num;

  dc = ::GetDC(NULL);
  num = GetDeviceCaps(dc, NUMCOLORS);
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
  HDC dc;
  int planes, bitsPerPixel, depth;
  dc = ::GetDC(NULL);
  planes = GetDeviceCaps(dc, PLANES);
  bitsPerPixel = GetDeviceCaps(dc, BITSPIXEL);
  depth = planes*bitsPerPixel;
  ReleaseDC(NULL, dc);
  return depth;
}

// Get size of display
void wxDisplaySize(int *width, int *height, int flags)
{
  RECT r;

  if (!flags && SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0)) {
    *width = (r.right - r.left);
    *height = (r.bottom - r.top);
  } else {
    HDC dc;
    dc = ::GetDC(NULL);
    *width = GetDeviceCaps(dc, HORZRES);
    *height = GetDeviceCaps(dc, VERTRES);
    ReleaseDC(NULL, dc);
  }
}

void wxDisplayOrigin(int *x, int *y)
{
  RECT r;

  if (SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0)) {
    *x = r.left;
    *y = r.top;
  } else {
    *x = 0;
    *y = 0;
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
  WXGC_IGNORE(this, selectedInto);
}

static char *map;

wxBitmap::wxBitmap(char bits[], int the_width, int the_height)
{
  int i, j;
  int rowwidth, offset;
  char *copy;
  int sp, cp;

  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  width = the_width;
  height = the_height;
  depth = 1;
  numColors = 0;
  bitmapColourMap = NULL;

  rowwidth = ((width + 7) / 8);
  if (rowwidth % sizeof(WORD))
    /* byte-aligned => word aligned */
    offset = 1;
  else
    offset = 0;

  if (!map) {
    wxREGGLOB(map);
    map = new char[256];
    for (i = 0; i < 256; i++) {
      j = (((i & 0x1) << 7)
	   | ((i & 0x2) << 5)
	   | ((i & 0x4) << 3)
	   | ((i & 0x8) << 1)
	   | ((i & 0x10) >> 1)
	   | ((i & 0x20) >> 3)
	   | ((i & 0x40) >> 5)
	   | ((i & 0x80) >> 6));
      j = 0xFF ^ j;
      map[i] = (char)j;
    }
  }

  copy = new char[(rowwidth + offset) * height];
  sp = 0; cp = 0;
  for (i = 0; i < height; i++) {
    for (j = 0; j < rowwidth; j++, sp++, cp++) {
      copy[cp] = map[((unsigned char *)bits)[sp]];
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
  WXGC_IGNORE(this, selectedInto);
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
  WXGC_IGNORE(this, selectedInto);
}

wxBitmap::wxBitmap(char *bitmap_file, long flags, wxColour *bg)
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

  LoadFile(bitmap_file, (int)flags, bg);
  WXGC_IGNORE(this, selectedInto);
}

#if USE_XPM_IN_MSW
// Create from data
wxBitmap::wxBitmap(char **data, wxItem *WXUNUSED(anItem))
{
  XImage *ximage;
  int     ErrorStatus;
  XpmAttributes xpmAttr;
  HDC     dc;

  COUNT_P(bitmap_count);

  __type = wxTYPE_BITMAP;
  selectedInto = NULL;
  bitmapColourMap = NULL;

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
      BITMAP  bm;

      /* ximage is malloced and contains bitmap and attributes */
      ms_bitmap = ximage->bitmap;
      RegisterGDIObject(ms_bitmap);

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
  WXGC_IGNORE(this, selectedInto);
}
#endif

Bool wxBitmap::Create(int w, int h, int d)
{
  width = w;
  height = h;
  depth = d;

  if (d > 0) {
    ms_bitmap = CreateBitmap(w, h, d, 1, NULL);
  } else {
    HDC dc;
    dc = GetDC(NULL);
    ms_bitmap = ::CreateCompatibleBitmap(dc, w, h);
    ReleaseDC(NULL, dc);
    depth = wxDisplayDepth();
  }
  RegisterGDIObject(ms_bitmap);
  if (ms_bitmap)
    ok = TRUE;
  else
    ok = FALSE;

  is_dib = 0;

  return ok;
}

void *wxBitmap::ChangeToDIBSection(Bool copy_old)
{
  /* Called only when the bitmap is not selected! */
  BITMAPINFO bmp = { { sizeof(BITMAPINFOHEADER), width, height, 1, 32 } };
  HBITMAP bm;
  void *pBits;
  
  bm = CreateDIBSection(NULL, &bmp, DIB_RGB_COLORS, &pBits, NULL, NULL);
  
  if (bm) {
    if (ms_bitmap) {
      if (copy_old) {
	int copied_ok = 0;
	HDC src, dest;

	src = CreateCompatibleDC(NULL);
	if (src) {
	  dest = CreateCompatibleDC(NULL);
	  if (dest) {
	    HANDLE src_old, dest_old;

	    src_old = SelectObject(src, ms_bitmap);
	    if (src_old != ERROR) {
	      dest_old = SelectObject(dest, bm);
	      if (dest_old != ERROR) {
		BitBlt(dest, 0, 0, width, height,
		       src, 0, 0,
		       SRCCOPY);
		copied_ok = 1;
		SelectObject(dest, dest_old);
	      }
	      SelectObject(dest, src_old);
	    }
	    DeleteDC(dest);
	  }
	  DeleteDC(src);
	}

	if (!copied_ok) {
	  DeleteObject(bm);
	  return NULL;
	}
      }

      DeleteRegisteredGDIObject(ms_bitmap);
    }
    ms_bitmap = bm;
    RegisterGDIObject(ms_bitmap);
    is_dib = 1;
    return pBits;
  } else
    return NULL;
}

Bool wxBitmap::IsDIB()
{
  return is_dib;
}

extern int wxsGetImageType(char *fn);

Bool wxBitmap::LoadFile(char *bitmap_file, long flags, wxColour *bg)  
{
  Bool getMask;
  wxMemoryDC *oldSel;

  if (selectedIntoDC)
    return FALSE;

  getMask = !!(flags & wxBITMAP_TYPE_MASK);

  if (!flags || (flags == wxBITMAP_TYPE_MASK))
    flags = wxsGetImageType(bitmap_file);

  /* Nevermind the palette */
  flags |= wxBITMAP_DISCARD_COLOURMAP;

  oldSel = (wxMemoryDC *)selectedInto;

  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  is_dib = 0;
  
  if (oldSel)
    oldSel->SelectObject(NULL);

  if (ms_bitmap) {
    DeleteRegisteredGDIObject(ms_bitmap);
    ms_bitmap = NULL;
  }

  if (flags & wxBITMAP_TYPE_BMP_RESOURCE)
  {
    ms_bitmap = LoadBitmap(wxhInstance, bitmap_file);
    if (ms_bitmap) {
      BITMAP bm;
      RegisterGDIObject(ms_bitmap);
      ok = TRUE;
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
      HDC glob_dc;
      glob_dc = GetDC(NULL);
      ms_bitmap = CreateBitmap(w, h, 1, 1, NULL);
      RegisterGDIObject(ms_bitmap);
      ReleaseDC(NULL, glob_dc);
      if (ms_bitmap) {
	HDC dc;

	dc = ::CreateCompatibleDC(NULL);
	
	if (dc)
	  {
	    HGDIOBJ orig;
	    int p;
	    COLORREF white = RGB(255, 255, 255);
	    COLORREF black = RGB(0, 0, 0);
	    int i, j;
				
	    orig = ::SelectObject(dc, ms_bitmap);

	    for (i = 0, p = 0; i < h; i++) {
	      for (j = 0; j < w; j++, p++) {
		::SetPixelV(dc, j, i, c[p] ? black : white);
	      }
	    }

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
      if (dc) {
	int errorStatus;
	xpmAttr.valuemask = XpmReturnPixels;
	errorStatus = XpmReadFileToImage(&dc, bitmap_file, &ximage, (XImage **) NULL, &xpmAttr);
	DeleteDC(dc);
	if (errorStatus == XpmSuccess) {
	  BITMAP  bm;

	  ms_bitmap = ximage->bitmap;
	  RegisterGDIObject(ms_bitmap);

	  GetObject(ms_bitmap, sizeof(bm), (LPSTR) & bm);

	  width = (bm.bmWidth);
	  height = (bm.bmHeight);
	  depth = (bm.bmPlanes * bm.bmBitsPixel);
	  numColors = xpmAttr.npixels;
	  XpmFreeAttributes(&xpmAttr);
	  XImageFree(ximage);
	
	  ok = TRUE;
	} else {
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
    if (!success && cmap) {
      delete cmap;
      cmap = NULL;
    }
    if (cmap)
      bitmapColourMap = cmap;
  }
#endif
  else if (flags & wxBITMAP_TYPE_GIF)
  {
    Bool success = FALSE;
    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadGifIntoBitmap(bitmap_file, this, NULL, getMask);
    else
      success = wxLoadGifIntoBitmap(bitmap_file, this, NULL, getMask);
  }
  else if (flags & wxBITMAP_TYPE_JPEG)
  {
    Bool success;
    success = read_JPEG_file(bitmap_file, this);
    if (!success) {
      if (ms_bitmap) {
	DeleteRegisteredGDIObject(ms_bitmap);
	ms_bitmap = NULL;
      }
      ok = FALSE;
    }
  }
  else if (flags & wxBITMAP_TYPE_PNG)
  {
    Bool success;
    success = wx_read_png(bitmap_file, this, getMask, bg);
    if (!success) {
      if (ms_bitmap) {
	DeleteRegisteredGDIObject(ms_bitmap);
	ms_bitmap = NULL;
      }
      ok = FALSE;
    }
  }
  
  if (oldSel && ok)
    oldSel->SelectObject(this);

  return ok;
}

wxBitmap::~wxBitmap(void)
{
  COUNT_M(bitmap_count);

  if (selectedInto) {
    ((wxMemoryDC *)selectedInto)->SelectObject(NULL);
    selectedInto = NULL;
  }
  if (ms_bitmap) {
    DeleteRegisteredGDIObject(ms_bitmap);
  }
  ms_bitmap = NULL;

  if (bitmapColourMap)
    delete bitmapColourMap;
}

Bool wxBitmap::SaveFile(char *filename, int typ, int quality, wxColourMap *cmap)
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
	char *c;
	int p;
	HGDIOBJ orig = NULL;
	int i, j;
	HDC dc;

	c = new char[width * height];
	
	dc = (selectedInto 
	      ? selectedInto->cdc
	      : CreateCompatibleDC(NULL));
			
	if (dc && !selectedInto) {
	  orig = SelectObject(dc, ms_bitmap);
	  if (!orig) {
	    DeleteDC(dc);
	    dc = NULL;
	  }
	}

	if (!dc) return FALSE;

	for (i = 0, p = 0; i < height; i++) {
	  for (j = 0; j < width; j++, p++) {
	    int v;
	    v = (::GetPixel(dc, j, i) ? 1 : 0);
	    c[p] = v;
	  }
	}
	
	if (!selectedInto) {
	  SelectObject(dc, orig);
	  DeleteDC(dc);
	}

	return wxSaveXBM(filename, c, width, height);
	break;
      }
    case wxBITMAP_TYPE_XPM:
      {
	HGDIOBJ orig = NULL;
	Visual *visual = NULL;
	XImage  ximage;
	HDC dc;
	int errorStatus;

	dc = (selectedInto 
	      ? selectedInto->cdc
	      : CreateCompatibleDC(NULL));

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
	errorStatus = XpmWriteFileFromImage(&dc, filename,
					    &ximage, (XImage *) NULL,
					    (XpmAttributes *) NULL);

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
    case wxBITMAP_TYPE_JPEG:
      return write_JPEG_file(filename, this, quality);
      break;
    case wxBITMAP_TYPE_PNG:
      return wx_write_png(filename, this);
      break;
    default:
      break;
  }
  return FALSE;
}

