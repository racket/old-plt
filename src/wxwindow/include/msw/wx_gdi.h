/*
 * File:	wx_gdi.h
 * Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_gdih
#define wx_gdih

#include "wb_gdi.h"

#ifdef MZ_PRECISE_GC
class Graphics;
class GraphicsPath;
class Pen;
class Brush;
class Matrix;
START_XFORM_SKIP;
#endif

#include <gdiplus.h>

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

// Font
class wxFont: public wxbFont
{
 public:
  HFONT screen_cfont;
  HFONT general_cfont;
  wxList *rotated_font;

  wxFont(void);
  wxFont(int PointSize, int Family, int Style, int Weight, Bool underlined = FALSE, 
	 int smoothing = wxSMOOTHING_DEFAULT, Bool sip = FALSE, double Rotation = 0.0);
  wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	 Bool underlined = FALSE, int smoothing = wxSMOOTHING_DEFAULT, Bool sip = FALSE);
  ~wxFont(void);
  Bool Create(int PointSize, int Family, int Style, int Weight, Bool underlined,
	      int smoothing, Bool sip, double Rotation);
  HFONT BuildInternalFont(HDC dc, Bool screen_font = TRUE, double angle = 0.0);
  inline HFONT GetInternalFont(HDC dc, double angle = 0.0) { return BuildInternalFont(dc, TRUE, angle); }

  Bool ScreenGlyphAvailable(int c);
  Bool GlyphAvailable(int c, HDC hdc, int screen_font);
};

class wxColourMap: public wxObject
{
 public:
  HPALETTE ms_palette;
  wxColourMap(void);
  ~wxColourMap(void);
};

#define wxColorMap wxColourMap

// Pen
class wxPen: public wxbPen
{
 public:
  double old_width;
  int old_style;
  int old_join;
  int old_cap;
  int old_nb_dash;
  wxDash *old_dash;
  wxBitmap *old_stipple;
  COLORREF old_color;
  Gdiplus::Pen *g_p;

  HPEN cpen;
  HPEN my_old_cpen;

  wxPen(void);
  wxPen(wxColour *col, double width, int style);
  wxPen(const char *col, double width, int style);
  ~wxPen(void);

  void ChangePen();
  HPEN SelectPen(HDC dc);

  Gdiplus::Pen *GraphicsPen();
  void ReleaseGraphics();
};

int wx2msPenStyle(int wx_style);

// Brush
class wxBrush: public wxbBrush
{
 public:
  HBRUSH cbrush;
  HBRUSH my_old_cbrush;
  int old_style;
  wxBitmap *old_stipple;
  COLORREF old_color;
  Gdiplus::Brush *g_b;

  wxBrush(void);
  wxBrush(wxColour *col, int style);
  wxBrush(const char *col, int style);
  ~wxBrush(void);

  void ChangeBrush();
  HBRUSH SelectBrush(HDC dc);

  Gdiplus::Brush *GraphicsBrush();
  void ReleaseGraphics();
};

// Bitmap
class wxDC;
class wxItem;

class wxBitmap: public wxObject
{
 protected:
  int width;
  int height;
  int depth;
  int is_dib;
  Bool ok;
  int numColors;
  wxColourMap *bitmapColourMap;
 public:
  wxBitmap *mask;
  HBITMAP ms_bitmap;
  HBITMAP label_bitmap; /* Gray background, drawn with mask */
  HBITMAP button_label_bitmap; /* White background, drawn with mask */
  void *accounting;
  wxDC *selectedInto; // So bitmap knows whether it's been selected into
                      // a device context (for error checking)
  Bool selectedIntoDC;

  wxBitmap(void); // Platform-specific

  // Initialize with raw data
  wxBitmap(char bits[], int width, int height);

#if USE_XPM_IN_MSW
  // Initialize with XPM data
  wxBitmap(char **data, wxItem *anItem = NULL);
#endif

  // Load a file or resource
  wxBitmap(char *name, long flags = 0, wxColour *bg = NULL);

  // If depth is omitted, will create a bitmap compatible with the display
  wxBitmap(int width, int height, Bool b_and_w = FALSE);
  ~wxBitmap(void);

  virtual Bool Create(int width, int height, int depth = -1);
  virtual Bool LoadFile(char *name, long flags = 0, wxColour *bg = NULL);
  virtual Bool SaveFile(char *name, int type, int quality = 75, wxColourMap *cmap = NULL);

  inline Bool Ok(void) { return ok; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }
  inline int GetDepth(void) { return depth; }
  inline void SetWidth(int w) { width = w; }
  inline void SetHeight(int h) { height = h; }
  inline void SetDepth(int d) { depth = d; }
  inline void SetOk(Bool isOk) { ok = isOk; }
  inline wxColourMap *GetColourMap(void) { return bitmapColourMap; }
  inline void SetColourMap(wxColourMap *cmap) { bitmapColourMap = cmap; }
  inline void SetMask(wxBitmap *newmask) { mask = newmask; }
  inline wxBitmap *GetMask(void) { return mask; }

  void *ChangeToDIBSection(Bool copy_old = FALSE);
  Bool IsDIB();

  HBITMAP GetLabelBitmap(Bool panel_bg);
  void ReleaseLabel();
};

// Cursor
class wxCursor: public wxBitmap
{
 public:
  HCURSOR ms_cursor;
  Bool destroyCursor;
  wxCursor(void);
  wxCursor(char bits[], int width, int height);
  wxCursor(wxBitmap *bm, wxBitmap *mask, int hotSpotX = 0, int hotSpotY = 0);
  wxCursor(int cursor_type);
  ~wxCursor(void);
};

#endif // wx_gdih
