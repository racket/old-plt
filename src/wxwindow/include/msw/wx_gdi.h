/*
 * File:	wx_gdi.h
 * Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_gdi.h	1.2 5/9/94" */


#ifndef wx_gdih
#define wx_gdih

#include "wb_gdi.h"

#ifdef IN_CPROTO
typedef       void    *wxFont ;
typedef       void    *wxColourMap;
typedef       void    *wxPen;
typedef       void    *wxBrush;
typedef       void    *wxCursor;
typedef       void    *wxBitmap;
#else

// Font
class wxFont: public wxbFont
{
  DECLARE_DYNAMIC_CLASS(wxFont)

 public:
  HFONT screen_cfont;
  HFONT general_cfont;

  wxFont(void);
  wxFont(int PointSize, int Family, int Style, int Weight, Bool underlined = FALSE);
  /* MATTHEW: [4] New font system */
  wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	 Bool underlined = FALSE);
  ~wxFont(void);
  Bool Create(int PointSize, int Family, int Style, int Weight, Bool underlined = FALSE);
  HFONT BuildInternalFont(HDC dc, Bool screen_font = TRUE);
  inline HFONT GetInternalFont(HDC dc) { return BuildInternalFont(dc, TRUE); }
};

class wxColourMap: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxColourMap)

 public:
 HPALETTE ms_palette;
  wxColourMap(void);
  wxColourMap(const int n, const unsigned char *red, const unsigned char *green, const unsigned char *blue);
  ~wxColourMap(void);
  Bool Create(const int n, const unsigned char *red, const unsigned char *green, const unsigned char *blue);
  int GetPixel(const unsigned char red, const unsigned char green, const unsigned char blue);
  Bool GetRGB(const int pixel, unsigned char *red, unsigned char *green, unsigned char *blue);
};

#define wxColorMap wxColourMap

// Pen
class wxPen: public wxbPen
{
  DECLARE_DYNAMIC_CLASS(wxPen)

 public:
  int old_width;
  int old_style;
  int old_join ;
  int old_cap ;
  int old_nb_dash ;
  wxDash *old_dash ;
  wxBitmap *old_stipple ;
  COLORREF old_color ;

  HPEN cpen;
  HPEN my_old_cpen ;

  wxPen(void);
  wxPen(wxColour& col, int width, int style);
  wxPen(const char *col, int width, int style);
  ~wxPen(void);

  void ChangePen() ;
  HPEN SelectPen(HDC dc) ;

};

int wx2msPenStyle(int wx_style);

// Brush
class wxBrush: public wxbBrush
{
  DECLARE_DYNAMIC_CLASS(wxBrush)

 public:
  HBRUSH cbrush;
  HBRUSH my_old_cbrush ;
  int old_style;
  wxBitmap *old_stipple ;
  COLORREF old_color ;


  wxBrush(void);
  wxBrush(wxColour& col, int style);
  wxBrush(const char *col, int style);
  ~wxBrush(void);

  void ChangeBrush() ;
  HBRUSH SelectBrush(HDC dc) ;
};

// Bitmap
class wxDC;
class wxItem;

class wxBitmap: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxBitmap)

 protected:
  int width;
  int height;
  int depth;
  Bool ok;
  int numColors;
  wxColourMap *bitmapColourMap;
 public:
  HBITMAP ms_bitmap;
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
  wxBitmap(char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_BMP_RESOURCE);

  // If depth is omitted, will create a bitmap compatible with the display
  wxBitmap(int width, int height, Bool b_and_w = FALSE);
  ~wxBitmap(void);

  virtual Bool Create(int width, int height, int depth = -1);
  virtual Bool LoadFile(char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_BMP_RESOURCE);
  virtual Bool SaveFile(char *name, int type, wxColourMap *cmap = NULL);

  inline Bool Ok(void) { return ok; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }
  inline int GetDepth(void) { return depth; }
  inline void SetWidth(int w) { width = w; }
  inline void SetHeight(int h) { height = h; }
  inline void SetDepth(int d) { depth = d; }
  inline void SetOk(Bool isOk) { ok = isOk; }
  inline wxColourMap *GetColourMap(void) { return bitmapColourMap; }
  inline void SetColourMap(wxColourMap *cmap) { bitmapColourMap = cmap ; }
};

// Cursor
class wxCursor: public wxBitmap
{
  DECLARE_DYNAMIC_CLASS(wxCursor)

 public:
  HCURSOR ms_cursor;
  Bool destroyCursor;
  wxCursor(void);
  wxCursor(char bits[], int width, int height);
  wxCursor(const char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_CUR_RESOURCE,
   int hotSpotX = 0, int hotSpotY = 0);
  wxCursor(int cursor_type);
  ~wxCursor(void);
};

#endif // IN_CPROTO
#endif // wx_gdih
