/*
 * File:	wx_dccan.h
 * Purpose:	Canvas device context declaration (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_dccan.h	1.2 5/9/94" */


#ifndef wx_dccanh
#define wx_dccanh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_dccan.h"

#ifdef IN_CPROTO
typedef       void    *wxCanvasDC ;
#else

#include <X11/Xlib.h>
#include <X11/Xutil.h>

class wxCanvas;
class wxCanvasDC: public wxbCanvasDC
{
  DECLARE_DYNAMIC_CLASS(wxCanvasDC)

  protected:
   /* MATTHEW: [7] Implement GetPixel */
  inline void FreeGetPixelCache(void) 
    { if (get_pixel_image_cache) DoFreeGetPixelCache(); }
  void DoFreeGetPixelCache(void);

 public:
  wxCanvas *canvas;
  // Every time a callback happens, these are set to point to the right values
  // for drawing calls to work
  GC gc;
  Display *display;

  Pixmap pixmap;
  int pixmapWidth;
  int pixmapHeight;
  Bool color;

  Region current_reg, onpaint_reg;

#ifdef wx_motif
  XFontStruct *xfont; /* MATTHEW: [4] Need to remember real font */
  GC gcBacking;
#endif
  int background_pixel;
  wxColour current_colour;
  int current_pen_width ;
  int current_pen_join ;
  int current_pen_cap ;
  int current_pen_nb_dash ;
  char *current_pen_dash ;
  wxBitmap *current_stipple ;
  int current_style ;
  int current_fill ; // Because wxPen::Style serves as 2 purposes,
		     // we need 2 vars.
		     // Else, try this:
		     // wxPen->SetStyle(wxDASH)
		     // some line, dashed: OK
		     // wxBrush->SetStyle(wxSOLID) ;
		     // some shape, filled: OK
		     // wxPen->SetStyle(wxSOLID)
		     // some line ==> it is dashed, wrong...

  wxBitmap *selected_pixmap;

  /* MATTHEW: [7] Implement GetPixel */
  XImage *get_pixel_image_cache;
  int get_pixel_cache_pos;
  XColor *get_pixel_color_cache;
  Bool get_pixel_cache_full;

  wxCanvasDC(void);

  // Create a DC corresponding to a canvas
  wxCanvasDC(wxCanvas *canvas);

  ~wxCanvasDC(void);

  void SetCanvasClipping() ;

  void GetSize(float *w, float *h);

  virtual void BeginDrawing(void) {} ;
  virtual void EndDrawing(void) {} ;

  void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE) ;
  Bool GetPixel(float x1, float y1, wxColour *col) ;

  /* MATHEW: [15] */
  void BeginSetPixel();
  void EndSetPixel();
  void SetPixel(float x1, float y1, wxColour *col) ;

  void DrawLine(float x1, float y1, float x2, float y2);
  void IntDrawLine(int x1, int y1, int x2, int y2);
  void CrossHair(float x, float y) ;
  void DrawArc(float x1,float y1,float x2,float y2,float xc,float yc);
  void DrawPoint(float x, float y);
  // Avoid compiler warning
  void DrawPoint(wxPoint& point) { wxbDC::DrawPoint(point); }
  void DrawLines(int n, wxPoint points[], float xoffset = 0, float yoffset = 0);
  void DrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0);
  // Avoid compiler warning
  void DrawLines(wxList *lines, float xoffset = 0, float yoffset = 0)
  { wxbDC::DrawLines(lines, xoffset, yoffset); }
  void DrawPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  // Avoid compiler warning
  void DrawPolygon(wxList *lines, float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE)
  { wxbDC::DrawPolygon(lines, xoffset, yoffset, fillStyle); }
  void DrawRectangle(float x, float y, float width, float height);
  void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20);
  void DrawEllipse(float x, float y, float width, float height);
  /* MATTHEW: [2] 16-bit fonts */
  void DrawText(const char *text, float x, float y, Bool use16 = FALSE);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetColourMap(wxColourMap *cmap);
  void SetBackground(wxColour *c);
  void SetClippingRect(float x, float y, float width, float height);
  wxRegion *GetClippingRegion();
  void SetClippingRegion(wxRegion *);

  float GetCharHeight(void);
  float GetCharWidth(void);
  /* MATTHEW: [2] 16-bit fonts */
  void GetTextExtent(const char *string, float *x, float *y,
                     float *descent = NULL, float *externalLeading = NULL, 
		     wxFont *theFont = NULL, Bool use16 = FALSE);
  Bool StartDoc(char *message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
  void SetMapMode(int mode);
  void SetUserScale(float x, float y);
  float DeviceToLogicalX(int x);
  float DeviceToLogicalY(int y);
  float DeviceToLogicalXRel(int x);
  float DeviceToLogicalYRel(int y);
  int LogicalToDeviceX(float x);
  int LogicalToDeviceY(float y);
  int LogicalToDeviceXRel(float x);
  int LogicalToDeviceYRel(float y);

  Bool Blit(float xdest, float ydest, float width, float height,
            wxBitmap *source, float xsrc, float ysrc, int rop = wxSOLID, wxColour *c = NULL);
  inline Bool CanGetTextExtent(void) { return TRUE; }
  inline Bool CanDrawBitmap(void) { return TRUE; }

  /* MATTHEW */
  void TryColour(wxColour *src, wxColour *dest);
};

class wxScreenDC: public wxCanvasDC
{
  DECLARE_DYNAMIC_CLASS(wxScreenDC)

 public:
  // Create a DC representing the whole screen
  wxScreenDC(void);
  ~wxScreenDC(void);
};

#endif // IN_CPROTO
#endif // wx_dccanh

