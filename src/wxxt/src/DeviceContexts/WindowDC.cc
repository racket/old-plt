/*								-*- C++ -*-
 * $Id: WindowDC.cc,v 1.31 1999/11/18 23:25:14 mflatt Exp $
 *
 * Purpose: device context to draw drawables
 *          (windows and pixmaps, even if pixmaps are covered by wxMemoryDC)
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
#pragma implementation "WindowDC.h"
#endif

#define  Uses_XLib
#define  Uses_wxWindowDC
#define  Uses_wxMemoryDC
#define  Uses_wxList
#include "wx.h"

#define  UseXtRegions
#include "wx_rgn.h"

#include <math.h>
#include <string.h>

extern "C" { 
#include "XWidgets/wxAllocColor.h"
};

// constant to convert radian to degree
#define RAD2DEG 57.2957795131

// shift between wxWindows RGB- and XColor RGB-values
// (necessary because the values specify an intensity)
#define SHIFT (8*(sizeof(short int)-sizeof(char)))

// translate defines from wxDefines.h to XLib constants
static int join_style[] = { JoinBevel, JoinMiter, JoinRound };
static int cap_style[]  = { CapRound, CapProjecting, CapButt, CapNotLast };
static int fill_rule[]  = { EvenOddRule, WindingRule };

// hatches, used for stippling in any DC
#include <DeviceContexts/bdiag.xbm>
#include <DeviceContexts/fdiag.xbm>
#include <DeviceContexts/cdiag.xbm>
#include <DeviceContexts/horiz.xbm>
#include <DeviceContexts/verti.xbm>
#include <DeviceContexts/cross.xbm>
#define  num_hatches 6

/* MATTHEW: */
// This normalizes the graphics code to behave inm a standard way when
// WX_STANDARD_GRAPHICS is 1.
#if WX_STANDARD_GRAPHICS
#define WX_GC_CF 1
#else
#define WX_GC_CF 0
#endif

#define IS_COLOR (DEPTH > 1)

#define FreeGetPixelCache() if (X->get_pixel_image_cache) DoFreeGetPixelCache()

Pixmap* hatch_bitmaps = NULL;

//-----------------------------------------------------------------------------
// create and destroy wxWindowDC
//-----------------------------------------------------------------------------

wxWindowDC::wxWindowDC(void) : wxDC()
{
    __type = wxTYPE_DC_CANVAS;

    device = wxDEVICE_CANVAS;

    X = new wxWindowDC_Xintern; // allocate space for X data

    PEN_GC = BRUSH_GC = TEXT_GC = BG_GC = NULL;
    USER_REG = EXPOSE_REG = CURRENT_REG = NULL;
    DPY = NULL;
    SCN = NULL;
    DRAWABLE = 0;
    DRAW_WINDOW = 0;
    WIDTH = HEIGHT = DEPTH = 0;

    /* MATTHEW: [5] Implement GetPixel */
    X->get_pixel_image_cache = NULL;

    if (!hatch_bitmaps) {
	Display *dpy = wxAPP_DISPLAY;
	Window  win  = RootWindow(dpy, DefaultScreen(dpy));
	hatch_bitmaps = new Pixmap[num_hatches];
	hatch_bitmaps[0] = XCreateBitmapFromData(dpy, win, bdiag_bits,
						 bdiag_width, bdiag_height);
	hatch_bitmaps[1] = XCreateBitmapFromData(dpy, win, cdiag_bits,
						 cdiag_width, cdiag_height);
	hatch_bitmaps[2] = XCreateBitmapFromData(dpy, win, fdiag_bits,
						 fdiag_width, fdiag_height);
	hatch_bitmaps[3] = XCreateBitmapFromData(dpy, win, cross_bits,
						 cross_width, cross_height);
	hatch_bitmaps[4] = XCreateBitmapFromData(dpy, win, horiz_bits,
						 horiz_width, horiz_height);
	hatch_bitmaps[5] = XCreateBitmapFromData(dpy, win, verti_bits,
						 verti_width, verti_height);
    }

    current_background_color = *wxWHITE;
    current_brush = wxTRANSPARENT_BRUSH;
    current_brush->Lock(1);
    current_pen = wxBLACK_PEN;
    current_pen->Lock(1);
    current_font = wxNORMAL_FONT;
}

wxWindowDC::~wxWindowDC(void)
{
    if (current_pen) current_pen->Lock(-1);
    if (current_brush) current_brush->Lock(-1);

    Destroy();
}

//-----------------------------------------------------------------------------
// drawing methods
//-----------------------------------------------------------------------------

Bool wxWindowDC::Blit(float xdest, float ydest, float w, float h, wxBitmap *src,
		      float xsrc, float ysrc, int rop, wxColor *dcolor)
{
    Bool retval = FALSE;
    wxPen *savePen, *apen;
    wxColor saveBack;
    int scaled_width;
    int scaled_height;

    if (!DRAWABLE) // ensure that a drawable has been associated
      return FALSE;
    
    if (!src->Ok())
      return FALSE;

    if (src->GetDepth() > 1) {
      /* Neither rop nor dcolor matter. Use GCBlit. */
      return GCBlit(xdest, ydest, w, h, src, xsrc, ysrc);
    }

    /* This is mono to mono/color */

    FreeGetPixelCache();

    if (src->selectedTo)
      src->selectedTo->EndSetPixel();
    
    xsrc = floor(xsrc);
    ysrc = floor(ysrc);

    savePen = current_pen;
    saveBack = current_background_color;
    /* Pen GC used for blit: */
    apen = wxThePenList->FindOrCreatePen(dcolor ? dcolor : wxBLACK, 0, rop);
    SetPen(apen);

    scaled_width = src->GetWidth()  < XLOG2DEVREL(w) ? src->GetWidth()  : XLOG2DEVREL(w);
    scaled_height = src->GetHeight() < YLOG2DEVREL(h) ? src->GetHeight() : YLOG2DEVREL(h);

    if (DRAWABLE && src->Ok()) {
      // Check if we're copying from a mono bitmap
      retval = TRUE;
      if ((rop == wxSOLID) || (rop == wxXOR)) {
	/* Seems like the easiest way to implement transparent backgrounds is to
	   use a stipple. */
	XGCValues values;
	unsigned long mask = GCFillStyle | GCStipple | GCTileStipXOrigin | GCTileStipYOrigin;
	values.stipple = GETPIXMAP(src);
	values.fill_style = FillStippled;
	values.ts_x_origin = ((XLOG2DEV(xdest) - (long)xsrc) % src->GetWidth());
	values.ts_y_origin = ((YLOG2DEV(ydest) - (long)ysrc) % src->GetHeight());
	XChangeGC(DPY, PEN_GC, mask, &values);
	XFillRectangle(DPY, DRAWABLE, PEN_GC, XLOG2DEV(xdest), YLOG2DEV(ydest), scaled_width, scaled_height);
      } else {
	Pixmap pm;
	pm = GETPIXMAP(src);
	XCopyPlane(DPY, pm, DRAWABLE, PEN_GC,
		   (long)xsrc, (long)ysrc,
		   scaled_width, scaled_height,
		   XLOG2DEV(xdest), YLOG2DEV(ydest), 1);
      }
      CalcBoundingBox(xdest, ydest);
      CalcBoundingBox(xdest + w, ydest + h);
    }

    SetPen(savePen);
    SetBackground(&saveBack);

    return retval; // someting wrong with the drawables
}

Bool wxWindowDC::GCBlit(float xdest, float ydest, float w, float h, wxBitmap *src,
			float xsrc, float ysrc)
{
    /* A non-allocating (of collectable memory) blit */
  Bool retval = FALSE;
  int scaled_width;
  int scaled_height;
  GC agc;

    if (!DRAWABLE) // ensure that a drawable has been associated
      return FALSE;
    
    if (!src->Ok())
      return FALSE;

    FreeGetPixelCache();

    if (src->selectedTo)
      src->selectedTo->EndSetPixel();
    
    xsrc = floor(xsrc);
    ysrc = floor(ysrc);

    scaled_width = src->GetWidth()  < XLOG2DEVREL(w) ? src->GetWidth()  : XLOG2DEVREL(w);
    scaled_height = src->GetHeight() < YLOG2DEVREL(h) ? src->GetHeight() : YLOG2DEVREL(h);

    if (DRAWABLE && src->Ok()) {
      XGCValues values;
      int mask = 0;
      Region free_rgn = (Region)NULL;

      if ((DEPTH == 1) && (src->GetDepth() > 1)) {
	/* May need to flip 1 & 0... */
	if (BlackPixelOfScreen(SCN) == 1) {
	  mask = GCFunction;
	  values.function = GXcopyInverted;
	}
      }

      agc = XCreateGC(DPY, DRAWABLE, mask, &values);
      if (USER_REG || EXPOSE_REG) {
	Region rgn;
	if (USER_REG && EXPOSE_REG) {
	  rgn = XCreateRegion();
	  free_rgn = rgn;
	  XIntersectRegion(EXPOSE_REG, USER_REG, rgn);
	} else if (USER_REG)
	  rgn = USER_REG;
	else
	  rgn = EXPOSE_REG;

	XSetRegion(DPY, agc, rgn);
      }

      retval = TRUE;
      if ((src->GetDepth() == 1) || (DEPTH == 1)) {
	/* mono to color/mono  or  color/mono to mono */
	Pixmap pm;
	pm = GETPIXMAP(src);
	XCopyPlane(DPY, pm, DRAWABLE, agc,
		   (long)xsrc, (long)ysrc,
		   scaled_width, scaled_height,
		   XLOG2DEV(xdest), YLOG2DEV(ydest), 1);
      } else if (src->GetDepth() == (int)DEPTH) {
	/* color to color */
	Pixmap pm;
	pm = GETPIXMAP(src);
	XCopyArea(DPY, pm, DRAWABLE, agc,
		  (long)xsrc, (long)ysrc,
		  scaled_width, scaled_height,
		  XLOG2DEV(xdest), YLOG2DEV(ydest));
      } else
	retval = FALSE;

      XFreeGC(DPY, agc);

      if (free_rgn)
	XDestroyRegion(free_rgn);
    }

    return retval; // someting wrong with the drawables
}

void wxWindowDC::Clear(void)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    // clear means to clear the entire canvas without expose region clipping
    // EXPOSE_REG = NULL;
    // SetCanvasClipping();

    // clear canvas
    {
      unsigned int w, h;
      Window wdummy; int sdummy; unsigned int udummy;
      XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
		   &w, &h, &udummy, &udummy);
      
      XFillRectangle(DPY, DRAWABLE, BG_GC, 0, 0, w, h);
    }
}

void wxWindowDC::CrossHair(float x, float y)
{
  int xx, yy;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
	return;
    xx = XLOG2DEV(x);
    yy = YLOG2DEV(y);

    float w, h;
    GetSize(&w, &h);

    XDrawLine(DPY, DRAWABLE, PEN_GC, 0, yy, (int)w, yy);
    XDrawLine(DPY, DRAWABLE, PEN_GC, xx, 0, xx, (int)h);
}

void wxWindowDC::DrawArc(float x, float y, float w, float h, float start, float end)
{
  int xx, yy, ww, hh;
  float xw, yh;
  double degrees1, degrees2;
  int alpha1, alpha2;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  /* MATTHEW: [5] Implement GetPixel */
  FreeGetPixelCache();
  
  xw = x + w, yh = y + h;
  
  xx = XLOG2DEV(x); yy = XLOG2DEV(y);
  ww = XLOG2DEV(xw) - xx; hh = XLOG2DEVREL(yh) - yy;
    
  degrees1 = start * RAD2DEG;
  degrees2 = end * RAD2DEG;
  alpha1 = int(degrees1 * 64.0);
  alpha2 = int((degrees2 - degrees1) * 64.0);
  while (alpha2 <= 0) {
    alpha2 += 360*64;
  }
  while (alpha1 > 360*64) {
    alpha1 -= 360*64;
  }
  
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
    XFillArc(DPY,DRAWABLE,BRUSH_GC,xx,yy,ww,hh,alpha1,alpha2);
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawArc(DPY,DRAWABLE,PEN_GC,xx,yy,ww,hh,alpha1,alpha2);
  CalcBoundingBox(x, y);
  CalcBoundingBox(x + w, y + h);
}

void wxWindowDC::DrawEllipse(float x, float y, float w, float h)
{
  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  /* MATTHEW: [5] Implement GetPixel */
  FreeGetPixelCache();
  
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
    XFillArc(DPY, DRAWABLE, BRUSH_GC, XLOG2DEV(x), YLOG2DEV(y),
	     XLOG2DEVREL(w) - WX_GC_CF, YLOG2DEVREL(h) - WX_GC_CF, 0, 64*360);
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawArc(DPY, DRAWABLE, PEN_GC, XLOG2DEV(x), YLOG2DEV(y),
	     XLOG2DEVREL(w) - WX_GC_CF, YLOG2DEVREL(h) - WX_GC_CF, 0, 64*360);
  CalcBoundingBox(x, y);
  CalcBoundingBox(x+w, y+h);
}

void wxWindowDC::DrawLine(float x1, float y1, float x2, float y2)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLine(DPY, DRAWABLE, PEN_GC,
		  XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
    CalcBoundingBox(x1, y1);
    CalcBoundingBox(x2, y2);
}

void wxWindowDC::DrawLines(int n, wxPoint pts[], float xoff, float yoff)
{
  XPoint *xpts;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

    /* MATTHEW: [5] Implement GetPixel */
  FreeGetPixelCache();
    
  xpts = new XPoint[n];
  for (int i=0; i<n; ++i) {
    short x, y;
    x = XLOG2DEV(pts[i].x + xoff);
    xpts[i].x = x;
    y = YLOG2DEV(pts[i].y + yoff);
    xpts[i].y = y;
    CalcBoundingBox(xpts[i].x, xpts[i].y);
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
}

void wxWindowDC::DrawLines(int n, wxIntPoint pts[], int xoff, int yoff)
{
  XPoint *xpts;
  
  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  /* MATTHEW: [5] Implement GetPixel */
  FreeGetPixelCache();
  
  xpts = new XPoint[n];
  for (int i=0; i<n; ++i) {
    short x, y;
    x = XLOG2DEV(pts[i].x + xoff);
    xpts[i].x = x;
    y = YLOG2DEV(pts[i].y + yoff);
    xpts[i].y = y;
    CalcBoundingBox(xpts[i].x, xpts[i].y);
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
}

void wxWindowDC::DrawLines(wxList *pts, float xoff, float yoff)
{
  int n;
  XPoint *xpts;
  int i;
  wxNode *node;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;
  
  /* MATTHEW: [5] Implement GetPixel */
  FreeGetPixelCache();
  
  n = pts->Number();
  xpts = new XPoint[n];
  i = 0;
  for (node = pts->First(); node; node = node->Next()) {
    wxPoint *point;
    short x, y;
    point = (wxPoint*)node->Data();
    x = XLOG2DEV(point->x + xoff);
    xpts[i].x = x;
    y = YLOG2DEV(point->y + yoff);
    xpts[i].y = y;
    CalcBoundingBox(xpts[i].x, xpts[i].y);
    ++i;
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
}

void wxWindowDC::DrawPoint(float x, float y)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawPoint(DPY, DRAWABLE, PEN_GC, XLOG2DEV(x), YLOG2DEV(y));
    CalcBoundingBox(x, y);
}

void wxWindowDC::DrawPolygon(int n, wxPoint pts[], float xoff, float yoff,
			     int fill)
{
  XPoint *xpts;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    xpts = new XPoint[n+1];
    for (int i=0; i<n; ++i) {
      short x, y;
      x = XLOG2DEV(pts[i].x + xoff);
      xpts[i].x = x;
      y = YLOG2DEV(pts[i].y + yoff);
      xpts[i].y = y;
      CalcBoundingBox(xpts[i].x, xpts[i].y);
    }
    xpts[n].x = xpts[0].x; // close figure
    xpts[n].y = xpts[0].y;
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
	XSetFillRule(DPY, BRUSH_GC, fill_rule[fill]);
	XFillPolygon(DPY, DRAWABLE, BRUSH_GC, xpts, n, Complex, 0);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n+1, 0);
}

void wxWindowDC::DrawPolygon(wxList *pts, float xoff, float yoff, int fill)
{
  int n;
  XPoint *xpts;
  int i;
  wxNode *node;

  if (!DRAWABLE) // ensure that a drawable has been associated
    return;

  FreeGetPixelCache();
    
  n = pts->Number();
  xpts = new XPoint[n+1];
  i = 0;
  for (node = pts->First(); node; node = node->Next()) {
    wxPoint *point;
    short x, y;
    point = (wxPoint*)node->Data();
    x = XLOG2DEV(point->x + xoff);
    xpts[i].x = x;
    y = YLOG2DEV(point->y + yoff);
    xpts[i].y = y;
    CalcBoundingBox(xpts[i].x, xpts[i].y);
    ++i;
  }
  xpts[n].x = xpts[0].x; // close figure
  xpts[n].y = xpts[0].y;
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    XSetFillRule(DPY, PEN_GC, fill_rule[fill]);
    XFillPolygon(DPY, DRAWABLE, BRUSH_GC, xpts, n, Complex, 0);
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
    XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n+1, 0);
}

void wxWindowDC::DrawRectangle(float x, float y, float w, float h)
{
    int x1, y1, w1, h1;
    float xw, yh;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    xw = x + w, yh = y + h;
   
    x1 = XLOG2DEV(x);
    y1 = YLOG2DEV(y);
    w1 = XLOG2DEV(xw) - x1;
    h1 = YLOG2DEV(yh) - y1;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
      XFillRectangle(DPY, DRAWABLE, BRUSH_GC, x1, y1, w1, h1);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
      XDrawRectangle(DPY, DRAWABLE, PEN_GC, x1, y1, w1 - WX_GC_CF, h1 - WX_GC_CF);
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+w, y+h);
}

void wxWindowDC::DrawRoundedRectangle(float x, float y, float w, float h,
				      float radius)
{
  int xx, yy, ww, hh, rr, dd;
  float xw, yh;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (radius < 0.0)
      radius = - radius * ((w < h) ? w : h);

    xw = x + w, yh = y + h;
    
    xx = XLOG2DEV(x);       yy = YLOG2DEV(y);
    ww = XLOG2DEV(xw) - xx; hh = YLOG2DEV(yh) - yy;
    rr = XLOG2DEVREL(radius);
    dd = 2 * rr;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
	XFillRectangle(DPY, DRAWABLE, BRUSH_GC, xx+rr, yy, ww-dd, hh);
	XFillRectangle(DPY, DRAWABLE, BRUSH_GC, xx, yy+rr, ww, hh-dd);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx, yy, dd, dd, 90*64, 90*64);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx+ww-dd, yy, dd, dd, 0, 90*64);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx+ww-dd, yy+hh-dd, dd, dd,
		 270*64, 90*64);
	XFillArc(DPY, DRAWABLE, BRUSH_GC, xx, yy+hh-dd, dd, dd, 180*64, 90*64);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT){
        ww -= WX_GC_CF;
        hh -= WX_GC_CF;
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx+rr, yy, xx+ww-rr, yy);
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx+rr, yy+hh, xx+ww-rr, yy+hh);
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx, yy+rr, xx, yy+hh-rr);
	XDrawLine(DPY, DRAWABLE, PEN_GC, xx+ww, yy+rr, xx+ww, yy+hh-rr);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx, yy, dd, dd, 90*64, 90*64);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx+ww-dd, yy, dd, dd, 0, 90*64);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx+ww-dd, yy+hh-dd, dd, dd,
		 270*64, 90*64);
	XDrawArc(DPY, DRAWABLE, PEN_GC, xx, yy+hh-dd, dd, dd, 180*64, 90*64);
    }
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+w, y+h);
}

void wxWindowDC::FloodFill(float WXUNUSED(x), float WXUNUSED(y),
			   wxColour *WXUNUSED(col),int WXUNUSED(style))
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    // don't know how to do it for X11
}

void wxWindowDC::IntDrawLine(int x1, int y1, int x2, int y2)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLine(DPY, DRAWABLE, PEN_GC,
		  XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
}

void wxWindowDC::IntDrawLines(int n, wxIntPoint pts[], int xoff, int yoff)
{
  XPoint *xpts;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    xpts = new XPoint[n];
    for (int i=0; i<n; ++i) {
      short x, y;
      x = XLOG2DEV(pts[i].x + xoff);
      xpts[i].x = x;
      y = YLOG2DEV(pts[i].y + yoff);
      xpts[i].y = y;
      CalcBoundingBox(xpts[i].x, xpts[i].y);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
}

//-----------------------------------------------------------------------------
// drawing tools
//-----------------------------------------------------------------------------

void wxWindowDC::SetBackground(wxColour *c)
{
  int style;
  unsigned long pixel;

  if (!DRAWABLE) /* MATTHEW: [5] */
    return;
  
  current_background_color = *c;
  
  pixel = current_background_color.GetPixel(current_cmap, IS_COLOR, 0);
  
  if (DRAW_WINDOW)
    XSetWindowBackground(DPY, DRAW_WINDOW, pixel);
  XSetForeground(DPY, BG_GC, pixel);
  XSetBackground(DPY, PEN_GC, pixel);
  XSetBackground(DPY, BRUSH_GC, pixel);
  
  // use the correct pixel values for XOR
  
  style = current_pen->GetStyle();
  if ((style >= wxXOR_DOT) && (style <= wxXOR_DOT_DASH))
    style = wxXOR;
  if (current_pen && (style == wxXOR))
    SetPen(current_pen);
  
  if (current_brush && (current_brush->GetStyle() == wxXOR))
    SetBrush(current_brush);
}

void wxWindowDC::SetBrush(wxBrush *brush)
{
  XGCValues     values;
  unsigned long mask;
  wxBitmap *bm;
  unsigned long pixel;

    if (!DRAWABLE) /* MATTHEW: [5] */
      return;

    if (current_brush) current_brush->Lock(-1);

    if (!(current_brush = brush)) // nothing to do without brush
      return;

    if (current_brush) current_brush->Lock(1);

    // for XChangeGC
    mask = GCFillStyle | GCForeground | GCFunction;

    values.fill_style = FillSolid;
    // wxXOR shall work the correct way
    pixel = brush->GetColour()->GetPixel(current_cmap, IS_COLOR, 1);
    if (brush->GetStyle() == wxXOR) {
	XGCValues values_req;
	XGetGCValues(DPY, BRUSH_GC, GCBackground, &values_req);
	values.foreground = pixel ^ values_req.background;
	values.function = GXxor;
    } else {
	values.foreground = pixel;
	values.function = GXcopy;
    }

    bm = brush->GetStipple();
    if (bm && !bm->Ok())
      bm = NULL;

    if (bm) {
      Pixmap stipple = (Pixmap)0; // for FillStippled
      Pixmap tile    = (Pixmap)0; // for FillTiled
      if (bm->GetDepth() == 1) {
	if (bm->selectedTo) bm->selectedTo->EndSetPixel();
	stipple = GETPIXMAP(bm);
	values.fill_style = ((brush->GetStyle()==wxSTIPPLE) ? FillOpaqueStippled : FillStippled);
      } else if (bm->GetDepth() == (signed)DEPTH) {
	if (bm->selectedTo) bm->selectedTo->EndSetPixel();
	tile = GETPIXMAP(bm);
	values.fill_style = FillTiled;
      } // else wrong depth
      if (stipple) {
	values.stipple = stipple;
	mask |= GCStipple;
      }
      if (tile) {
	values.tile = tile;
	mask |= GCTile;
	values.foreground = BlackPixel(DPY, DefaultScreen(DPY));
	values.function = GXcopy;
      }
    } else {
      int style;
      style = brush->GetStyle();
      if (wxIS_HATCH(style)) {
	Pixmap stipple = (Pixmap)0; // for FillStippled
	stipple = hatch_bitmaps[style-wxFIRST_HATCH];
	values.fill_style = FillStippled;
	if (stipple) {
	  values.stipple = stipple;
	  mask |= GCStipple;
	}
      }
    }
    XChangeGC(DPY, BRUSH_GC, mask, &values);
}

void wxWindowDC::SetColourMap(wxColourMap *new_cmap)
{
  current_cmap = new_cmap ? new_cmap : wxAPP_COLOURMAP;
  
  if (DRAW_WINDOW) {
    Window w;
    Colormap cm;
    w = DRAW_WINDOW;
    cm = CMAP;
    XSetWindowColormap(DPY, w, cm);
  }
}

void wxWindowDC::SetPen(wxPen *pen)
{
  XGCValues     values;
  unsigned long mask;
  wxBitmap *bm;
  int scale;
  int style, xor, js, cs, pw;
  unsigned long pixel;

    if (!DRAWABLE) /* MATTHEW: [5] */
      return;

    if (current_pen) current_pen->Lock(-1);
    
    if (!(current_pen = pen)) // nothing to do without pen
      return;

    if (current_pen) current_pen->Lock(1);

    // for XChangeGC
    mask = (GCCapStyle  | GCFillStyle | GCForeground
	    | GCJoinStyle | GCLineStyle | GCLineWidth
	    | GCFunction);

    cs = pen->GetCap();
    values.cap_style  = cap_style[cs];
    values.fill_style = FillSolid;
    js = pen->GetJoin();
    values.join_style = join_style[js];
    values.line_style = LineSolid;
    pw = pen->GetWidth();
    scale = XLOG2DEVREL(pw);
    values.line_width = scale;
    // wxXOR shall work the correct way
    pixel = pen->GetColour()->GetPixel(current_cmap, IS_COLOR, 1);
    style = pen->GetStyle();
    xor = 0;

    switch (style) {
    case wxXOR:
      xor = 1;
      break;
    case wxXOR_DOT:
    case wxXOR_SHORT_DASH:
    case wxXOR_LONG_DASH:
    case wxXOR_DOT_DASH:
      xor = 1;
      style -= (wxXOR_DOT - wxDOT);
      break;
    }

    if (xor) {
	XGCValues values_req;
	XGetGCValues(DPY, PEN_GC, GCBackground, &values_req);
	values.foreground = pixel ^ values_req.background;
	values.function = GXxor;
    } else {
	values.foreground = pixel;
	values.function = GXcopy;
    }

    bm = pen->GetStipple();
    if (bm && !bm->Ok())
      bm = NULL;

    if (bm) {
      Pixmap stipple = (Pixmap)0; // for FillStippled
      Pixmap tile    = (Pixmap)0; // for FillTiled
      if (bm->GetDepth() == 1) {
	if (bm->selectedTo) bm->selectedTo->EndSetPixel();
	stipple = GETPIXMAP(bm);
	values.fill_style = FillStippled;
      } else if (bm->GetDepth() == (signed)DEPTH) {
	if (bm->selectedTo) bm->selectedTo->EndSetPixel();
	tile = GETPIXMAP(bm);
	values.fill_style = FillTiled;
      } // else wrong depth
      if (stipple) {
	values.stipple = stipple;
	mask |= GCStipple;
      }
      if (tile) {
	values.tile = tile;
	mask |= GCTile;
	values.foreground = BlackPixel(DPY, DefaultScreen(DPY));
	values.function = GXcopy;
      }
    } else {
      if (wxIS_DASH(style) || style==wxUSER_DASH) {
	static wxDash dashdefs[4][4] = {
	  { 2, 5, 0, 0 }, // wxDOT
	  { 4, 8, 0, 0 }, // wxLONG_DASH
	  { 4, 4, 0, 0 }, // wxSHORT_DASH
	  { 6, 6, 2, 6 }  // wxDOT_DASH
	};
	static int    num_dashes[] = { 2, 2, 2, 4 };
	int           num_dash;
	wxDash        *dashdef, *scaleddef;
	if (style==wxUSER_DASH) {
	  num_dash = pen->GetDashes(&dashdef);
	} else {
	  num_dash = num_dashes[style-wxFIRST_DASH];
	  dashdef  = dashdefs[style-wxFIRST_DASH];
	}
	if ((scaleddef = new wxDash[num_dash])) {
	  int dscale = scale, i;
	  if (!dscale) dscale = 1;
	  for (i = 0; i < num_dash; i++) {
	    scaleddef[i] = dscale * dashdef[i];
	  }
	  XSetDashes(DPY, PEN_GC, 0, (char*)scaleddef, num_dash);
	  delete[] scaleddef;
	} else { // not enough memory to scale
	  XSetDashes(DPY, PEN_GC, 0, (char*)dashdef, num_dash);
	}
	values.line_style = LineOnOffDash;
      }
    }
    XChangeGC(DPY, PEN_GC, mask, &values);
}

void wxWindowDC::TryColour(wxColour *src, wxColour *dest)
{
  XColor xcol;

  if (!DRAWABLE)
    return;

  xcol.pixel = src->GetPixel(current_cmap, IS_COLOR, 1);

  if (IS_COLOR) {
    Colormap cm;
    cm = GETCOLORMAP(current_cmap);
    XQueryColor(wxAPP_DISPLAY, cm, &xcol);
    
    dest->Set(xcol.red >> SHIFT, xcol.green >> SHIFT, xcol.blue >> SHIFT);
  } else if (xcol.pixel == BlackPixel(DPY, DefaultScreen(DPY))) {
    dest->Set(0, 0, 0);
  } else {
    dest->Set(255, 255, 255);
  }
}


void wxWindowDC::FillPrivateColor(wxColour *c)
{
  XColor xcol;
  int free = 0;
  XGCValues values;
  int mask = 0;
  GC agc;
  float w, h;
  Colormap cm;

  if (!DRAWABLE)
    return;

  xcol.red = c->Red() << SHIFT;
  xcol.green = c->Green() << SHIFT;
  xcol.blue = c->Blue() << SHIFT;

  cm = GETCOLORMAP(current_cmap);

  if (XAllocColor(wxAPP_DISPLAY, cm, &xcol) == 1) {
    XQueryColor(wxAPP_DISPLAY, cm, &xcol);
    c->Set(xcol.red >> SHIFT, xcol.green >> SHIFT, xcol.blue >> SHIFT);
    free = 1;
  } else {
    xcol.pixel = BlackPixel(DPY, DefaultScreen(DPY));
    c->Set(0, 0, 0);
  }

  values.foreground = xcol.pixel;
  values.fill_style = FillSolid;
  mask |= GCForeground | GCFillStyle;

  agc = XCreateGC(DPY, DRAWABLE, mask, &values);
  
  GetSize(&w, &h);

  XFillRectangle(DPY, DRAWABLE, agc, 0, 0, (int)w, (int)h);

  XFreeGC(DPY, agc);

  if (free)
    XFreeColors(wxAPP_DISPLAY, cm, &xcol.pixel, 1, 0);
}

//-----------------------------------------------------------------------------
// text and font methods
//-----------------------------------------------------------------------------

static int str16len(const char *s)
{
  int count = 0;

  while (s[0] && s[1]) {
    count++;
    s += 2;
  }

  return count;
}

void wxWindowDC::DrawText(char *text, float x, float y, Bool use16bit)
{
    XFontStruct *fontinfo;
    int         direction;
    int         ascent, descent, cx, cy;
    int         dev_x;
    int         dev_y;
    int         textlen;
    XCharStruct overall;

    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    if (!current_font) // a font must be associated for drawing
	return;

    fontinfo = (XFontStruct*)current_font->GetInternalFont(scale_x);
    dev_x = XLOG2DEV(x);
    dev_y = YLOG2DEV(y);
    textlen = use16bit ? str16len(text) : strlen(text);

    if (use16bit)
      (void)XTextExtents16(fontinfo, (XChar2b *)text, textlen, &direction, 
			   &ascent, &descent, &overall);
    else
      (void)XTextExtents(fontinfo, text, textlen, &direction, &ascent, &descent,
			 &overall);

    cx = overall.width;
    cy = ascent + descent;
    if (current_text_bgmode == wxSOLID) {
      if (use16bit)
	XDrawImageString16(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, (XChar2b *)text, textlen);
      else
	XDrawImageString(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, text, textlen);
    } else {
      if (use16bit)
	XDrawString16(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, (XChar2b *)text, textlen);
      else
	XDrawString(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, text, textlen);
    }
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+XDEV2LOG(cx), y+YDEV2LOG(cy));
}

float wxWindowDC::GetCharHeight(void)
{
    int         direction, ascent, descent;
    XCharStruct overall;
    XFontStruct *xfs;

    if (!DRAWABLE) /* MATTHEW: [5] */
      return 0;

    if (!current_font) // no font
	return YDEV2LOGREL(12);

    xfs = (XFontStruct*)current_font->GetInternalFont(scale_x);
    XTextExtents (xfs, "x", 1,
		  &direction, &ascent, &descent, &overall);
    return YDEV2LOGREL(ascent + descent);
}

float wxWindowDC::GetCharWidth(void)
{
    int         direction, ascent, descent;
    XCharStruct overall;
    XFontStruct *xfs;

    if (!DRAWABLE) /* MATTHEW: [5] */
	return 0;

    if (!current_font)
	return XDEV2LOGREL(16);

    xfs = (XFontStruct*)current_font->GetInternalFont(scale_x);
    XTextExtents (xfs, "x", 1,
		  &direction, &ascent, &descent, &overall);
    return XDEV2LOGREL(overall.width);
}

void wxWindowDC::GetTextExtent(const char *s, float *_w, float *_h, float *_descent,
			       float *_topspace, wxFont *_font,
			       Bool use16bit)
{
  wxFont *font_to_use;
  int         direction, ascent, descent;
  XCharStruct overall;
  XFontStruct *fontinfo;
  float w, h;

    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    font_to_use = _font ? _font : current_font;
    if (!font_to_use) {
	wxError("set a font before calling GetTextExtent", "wxWindowDC");
	*_w = *_h = -1.0;
	return;
    }

    fontinfo = (XFontStruct*)font_to_use->GetInternalFont(scale_x);

    if (use16bit)
      XTextExtents16(fontinfo, (XChar2b *)s, str16len(s),
		     &direction, &ascent, &descent, &overall);
    else
      XTextExtents(fontinfo, s, strlen(s),
		   &direction, &ascent, &descent, &overall);

    w = XDEV2LOGREL(overall.width);
    *_w = w;
    h = YDEV2LOGREL(ascent + descent);
    *_h = h;
    if (_descent) {
      float d;
      d = YDEV2LOGREL(descent);
      *_descent = d;
    }
    if (_topspace)
      *_topspace = 0.0;
}

void wxWindowDC::SetFont(wxFont *font)
{
    XFontStruct *xfs;

    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!(current_font = font)) // nothing to do without a font
	return;

    xfs  =(XFontStruct*)font->GetInternalFont(scale_x);
    XSetFont(DPY, TEXT_GC, xfs->fid);
}

void wxWindowDC::SetTextForeground(wxColour *col)
{
    unsigned long pixel;

    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!col)
	return;
    current_text_fg = *col;
    pixel = current_text_fg.GetPixel(current_cmap, IS_COLOR, 1);
    XSetForeground(DPY, TEXT_GC, pixel);
}

void wxWindowDC::SetTextBackground(wxColour *col)
{
    unsigned long pixel;

    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!col)
	return;
    current_text_bg = *col;
    pixel = current_text_bg.GetPixel(current_cmap, IS_COLOR, 0);
    XSetBackground(DPY, TEXT_GC, pixel);
}

//-----------------------------------------------------------------------------
// clipping region
//-----------------------------------------------------------------------------

void wxWindowDC::SetClippingRect(float x, float y, float w, float h)
{
  wxRegion *r;
  r = new wxRegion(this);
  r->SetRectangle(x, y, w, h);

  SetClippingRegion(r);
}

void wxWindowDC::SetClippingRegion(wxRegion *r)
{
  clipping = r;
  if (r)
    USER_REG = r->rgn;
  else
    USER_REG = NULL;
  SetCanvasClipping();
}

wxRegion *wxWindowDC:: GetClippingRegion()
{
  return clipping;
}

void wxWindowDC::GetSize(float *w, float *h)
{
  Window wdummy; int sdummy; unsigned int udummy, width, height;

  if (DRAWABLE) {
    XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
		 &width, &height, &udummy, &DEPTH);
    
    *w = width;
    *h = height;
  } else {
    *w = *h = 0;
  }
}

//-----------------------------------------------------------------------------
// methods unique to wxWindowDC
//-----------------------------------------------------------------------------

void wxWindowDC::Initialize(wxWindowDC_Xinit* init)
{
    Drawable GC_drawable; // necessary create a GC (needed for depth,...)
    wxFont *font;
    XGCValues values;
    unsigned long mask;
    wxColour c;
    int width, height;

    DPY = init->dpy; SCN = init->scn;

    if (init->drawable) {
	Window wdummy; int sdummy; unsigned int udummy;
	 // I have a specified drawable -> get width, height, and depth
	GC_drawable = DRAWABLE = init->drawable;
	XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
		     &WIDTH, &HEIGHT, &udummy, &DEPTH);
    } else {
	GC_drawable = wxAPP_ROOT; // defaults to root
	DEPTH = wxDisplayDepth(); // depth is display depth
    }
    Colour = (DEPTH != 1); // accept everything else than depth one as colour display

    X->owner = init->owner;

    WXGC_IGNORE(X->owner);

    values.foreground = BlackPixelOfScreen(SCN);
    values.background = WhitePixelOfScreen(SCN);
    values.graphics_exposures = FALSE;
    values.line_width = 1;
    mask = GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth;
    PEN_GC   = XCreateGC(DPY, GC_drawable, mask, &values);
    TEXT_GC  = XCreateGC(DPY, GC_drawable, mask, &values);
    values.foreground = WhitePixelOfScreen(SCN);
    values.background = BlackPixelOfScreen(SCN);
    BG_GC    = XCreateGC(DPY, GC_drawable, mask, &values);
    BRUSH_GC = XCreateGC(DPY, GC_drawable, mask, &values);

    // set drawing tools
    c = current_text_fg; /* Don't pass &current_text_fg to SetTextForeground */
    SetTextForeground(&c);
    c = current_text_bg;
    SetTextBackground(&c);
    SetBackground(&current_background_color); 
    SetBrush(current_brush);
    SetPen(current_pen);

    font = current_font;
    current_font = NULL;
    SetFont(font ? font : wxNORMAL_FONT);

    // set display scaling
    width  = WidthOfScreen(SCN);
    height = HeightOfScreen(SCN);
    mm_to_pix_x = ((float)width)  / ((float)WidthMMOfScreen(SCN));
    mm_to_pix_y = ((float)height) / ((float)HeightMMOfScreen(SCN));
}

void wxWindowDC::Destroy(void)
{
    if (PEN_GC)    XFreeGC(DPY, PEN_GC);
    if (BRUSH_GC)  XFreeGC(DPY, BRUSH_GC);
    if (TEXT_GC)   XFreeGC(DPY, TEXT_GC);
    if (BG_GC)     XFreeGC(DPY, BG_GC);
    PEN_GC = BRUSH_GC = TEXT_GC = BG_GC = NULL;

    if (CURRENT_REG) XDestroyRegion(CURRENT_REG);
    if (EXPOSE_REG) XDestroyRegion(EXPOSE_REG);
    CURRENT_REG = USER_REG = EXPOSE_REG = NULL;
}

void wxWindowDC::SetCanvasClipping(void)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (CURRENT_REG)
	XDestroyRegion(CURRENT_REG);
    if (USER_REG || EXPOSE_REG) {
	CURRENT_REG = XCreateRegion();
	XIntersectRegion(EXPOSE_REG ? EXPOSE_REG : USER_REG,
			 USER_REG ? USER_REG : EXPOSE_REG,
			 CURRENT_REG);
	XSetRegion(DPY, PEN_GC, CURRENT_REG);
	XSetRegion(DPY, BRUSH_GC, CURRENT_REG);
	XSetRegion(DPY, BG_GC, CURRENT_REG);
	XSetRegion(DPY, TEXT_GC, CURRENT_REG);
    } else {
	CURRENT_REG = NULL;
	XSetClipMask(DPY, PEN_GC, None);
	XSetClipMask(DPY, BRUSH_GC, None);
	XSetClipMask(DPY, BG_GC, None);
	XSetClipMask(DPY, TEXT_GC, None);
    }
}

/* MATTHEW */
Bool wxWindowDC::Ok(void)
{
  return !!DRAWABLE;
}


/* MATTHEW: [5] */
Bool wxWindowDC::GetPixel(float x, float y, wxColour * col)
{
  int i, j;
  int k;
  unsigned long pixel;
  XColor xcol;
  int get_pixel_cache_pos;
  XColor *get_pixel_color_cache;
  Bool get_pixel_cache_full;
  unsigned int w, h;

  if (!DRAWABLE)
    return FALSE;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  
  w = X->width;
  h = X->height;

  if (i < 0 || (unsigned int)i >= w
      || j < 0 || (unsigned int)j >= h)
    return FALSE;

#define NUM_GETPIX_CACHE_COLORS 256

  if (!X->get_pixel_image_cache) {
    BeginSetPixel();

    if (X->get_pixel_image_cache->depth == 1) {
      get_pixel_color_cache = X->get_pixel_color_cache;
      
      get_pixel_color_cache[0].pixel = 1;
      get_pixel_color_cache[0].red = 0;
      get_pixel_color_cache[0].green = 0;
      get_pixel_color_cache[0].blue = 0;

      get_pixel_color_cache[1].pixel = 0;
      get_pixel_color_cache[1].red = 255;
      get_pixel_color_cache[1].green = 255;
      get_pixel_color_cache[1].blue = 255;
      
      X->get_pixel_cache_pos = 2;
    }
  }

  get_pixel_cache_pos = X->get_pixel_cache_pos;
  get_pixel_color_cache = X->get_pixel_color_cache;
  get_pixel_cache_full = X->get_pixel_cache_full;

  pixel = XGetPixel(X->get_pixel_image_cache, i, j);

  for (k = get_pixel_cache_pos; k--; ) {
    if (get_pixel_color_cache[k].pixel == pixel) {
      col->Set(get_pixel_color_cache[k].red,
	       get_pixel_color_cache[k].green,
	       get_pixel_color_cache[k].blue);
      return TRUE;
    }
  }

  if (get_pixel_cache_full) {
    for (k = NUM_GETPIX_CACHE_COLORS; k-- > get_pixel_cache_pos; ) {
      if (get_pixel_color_cache[k].pixel == pixel) {
	col->Set(get_pixel_color_cache[k].red,
		 get_pixel_color_cache[k].green,
		 get_pixel_color_cache[k].blue);
	return TRUE;
      }
    }
  }
  
  xcol.pixel = pixel;
  {
    Colormap cm;
    cm = GETCOLORMAP(current_cmap);
    XQueryColor(wxAPP_DISPLAY, cm, &xcol);
  }

  get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
  get_pixel_color_cache[get_pixel_cache_pos].red = xcol.red >> SHIFT;
  get_pixel_color_cache[get_pixel_cache_pos].green = xcol.green >> SHIFT;
  get_pixel_color_cache[get_pixel_cache_pos].blue = xcol.blue >> SHIFT;

  col->Set(get_pixel_color_cache[get_pixel_cache_pos].red,
	   get_pixel_color_cache[get_pixel_cache_pos].green,
	   get_pixel_color_cache[get_pixel_cache_pos].blue);

  if (++get_pixel_cache_pos >= NUM_GETPIX_CACHE_COLORS) {
    get_pixel_cache_pos = 0;
    X->get_pixel_cache_full = TRUE;
  }

  X->get_pixel_cache_pos = get_pixel_cache_pos;

  return TRUE;
}

void wxWindowDC::BeginSetPixel()
{
  unsigned int w, h;

  if (!DRAWABLE)
    return;

  if (X->get_pixel_image_cache)
    return;

  w = X->width;
  h = X->height;

  if (X->is_window) {
    /* For now, disallow: */
    return;
  }

  {
    XImage *img;
    img = XGetImage(DPY, DRAWABLE, 0, 0, w, h, AllPlanes, ZPixmap);
    X->get_pixel_image_cache = img;
  }
  
  X->get_pixel_cache_pos = 0;
  X->get_pixel_cache_full = FALSE;
  {
    XColor *cols;
    cols = new XColor[NUM_GETPIX_CACHE_COLORS];
    X->get_pixel_color_cache = cols;
  }
  X->set_a_pixel = FALSE;
}

void wxWindowDC::EndSetPixel()
{
  if (!X->get_pixel_image_cache)
    return;

  if (X->set_a_pixel) {
    int w, h;
    w = X->get_pixel_image_cache->width;
    h = X->get_pixel_image_cache->height;
    
    XPutImage(DPY, DRAWABLE, PEN_GC, X->get_pixel_image_cache, 0, 0, 0, 0, w, h);
  }

  if (X->get_pixel_image_cache) {
    XDestroyImage(X->get_pixel_image_cache);
    X->get_pixel_image_cache = NULL;
    X->get_pixel_color_cache = NULL;
  }
}

void wxWindowDC::SetPixel(float x, float y, wxColour * col)
{
  int i, j;
  int w, h, k;
  int red, green, blue;
  XColor xcol;
  unsigned long pixel;
  XImage *get_pixel_image_cache;
  int get_pixel_cache_pos;
  XColor *get_pixel_color_cache;
  Bool get_pixel_cache_full;

  BeginSetPixel();

  if (!X->get_pixel_image_cache)
    return;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  w = X->get_pixel_image_cache->width;
  h = X->get_pixel_image_cache->height;

  if (i < 0 || i >= w
      || j < 0 || j >= h)
    return;

  red = col->Red();
  green = col->Green();
  blue = col->Blue();

  get_pixel_image_cache = X->get_pixel_image_cache;
  get_pixel_cache_pos = X->get_pixel_cache_pos;
  get_pixel_color_cache = X->get_pixel_color_cache;
  get_pixel_cache_full = X->get_pixel_cache_full;

  X->set_a_pixel = TRUE;

  if (X->get_pixel_image_cache->depth == 1) {
    if ((red == 255) && (green == 255) && (blue == 255))
      pixel = 0;
    else
      pixel = 1;
  } else {
    for (k = get_pixel_cache_pos; k--; ) {
      if ((get_pixel_color_cache[k].red == red)
	  && (get_pixel_color_cache[k].green == green)
	  && (get_pixel_color_cache[k].blue == blue)) {
	pixel = get_pixel_color_cache[k].pixel;
	goto put;
      }
    }
    
    if (get_pixel_cache_full) {
      for (k = NUM_GETPIX_CACHE_COLORS; k-- > get_pixel_cache_pos; ) {
	if ((get_pixel_color_cache[k].red == red)
	    && (get_pixel_color_cache[k].green == green)
	    && (get_pixel_color_cache[k].blue == blue)) {
	  pixel = get_pixel_color_cache[k].pixel;
	  goto put;
	}
      }
    }

    xcol.red = red << 8;
    xcol.green = green << 8;
    xcol.blue = blue << 8;

    {
      Colormap cm;
      cm = GETCOLORMAP(current_cmap);
      wxAllocColor(DPY, cm, &xcol);
    }
    
    pixel = xcol.pixel;
    
    get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
    get_pixel_color_cache[get_pixel_cache_pos].red = red;
    get_pixel_color_cache[get_pixel_cache_pos].green = green;
    get_pixel_color_cache[get_pixel_cache_pos].blue = blue;
    
    if (++(X->get_pixel_cache_pos) >= NUM_GETPIX_CACHE_COLORS) {
      X->get_pixel_cache_pos = 0;
      X->get_pixel_cache_full = TRUE;
    }
  }

 put:
  pixel = XPutPixel(get_pixel_image_cache, i, j, pixel);
}

void wxWindowDC::DoFreeGetPixelCache(void)
{
  EndSetPixel();
}

