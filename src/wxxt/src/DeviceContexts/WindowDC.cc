/*								-*- C++ -*-
 * $Id: WindowDC.cc,v 1.3 1998/08/01 12:42:15 mflatt Exp $
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
#define  Uses_wxList
#include "wx.h"

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
static int function[]   = { GXand, GXandInverted, GXandReverse, GXclear, GXcopy,
			    GXequiv, GXinvert, GXnand, GXnor, GXnoop, GXor,
			    GXorInverted, GXorReverse, GXset, GXcopyInverted,
			    GXxor, GXcopy };

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

IMPLEMENT_DYNAMIC_CLASS(wxWindowDC, wxDC)

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

    current_background_brush = wxWHITE_BRUSH;
    current_background_brush->Lock(1);
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
    if (current_background_brush) current_background_brush->Lock(-1);

    Destroy();

    delete X; // free space for X data
}

//-----------------------------------------------------------------------------
// drawing methods
//-----------------------------------------------------------------------------

Bool wxWindowDC::Blit(float xdest, float ydest, float w, float h, wxDC *_src,
		      float xsrc, float ysrc, int rop)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return FALSE;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    wxWindowDC *src;
    
    switch (_src->device) {
    case wxDEVICE_WINDOW: // I can only handle CanvasDC and its children
    case wxDEVICE_MEMORY:
	src = (wxWindowDC*)_src;
	break;
    default:
	return FALSE;
    }

    Bool retval = FALSE;
    Bool resetPen;
    wxPen *savePen;
    wxBrush *saveBack;

    saveBack = current_background_brush;
    if (current_background_brush != wxWHITE_BRUSH)
      SetBackground(wxWHITE_BRUSH);

    if (rop == wxCOLOR) {
      rop = wxCOPY;
      if (auto_setting && current_pen)
	SetPen(current_pen);
      savePen = NULL;
      resetPen = FALSE;
    } else {
      savePen = current_pen;
      if (current_pen != wxBLACK_PEN) {
	SetPen(wxBLACK_PEN);
	resetPen = TRUE;
      } else
	resetPen = FALSE;
    }

    // until I know how to scale bitmaps
    int scaled_width
	= (signed)src->X->width  < XLOG2DEVREL(w) ? src->X->width  : XLOG2DEVREL(w);
    int scaled_height
	= (signed)src->X->height < YLOG2DEVREL(h) ? src->X->height : YLOG2DEVREL(h);

    if (DRAWABLE && src->X->drawable) {
	// Check if we're copying from a mono bitmap
        retval = TRUE;
	if (src->X->depth == 1) {
	    int old_logical_fkt = current_logical_fkt;
	    if (current_logical_fkt != rop)
		SetLogicalFunction(rop);
	    XCopyPlane(DPY, src->X->drawable, DRAWABLE, PEN_GC,
		       src->XLOG2DEV(xsrc), src->YLOG2DEV(ysrc),
		       scaled_width, scaled_height,
		       XLOG2DEV(xdest), YLOG2DEV(ydest), 1);
	    if (current_logical_fkt != old_logical_fkt)
		SetLogicalFunction(old_logical_fkt);
	} else if (src->X->depth == DEPTH) {
	    int old_logical_fkt = current_logical_fkt;
	    if (current_logical_fkt != rop)
		SetLogicalFunction(rop);
	    XCopyArea(DPY, src->X->drawable, DRAWABLE, PEN_GC,
		      src->XLOG2DEV(xsrc), src->YLOG2DEV(ysrc),
		      scaled_width, scaled_height,
		      XLOG2DEV(xdest), YLOG2DEV(ydest));
	    if (current_logical_fkt != old_logical_fkt)
		SetLogicalFunction(old_logical_fkt);
	} else
	  retval = FALSE;
	CalcBoundingBox(xdest, ydest);
	CalcBoundingBox(xdest + w, ydest + h);
    }

    if (current_background_brush != saveBack)
      SetBackground(saveBack);

    if (resetPen)
      SetPen(savePen);

    return retval; // someting wrong with the drawables
}

void wxWindowDC::Clear(void)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_background_brush)
	SetBackground(current_background_brush);

    // clear means to clear the entire canvas without expose region clipping
    // EXPOSE_REG = NULL;
    // SetCanvasClipping();

    // clear canvas
    if (DRAW_WINDOW)
	XClearWindow(DPY, DRAW_WINDOW);
    else {
      unsigned int w, h;
      Window wdummy; int sdummy; unsigned int udummy;
      XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
		   &w, &h, &udummy, &udummy);
      
      XFillRectangle(DPY, DRAWABLE, BG_GC, 0, 0, w, h);
    }
}

void wxWindowDC::CrossHair(float x, float y)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
	return;
    int xx = XLOG2DEV(x);
    int yy = YLOG2DEV(y);

    float w, h;
    GetSize(&w, &h);

    XDrawLine(DPY, DRAWABLE, PEN_GC, 0, yy, (int)w, yy);
    XDrawLine(DPY, DRAWABLE, PEN_GC, xx, 0, xx, (int)h);
}

void wxWindowDC::DrawArc(float x1, float y1, float x2, float y2,
			 float xc, float yc)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting) {
	if (current_brush)  SetBrush(current_brush);
	if (current_pen)    SetPen(current_pen);
    }

    int xx1 = XLOG2DEV(x1); int yy1 = XLOG2DEV(y1);
    int xx2 = XLOG2DEV(x2); int yy2 = XLOG2DEV(y2);
    int xxc = XLOG2DEV(xc); int yyc = XLOG2DEV(yc);
    double dx1 = xx1 - xxc; double dy1 = yy1 - yyc;
    // double dx2 = xx1 - xxc; double dy2 = yy1 - yyc;
    double radius1 = sqrt(dx1*dx1+dy1*dy1);
    // double radius2 = sqrt(dx2*dx2+dy2*dy2);
    int    r      = (int)radius1;
    double degrees1, degrees2;

    if (xx1 == xx2 && yy1 == yy2) {
	degrees1 = 0.0;
	degrees2 = 360.0;
    } else if (radius1 == 0.0) {
	degrees1 = degrees2 = 0.0;
    } else {
	degrees1 = (xx1 - xxc == 0) ?
	    (yy1 - yyc < 0) ? 90.0 : -90.0 :
	    -atan2(double(yy1-yyc), double(xx1-xxc)) * RAD2DEG;
	degrees2 = (xx2 - xxc == 0) ?
	    (yy2 - yyc < 0) ? 90.0 : -90.0 :
	    -atan2(double(yy2-yyc), double(xx2-xxc)) * RAD2DEG;
    }
    int alpha1 = int(degrees1 * 64.0);
    int alpha2 = int((degrees2 - degrees1) * 64.0);
    while (alpha2 <= 0)
	alpha2 += 360*64;
    while (alpha1 > 360*64)
	alpha1 -= 360*64;

    int width = r;
    int height = r;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	XFillArc(DPY,DRAWABLE,BRUSH_GC,xxc-width,yyc-height,2*width,2*height,alpha1,alpha2);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawArc(DPY,DRAWABLE,PEN_GC,xxc-width,yyc-height,2*width,2*height,alpha1,alpha2);
    CalcBoundingBox(x1, y1);
    CalcBoundingBox(x2, y2);
}

void wxWindowDC::DrawEllipse(float x, float y, float w, float h)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting) {
	if (current_brush)  SetBrush(current_brush);
	if (current_pen)    SetPen(current_pen);
    }
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	XFillArc(DPY, DRAWABLE, BRUSH_GC, XLOG2DEV(x), YLOG2DEV(y),
		 XLOG2DEVREL(w), YLOG2DEVREL(h), 0, 64*360);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawArc(DPY, DRAWABLE, PEN_GC, XLOG2DEV(x), YLOG2DEV(y),
		 XLOG2DEVREL(w) - WX_GC_CF, YLOG2DEVREL(h) - WX_GC_CF, 0, 64*360);
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+w, y+h);
}

void wxWindowDC::DrawIcon(wxIcon *icon, float x, float y)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    if (icon->Ok() && current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	if (icon->GetDepth() == 1) {
	    XCopyPlane(DPY, GETPIXMAP(icon), DRAWABLE, PEN_GC,
		       0, 0, icon->GetWidth(), icon->GetHeight(),
		       XLOG2DEV(x), YLOG2DEV(y), 1);
	} else if (icon->GetDepth() == (signed)DEPTH) {
	    XCopyArea(DPY, GETPIXMAP(icon), DRAWABLE, PEN_GC,
		       0, 0, icon->GetWidth(), icon->GetHeight(),
		       XLOG2DEV(x), YLOG2DEV(y));
	}
    CalcBoundingBox(x, y);
}

void wxWindowDC::DrawLine(float x1, float y1, float x2, float y2)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLine(DPY, DRAWABLE, PEN_GC,
		  XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
    CalcBoundingBox(x1, y1);
    CalcBoundingBox(x2, y2);
}

void wxWindowDC::DrawLines(int n, wxPoint pts[], float xoff, float yoff)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    XPoint *xpts = new XPoint[n];
    for (int i=0; i<n; ++i)
	CalcBoundingBox(xpts[i].x = XLOG2DEV(pts[i].x + xoff),
			xpts[i].y = YLOG2DEV(pts[i].y + yoff));
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
    delete[] xpts;
}

void wxWindowDC::DrawLines(int n, wxIntPoint pts[], int xoff, int yoff)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    XPoint *xpts = new XPoint[n];
    for (int i=0; i<n; ++i)
	CalcBoundingBox(xpts[i].x = XLOG2DEV(pts[i].x + xoff),
			xpts[i].y = YLOG2DEV(pts[i].y + yoff));
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
    delete[] xpts;
}

void wxWindowDC::DrawLines(wxList *pts, float xoff, float yoff)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    int n = pts->Number();
    XPoint *xpts = new XPoint[n];
    int i = 0;
    for (wxNode *node=pts->First(); node; node=node->Next()) {
	wxPoint *point = (wxPoint*)node->Data();
	CalcBoundingBox(xpts[i].x = XLOG2DEV(point->x + xoff),
			xpts[i].y = YLOG2DEV(point->y + yoff));
	++i;
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
    delete[] xpts;
}

void wxWindowDC::DrawPoint(float x, float y)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawPoint(DPY, DRAWABLE, PEN_GC, XLOG2DEV(x), YLOG2DEV(y));
    CalcBoundingBox(x, y);
}

void wxWindowDC::DrawPolygon(int n, wxPoint pts[], float xoff, float yoff,
			     int fill)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting) {
	if (current_brush)  SetBrush(current_brush);
	if (current_pen)    SetPen(current_pen);
    }
    XPoint *xpts = new XPoint[n+1];
    for (int i=0; i<n; ++i)
	CalcBoundingBox(xpts[i].x = XLOG2DEV(pts[i].x + xoff),
			xpts[i].y = YLOG2DEV(pts[i].y + yoff));
    xpts[n].x = xpts[0].x; // close figure
    xpts[n].y = xpts[0].y;
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
	XSetFillRule(DPY, PEN_GC, fill_rule[fill]);
	XFillPolygon(DPY, DRAWABLE, BRUSH_GC, xpts, n, Complex, 0);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n+1, 0);
    delete[] xpts;
}

void wxWindowDC::DrawPolygon(wxList *pts, float xoff, float yoff, int fill)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting) {
	if (current_brush)  SetBrush(current_brush);
	if (current_pen)    SetPen(current_pen);
    }
    int n = pts->Number();
    XPoint *xpts = new XPoint[n+1];
    int i=0;
    for(wxNode *node=pts->First(); node; node=node->Next()) {
	wxPoint *point = (wxPoint*)node->Data();
	CalcBoundingBox(xpts[i].x = XLOG2DEV(point->x + xoff),
			xpts[i].y = YLOG2DEV(point->y + yoff));
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
    delete[] xpts;
}

void wxWindowDC::DrawRectangle(float x, float y, float w, float h)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting) {
	if (current_brush)  SetBrush(current_brush);
	if (current_pen)    SetPen(current_pen);
    }
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	XFillRectangle(DPY, DRAWABLE, BRUSH_GC, XLOG2DEV(x), YLOG2DEV(y),
		       XLOG2DEVREL(w), YLOG2DEVREL(h));
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawRectangle(DPY, DRAWABLE, PEN_GC, XLOG2DEV(x), YLOG2DEV(y),
		       XLOG2DEVREL(w) - WX_GC_CF, YLOG2DEVREL(h) - WX_GC_CF);
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+w, y+h);
}

void wxWindowDC::DrawRoundedRectangle(float x, float y, float w, float h,
				      float radius)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting) {
	if (current_brush)  SetBrush(current_brush);
	if (current_pen)    SetPen(current_pen);
    }
    if (radius < 0.0)
	radius = - radius * ((w < h) ? w : h);
    int xx = XLOG2DEV(x);    int yy = YLOG2DEV(y);
    int ww = XLOG2DEVREL(w); int hh = YLOG2DEVREL(h);
    int rr = XLOG2DEVREL(radius);
    int dd = 2 * rr;

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
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLine(DPY, DRAWABLE, PEN_GC,
		  XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
}

void wxWindowDC::IntDrawLines(int n, wxIntPoint pts[], int xoff, int yoff)
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    /* MATTHEW: [5] Implement GetPixel */
    FreeGetPixelCache();
    
    if (auto_setting && current_pen)
	SetPen(current_pen);
    XPoint *xpts = new XPoint[n];
    for (int i=0; i<n; ++i)
	CalcBoundingBox(xpts[i].x = XLOG2DEV(pts[i].x + xoff),
			xpts[i].y = YLOG2DEV(pts[i].y + yoff));
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	XDrawLines(DPY, DRAWABLE, PEN_GC, xpts, n, 0);
    delete[] xpts;
}

//-----------------------------------------------------------------------------
// drawing tools
//-----------------------------------------------------------------------------

void wxWindowDC::SetBackground(wxBrush *brush)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (current_background_brush) current_background_brush->Lock(-1);

    if (!(current_background_brush = brush)) // nothing to do without brush
	return;
    
    if (current_background_brush) current_background_brush->Lock(1);

    unsigned long pixel = brush->GetColour().GetPixel(current_cmap, IS_COLOR, 0);

    if (DRAW_WINDOW)
	XSetWindowBackground(DPY, DRAW_WINDOW, pixel);
    else
	XSetForeground(DPY, BG_GC, pixel);
    XSetBackground(DPY, PEN_GC, pixel);
    XSetBackground(DPY, BRUSH_GC, pixel);

    if (current_logical_fkt == wxXOR) { // use the correct pixel values for XOR
	SetPen(current_pen);
	SetBrush(current_brush);
    }
}

void wxWindowDC::SetBrush(wxBrush *brush)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (current_brush) current_brush->Lock(-1);

    if (!(current_brush = brush)) // nothing to do without brush
	return;

    if (current_brush) current_brush->Lock(1);

    // for XChangeGC
    XGCValues     values;
    unsigned long mask = GCFillStyle | GCForeground;

    values.fill_style = FillSolid;
    // wxXOR shall work the correct way
    unsigned long pixel = brush->GetColour().GetPixel(current_cmap, IS_COLOR, 1);
    if (current_logical_fkt == wxXOR) {
	XGCValues values_req;
	XGetGCValues(DPY, BRUSH_GC, GCBackground, &values_req);
	values.foreground = pixel ^ values_req.background;
    } else
	values.foreground = pixel;

    int style;
    switch (style=brush->GetStyle()) {
    case wxSOLID: case wxTRANSPARENT:
	break;
    default:
	if (wxIS_HATCH(style) || style==wxSTIPPLE || style==wxOPAQUE_STIPPLE) {
	    Pixmap stipple = (Pixmap)0; // for FillStippled
	    Pixmap tile    = (Pixmap)0; // for FillTiled
	    if (style==wxSTIPPLE || style==wxOPAQUE_STIPPLE) {
		if (brush->GetStipple() && brush->GetStipple()->Ok()) {
		    if (brush->GetStipple()->GetDepth() == 1) {
			stipple = GETPIXMAP(brush->GetStipple());
			values.fill_style = ((style==wxSTIPPLE) ? FillStippled : FillOpaqueStippled);
		    } else if (brush->GetStipple()->GetDepth() == (signed)DEPTH) {
			tile = GETPIXMAP(brush->GetStipple());
			values.fill_style = FillTiled;
		    } // else wrong depth
		}
		if (stipple || tile) 
		  values.foreground = IS_COLOR ? BlackPixel(DPY, DefaultScreen(DPY)) : 1;
	    } else {
		stipple = hatch_bitmaps[style-wxFIRST_HATCH];
		values.fill_style = FillStippled;
	    }
	    if (stipple) {
	      values.stipple = stipple;
	      mask |= GCStipple;
	    }
	    if (tile) {
	      values.tile = tile;
	      mask |= GCTile;
	    }
	}
	break;
    }
    XChangeGC(DPY, BRUSH_GC, mask, &values);
}

void wxWindowDC::SetColourMap(wxColourMap *new_cmap)
{
    current_cmap = new_cmap ? new_cmap : wxAPP_COLOURMAP;

    if (DRAW_WINDOW)
	XSetWindowColormap(DPY, DRAW_WINDOW, CMAP);
}

void wxWindowDC::SetLogicalFunction(int fkt)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (current_logical_fkt == fkt)
	return;
    Bool must_reset_tools = ((fkt==wxXOR) || (current_logical_fkt==wxXOR));
    XSetFunction(DPY, PEN_GC, function[fkt]);
    XSetFunction(DPY, BRUSH_GC, function[fkt]);
    current_logical_fkt = fkt;
    if (must_reset_tools) {
	// function has changed from or to XOR -> use the correct pixel values
	SetPen(current_pen);
	SetBrush(current_brush);
    }
}

void wxWindowDC::SetPen(wxPen *pen)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (current_pen) current_pen->Lock(-1);
      
    if (!(current_pen = pen)) // nothing to do without pen
	return;

    if (current_pen) current_pen->Lock(1);

    // for XChangeGC
    XGCValues     values;
    unsigned long mask = GCCapStyle  | GCFillStyle | GCForeground |
	                 GCJoinStyle | GCLineStyle | GCLineWidth;

    values.cap_style  = cap_style[pen->GetCap()];
    values.fill_style = FillSolid;
    values.join_style = join_style[pen->GetJoin()];
    values.line_style = LineSolid;
    int scale = // needed for dash-scaling
    values.line_width = XLOG2DEVREL(pen->GetWidth());
    // wxXOR shall work the correct way
    unsigned long pixel = pen->GetColour().GetPixel(current_cmap, IS_COLOR, 1);
    if (current_logical_fkt == wxXOR) {
	XGCValues values_req;
	XGetGCValues(DPY, PEN_GC, GCBackground, &values_req);
	values.foreground = pixel ^ values_req.background;
    } else
	values.foreground = pixel;

    int style;
    switch (style=pen->GetStyle()) {
    case wxSOLID: case wxTRANSPARENT:
	break;
    default:
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
	        int dscale = scale;
		if (!dscale) dscale = 1;
		for (int i=0; i<num_dash; ++i)
		    scaleddef[i] = dscale * dashdef[i];
		XSetDashes(DPY, PEN_GC, 0, (char*)scaleddef, num_dash);
		delete[] scaleddef;
	    } else { // not enough memory to scale
		XSetDashes(DPY, PEN_GC, 0, (char*)dashdef, num_dash);
	    }
	    values.line_style = LineOnOffDash; // for XChangeGC
	}
	if (wxIS_HATCH(style) || style==wxSTIPPLE || style==wxOPAQUE_STIPPLE) {
	    Pixmap stipple = (Pixmap)0; // for FillStippled
	    Pixmap tile    = (Pixmap)0; // for FillTiled
	    if (style==wxSTIPPLE || style==wxOPAQUE_STIPPLE) {
		if (pen->GetStipple() && pen->GetStipple()->Ok()) {
		    if (pen->GetStipple()->GetDepth() == 1) {
			stipple = GETPIXMAP(pen->GetStipple());
			values.fill_style =((style==wxSTIPPLE) ? FillStippled : FillOpaqueStippled);
		    } else if (pen->GetStipple()->GetDepth() == (signed)DEPTH) {
			tile = GETPIXMAP(pen->GetStipple());
			values.fill_style = FillTiled;
		    } // else wrong depth
		}
		if (stipple || tile) values.foreground = BlackPixel(DPY, DefaultScreen(DPY));
	    } else {
		stipple = hatch_bitmaps[style-wxFIRST_HATCH];
		values.fill_style = FillStippled;
	    }
	    if (stipple) XSetStipple(DPY, PEN_GC, stipple);
	    if (tile)    XSetTile(DPY, PEN_GC, tile);
	}
	break;
    }
    XChangeGC(DPY, PEN_GC, mask, &values);
}

void wxWindowDC::TryColour(wxColour *src, wxColour *dest)
{
  XColor xcol;

  xcol.pixel = src->GetPixel(current_cmap, IS_COLOR, 1);

  if (IS_COLOR) {
    XQueryColor(wxAPP_DISPLAY, GETCOLORMAP(current_cmap), &xcol);
    
    dest->Set(xcol.red >> SHIFT, xcol.green >> SHIFT, xcol.blue >> SHIFT);
  } else if (xcol.pixel == BlackPixel(DPY, DefaultScreen(DPY))) {
    dest->Set(0, 0, 0);
  } else {
    dest->Set(255, 255, 255);
  }
}


//-----------------------------------------------------------------------------
// text and font methods
//-----------------------------------------------------------------------------

void wxWindowDC::DrawText(char *text, float x, float y, Bool WXUNUSED(use16))
{
    if (!DRAWABLE) // ensure that a drawable has been associated
	return;

    if (!current_font) // a font must be associated for drawing
	return;

    XFontStruct *fontinfo = (XFontStruct*)current_font->GetInternalFont(scale_x);
    int         direction;
    int         ascent, descent, cx, cy;
    int         dev_x = XLOG2DEV(x);
    int         dev_y = YLOG2DEV(y);
    int         textlen = strlen(text);
    XCharStruct overall;

    (void)XTextExtents(fontinfo, text, textlen, &direction, &ascent, &descent,
		       &overall);
    cx = overall.width;
    cy = ascent + descent;
    if (current_text_bgmode != wxTRANSPARENT) {
	XDrawImageString(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, text, textlen);
    } else {
	XDrawString(DPY, DRAWABLE, TEXT_GC, dev_x, dev_y+ascent, text, textlen);
    }
    CalcBoundingBox(x, y);
    CalcBoundingBox(x+XDEV2LOG(cx), y+YDEV2LOG(cy));
}

float wxWindowDC::GetCharHeight(void)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return 0;

    if (!current_font) // no font
	return YDEV2LOGREL(12);

    int         direction, ascent, descent;
    XCharStruct overall;
    XTextExtents ((XFontStruct*)current_font->GetInternalFont(scale_x), "x", 1,
		  &direction, &ascent, &descent, &overall);
    return YDEV2LOGREL(ascent + descent);
}

float wxWindowDC::GetCharWidth(void)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return 0;

    if (!current_font)
	return XDEV2LOGREL(16);

    int         direction, ascent, descent;
    XCharStruct overall;
    XTextExtents ((XFontStruct*)current_font->GetInternalFont(scale_x), "x", 1,
		  &direction, &ascent, &descent, &overall);
    return XDEV2LOGREL(overall.width);
}

void wxWindowDC::GetTextExtent(const char *s, float *_w, float *_h, float *_descent,
			       float *_ext_leading, wxFont *_font,
			       Bool WXUNUSED(use16bit))
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    wxFont *font_to_use = _font ? _font : current_font;
    if (!font_to_use) {
	wxError("set a font before calling GetTextExtent", "wxWindowDC");
	*_w = *_h = -1.0;
	return;
    }

    int         direction, ascent, descent;
    XCharStruct overall;
    XTextExtents ((XFontStruct*)font_to_use->GetInternalFont(scale_x), s, strlen(s),
		  &direction, &ascent, &descent, &overall);
    *_w = XDEV2LOGREL(overall.width);
    *_h = YDEV2LOGREL(ascent + descent);
    if (_descent)
	*_descent = YDEV2LOGREL(descent);
    if (_ext_leading)
	*_ext_leading = 0.0;
}

void wxWindowDC::SetFont(wxFont *font)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!(current_font = font)) // nothing to do without a font
	return;

    XSetFont(DPY, TEXT_GC, ((XFontStruct*)font->GetInternalFont(scale_x))->fid);
}

void wxWindowDC::SetTextForeground(wxColour *col)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!col)
	return;
    current_text_fg = *col;
    XSetForeground(DPY, TEXT_GC, current_text_fg.GetPixel(current_cmap, IS_COLOR, 1));
}

void wxWindowDC::SetTextBackground(wxColour *col)
{
    if (!DRAWABLE) /* MATTHEW: [5] */
	return;

    if (!col)
	return;
    current_text_bg = *col;
    XSetBackground(DPY, TEXT_GC, current_text_bg.GetPixel(current_cmap, IS_COLOR, 0));
}

//-----------------------------------------------------------------------------
// clipping region
//-----------------------------------------------------------------------------

void wxWindowDC::SetClippingRegion(float x, float y, float w, float h)
{
    XRectangle r;

    if (w < 0)
      w = 0;
    if (h < 0)
      h = 0;

    if (USER_REG)
      XDestroyRegion(USER_REG);
    USER_REG = XCreateRegion();
    r.x      = XLOG2DEV(x);
    r.y      = XLOG2DEV(y);
    r.width  = XLOG2DEVREL(w);
    r.height = XLOG2DEVREL(h);
    XUnionRectWithRegion(&r, USER_REG, USER_REG);
    SetCanvasClipping();
}

/* MATTHEW: */
void wxWindowDC:: GetClippingRegion(float *x, float *y, float *w, float *h)
{
  if (USER_REG) {
    XRectangle r;
    XClipBox(USER_REG, &r);
    *x = XDEV2LOG(r.x);
    *y = YDEV2LOG(r.y);
    *w = XDEV2LOGREL(r.width);
    *h = YDEV2LOGREL(r.height);
  } else {
    *x = *y = 0;
    *w = *h = -1;
  }
}

void wxWindowDC::DestroyClippingRegion(void)
{
    if (USER_REG)
      XDestroyRegion(USER_REG);
    USER_REG = NULL;
    SetCanvasClipping();
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

    XGCValues values; unsigned long mask;
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
    wxColour c = current_text_fg; /* Don't pass &current_text_fg to SetTextForeground */
    SetTextForeground(&c);
    c = current_text_bg;
    SetTextBackground(&c);
    SetBackground(current_background_brush); 
    SetBrush(current_brush);
    SetPen(current_pen);

    wxFont *font = current_font;
    current_font = NULL;
    SetFont(font ? font : wxNORMAL_FONT);

    // set display scaling
    int width  = WidthOfScreen(SCN);
    int height = HeightOfScreen(SCN);
    mm_to_pix_x = float(width)  / float(WidthMMOfScreen(SCN));
    mm_to_pix_y = float(height) / float(HeightMMOfScreen(SCN));
}

void wxWindowDC::Destroy(void)
{
    if (PEN_GC)    XFreeGC(DPY, PEN_GC);
    if (BRUSH_GC)  XFreeGC(DPY, BRUSH_GC);
    if (TEXT_GC)   XFreeGC(DPY, TEXT_GC);
    if (BG_GC)     XFreeGC(DPY, BG_GC);
    PEN_GC = BRUSH_GC = TEXT_GC = BG_GC = NULL;

    if (CURRENT_REG) XDestroyRegion(CURRENT_REG);
    if (USER_REG) XDestroyRegion(USER_REG);
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

void wxWindowDC::GetClippingBox(float *x, float *y, float *w, float *h)
{
    if (CURRENT_REG) {
	XRectangle r;
	XClipBox(CURRENT_REG, &r);
	*x = XDEV2LOG(r.x);
	*y = YDEV2LOG(r.y);
	*w = XDEV2LOGREL(r.width);
	*h = YDEV2LOGREL(r.height);
    } else {
	*x = *y = *w = *h = 0.0;
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

  if (!DRAWABLE)
    return FALSE;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  unsigned int w, h;
  Window wdummy; int sdummy; unsigned int udummy;
  XGetGeometry(DPY, DRAWABLE, &wdummy, &sdummy, &sdummy,
	       &w, &h, &udummy, &udummy);

  if (i < 0 || (unsigned int)i >= w
      || j < 0 || (unsigned int)j >= h)
    return FALSE;

#define NUM_GETPIX_CACHE_COLORS 256

  if (!X->get_pixel_image_cache) {
    if (X->is_window) /* Disallow for now */
      return FALSE;

    X->get_pixel_image_cache = 
      XGetImage(DPY, DRAWABLE, 0, 0, w, h, AllPlanes, ZPixmap);

    X->get_pixel_cache_pos = 0;
    X->get_pixel_cache_full = FALSE;
    X->get_pixel_color_cache = new XColor[NUM_GETPIX_CACHE_COLORS];

    if (X->get_pixel_image_cache->depth == 1) {
      XColor *get_pixel_color_cache = X->get_pixel_color_cache;
      
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

  int get_pixel_cache_pos = X->get_pixel_cache_pos;
  XColor *get_pixel_color_cache = X->get_pixel_color_cache;
  Bool get_pixel_cache_full = X->get_pixel_cache_full;

  int k;
  unsigned long pixel;
  XColor xcol;

  pixel = XGetPixel(X->get_pixel_image_cache, i, j);

  for (k = get_pixel_cache_pos; k--; )
    if (get_pixel_color_cache[k].pixel == pixel) {
      col->Set(get_pixel_color_cache[k].red,
	       get_pixel_color_cache[k].green,
	       get_pixel_color_cache[k].blue);
      return TRUE;
    }

  if (get_pixel_cache_full)
    for (k = NUM_GETPIX_CACHE_COLORS; k-- > get_pixel_cache_pos; )
      if (get_pixel_color_cache[k].pixel == pixel) {
	col->Set(get_pixel_color_cache[k].red,
		 get_pixel_color_cache[k].green,
		 get_pixel_color_cache[k].blue);
	return TRUE;
      }
  
  xcol.pixel = pixel;
  XQueryColor(wxAPP_DISPLAY, GETCOLORMAP(current_cmap), &xcol);

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
  if (!DRAWABLE)
    return;

  if (X->get_pixel_image_cache)
    return;

  int x, y;
  unsigned int w, h;
  Window wdummy; unsigned int udummy;
  XGetGeometry(DPY, DRAWABLE, &wdummy, &x, &y, &w, &h, &udummy, &udummy);

  if (X->is_window) {
    /* For now, disallow: */
    return;
  }

  X->get_pixel_image_cache = XGetImage(DPY, DRAWABLE, 0, 0, w, h, AllPlanes, ZPixmap);
  
  X->get_pixel_cache_pos = 0;
  X->get_pixel_cache_full = FALSE;
  X->get_pixel_color_cache = new XColor[NUM_GETPIX_CACHE_COLORS];
}

void wxWindowDC::EndSetPixel()
{
  if (!X->get_pixel_image_cache)
    return;

  int w, h;
  w = X->get_pixel_image_cache->width;
  h = X->get_pixel_image_cache->height;

  XPutImage(DPY, DRAWABLE, PEN_GC, X->get_pixel_image_cache, 0, 0, 0, 0, w, h);

  FreeGetPixelCache();
}

void wxWindowDC::SetPixel(float x, float y, wxColour * col)
{
  int i, j;

  if (!X->get_pixel_image_cache)
    return;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  int w, h, k;
  w = X->get_pixel_image_cache->width;
  h = X->get_pixel_image_cache->height;

  if (i < 0 || i >= w
      || j < 0 || j >= h)
    return;

  int red, green, blue;
  XColor xcol;
  unsigned long pixel;

  red = col->Red();
  green = col->Green();
  blue = col->Blue();

  XImage *get_pixel_image_cache = X->get_pixel_image_cache;
  int get_pixel_cache_pos = X->get_pixel_cache_pos;
  XColor *get_pixel_color_cache = X->get_pixel_color_cache;
  Bool get_pixel_cache_full = X->get_pixel_cache_full;

  
  for (k = get_pixel_cache_pos; k--; )
    if ((get_pixel_color_cache[k].red == red)
	&& (get_pixel_color_cache[k].green == green)
	&& (get_pixel_color_cache[k].blue == blue)) {
      pixel = get_pixel_color_cache[k].pixel;
      goto put;
    }

  if (get_pixel_cache_full)
    for (k = NUM_GETPIX_CACHE_COLORS; k-- > get_pixel_cache_pos; )
      if ((get_pixel_color_cache[k].red == red)
	  && (get_pixel_color_cache[k].green == green)
	  && (get_pixel_color_cache[k].blue == blue)) {
	pixel = get_pixel_color_cache[k].pixel;
	goto put;
      }

  xcol.red = red << 8;
  xcol.green = green << 8;
  xcol.blue = blue << 8;
  
  wxAllocColor(DPY, GETCOLORMAP(current_cmap), &xcol);

  pixel = xcol.pixel;

  get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
  get_pixel_color_cache[get_pixel_cache_pos].red = red;
  get_pixel_color_cache[get_pixel_cache_pos].green = green;
  get_pixel_color_cache[get_pixel_cache_pos].blue = blue;

  if (++(X->get_pixel_cache_pos) >= NUM_GETPIX_CACHE_COLORS) {
    X->get_pixel_cache_pos = 0;
    X->get_pixel_cache_full = TRUE;
  }

 put:
  pixel = XPutPixel(get_pixel_image_cache, i, j, pixel);
}

void wxWindowDC::DoFreeGetPixelCache(void)
{
  if (X->get_pixel_image_cache) {
    XDestroyImage(X->get_pixel_image_cache);
    X->get_pixel_image_cache = NULL;
    delete[] X->get_pixel_color_cache;
    X->get_pixel_color_cache = NULL;
  }
}

