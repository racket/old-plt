/*
 * File:      wx_dc.cc
 * Purpose:     Device context implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_dc.cxx,v 1.8 1999/01/09 19:18:40 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_dc.cc	1.2 5/9/94";

/* MATTHEW: [9] 
   About pens, brushes, and the autoSetting flag:

   In this implementation, pens and brushes control some of the same X
   drawing parameters. Therefore, it is impossible to independently
   maintain the current pen and the current brush. Also, some settings
   depend on the current logical function. The current_fill,
   etc. instance variables remember state across the brush and pen.

   Since pens are used more than brushes, the autoSetting flag
   is used to indicate that a brush was recently used, and SetPen
   must be called to reinstall the current pen's parameters.

   There is, unfortunately, some confusion between setting the
   current pen/brush and actually installing the brush/pen parameters.
   Both functionalies are perform by SetPen and SetBrush. C'est la vie.
*/

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_dc.h"
#pragma implementation "wx_dccan.h"
#pragma implementation "wx_dcpan.h"
#pragma implementation "wx_dcmem.h"
#endif

#define UseXtRegions
#include "wx_rgn.h"

/* Moved here to avoid platform-detection hacks include conflicts */
#include "wx_obj.h"

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#ifndef M_PI
/*steve: in some math libs this is not defined! */
#define M_PI 3.14159265
#endif
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// shift between wxWindows RGB- and XColor RGB-values
// (necessary because the values specify an intensity)
#define SHIFT (8*(sizeof(short int)-sizeof(char)))

#include "common.h"
#include "wx_frame.h"
#include "wx_dc.h"
#include "wx_dccan.h"
#include "wx_dcpan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_main.h"
#include "wx_privt.h"

#include "bdiag.xbm"
#include "fdiag.xbm"
#include "cdiag.xbm"
#include "horiz.xbm"
#include "verti.xbm"
#include "cross.xbm"

static Pixmap bdiag, cdiag, fdiag, cross, horiz, verti;

int wxGetBestMatchingPixel(Display *display,Colormap cmap,XColor * desiredColor);

// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

/* MATTHEW: [6] Changed normalization */
// This normalizes the graphics code to behave inm a standard way when
// WX_STANDARD_GRAPHICS is 1.
#if WX_STANDARD_GRAPHICS
#define WX_GC_CF 1
#else
#define WX_GC_CF 0
#endif

// Added by Patrick...
// Needs further explanation before it's used!
#if PIXEL0_DISABLE
Status 
YAllocColor (Display * dpy, Colormap cmap, XColor * color)
{
  // Allocate a Color, but avoid to get the pixel #0
  // If such pixel is returned, return a R/W cell instead.
  // This is necessary to have workable wxXOR, wxOR,...
  // because anything XOR 0 = anything,
  //         anything  OR 0 = anything, so no visual effects!
  // Please note that there is also a potential pb whith pixel 255 and
  // wxAND -- but if we allocate pixel 255 as ReadOnly, there is probably
  // no more R/W cells.

  //wxDebugMsg("YAllocColor %d,%d,%d\n",color->red,color->green,color->blue) ;
  Status result = XAllocColor (dpy, cmap, color);

  if (result == 0)
    return (0);
  if (color->pixel == 0)
    {
      // Ooops... we don't want THIS pixel!!!
      result = XAllocColorCells (dpy, cmap, False, NULL, 0, &color->pixel, 1);
      // No R/W. We must deal with pixel 0. Aaaaarghhh!
      if (result == 0)
	return XAllocColor (dpy, cmap, color);
      XStoreColor (dpy, cmap, color);
    }
  return (result);
}
#else
#define YAllocColor XAllocColor
#endif

static int failmsg = 1;

static Status 
AllocDCColor(Display * dpy, Colormap cmap, XColor * color, int fg, int is_color)
{
  if (is_color)
    return wxAllocColor(dpy, cmap, color);
  else {
    int white;
    if (fg) {
      /* foreground: white = white, all else = black */
      white = (((color->red >> SHIFT) == 255)
	       && ((color->green >> SHIFT) == 255)
	       && ((color->blue >> SHIFT) == 255));
    } else {
      /* background: black = black, all else = white */
      white = (color->red || color->green || color->blue);
    }

    if (white)
      color->pixel = 0;
    else
      color->pixel = 1;

    return 1;
  }
}

static void ColourFailed()
{
  if (failmsg) {
    cerr << "Warning: cannot allocate colour, using black/white instead.\n";
    cerr << "(Future allocations may fail, but this warning is only printed once.)\n";
    
    failmsg = 0;
  }
}

extern Colormap wxMainColormap;

IMPLEMENT_ABSTRACT_CLASS(wxDC, wxWindow)
IMPLEMENT_DYNAMIC_CLASS(wxCanvasDC, wxDC)

// Default constructor
wxCanvasDC::wxCanvasDC (void)
{
  __type = wxTYPE_DC_CANVAS;

  selected_pixmap = NULL;
  canvas = NULL;
  WXGC_IGNORE(canvas);
  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  display = wxGetDisplay(); /* MATTHEW: [13] */

  current_reg = NULL;
  onpaint_reg = NULL;

  device = wxDEVICE_CANVAS;
  font = NULL;

  min_x = 0;
  min_y = 0;
  max_x = 0;
  max_y = 0;

  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;

  gc = NULL;

  background_pixel = 0;
  ok = FALSE; /* MATTHEW: [13] */
  current_pen_width = -1;
  current_pen_join = -1;
  current_pen_cap = -1;
  current_pen_nb_dash = -1;
  current_pen_dash = NULL;
  current_stipple = NULL;
  current_style = -1;
  current_fill = -1;

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = *wxWHITE;

  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
  current_bk_mode = wxTRANSPARENT;

  color = Colour = wxColourDisplay ();

  SetFont(wxNORMAL_FONT);

  /* MATTHEW: [7] Implement GetPixel */
  get_pixel_image_cache = NULL;
}


wxCanvasDC:: wxCanvasDC (wxCanvas * the_canvas):wxbCanvasDC (the_canvas)
{
  __type = wxTYPE_DC_CANVAS;

  canvas = the_canvas;
  WXGC_IGNORE(canvas);
  display = canvas->GetXDisplay();
  selected_pixmap = NULL;
  pixmap = XtWindow((Widget)canvas->handle); /* MATTHEW: [15] */
  XSetWindowColormap (display, pixmap, wxGetMainColormap(display));

  pixmapWidth = 0;
  pixmapHeight = 0;

  current_reg = NULL;
  onpaint_reg = NULL;

  min_x = 0;
  min_y = 0;
  max_x = 0;
  max_y = 0;

  device = wxDEVICE_CANVAS;
  font = NULL;

  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;

  XGCValues gcvalues;
  gcvalues.foreground = BlackPixel (display, DefaultScreen (display));
  gcvalues.background = WhitePixel (display, DefaultScreen (display));
  gcvalues.graphics_exposures = False;
  gcvalues.line_width = 1;
  gc = XCreateGC (display, RootWindow (display, DefaultScreen (display)),
	    GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth,
		  &gcvalues);

  background_pixel = (int) gcvalues.background;
  ok = TRUE;
  current_pen_width = -1;
  current_pen_join = -1;
  current_pen_cap = -1;
  current_pen_nb_dash = -1;
  current_pen_dash = NULL;
  current_stipple = NULL;
  current_style = -1;
  current_fill = -1;

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = *wxWHITE;

  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
  current_bk_mode = wxTRANSPARENT;
  color = Colour = wxColourDisplay ();
  SetBrush (wxWHITE_BRUSH);
  SetPen (wxBLACK_PEN);
  SetFont(wxNORMAL_FONT);

  /* MATTHEW: [7] Implement GetPixel */
  get_pixel_image_cache = NULL;
}

wxCanvasDC::~wxCanvasDC (void)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);

  if (gc)
    XFreeGC (display, gc);
  gc = NULL;
}

void wxCanvasDC::DoFreeGetPixelCache(void)
{
  EndSetPixel();
}

void wxCanvasDC::SetCanvasClipping(void)
{
  if (current_reg)
    XDestroyRegion (current_reg);

  if (clipping || onpaint_reg)
    current_reg = XCreateRegion();
  else
    current_reg = NULL;

  if (onpaint_reg && clipping)
    XIntersectRegion(onpaint_reg, clipping->rgn, current_reg);
  else if (clipping)
    XIntersectRegion(clipping->rgn, clipping->rgn, current_reg);
  else if (onpaint_reg)
    XIntersectRegion(onpaint_reg, onpaint_reg, current_reg);

  if (current_reg)
    XSetRegion(display, gc, current_reg);
  else
    XSetClipMask(display, gc, None);
}

void wxCanvasDC::SetClippingRect(float cx, float cy, float cw, float ch)
{
  clipping = new wxRegion(this);
  clipping->SetRectangle(cx, cy, cw, ch);
  SetCanvasClipping();
}

void wxCanvasDC::SetClippingRegion(wxRegion *r)
{
  clipping = r;
  SetCanvasClipping();
}

wxRegion *wxCanvasDC::GetClippingRegion()
{
  return clipping;
}

void wxCanvasDC::GetSize(float *w, float *h)
{
  if (canvas) {
    int ww, hh;

    canvas->GetVirtualSize(&ww, &hh);
    *w = ww;
    *h = hh;
  } else {
    *w = *h = 0;
  }
}

void wxCanvasDC:: Clear(void)
{
  int w, h;
  float fw, fh;

  GetSize(&fw, &fh);
  w = (int)fw;
  h = (int)fh;
  if (canvas) {
    int ww, hh;
    canvas->GetClientSize(&ww, &hh);
    if (ww > w)
      w = ww;
    if (hh > h)
      h = hh;
  }

  /* MATTHEW: [9] Save old brush */
  wxBrush *save_brush = current_brush;

  /* MATTHEW: [13] Implement GetPixel */
  FreeGetPixelCache();

  SetBrush(wxTheBrushList->FindOrCreateBrush(&current_background_color, wxSOLID));
  XFillRectangle(display, pixmap, gc, 0, 0, w, h);

  /* MATTHEW: [9] restore */
  current_brush = save_brush;
}

void wxCanvasDC:: CrossHair (float x, float y)
{
  if (current_pen && autoSetting)
    SetPen (current_pen);

  /* MATTHEW: [13] Implement GetPixel */
  FreeGetPixelCache();

  int xx = XLOG2DEV (x);
  int yy = YLOG2DEV (y);
  float ww, hh;
  GetSize(&ww, &hh);
  XDrawLine (display, pixmap, gc, 0, yy, (int)ww, yy);
  XDrawLine (display, pixmap, gc, xx, 0, xx, (int)hh);
}

void wxCanvasDC::FloodFill(float x, float y, wxColour * col, int style)
{
}

Bool wxCanvasDC::GetPixel(float x, float y, wxColour * col)
{
  int i, j;

  if (!pixmap)
    return FALSE;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  int w, h;
  if (canvas) {
    float fw, fh;
    GetSize(&fw, &fh);
    w = (int)fw; 
    h = (int)fh;
  } else {
    w = pixmapWidth;
    h = pixmapHeight;
  }

  if (i < 0 || i >= w
      || j < 0 || j >= h)
    return FALSE;

#define NUM_GETPIX_CACHE_COLORS 256

  if (!get_pixel_image_cache) {
    if (canvas) /* Disallow for now */
      return FALSE;
    
    BeginSetPixel();

    if (selected_pixmap && (selected_pixmap->GetDepth() == 1)) {
      get_pixel_color_cache[0].pixel = 1;
      get_pixel_color_cache[0].red = 0;
      get_pixel_color_cache[0].green = 0;
      get_pixel_color_cache[0].blue = 0;

      get_pixel_color_cache[1].pixel = 0;
      get_pixel_color_cache[1].red = 255;
      get_pixel_color_cache[1].green = 255;
      get_pixel_color_cache[1].blue = 255;
      
      get_pixel_cache_pos = 2;
    }
  }

  int k;
  unsigned long pixel;
  XColor xcol;

  pixel = XGetPixel(get_pixel_image_cache, i, j);

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
  XQueryColor(display, wxGetMainColormap(display), &xcol);

  get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
  get_pixel_color_cache[get_pixel_cache_pos].red = xcol.red >> SHIFT;
  get_pixel_color_cache[get_pixel_cache_pos].green = xcol.green >> SHIFT;
  get_pixel_color_cache[get_pixel_cache_pos].blue = xcol.blue >> SHIFT;

  col->Set(get_pixel_color_cache[get_pixel_cache_pos].red,
	   get_pixel_color_cache[get_pixel_cache_pos].green,
	   get_pixel_color_cache[get_pixel_cache_pos].blue);

  if (++get_pixel_cache_pos >= NUM_GETPIX_CACHE_COLORS) {
    get_pixel_cache_pos = 0;
    get_pixel_cache_full = TRUE;
  }

  return TRUE;
}

void wxCanvasDC::BeginSetPixel()
{
  if (!pixmap)
    return;

  if (get_pixel_image_cache)
    return;

  if (canvas) /* Disallow for now */
    return;

  int w, h;
  w = pixmapWidth;
  h = pixmapHeight;

  get_pixel_image_cache = 
    XGetImage(display, pixmap, 0, 0, w, h, AllPlanes, ZPixmap);
  
  get_pixel_cache_pos = 0;
  get_pixel_cache_full = FALSE;
  get_pixel_color_cache = new XColor[NUM_GETPIX_CACHE_COLORS];
  set_a_pixel = 0;
}

void wxCanvasDC::EndSetPixel()
{
  if (!get_pixel_image_cache)
    return;

  if (set_a_pixel) {
    int w, h;
    w = get_pixel_image_cache->width;
    h = get_pixel_image_cache->height;
    
    XPutImage(display, pixmap, gc, get_pixel_image_cache, 0, 0, 0, 0, w, h);
  }

  if (get_pixel_image_cache) {
    XDestroyImage(get_pixel_image_cache);
    get_pixel_image_cache = NULL;
    delete[] get_pixel_color_cache;
    get_pixel_color_cache = NULL;
  }
}

void wxCanvasDC::SetPixel(float x, float y, wxColour * col)
{
  int i, j;

  BeginSetPixel();

  if (!get_pixel_image_cache)
    return;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  int w, h;
  w = get_pixel_image_cache->width;
  h = get_pixel_image_cache->height;

  if (i < 0 || i >= w
      || j < 0 || j >= h)
    return;

  int red, green, blue, k;
  XColor xcol;
  unsigned long pixel;

  red = col->Red();
  green = col->Green();
  blue = col->Blue();

  set_a_pixel = 1;

  if (selected_pixmap && (selected_pixmap->GetDepth() == 1)) {
    if ((red == 255) && (green == 255) && (blue == 255))
      pixel = 0;
    else
      pixel = 1;
  } else {
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
    
    xcol.red = red << SHIFT;
    xcol.green = green << SHIFT;
    xcol.blue = blue << SHIFT;
    
    AllocDCColor(display, wxGetMainColormap(display), &xcol, 1, color);
    
    pixel = xcol.pixel;
    
    get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
    get_pixel_color_cache[get_pixel_cache_pos].red = red;
    get_pixel_color_cache[get_pixel_cache_pos].green = green;
    get_pixel_color_cache[get_pixel_cache_pos].blue = blue;
    
    if (++get_pixel_cache_pos >= NUM_GETPIX_CACHE_COLORS) {
      get_pixel_cache_pos = 0;
      get_pixel_cache_full = TRUE;
    }
  }

 put:
  pixel = XPutPixel(get_pixel_image_cache, i, j, pixel);
}

void wxCanvasDC:: IntDrawLine (int x1, int y1, int x2, int y2)
{
  DrawLine((float)x1, (float)y1, (float)x2, (float)y2);
}

void wxCanvasDC:: DrawLine (float x1, float y1, float x2, float y2)
{
  int x1d, y1d, x2d, y2d;

  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  x1d = XLOG2DEV(x1);
  y1d = YLOG2DEV(y1);
  x2d = XLOG2DEV(x2);
  y2d = YLOG2DEV(y2);

  if (current_pen && autoSetting)
    SetPen (current_pen);
  XDrawLine (display, pixmap, gc, x1d, y1d, x2d, y2d);
  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
}

void wxCanvasDC:: DrawArc (float x, float y, float w, float h, float start, float end)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();
    
  int xx = XLOG2DEV(x); int yy = XLOG2DEV(y);
  int ww = XLOG2DEVREL(w); int hh = XLOG2DEVREL(h);
  double degrees1, degrees2;
  degrees1 = start * 360.0 / (2 * M_PI);
  degrees2 = end * 360.0 / (2 * M_PI);
  int alpha1 = int(degrees1 * 64.0);
  int alpha2 = int((degrees2 - degrees1) * 64.0);
  while (alpha2 <= 0)
    alpha2 += 360*64;
  while (alpha1 > 360*64)
    alpha1 -= 360*64;
  
  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    SetBrush(current_brush);
    XFillArc(display, pixmap, gc, xx, yy, ww, hh, alpha1, alpha2);
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    if (autoSetting) SetPen (current_pen);
    XDrawArc(display, pixmap, gc, xx, yy, ww, hh, alpha1, alpha2);
  }

  CalcBoundingBox(x, y);
  CalcBoundingBox(x + w, y + h);
}

void wxCanvasDC:: DrawPoint (float x, float y)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  if (current_pen && autoSetting)
    SetPen (current_pen);

  XDrawPoint (display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y));
  CalcBoundingBox (x, y);
}

void wxCanvasDC:: DrawPolygon (int n, wxPoint points[], float xoffset, float yoffset, int fillStyle)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  XPoint *xpoints1 = new XPoint[n + 1];
  XPoint *xpoints2 = new XPoint[n + 1];
  int i;
  for (i = 0; i < n; i++)
    {
      xpoints1[i].x = XLOG2DEV (points[i].x + xoffset);
      xpoints1[i].y = YLOG2DEV (points[i].y + yoffset);
      xpoints2[i].x = XLOG2DEV_2 (points[i].x + xoffset);
      xpoints2[i].y = YLOG2DEV_2 (points[i].y + yoffset);
      CalcBoundingBox (points[i].x + xoffset, points[i].y + yoffset);
    }

  // Close figure for XDrawLines (not needed for XFillPolygon)
  xpoints1[i].x = xpoints1[0].x;
  xpoints1[i].y = xpoints1[0].y;
  xpoints2[i].x = xpoints2[0].x;
  xpoints2[i].y = xpoints2[0].y;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      XSetFillRule (display, gc, fillStyle == wxODDEVEN_RULE ? EvenOddRule : WindingRule);
      XFillPolygon (display, pixmap, gc, xpoints1, n, Complex, 0);
      XSetFillRule (display, gc, EvenOddRule);	// default mode
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawLines (display, pixmap, gc, xpoints1, n + 1, 0);
    }

  delete[]xpoints1;
  delete[]xpoints2;
}

void wxCanvasDC:: DrawLines (int n, wxIntPoint points[], int xoffset, int yoffset)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);

      XPoint *xpoints = new XPoint[n];
      int i;

      for (i = 0; i < n; i++)
	{
	  xpoints[i].x = XLOG2DEV (points[i].x + xoffset);
	  xpoints[i].y = YLOG2DEV (points[i].y + yoffset);
	}
      XDrawLines (display, pixmap, gc, xpoints, n, 0);
      delete[]xpoints;
    }

}

void wxCanvasDC:: DrawLines (int n, wxPoint points[], float xoffset, float yoffset)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);

      XPoint *xpoints = new XPoint[n];
      int i;

      for (i = 0; i < n; i++)
	{
	  xpoints[i].x = XLOG2DEV (points[i].x + xoffset);
	  xpoints[i].y = YLOG2DEV (points[i].y + yoffset);
	}

      XDrawLines (display, pixmap, gc, xpoints, n, 0);

      delete[]xpoints;
    }
}

void wxCanvasDC:: DrawRectangle (float x, float y, float width, float height)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  int xd, yd, wfd, hfd, wd, hd;

  xd = XLOG2DEV(x);
  yd = YLOG2DEV(y);
  wfd = XLOG2DEVREL(width);
  wd = wfd - WX_GC_CF;
  hfd = YLOG2DEVREL(height);
  hd = hfd - WX_GC_CF;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      XFillRectangle (display, pixmap, gc, xd, yd, wfd, hfd);
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawRectangle (display, pixmap, gc, xd, yd, wd, hd);
    }
  CalcBoundingBox (x, y);
  CalcBoundingBox (x + width, y + height);
}

void wxCanvasDC:: DrawRoundedRectangle (float x, float y, float width, float height, float radius)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  // If radius is negative, you can crash your ENTIRE X server. Wow!

  if (radius < 0.0)
    {
      // Now, a negative radius is interpreted to mean
      // 'the proportion of the smallest X or Y dimension'
      float smallest = 0.0;
      if (width < height)
	smallest = width;
      else
	smallest = height;
      radius = (float) (-radius * smallest);
    }

  int phys_x = XLOG2DEV (x);
  int phys_y = YLOG2DEV (y);
  int phys_radius = XLOG2DEVREL (radius);
  int phys_width = XLOG2DEVREL (width) - WX_GC_CF;
  int phys_height = YLOG2DEVREL (height) - WX_GC_CF;

  

  int phys_rwidth = phys_radius * 2;
  int phys_rheight = phys_rwidth;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      XFillRectangle (display, pixmap, gc, phys_x + phys_radius, phys_y,
		      phys_width - phys_rwidth, phys_height);
      XFillRectangle (display, pixmap, gc, phys_x, phys_y + phys_radius,
		      phys_width, phys_height - phys_rheight);

      XFillArc (display, pixmap, gc, phys_x, phys_y,
		phys_rwidth, phys_rheight, 90 * 64, 90 * 64);
      XFillArc (display, pixmap, gc, phys_x + phys_width - phys_rwidth, phys_y,
		phys_rwidth, phys_rheight, 0, 90 * 64);
      XFillArc (display, pixmap, gc, phys_x + phys_width - phys_rwidth,
		phys_y + phys_height - phys_rheight,
		phys_rwidth, phys_rheight, 270 * 64, 90 * 64);
      XFillArc (display, pixmap, gc, phys_x, phys_y + phys_height - phys_rheight,
		phys_rwidth, phys_rheight, 180 * 64, 90 * 64);
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawLine (display, pixmap, gc, phys_x + phys_radius, phys_y,
		 phys_x + phys_width - phys_radius, phys_y);
      XDrawLine (display, pixmap, gc, phys_x + phys_radius, phys_y + phys_height,
		 phys_x + phys_width - phys_radius, phys_y + phys_height);

      XDrawLine (display, pixmap, gc, phys_x, phys_y + phys_radius,
		 phys_x, phys_y + phys_height - phys_radius);
      XDrawLine (display, pixmap, gc, phys_x + phys_width, phys_y + phys_radius,
		 phys_x + phys_width, phys_y + phys_height - phys_radius);
      XDrawArc (display, pixmap, gc, phys_x, phys_y,
		phys_rwidth, phys_rheight, 90 * 64, 90 * 64);
      XDrawArc (display, pixmap, gc, phys_x + phys_width - phys_rwidth, phys_y,
		phys_rwidth, phys_rheight, 0, 90 * 64);
      XDrawArc (display, pixmap, gc, phys_x + phys_width - phys_rwidth,
		phys_y + phys_height - phys_rheight,
		phys_rwidth, phys_rheight, 270 * 64, 90 * 64);
      XDrawArc (display, pixmap, gc, phys_x, phys_y + phys_height - phys_rheight,
		phys_rwidth, phys_rheight, 180 * 64, 90 * 64);
    }
  CalcBoundingBox (x, y);
  CalcBoundingBox (x + width, y + height);
}

void wxCanvasDC:: DrawEllipse (float x, float y, float width, float height)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

 static const int angle = 23040;

  int xd, yd, wd, hd;

  xd = XLOG2DEV(x);
  yd = YLOG2DEV(y);
  wd = XLOG2DEVREL(width) - WX_GC_CF;
  hd = YLOG2DEVREL(height) - WX_GC_CF;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      XFillArc (display, pixmap, gc, xd, yd, wd, hd, 0, angle);
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawArc (display, pixmap, gc, xd, yd, wd, hd, 0, angle);
    }
  CalcBoundingBox (x, y);
  CalcBoundingBox (x + width, y + height);
}

void wxCanvasDC:: SetFont (wxFont * the_font)
{
  font = the_font;
  if (!font)
    return;

  int res_x = 100;
  int res_y = 100;

  int scaled_size = (int) (10 * ((int) (user_scale_y * font->GetPointSize () + 0.5)));

  /* MATTHEW: Provide display, use fontid */
  XFontStruct *fontStruct = wxFontPool->FindNearestFont (font->GetFontId(), 
							 font->GetStyle (),
							 font->GetWeight (), scaled_size,
							 font->GetUnderlined (), 
							 res_x, res_y,
							 display);
  xfont = fontStruct; /* MATTHEW: xfont */

  Font theFont = fontStruct->fid;

  if (gc)
    XSetFont(display, gc, theFont);
}

static int alloc_close_color(Display *display, Colormap cmap, XColor *xc)
{
  XColor ctab[256];
  int ncells = DisplayCells(display, DefaultScreen(display)), j;

  ncells = (ncells < 256) ? ncells : 256;
  
  for (j = 0; j < ncells; j++)
    ctab[j].pixel = j;

  XQueryColors(display, cmap, ctab, ncells);

  int           d, mdist, close;
  
  mdist = 0;   close = -1;
  
  for (j = 0; j < ncells; j++) {
    d = (abs((int)(xc->red - ctab[j].red)) +
	 abs((int)(xc->green - ctab[j].green)) +
	 abs((int)(xc->blue - ctab[j].blue)));
    if (!mdist || (d < mdist)) { 
      mdist = d; 
      close = j;
    }
  }

  if (wxAllocColor(display, cmap, &ctab[close])) { 
    static int approxmsg = 1;
    if (approxmsg) {
      cerr << "Warning: cannot allocate colour, using approximate match instead.\n";
      cerr << "(Future allocations may be approximate without this report.)\n";
      
      approxmsg = 0;
    }

    xc->pixel = ctab[close].pixel;
    return 1;
  } else
    return 0;
}

void wxCanvasDC::SetPen(wxPen * pen)
{
  wxBitmap *old_stipple = current_stipple;
  int old_style = current_style;
  int old_fill = current_fill;
  int old_pen_width = current_pen_width;
  int old_pen_join = current_pen_join;
  int old_pen_cap = current_pen_cap;
  int old_pen_nb_dash = current_pen_nb_dash;
  char *old_pen_dash = current_pen_dash;
  int is_bitmap = 0;

  if (current_pen) current_pen->Lock(-1);
  current_pen = pen;
  if (current_pen) current_pen->Lock(1);

  if (!pen)
    return;

  if (pen->GetStyle() == wxTRANSPARENT)
    return; /* handled at drawing sites */

  wxColour old_pen_colour = current_colour;
  current_colour = pen->GetColour();
  current_style = pen->GetStyle();
  current_fill = pen->GetStyle();
  current_pen_width = pen->GetWidth();
  current_pen_join = pen->GetJoin();
  current_pen_cap = pen->GetCap();
  current_pen_nb_dash = pen->nb_dash;
  current_pen_dash = pen->dash;
  current_stipple = pen->GetStipple();
  if (current_stipple) {
    if (current_stipple->Ok()) {
      if (current_stipple->GetDepth() == 1)
	is_bitmap = 1;
      else if (!color)
	current_stipple = NULL;
    } else
      current_stipple = NULL;
  }

  Bool same_style = (old_style == current_style
		     && old_fill == current_fill
		     && old_pen_join == current_pen_join
		     && old_pen_cap == current_pen_cap
		     && old_pen_nb_dash == current_pen_nb_dash
		     && old_pen_dash == current_pen_dash
		     && old_pen_width == current_pen_width
		     && (!!old_stipple) == (!!current_stipple));

  int style = current_style;
  int xor = 0, old_xor = 0;
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
  switch (old_style) {
  case wxXOR:
  case wxXOR_DOT:
  case wxXOR_SHORT_DASH:
  case wxXOR_LONG_DASH:
  case wxXOR_DOT_DASH:
    old_xor = 1;
    break;
  }

  Bool same_colour = (old_xor == xor
		      && !!current_stipple == !!old_stipple
		      && ((current_stipple && !is_bitmap)
			  || (old_pen_colour.Red() == current_colour.Red()
			      && old_pen_colour.Blue() == current_colour.Blue()
			      && old_pen_colour.Green() == current_colour.Green()
			      && old_pen_colour.pixel == current_colour.pixel)));

  if (!same_style || !dcOptimize) {
    int scaled_width = (int)XLOG2DEVREL(pen->GetWidth());

    if (scaled_width < 0)
      scaled_width = 0;
    
    int join;
    int cap;
    static char dotted[] = {2, 5};
    static char short_dashed[] = {4, 4};
    static char long_dashed[] = {4, 8};
    static char dotted_dashed[] = {6, 6, 2, 6};

    // We express dash pattern in pen width unit, so we are
    // independent of zoom factor and so on...
    int req_nb_dash;
    char *req_dash;

    if (current_stipple)
      style = wxSOLID;
     
    switch (style) {
      case wxUSER_DASH:
	req_nb_dash = current_pen_nb_dash;
	req_dash = current_pen_dash;
	style = LineOnOffDash;
	break;
      case wxDOT:
	req_nb_dash = 2;
	req_dash = dotted;
	style = LineOnOffDash;
	break;
      case wxSHORT_DASH:
	req_nb_dash = 2;
	req_dash = short_dashed;
	style = LineOnOffDash;
	break;
      case wxLONG_DASH:
	req_nb_dash = 2;
	req_dash = long_dashed;
	style = LineOnOffDash;
	break;
      case wxDOT_DASH:
	req_nb_dash = 4;
	req_dash = dotted_dashed;
	style = LineOnOffDash;
	break;
      default:
	style = LineSolid;
	req_dash = NULL;
	req_nb_dash = 0;
      }

      if (req_dash && req_nb_dash) {
	char *real_req_dash = new char[req_nb_dash];
	if (real_req_dash) {
	  int factor = scaled_width == 0 ? 1 : scaled_width;
	  for (int i = 0; i < req_nb_dash; i++)
	    real_req_dash[i] = req_dash[i] * factor;
	  XSetDashes (display, gc, 0, real_req_dash, req_nb_dash);
	  delete[]real_req_dash;
	} else {
	  // No Memory. We use non-scaled dash pattern...
	  XSetDashes (display, gc, 0, req_dash, req_nb_dash);
	}
      }

      switch (pen->GetCap()) {
      case wxCAP_PROJECTING:
	cap = CapProjecting;
	break;
      case wxCAP_BUTT:
	cap = CapButt;
	break;
      case wxCAP_ROUND:
      default:
	cap = CapRound;
	break;
      }

      switch (pen->GetJoin()) {
      case wxJOIN_BEVEL:
	join = JoinBevel;
	break;
      case wxJOIN_MITER:
	join = JoinMiter;
	break;
      case wxJOIN_ROUND:
      default:
	join = JoinRound;
	break;
      }
      
      XSetLineAttributes(display, gc, scaled_width, style, cap, join);

      if (current_stipple && ((current_stipple != old_stipple) || !dcOptimize)) {
	if (is_bitmap) {
	  if (current_stipple->selectedTo) current_stipple->selectedTo->EndSetPixel();
	  XSetStipple(display, gc, current_stipple->x_pixmap);
	  XSetFillStyle(display, gc, (style == wxSOLID) ? FillStippled : FillOpaqueStippled);
	} else { 
	  if (current_stipple->selectedTo) current_stipple->selectedTo->EndSetPixel();
	  XSetTile(display, gc, current_stipple->x_pixmap);
	  XSetFillStyle(display, gc, FillTiled);
	}
      } else {
	XSetFillStyle(display, gc, FillSolid);
      }

      XSetFunction(display, gc, xor ? GXxor : GXcopy);
  }

  if (!same_colour || !dcOptimize) {
    int pixel = -1;
    if (pen->GetStyle() == wxTRANSPARENT)
      pixel = background_pixel;
    else if (current_stipple && !is_bitmap)
      pixel = color ? (int)BlackPixel(display, DefaultScreen(display)) : 1;
    else {
      if (Colour && (pen->GetColour ().pixel != -1))
	pixel = pen->GetColour ().pixel;
      else {
	XColor exact_def;
	exact_def.red = (unsigned short) (((long) pen->GetColour ().Red ()) << SHIFT);
	exact_def.green = (unsigned short) (((long) pen->GetColour ().Green ()) << SHIFT);
	exact_def.blue = (unsigned short) (((long) pen->GetColour ().Blue ()) << SHIFT);
	exact_def.flags = DoRed | DoGreen | DoBlue;
	
	Colormap cmap = wxGetMainColormap(display);
	if (!AllocDCColor (display, cmap, &exact_def, 1, color)
	    && !alloc_close_color(display, cmap, &exact_def)) {
	  ColourFailed();
	  pixel = wxGetBestMatchingPixel(display,cmap,&exact_def);
	} else
	  pixel = (int) exact_def.pixel;
	current_colour.pixel = pixel;
	if (Colour)
	  pen->GetColour().pixel = pixel;
      }
    }

    // Finally, set the GC to the required colour
    if (pixel > -1) {
      if (xor) {
	XGCValues values;
	XGetGCValues (display, gc, GCBackground, &values);
	XSetForeground (display, gc, pixel ^ values.background);
      } else {
	XSetForeground (display, gc, pixel);
      }
    }
  } else
    pen->GetColour().pixel = old_pen_colour.pixel;
  
  /* No longer need setting: */
  autoSetting = 0;
}

void wxCanvasDC::SetBrush(wxBrush * brush)
{
  int old_fill = current_fill;
  wxBitmap *old_stipple = current_stipple;
  int is_bitmap = 0;

  if (current_brush) current_brush->Lock(-1);
  current_brush = brush;
  if (current_brush) current_brush->Lock(1);

  if (!brush || brush->GetStyle() == wxTRANSPARENT)
    return;

  /* Pen must be reset */
  autoSetting |= 0x1;

  current_stipple = brush->GetStipple();
  if (current_stipple) {
    if (current_stipple->Ok()) {
      if (current_stipple->GetDepth() == 1)
	is_bitmap = 1;
      else if (!color)
	current_stipple = NULL;
    } else
      current_stipple = NULL;
  }

  current_fill = brush->GetStyle();

  wxColour old_brush_colour(current_colour);
  current_colour = brush->GetColour();
  Bool same_colour = (!!current_stipple == !!old_stipple
		      && (old_fill == wxXOR) == (current_fill == wxXOR)
		      && ((current_stipple && !is_bitmap)
			  || ((!old_stipple || (old_stipple->GetDepth() == 1))
			      && old_brush_colour.Red() == current_colour.Red()
			      && old_brush_colour.Blue() == current_colour.Blue()
			      && old_brush_colour.Green() == current_colour.Green()
			      && old_brush_colour.pixel == current_colour.pixel)));
  
  if ((old_fill != current_fill) || (current_stipple != old_stipple) || !dcOptimize) {
    if (current_stipple) {
      if (is_bitmap) {
	if (current_stipple->selectedTo) current_stipple->selectedTo->EndSetPixel();
	XSetStipple(display, gc, current_stipple->x_pixmap);
	XSetFillStyle(display, gc, (current_fill == wxSOLID) ? FillStippled : FillOpaqueStippled);
      } else { 
	if (current_stipple->selectedTo) current_stipple->selectedTo->EndSetPixel();
	XSetTile(display, gc, current_stipple->x_pixmap);
	XSetFillStyle(display, gc, FillTiled);
      }
    } else {
      Pixmap my_stipple = 0;

      switch (current_fill) {
      case wxBDIAGONAL_HATCH:
	if (bdiag == (Pixmap) 0)
	  bdiag = XCreateBitmapFromData (display,
					 RootWindow (display, DefaultScreen (display)),
					 bdiag_bits, bdiag_width, bdiag_height);
	my_stipple = bdiag;
	break;
      case wxFDIAGONAL_HATCH:
	if (fdiag == (Pixmap) 0)
	  fdiag = XCreateBitmapFromData (display,
					 RootWindow (display, DefaultScreen (display)),
					 fdiag_bits, fdiag_width, fdiag_height);
	my_stipple = fdiag;
	break;
      case wxCROSS_HATCH:
	if (cross == (Pixmap) 0)
	  cross = XCreateBitmapFromData (display,
					 RootWindow (display, DefaultScreen (display)),
					 cross_bits, cross_width, cross_height);
	my_stipple = cross;
	break;
      case wxHORIZONTAL_HATCH:
	if (horiz == (Pixmap) 0)
	  horiz = XCreateBitmapFromData (display,
					 RootWindow (display, DefaultScreen (display)),
					 horiz_bits, horiz_width, horiz_height);
	my_stipple = horiz;
	break;
      case wxVERTICAL_HATCH:
	if (verti == (Pixmap) 0)
	  verti = XCreateBitmapFromData (display,
					 RootWindow (display, DefaultScreen (display)),
					 verti_bits, verti_width, verti_height);
	my_stipple = verti;
	break;
      case wxCROSSDIAG_HATCH:
	if (cdiag == (Pixmap) 0)
	  cdiag = XCreateBitmapFromData (display,
					 RootWindow (display, DefaultScreen (display)),
					 cdiag_bits, cdiag_width, cdiag_height);
	my_stipple = cdiag;
	break;
      default:
	break;
      }

      if (my_stipple) XSetStipple (display, gc, my_stipple);
      XSetFillStyle(display, gc, my_stipple ? FillStippled : FillSolid);
    }

    XSetFunction(display, gc, (current_fill == wxXOR) ? GXxor : GXcopy);
  }

  if (!same_colour || !dcOptimize) {
    int pixel = -1;
    if (current_stipple && !is_bitmap)
      pixel = color ? (int)BlackPixel(display, DefaultScreen(display)) : 1;
    else if (brush->GetStyle () != wxTRANSPARENT) {
      if (Colour && (brush->GetColour ().pixel > -1))
	pixel = brush->GetColour ().pixel;
      else {
	XColor exact_def;
	exact_def.red = (unsigned short) (((long) brush->GetColour ().Red ()) << SHIFT);
	exact_def.green = (unsigned short) (((long) brush->GetColour ().Green ()) << SHIFT);
	exact_def.blue = (unsigned short) (((long) brush->GetColour ().Blue ()) << SHIFT);
	exact_def.flags = DoRed | DoGreen | DoBlue;
	
	Colormap cmap = wxGetMainColormap(display);
	if (!AllocDCColor (display, cmap, &exact_def, 1, color)
	    && !alloc_close_color(display, cmap, &exact_def)) {
	  ColourFailed();
	  pixel = wxGetBestMatchingPixel(display,cmap,&exact_def);
	} else
	  pixel = (int) exact_def.pixel;
	current_colour.pixel = pixel;
	if (Colour)
	  brush->GetColour().pixel = pixel;
      }
    } 

    if (pixel > -1) {
      // Finally, set the GC to the required colour
      if (current_fill == wxXOR) {
	XGCValues values;
	XGetGCValues(display, gc, GCBackground, &values);
	XSetForeground(display, gc, pixel ^ values.background);
      } else {
	XSetForeground(display, gc, pixel);
      }
    }
  } else
    brush->GetColour().pixel = old_brush_colour.pixel;
}

/* MATTHEW */
void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  XColor xcol;

  xcol.red = (unsigned short) (((long) src->Red()) << SHIFT);
  xcol.green = (unsigned short) (((long) src->Green()) << SHIFT);
  xcol.blue = (unsigned short) (((long) src->Blue()) << SHIFT);
  xcol.flags = DoRed | DoGreen | DoBlue;

  Colormap cmap = wxGetMainColormap(display);
  
  if (!AllocDCColor(display, cmap, &xcol, 1, color))
    dest->Set(0, 0, 0);
  else
    dest->Set(xcol.red >> SHIFT, xcol.green >> SHIFT, xcol.blue >> SHIFT);
}

void wxCanvasDC::SetColourMap(wxColourMap *cmap)
{
  if (canvas)
  {
    if (cmap)
      /* Use GetXColormap */
      XSetWindowColormap (display, canvas->xwindow, 
			  cmap->GetXColormap(display));
    else
      /* Use wxGetMainColormap */
      XSetWindowColormap (display, canvas->xwindow, 
			  wxGetMainColormap(display));
  }
}

/* Helper function for 16-bit fonts */
static int str16len(const char *s)
{
  int count = 0;

  while (s[0] && s[1]) {
    count++;
    s += 2;
  }

  return count;
}

void wxCanvasDC:: DrawText (const char *text, float x, float y, Bool use16Bit)
{
  // Since X draws from the baseline of the text, must
  // add the text height
  int cx = 0;
  int cy = 0;
  int ascent = 0;
  int slen;

  if (use16Bit)
    slen = str16len(text);
  else
    slen = strlen(text);

  if (font)
    {
      XFontStruct *font_info = xfont; /* MATTHEW: [4] xfont */
      int direction, descent;
      XCharStruct overall_return;
      /* MATTHEW: [2] handle 16-bit mode */
      if (use16Bit)
	(void)XTextExtents16(font_info, (XChar2b *)text, slen, &direction,
			     &ascent, &descent, &overall_return);
      else
	(void)XTextExtents(font_info, text, slen, &direction, 
			   &ascent, &descent, &overall_return);
      cx = overall_return.width;
      cy = ascent + descent;
    }

  // First draw a rectangle representing the text background,
  // if a text background is specified
  if (current_text_background.Ok () && (current_bk_mode != wxTRANSPARENT))
    {
      wxColour old_pen_colour = current_colour;
      current_colour = current_text_background;
      Bool same_colour = (old_pen_colour.Ok () && current_text_background.Ok () &&
		(old_pen_colour.Red () == current_text_background.Red ()) &&
	      (old_pen_colour.Blue () == current_text_background.Blue ()) &&
	    (old_pen_colour.Green () == current_text_background.Green ()));
            
      // This separation of the big && test required for gcc2.7/HP UX 9.02
      // or pixel value can be corrupted!
      same_colour = (same_colour &&
		   (old_pen_colour.pixel == current_text_background.pixel));

      if (!same_colour || !dcOptimize)
	{
	  int pixel = -1;
	  if (current_text_background.Ok ())
	    {
	      if (current_text_background.pixel > -1)
		pixel = current_text_background.pixel;
	      else
		{
		  XColor exact_def;
		  exact_def.red = (unsigned short) (((long) current_text_background.Red ()) << SHIFT);
		  exact_def.green = (unsigned short) (((long) current_text_background.Green ()) << SHIFT);
		  exact_def.blue = (unsigned short) (((long) current_text_background.Blue ()) << SHIFT);
		  exact_def.flags = DoRed | DoGreen | DoBlue;

//     Colormap cmap = DefaultColormap(display, DefaultScreen(display));
		  /* MATTHEW: [4] Use wxGetMainColormap */
		  Colormap cmap = wxGetMainColormap(display);

		  if (!AllocDCColor(display, cmap, &exact_def, 0, color)
		      && !alloc_close_color(display, cmap, &exact_def))
		    {
		      pixel = (int) WhitePixel (display, DefaultScreen (display));
		      ColourFailed();
		    }
		  else
		    pixel = (int) exact_def.pixel;
		  current_colour.pixel = current_text_background.pixel = pixel;
		}
	    }
	  // Set the GC to the required colour
	  if (pixel > -1)
	    {
	      XSetForeground (display, gc, pixel);
	    }
	}
      else
	current_text_background.pixel = old_pen_colour.pixel;

      XFillRectangle (display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y), cx, cy);
    }

  // Now set the text foreground and draw the text
  if (current_text_foreground.Ok ())
    {
      wxColour old_pen_colour = current_colour;
      current_colour = current_text_foreground;
      Bool same_colour = (old_pen_colour.Ok () && current_colour.Ok () &&
			  (old_pen_colour.Red () == current_colour.Red ()) &&
			  (old_pen_colour.Blue () == current_colour.Blue ()) &&
			  (old_pen_colour.Green () == current_colour.Green ()) &&
			  (old_pen_colour.pixel == current_colour.pixel));

      if (!same_colour || !dcOptimize)
	{
	  int pixel = -1;
	    {
	      if (Colour && (current_text_foreground.pixel > -1))
		pixel = current_text_foreground.pixel;
	      else
		{
		  XColor exact_def;
		  exact_def.red = (unsigned short) (((long) current_text_foreground.Red ()) << SHIFT);
		  exact_def.green = (unsigned short) (((long) current_text_foreground.Green ()) << SHIFT);
		  exact_def.blue = (unsigned short) (((long) current_text_foreground.Blue ()) << SHIFT);
		  exact_def.flags = DoRed | DoGreen | DoBlue;

		  /* MATTHEW: [4] Use wxGetMainColormap */
		  Colormap cmap = wxGetMainColormap(display);

		  if (!AllocDCColor (display, cmap, &exact_def, 1, color)
		      && !alloc_close_color(display, cmap, &exact_def))
		    {
		      pixel = (int) BlackPixel (display, DefaultScreen (display));
		      ColourFailed();
		    }
		  else
		    pixel = (int) exact_def.pixel;
		  current_colour.pixel = current_text_foreground.pixel = pixel;
		}
	    }

	  // Set the GC to the required colour
	  if (pixel > -1)
	    {
	      XSetForeground (display, gc, pixel);
	    }
	}
      else
	current_text_foreground.pixel = old_pen_colour.pixel;
    }

  current_fill = wxCOPY;
  XSetFunction(display, gc, GXcopy);

  // We need to add the ascent, not the whole height, since X draws
  // at the point above the descender.
  /* MATTHEW: [2] handle 16-bit mode */
  if (use16Bit)
    XDrawString16(display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y) + ascent, 
		  (XChar2b *)text, slen);
  else
    XDrawString(display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y) + ascent, 
		text, slen);
  float w, h;
  GetTextExtent (text, &w, &h);
  CalcBoundingBox (x + w, y + h);
  CalcBoundingBox (x, y);

  autoSetting = 1;
}

void wxCanvasDC::SetBackground(wxColor * c)
{
  int pixel;

  current_background_color = *c;

  autoSetting = 0x1;

  if (Colour && (current_background_color.pixel > -1))
    pixel = current_background_color.pixel;
  else {
    XColor exact_def;
    exact_def.red = (unsigned short) (((long) current_background_color.Red ()) << SHIFT);
    exact_def.green = (unsigned short) (((long) current_background_color.Green()) << SHIFT);
    exact_def.blue = (unsigned short) (((long)  current_background_color.Blue()) << SHIFT);
    exact_def.flags = DoRed | DoGreen | DoBlue;
    
    Colormap cmap = wxGetMainColormap(display);
    if (!AllocDCColor (display, cmap, &exact_def, 0, color)
	&& !alloc_close_color(display, cmap, &exact_def)) {
      pixel = (int) WhitePixel (display, DefaultScreen (display));
      ColourFailed();
    } else
      pixel = (int) exact_def.pixel;

    if (Colour)
      current_background_color.pixel = pixel;
  }

  if (canvas) XSetWindowBackground(display, pixmap, pixel);

  // Necessary for ::DrawIcon, which use fg/bg pixel or the GC.
  // And Blit,... (Any fct that use XCopyPlane, in fact.)
  XSetBackground (display, gc, pixel);
}

Bool wxCanvasDC:: StartDoc (char *)
{
  return TRUE;
}

void wxCanvasDC:: EndDoc (void)
{
}

void wxCanvasDC:: StartPage (void)
{
}

void wxCanvasDC:: EndPage (void)
{
}

float wxCanvasDC:: GetCharHeight (void)
{
  if (!xfont)
    return XDEV2LOGREL (12);

  int direction, ascent, descent;
  XCharStruct overall;
  XTextExtents (xfont, "x", 1, &direction, &ascent,
		&descent, &overall);

  return XDEV2LOGREL(ascent + descent);
}

float wxCanvasDC:: GetCharWidth (void)
{
  if (!xfont)
    return XDEV2LOGREL (16);

  int direction, ascent, descent;
  XCharStruct overall;
  XTextExtents (xfont, "x", 1, &direction, &ascent,
		&descent, &overall);
  return XDEV2LOGREL(overall.width);
}

/* MATTHEW: This was broken, since it assumed that theFont has been setup to
   draw to this canvas. Only fixed for Motif, where we have to do display-specific
   work anyway. */
void wxCanvasDC:: GetTextExtent (const char *string, float *x, float *y,
	       float *descent, float *externalLeading, wxFont *theFont,
				 Bool use16Bit)
{
   wxFont *oldfont;
   XFontStruct *oldxfont;

   oldfont = font;
   oldxfont = xfont;
   if (theFont)
     SetFont(theFont);

  if (!xfont) {
    cerr << "Warning: set a font before calling GetTextExtent!\n";
    *x = -1;
    *y = -1;

    font = oldfont;
    xfont = oldxfont;

    return;
  }

  XFontStruct *fontStruct = xfont;
  int direction, ascent, descent2;
  XCharStruct overall;
  int slen;
  
  if (use16Bit) slen = str16len(string); else slen = strlen(string);

  if (use16Bit)
    XTextExtents16(fontStruct, (XChar2b *)string, slen, &direction,
		   &ascent, &descent2, &overall);
  else
    XTextExtents(fontStruct, string, slen, &direction, 
		 &ascent, &descent2, &overall);
    
  *x = XDEV2LOGREL (overall.width);
  *y = YDEV2LOGREL (ascent + descent2);
  if (descent)
    *descent = (float) descent2;
  if (externalLeading)
    *externalLeading = 0.0;
}

void wxCanvasDC:: SetMapMode (int mode)
{
  mapping_mode = mode;

  int pixel_width = 0;
  int pixel_height = 0;
  int mm_width = 0;
  int mm_height = 0;

  // First, calculate how to scale from mm to pixels.
  // Then we just need to find the scaling factor from ? to mm and multiply
  // by the first scaling factor.
  Display *dpy = display;
  if (!dpy && wxTheApp->wx_frame)
      dpy = wxGetDisplay();

  int screen_no = DefaultScreen (dpy);
  pixel_width = DisplayWidth (dpy, screen_no);
  pixel_height = DisplayHeight (dpy, screen_no);
  mm_width = DisplayWidthMM (dpy, screen_no);
  mm_height = DisplayHeightMM (dpy, screen_no);

  float mm2pixelsX = pixel_width / mm_width;
  float mm2pixelsY = pixel_height / mm_height;

  switch (mode)
    {
    case MM_TWIPS:
      {
	logical_scale_x = (float) (twips2mm * mm2pixelsX);
	logical_scale_y = (float) (twips2mm * mm2pixelsY);
	break;
      }
    case MM_POINTS:
      {
	logical_scale_x = (float) (pt2mm * mm2pixelsX);
	logical_scale_y = (float) (pt2mm * mm2pixelsY);
	break;
      }
    case MM_METRIC:
      {
	logical_scale_x = mm2pixelsX;
	logical_scale_y = mm2pixelsY;
	break;
      }
    case MM_LOMETRIC:
      {
	logical_scale_x = (float) (mm2pixelsX / 10.0);
	logical_scale_y = (float) (mm2pixelsY / 10.0);
	break;
      }
    default:
    case MM_TEXT:
      {
	logical_scale_x = 1.0;
	logical_scale_y = 1.0;
	break;
      }
    }
}

void wxCanvasDC:: SetUserScale (float x, float y)
{
  user_scale_x = x;
  user_scale_y = y;

  // Force recalculation of line width
  wxPen *old_pen = current_pen;
  if (old_pen)
    {
      current_pen = NULL;
      SetPen (old_pen);
    }
}

float wxCanvasDC:: DeviceToLogicalX (int x)
{
  return XDEV2LOG (x);
}

float wxCanvasDC:: DeviceToLogicalXRel (int x)
{
  return XDEV2LOGREL (x);
}

float wxCanvasDC:: DeviceToLogicalY (int y)
{
  return YDEV2LOG (y);
}

float wxCanvasDC:: DeviceToLogicalYRel (int y)
{
  return YDEV2LOGREL (y);
}

int wxCanvasDC:: LogicalToDeviceX (float x)
{
  return XLOG2DEV (x);
}

int wxCanvasDC:: LogicalToDeviceXRel (float x)
{
  return XLOG2DEVREL (x);
}

int wxCanvasDC:: LogicalToDeviceY (float y)
{
  return YLOG2DEV (y);
}

int wxCanvasDC:: LogicalToDeviceYRel (float y)
{
  return YLOG2DEVREL (y);
}

Bool wxCanvasDC::Blit(float xdest, float ydest, float width, float height,
		      wxBitmap *source, float xsrc, float ysrc, int rop, wxColour *c)
{
  if (!source->Ok() || !pixmap) return FALSE;

  if (source->GetDepth() > 1) {
    /* rop & c don't matter; use GCBlit */
    return GCBlit(xdest, ydest, width, height, source, xsrc, ysrc);
  }

  FreeGetPixelCache();

  wxColor bg;
  wxPen *save_pen = current_pen;
  
  bg = current_background_color;
  if (!c) c = wxBLACK;

  if (source->selectedTo) 
    source->selectedTo->EndSetPixel();

  if (rop != wxSTIPPLE)
    SetBackground(wxWHITE);

  int scaled_width
    = source->GetWidth()  < XLOG2DEVREL(width) ? source->GetWidth()  : XLOG2DEVREL(width);
  int scaled_height
    = source->GetHeight() < YLOG2DEVREL(height) ? source->GetHeight() : YLOG2DEVREL(height);

  SetPen(wxThePenList->FindOrCreatePen(c, 0, rop));
  save_pen->Lock(1); /* We're going to mash save_pen back into current_pen */
    
  if ((rop == wxSOLID) || (rop == wxXOR)) {
    /* Seems like the easiest way to implement transparent backgrounds is to
       use a stipple. */
    XGCValues values;
    unsigned long mask = GCFillStyle | GCStipple | GCTileStipXOrigin | GCTileStipYOrigin;
    values.stipple = source->x_pixmap;
    values.fill_style = FillStippled;
    values.ts_x_origin = ((XLOG2DEV(xdest) - (long)xsrc) % source->GetWidth());
    values.ts_y_origin = ((YLOG2DEV(ydest) - (long)ysrc) % source->GetHeight());
    current_stipple = source;
    XChangeGC(display, gc, mask, &values);
    XFillRectangle(display, pixmap, gc, XLOG2DEV(xdest), YLOG2DEV(ydest), 
		   scaled_width, scaled_height);
  } else {
    XCopyPlane (display, source->x_pixmap, pixmap, gc,
		(long)xsrc, (long)ysrc, scaled_width, scaled_height,
		XLOG2DEV(xdest), YLOG2DEV(ydest), 1);
  }

  CalcBoundingBox(xdest, ydest);
  CalcBoundingBox(xdest + width, ydest + height);

  SetBackground(&bg);
  autoSetting = 0x1;
  current_pen = save_pen;

  return TRUE;
}

Bool wxCanvasDC::GCBlit(float xdest, float ydest, float width, float height,
			wxBitmap *source, float xsrc, float ysrc)
{
  /* Non-allocating (of collectable memory) Blit */

  if (!source->Ok() || !pixmap) return FALSE;

  FreeGetPixelCache();

  Bool retval = FALSE;

  if (source->selectedTo) 
    source->selectedTo->EndSetPixel();

  int scaled_width
    = source->GetWidth()  < XLOG2DEVREL(width) ? source->GetWidth()  : XLOG2DEVREL(width);
  int scaled_height
    = source->GetHeight() < YLOG2DEVREL(height) ? source->GetHeight() : YLOG2DEVREL(height);

  if (pixmap && source->Ok()) {
    XGCValues values;
    int mask = 0;

    if (!color && (source->GetDepth() > 1)) {
      /* color->mono; may need to flip bitmaps */
      if (BlackPixel(display, DefaultScreen(display)) == 1) {
	mask = GCFunction;
	values.function = GXcopyInverted;
      }
    }

    GC gc = XCreateGC(display, pixmap, mask, &values);
    if (clipping)
      XSetRegion(display, gc, clipping->rgn);

    if (!color || (source->GetDepth() == 1)) {
      XCopyPlane (display, source->x_pixmap, pixmap, gc,
		  (long)xsrc, (long)ysrc, scaled_width, scaled_height,
		  XLOG2DEV(xdest), YLOG2DEV(ydest), 1);
    } else {
      XCopyArea (display, source->x_pixmap, pixmap, gc,
		 (long)xsrc, (long)ysrc, scaled_width, scaled_height,
		 XLOG2DEV(xdest), YLOG2DEV(ydest));
    }
    
    XFreeGC(display, gc);
  
    retval = TRUE;
  }

  return retval;
}

/*
 * Memory DC
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxMemoryDC, wxCanvasDC)

wxMemoryDC::wxMemoryDC (Bool ro)
{
  __type = wxTYPE_DC_MEMORY;
  display = wxGetDisplay();

  read_only = ro;
  device = wxDEVICE_PIXMAP;
//  current_colour = NULL;
  current_pen_width = -1;
  current_pen_join = -1;
  current_pen_cap = -1;
  current_pen_nb_dash = -1;
  current_pen_dash = NULL;
  current_stipple = NULL;
  current_style = -1;
  current_fill = -1;

  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  canvas = NULL;
  clipping = FALSE;

  ok = FALSE;
  title = NULL;

  font = NULL;
  min_x = 0;
  min_y = 0;
  max_x = 0;
  max_y = 0;
  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = *wxWHITE;
  current_text_foreground = *wxBLACK;
//  current_text_background = NULL;

  XGCValues gcvalues;
  gcvalues.foreground = BlackPixel (display,
				    DefaultScreen (display));
  gcvalues.background = WhitePixel (display,
				    DefaultScreen (display));
  gcvalues.graphics_exposures = False;
  gcvalues.line_width = 1;
  gc = XCreateGC (display, RootWindow (display, DefaultScreen (display)),
	    GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth,
		  &gcvalues);

  background_pixel = (int) gcvalues.background;
  ok = TRUE;
//  current_colour = NULL;
  selected_pixmap = NULL;

  color = Colour = wxColourDisplay ();
  SetBrush (wxWHITE_BRUSH);
  SetPen (wxBLACK_PEN);
  SetFont(wxNORMAL_FONT);
}

wxMemoryDC::~wxMemoryDC (void)
{
  if (selected_pixmap) {
    if (!read_only) {
      selected_pixmap->selectedIntoDC = 0;
      selected_pixmap->selectedTo = NULL;
    }
    selected_pixmap = NULL;
  }
}

void wxMemoryDC:: SelectObject (wxBitmap * bitmap)
{
  if (bitmap == selected_pixmap)
    return;

  FreeGetPixelCache();

  if (gc)
    XFreeGC(display, gc);
  gc = NULL;

  if (bitmap && (!bitmap->Ok() || (!read_only && (bitmap->display != display || bitmap->selectedIntoDC))))
    bitmap = NULL;

  if (!read_only) {
    if (selected_pixmap) {
      selected_pixmap->selectedIntoDC = 0;
      selected_pixmap->selectedTo = NULL;
    }
  }

  if (!bitmap)
  {
    selected_pixmap = NULL;
    pixmap = 0;
    pixmapWidth = 0;
    pixmapHeight = 0;
    ok = FALSE; /* MATTHEW: [6] We've killed the GC, so it's not OK anymore */
    return;
  }
  
  selected_pixmap = bitmap;
  pixmap = bitmap->x_pixmap;
  pixmapWidth = bitmap->GetWidth ();
  pixmapHeight = bitmap->GetHeight ();
  if (!read_only) {
    bitmap->selectedIntoDC = -1;
    bitmap->selectedTo = this;
  }
  color = Colour = (bitmap->GetDepth() != 1);

  XGCValues gcvalues;
  gcvalues.foreground = color ? BlackPixel(display, DefaultScreen(display)) : 1;
  gcvalues.background = color ? WhitePixel(display, DefaultScreen(display)) : 0;
  gcvalues.graphics_exposures = False;
  gcvalues.line_width = 1;
  gc = XCreateGC (display, pixmap,
		  GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth,
		  &gcvalues);
  ok = TRUE;

  /* MATTHEW: [6] Reset pen and brush */
  int save_opt = dcOptimize;
  dcOptimize = FALSE;
  SetPen(current_pen);
  SetBrush(current_brush);
  SetFont(font);
  dcOptimize = save_opt;
}

wxBitmap* wxMemoryDC::GetObject()
{
  return selected_pixmap;
}

void wxMemoryDC::GetSize(float *w, float *h)
{
  *w = pixmapWidth;
  *h = pixmapHeight;
}

/****** Utility function for wxItem/wxPanel ***********/

#ifdef wx_motif
XColor itemColors[5];
int 
wxComputeColors (Display *display, wxColour * back, wxColour * fore)
#else
int 
wxComputeColors (Display *display, wxColour *, wxColour *)
#endif
{
  int result;
#ifdef wx_motif
  static XmColorProc colorProc;
#endif

  result = wxNO_COLORS;
#ifdef wx_motif
  if (back)
    {
      itemColors[0].red = (((long) back->Red ()) << SHIFT);
      itemColors[0].green = (((long) back->Green ()) << SHIFT);
      itemColors[0].blue = (((long) back->Blue ()) << SHIFT);
      itemColors[0].flags = DoRed | DoGreen | DoBlue;
      if (colorProc == (XmColorProc) NULL)
	{
	  // Get a ptr to the actual function
	  colorProc = XmSetColorCalculation ((XmColorProc) NULL);
	  // And set it back to motif.
	  XmSetColorCalculation (colorProc);
	}
      (*colorProc) (&itemColors[wxBACK_INDEX],
		    &itemColors[wxFORE_INDEX],
		    &itemColors[wxSELE_INDEX],
		    &itemColors[wxTOPS_INDEX],
		    &itemColors[wxBOTS_INDEX]);
      result = wxBACK_COLORS;
    }
  if (fore)
    {
      itemColors[wxFORE_INDEX].red = (((long) fore->Red ()) << SHIFT);
      itemColors[wxFORE_INDEX].green = (((long) fore->Green ()) << SHIFT);
      itemColors[wxFORE_INDEX].blue = (((long) fore->Blue ()) << SHIFT);
      itemColors[wxFORE_INDEX].flags = DoRed | DoGreen | DoBlue;
      if (result == wxNO_COLORS)
	result = wxFORE_COLORS;
    }

  Display *dpy = display;
  Colormap cmap = wxGetMainColormap(dpy);

  if (back)
    {
      /* 5 Colours to allocate */
      for (int i = 0; i < 5; i++)
	if (!wxAllocColor (dpy, cmap, &itemColors[i]))
	  result = wxNO_COLORS;
    }
  else if (fore)
    {
      /* Only 1 colour to allocate */
      if (!wxAllocColor (dpy, cmap, &itemColors[wxFORE_INDEX]))
	result = wxNO_COLORS;
    }
#endif

  return (result);

}

/*
 * Panel device context
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxPanelDC, wxCanvasDC)

// Default constructor
wxPanelDC::wxPanelDC (void)
{
  __type = wxTYPE_DC_PANEL;
  panel = NULL;
  device = wxDEVICE_PANEL;
}

wxPanelDC:: wxPanelDC (wxPanel * the_panel) :
            wxbPanelDC (the_panel)
{
  panel = the_panel;
  WXGC_IGNORE(panel);
  device = wxDEVICE_PANEL;
}

wxPanelDC::~wxPanelDC (void)
{
}

/*
 * Screen device context
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxScreenDC, wxCanvasDC)
 
wxScreenDC::wxScreenDC(void)
{
  display = wxGetDisplay();
  pixmap = RootWindow(display, DefaultScreen(display));
}

wxScreenDC::~wxScreenDC(void)
{
}

 /* Markus Emmenegger <mege@iqe.ethz.ch> */
 /* Find the pixel value with an assigned color closest to the desired color */
 /* Used if color cell allocation fails */
 /* As the returned pixel value may be in use by another application, the color might change anytime. */
 /* But in many cases, that is still better than always using black.*/
int wxGetBestMatchingPixel(Display *display,Colormap cmap,XColor * desiredColor)
{
   int numPixVals = XDisplayCells(display, DefaultScreen (display));
   double dist,mindist=1E100;
   int bestpixel = (int) BlackPixel (display, DefaultScreen (display));
   int pixelcount;
   for(pixelcount = 0;pixelcount<numPixVals;pixelcount++){
     XColor matching_color;
     matching_color.pixel = pixelcount;
     XQueryColor(display,cmap,&matching_color);
     dist = sqrt( 
 		((double)matching_color.red - desiredColor->red)* ((double)matching_color.red - desiredColor->red) +
		((double)matching_color.green - desiredColor->green) * ((double)matching_color.green - desiredColor->green) +
 		((double)matching_color.blue - desiredColor->blue)*((double)matching_color.blue - desiredColor->blue) 
 		);		    
     if(dist < mindist) {
       bestpixel = pixelcount;
       mindist = dist;
     }
   }
   return bestpixel;
 }

 
