/*
 * File:      wx_dc.cc
 * Purpose:     Device context implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_dc.cc,v 1.4 1994/08/14 21:28:43 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_dc.cc	1.2 5/9/94";

/* MATTHEW: [9] 
   About pens, brushes, and the autoSetting flag:

   Under X, pens and brushes control some of the same X drawing 
   parameters. Therefore, it is impossible to independently maintain
   the current pen and the current brush. Also, some settings depend
   on the current logical function. The current_fill, etc. instance
   variables remember state across the brush and pen.

   Since pens are used more than brushes, the autoSetting flag
   is used to indicate that a brush was recently used, and SetPen
   must be called to reinstall the current pen's parameters.
   If autoSetting includes 0x2, then the pens color may need
   to be set based on XOR.

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
  clipping = FALSE;

  current_reg = NULL;
  user_reg = NULL;
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
#ifdef wx_motif
  gcBacking = NULL;
#endif

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

  current_logical_function = wxCOPY;

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = wxWHITE_BRUSH;
  current_background_brush->Lock(1);

  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
  current_bk_mode = wxTRANSPARENT;

  Colour = wxColourDisplay ();

  SetFont(wxNORMAL_FONT);

  /* MATTHEW: [7] Implement GetPixel */
  get_pixel_image_cache = NULL;
}


wxCanvasDC:: wxCanvasDC (wxCanvas * the_canvas):wxbCanvasDC (the_canvas)
{
  __type = wxTYPE_DC_CANVAS;

  canvas = the_canvas;
  WXGC_IGNORE(canvas);
  clipping = FALSE;
  display = canvas->GetXDisplay();
  selected_pixmap = NULL;
  pixmap = XtWindow((Widget)canvas->handle); /* MATTHEW: [15] */
  XSetWindowColormap (display, pixmap, wxGetMainColormap(display));

  pixmapWidth = 0;
  pixmapHeight = 0;

  current_reg = NULL;
  user_reg = NULL;
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
#ifdef wx_motif
  gcBacking = XCreateGC (display, RootWindow (display,
					      DefaultScreen (display)),
	    GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth,
			 &gcvalues);
#endif

  background_pixel = (int) gcvalues.background;
  ok = TRUE;
  current_pen_width = -1;
  current_pen_join = -1;
  current_pen_cap = -1;
  current_pen_nb_dash = -1;
  current_pen_dash = NULL;
  current_logical_function = wxCOPY;
  current_stipple = NULL;
  current_style = -1;
  current_fill = -1;

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = wxWHITE_BRUSH;
  current_background_brush->Lock(1);

  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
  current_bk_mode = wxTRANSPARENT;
  Colour = wxColourDisplay ();
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
  if (current_background_brush) current_background_brush->Lock(-1);

  if (gc)
    XFreeGC (display, gc);
  gc = NULL;
#ifdef wx_motif
  if (gcBacking)
    XFreeGC (display, gcBacking);
  gcBacking = NULL;
#endif
}

/* MATTHEW: [7] Implement GetPixel */
void wxCanvasDC::DoFreeGetPixelCache(void)
{
  if (get_pixel_image_cache) {
    XDestroyImage(get_pixel_image_cache);
    get_pixel_image_cache = NULL;
    delete[] get_pixel_color_cache;
    get_pixel_color_cache = NULL;
  }
}

void wxCanvasDC:: SetCanvasClipping (void)
{
  if (current_reg)
    XDestroyRegion (current_reg);

  if (user_reg || onpaint_reg)
    current_reg = XCreateRegion ();
  else
    current_reg = NULL;

  if (onpaint_reg && user_reg)
    XIntersectRegion (onpaint_reg, user_reg, current_reg);
  else if (user_reg)
    XIntersectRegion (user_reg, user_reg, current_reg);
  else if (onpaint_reg)
    XIntersectRegion (onpaint_reg, onpaint_reg, current_reg);

  if (current_reg)
    {
      XSetRegion (display, gc, current_reg);
#ifdef wx_motif
// NO, see SetClippingRegion
//      if (canvas && canvas->is_retained)
//	XSetRegion (display, gcBacking, current_reg);
#endif
    }
  else
    {
      XSetClipMask (display, gc, None);
#ifdef wx_motif
//      if (canvas && canvas->is_retained)
//	XSetClipMask (display, gcBacking, None);
#endif
    }

}

void wxCanvasDC:: GetClippingBox (float *x, float *y, float *w, float *h)
{
  /* MATTHEW: [8] Remove WX_GC_CF */
  if (current_reg)
    {
      XRectangle r;
      XClipBox (current_reg, &r);
      *x = XDEV2LOG (r.x);
      *y = YDEV2LOG (r.y);
      *w = XDEV2LOGREL (r.width);
      *h = YDEV2LOGREL (r.height);
    }
  else
    *x = *y = *w = *h = 0;
}

void wxCanvasDC:: SetClippingRegion (float cx, float cy, float cw, float ch)
{
/****
old code, not using optimized Regions

        XRectangle rects[1];

        rects[0].x = XLOG2DEV(cx) - WX_GC_CF; 
	rects[0].y = YLOG2DEV(cy) - WX_GC_CF;
        rects[0].width = XLOG2DEVREL(cw); rects[0].height = YLOG2DEVREL(ch);
        XSetClipRectangles(display, gc, 0, 0, rects, 1, Unsorted);
#ifdef wx_motif
        if (canvas && canvas->is_retained)
	{
          rects[0].x = XLOG2DEV_2(cx) - WX_GC_CF; 
	  rects[0].y = YLOG2DEV_2(cy) - WX_GC_CF;
          rects[0].width = XLOG2DEVREL(cw); rects[0].height = YLOG2DEVREL(ch);
          XSetClipRectangles(display, gcBacking, 0, 0, rects, 1, Unsorted);
	}
#endif
****/

  if (cw < 0)
    cw = 0;
  if (ch < 0)
    ch = 0;

  /* MATTHEW: [8] Remove WX_GC_CF */
  if (user_reg)
    XDestroyRegion (user_reg);
  user_reg = XCreateRegion ();
  XRectangle r;
  r.x = XLOG2DEV (cx);
  r.y = YLOG2DEV (cy);
  r.width = XLOG2DEVREL(cw);
  r.height = YLOG2DEVREL(ch);
  XUnionRectWithRegion (&r, user_reg, user_reg);
  SetCanvasClipping ();

  // Needs to work differently for Pixmap: without this,
  // there's a nasty display bug. 8/12/94
#ifdef wx_motif
  if (canvas && canvas->is_retained)
  {
    XRectangle rects[1];
    rects[0].x = XLOG2DEV_2(cx); 
    rects[0].y = YLOG2DEV_2(cy);
    rects[0].width = XLOG2DEVREL(cw);
    rects[0].height = YLOG2DEVREL(ch);
    XSetClipRectangles(display, gcBacking, 0, 0, rects, 1, Unsorted);
  }
#endif
}

void wxCanvasDC:: DestroyClippingRegion (void)
{
/***
old code, not using optimized Regions

        XGCValues gc_val;
        gc_val.clip_mask = None;
        XChangeGC(display, gc, GCClipMask, &gc_val);
#ifdef wx_motif
        if (canvas && canvas->is_retained)
	{
          XChangeGC(display, gcBacking, GCClipMask, &gc_val);
	}
#endif
#ifdef wx_xview
        if (canvas && canvas->xrects)
          XSetClipRectangles(display, gc, 0, 0, canvas->xrects->rect_array,
                           canvas->xrects->count, Unsorted);
#endif
***/

  if (user_reg)
    XDestroyRegion (user_reg);
  user_reg = NULL;
  SetCanvasClipping ();

  // Bug fix for Motif backing pixmap, see comment above.
#ifdef wx_motif
  XGCValues gc_val;
  gc_val.clip_mask = None;
  if (canvas && canvas->is_retained)
    XChangeGC(display, gcBacking, GCClipMask, &gc_val);
#endif
}

/* MATTHEW: [8] */
void wxCanvasDC:: GetClippingRegion(float *x, float *y, float *w, float *h)
{
  if (!current_reg) {
    *x = *y = 0;
    *w = *h = -1;
  } else
    GetClippingBox(x, y, w, h);
}

void wxCanvasDC::GetSize(float *w, float *h)
{
  if (canvas) {
    int ww, hh;

    canvas->GetVirtualSize(&ww, &hh);
    *w = ww;
    *h = hh;

    /* MATTHEW: [7] Check canvas->scrolls_set_size */
    if (canvas->hScroll && canvas->scrolls_set_size)
      *w = canvas->hExtent;
    if (canvas->vScroll && canvas->scrolls_set_size)
      *h = canvas->vExtent;
  } else {
    *w = *h = 0;
  }
}

void wxCanvasDC:: Clear (void)
{
/*
   #ifdef wx_xview
   // If we're not in the middle of a paint event...
   if (!canvas->xrects || !(canvas && canvas->is_retained))
   XClearWindow(display, pixmap);
   #endif
 */
  int w, h;
  float fw, fh;

  GetSize(&fw, &fh);
  w = (int)fw;
  h = (int)fh;

  /* MATTHEW: [9] Save old brush */
  wxBrush *save_brush = current_brush;

  /* MATTHEW: [13] Implement GetPixel */
  FreeGetPixelCache();

  SetBrush (current_background_brush);
  XFillRectangle (display, pixmap, gc, 0, 0, w, h);
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XFillRectangle (display, canvas->backingPixmap, gcBacking, 0, 0, w, h);
#endif

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
  XDrawLine (display, pixmap, gc, 0, yy,
	     ww, yy);
  XDrawLine (display, pixmap, gc, xx, 0,
	     xx, hh);
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    {
      xx = XLOG2DEV_2 (x);
      yy = YLOG2DEV_2 (y);
      XDrawLine (display, canvas->backingPixmap, gcBacking,
		 0, yy,
		 ww, yy);
      XDrawLine (display, canvas->backingPixmap, gcBacking,
		 xx, 0,
		 xx, hh);
    }
#endif
}

void wxCanvasDC:: FloodFill (float x, float y, wxColour * col, int style)
{
}

Bool wxCanvasDC:: GetPixel (float x, float y, wxColour * col)
{
  int i, j;

  if (!pixmap)
    return FALSE;

  i = XLOG2DEV(x);
  j = YLOG2DEV(y);

  int w, h;
  if (canvas) {
    canvas->GetVirtualSize (&w, &h);
#ifdef wx_motif
    if (canvas->hScroll && canvas->scrolls_set_size)
      w = canvas->hExtent;
    if (canvas->vScroll && canvas->scrolls_set_size)
      h = canvas->vExtent;
    
    if (canvas && canvas->is_retained) {
      w = canvas->pixmapWidth;
      h = canvas->pixmapHeight;
    }
#endif
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

    get_pixel_image_cache = 
      XGetImage(display, pixmap, 0, 0, w, h, AllPlanes, ZPixmap);

    get_pixel_cache_pos = 0;
    get_pixel_cache_full = FALSE;
    get_pixel_color_cache = new XColor[NUM_GETPIX_CACHE_COLORS];

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
  if (canvas) {
    canvas->GetVirtualSize (&w, &h);
#ifdef wx_motif
    if (canvas->hScroll && canvas->scrolls_set_size)
      w = canvas->hExtent;
    if (canvas->vScroll && canvas->scrolls_set_size)
      h = canvas->vExtent;
    
    if (canvas && canvas->is_retained) {
      w = canvas->pixmapWidth;
      h = canvas->pixmapHeight;
    }
#endif
  } else {
    w = pixmapWidth;
    h = pixmapHeight;
  }

  get_pixel_image_cache = 
    XGetImage(display, pixmap, 0, 0, w, h, AllPlanes, ZPixmap);
  
  get_pixel_cache_pos = 0;
  get_pixel_cache_full = FALSE;
  get_pixel_color_cache = new XColor[NUM_GETPIX_CACHE_COLORS];
}

void wxCanvasDC::EndSetPixel()
{
  if (!get_pixel_image_cache)
    return;

  int w, h;
  w = get_pixel_image_cache->width;
  h = get_pixel_image_cache->height;

  XPutImage(display, pixmap, gc, get_pixel_image_cache, 0, 0, 0, 0, w, h);

  FreeGetPixelCache();
}

void wxCanvasDC::SetPixel(float x, float y, wxColour * col)
{
  int i, j;

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
  
  wxAllocColor(display, wxGetMainColormap(display), &xcol);

  pixel = xcol.pixel;

  get_pixel_color_cache[get_pixel_cache_pos].pixel = pixel;
  get_pixel_color_cache[get_pixel_cache_pos].red = red;
  get_pixel_color_cache[get_pixel_cache_pos].green = green;
  get_pixel_color_cache[get_pixel_cache_pos].blue = blue;

  if (++get_pixel_cache_pos >= NUM_GETPIX_CACHE_COLORS) {
    get_pixel_cache_pos = 0;
    get_pixel_cache_full = TRUE;
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
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XDrawLine (display, canvas->backingPixmap, gcBacking, 
	       XLOG2DEV_2(x1), YLOG2DEV_2(y1),
	       XLOG2DEV_2(x2), YLOG2DEV_2(y2));
#endif
  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
}

void wxCanvasDC:: DrawArc (float x1, float y1, float x2, float y2, float xc, float yc)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  int xx1 = XLOG2DEV (x1);
  int yy1 = YLOG2DEV (y1);
  int xx2 = XLOG2DEV (x2);
  int yy2 = YLOG2DEV (y2);
  int xxc = XLOG2DEV (xc);
  int yyc = YLOG2DEV (yc);
  int xxc_2 = XLOG2DEV_2 (xc);
  int yyc_2 = YLOG2DEV_2 (yc);

  double dx = xx1 - xxc;
  double dy = yy1 - yyc;
  double radius = sqrt (dx * dx + dy * dy);
  int r = (int) radius;

  double radius1, radius2;

  if (xx1 == xx2 && yy1 == yy2)
    {
      radius1 = 0.0;
      radius2 = 360.0;
    }
  else if (radius == 0.0)
    radius1 = radius2 = 0.0;
  else
    {
      if (xx1 - xxc == 0)
	if (yy1 - yyc < 0)
	  radius1 = 90.0;
	else
	  radius1 = -90.0;
      else
	radius1 = -atan2 ((double) (yy1 - yyc), (double) (xx1 - xxc)) * 360.0 / (2 * M_PI);

      if (xx2 - xxc == 0)
	if (yy2 - yyc < 0)
	  radius2 = 90.0;
	else
	  radius2 = -90.0;
      else
	radius2 = -atan2 ((double) (yy2 - yyc), (double) (xx2 - xxc)) * 360.0 / (2 * M_PI);
    }
  radius1 *= 64.0;
  radius2 *= 64.0;
  int alpha1 = (int) radius1;
  int alpha2 = (int) (radius2 - radius1);
  while (alpha2 <= 0)
    alpha2 += 360 * 64;
  while (alpha2 > 360 * 64)
    alpha2 -= 360 * 64;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      XFillArc (display, pixmap, gc,
		xxc - r, yyc - r, 2 * r, 2 * r, alpha1, alpha2);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XFillArc (display, canvas->backingPixmap, gcBacking,
		  xxc_2 - r, yyc_2 - r, 2 * r, 2 * r, alpha1, alpha2);
#endif
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawArc (display, pixmap, gc,
		xxc - r, yyc - r, 2 * r, 2 * r, alpha1, alpha2);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XDrawArc (display, canvas->backingPixmap, gcBacking,
		  xxc_2 - r, yyc_2 - r, 2 * r, 2 * r, alpha1, alpha2);
#endif
    }
  CalcBoundingBox (x1, y1);
  CalcBoundingBox (x2, y2);
}

void wxCanvasDC:: DrawPoint (float x, float y)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  if (current_pen && autoSetting)
    SetPen (current_pen);

  XDrawPoint (display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y));
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XDrawPoint (display, canvas->backingPixmap, gcBacking, XLOG2DEV_2 (x), YLOG2DEV_2 (y));
#endif
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
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	{
	  XSetFillRule (display, gcBacking,
		   fillStyle == wxODDEVEN_RULE ? EvenOddRule : WindingRule);
	  XFillPolygon (display, canvas->backingPixmap, gcBacking, xpoints2, n, Complex, 0);
	  XSetFillRule (display, gcBacking, EvenOddRule);	// default mode

	}
#endif
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawLines (display, pixmap, gc, xpoints1, n + 1, 0);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XDrawLines (display, canvas->backingPixmap, gcBacking, xpoints2, n + 1, 0);
#endif
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
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	{
	  for (i = 0; i < n; i++)
	    {
	      xpoints[i].x = XLOG2DEV_2 (points[i].x + xoffset);
	      xpoints[i].y = YLOG2DEV_2 (points[i].y + yoffset);
	    }
	  XDrawLines (display, canvas->backingPixmap, gcBacking, xpoints, n, 0);
	}
#endif
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

#ifdef wx_motif
      if (canvas && canvas->is_retained)
	{
	  for (i = 0; i < n; i++)
	    {
	      xpoints[i].x = XLOG2DEV_2 (points[i].x + xoffset);
	      xpoints[i].y = YLOG2DEV_2 (points[i].y + yoffset);
	    }
	  XDrawLines (display, canvas->backingPixmap, gcBacking, xpoints, n, 0);
	}
#endif
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
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XFillRectangle (display, canvas->backingPixmap, gcBacking, 
			XLOG2DEV_2 (x), YLOG2DEV_2 (y),
			wfd, hfd);
#endif
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawRectangle (display, pixmap, gc, xd, yd, wd, hd);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XDrawRectangle (display, canvas->backingPixmap, gcBacking, 
			XLOG2DEV_2 (x), YLOG2DEV_2 (y),
			wd, hd);
#endif
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

#ifdef wx_motif
  int phys2_x = XLOG2DEV_2 (x);
  int phys2_y = YLOG2DEV_2 (y);
  int phys2_radius = XLOG2DEVREL (radius);
  int phys2_width = XLOG2DEVREL (width) - WX_GC_CF;
  int phys2_height = YLOG2DEVREL (height) - WX_GC_CF;

  int phys2_rwidth = phys2_radius * 2;
  int phys2_rheight = phys2_rwidth;
#endif

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

#ifdef wx_motif
      if (canvas && canvas->is_retained)
	{
	  XFillRectangle (display, canvas->backingPixmap, gcBacking,
			  phys2_x + phys2_radius, phys2_y, phys2_width - phys2_rwidth, phys2_height);
	  XFillRectangle (display, canvas->backingPixmap, gcBacking,
			  phys2_x, phys2_y + phys2_radius, phys2_width, phys2_height - phys2_rheight);

	  XFillArc (display, canvas->backingPixmap, gcBacking,
	   phys2_x, phys2_y, phys2_rwidth, phys2_rheight, 90 * 64, 90 * 64);
	  XFillArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x + phys2_width - phys2_rwidth, phys2_y,
		    phys2_rwidth, phys2_rheight, 0, 90 * 64);
	  XFillArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x + phys2_width - phys2_rwidth,
		    phys2_y + phys2_height - phys2_rheight,
		    phys2_rwidth, phys2_rheight, 270 * 64, 90 * 64);
	  XFillArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x, phys2_y + phys2_height - phys2_rheight,
		    phys2_rwidth, phys2_rheight, 180 * 64, 90 * 64);
	}
#endif
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
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	{
	  XDrawLine (display, canvas->backingPixmap, gcBacking,
		     phys2_x + phys2_radius, phys2_y,
		     phys2_x + phys2_width - phys2_radius, phys2_y);
	  XDrawLine (display, canvas->backingPixmap, gcBacking,
		     phys2_x + phys2_radius, phys2_y + phys2_height,
	      phys2_x + phys2_width - phys2_radius, phys2_y + phys2_height);

	  XDrawLine (display, canvas->backingPixmap, gcBacking,
		     phys2_x, phys2_y + phys2_radius,
		     phys2_x, phys2_y + phys2_height - phys2_radius);
	  XDrawLine (display, canvas->backingPixmap, gcBacking,
		     phys2_x + phys2_width, phys2_y + phys2_radius,
	      phys2_x + phys2_width, phys2_y + phys2_height - phys2_radius);
	  XDrawArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x, phys2_y,
		    phys2_rwidth, phys2_rheight, 90 * 64, 90 * 64);
	  XDrawArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x + phys2_width - phys2_rwidth, phys2_y,
		    phys2_rwidth, phys2_rheight, 0, 90 * 64);
	  XDrawArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x + phys2_width - phys2_rwidth,
		    phys2_y + phys2_height - phys2_rheight,
		    phys2_rwidth, phys2_rheight, 270 * 64, 90 * 64);
	  XDrawArc (display, canvas->backingPixmap, gcBacking,
		    phys2_x, phys2_y + phys2_height - phys2_rheight,
		    phys2_rwidth, phys2_rheight, 180 * 64, 90 * 64);
	}
#endif
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
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XFillArc (display, canvas->backingPixmap, gcBacking, 
		  XLOG2DEV_2 (x), YLOG2DEV_2 (y),
		  XLOG2DEVREL (width) - WX_GC_CF,
		  YLOG2DEVREL (height) - WX_GC_CF, 0, angle);
#endif
    }

  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      if (autoSetting)
	SetPen (current_pen);
      XDrawArc (display, pixmap, gc, xd, yd, wd, hd, 0, angle);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XDrawArc (display, canvas->backingPixmap, gcBacking, 
		  XLOG2DEV_2 (x), YLOG2DEV_2 (y),
		  XLOG2DEVREL (width),
		  YLOG2DEVREL (height), 0, angle);
#endif
    }
  CalcBoundingBox (x, y);
  CalcBoundingBox (x + width, y + height);
}

/* Matthew Flatt: Used when copying between drawables on different displays.
   Not very fast, but better than giving up. */
#ifdef wx_motif
static void XCopyRemote(Display *srcdisplay, Display *destdisplay,
			Drawable src, Drawable dest,
			GC destgc,
			int srcx, int srcy,
			unsigned int w, unsigned int h,
			int destx, int desty,
			Bool more, XImage **cache)
{
  XImage *image, *destimage;
  Colormap destcm, srccm;
#define CACHE_SIZE 256
  unsigned int i, j;
  unsigned long cachesrc[CACHE_SIZE], cachedest[CACHE_SIZE];
  int k, cache_pos, all_cache;

  if (!cache || !*cache)
    image = XGetImage(srcdisplay, src, srcx, srcy, w, h, AllPlanes, ZPixmap);
  else
    image = *cache;

  destimage = XGetImage(destdisplay, dest, destx, desty, w, h, AllPlanes, ZPixmap);

  srccm = wxGetMainColormap(srcdisplay);
  destcm = wxGetMainColormap(destdisplay);

  cache_pos = 0;
  all_cache = FALSE;

  for (i = 0; i < w; i++)
    for (j = 0; j < h; j++) {
      unsigned long pixel;
      XColor xcol;

      pixel = XGetPixel(image, i, j);
      for (k = cache_pos; k--; )
	if (cachesrc[k] == pixel) {
	  pixel = cachedest[k];
	  goto install;
	}
      if (all_cache)
	for (k = CACHE_SIZE; k-- > cache_pos; )
	  if (cachesrc[k] == pixel) {
	    pixel = cachedest[k];
	    goto install;
	  }
      
      cachesrc[cache_pos] = xcol.pixel = pixel;
      XQueryColor(srcdisplay, srccm, &xcol);
      if (!wxAllocColor(destdisplay, destcm, &xcol))
	xcol.pixel = 0;
      cachedest[cache_pos] = pixel = xcol.pixel;
      
      if (++cache_pos >= CACHE_SIZE) {
	cache_pos = 0;
	all_cache = TRUE;
      }

    install:
      XPutPixel(destimage, i, j, pixel);
    }

  XPutImage(destdisplay, dest, destgc, destimage, 0, 0, destx, desty, w, h);
  XDestroyImage(destimage);

  if (more)
    *cache = image;
  else
    XDestroyImage(image);
}
#endif

void wxCanvasDC:: DrawIcon (wxIcon * icon, float x, float y)
{
  /* MATTHEW: [6] Safety */
  if (!icon->Ok())
    return;

  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  // Be sure that foreground pixels (1) of
  // the Icon will be painted with pen colour. [current_pen->SetColour()]
  // Background pixels (0) will be painted with 
  // last selected background color. [::SetBackground]
  if (current_pen && autoSetting)
    SetPen (current_pen);

  int width, height;
#ifdef wx_xview
  Pixmap iconPixmap = (Pixmap) xv_get (icon->x_image, SERVER_IMAGE_PIXMAP);
  width = (int) xv_get (icon->x_image, XV_WIDTH);
  height = (int) xv_get (icon->x_image, XV_HEIGHT);
#endif
#ifdef wx_motif
  Pixmap iconPixmap = icon->x_pixmap;
  width = icon->GetWidth();
  height = icon->GetHeight();
#endif
  /* MATTHEW: [4] Check display */
#ifdef wx_motif
  if (icon->display == display) {
#endif
    XCopyPlane (display, iconPixmap, pixmap, gc,
		0, 0, width, height, 
		(int) XLOG2DEV (x), (int) YLOG2DEV (y), 1);

/***
An other way to draw icons. Experimental.
XCopyPlane seems to be confused if logical function is not wxCOPY, so
we try to use stippling. Or is it me, who is confused?

  XSetStipple(display,gc,iconPixmap) ;
  XSetFillStyle(display,gc,FillOpaqueStippled) ;
  XSetTSOrigin(display,gc,(int)XLOG2DEV(x),(int)YLOG2DEV(y)) ;
  XFillRectangle(display,pixmap,gc,(int)XLOG2DEV(x),(int)YLOG2DEV(y),width,height) ;
  XSetFillStyle(display,gc,FillSolid) ;
***/

#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XCopyPlane (display, iconPixmap, canvas->backingPixmap, gcBacking,
	0, 0, width, height, (int) XLOG2DEV_2 (x), (int) YLOG2DEV_2 (y), 1);
#endif

#ifdef wx_motif
  } else { /* Remote copy (different displays) */
    XImage *cache = NULL;
    if (canvas && canvas->is_retained)
      XCopyRemote(icon->display, display, iconPixmap, canvas->backingPixmap, 
		  gcBacking, 0, 0, width, height, 
		  (int) XLOG2DEV_2 (x), (int) YLOG2DEV_2 (y), TRUE, &cache);
    XCopyRemote(icon->display, display, iconPixmap, pixmap, gc,
		0, 0, width, height, 
		(int) XLOG2DEV (x), (int) YLOG2DEV (y), FALSE, &cache);
  }
#endif
  CalcBoundingBox (x, y);
}

void wxCanvasDC:: SetFont (wxFont * the_font)
{
  font = the_font;
  if (!font)
    return;
#ifdef wx_xview
  // Set the font according to the current scaling
//  int scaled_size = (int) (user_scale_y * font->GetPointSize () + 0.5);

  /* MATTHEW: Provide display, use fontid, x10 */
  int scaled_size = (int) (10 * ((int)(user_scale_y * font->GetPointSize () + 0.5)));
  Xv_Font xfont = wxFontPool->FindNearestFont (font->GetFontId(), font->GetStyle (),
	     font->GetWeight (), scaled_size, font->GetUnderlined (), 0, 0,
					       display);
  font->x_font = xfont;
  Font theFont = (Font) xv_get (xfont, XV_XID);
#endif
#ifdef wx_motif
/*
   int res_x = (int)(DisplayWidth(dpy, screen)/(DisplayWidthMM(dpy, screen)/25.4));
   int res_y = (int)(DisplayHeight(dpy, screen)/(DisplayHeightMM(dpy, screen)/25.4));
 */
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
#endif
  if (gc)
    XSetFont(display, gc, theFont);
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XSetFont(display, gcBacking, theFont);
#endif
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

void wxCanvasDC:: SetPen (wxPen * pen)
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

  wxColour old_pen_colour = current_colour;
  current_colour = pen->GetColour ();
  current_style = pen->GetStyle ();
  current_fill = pen->GetStyle ();
  current_pen_width = pen->GetWidth ();
  current_pen_join = pen->GetJoin ();
  current_pen_cap = pen->GetCap ();
  current_pen_nb_dash = pen->nb_dash;
  current_pen_dash = pen->dash;
  if ((current_style == wxSTIPPLE) || (current_style == wxOPAQUE_STIPPLE)) {
    current_stipple = pen->GetStipple();
    if (current_stipple->Ok()) {
      int depth = current_stipple->GetDepth();
      if (depth == 1)
	is_bitmap = 1;
    } else
      current_stipple = NULL;
  } else
    current_stipple = NULL;

  Bool same_style = (old_style == current_style &&
		     old_fill == current_fill &&
		     old_pen_join == current_pen_join &&
		     old_pen_cap == current_pen_cap &&
		     old_pen_nb_dash == current_pen_nb_dash &&
		     old_pen_dash == current_pen_dash &&
		     old_pen_width == current_pen_width);

  Bool same_colour = ((!!current_stipple == !!old_stipple)
		      && (current_stipple
			  || (old_pen_colour.Ok () &&
			      (old_pen_colour.Red () == current_colour.Red ()) &&
			      (old_pen_colour.Blue () == current_colour.Blue ()) &&
			      (old_pen_colour.Green () == current_colour.Green ()) &&
			      (old_pen_colour.pixel == current_colour.pixel))));

  if (!same_style || !dcOptimize)
    {
      int scaled_width = (int) XLOG2DEVREL (pen->GetWidth ());
      if (scaled_width < 0)
	scaled_width = 0;

      int style;
      int join;
      int cap;
      static char dotted[] =
      {2, 5};
      static char short_dashed[] =
      {4, 4};
      static char long_dashed[] =
      {4, 8};
      static char dotted_dashed[] =
      {6, 6, 2, 6};

      // We express dash pattern in pen width unit, so we are
      // independent of zoom factor and so on...
      int req_nb_dash;
      char *req_dash;

      switch (pen->GetStyle ())
	{
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
	case wxSTIPPLE:
	case wxOPAQUE_STIPPLE:
	case wxSOLID:
	case wxTRANSPARENT:
	default:
	  style = LineSolid;
	  req_dash = NULL;
	  req_nb_dash = 0;
	}

      if (req_dash && req_nb_dash)
	{
	  char *real_req_dash = new char[req_nb_dash];
	  if (real_req_dash)
	    {
	      int factor = scaled_width == 0 ? 1 : scaled_width;
	      for (int i = 0; i < req_nb_dash; i++)
		real_req_dash[i] = req_dash[i] * factor;
	      XSetDashes (display, gc, 0, real_req_dash, req_nb_dash);
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetDashes (display, gcBacking, 0, real_req_dash, req_nb_dash);
#endif
	      delete[]real_req_dash;
	    }
	  else
	    {
	      // No Memory. We use non-scaled dash pattern...
	      XSetDashes (display, gc, 0, req_dash, req_nb_dash);
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetDashes (display, gcBacking, 0, req_dash, req_nb_dash);
#endif
	    }
	}

      switch (pen->GetCap ())
	{
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

      switch (pen->GetJoin ())
	{
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

      XSetLineAttributes (display, gc, scaled_width, style, cap, join);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XSetLineAttributes (display, gcBacking, scaled_width, style, cap, join);
#endif
    }

  if (IS_HATCH(current_fill) && ((current_fill != old_fill) || !dcOptimize))
    {
      Pixmap my_stipple;

      old_stipple = NULL;	// For later reset!!

      switch (current_fill)
	{
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
	default:
	  if (cdiag == (Pixmap) 0)
	    cdiag = XCreateBitmapFromData (display,
			      RootWindow (display, DefaultScreen (display)),
				     cdiag_bits, cdiag_width, cdiag_height);
	  my_stipple = cdiag;
	  break;
	}
      XSetStipple (display, gc, my_stipple);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XSetStipple (display, gcBacking, my_stipple);
#endif
    }
  else if (current_stipple
	   && ((current_stipple != old_stipple) || !dcOptimize))
    {
      if (is_bitmap) {
	XSetStipple (display, gc, current_stipple->x_pixmap);
#ifdef wx_motif
	if (canvas && canvas->is_retained)
	  XSetStipple (display, gcBacking, current_stipple->x_pixmap);
#endif
      } else {
	XSetTile(display, gc, current_stipple->x_pixmap);
#ifdef wx_motif
	if (canvas && canvas->is_retained)
	  XSetTile(display, gcBacking, current_stipple->x_pixmap);
#endif
      }
    }

  if ((current_fill != old_fill) || !dcOptimize)
    {
      int fill_style;

      if (current_fill == wxSTIPPLE)
	fill_style = (is_bitmap ? FillStippled : FillTiled);
      else if (current_fill == wxOPAQUE_STIPPLE)
	fill_style = (is_bitmap ? FillOpaqueStippled : FillTiled);
      else if (IS_HATCH (current_fill))
	fill_style = FillStippled;
      else
	fill_style = FillSolid;
      XSetFillStyle (display, gc, fill_style);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XSetFillStyle (display, gcBacking, fill_style);
#endif
    }

  // must test current_logical_function, because it involves background!
  if (!same_colour || !dcOptimize 
      || ((current_logical_function == wxXOR) || (autoSetting & 0x2)))
    {
      int pixel = -1;
      if (pen->GetStyle () == wxTRANSPARENT)
	pixel = background_pixel;
      else if (current_stipple)
	pixel = (int)BlackPixel(display, DefaultScreen(display));
      else if (!Colour)
	{
	  unsigned char red = pen->GetColour ().Red ();
	  unsigned char blue = pen->GetColour ().Blue ();
	  unsigned char green = pen->GetColour ().Green ();
	  if (red == (unsigned char) 255 && blue == (unsigned char) 255
	      && green == (unsigned char) 255)
	    {
	      pixel = (int) WhitePixel (display, DefaultScreen (display));
	      current_colour = *wxWHITE;
	      current_pen->GetColour().pixel = current_colour.pixel = pixel;
	    }
	  else
	    {
	      pixel = (int) BlackPixel (display, DefaultScreen (display));
	      current_colour = *wxBLACK;
	      current_pen->GetColour().pixel = current_colour.pixel = pixel;
	    }
	}
      else
	{
	  if (pen->GetColour ().pixel != -1)
	    pixel = pen->GetColour ().pixel;
	  else
	    {
	      XColor exact_def;
	      exact_def.red = (unsigned short) (((long) pen->GetColour ().Red ()) << SHIFT);
	      exact_def.green = (unsigned short) (((long) pen->GetColour ().Green ()) << SHIFT);
	      exact_def.blue = (unsigned short) (((long) pen->GetColour ().Blue ()) << SHIFT);
	      exact_def.flags = DoRed | DoGreen | DoBlue;

	      Colormap cmap = wxGetMainColormap(display);
	      if (!wxAllocColor (display, cmap, &exact_def)
		  && !alloc_close_color(display, cmap, &exact_def))
		{
//		  pixel = (int) BlackPixel (display, DefaultScreen (display));
		  ColourFailed();
                  pixel = wxGetBestMatchingPixel(display,cmap,&exact_def);
		}
	      else
		pixel = (int) exact_def.pixel;
	      current_colour.pixel = pen->GetColour().pixel = pixel;
	    }
	}

      // Finally, set the GC to the required colour
      if (pixel > -1)
	{
	  if (current_logical_function == wxXOR)
	    {
	      XGCValues values;
	      XGetGCValues (display, gc, GCBackground, &values);
	      XSetForeground (display, gc, pixel ^ values.background);
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetForeground (display, gcBacking, pixel ^ values.background);
#endif
	    }
	  else
	    {
	      XSetForeground (display, gc, pixel);
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetForeground (display, gcBacking, pixel);
#endif
	    }
	}
    }
  else
    pen->GetColour().pixel = old_pen_colour.pixel;

  /* MATTTHEW: [9] No longer need setting: */
  autoSetting = 0;
}

void wxCanvasDC:: SetBrush (wxBrush * brush)
{
  int old_fill = current_fill;
  wxBitmap *old_stipple = current_stipple;
  int is_bitmap = 0;

  if (current_brush) current_brush->Lock(-1);
  current_brush = brush;
  if (current_brush) current_brush->Lock(1);

  if (!brush || brush->GetStyle () == wxTRANSPARENT)
    return;

  /* MATTHEW: [9] Pen must be reset */
  autoSetting |= 0x1;

  current_fill = current_brush->GetStyle ();
  if ((current_fill == wxSTIPPLE) || (current_fill == wxOPAQUE_STIPPLE)) {
    current_stipple = current_brush->GetStipple();
    if (current_stipple->Ok()) {
      int depth = current_stipple->GetDepth();
      if (depth == 1)
	is_bitmap = 1;
    } else
      current_stipple = NULL;
  } else
    current_stipple = NULL;

  wxColour old_brush_colour(current_colour);
  current_colour = brush->GetColour ();
  Bool same_colour = ((!!current_stipple == !!old_stipple)
		      && (current_stipple
			  || (old_brush_colour.Ok () &&
			      (old_brush_colour.Red () == current_colour.Red ()) &&
			      (old_brush_colour.Blue () == current_colour.Blue ()) &&
			      (old_brush_colour.Green () == current_colour.Green ()) &&
			      (old_brush_colour.pixel == current_colour.pixel))));

  if ((old_fill != brush->GetStyle ()) || !dcOptimize)
    {
      switch (brush->GetStyle ())
	{
	case wxTRANSPARENT:
	  break;
	case wxBDIAGONAL_HATCH:
	case wxCROSSDIAG_HATCH:
	case wxFDIAGONAL_HATCH:
	case wxCROSS_HATCH:
	case wxHORIZONTAL_HATCH:
	case wxVERTICAL_HATCH:
	  XSetFillStyle (display, gc, FillStippled);
#ifdef wx_motif
	  if (canvas && canvas->is_retained)
	    XSetFillStyle (display, gcBacking, FillStippled);
#endif
	  break;
	case wxSTIPPLE:
	  XSetFillStyle (display, gc, is_bitmap ? FillStippled : FillTiled);
#ifdef wx_motif
	  if (canvas && canvas->is_retained)
	    XSetFillStyle (display, gcBacking, is_bitmap ? FillStippled : FillTiled);
#endif
	  break;
	case wxOPAQUE_STIPPLE:
	  XSetFillStyle (display, gc, is_bitmap ? FillOpaqueStippled : FillTiled);
#ifdef wx_motif
	  if (canvas && canvas->is_retained)
	    XSetFillStyle (display, gcBacking, is_bitmap ? FillOpaqueStippled : FillTiled);
#endif
	  break;
	case wxSOLID:
	default:
	  XSetFillStyle (display, gc, FillSolid);
#ifdef wx_motif
	  if (canvas && canvas->is_retained)
	    XSetFillStyle (display, gcBacking, FillSolid);
#endif
	}
    }

  if (IS_HATCH(current_fill) && ((current_fill != old_fill) || !dcOptimize))
    {
      Pixmap my_stipple;

      old_stipple = NULL;	// For later reset!!

      switch (current_fill)
	{
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
	default:
	  if (cdiag == (Pixmap) 0)
	    cdiag = XCreateBitmapFromData (display,
			      RootWindow (display, DefaultScreen (display)),
				     cdiag_bits, cdiag_width, cdiag_height);
	  my_stipple = cdiag;
	  break;
	}
      XSetStipple (display, gc, my_stipple);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XSetStipple (display, gcBacking, my_stipple);
#endif
    }
  // X can forget the stipple value when resizing a window (apparently)
  // so always set the stipple.
  else if (current_stipple) // && current_stipple != old_stipple)
    {
      if (is_bitmap) {
	XSetStipple (display, gc, current_stipple->x_pixmap);
#ifdef wx_motif
	if (canvas && canvas->is_retained)
	  XSetStipple (display, gcBacking, current_stipple->x_pixmap);
#endif
      } else {
	XSetTile(display, gc, current_stipple->x_pixmap);
#ifdef wx_motif
	if (canvas && canvas->is_retained)
	  XSetTile(display, gcBacking, current_stipple->x_pixmap);
#endif
      }
    }

  // must test current_logical_function, because it involves background!
  if (!same_colour || !dcOptimize || current_logical_function == wxXOR)
    {
      int pixel = -1;
      if (current_stipple)
	pixel = (int)BlackPixel(display, DefaultScreen(display));
      else if (!Colour)
	{
	  // Policy - on a monochrome screen, all brushes are white,
	  // except when they're REALLY black!!!
	  unsigned char red = brush->GetColour ().Red ();
	  unsigned char blue = brush->GetColour ().Blue ();
	  unsigned char green = brush->GetColour ().Green ();

	  if (red == (unsigned char) 0 && blue == (unsigned char) 0
	      && green == (unsigned char) 0)
	    {
	      pixel = (int) BlackPixel (display, DefaultScreen (display));
	      current_colour = *wxBLACK;
	      current_brush->GetColour().pixel = current_colour.pixel = pixel;
	    }
	  else
	    {
	      pixel = (int) WhitePixel (display, DefaultScreen (display));
	      current_colour = *wxWHITE;
	      current_brush->GetColour().pixel = current_colour.pixel = pixel;
	    }

	  // N.B. comment out the above line and uncomment the following lines
	  // if you want non-white colours to be black on a monochrome display.
	  /*
	     if (red == (unsigned char )255 && blue == (unsigned char)255
	     && green == (unsigned char)255)
	     pixel = (int)WhitePixel(display, DefaultScreen(display));
	     else
	     pixel = (int)BlackPixel(display, DefaultScreen(display));
	   */
	}
      else if (brush->GetStyle () != wxTRANSPARENT)
	{
	  if (brush->GetColour ().pixel > -1)
	    pixel = brush->GetColour ().pixel;
	  else
	    {
	      XColor exact_def;
	      exact_def.red = (unsigned short) (((long) brush->GetColour ().Red ()) << SHIFT);
	      exact_def.green = (unsigned short) (((long) brush->GetColour ().Green ()) << SHIFT);
	      exact_def.blue = (unsigned short) (((long) brush->GetColour ().Blue ()) << SHIFT);
	      exact_def.flags = DoRed | DoGreen | DoBlue;

	      Colormap cmap = wxGetMainColormap(display);
	      if (!wxAllocColor (display, cmap, &exact_def)
		  && !alloc_close_color(display, cmap, &exact_def))
		{
//		  pixel = (int) BlackPixel (display, DefaultScreen (display));
		  ColourFailed();
                  pixel = wxGetBestMatchingPixel(display,cmap,&exact_def);
		}
	      else
		pixel = (int) exact_def.pixel;
	      current_colour.pixel = brush->GetColour().pixel = pixel;
	    }
	}
      if (pixel > -1)
	{
	  // Finally, set the GC to the required colour
	  if (current_logical_function == wxXOR)
	    {
	      XGCValues values;
	      XGetGCValues (display, gc, GCBackground, &values);
	      XSetForeground (display, gc, pixel ^ values.background);
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetForeground (display, gcBacking, pixel ^ values.background);
#endif
	    }
	  else
	    {
	      XSetForeground (display, gc, pixel);
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetForeground (display, gcBacking, pixel);
#endif
	    }
	}
    }
  else
    brush->GetColour().pixel = old_brush_colour.pixel;
  //current_colour.pixel = brush->colour-pixel = old_brush_colour.pixel ;
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
  
  if (!wxAllocColor(display, cmap, &xcol))
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
#ifdef wx_xview
      XFontStruct *font_info = (XFontStruct *) xv_get (font->x_font, FONT_INFO);
#endif
#ifdef wx_motif
      XFontStruct *font_info = xfont; /* MATTHEW: [4] xfont */
#endif
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

		  if (!wxAllocColor (display, cmap, &exact_def)
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
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetForeground (display, gcBacking, pixel);
#endif
	    }
	}
      else
	current_text_background.pixel = old_pen_colour.pixel;

      XFillRectangle (display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y), cx, cy);
#ifdef wx_motif
      if (canvas && canvas->is_retained)
	XFillRectangle (display, canvas->backingPixmap, gcBacking,
			XLOG2DEV_2 (x), YLOG2DEV_2 (y), cx, cy);
#endif
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
	  if (!Colour)
	    {
	      // Unless foreground is really white, draw it in black
	      unsigned char red = current_text_foreground.Red ();
	      unsigned char blue = current_text_foreground.Blue ();
	      unsigned char green = current_text_foreground.Green ();
	      if (red == (unsigned char) 255 && blue == (unsigned char) 255
		  && green == (unsigned char) 255)
		{
		  pixel = (int) WhitePixel (display, DefaultScreen (display));
		  current_colour = *wxWHITE;
		  current_colour.pixel = current_text_foreground.pixel = pixel;
		}
	      else
		{
		  pixel = (int) BlackPixel (display, DefaultScreen (display));
		  current_colour = *wxBLACK;
		  current_colour.pixel = current_text_foreground.pixel = pixel;
		}
	    }
	  else
	    {
	      if (current_text_foreground.pixel > -1)
		pixel = current_text_foreground.pixel;
	      else
		{
		  XColor exact_def;
		  exact_def.red = (unsigned short) (((long) current_text_foreground.Red ()) << SHIFT);
		  exact_def.green = (unsigned short) (((long) current_text_foreground.Green ()) << SHIFT);
		  exact_def.blue = (unsigned short) (((long) current_text_foreground.Blue ()) << SHIFT);
		  exact_def.flags = DoRed | DoGreen | DoBlue;

//       Colormap cmap = DefaultColormap(display, DefaultScreen(display));
		  /* MATTHEW: [4] Use wxGetMainColormap */
		  Colormap cmap = wxGetMainColormap(display);

		  if (!wxAllocColor (display, cmap, &exact_def)
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
#ifdef wx_motif
	      if (canvas && canvas->is_retained)
		XSetForeground (display, gcBacking, pixel);
#endif
	    }
	}
      else
	current_text_foreground.pixel = old_pen_colour.pixel;
    }

  // We need to add the ascent, not the whole height, since X draws
  // at the point above the descender.
  /* MATTHEW: [2] handle 16-bit mode */
  if (use16Bit)
    XDrawString16(display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y) + ascent, 
		  (XChar2b *)text, slen);
  else
    XDrawString(display, pixmap, gc, XLOG2DEV (x), YLOG2DEV (y) + ascent, 
		text, slen);
#ifdef wx_motif
  if (canvas && canvas->is_retained) {
    if (use16Bit)
      XDrawString16(display, canvas->backingPixmap, gcBacking,
		    XLOG2DEV_2 (x), YLOG2DEV_2 (y) + ascent, 
		    (XChar2b *)text, slen);
    else
      XDrawString(display, canvas->backingPixmap, gcBacking,
		  XLOG2DEV_2 (x), YLOG2DEV_2 (y) + ascent, text, slen);      
  }

#endif
  float w, h;
  GetTextExtent (text, &w, &h);
  CalcBoundingBox (x + w, y + h);
  CalcBoundingBox (x, y);
}

void wxCanvasDC:: SetBackground (wxBrush * brush)
{
  int pixel;

  if (current_background_brush) current_background_brush->Lock(-1);
  current_background_brush = brush;
  if (current_background_brush) current_background_brush->Lock(1);

  if (!canvas)
    return;

  if (!brush)
    return;

  pixel = brush->GetColour ().pixel;

  if (current_background_brush)
    {
      if (current_background_brush->GetColour ().pixel > -1)
	pixel = current_background_brush->GetColour ().pixel;
      else
	{
	  XColor exact_def;
	  exact_def.red = (unsigned short) (((long) brush->GetColour ().Red ()) << SHIFT);
	  exact_def.green = (unsigned short) (((long) brush->GetColour ().Green ()) << SHIFT);
	  exact_def.blue = (unsigned short) (((long) brush->GetColour ().Blue ()) << SHIFT);
	  exact_def.flags = DoRed | DoGreen | DoBlue;

	  Colormap cmap = wxGetMainColormap(display);
	  if (!wxAllocColor (display, cmap, &exact_def)
	      && !alloc_close_color(display, cmap, &exact_def))
	    {
	      pixel = (int) WhitePixel (display, DefaultScreen (display));
	      ColourFailed();
	    }
	  else
	    pixel = (int) exact_def.pixel;
	  current_background_brush->GetColour().pixel = pixel;
	}


#ifdef wx_xview
      // Finally, set the background to the required colour
      Xv_Window pw = canvas_paint_window ((Canvas) (canvas->handle));

      Window win = (Window) xv_get (pw, XV_XID);
      XSetWindowBackground (display, win, pixel);
      // Necessary for ::DrawIcon, which use fg/bg pixel or the GC.
      // And Blit,... (Any fct that use XCopyPlane, in fact.)
      XSetBackground (display, gc, pixel);
#endif
#ifdef wx_motif
      XSetWindowBackground (display, pixmap, pixel);
/*
      if (canvas && canvas->is_retained)
	XSetWindowBackground (display, canvas->backingPixmap, pixel);
*/
      // Necessary for ::DrawIcon, which use fg/bg pixel or the GC.
      // And Blit,... (Any fct that use XCopyPlane, in fact.)
      XSetBackground (display, gc, pixel);
      if (canvas && canvas->is_retained)
	XSetBackground (display, gcBacking, pixel);
#endif
    }
}

void wxCanvasDC:: SetLogicalFunction (int function)
{
  int x_function;

  /* MATTHEW: [9] */
  if (current_logical_function == function)
    return;

  switch (function)
    {
    case wxCLEAR:
      x_function = GXclear;
      break;
    case wxXOR:
      x_function = GXxor;
      break;
    case wxINVERT:
      x_function = GXinvert;
      break;
    case wxOR_REVERSE:
      x_function = GXorReverse;
      break;
    case wxAND_REVERSE:
      x_function = GXandReverse;
      break;
    case wxAND:
      x_function = GXand;
      break;
    case wxOR:
      x_function = GXor;
      break;
    case wxAND_INVERT:
      x_function = GXandInverted;
      break;
    case wxNO_OP:
      x_function = GXnoop;
      break;
    case wxNOR:
      x_function = GXnor;
      break;
    case wxEQUIV:
      x_function = GXequiv;
      break;
    case wxSRC_INVERT:
      x_function = GXcopyInverted;
      break;
    case wxOR_INVERT:
      x_function = GXorInverted;
      break;
    case wxNAND:
      x_function = GXnand;
      break;
    case wxSET:
      x_function = GXset;
      break;
    case wxCOPY:
    default:
      x_function = GXcopy;
      break;
    }

  XSetFunction(display, gc, x_function);
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XSetFunction(display, gcBacking, x_function);
#endif

  if ((current_logical_function == wxXOR) != (function == wxXOR))
    /* MATTHEW: [9] Need to redo pen simply */
    autoSetting |= 0x2;

  current_logical_function = function;
}

Bool wxCanvasDC:: StartDoc (char *message)
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
#ifdef wx_xview
  if (!(font && font->x_font))
    return YDEV2LOGREL (12);

  return YDEV2LOGREL ((int) xv_get ((Xv_opaque) NULL, font->x_font, FONT_DEFAULT_CHAR_HEIGHT));
#endif
#ifdef wx_motif
  if (!xfont)
    return XDEV2LOGREL (12);

  int direction, ascent, descent;
  XCharStruct overall;
  XTextExtents (xfont, "x", 1, &direction, &ascent,
		&descent, &overall);
//  return XDEV2LOGREL(overall.ascent + overall.descent);
  return XDEV2LOGREL(ascent + descent);
#endif
}

float wxCanvasDC:: GetCharWidth (void)
{
#ifdef wx_xview
  if (!(font && font->x_font))
    return XDEV2LOGREL (16);

  return XDEV2LOGREL ((int) xv_get ((Xv_opaque) NULL, font->x_font, FONT_DEFAULT_CHAR_WIDTH));
#endif
#ifdef wx_motif
  if (!xfont)
    return XDEV2LOGREL (16);

  int direction, ascent, descent;
  XCharStruct overall;
  XTextExtents (xfont, "x", 1, &direction, &ascent,
		&descent, &overall);
  return XDEV2LOGREL(overall.width);
#endif
}

/* MATTHEW: This was broken, since it assumed that theFont has been setup to
   draw to this canvas. Only fixed for Motif, where we have to do display-specific
   work anyway. */
void wxCanvasDC:: GetTextExtent (const char *string, float *x, float *y,
	       float *descent, float *externalLeading, wxFont *theFont,
				 Bool use16Bit)
{
#ifdef wx_xview
   wxFont *fontToUse = theFont;
  if (!fontToUse)
    fontToUse = font;
#endif
#ifdef wx_motif
   wxFont *oldfont;
   XFontStruct *oldxfont;
#endif
  
#ifdef wx_motif
   oldfont = font;
   oldxfont = xfont;
   if (theFont)
     SetFont(theFont);
#endif

#ifdef wx_motif
  if (!xfont)
#endif
#ifdef wx_xview
    if (!(fontToUse && fontToUse->x_font))
#endif
      {
	cerr << "Warning: set a font before calling GetTextExtent!\n";
	*x = -1;
	*y = -1;
#ifdef wx_motif
	font = oldfont;
	xfont = oldxfont;
#endif
	return;
      }
#ifdef wx_motif
  XFontStruct *fontStruct = xfont;
#endif
#ifdef wx_xview
  XFontStruct *fontStruct = (XFontStruct *) xv_get (fontToUse->x_font, FONT_INFO);
#endif
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
#ifdef wx_xview
  Display *dpy = wxGetDisplay();
#endif
#ifdef wx_motif
  Display *dpy = display;
  if (!dpy && wxTheApp->wx_frame)
      dpy = wxGetDisplay();
#endif

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

Bool wxCanvasDC:: Blit (float xdest, float ydest, float width, float height,
      wxCanvasDC * source, float xsrc, float ysrc, int rop)
{
  /* MATTHEW: [7] Implement GetPixel */
  FreeGetPixelCache();

  Bool retval = FALSE;
  Bool resetPen;
  wxPen *savePen;
  wxBrush *saveBrush;

  saveBrush = current_background_brush;
  SetBackground(wxWHITE_BRUSH);

  if (rop == wxCOLOR) {
    // Be sure that foreground pixels (1) of
    // the Icon will be painted with pen colour. [current_pen->SetColour()]
    // Background pixels (0) will be painted with 
    // last selected background color. [::SetBackground]
    if (current_pen && autoSetting)
      SetPen(current_pen);
    savePen = NULL;
    resetPen = FALSE;
    rop = wxCOPY;
  } else {
    savePen = current_pen;
    SetPen(wxBLACK_PEN);
    resetPen = TRUE;
  }

  if (pixmap && source->pixmap)
    {
      /* MATTHEW: [9] */
      int orig = current_logical_function;

      SetLogicalFunction (rop);

      /* MATTHEW: [9] Need to use the current pen: */
      if (current_pen && autoSetting)
	SetPen(current_pen);

#ifdef wx_motif
      if (display != source->display)
      {
	XImage *cache = NULL;

	if (canvas && canvas->is_retained)
	  XCopyRemote(source->display, display, 
		      source->pixmap, canvas->backingPixmap, 
		      gcBacking,
		      source->LogicalToDeviceX (xsrc), 
		      source->LogicalToDeviceY (ysrc),
		      source->LogicalToDeviceXRel(width), 
		      source->LogicalToDeviceYRel(height),
		      XLOG2DEV_2 (xdest), YLOG2DEV_2 (ydest),
		      TRUE, &cache);

	XCopyRemote(source->display, display, source->pixmap, pixmap, gc,
		    source->LogicalToDeviceX (xsrc), 
		    source->LogicalToDeviceY (ysrc), 
		    source->LogicalToDeviceXRel(width), 
		    source->LogicalToDeviceYRel(height),
		    XLOG2DEV (xdest), YLOG2DEV (ydest), 
		    FALSE, &cache);
      } else {
      if (canvas && canvas->is_retained)
	{
	  XCopyArea (display, source->pixmap, canvas->backingPixmap, gcBacking,
		     source->LogicalToDeviceX (xsrc), 
		     source->LogicalToDeviceY (ysrc),
		     source->LogicalToDeviceXRel(width), 
		     source->LogicalToDeviceYRel(height),
		     XLOG2DEV_2 (xdest), YLOG2DEV_2 (ydest));
/*
   XCopyPlane(display, source->pixmap, canvas->backingPixmap, gcBacking,
   (int)source->LogicalToDeviceX(xsrc), (int)source->LogicalToDeviceY(ysrc), (int)width, (int)height,
   (int)XLOG2DEV_2(xdest), (int)YLOG2DEV_2(ydest), 1);
 */
	}
#endif
      // Check if we're copying from a mono bitmap
      if (source->selected_pixmap && (source->selected_pixmap->GetDepth () == 1))
	{
	  XCopyPlane (display, source->pixmap, pixmap, gc,
		      source->LogicalToDeviceX (xsrc), 
		      source->LogicalToDeviceY (ysrc), 
		      source->LogicalToDeviceXRel(width), 
		      source->LogicalToDeviceYRel(height),
		      XLOG2DEV (xdest), YLOG2DEV (ydest), 1);
	}
      else
	{
	  XCopyArea (display, source->pixmap, pixmap, gc,
		     source->LogicalToDeviceX (xsrc), 
		     source->LogicalToDeviceY (ysrc), 
		     source->LogicalToDeviceXRel(width), 
		     source->LogicalToDeviceYRel(height),
		     XLOG2DEV (xdest), YLOG2DEV (ydest));

	}
#ifdef wx_motif
    } /* Remote/local display */
#endif
      CalcBoundingBox (xdest, ydest);
      CalcBoundingBox (xdest + width, ydest + height);
    
      /* MATTHEW: [9] */
      SetLogicalFunction(orig);

      retval = TRUE;
    }

  SetBackground(saveBrush);
  if (resetPen) {
    current_pen = savePen;
    autoSetting = 1;
  }

  return retval;
}


/*
 * Memory DC
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxMemoryDC, wxCanvasDC)

wxMemoryDC::wxMemoryDC (void)
{
  __type = wxTYPE_DC_MEMORY;
  display = wxGetDisplay();

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

  current_logical_function = wxCOPY;
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
  current_background_brush = wxWHITE_BRUSH;
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

#ifdef wx_motif
  gcBacking = XCreateGC (display, RootWindow (display,
					      DefaultScreen (display)),
	    GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth,
			 &gcvalues);
#endif

  background_pixel = (int) gcvalues.background;
  ok = TRUE;
//  current_colour = NULL;
  selected_pixmap = NULL;

  Colour = wxColourDisplay ();
  SetBrush (wxWHITE_BRUSH);
  SetPen (wxBLACK_PEN);
  SetFont(wxNORMAL_FONT);
}

/*
 * Create a new dc from an old dc
 *
 */

wxMemoryDC:: wxMemoryDC (wxCanvasDC * old_dc):wxbMemoryDC (old_dc)
{
  __type = wxTYPE_DC_MEMORY;
  
  min_x = 0;
  min_y = 0;
  max_x = 0;
  max_y = 0;

  display = old_dc ? old_dc->display : wxGetDisplay();

  device = wxDEVICE_PIXMAP;

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

  ok = FALSE;
  title = NULL;

  current_logical_function = wxCOPY;
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

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = wxWHITE_BRUSH;
  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
  current_bk_mode = wxTRANSPARENT;

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

#ifdef wx_motif
  gcBacking = XCreateGC (display, RootWindow (display,
					      DefaultScreen (display)),
	    GCForeground | GCBackground | GCGraphicsExposures | GCLineWidth,
			 &gcvalues);
#endif

  background_pixel = (int) gcvalues.background;
  ok = TRUE;
//  current_colour = NULL;
  selected_pixmap = NULL;

  Colour = wxColourDisplay ();
  SetBrush (wxWHITE_BRUSH);
  SetPen (wxBLACK_PEN);
  SetFont(wxNORMAL_FONT);
}

wxMemoryDC::~wxMemoryDC (void)
{
  if (selected_pixmap) {
    selected_pixmap->selectedIntoDC = 0;
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

 /* MATTHEW: [4] Check display */
#ifdef wx_motif
  if (bitmap && (!bitmap->Ok() || bitmap->display != display || bitmap->selectedIntoDC))
    bitmap = NULL;
#endif

  if (selected_pixmap)
    selected_pixmap->selectedIntoDC = 0;

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
  bitmap->selectedIntoDC = -1;

  XGCValues gcvalues;
  gcvalues.foreground = BlackPixel(display, DefaultScreen(display));
  gcvalues.background = WhitePixel(display, DefaultScreen(display));
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

 
