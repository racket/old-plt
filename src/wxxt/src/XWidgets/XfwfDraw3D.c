/*								-*- C -*-
 * $Id: XfwfDraw3D.c,v 1.3 1996/06/16 13:45:45 markus Exp $
 *
 * Purpose: 3D drawing
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1996, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1996, GNU (Markus)
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

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#include "XfwfDraw3D.h"
#include "wxAllocColor.h"

#define USEGRAY

/*
 * Shadow pixmap data used for XfwfAllocPixmap. There are 3 types of
 * pixmaps to handle mono monitors
 */

/* shadow pixmap data */
#define dark_size 3
static char dark_bits[] = { 0x05, 0x03, 0x06};
#define light_size 3
static char light_bits[] = { 0x02, 0x04, 0x01};
#define gray_size 2
static char gray_bits[] = { 0x02, 0x01};
#ifdef USEGRAY
  static XColor Gray = { 0, 0, 0, 0, 0, 0 };
#   define ALLOCGRAY { \
        if (!Gray.pixel) { XColor d;\
          XAllocNamedColor(dpy,DefaultColormapOfScreen(scn),"gray",&Gray,&d);}}
#   define WHITEGRAY (Gray.pixel ? Gray.pixel : WhitePixelOfScreen(scn))
#   define BLACKGRAY (Gray.pixel ? Gray.pixel : BlackPixelOfScreen(scn))
#   define GRAYMIX   {if (Gray.pixel) mix=XfwfGray;}
#else
#   define ALLOCGRAY {}
#   define WHITEGRAY (WhitePixelOfScreen(scn))
#   define BLACKGRAY (BlackPixelOfScreen(scn))
#   define GRAYMIX   {}
#endif

/*
 * Allocate a pixel that has the 'contrast' to the base pixel.
 * If the allocation failes, returns FALSE.
 * If the base and the computed pixel are the same, use grey75
 */

static Boolean XfwfAllocPixel(Widget w, double contrast,
			      Pixel base, Pixel *result)
{
    Display  *dpy = XtDisplay(w);
    Screen   *scn = XtScreen(w);
    Colormap cmap = XtIsRealized(w) ? w->core.colormap : DefaultColormapOfScreen(scn);
    XColor   color, dummy;

    if (base==BlackPixelOfScreen(scn) || base==WhitePixelOfScreen(scn)) {
	/* adjust for black or white base */
	if (contrast>=1.0) 
	    contrast=2.0-contrast;
	color.red = color.green = color.blue = (unsigned short)(contrast*65535.0);
    } else {
	/* I have a colour base */
#       define MIN(x,y) (unsigned short)(((x)<(y))?(x):(y))
	color.pixel = base;
	XQueryColor(dpy, cmap, &color);
	color.red   = MIN(65535, (int)(contrast*(float)color.red));
	color.green = MIN(65535, (int)(contrast*(float)color.green));
	color.blue  = MIN(65535, (int)(contrast*(float)color.blue));
#       undef MIN
    }
    if (!wxAllocColor(dpy, cmap, &color))
	/* allocation failed */
	return False;
    if (base == color.pixel
    &&  !XAllocNamedColor(dpy, cmap, "gray75", &color, &dummy))
	/* there is no contrast */
	return False;
    /* return computed pixel */
    *result = color.pixel;
    return True;
}

/*
 * Allocate a shadow pixmap to use for shadow drawing.
 * There are certain adjustments for mono screens and
 * black or white backgrounds.
 */

static Pixmap XfwfAllocPixmap(Widget w, Pixel bg, XfwfMixType mix)
{
    Display       *dpy = XtDisplay(w);
    Screen        *scn = XtScreen(w);
    Pixel         fg;
    char          *bits;
    unsigned int  size;
 
    ALLOCGRAY; /* once I need a gray color */
    if (DefaultDepthOfScreen(scn) == 1) {
	fg = BlackPixelOfScreen(scn);
	bg = WhitePixelOfScreen(scn);
    } else if (bg == WhitePixelOfScreen(scn)) { /* background white */
	switch (mix) {
	case XfwfLighter:       fg = BLACKGRAY; break;
	case XfwfDarker:        fg = WHITEGRAY; bg = BlackPixelOfScreen(scn); break;
	case XfwfGray: default: fg = WHITEGRAY; bg = BLACKGRAY;
	}
	GRAYMIX;
    } else if (bg == BlackPixelOfScreen(scn)) { /* background black */
	switch (mix) {
	case XfwfLighter:       fg = WhitePixelOfScreen(scn); break;
	case XfwfDarker:        fg = WHITEGRAY; break;
	case XfwfGray: default: fg = WHITEGRAY; bg = BLACKGRAY;
	}
	GRAYMIX;
    } else { /* background coloured */
	switch (mix) {
	case XfwfLighter:        fg = WhitePixelOfScreen(scn); break;
	case XfwfDarker:	 fg = BlackPixelOfScreen(scn); break;
	case XfwfGray: default:  fg = WHITEGRAY; bg = BLACKGRAY;
	}
	mix = XfwfGray;
    }
    switch (mix) {
    case XfwfLighter:       bits = light_bits; size = light_size; break;
    case XfwfDarker:        bits = dark_bits;  size = dark_size;  break;
    case XfwfGray: default: bits = gray_bits;  size = gray_size;  break;
    }
    return(XCreatePixmapFromBitmapData(dpy,
				       RootWindowOfScreen (scn),
				       bits, size, size, fg, bg,
				       DefaultDepthOfScreen (scn)));
}

/*
 * Get a GC depending on the shadow scheme and if to be nice to the
 * colormap.
 */

GC XfwfGetShadowGC(Widget w, XfwfShadowScheme scheme, Boolean be_nice,
		   double contrast, XfwfMixType mix,
		   Pixel *fg, Pixel *bg, Pixmap *stipple)
{
    Screen    *scn      = XtScreen(w);
    XtGCMask  valuemask = 0;
    XGCValues values;

    switch (scheme) {
    case XfwfColor: /* use user specified color */
	valuemask         = GCForeground;
	values.foreground = *fg;
	break;
    case XfwfStipple:
	valuemask         = GCFillStyle | GCStipple | GCForeground | GCBackground;
	values.fill_style = FillOpaqueStippled;
	values.background = *bg;
	values.stipple    = *stipple;
	switch (mix) {
	case XfwfLighter:   values.foreground = WhitePixelOfScreen(scn); break;
	case XfwfDarker:    values.foreground = BlackPixelOfScreen(scn); break;
	default:            values.foreground = *bg;
	}
	break;
    case XfwfAuto:
	/* compute a color if more than vga color screen and not be nice to cmap */
	if ((!be_nice && DefaultDepthOfScreen(scn) > 4)
	&&  (XfwfAllocPixel(w, contrast, *bg, fg))) {
	    /* printf("%s: create shadow pixel\n", XtName(w)); */
	    *stipple          = (Pixmap)0;
	    valuemask         = GCForeground;
	    values.foreground = *fg;
	} else {
	    /* printf("%s: create shadow pixmap\n", XtName(w)); */
	    *stipple          = XfwfAllocPixmap(w, *bg, mix);
	    valuemask         = GCTile | GCFillStyle;
	    values.tile       = *stipple;
	    values.fill_style = FillTiled;
	}
    }
    return (XtGetGC(w, valuemask, &values));
}

/*
 * Frees the resources of the specified GC. XfwfGetShadowGC may create either
 * a pixmap or alloc a pixel. To behave well I have to free the pixmap/pixel
 * so that it may be reused.
 */

void XfwfFreeShadowGC(Widget w, XfwfShadowScheme scheme,
		      GC gc, Pixel *fg, Pixmap *stipple)
{
    Colormap cmap
	= ( XtIsRealized(w) ? w->core.colormap : DefaultColormapOfScreen(XtScreen(w)));

    switch (scheme) {
    case XfwfColor:    /* user specified color */
    case XfwfStipple:  /* user specified stipple */
	break; /* nothing to free */
    case XfwfAuto:
	if (*stipple) {
	    /* printf("%s: destroy shadow pixmap\n", XtName(w)); */
	    XFreePixmap(XtDisplay(w), *stipple);
	    *stipple = (Pixmap)0;
	} else {
	    /* printf("%s: destroy shadow pixel\n", XtName(w)); */
	    XFreeColors(XtDisplay(w), cmap, fg, 1, 0);
	}
    }
    XtReleaseGC(w, gc);
}

/*
 * Draw a rectangle depending on the shadow type.
 * it is possible to draw in fore- or background color too.
 */

void XfwfDrawRectangle(Widget w, Window win, GC light, GC dark, GC fg, GC bg,
		       int x, int y, int width, int height, int thickness,
		       XfwfShadowType type)
{
    Display    *dpy = XtDisplay(w);
    GC         topGC, botGC;
    unsigned   inner_thickness = 0;
    XPoint     pt[6];

    /* The widget has to be realized */
    if (!XtIsRealized(w))
	return;

    if (type == XfwfChiseled || type == XfwfLedged) {
	inner_thickness = thickness/2;
	thickness      -= inner_thickness;
    }
    switch (type) {
    case XfwfBackground:	topGC = botGC = bg; break;
    case XfwfForeground:	topGC = botGC = fg; break;
    case XfwfSunken:
    case XfwfChiseled:		topGC = dark; botGC = light; break;
    case XfwfRaised:
    case XfwfLedged:		topGC = light; botGC = dark; break;
    default:			return; /* do nothing */
    }
    /* The GCs must be defined */
    if (topGC == (GC)0 || botGC == (GC)0)
	return;
    /*
     * 1 shadow:  thickness == thickness,
     *            inner_thickness == 0
     * 2 shadows: thickness == thickness-inner_thickness
     *            inner_thickness != 0
     */
    while (thickness) {
       /* Points for shadows are numbered as follows:
	*
	*  0-------------------------1
	*  |                        /|     there are only
	*  |  3                    / |     the points 0 and 3
	*  |  |-------------------/  |     to change
	*  |  |                  2|  |     from top to bottom shadow
	*  |  |                   |  |
	*  |  |4                  |  |
	*  |  /-------------------|  |
	*  | /                    3' |
	*  |/                        |
	*  5-------------------------0'
	*/
	/* top-left shadow */
	pt[0].x = x;                 pt[0].y = y;
	pt[1].x = x+width;           pt[1].y = y;
	pt[2].x = x+width-thickness; pt[2].y = y+thickness;
	pt[3].x = x+thickness;       pt[3].y = y+thickness;
	pt[4].x = x+thickness;       pt[4].y = y+height-thickness ;
	pt[5].x = x;                 pt[5].y = y+height;
	XFillPolygon(dpy, win, topGC, pt, 6, Complex, CoordModeOrigin);
	/* bottom-right shadow */
	pt[0].x = x+width;           pt[0].y = y+height;
	pt[3].x = x+width-thickness; pt[3].y = y+height-thickness;
	XFillPolygon(dpy, win, botGC, pt, 6, Complex, CoordModeOrigin);
	/* an inner shadow to draw? */
	if (inner_thickness) {
	    /* exchange top and bottom color */
	    GC tempGC = topGC; topGC = botGC; botGC = tempGC;
	    /* thickness of inner shadow and no further to draw */
	    x     +=   thickness; y      +=   thickness;
	    width -= 2*thickness; height -= 2*thickness;
	    thickness=inner_thickness; inner_thickness = 0;
	} else {
	    /* terminate loop, no further shadow to draw */
	    thickness = 0;
	}
    }
}

/*
 * Draw a line depending on the shadow type.
 * it is possible to draw in fore- or background color too.
 */

void XfwfDrawLine(Widget w, Window win, GC light, GC dark, GC fg, GC bg,
		  int x, int y, int length, int thickness, Boolean vertical,
		  XfwfShadowType type, Boolean dashed, Boolean double_line)
{
    Display   *dpy = XtDisplay(w);
    GC        topGC, botGC;
    unsigned  i, topThickness = 0, botThickness = 0, offset = 0;

    /* The widget has to be realized */
    if (!XtIsRealized(w))
	return;

    switch (type) {
    case XfwfForeground:
	topGC = botGC = fg; topThickness = 1;
	if (double_line)    botThickness = offset = 1;
	break;
    case XfwfBackground:
	topGC = botGC = bg; topThickness = 1;
	if (double_line)    botThickness = offset = 1;
	break;
    case XfwfChiseled:
	topGC = dark; botGC = light;
	topThickness = thickness/2; botThickness = thickness-topThickness;
	break;
    case XfwfLedged:
	topGC = light; botGC = dark;
	topThickness = thickness/2; botThickness = thickness-topThickness;
	break;
    default:
	return;
    }
    /* The GCs must be defined */
    if (topGC == (GC)0 || botGC == (GC)0)
	return;
    if (dashed) { /* Change topGC and botGC to draw dashed lines  */
	XGCValues values;
	values.line_style = LineOnOffDash;
	if (topThickness > 0)
	    XChangeGC(dpy, topGC, GCLineStyle, &values);
	if (botThickness > 0 && botGC != topGC)
	    XChangeGC(dpy, botGC, GCLineStyle, &values);
    }
    for (i = 0; i < topThickness; i++) /* draw the line(s) */
	if (vertical) XDrawLine(dpy, win, topGC, x+i, y, x+i, y+length);
	else          XDrawLine(dpy, win, topGC, x, y+i, x+length, y+i);
    for (i = topThickness+offset; i < topThickness+botThickness+offset; i++)
	if (vertical) XDrawLine(dpy, win, botGC, x+i, y, x+i, y+length);
        else          XDrawLine(dpy, win, botGC, x, y+i, x+length, y+i);
    if (dashed) { /* changed GCs back to solid lines */
	XGCValues values;
	values.line_style = LineSolid;
	if (topThickness > 0)
	    XChangeGC(dpy, topGC, GCLineStyle, &values);
	if (botThickness > 0 && botGC != topGC)
	    XChangeGC(dpy, botGC, GCLineStyle, &values);
    }
}

void XfwfDrawSquare(Widget w, Window win, GC light, GC dark, GC in, GC out, GC border,
		    int x, int y, int width, int thickness, int bw, Boolean set)
{
    Display *dpy = XtDisplay(w);

    /* The widget has to be realized and the GCs must be defined */
    if (!XtIsRealized(w)
    ||  light == (GC)0 || dark == (GC)0 || in == (GC)0 || out == (GC)0)
	return;

    XFillRectangle(dpy, win, set ? in : out,
		   x+thickness, y+thickness,
		   width-(2*thickness), width-(2*thickness));
    XfwfDrawRectangle(w, win, light, dark, (GC)0, (GC)0,
		      x, y, width, width, thickness, set ? XfwfSunken : XfwfRaised);
    XfwfDrawRectangle(w, win, (GC)0, (GC)0, border, (GC)0,
		      x-bw, y-bw, width+2*bw, width+2*bw, bw, XfwfForeground);
}

void XfwfDrawDiamond(Widget w, Window win, GC light, GC dark, GC in, GC out, GC border,
		     int x, int y, int width, int thickness, int bw, Boolean set)
{
    Display *dpy = XtDisplay(w);

    XPoint   pt[6];
    unsigned half;
    GC       topGC, botGC, plainGC;

    /* The widget has to be realized and the GCs must be defined */
    if (!XtIsRealized(w)
    ||  light == (GC)0 || dark == (GC)0 || in == (GC)0 || out == (GC)0)
	return;

    half = ( width += width & 0x01 ) / 2;
    if (set)  { topGC = dark;  botGC = light; plainGC = in;  }
    else      { topGC = light; botGC = dark;  plainGC = out; }
	
    /* Points are numbered as follows:
     *
     *               1'
     *               /\
     *              /4'\
     *             / /\ \
     *           0/_/  \_\2
     *            \5\  /3/
     *             \ \/ /
     *              \ 4/   2''=4
     *               \/
     *                1
     */
    if (bw) {
	XfwfDrawRectangle(w, win, (GC)0, (GC)0, border, (GC)0,
			  x-bw, y-bw, width+2*bw, width+2*bw, bw, XfwfForeground);
    }
    /* bottom part of radio button (0 1 2 3 4 5) */
    pt[0].x = x;                    pt[0].y = y+half;
    pt[1].x = x+half;               pt[1].y = y+width;
    pt[2].x = x+width;              pt[2].y = y+half;
    pt[3].x = x+width-thickness;    pt[3].y = y+half;
    pt[4].x = x+half;               pt[4].y = y+width-thickness;
    pt[5].x = x+thickness;          pt[5].y = y+half;
    XFillPolygon(dpy, win, botGC, pt, 6, Complex, CoordModeOrigin);
    /* top part of border (0 1' 2 3 4' 5) */
    pt[1].x = x+half;               pt[1].y = y;
    pt[4].x = x+half;               pt[4].y = y+thickness;
    XFillPolygon(dpy, win, topGC, pt, 6, Complex, CoordModeOrigin);
    /* inner plain of radio button */
    pt[2].x = x+half;               pt[2].y = y+width-thickness;
    XFillPolygon(dpy, win, plainGC, pt+2, 4, Convex, CoordModeOrigin);
}

void XfwfDrawArrow(Widget w, Window win, GC light, GC dark, GC in, GC out,
		   int x, int y, int width, int height, int thickness,
		   XfwfArrowType type, Boolean set)
{
    Display *dpy = XtDisplay(w);

    XPoint   ptplain[3], pt[8];
    XPoint   *topPt=NULL,  *botPt=NULL;
    int      topNpts=0,    botNpts=0;
    GC       topGC,        botGC,
	     plainGC;
    unsigned top_off, base_off;

    /* The widget has to be realized and the GCs must be defined */
    if (!XtIsRealized(w)
    ||  light == (GC)0 || dark == (GC)0 || in == (GC)0 || out == (GC)0)
	return;

    /* compute which GCs to use */
    if (set)  { topGC = dark;  botGC = light; plainGC = in;  }
    else      { topGC = light; botGC = dark;  plainGC = out; }
	
    switch (type) {
    case XfwfArrowRight:
	top_off  = (1.0 + 0.83 * width/height) * thickness;
	base_off = (1.0 + 0.71 * width/height) * thickness;
	pt[0].x = pt[5].x = x + width;
	pt[1].x = pt[6].x = x + width - top_off;
	pt[2].x = pt[7].x = x + thickness;
	pt[3].x = pt[4].x = x;
	pt[0].y = pt[1].y = pt[5].y = pt[6].y = y + height/2;
	pt[2].y = y + height - base_off;
	pt[3].y = y + height;
	pt[4].y = y - 1;
	pt[7].y = y + base_off;
	topPt = pt+2; topNpts = 6;
	botPt = pt;   botNpts = 4;
	break;
    case XfwfArrowLeft:
	top_off  = (1.0 + 0.83 * width/height) * thickness;
	base_off = (1.0 + 0.71 * width/height) * thickness;
	pt[0].x = pt[5].x = x;
	pt[1].x = pt[6].x = x + top_off;
	pt[2].x = pt[7].x = x + width - thickness;
	pt[3].x = pt[4].x = x + width;
	pt[0].y = pt[1].y = pt[5].y = pt[6].y = y + height/2;
	pt[2].y = y + base_off;
	pt[3].y = y - 2;
	pt[4].y = y + height;
	pt[7].y = y + height - base_off;
	topPt = pt;   topNpts = 4;
	botPt = pt+2; botNpts = 6;
	break;
    case XfwfArrowUp:
	top_off  = (1.0 + 0.83 * height/width) * thickness;
	base_off = (1.0 + 0.71 * height/width) * thickness;
	pt[0].x = pt[1].x = pt[5].x = pt[6].x = x + width/2;
	pt[2].x = x + base_off;
	pt[3].x = x - 1;
	pt[4].x = x + width;
	pt[7].x = x + width - base_off;
	pt[0].y = pt[5].y = y - 1;
	pt[1].y = pt[6].y = y + top_off;
	pt[2].y = pt[7].y = y + height - thickness;
	pt[3].y = pt[4].y = y + height;
	topPt = pt;   topNpts = 4;
	botPt = pt+2; botNpts = 6;
	break;
    case XfwfArrowDown:
	top_off  = (1.0 + 0.83 * height/width) * thickness;
	base_off = (1.0 + 0.71 * height/width) * thickness;
	pt[0].x = pt[1].x = pt[5].x = pt[6].x = x + width/2;
	pt[2].x = x + width - base_off;
	pt[3].x = x + width;
	pt[4].x = x - 1;
	pt[7].x = x + base_off;
	pt[0].y = pt[5].y = y + height;
	pt[1].y = pt[6].y = y + height - top_off;
	pt[2].y = pt[7].y = y + thickness;
	pt[3].y = pt[4].y = y;
	topPt = pt+2; topNpts = 6;
	botPt = pt;   botNpts = 4;
    }

    /* area inside of the shadow triangle */
    ptplain[0] = pt[1];
    ptplain[1] = pt[2];
    ptplain[2] = pt[7];
    /* first draw inner area so that the borders have a nice edge */
    XFillPolygon(dpy, win, plainGC, ptplain, 3, Convex, CoordModeOrigin);
    /* draw bottom before top so that top casts the longer shadow */
    XFillPolygon(dpy, win, botGC, botPt, botNpts, Complex, CoordModeOrigin);
    XFillPolygon(dpy, win, topGC, topPt, topNpts, Complex, CoordModeOrigin);
}

