/*								-*- C -*-
 * $Id: XfwfDraw3D.h,v 1.3 1996/06/16 13:45:45 markus Exp $
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

#ifndef XfwfDraw3D_h
#define XfwfDraw3D_h

/* the shadow scheme to specify which way the GC should be allocated */
typedef enum {
    XfwfAuto, XfwfColor, XfwfStipple,
} XfwfShadowScheme;

/* How to mix back- and foreground for a stippled shadow GCs */
typedef enum {
    XfwfLighter=1, XfwfDarker, XfwfGray, XfwfGrey=XfwfGray
} XfwfMixType;

/* Of which type shall the shadow be? */
typedef enum {
    XfwfNone            = 0,    XfwfNoFrame     = 0,
    XfwfBackground	= 1,	XfwfForeground	= 2,
    XfwfRaised		= 3,	XfwfSunken	= 4,
    XfwfChiseled	= 5, 	XfwfLedged	= 6,
} XfwfShadowType;

/* direction to which the arrow shall point */
typedef enum {
    XfwfArrowLeft, XfwfArrowRight, XfwfArrowUp, XfwfArrowDown,
} XfwfArrowType;

/* gets GC for the specified widget */
GC XfwfGetShadowGC(Widget w, XfwfShadowScheme scheme, Boolean be_nice,
		   double contrast, XfwfMixType mix,
		   Pixel *fg, Pixel *bg, Pixmap *stipple);

/* frees the resources of the specified GC */
void XfwfFreeShadowGC(Widget w, XfwfShadowScheme scheme,
		      GC gc, Pixel *fg, Pixmap *stipple);

/* routines to draw with a 3D look */
void XfwfDrawRectangle(Widget w, Window win, GC light, GC dark, GC fg, GC bg,
		       int x, int y, int width, int height, int thickness,
		       XfwfShadowType type);

void XfwfDrawLine(Widget w, Window win, GC light, GC dark, GC fg, GC bg,
		  int x, int y, int length, int thickness, Boolean vertical,
		  XfwfShadowType type, Boolean dashed, Boolean double_line);

void XfwfDrawSquare(Widget w, Window win, GC light, GC dark, GC in, GC out, GC border,
		    int x, int y, int width, int thickness, int bw, Boolean set);
		    
void XfwfDrawDiamond(Widget w, Window win, GC light, GC dark, GC in, GC out, GC border,
		     int x, int y, int width, int thickness, int bw, Boolean set);

void XfwfDrawArrow(Widget w, Window win, GC light, GC dark, GC in, GC out,
		   int x, int y, int width, int height, int thickness,
		   XfwfArrowType type, Boolean set);

#endif /* XfwfDraw3D_h */
