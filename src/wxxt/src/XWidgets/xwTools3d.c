/***********************************************************
Copyright 2004 PLT Scheme, Inc.
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
Copyright 1995 by Markus Holzem

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital, MIT, or Kaleb 
Keithley not be used in advertising or publicity pertaining to distribution 
of the software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * For documeantation see headerfile
 */

#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>

#include <xwTools3d.h>

#include "wxAllocColor.h"

#define Xaw3dDisplay(w) (XtIsWidget(w) ? XtDisplay(w) : XtDisplayOfObject(w))
#define Xaw3dScreen(w)  (XtIsWidget(w) ? XtScreen(w)  : XtScreenOfObject(w))

Pixel Xaw3dAllocPixel(
    Widget w,			/* widget, that needs the new color value */
    Pixel  bg,			/* background pixel of widget */
    float  contrast		/* contrast of new color to background */
)
{
    XColor   fg_color,
	     bg_color;
    Display  *dpy = Xaw3dDisplay(w);
    Screen   *scn = Xaw3dScreen(w);
    Colormap cmap = wx_default_colormap;

    if (bg==BlackPixelOfScreen(scn) || bg==WhitePixelOfScreen(scn)) {
	if (contrast>=1.0) 
	    contrast=2.0-contrast;
	fg_color.red = fg_color.green =	fg_color.blue 
	    = (unsigned short)(contrast*65535.0);
    } else {
#       define MIN(x,y) (unsigned short)(((x)<(y))?(x):(y))
	bg_color.pixel = bg;
	XQueryColor(dpy, cmap, &bg_color);
	fg_color.red   = MIN(65535, (int)(contrast*(float)bg_color.red));
	fg_color.green = MIN(65535, (int)(contrast*(float)bg_color.green));
	fg_color.blue  = MIN(65535, (int)(contrast*(float)bg_color.blue));
#       undef MIN
    }
    (void)wxAllocColor(dpy, cmap, &fg_color);
    return (fg_color.pixel);
}

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
          XAllocNamedColor(dpy,wx_default_colormap,"gray",&Gray,&d);}}
#   define WHITEGRAY (Gray.pixel ? Gray.pixel : WhitePixelOfScreen(scn))
#   define BLACKGRAY (Gray.pixel ? Gray.pixel : BlackPixelOfScreen(scn))
#   define GRAYMIX   {if (Gray.pixel) mix=GRAY;}
#else
#   define ALLOCGRAY {}
#   define WHITEGRAY (WhitePixelOfScreen(scn))
#   define BLACKGRAY (BlackPixelOfScreen(scn))
#   define GRAYMIX   {}
#endif

Pixmap Xaw3dAllocPixmap(
    Widget w,			/* widget, that needs the new color value */
    Pixel  bg,			/* background pixel of widget */
    MixType mix 		/* how to mix fore- and background */
)
{
    Display       *dpy = Xaw3dDisplay(w);
    Screen        *scn = Xaw3dScreen(w);
    Pixel         fg;
    char          *bits;
    unsigned int  size;
 
    ALLOCGRAY;
    if (DefaultDepthOfScreen(scn) == 1) {
	fg = BlackPixelOfScreen(scn);
	bg = WhitePixelOfScreen(scn);
    } else if (bg == WhitePixelOfScreen(scn)) {
	switch (mix) {
	case LIGHTER:       fg = BLACKGRAY; break;
	case DARKER:        fg = WHITEGRAY; bg = BlackPixelOfScreen(scn);break;
	case GRAY: default: fg = WHITEGRAY; bg = BLACKGRAY;
	}
	GRAYMIX;
    } else if (bg == BlackPixelOfScreen(scn)) {
	switch (mix) {
	case LIGHTER:       fg = WhitePixelOfScreen(scn); break;
	case DARKER:        fg = WHITEGRAY; break;
	case GRAY: default: fg = WHITEGRAY; bg = BLACKGRAY;
	}
	GRAYMIX;
    } else {
	switch (mix) {
	case LIGHTER:
	    fg = WhitePixelOfScreen(scn);
	    break;
	case DARKER:
	    fg = BlackPixelOfScreen(scn);
	    break;
	case GRAY: default:
	    fg = WHITEGRAY;
	    bg = BLACKGRAY;
	}
	mix = GRAY;
    }
    switch (mix) {
    case LIGHTER:       bits = light_bits; size = light_size; break;
    case DARKER:        bits = dark_bits;  size = dark_size;  break;
    case GRAY: default: bits = gray_bits;  size = gray_size;  break;
    }
    return(XCreatePixmapFromBitmapData(dpy,
				       RootWindowOfScreen (scn),
				       bits, size, size, fg, bg,
				       DefaultDepthOfScreen (scn)));
}

GC Xaw3dGetGC(
    Widget  w,			/* widget, that needs the new color value */
    Boolean be_nice_to_cmap,	/* True: GC uses pxmap, False: GC uses pixel */
    Pixmap  pxmap,		/* Pixmap for GC */
    Pixel   pixel		/* Pixel for GC */
)
{
    XtGCMask  valuemask;
    XGCValues values;
	
    if (be_nice_to_cmap || DefaultDepthOfScreen(Xaw3dScreen(w))==1) {
	valuemask         = GCTile | GCFillStyle;
	values.tile       = pxmap;
	values.fill_style = FillTiled;
    } else {
	valuemask         = GCForeground;
	values.foreground = pixel;
    }
    return (XtGetGC(w, valuemask, &values));
}

GC Xaw3dRecomputeGC(
    Widget  new,
    Boolean be_nice_to_cmap,
    Pixmap  new_pxmap,
    Pixel   new_pixel,
    Widget  old,
    Pixmap  old_pxmap,
    Pixel   old_pixel,
    GC      old_GC)
{
    if (be_nice_to_cmap)
	Xaw3dFreePixmap(old, old_pxmap);
    Xaw3dReleaseGC(old, old_GC);
    return (Xaw3dGetGC(new, be_nice_to_cmap, new_pxmap, new_pixel));
}

void Xaw3dDrawRectangle(
Display    *dpy,		/* Display for drawing */
Window     win,			/* Window for drawing */
GC         lightGC,		/* GC for light color */
GC         shadowGC,		/* GC for shadow color */
GC         backgroundGC,	/* GC for background color */
GC         fgGC,
int        x, int y,		/* upper left corner of rectangle */
unsigned   width, unsigned height,	/* width and height of rectangle */
int        thickness,		/* thickness of shadow */
ShadowType shadow_type		/* type of shadow */
)
{
    GC       topGC, botGC, tempGC;
    unsigned inner_thickness = 0, orig_thickness;
    XPoint   pt[6];

    switch (shadow_type) {
    case XAW3D_BACKGROUND:
	topGC = botGC = backgroundGC;
	break;
    case XAW3D_ETCHED_IN:
	inner_thickness = thickness/2;
	thickness      -= inner_thickness;
    case XAW3D_IN:
    case XAW3D_XED:
	topGC = shadowGC;
	botGC = lightGC;
	break;
    case XAW3D_ETCHED_OUT:
	inner_thickness = thickness/2;
	thickness      -= inner_thickness;
    case XAW3D_OUT:
    default:
	topGC = lightGC;
	botGC = shadowGC;
    }

    orig_thickness = thickness;

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
	    x += thickness; y += thickness;
	    width -= 2*thickness; height -= 2*thickness;
	    /* exchange top and bottom color */
	    tempGC = topGC; topGC = botGC; botGC = tempGC;
	    /* thickness of inner shadow and no further to draw */
	    thickness=inner_thickness; inner_thickness = 0;
	} else {
	    /* terminate loop, no further shadow to draw */
	    thickness = 0;
	}
    }

    if (shadow_type == XAW3D_XED) {
      thickness = orig_thickness;
      XDrawLine(dpy, win, fgGC, x+thickness, y+thickness, 
		x+width-thickness-1, y+height-thickness-1);
      XDrawLine(dpy, win, fgGC, x+thickness, y+height-thickness-1,
		x+width-thickness-1, y+thickness);
    }
}

void Xaw3dDrawLine(
Display    *dpy,		/* Display for drawing */
Window     win,			/* Window for drawing */
GC         lightGC,		/* GC for light color */
GC         shadowGC,		/* GC for shadow color */
GC         foregroundGC,	/* GC for foreground color */
int        x, int y,		/* upper left corner of line */
unsigned   length,		/* length of line */
int        thickness,		/* thickness of line */
Boolean    vertical,		/* shall line be drawn vertical? */
ShadowType shadow_type		/* type of shadow */
)
{
    GC       topGC, botGC;
    unsigned topThickness = 0;
    unsigned botThickness = 0;
    unsigned offset = 0;
    unsigned i;
    Boolean  dashed = FALSE;

    switch (shadow_type) {
    case XAW3D_NO_LINE: /* nothing to do */
	return; 
    case XAW3D_SINGLE_LINE_DASH:
	dashed = True;
    case XAW3D_SINGLE_LINE:
	topGC = botGC = foregroundGC;
	topThickness = 1;
	break;
    case XAW3D_DOUBLE_LINE_DASH:
	dashed = True;
    case XAW3D_DOUBLE_LINE:
	topGC = botGC = foregroundGC;
	topThickness = botThickness = 1;
	offset = 1;
	break;
    case XAW3D_ETCHED_OUT_DASH:
	dashed = True;
    case XAW3D_ETCHED_OUT:
	topGC = lightGC; botGC = shadowGC;
	topThickness = thickness/2; botThickness = thickness-topThickness;
	break;
    case XAW3D_ETCHED_IN_DASH:
	dashed = True;
    case XAW3D_ETCHED_IN:
    default:
	topGC = shadowGC; botGC = lightGC;
	topThickness = thickness/2; botThickness = thickness-topThickness;
    }
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

void Xaw3dDrawToggle(
Display    *dpy,		/* Display for drawing */
Window     win,			/* Window for drawing */
GC         lightGC,		/* GC for light color */
GC         shadowGC,		/* GC for shadow color */
GC         inGC,		/* GC for pushed/set toggle */
GC         outGC,		/* GC for released/unset toggle */
GC         fgGC,                /* GC for checkmark */
int        x, int y,		/* upper left corner */
unsigned   width,		/* width of toggle button */
int        thickness,		/* thickness of shadow */
Boolean    pushed		/* is toggle pushed(in) or released(out) */
)
{
  if (outGC) {
    XFillRectangle(dpy, win, /* pushed ? inGC : */ outGC,
		   x+thickness, y+thickness,
		   width-(2*thickness), width-(2*thickness));
  }
  Xaw3dDrawRectangle(dpy, win, lightGC, shadowGC, (GC)0, fgGC,
		     x, y, width, width, thickness,
		     pushed ? XAW3D_XED : XAW3D_OUT);
}

void Xaw3dDrawRadio(
Display    *dpy,		/* Display for drawing */
Window     win,			/* Window for drawing */
GC         lightGC,		/* GC for light color */
GC         shadowGC,		/* GC for shadow color */
GC         inGC,		/* GC for pushed/set radio */
GC         outGC,		/* GC for released/unset radio */
GC         fgGC,                /* GC for dont */
int        x, int y,		/* upper left corner */
unsigned   width,		/* width of radio button */
int        thickness,		/* thickness of shadow */
Boolean    pushed		/* is radio pushed(in) or released(out) */
)
{
    XPoint   pt[6];
    unsigned half;
    GC       topGC, botGC, plainGC;

    if (width & 0x01)
	width++;
    half=width/2;
    if (pushed) {
	topGC = shadowGC;
	botGC = lightGC;
	plainGC = inGC;
    } else {
	topGC = lightGC;
	botGC = shadowGC;
	plainGC = outGC;
    }
	
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
    /* bottom part of radio button */
    pt[0].x = x;                    pt[0].y = y+half;
    pt[1].x = x+half;               pt[1].y = y+width;
    pt[2].x = x+width;              pt[2].y = y+half;
    pt[3].x = x+width-thickness;    pt[3].y = y+half;
    pt[4].x = x+half;               pt[4].y = y+width-thickness;
    pt[5].x = x+thickness;          pt[5].y = y+half;
    XFillPolygon(dpy, win, botGC, pt, 6, Complex, CoordModeOrigin);
    /* top part of radio button */
    pt[1].x = x+half;               pt[1].y = y;
    pt[4].x = x+half;               pt[4].y = y+thickness;
    XFillPolygon(dpy, win, topGC, pt, 6, Complex, CoordModeOrigin);
    if (plainGC) {
      /* inner plain of radio button */
      pt[2].x = x+half;               pt[2].y = y+width-thickness;
      XFillPolygon(dpy, win, plainGC, pt+2, 4, Convex, CoordModeOrigin);
    }

#if 0
    if (pushed) {
      int i;
      pt[2].y -= 2;
      pt[3].x -= 2;
      pt[4].y += 2;
      pt[5].x += 2;
      XFillPolygon(dpy, win, fgGC, pt+2, 4, Convex, CoordModeOrigin);
    }
#endif
}

void Xaw3dDrawArrow(
Display    *dpy,		/* Display for drawing */
Window     win,			/* Window for drawing */
GC         lightGC,		/* GC for light color */
GC         shadowGC,		/* GC for shadow color */
GC         inGC,		/* GC for pushed arrow */
GC         outGC,		/* GC for released arrow */
int        x, int y,		/* upper left corner */
unsigned   width,		/* width of arrow */
int        thickness,		/* thickness of shadow */
ArrowType  arrow_type,		/* LEFT, RIGHT, UP or DOWN arrow */
Boolean    pushed		/* is radio pushed(in) or released(out) */
)
{
    XPoint   ptplain[3], pt[8];
    XPoint   *topPt,  *botPt;
    int      topNpts, botNpts;
    GC       topGC,   botGC,   plainGC;

    unsigned half        = width/2+(width&0x1);
    unsigned thickness30 = (unsigned)(1.732*thickness);	/* cotangent 30° */

    if (pushed) {
	topGC = shadowGC;
	botGC = lightGC;
	plainGC = inGC;
    } else {
	topGC = lightGC;
	botGC = shadowGC;
	plainGC = outGC;
    }
	
#   define DEC(num) ((num) > 0 ? ((num)-1) : (num))
    switch (arrow_type) {
    default: /* should never happen but sure is sure */
    case RIGHT:
	pt[0].x = pt[5].x = x+width;
	pt[1].x = pt[6].x = x+width-thickness30;
	pt[2].x = pt[7].x = x+thickness;
	pt[3].x = pt[4].x = x;
	pt[0].y = pt[1].y = pt[5].y = pt[6].y = y+half;
	pt[2].y = y+width-thickness30;
	pt[3].y = y+width;
	pt[4].y = DEC(y);
	pt[7].y = y+thickness30-1;
	topPt = pt+2; topNpts=6;
	botPt = pt;   botNpts=4;
	break;
    case LEFT:
	pt[0].x = pt[5].x = x;
	pt[1].x = pt[6].x = x+thickness30;
	pt[2].x = pt[7].x = x+width-thickness;
	pt[3].x = pt[4].x = x+width;
	pt[0].y = pt[1].y = pt[5].y = pt[6].y = y+half;
	pt[2].y = y+thickness30;
	pt[3].y = DEC(y);
	pt[4].y = y+width;
	pt[7].y = y+width-thickness30;
	topPt = pt;   topNpts=4;
	botPt = pt+2; botNpts=6;
	break;
    case UP:
	pt[0].x = pt[1].x = pt[5].x = pt[6].x = x+half;
	pt[2].x = x+thickness30;
	pt[3].x = DEC(x);
	pt[4].x = x+width;
	pt[7].x = x+width-thickness30;
	pt[0].y = pt[5].y = DEC(y);
	pt[1].y = pt[6].y = y+thickness30;
	pt[2].y = pt[7].y = y+width-thickness;
	pt[3].y = pt[4].y = y+width;
	topPt = pt;   topNpts=4;
	botPt = pt+2; botNpts=6;
	break;
    case DOWN:
	pt[0].x = pt[1].x = pt[5].x = pt[6].x = x+half;
	pt[2].x = x+width-thickness30;
	pt[3].x = x+width;
	pt[4].x = x;
	pt[7].x = x+thickness30;
	pt[0].y = pt[5].y = y+width;
	pt[1].y = pt[6].y = y+width-thickness30;
	pt[2].y = pt[7].y = y+thickness;
	pt[3].y = pt[4].y = y;
	topPt = pt+2; topNpts=6;
	botPt = pt;   botNpts=4;
    }
#   undef DEC

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
