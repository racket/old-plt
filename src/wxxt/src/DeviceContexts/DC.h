/*								-*- C++ -*-
 * $Id: DC.h,v 1.2 1996/01/10 23:46:44 markus Exp $
 *
 * Purpose: basic device context
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

#ifndef DC_h
#define DC_h

#ifdef __GNUG__
#pragma interface
#endif

/* MATTHEW: for floor(): */
#include <math.h>

// wxPoint
class wxPoint : public wxObject {
DECLARE_DYNAMIC_CLASS(wxPoint)
public:
    wxPoint(void) : wxObject(WXGC_NO_CLEANUP) { x = y = 0.0; }
    /* MATTHEW: */
    wxPoint(float a, float b) : wxObject(WXGC_NO_CLEANUP) { x = a; y = b; }
    float x, y;
};

// wxIntPoint
class wxIntPoint : public wxObject {
DECLARE_DYNAMIC_CLASS(wxIntPoint)
public:
    wxIntPoint(void) : wxObject(WXGC_NO_CLEANUP) { x = y = 0; }
    /* MATTHEW: */
    wxIntPoint(int a, int b)  : wxObject(WXGC_NO_CLEANUP) { x = a; y = b; }
    int x, y;
};

#if 0
// wxRectangle
class wxRectangle : public wxObject {
DECLARE_DYNAMIC_CLASS(wxRectangle)
public:
    wxRectangle(void) : wxObject(WXGC_NO_CLEANUP) { x = y = width = height = 0; }
    int x, y, width, height;
};
#endif

class wxBitmap;
class wxBrush;
class wxCanvas;
class wxColour;
class wxColourMap;
class wxFont;
class wxIcon;
class wxList;
class wxPen;

class wxDC : public wxObject {
DECLARE_DYNAMIC_CLASS(wxDC)
public:
    wxDC(void);

    // pure virtual methods, must be implemented for all DCs
    virtual Bool  Blit(float xdest, float ydest, float w, float h, wxDC *src,
		       float xsrc, float ysrc, int rop=wxCOPY) = 0;
    virtual Bool  CanGetTextExtent(void) = 0;
    virtual Bool  CanDrawBitmap(void) = 0;
    virtual void  Clear(void) = 0;
    virtual void  CrossHair(float x, float y) = 0;
    virtual void  DestroyClippingRegion(void) = 0;
    virtual void  DrawArc(float x1, float y1, float x2, float y2,
			  float xc, float yc) = 0;
    virtual void  DrawEllipse(float x, float y, float w, float h) = 0;
    virtual void  DrawIcon(wxIcon *icon, float x, float y) = 0;
    virtual void  DrawLine(float x1, float y1, float x2, float y2) = 0;
    virtual void  DrawLines(int n, wxPoint pts[],
			    float xoff=0, float yoff=0) = 0;
    virtual void  DrawLines(int n, wxIntPoint pts[], int xoff=0, int yoff=0)=0;
    virtual void  DrawLines(wxList *pts, float xoff=0, float yoff=0) = 0;
    virtual void  DrawPoint(float x, float y) = 0;
            void  DrawPoint(wxPoint &pt)  { DrawPoint(pt.x, pt.y); }
    virtual void  DrawPolygon(int n, wxPoint pts[], float xoff=0, float yoff=0,
			      int fill=wxODDEVEN_RULE) = 0;
    virtual void  DrawPolygon(wxList *pts, float xoff=0, float yoff=0,
			      int fill=wxODDEVEN_RULE) = 0;
    virtual void  DrawRectangle(float x, float y, float w, float h) = 0;
    virtual void  DrawRoundedRectangle(float x, float y, float w, float h,
				       float radius=20) = 0;
    /* MATTHEW: */
    virtual void  DrawText(char *text, float x, float y,
			   Bool use16 = FALSE) = 0;
    virtual void  FloodFill(float x, float y, wxColour *col,
			    int style=wxFLOOD_SURFACE) = 0;
    virtual float GetCharHeight(void) = 0;
    virtual float GetCharWidth(void) = 0;
    virtual void  GetTextExtent(const char *s, float *w, float *h,
				float *descent = 0, float *ext_leading = 0,
				wxFont *font=NULL, Bool use16bit=FALSE) = 0;
    virtual void  IntDrawLine(int x1, int y1, int x2, int y2) = 0;
    virtual void  IntDrawLines(int n, wxIntPoint pts[],
			       int xoff=0, int yoff=0) = 0;
    virtual void  SetBackground(wxBrush *brush) = 0;
    virtual void  SetBrush(wxBrush *brush) = 0;
    virtual void  SetClippingRegion(float x, float y, float w, float h) = 0;
    /* MATTHEW: */
    virtual void  GetClippingRegion(float *x, float *y, float *w, float *h) = 0;
    virtual void  SetColourMap(wxColourMap *cmap) = 0;
    virtual void  SetFont(wxFont *font) = 0;
    virtual void  SetLogicalFunction(int fkt) = 0;
    virtual void  SetPen(wxPen *pen) = 0;
    virtual void  SetTextBackground(wxColour *col) = 0;
    virtual void  SetTextForeground(wxColour *col) = 0;

    /* MATTHEW */
    virtual void TryColour(wxColour *src, wxColour *dest) = 0;

    // only necessary for printing
    virtual Bool  StartDoc(char *WXUNUSED(message)) { return TRUE; }
    virtual void  EndDoc(void) {}
    virtual void  StartPage(void) {}
    virtual void  EndPage(void) {}

    // non virtual methods, same in all subclasses
    void AutoSetTools(Bool set_auto)
	{ auto_setting = set_auto; }
    void BeginDrawing(void)
	{}
    float DeviceToLogicalX(int x)
	{ return float( int( (float(x) + origin_x) / scale_x + 0.5) ); }
    float DeviceToLogicalXRel(int x)
	{ return float( int( float(x) / scale_x + 0.5) ); }
    float DeviceToLogicalY(int y)
	{ return float( int( (float(y) + origin_y) / scale_y + 0.5) ); }
    float DeviceToLogicalYRel(int y)
	{ return float( int( float(y) / scale_y + 0.5) ); }
#if USE_SPLINES
    void  DrawSpline(int n, wxPoint pts[]);
    void  DrawSpline(wxList *pts);
    void  DrawSpline(float x1,float y1, float x2,float y2, float x3,float y3);
#endif
    void  EndDrawing(void)
	{}
    wxBrush *GetBackground(void)
	{ return current_background_brush; }
    wxBrush *GetBrush(void)
	{ return current_brush; }
    wxFont *GetFont(void)
	{ return current_font; }
    int GetLogicalFunction(void)
	{ return current_logical_fkt; }
    int GetMapMode(void)
	{ return current_map_mode; }
    Bool GetOptimization(void)
	{ return optimize; }
    wxPen *GetPen(void)
	{ return current_pen; }
    Bool GetPixel(float WXUNUSED(x), float WXUNUSED(y),
		  wxColour *WXUNUSED(col))
	{ return FALSE; }
    /* MATTHEW */
    virtual void GetSize(float *w, float *h)
	{ *w = (float)(max_x-min_x); *h = (float)(max_y-min_y); }
    void GetSizeMM(float *w, float *h)
	{ GetSize(w, h); *w/=(scale_x*mm_to_pix_x); *h/=(scale_y*mm_to_pix_y); }
    int GetTextAlignment(void)
	{ return current_text_alignment; }
    wxColour& GetTextBackground(void)
	{ return current_text_bg; }
    wxColour& GetTextForeground(void)
	{ return current_text_fg; }
    int LogicalToDeviceX(float x)
	{ return int( (float(x) - origin_x) * scale_x + 0.5); }
    int LogicalToDeviceXRel(float x)
	{ return int(float(x) * scale_x + 0.5); }
    int LogicalToDeviceY(float y)
	{ return int( (float(y) - origin_y) * scale_y + 0.5); }
    int LogicalToDeviceYRel(float y)
	{ return int(float(y) * scale_y + 0.5); }
    float MaxX(void)
	{ return max_x; }
    float MaxY(void)
	{ return max_y; }
    float MinX(void)
	{ return min_x; }
    float MinY(void)
	{ return min_y; }
    /* MATTHEW: */
    virtual Bool Ok(void)
	{ return ok; }
    void SetBackgroundMode(int mode)
	{ current_text_bgmode = mode; }
    void SetOptimization(Bool opt)
	{ optimize = opt; }
    void SetTextAlignment(int new_alignment)
	{ current_text_alignment = new_alignment; }
    // scale and origin methods
    void  SetDeviceOrigin(float x, float y);
    void  SetLogicalOrigin(float x, float y);
    void  SetLogicalScale(float xs, float ys);
    void  SetMapMode(int mode);
    void  SetUserScale(float xs, float ys);
    // public data members
    Bool  Colour;
    int   device;
protected:
    Bool  auto_setting, optimize, ok;
    // everything needed for sizing
    float mm_to_pix_x, mm_to_pix_y;
    float origin_x, origin_y, scale_x, scale_y;
    float logical_origin_x, logical_origin_y, device_origin_x, device_origin_y;
    float logical_scale_x, logical_scale_y, user_scale_x, user_scale_y;
    float max_x, max_y, min_x, min_y;
    // Tools for drawing
    wxBrush*     current_background_brush;
    wxBrush*     current_brush;
    wxColourMap* current_cmap;
    wxFont*      current_font;
    int          current_logical_fkt;
    int          current_map_mode;
    wxPen*       current_pen;
    int		 current_text_alignment;
    wxColour     current_text_bg;
    int		 current_text_bgmode;
    wxColour     current_text_fg;
    // utilities for internal use
    void  CalcBoundingBox(float x, float y);
    void  ComputeScaleAndOrigin(void);
    // abbreviations
    /* MATTHEW: fixed all abbrevs */
    float XDEV2LOG(int x)
	{ return float((float(x) + origin_x) / scale_x); }
    float XDEV2LOGREL(int x)
	{ return float(float(x) / scale_x); }
    float YDEV2LOG(int y)
	{ return float((float(y) + origin_y) / scale_y); }
    float YDEV2LOGREL(int y)
	{ return float(float(y) / scale_y); }
    int XLOG2DEV(float x)
	{ return int(floor((float(x) - origin_x) * scale_x)); }
    int XLOG2DEVREL(float x)
	{ return int(floor(float(x) * scale_x)); }
    int YLOG2DEV(float y)
	{ return int(floor((float(y) - origin_y) * scale_y)); }
    int YLOG2DEVREL(float y)
	{ return int(floor(float(y) * scale_y)); }
#if USE_SPLINES
    // virtual function for spline drawing
    virtual void DrawOpenSpline(wxList *pts);
#endif
};

#endif // DC_h
