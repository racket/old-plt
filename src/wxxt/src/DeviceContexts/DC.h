/*								-*- C++ -*-
 *
 * Purpose: basic device context
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004 PLT Scheme, Inc.
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

#ifndef MZ_PRECISE_GC
# ifdef __GNUG__
# pragma interface
# endif
#endif

/* for floor(): */
#include <math.h>

#ifdef USE_GL
class wxGL;
#endif

// wxPoint
class wxPoint : public wxObject {
public:
  inline wxPoint(void);
  inline wxPoint(float a, float b);
  float x, y;
};

inline wxPoint::wxPoint(void) 
: wxObject(WXGC_NO_CLEANUP)
{ 
  x = y = 0.0;
}

inline wxPoint::wxPoint(float a, float b) 
: wxObject(WXGC_NO_CLEANUP)
{
  x = a;
  y = b;
}

// wxIntPoint
class wxIntPoint : public wxObject {
public:
  inline wxIntPoint(void);
  inline wxIntPoint(int a, int b);
  int x, y;
};

inline wxIntPoint::wxIntPoint(void) 
: wxObject(WXGC_NO_CLEANUP)
{
  x = y = 0;
}

inline wxIntPoint::wxIntPoint(int a, int b)
: wxObject(WXGC_NO_CLEANUP)
{
  x = a;
  y = b;
}


class wxBitmap;
class wxBrush;
class wxCanvas;
class wxColour;
class wxColourMap;
class wxFont;
class wxList;
class wxPen;
class wxRegion;

class wxDC : public wxObject {
public:
    wxDC(void);

    // pure virtual methods, must be implemented for all DCs
    virtual Bool  Blit(float xdest, float ydest, float w, float h, wxBitmap *src,
		       float xsrc, float ysrc, int rop=wxSOLID, wxColour *c=NULL, wxBitmap *mask=NULL) = 0;
    virtual Bool  CanGetTextExtent(void) = 0;
    virtual Bool  CanDrawBitmap(void) = 0;
    virtual void  Clear(void) = 0;
    virtual void  CrossHair(float x, float y) = 0;
    virtual void  DrawArc(float x, float y, float w, float h, float start, float end) = 0;
    virtual void  DrawEllipse(float x, float y, float w, float h) = 0;
    virtual void  DrawLine(float x1, float y1, float x2, float y2) = 0;
    virtual void  DrawLines(int n, wxPoint pts[],
			    float xoff=0, float yoff=0) = 0;
    virtual void  DrawLines(int n, wxIntPoint pts[], int xoff=0, int yoff=0)=0;
    virtual void  DrawLines(wxList *pts, float xoff=0, float yoff=0) = 0;
    virtual void  DrawPoint(float x, float y) = 0;
            void  DrawPoint(wxPoint *pt)  { DrawPoint(pt->x, pt->y); }
    virtual void  DrawPolygon(int n, wxPoint pts[], float xoff=0, float yoff=0,
			      int fill=wxODDEVEN_RULE) = 0;
    virtual void  DrawPolygon(wxList *pts, float xoff=0, float yoff=0,
			      int fill=wxODDEVEN_RULE) = 0;
    virtual void  DrawRectangle(float x, float y, float w, float h) = 0;
    virtual void  DrawRoundedRectangle(float x, float y, float w, float h,
				       float radius=20) = 0;

    virtual void  DrawText(char *text, float x, float y,
			   Bool use16 = FALSE, int dt = 0, float angle = 0.0) = 0;
    virtual void  FloodFill(float x, float y, wxColour *col,
			    int style=wxFLOOD_SURFACE) = 0;
    virtual float GetCharHeight(void) = 0;
    virtual float GetCharWidth(void) = 0;
    virtual void  GetTextExtent(const char *s, float *w, float *h,
				float *descent = 0, float *ext_leading = 0,
				wxFont *font=NULL, Bool use16bit=FALSE, int dt=0) = 0;
    virtual void  IntDrawLine(int x1, int y1, int x2, int y2) = 0;
    virtual void  IntDrawLines(int n, wxIntPoint pts[],
			       int xoff=0, int yoff=0) = 0;
    virtual void  SetBackground(wxColour *c) = 0;
    virtual void  SetBrush(wxBrush *brush) = 0;
    virtual void  SetClippingRect(float x, float y, float w, float h) = 0;
    virtual void  SetClippingRegion(wxRegion *r) = 0;
    virtual wxRegion *GetClippingRegion() = 0;
    virtual void  SetColourMap(wxColourMap *cmap) = 0;
    virtual void  SetFont(wxFont *font) = 0;
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
    virtual float DeviceToLogicalX(int x)
	{ return XDEV2LOG(x); }
    virtual float DeviceToLogicalXRel(int x)
	{ return XDEV2LOGREL(x); }
    virtual float DeviceToLogicalY(int y)
	{ return YDEV2LOG(y); }
    virtual float DeviceToLogicalYRel(int y)
	{ return YDEV2LOGREL(y); }
    void  DrawSpline(int n, wxPoint pts[]);
    void  DrawSpline(wxList *pts);
    virtual void DrawSpline(float x1,float y1, float x2,float y2, float x3,float y3);
    void  EndDrawing(void)
	{}
    wxColour *GetBackground(void);
    wxBrush *GetBrush(void)
	{ return current_brush; }
    wxFont *GetFont(void)
	{ return current_font; }
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
    wxColour* GetTextBackground(void)
	{ return current_text_bg; }
    wxColour* GetTextForeground(void)
	{ return current_text_fg; }
    virtual int LogicalToDeviceX(float x)
	{ return XLOG2DEV(x); }
    virtual int LogicalToDeviceXRel(float x)
	{ return XLOG2DEVREL(x); }
    virtual int LogicalToDeviceY(float y)
	{ return YLOG2DEV(y); }
    virtual int LogicalToDeviceYRel(float y)
	{ return YLOG2DEVREL(y); }
    virtual float FLogicalToDeviceX(float x)
	{ return XLOG2DEV(x); }
    virtual float FLogicalToDeviceXRel(float x)
	{ return XLOG2DEVREL(x); }
    virtual float FLogicalToDeviceY(float y)
	{ return YLOG2DEV(y); }
    virtual float FLogicalToDeviceYRel(float y)
	{ return YLOG2DEVREL(y); }
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
    int GetBackgroundMode()
	{ return current_text_bgmode; }
    void SetOptimization(Bool opt)
	{ optimize = opt; }
    void SetTextAlignment(int new_alignment)
	{ current_text_alignment = new_alignment; }
    // scale and origin methods
    void  SetDeviceOrigin(float x, float y);
    void  SetLogicalScale(float xs, float ys);
    void  SetMapMode(int mode);
    virtual void  SetUserScale(float xs, float ys);

    void GetUserScale(float *xs, float *ys)
      { *xs = user_scale_x; *ys = user_scale_y; }
    void GetDeviceOrigin(float *x, float *y) 
      { *x = device_origin_x; *y = device_origin_y; }

#ifdef USE_GL
    virtual wxGL *GetGL();
#endif
  
    // public data members
    Bool  Colour;
    int   device;
protected:
    Bool  auto_setting, optimize, ok;
    // everything needed for sizing
    float mm_to_pix_x, mm_to_pix_y;
    float scale_x, scale_y;
    float device_origin_x, device_origin_y;
    float logical_scale_x, logical_scale_y, user_scale_x, user_scale_y;
    float max_x, max_y, min_x, min_y;
    // Tools for drawing
    wxColour*    current_background_color;
    wxBrush*     current_brush;
    wxColourMap* current_cmap;
    wxFont*      current_font;
    int          current_map_mode;
    wxPen*       current_pen;
    int		 current_text_alignment;
    wxColour*    current_text_bg;
    int		 current_text_bgmode;
    wxColour*    current_text_fg;
    wxRegion     *clipping;
    // utilities for internal use
    void  CalcBoundingBox(float x, float y);
    void  ComputeScaleAndOrigin(void);
    // abbreviations
    float XDEV2LOG(int x)
      { return (float(x) / scale_x) - device_origin_x; }
    float XDEV2LOGREL(int x)
      { return float(float(x) / scale_x); }
    float YDEV2LOG(int y)
      { return (float(y) / scale_y) - device_origin_y; }
    float YDEV2LOGREL(int y)
      { return float(float(y) / scale_y); }
    int XLOG2DEV(float x)
      { float a = (x * scale_x) + device_origin_x;
	return (int)floor(a); }
    int XLOG2DEVREL(float x)
      { float a = x * scale_x;
	return (int)floor(a); }
    int YLOG2DEV(float y)
      { float a = (y * scale_y) + device_origin_y;
	return (int)floor(a); }
    int YLOG2DEVREL(float y)
      { float a = y * scale_y;
	return (int)floor(a); }
    // virtual function for spline drawing
    virtual void DrawOpenSpline(wxList *pts);
};

#endif // DC_h
