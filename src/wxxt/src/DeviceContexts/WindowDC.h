/*								-*- C++ -*-
 *
 * Purpose: device context to draw drawables
 *          (windows and pixmaps, even if pixmaps are covered by wxMemoryDC)
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

#ifndef WindowDC_h
#define WindowDC_h

// wxWindowDC is applyable to ALL from wxWindow derived classes!
// wxCanvasDC and wxPanelDC are aliases for compatibility
#define wxCanvasDC wxWindowDC
#define wxPanelDC  wxWindowDC

#ifdef __GNUG__
#pragma interface
#endif

class wxBitmap;
class wxBrush;
class wxColour;
class wxColourMap;
class wxFont;
class wxIcon;
class wxList;
class wxPen;

#ifdef Have_X_Types
class wxWindowDC_Xinit {
public:
    Display* dpy;		// display of drawable
    Screen* scn;		// screen of drawable
    Drawable drawable;		// init to 0 if drawable is not created
    wxWindow *owner;
};
class wxWindowDC_Xintern { // X GDI data
public:
    GC           pen_gc, brush_gc, text_gc, bg_gc;
    Region       user_reg, expose_reg, current_reg;
    Display      *dpy;
    Screen       *scn;
    Drawable     drawable;
#ifdef WX_USE_XRENDER
    long picture; /* If WX_USE_XFT, picture is actually an XftDraw* */
#endif
    Window       draw_window;
    unsigned int width, height, depth;
    wxWindow     *owner;

#ifdef USE_GL
    wxGL *wx_gl;
#endif

    /* Implement GetPixel */
    XImage *get_pixel_image_cache;
    int get_pixel_cache_pos;
    XColor *get_pixel_color_cache;
    short get_pixel_cache_full, set_a_pixel;
    Bool         is_window;
    int cache_dx, cache_dy;
};
// easier access to private data
#define PEN_GC		(X->pen_gc)
#define BRUSH_GC	(X->brush_gc)
#define TEXT_GC		(X->text_gc)
#define BG_GC		(X->bg_gc)
#define USER_REG	(X->user_reg)
#define EXPOSE_REG	(X->expose_reg)
#define CURRENT_REG	(X->current_reg)
#define DPY		(X->dpy)
#define SCN		(X->scn)
#define DRAWABLE	(X->drawable)
#define DRAW_WINDOW	(X->draw_window)
#define WIDTH		(X->width)
#define HEIGHT		(X->height)
#define DEPTH		(X->depth)
#define CMAP            GETCOLORMAP(current_cmap)
# ifdef WX_USE_XFT
#  define XFTDRAW         ((XftDraw *)X->picture)
#  define TO_PICTURE(x)   XftDrawPicture((XftDraw *)x)
#  define PICTURE         TO_PICTURE(XFTDRAW)
# else
#  define TO_PICTURE(x)   ((Picture)x)
#  define PICTURE         ((Picture)X->picture)
# endif
#else // not implementation but use!
class wxWindowDC_Xinit;
class wxWindowDC_Xintern;
#endif

class wxWindowDC : public wxDC {
public:
    wxWindowDC(void);
    ~wxWindowDC(void);

    // virtual methods, declared in wxDC
    Bool  Blit(float xdest, float ydest, float w, float h, wxBitmap *bm,
	       float xsrc, float ysrc, int rop=wxSOLID, wxColour *c=NULL, wxBitmap *mask=NULL);
    Bool  GCBlit(float xdest, float ydest, float w, float h, wxBitmap *bm,
		 float xsrc, float ysrc, wxBitmap *mask=NULL);
    Bool  CanGetTextExtent(void) { return TRUE; }
    Bool  CanDrawBitmap(void) { return TRUE; }
    void  Clear(void);
    void  CrossHair(float x, float y);
    void  DrawArc(float x1, float y1, float x2, float y2, float xc, float yc);
    void  DrawEllipse(float x, float y, float w, float h);
    void  DrawLine(float x1, float y1, float x2, float y2);
    void  DrawLines(int n, wxPoint pts[], float xoff=0, float yoff=0);
    void  DrawLines(int n, wxIntPoint pts[], int xoff=0, int yoff=0);
    void  DrawLines(wxList *pts, float xoff=0, float yoff=0);
    void  DrawPoint(float x, float y);
    void  DrawPolygon(int n, wxPoint pts[], float xoff=0, float yoff=0,
			      int fill=wxODDEVEN_RULE);
    void  DrawPolygon(wxList *pts, float xoff=0, float yoff=0,
			      int fill=wxODDEVEN_RULE);
    void  DrawRectangle(float x, float y, float w, float h);
    void  DrawRoundedRectangle(float x, float y, float w, float h,
				       float radius=20);

    void  DrawText(char *text, float x, float y, Bool use16 = FALSE, int dt = 0, float angle = 0.0);
    void  FloodFill(float x, float y, wxColour *col,int style=wxFLOOD_SURFACE);
    float GetCharHeight(void);
    float GetCharWidth(void);
    void  GetTextExtent(const char *s, float *w, float *h, float *descent = 0,
			float *ext_leading = 0,	wxFont *font=NULL,
			Bool use16bit=FALSE, int dt=0);
    void  IntDrawLine(int x1, int y1, int x2, int y2);
    void  IntDrawLines(int n, wxIntPoint pts[], int xoff=0, int yoff=0);
    void  SetBackground(wxColour *c);
    void  SetBrush(wxBrush *brush);
    void  SetClippingRect(float x, float y, float w, float h);
    void  SetClippingRegion(wxRegion*);
    wxRegion* GetClippingRegion();
    void  SetColourMap(wxColourMap *cmap);
    void  SetFont(wxFont *font);
    void  SetPen(wxPen *pen);
    void  SetTextBackground(wxColour *col);
    void  SetTextForeground(wxColour *col);
    // methods unique to wxWindowDC
    void  SetCanvasClipping(void);

    virtual void GetSize(float *w, float *h);

    void TryColour(wxColour *src, wxColour *dest);

    Bool GetPixel(float x, float y, wxColour *col);

    void BeginSetPixel(int mini, int near_i, int near_j);
    void EndSetPixel();
    void SetPixel(float x, float y, wxColour *col);

    void FillPrivateColor(wxColour *c);

    virtual Bool Ok(void);

#ifdef WX_USE_XRENDER
    virtual void InitPicture();
#endif

#ifdef USE_GL
    virtual wxGL *GetGL();
#endif

protected:
    friend class wxWindow;
    friend class wxPostScriptDC;

    void  Initialize(wxWindowDC_Xinit* init);
    void  Destroy(void);

    /* Implement GetPixel */
    void DoFreeGetPixelCache(void);

    wxWindowDC_Xintern* X;
};

#ifdef WX_USE_XRENDER
# ifdef Have_X_Types
extern int wxXRenderHere(void);
extern long wxMakePicture(Drawable d, int color); // returns Picture or XftDraw*
extern void wxFreePicture(long);
# endif
#endif


#ifdef USE_GL
class wxGL : public wxObject {
public:
  wxGL();
  virtual ~wxGL();

  int Ok();

  void Reset(long d, Bool offscreen);

  long draw_to; /* really a Drawable */
  long GLctx; /* really a GLXContext */
  long glx_pm; /* really a GLXPixmap */

  void SwapBuffers(void);
  void ThisContextCurrent(void);
};
#endif

#endif // WindowDC_hh
