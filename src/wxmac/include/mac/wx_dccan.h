///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan.h
// Purpose:	Canvas device context declaration (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dccanh
#define wx_dccanh

#include "wx_gdi.h"
#include "wb_dccan.h"

#ifdef IN_CPROTO
typedef       void* wxCanvasDC ;
#else

class wxCanvas;
class wxCanvasDC: public wxbCanvasDC
{
 protected:
  Bool cMacDoingDrawing;  // mac platform only; internal use only; used by Begin/EndDrawing
 public:
  wxCanvas* canvas;
  wxGL *gl;
  // Every time a callback happens, these are set to point to the right values
  // for drawing calls to work

  int dc_set_depth;
  int paint_brush_with_erase; // used for wxPANEL_PATTERN brushes

  int pixmapWidth;
  int pixmapHeight;

  RgnHandle current_reg, onpaint_reg;

  int current_pen_join;
  int current_pen_cap;
  int current_pen_nb_dash;
  char* current_pen_dash;
  wxBitmap* current_stipple;

  wxBitmap* selected_pixmap;

  wxCanvasDC(void);
  wxCanvasDC(wxCanvas* canvas); // Create a DC corresponding to a canvas

  void Init(wxCanvas*);

  ~wxCanvasDC(void);

  void SetCanvasClipping() ;
  void GetClippingBox(float* x,float* y,float* w,float* h) ;
  void ReleaseCurrentDC();

  virtual void BeginDrawing(void);
  virtual void EndDrawing(void);
  virtual void SetCurrentDC(void); // mac platform only
  int DCOffsetX; // mac platform only 
  int DCOffsetY; // mac platform only

  void FloodFill(float x1, float y1, wxColour* col, int style=wxFLOOD_SURFACE) ;
  Bool GetPixel(float x1, float y1, wxColour* col) ;

  void SetPixel(float x1, float y1, wxColour* col) ;
  inline void BeginSetPixel() {}
  inline void EndSetPixel() {}

  void DrawLine(float x1, float y1, float x2, float y2);
  void IntDrawLine(int x1, int y1, int x2, int y2);
  void CrossHair(float x, float y) ;
  void DrawArc(float x1,float y1,float x2,float y2,float xc,float yc);
  void DrawPoint(float x, float y);
  void DrawLines(int n, wxPoint points[], float xoffset = 0, float yoffset = 0);
  void DrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0);
  void DrawPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0,
  					int fillStyle=wxODDEVEN_RULE);
  void DrawRectangle(float x, float y, float width, float height);
  void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20);
  void DrawEllipse(float x, float y, float width, float height);
  void DrawText(const char* text, float x, float y, Bool use16 = FALSE, int d = 0, float angle = 0.0);

  void GetSize(float *width, float *height);

  void Clear(void);
  void SetFont(wxFont* font);
  void SetPen(wxPen* pen);
  void SetBrush(wxBrush* brush);
  void SetLogicalFunction(int function);
  void SetBackground(wxColour* c);
  virtual void SetPaintRegion(Rect* paintRect); // mac platform only
  void SetClippingRect(float x, float y, float width, float height);
  wxRegion* GetClippingRegion();
  void SetClippingRegion(wxRegion*);

  float GetCharHeight(void);
  float GetCharWidth(void);
  virtual void GetTextExtent(const char* string, float* x, float* y, float* descent = NULL,
			     float* externalLeading = NULL, wxFont* the_font = NULL, Bool use16 = FALSE,
			     int d = 0);
  Bool StartDoc(char* message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
  void SetMapMode(int mode);
  void SetUserScale(float x, float y);
  float DeviceToLogicalX(int x);
  float DeviceToLogicalY(int y);
  float DeviceToLogicalXRel(int x);
  float DeviceToLogicalYRel(int y);
  int LogicalToDeviceX(float x);
  int LogicalToDeviceY(float y);
  int LogicalToDeviceXRel(float x);
  int LogicalToDeviceYRel(float y);
  float FLogicalToDeviceX(float x);
  float FLogicalToDeviceY(float y);
  float FLogicalToDeviceXRel(float x);
  float FLogicalToDeviceYRel(float y);

  Bool Blit(float xdest, float ydest, float width, float height,
            wxBitmap* source, float xsrc, float ysrc, int rop = wxSOLID, wxColour *c = NULL,
            wxBitmap* mask = NULL);
  Bool GCBlit(float xdest, float ydest, float width, float height,
            wxBitmap* source, float xsrc, float ysrc);

  void wxMacSetClip(void); // Internal only
  void wxMacSetCurrentTool(wxMacToolType whichTool); // Internal only

  void TryColour(wxColour *src, wxColour *dest);
  
  void InstallColor(wxColour *c, int fg);
  void InstallLogicalFunction(int func);
  
  wxRegion *BrushStipple();
  void PaintStipple(wxRegion *);

  wxGL *GetGL();
};

long wxTextFontInfo(int font, int size, int face, FontInfo *finfo, char *str, int d = 0, int len = -1);
void DrawLatin1Text(const char *text, int d, int len = -1, int bit16 = FALSE, Bool qd_spacing = FALSE, 
		    int smoothing = wxSMOOTHING_DEFAULT, float angle = 0.0);
void GetLatin1TextWidth(const char *text, int d, int theStrlen, 
			short txFont, short txSize, short txFace,
			int bit16, float scale,
			float* x, float* y,
			float* descent, float* externalLeading,
			Bool qd_spacing);

extern Pattern wx_white_pat, wx_black_pat, wx_light_gray_pat, wx_dark_gray_pat;
#define GetWhitePattern() &wx_white_pat
#define GetBlackPattern() &wx_black_pat
#define GetLightGrayPattern() &wx_light_gray_pat
#define GetDarkGrayPattern() &wx_dark_gray_pat
extern void wx_init_patterns();

class wxGL : public wxObject {
public:
  wxGL();

  long gl_ctx; /* really an AGLContext */

  int Ok();
  
  void Reset(CGrafPtr gp, int offscreen, int w, int h);
  
  void SwapBuffers(void);
  void ThisContextCurrent(void);
  
  void ResetGLView(int x, int y, int w, int h);
};

void wxInitGL();

#endif // IN_CPROTO
#endif // wx_dccanh

