/*
 * File:	wx_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */


#ifndef wx_dch
#define wx_dch

#include "wx_gdi.h"
#include "wb_dc.h"

#ifdef IN_CPROTO
typedef       void    *wxDC;
typedef       void    *wxPrinterDC;
#else

class wxRegion;
class wxGL;

// Since Windows handles DCs quite uniformly, we can have
// a general wxWindowsDC, and derive canvas and printer DCs from this
// with minimum extra functionality.
class wxDC: public wxbDC
{
 public:
  Bool dont_delete;
  Bool screen_font;
  int combine_status;
  int window_ext_x;
  int window_ext_y;
  float system_scale_x;
  float system_scale_y;

  wxCanvas *canvas;
  wxBitmap *selected_bitmap;
  char *filename;

  HDC cdc;

  HDC      cur_dc;
  COLORREF cur_bk;
  int cur_rop;

  // Store all old GDI objects when do a SelectObject,
  // so we can select them back in (this unselecting user's
  // objects) so we can safely delete the DC.
  HBITMAP old_bitmap;
  HPEN    old_pen;
  HBRUSH  old_brush;
  HFONT   old_font;
  HPALETTE old_palette;

  wxGL *wx_gl;

  wxDC(void);
  ~wxDC(void);

  virtual void BeginDrawing(void);
  virtual void EndDrawing(void);

  void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE);
  Bool GetPixel(float x1, float y1, wxColour *col);
  void SetPixel(float x1, float y1, wxColour *col);

  inline void BeginSetPixel() {}
  inline void EndSetPixel() {}

  Bool BeginSetPixelFast(int x, int y, int w, int h);
  void EndSetPixelFast();
  void SetPixelFast(int x1, int y1, int r, int g, int b);

  Bool BeginGetPixelFast(int x, int y, int w, int h);
  void EndGetPixelFast();
  void GetPixelFast(int x, int y, int *r, int *g, int *b);

  void DrawLine(float x1, float y1, float x2, float y2);
  void IntDrawLine(int x1, int y1, int x2, int y2);
  void CrossHair(float x, float y);
  void DrawArc(float x1,float y1,float x2,float y2,float xc,float yc);
  void DrawPoint(float x, float y);

  void DrawLines(int n, wxPoint points[], float xoffset = 0, float yoffset = 0);
  void DrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0);
  // MS C7 complains if this overloaded function isn't explicitly mentioned
  inline void DrawLines(wxList *list, float xoffset = 0, float yoffset = 0)
  { wxbDC::DrawLines(list, xoffset, yoffset); }

  void DrawPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  // See MS C7 comment above
  inline void DrawPolygon(wxList *list, float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE)
  { wxbDC::DrawPolygon(list, xoffset, yoffset,fillStyle); }

  void DrawRectangle(float x, float y, float width, float height);
  void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20.0);
  void DrawEllipse(float x, float y, float width, float height);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetBackground(wxColour *c);
  void SetBackgroundMode(int mode);
  void SetClippingRect(float x, float y, float width, float height);
  wxRegion* GetClippingRegion();
  void SetClippingRegion(wxRegion*);
  void SetColourMap(wxColourMap *cmap);
  void DrawText(const char *text, float x, float y, 
		Bool combine = FALSE, Bool use16bit = FALSE, 
		int d = 0, float angle = 0.0);

  float GetCharHeight(void);
  float GetCharWidth(void);
  void GetTextExtent(const char *string, float *x, float *y,
                     float *descent = NULL, float *externalLeading = NULL, 
		     wxFont *theFont = NULL, 
		     Bool combine = FALSE, Bool use16bit = FALSE, int d = 0);
  void GetSize(float *width, float *height);
  void GetSizeMM(float *width, float *height);
  Bool StartDoc(char *message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
  void SetMapMode(int mode);
  void SetUserScale(float x, float y);
  void SetSystemScale(float x, float y);
  void SetLogicalOrigin(float x, float y);
  void SetDeviceOrigin(float x, float y);
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

  Bool GlyphAvailable(int c, wxFont *f = NULL);

  Bool Blit(float xdest, float ydest, float width, float height,
            wxBitmap *source, float xsrc, float ysrc, int rop = wxSOLID,
	    wxColour *c=NULL, wxBitmap *mask=NULL);
            
  Bool CanDrawBitmap(void);
  Bool CanGetTextExtent(void);

  void SetRop(HDC cdc, int mode);
  void DoClipping(HDC cdc);
  void SelectOldObjects(HDC dc);
   HDC ThisDC();
   void DoneDC(HDC dc);
   void ShiftXY(float x, float y, int *ix, int *iy);

  Bool StartBrush(HDC dc, Bool no_stipple = FALSE);
  Bool StartPen(HDC dc);
  void DoneBrush(HDC dc);
  void DonePen(HDC dc);

  wxBitmap *StippleBrush();				   

  virtual wxGL *GetGL();
};

// This class specific to Windows 3.1
class wxPrinterDC: public wxDC
{
 public:
  // Create a printer DC
  wxPrinterDC(wxWindow *parent = NULL, 
	      char *driver = NULL, char *device = NULL, char *output = NULL, Bool interactive = TRUE);
  wxPrinterDC(HDC theDC);

  ~wxPrinterDC(void);
};

// Gets an HDC for the default printer configuration
HDC wxGetPrinterDC(void);

// Logical to device
// Absolute
#define XLOG2DEV(x) (x)

#define YLOG2DEV(y) (y)

// Relative
#define XLOG2DEVREL(x) (x)
#define YLOG2DEVREL(y) (y)

// Device to logical
// Absolute
#define XDEV2LOG(x) (x)

#define YDEV2LOG(y) (y)

// Relative
#define XDEV2LOGREL(x) (x)
#define YDEV2LOGREL(y) (y)

/*
 * Have the same macros as for XView but not for every operation:
 * just for calculating window/viewport extent (a better way of scaling).
 */

// Logical to device
// Absolute
#define MS_XLOG2DEV(x) ((int)floor(((x) - logical_origin_x)*logical_scale_x*user_scale_x*system_scale_x + device_origin_x))

#define MS_YLOG2DEV(y) ((int)floor(((y) - logical_origin_y)*logical_scale_y*user_scale_y*system_scale_y + device_origin_y))

// Relative
#define MS_XLOG2DEVREL(x) ((int)floor((x)*logical_scale_x*user_scale_x*system_scale_x))
#define MS_YLOG2DEVREL(y) ((int)floor((y)*logical_scale_y*user_scale_y*system_scale_y))

// Device to logical
// Absolute
// #define MS_XDEV2LOG(x) (int)(((x) - device_origin_x)/(logical_scale_x*user_scale_x*system_scale_x) + logical_origin_x)
#define MS_XDEV2LOG(x) (((x) - device_origin_x)/(logical_scale_x*user_scale_x*system_scale_x) + logical_origin_x)

//#define MS_YDEV2LOG(y) (int)(((y) - device_origin_y)/(logical_scale_y*user_scale_y*system_scale_y) + logical_origin_y)
#define MS_YDEV2LOG(y) (((y) - device_origin_y)/(logical_scale_y*user_scale_y*system_scale_y) + logical_origin_y)

// Relative
//#define MS_XDEV2LOGREL(x) (int)((x)/(logical_scale_x*user_scale_x*system_scale_x))
#define MS_XDEV2LOGREL(x) ((x)/(logical_scale_x*user_scale_x*system_scale_x))
//#define MS_YDEV2LOGREL(y) (int)((y)/(logical_scale_y*user_scale_y*system_scale_y))
#define MS_YDEV2LOGREL(y) ((y)/(logical_scale_y*user_scale_y*system_scale_y))


class wxGL : public wxObject {
public:
  wxGL();
  virtual ~wxGL();

  virtual int Ok() = 0;

  virtual void Reset(HDC dc, int offscreen) = 0;

  virtual void SwapBuffers(void) = 0;
  virtual void ThisContextCurrent(void) = 0;
};

#endif // IN_CPROTO
#endif // wx_dc.h
