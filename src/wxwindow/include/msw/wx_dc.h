/*
 * File:	wx_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */


#ifndef wx_dch
#define wx_dch

#include "wx_gdi.h"
#include "wb_dc.h"

#ifdef IN_CPROTO
typedef       void    *wxDC ;
typedef       void    *wxPrinterDC;
#else

// Since Windows handles DCs quite uniformly, we can have
// a general wxWindowsDC, and derive canvas and printer DCs from this
// with minimum extra functionality.
class wxDC: public wxbDC
{
  DECLARE_ABSTRACT_CLASS(wxDC)

 public:
  int clip_x1;
  int clip_y1;
  int clip_x2;
  int clip_y2;
  Bool dont_delete;
  int window_ext_x;
  int window_ext_y;
  float system_scale_x;
  float system_scale_y;

  wxCanvas *canvas;
  wxBitmap *selected_bitmap;
  char *filename;

  HDC cdc;

  HPEN     cur_cpen ;
  HBRUSH   cur_cbrush ;
  HDC      cur_dc ;
  COLORREF cur_bk ;

  // Store all old GDI objects when do a SelectObject,
  // so we can select them back in (this unselecting user's
  // objects) so we can safely delete the DC.
  HBITMAP old_bitmap;
  HPEN    old_pen;
  HBRUSH  old_brush;
  HFONT   old_font;
  HPALETTE old_palette;

  wxDC(void);
  ~wxDC(void);

  virtual void BeginDrawing(void) ;
  virtual void EndDrawing(void) ;

  void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE) ;
  Bool GetPixel(float x1, float y1, wxColour *col) ;
  void SetPixel(float x1, float y1, wxColour *col) ;

  inline void BeginSetPixel() {}
  inline void EndSetPixel() {}

  void DrawLine(float x1, float y1, float x2, float y2);
  void IntDrawLine(int x1, int y1, int x2, int y2);
  void CrossHair(float x, float y) ;
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

  void DrawIcon(wxIcon *icon, float x, float y);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetLogicalFunction(int function);
  void SetBackground(wxBrush *brush);
  void SetBackgroundMode(int mode);
  void SetClippingRegion(float x, float y, float width, float height);
  /* MATTHEW: [8] */
  void GetClippingRegion(float *x, float *y, float *width, float *height);
  void SetColourMap(wxColourMap *cmap);
  void DestroyClippingRegion(void);
  /* MATTHEW: [2] 16-bit flag */
  void DrawText(const char *text, float x, float y, Bool use16bit = FALSE);

  float GetCharHeight(void);
  float GetCharWidth(void);
  /* MATTHEW: [2] 16-bit flag */
  void GetTextExtent(const char *string, float *x, float *y,
                     float *descent = NULL, float *externalLeading = NULL, 
		     wxFont *theFont = NULL, Bool use16bit = FALSE);
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

  Bool Blit(float xdest, float ydest, float width, float height,
            wxCanvasDC *source, float xsrc, float ysrc, int rop = wxCOPY);
            
  Bool CanDrawBitmap(void);
  Bool CanGetTextExtent(void);

  void SetRop(HDC cdc);
  void DoClipping(HDC cdc);
  void SelectOldObjects(HDC dc);

   HDC ThisDC();
   void DoneDC(HDC dc);
   void ShiftXY(float x, float y, int &ix, int &iy);
};

// This class specific to Windows 3.1
class wxPrinterDC: public wxDC
{
 public:
  DECLARE_CLASS(wxPrinterDC)

  // Create a printer DC
  wxPrinterDC(char *driver, char *device, char *output, Bool interactive = TRUE);
  wxPrinterDC(HDC theDC);

  ~wxPrinterDC(void);
};

// Gets an HDC for the default printer configuration
HDC wxGetPrinterDC(void);

// Logical to device
// Absolute
#define XLOG2DEV(x) (floor(x))

#define YLOG2DEV(y) (floor(y))

// Relative
#define XLOG2DEVREL(x) (floor(x))
#define YLOG2DEVREL(y) (floor(y))

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

#endif // IN_CPROTO
#endif // wx_dc.h
