/*
 * File:	wb_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */


#ifndef wxb_dch
#define wxb_dch

#include "common.h"
#include "wx_frame.h"
#include "wx_gdi.h"

#define wxDEVICE_CANVAS  1
                            // X canvas
#define wxDEVICE_EPS     2
                            // Encapsulated PostScript on any platform
#define wxDEVICE_WINDOWS 3
                            // MS Windows device (canvas, printer)
#define wxDEVICE_PIXMAP  4
                            // X pixmap

#define MM_POINTS      7
#define MM_METRIC      8

class wxRegion;
class wxCanvas;
class wxCanvasDC;
class wxDC;
class wxbDC: public wxObject
{
 protected:
  Bool dcOptimize;
 public:
  int device;
  Bool ok;
  Bool wx_interactive;
  wxRegion *clipping;

  // Coordinate system variables
  float logical_origin_x;
  float logical_origin_y;

  float device_origin_x;
  float device_origin_y;

  float logical_scale_x;
  float logical_scale_y;

  float user_scale_x;
  float user_scale_y;

  int mapping_mode;

  float min_x;          // bounding box
  float min_y;
  float max_x;
  float max_y;
  char *title;

  Bool Colour;

  int current_bk_mode;

  wxPen *current_pen;
  wxBrush *current_brush;
  wxColour *current_background_color;
  wxColour *current_text_foreground;
  wxColour *current_text_background;
  wxFont *font;
  Bool autoSetting ;

  wxbDC(void);

  // Create a printer DC
//  wxDC(char *driver, char *device, char *output, Bool interactive = TRUE);

  ~wxbDC(void);

  //
  // This function is intended to improves drawing, by avoiding to
  // repeatly call ::SetPen/::SetBrush. If set to FALSE, these functions
  // aren't called when calling ::DrawLine(),...
  // Please note that this is YOUR responsability to use it, and do it
  // only when you KNOWN that pen/brush isn't changed between 2 calls to
  // DrawLine,... !!!
  // Note also that in X, we don't test autoSetting on brushes, because they
  // modify Foreground, as pens. So, convention is:
  //   - call your SetBrush(), THEN your SetPen, THEN AutoSetTools(FALSE)
  //   - call DrawLine,...
  // [mainly coded for Windows]
  inline virtual void AutoSetTools(Bool auto_setting) { autoSetting = auto_setting ; }
 
  inline virtual void BeginDrawing(void) {} ;
  inline virtual void EndDrawing(void) {} ;

  virtual void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE) = 0;
  virtual Bool GetPixel(float x1, float y1, wxColour *col) = 0;

  virtual void DrawLine(float x1, float y1, float x2, float y2) = 0;
  virtual void IntDrawLine(int x1, int y1, int x2, int y2) = 0;
  virtual void CrossHair(float x, float y) = 0;
  virtual void DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)=0;
  virtual void DrawPoint(float x, float y) = 0;
  inline virtual void DrawPoint(wxPoint *point) { DrawPoint(point->x, point->y); }
  virtual void DrawLines(int n, wxPoint points[], float xoffset = 0, float yoffset = 0) = 0;
  virtual void DrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0) = 0;
  virtual void DrawLines(wxList *list, float xoffset = 0, float yoffset = 0);
  virtual void DrawPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE) = 0;
  virtual void DrawPolygon(wxList *list, float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  virtual void DrawRectangle(float x, float y, float width, float height) = 0;
  virtual void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20) = 0;
  virtual void DrawEllipse(float x, float y, float width, float height) = 0;
#if USE_SPLINES
  // Splines
  // 3-point spline
  virtual void DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3);
  // Any number of control points - a list of pointers to wxPoints
  virtual void DrawSpline(wxList *points);
#endif
  virtual void DrawText(const char *text, float x, float y,
                        Bool use16bit = FALSE, int d = 0, float angle = 0.0) = 0;
  virtual void Clear(void) = 0;

  virtual Bool StartDoc(char *message) = 0;
  virtual void EndDoc(void) = 0;
  virtual void StartPage(void) = 0;
  virtual void EndPage(void) = 0;

  virtual void SetFont(wxFont *font) = 0;
  virtual void SetPen(wxPen *pen) = 0;
  virtual void SetBrush(wxBrush *brush) = 0;
  virtual void SetBackground(wxColour *c) = 0;
  virtual void SetTextForeground(wxColour *colour);
  virtual void SetTextBackground(wxColour *colour);
  virtual void SetBackgroundMode(int mode); // wxSOLID or wxTRANSPARENT
                                            // for drawing background colour
  int GetBackgroundMode(void);
  virtual void SetClippingRect(float x, float y, float width, float height)= 0;
  virtual wxRegion *GetClippingRegion() = 0;
  virtual void SetClippingRegion(wxRegion *r) = 0;
  inline virtual void SetColourMap(wxColourMap *WXUNUSED(cmap)) {};

  virtual float GetCharHeight(void) = 0;
  virtual float GetCharWidth(void) = 0;
  virtual void GetTextExtent(const char *string, float *x, float *y,
                             float *descent = NULL, 
                             float *externalLeading = NULL, 
                             wxFont *theFont = NULL,
                             Bool use16bit = FALSE,
			     int d = 0) = 0;

  inline virtual Bool Ok(void) {return ok;};
  virtual Bool CanGetTextExtent(void) = 0;
  virtual Bool CanDrawBitmap(void) = 0;
  virtual void SetMapMode(int mode) = 0;
  inline virtual int  GetMapMode(void) {return mapping_mode;};

  // The following methods provide a cleaner interface
  virtual wxColour *GetBackground(void);
  inline virtual wxBrush *GetBrush(void)           { return current_brush;}
  inline virtual wxFont  *GetFont(void)            { return font;}
  inline virtual wxPen   *GetPen(void)             { return current_pen;}
  inline virtual wxColour* GetTextBackground(void)  { return current_text_background;}
  inline virtual wxColour* GetTextForeground(void)  { return current_text_foreground;}
 
  virtual void SetLogicalOrigin(float x, float y);
  virtual void SetDeviceOrigin(float x, float y);
  void GetDeviceOrigin(float *x, float *y) {*x = device_origin_x; *y = device_origin_y; }
  virtual void SetLogicalScale(float x, float y);
  virtual void SetUserScale(float x, float y) = 0;
  void GetUserScale(float *x, float *y) {*x = user_scale_x; *y = user_scale_y; }
  virtual float DeviceToLogicalX(int x) = 0;
  virtual float DeviceToLogicalY(int y) = 0;
  virtual float DeviceToLogicalXRel(int x) = 0;
  virtual float DeviceToLogicalYRel(int y) = 0;
  virtual int LogicalToDeviceX(float x) = 0;
  virtual int LogicalToDeviceY(float y) = 0;
  virtual int LogicalToDeviceXRel(float x) = 0;
  virtual int LogicalToDeviceYRel(float y) = 0;
  virtual float FLogicalToDeviceX(float x) = 0;
  virtual float FLogicalToDeviceY(float y) = 0;
  virtual float FLogicalToDeviceXRel(float x) = 0;
  virtual float FLogicalToDeviceYRel(float y) = 0;
  // Only works for PostScript *after* you've printed an image.
  // Gives width and height of image.
  virtual void GetSize(float *width, float *height);
  virtual inline void GetSizeMM(float *width, float *height) { *width = 0.0; *height = 0.0; };
  virtual void CalcBoundingBox(float x, float y);
  // Get the final bounding box of the PostScript or Metafile picture.
  virtual inline float MinX(void) { return min_x; }
  virtual inline float MaxX(void) { return max_x; }
  virtual inline float MinY(void) { return min_y; }
  virtual inline float MaxY(void) { return max_y; }
  virtual Bool Blit(float xdest, float ydest, float width, float height,
                    wxBitmap *source, float xsrc, float ysrc, int rop = wxSOLID, 
		    wxColour* c = NULL, wxBitmap *mask = NULL) = 0;

  // Sometimes we need to override optimization, e.g.
  // if other software is drawing onto our surface and we
  // can't be sure of who's done what.
  inline virtual void SetOptimization(Bool opt) { dcOptimize = opt; }
  inline virtual Bool GetOptimization(void) { return dcOptimize; }

  virtual void TryColour(wxColour *src, wxColour *dest);
};

/*
extern char wx_printer_file[];
extern float wx_printer_scale_x;
extern float wx_printer_scale_y;
extern float wx_printer_translate_x;
extern float wx_printer_translate_y;
*/
extern int wxPageNumber;

// Conversion
#define METRIC_CONVERSION_CONSTANT  0.0393700787

// Scaling factors for various unit conversions
#define mm2inches (METRIC_CONVERSION_CONSTANT)
#define inches2mm (1/METRIC_CONVERSION_CONSTANT)

#define mm2twips (METRIC_CONVERSION_CONSTANT*1440)
#define twips2mm (1/(METRIC_CONVERSION_CONSTANT*1440))

#define mm2pt (METRIC_CONVERSION_CONSTANT*72)
#define pt2mm (1/(METRIC_CONVERSION_CONSTANT*72))

#define     wx_round(a)    (int)((a)+.5)

#endif // wxb_dch
