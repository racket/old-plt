/*
 * File:	wx_dc.cc
 * Purpose:	Device context implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_dc.cxx,v 1.2 1998/05/21 19:03:37 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

// For some reason, this must be defined for common dialogs to work.
#ifdef __WATCOMC__
#define INCLUDE_COMMDLG_H	1
#endif

#include "wx_privt.h"
#include "wx_dcpan.h"
#include "wx_frame.h"
#include "wx_dc.h"
#include "wx_dccan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_dialg.h"
#include "wx_main.h"
#include "wx_wmgr.h"

#endif

#include "wx_pdf.h"

#include <math.h>

#if USE_COMMON_DIALOGS
#include <commdlg.h>
#endif

#ifndef WIN32
#include <print.h>
#endif

// Declarations local to this file

#define YSCALE(y) (yorigin - (y))

// #define     wx_round(a)    (int)((a)+.5)

IMPLEMENT_DYNAMIC_CLASS(wxDC, wxObject)

// Default constructor
wxDC::wxDC(void)
{
  __type = wxTYPE_DC;
  filename = NULL;
  selected_bitmap = NULL;
  canvas = NULL;
  cur_dc = NULL ;
  cur_bk = 0 ;
  cur_cpen = NULL ;
  cur_cbrush = NULL ;
  old_bitmap = 0;
  old_pen = 0;
  old_brush = 0;
  old_font = 0;
  old_palette = 0;
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  font = wxNORMAL_FONT;
  logical_origin_x = 0;
  logical_origin_y = 0;
  device_origin_x = 0;
  device_origin_y = 0;
  logical_scale_x = 1.0;
  logical_scale_y = 1.0;
  user_scale_x = 1.0;
  user_scale_y = 1.0;
  system_scale_x = 1.0;
  system_scale_y = 1.0;
  mapping_mode = MM_TEXT;
  title = NULL;
  dont_delete = FALSE;
  cdc = NULL;
  clipping = FALSE;
  ok = TRUE;
  window_ext_x = VIEWPORT_EXTENT;
  window_ext_y = VIEWPORT_EXTENT;
  current_logical_function = wxCOPY;
  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = wxWHITE_BRUSH;
  current_background_brush->Lock(1);
  current_text_foreground = *wxBLACK;
  current_bk_mode = wxTRANSPARENT;
  Colour = wxColourDisplay();
}


wxDC::~wxDC(void)
{
  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  if (current_background_brush) current_background_brush->Lock(-1);

  if (filename)
    delete[] filename;

  if (cdc)
  {
    SelectOldObjects(cdc);
    DeleteDC(cdc);
  }
}

// This will select current objects out of the DC,
// which is what you have to do before deleting the
// DC.
void wxDC::SelectOldObjects(HDC dc)
{
#if DEBUG > 1
  wxDebugMsg("wxDC::SelectOldObjects %X\n", this);
#endif
  if (dc)
  {
    if (old_bitmap)
    {
#if DEBUG > 1
      wxDebugMsg("wxDC::SelectOldObjects: Selecting old HBITMAP %X\n", old_bitmap);
#endif
      ::SelectObject(dc, old_bitmap);
      if (selected_bitmap)
      {
        selected_bitmap->selectedInto = NULL;
        selected_bitmap->selectedIntoDC = 0;

        selected_bitmap = NULL;
      }
    }
    old_bitmap = NULL ;
    if (old_pen)
    {
#if DEBUG > 1
      wxDebugMsg("wxDC::SelectOldObjects: Selecting old HPEN %X\n", old_pen);
#endif
      ::SelectObject(dc, old_pen);
    }
    old_pen = NULL ;
    if (old_brush)
    {
#if DEBUG > 1
      wxDebugMsg("wxDC::SelectOldObjects: Selecting old HBRUSH %X\n", old_brush);
#endif
      ::SelectObject(dc, old_brush);
    }
    old_brush = NULL ;
    if (old_font)
    {
#if DEBUG > 1
      wxDebugMsg("wxDC::SelectOldObjects: Selecting old HFONT %X\n", old_font);
#endif
      ::SelectObject(dc, old_font);
    }
    old_font = NULL ;
    if (old_palette)
    {
#if DEBUG > 1
      wxDebugMsg("wxDC::SelectOldObjects: Selecting old HPALETTE %X\n", old_palette);
#endif
      ::SelectPalette(dc, old_palette, TRUE);
    }
#if DEBUG > 1
    wxDebugMsg("wxDC::SelectOldObjects: Done.\n");
#endif
    old_palette = NULL ;
  }
}



HDC wxDC::ThisDC(void)

{
  HDC dc = NULL;
  wxWnd *wnd = NULL;

  if (canvas) wnd = (wxWnd *)canvas->handle;
  if (cdc)
    dc = cdc;
  else if (wnd)
    dc = wnd->GetHDC();

  return dc;
}



void wxDC::DoneDC(HDC dc)

{

  if (dc && !cdc) {

    wxWnd *wnd = NULL;

    if (canvas) wnd = (wxWnd *)canvas->handle;

    if (!cdc && wnd)

      wnd->ReleaseHDC();

  }

}



void wxDC::ShiftXY(float x, float y, int &ix, int &iy)

{
  ix = (int)x;
  iy = (int)y;

  if (canvas)
  {
    wxWnd *wnd = (wxWnd *)canvas->handle;
    wnd->CalcScrolledPosition((int)x, (int)y, &ix, &iy);
  }
}

void wxDC::SetClippingRegion(float cx, float cy, float cw, float ch)
{
  clipping = TRUE;
  clip_x1 = (int)cx;
  clip_y1 = (int)cy;
  clip_x2 = (int)(cx + cw);
  clip_y2 = (int)(cy + ch);

  HDC dc = ThisDC();

  if (dc)
	  DoClipping(dc);

  DoneDC(dc);
}

/* MATTHEW: [8] */
void wxDC::GetClippingRegion(float *cx, float *cy, float *cw, float *ch)
{
  if (clipping) {
    *cx = clip_x1;
    *cy = clip_y1;
    *cw = clip_x2 - clip_x1;
    *ch = clip_y2 - clip_y1;
  } else {
    *cx = *cy = 0;
    *cw = *ch = -1;
  }
}

void wxDC::DoClipping(HDC dc)
{
  if (clipping && dc)
  {
    int x1, x2;
    int y1, y2;

	ShiftXY(clip_x1, clip_y1, x1, y1);
    ShiftXY(clip_x2, clip_y2, x2, y2);
    
	/* Reset back to unclipped (before we intersect) */
	HRGN rgn = CreateRectRgn(0, 0, 32000, 32000);
    SelectClipRgn(dc, rgn);
    DeleteObject(rgn);

	/* Use IntersectClipRect because it works in logical units */
	IntersectClipRect(dc, XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
  }
}

void wxDC::DestroyClippingRegion(void)
{
  HDC dc = ThisDC();

  if (clipping && dc)
  {
    HRGN rgn = CreateRectRgn(0, 0, 32000, 32000);
    SelectClipRgn(dc, rgn);
    DeleteObject(rgn);
   }
   clipping = FALSE;

  DoneDC(dc);
}

Bool wxDC::CanDrawBitmap(void)
{
  return TRUE;
}

Bool wxDC::CanGetTextExtent(void)
{
  HDC dc = ThisDC();
  
  // What sort of display is it?

  if (dc) {
	  int technology = ::GetDeviceCaps(dc, TECHNOLOGY);

	  Bool ok;
  
	  if (technology != DT_RASDISPLAY && technology != DT_RASPRINTER)
		ok = FALSE;
	  else ok = TRUE;
  } else

		ok = FALSE;


  DoneDC(dc);

  return ok;
}

void wxDC::SetColourMap(wxColourMap *cmap)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (!cmap)
  {
    // Setting a NULL colourmap is a way of restoring
    // the original colourmap
    if (old_palette)
    {
      ::SelectPalette(dc, old_palette, TRUE);
#if DEBUG > 1
      wxDebugMsg("wxDC::SetColourMap: set old palette %X\n", old_palette);
#endif
      old_palette = 0;
    }
  }
    
  if (cmap && cmap->ms_palette)
  {
    HPALETTE oldPal = ::SelectPalette(dc, cmap->ms_palette, TRUE);
    if (!old_palette)
      old_palette = oldPal;
      
#if DEBUG > 1
    wxDebugMsg("wxDC::SetColourMap %X: selected palette %X\n", this, cmap->ms_palette);
    if (oldPal)
      wxDebugMsg("wxDC::SetColourMap: oldPal was palette %X\n", oldPal);
    if (old_palette)
      wxDebugMsg("wxDC::SetColourMap: old_palette is palette %X\n", old_palette);
#endif
    ::RealizePalette(dc);
  }

  DoneDC(dc);
}

void wxDC::Clear(void)
{
  HDC dc = ThisDC();

  if (!dc) return;

  RECT rect;
  if (canvas)
    GetClientRect(((wxWnd *)canvas->handle)->handle, &rect);
  else if (selected_bitmap)
  {
    rect.left = 0; rect.top = 0;
    rect.right = selected_bitmap->GetWidth();
    rect.bottom = selected_bitmap->GetHeight();
  }
  (void) ::SetMapMode(dc, MM_TEXT);
  ::SetViewportOrgEx(dc, 0, 0, NULL);

  ::SetWindowOrgEx(dc, 0, 0, NULL);


  DWORD colour = GetBkColor(dc);
  HBRUSH brush = CreateSolidBrush(colour);
  FillRect(dc, &rect, brush);
  DeleteObject(brush);

  ::SetMapMode(dc, MM_ANISOTROPIC);
  ::SetViewportExtEx(dc, VIEWPORT_EXTENT, VIEWPORT_EXTENT, NULL);
  ::SetWindowExtEx(dc, window_ext_x, window_ext_y, NULL);
  ::SetViewportOrgEx(dc, (int)device_origin_x, (int)device_origin_y, NULL);
  ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);

   DoneDC(dc);
}

void wxDC::BeginDrawing(void)
{
}

void wxDC::EndDrawing(void)
{
}

void wxDC::FloodFill(float x, float y, wxColour *col, int style)
{
      HDC dc = ThisDC();

	  if (!dc) return;



	  int xx;
      int yy;


	  ShiftXY(x, y, xx, yy);


      if (autoSetting && current_brush)
          SetBrush(current_brush);


      if (autoSetting)

		SetRop(dc);


      (void)ExtFloodFill(dc, XLOG2DEV(xx), YLOG2DEV(yy),
                        col->pixel,
                        style==wxFLOOD_SURFACE?
                          FLOODFILLSURFACE:FLOODFILLBORDER
                        );

      DoneDC(dc) ;

      CalcBoundingBox((float)x, (float)y);
}

Bool wxDC::GetPixel(float x, float y, wxColour *col)
{
  HDC dc = ThisDC();

  if (!dc) return FALSE;



  // added by steve 29.12.94 (copied from DrawPoint)
  // returns TRUE for pixels in the color of the current pen
  // and FALSE for all other pixels colors
  // if col is non-NULL return the color of the pixel
  int xx1;
  int yy1;

  ShiftXY(x, y, xx1, yy1);
  
  // get the color of the pixel
  COLORREF pixelcolor = ::GetPixel(dc, XLOG2DEV(xx1), YLOG2DEV(yy1));
  // get the color of the pen
  COLORREF pencolor = 0x00ffffff;
  if (current_pen)
    pencolor = current_pen->GetColour().pixel ;
  
  DoneDC(dc);

  // return the color of the pixel
  if(col)
    col->Set(GetRValue(pixelcolor),GetGValue(pixelcolor),GetBValue(pixelcolor));
  
  // check, if color of the pixels is the same as the color
  // of the current pen
  return(pixelcolor==pencolor);
}

void wxDC::IntDrawLine(int x1, int y1, int x2, int y2)
{
  HDC dc = ThisDC() ;

  if (!dc) return;

 

  int xx1, yy1, xx2, yy2;
  
  if (current_pen && autoSetting)
    SetPen(current_pen);
 

  ShiftXY(x1, y1, xx1, yy1);

  ShiftXY(x2, y2, xx2, yy2);

  
  (void)MoveToEx(dc, XLOG2DEV(xx1), YLOG2DEV(yy1), NULL);
  (void)LineTo(dc, XLOG2DEV(xx2), YLOG2DEV(yy2));

  DoneDC(dc) ;
 
  CalcBoundingBox((float)x1, (float)y1);
  CalcBoundingBox((float)x2, (float)y2);
}

void wxDC::CrossHair(float x, float y)
{
      HDC dc = ThisDC(); 

	  if (!dc) return;



	  int xx, yy;



	  ShiftXY(x, y, xx, yy);


      // We suppose that our screen is 2000x2000 max.

      int xx1 = xx-2000;

      int yy1 = yy-2000;

      int xx2 = xx+2000;

      int yy2 = yy+2000;



      if (current_pen && autoSetting)
        SetPen(current_pen);

      (void)MoveToEx(dc, XLOG2DEV(xx1), YLOG2DEV(yy), NULL);
      (void)LineTo(dc, XLOG2DEV(xx2), YLOG2DEV(yy));

      (void)MoveToEx(dc, XLOG2DEV(xx), YLOG2DEV(yy1), NULL);
      (void)LineTo(dc, XLOG2DEV(xx), YLOG2DEV(yy2));

      DoneDC(dc) ;

      CalcBoundingBox((float)x - 2000, (float)y - 2000);
      CalcBoundingBox((float)x + 2000, (float)y + 2000);
}

void wxDC::DrawLine(float x1, float y1, float x2, float y2)
{
  HDC dc = ThisDC();

  if (!dc) return;



  int xx1, yy1, xx2, yy2;
  
  ShiftXY(x1, y1, xx1, yy1);
  ShiftXY(x2, y2, xx2, yy2);


// BUGBUG - is this necessary? YES YES YES YEs Yes yes ye....
  if (current_pen && autoSetting)
    SetPen(current_pen);

  (void)MoveToEx(dc, XLOG2DEV(xx1), YLOG2DEV(yy1), NULL);
  (void)LineTo(dc, XLOG2DEV(xx2), YLOG2DEV(yy2));

  DoneDC(dc);

  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
}

void wxDC::DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)
{
  HDC dc = ThisDC();

  if (!dc) return;



  int xx1, yy1, xx2, yy2, xxc, yyc;

  

  ShiftXY(x1, y1, xx1, yy1);

  ShiftXY(x2, y2, xx2, yy2);

  ShiftXY(xc, yc, xxc, yyc);


  double dx = xc-x1 ;
  double dy = yc-y1 ;
  double radius = (double)sqrt(dx*dx+dy*dy) ;;
  if (xx1==xx2 && xx2==yy2)
  {
    DrawEllipse(xc,yc,(float)(radius*2.0),(float)(radius*2)) ;
    return ;
  }

// BUGBUG - is this necessary?
  if (current_pen && autoSetting)
    SetPen(current_pen) ;
  
  xx1 = XLOG2DEV(xx1) ;
  yy1 = YLOG2DEV(yy1) ;
  xx2 = XLOG2DEV(xx2) ;
  yy2 = YLOG2DEV(yy2) ;
  xxc = XLOG2DEV(xxc) ;
  yyc = YLOG2DEV(yyc) ;
  double ray = sqrt((xxc-xx1)*(xxc-xx1)+(yyc-yy1)*(yyc-yy1)) ;
 
  (void)MoveToEx(dc, xx1, yy1, NULL);
  int xxx1 = (int)(xxc-ray);
  int yyy1 = (int)(yyc-ray);
  int xxx2 = (int)(xxc+ray);
  int yyy2 = (int)(yyc+ray);
  if (current_brush && current_brush->GetStyle() !=wxTRANSPARENT)
  {
// BUGBUG - is this necessary?
    if (current_brush&&current_brush->GetStyle()!=wxTRANSPARENT&&autoSetting)
      SetBrush(current_brush) ;

	if (autoSetting)

		SetRop(dc);
    Pie(dc,xxx1,yyy1,xxx2,yyy2,
        xx1,yy1,xx2,yy2) ;
  }
  else
    Arc(dc,xxx1,yyy1,xxx2,yyy2,
        xx1,yy1,xx2,yy2) ;

  DoneDC(dc);

  CalcBoundingBox((float)(xc-radius), (float)(yc-radius));
  CalcBoundingBox((float)(xc+radius), (float)(yc+radius));
}

void wxDC::DrawPoint(float x, float y)
{
  if (current_pen)
    SetPixel(x, y, &current_pen->GetColour());
}

void wxDC::SetPixel(float x, float y, wxColour *c)
{
  HDC dc = ThisDC();

  if (!dc) return;



  wxWnd *wnd = NULL;


  int xx1, yy1;

  ShiftXY(x, y, xx1, yy1);
  
  ::SetPixel(dc, XLOG2DEV(xx1), YLOG2DEV(yy1), c->pixel);

  if (!cdc)
    wnd->ReleaseHDC();

  CalcBoundingBox(x, y);
}

void wxDC::DrawPolygon(int n, wxPoint points[], float xoffset, float yoffset,int fillStyle)
{
  HDC dc = ThisDC();

  if (!dc) return;



 // BUGBUG - is this necessary?
  if (autoSetting) {
    if (current_pen)
      SetPen(current_pen);
    if (current_brush)
      SetBrush(current_brush);
  }
  

  if (autoSetting)

		SetRop(dc);


  int xoffset1;

  int yoffset1;

  

  ShiftXY(xoffset, yoffset, xoffset1, yoffset1);

  

  POINT *cpoints = new POINT[n];
  int i;
  for (i = 0; i < n; i++)
  {

	cpoints[i].x = (int)(XLOG2DEV(points[i].x + xoffset1));

	cpoints[i].y = (int)(YLOG2DEV(points[i].y + yoffset1));

    CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
  }

  int prev = SetPolyFillMode(dc,fillStyle==wxODDEVEN_RULE?ALTERNATE:WINDING) ;
  (void)Polygon(dc, cpoints, n);
  SetPolyFillMode(dc,prev) ;

  DoneDC(dc);

  delete[] cpoints;
}

void wxDC::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
{
  HDC dc = ThisDC() ;

  if (!dc) return;



  if (current_pen && autoSetting)
    SetPen(current_pen);
 

  int xoffset1;

  int yoffset1;

  

  ShiftXY(xoffset, yoffset, xoffset1, yoffset1);
  
  POINT *cpoints = new POINT[n];
  int i;
  for (i = 0; i < n; i++)
  {
	 cpoints[i].x = (int)(XLOG2DEV(points[i].x + xoffset1));
	 cpoints[i].y = (int)(YLOG2DEV(points[i].y + yoffset1));

	 CalcBoundingBox((float)points[i].x + xoffset, (float)points[i].y + yoffset);
  }

  (void)Polyline(dc, cpoints, n);

  DoneDC(dc) ;

  delete[] cpoints;
}

void wxDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
  HDC dc = ThisDC();

  if (!dc) return;



  // BUGBUG - is this necessary?
  if (current_pen && autoSetting)
    SetPen(current_pen);

  int xoffset1;

  int yoffset1;

  

  ShiftXY(xoffset, yoffset, xoffset1, yoffset1);

  

  POINT *cpoints = new POINT[n];
  int i;
  for (i = 0; i < n; i++)
  {
	 cpoints[i].x = (int)(XLOG2DEV(points[i].x + xoffset1));
	 cpoints[i].y = (int)(YLOG2DEV(points[i].y + yoffset1));

	 CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
  }

 (void)Polyline(dc, cpoints, n);

  DoneDC(dc);

  delete[] cpoints;
}

void wxDC::DrawRectangle(float x, float y, float width, float height)
{
  HDC dc = ThisDC();

  if (!dc) return;



  int x1, y1, x2, y2;



  ShiftXY(x, y, x1, y1);
  ShiftXY(x + width, y + height, x2, y2);



// BUGBUG - is this necessary?
  if (autoSetting) {
    if (current_pen) 
      SetPen(current_pen);
    if (current_brush)
      SetBrush(current_brush);

  }

  

  if (autoSetting)

	  SetRop(dc);



 /* MATTHEW: [6] new normalization */
#if WX_STANDARD_GRAPHICS
  Bool do_brush, do_pen;

  do_brush = current_brush && current_brush->GetStyle() != wxTRANSPARENT;
  do_pen = current_pen && current_pen->GetStyle() != wxTRANSPARENT;

  if (do_brush) {
	 HPEN orig_pen = NULL;

	 if (do_pen || !current_pen)
		orig_pen = ::SelectObject(dc, ::GetStockObject(NULL_PEN));

	 (void)Rectangle(dc, XLOG2DEV(x1), YLOG2DEV(y1),
		  XLOG2DEV(x2) + 1, YLOG2DEV(y2) + 1);

	 if (do_pen || !current_pen)
		::SelectObject(dc , orig_pen);
  }
  if (do_pen) {
	 HBRUSH orig_brush = NULL;

	 if (do_brush || !current_brush)
		orig_brush = ::SelectObject(dc, ::GetStockObject(NULL_BRUSH));

	 (void)Rectangle(dc, XLOG2DEV(x1), YLOG2DEV(y1),
		  XLOG2DEV(x2), YLOG2DEV(y2));

	 if (do_brush || !current_brush)
		::SelectObject(dc, orig_brush);
  }
#else
  (void)Rectangle(dc, XLOG2DEV(x1), YLOG2DEV(y1),
		  XLOG2DEV(x2), YLOG2DEV(y2));
#endif

  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);

  DoneDC(dc);
}

void wxDC::DrawRoundedRectangle(float x, float y, float width, float height, float radius)
{
  HDC dc = ThisDC();

  if (!dc) return;



  int x1, y1, x2, y2;



  ShiftXY(x, y, x1, y1);

  ShiftXY(x + width, y + height, x2, y2);

  

  // Now, a negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'

  if (radius < 0.0)
  {
	 float smallest = 0.0;
	 if (width < height)
		smallest = width;
	 else
		smallest = height;
	 radius = (float)(- radius * smallest);
  }

// BUGBUG - is this necessary?
  if (autoSetting) {
    if (current_pen) 
      SetPen(current_pen);
    if (current_brush)
      SetBrush(current_brush);
  }

  if (autoSetting)

	  SetRop(dc);


  (void)RoundRect(dc, XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2),
							 YLOG2DEV(y2), XLOG2DEV(radius), YLOG2DEV(radius));

  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);

  DoneDC(dc);
}

void wxDC::DrawEllipse(float x, float y, float width, float height)
{
  HDC dc = ThisDC();

  if (!dc) return;



  int x1, y1, x2, y2;



  ShiftXY(x, y, x1, y1);

  ShiftXY(x + width, y + height, x2, y2);

  

  wxWnd *wnd = NULL;

// BUGBUG - is this necessary?
  if (autoSetting) {
    if (current_pen) 
      SetPen(current_pen);
    if (current_brush)
      SetBrush(current_brush);
  }

  if (autoSetting)

	SetRop(dc);

  (void)Ellipse(dc, XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));

  DoneDC(dc);

  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);
}

void wxDC::DrawIcon(wxIcon *icon, float x, float y)
{
  if (icon->Ok() && !icon->selectedIntoDC) {

    int w, h;

    wxMemoryDC *mdc;

    w = icon->GetWidth();

    h = icon->GetHeight();



    mdc = new wxMemoryDC();

    mdc->SelectObject(icon);

    if (mdc->Ok()) {

      Blit(x, y, w, h, mdc, 0, 0);

    }

    mdc->SelectObject(NULL);

    delete mdc;

  }
}

void wxDC::SetFont(wxFont *the_font)
{
  HDC dc = ThisDC();
  if (!dc) return;


  font = the_font;

  if (!the_font)
  {
    if (old_font)
      ::SelectObject(dc, old_font);
    old_font = NULL ;
  }
  if (font)
    font->BuildInternalFont(dc) ;

  if (font && font->cfont)
  {
#if DEBUG > 1
    wxDebugMsg("wxDC::SetFont: Selecting HFONT %X\n", font->cfont);
#endif
    HFONT f = ::SelectObject(dc, font->cfont);
    if (!old_font)
      old_font = f;
  }
  DoneDC(dc);
}

void wxDC::SetPen(wxPen *pen)
{
  HDC dc = ThisDC();
  if (!dc) return;


  if (current_pen) current_pen->Lock(-1);
  current_pen = pen;
  if (current_pen) current_pen->Lock(1);

  if (!pen)
  {
    if (old_pen)
      ::SelectObject(dc, old_pen);
    old_pen = NULL ;
  }

  if (pen)
  {
    pen->ChangePen();

    HPEN p = pen->SelectPen(dc) ;
    if (!old_pen)
      old_pen = p;
  }

  DoneDC(dc);
}

void wxDC::SetBrush(wxBrush *brush)
{
  HDC dc = ThisDC();
  if (!dc) return;


  if (current_brush) current_brush->Lock(-1);
  current_brush = brush;
  if (current_brush) current_brush->Lock(1);

  if (!brush)
  {
    if (old_brush)
      ::SelectObject(dc, old_brush);
    old_brush = NULL ;
  }

  if (brush)
  {
    brush->ChangeBrush();

    HBRUSH b = 0;
//    if (brush->cbrush) // JACS change 21/6/94 to make it select a NULL brush
                         // for wxTRANSPARENT
      b = brush->SelectBrush(dc) ;
    if (!old_brush)
      old_brush = b;
  }
  DoneDC(dc);
}

/* MATTHEW: [2] 16-bit flag */
void wxDC::DrawText(const char *text, float x, float y, Bool use16bit)
{
  HDC dc = ThisDC();

  if (!dc) return;



  int xx1, yy1;

  

  ShiftXY(x, y, xx1, yy1);

  
  
  if (font && font->cfont)
  {
#if DEBUG > 1
    wxDebugMsg("wxDC::DrawText: Selecting HFONT %X\n", font->cfont);
#endif
    HFONT f = ::SelectObject(dc, font->cfont);
    if (!old_font)
      old_font = f;
  }

  if (current_text_foreground.Ok())
    SetTextColor(dc, current_text_foreground.pixel) ;

  DWORD old_background;
  if (current_text_background.Ok())
  {
    old_background = SetBkColor(dc, current_text_background.pixel) ;
  }

  if (current_bk_mode == wxTRANSPARENT)
    SetBkMode(dc, TRANSPARENT);
  else
    SetBkMode(dc, OPAQUE);

  (void)TextOut(dc, XLOG2DEV(xx1), YLOG2DEV(yy1), text, strlen(text));

  if (current_text_background.Ok())
    (void)SetBkColor(dc, old_background);

  DoneDC(dc);
  
  CalcBoundingBox((float)x, (float)y);

  float w, h;
  GetTextExtent(text, &w, &h);
  CalcBoundingBox((float)(x + w), (float)(y + h));
}

void wxDC::SetBackground(wxBrush *brush)
{
  if (current_background_brush) current_background_brush->Lock(-1);
  current_background_brush = brush;
  if (current_background_brush) current_background_brush->Lock(1);

  if (!brush)
    return;

  brush->ChangeBrush() ;
  if (canvas)
  {
    wxCanvasWnd *wnd = (wxCanvasWnd *)canvas->handle;
    //wnd->background_brush = brush->cbrush;
    HBRUSH br = (brush->GetStyle()==wxTRANSPARENT) ?
                            GetStockObject(NULL_BRUSH) : brush->cbrush;
    // Remember we don't want to delete this brush when we change the
    // background again or delete the window.
    wnd->SetBackgroundBrush(br, FALSE);
    if (brush->GetStyle()==wxTRANSPARENT)
    {
      wnd->background_transparent = TRUE;
    }
    else
    {
      wnd->background_colour = RGB(brush->GetColour().Red(), brush->GetColour().Green(), brush->GetColour().Blue());
      wnd->background_transparent = FALSE;
    }
  }
  
  HDC dc = ThisDC() ;

  if (current_background_brush && dc)
  {
  COLORREF new_color = current_background_brush->colour.pixel ;
    if (new_color!=cur_bk || dc!=cur_dc)
    {
      (void)SetBkColor(dc, new_color);
      cur_bk = new_color ;
      cur_dc = dc ;
    }
  }

  DoneDC(dc) ;
}

void wxDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;

  HDC dc = ThisDC();

  if (!dc) return;

  if (current_bk_mode == wxTRANSPARENT)
    ::SetBkMode(dc, TRANSPARENT);
  else
    ::SetBkMode(dc, OPAQUE);

  DoneDC(dc);
}

void wxDC::SetLogicalFunction(int function)
{
  current_logical_function = function;

  HDC dc = ThisDC();

  if (dc) SetRop(dc);

  DoneDC(dc);
}

void wxDC::SetRop(HDC dc)
{
  if (!dc || current_logical_function < 0)
    return;

  int c_rop;
  // These may be wrong
  switch (current_logical_function)
  {
//    case wxXOR: c_rop = R2_XORPEN; break;
    case wxXOR: c_rop = R2_NOTXORPEN; break;
    case wxINVERT: c_rop = R2_NOT; break;
    case wxOR_REVERSE: c_rop = R2_MERGEPENNOT; break;
    case wxAND_REVERSE: c_rop = R2_MASKPENNOT; break;
    case wxCLEAR: c_rop = R2_WHITE; break;
    case wxSET: c_rop = R2_BLACK; break;
    case wxSRC_INVERT: c_rop = R2_NOTCOPYPEN; break;
    case wxOR_INVERT: c_rop = R2_MERGENOTPEN; break;
    case wxAND: c_rop = R2_MASKPEN; break;
    case wxOR: c_rop = R2_MERGEPEN; break;
    case wxAND_INVERT: c_rop = R2_MASKNOTPEN; break;
    case wxEQUIV:
    case wxNAND:
    case wxCOPY:
    default:
      c_rop = R2_COPYPEN; break;
  }
  SetROP2(dc, c_rop);
}

Bool wxDC::StartDoc(char *message)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return TRUE;
    
  Bool flag = FALSE;

  DOCINFO docinfo;
  docinfo.cbSize = sizeof(DOCINFO);
  docinfo.lpszDocName = message;
  docinfo.lpszOutput = filename;
  if (cdc) flag = (SP_ERROR != ::StartDoc(cdc, &docinfo));
  else flag = FALSE;

  return flag;
}

void wxDC::EndDoc(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc) ::EndDoc(cdc);
}

void wxDC::StartPage(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc)
    ::StartPage(cdc);
}

void wxDC::EndPage(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc)
    ::EndPage(cdc);
}

float wxDC::GetCharHeight(void)
{
  TEXTMETRIC lpTextMetric;
  
  HDC dc = ThisDC();

  if (!dc) return 10;

	  

  GetTextMetrics(dc, &lpTextMetric);

  DoneDC(dc);

  return (float)YDEV2LOGREL(lpTextMetric.tmHeight);
}

float wxDC::GetCharWidth(void)
{
  TEXTMETRIC lpTextMetric;
  HDC dc = ThisDC();


  if (!dc) return 5;


  GetTextMetrics(dc, &lpTextMetric);

  DoneDC(dc);

  return (float)XDEV2LOGREL(lpTextMetric.tmAveCharWidth);
}

/* MATTHEW: [2] 16-bit flag */
void wxDC::GetTextExtent(const char *string, float *x, float *y,
                         float *descent, float *externalLeading, 
			 wxFont *theFont, Bool use16bt)
{
  wxFont *oldFont = NULL;
  if (theFont) {
	oldFont = font;
    SetFont(theFont);
  }

  HDC dc = ThisDC();

  if (!dc) {

	  *x = 5;

	  *y = 10;

	  if (descent) *descent = 0;

	  if (externalLeading) *externalLeading= 0;

	  return;

  }
  
  SIZE sizeRect;
  TEXTMETRIC tm;

  GetTextExtentPoint(dc, string, strlen(string), &sizeRect);
  GetTextMetrics(dc, &tm);

  DoneDC(dc);

  *x = (float)XDEV2LOGREL(sizeRect.cx);
  *y = (float)YDEV2LOGREL(sizeRect.cy);
  if (descent) *descent = (float)tm.tmDescent;
  if (externalLeading) *externalLeading = (float)tm.tmExternalLeading;

  if (oldFont)
    SetFont(oldFont);
}

void wxDC::SetMapMode(int mode)
{
  mapping_mode = mode;

  int pixel_width = 0;
  int pixel_height = 0;
  int mm_width = 0;
  int mm_height = 0;

  HDC dc = ThisDC() ;


  if (!dc) return;


  pixel_width = GetDeviceCaps(dc, HORZRES);
  pixel_height = GetDeviceCaps(dc, VERTRES);
  mm_width = GetDeviceCaps(dc, HORZSIZE);
  mm_height = GetDeviceCaps(dc, VERTSIZE);

  if ((pixel_width == 0) || (pixel_height == 0) || (mm_width == 0) || (mm_height == 0))
  {
    DoneDC(dc) ;
    return;
  }

  float mm2pixelsX = (float)pixel_width/mm_width;
  float mm2pixelsY = (float)pixel_height/mm_height;

  switch (mode)
  {
    case MM_TWIPS:
    {
      logical_scale_x = (float)(twips2mm * mm2pixelsX);
      logical_scale_y = (float)(twips2mm * mm2pixelsY);
      break;
    }
    case MM_POINTS:
    {
      logical_scale_x = (float)(pt2mm * mm2pixelsX);
      logical_scale_y = (float)(pt2mm * mm2pixelsY);
      break;
    }
    case MM_METRIC:
    {
      logical_scale_x = mm2pixelsX;
      logical_scale_y = mm2pixelsY;
      break;
    }
    case MM_LOMETRIC:
    {
      logical_scale_x = (float)(mm2pixelsX/10.0);
      logical_scale_y = (float)(mm2pixelsY/10.0);
      break;
    }
    default:
    case MM_TEXT:
    {
      logical_scale_x = 1.0;
      logical_scale_y = 1.0;
      break;
    }
  }

  if (::GetMapMode(dc) != MM_ANISOTROPIC)
    ::SetMapMode(dc, MM_ANISOTROPIC);
    
  SetViewportExtEx(dc, VIEWPORT_EXTENT, VIEWPORT_EXTENT, NULL);
  window_ext_x = (int)MS_XDEV2LOG(VIEWPORT_EXTENT);
  window_ext_y = (int)MS_YDEV2LOG(VIEWPORT_EXTENT);
  ::SetWindowExtEx(dc, window_ext_x, window_ext_y, NULL);
  ::SetViewportOrgEx(dc, (int)device_origin_x, (int)device_origin_y, NULL);
  ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);

  DoneDC(dc) ;
}

void wxDC::SetUserScale(float x, float y)
{
  user_scale_x = x;
  user_scale_y = y;

  SetMapMode(mapping_mode);
}

void wxDC::SetSystemScale(float x, float y)
{
  system_scale_x = x;
  system_scale_y = y;

  SetMapMode(mapping_mode);
}

void wxDC::SetLogicalOrigin(float x, float y)
{
  logical_origin_x = x;
  logical_origin_y = y;

  HDC dc = ThisDC() ;

  if (dc) ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);

  DoneDC(dc) ;
}

void wxDC::SetDeviceOrigin(float x, float y)
{
  device_origin_x = x;
  device_origin_y = y;
  HDC dc = ThisDC() ;

  if (dc) ::SetViewportOrgEx(dc, (int)device_origin_x, (int)device_origin_y, NULL);

  DoneDC(dc);
}

float wxDC::DeviceToLogicalX(int x)
{
  return (float)MS_XDEV2LOG(x);
}

float wxDC::DeviceToLogicalXRel(int x)
{
  return (float)MS_XDEV2LOGREL(x);
}

float wxDC::DeviceToLogicalY(int y)
{
  return (float)MS_YDEV2LOG(y);
}

float wxDC::DeviceToLogicalYRel(int y)
{
  return (float)MS_YDEV2LOGREL(y);
}

int wxDC::LogicalToDeviceX(float x)
{
  return MS_XLOG2DEV(x);
}

int wxDC::LogicalToDeviceXRel(float x)
{
  return MS_XLOG2DEVREL(x);
}

int wxDC::LogicalToDeviceY(float y)
{
  return MS_YLOG2DEV(y);
}

int wxDC::LogicalToDeviceYRel(float y)
{
  return MS_YLOG2DEVREL(y);
}

Bool wxDC::Blit(float xdest, float ydest, float width, float height,
                wxCanvasDC *source, float xsrc, float ysrc, int rop)
{
  wxWnd *wnd = NULL;
  wxWnd *wnd_src = NULL;
  if (canvas)
    wnd = (wxWnd *)canvas->handle;
  if (source->canvas)
    wnd_src = (wxWnd *)source->canvas->handle;

  HDC dc = ThisDC();

  if (!dc) return FALSE;



  int xdest1, ydest1, xsrc1, ysrc1;



  ShiftXY(xdest, ydest, xdest1, ydest1);

  source->ShiftXY(xsrc, ysrc, xsrc1, ysrc1);

  HDC dc_src = source->ThisDC();


  if (!dc_src) return FALSE;


  xdest1 = XLOG2DEV(xdest1);
  ydest1 = YLOG2DEV(ydest1);
  
  Bool special_op = FALSE;
  Bool success;

  DWORD op = 0;

  wxBrush *tmpbrush = NULL, *savebrush = NULL;



  if (rop == wxCOLOR) {

    if (current_pen) {

      wxColour c = current_pen->GetColour();

      if (c.Red() || c.Green() || c.Blue()) {

	tmpbrush = wxTheBrushList->FindOrCreateBrush(&c, wxSOLID);

	if (tmpbrush) {

	  op = 0x00FC008A;

	  special_op = TRUE;

	  savebrush = current_brush;

	  SetBrush(tmpbrush);

	}

      }

    }

  } 

    

  if (!special_op) {

    op = rop == wxCOPY ? SRCCOPY :

                rop == wxCLEAR ? WHITENESS :

                rop == wxSET ? BLACKNESS :

                rop == wxINVERT ? DSTINVERT :

                rop == wxAND ? MERGECOPY :

                rop == wxOR ? MERGEPAINT :

                rop == wxSRC_INVERT ? NOTSRCCOPY :

                rop == wxXOR ? SRCINVERT :

                rop == wxOR_REVERSE ? MERGEPAINT :

                rop == wxAND_REVERSE ? SRCERASE :

                rop == wxSRC_OR ? SRCPAINT :

                rop == wxSRC_AND ? SRCAND :

                rop == wxCOLOR ? SRCCOPY :

                SRCCOPY;		

  }



  success = BitBlt(dc, xdest1, ydest1, 

		       XLOG2DEVREL(width), YLOG2DEVREL(height), 

		       dc_src, xsrc1, ysrc1, op);


  if (special_op) {

    SetBrush(savebrush);

  }


  DoneDC(dc);

  source->DoneDC(dc_src);
  

  return success;
}

void wxDC::GetSize(float *width, float *height)
{
  HDC dc = ThisDC();

  if (!dc) {

	  *width = *height = 0;

	  return;

  }

  int w=::GetDeviceCaps(dc,HORZRES);
  int h=::GetDeviceCaps(dc,VERTRES);
  *width = (float)MS_XDEV2LOGREL(w);
  *height = (float)MS_YDEV2LOGREL(h);

  DoneDC(dc);
}

void wxDC::GetSizeMM(float *width, float *height)
{
  HDC dc = ThisDC();

  if (!dc) {

	  *width = *height = 0;

	  return;

  }

  int w=::GetDeviceCaps(dc,HORZSIZE);
  int h=::GetDeviceCaps(dc,VERTSIZE);
  *width = (float)w;
  *height = (float)h;

  DoneDC(dc);
}

IMPLEMENT_DYNAMIC_CLASS(wxCanvasDC, wxDC)

wxCanvasDC::wxCanvasDC(void)
{
  __type = wxTYPE_DC_CANVAS;
  device = wxDEVICE_WINDOWS;
}

wxCanvasDC::wxCanvasDC(wxCanvas *the_canvas):wxbCanvasDC(the_canvas)
{
  __type = wxTYPE_DC_CANVAS;
  canvas = the_canvas;
  WXGC_IGNORE(canvas);
  device = wxDEVICE_WINDOWS;
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  /* MATTHEW: [11] */
  if (canvas)
    cdc = ((wxWnd *)canvas->handle)->GetHDC();
}

wxCanvasDC::~wxCanvasDC(void)
{
  /* MATTHEW: [11] */
  if (canvas)
    ((wxWnd *)canvas->handle)->ReleaseHDC();
}

void wxCanvasDC::GetClippingBox(float *x,float *y,float *w,float *h)
{
  if (clipping)
  {
    *x = (float)clip_x1 ;
    *y = (float)clip_y1 ;
    *w = (float)(clip_x2 - clip_x1) ;
    *h = (float)(clip_y2 - clip_y1) ;
  }
  else
   *x = *y = *w = *h = 0 ;
}

/* MATTHEW: [8] */
void wxCanvasDC:: GetClippingRegion(float *x, float *y, float *w, float *h)
{
  if (!clipping) {
    *x = *y = 0;
    *w = *h = -1;
  } else
    GetClippingBox(x, y, w, h);
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  COLORREF result;

  HDC dc = ThisDC();
  if (!dc) {
    dest->Set(0, 0, 0);
    return;
  }

  result = GetNearestColor(dc, RGB(src->Red(), src->Blue(), src->Green()));
  dest->Set(GetRValue(result), GetGValue(result), GetBValue(result));
  DoneDC(dc);
}

IMPLEMENT_CLASS(wxPrinterDC, wxDC)

static BOOL DoPrintDlg(PRINTDLG *pd, HWND parent)
{
  if (!pd->hwndOwner)
    pd->hwndOwner = parent;

  return PrintDlg(pd);
}

wxPrinterDC::wxPrinterDC(char *driver_name, char *device_name, char *file, Bool interactive)
{
  __type = wxTYPE_DC_PRINTER;
  wx_interactive = interactive;
  device = wxDEVICE_WINDOWS;

  if (file)
    filename = copystring(file);
  else filename = NULL;

#if USE_COMMON_DIALOGS
  if (interactive) {
    PRINTDLG *pd = new PRINTDLG;
    
    pd->lStructSize = sizeof( PRINTDLG );
    pd->hwndOwner=NULL;
    pd->hDevMode=(HANDLE)NULL;
    pd->hDevNames=(HANDLE)NULL;
    pd->Flags=PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
    pd->nFromPage=0;
    pd->nToPage=0;
    pd->nMinPage=0;
    pd->nMaxPage=0;
    pd->nCopies=1;
    pd->hInstance=(HINSTANCE)NULL;
    
    if ( wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0) != 0 ) {
      cdc = pd->hDC;
      ok = TRUE;
    } else {
      ok = FALSE;
      return;
    }
    
    dont_delete = TRUE; // ??? WHY???
  } else
#endif
  if (driver_name && device_name && file) {
    cdc = CreateDC(driver_name, device_name, file, NULL);
    ok = cdc ? TRUE : FALSE;
  } else {
    cdc = wxGetPrinterDC();
    ok = cdc ? TRUE : FALSE;
  }
  
  if (cdc) {
    // int width = GetDeviceCaps(cdc, VERTRES);
    // int height = GetDeviceCaps(cdc, HORZRES);
    SetMapMode(MM_POINTS);
  }

  SetBrush(wxBLACK_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxPrinterDC::wxPrinterDC(HDC theDC)
{
  __type = wxTYPE_DC_PRINTER;
  wx_interactive = FALSE;
  device = wxDEVICE_WINDOWS;

  filename = NULL;

  cdc = theDC;
  ok = TRUE;

  if (cdc) {
    // int width = GetDeviceCaps(cdc, VERTRES);
    // int height = GetDeviceCaps(cdc, HORZRES);
    SetMapMode(MM_POINTS);
  }

  SetBrush(wxBLACK_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxPrinterDC::~wxPrinterDC(void)
{
}

HDC wxGetPrinterDC(void)
{
#if USE_COMMON_DIALOGS
    HDC         hDC;
    LPDEVMODE   lpDevMode = NULL;
    LPDEVNAMES  lpDevNames;
    LPSTR       lpszDriverName;
    LPSTR       lpszDeviceName;
    LPSTR       lpszPortName;

    PRINTDLG    *pd = new PRINTDLG;
    pd->lStructSize    = sizeof(PRINTDLG);
    pd->hwndOwner      = (HWND)NULL;
    pd->hDevMode       = NULL;
    pd->hDevNames      = NULL;
    pd->Flags          = PD_RETURNDEFAULT;
    pd->nCopies        = 1;

    if (!wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0))
      return NULL;

    if (!pd->hDevNames)
        return(NULL);

    lpDevNames = (LPDEVNAMES)GlobalLock(pd->hDevNames);
    lpszDriverName = (LPSTR)lpDevNames + lpDevNames->wDriverOffset;
    lpszDeviceName = (LPSTR)lpDevNames + lpDevNames->wDeviceOffset;
    lpszPortName   = (LPSTR)lpDevNames + lpDevNames->wOutputOffset;
    GlobalUnlock(pd->hDevNames);

    if (pd->hDevMode)
        lpDevMode = (LPDEVMODE)GlobalLock(pd->hDevMode);

#ifdef WIN32
    hDC = CreateDC(lpszDriverName, lpszDeviceName, lpszPortName, (DEVMODE *)lpDevMode);
#else
    hDC = CreateDC(lpszDriverName, lpszDeviceName, lpszPortName, (LPSTR)lpDevMode);
#endif

    if (pd->hDevMode && lpDevMode)
        GlobalUnlock(pd->hDevMode);

    if (pd->hDevNames)
    {
	GlobalFree(pd->hDevNames);
	pd->hDevNames=NULL;
    }
    if (pd->hDevMode)
    {
       GlobalFree(pd->hDevMode);
       pd->hDevMode=NULL;
    }
    return(hDC);
#else
  return 0;
#endif
}

/*
 * Memory DC
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxMemoryDC, wxCanvasDC)

wxMemoryDC::wxMemoryDC(void)
{
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_WINDOWS;

  cdc = wxwmCreateCompatibleDC(NULL);
  ok = (cdc != NULL);

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::wxMemoryDC(wxCanvasDC *old_dc):wxbMemoryDC(old_dc)
{
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_WINDOWS;

  wxWnd *wnd = NULL;
  if (old_dc->canvas)
	 wnd = (wxWnd *)old_dc->canvas->handle;

  HDC dc = NULL;
  if (old_dc->cdc)
	 dc = old_dc->cdc;
  else if (wnd)
	 dc = wnd->GetHDC() ;

  cdc = wxwmCreateCompatibleDC(dc);
  ok = (cdc != NULL);

  if (!old_dc->cdc && wnd)
	 wnd->ReleaseHDC() ;

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::~wxMemoryDC(void)
{
	if (selected_bitmap)
		SelectObject(NULL);
}

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  if (bitmap == selected_bitmap)
    return;

  if (!bitmap)
  {
	 // Selecting nothing means, select old bitmap
    // out of the device context (e.g. so it can be deleted)
    if (old_bitmap)
    {
      ::SelectObject(cdc, old_bitmap);
      if (selected_bitmap)
      {
        selected_bitmap->selectedInto = NULL;

	selected_bitmap->selectedIntoDC = 0;
        selected_bitmap = NULL;
      }
    }

    if (old_palette) {

      SelectPalette(cdc, old_palette, TRUE);

      old_palette = NULL;

    }
    return;
  }

  // Do own check for whether the bitmap is already selected into
  // a device context
  if (bitmap->selectedIntoDC || !bitmap->Ok())
  {
    // wxFatalError("Error in wxMemoryDC::SelectObject\nBitmap is selected in another wxMemoryDC.\nDelete the first wxMemoryDC or use SelectObject(NULL)");
    return;
  }



  if (selected_bitmap) {

	  selected_bitmap->selectedInto = NULL;

	  selected_bitmap->selectedIntoDC = 0;

  }
  
  selected_bitmap = bitmap;
  bitmap->selectedInto = this;

  bitmap->selectedIntoDC = -1;
#if DEBUG > 1
  wxDebugMsg("wxMemoryDC::SelectObject: Selecting HBITMAP %X\n", bitmap->ms_bitmap);
#endif
  HBITMAP bm = ::SelectObject(cdc, bitmap->ms_bitmap);

  if (bm == ERROR)
  {
    // wxFatalError("Error in wxMemoryDC::SelectObject\nBitmap may not be loaded, or may be selected in another wxMemoryDC.\nDelete the first wxMemoryDC to deselect bitmap.");
   selected_bitmap = NULL;
   bitmap->selectedInto = NULL;
   bitmap->selectedIntoDC = 0;

   if (old_bitmap) {

	   ::SelectObject(cdc, old_bitmap);

	   old_bitmap = NULL;

   }

   bitmap = NULL;

  }
  else if (!old_bitmap)
    old_bitmap = bm;



  wxColourMap *cm = (bitmap ? bitmap->GetColourMap() : NULL);

  if (cm && cm->ms_palette) {

    HPALETTE p;

    p = SelectPalette(cdc, cm->ms_palette, TRUE);

    if (p) {

      RealizePalette(cdc);

      old_palette = p;

    }

  } else if (old_palette) {

    SelectPalette(cdc, old_palette, TRUE);

    RealizePalette(cdc);

    old_palette = NULL;

  }
}

void wxMemoryDC::GetSize(float *width, float *height)
{
  if (!selected_bitmap)
  {
    *width = 0.0; *height = 0.0;
    return;
  }
  *width = selected_bitmap->GetWidth();
  *height = selected_bitmap->GetHeight();
}

/*
 * Panel device context
 */
 
IMPLEMENT_DYNAMIC_CLASS(wxPanelDC, wxCanvasDC)

wxPanelDC::wxPanelDC(void)
{
  __type = wxTYPE_DC_PANEL;
  panel = NULL;
}

wxPanelDC::wxPanelDC(wxPanel *the_panel):wxbPanelDC(the_panel)
{
  panel = the_panel;
  WXGC_IGNORE(panel);
}

wxPanelDC::~wxPanelDC(void)
{
}


// Create a DC representing the whole screen
IMPLEMENT_DYNAMIC_CLASS(wxScreenDC, wxCanvasDC)

wxScreenDC::wxScreenDC(void)
{
  __type = wxTYPE_DC_CANVAS; // ?
  cdc = wxwmGetDC(NULL);
}

wxScreenDC::~wxScreenDC(void)
{
  wxwmReleaseDC(NULL, cdc);
}

