/*
 * File:	wx_dc.cc
 * Purpose:	Device context implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "../../../wxcommon/Region.h"
#include "wx_pdf.h"

#include <math.h>

#if USE_COMMON_DIALOGS
# include <commdlg.h>
#endif

// Declarations local to this file

static wxMemoryDC *blit_dc;

#define YSCALE(y) (yorigin - (y))

static HANDLE null_brush;
static HANDLE null_pen;

// Default constructor
wxDC::wxDC(void)
{
  __type = wxTYPE_DC;
  filename = NULL;
  selected_bitmap = NULL;
  canvas = NULL;
  cur_dc = NULL;
  cur_bk = 0;
  old_bitmap = 0;
  old_pen = 0;
  old_brush = 0;
  old_font = 0;
  old_palette = 0;
  cur_rop = -1;
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
  clipping = NULL;
  screen_font = TRUE;
  ok = TRUE;
  window_ext_x = VIEWPORT_EXTENT;
  window_ext_y = VIEWPORT_EXTENT;
  current_pen = NULL;
  current_brush = NULL;
  current_background_color = new wxColour(wxWHITE);
  current_text_foreground = new wxColour(wxBLACK);
  current_text_background = new wxColour(wxWHITE);
  current_bk_mode = wxTRANSPARENT;
  Colour = wxColourDisplay();

  null_pen = ::GetStockObject(NULL_PEN);
  null_brush = ::GetStockObject(NULL_BRUSH);
}


wxDC::~wxDC(void)
{
  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);

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
  if (dc)
  {
    if (old_bitmap) {
      ::SelectObject(dc, old_bitmap);
      if (selected_bitmap) {
        selected_bitmap->selectedInto = NULL;
        selected_bitmap->selectedIntoDC = 0;
      }
      selected_bitmap = NULL;
    }
    old_bitmap = NULL;

    if (old_pen) {
      ::SelectObject(dc, old_pen);
    }
    old_pen = NULL;

    if (old_brush) {
      ::SelectObject(dc, old_brush);
    }
    old_brush = NULL;

    if (old_font) {
      ::SelectObject(dc, old_font);
    }
    old_font = NULL;

    if (old_palette) {
      ::SelectPalette(dc, old_palette, TRUE);
    }
    old_palette = NULL;
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

  if (!old_pen) {
    old_pen = (HPEN)::SelectObject(dc, null_pen);
    old_brush = (HBRUSH)::SelectObject(dc, null_brush);
  }

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
  ix = (int)floor(x);
  iy = (int)floor(y);

  if (canvas) {
    wxWnd *wnd = (wxWnd *)canvas->handle;
    wnd->CalcScrolledPosition((int)x, (int)y, &ix, &iy);
  }
}

void wxDC::SetClippingRect(float cx, float cy, float cw, float ch)
{
  if (clipping) delete clipping;

  clipping = new wxRegion(this);
  clipping->SetRectangle(cx, cy, cw, ch);

  HDC dc = ThisDC();
  if (dc) DoClipping(dc);
  DoneDC(dc);
}

wxRegion* wxDC::GetClippingRegion()
{
  if (clipping)
    return new wxRegion(this, clipping);
  else
    return NULL;
}

void wxDC::SetClippingRegion(wxRegion *c)
{
  if (c && (c->dc != this)) return;

  if (clipping) delete clipping;

  if (c)
    clipping = new wxRegion(this, c);
  else
    clipping = NULL;

  HDC dc = ThisDC();
  if (dc) DoClipping(dc);
  DoneDC(dc);
}

void wxDC::DoClipping(HDC dc)
{
  if (clipping) {
    SelectClipRgn(dc, clipping->rgn);
  } else {
    HRGN rgn = CreateRectRgn(0, 0, 32000, 32000);
    SelectClipRgn(dc, rgn);
    DeleteObject(rgn);
  }
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

  if (!cmap) {
    // Setting a NULL colourmap is a way of restoring
    // the original colourmap
    if (old_palette) {
      ::SelectPalette(dc, old_palette, TRUE);
      old_palette = 0;
    }
  }
    
  if (cmap && cmap->ms_palette) {
    HPALETTE oldPal = ::SelectPalette(dc, cmap->ms_palette, TRUE);
    if (!old_palette)
      old_palette = oldPal;
      
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
  else if (selected_bitmap) {
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
  
  DoneDC(dc);

  SetMapMode(mapping_mode);
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

  SetBrush(current_brush);

  (void)ExtFloodFill(dc, (int)XLOG2DEV(xx), (int)YLOG2DEV(yy),
		     col->pixel,
		     (style == wxFLOOD_SURFACE
		      ? FLOODFILLSURFACE
		      : FLOODFILLBORDER));

  DoneDC(dc);

  CalcBoundingBox((float)x, (float)y);
}

Bool wxDC::GetPixel(float x, float y, wxColour *col)
{
  HDC dc = ThisDC();

  if (!dc) return FALSE;

  int xx1;
  int yy1;

  ShiftXY(x, y, xx1, yy1);
  
  // get the color of the pixel
  COLORREF pixelcolor = ::GetPixel(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1));
  
  DoneDC(dc);

  // return the color of the pixel
  if (col)
    col->Set(GetRValue(pixelcolor),GetGValue(pixelcolor),GetBValue(pixelcolor));
  
  return TRUE;
}

void wxDC::IntDrawLine(int x1, int y1, int x2, int y2)
{
  DrawLine(x1, y1, x2, y2);
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

  if (StartPen(dc)) {
    (void)MoveToEx(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy), NULL);
    (void)LineTo(dc, (int)XLOG2DEV(xx2), (int)YLOG2DEV(yy));
    
    (void)MoveToEx(dc, (int)XLOG2DEV(xx), (int)YLOG2DEV(yy1), NULL);
    (void)LineTo(dc, (int)XLOG2DEV(xx), (int)YLOG2DEV(yy2));

    DonePen(dc);
  }

  DoneDC(dc);
  
  CalcBoundingBox((float)x - 2000, (float)y - 2000);
  CalcBoundingBox((float)x + 2000, (float)y + 2000);
}

void wxDC::DrawLine(float x1, float y1, float x2, float y2)
{
  HDC dc = ThisDC();

  if (!dc) return;

  int xx1, yy1, xx2, yy2;

  if (StartPen(dc)) {
    ShiftXY(x1, y1, xx1, yy1);
    ShiftXY(x2, y2, xx2, yy2);
    
    /* Convention across platforms: line includes pixel on endpoint */
    int pw = current_pen->GetWidth();
    int forward = 0;
    if (!pw)
      forward = 1;
    else if (pw == 1) {
      if (current_pen->GetCap() != wxCAP_BUTT) {
	/* Pen size 1: no need to forward under NT */
	static int nt = -1;
	if (nt < 0) {
	  OSVERSIONINFO info;
	  info.dwOSVersionInfoSize = sizeof(info);
	  GetVersionEx(&info);
	  if (info.dwPlatformId == VER_PLATFORM_WIN32_NT)
	    nt = 1;
	  else
	    nt = 0;
	}
	forward = !nt;
      } else
	forward = 0;
    }
    if (forward) {
      int dx = (xx2 - xx1);
      int dy = (yy2 - yy1);

      if (!dx && !dy) {
	xx2++;
      } else {
	int adx = ((dx < 0) ? -dx : dx);
	int ady = ((dy < 0) ? -dy : dy);

	if (ady >= adx) {
	  if (yy1 < yy2)
	    yy2++;
	  else
	    --yy2;
	}
	
	if (adx >= ady) {
	  if (xx1 < xx2)
	    xx2++;
	  else
	    --xx2;
	}
      }
    }

    (void)MoveToEx(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1), NULL);
    (void)LineTo(dc, (int)XLOG2DEV(xx2), (int)YLOG2DEV(yy2));

    DonePen(dc);
  }

  DoneDC(dc);

  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
}

static void FillWithStipple(wxDC *dc, wxRegion *r, wxBrush *brush)
{
  float x, y, w, h, bw, bh;
  int xstart, xend, ystart, yend, i, j;
  wxRegion *old;

  wxBitmap *bm = brush->GetStipple();
  int style = brush->GetStyle();
  wxColour *c = brush->GetColour();

  old = dc->GetClippingRegion();
  if (old) r->Intersect(old);

  r->BoundingBox(&x, &y, &w, &h);
  bw = bm->GetWidth();
  bh = bm->GetHeight();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  w = dc->LogicalToDeviceXRel(w);
  h = dc->LogicalToDeviceYRel(h);
  
  xstart = floor(x / bw);
  xend = floor((x + w + bw - 0.00001) / bw);

  ystart = floor(y / bh);
  yend = floor((y + h + bh - 0.00001) / bh);

  dc->SetClippingRegion(r);

  for (i = xstart; i < xend; i++)
    for (j = ystart; j < yend; j++)
      dc->Blit(dc->DeviceToLogicalX(i * bw), 
	       dc->DeviceToLogicalY(j * bh), 
	       dc->DeviceToLogicalXRel(bw), 
	       dc->DeviceToLogicalYRel(bh),
	       bm, 0, 0, style, c);

  dc->SetClippingRegion(old);
}

static int round(float f)
{
  double d;
  
  (void)modf(f, &d);

  return (int)d;
}

void wxDC::DrawArc(float x, float y, float w, float h, float start, float end)
{
  int xx1, yy1, xx2, yy2, hh, ww;
  float cx, cy;
  float rx1, ry1, rx2, ry2;

  HDC dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r = new wxRegion(this);
    r->SetArc(x, y, w, h, start, end);
    FillWithStipple(this, r, current_brush);
  }

  ShiftXY(x, y, xx1, yy1);
  ShiftXY(x + w, y + h, xx2, yy2);
  hh = yy2 - yy1;
  ww = xx2 - xx1;
  
  /* Adjust w & h for Windows conventions: */
  hh++; xx2++;
  ww++; yy2++;

  cx = xx1 + (float)ww/2;
  cy = yy1 + (float)hh/2;

  rx1 = cx + ((float)ww / 2) * cos(start);
  ry1 = cy - (((float)hh / 2) * sin(start));
  rx2 = cx + ((float)ww / 2) * cos(end);
  ry2 = cy - (((float)hh / 2) * sin(end));

  if (StartBrush(dc, 1)) {
    Pie(dc, xx1, yy1, xx2, yy2, 
	round(rx1), round(ry1), 
	round(rx2), round(ry2));
    DoneBrush(dc);
  }

  if (StartPen(dc)) {
    Arc(dc, xx1, yy1, xx2, yy2, 
	round(rx1), round(ry1), 
	round(rx2), round(ry2));
    DonePen(dc);
  }
  
  DoneDC(dc);
  
  CalcBoundingBox(x, y);
  CalcBoundingBox(x + w, y + h);
}

void wxDC::DrawPoint(float x, float y)
{
  if (current_pen)
    SetPixel(x, y, current_pen->GetColour());
}

void wxDC::SetPixel(float x, float y, wxColour *c)
{
  HDC dc = ThisDC();

  if (!dc) return;

  wxWnd *wnd = NULL;

  int xx1, yy1;

  ShiftXY(x, y, xx1, yy1);
  
  ::SetPixelV(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1), c->pixel);

  DoneDC(dc);

  CalcBoundingBox(x, y);
}

void wxDC::DrawPolygon(int n, wxPoint points[], float xoffset, float yoffset,int fillStyle)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r = new wxRegion(this);
    r->SetPolygon(n, points, xoffset, yoffset, fillStyle);
    FillWithStipple(this, r, current_brush);
  }

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

  int prev = SetPolyFillMode(dc, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);

  if (StartBrush(dc, 1)) {
    (void)Polygon(dc, cpoints, n);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Polygon(dc, cpoints, n);
    DonePen(dc);
  }

  SetPolyFillMode(dc, prev);

  DoneDC(dc);
}

void wxDC::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (StartPen(dc)) {
    int xoffset1;
    int yoffset1;
    
    ShiftXY(xoffset, yoffset, xoffset1, yoffset1);
    
    POINT *cpoints = new POINT[n];
    int i;
    for (i = 0; i < n; i++) {
      cpoints[i].x = (int)(XLOG2DEV(points[i].x + xoffset1));
      cpoints[i].y = (int)(YLOG2DEV(points[i].y + yoffset1));
      
      CalcBoundingBox((float)points[i].x + xoffset, (float)points[i].y + yoffset);
    }
    
    (void)Polyline(dc, cpoints, n);

    DonePen(dc);

    delete[] cpoints;
  }

  DoneDC(dc);

}

void wxDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (StartPen(dc)) {
    int xoffset1;
    int yoffset1;
    
    ShiftXY(xoffset, yoffset, xoffset1, yoffset1);
    
    POINT *cpoints = new POINT[n];
    int i;
    for (i = 0; i < n; i++) {
      cpoints[i].x = (int)(XLOG2DEV(points[i].x + xoffset1));
      cpoints[i].y = (int)(YLOG2DEV(points[i].y + yoffset1));
      
      CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
    }
    
    (void)Polyline(dc, cpoints, n);

    DonePen(dc);

    delete[] cpoints;
  }

  DoneDC(dc);
  
}

void wxDC::DrawRectangle(float x, float y, float width, float height)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r = new wxRegion(this);
    r->SetRectangle(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  int x1, y1, x2, y2;

  ShiftXY(x, y, x1, y1);
  ShiftXY(x + width, y + height, x2, y2);

  Bool do_brush, do_pen;

  if (StartBrush(dc, 1)) {
    (void)Rectangle(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1),
		    (int)XLOG2DEV(x2) + 1, (int)YLOG2DEV(y2) + 1);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Rectangle(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1),
		    (int)XLOG2DEV(x2), (int)YLOG2DEV(y2));
    DonePen(dc);
  }

  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);

  DoneDC(dc);
}

void wxDC::DrawRoundedRectangle(float x, float y, float width, float height, float radius)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r = new wxRegion(this);
    r->SetRoundedRectangle(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  int x1, y1, x2, y2;
  ShiftXY(x, y, x1, y1);
  ShiftXY(x + width, y + height, x2, y2);

  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    float smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (float)(- radius * smallest);
  }

  if (StartBrush(dc, 1)) {
    (void)RoundRect(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1), (int)XLOG2DEV(x2) + 1,
		    (int)YLOG2DEV(y2) + 1, (int)XLOG2DEV(radius), (int)YLOG2DEV(radius));
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)RoundRect(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1), (int)XLOG2DEV(x2),
		    (int)YLOG2DEV(y2), (int)XLOG2DEV(radius), (int)YLOG2DEV(radius));
    DonePen(dc);
  }
  
  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);

  DoneDC(dc);
}

void wxDC::DrawEllipse(float x, float y, float width, float height)
{
  HDC dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r = new wxRegion(this);
    r->SetEllipse(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  int x1, y1, x2, y2;
  ShiftXY(x, y, x1, y1);
  ShiftXY(x + width, y + height, x2, y2);

  if (StartBrush(dc, 1)) {
    (void)Ellipse(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1), 
		  (int)XLOG2DEV(x2) + 1, (int)YLOG2DEV(y2) + 1);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Ellipse(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1),
		  (int)XLOG2DEV(x2), (int)YLOG2DEV(y2));
    DonePen(dc);
  }

  DoneDC(dc);

  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);
}

void wxDC::SetFont(wxFont *the_font)
{
  HDC dc = ThisDC();
  if (!dc) return;

  font = the_font;

  if (!the_font) {
    if (old_font)
      ::SelectObject(dc, old_font);
    old_font = NULL;
  }

  if (font) {
    HFONT cfont = font->BuildInternalFont(dc, screen_font);

    if (cfont) {
      HFONT f = (HFONT)::SelectObject(dc, cfont);
      if (!old_font)
	old_font = f;
    }
  }
  
  DoneDC(dc);
}

void wxDC::SetPen(wxPen *pen)
{
  if (current_pen) current_pen->Lock(-1);
  current_pen = pen;
  if (current_pen) current_pen->Lock(1);

  if (pen)
    pen->ChangePen();
}

void wxDC::SetBrush(wxBrush *brush)
{
  if (current_brush) current_brush->Lock(-1);
  current_brush = brush;
  if (current_brush) current_brush->Lock(1);

  if (brush)
    brush->ChangeBrush();
}

void wxDC::DrawText(const char *text, float x, float y, Bool use16bit, int d)
{
  HDC dc = ThisDC();

  if (!dc) return;

  int xx1, yy1;

  ShiftXY(x, y, xx1, yy1);

  if (font) {
    HFONT cfont = font->BuildInternalFont(dc, screen_font);
    if (cfont) {
      HFONT f = (HFONT)::SelectObject(dc, cfont);
      if (!old_font)
        old_font = f;
    }
  }
  
  if (current_text_foreground->Ok())
    SetTextColor(dc, current_text_foreground->pixel);

  DWORD old_background;
  if (current_text_background->Ok()) {
    old_background = SetBkColor(dc, current_text_background->pixel);
  }
  
  SetBkMode(dc, ((current_bk_mode == wxTRANSPARENT) 
		 ? TRANSPARENT
		 : OPAQUE));
  
  SetRop(dc, wxSOLID);

  (void)TextOut(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1), 
		text + d, strlen(text + d));

  if (current_text_background->Ok())
    (void)SetBkColor(dc, old_background);

  DoneDC(dc);
  
  CalcBoundingBox((float)x, (float)y);

  float w, h;
  GetTextExtent(text, &w, &h, NULL, NULL, NULL, use16bit, d);
  CalcBoundingBox((float)(x + w), (float)(y + h));
}

void wxDC::SetBackground(wxColour *c)
{
  current_background_color->CopyFrom(c);

#if 0
  if (canvas) {
    wxCanvasWnd *wnd = (wxCanvasWnd *)canvas->handle;
    wxBrush *br = wxTheBrushList->FindOrCreateBrush(c, wxSOLID);
    wnd->SetBackgroundBrush(br->cbrush, FALSE);
    wnd->background_colour = RGB(c->Red(), c->Green(), c->Blue());
    wnd->background_transparent = FALSE;
  }
#endif
  
  HDC dc = ThisDC();

  COLORREF new_color = c->pixel;
  if (new_color != cur_bk || dc != cur_dc) {
    (void)SetBkColor(dc, new_color);
    cur_bk = new_color;
    cur_dc = dc;
  }

  DoneDC(dc);
}

void wxDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxDC::SetRop(HDC dc, int style)
{
  if (!dc) return;

  if (style == cur_rop)
    return;
  cur_rop = style;
  
  int c_rop;
  switch (style) {
  case wxXOR_DOT:
  case wxXOR_SHORT_DASH:
  case wxXOR_LONG_DASH:
  case wxXOR_DOT_DASH:
  case wxXOR: 
    c_rop = R2_NOTXORPEN;
    break;
  default:
    c_rop = R2_COPYPEN;
    break;
  }
  SetROP2(dc, c_rop);
}

int wxDC::StartBrush(HDC dc, Bool no_stipple)
{
  if (current_brush && current_brush->GetStyle() !=wxTRANSPARENT) {
    if (no_stipple) {
      wxBitmap *bm = current_brush->GetStipple();
      if (bm && bm->Ok())
	return FALSE;
    }
    current_brush->SelectBrush(dc);
    SetRop(dc, current_brush->GetStyle());
    return TRUE;
  } else
    return FALSE;
}

void wxDC::DoneBrush(HDC dc)
{
  ::SelectObject(dc, null_brush);
}

int wxDC::StartPen(HDC dc)
{
  if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
    current_pen->SelectPen(dc);
    SetRop(dc, current_pen->GetStyle());
    return TRUE;
  } else
    return FALSE;
}

void wxDC::DonePen(HDC dc)
{
  ::SelectObject(dc, null_pen);
}

wxBitmap *wxDC::StippleBrush()
{
  if (current_brush) {
    wxBitmap *bm = current_brush->GetStipple();
    if (bm && bm->Ok())
      return bm;
  }
  return NULL;
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
  if (cdc) {
    ::StartPage(cdc);
    SetMapMode(mapping_mode);
  }
}

void wxDC::EndPage(void)
{
  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return;
  if (cdc) {
    ::EndPage(cdc);
  }
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

void wxDC::GetTextExtent(const char *string, float *x, float *y,
                         float *descent, float *topSpace, 
			 wxFont *theFont, Bool use16bt, int d)
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
    if (topSpace) *topSpace= 0;
    return;
  }
  
  SIZE sizeRect;
  TEXTMETRIC tm;
  int len = strlen(string + d);

  GetTextExtentPoint(dc, len ? string + d : " ", len ? len : 1, &sizeRect);
  if (descent || topSpace)
    GetTextMetrics(dc, &tm);

  DoneDC(dc);

  *x = (len ? (float)XDEV2LOGREL(sizeRect.cx) : (float)0.0);
  *y = (float)YDEV2LOGREL(sizeRect.cy);
  if (descent) *descent = (float)tm.tmDescent;
  if (topSpace) *topSpace = (float)tm.tmInternalLeading;
  
  if (oldFont)
    SetFont(oldFont);
}

void wxDC::SetMapMode(int mode)
{
  mapping_mode = mode;

  HDC dc = ThisDC();

  if (!dc) return;

  float mm2pixelsX = GetDeviceCaps(dc, LOGPIXELSX) * mm2inches;
  float mm2pixelsY = GetDeviceCaps(dc, LOGPIXELSY) * mm2inches;

  if (!mm2pixelsX || !mm2pixelsY) {
    /* Guess 300 dpi. At least we should start getting bug reports
       about text too large, instead of too small, if this is where
       things fail. */
    mm2pixelsX = 300 * mm2inches;
    mm2pixelsY = 300 * mm2inches;
  }

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

  ::SetViewportExtEx(dc, 1000, 1000, NULL);
  ::SetWindowExtEx(dc, MS_XDEV2LOGREL(1000), MS_YDEV2LOGREL(1000), NULL);
  ::SetViewportOrgEx(dc, (int)device_origin_x, (int)device_origin_y, NULL);
  ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);

  DoneDC(dc);
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

  HDC dc = ThisDC();

  if (dc) ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);

  DoneDC(dc);
}

void wxDC::SetDeviceOrigin(float x, float y)
{
  device_origin_x = x;
  device_origin_y = y;
  HDC dc = ThisDC();

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

float wxDC::FLogicalToDeviceX(float x)
{
  return MS_XLOG2DEV(x);
}

float wxDC::FLogicalToDeviceXRel(float x)
{
  return MS_XLOG2DEVREL(x);
}

float wxDC::FLogicalToDeviceY(float y)
{
  return MS_YLOG2DEV(y);
}

float wxDC::FLogicalToDeviceYRel(float y)
{
  return MS_YLOG2DEVREL(y);
}

Bool wxDC::Blit(float xdest, float ydest, float width, float height,
                wxBitmap *source, float xsrc, float ysrc, int rop,
		wxColour *c)
{
  HDC dc = ThisDC();

  if (!dc) return FALSE;

  if (!blit_dc) {
    wxREGGLOB(blit_dc);
    blit_dc = new wxMemoryDC(1);
  }

  wxMemoryDC *sel = (wxMemoryDC *)source->selectedInto;
  if (sel) sel->SelectObject(NULL);
  blit_dc->SelectObject(source);

  HDC dc_src = blit_dc->ThisDC();

  if (!dc_src) {
    blit_dc->SelectObject(NULL);
    if (sel) sel->SelectObject(source);
    DoneDC(dc);
    return FALSE;
  }

  int xdest1, ydest1, xsrc1, ysrc1;
  ShiftXY(xdest, ydest, xdest1, ydest1);
  xsrc1 = floor(xsrc);
  ysrc1 = floor(ysrc);

  Bool success = 0;
  DWORD op = 0;

  SetTextColor(dc, 0); /* 0 = black */
  if (source->GetDepth() == 1) {
    if (rop == wxSOLID) {
      SetTextColor(dc, wxBLACK->pixel);
      success = BitBlt(dc, xdest1, ydest1, 
		       width, height,
		       dc_src, xsrc1, ysrc1,
		       MERGEPAINT);
      op = SRCAND;
    } else
      op = ((rop == wxXOR) 
	    ? 0x00990066 /* => DSnx */
	    : SRCCOPY);  /* opaque */
    SetTextColor(dc, c ? c->pixel : wxBLACK->pixel);
  } else {
    op = SRCCOPY;		
    SetTextColor(dc, wxBLACK->pixel);
  }
  
  if (op)
    success = BitBlt(dc, xdest1, ydest1, 
		     width, height,
		     dc_src, xsrc1, ysrc1, op);

  DoneDC(dc);
  blit_dc->DoneDC(dc_src);
  blit_dc->SelectObject(NULL);
  if (sel) sel->SelectObject(source);

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

wxCanvasDC::wxCanvasDC(void)
{
  __type = wxTYPE_DC_CANVAS;
  device = wxDEVICE_WINDOWS;
}

wxCanvasDC::wxCanvasDC(wxCanvas *the_canvas) : wxbCanvasDC()
{
  __type = wxTYPE_DC_CANVAS;
  canvas = the_canvas;
  WXGC_IGNORE(this, canvas);
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

void wxCanvasDC::GetSize(float *width, float *height)
{
  int ww, hh;

  canvas->GetClientSize(&ww, &hh);
  *width = ww;
  *height = hh;
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  COLORREF result;

  HDC dc = ThisDC();
  if (!dc) {
    dest->Set(0, 0, 0);
    return;
  }

  result = GetNearestColor(dc, RGB(src->Red(), src->Green(), src->Blue()));
  dest->Set(GetRValue(result), GetGValue(result), GetBValue(result));
  DoneDC(dc);
}

Bool wxCanvasDC::GCBlit(float xdest, float ydest, float width, float height,
			wxBitmap *source, float xsrc, float ysrc)
{
  if (blit_dc)
    return Blit(xdest, ydest, width, height, source, xsrc, ysrc, wxSTIPPLE, NULL);
  else
    return FALSE;
}

static BOOL DoPrintDlg(PRINTDLG *pd, HWND parent)
{
  if (!pd->hwndOwner)
    pd->hwndOwner = parent;

  return PrintDlg(pd);
}

wxPrinterDC::wxPrinterDC(wxWindow *parent, char *driver_name, char *device_name, char *file, Bool interactive)
{
  __type = wxTYPE_DC_PRINTER;
  wx_interactive = interactive;
  device = wxDEVICE_WINDOWS;

  if (file)
    filename = copystring(file);
  else
    filename = NULL;

  HWND hwnd = NULL;
  if (parent) {
    wxWnd *wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }

  screen_font = FALSE;

  if (interactive) {
    PRINTDLG *pd = new PRINTDLG;
    
    pd->lStructSize = sizeof(PRINTDLG);
    pd->hwndOwner=hwnd;
    pd->hDevMode=(HANDLE)NULL;
    pd->hDevNames=(HANDLE)NULL;
    pd->Flags = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS | PD_HIDEPRINTTOFILE;
    pd->nFromPage=0;
    pd->nToPage=0;
    pd->nMinPage=0;
    pd->nMaxPage=0;
    pd->nCopies=1;
    pd->hInstance=(HINSTANCE)NULL;
    
    if (wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0)) {
      cdc = pd->hDC;
      ok = TRUE;
    } else {
      ok = FALSE;
      return;
    }
    
    dont_delete = TRUE; // ??? WHY???
  } else if (driver_name && device_name && file) {
    cdc = CreateDC(driver_name, device_name, file, NULL);
    ok = cdc ? TRUE : FALSE;
  } else {
    cdc = wxGetPrinterDC();
    ok = cdc ? TRUE : FALSE;
  }
  
  if (cdc) {
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

  screen_font = FALSE;

  cdc = theDC;
  ok = TRUE;

  if (cdc) {
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

    hDC = CreateDC(lpszDriverName, lpszDeviceName, lpszPortName, (DEVMODE *)lpDevMode);

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
}

/*
 * Memory DC
 *
 */

wxMemoryDC::wxMemoryDC(Bool ro)
{
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_WINDOWS;

  read_only = ro;

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
    dc = wnd->GetHDC();

  cdc = wxwmCreateCompatibleDC(dc);
  ok = (cdc != NULL);

  if (!old_dc->cdc && wnd)
    wnd->ReleaseHDC();

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::~wxMemoryDC(void)
{
  if (selected_bitmap)
    SelectObject(NULL);
}

Bool wxMemoryDC::Ok(void)
{
  return (ok && selected_bitmap);
}

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  if (bitmap == selected_bitmap)
    return;

  if (!cdc)
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
	if (!read_only) {
	  selected_bitmap->selectedInto = NULL;
	  selected_bitmap->selectedIntoDC = 0;
	}
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
  if ((!read_only && bitmap->selectedIntoDC) || !bitmap->Ok())
  {
    return;
  }



  if (selected_bitmap) {
    if (!read_only) {
      selected_bitmap->selectedInto = NULL;
      selected_bitmap->selectedIntoDC = 0;
    }
  }
  
  selected_bitmap = bitmap;
  if (!read_only) {
    bitmap->selectedInto = this;
    bitmap->selectedIntoDC = -1;
  }

  HBITMAP bm = (HBITMAP)::SelectObject(cdc, bitmap->ms_bitmap);

  if (bm == ERROR)
  {
    selected_bitmap = NULL;
    if (!read_only) {
      bitmap->selectedInto = NULL;
      bitmap->selectedIntoDC = 0;
    }

   if (old_bitmap) {
     ::SelectObject(cdc, old_bitmap);
     old_bitmap = NULL;
   }

   bitmap = NULL;
  } else if (!old_bitmap)
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

wxBitmap* wxMemoryDC::GetObject(void)
{
  return selected_bitmap;
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
