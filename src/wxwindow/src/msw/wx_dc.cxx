/*
 * File:	wx_dc.cc
 * Purpose:	Device context implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "../../../wxcommon/Region.h"
#include "wx_pdf.h"
#include "../../../mzscheme/include/scheme.h"

#include <math.h>

#include <commdlg.h>

// Declarations local to this file

static wxMemoryDC *blit_dc, *blit_mdc;

#define YSCALE(y) (yorigin - (y))

static HANDLE null_brush;
static HANDLE null_pen;

void RegisterGDIObject(HANDLE x);

static is_nt()
{
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
  return nt;
}

class wxWinGL : public wxGL {
public:
  wxWinGL();

  int Ok();

  void SwapBuffers(void);
  void ThisContextCurrent(void);

  void Reset(HDC dc, int offscreen);

  void SetupPalette(PIXELFORMATDESCRIPTOR *pfd);
  wxColourMap* CreateDefaultPalette(PIXELFORMATDESCRIPTOR *pfd);

  HGLRC m_hGLRC;
  HDC m_hDC;
  wxColourMap *m_palette;
  Bool m_deletePalette;
};

/******************************************************************/

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
  if (clipping) --clipping->locked;

  if (filename)
    delete[] filename;

  if (wx_gl) {
    wx_gl->Reset(0, 0);
    wx_gl = NULL;
  }

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
    HPEN op;
    HBRUSH ob;
    op = (HPEN)::SelectObject(dc, null_pen);
    old_pen = op;
    ob = (HBRUSH)::SelectObject(dc, null_brush);
    old_brush = ob;
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

wxGL *wxDC::GetGL()
{
  if (!wx_gl) {
    if (__type == wxTYPE_DC_CANVAS) {
      wx_gl = new wxWinGL();
      wx_gl->Reset(cdc, 0);
    }
  }

  return wx_gl;
}

void wxDC::ShiftXY(float x, float y, int *ix, int *iy)
{
  *ix = (int)floor(x);
  *iy = (int)floor(y);

  if (canvas) {
    wxWnd *wnd = (wxWnd *)canvas->handle;
    wnd->CalcScrolledPosition((int)x, (int)y, ix, iy);
  }
}

void wxDC::SetClippingRect(float cx, float cy, float cw, float ch)
{
  HDC dc;

  if (clipping) delete clipping;

  clipping = new wxRegion(this);
  clipping->SetRectangle(cx, cy, cw, ch);

  dc = ThisDC();
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
  HDC dc;

  if (c && (c->dc != this)) return;

  if (clipping)
    --clipping->locked;

  clipping = c;

  if (clipping)
    clipping->locked++;

  dc = ThisDC();
  if (dc) DoClipping(dc);
  DoneDC(dc);
}

static HRGN empty_rgn;

void wxDC::DoClipping(HDC dc)
{
  if (clipping) {
    if (clipping->rgn)
      SelectClipRgn(dc, clipping->rgn);
    else {
      if (!empty_rgn)
	empty_rgn = CreateRectRgn(0, 0, 0, 0);
      SelectClipRgn(dc, empty_rgn);
    }
  } else {
    HRGN rgn;
    rgn = CreateRectRgn(0, 0, 32000, 32000);
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
  HDC dc;
  Bool tok;

  dc = ThisDC();
  
  // What sort of display is it?

  if (dc) {
    int technology;
    
    technology = ::GetDeviceCaps(dc, TECHNOLOGY);
    
    if (technology != DT_RASDISPLAY && technology != DT_RASPRINTER)
      tok = FALSE;
    else 
      tok = TRUE;
  } else
    tok = FALSE;

  DoneDC(dc);
  
  return tok;
}

void wxDC::SetColourMap(wxColourMap *cmap)
{
  HDC dc;

  dc = ThisDC();

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
    HPALETTE oldPal;
    oldPal = ::SelectPalette(dc, cmap->ms_palette, TRUE);
    if (!old_palette)
      old_palette = oldPal;
      
    ::RealizePalette(dc);
  }

  DoneDC(dc);
}

void wxDC::Clear(void)
{
  HDC dc;
  RECT rect;
  XFORM orig_xform;
  int use_xform;

  dc = ThisDC();

  if (!dc) return;

  /* Get the DC's width and height */
  if (canvas)
    GetClientRect(((wxWnd *)canvas->handle)->handle, &rect);
  else if (selected_bitmap) {
    rect.left = 0; rect.top = 0;
    rect.right = selected_bitmap->GetWidth();
    rect.bottom = selected_bitmap->GetHeight();
  } else {
    rect.left = 0;
    rect.top = 0;
    rect.right = ::GetDeviceCaps(dc, HORZRES);
    rect.bottom = ::GetDeviceCaps(dc, VERTRES);
  }

  /* Make sure the mapping mode is 1:1 */
  if (::GetGraphicsMode(dc) == GM_ADVANCED) {
    XFORM xform;

    GetWorldTransform(dc, &orig_xform);
    use_xform = 1;

    xform.eM11 = 1;
    xform.eM21 = 0;
    xform.eM12 = 0;
    xform.eM22 = 1;
    xform.eDx = 0;
    xform.eDy = 0;
    ::SetWorldTransform(dc, &xform);
  } else {
    (void) ::SetMapMode(dc, MM_TEXT);
    ::SetViewportOrgEx(dc, 0, 0, NULL);
    ::SetWindowOrgEx(dc, 0, 0, NULL);
    use_xform = 1;
  }

  /* Here's the actual clear operation */
  {
    HBRUSH brush;
    brush = CreateSolidBrush(GetBkColor(dc));
    FillRect(dc, &rect, brush);
    DeleteObject(brush);
  }

  /* Restore the mapping mode and release the DC */
  if (use_xform) {
    ::SetWorldTransform(dc, &orig_xform);
    DoneDC(dc);
  } else {
    DoneDC(dc);
    SetMapMode(mapping_mode);
  }
}

void wxDC::BeginDrawing(void)
{
}

void wxDC::EndDrawing(void)
{
}

Bool wxDC::GlyphAvailable(int c, wxFont *f)
{
  HDC dc;
  Bool r;

  if (!f)
    f = font;

  dc = ThisDC();
  if (!dc) return 0;

  r = f->GlyphAvailable(c, dc);

  DoneDC(dc);

  return r;
}

void wxDC::FloodFill(float x, float y, wxColour *col, int style)
{
  HDC dc;
  int xx;
  int yy;

  dc = ThisDC();
  if (!dc) return;

  ShiftXY(x, y, &xx, &yy);

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
  int xx1;
  int yy1;
  HDC dc;
  COLORREF pixelcolor;

  dc = ThisDC();

  if (!dc) return FALSE;

  ShiftXY(x, y, &xx1, &yy1);
  
  // get the color of the pixel
  pixelcolor = ::GetPixel(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1));
  
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
  HDC dc;
  int xx, yy;
  int xx1;
  int yy1;
  int xx2;
  int yy2;

  dc = ThisDC(); 

  if (!dc) return;

  ShiftXY(x, y, &xx, &yy);

  // We suppose that our screen is 2000x2000 max.

  xx1 = xx-2000;
  yy1 = yy-2000;
  xx2 = xx+2000;
  yy2 = yy+2000;

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
  int xx1, yy1, xx2, yy2;
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (StartPen(dc)) {
    int pw;
    int forward;

    ShiftXY(x1, y1, &xx1, &yy1);
    ShiftXY(x2, y2, &xx2, &yy2);
    
    /* Convention across platforms: line includes pixel on endpoint */
    pw = current_pen->GetWidth();
    forward = 0;
    if (!pw)
      forward = 1;
    else if (pw == 1) {
      if (current_pen->GetCap() != wxCAP_BUTT) {
	/* Pen size 1: no need to forward under NT */
	forward = !is_nt();
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
  wxBitmap *bm;
  int style;
  wxColour *c;

  bm = brush->GetStipple();
  style = brush->GetStyle();
  c = brush->GetColour();

  old = dc->GetClippingRegion();
  if (old) r->Intersect(old);

  r->BoundingBox(&x, &y, &w, &h);
  bw = bm->GetWidth();
  bh = bm->GetHeight();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  w = dc->LogicalToDeviceXRel(w);
  h = dc->LogicalToDeviceYRel(h);
  
  xstart = (int)floor(x / bw);
  xend = (int)floor((x + w + bw - 0.00001) / bw);

  ystart = (int)floor(y / bh);
  yend = (int)floor((y + h + bh - 0.00001) / bh);

  dc->SetClippingRegion(r);

  for (i = xstart; i < xend; i++) {
    for (j = ystart; j < yend; j++) {
      dc->Blit(dc->DeviceToLogicalX(i * bw), 
	       dc->DeviceToLogicalY(j * bh), 
	       dc->DeviceToLogicalXRel(bw), 
	       dc->DeviceToLogicalYRel(bh),
	       bm, 0, 0, style, c);
    }
  }

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

  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetArc(x, y, w, h, start, end);
    FillWithStipple(this, r, current_brush);
  }

  ShiftXY(x, y, &xx1, &yy1);
  ShiftXY(x + w, y + h, &xx2, &yy2);
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
  wxWnd *wnd = NULL;
  int xx1, yy1;
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  ShiftXY(x, y, &xx1, &yy1);
  
  ::SetPixelV(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1), c->pixel);

  DoneDC(dc);

  CalcBoundingBox(x, y);
}

void wxDC::DrawPolygon(int n, wxPoint points[], float xoffset, float yoffset,int fillStyle)
{
  HDC dc;
  int xoffset1;
  int yoffset1;
  POINT *cpoints;
  int i, prev;

  dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetPolygon(n, points, xoffset, yoffset, fillStyle);
    FillWithStipple(this, r, current_brush);
  }

  cpoints = new POINT[n];
  for (i = 0; i < n; i++) {
    ShiftXY(points[i].x + xoffset, points[i].y + yoffset, &xoffset1, &yoffset1);
    cpoints[i].x = xoffset1;
    cpoints[i].y = yoffset1;
    CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
  }

  prev = SetPolyFillMode(dc, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);

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
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (StartPen(dc)) {
    int xoffset1;
    int yoffset1;
    POINT *cpoints;
    int i;

    ShiftXY(xoffset, yoffset, &xoffset1, &yoffset1);
    
    cpoints = new POINT[n];
    for (i = 0; i < n; i++) {
      cpoints[i].x = (int)(XLOG2DEV(points[i].x + xoffset1));
      cpoints[i].y = (int)(YLOG2DEV(points[i].y + yoffset1));
      CalcBoundingBox((float)points[i].x + xoffset, (float)points[i].y + yoffset);
    }
    
    (void)Polyline(dc, cpoints, n);

    DonePen(dc);
  }

  DoneDC(dc);

}

void wxDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (StartPen(dc)) {
    int xoffset1;
    int yoffset1;
    POINT *cpoints;
    int i;
    
    cpoints = new POINT[n];
    for (i = 0; i < n; i++) {
      ShiftXY(points[i].x + xoffset, points[i].y + yoffset, &xoffset1, &yoffset1);
      cpoints[i].x = xoffset1;
      cpoints[i].y = yoffset1;
      CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
    }
    
    (void)Polyline(dc, cpoints, n);

    DonePen(dc);
  }

  DoneDC(dc);
  
}

void wxDC::DrawRectangle(float x, float y, float width, float height)
{
  int x1, y1, x2, y2;
  Bool do_brush, do_pen;
  HDC dc;

  dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetRectangle(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  ShiftXY(x, y, &x1, &y1);
  ShiftXY(x + width, y + height, &x2, &y2);

  if (StartBrush(dc, 1)) {
    (void)Rectangle(dc, (int)XLOG2DEV(x1), (int)YLOG2DEV(y1),
		    (int)XLOG2DEV(x2) + 1, (int)YLOG2DEV(y2) + 1);
    DoneBrush(dc);
  }
  if (StartPen(dc)) {
    (void)Rectangle(dc, 
		    (int)XLOG2DEV(x1), (int)YLOG2DEV(y1),
		    (int)XLOG2DEV(x2), (int)YLOG2DEV(y2));
    DonePen(dc);
  }

  CalcBoundingBox((float)x, (float)y);
  CalcBoundingBox((float)x + width, (float)y + height);

  DoneDC(dc);
}

void wxDC::DrawRoundedRectangle(float x, float y, float width, float height, float radius)
{
  HDC dc;
  int x1, y1, x2, y2;

  dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetRoundedRectangle(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  ShiftXY(x, y, &x1, &y1);
  ShiftXY(x + width, y + height, &x2, &y2);

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
  HDC dc;
  int x1, y1, x2, y2;

  dc = ThisDC();

  if (!dc) return;

  if (StippleBrush()) {
    wxRegion *r;
    r = new wxRegion(this);
    r->SetEllipse(x, y, width, height);
    FillWithStipple(this, r, current_brush);
  }

  ShiftXY(x, y, &x1, &y1);
  ShiftXY(x + width, y + height, &x2, &y2);

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
  HDC dc;

  dc = ThisDC();
  if (!dc) return;

  font = the_font;

  if (!the_font) {
    if (old_font)
      ::SelectObject(dc, old_font);
    old_font = NULL;
  }

  if (font) {
    HFONT cfont;
    cfont = font->BuildInternalFont(dc, screen_font);

    if (cfont) {
      HFONT f;
      f = (HFONT)::SelectObject(dc, cfont);
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

static int ucs4_strlen(const unsigned int *c)
{
  int i;
  
  for (i = 0; c[i]; i++) {
  }

  return i;
}

#define QUICK_UBUF_SIZE 1024
static wchar_t u_buf[QUICK_UBUF_SIZE];

int CALLBACK proc(CONST LOGFONT *lplf, CONST TEXTMETRIC *lptm, DWORD dwType, LPARAM lpData)
{
  GCP_RESULTSW gcp;
  wchar_t s[4], gl[4];
  char classes[4];
  DWORD ok;
  HDC hdc = (HDC)lpData;
  HFONT fnt, old;

  fnt = CreateFontIndirect(lplf);

  old = (HFONT)::SelectObject(hdc, fnt);

  s[0] = 0x202D;
  s[1] = 'a';
  s[2] = 0x200C;
  s[3] = 'a';
  
  gcp.lStructSize = sizeof(GCP_RESULTSW);
  gcp.lpOutString = NULL;
  gcp.lpDx = NULL;
  gcp.lpCaretPos = NULL;
  gcp.lpOrder = NULL;
  gcp.lpClass = classes;
  gcp.lpGlyphs = gl;
  gcp.nGlyphs = 4;

  ok = GetCharacterPlacementW(hdc, s, 4, 0, &gcp,
	  FLI_MASK & GetFontLanguageInfo(hdc));
  
  ::SelectObject(hdc, old);

  if (ok && (gcp.nGlyphs == 2))
    return 0;

  return 1;
}

wchar_t *convert_to_drawable_format(const char *text, int d, int ucs4, long *_ulen, 
				    Bool combine, wxDC *dc, HDC hdc)
{
  int ulen, alloc_ulen;
  wchar_t *unicode;
  int theStrlen;

  if (!combine) {
    if (!dc->combine_status) {
      /* Check whether text renderer knows how to deal with ZWNJ, etc. */
      if (!EnumFonts(hdc, NULL, (FONTENUMPROC)proc, (long)hdc))
	dc->combine_status = 1;
      else
	dc->combine_status = -1;
    }
    if (dc->combine_status == -1)
      combine = 1; /* DC doesn't combine, so no need to avoid combinations */
  }

  if (ucs4) {
    theStrlen = ucs4_strlen((const unsigned int *)text XFORM_OK_PLUS d);
  } else {
    theStrlen = strlen(text XFORM_OK_PLUS d);
  }

  if (ucs4) {
    int i, extra;
    unsigned int v;

    /* Count characters that fall outside UCS-2: */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      if (((unsigned int *)text)[d+i] > 0xFFFF)
	extra++;
    }

    ulen = theStrlen + extra;
    if (combine)
      alloc_ulen = ulen;
    else
      alloc_ulen = 2 * (ulen + 1);
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = new WXGC_ATOMIC wchar_t[alloc_ulen];
    else
      unicode = u_buf;
    
    /* UCS-4 -> UTF-16 conversion */
    for (i = 0, extra = (combine ? 0 : 1); i < theStrlen; i++) {
      v = ((unsigned int *)text)[d+i];
      if (v > 0xFFFF) {
	unicode[i+extra] = 0xD8000000 | ((v >> 10) & 0x3FF);
	extra++;
	unicode[i+extra] = 0xDC000000 | (v & 0x3FF);
      } else
	unicode[i+extra] = v;
    }
  } else {
    /* UTF-8 -> UTF-16 conversion */
    ulen = scheme_utf8_decode((unsigned char *)text, d, 
			      theStrlen, NULL, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
    if (combine)
      alloc_ulen = ulen;
    else
      alloc_ulen = 2 * (ulen + 1);
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = new WXGC_ATOMIC wchar_t[alloc_ulen];
    else
      unicode = u_buf;
    ulen = scheme_utf8_decode((unsigned char *)text, d, theStrlen, 
			      (unsigned int *)unicode, combine ? 0 : 1, -1, 
			      NULL, 1 /*UTF-16*/, '?');
  }

  /* In non-combine mode, prevent all sorts of glyph combinations */
  if (!combine) {
    unsigned int i, j = 0;
    /* start with left-to-right override: */
    ulen++;
    unicode[0] = 0x202D;
    /* remove other control characters: */
    for (i = 1; i < ulen; i++) {
      if (scheme_iscontrol(unicode[i])) {
	j++;
      } else {
	unicode[i - j] = unicode[i];
      }
    }
    ulen -= j;
    /* Add ZWNJ to prevent other combinations */
    for (i = ulen; i--; ) {
      unicode[(2 * i) + 1] = 0x200C;
      unicode[(2 * i)] = unicode[i];
	}
    ulen *= 2;
  }
  
  *_ulen = ulen;
  return unicode;
}

void wxDC::DrawText(const char *text, float x, float y, Bool combine, Bool ucs4, int d, float angle)
{
  int xx1, yy1;
  HDC dc;
  DWORD old_background;
  float w, h;
  wchar_t *ustring;
  long len;

  dc = ThisDC();

  if (!dc) return;

  ShiftXY(x, y, &xx1, &yy1);

  if (font) {
    HFONT cfont;
    cfont = font->BuildInternalFont(dc, screen_font, angle);
    if (cfont) {
      HFONT f;
      f = (HFONT)::SelectObject(dc, cfont);
      if (!old_font)
        old_font = f;
    }
  }
  
  if (current_text_foreground->Ok())
    SetTextColor(dc, current_text_foreground->pixel);

  if (current_text_background->Ok()) {
    old_background = SetBkColor(dc, current_text_background->pixel);
  }
  
  SetBkMode(dc, (((current_bk_mode == wxTRANSPARENT) 
		  || (angle != 0.0))
		 ? TRANSPARENT
		 : OPAQUE));
  
  SetRop(dc, wxSOLID);

  ustring = convert_to_drawable_format(text, d, ucs4, &len, combine, this, dc);

  (void)TextOutW(dc, (int)XLOG2DEV(xx1), (int)YLOG2DEV(yy1), ustring, len);

  if (current_text_background->Ok())
    (void)SetBkColor(dc, old_background);

  DoneDC(dc);
  
  CalcBoundingBox((float)x, (float)y);

  GetTextExtent(text, &w, &h, NULL, NULL, NULL, combine, ucs4, d);
  CalcBoundingBox((float)(x + w), (float)(y + h));
}

void wxDC::SetBackground(wxColour *c)
{
  HDC dc;
  COLORREF new_color;

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
  
  dc = ThisDC();

  new_color = c->pixel;
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
  int c_rop;

  if (!dc) return;

  if (style == cur_rop)
    return;
  cur_rop = style;
  
  switch (style) {
  case wxXOR_DOT:
  case wxXOR_SHORT_DASH:
  case wxXOR_LONG_DASH:
  case wxXOR_DOT_DASH:
  case wxXOR: 
  case wxCOLOR: 
    c_rop = R2_NOTXORPEN;
    break;
  default:
    c_rop = R2_COPYPEN;
    break;
  }
  SetROP2(dc, c_rop);
}

static HBRUSH hilite_brush;

int wxDC::StartBrush(HDC dc, Bool no_stipple)
{
  if (current_brush && current_brush->GetStyle() !=wxTRANSPARENT) {
    int ps;
    ps = current_brush->GetStyle();
    if (Colour && (ps == wxCOLOR)) {
      if (!hilite_brush) {
	hilite_brush = CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT));
	RegisterGDIObject(hilite_brush);
      }
      SelectObject(dc, hilite_brush);
      SetROP2(dc, R2_MERGEPENNOT);
    } else {
      if (no_stipple) {
	wxBitmap *bm;
	bm = current_brush->GetStipple();
	if (bm && bm->Ok())
	  return FALSE;
      }
      current_brush->SelectBrush(dc);
      SetRop(dc, current_brush->GetStyle());
    }
    return TRUE;
  } else
    return FALSE;
}

void wxDC::DoneBrush(HDC dc)
{
  ::SelectObject(dc, null_brush);
}

static HPEN hilite_pens[256];

int wxDC::StartPen(HDC dc)
{
  if (current_pen && (current_pen->GetStyle() != wxTRANSPARENT)) {
    int ps;
    ps = current_pen->GetStyle();
    if (Colour && (ps == wxCOLOR)) {
      int size;
      size = current_pen->GetWidth();
      if (!hilite_pens[size]) {
	HPEN p;
	p = CreatePen(PS_SOLID, size, GetSysColor(COLOR_HIGHLIGHT));
	hilite_pens[size] = p;
	RegisterGDIObject(p);
      }
      SelectObject(dc, hilite_pens[size]);
      SetROP2(dc, R2_MERGEPENNOT);
    } else {
      current_pen->SelectPen(dc);
      SetRop(dc, current_pen->GetStyle());
    }
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
    wxBitmap *bm;
    bm = current_brush->GetStipple();
    if (bm && bm->Ok())
      return bm;
  }
  return NULL;
}

Bool wxDC::StartDoc(char *message)
{
  Bool flag = FALSE;

  DOCINFO docinfo;

  if (!wxSubType(__type, wxTYPE_DC_PRINTER))
    return TRUE;
    
  docinfo.cbSize = sizeof(DOCINFO);
  docinfo.lpszDocName = message;
  docinfo.lpszOutput = filename;
  docinfo.lpszDatatype = NULL;
  docinfo.fwType = 0;
  if (cdc)
    flag = (SP_ERROR != ::StartDoc(cdc, &docinfo));
  else
    flag = FALSE;

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
  HDC dc;

  dc = ThisDC();

  if (!dc) return 10;

  GetTextMetrics(dc, &lpTextMetric);

  DoneDC(dc);

  return (float)YDEV2LOGREL(lpTextMetric.tmHeight);
}

float wxDC::GetCharWidth(void)
{
  TEXTMETRIC lpTextMetric;
  HDC dc;

  dc = ThisDC();

  if (!dc) return 5;

  GetTextMetrics(dc, &lpTextMetric);

  DoneDC(dc);

  return (float)XDEV2LOGREL(lpTextMetric.tmAveCharWidth);
}

void wxDC::GetTextExtent(const char *string, float *x, float *y,
                         float *descent, float *topSpace, 
			 wxFont *theFont, Bool combine, Bool ucs4, int d)
{
  wxFont *oldFont = NULL;
  HDC dc;
  SIZE sizeRect;
  TEXTMETRIC tm;
  long len;
  wchar_t *ustring;

  if (theFont) {
    oldFont = font;
    SetFont(theFont);
  } else
    SetFont(font);

  dc = ThisDC();

  if (!dc) {
    *x = 5;
    *y = 10;
    if (descent) *descent = 0;
    if (topSpace) *topSpace= 0;
    return;
  }

  ustring = convert_to_drawable_format(string, d, ucs4, &len, combine, this, dc);

  GetTextExtentPointW(dc, ustring, len, &sizeRect);
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
  HDC dc;

  mapping_mode = mode;

  dc = ThisDC();

  if (!dc) return;

  if (mode == MM_TEXT) {
    logical_scale_x = 1.0;
    logical_scale_y = 1.0;
  } else {
    float mm2pixelsX;
    float mm2pixelsY;
  
    mm2pixelsX = GetDeviceCaps(dc, LOGPIXELSX) * mm2inches;
    mm2pixelsY = GetDeviceCaps(dc, LOGPIXELSY) * mm2inches;

    if (!mm2pixelsX || !mm2pixelsY) {
      /* Guess 300 dpi. At least we should start getting bug reports
	 about text too large, instead of too small, if this is where
	 things fail. */
      mm2pixelsX = 300 * mm2inches;
      mm2pixelsY = 300 * mm2inches;
    }

    switch (mode) {
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
      {
	logical_scale_x = 1.0;
	logical_scale_y = 1.0;
	break;
      }
    }
  }

  if (::GetMapMode(dc) != MM_ANISOTROPIC)
    ::SetMapMode(dc, MM_ANISOTROPIC);

  if ((__type != wxTYPE_DC_PRINTER)
      && (::SetGraphicsMode(dc, GM_ADVANCED) != 0)) {
    XFORM xform;
    xform.eM11 = user_scale_x;
    xform.eM21 = 0;
    xform.eM12 = 0;
    xform.eM22 = user_scale_y;
    xform.eDx = device_origin_x;
    xform.eDy = device_origin_y;
    ::SetWorldTransform(dc, &xform);
  } else {
    ::SetViewportExtEx(dc, 1000, 1000, NULL);
    ::SetWindowExtEx(dc, MS_XDEV2LOGREL(1000), MS_YDEV2LOGREL(1000), NULL);
    ::SetViewportOrgEx(dc, (int)device_origin_x, (int)device_origin_y, NULL);
    ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);
  }

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
  HDC dc;

  logical_origin_x = x;
  logical_origin_y = y;

  dc = ThisDC();

  if (dc) {
    ::SetWindowOrgEx(dc, (int)logical_origin_x, (int)logical_origin_y, NULL);
  }

  DoneDC(dc);
}

void wxDC::SetDeviceOrigin(float x, float y)
{
  device_origin_x = x;
  device_origin_y = y;
  
  SetMapMode(mapping_mode);
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

#define wxKEEPDEST (DWORD)0x00AA0029

typedef BOOL (WINAPI *wxALPHA_BLEND)(HDC,int,int,int,int,HDC,int,int,int,int,BLENDFUNCTION);
static wxALPHA_BLEND wxAlphaBlend;
static int tried_ab = 0;
#ifndef AC_SRC_ALPHA
# define AC_SRC_ALPHA 0x01
#endif

Bool wxDC::Blit(float xdest, float ydest, float width, float height,
                wxBitmap *source, float xsrc, float ysrc, int rop,
		wxColour *c, wxBitmap *mask)
{
  int xdest1, ydest1, xdest2, ydest2, xsrc1, ysrc1, iw, ih;
  HDC dc, dc_src, invented_dc, mdc = NULL;
  wxMemoryDC *sel, *msel = NULL, *invented_memdc = NULL;
  wxBitmap *invented = NULL;
  Bool success = 1, invented_col = 0, use_alpha = 0;
  DWORD op = 0;

  dc = ThisDC();

  if (!dc) return FALSE;

  if (!blit_dc) {
    wxREGGLOB(blit_dc);
    blit_dc = new wxMemoryDC(1);
  }

  sel = (wxMemoryDC *)source->selectedInto;
  if (sel) 
    dc_src = sel->ThisDC();
  else {
    blit_dc->SelectObject(source);
    dc_src = blit_dc->ThisDC();
  }

  if (!dc_src) {
    DoneDC(dc);
    return FALSE;
  }

  ShiftXY(xdest, ydest, &xdest1, &ydest1);
  xsrc1 = (int)floor(xsrc);
  ysrc1 = (int)floor(ysrc);

  iw = (int)floor(width);
  ih = (int)floor(height);

  if (mask && ((mask->GetDepth() > 1) || !is_nt())) {
    /* No MaskBlt in 95/98/Me, so we invent a bitmap like the source,
       but with white where the mask has white.

       Also, when AlphaBlend is available, we need to create a bitmaps
       with alphas in it. */
    int mono_src;

    if (mask == source) {
      /* This is ok. Just use dc_src as mdc. */
      mdc = dc_src;
    } else {
      msel = (wxMemoryDC *)mask->selectedInto;
      if (msel) {
	mdc = msel->ThisDC();
      } else {
	if (!blit_mdc) {
	  wxREGGLOB(blit_mdc);
	  blit_mdc = new wxMemoryDC(1);
	}
	
	blit_mdc->SelectObject(mask);
	mdc = blit_mdc->ThisDC();
      }
    }

    mono_src = (source->GetDepth() == 1);
    
    invented = new wxBitmap(iw, ih, mono_src);
    if (invented->Ok()) {
      GC_CAN_IGNORE void *pBits = NULL; /* set with use_alpha... */
      int mono_mask;

      if (mask->GetDepth() > 1) {
	if (!tried_ab) {
	  HMODULE mod;
	  mod = LoadLibrary("Msimg32.dll");
	  if (mod)
	    wxAlphaBlend = (wxALPHA_BLEND)GetProcAddress(mod, "AlphaBlend");
	  tried_ab = 1;
	}
	if (wxAlphaBlend)
	  use_alpha = 1;
	/* Otherwise, no AlphaBlend. The result is somewhat unpredictable,
	   but somewhat as intended --- especially if we happend
	   to be drawing onto white. :) */
      }

      if (use_alpha) {
	pBits = invented->ChangeToDIBSection();
	if (!pBits) {
	  use_alpha = 0;
	  /* half-failure... act like AlphaBlend isn't there */
	}
      }

      invented_memdc = new wxMemoryDC();
      invented_memdc->SelectObject(invented);
      
      if (invented_memdc->Ok()) {
	invented_dc = invented_memdc->ThisDC();

	/* Copy original src image here: */
	BitBlt(invented_dc, 0, 0,
	       iw, ih,
	       dc_src, xsrc1, ysrc1,
	       SRCCOPY);

	if (use_alpha) {
	  /* "Pre-compute" alpha in the invented DC */
	  GC_CAN_IGNORE BYTE *pPixel;
	  COLORREF mcol;
	  int i, j, gray;

	  GdiFlush();
	  for (j = 0; j < ih; j++) {
	    pPixel = (BYTE *) pBits + iw * 4 * (ih - j - 1);
	    for (i = 0; i < iw; i++) {
	      mcol = ::GetPixel(mdc, i + xsrc1, j + ysrc1);
	      gray = ((int)GetRValue(mcol)
		      + (int)GetGValue(mcol)
		      + (int)GetBValue(mcol)) / 3;
	      pPixel[0] = pPixel[0] * (255 - gray) / 255; 
	      pPixel[1] = pPixel[1] * (255 - gray) / 255; 
	      pPixel[2] = pPixel[2] * (255 - gray) / 255; 
	      pPixel[3] = (255 - gray);
	      
	      pPixel += 4;
            }
	  }
	} else {
	  /* Want white where mask was white,
	     src otherwise: */
	  BitBlt(invented_dc, 0, 0,
		 iw, ih,
		 mdc, xsrc1, ysrc1,
		 SRCPAINT /* DSo */);

	  /* Ignore the mask and... */
	  mask = NULL;
	  if (mono_src) {
	    /* Mono source: Now use invented_dc instead of src_dc,
	       and it all works out. */
	    xsrc1 = 0;
	    xsrc1 = 0;
	  } else {
	    /* Paint on dest using mask, then "and" invented image
	       with dest. */
	    invented_col = 1;
	  }
	}
      } else {
	/* Failed (Rest of failure handling below since !invented_memdc) */
	invented_memdc->SelectObject(NULL);
	DELETE_OBJ invented_memdc;
	invented_memdc = NULL;
	DELETE_OBJ invented;
      }
    }

    if (!invented_memdc) {
      /* Failed */
      if (msel) 
	msel->DoneDC(mdc);
      else {
	blit_mdc->DoneDC(mdc);
	blit_mdc->SelectObject(NULL);
      }
      DoneDC(dc);
      if (sel)
	sel->DoneDC(dc_src);
      else {
	blit_dc->DoneDC(dc_src);
	blit_dc->SelectObject(NULL);
      }
      return 0;
    }
  }

  if (use_alpha) {
    BLENDFUNCTION bf;

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.SourceConstantAlpha = 0xff;
    bf.AlphaFormat = AC_SRC_ALPHA;
    
    success = wxAlphaBlend(dc, 
			   xdest1, ydest1, iw, ih,
			   invented_dc,
			   0, 0, iw, ih,
			   bf);
  } else {
    SetTextColor(dc, 0);     /* 0 = black */
    if ((source->GetDepth() == 1) || invented_col)  {
      if ((rop == wxSOLID) || invented_col) {
	/* White pixels in the src aren't supposed to count.
	   First, paint black everywhere where the bitmap has black.
	   Then, below, "or" over the destination with a coloring of the image.
	   
	   If we're painting a color src through an invented image,
	   use the mask instead of src for this first step. */
	SetTextColor(dc, wxBLACK->pixel);
	if (mask) {
	  success = MaskBlt(dc, xdest1, ydest1, 
			    iw, ih,
			    dc_src, xsrc1, ysrc1,
			    mask->ms_bitmap, xsrc1, ysrc1,
			    MAKEROP4(wxKEEPDEST, MERGEPAINT));
	} else {
	  success = BitBlt(dc, xdest1, ydest1, 
			   iw, ih,
			   (invented_col
			    ? mdc 
			    : (invented_memdc 
			       ? invented_dc 
			       : dc_src)), 
			   xsrc1, ysrc1,
			   MERGEPAINT);
	  if (invented_col) {
	    /* zero src offset for second step, which uses the invented_dc */
	    xsrc1 = ysrc1 = 0;
	  }
	}
	op = SRCAND;
      } else {
	/* Straightforward copy (including white pixels) */
	op = ((rop == wxXOR) 
	      ? 0x00990066 /* => DSnx */
	      : SRCCOPY);  /* opaque */
      }
      SetTextColor(dc, c ? c->pixel : wxBLACK->pixel);
    } else {
      op = SRCCOPY;
      SetTextColor(dc, wxBLACK->pixel);
    }
    
    if (op && success) {
      if (mask) {
	success = MaskBlt(dc, xdest1, ydest1, 
			  iw, ih,
			  dc_src, xsrc1, ysrc1,
			  mask->ms_bitmap, xsrc1, ysrc1,
			  MAKEROP4(wxKEEPDEST, op));
      } else {
	success = BitBlt(dc, xdest1, ydest1, 
			 iw, ih,
			 invented_memdc ? invented_dc : dc_src,
			 xsrc1, ysrc1, 
			 op);
      }
    }
  }

  DoneDC(dc);
  if (sel)
    sel->DoneDC(dc_src);
  else {
    blit_dc->DoneDC(dc_src);
    blit_dc->SelectObject(NULL);
  }
  if (mdc && (mdc != dc_src)) {
    if (msel) {
      msel->DoneDC(mdc);
    } else {
      blit_mdc->DoneDC(mdc);
      blit_mdc->SelectObject(NULL);
    }
  }
  if (invented_memdc) {
    invented_memdc->DoneDC(invented_dc);
    invented_memdc->SelectObject(NULL);
    DELETE_OBJ invented_memdc;
    DELETE_OBJ invented;
  }

  return success;
}

void wxDC::GetSize(float *width, float *height)
{
  HDC dc;
  int w, h;

  dc = ThisDC();

  if (!dc) {
    *width = *height = 0;
    return;
  }

  w=::GetDeviceCaps(dc,HORZRES);
  h=::GetDeviceCaps(dc,VERTRES);
  *width = (float)MS_XDEV2LOGREL(w);
  *height = (float)MS_YDEV2LOGREL(h);

  DoneDC(dc);
}

void wxDC::GetSizeMM(float *width, float *height)
{
  HDC dc;
  int w, h;

  dc = ThisDC();

  if (!dc) {
    *width = *height = 0;
    return;
  }

  w=::GetDeviceCaps(dc,HORZSIZE);
  h=::GetDeviceCaps(dc,VERTSIZE);
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

  if (canvas) {
    cdc = ((wxWnd *)canvas->handle)->GetHDC();
  }
}

wxCanvasDC::~wxCanvasDC(void)
{
  if (wx_gl) {
    wx_gl->Reset(0, 0);
    wx_gl = NULL;
  }

  if (canvas) {
    ((wxWnd *)canvas->handle)->ReleaseHDC();
    canvas = NULL;
  }
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
  COLORREF result, col;
  HDC dc;
  int r, g, b;

  dc = ThisDC();
  if (!dc) {
    dest->Set(0, 0, 0);
    return;
  }

  r = src->Red();
  g = src->Green();
  b = src->Blue();
  col = RGB(r, g, b);
  result = GetNearestColor(dc, col);
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
  HWND hwnd = NULL;

  __type = wxTYPE_DC_PRINTER;
  wx_interactive = interactive;
  device = wxDEVICE_WINDOWS;

  if (file) {
    filename = copystring(file);
  } else
    filename = NULL;

  if (parent) {
    wxWnd *wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }

  screen_font = FALSE;

  if (interactive) {
    PRINTDLG *pd;

    pd = (PRINTDLG*)malloc(sizeof(PRINTDLG));
    
    memset(pd, 0, sizeof(PRINTDLG));
    pd->lStructSize = sizeof(PRINTDLG);
    pd->hwndOwner=hwnd;
    pd->hDevMode=(HANDLE)NULL;
    pd->hDevNames=(HANDLE)NULL;
    pd->Flags = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
    pd->nFromPage=0xFFFF;
    pd->nToPage=0xFFFF;
    pd->nMinPage=1;
    pd->nMaxPage=0xFFFF;
    pd->nCopies=1;
    pd->hInstance=(HINSTANCE)NULL;
    
    if (wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0)) {
      cdc = pd->hDC;
      ok = TRUE;
      free(pd);
    } else {
      ok = FALSE;
      free(pd);    
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

    PRINTDLG *pd;

    pd = (PRINTDLG*)malloc(sizeof(PRINTDLG));

    memset(pd, 0, sizeof(PRINTDLG));
    pd->lStructSize    = sizeof(PRINTDLG);
    pd->hwndOwner      = (HWND)NULL;
    pd->hDevMode       = NULL;
    pd->hDevNames      = NULL;
    pd->Flags          = PD_RETURNDEFAULT;
    pd->nCopies        = 1;

    if (!wxPrimitiveDialog((wxPDF)DoPrintDlg, pd, 0)) {
      free(pd);
      return NULL;
    }

    if (!pd->hDevNames) {
      free(pd);
      return NULL;
    }

    lpDevNames = (LPDEVNAMES)GlobalLock(pd->hDevNames);
    lpszDriverName = (LPSTR)lpDevNames XFORM_OK_PLUS lpDevNames->wDriverOffset;
    lpszDeviceName = (LPSTR)lpDevNames XFORM_OK_PLUS lpDevNames->wDeviceOffset;
    lpszPortName   = (LPSTR)lpDevNames XFORM_OK_PLUS lpDevNames->wOutputOffset;
    GlobalUnlock(pd->hDevNames);

    if (pd->hDevMode)
      lpDevMode = (LPDEVMODE)GlobalLock(pd->hDevMode);

    hDC = CreateDC(lpszDriverName, lpszDeviceName, lpszPortName, (DEVMODE *)lpDevMode);

    if (pd->hDevMode && lpDevMode)
      GlobalUnlock(pd->hDevMode);

    if (pd->hDevNames) {
	GlobalFree(pd->hDevNames);
	pd->hDevNames=NULL;
    }
    if (pd->hDevMode) {
       GlobalFree(pd->hDevMode);
       pd->hDevMode=NULL;
    }

    free(pd);

    return hDC;
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
  wxWnd *wnd = NULL;
  HDC dc = NULL;

  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_WINDOWS;

  if (old_dc->canvas)
    wnd = (wxWnd *)old_dc->canvas->handle;

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
  HBITMAP bm;
  wxColourMap *cm;

  if (bitmap == selected_bitmap)
    return;

  if (!cdc)
    return;

  if (wx_gl)
    wx_gl->Reset(0, 1);

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

  if (wx_gl)
    bitmap->ChangeToDIBSection(TRUE);
  
  selected_bitmap = bitmap;
  if (!read_only) {
    bitmap->selectedInto = this;
    bitmap->selectedIntoDC = -1;
  }

  bm = (HBITMAP)::SelectObject(cdc, bitmap->ms_bitmap);

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

  cm = (bitmap ? bitmap->GetColourMap() : NULL);
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

  if (wx_gl && selected_bitmap)
    wx_gl->Reset(cdc, 1);
}

wxBitmap* wxMemoryDC::GetObject(void)
{
  return selected_bitmap;
}

void wxMemoryDC::GetSize(float *width, float *height)
{
  float bw, bh;

  if (!selected_bitmap)
  {
    *width = 0.0; *height = 0.0;
    return;
  }
  bw = selected_bitmap->GetWidth();
  *width = bw;
  bh = selected_bitmap->GetHeight();
  *height = bh;
}

wxGL *wxMemoryDC::GetGL()
{
  if (!wx_gl) {
    if (cdc) {
      if (selected_bitmap && !selected_bitmap->IsDIB()) {
	::SelectObject(cdc, old_bitmap);
	selected_bitmap->ChangeToDIBSection(TRUE);
	::SelectObject(cdc, selected_bitmap->ms_bitmap);
      }

      wx_gl = new wxWinGL();
      wx_gl->Reset(cdc, 1);
    }
  }

  return wx_gl;
}

/**************************************************/

/*
 * wxGL implementation
 */

#include <gl/gl.h>
#include <gl/glu.h>
#include <gl/glaux.h>

static wxWinGL *current_gl_context;

wxGL::wxGL()
  : wxObject(WXGC_NO_CLEANUP)
{
}

wxGL::~wxGL()
{
}

wxWinGL::wxWinGL()
{
}

void wxWinGL::Reset(HDC dc, int offscreen)
{
  if (current_gl_context == this) {
    wglMakeCurrent(NULL, NULL);
  }

  if (m_hGLRC) {
    wglDeleteContext(m_hGLRC);
    m_hGLRC = NULL;
  }
  if (m_deletePalette) {
    DELETE_OBJ m_palette;
    m_palette = NULL;
    m_deletePalette = 0;
  }

  if (dc) {
    int pixelFormat;
#ifdef MZ_PRECISE_GC
    START_XFORM_SKIP;
#endif
    PIXELFORMATDESCRIPTOR pfd = {
      sizeof(PIXELFORMATDESCRIPTOR),	/* size */
      1,				/* version */
      (PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER),
      PFD_TYPE_RGBA,			/* color type */
      16,				/* prefered color depth */
      0, 0, 0, 0, 0, 0,		/* color bits (ignored) */
      0,				/* no alpha buffer */
      0,				/* alpha bits (ignored) */
      0,				/* no accumulation buffer */
      0, 0, 0, 0,			/* accum bits (ignored) */
      16,				/* depth buffer */
      0,				/* no stencil buffer */
      0,				/* no auxiliary buffers */
      PFD_MAIN_PLANE,			/* main layer */
      0,				/* reserved */
      0, 0, 0			/* no layer, visible, damage masks */
    };
#ifdef MZ_PRECISE_GC
    END_XFORM_SKIP;
#endif

    if (offscreen) {
      pfd.dwFlags = (PFD_SUPPORT_OPENGL 
		     | PFD_DRAW_TO_BITMAP
		     | PFD_SUPPORT_GDI);
      pfd.cColorBits = 32;
      pfd.cDepthBits = 32;
    }
    
    pixelFormat = ChoosePixelFormat(dc, &pfd);
    if (pixelFormat != 0) {
      if (SetPixelFormat(dc, pixelFormat, &pfd)) {
	DescribePixelFormat(dc, pixelFormat, sizeof(PIXELFORMATDESCRIPTOR), &pfd);
	if (pfd.dwFlags & PFD_NEED_PALETTE)
	  SetupPalette(&pfd);

	m_hGLRC = wglCreateContext(dc);
	m_hDC = dc;

	if (current_gl_context == this) {
	  current_gl_context = NULL;
	  ThisContextCurrent();
	}
      }
    }
  }
}

int wxWinGL::Ok(void)
{
  return !!m_hGLRC;
}

void wxWinGL::SwapBuffers(void)
{
  if (m_hDC) {
    ::SwapBuffers(m_hDC);
  }
}

void wxWinGL::ThisContextCurrent(void)
{
  if (current_gl_context != this) {
    current_gl_context = this;
    if (m_hGLRC && m_hDC)
      wglMakeCurrent(m_hDC, m_hGLRC);
    else
      wglMakeCurrent(NULL, NULL);
  }
}

void wxWinGL::SetupPalette(PIXELFORMATDESCRIPTOR *pfd)
{
  m_palette = CreateDefaultPalette(pfd);
  m_deletePalette = TRUE;
  
  if (m_palette && m_palette->ms_palette) {
    SelectPalette(m_hDC, m_palette->ms_palette, FALSE);
    RealizePalette(m_hDC);
  }
}

 wxColourMap* wxWinGL::CreateDefaultPalette(PIXELFORMATDESCRIPTOR *pfd)
{
  int paletteSize;
  LOGPALETTE* pPal;
  HPALETTE hPalette;
  wxColourMap* cmap;

  paletteSize = 1 << pfd->cColorBits;
  
  pPal = (LOGPALETTE*)new char[sizeof(LOGPALETTE) + paletteSize * sizeof(PALETTEENTRY)];
  pPal->palVersion = 0x300;
  pPal->palNumEntries = paletteSize;

  /* build a simple RGB color palette */
  {
    int redMask = (1 << pfd->cRedBits) - 1;
    int greenMask = (1 << pfd->cGreenBits) - 1;
    int blueMask = (1 << pfd->cBlueBits) - 1;
    int i;

    for (i=0; i<paletteSize; ++i) {
      pPal->palPalEntry[i].peRed =
	(((i >> pfd->cRedShift) & redMask) * 255) / redMask;
      pPal->palPalEntry[i].peGreen =
	(((i >> pfd->cGreenShift) & greenMask) * 255) / greenMask;
      pPal->palPalEntry[i].peBlue =
	(((i >> pfd->cBlueShift) & blueMask) * 255) / blueMask;
      pPal->palPalEntry[i].peFlags = 0;
    }
  }

  hPalette = CreatePalette(pPal);
  free(pPal);

  cmap = new wxColourMap;
  cmap->ms_palette = hPalette;
  
  return cmap;
}

void wxGLNoContext()
{
  current_gl_context = NULL;
  wglMakeCurrent(NULL, NULL);
}
