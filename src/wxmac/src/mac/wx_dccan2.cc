///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan2.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 2)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_area.h"
#include "wx_rgn.h"

extern CGrafPtr wxMainColormap;

// constant to convert radian to degree
#define RAD2DEG 57.2957795131

//-----------------------------------------------------------------------------
void wxCanvasDC::Clear(void)
     //-----------------------------------------------------------------------------
{
  int w, h;
  Rect theClearRect;
  
  if (!Ok() || !cMacDC) return;
  
  SetCurrentDC();
  wxMacSetCurrentTool(kBGTool);

  if (canvas)
    canvas->GetVirtualSize(&w, &h);
  else {
    w = pixmapWidth;
    h = pixmapHeight;
  }

  ::SetRect(&theClearRect, 0, 0, w, h);
  OffsetRect(&theClearRect,SetOriginX,SetOriginY);
  ::EraseRect(&theClearRect);

  ReleaseCurrentDC();
}

void wxCanvasDC::GetSize(float *width, float *height)
{
  if (canvas) {
    int w, h;
    canvas->GetVirtualSize(&w, &h);
    *width = w;
    *height = h;
  } else {
    *width = pixmapWidth;
    *height = pixmapHeight;
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::CrossHair(float x, float y)
     //-----------------------------------------------------------------------------
{
  int xx, yy, dpx, dpy;
  float ww, hh;
  
  if (!Ok() || !cMacDC || !current_pen || current_pen->GetStyle() == wxTRANSPARENT)
    return;

  SetCurrentDC();

  dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
  dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

  wxMacSetCurrentTool(kPenTool);
  xx = XLOG2DEV(x);
  yy = YLOG2DEV(y);
  GetSize(&ww, &hh) ;
  wxMacDrawLine(0, yy-dpy, (int)floor(ww), yy-dpy);
  wxMacDrawLine(xx-dpx, 0, xx-dpx, (int)floor(hh));

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::FloodFill(float x, float y, wxColour *col, int style)
     //=============================================================================
{
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::GetPixel(float x, float y, wxColour *col)
     //=============================================================================
{
  RGBColor rgb;

  if (!Ok() || !cMacDC) return FALSE;
  
  SetCurrentDC();

  GetCPixel(XLOG2DEV(x) + SetOriginX, YLOG2DEV(y) + SetOriginY, &rgb);
  col->Set(rgb.red >> 8, rgb.green >> 8, rgb.blue >> 8);

  ReleaseCurrentDC();

  return TRUE;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPixel(float x, float y, wxColour *col)
     //=============================================================================
{
  RGBColor rgb;

  if (!Ok() || !cMacDC) return;
  
  SetCurrentDC();

  if (Colour) {
    rgb.red = col->Red();
    rgb.red = (rgb.red << 8) | rgb.red;
    rgb.green = col->Green();
    rgb.green = (rgb.green << 8) | rgb.green;
    rgb.blue = col->Blue();
    rgb.blue = (rgb.blue << 8) | rgb.blue;

    SetCPixel(XLOG2DEV(x) + SetOriginX, YLOG2DEV(y) + SetOriginY, &rgb);
  } else {
    int qcol;

    if ((col->Red() == 255) && (col->Blue() == 255) && (col->Green() == 255)) {
      qcol = whiteColor;
    } else {
      qcol = blackColor;      
    }

    GetForeColor(&rgb);
    ForeColor(qcol);
    wxMacDrawPoint(XLOG2DEV(x) + SetOriginX, YLOG2DEV(y) + SetOriginY);
    if (rgb.red) {
      if (qcol != whiteColor)
	ForeColor(whiteColor);
    } else {
      if (qcol != blackColor)
	ForeColor(blackColor);
    }
  }

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::IntDrawLine(int x1, int y1, int x2, int y2)
     //-----------------------------------------------------------------------------
{
  DrawLine(x1, y1, x2, y2);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLine(float x1, float y1, float x2, float y2)
     //-----------------------------------------------------------------------------
{
  int dpx, dpy;

  if (!Ok() || !cMacDC) return;
  
  if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
    return;

  dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
  dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

  SetCurrentDC();
  wxMacSetCurrentTool(kPenTool);
  wxMacDrawLine(XLOG2DEV(x1)-dpx, YLOG2DEV(y1)-dpy, XLOG2DEV(x2)-dpx, YLOG2DEV(y2)-dpy);
  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
static void FillWithStipple(wxDC *dc, wxRegion *r, wxBrush *brush)
{
  float x, y, w, h, bw, bh;
  int xstart, xend, ystart, yend, i, j, ibw, ibh;
  wxRegion *old;
  int style;
  wxColour *c;
  wxBitmap *bm;

  old = dc->GetClippingRegion();
  if (old)
    r->Intersect(old);

  if (r->Empty())
    return;

  bm = brush->GetStipple();
  style = brush->GetStyle();
  c = brush->GetColour();

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

  ibw = (int)floor(bw);
  ibh = (int)floor(bh);

  dc->SetClippingRegion(r);

  for (i = xstart; i < xend; i++) {
    for (j = ystart; j < yend; j++) {
      dc->Blit(dc->DeviceToLogicalX(i * ibw), 
               dc->DeviceToLogicalY(j * ibh), 
               dc->DeviceToLogicalXRel(ibw), 
               dc->DeviceToLogicalYRel(ibh),
               bm, 0, 0, style, c);
    }
  }

  dc->SetClippingRegion(old);
}

wxRegion *wxCanvasDC::BrushStipple()
{
  if (current_brush) {
    wxBitmap *bm;
    bm = current_brush->GetStipple();
    if (bm && bm->Ok())
      return new wxRegion(this);
  }
  return NULL;
}

void wxCanvasDC::PaintStipple(wxRegion *r)
{
  FillWithStipple(this, r, current_brush);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawArc(float x,float y,float w,float h,float start,float end)
{
  wxRegion *rgn;
  int xx, yy, xx2, yy2;
  int alpha1, alpha2;
  double degrees1, degrees2;
  Rect rect;

  if (!Ok() || !cMacDC) return;
  
  if (start == end) {
    DrawEllipse(x, y, w, h);
    return;
  }

  if ((rgn = BrushStipple())) {
    rgn->SetArc(x, y, w, h, start, end);
    PaintStipple(rgn);
  }

  SetCurrentDC();

  xx = XLOG2DEV(x); yy = YLOG2DEV(y);
  xx2 = XLOG2DEV(x+w); yy2 = YLOG2DEV(y+h);
  
  degrees1 = start * RAD2DEG;
  degrees2 = end * RAD2DEG;
  
  /* Convert to QD angles for clockwise arc: */
   alpha1 = (int)(-degrees2 + 90) % 360;
  if (alpha1 < 0)
    alpha1 += 360;
  alpha2 = (int)(-degrees1 + 90) % 360;
  if (alpha2 < 0)
    alpha2 += 360;
  
  /* Alpha2 should be positive difference: */  
  alpha2 -= alpha1;
  if (alpha2 < 0)
    alpha2 += 360;

  rect.left = xx;
  rect.top = yy;
  rect.right = xx2;
  rect.bottom = yy2;
  OffsetRect(&rect,SetOriginX,SetOriginY);

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    if (!rgn) {
      wxMacSetCurrentTool(kBrushTool);
      if (paint_brush_with_erase)
	EraseArc(&rect, alpha1, alpha2);
      else
	PaintArc(&rect, alpha1, alpha2);
    }
  }
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    wxMacSetCurrentTool(kPenTool);
    FrameArc(&rect, alpha1, alpha2);
  }
  
  CalcBoundingBox(x, y);
  CalcBoundingBox(x + w, y + h);

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPoint(float x, float y)
     //-----------------------------------------------------------------------------
{
  if (!Ok() || !cMacDC) return;
  
  if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
    return;

  SetCurrentDC();
  wxMacSetCurrentTool(kPenTool);
  wxMacDrawPoint(XLOG2DEV(x), YLOG2DEV(y));
  CalcBoundingBox(x, y);
  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPolygon(int n, wxPoint points[],
			     float xoffset, float yoffset, int fillStyle)
{
  wxRegion *rgn;
  Point *xpoints1;
  int i, j;
  PolyHandle thePolygon;

  if (!Ok() || !cMacDC) return;
  
  if (n <= 0) return;

  if ((rgn = BrushStipple())) {
    rgn->SetPolygon(n, points, xoffset, yoffset, fillStyle);
    PaintStipple(rgn);
  }

  SetCurrentDC();

  xpoints1 = new Point[n+1];
  for (i = 0; i < n; i++) {
    xpoints1[i].h = XLOG2DEV(points[i].x + xoffset);
    xpoints1[i].v = YLOG2DEV(points[i].y + yoffset);
    CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
  }

  // Close figure
  xpoints1[n].h = xpoints1[0].h;
  xpoints1[n].v = xpoints1[0].v;

  thePolygon = OpenPoly();
  MoveTo(xpoints1[0].h + SetOriginX, xpoints1[0].v + SetOriginY);
  for (j = 1; j <= n; j++) {
    LineTo(xpoints1[j].h + SetOriginX, xpoints1[j].v + SetOriginY);
  }
  ClosePoly();

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
    if (!rgn) {
      if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
      if (paint_brush_with_erase)
	ErasePoly(thePolygon);
      else
	PaintPoly(thePolygon);
    }
  }

  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    wxMacSetCurrentTool(kPenTool);
    FramePoly(thePolygon);
  }

  KillPoly(thePolygon);

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
{
  if (!Ok() || !cMacDC) return;
  
  if (n <= 0) return;
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    Point *xpoints;
    int i, j, dpx, dpy;
    PolyHandle thePolygon;

    SetCurrentDC();
    wxMacSetCurrentTool(kPenTool);
    
    xpoints = new Point[n];
    
    dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
    dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

    for (i = 0; i < n; i++) {
      xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
      xpoints[i].v = YLOG2DEV(points[i].y + yoffset);
      CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset); // WCH: not in original??
    }
    
    thePolygon = OpenPoly();
    MoveTo(xpoints[0].h + SetOriginX - dpx, xpoints[0].v + SetOriginY - dpy);
    for (j = 1; j < n; j++) {
      LineTo(xpoints[j].h + SetOriginX - dpx, xpoints[j].v + SetOriginY - dpy);
    }
    ClosePoly();
      
    FramePoly(thePolygon);
    
    KillPoly(thePolygon);

    ReleaseCurrentDC();
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
  if (!Ok() || !cMacDC) return;
  
  if (n <= 0) return;
  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
    Point *xpoints;
    int i, j, dpx, dpy;
    PolyHandle thePolygon;

    SetCurrentDC();
    wxMacSetCurrentTool(kPenTool);
    
    xpoints = new Point[n];
      
    dpx = (XLOG2DEVREL(current_pen->GetWidth()) >> 1);
    dpy = (YLOG2DEVREL(current_pen->GetWidth()) >> 1);

    for (i = 0; i < n; i++) {
      xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
      xpoints[i].v = YLOG2DEV(points[i].y + yoffset); // WCH: original mistype "h" for "v"
      CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset); // WCH: not in original??
    }
      
    thePolygon = OpenPoly();
    MoveTo(xpoints[0].h + SetOriginX - dpx, xpoints[0].v + SetOriginY - dpy);
    for (j = 1; j < n; j++) {
      LineTo(xpoints[j].h + SetOriginX - dpx, xpoints[j].v + SetOriginY - dpy);
    }
    ClosePoly();
    
    FramePoly(thePolygon);
    
    KillPoly(thePolygon);
    
    ReleaseCurrentDC();
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRectangle(float x, float y, float width, float height)
     //-----------------------------------------------------------------------------
{
  wxRegion *rgn;

  if (!Ok() || !cMacDC) return;
  
  if ((rgn = BrushStipple())) {
    rgn->SetRectangle(x, y, width, height);
    PaintStipple(rgn);
  }

  SetCurrentDC();
  
  {
    int top = YLOG2DEV(y);
    int left = XLOG2DEV(x);
    int bottom = YLOG2DEV(y + height);
    int right = XLOG2DEV(x + width);
    Rect theRect = {top, left, bottom, right};
    OffsetRect(&theRect,SetOriginX,SetOriginY);
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      if (!rgn) {
	wxMacSetCurrentTool(kBrushTool);
	if (paint_brush_with_erase)
	  EraseRect(&theRect);
	else
	  PaintRect(&theRect);
      }
    }
    
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      FrameRect(&theRect);
    }
    CalcBoundingBox(x, y);
    CalcBoundingBox(x + width, y + height);
  }

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRoundedRectangle
(float x, float y, float width, float height, float radius)
{
  wxRegion *rgn;

  if (!Ok() || !cMacDC) return;
  
  if ((rgn = BrushStipple())) {
    rgn->SetRoundedRectangle(x, y, width, height, radius);
    PaintStipple(rgn);
  }

  SetCurrentDC();
  
  if (radius < 0.0) {
    float w = width;
    if (height < w)
      w = height;
    radius = (-radius) * w;
  }

  {
    int phys_radius = XLOG2DEVREL(radius);
    
    int phys_rwidth = phys_radius * 2;
    int phys_rheight = phys_rwidth;
    
    int top = YLOG2DEV(y);
    int left = XLOG2DEV(x);
    int bottom = YLOG2DEV(y + height);
    int right = XLOG2DEV(x + width);
    Rect theRect = {top, left, bottom, right};

    OffsetRect(&theRect,SetOriginX,SetOriginY);
    
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      if (!rgn) {
	wxMacSetCurrentTool(kBrushTool);
	if (paint_brush_with_erase)
	  EraseRoundRect(&theRect, phys_rwidth, phys_rheight);
	else
	  PaintRoundRect(&theRect, phys_rwidth, phys_rheight);
      }
    }

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      FrameRoundRect(&theRect, phys_rwidth, phys_rheight);
    }
    
    CalcBoundingBox(x, y);
    CalcBoundingBox(x + width, y + height);
  }

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawEllipse(float x, float y, float width, float height)
{
  wxRegion *rgn;

  if (!Ok() || !cMacDC) return;
  
  if ((rgn = BrushStipple())) {
    rgn->SetEllipse(x, y, width, height);
    PaintStipple(rgn);
  }

  SetCurrentDC();
  
  {
    int top = YLOG2DEV(y);
    int left = XLOG2DEV(x);
    int bottom = YLOG2DEV(y + height);
    int right = XLOG2DEV(x + width);
    Rect theRect = {top, left, bottom, right};
    OffsetRect(&theRect,SetOriginX,SetOriginY);
    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      if (!rgn) {
	wxMacSetCurrentTool(kBrushTool);
	if (paint_brush_with_erase)
	  EraseOval(&theRect);
	else
	  PaintOval(&theRect);
      }
    }

    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      FrameOval(&theRect);
    }
    CalcBoundingBox(x, y);
    CalcBoundingBox(x + width, y + height);
  }

  ReleaseCurrentDC();
}

static int noDCSet = 0; /* back door for GCBlit */

Bool wxCanvasDC::Blit(float xdest, float ydest, float width, float height,
		      wxBitmap *source, float xsrc, float ysrc, int rop, wxColour *c,
		      wxBitmap *mask)
{
  if (!Ok() || !cMacDC || !source->Ok()) return FALSE;
  if (mask && !mask->Ok()) return FALSE;

  if (!noDCSet) {
    SetCurrentDC();
  
    if (source->GetDepth() == 1) {
      wxMacSetCurrentTool(kColorBlitTool);
      if (rop == wxSOLID) BackColor(whiteColor);
      if (c)
	InstallColor(c, TRUE);
      else
	ForeColor(blackColor);
    } else {
      wxMacSetCurrentTool(kBlitTool);
      rop = wxSTIPPLE;
    }
  }

  {
    int mode;
    int ixsrc, iysrc, h, w, x, y;

    if (source->GetDepth() == 1) {
      switch (rop)
	{
	case wxXOR:  
	  mode = srcXor; 
	  break;
	case wxSOLID:
	  mode = srcOr;
	  break;
	case wxSTIPPLE: /* = opaque */
	default:
	  mode = srcCopy;
	  break;
	}
    } else
      mode = srcCopy;
    
    ixsrc = (int)floor(xsrc);
    iysrc = (int)floor(ysrc);
    
    if (ixsrc > source->GetWidth()) {
      if (!noDCSet)
	ReleaseCurrentDC();
      return TRUE;
    }
    if (iysrc > source->GetHeight()) {
      if (!noDCSet)
	ReleaseCurrentDC();
      return TRUE;
    }

    if (iysrc + height > source->GetHeight())
      height = source->GetHeight() - iysrc;
    if (ixsrc + width > source->GetWidth())
      width = source->GetWidth() - ixsrc;

    x = XLOG2DEV(xdest);
    y = YLOG2DEV(ydest);
    h = YLOG2DEV(height + xdest) - x;
    w = XLOG2DEV(width + ydest) - y;
    
    {
      Rect srcr = {iysrc, ixsrc, iysrc + (int)height, ixsrc + (int)width};
      Rect destr = {y, x, y+h, x+w };
      CGrafPtr theMacGrafPort;
      const BitMap *dstbm;
      const BitMap *srcbm;

      OffsetRect(&destr,SetOriginX,SetOriginY);
      
      theMacGrafPort = cMacDC->macGrafPort();

      dstbm = GetPortBitMapForCopyBits(theMacGrafPort);
      srcbm = GetPortBitMapForCopyBits(source->x_pixmap);
      
      if (mask) {
	const BitMap *maskbm;
	
	maskbm = GetPortBitMapForCopyBits(mask->x_pixmap);

	::CopyDeepMask(srcbm, maskbm, dstbm, &srcr, &srcr, &destr, mode, NULL);
      } else {
	::CopyBits(srcbm, dstbm, &srcr, &destr, mode, NULL);
      }
      
      CalcBoundingBox(xdest, ydest);
      CalcBoundingBox(xdest + width, ydest + height);
    }
  }

  if (!noDCSet)
    ReleaseCurrentDC();

  return TRUE;
}

Bool wxCanvasDC::GCBlit(float xdest, float ydest, float width, float height,
			wxBitmap *source, float xsrc, float ysrc)
{
  /* Non-allocating (i.e. no collectable allocation) Blit. Looks like
     the normal one will work, but we need to be careful about shifting the
     current drawing port. So we do the setup manually here and restore it
     completely. */
  Bool isok;
  CGrafPtr savep;
  GDHandle savegd;
  ThemeDrawingState state;
  long ox, oy;
  Rect clientRect = {-32767, -32767, 32767, 32767};
  CGrafPtr theMacGrafPort;
  RgnHandle rgn;

  ::GetGWorld(&savep, &savegd);  

  theMacGrafPort = cMacDC->macGrafPort();
  if (IsPortOffscreen(theMacGrafPort)) {
    ::SetGWorld(theMacGrafPort, NULL);
  } else {
    ::SetGWorld(theMacGrafPort, GetMainDevice());
  }

  ox = SetOriginX;
  oy = SetOriginY;
  SetOriginX = SetOriginY = 0;
  if (canvas) {
    wxArea *area;
    int aw, ah;
    area = canvas->ClientArea();
    area->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
    aw = area->Width();
    ah = area->Height();
    ::SetRect(&clientRect, SetOriginX, SetOriginY, SetOriginX + aw, SetOriginY + ah);
  }

  GetThemeDrawingState(&state);

  noDCSet = 1;

  ForeColor(blackColor);
  BackColor(whiteColor);
  BackPat(GetWhitePattern());
  PenMode(patCopy);

  rgn = NewRgn();
  if (rgn) {
    GetClip(rgn);
    ::ClipRect(&clientRect);
  }

  isok = Blit(xdest, ydest, width, height, source, xsrc, ysrc, wxSTIPPLE, NULL);

  noDCSet = 0;

  if (rgn) {
    SetClip(rgn);
    DisposeRgn(rgn);
  }

  SetThemeDrawingState(state, TRUE);
  SetOriginX = ox;
  SetOriginY = oy;

  ::SetGWorld(savep, savegd);

  if (canvas)
    canvas->FlushDisplay();

  return isok;
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  if (!Ok() || !cMacDC) return;
  
  SetCurrentDC();

  if (Colour) {
    RGBColor pixel = src->pixel;
    
    Index2Color(Color2Index(&pixel), &pixel);
    
    dest->Set(pixel.red >> 8, pixel.green >> 8, pixel.blue >> 8); 
  } else {
    unsigned char red, blue, green;
    Bool isWhiteColour;

    red = src->Red();
    blue = src->Blue();
    green = src->Green();
    isWhiteColour =
      (red == (unsigned char )255 &&
       blue == (unsigned char)255 &&
       green == (unsigned char)255);
    if (isWhiteColour)
      dest->Set(255, 255, 255);
    else
      dest->Set(0, 0, 0);
  }

  ReleaseCurrentDC();
}
