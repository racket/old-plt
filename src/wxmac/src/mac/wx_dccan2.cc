///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan2.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 2)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <QDOffscreen.h>
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_privt.h"

static PixMapHandle	bdiag,
		cdiag,
		fdiag,
		cross,
		horiz,
		verti;

// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

extern CGrafPtr wxMainColormap;

// constant to convert radian to degree
#define RAD2DEG 57.2957795131

//-----------------------------------------------------------------------------
void wxCanvasDC::Clear(void)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	wxMacSetCurrentTool(kNoTool);

	int w, h;
	if (canvas)
		canvas->GetVirtualSize(&w, &h);
	else {
		w = pixmapWidth;
		h = pixmapHeight;
	}

	Rect theClearRect = {0, 0, h, w};
	::EraseRect(&theClearRect);
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
   if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
     return;

	SetCurrentDC();

	wxMacSetCurrentTool(kPenTool);
	int xx = XLOG2DEV(x);
	int yy = YLOG2DEV(y);
	float ww, hh;
	GetSize(&ww, &hh) ;
	wxMacDrawLine(0, yy, ww, yy);
	wxMacDrawLine(xx, 0, xx, hh);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::FloodFill(float x, float y, wxColour *col, int style)
//=============================================================================
{
	SetCurrentDC();
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::GetPixel(float x, float y, wxColour *col)
//=============================================================================
{
	RGBColor rgb;

	SetCurrentDC();

	GetCPixel(XLOG2DEV(x), YLOG2DEV(y), &rgb);
	col->Set(rgb.red >> 8, rgb.green >> 8, rgb.blue >> 8);

	return TRUE;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPixel(float x, float y, wxColour *col)
//=============================================================================
{
	RGBColor rgb;

	SetCurrentDC();

	rgb.red = col->Red() << 8;
	rgb.green = col->Green() << 8;
	rgb.blue = col->Blue() << 8;
	SetCPixel(XLOG2DEV(x), YLOG2DEV(y), &rgb);
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
    if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
      return;

	SetCurrentDC();
	wxMacSetCurrentTool(kPenTool);
	wxMacDrawLine(XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
	CalcBoundingBox(x1, y1);
	CalcBoundingBox(x2, y2);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)
{
	SetCurrentDC();

    int xx1 = XLOG2DEV(x1); int yy1 = XLOG2DEV(y1);
    int xx2 = XLOG2DEV(x2); int yy2 = XLOG2DEV(y2);
    int xxc = XLOG2DEV(xc); int yyc = XLOG2DEV(yc);
    double dx1 = xx1 - xxc; double dy1 = yy1 - yyc;
    // double dx2 = xx1 - xxc; double dy2 = yy1 - yyc;
    double radius1 = sqrt(dx1*dx1+dy1*dy1);
    // double radius2 = sqrt(dx2*dx2+dy2*dy2);
    int    r      = (int)radius1;
    double degrees1, degrees2;

    if (xx1 == xx2 && yy1 == yy2) {
	degrees1 = 0.0;
	degrees2 = 360.0;
    } else if (radius1 == 0.0) {
	degrees1 = degrees2 = 0.0;
    } else {
	degrees1 = (xx1 - xxc == 0) ?
	    (yy1 - yyc < 0) ? 90.0 : -90.0 :
	    -atan2(double(yy1-yyc), double(xx1-xxc)) * RAD2DEG;
	degrees2 = (xx2 - xxc == 0) ?
	    (yy2 - yyc < 0) ? 90.0 : -90.0 :
	    -atan2(double(yy2-yyc), double(xx2-xxc)) * RAD2DEG;
    }
    
    /* Convert to QD angles for clockwise arc: */
    int alpha1 = (int)(-degrees2 + 90) % 360;
    if (alpha1 < 0)
      alpha1 += 360;
	int alpha2 = (int)(-degrees1 + 90) % 360;
    if (alpha2 < 0)
      alpha2 += 360;
    
    /* Alpha2 should be positive difference: */  
    alpha2 -= alpha1;
    if (alpha2 < 0)
      alpha2 += 360;

    int width = r;
    int height = r;

	Rect rect;
	rect.left = xxc - width;
	rect.top = yyc - height;
	rect.right = rect.left + 2 * width;
	rect.bottom = rect.top + 2 * height;

    if (current_brush && current_brush->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kBrushTool);
      PaintArc(&rect, alpha1, alpha2);
    }
    if (current_pen && current_pen->GetStyle() != wxTRANSPARENT) {
      wxMacSetCurrentTool(kPenTool);
      FrameArc(&rect, alpha1, alpha2);
    }
	
	CalcBoundingBox(x1, y1);
    CalcBoundingBox(x2, y2);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPoint(float x, float y)
//-----------------------------------------------------------------------------
{
    if (!current_pen || current_pen->GetStyle() == wxTRANSPARENT)
      return;

	SetCurrentDC();
	wxMacSetCurrentTool(kPenTool);
	wxMacDrawPoint(XLOG2DEV(x), YLOG2DEV(y));
	CalcBoundingBox(x, y);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPolygon(int n, wxPoint points[],
								float xoffset, float yoffset, int fillStyle)
{
	SetCurrentDC();
	if (n <= 0) return;
	Point *xpoints1 = new Point[n+1];
	for (int i = 0; i < n; i++)
	{
		xpoints1[i].h = XLOG2DEV(points[i].x + xoffset);
		xpoints1[i].v = YLOG2DEV(points[i].y + yoffset);
		CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
	}

	// Close figure
	xpoints1[n].h = xpoints1[0].h;
	xpoints1[n].v = xpoints1[0].v;

	PolyHandle thePolygon = OpenPoly();
	MoveTo(xpoints1[0].h, xpoints1[0].v);
	for (int j = 1; j <= n; j++)
	{
		LineTo(xpoints1[j].h, xpoints1[j].v);
	}
	ClosePoly();

	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
		PaintPoly(thePolygon);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kPenTool);
		FramePoly(thePolygon);
	}

	delete[] xpoints1;
	KillPoly(thePolygon);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
{
	if (n <= 0) return;
	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		SetCurrentDC();
		wxMacSetCurrentTool(kPenTool);
	
		Point *xpoints = new Point[n];
	
		for (int i = 0; i < n; i++)
		{
			xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
			xpoints[i].v = YLOG2DEV(points[i].y + yoffset);
			CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset); // WCH: not in original??
		}
	
		PolyHandle thePolygon = OpenPoly();
		MoveTo(xpoints[0].h, xpoints[0].v);
		for (int j = 1; j < n; j++)
		{
			LineTo(xpoints[j].h, xpoints[j].v);
		}
		ClosePoly();
	
		FramePoly(thePolygon);
	
		delete[] xpoints;
		KillPoly(thePolygon);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
	if (n <= 0) return;
	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		SetCurrentDC();
		wxMacSetCurrentTool(kPenTool);
	
		Point *xpoints = new Point[n];
	
		for (int i = 0; i < n; i++)
		{
			xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
			xpoints[i].v = YLOG2DEV(points[i].y + yoffset); // WCH: original mistype "h" for "v"
			CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset); // WCH: not in original??
		}
	
		PolyHandle thePolygon = OpenPoly();
		MoveTo(xpoints[0].h, xpoints[0].v);
		for (int j = 1; j < n; j++)
		{
			LineTo(xpoints[j].h, xpoints[j].v);
		}
		ClosePoly();
	
		FramePoly(thePolygon);
	
		delete[] xpoints;
		KillPoly(thePolygon);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRectangle(float x, float y, float width, float height)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	int top = YLOG2DEV(y);
	int left = XLOG2DEV(x);
	int bottom = YLOG2DEV(y + height);
	int right = XLOG2DEV(x + width);
	Rect theRect = {top, left, bottom, right};
	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kBrushTool);
		PaintRect(&theRect);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kPenTool);
		FrameRect(&theRect);
	}
	CalcBoundingBox(x, y);
	CalcBoundingBox(x + width, y + height);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRoundedRectangle
					(float x, float y, float width, float height, float radius)
{
	SetCurrentDC();
	if (radius < 0.0) radius = 0.0; // Negative radius can crash your ENTIRE X server. Wow!
    
	int phys_radius = XLOG2DEVREL(radius);

	int phys_rwidth = phys_radius * 2;
	int phys_rheight = phys_rwidth;

	int top = YLOG2DEV(y);
	int left = XLOG2DEV(x);
	int bottom = YLOG2DEV(y + height);
	int right = XLOG2DEV(x + width);
	Rect theRect = {top, left, bottom, right};

	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kBrushTool);
		PaintRoundRect(&theRect, phys_rwidth, phys_rheight);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kPenTool);
		FrameRoundRect(&theRect, phys_rwidth, phys_rheight);
	}

	CalcBoundingBox(x, y);
	CalcBoundingBox(x + width, y + height);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawEllipse(float x, float y, float width, float height)
{
	SetCurrentDC();
	int top = YLOG2DEV(y);
	int left = XLOG2DEV(x);
	int bottom = YLOG2DEV(y + height);
	int right = XLOG2DEV(x + width);
	Rect theRect = {top, left, bottom, right};
	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kBrushTool);
		PaintOval(&theRect);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		wxMacSetCurrentTool(kPenTool);
		FrameOval(&theRect);
	}
	CalcBoundingBox(x, y);
	CalcBoundingBox(x + width, y + height);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawIcon(wxIcon *icon, float x, float y)
{
	int h, w;
	w = icon->GetWidth();
	h = icon->GetHeight();
	if (!icon->selectedIntoDC && icon->Ok()) {
		wxMemoryDC *mdc;
		mdc = new wxMemoryDC();
		mdc->SelectObject(icon);
		if (mdc->Ok())
			Blit(x, y, w, h, mdc, 0, 0);
		mdc->SelectObject(NULL);
		delete mdc;
	}
}

/*-----------------------------------
	Ideally, Blit() should munged its args and call the Mac ToolBox 
	CopyBits() function. 
    We also have to be aware that the 'source' arg could actually be a
	wxMemoryDC and we may have to switch GWorlds. 
		I.e if (this->device != source->device)
*/
Bool wxCanvasDC::Blit(float xdest, float ydest, float width, float height,
                wxCanvasDC *source, float xsrc, float ysrc, int rop)
{
	if (device != source->device) {
		// Switch Gworld to this
		SetGWorld(cMacDC->macGrafPort(), 0);
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC();
	}
	else {
		SetCurrentDC();
	}
	if (rop == wxCOLOR) {
	  wxMacSetCurrentTool(kColorBlitTool);
	  rop = wxCOPY;
	} else
	  wxMacSetCurrentTool(kBlitTool);
	Bool theResult = FALSE;

	if (pixmap && source->pixmap)
	{
		int mode;
		switch (rop)
		{ // FIXME -  these modes have not be tested
		//	case wxCLEAR:  theMacPenMode = GXclear; break;
			case wxXOR:  
				mode = srcXor; 
				break;
		//	case wxINVERT: theMacPenMode = GXinvert; break;
		//	case wxOR_REVERSE: theMacPenMode = GXorReverse; break;
		//	case wxAND_REVERSE: theMacPenMode = GXandReverse; break;
			case wxAND:
				mode = srcBic;
				break;
			case wxOR: 
				mode = srcOr; 
				break;
			case wxAND_INVERT: 
				mode = notSrcBic; break;
		//	case wxNO_OP: theMacPenMode = GXnoop; break;
		//	case wxNOR: theMacPenMode = GXnor; break;
			case wxEQUIV: 
				mode = notSrcXor; break;
			case wxSRC_INVERT: 
				mode = notSrcCopy; break;
			case wxOR_INVERT: 
				mode = notSrcOr; break;
		//	case wxNAND: theMacPenMode = GXnand; break;
		//	case wxSET: theMacPenMode = GXset; break;
			case wxCOPY:
			default:
				mode = srcCopy;
				break;
		}
		int h = height;
		int w = width;
		int x = XLOG2DEV(xdest);
		int y = YLOG2DEV(ydest);
		int ixsrc = source->LogicalToDeviceX(xsrc);
		int iysrc = source->LogicalToDeviceY(ysrc);
		Rect srcr = {iysrc, ixsrc, iysrc + h, ixsrc + w};
		Rect destr = {y, x, y+h, x+w };
		
		GrafPtr theMacGrafPort = (GrafPtr)cMacDC->macGrafPort();
        BitMap *dstbm;
        PixMapHandle destpixh;
        if ((((CGrafPtr)theMacGrafPort)->portVersion & 0xC000) != 0xC000) {
          destpixh = NULL;
          dstbm = (BitMap *) &((GrafPtr)(cMacDC->macGrafPort()))->portBits;
        } else {
          destpixh = ((CGrafPtr)theMacGrafPort)->portPixMap;
		  ::LockPixels(destpixh);
		  dstbm = (BitMap *)(* destpixh);
        }

		// Lock PixMaps
		PixMapHandle srpixh = source->pixmap;
		int rs = ::LockPixels(srpixh);

		::CopyBits((BitMap *)(*srpixh), dstbm, &srcr, &destr, mode, NULL);

		if (destpixh) ::UnlockPixels(destpixh);
		::UnlockPixels(srpixh);
		CalcBoundingBox(xdest, ydest);
		CalcBoundingBox(xdest + width, ydest + height);
		theResult = TRUE;
	}

  	return theResult;
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  SetCurrentDC();

  if (Colour) {
	RGBColor pixel = src->pixel;
	
	Index2Color(Color2Index(&pixel), &pixel);
	
	dest->Set(pixel.red >> 8, pixel.green >> 8, pixel.blue >> 8); 
  } else {
	unsigned char red = src->Red();
	unsigned char blue = src->Blue();
	unsigned char green = src->Green();
	Bool isWhiteColour =
		(red == (unsigned char )255 &&
		 blue == (unsigned char)255 &&
		 green == (unsigned char)255);
	if (isWhiteColour)
		dest->Set(255, 255, 255);
	else
		dest->Set(0, 0, 0);
  }
}
