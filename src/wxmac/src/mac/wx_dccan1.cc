///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan1.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 1)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef OS_X
  #include <QuickDraw.h>
#endif
#include "wx_dccan.h"
#include "wx_canvs.h"
#include "wx_privt.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"
#include "wx_rgn.h"

//#include "bdiag.xbm"
//#include "fdiag.xbm"
//#include "cdiag.xbm"
//#include "horiz.xbm"
//#include "verti.xbm"
//#include "cross.xbm"
//#include "wx_area.h"
//
//static PixMapHandle	bdiag,
//		cdiag,
//		fdiag,
//		cross,
//		horiz,
//		verti;


extern CGrafPtr wxMainColormap;

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(void)
{
   Init(NULL);
}


//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(wxCanvas* the_canvas): wxbCanvasDC(the_canvas)
{
  Init(the_canvas);
}

void wxCanvasDC::Init(wxCanvas* the_canvas)
{
  __type = wxTYPE_DC_CANVAS;

  canvas = the_canvas;
  if (canvas) {
    WXGC_IGNORE(this, canvas);
   cMacDC = canvas->MacDC();
   CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
   pixmap = GetPortPixMap(theMacGrafPort);
  }

  cMacDoingDrawing = FALSE;

  clipping = FALSE;
  selected_pixmap = NULL;

  pixmapWidth = 0;
  pixmapHeight = 0;

  current_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0; max_x = 0; max_y = 0;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;
  logical_origin_x = 0;
  logical_origin_y = 0;
  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;

  ok = TRUE;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = new wxColour(wxWHITE);
  current_text_foreground = new wxColour(wxBLACK);
  current_text_background = new wxColour(wxWHITE);
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  if (the_canvas) {
    int clientWidth, clientHeight;
    the_canvas->GetClientSize(&clientWidth, &clientHeight);
    Rect paintRect = {0, 0, clientHeight, clientWidth};
    SetPaintRegion(&paintRect);
  }
}

//-----------------------------------------------------------------------------
wxCanvasDC::~wxCanvasDC(void)
{
  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  
  if (current_reg) ::DisposeRgn(current_reg);
  current_reg = NULL;
  if (onpaint_reg) ::DisposeRgn(onpaint_reg);
  onpaint_reg = NULL;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::BeginDrawing(void)
//-----------------------------------------------------------------------------
{
	if (cMacDoingDrawing) {
	 // wxFatalError("Tried BeginDrawing while already DoingDrawing.");
	 return;
	}
	cMacDoingDrawing = TRUE;

	SetCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::EndDrawing(void)
//-----------------------------------------------------------------------------
{
	if (!cMacDoingDrawing) {
	  // wxFatalError("Tried EndDrawing while not DoingDrawing.");
	  return;
	}
	
	cMacDoingDrawing = FALSE;
}

GDHandle wxGetGDHandle(void)
{
  static GDHandle def_dev_handle = 0;

  if (!def_dev_handle) {
    CGrafPtr p;
    GetGWorld(&p, &def_dev_handle);
  }
  
  return def_dev_handle;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCurrentDC(void) // mac platform only
//-----------------------------------------------------------------------------
{
	if (!Ok()) return;
	 
        CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
        if (theMacGrafPort != GetQDGlobalsThePort())
	  ::SetGWorld(theMacGrafPort, wxGetGDHandle());

	if (cMacDC->currentUser() != this)
	{ // must setup platform
		cMacDC->setCurrentUser(this);
                SetOriginX = SetOriginY = 0;
		if (canvas)
			canvas->ClientArea()->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
		wxMacSetClip();
		cMacCurrentTool = kPenTool; /* to force setting bg, etc. */
		wxMacSetCurrentTool(kNoTool);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCanvasClipping(void)
//-----------------------------------------------------------------------------
{
        if (current_reg) ::DisposeRgn(current_reg);
	if (clipping || onpaint_reg) {
		current_reg = ::NewRgn();
		CheckMemOK(current_reg);
	} else
		current_reg = NULL;
		
	if (onpaint_reg && clipping)
		::SectRgn(clipping->rgn, onpaint_reg, current_reg) ;
	else if (clipping)
		::CopyRgn(clipping->rgn, current_reg) ;
	else if (onpaint_reg)
		::CopyRgn(onpaint_reg, current_reg);

	if (!Ok()) return;
 
    	wxObject* theCurrentUser = cMacDC->currentUser();
	if (theCurrentUser == this)
	{ // must update platfrom
		CGrafPtr savep;
		GDHandle savegd;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		 
		::GetGWorld(&savep, &savegd);  
		::SetGWorld(theMacGrafPort, wxGetGDHandle());

		wxMacSetClip();

		::SetGWorld(savep, savegd);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetClippingBox(float *x,float *y,float *w,float *h)
//-----------------------------------------------------------------------------
{
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();

	if (current_reg)
	{
                RgnHandle clipRgn;
                Rect theClipRect;
                
                GetPortClipRegion(theMacGrafPort,clipRgn);
                GetRegionBounds(clipRgn,&theClipRect);

		int theX = theClipRect.left;
		int theY = theClipRect.top;
		int theWidth = theClipRect.right - theClipRect.left;
		int theHeight = theClipRect.bottom - theClipRect.top;
		*x = XDEV2LOG(theX) ;
		*y = YDEV2LOG(theY) ;
		*w = XDEV2LOGREL(theWidth) ;
		*h = YDEV2LOGREL(theHeight) ;
	}
	else
		*x = *y = *w = *h = 0; // WCH wx_win: this doesn't seem right
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPaintRegion(Rect* paintRect)
//-----------------------------------------------------------------------------
{
	if (onpaint_reg) ::DisposeRgn(onpaint_reg);
	onpaint_reg = ::NewRgn();
	CheckMemOK(onpaint_reg);
	::RectRgn(onpaint_reg, paintRect);
	SetCanvasClipping();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRect(float cx, float cy, float cw, float ch)
//-----------------------------------------------------------------------------
{
  clipping = new wxRegion(this);
  clipping->SetRectangle(cx, cy, cw, ch);
  SetCanvasClipping();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRegion(wxRegion *r)
//-----------------------------------------------------------------------------
{
  clipping = r;
  SetCanvasClipping();
}

//-----------------------------------------------------------------------------
wxRegion* wxCanvasDC::GetClippingRegion()
//-----------------------------------------------------------------------------
{
  return clipping;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetFont(wxFont *the_font)
{
	font = the_font;
	ToolChanged(kTextTool);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPen(wxPen *pen)
{
	if (current_pen) current_pen->Lock(-1);
	current_pen = pen;
	if (current_pen) current_pen->Lock(1);

    ToolChanged(kPenTool);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetBrush(wxBrush *brush)
{
	if (current_brush) current_brush->Lock(-1);
	current_brush = brush;
	if (current_brush) current_brush->Lock(1);

    ToolChanged(kBrushTool);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::InstallColor(wxColour *c, int fg)
{
   RGBColor pixel;
   pixel = c->pixel;
   if (Colour) {
	  if (fg)
	    RGBForeColor(&pixel);
	  else
	    RGBBackColor(&pixel);
   } else {
		unsigned char red = c->Red();
		unsigned char blue = c->Blue();
		unsigned char green = c->Green();
		if (fg) {
		  Bool isWhiteColour =
			(red == (unsigned char )255 &&
			 blue == (unsigned char)255 &&
			 green == (unsigned char)255);
		  ForeColor(isWhiteColour ? whiteColor : blackColor);
		} else {
		  Bool isBlackColour =
				(red == (unsigned char )0 &&
				 blue == (unsigned char)0 &&
				 green == (unsigned char)0);
		  BackColor(isBlackColour ? blackColor : whiteColor);
		}
	}
}

void wxCanvasDC::InstallLogicalFunction(int function)
{
		int theMacPenMode;
		switch (function)
		{
			case wxCLEAR:  theMacPenMode = patBic; break;
			case wxXOR:  theMacPenMode = patXor; break;
			case wxINVERT: theMacPenMode = notPatCopy /* GXinvert */; break;
			case wxOR_REVERSE: theMacPenMode = patXor /* GXorReverse */; break;
			case wxAND_REVERSE: theMacPenMode = patCopy /* GXandReverse */; break;
			case wxAND: theMacPenMode = adMin; break;
			case wxOR: theMacPenMode = adMax; break;
			case wxAND_INVERT: theMacPenMode = patBic; break;
			case wxNO_OP: theMacPenMode = patCopy /* GXnoop */; break;
			case wxNOR: theMacPenMode = notPatOr /* GXnor */; break;
			case wxEQUIV: theMacPenMode = notPatXor; break;
			case wxSRC_INVERT: theMacPenMode = notPatCopy; break;
			case wxOR_INVERT: theMacPenMode = notPatOr; break;
			case wxNAND: theMacPenMode = notPatCopy /* GXnand */; break;
			case wxSET: theMacPenMode = patCopy /* GXset */; break;
			case wxCOPY:
			default:
				theMacPenMode = patCopy; break;
		}
	
		PenMode(theMacPenMode);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetBackground(wxColour* color)
{
    current_background_color = color;

    if (Ok()) {
 
        if (cMacDC->currentUser() == this
		&& (cMacCurrentTool == kNoTool))
	{ // must update platform to kNoTool
		CGrafPtr savep;
		GDHandle savegd;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		 
		::GetGWorld(&savep, &savegd);  
		::SetGWorld(theMacGrafPort, wxGetGDHandle());

        InstallColor(color, FALSE);

		::SetGWorld(savep, savegd);
	}
    }

    ToolChanged(kNoTool);
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::StartDoc(char *message) { return TRUE; }

//-----------------------------------------------------------------------------
void wxCanvasDC::EndDoc(void) { }

//-----------------------------------------------------------------------------
void wxCanvasDC::StartPage(void) { }

//-----------------------------------------------------------------------------
void wxCanvasDC::EndPage(void){ }

//-----------------------------------------------------------------------------
void wxCanvasDC::SetMapMode(int mode)
{
  mapping_mode = mode;

  int pixel_width = 0;
  int pixel_height = 0;
  int mm_width = 0;
  int mm_height = 0;

  // First, calculate how to scale from mm to pixels.
  // Then we just need to find the scaling factor from ? to mm and multiply
  // by the first scaling factor.

  if (wxSubType(__type, wxTYPE_DC_PRINTER)) {
    pixel_width = 800;
    pixel_height = 1200;
  } else {
    pixel_width = 640;
    pixel_height = 480;
  }
  mm_width = 225;
  mm_height = 169;

  float mm2pixelsX = pixel_width/mm_width;
  float mm2pixelsY = pixel_height/mm_height;

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

  ToolChanged(kNoTool);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetUserScale(float x, float y)
//-----------------------------------------------------------------------------
{
	user_scale_x = x;
	user_scale_y = y;

	ToolChanged(kNoTool);
}

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalX(int x) { return XDEV2LOG(x); }

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalXRel(int x) { return XDEV2LOGREL(x); }

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalY(int y) { return YDEV2LOG(y); }

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalYRel(int y) { return YDEV2LOGREL(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceX(float x) { return XLOG2DEV(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceXRel(float x) { return XLOG2DEVREL(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceY(float y) { return YLOG2DEV(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceYRel(float y) { return YLOG2DEVREL(y); }

//-----------------------------------------------------------------------------
float wxCanvasDC::FLogicalToDeviceX(float x) { return XLOG2DEV(x); }

//-----------------------------------------------------------------------------
float wxCanvasDC::FLogicalToDeviceXRel(float x) { return XLOG2DEVREL(x); }

//-----------------------------------------------------------------------------
float wxCanvasDC::FLogicalToDeviceY(float y) { return YLOG2DEV(y); }

//-----------------------------------------------------------------------------
float wxCanvasDC::FLogicalToDeviceYRel(float y) { return YLOG2DEVREL(y); }

//-----------------------------------------------------------------------------
void wxCanvasDC::wxMacSetClip(void)
{
	SetCurrentDC();
	if (canvas && !canvas->WantsFocus()) { // => canvas is hidden (HACK!)
		Rect zeroClipRect = {0, 0, 0, 0};
		::ClipRect(&zeroClipRect);
	} else {
		if (current_reg) {
                    RgnHandle rgn = ::NewRgn();
                    ::CopyRgn(current_reg,rgn);
                    ::OffsetRgn(rgn,SetOriginX,SetOriginY);
                    ::SetClip(current_reg);
                } else {
			Rect largestClipRect = {-32767, -32767, 32767, 32767};
			::ClipRect(&largestClipRect);
		}
	}
}

//-----------------------------------------------------------------------------

void wxCanvasDC::wxMacSetCurrentTool(wxMacToolType whichTool)
{
	if (!Ok()) return;
 
    SetCurrentDC();

	// mflatt: shortcut
	if (whichTool == cMacCurrentTool)
		return;

	switch (whichTool)
	{
		case kNoTool:
			InstallColor(current_background_color, FALSE);
            		PenMode(patCopy);
			break;
		case kBrushTool:
			{
                        Pattern color;
			int theBrushStyle = current_brush->GetStyle();
			int log = theBrushStyle == wxXOR ? patXor : patCopy;
			if ((theBrushStyle == wxSOLID) || (theBrushStyle == wxXOR))
				PenPat(GetQDGlobalsBlack(&color));
			else if (theBrushStyle == wxTRANSPARENT)
				PenPat(GetQDGlobalsWhite(&color)); // WCH : does this work??
			else if (IS_HATCH(theBrushStyle)) {
				macGetHatchPattern(theBrushStyle, &cMacPattern);
				PenPat(&cMacPattern);
				log = patOr;
			} else {
				PenPat(GetQDGlobalsBlack(&color));
			}
	
			InstallColor(current_background_color, FALSE);
			InstallColor(current_brush->GetColour(), TRUE);
			PenMode(log);
			}
			break;
		case kPenTool:
		        {
			int pensize = current_pen->GetWidth();
			int thePenWidth = (pensize ? pensize : 1);
			PenSize(thePenWidth, thePenWidth);

			int thePenStyle = current_pen->GetStyle();
			int log = patCopy;
			switch (thePenStyle) {
			  case wxXOR:
			    thePenStyle = wxSOLID;
			    log = patXor;
			    break;
			  case wxXOR_DOT:
			    thePenStyle = wxDOT;
			    log = patXor;
			    break;
			  case wxXOR_LONG_DASH:
			    thePenStyle = wxLONG_DASH;
			    log = patXor;
			    break;
			  case wxXOR_SHORT_DASH:
			    thePenStyle = wxSHORT_DASH;
			    log = patXor;
			    break;
			  case wxXOR_DOT_DASH:
			    thePenStyle = wxDOT_DASH;
			    log = patXor;
			    break;
			}
                        Pattern color;
			wxBitmap *bm = current_pen->GetStipple();
			if (bm && bm->Ok() && (bm->GetDepth() == 1)
			    && (bm->GetWidth() == 8) && (bm->GetHeight() == 8)) {
			  GDHandle savegd; CGrafPtr saveport;
			  char p[8]; int i, k;
                          GetGWorld(&saveport, &savegd);
                          SetGWorld(bm->x_pixmap, 0);
			  for (i = 0; i < 8; i++) {
			    p[i] = 0;
			    for (k = 0; k < 8; k++) {
			      RGBColor cpix;
			      ::GetCPixel(k, i, &cpix);
			      p[i] = p[i] << 1;
			      if (!cpix.red) {
			        p[i] |= 1;
			      }
			    }
			  }
			  SetGWorld(saveport, savegd);
			  PenPat((Pattern *)p);
			} else if (thePenStyle == wxSOLID)
				PenPat(GetQDGlobalsBlack(&color));
			else if (thePenStyle == wxTRANSPARENT)
				PenPat(GetQDGlobalsWhite(&color));
			else if ((thePenStyle == wxDOT)
			         || (thePenStyle == wxSHORT_DASH)) {
				PenPat(GetQDGlobalsLightGray(&color));
				if (log == patCopy) log = patOr;
			} else if ((thePenStyle == wxLONG_DASH)
			         || (thePenStyle == wxDOT_DASH)) {
				PenPat(GetQDGlobalsDarkGray(&color));
				if (log == patCopy) log = patOr;
			} else if (IS_HATCH(thePenStyle)) {
				macGetHatchPattern(thePenStyle, &cMacPattern);
				PenPat(&cMacPattern);
			} else {
				PenPat(GetQDGlobalsBlack(&color));
			}

			InstallColor(current_pen->GetColour(), TRUE);
			InstallColor(current_background_color, FALSE);
			PenMode(log);
			}
			break;
		case kTextTool:
			InstallColor(current_text_foreground, TRUE);
			if (current_bk_mode != wxTRANSPARENT)
			  InstallColor(current_text_background, FALSE);
			else
			   BackColor(whiteColor);
			::TextFont(font->GetMacFontNum());
	        ::TextSize(font->GetPointSize());
	        ::TextFace(font->GetMacFontStyle());
	        ::TextMode((current_bk_mode == wxTRANSPARENT) ? srcOr : srcCopy);
			InstallLogicalFunction(wxCOPY);
			break;
		case kBlitTool:
			ForeColor(blackColor);
			BackColor(whiteColor);
			InstallLogicalFunction(wxCOPY);
			break;
		case kColorBlitTool:
			InstallColor(current_background_color, FALSE);
			InstallColor(current_pen->GetColour(), TRUE);
			InstallLogicalFunction(wxCOPY);
			break;
		case kQuillTool:
			break;
	}

	cMacCurrentTool = whichTool;
}
