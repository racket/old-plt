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
#include <QuickDraw.h>
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

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(void)
{
  __type = wxTYPE_DC_CANVAS;

	selected_pixmap = NULL;
	canvas = NULL;
	cMacDoingDrawing = FALSE;

  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  clipping = FALSE;

  current_reg = NULL ;
  onpaint_reg = NULL ;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;

  min_x = 0; min_y = 0; max_x = 0; max_y = 0;

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

#ifdef wx_motif
  gc = NULL;
  gcBacking = NULL;
#endif

  ok = TRUE;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = *wxWHITE;
  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
}


//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(wxCanvas* the_canvas): wxbCanvasDC(the_canvas)
{
__type = wxTYPE_DC_CANVAS;

	GrafPtr oldPort;
	::GetPort(&oldPort);
	canvas = the_canvas;
	WXGC_IGNORE(canvas);
	cMacDC = canvas->MacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	::SetPort((GrafPtr)theMacGrafPort);

	cMacDoingDrawing = FALSE;
	pixmap = theMacGrafPort->portPixMap;	//CJC 

  clipping = FALSE;
  selected_pixmap = NULL;

  pixmapWidth = 0;
  pixmapHeight = 0;

  current_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0; max_x = 0; max_y = 0;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;
#ifndef LkB
  logical_origin_x = 0;
  logical_origin_y = 0;
#else
  // TO DO: temp fix for logical positioning
  int x,y;
  canvas->GetPosition(&x, &y);
  logical_origin_x = -x;
  logical_origin_y = -y;
#endif
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
  current_background_color = *wxWHITE;
  current_text_foreground = *wxBLACK;
  current_text_background = *wxWHITE;
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  ::SetPort(oldPort);

// More initialization
	if (the_canvas)
	{
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

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCurrentDC(void) // mac platform only
//-----------------------------------------------------------------------------
{
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	if ((GrafPtr)theMacGrafPort != qd.thePort) 
		::SetPort((GrafPtr)theMacGrafPort);

	if (cMacDC->currentUser() != this)
	{ // must setup platform
		cMacDC->setCurrentUser(this);
		int theRootX = 0, theRootY = 0;
		if (canvas)
			canvas->ClientArea()->FrameContentAreaOffset(&theRootX, &theRootY);
		::SetOrigin(-theRootX, -theRootY);
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
		::SectRgn(onpaint_reg, clipping->rgn, current_reg) ;
	else if (clipping)
		::CopyRgn(clipping->rgn, current_reg) ;
	else if (onpaint_reg)
		::CopyRgn(onpaint_reg, current_reg) ;

	wxObject* theCurrentUser = cMacDC->currentUser();
	if (theCurrentUser == this)
	{ // must update platfrom
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		wxMacSetClip();

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetClippingBox(float *x,float *y,float *w,float *h)
//-----------------------------------------------------------------------------
{
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();

	if (current_reg)
	{
		Rect theClipRect = (**(theMacGrafPort->clipRgn)).rgnBBox;
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
	float cx, cy, cw, ch;
	cx = paintRect->left;
	cy = paintRect->top;
	cw =  paintRect->right;
	ch =  paintRect->bottom;
	int left = XLOG2DEV(cx);
	int top = YLOG2DEV(cy);
	int right = XLOG2DEVREL(cx + cw);
	int bottom = YLOG2DEVREL(cy + ch);
	::SetRectRgn(onpaint_reg, left, top, right, bottom);
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
void wxCanvasDC::InstallColor(wxColour &c, int fg)
{
   RGBColor pixel;
   pixel = c.pixel;
   if (Colour) {
	  if (fg)
	    RGBForeColor(&pixel);
	  else
	    RGBBackColor(&pixel);
   } else {
		unsigned char red = c.Red();
		unsigned char blue = c.Blue();
		unsigned char green = c.Green();
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
	current_background_color = *color;

	if (cMacDC->currentUser() == this
		&& (cMacCurrentTool == kNoTool))
	{ // must update platform to kNoTool
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

        InstallColor(*color, FALSE);

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
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
void wxCanvasDC::wxMacSetClip(void)
{
	SetCurrentDC();
	if (canvas && !canvas->WantsFocus()) { // => canvas is hidden (HACK!)
		Rect zeroClipRect = {0, 0, 0, 0};
		::ClipRect(&zeroClipRect);
	} else {
		if (current_reg)
			::SetClip(current_reg);
		else {
			Rect largestClipRect = {-32767, -32767, 32767, 32767};
			::ClipRect(&largestClipRect);
		}
	}
}

//-----------------------------------------------------------------------------

void wxCanvasDC::wxMacSetCurrentTool(wxMacToolType whichTool)
{
	SetCurrentDC();

	// mflatt: shortcut
	if (whichTool == cMacCurrentTool)
		return;

	RGBColor pixel;

	switch (whichTool)
	{
		case kNoTool:
			InstallColor(current_background_color, FALSE);
            		PenMode(patCopy);
			break;
		case kBrushTool:
			{
			int theBrushStyle = current_brush->GetStyle();
			int log = theBrushStyle == wxXOR ? patXor : patCopy;
			if (theBrushStyle == wxSOLID)
				PenPat(&qd.black);
			else if (theBrushStyle == wxTRANSPARENT)
				PenPat(&qd.white); // WCH : does this work??
			else if (IS_HATCH(theBrushStyle)) {
				macGetHatchPattern(theBrushStyle, &cMacPattern);
				PenPat(&cMacPattern);
				log = patOr;
			} else {
				PenPat(&qd.black);
			}
	
			InstallColor(current_background_color, FALSE);
			InstallColor(current_brush->GetColour(), TRUE);
			PenMode(log);
			}
			break;
		case kPenTool:
		        {
			int thePenWidth = current_pen->GetWidth();
			int thePenHeight = current_pen->GetWidth();
			PenSize(thePenWidth, thePenHeight);

			int thePenStyle = current_pen->GetStyle();
			int log = thePenStyle == wxXOR ? patXor : patCopy;
			if (thePenStyle == wxSOLID)
				PenPat(&qd.black);
			else if (thePenStyle == wxTRANSPARENT)
				PenPat(&qd.white);
			else if ((thePenStyle == wxDOT)
			         || (thePenStyle == wxSHORT_DASH)) {
				PenPat(&qd.ltGray);
				log = patOr;
			} else if ((thePenStyle == wxLONG_DASH)
			         || (thePenStyle == wxDOT_DASH)) {
				PenPat(&qd.dkGray);
				log = patOr;
			} else if (IS_HATCH(thePenStyle)) {
				macGetHatchPattern(thePenStyle, &cMacPattern);
				PenPat(&cMacPattern);
			} else {
				PenPat(&qd.black);
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
