///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan1.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 1)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif
#include "wx_dccan.h"
#include "wx_canvs.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"
#include "wx_rgn.h"

extern CGrafPtr wxMainColormap;

static int patterns_inited;
Pattern wx_white_pat, wx_black_pat, wx_light_gray_pat, wx_dark_gray_pat;

void wx_init_patterns(void)
{
  GetQDGlobalsWhite(&wx_white_pat);
  GetQDGlobalsBlack(&wx_black_pat);
  GetQDGlobalsLightGray(&wx_light_gray_pat);
  GetQDGlobalsDarkGray(&wx_dark_gray_pat);
  patterns_inited = 1;
}

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

void wxCanvasDC::BeginDrawing(void)
{
}

void wxCanvasDC::EndDrawing(void)
{
}


static GDHandle def_dev_handle = 0;
static CGrafPtr def_grafptr = NULL;

GDHandle wxGetGDHandle(void)
{
  if (!def_dev_handle) {
    GetGWorld(&def_grafptr, &def_dev_handle);
  }
  
  return def_dev_handle;
}

CGrafPtr wxGetGrafPtr(void)
{
  return def_grafptr;
}

extern "C" {
  GDHandle alist_GetGDHandle(void) {
    return wxGetGDHandle();
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCurrentDC(void) // mac platform only
  //-----------------------------------------------------------------------------
{
  if (!Ok() || !cMacDC) return;
  
  dc_set_depth++;

  CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
  if (IsPortOffscreen(theMacGrafPort)) {
    ::SetGWorld(theMacGrafPort, NULL);
  } else {
    ::SetGWorld(theMacGrafPort, wxGetGDHandle());
  }
  
  SetOriginX = SetOriginY = 0;
  if (canvas)
    canvas->ClientArea()->FrameContentAreaOffset(&SetOriginX, &SetOriginY);

  if (cMacDC->currentUser() != this) { 
    // must setup platform
    cMacDC->setCurrentUser(this);
    wxMacSetClip();
    ToolChanged(kNoTool);
  }
}

void wxCanvasDC::ReleaseCurrentDC(void)
{
  if (!--dc_set_depth) {
    if (canvas && (cMacCurrentTool != kNoTool)) {
      /* We have to go back to the canvas's drawings settings --- at
	 least the bg color --- otherwise controls might get drawn
	 wrong. That's because the DC GrafPtr is the same as the Window GrafPtr. */
      cMacCurrentTool = kNoTool;
      canvas->MacSetBackground();
    }
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCanvasClipping(void)
  //-----------------------------------------------------------------------------
{
  if (!Ok() || !cMacDC)
    return;

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

  wxObject* theCurrentUser = cMacDC->currentUser();
  if (theCurrentUser == this) { 
    // must update
    CGrafPtr savep;
    GDHandle savegd;
    CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
    long oox, ooy;
    
    ::GetGWorld(&savep, &savegd);  
    if (IsPortOffscreen(theMacGrafPort)) {
      ::SetGWorld(theMacGrafPort, NULL);
    } else {
      ::SetGWorld(theMacGrafPort, wxGetGDHandle());
    }
    
    oox = SetOriginX;
    ooy = SetOriginY;
    SetOriginX = SetOriginY = 0;
    if (canvas)
      canvas->ClientArea()->FrameContentAreaOffset(&SetOriginX, &SetOriginY);

    wxMacSetClip();

    SetOriginX = oox;
    SetOriginY = ooy;
    
    ::SetGWorld(savep, savegd);
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetClippingBox(float *x,float *y,float *w,float *h)
  //-----------------------------------------------------------------------------
{
  if (current_reg && cMacDC) {
    CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
    RgnHandle clipRgn = NewRgn();
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
  } else
    *x = *y = *w = *h = 0;
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
  /* not used */
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
  /* current GrafPtr is set when this function is called */

  if (canvas && !canvas->WantsFocus()) { // => canvas is hidden
    Rect zeroClipRect = {0, 0, 0, 0};
    ::ClipRect(&zeroClipRect);
  } else {
    if (current_reg) {
      if (SetOriginX || SetOriginY) {
	RgnHandle rgn;
	rgn = ::NewRgn();
	::CopyRgn(current_reg,rgn);
	::OffsetRgn(rgn,SetOriginX,SetOriginY);
	::SetClip(rgn);
	DisposeRgn(rgn);
      } else
	::SetClip(current_reg);
    } else {
      Rect largestClipRect = {-32767, -32767, 32767, 32767};
      ::ClipRect(&largestClipRect);
    }
  }
}

//-----------------------------------------------------------------------------

void wxCanvasDC::wxMacSetCurrentTool(wxMacToolType whichTool)
  /* assumes that SetCurrentDC() has been called, already */  
{
  if (!Ok() || !cMacDC) return;

  if (whichTool == cMacCurrentTool)
    return;

  switch (whichTool) {
  case kNoTool:
    break;
  case kBGTool:
    InstallColor(current_background_color, FALSE);
    BackPat(GetWhitePattern());
    PenMode(patCopy);
    break;
  case kBrushTool:
    {
      int theBrushStyle;
      theBrushStyle = current_brush->GetStyle();
      if (theBrushStyle == wxPANEL_PATTERN) {
	int depth;
	depth = wxDisplayDepth();
	SetThemeBackground(kThemeBrushDialogBackgroundActive, depth, depth > 1);
	paint_brush_with_erase = 1;
      } else {
	int log = (theBrushStyle == wxXOR ? patXor : patCopy);
	if ((theBrushStyle == wxSOLID) || (theBrushStyle == wxXOR))
	  PenPat(GetBlackPattern());
	else if (theBrushStyle == wxTRANSPARENT)
	  PenPat(GetWhitePattern());
	else if (IS_HATCH(theBrushStyle)) {
	  macGetHatchPattern(theBrushStyle, &cMacPattern);
	  PenPat(&cMacPattern);
	  log = patOr;
	} else {
	  PenPat(GetBlackPattern());
	}
	
	InstallColor(current_background_color, FALSE);
	BackPat(GetWhitePattern());
	InstallColor(current_brush->GetColour(), TRUE);
	PenMode(log);
	paint_brush_with_erase = 0;
      }
    }
    break;
  case kPenTool:
    {
      int pensize;
      int thePenWidth;
      int thePenStyle;
      int log;
      wxBitmap *bm;

      pensize = current_pen->GetWidth();
      thePenWidth = (pensize ? pensize : 1);
      PenSize(thePenWidth, thePenWidth);
      
      thePenStyle = current_pen->GetStyle();
      log = patCopy;
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
      bm = current_pen->GetStipple();
      if (bm && bm->Ok() && (bm->GetDepth() == 1)
	  && (bm->GetWidth() == 8) && (bm->GetHeight() == 8)) {
	GDHandle savegd;
	CGrafPtr saveport;
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
	PenPat(GetBlackPattern());
      else if (thePenStyle == wxTRANSPARENT)
	PenPat(GetWhitePattern());
      else if ((thePenStyle == wxDOT)
	       || (thePenStyle == wxSHORT_DASH)) {
	PenPat(GetLightGrayPattern());
	if (log == patCopy) log = patOr;
      } else if ((thePenStyle == wxLONG_DASH)
		 || (thePenStyle == wxDOT_DASH)) {
	PenPat(GetDarkGrayPattern());
	if (log == patCopy) log = patOr;
      } else if (IS_HATCH(thePenStyle)) {
	macGetHatchPattern(thePenStyle, &cMacPattern);
	PenPat(&cMacPattern);
      } else {
	PenPat(GetBlackPattern());
      }

      InstallColor(current_pen->GetColour(), TRUE);
      BackPat(GetWhitePattern());
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
    BackPat(GetWhitePattern());
    ::TextFont(font->GetMacFontNum());
    ::TextSize(floor(font->GetPointSize() * user_scale_y));
    ::TextFace(font->GetMacFontStyle());
    ::TextMode((current_bk_mode == wxTRANSPARENT) ? srcOr : srcCopy);
    InstallLogicalFunction(wxCOPY);
    break;
  case kBlitTool:
    ForeColor(blackColor);
    BackColor(whiteColor);
    BackPat(GetWhitePattern());
    InstallLogicalFunction(wxCOPY);
    break;
  case kColorBlitTool:
    InstallColor(current_background_color, FALSE);
    BackPat(GetWhitePattern());
    InstallColor(current_pen->GetColour(), TRUE);
    InstallLogicalFunction(wxCOPY);
    break;
  case kQuillTool:
    break;
  }

  cMacCurrentTool = whichTool;
}
