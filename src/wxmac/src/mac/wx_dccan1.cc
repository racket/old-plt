///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan1.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 1)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include <math.h>
#include "wx_dccan.h"
#include "wx_canvs.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"
#include "wx_rgn.h"
#ifdef OS_X
# include <AGL/agl.h> 
#endif

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

  clipping = NULL;
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
    Rect paintRect;
    the_canvas->GetClientSize(&clientWidth, &clientHeight);
    ::SetRect(&paintRect, 0, 0, clientWidth, clientHeight);
    SetPaintRegion(&paintRect);
  }
}

//-----------------------------------------------------------------------------
wxCanvasDC::~wxCanvasDC(void)
{
  if (gl) {
    gl->Reset(NULL, 0, 0, 0);
    gl = NULL;
  }

  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  if (clipping) --clipping->locked;
  
  if (current_reg) {
    ::DisposeRgn(current_reg);
    current_reg = NULL;
  }
  if (onpaint_reg) {
    ::DisposeRgn(onpaint_reg);
    onpaint_reg = NULL;
  }
  canvas = NULL;
}

void wxCanvasDC::BeginDrawing(void)
{
}

void wxCanvasDC::EndDrawing(void)
{
}


static GDHandle def_dev_handle = 0;
static CGrafPtr def_grafptr = NULL;

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCurrentDC(void) // mac platform only
  //-----------------------------------------------------------------------------
{
  CGrafPtr theMacGrafPort;

  if (!Ok() || !cMacDC) return;
  
  dc_set_depth++;

  if ((dc_set_depth != 1)
      || def_grafptr)
    printf("Nested SetDCs\n");

  theMacGrafPort = cMacDC->macGrafPort();

  if (!canvas)
    GetGWorld(&def_grafptr, &def_dev_handle);

  if (IsPortOffscreen(theMacGrafPort)) {
    ::SetGWorld(theMacGrafPort, NULL);
  } else {
    ::SetPort(theMacGrafPort);
  }
  
  SetOriginX = SetOriginY = 0;
  if (canvas) {
    wxArea *area;
    area = canvas->ClientArea();
    area->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
  }

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
    if (canvas) {
      if (cMacCurrentTool != kNoTool) {
	/* We have to go back to the canvas's drawings settings --- at
	   least the bg color --- otherwise controls might get drawn
	   wrong. That's because the DC GrafPtr is the same as the Window GrafPtr. */
	cMacCurrentTool = kNoTool;
	canvas->MacSetBackground();
      }
    } else {
      ::SetGWorld(def_grafptr, def_dev_handle);
      def_grafptr = NULL;
    }
  }
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCanvasClipping(void)
  //-----------------------------------------------------------------------------
{
  wxObject* theCurrentUser;

  if (!Ok() || !cMacDC)
    return;

  if (current_reg) {
    ::DisposeRgn(current_reg);
  }
  if (clipping || onpaint_reg) {
    current_reg = ::NewRgn();
    CheckMemOK(current_reg);
  } else
    current_reg = NULL;
  
  if (clipping && !clipping->rgn) {
    /* NULL rgn pointer means the empty region */
    if (!current_reg) {
      current_reg = ::NewRgn();
      CheckMemOK(current_reg);
    }
  } else if (onpaint_reg && clipping) {
    ::SectRgn(clipping->rgn, onpaint_reg, current_reg) ;
  } else if (clipping) {
    ::CopyRgn(clipping->rgn, current_reg) ;
  } else if (onpaint_reg) {
    ::CopyRgn(onpaint_reg, current_reg);
  }

  theCurrentUser = cMacDC->currentUser();
  if (theCurrentUser == this) { 
    // must update
    CGrafPtr savep;
    GDHandle savegd;
    CGrafPtr theMacGrafPort;
    long oox, ooy;

    theMacGrafPort = cMacDC->macGrafPort();
    
    ::GetGWorld(&savep, &savegd);  
    if (IsPortOffscreen(theMacGrafPort)) {
      ::SetGWorld(theMacGrafPort, NULL);
    } else {
      ::SetPort(theMacGrafPort);
    }
    
    oox = SetOriginX;
    ooy = SetOriginY;
    SetOriginX = SetOriginY = 0;
    if (canvas) {
      wxArea *area;
      area = canvas->ClientArea();
      area->FrameContentAreaOffset(&SetOriginX, &SetOriginY);
    }

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
    CGrafPtr theMacGrafPort;
    RgnHandle clipRgn;
    Rect theClipRect;
    int theX, theY, theWidth, theHeight;

    theMacGrafPort = cMacDC->macGrafPort();
    clipRgn = NewRgn();
      
    GetPortClipRegion(theMacGrafPort,clipRgn);
    GetRegionBounds(clipRgn,&theClipRect);
    
    DisposeRgn(clipRgn);

    theX = theClipRect.left;
    theY = theClipRect.top;
    theWidth = theClipRect.right - theClipRect.left;
    theHeight = theClipRect.bottom - theClipRect.top;
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
  if (onpaint_reg) { ::DisposeRgn(onpaint_reg); }
  onpaint_reg = ::NewRgn();
  CheckMemOK(onpaint_reg);
  ::RectRgn(onpaint_reg, paintRect);
  SetCanvasClipping();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRect(float cx, float cy, float cw, float ch)
  //-----------------------------------------------------------------------------
{
  wxRegion *r;
  r = new wxRegion(this);
  r->SetRectangle(cx, cy, cw, ch);
  SetClippingRegion(r);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRegion(wxRegion *r)
  //-----------------------------------------------------------------------------
{
  if (clipping)
    --clipping->locked;

  clipping = r;

  if (clipping)
    clipping->locked++;

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
  if (Colour) {
    RGBColor pixel;
    pixel = c->pixel;
    if (fg)
      RGBForeColor(&pixel);
    else
      RGBBackColor(&pixel);
  } else {
    unsigned char red, blue, green;
    red = c->Red();
    blue = c->Blue();
    green = c->Green();
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

  if (canvas && canvas->IsHidden()) {
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
      } else {
	::SetClip(current_reg);
      }
    } else {
      Rect largestClipRect = {-32767, -32767, 32767, 32767};
      ::ClipRect(&largestClipRect);
    }
  }
}

static void HiliteMode()
{
  RGBColor col;
  LMGetHiliteRGB(&col);
  col.red = 0xFFFF - col.red;
  col.green = 0xFFFF - col.green;
  col.blue = 0xFFFF - col.blue;
  PenMode(subPin);
  RGBForeColor(&col);
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
	if ((theBrushStyle == wxSOLID) || (theBrushStyle == wxXOR) || (theBrushStyle == wxCOLOR))
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
	if (Colour && (theBrushStyle == wxCOLOR))
	  HiliteMode();
	else {
	  InstallColor(current_brush->GetColour(), TRUE);
	  PenMode(log);
	}
	paint_brush_with_erase = 0;
      }
    }
    break;
  case kPenTool:
    {
      int pensize;
      int thePenWidth;
      int thePenStyle, origPenStyle;
      int log;
      wxBitmap *bm;

      pensize = current_pen->GetWidth();
      thePenWidth = (pensize ? pensize : 1);
      if (pensize) {
	int sx, sy;
	sx = XLOG2DEVREL(thePenWidth);
	sy = YLOG2DEVREL(thePenWidth);
	PenSize(sx ? sx : 1, sy ? sy : 1);
      } else
	PenSize(1, 1);
      
      thePenStyle = current_pen->GetStyle();
      origPenStyle = thePenStyle;
      log = patCopy;
      switch (thePenStyle) {
      case wxXOR:
      case wxCOLOR:
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
	if (origPenStyle == wxCOLOR)
	  origPenStyle = wxXOR;
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

      BackPat(GetWhitePattern());
      InstallColor(current_background_color, FALSE);
      if (Colour && (origPenStyle == wxCOLOR))
	HiliteMode();
      else {
	InstallColor(current_pen->GetColour(), TRUE);
	PenMode(log);
      }
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
    ::TextSize(font->GetPointSize());
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

Bool wxCanvasDC::GlyphAvailable(int c, wxFont *f)
{
  if (!f)
    f = font;

  return f->ScreenGlyphAvailable(c);
}

/************************************************************************/
/*                                GL                                    */
/************************************************************************/

wxGL *wxCanvasDC::GetGL()
{
  if (!gl) {
    CGrafPtr cp;
    gl = new wxGL();
    cp = cMacDC->macGrafPort();
    gl->Reset(cp, 0, 0, 0);
    canvas->ResetGLView();
  }

  return gl;
}

static wxGL *current_gl_context = NULL;
#ifdef OS_X
static AGLPixelFormat fmt;
static AGLPixelFormat sb_fmt;
static AGLContext dummy;
#endif

void wxInitGL()
{
#ifdef OS_X
  GC_CAN_IGNORE GLint attrib[] = { AGL_RGBA, AGL_DOUBLEBUFFER, AGL_DEPTH_SIZE, 1, AGL_NONE };
  GC_CAN_IGNORE GLint sb_attrib[] = { AGL_RGBA, AGL_OFFSCREEN, AGL_PIXEL_SIZE, 32, AGL_DEPTH_SIZE, 1, AGL_NONE };
  
  fmt = aglChoosePixelFormat(NULL, 0, attrib);
  sb_fmt = aglChoosePixelFormat(NULL, 0, sb_attrib);

  if (fmt) {
    dummy = aglCreateContext(fmt, NULL);
    aglSetCurrentContext(dummy);
  }
#endif

  wxREGGLOB(current_gl_context); 
}

wxGL::wxGL()
  : wxObject(WXGC_NO_CLEANUP)
{
}

void wxGL::Reset(CGrafPtr gp, int offscreen, int w, int h)
{
#ifdef OS_X
  AGLContext ctx; 

  ctx = (AGLContext)gl_ctx;
  if (gl_ctx) {
    if (this == current_gl_context) {
      aglSetCurrentContext(dummy);
    }
    
    aglSetDrawable(ctx, NULL);
    aglDestroyContext(ctx); 

    gl_ctx = NULL;
  }

  if (gp) {
    if (offscreen) {
      if (sb_fmt) {
	/* Note: gp has been locked by LockPixels already */
	PixMapHandle pm;
	pm = GetGWorldPixMap(gp);
	ctx = aglCreateContext(sb_fmt, NULL);	
	if (ctx)
	  aglSetOffScreen(ctx, w, h, GetPixRowBytes(pm), GetPixBaseAddr(pm));
      }
    } else {
      if (fmt) {
	ctx = aglCreateContext(fmt, NULL);	
	if (ctx)
	  aglSetDrawable(ctx, gp);
      }
    }

    gl_ctx = (long)ctx;
    if (ctx && (current_gl_context == this)) {
      aglSetCurrentContext(ctx);
    }
  }
#endif
}

int wxGL::Ok()
{
  return !!gl_ctx;
}

void wxGL::SwapBuffers(void)
{
#ifdef OS_X
  if (gl_ctx) {
    aglSwapBuffers((AGLContext)gl_ctx);
  }
#endif
}

void wxGL::ThisContextCurrent(void)
{
#ifdef OS_X
  if (current_gl_context != this) {
    current_gl_context = this;
    if (gl_ctx) {
      aglSetCurrentContext((AGLContext)gl_ctx);
    }
  }
#endif
}

void wxGL::ResetGLView(int x, int y, int w, int h)
{
#ifdef OS_X
  GLint bufferRect[4];
  AGLContext ctx = (AGLContext)gl_ctx;
  
  if (ctx) {
    bufferRect[0] = x;
    bufferRect[1] = y;
    bufferRect[2] = w;
    bufferRect[3] = h;
  
    aglSetInteger(ctx, AGL_BUFFER_RECT, bufferRect);
    aglEnable(ctx, AGL_BUFFER_RECT);
    
    if (current_gl_context != this) {
      aglSetCurrentContext(ctx);
    }
    glViewport(0, 0, (GLsizei)w, (GLsizei)h);
    if (current_gl_context != this) {
      if (current_gl_context) {
	aglSetCurrentContext((AGLContext)current_gl_context->gl_ctx);
      } else {
	aglSetCurrentContext(dummy);
      }
    }
  }
#endif
}

void wxGLNoContext()
{
#ifdef OS_X
  current_gl_context = NULL;
  aglSetCurrentContext(dummy);
#endif
}
