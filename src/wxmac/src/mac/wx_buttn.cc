///////////////////////////////////////////////////////////////////////////////
// File:	wx_buttn.cc
// Purpose:	Panel item button implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_buttn.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wb_gdi.h"
#ifndef WX_CARBON
#include <Windows.h>
#endif
#include "wxButtonBorder.h"

#define MIN_BUTTON_WIDTH 58
// Under OS X, an inset is necessary because the OS draws outside of the control rectangle.
#define PAD_X 5
#define PAD_Y 5

#define IB_MARGIN_X 3
#define IB_MARGIN_Y 3

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::wxButton // Constructor (given parentPanel, label)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 char*		label,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbButton (parentPanel, function, x, y, width, height, style, windowName)
     
{
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxButton::Create // Real constructor (given parentPanel, label)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 char*		label,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) 	
{
  OSErr err;
  Rect boundsRect = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  CFStringRef title;
  CGrafPtr theMacGrafPort;

  buttonBitmap = NULL;
  cColorTable = NULL;

  padLeft = padRight = PAD_X;
  padTop = padBottom = PAD_Y;
  
  Callback(function);

  font = buttonFont; // WCH: mac platform only
  
  label = wxItemStripLabel(label);

  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();

  // First, create the control with a bogus rectangle;
  ::OffsetRect(&boundsRect,SetOriginX,SetOriginY);
  title = CFStringCreateWithCString(NULL,label,kCFStringEncodingISOLatin1);
  ::CreatePushButtonControl(GetWindowFromPort(theMacGrafPort), &boundsRect, title, &cMacControl);
  CFRelease(title);

  // Now, ignore the font data and let the control find the "best" size 
  ::SetRect(&boundsRect,0,0,0,0);
  err = ::GetBestControlRect(cMacControl,&boundsRect,&baselineOffset);
  cWindowWidth = boundsRect.right - boundsRect.left + (padLeft + padRight);
  cWindowHeight = boundsRect.bottom - boundsRect.top + (padTop + padBottom);
  ::SizeControl(cMacControl,boundsRect.right - boundsRect.left, boundsRect.bottom - boundsRect.top);
  
  ::EmbedControl(cMacControl, GetRootControl());

  if (style & 1) OnSetDefault(TRUE);

  if (GetParent()->IsHidden())
    DoShow(FALSE);
  InitInternalGray();
}

//-----------------------------------------------------------------------------
wxButton::wxButton // Constructor (given parentPanel, bitmap)
(
 wxPanel*	parentPanel,
 wxFunction	function,
 wxBitmap*	bitmap,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbButton (parentPanel, function, x, y, width, height, style, windowName)
{
  CGrafPtr theMacGrafPort;
  Rect bounds;

 cColorTable = NULL;

  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
  } else {
    Create(parentPanel, function, "<bad-image>", x, y, width, height, style, windowName, objectType);
    return;
  }

  Callback(function);
  
  SetEraser(wxWHITE_BRUSH);

  cBorderArea = new wxArea(this);
  new wxButtonBorder(cBorderArea);

  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
#if 0
  // bevel buttons for bitmap buttons can wait until other things are done. ugh.
  Rect bounds;
  ::SetRect(&bounds,0,0,0,0)
    cMacControl = ::NewControl(GetWindowFromPort(theMacGrafPort),&bounds,"\p",TRUE,
			       kControlContentIconSuiteRes...);
#endif
  cMacControl = NULL;


  ::SetRect(&bounds, 0, 0, buttonBitmap->GetWidth(), buttonBitmap->GetHeight());
  bounds.bottom += 2 * IB_MARGIN_Y;
  bounds.right += 2 * IB_MARGIN_X;
  cWindowHeight = bounds.bottom;
  cWindowWidth = bounds.right;
  OffsetRect(&bounds,SetOriginX,SetOriginY);
  
  if (GetParent()->IsHidden())
    DoShow(FALSE);
  else
    ::InvalWindowRect(GetWindowFromPort(theMacGrafPort),&bounds);

  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::~wxButton(void)
{
  if (buttonBitmap == NULL) {
    if (cMacControl) ::DisposeControl(cMacControl);
    cMacControl = NULL;
  } else
    --buttonBitmap->selectedIntoDC;
}

//-----------------------------------------------------------------------------
void wxButton::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxButton::GetLabel(void)
{
  Str255	pTitle;
  if (buttonBitmap)
    return NULL;
  if (cMacControl)
    ::GetControlTitle(cMacControl, pTitle);
  CopyPascalStringToC(pTitle, wxBuffer);
  return wxBuffer;
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(char* label)
{
  if (buttonBitmap)
    return;
  if (label) {
    if (cMacControl) {
      SetCurrentDC();
      {
	CFStringRef llabel;
	llabel = CFStringCreateWithCString(NULL, wxItemStripLabel(label), kCFStringEncodingISOLatin1);
	SetControlTitleWithCFString(cMacControl, llabel);
	CFRelease(llabel);
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(wxBitmap* bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;
  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;
  Refresh();
}

//-----------------------------------------------------------------------------
void wxButton::SetDefault(Bool flag) // WCH : modification of original (see below too)
{ 
  wxPanel* panel;
  wxButton* currentDefault;

  panel = (wxPanel*) GetParent();
  currentDefault = panel->defaultItem;

  if (flag) {
    if (currentDefault != this) {
      if (currentDefault) currentDefault->OnSetDefault(FALSE);
      panel->defaultItem = this;
      OnSetDefault(TRUE);
    }
  } else {
    if (currentDefault == this) {
      currentDefault->OnSetDefault(FALSE);
      panel->defaultItem = NULL;
    }
  }
}

//-----------------------------------------------------------------------------
void wxButton::OnSetDefault(Bool flag) // WCH : addition to original
{
  if (cMacControl) {
    char byteFlag = (char)flag;
    SetCurrentDC();
    SetControlData(cMacControl,kControlEntireControl,kControlPushButtonDefaultTag,1,&byteFlag);
  } else {
    if (buttonBitmap)
      return;
    if (flag) {
      START_XFORM_SKIP;
      wxMargin margin(4);
      END_XFORM_SKIP;
      cBorderArea->SetMargin(margin, wxAll,
			     cWindowWidth + 8, cWindowHeight + 8,
			     cWindowX - 4, cWindowY - 4);
    } else {
      START_XFORM_SKIP;
      wxMargin margin(0);
      END_XFORM_SKIP;
      cBorderArea->SetMargin(margin, wxAll,
			     cWindowWidth - 8, cWindowHeight - 8,
			     cWindowX + 4, cWindowY + 4);
    }
  }
}

static wxColour *dark, *darker, *lite;

//-----------------------------------------------------------------------------
static void PaintBitmapButton(Rect *r, wxBitmap *buttonBitmap, Bool pressed, Bool isgray, 
                              int cColour)
{
  wxColour *back, *bright, *dim;
  Rect rr;

  if (!dark) {
    wxColour *norm;
    int nr, ng, nb;
    norm = wxCONTROL_BACKGROUND_BRUSH->GetColour();
    
#   define DARK_SCALE(x) (x - (x >> 2))
#   define DARKER_SCALE(x) (x >> 1)
#   define LITE_SCALE(x) 0xFF

    wxREGGLOB(dark);
    wxREGGLOB(darker);
    wxREGGLOB(lite);

    nr = norm->Red();
    ng = norm->Green();
    nb = norm->Blue();

    dark = new wxColour(DARK_SCALE(nr), DARK_SCALE(ng), DARK_SCALE(nb));
    darker = new wxColour(DARKER_SCALE(nr), DARKER_SCALE(ng), DARKER_SCALE(nb));
    lite = new wxColour(LITE_SCALE(nr), LITE_SCALE(ng), LITE_SCALE(nb));
  }

  if (pressed) {
    back = dark;
    dim = wxCONTROL_BACKGROUND_BRUSH->GetColour();
    bright = darker;
  } else {
    back = wxCONTROL_BACKGROUND_BRUSH->GetColour();
    dim = darker;
    bright = lite;
  }

  rr = *r;
  InsetRect(&rr, 1, 1);
  
  if (cColour)
    RGBBackColor(&back->pixel);
  ::EraseRect(&rr);

  if (isgray && cColour)
    RGBForeColor(&dark->pixel);
  else
    ForeColor(blackColor);
  FrameRoundRect(r, 2 * IB_MARGIN_X, 2 * IB_MARGIN_Y);
  
  if (cColour) {
    RGBForeColor(&bright->pixel);  
    MoveTo(rr.left + 1, rr.top);
    LineTo(rr.right - 2, rr.top);
    
    MoveTo(rr.left, rr.top + 1);
    LineTo(rr.left, rr.bottom - 2);

    RGBForeColor(&dim->pixel);  
    MoveTo(rr.left + 1, rr.bottom - 1);
    LineTo(rr.right - 2, rr.bottom - 1);
    
    MoveTo(rr.right - 1, rr.top + 1);
    LineTo(rr.right - 1, rr.bottom - 2);
    
    // Reset color for blit
    if (isgray && cColour)
      RGBForeColor(&dark->pixel);
    else
      ForeColor(blackColor);
  }
  buttonBitmap->DrawMac(IB_MARGIN_X, IB_MARGIN_Y);
}

void wxButton::Paint(void)
{
  if (cHidden) return;
  if (SetCurrentDC()) {
    if (buttonBitmap) {
      Rect r = { 0, 0, cWindowHeight, cWindowWidth };
      OffsetRect(&r,SetOriginX,SetOriginY);
      PaintBitmapButton(&r, buttonBitmap, 0, IsGray(), cColour);
    } else if (cMacControl) {
      ::Draw1Control(cMacControl);
    }
    wxWindow::Paint();
  }
}

//-----------------------------------------------------------------------------
void wxButton::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (!buttonBitmap && cMacControl) {
    SetCurrentDC();
    if (show)
      ::ShowControl(cMacControl);
    else
      ::HideControl(cMacControl);
  }
  
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxButton::Highlight(Bool flag) // mac platform only
{
  if (buttonBitmap) {
    if (SetCurrentDC()) {
      Rect bounds = {0, 0, cWindowHeight, cWindowWidth};
      PaintBitmapButton(&bounds, buttonBitmap, flag, FALSE, cColour);
    }
  } else if (cMacControl) {
    SetCurrentDC();
    if (cEnable) {
      ::HiliteControl(cMacControl, flag ? kControlButtonPart : 0);
    }
  }
}

//-----------------------------------------------------------------------------
void wxButton::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (event->LeftDown())
    {
      int startH, startV;
      Point startPt;
      int trackResult;

      SetCurrentDC();
      
      event->Position(&startH, &startV); // client c.s.
      
      startPt.v = startV + SetOriginY; // port c.s.
      startPt.h = startH + SetOriginX;

      if (::StillDown()) {
	if (buttonBitmap == NULL && cMacControl) {
	  trackResult = ::TrackControl(cMacControl, startPt, NULL);
	} else {
	  trackResult = Track(startPt);
	}
      } else {
	if (cActive) {
	  long delayTicks = 4; // one tick is 1/60th of a second
	  unsigned long finalTicks;

	  Highlight(TRUE); // highlight button
	  Delay(delayTicks, &finalTicks);
	  Highlight(FALSE); // unhighlight button
	  
	  trackResult = 1;
	} else
	  trackResult = 0;
      }
      if (trackResult)
	{
	  wxCommandEvent *commandEvent;
	  commandEvent = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
	  ProcessCommand(commandEvent);
	}
    }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  Bool isVisible;
  Bool hideToPreventFlicker;

  if (buttonBitmap || !cMacControl)
    return;

  SetCurrentDC();

  isVisible = cMacControl && IsShown();
  hideToPreventFlicker = (isVisible && (dX || dY) && (dW || dH));
  if (hideToPreventFlicker) 
    ::HideControl(cMacControl);

  if (dW || dH)
    {
      int clientWidth, clientHeight;
      GetClientSize(&clientWidth, &clientHeight);
      ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		    clientHeight - (padTop + padBottom));
    }

  if (dX || dY)
    {
      MaybeMoveControls();
    }

  if (hideToPreventFlicker) ::ShowControl(cMacControl);

  if (!cHidden && (dW || dH || dX || dY))
    {
      int clientWidth, clientHeight;
      Rect clientRect;
      GetClientSize(&clientWidth, &clientHeight);
      ::SetRect(&clientRect, 0, 0, clientWidth, clientHeight);
      OffsetRect(&clientRect,SetOriginX,SetOriginY);
      ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&clientRect);
    }
}
