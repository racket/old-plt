///////////////////////////////////////////////////////////////////////////////
// File:	wx_check.cc
// Purpose:	Panel item checkbox implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_check.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif

#define IC_BOX_SIZE 12
#define IC_X_SPACE 3
#define IC_Y_SPACE 2
#define IC_MIN_HEIGHT (IC_BOX_SIZE + 2 * IC_Y_SPACE)

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxCheckBox::wxCheckBox // Constructor (given parentPanel, label)
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
 wxbCheckBox (parentPanel, function, x, y, width, height, style, windowName)
{
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxCheckBox::Create // Constructor (given parentPanel, label)
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
  CGrafPtr theMacGrafPort;
  Rect boundsRect = {0, 0, 0, 0};
  Rect r = {0,0,0,0};
  CFStringRef title;
  SInt16 baselineOffset; // ignored
  OSErr err;

  buttonBitmap = NULL;
  
  Callback(function);

  font = buttonFont; // WCH: mac platform only
  
  label = wxItemStripLabel(label);

  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
  OffsetRect(&boundsRect,SetOriginX + padLeft,SetOriginY + padTop);

  title = CFStringCreateWithCString(NULL,label,kCFStringEncodingISOLatin1);
  cMacControl = NULL;
  CreateCheckBoxControl(GetWindowFromPort(theMacGrafPort), &boundsRect, 
			title, 0, FALSE, &cMacControl);
  CFRelease(title);
  CheckMemOK(cMacControl);
  
  err = ::GetBestControlRect(cMacControl,&r,&baselineOffset);
  
  cWindowWidth = r.right - r.left + (padLeft + padRight);
  cWindowHeight = r.bottom - r.top + (padTop + padBottom);
  
  ::SizeControl(cMacControl,r.right-r.left,r.bottom-r.top);

  ::EmbedControl(cMacControl, GetRootControl());
  
  {
    wxWindow*p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();
}

//-----------------------------------------------------------------------------
wxCheckBox::wxCheckBox // Constructor (given parentPanel, bitmap)
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
 wxbCheckBox (parentPanel, function, x, y, width, height, style, windowName)
{
  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
  } else {
    Create(parentPanel, function, "<bad-bitmap>", x, y, width, height, style, windowName, objectType);
    return;
  }
  
  Callback(function);
  
  SetCurrentMacDC();
  {
    Rect bounds;
    ::SetRect(&bounds, 0, 0, buttonBitmap->GetWidth(), buttonBitmap->GetHeight());
    cWindowHeight = bounds.bottom;
    cWindowWidth = bounds.right + IC_BOX_SIZE + IC_X_SPACE;
    if (cWindowHeight < IC_MIN_HEIGHT)
      cWindowHeight = IC_MIN_HEIGHT;
    OffsetRect(&bounds,SetOriginX,SetOriginY);

    {
      wxWindow*p;
      p = GetParent();
      if (p->IsHidden())
	DoShow(FALSE);
      else if (!(style & wxINVISIBLE)) {
	::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&bounds);
      }
    }
    if (style & wxINVISIBLE)
      Show(FALSE);
  }

  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxCheckBox::~wxCheckBox(void)
{
  if (cMacControl) { ::DisposeControl(cMacControl); }
  if (buttonBitmap) --buttonBitmap->selectedIntoDC;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Item methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
char* wxCheckBox::GetLabel(void)
{
  if (cMacControl) {
    Str255 pLabel;
    ::GetControlTitle(cMacControl, pLabel);
    CopyPascalStringToC(pLabel, wxBuffer);
    return wxBuffer;
  } else if (buttonBitmap)
    return NULL;
  else
    return labelString;
}

//-----------------------------------------------------------------------------
void wxCheckBox::SetLabel(char* label)
{
  if (buttonBitmap)
    return;
  
  if (label) {
    char *s;
    s = copystring(wxItemStripLabel(label));
    labelString = s;
  } else
    labelString = NULL;

  if (label && cMacControl) {
    CFStringRef llabel;
    llabel = CFStringCreateWithCString(NULL, labelString, kCFStringEncodingISOLatin1);
    SetControlTitleWithCFString(cMacControl, llabel);
    CFRelease(llabel);
    RefreshIfUpdating();
  } else
    Refresh();
}

//-----------------------------------------------------------------------------
void wxCheckBox::SetLabel(wxBitmap* bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;
  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;
  Refresh();
}

//-----------------------------------------------------------------------------
void wxCheckBox::SetValue(Bool value)
{
  if (cMacControl) {
    SetCurrentDC();
    ::SetControlValue(cMacControl, value ? 1 : 0);
    RefreshIfUpdating();
  } else {
    bitmapState = !!value;
    if (!cHidden) {
      Paint();
      /* in case paint didn't take, because an update is already in progress: */
      RefreshIfUpdating();
    }
  }
}

//-----------------------------------------------------------------------------
Bool wxCheckBox::GetValue(void)
{
  if (cMacControl) {
    short value;
    value = ::GetControlValue(cMacControl);
    return (value != 0) ? TRUE : FALSE;
  } else
    return bitmapState;
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxCheckBox::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (!cMacControl)
    return;
  
  SetCurrentDC();

#ifndef WX_CARBON
  Bool hideToPreventFlicker = (IsControlVisible(cMacControl) && (dX || dY) && (dW || dH));
  if (hideToPreventFlicker) ::HideControl(cMacControl);
#endif

  if (dW || dH) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), clientHeight - (padTop + padBottom));
  }

  if (dX || dY) {
    MaybeMoveControls();
  }

#ifndef WX_CARBON
  if (hideToPreventFlicker) ::ShowControl(cMacControl);
#endif
  if (!cHidden && (dW || dH || dX || dY)) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    {
      Rect clientRect = {0, 0, clientHeight, clientWidth};
      OffsetRect(&clientRect,SetOriginX,SetOriginY);
      ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&clientRect);
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxCheckBox::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
void wxCheckBox::Paint(void)
{
  if (cHidden) return;
  SetCurrentDC();
  {
    Rect r;
    ::SetRect(&r, 0, 0, cWindowWidth, cWindowHeight);
    ::OffsetRect(&r,SetOriginX,SetOriginY);
    if (cMacControl) {
      ::Draw1Control(cMacControl);
    } else {
      int top;
      Rect r;

      ::EraseRect(&r);
      if (buttonBitmap) {
	int btop, h;
	h = buttonBitmap->GetHeight();
	btop = (cWindowHeight - h) / 2;
	buttonBitmap->DrawMac(IC_BOX_SIZE + IC_X_SPACE, btop);
      } else if (labelString) {
	float fWidth = 50.0;
	float fHeight = 12.0;
	float fDescent = 0.0;
	float fLeading = 0.0;
	int stop;
	GetTextExtent(labelString, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
	stop = (int)((cWindowHeight + fHeight) / 2);
	::MoveTo(IC_BOX_SIZE + IC_X_SPACE + SetOriginX, (short)(stop - fDescent - fLeading) + SetOriginY);
	DrawLatin1Text(labelString, 0);
      }
      top = (cWindowHeight - IC_BOX_SIZE) / 2;
      ::SetRect(&r, 0, top, IC_BOX_SIZE, top + IC_BOX_SIZE);
      OffsetRect(&r,SetOriginX,SetOriginY);
      ForeColor(blackColor);
      PenSize(1, 1);
      FrameRect(&r);
      ForeColor(whiteColor);
      InsetRect(&r, 1, 1);
      PaintRect(&r);
      ForeColor(blackColor);
      if (bitmapState) {
	MoveTo(SetOriginX, top + SetOriginY);
	Line(IC_BOX_SIZE - 1, IC_BOX_SIZE - 1);
	MoveTo(SetOriginX, top + IC_BOX_SIZE - 1 + SetOriginY);
	Line(IC_BOX_SIZE - 1, -(IC_BOX_SIZE - 1));
      } else {
      }
      cMacDC->setCurrentUser(NULL);
    }
  }
}

void wxCheckBox::Highlight(Bool on)
{
  int top = (cWindowHeight - IC_BOX_SIZE) / 2;
  Rect r = { top + 1, 1, top + IC_BOX_SIZE - 1, IC_BOX_SIZE - 1};
  OffsetRect(&r,SetOriginX,SetOriginY);
  PenSize(1, 1);
  if (on)
    FrameRect(&r);
  else {
    ForeColor(whiteColor);
    FrameRect(&r);
    ForeColor(blackColor);
    if (bitmapState) {
      MoveTo(SetOriginX, top + SetOriginY);
      Line(IC_BOX_SIZE - 1, IC_BOX_SIZE - 1);
      MoveTo(SetOriginX, top + IC_BOX_SIZE - 1 + SetOriginY);
      Line(IC_BOX_SIZE - 1, -(IC_BOX_SIZE - 1));
    }
  }	
}

//-----------------------------------------------------------------------------
void wxCheckBox::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (cMacControl) {
    SetCurrentDC();
    if (show) {
      ::ShowControl(cMacControl);
    } else {
      ::HideControl(cMacControl);
    }
    RefreshIfUpdating();
  }
  
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxCheckBox::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (event->LeftDown())
    {
      int startH;
      int startV;
      Point startPt;
      int trackResult;

      this->Enable(true);
      wxWindow::Enable(true);
      
      SetCurrentDC();
      
      event->Position(&startH, &startV); // client c.s.
      
      startPt.v = startV + SetOriginY;
      startPt.h = startH + SetOriginX; // port c.s.
      if (::StillDown()) {
	wxTracking();
	if (cMacControl)
	  trackResult = ::TrackControl(cMacControl, startPt, NULL);
	else
	  trackResult = Track(startPt);
      } else
	trackResult = 1;
      if (trackResult)
	{
	  wxCommandEvent *commandEvent;
	  commandEvent = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);
	  SetValue(!GetValue()); // toggle checkbox
	  ProcessCommand(commandEvent);
	}
    }
}
