///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbut.cc
// Purpose:	Panel item radioButton implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_rbut.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif

#define IR_CIRCLE_SIZE 12
#define IR_X_SPACE 3
#define IR_Y_SPACE 2
#define IR_MIN_HEIGHT (IR_CIRCLE_SIZE + 2 * IR_Y_SPACE)
#define IR_ON_INSET 3

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioButton::wxRadioButton // Constructor (given parentPanel, label)
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
 wxItem (parentPanel, x, y, width, height, style, windowName)
{
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxRadioButton::Create // Real constructor (given parentPanel, label)
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
  buttonBitmap = NULL;
  Callback(function);
  
  font = buttonFont; // WCH: mac platform only

  // First, create the control with a bogus rectangle;
  SetCurrentMacDC();
  CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
  Rect boundsRect = {0, 0, 0, 0};
  OffsetRect(&boundsRect,SetOriginX,SetOriginY);
  CFStringRef theMacLabel = CFStringCreateWithCString(NULL, label, kCFStringEncodingISOLatin1);
  OSErr err;
  
  err = CreateRadioButtonControl(GetWindowFromPort(theMacGrafPort), &boundsRect, theMacLabel,
				 0, FALSE, &cMacControl);

  CFRelease(theMacLabel);
  
  // Now, ignore the font data and let the control find the "best" size 
  SInt16 baselineOffset; // ignored
  err = ::GetBestControlRect(cMacControl,&boundsRect,&baselineOffset);
  cWindowWidth = boundsRect.right - boundsRect.left;
  cWindowHeight = boundsRect.bottom - boundsRect.top;
  ::SizeControl(cMacControl, boundsRect.right - boundsRect.left, boundsRect.bottom - boundsRect.top);

  ::EmbedControl(cMacControl, GetRootControl());

  if (GetParent()->IsHidden())
    DoShow(FALSE);
  InitInternalGray();
}

//-----------------------------------------------------------------------------
wxRadioButton::wxRadioButton // Constructor (given parentPanel, bitmap)
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
 wxItem (parentPanel, x, y, width, height, style, windowName)
{
  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
  } else {
    Create(parentPanel, function, "<bad-image>", x, y, width, height, style, windowName, objectType);
  }

  Callback(function);
  cMacControl = NULL;
  
  Rect bounds = {0, 0, buttonBitmap->GetHeight(), buttonBitmap->GetWidth()};
  cWindowHeight = bounds.bottom;
  cWindowWidth = bounds.right + IR_CIRCLE_SIZE + IR_X_SPACE;
  if (cWindowHeight < IR_MIN_HEIGHT)
    cWindowHeight = IR_MIN_HEIGHT;
  OffsetRect(&bounds,SetOriginX,SetOriginY);
  
  if (SetCurrentMacDC())
    ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&bounds);

  if (GetParent()->IsHidden())
    DoShow(FALSE);
  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioButton::~wxRadioButton(void)
{
  if (cMacControl) ::DisposeControl(cMacControl);
  if (buttonBitmap)
    --buttonBitmap->selectedIntoDC;
}


//-----------------------------------------------------------------------------
void wxRadioButton::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxRadioButton::GetLabel()
{
  if (cMacControl) {
    Str255	pLabel;

    ::GetControlTitle(cMacControl, pLabel);
    ::CopyPascalStringToC(pLabel, wxBuffer);
    return copystring(wxBuffer);
  } else if (labelString)
    return labelString;
  else
    return NULL;
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetLabel(char* label)
{
  if (label && !buttonBitmap) {
    if (cMacControl) {
      SetCurrentDC();
      {
	CFStringRef llabel = CFStringCreateWithCString(NULL, label, kCFStringEncodingISOLatin1);
	SetControlTitleWithCFString(cMacControl, llabel);
	CFRelease(llabel);
      }
    } else
      labelString = label;
  }
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetLabel(wxBitmap* bitmap)
{
  if (buttonBitmap && bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    --buttonBitmap->selectedIntoDC;
    buttonBitmap = bitmap;
    buttonBitmap->selectedIntoDC++;
    Refresh();
  }
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetValue(Bool val)
{
  if (cMacControl) {
    SetCurrentDC();
    ::SetControlValue(cMacControl, val ? 1 : 0);
  } else {
    bitmapState = !!val;
    if (!cHidden)
      Paint();
  }
}

//-----------------------------------------------------------------------------
Bool wxRadioButton::GetValue(void)
{
  if (cMacControl) {
    short value = ::GetControlValue(cMacControl);
    return (value != 0) ? TRUE : FALSE;
  } else
    return bitmapState;
}

//-----------------------------------------------------------------------------
void wxRadioButton::Paint(void)
{
  if (cHidden) return;
  SetCurrentDC();
  Rect r = { 0, 0, cWindowHeight, cWindowWidth};
  OffsetRect(&r,SetOriginX,SetOriginY);
  ::EraseRect(&r);
  if (cMacControl) {
    ::Draw1Control(cMacControl);
  } else {
    if (buttonBitmap) {
      int btop = (cWindowHeight - buttonBitmap->GetHeight()) / 2;
      buttonBitmap->DrawMac(IR_CIRCLE_SIZE + IR_X_SPACE, btop);
    } else if (labelString) {
      Rect r = { SetOriginY, IR_CIRCLE_SIZE + IR_X_SPACE + SetOriginX, 
		 SetOriginY + cWindowHeight, SetOriginX + cWindowWidth };
      CFStringRef str = CFStringCreateWithCString(NULL, labelString, kCFStringEncodingISOLatin1);

      DrawThemeTextBox(str, kThemeSystemFont, kThemeStateActive,
		       0, &r, teJustLeft, NULL);

      CFRelease(str);
    }
    int top = (cWindowHeight - IR_CIRCLE_SIZE) / 2;
    Rect r = { top, 0, top + IR_CIRCLE_SIZE, IR_CIRCLE_SIZE };
    OffsetRect(&r,SetOriginX,SetOriginY);
    PenSize(1, 1);
    ForeColor(blackColor);
    FrameOval(&r);
    InsetRect(&r, 1, 1);
    ForeColor(whiteColor);
    PaintOval(&r);
    ForeColor(blackColor);
    if (bitmapState) {
      InsetRect(&r, IR_ON_INSET - 1, IR_ON_INSET - 1);
      PaintOval(&r);
    }
    cMacDC->setCurrentUser(NULL);
  }
}

void wxRadioButton::Highlight(Bool on)
{
  int top = (cWindowHeight - IR_CIRCLE_SIZE) / 2;
  Rect r = { top + 1, 1, top + IR_CIRCLE_SIZE - 1, IR_CIRCLE_SIZE - 1};
  OffsetRect(&r,SetOriginX,SetOriginY);
  if (!on)
    ForeColor(whiteColor);
  PenSize(1, 1);
  FrameOval(&r);
  if (!on) ForeColor(blackColor);
}

//-----------------------------------------------------------------------------
void wxRadioButton::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (cMacControl) {
    if (show)
      ::ShowControl(cMacControl);
    else
      ::HideControl(cMacControl);
  }

  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxRadioButton::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (cEnable) {
    if (event->LeftDown()) {
      SetCurrentDC();
      
      int startH, startV;
      event->Position(&startH, &startV); // client c.s.
      
      Point startPt = {startV + SetOriginY, startH + SetOriginX}; // port c.s.
      int trackResult;
      if (::StillDown()) {
	if (cMacControl)
	  trackResult = ::TrackControl(cMacControl, startPt, NULL);
	else
	  trackResult = Track(startPt);
      } else
	trackResult = 1;
      if (trackResult) {
	wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
	ProcessCommand(commandEvent);
      }
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxRadioButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (!cMacControl) return;

  SetCurrentDC();

  int clientWidth, clientHeight;
  GetClientSize(&clientWidth, &clientHeight);

  ::SizeControl(cMacControl, clientWidth, clientHeight);
  ::MoveControl(cMacControl, SetOriginX, SetOriginY);
}
