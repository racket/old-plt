///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbut.cc
// Purpose:	Panel item radioButton implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_rbut.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#ifndef OS_X
  #include <QuickDraw.h>
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

#if 1
	float fLabelWidth = 100.0;
	float fLabelHeight = 20.0;
	if (label)
	{
		GetTextExtent(label, &fLabelWidth, &fLabelHeight, NULL, NULL, buttonFont);
		fLabelWidth += 20; // add 20 for width of radio button icon
		if (fLabelHeight < 12) fLabelHeight = 12; // height of radio button icon is 12
	}

	if (width < 0) cWindowWidth = (int)fLabelWidth;
	if (height < 0) cWindowHeight = (int)fLabelHeight;

	labelString = label;
#else
        // First, create the control with a bogus rectangle;
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect boundsRect = {0, 0, 0, 0};
        OffsetRect(&boundsRect,SetOriginX,SetOriginY);
	CFStringRef theMacLabel = CFStringCreateWithCString(NULL,label,kCFStringEncodingISOLatin1);
        OSErr err;
        
        err = CreateRadioButtonControl(GetWindowFromPort(theMacGrafPort),&boundsRect,theMacLabel,
                                        0,FALSE,&cMacControl);
                                        
        
        // Now, ignore the font data and let the control find the "best" size 
        ::SetRect(&boundsRect,0,0,0,0);
        SInt16 baselineOffset; // ignored
        err = ::GetBestControlRect(cMacControl,&boundsRect,&baselineOffset);
        cWindowWidth = boundsRect.right - boundsRect.left;
        cWindowHeight = boundsRect.bottom - boundsRect.top;
        ::SizeControl(cMacControl,boundsRect.right - boundsRect.left, boundsRect.bottom - boundsRect.top);

#if 0
	// EMBEDDING
        // Embed the control, if possible
        if (parentPanel->cEmbeddingControl && cMacControl) {
            ::EmbedControl(cMacControl,parentPanel->cEmbeddingControl);
        }
#endif        
        
#endif
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
	
	SetCurrentMacDC();
	Rect bounds = {0, 0, buttonBitmap->GetHeight(), buttonBitmap->GetWidth()};
	cWindowHeight = bounds.bottom;
	cWindowWidth = bounds.right + IR_CIRCLE_SIZE + IR_X_SPACE;
	if (cWindowHeight < IR_MIN_HEIGHT)
	  cWindowHeight = IR_MIN_HEIGHT;
        OffsetRect(&bounds,SetOriginX,SetOriginY);

	::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&bounds);
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
  if (label && !buttonBitmap)
  {
    if (cMacControl) {
	  SetCurrentDC();
  	  wxMacString1 theMacString1 = label;
  	  ::SetControlTitle(cMacControl, theMacString1());
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
	SetCurrentDC();
	if (cMacControl)
	  ::SetControlValue(cMacControl, val ? 1 : 0);
	else {
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
	    float fWidth = 50.0;
		float fHeight = 12.0;
		float fDescent = 0.0;
		float fLeading = 0.0;
		GetTextExtent(labelString, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		int stop = (int)((cWindowHeight + fHeight) / 2);
		::MoveTo(IR_CIRCLE_SIZE + IR_X_SPACE + SetOriginX, (short)(stop - fDescent - fLeading) + SetOriginY);
	  	::DrawText(labelString, 0, strlen(labelString));
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

	SetCurrentDC();
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
	if (cEnable)
	{
		if (event->LeftDown())
		{
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
			if (trackResult)
			{
				wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND); // WCH: must change constant
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
#ifndef OS_X
	Bool hideToPreventFlicker = (IsControlVisible(cMacControl) && (dX || dY) && (dW || dH));
	if (hideToPreventFlicker) ::HideControl(cMacControl);
#endif
	if (dW || dH)
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		::SizeControl(cMacControl, clientWidth, clientHeight);
	}

	if (dX || dY)
	{
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put new origin at (SetOriginX,SetOriginY)
		::MoveControl(cMacControl, SetOriginX, SetOriginY);
	}
#ifndef OS_X
	if (hideToPreventFlicker) ::ShowControl(cMacControl);
#endif
	if (!cHidden && (dW || dH || dX || dY))
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
                OffsetRect(&clientRect,SetOriginX,SetOriginY);
		::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&clientRect);
	}
}
