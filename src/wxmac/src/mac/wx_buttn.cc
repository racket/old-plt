///////////////////////////////////////////////////////////////////////////////
// File:	wx_buttn.cc
// Purpose:	Panel item button implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_buttn.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wb_gdi.h"
#ifndef OS_X
#include <Windows.h>
#endif
#include "wxButtonBorder.h"

#if 1
#define MIN_BUTTON_WIDTH 58
#define BUTTON_H_SPACE 12
#define BUTTON_V_SPACE 4
#else
/* Original parameters */
#define MIN_BUTTON_WIDTH 60
#define BUTTON_H_SPACE 20
#define BUTTON_V_SPACE 10
#endif

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
		wxbButton (parentPanel, x, y, width, height, style, windowName)
		
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
    buttonBitmap = NULL;
	cColorTable = NULL;
		
	Callback(function);

	font = buttonFont; // WCH: mac platform only
	
	label = wxItemStripLabel(label);

	if (width <= 0 || height <= 0)
	{
		float fWidth, fHeight;
		GetTextExtent(label, &fWidth, &fHeight, NULL, NULL, buttonFont);
		if (width <= 0)
		{
			width = (int)(fWidth + BUTTON_H_SPACE);
			if (width < MIN_BUTTON_WIDTH) width = MIN_BUTTON_WIDTH;
			cWindowWidth = width;
		}

		if (height <= 0)
		{
			height = (int)(fHeight + BUTTON_V_SPACE);
			cWindowHeight = height;
		}
	}

	cBorderArea = new wxArea(this);
	new wxButtonBorder(cBorderArea);

    if (style & 1) OnSetDefault(TRUE);

	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect boundsRect = {0, 0, ClientArea()->Height(), ClientArea()->Width()};
	wxMacString theMacTitle = label;
	const Bool drawNow = TRUE; // WCH: use FALSE, then show after ChangeColour??
	const short offValue = 0;
	const short minValue = 0;
	const short maxValue = 1;
	short refCon = 0;
	cMacControl = ::NewControl(GetWindowFromPort(theMacGrafPort), &boundsRect, theMacTitle(),
			drawNow, offValue, minValue, maxValue, pushButProc + popupUseWFont, refCon);
	CheckMemOK(cMacControl);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
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
		wxbButton (parentPanel, x, y, width, height, style, windowName),
		cColorTable(NULL)
{
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

	cMacControl = NULL;
	
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect bounds = {0, 0, buttonBitmap->GetHeight(), buttonBitmap->GetWidth()};
	bounds.bottom += 2 * IB_MARGIN_Y;
	bounds.right += 2 * IB_MARGIN_X;
	cWindowHeight = bounds.bottom;
	cWindowWidth = bounds.right;

	::InvalWindowRect(GetWindowFromPort(theMacGrafPort),&bounds);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::~wxButton(void)
{
	if (buttonBitmap == NULL) {
		if (cMacControl) ::DisposeControl(cMacControl);
	} else
		--buttonBitmap->selectedIntoDC;
}

//-----------------------------------------------------------------------------
void wxButton::ChangeColour(void)
{
#ifndef OS_X
// all the other controls have null defns for ChangeColour, I don't see why
// buttons can't, as well.  Particularly because Custom Color tables aren't 
// supported for Carbon.

	if (buttonBitmap)
		return;
	if (cColorTable == NULL)
	{
		cColorTable = (CCTabHandle)NewHandle(40);
		CheckMemOK(cColorTable);
		(**cColorTable).ccSeed = 0;
		(**cColorTable).ccRider = 0;
		(**cColorTable).ctSize = 2;
		(**cColorTable).ctTable[0].value = cFrameColor;
		(**cColorTable).ctTable[1].value = cBodyColor;
		(**cColorTable).ctTable[2].value = cTextColor;
		(**cColorTable).ctTable[3].value = cThumbColor;
	}

	RGBColor whiteColor = {65535, 65535, 65535};
	RGBColor blackColor = {0, 0, 0};

	wxColour* backgroundColour = GetBackgroundColour();
	RGBColor backgroundRGB;
	backgroundRGB = backgroundColour ? backgroundColour->pixel : blackColor;
	(**cColorTable).ctTable[0].rgb = backgroundRGB;

	wxColour* buttonColour = GetButtonColour();
	RGBColor buttonRGB;
	buttonRGB = buttonColour ? buttonColour->pixel : whiteColor;
	(**cColorTable).ctTable[1].rgb = buttonRGB;


	wxColour* labelColour = GetLabelColour();
	RGBColor labelRGB;
	labelRGB = labelColour ? labelColour->pixel : blackColor;
	(**cColorTable).ctTable[2].rgb = labelRGB;

     if (cMacControl)
	  ::SetControlColor(cMacControl, cColorTable);
#endif          
}

//-----------------------------------------------------------------------------
char* wxButton::GetLabel(void)
{
	Str255	pTitle;
	if (buttonBitmap)
		return NULL;
	if (cMacControl)
	  ::GetControlTitle(cMacControl, pTitle);
	wxMacPtoCString(pTitle, wxBuffer);
    return wxBuffer;
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(char* label)
{
  if (buttonBitmap)
		return;
  if (label)
  {
	SetCurrentDC();
  	wxMacString1 theMacString1 = wxItemStripLabel(label);
  	if (cMacControl)
  	  ::SetControlTitle(cMacControl, theMacString1());
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
{ // WCH: a panel method should be swapping default buttons
  // WCH: we would then have: void wxPanel::SetDefault(wxItem* item), NULL item allowed
	wxPanel* panel = (wxPanel*) GetParent();
	if (!panel) wxFatalError("No panel for wxButton::SetDefault.");
	wxButton* currentDefault = panel->defaultItem; // WCH: let any wxItem be a default item ?

	if (flag) // this becoming default item
	{
		if (currentDefault != this)
		{
			if (currentDefault) currentDefault->OnSetDefault(FALSE);
			panel->defaultItem = this;
			OnSetDefault(TRUE);
		}
	}
	else // this no longer default item
	{
		if (currentDefault == this)
		{
			currentDefault->OnSetDefault(FALSE);
			panel->defaultItem = NULL;
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::OnSetDefault(Bool flag) // WCH : addition to original
{ // WCH: the panel should invoke the default button to distinguish itself
	if (buttonBitmap)
		return;
	if (flag)
	{
		wxMargin margin(4);
		cBorderArea->SetMargin(margin, Direction::wxAll,
						cWindowWidth + 8, cWindowHeight + 8,
						cWindowX - 4, cWindowY - 4);
	}
	else
	{
		wxMargin margin(0);
		cBorderArea->SetMargin(margin, Direction::wxAll,
						cWindowWidth - 8, cWindowHeight - 8,
						cWindowX + 4, cWindowY + 4);
	}
}

//-----------------------------------------------------------------------------
void wxButton::Enable(Bool enable)
{
	if ((enable != cEnable) && cActive && cMacControl) {
		SetCurrentDC();
		if (enable) {
			::ActivateControl(cMacControl);
		}
		else {
			::DeactivateControl(cMacControl);
		}
	}
	wxWindow::Enable(enable);
}

//-----------------------------------------------------------------------------
static void PaintBitmapButton(Rect *r, wxBitmap *buttonBitmap, Bool pressed, Bool isgray, 
                              int cColour)
{
  static wxColour *dark, *darker, *lite;
  wxColour *back, *bright, *dim;
  
  if (!dark) {
  	wxColour *norm;
    norm = wxCONTROL_BACKGROUND_BRUSH->GetColour();
    
#   define DARK_SCALE(x) (x - (x >> 2))
#   define DARKER_SCALE(x) (x >> 1)
#   define LITE_SCALE(x) 0xFF

    dark = new wxColour(DARK_SCALE(norm->Red()), 
    				    DARK_SCALE(norm->Green()), 
    				    DARK_SCALE(norm->Blue()));
    				    
    darker = new wxColour(DARKER_SCALE(norm->Red()), 
    				      DARKER_SCALE(norm->Green()), 
    				      DARKER_SCALE(norm->Blue()));
    				    
    lite = new wxColour(LITE_SCALE(norm->Red()), 
    				    LITE_SCALE(norm->Green()), 
    				    LITE_SCALE(norm->Blue()));
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

  Rect rr = *r;
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
	SetCurrentDC();
	Rect r = { 0, 0, cWindowHeight, cWindowWidth };
	if (buttonBitmap) {
	    PaintBitmapButton(&r, buttonBitmap, 0, IsGray(), cColour);
	} else if (cMacControl) {
	    ::EraseRect(&r);
            if (!IsControlVisible(cMacControl)) return;
            ::Draw1Control(cMacControl);
	}
	wxWindow::Paint();
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
void wxButton::ShowAsActive(Bool flag) // mac platform only
{
	if ((! buttonBitmap) && cEnable && cMacControl) {
		SetCurrentDC();
		if (flag) {
			ActivateControl(cMacControl);
		}
		else {
			DeactivateControl(cMacControl);
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::Highlight(Bool flag) // mac platform only
{
	if (buttonBitmap) {
		SetCurrentDC();
		Rect bounds = {0, 0, cWindowHeight, cWindowWidth};
		PaintBitmapButton(&bounds, buttonBitmap, flag, FALSE, cColour);
	} else if (cMacControl) {
		if (cEnable)
		{
			SetCurrentDC();
			::HiliteControl(cMacControl, flag ? kControlButtonPart : 0);
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::OnEvent(wxMouseEvent *event) // mac platform only
{
	if (event->LeftDown())
	{
		SetCurrentDC();
	
		int startH, startV;
		event->Position(&startH, &startV); // client c.s.
	
		Point startPt = {startH, startV}; // client c.s.
		int trackResult;
		if (::StillDown()) {
			if (buttonBitmap == NULL && cMacControl)
				trackResult = ::TrackControl(cMacControl, startPt, NULL);
			else
				trackResult = Track(startPt);
		} else {
			Highlight(TRUE); // highlight button
			long delayTicks = 4; // one tick is 1/60th of a second
			unsigned long finalTicks;
			Delay(delayTicks, &finalTicks);
			Highlight(FALSE); // unhighlight button
		
			trackResult = 1;
		}
		if (trackResult)
		{
			wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
	  		ProcessCommand(commandEvent);
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::ChangeToGray(Bool gray)
{
  /* graying is now handled by the ShowAsActive routine. 
     as far as I can tell, this code was never called anyway. */  
  /*
  SetCurrentDC();
  if (cMacControl)
    ::HiliteControl(cMacControl, gray ? kInactiveControl : kActiveControl);
  */
    
  wxWindow::ChangeToGray(gray);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
	SetCurrentDC();
	if (buttonBitmap || !cMacControl)
		return;

	Bool isVisible = cMacControl && IsShown();
	Bool hideToPreventFlicker = (isVisible && (dX || dY) && (dW || dH));
	if (hideToPreventFlicker) ::HideControl(cMacControl);

	if (dW || dH)
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		::SizeControl(cMacControl, clientWidth, clientHeight);
	}

	if (dX || dY)
	{
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put new origin at (0, 0)
		::MoveControl(cMacControl, 0, 0);
	}

	if (hideToPreventFlicker) ::ShowControl(cMacControl);

	if (!cHidden && (dW || dH || dX || dY))
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&clientRect);
	}
}
