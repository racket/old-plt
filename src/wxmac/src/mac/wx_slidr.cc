/*
 * File:	wx_slidr.cc
 * Purpose:	Panel item slider implementation (Macintosh version)
 * Author:	Cecil Coupe
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

static const char sccsid[] = "%W% %G%";



#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "wx_messg.h"
#include "wx_mgstr.h"
#include "wx_utils.h"
#include "wx_slidr.h"


#define MEANING_CHARACTER	'0'

#define  USE_ACTIONPROC 0	// set to 1 to try the thumbtracking stuff
							// unfinished (crashes system) as of 7/29/95

// Slider
/* 
	For wxMac a wxSlider contains
	1. A scroll control (horizontal)
	2. A wxLabelArea for the Label/Title
	3. a Rect for displaying the current value

*/

#define KDEFAULTW  60	// number pixels wide for a default scroll control
#ifdef OS_X
# define KSCROLLH   15
#else
# define KSCROLLH   16	// height of a mac scrollbar control
#endif
#define VSP			3	// space between scrollbar and value
#define HSP			3	
// Because I never get this right and t,l,b,r makes sense to me - CJC
//
#define SetBounds(rect, top, left, bottom, right) ::SetRect(rect, left, top, right, bottom)

wxSlider::wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y,
           long style, char *name, WXTYPE objectType
	):
  wxbSlider(panel, label, value, min_value, max_value, width, x, y, style, name)
{
  Create(panel, func, label, value, min_value, max_value, width, x, y, style, name);
}

Bool wxSlider::Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y,
           long style, char *name)
{
	windowStyle = style;
	window_parent = panel;
	labelPosition = panel->label_position;
	buttonFont = panel->buttonFont;
	labelFont = panel->labelFont;
	backColour = panel->backColour;
	labelColour = panel->labelColour;
	buttonColour = panel->buttonColour;
	
	label = wxItemStripLabel(label);

    Callback(func);
	SetCurrentDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	
  	s_min = min_value;
  	s_max = max_value;
	// page_size calculated 
	page_size = (s_max - s_min) / 15; // 15 == size of thumb ?
	if (page_size < 2)
		page_size = 2;

	valueFont = buttonFont;
	float fWidth;
	float fHeight;
	float fDescent;
	float fLeading;
	int	lblh=0;
	int lblw=0;
	if (label) {
		GetTextExtent(label, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		lblh = (int)fHeight;
		lblw = (int)fWidth;
	}
	int vwid, vhgt, hsp, vsp;
	if (style & (wxHORIZONTAL << 2)) {
	  vwid = 0;
	  vhgt = 0;
	  hsp = 0;
	  vsp = 0;
	} else {
	  GetTextExtent("258", &fWidth, &fHeight, &fDescent, &fLeading, valueFont);
	  vwid = (int)fWidth;
	  vhgt = (int)fHeight;
	  hsp = HSP;
	  vsp = VSP;
    }
    
	Rect boundsRect = {0, 0, KSCROLLH, KDEFAULTW};
	
	int adjust = 0;
	if (style & wxVERTICAL) {
		if (width < 0)
			cWindowHeight = KDEFAULTW + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		else
			cWindowHeight = width;
		cWindowWidth = vwid + KSCROLLH + hsp + ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
		
		boundsRect.right = KSCROLLH;
		boundsRect.bottom = cWindowHeight - ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		
		valueRect.left = cWindowWidth - vwid + 1;
		valueRect.top = (cWindowHeight - vhgt) / 2;
		adjust = -1;
	} else {
		if (width < 0)
			cWindowWidth = KDEFAULTW + ((labelPosition == wxHORIZONTAL) ? lblw + HSP : 0);
		else
			cWindowWidth = width;
		cWindowHeight = vhgt + KSCROLLH + vsp + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		
		boundsRect.right = cWindowWidth - ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
		
		valueRect.top = cWindowHeight - vhgt;
		valueRect.left = (cWindowWidth - vwid) / 2;
	}

	if (style & (wxHORIZONTAL << 2)) {
	  valueRect.bottom = valueRect.top = 0;
	  valueRect.right = valueRect.left = 0;
	} else {
	  valueRect.bottom = valueRect.top + vhgt;
	  valueRect.right = valueRect.left + vwid + adjust;
	}
	valuebase = (int)fDescent;
        
        OffsetRect(&boundsRect,SetOriginX,SetOriginY);
	cMacControl = ::NewControl(GetWindowFromPort(theMacGrafPort), &boundsRect, NULL,
			TRUE, value, min_value, max_value, kControlSliderProc, (long)this);
	CheckMemOK(cMacControl);
	
	if (label) {
	 if (labelPosition == wxVERTICAL) {
	   if (cWindowWidth < lblw)
	     cWindowWidth = lblw;
	  } else {
	   if (cWindowHeight < lblh)
	     cWindowHeight = lblh;
	  }
	}

	::SetControlReference(cMacControl, (long)this);
	if (label)
	{
		cTitle = new wxLabelArea(this, label, labelFont,
				labelPosition == wxVERTICAL ? Direction::wxTop : Direction::wxLeft);
	}
	else
		cTitle = NULL;

	if (GetParent()->IsHidden())
		DoShow(FALSE);
                
        // Embed the control, if possible
        if (panel->cEmbeddingControl && cMacControl) {
            ::EmbedControl(cMacControl,panel->cEmbeddingControl);
        }
                
	
	return TRUE;
}

// ------------ Destructor ----------------------------------------
wxSlider::~wxSlider(void)
{
	 delete cTitle;	// Special care needed to delete Areas
	::DisposeControl(cMacControl);
}


//------------ Event Handling --------------------------------------
void wxSlider::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
	::Draw1Control(cMacControl);

    if (!(windowStyle & (wxHORIZONTAL << 2))) {
		SetFont(valueFont);
		SetTextInfo();
		
                Rect r = valueRect;
                OffsetRect(&r,SetOriginX,SetOriginY);
		::MoveTo(r.left, r.bottom - valuebase);
		::EraseRect(&r);
		char t[8];
		sprintf(t,"%d",::GetControlValue(cMacControl));
		::DrawText(t,0,strlen(t));
	}
	
	wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxSlider::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	SetCurrentDC();

	if (show)
		::ShowControl(cMacControl);
	else
		::HideControl(cMacControl);
		
	cTitle->DoShow(show);
		
	wxWindow::DoShow(show);
}

void wxSlider::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
	SetCurrentDC();
	if (dW || dH)
	{	
		int clientWidth = ClientArea()->Width();
		int clientHeight= ClientArea()->Height();
		Rect viewRect = *GetControlBounds(cMacControl,NULL);

		int vwid = valueRect.right - valueRect.left;
		int vhgt = valueRect.bottom - valueRect.top;
			
		if (windowStyle & wxVERTICAL) {
			int w = viewRect.right - viewRect.left;
			// the wid can't change
			::SizeControl(cMacControl, w, clientHeight);
			valueRect.top = (clientHeight - vhgt) / 2;
			valueRect.bottom = valueRect.top + vhgt;
			valueRect.left = KSCROLLH + HSP;
			valueRect.right = valueRect.left + vwid;
		} else {
			int h = viewRect.bottom - viewRect.top;
			// the hgt can't change
			::SizeControl(cMacControl, clientWidth, h);
			valueRect.left = (clientWidth - vwid) / 2;
			valueRect.right = valueRect.left + vwid;
			valueRect.top = KSCROLLH + VSP;
			valueRect.bottom = valueRect.top + vhgt;
		}
	}

	if (dX || dY)
	{	// Changing the position
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put newViewRect at (0, 0)
                ::MoveControl(cMacControl,SetOriginX + valueRect.left,SetOriginY + valueRect.top);
	}
}

static pascal void SCTrackActionProc(ControlHandle theControl, short thePart);
static ControlActionUPP SCTrackActionProcUPP = NewControlActionUPP(SCTrackActionProc);

#define max(x, y) ((x > y) ? x : y)
#define min(x, y) ((x > y) ? y : x)

void wxSlider::OnEvent(wxMouseEvent *event) // WCH: mac only ?
{
	if (event->leftDown) {
		int startH, startV;
		SetCurrentDC();
		event->Position(&startH, &startV); // client c.s.
		Point pt = {startV + SetOriginY, startH + SetOriginX};
		int part;
		part = ::TestControl(cMacControl, pt);
		if (part) {
		  if (part == kControlIndicatorPart) {
		    if (::TrackControl(cMacControl, pt, NULL))
		      TrackPart(part);
		  } else 
		    ::TrackControl(cMacControl, pt, SCTrackActionProcUPP);
		}
	}
}

void wxSlider::TrackPart(int part)
{
	int oldval = ::GetControlValue(cMacControl);

    switch (part) {
	case kControlUpButtonPart:
		::SetControlValue(cMacControl, max(s_min, oldval-1));
		break;
	case kControlDownButtonPart:
		::SetControlValue(cMacControl, min(s_max, oldval+1));
		break;
	case kControlPageUpPart:
		::SetControlValue(cMacControl, max(s_min, oldval-page_size));
		break;
	case kControlPageDownPart:
		::SetControlValue(cMacControl, min(s_max, oldval+page_size));
		break;
	case kControlIndicatorPart:
		break;
	} // end switch
	
	if (!(windowStyle & (wxHORIZONTAL << 2))) {
		// Draw the new value
                Rect r = valueRect;
                OffsetRect(&r,SetOriginX,SetOriginY);
		::MoveTo(r.left+HSP, r.bottom - valuebase);
		::EraseRect(&r);
		char t[8];
		sprintf(t,"%d",::GetControlValue(cMacControl));
		::DrawText(t,0,strlen(t));
	}
	wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
	ProcessCommand(commandEvent);
	
	// So update happens correctly as we return...
	SetCurrentDC();
}

// Update the Value rect as the thumb is dragged around
static pascal void SCTrackActionProc(ControlHandle theControl, short thePart)
{
	wxSlider*	slider;
	slider = (wxSlider*) GetControlReference(theControl);
	slider->TrackPart(thePart);
}

// --------------------- Client API ---------------------
int wxSlider::GetValue(void)
{
	return ::GetControlValue(cMacControl);
}

void wxSlider::SetValue(int value)
{
	SetCurrentDC();
	::SetControlValue(cMacControl, value);
	if (!(windowStyle & (wxHORIZONTAL << 2))) {
          Rect r = valueRect;
          OffsetRect(&r,SetOriginX,SetOriginY);
	  ::MoveTo(r.left+HSP, r.bottom - valuebase);
	  ::EraseRect(&r);
	  char t[8];
	  sprintf(t,"%d",::GetControlValue(cMacControl));
	  ::DrawText(t,0,strlen(t));
	}
}

void wxSlider::SetBackgroundColour(wxColour*col)
{
} 

void wxSlider::SetLabelColour(wxColour*col)
{
}

void wxSlider::SetButtonColour(wxColour*col) 
{
}

char* wxSlider::GetLabel(void)
{
	return (cTitle ? cTitle->GetLabel() : NULL);
}

void wxSlider::SetLabel(char *label)
{
	if (cTitle) cTitle->SetLabel(label);
	
}

void wxSlider::ChangeToGray(Bool gray)
{
  if (cTitle)
	((wxLabelArea *)cTitle)->GetMessage()->InternalGray(gray);
  wxItem::ChangeToGray(gray);
}
