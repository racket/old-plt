/*
 * File:	wx_gauge.cc
 * Purpose:	Panel item gauge implementation (Macintosh version)
 * Author:	Cecil Coupe
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

static const char sccsid[] = "%W% %G%";



#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "wx_mgstr.h"
#include "wx_utils.h"
#include "wx_gauge.h"
#include "wx_messg.h"

#define MEANING_CHARACTER	'0'


// Slider
/* 
	For wxMac a wxSlider contains
	1. A gauge control (horizontal)
	2. A wxLabelArea for the Label/Title
	3. a Rect for displaying the current value

*/

#define KDEFAULTW  60	// number pixels wide for a default scroll control
#define KGAUGEH    12	
#define VSP			3	// space between scrollbar and value
#define HSP			3	
// Because I never get this right and t,l,b,r makes sense to me - CJC
//
#define SetBounds(rect, top, left, bottom, right) ::SetRect(rect, left, top, right, bottom)

wxGauge::wxGauge()
{
  /* Bad */
}

wxGauge::wxGauge(wxPanel *panel, char *label, int _range, int x, int y,
           int width, int height, long style, char *name) :
  wxbGauge(panel, label, _range, x, y, width, height, style, name)
{
	SetCurrentDC();

	float fWidth;
	float fHeight;
	float fDescent;
	float fLeading;
	int	lblh=0;
	int lblw=0;
	
	range = _range;
	value = 0;
	if (range < 1)
		range = 1;

	if (label)
      label = wxItemStripLabel(label);
	
	if (label) {
		GetTextExtent(label, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		lblh = (int)fHeight;
		lblw = (int)fWidth;
	}

	valueRect.top = valueRect.left = 0;
	
	if (style & wxVERTICAL) {
		if (height < 0)
			cWindowHeight = KDEFAULTW + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		else
			cWindowHeight = height;
		cWindowWidth = KGAUGEH + ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
		
		valueRect.right = KGAUGEH;
		valueRect.bottom = cWindowHeight - ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
	} else {
		if (width < 0)
			cWindowWidth = KDEFAULTW + ((labelPosition == wxHORIZONTAL) ? lblw + HSP : 0);
		else
			cWindowWidth = width;
		cWindowHeight = KGAUGEH + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		
		valueRect.right = cWindowWidth - ((labelPosition == wxHORIZONTAL) ? lblw + HSP : 0);
		valueRect.bottom = KGAUGEH;
#ifdef OS_X // for horizontal gauges, use the native control
                OSErr err;
                Rect bounds = valueRect;
                
                OffsetRect(&bounds,SetOriginX,SetOriginY);
                err = CreateProgressBarControl(GetWindowFromPort(cMacDC->macGrafPort()),&bounds,
                                                0,0,range,FALSE,&cMacControl);

                // Embed the control, if possible
                if (panel->cEmbeddingControl && cMacControl) {
                    ::EmbedControl(cMacControl,panel->cEmbeddingControl);
                }
#endif
	}
	
	if (label) {
	 if (labelPosition == wxVERTICAL) {
	   if (cWindowWidth < lblw)
	     cWindowWidth = lblw;
	  } else {
	   if (cWindowHeight < lblh)
	     cWindowHeight = lblh;
	  }
	}

	if (label)
	{
		cTitle = new wxLabelArea(this, label, labelFont,
				labelPosition == wxVERTICAL ? Direction::wxTop : Direction::wxLeft);
	} else
		cTitle = NULL;
		
	if (GetParent()->IsHidden())
		DoShow(FALSE);
                
}

// ------------ Destructor ----------------------------------------
wxGauge::~wxGauge(void)
{
	 delete cTitle;	// Special care needed to delete Areas
}


//------------ Event Handling --------------------------------------
void wxGauge::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
        
        if (cMacControl) {
            Draw1Control(cMacControl);
        } else {

            Rect s = valueRect;
            OffsetRect(&s,SetOriginX,SetOriginY);
            FrameRect(&s);

            Rect r, w;
            r = valueRect;
            InsetRect(&r, 1, 1);
            w = r;
            long d;
            if (windowStyle & wxVERTICAL) 
                    d = (valueRect.bottom - valueRect.top);
            else
                    d = (valueRect.right - valueRect.left);
            if (value < range)
                    d = (d * value) / range;
            if (windowStyle & wxVERTICAL) {
                    r.top = r.bottom - d;
                    w.bottom = r.top;
            } else {
                    r.right = r.left + d;
                    w.left = r.right;
            }

            RGBColor save;
            GetForeColor(&save);

            if (value) {
                    if (cColour) {
                            RGBColor c;
                            c.red = 66 << 8;
                            c.green = 66 << 8;
                            c.blue = 66 << 8;
                            RGBForeColor(&c);
                    }
                    OffsetRect(&r,SetOriginX,SetOriginY);
                    PaintRect(&r);
            }
	
            if (value < range) {
                    if (cColour) {
                            RGBColor c;
                            c.red = 204 << 8;
                            c.green = 204 << 8;
                            c.blue = 0xFFFF;
                            RGBForeColor(&c);
                    } else
                            ForeColor(whiteColor);
                    OffsetRect(&w,SetOriginX,SetOriginY);
                    PaintRect(&w);
            }

            RGBForeColor(&save);
        }

	wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxGauge::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	cTitle->DoShow(show);

        if (cMacControl) {
            if (show) {
                ShowControl(cMacControl);
            } else {
                HideControl(cMacControl);
            }
        }
	wxWindow::DoShow(show);
}

void wxGauge::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
	SetCurrentDC();

	if (dW || dH)
	{	
		int clientWidth = ClientArea()->Width();
		int clientHeight= ClientArea()->Height();

		int vwid = valueRect.right - valueRect.left;
		int vhgt = valueRect.bottom - valueRect.top;
			
		if (windowStyle & wxVERTICAL) {
			// the wid can't change
			valueRect.left = (clientWidth - vwid) / 2;
			valueRect.right = valueRect.left + vwid;
			valueRect.bottom = clientHeight;
		} else {
			// the hgt can't change
			valueRect.top = (clientHeight - vhgt) / 2;
			valueRect.bottom = valueRect.top + vhgt;
			valueRect.right = clientWidth;
		}
                if (cMacControl) {
                    SizeControl(cMacControl,valueRect.right - valueRect.left, valueRect.bottom - valueRect.top);
                }
	}

	if (dX || dY)
	{	// Changing the position
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put newViewRect at (0, 0)
                if (cMacControl) {
                    MoveControl(cMacControl,SetOriginX + valueRect.left,SetOriginY + valueRect.top);
                }
	}
}


// --------------------- Client API ---------------------

void wxGauge::SetValue(int v)
{
	value = v;
	if (value > range)
		value = range;
	else if (value < 0)
		value = 0;
    if (cMacControl) {
        SetControlValue(cMacControl,value);
    }
	Paint();
}

void wxGauge::SetRange(int v)
{
	range = v;
	if (range < 1)
		range = 1;
	if (value > range)
		value = range;
        if (cMacControl) {
            SetControlMaximum(cMacControl,range);
            SetControlValue(cMacControl,value);
        }
	Paint();
}

char* wxGauge::GetLabel(void)
{
	return (cTitle ? cTitle->GetLabel() : NULL);
}

void wxGauge::SetLabel(char *label)
{
	if (cTitle) cTitle->SetLabel(label);
	
}

void wxGauge::ChangeToGray(Bool gray)
{
  if (cTitle)
	((wxLabelArea *)cTitle)->GetMessage()->InternalGray(gray);
  wxItem::ChangeToGray(gray);
}
