///////////////////////////////////////////////////////////////////////////////
// File:	wxRectBorder.cc
// Purpose:	Macintosh RectBorder implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxRectBorder.h"
#include "wxMacDC.h"
#include "wx_area.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRectBorder::wxRectBorder // Constructor (given parentArea)
(
 wxArea*		parentArea,
 int			margin,
 Direction	direction,
 int         whitespace,
 Bool        te_border,
 char*		windowName,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 WXTYPE		objectType
 ) :
  wxBorder (parentArea, windowName, wxNEGPOS_IS_DEFAULT(x), wxNEGPOS_IS_DEFAULT(y), width, height, style, objectType)
{
  cWhitespace = whitespace;
  parentArea->SetMargin(margin, direction);
  if (te_border) {
    cPaintFocus = -1;
  }
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRectBorder::~wxRectBorder(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxRectBorder::DoShow(Bool on)
{
  wxWindow::DoShow(on);
}

//-----------------------------------------------------------------------------
void wxRectBorder::Paint(void)
{
  if (cHidden) return;

  if (SetCurrentDC()) {
    int clientWidth, clientHeight;
    Rect clientRect;
    int margin, disabled;
    wxArea *area;
    wxMargin m;

    GetClientSize(&clientWidth, &clientHeight);
    ::SetRect(&clientRect, 0, 0, clientWidth, clientHeight);
    OffsetRect(&clientRect,SetOriginX,SetOriginY);

    disabled = IsGray();

    if (cPaintFocus) {
      InsetRect(&clientRect, 2, 2);
      if (cPaintFocus < 0)
	DrawThemeFocusRect (&clientRect, FALSE);
      DrawThemeEditTextFrame(&clientRect, disabled ? kThemeStateInactive : kThemeStateActive);
      if (cPaintFocus > 0)
	DrawThemeFocusRect (&clientRect, TRUE);
    } else {
      RGBColor c;

      area = ParentArea();
      m = area->Margin();

      if (!disabled) {
	c.red = c.green = c.blue = 0x7FFF;
      } else {
	c.red = c.green = c.blue = 0xaFFF;
      }

      RGBForeColor(&c);

      margin = m.Offset(wxTop) - cWhitespace;
      if (margin > 0) {
	::PenSize(margin, margin);
	::MoveTo(clientRect.left, clientRect.top);
	::LineTo(clientRect.right - margin, clientRect.top);
      }

      margin = m.Offset(wxBottom) - cWhitespace;
      if (margin > 0) {
	::PenSize(margin, margin);
	::MoveTo(clientRect.left, clientRect.bottom - margin);
	::LineTo(clientRect.right - margin, clientRect.bottom - margin);
      }

      margin = m.Offset(wxLeft) - cWhitespace;
      if (margin > 0) {
	::PenSize(margin, margin);
	::MoveTo(clientRect.left, clientRect.top);
	::LineTo(clientRect.left, clientRect.bottom - margin);
      }

      margin = m.Offset(wxRight) - cWhitespace;
      if (margin > 0) {
	::PenSize(margin, margin);
	::MoveTo(clientRect.right - margin, clientRect.top);
	::LineTo(clientRect.right - margin, clientRect.bottom - margin);
      }
    }
  }
}


//-----------------------------------------------------------------------------
void wxRectBorder::ChangeToGray(Bool gray) 
{
  if (SetCurrentMacDC()) {
    RgnHandle rgn, rgn2;
    int margin;
    Rect clientRect;
    wxArea *area;
    wxMargin m;
    int clientWidth, clientHeight;

    GetClientSize(&clientWidth, &clientHeight);
    ::SetRect(&clientRect, 0, 0, clientWidth, clientHeight);
    OffsetRect(&clientRect,SetOriginX,SetOriginY);

    /* We should really get all 4 margins... */
    area = ParentArea();
    m = area->Margin();
    margin = m.Offset(wxTop);
    
    rgn = NewRgn();
    if (rgn) {
      CGrafPtr graf;
      RectRgn(rgn, &clientRect);
      rgn2 = NewRgn();
      if (rgn2) {
	InsetRect(&clientRect, margin, margin);
	RectRgn(rgn2, &clientRect);
	DiffRgn(rgn, rgn2, rgn);
	DisposeRgn(rgn2);
      }
      graf = cMacDC->macGrafPort();
      InvalWindowRgn(GetWindowFromPort(graf), rgn);
      DisposeRgn(rgn);
    }
  }

  wxWindow::ChangeToGray(gray);
}


void wxRectBorder::PaintFocus(Bool on) 
{
  if (cPaintFocus) {
    cPaintFocus = (on ? 1 : -1);
    Paint();
    RefreshIfUpdating();
  }
}
