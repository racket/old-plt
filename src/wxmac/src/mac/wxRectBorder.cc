///////////////////////////////////////////////////////////////////////////////
// File:	wxRectBorder.cc
// Purpose:	Macintosh RectBorder implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
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
 char*		windowName,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 WXTYPE		objectType
 ) :
  wxBorder (parentArea, windowName, x, y, width, height, style, objectType)
{
  cWhitespace = whitespace;
  parentArea->SetMargin(margin, direction);
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
    GetClientSize(&clientWidth, &clientHeight);
    Rect clientRect = {0, 0, clientHeight, clientWidth};
    OffsetRect(&clientRect,SetOriginX,SetOriginY);
    int margin;

    margin = ParentArea()->Margin().Offset(Direction::wxTop) - cWhitespace;
    if (margin > 0) {
      ::PenSize(margin, margin);
      ::MoveTo(clientRect.left, clientRect.top);
      ::LineTo(clientRect.right - margin, clientRect.top);
    }

    margin = ParentArea()->Margin().Offset(Direction::wxBottom) - cWhitespace;
    if (margin > 0) {
      ::PenSize(margin, margin);
      ::MoveTo(clientRect.left, clientRect.bottom - margin);
      ::LineTo(clientRect.right - margin, clientRect.bottom - margin);
    }

    margin = ParentArea()->Margin().Offset(Direction::wxLeft) - cWhitespace;
    if (margin > 0) {
      ::PenSize(margin, margin);
      ::MoveTo(clientRect.left, clientRect.top);
      ::LineTo(clientRect.left, clientRect.bottom - margin);
    }

    margin = ParentArea()->Margin().Offset(Direction::wxRight) - cWhitespace;
    if (margin > 0) {
      ::PenSize(margin, margin);
      ::MoveTo(clientRect.right - margin, clientRect.top);
      ::LineTo(clientRect.right - margin, clientRect.bottom - margin);
    }
  }
}


//-----------------------------------------------------------------------------
void wxRectBorder::ChangeToGray(Bool gray) 
{
  if (SetCurrentMacDC()) {
    RgnHandle rgn, rgn2;
    int margin;

    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    Rect clientRect = {0, 0, clientHeight, clientWidth};
    OffsetRect(&clientRect,SetOriginX,SetOriginY);

    /* We should really get all 4 margins... */
    margin = ParentArea()->Margin().Offset(Direction::wxTop);
    
    rgn = NewRgn();
    if (rgn) {
      RectRgn(rgn, &clientRect);
      rgn2 = NewRgn();
      if (rgn2) {
	InsetRect(&clientRect, margin, margin);
	RectRgn(rgn2, &clientRect);
	DiffRgn(rgn, rgn2, rgn);
	DisposeRgn(rgn2);
      }
      InvalWindowRgn(GetWindowFromPort(cMacDC->macGrafPort()), rgn);
      DisposeRgn(rgn);
    }
  }

  wxWindow::ChangeToGray(gray);
}
