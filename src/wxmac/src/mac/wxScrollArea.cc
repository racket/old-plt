///////////////////////////////////////////////////////////////////////////////
// File:	wxScrollArea.cc
// Purpose:	Scroll area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxScrollArea.h"
#include "wx_sbar.h"
#include "wx_utils.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollArea::wxScrollArea
(
 wxWindow*	parentWindow,
 wxWindow*	parentScrollWindow,
 long		style
 ) :
 wxArea (parentWindow)
{
  Bool bothScrolls;

  cStyle = style;
  cVScrollBar = NULL;
  cHScrollBar = NULL;

  if (!parentScrollWindow) wxFatalError("No parentScrollWindow for wxScrollArea");

  bothScrolls = ((cStyle & wxVSCROLL) && (cStyle & wxHSCROLL));

  // mflatt:
  //  While a scrollbar should overap a frame edge when the scrollbar is positioned
  //    along a frame edge, the full scroll bar outline must be drawn when a scrollbar
  //    is not on a frame edge. This is why I went back to the old scrollbar positioning.
  //  If someone can get the new positioning but still have all of the scrollbar outline
  //    drawn, I will be happy.

  if (cStyle & wxVSCROLL)
    {
      cVScrollBar = new wxScrollBar(this, NULL, "",
				    0, 0, kVScrollBarWidth, 0, wxVSCROLL);
      parentScrollWindow->AddChildScrollWindow(cVScrollBar);

      {
	int h;
	h = Height();
	cVScrollBar->GravitateJustify
	  (wxRight | wxTop,
	   wxVertical,
	   0, 0,
	   Width(),
	   h - (bothScrolls ? kHScrollBarHeight - 1 : 0));
      }
      cVScrollBar->SetJustify(wxVertical);
      cVScrollBar->SetGravitate(wxRight);
      SetMargin(kVScrollBarWidth, wxRight);
    }

  if (cStyle & wxHSCROLL)
    {
      cHScrollBar = new wxScrollBar(this, NULL, "",
				    0, 0, 0, kHScrollBarHeight, wxHSCROLL);
      parentScrollWindow->AddChildScrollWindow(cHScrollBar);
      {
	int w;
	w = Width();
	cHScrollBar->GravitateJustify
	  (wxBottom | wxLeft,
	   wxHorizontal,
	   0, 0,
	   w - (bothScrolls ? kVScrollBarWidth - 1 : 0),
	   Height());
      }
      cHScrollBar->SetJustify(wxHorizontal);
      cHScrollBar->SetGravitate(wxBottom);
      SetMargin(kHScrollBarHeight, wxBottom);
    }
}

//-----------------------------------------------------------------------------
wxScrollArea::~wxScrollArea(void)	// destructor
{
}
