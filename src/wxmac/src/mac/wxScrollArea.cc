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

#define kVScrollBarWidth 15
#define kHScrollBarHeight 15

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

//-----------------------------------------------------------------------------
void wxScrollArea::ShowScrolls(Bool h, Bool v)
{
  
}
