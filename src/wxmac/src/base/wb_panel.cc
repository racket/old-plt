/*
 * File:	wb_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_panel.cc,v 1.4 1998/08/16 13:52:53 robby Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_panel.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wb_panel.h"
#include "wx_buttn.h"
#include "wx_stdev.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

class wxFrame;
class wxPanel;

// Constructors

wxbPanel::~wxbPanel(void)
{
}

wxObject* wxbPanel::GetChild(int number)
{
  // Return a pointer to the Nth object in the Panel
  if (!children)
    return(NULL) ;
  wxChildNode *node = GetChildren()->First();
  while (node && number--)
    node = node->Next() ;
  if (node)
  {
    wxObject *obj = (wxObject *)node->Data();
    return(obj) ;
  }
  else
    return NULL ;
}

void wxbPanel::SetLabelPosition(int pos)  // wxHORIZONTAL or wxVERTICAL
{
  label_position = pos;
}

int wxbPanel::GetLabelPosition(void)
{
  return label_position;
}

void wxbPanel::OnDefaultAction(wxItem *initiatingItem)
{
}

void wxbPanel::SetLabelFont(wxFont *fnt)
{
  labelFont = fnt ;
//***added by D.Chubraev 
  this->font=labelFont; 
#ifdef wx_motif
  int scaled_size = (int) (10 * ((int) (this->font->GetPointSize () + 0.5))); 
  int res_x = 100; 
  int res_y = 100; 
  XFontStruct *fontStruct = wxFontPool->FindNearestFont (this->font->GetFamily (
),  
                                this->font->GetStyle (), 
                                this->font->GetWeight (), scaled_size, 
                                this->font->GetUnderlined (), res_x, res_y); 
  this->font->xFont=fontStruct; 
#endif 
//*** 
}

void wxbPanel::SetButtonFont(wxFont *font)
{
  buttonFont = font ;
}

void wxbPanel::SetBackgroundColour(wxColour *col)
{
  backColour = col ;
}

void wxbPanel::SetLabelColour(wxColour *col)
{
  labelColour = col ;
}

void wxbPanel::SetButtonColour(wxColour *col)
{
  buttonColour = col ;
}

//=============================================================================
// Protected constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxbPanel::wxbPanel // Constructor (given parentArea)
	(
		char*		windowName,
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxCanvas (parentArea, x, y, width, height, style, windowName)
{
	__type = wxTYPE_PANEL;
	InitDefaults();
	InitMoreDefaults();
}

//-----------------------------------------------------------------------------
wxbPanel::wxbPanel // Constructor (given parentWindow)
	(
		char*		windowName,
		wxWindow*	parentWindow,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxCanvas (parentWindow, x, y, width, height, style, windowName)
{
	__type = wxTYPE_PANEL;
	InitDefaults();
	InitMoreDefaults();
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxbPanel::InitDefaults(void)
{
	defaultItem = NULL;

	has_child = FALSE;

	hSpacing = PANEL_HSPACING;
	vSpacing = PANEL_VSPACING;
	initial_hspacing = hSpacing;
	initial_vspacing = vSpacing;
	current_hspacing = hSpacing;
	current_vspacing = vSpacing;

	new_line = FALSE;
}

//-----------------------------------------------------------------------------
void wxbPanel::InitMoreDefaults(void) // Poor name for this method
{
    if (wxSubType(window_parent->__type, wxTYPE_PANEL) &&
    	cParentArea == window_parent->ClientArea())
	{
    	wxPanel* parentPanel = (wxPanel*) window_parent;
		backColour = parentPanel->backColour;
		buttonColour = parentPanel->buttonColour;
		buttonFont = parentPanel->buttonFont;
		labelColour = parentPanel->labelColour;
		labelFont = parentPanel->labelFont;
		label_position = parentPanel->label_position;
	}
	else
	{
		backColour = NULL;
		buttonColour = NULL;
		buttonFont = wxNORMAL_FONT;
		labelColour = NULL;
		labelFont = wxNORMAL_FONT;
		label_position = wxHORIZONTAL;
	}
}
