///////////////////////////////////////////////////////////////////////////////
// File:	wx_item.cc
// Purpose:	Panel items implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

/* When implementing a new item, be sure to:
 *
 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

#include "wx_item.h"
#include "wx_gdi.h"


wxItem::wxItem(void)
: wxbItem()
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
}

//-----------------------------------------------------------------------------
// Constructor (given parentArea)
wxItem::wxItem (wxArea* parentArea, int x, int y, int width, int height,
		long style, char* windowName)
: wxbItem (windowName, parentArea, x, y, width, height, style)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  cMacControl = NULL;
  padLeft = padRight = padTop = padBottom = 0;
}

//-----------------------------------------------------------------------------
// Constructor (given parentWindow)
wxItem::wxItem (wxWindow* parentWindow, int x, int y, int width, int height, 
		long style, char* windowName) 
: wxbItem (windowName, parentWindow, x, y, width, height, style)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  cMacControl = NULL;
  padLeft = padRight = padTop = padBottom = 0;
}

//-----------------------------------------------------------------------------
// Constructor (given objectType; i.e., menu or menuBar)
wxItem::wxItem (char* windowName) 
: wxbItem (windowName)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  cMacControl = NULL;
  padLeft = padRight = padTop = padBottom = 0;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxItem::~wxItem(void)
{
}

//-----------------------------------------------------------------------------
void wxItem::SetBackgroundColour(wxColour*col)
{
  backColour = col;
  ChangeColour();
}

//-----------------------------------------------------------------------------
void wxItem::SetLabelColour(wxColour*col)
{
  labelColour = col ;
  ChangeColour();
}

//-----------------------------------------------------------------------------
void wxItem::SetButtonColour(wxColour*col)
{
  buttonColour = col ;
  ChangeColour();
}

//-----------------------------------------------------------------------------
void wxItem::ChangeColour(void)
{
}


//-----------------------------------------------------------------------------
void wxItem::ChangeToGray(Bool gray)
{
  if (cMacControl) {
    SetCurrentDC();
    if (gray) {
      DisableControl(cMacControl);
    } else {
      EnableControl(cMacControl);
    }
  }
  
  wxWindow::ChangeToGray(gray);
}

void wxItem::Activate(Bool on)
{
  if (cMacControl) {
    SetCurrentDC();
    if (!on) {
      DeactivateControl(cMacControl);
    } else {
      ActivateControl(cMacControl);
    }
  }

  wxWindow::Activate(on);
}



//-----------------------------------------------------------------------------
void wxItem::OnChar(wxKeyEvent *event)
{
  // Default is to pass chars up to our panel
  wxPanel *parent = (wxPanel *) GetParent();
  if (parent)
    {
      // parent is not always a wxPanel: can be a wxMenu...
      if (wxSubType(parent->__type,wxTYPE_PANEL))
	{
	  parent->OnChar(event);
	}
    }
}


char *wxItemStripLabel(char *label)
{
  if (!label)
    return NULL;
  
  int i;
  for (i = 0; label[i]; i++) {
    if (label[i] == '&') {
      /* Strip it: */
      char *naya = new char[strlen(label) + 1];
      int j = 0;
      for (i = 0; label[i]; i++) {
        if (label[i] == '&') {
          if (label[i + 1]) {
            naya[j++] = label[i + 1];
            i++;
          }
        } else
          naya[j++] = label[i];
      }
      
      return naya;
    }
  }
  
  return label;
}


//-----------------------------------------------------------------------------

void wxItem::MaybeMoveControls()
{
  if (cMacControl) {
    // This operation could be guarded by a memoized check, 
    // if it turns out to be expensive.  I doubt it will.
    cMacDC->setCurrentUser(NULL); // macDC no longer valid
    SetCurrentDC();
    
    MoveControl(cMacControl,SetOriginX + padLeft, SetOriginY + padTop);
  }
}

