///////////////////////////////////////////////////////////////////////////////
// File:	wx_item.cc
// Purpose:	Panel items implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
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

extern Bool wx_ignore_key;
Bool wx_propagate_key;

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
    if (gray) {
#ifdef OS_X
      DisableControl(cMacControl);
#else
      HiliteControl(cMacControl, 255);
#endif
    } else {
#ifdef OS_X
      EnableControl(cMacControl);
#else
      HiliteControl(cMacControl, 0);
#endif
    }
  }

  wxWindow::ChangeToGray(gray);
}

void wxItem::Activate(Bool on)
{
  if (cMacControl) {
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
  wxPanel *parent;
  parent = (wxPanel *) GetParent();
  if (parent) {
    // parent is not always a wxPanel: can be a wxMenu...
    if (wxSubType(parent->__type,wxTYPE_PANEL))
      parent->OnChar(event);
  }
}


char *wxItemStripLabel(char *label)
{
  int i, j;
  char *naya;

  if (!label)
    return NULL;
  
  for (i = 0; label[i]; i++) {
    if (label[i] == '&') {
      /* Strip it: */
      naya = new WXGC_ATOMIC char[strlen(label) + 1];
      j = 0;
      for (i = 0; label[i]; i++) {
        if (label[i] == '&') {
          if (label[i + 1]) {
            naya[j++] = label[i + 1];
            i++;
          }
        } else
          naya[j++] = label[i];
      }
      naya[j] = 0;
      
      return naya;
    }
  }
  
  return label;
}


//-----------------------------------------------------------------------------

void wxItem::MaybeMoveControls()
{
  if (cMacControl) {
    int x, y;
    GetWinOrigin(&x, &y);
    MoveControl(cMacControl, x + padLeft, y + padTop);
  }

  wxWindow::MaybeMoveControls();
}

//-----------------------------------------------------------------------------
static ControlRef test_control;
static WindowRef test_win;

Bool wxItem::AcceptsExplicitFocus()
{
  if (WantsFocus())
    return TRUE;

  /* Create a hidden window/control to check wherg Full Keyboard Access
     has been enabled by the user: */
  if (!test_control && !test_win) {
    OSErr result;
    Rect r = { 0 , 0, 100, 100 };

    result = ::CreateNewWindow(kDocumentWindowClass, kWindowStandardDocumentAttributes, &r, &test_win);
    if (result == noErr) {
      Rect r2 = { 10 , 10, 90, 90 };
      result = ::CreatePushButtonControl(test_win, &r2, CFSTR("Ok"), &test_control);
      if (result == noErr) {
	/* We have a test control for checking focus, now */
      } else {
	::DisposeWindow(test_win);
	test_control = NULL;
      }
    }
  }

  if (test_control) {
    /* Check for Full Keyboard Access by trying to set the focus: */
    ControlRef c;
    ::ClearKeyboardFocus(test_win);
    ::SetKeyboardFocus(test_win, test_control, kControlFocusNextPart);
    ::GetKeyboardFocus(test_win, &c);
    return (c == test_control);
  } else
    return FALSE;
}

//-----------------------------------------------------------------------------
void wxItem::OnSetFocus()
{
  if (cMacControl) {
    ::ClearKeyboardFocus(GetWindowFromPort(cMacDC->macGrafPort()));
    ::SetKeyboardFocus(GetWindowFromPort(cMacDC->macGrafPort()),
		       cMacControl,
		       kControlFocusNextPart);
  }
}

void wxItem::OnKillFocus()
{
  ::ClearKeyboardFocus(GetWindowFromPort(cMacDC->macGrafPort()));
}

static OSStatus myEventHandler(EventHandlerCallRef inHandlerCallRef, 
			       EventRef inEvent, 
			       void *inUserData)
{
  if (wx_propagate_key) {
    return eventNotHandledErr;
  } else {
    wx_ignore_key = TRUE;
    return noErr;
  }
}

void wxItem::IgnoreKeyboardEvents()
{
  if (cMacControl) {
    EventHandlerRef ref;
    EventTypeSpec evts[3];

    evts[0].eventClass = kEventClassKeyboard;
    evts[0].eventKind = kEventRawKeyDown;
    evts[1].eventClass = kEventClassKeyboard;
    evts[1].eventKind = kEventRawKeyRepeat;
    evts[2].eventClass = kEventClassKeyboard;
    evts[2].eventKind = kEventRawKeyUp;

    ::InstallEventHandler(GetControlEventTarget(cMacControl), 
			  myEventHandler,
			  3,
			  evts,
			  NULL,
			  &ref);
  }
}
