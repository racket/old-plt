///////////////////////////////////////////////////////////////////////////////
// File:	wx_frame.cc
// Purpose:	wxFrame implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_gdi.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_txt.h"
#include "wx_area.h"
#include "wx_screen.h"
#include "wx_mac_utils.h"
#include "wx_main.h"
#include "wx_messg.h"
#include "wx_utils.h"
#ifndef WX_CARBON
#include <QuickDraw.h>
#include <TextEdit.h>
#include <Menus.h>
#include <Windows.h>
#endif

static wxMenuBar *close_menu_bar;

//=============================================================================
// Public constructors
//=============================================================================


//-----------------------------------------------------------------------------
wxFrame::wxFrame // Constructor (for frame window)
(
 wxFrame*	parentFrame,		// this is ignored
 char*		windowTitle,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbFrame (windowName, wxScreen::gScreenWindow,
	   x, y, 
	   (width < 30) ? 30 : width, 
	   (height < 40) ? 40 : height, style)
{
  InitDefaults();

  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  int X = cWindowX;
  int Y = cWindowY;

  Rect theBoundsRect;
  ::SetRect(&theBoundsRect, X, Y, X + cWindowWidth, Y + cWindowHeight);
  Str255 theWindowTitle = "\p";
  if (windowTitle) CopyCStringToPascal(windowTitle, theWindowTitle);

  cUserHidden = TRUE;

  if (parentFrame)
    if (wxSubType(parentFrame->__type, wxTYPE_DIALOG_BOX))
      parentFrame = (wxFrame *)parentFrame->GetParent();

  WindowPtr theMacWindow;

  /* Make sure we have the right device: */
  SetGWorld(wxGetGrafPtr(), wxGetGDHandle());
  
  OSErr result;
  WindowClass windowClass;
  WindowAttributes windowAttributes;
  
  if (cStyle & wxMDI_CHILD) { // hack : MDI_CHILD means dialog box
#ifdef OS_X
    if (parentFrame) {
      WXGC_IGNORE(this, cSheetParent);
      cSheetParent = parentFrame;
      windowClass = kSheetWindowClass;
    } else
#endif
      windowClass = kDocumentWindowClass;  /* kMovableModalWindowClass => OS X does modality, which we don't want */
    if (cStyle & wxNO_RESIZE_BORDER) {
      windowAttributes = kWindowNoAttributes;
    } else {
      cIsResizableDialog = TRUE;
      windowAttributes = kWindowResizableAttribute;
    }
  } else {
    windowClass = kDocumentWindowClass;
    if (cStyle & wxNO_RESIZE_BORDER) {
      windowAttributes = kWindowStandardFloatingAttributes;
    } else {
      windowAttributes = kWindowStandardDocumentAttributes;
    }
  }

  result = ::CreateNewWindow(windowClass, windowAttributes, &theBoundsRect, &theMacWindow);  
  
  if (result != noErr) {
    char error[256];
    sprintf(error,"wxFrameConstructor: Attempt to create window failed with error: %d.",
	    result);
    wxFatalError(error);
  }

  ::SetWTitle(theMacWindow, theWindowTitle);
  
  SetWRefCon(theMacWindow, (long)this);
  
  CheckMemOK(theMacWindow);
  
  cMacDC = new wxMacDC(GetWindowPort(theMacWindow));

  // Calculate the platformArea size
  Rect theStrucRect;
  Rect theContRect;
  GetWindowBounds(theMacWindow,kWindowStructureRgn,&theStrucRect);
  GetWindowBounds(theMacWindow,kWindowContentRgn,&theContRect);
  wxMargin platformMargin;
  platformMargin.SetMargin(theContRect.left - theStrucRect.left, Direction::wxLeft);
  platformMargin.SetMargin(theContRect.top - theStrucRect.top, Direction::wxTop);
  platformMargin.SetMargin(theStrucRect.right - theContRect.right, Direction::wxRight);
  platformMargin.SetMargin(theStrucRect.bottom - theContRect.bottom, Direction::wxBottom);
  PlatformArea()->SetMargin(platformMargin);

  // The client has the requested window position, not the window: must move
  SetCurrentDC();
  wxMargin contentAreaMargin = ContentArea()->Margin(wxScreen::gScreenWindow);
  int theMacX = contentAreaMargin.Offset(Direction::wxLeft);
  int theMacY = contentAreaMargin.Offset(Direction::wxTop);
  MoveWindow(theMacWindow, theMacX, theMacY, FALSE);

  // The client has the requested window size, not the window: must resize
  int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
  int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
  SizeWindow(theMacWindow, theMacWidth, theMacHeight, FALSE);
  
  wx_cursor = wxSTANDARD_CURSOR;
  
  if (wxIsBusy())
    cBusyCursor = 1;

  // create a root control, to enable control embedding
  ControlRef rootControl;
  ::CreateRootControl(theMacWindow,&rootControl);
  cMacControl = rootControl;
}

//=============================================================================
// Public destructor
//=============================================================================

wxFrame::~wxFrame(void)
{
  if (IsVisible()) 
    Show(FALSE);

  // Kludge needed here:
  // Bad:  DisposeWindow deletes the mac controls, but not their wxItems.
  // Good: DestroyChildren deletes the wxItems and their associated mac controls.
  if (cDialogPanel)
    cDialogPanel = NULL;
  DestroyChildren();

  CWindowPtr theMacWindow;
  theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  ::DisposeWindow(theMacWindow);
  delete cMacDC;
  if (wx_menu_bar)
    delete wx_menu_bar;
  if (cControlArea)
    delete cControlArea;
  if (cContentArea)
    delete cContentArea;
  if (cPlatformArea)
    delete cPlatformArea;
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxFrame::InitDefaults(void)
{
  cMaximized = FALSE;
  cIsModal = FALSE;
  cBusyCursor = 0;
  cFocusWindow = NULL;
  cStatusPanel = NULL;
  cStatusText = NULL;
  cDialogPanel = NULL;
  cControlArea = new wxArea(this);
  cContentArea = new wxArea(this);
  cPlatformArea = new wxArea(this);
  cIsResizableDialog = FALSE;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Geometry methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea* wxFrame::PlatformArea(void) { return cPlatformArea; } // mac platform only

//-----------------------------------------------------------------------------
wxArea* wxFrame::ContentArea(void) { return cContentArea; } // mac platform only

//-----------------------------------------------------------------------------
wxArea* wxFrame::ControlArea(void) { return cControlArea; } // mac platform only

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define max(x, y) ((x > y) ? x : y)

//-----------------------------------------------------------------------------
// Note: Can't set width < width of borders + etc.
// Note: Can't set height < height of borders + statusLine + etc.
// Note: Mac platform can't set width = width of borders + etc.
// Note: Mac platform can't set height = height of borders + statusLine + etc.
//-----------------------------------------------------------------------------
void wxFrame::DoSetSize(int x, int y, int width, int height)
{
  if (x==-1) 
    x= cWindowX;
  if (y==-1) 
    y = cWindowY;
  if (width==-1) 
    width = cWindowWidth;
  if (height==-1) 
    height = cWindowHeight;
  
  Bool xIsChanged = (x != cWindowX);
  Bool yIsChanged = (y != cWindowY);
  Bool widthIsChanged = (width != cWindowWidth);
  Bool heightIsChanged = (height != cWindowHeight);

  if (! wxTheApp->MacOS85WindowManagerPresent) {
    if (width > cWindowWidth || height > cWindowHeight)
      {
	// Invalidate grow box:
	int oldMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	int oldMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	Rect oldGrowRect = {oldMacHeight - 15, oldMacWidth - 15, oldMacHeight, oldMacWidth};
	if (SetCurrentMacDC()) {
	  InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&oldGrowRect);
	  ::EraseRect(&oldGrowRect);
	}
      }
  }

  int dw, dh;

  dw = width - cWindowWidth;
  dh = height - cWindowHeight;

  if (xIsChanged) cWindowX = x;
  if (yIsChanged) cWindowY = y;
  if (widthIsChanged) cWindowWidth = width;
  if (heightIsChanged) cWindowHeight = height;

  SetCurrentDC();
  WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());

  if (xIsChanged || yIsChanged)
    {
      wxMargin contentAreaMargin = ContentArea()->Margin(wxScreen::gScreenWindow);
      int theMacX = contentAreaMargin.Offset(Direction::wxLeft);
      int theMacY = contentAreaMargin.Offset(Direction::wxTop);
      ::MoveWindow(theMacWindow, theMacX, theMacY, FALSE);
    }

  if (widthIsChanged || heightIsChanged)
    {
      int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
      int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
      ::SizeWindow(theMacWindow, theMacWidth, theMacHeight, TRUE);
      // Resizing puts windows into the unzoomed state
      cMaximized = FALSE;

      if (dw > 0 || dh > 0) {
	// Invalidate new region:
	int w = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	int h = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	Rect r;
	if (SetCurrentMacDC()) {
	  if (dw) {
	    r.top = 0;
	    r.bottom = h;
	    r.left = max(0, w - dw);
	    r.right = w;
	    ::InvalWindowRect(theMacWindow, &r);
	  }
	  if (dh) {
	    r.top = max(0, h - dh);
	    r.bottom = h;
	    r.left = 0;
	    r.right = w;
	    ::InvalWindowRect(theMacWindow, &r);
	  }
	}
      }
      
      if (cStatusPanel) {
	if (cStatusPanel->SetCurrentDC()) {
	  Rect r = {0, 0, 10000, 10000};
	  OffsetRect(&r,SetOriginX,SetOriginY);
	  EraseRect(&r);
	}
	
	cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
			      theMacWidth, -1);
	int w, h;
	cStatusPanel->GetClientSize(&w, &h);
	cStatusText->SetSize(-1, -1, w, -1);
	
	if (cStatusPanel->SetCurrentDC()) {
	  Rect s = {0, 0, 10000, 10000};
	  OffsetRect(&s,SetOriginX,SetOriginY);
	  EraseRect(&s);
	}
      }
      
      // Call OnSize handler
      OnSize(width, height);
    }
}

//-----------------------------------------------------------------------------
void wxFrame::Maximize(Bool maximize)
{
  if (cMaximized != maximize)
    {
      int oldWindowX = cWindowX;
      int oldWindowY = cWindowY;
      int oldWindowWidth = cWindowWidth;
      int oldWindowHeight = cWindowHeight;

      CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
      WindowPtr theMacWindow = GetWindowFromPort(theMacGrafPort);
      if (SetCurrentDC()) {
	Rect portBounds;
	::EraseRect(GetPortBounds(theMacGrafPort,&portBounds));
	::ZoomWindow(theMacWindow, maximize ? inZoomOut : inZoomIn, TRUE);
	InvalWindowRect(theMacWindow,&portBounds);
      } else
	::ZoomWindow(theMacWindow, maximize ? inZoomOut : inZoomIn, TRUE);

      cMaximized = maximize;

      wxMacRecalcNewSize();
      
      if (cStatusPanel) {
	int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	if (cStatusPanel->SetCurrentDC()) {
	  Rect r = {0, 0, 10000, 10000};
	  OffsetRect(&r,SetOriginX,SetOriginY);
	  EraseRect(&r);
	}
	
	cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
			      theMacWidth, -1);
	int w, h;
	cStatusPanel->GetClientSize(&w, &h);
	cStatusText->SetSize(-1, -1, w, -1);

	if (cStatusPanel->SetCurrentDC()) {
	  Rect s = {0, 0, 10000, 10000};
	  OffsetRect(&s,SetOriginX,SetOriginY);
	  EraseRect(&s);
	}
      }
      
      int dW = cWindowWidth - oldWindowWidth;
      int dH = cWindowHeight - oldWindowHeight;
      int dX = cWindowX - oldWindowX;
      int dY = cWindowY - oldWindowY;
      OnWindowDSize(dW, dH, dX, dY);
    }
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
//-----------------------------------------------------------------------------
void wxFrame::wxMacRecalcNewSize(Bool resize)
{
  Rect theStrucRect;
  Rect theContRect;
  GetWindowBounds(GetWindowFromPort(cMacDC->macGrafPort()),kWindowStructureRgn,&theStrucRect);
  GetWindowBounds(GetWindowFromPort(cMacDC->macGrafPort()),kWindowContentRgn,&theContRect);
  cWindowX = theStrucRect.left;
  cWindowY = theStrucRect.top - GetMBarHeight(); // WCH: kludge
  if (resize) {
    cWindowWidth = theStrucRect.right - theStrucRect.left;
    cWindowHeight = theStrucRect.bottom - theStrucRect.top;
  }

  if (sheets) {
    wxChildNode* childWindowNode = sheets->First();
    while (childWindowNode) {
      wxFrame* sheet = (wxFrame*)childWindowNode->Data();
      if (sheet) {
	sheet->wxMacRecalcNewSize(FALSE);
	childWindowNode = childWindowNode->Next();
      }
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Status line methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxFrame::CreateStatusLine(int number, char* name)
{
  if (status_line_exists) return;

  nb_status = number;
  status_line_exists = TRUE;
  cStatusPanel = new wxPanel(ControlArea());
  cStatusText = new wxMessage(cStatusPanel, "");
  cStatusPanel->SetEraser(cEraser);
  cStatusText->SetEraser(cEraser);
  cStatusText->SetFont(wxNORMAL_FONT);
  int statusLineHeight = (int)(cStatusText->GetCharHeight() * nb_status);
  int clientWidth, clientHeight;
  GetClientSize(&clientWidth, &clientHeight);
  cStatusText->SetWidthHeight(clientWidth, statusLineHeight);
  cStatusPanel->Fit();
#if 1
  // it is a hack to put the line down..
  cStatusPanel->SetSize(0, clientHeight - cStatusPanel->Height(),
			cStatusPanel->Width(), cStatusPanel->Height()); // tom!!
  // tom: here the Statuspanel is placed over the controlArea!!
  cControlArea->SetMargin(cStatusPanel->Height() + 1, Direction::wxBottom);
#else
  cStatusPanel->SetSize(0, 0, cStatusPanel->Width(), cStatusPanel->Height());
  cControlArea->SetMargin(cStatusPanel->Height() + 1, Direction::wxTop);
#endif
  // cStatusPanel->SetJustify(Direction::wxLeft);

  wxMargin clientAreaMargin = ClientArea()->Margin(wxScreen::gScreenWindow);
  clientAreaMargin.SetMargin(clientAreaMargin.Offset(Direction::wxBottom) 
			     + statusLineHeight - 1, 
			     Direction::wxBottom);
  ClientArea()->SetMargin(clientAreaMargin);
  OnSize(cWindowWidth, cWindowHeight);
}

//-----------------------------------------------------------------------------
void wxFrame::SetStatusText(char* text, int number)
{
  if (!status_line_exists) 
    return;
  cStatusText->SetLabel(text);
}

// tom: perhaps this could be done otherwise:
//      get statusline from Frame
//		do statusline->SetEraser
void wxFrame::SetStatusEraser(wxBrush* b)
{
  if (!status_line_exists) 
    return;
  cStatusPanel->SetEraser(b);
  cStatusText->SetEraser(b);
  
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Menubar methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxFrame::SetMenuBar(wxMenuBar* menu_bar)
{
  // mflatt: if this menu bar is already in use, give up
  if (menu_bar && menu_bar->menu_bar_frame)
    return;

  WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  wxMenuBar* oldMenuBar = wx_menu_bar;
  if (oldMenuBar)
    {
      wx_menu_bar = NULL;
      oldMenuBar->menu_bar_frame = NULL;
      // must correct each menu->menu_bar and menuItem->menuBar
    }

  menu_bar->handle = NULL; // menu_bar->handle is not used
  if (menu_bar) menu_bar->menu_bar_frame = this;
  wx_menu_bar = menu_bar;

  if (theMacWindow == ::FrontWindow())
    {
      NowFront(TRUE);
    }
}

//-----------------------------------------------------------------------------
void wxFrame::Command(int id)
{ // Call this to simulate a menu command
  wxMenuBar* menuBar = GetMenuBar();
  if (menuBar)
    {
      wxMenuItem* item = menuBar->FindItemForId(id);
      if (item)
	{
	  wxMenu* theParentMenu = item->ParentMenu();
	  if (theParentMenu)
	    {
	      HiliteMenu(theParentMenu->cMacMenuId); // hilite the menu
	      ProcessCommand(id);
	    }
	}
    }
}

void wxFrame::OnMenuClick()
     /* Called when the user clicks on the menu bar */
{
  /* Do nothing; MrEd overrides it. */
}

//-----------------------------------------------------------------------------
void wxFrame::ProcessCommand(int id)
{
  OnMenuCommand(id);
  HiliteMenu(0); // unhilite the hilited menu
}

//-----------------------------------------------------------------------------
void wxFrame::NowFront(Bool flag) // mac platform only
{
  // Show the menuBar for this frame on becoming active
  if (flag)
    {
      wxWindow::gMouseWindow = NULL; // If the frame changes, force capture off
      
      if (wx_menu_bar)
	wx_menu_bar->Install();
      else {
	if (!close_menu_bar) {
	  wxREGGLOB(close_menu_bar);
	  close_menu_bar = new wxMenuBar;
#ifndef OS_X
	  /* When a frame doesn't have a menubar, doMacInMenuBar
	     assumes that any menulelection is the close item. */
	  wxMenu *file = new wxMenu();
	  file->Append(1, "Close\tCmd+W");
	  close_menu_bar->Append(file, "File");
#endif
	}
	close_menu_bar->Install();
      }
    }
}

void wxFrame::ShowAsActive(Bool flag)
{
  if (! wxTheApp->MacOS85WindowManagerPresent) {
    // Invalidate grow box (appearance changes with window active/inactive)
    if (SetCurrentDC()) {
      int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
      int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
      Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
      // Erase it now if we're becoming inactive
      if (!flag)
	::EraseRect(&growRect);
      ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&growRect);
    }
  }
  
  if (flag && !cFocusWindow && children) {
    wxChildNode *node = children->First();
    while (node) {
      wxWindow *win = (wxWindow *)(node->Data());
      if (win->WantsFocus() && win->CanAcceptEvent()) {
	cFocusWindow = win;
	break;
      }
      node = node->Next();
    }
  }

  if (cFocusWindow) {
    if (flag)
      cFocusWindow->OnSetFocus();
    else
      cFocusWindow->OnKillFocus();
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Icon methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxFrame::SetIcon(wxBitmap* wx_icon, wxBitmap* mask, int kind) { }		// not implemented

void wxFrame::Iconize(Bool iconize) { }			// not implemented

Bool wxFrame::Iconized(void) { return FALSE; } 	// not implemented

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Platform methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
WindowPtr wxFrame::macWindow(void)
{
  return cMacDC ? GetWindowFromPort(cMacDC->macGrafPort()) : NULL;
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsMacWindow(void) { return TRUE; }

//-----------------------------------------------------------------------------
Bool wxFrame::IsVisible(void)
{
  if (cMacDC)
    return IsWindowVisible(GetWindowFromPort(cMacDC->macGrafPort()));
  else
    return FALSE;
}

//-----------------------------------------------------------------------------
void wxFrame::MacUpdateWindow(void)
{
  WindowPtr theMacWindow = macWindow();
  if (theMacWindow && SetCurrentDC()) {
    ::BeginUpdate(theMacWindow);

    Paint();

    ::EndUpdate(theMacWindow);
  }
}

//-----------------------------------------------------------------------------
void wxFrame::MacDrawGrowIcon(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
char* wxFrame::GetTitle(void) // WCH: return type should be "const char*"
{
  Str255 theTitle;
  WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  ::GetWTitle(theMacWindow, theTitle);
  return wxP2C(theTitle);
}

//-----------------------------------------------------------------------------
void wxFrame::SetTitle(char* title)
{
  WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  ::SetWTitle(theMacWindow, wxC2P(title));
}

//-----------------------------------------------------------------------------
void wxFrame::Show(Bool show)
{
  if (show == IsVisible()) {
    if (show)
      ::SelectWindow(GetWindowFromPort(cMacDC->macGrafPort()));
    return;
  }
  
  cUserHidden = !show;

  if (window_parent)
    window_parent->GetChildren()->Show(this, show);
  if (cParentArea)
    cParentArea->Windows()->Show(this, show);
  wxTopLevelWindows(ContextWindow())->Show(this, show);

  WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
  if (show) {
#ifdef OS_X
    if (cSheetParent) {
      WindowPtr pwin;
      pwin = GetWindowFromPort(cSheetParent->cMacDC->macGrafPort());
      ::ShowSheetWindow(theMacWindow, pwin);
      if (!cSheetParent->sheets)
	cSheetParent->sheets = new wxChildList();
      if (!cSheetParent->sheets->Number())
	ChangeWindowAttributes(pwin, 0, kWindowCloseBoxAttribute);
      cSheetParent->sheets->Append(this);
      // Showing a sheet picks a place for the sheet:
      wxMacRecalcNewSize(FALSE); // recalc new position only
    } else
#endif
      ::ShowWindow(theMacWindow);
    ::SelectWindow(theMacWindow); 

    if (cMacDC->currentUser() == this)
      /* b/c may be optimized for hidden: */
      cMacDC->setCurrentUser(NULL);
  } else {
    if (cFocusWindow) {
      cFocusWindow->OnKillFocus();
      cFocusWindow = NULL;
    }
#ifdef OS_X
    if (cSheetParent) {
      ::HideSheetWindow(theMacWindow);
      cSheetParent->sheets->DeleteObject(this);
      if (!cSheetParent->sheets->Number()) {
	WindowPtr pwin;
	pwin = GetWindowFromPort(cSheetParent->cMacDC->macGrafPort());
	ChangeWindowAttributes(pwin, kWindowCloseBoxAttribute, 0);
      }
    } else
#endif
      ::HideWindow(theMacWindow);
  }

  /* Paint(); */
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsFrontWindow(void)
{
  WindowPtr theMacWindow = macWindow();
  return (theMacWindow ? theMacWindow == ::FrontWindow() : FALSE);
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsModal(void) { return cIsModal; } //cjc, mflatt

//-----------------------------------------------------------------------------
void wxFrame::MakeModal(Bool modal)
{
  cIsModal = modal;
  // wxbWindow::MakeModal(modal);
}

//-----------------------------------------------------------------------------
wxWindow* wxFrame::GetFocusWindow(void) { return cFocusWindow; }

//-----------------------------------------------------------------------------
void wxFrame::SetFocusWindow(wxWindow* window)
{
  if (window != cFocusWindow) {
    if (cFocusWindow && cActive) cFocusWindow->OnKillFocus();
    cFocusWindow = window;
    if (window && cActive) window->OnSetFocus();
  }
}

//-----------------------------------------------------------------------------

void wxFrame::LoadAccelerators(char* table) { } // Not Applicable for Mac platform

//-----------------------------------------------------------------------------
void wxFrame::Paint(void)
{
  if (SetCurrentDC()) {
    RgnHandle rgn, subrgn;
    rgn = NewRgn();
    if (rgn) {
      subrgn = NewRgn();
      if (subrgn) {
	SetRectRgn(rgn, 0, 0, cWindowWidth, cWindowHeight + 1);
	AddWhiteRgn(subrgn);
	DiffRgn(rgn, subrgn, rgn);
	EraseRgn(rgn);
	RGBColor save;
	GetForeColor(&save);
	ForeColor(whiteColor);
	PaintRgn(subrgn);
	RGBForeColor(&save);
	DisposeRgn(subrgn);
      }
      DisposeRgn(rgn);
    }
    wxWindow::Paint();
    if (cStatusPanel) {
      int w, h;
      cStatusPanel->GetSize(&w, &h);

      cStatusPanel->SetCurrentDC();
      Rect r = { -1, 0, h + 1, w };
      ::OffsetRect(&r,SetOriginX, SetOriginY);
      
      ClipRect(&r); /* hack! */
      
      EraseRect(&r);
      cStatusPanel->Paint();
    }
  }
}

RgnHandle wxFrame::GetCoveredRegion(int x, int y, int w, int h)
{
  if (!(cStyle & wxNO_RESIZE_BORDER)) {
    int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
    int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
    if (((theMacWidth-15 >= x && theMacWidth-15 <= x + w)
	 || (theMacWidth >= x && theMacWidth <= x + w))
	&& (theMacHeight-15 >= y && theMacHeight-15 <= y + h)
	|| (theMacHeight >= y && theMacHeight <= y + h)) {
      RgnHandle rgn = NewRgn();  // this can fail.  use MaxMem to determine validity?       
      if (FALSE) {  //(cIsResizableDialog) {
	OpenRgn();
	MoveTo(theMacHeight, theMacWidth - 15);
	LineTo(theMacHeight, theMacWidth);
	LineTo(theMacHeight - 15, theMacWidth);
	LineTo(theMacHeight, theMacWidth - 15);
	CloseRgn(rgn);
      } else {
	Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
	RectRgn(rgn, &growRect);
      }
      OffsetRgn(rgn, -x, -y);
      return rgn;
    } else
      return NULL;
  } else
    return NULL;
}

//-----------------------------------------------------------------------------
void wxFrame::OnChar(wxKeyEvent *event) // mac platform only
{
  if (cFocusWindow)
    {
      if (cFocusWindow != this) // kludge to prevent infinite loop
	cFocusWindow->OnChar(event);
    }
}

//-----------------------------------------------------------------------------
void wxFrame::Enable(Bool enable)
{
  wxWindow::Enable(enable);
  // Enable/disbale menubar
  if (wx_menu_bar)
    wx_menu_bar->Install();	
}


wxFrame *wxFrame::GetRootFrame()
{
  return this;
}

