///////////////////////////////////////////////////////////////////////////////
// File:	wx_frame.cc
// Purpose:	wxFrame implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

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
#ifndef OS_X
  #include <QuickDraw.h>
  #include <TextEdit.h>
  #include <Menus.h>
  #include <Windows.h>
#endif

extern wxScreen *theScreen;
static wxMenuBar *empty_menu_bar, *close_menu_bar;

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
	if (windowTitle) wxMacCtoPString(windowTitle, theWindowTitle);
	const Bool WindowIsVisible = TRUE;

	cUserHidden = TRUE;

	WindowPtr theMacWindow;

	/* Make sure we have the right device: */
    CGrafPtr wPort;
    GDHandle gdh;
    GetGWorld(&wPort,&gdh);
    SetGWorld(wPort, wxGetGDHandle());
	
    OSErr result;

#if (__powerc) || defined(__ppc__)

	if (wxTheApp->MacOS85WindowManagerPresent) {
			
		// OS 8.5 window creation enabled
		// (so we can get resizeable modal windows)
		
		WindowClass windowClass;
		WindowAttributes windowAttributes;
		
		if (cStyle & wxMDI_CHILD) { // hack : MDI_CHILD means dialog box
			windowClass = kMovableModalWindowClass;
			if (cStyle & wxNO_RESIZE_BORDER) {
				windowAttributes = kWindowNoAttributes;
			} else {
				cIsResizableDialog = TRUE;
				windowAttributes = (kWindowResizableAttribute);
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
		
	} else {
#endif
		// OS 8.5 not enabled, go with old-style window creation
		
		int theProcID;
	 	// GRW adds
	 	if (cStyle & wxMDI_CHILD) // hack: MDI_CHILD means dialog box
	 		theProcID = movableDBoxProc;
	  	else if (cStyle & wxNO_RESIZE_BORDER)
	 		theProcID = noGrowDocProc;
	    else
	 		theProcID = zoomDocProc;
	
		const WindowPtr MoveToFront = WindowPtr(-1L);
		const Bool HasGoAwayBox = TRUE;
		long theRefCon = (long)this;
		theMacWindow = ::NewCWindow(NULL, &theBoundsRect, theWindowTitle,
						!WindowIsVisible, theProcID, MoveToFront, HasGoAwayBox, theRefCon);

#if (__powerc) || defined(__ppc__)

	}
#endif

	CheckMemOK(theMacWindow);
	
	cMacDC = new wxMacDC((GrafPtr)GetWindowPort(theMacWindow));

#ifndef OS_X
 	WStateData **wstatedata = (WStateData**)((WindowPeek)theMacWindow)->dataHandle;
	(*wstatedata)->stdState.right -= 80;
	//(*wstatedata)->stdState.top = GetMBarHeight() + 5;
#endif

  // Calculate the platformArea size
        Rect theStrucRect;
        Rect theContRect;
#ifdef OS_X  
        GetWindowBounds(theMacWindow,kWindowStructureRgn,&theStrucRect);
        GetWindowBounds(theMacWindow,kWindowContentRgn,&theContRect);
#else        
	theStrucRect = wxMacGetStrucRect();
	theContRect = wxMacGetContRect();
#endif        
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
}

//=============================================================================
// Public destructor
//=============================================================================

wxFrame::~wxFrame(void)
{
#if WXGARBAGE_COLLECTION_ON
    if (IsVisible()) 
		Show(FALSE);
#endif

	// Kludge needed here:
	// Bad:  DisposeWindow deletes the mac controls, but not their wxItems.
	// Good: DestroyChildren deletes the wxItems and their associated mac controls.
	if (cDialogPanel)
		cDialogPanel = NULL;
	DestroyChildren();

#ifdef OS_X
	CWindowPtr theMacWindow;
#else
	WindowPtr theMacWindow;
#endif
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
	cWindowTitle[0] = 0; // kludge

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
 	// tom: to avoid unnecessary calculations!
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

 	int oldWindowX = cWindowX;
 	int oldWindowY = cWindowY;
 	int oldWindowWidth = cWindowWidth;
 	int oldWindowHeight = cWindowHeight;
 
 	if (! wxTheApp->MacOS85WindowManagerPresent) {
		if (width > cWindowWidth || height > cWindowHeight)
	 	{
	 		// Invalidate grow box:
	 		int oldMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	 		int oldMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	 		Rect oldGrowRect = {oldMacHeight - 15, oldMacWidth - 15, oldMacHeight, oldMacWidth};
	 		SetCurrentMacDC();
	 		InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&oldGrowRect);
	 		::EraseRect(&oldGrowRect); // MATTHEW: [5]
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
	 		SetCurrentMacDC();
	 		if (dw) {
	 			r.top = 0;
	 			r.bottom = h;
	 			r.left = max(0, w - dw);
	 			r.right = w;
		 		::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&r);
	 		}
	 		if (dh) {
	 			r.top = max(0, h - dh);
	 			r.bottom = h;
	 			r.left = 0;
	 			r.right = w;
		 		::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&r);
	 		}
	 	}
 		
 		if (cStatusPanel) {
 		  cStatusPanel->SetCurrentDC();
 		  Rect r = {0, 0, 32000, 32000};
                  OffsetRect(&r,SetOriginX,SetOriginY);
 		  EraseRect(&r);
 		  
 		  cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
 		  	                    theMacWidth, -1);
 		  int w, h;
 		  cStatusPanel->GetClientSize(&w, &h);
 		  cStatusText->SetSize(-1, -1, w, -1);

 		  cStatusPanel->SetCurrentDC();
 		  Rect s = {0, 0, 32000, 32000};
                  OffsetRect(&s,SetOriginX,SetOriginY);
 		  EraseRect(&s);
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

		SetCurrentDC();
        CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
        WindowPtr theMacWindow = GetWindowFromPort(theMacGrafPort);
        Rect portBounds;
		::EraseRect(GetPortBounds(theMacGrafPort,&portBounds));
		::ZoomWindow(theMacWindow, maximize ? inZoomOut : inZoomIn, TRUE);
		InvalWindowRect(theMacWindow,&portBounds);
		cMaximized = maximize;

		wxMacRecalcNewSize();
		
		if (cStatusPanel) {
 		  int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
		  int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
 		  cStatusPanel->SetCurrentDC();
		  Rect r = {0, 0, 32000, 32000};
                  OffsetRect(&r,SetOriginX,SetOriginY);
 		  EraseRect(&r);
 		  
 		  cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
 		  	                    theMacWidth, -1);
 		  int w, h;
 		  cStatusPanel->GetClientSize(&w, &h);
 		  cStatusText->SetSize(-1, -1, w, -1);

 		  cStatusPanel->SetCurrentDC();
                  Rect s = {0, 0, 32000, 32000};
                  OffsetRect(&s,SetOriginX,SetOriginY);
 		  EraseRect(&s);
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
#ifdef OS_X
        GetWindowBounds(GetWindowFromPort(cMacDC->macGrafPort()),kWindowStructureRgn,&theStrucRect);
        GetWindowBounds(GetWindowFromPort(cMacDC->macGrafPort()),kWindowContentRgn,&theContRect);
#else        
	theStrucRect = wxMacGetStrucRect();
	theContRect = wxMacGetContRect();
#endif        
	cWindowX = theStrucRect.left;
	cWindowY = theStrucRect.top - GetMBarHeight(); // WCH: kludge
	if (resize) {
	  cWindowWidth = theStrucRect.right - theStrucRect.left;
	  cWindowHeight = theStrucRect.bottom - theStrucRect.top;
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
	cStatusText->SetWidthHeight(clientWidth - 18, statusLineHeight);
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
			if (0 && cIsModal) {
				if (!empty_menu_bar)
					empty_menu_bar = new wxMenuBar;
				empty_menu_bar->Install();
			} else {
				if (!close_menu_bar) {
					close_menu_bar = new wxMenuBar;
					/* When a frame doesn't have a menubar, doMacInMenuBar
					 assumes that any menulelection is the close item. */
					wxMenu *file = new wxMenu();
					file->Append(1, "Close\tCmd+W");
					close_menu_bar->Append(file, "File");
				}
				close_menu_bar->Install();
			}
		}
	}
}

void wxFrame::ShowAsActive(Bool flag)
{
	if (! wxTheApp->MacOS85WindowManagerPresent) {
		// Invalidate grow box (appearance changes with window active/inactive)
	 	SetCurrentDC();
	 	int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	 	int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	 	Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
	 	// Erase it now if we're becoming inactive
	 	if (!flag)
	 		::EraseRect(&growRect);
	 	::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&growRect);
	 }
 	
 	if (!cFocusWindow && children) {
		wxChildNode *node = children->First();
		while (node) {
			wxWindow *win = (wxWindow *)node->Data();
			if (win->WantsFocus() && win->CanAcceptEvent()) {
				cFocusWindow = win;
				break;
			}
			node = node->Next();
		}
	}

	if (cFocusWindow && this != cFocusWindow)
    {
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

#ifndef OS_X

//-----------------------------------------------------------------------------
void wxFrame::wxMacStartDrawing(GrafPtr * oldPort, GDHandle * oldGD)
{
    ::GetGWorld((CGrafPtr *)oldPort, oldGD);

    CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	::SetGWorld(theMacGrafPort, wxGetGDHandle());

    //Rect portBounds;
	//savePortH = GetPortBounds(theMacGrafPort,&portBounds)->left;
	//savePortV = portBounds.top;
	//::SetOrigin(0, 0);
}

//-----------------------------------------------------------------------------
void wxFrame::wxMacStopDrawing(GrafPtr oldPort, GDHandle oldGD)
{
	//::SetOrigin(savePortH, savePortV);
	::SetGWorld(oldPort, oldGD);
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
// Based on code in 2nd edition of "Macintosh Programming Secrets"
// by Scott Knaster and Keith Rollin.
//-----------------------------------------------------------------------------
Rect wxFrame::wxMacGetContRect(void)
{// express client area in screen window c.s.
	GrafPtr oldPort; GDHandle oldGD;
	wxMacStartDrawing(&oldPort, &oldGD);
	Rect theContRect;
        GetPortBounds(cMacDC->macGrafPort(),&theContRect); // client c.s.
	Point topLeftPt = {0, 0};
	::LocalToGlobal (&topLeftPt);
	::OffsetRect(&theContRect, topLeftPt.h, topLeftPt.v); // screen window c.s.
	wxMacStopDrawing(oldPort, oldGD);

	return theContRect; // screen window c.s.
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
// Based on code in 2nd edition of "Macintosh Programming Secrets"
// by Scott Knaster and Keith Rollin.
//-----------------------------------------------------------------------------
Rect wxFrame::wxMacGetStrucRect(void)
{// express window area in screen window c.s.
	Rect theStrucRect;

	GrafPtr oldPort; GDHandle oldGD;
	wxMacStartDrawing(oldPort, oldGD);

	WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());

	if (IsWindowVisible(theMacWindow))
	{
        RgnHandle strucRgn = NewRgn();
        GetWindowRegion(theMacWindow,kWindowStructureRgn,strucRgn);
		GetRegionBounds(strucRgn,&theStrucRect); // screen window c.s.
	}
	else
	{
		const int kOffScreenLocation = 0x4000;
	
		Rect theClientRect;
                GetPortBounds(cMacDC->macGrafPort(),&theClientRect); // client c.s.
		Point thePosition = {theClientRect.top, theClientRect.left}; // client c.s.
		::LocalToGlobal(&thePosition); // screen window c.s.
		::MoveWindow(theMacWindow, thePosition.h, kOffScreenLocation, FALSE);
		::ShowHide(theMacWindow, TRUE);
                RgnHandle strucRgn = NewRgn();
                GetWindowRegion(theMacWindow,kWindowStructureRgn,strucRgn);
                GetRegionBounds(strucRgn,&theStrucRect);
		::ShowHide(theMacWindow, FALSE);
		::MoveWindow(theMacWindow, thePosition.h, thePosition.v, FALSE);
		::OffsetRect(&theStrucRect, 0, thePosition.v - kOffScreenLocation); // screen window c.s.
	}

	wxMacStopDrawing(oldPort, oldGD, savePortH, savePortV);

	return theStrucRect; // screen window c.s.
}

#endif // OS_X

//-----------------------------------------------------------------------------
void wxFrame::MacUpdateWindow(void)
{
	WindowPtr theMacWindow = macWindow();
	if (theMacWindow)
	{
		SetCurrentDC();
		::BeginUpdate(theMacWindow);
                RgnHandle visibleRgn = NewRgn();
		if (!::EmptyRgn(GetPortVisibleRegion(GetWindowPort(theMacWindow),visibleRgn)))
		{
 			// Erase update region
 			// ::EraseRect(&theMacWindow->portRect);

 			// Can't use UpdateControls since each control has it's own coordinate system
 			//		::UpdateControls(theMacWindow, theMacWindow->visRgn);

			Paint();

 			// Draw the grow box
 			if (!(cStyle & wxNO_RESIZE_BORDER))
 				MacDrawGrowIcon();
		}
		::EndUpdate(theMacWindow);
	}
}

 //-----------------------------------------------------------------------------
 void wxFrame::MacDrawGrowIcon(void)
 {
#if (__powerc) || defined(__ppc__)
 	if (! wxTheApp->MacOS85WindowManagerPresent) {
#endif
	 	SetCurrentMacDCNoMargin();
	 	// Save the clipping region
	 	RgnHandle saveClip = NewRgn();
	 	CheckMemOK(saveClip);
		::GetClip(saveClip);
	 	// Compute the bounding rect of the grow icon
	 	int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	 	int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	 	Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
	 	// Avoid drawing scrollbar outlines
	 	::ClipRect(&growRect);
	 	// Draw it
	 	WindowPtr theMacWindow = macWindow();
	 	RGBColor fore, back;
	 	::GetForeColor(&fore);
	 	::GetBackColor(&back);
	 	::ForeColor(blackColor);
	 	::BackColor(whiteColor);
	 	::EraseRect(&growRect);
	 	::DrawGrowIcon(theMacWindow);
	 	// Restore the clipping region
	 	::SetClip(saveClip);
	 	::DisposeRgn(saveClip);
	 	::RGBForeColor(&fore);
	 	::RGBBackColor(&back);
#if (__powerc) || defined(__ppc__)
	 }
#endif
 }

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
char* wxFrame::GetTitle(void) // WCH: return type should be "const char*"
{
	Str255		theTitle;

	WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
	::GetWTitle(theMacWindow, theTitle);
	wxMacPtoCString(theTitle, cWindowTitle);
	return cWindowTitle;
}

//-----------------------------------------------------------------------------
void wxFrame::SetTitle(char* title)
{
	wxMacString theMacString = title;
	WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
	::SetWTitle(theMacWindow, theMacString());
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

#if 0
	if (!show && IsModal())
		MakeModal(FALSE);
#endif

	WindowPtr theMacWindow = GetWindowFromPort(cMacDC->macGrafPort());
	if (show) {
#if __WXGARBAGE_COLLECTION_ON
	   wxTopLevelWindows(ContextWindow())->Append(this);
#else
		wxTopLevelWindows(ContextWindow())->Show(this, show);
#endif
		::ShowWindow(theMacWindow);
 		::SelectWindow(theMacWindow); 
 		
 		if (cMacDC->currentUser() == this)
			/* b/c may be optimized for hidden: */
			cMacDC->setCurrentUser(NULL);
	} else {
#if __WXGARBAGE_COLLECTION_ON
        wxTopLevelWindows(ContextWindow())->DeleteObject(this);
#else
		wxTopLevelWindows(ContextWindow())->Show(this, show);
#endif
		if (cFocusWindow) {
		  cFocusWindow->OnKillFocus();
		  cFocusWindow = NULL;
		}
		::HideWindow(theMacWindow);
    }
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
    if (window != cFocusWindow)
    {
	    if (cFocusWindow) cFocusWindow->OnKillFocus();
	    cFocusWindow = window;
	    if (window) window->OnSetFocus();
    }
}

//-----------------------------------------------------------------------------
void wxFrame::OnActivate(Bool active)
{
	wxWindow::OnActivate(active);
}

//-----------------------------------------------------------------------------
void wxFrame::LoadAccelerators(char* table) { } // Not Applicable for Mac platform

//-----------------------------------------------------------------------------
void wxFrame::Paint(void)
{
	SetCurrentDC();
	RgnHandle rgn, subrgn, borderRgn = NULL;
	if (rgn = NewRgn()) {
		if (subrgn = NewRgn()) {
		  if ((cStyle & wxNO_RESIZE_BORDER) || (borderRgn = NewRgn())) {
		    if (borderRgn) {
                      int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	              int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
                      Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
                      RectRgn(borderRgn, &growRect);
		    }
		    SetRectRgn(rgn, 0, 0, cWindowWidth, cWindowHeight + 1);
                    AddWhiteRgn(subrgn);
                    DiffRgn(rgn, subrgn, rgn);
                    if (borderRgn)
                      DiffRgn(rgn, borderRgn, rgn);
                    EraseRgn(rgn);
                    RGBColor save;
                    GetForeColor(&save);
                    ForeColor(whiteColor);
                    if (borderRgn)
                      DiffRgn(subrgn, borderRgn, subrgn);
                    PaintRgn(subrgn);
                    RGBForeColor(&save);
                    if (borderRgn)
                      DisposeRgn(borderRgn);
		  }
		  DisposeRgn(subrgn);
		}
		DisposeRgn(rgn);
	}
	wxWindow::Paint();
	if (cStatusPanel) {
	  int statusLineHeight = cStatusPanel->Height();

	  int w, h;
	  cStatusPanel->GetSize(&w, &h);

	  cStatusPanel->SetCurrentDC();
      Rect r = { -1, 0, h + 1, w };
      
      ClipRect(&r); /* hack! */
      
	  EraseRect(&r);
	  cStatusPanel->Paint();
    }
 	// SetCurrentDC();
 	// If the right type of Window: MacDrawGrowIcon();
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

void wxFrame::ChangeToGray(Bool gray)
{
  ChildrenInternalGray(gray);
  wxWindow::ChangeToGray(gray);
}
