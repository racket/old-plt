////////////////////////////////////////////////////////////////////////////////
// File:	wx_app.cc
// Purpose:	wxApp implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
////////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

// #include <stdlib.h>
#ifndef OS_X
#include <Events.h>
#include <AppleEvents.h>
#include <DiskInit.h>
#include <Devices.h>
#include <Resources.h>
#include <Balloons.h>
#include <QuickDraw.h>
#include <Gestalt.h>
#include <Sound.h>
#endif
#include "wx_main.h"	// WCH : should split out stuff for wx_app.h and wb_app.h
#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_stdev.h"
#include "wx_screen.h"
#include "wx_area.h"
#include "wx_timer.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wxTimeScale.h"
#include "wx_print.h"
#if (defined(powerc) || defined(__powerc)) && defined(MPW)
QDGlobals 	qd;
#endif

#include <stdlib.h>

extern void wxDoEvents();
extern void wxDoNextEvent();
extern int wxEventReady();

extern CGrafPtr gMacFontGrafPort;

extern int wxNumHelpItems;

extern Bool doCallPreMouseEvent(wxWindow *in_win, wxWindow *win, wxMouseEvent *evt);

extern wxApp *wxTheApp;
//-----------------------------------------------------------------------------
wxApp::wxApp(wxlanguage_t language):wxbApp(language)
{
  wxREGGLOB(wxTheApp);
  wxTheApp = this;

  OSErr myErr;
  long quickdrawVersion;
  long windowMgrAttributes;
  long dontcare;
  
  if (::Gestalt(gestaltQuickdrawVersion, &quickdrawVersion) != noErr) {
    wxFatalError("Unable to invoke the Gestalt Manager.", "");
  } else {
    if ((quickdrawVersion >> 8) == 0) {
      wxFatalError("Color QuickDraw is required, but not present.", "");
    }
  }
  
  // Version 1.0 of the Appearance Manager appeared in 8.0. We don't support pre-8.0
  // any more.
  myErr = ::Gestalt(gestaltAppearanceAttr, &dontcare);
  if (myErr == gestaltUndefSelectorErr) {
    wxFatalError("Version 1.0 of the Appearance Manager is required.", "");
  }
  
  
  myErr = ::Gestalt(gestaltWindowMgrAttr, &windowMgrAttributes);
  if (myErr == gestaltUndefSelectorErr) {
    MacOS85WindowManagerPresent = FALSE;
  } else if (myErr == noErr) {
    if (windowMgrAttributes & gestaltWindowMgrPresent) {
      MacOS85WindowManagerPresent = TRUE;
    } else {
      MacOS85WindowManagerPresent = FALSE;
    } 
  } else {
    wxFatalError("Unable to invoke the Gestalt Manager.", "");
  }
  
  ::FlushEvents(everyEvent, 0);

  gMacFontGrafPort = CreateNewPort();

  cMacCursorRgn = ::NewRgn(); // forces cursor-move event right away
  CheckMemOK(cMacCursorRgn);
  wxREGGLOB(wxScreen::gScreenWindow);
  wxScreen::gScreenWindow = new wxScreen;
  wxArea* menuArea = (wxScreen::gScreenWindow)->MenuArea();
  int menuBarHeight = GetMBarHeight();
  menuArea->SetMargin(menuBarHeight, Direction::wxTop);

  wx_frame = NULL;
  death_processed = FALSE;
  work_proc = NULL;
  wx_class = NULL;
  wxSetLanguage(language);
  cLastMousePos.v = cLastMousePos.h = -1;
  
}

//-----------------------------------------------------------------------------
wxApp::~wxApp(void)
{
#if defined(DEBUG_NEW) && (DEBUG_NEW >= 2)
  delete wxScreen::gScreenWindow;
  DebugNewReportLeaks();
#endif
}

//-----------------------------------------------------------------------------
Bool wxApp::Initialized(void)
{
  return (wx_frame != NULL);
}

extern void wxSetUpAppleMenu(wxMenuBar *mbar);
extern void wxCheckFinishedSounds(void);

//-----------------------------------------------------------------------------
// Keep trying to process messages until WM_QUIT received
//-----------------------------------------------------------------------------

int wxApp::MainLoop(void)
{
  keep_going = TRUE;
  
  while (1) { wxDoEvents(); }

  return 0;
}


void wxApp::ExitMainLoop(void)
{
  death_processed = TRUE; 
  keep_going = FALSE;
}

//-----------------------------------------------------------------------------
Bool wxApp::Pending(void)
{
  return wxEventReady();
}

//-----------------------------------------------------------------------------
void wxApp::DoIdle(void)
{
  if ((cCurrentEvent.what == nullEvent) && cCurrentEvent.message)
    doMacMouseMotion();
  AdjustCursor();
}

//-----------------------------------------------------------------------------
void wxApp::Dispatch(void)
{
  wxDoNextEvent();
}

static wxFrame *oldFrontWindow = NULL;

void wxRegisterOldFrontWindow();

void wxRegisterOldFrontWindow()
{
  wxREGGLOB(oldFrontWindow);
}

void wxApp::doMacPreEvent()
{
  static Bool noWinMode = FALSE;
  WindowPtr w = ::FrontWindow();

  wxCheckFinishedSounds();

  if (!w && !noWinMode) {
    ::ClearMenuBar();
    wxSetUpAppleMenu(NULL);
    {
      static MenuHandle m = NULL;
      if (!m)
	m = GetMenu(129);
      if (m)
	::InsertMenu(m, 0);
    }
    ::InvalMenuBar();
    wxSetCursor(wxSTANDARD_CURSOR);
    noWinMode = TRUE;
  } else if (w && noWinMode)
    noWinMode = FALSE;

  if (w) {
    wxFrame* macWxFrame = findMacWxFrame(w);
    if (macWxFrame)
      {
	if (oldFrontWindow != macWxFrame) {
	  oldFrontWindow->NowFront(FALSE);
	  macWxFrame->NowFront(TRUE);
	  oldFrontWindow = macWxFrame;
	}
	
	wxWindow* focusWindow = macWxFrame->cFocusWindow;
	if (focusWindow)
	  {
	    focusWindow->DoPeriodicAction();
	  }
      }
  }
}

void wxApp::doMacPostEvent()
{
  DoIdle();
}

void wxApp::doMacDispatch(EventRecord *e)
{
  memcpy(&cCurrentEvent, e, sizeof(EventRecord));

  switch (e->what)
    {
    case mouseDown:
      doMacMouseDown(); break;
    case mouseUp:
      doMacMouseUp(); break;
    case keyDown:
      doMacKeyDown(); break;
    case autoKey:
      doMacAutoKey(); break;
    case keyUp:
      doMacKeyUp(); break;
    case activateEvt:
      doMacActivateEvt(); break;
    case updateEvt:
      doMacUpdateEvt(); break;
    case diskEvt:
      doMacDiskEvt(); break;
    case osEvt:
      doMacOsEvt(); break;
    case kHighLevelEvent:
      doMacHighLevelEvent(); break;
    case 42:
      doMacMouseLeave(); break;
    default:
      break;
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseDown(void)
{
  WindowPtr window;
  short windowPart = FindWindow(cCurrentEvent.where, &window);
  switch (windowPart)
    {
    case inMenuBar:
      {
	long menuResult;
	WindowPtr theMacWindow = ::FrontWindow();

	/* Give the menu bar a chance to build on-demand items: */
	if (theMacWindow) {
	  wxFrame* theMacWxFrame = findMacWxFrame(theMacWindow);
	  if (theMacWxFrame)
	    theMacWxFrame->OnMenuClick();
	}

	menuResult = MenuSelect(cCurrentEvent.where);
	doMacInMenuBar(menuResult, FALSE);
	break;
      }
    case inContent:
      doMacInContent(window); break;
    case inDrag:
      doMacInDrag(window); break;
    case inGrow:
      doMacInGrow(window); break;
    case inGoAway:
      doMacInGoAway(window); break;
    case inZoomIn:
    case inZoomOut:
      doMacInZoom(window, windowPart); break;
    default:
      break;
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseUp(void)
{
  wxWindow* mouseWindow = wxWindow::gMouseWindow;
  if (mouseWindow)
    {
      int hitX = cCurrentEvent.where.h; // screen window c.s.
      int hitY = cCurrentEvent.where.v; // screen window c.s.
      mouseWindow->ScreenToClient(&hitX, &hitY); // mouseWindow client c.s.
      mouseWindow->ClientToLogical(&hitX, &hitY); // mouseWindow logical c.s.
      
      // RightButton is cmdKey click  on the mac platform for one-button mouse
      Bool rightButton = cCurrentEvent.modifiers & cmdKey;
      int type = rightButton ? wxEVENT_TYPE_RIGHT_UP : wxEVENT_TYPE_LEFT_UP;
      
      wxMouseEvent *_theMouseEvent = new wxMouseEvent(type);
      wxMouseEvent &theMouseEvent = *_theMouseEvent;
      theMouseEvent.leftDown = FALSE;
      theMouseEvent.middleDown = FALSE;
      theMouseEvent.rightDown = FALSE;
      theMouseEvent.shiftDown = cCurrentEvent.modifiers & shiftKey;
      theMouseEvent.controlDown = cCurrentEvent.modifiers & controlKey;
      // altKey is optionKey on the mac platform:
      theMouseEvent.altDown = cCurrentEvent.modifiers & optionKey;
      theMouseEvent.metaDown = FALSE;  // mflatt
      theMouseEvent.x = hitX;
      theMouseEvent.y = hitY;
      theMouseEvent.timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt
      
      /* Grab is now only used for grabbing on mouse-down for canvases */
      if (wxSubType(mouseWindow->__type, wxTYPE_CANVAS))
	mouseWindow->ReleaseMouse();
      
      if (!doCallPreMouseEvent(mouseWindow, mouseWindow, &theMouseEvent))
	if (!mouseWindow->IsGray())
	  mouseWindow->OnEvent(&theMouseEvent);
    }
  else
    {
      wxFrame* macWxFrame = findMacWxFrame(::FrontWindow());
      if (macWxFrame)
	{
	  int hitX = cCurrentEvent.where.h; // screen window c.s.
	  int hitY = cCurrentEvent.where.v; // screen window c.s.
	  wxArea* frameParentArea = macWxFrame->ParentArea();
	  frameParentArea->ScreenToArea(&hitX, &hitY);

	  // RightButton is cmdKey click  on the mac platform for one-button mouse
	  Bool rightButton = cCurrentEvent.modifiers & cmdKey;
	  int type = rightButton ? wxEVENT_TYPE_RIGHT_UP : wxEVENT_TYPE_LEFT_UP;
	  
	  wxMouseEvent *_theMouseEvent = new wxMouseEvent(type);
	  wxMouseEvent &theMouseEvent = *_theMouseEvent;
	  theMouseEvent.leftDown = FALSE;
	  theMouseEvent.middleDown = FALSE;
	  theMouseEvent.rightDown = FALSE;
	  theMouseEvent.shiftDown = cCurrentEvent.modifiers & shiftKey;
	  theMouseEvent.controlDown = cCurrentEvent.modifiers & controlKey;
	  // altKey is optionKey on the mac platform:
	  theMouseEvent.altDown = cCurrentEvent.modifiers & optionKey;
	  theMouseEvent.metaDown = FALSE; // mflatt
	  theMouseEvent.x = hitX;
	  theMouseEvent.y = hitY;
	  theMouseEvent.timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt

	  macWxFrame->SeekMouseEventArea(&theMouseEvent);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseMotion(void)
{
  // RightButton is cmdKey click on the mac platform for one-button mouse
  Bool isRightButton = cCurrentEvent.modifiers & cmdKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  Bool isMouseDown = (cCurrentEvent.modifiers & btnState);

  wxMouseEvent *_theMouseEvent = new wxMouseEvent(wxEVENT_TYPE_MOTION);
  wxMouseEvent &theMouseEvent = *_theMouseEvent;
  theMouseEvent.leftDown = isMouseDown && !isRightButton;
  theMouseEvent.middleDown = FALSE;
  theMouseEvent.rightDown = isMouseDown && isRightButton;
  theMouseEvent.shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent.controlDown = cCurrentEvent.modifiers & controlKey;
  theMouseEvent.altDown = isAltKey;
  theMouseEvent.metaDown = FALSE;  // mflatt
  theMouseEvent.timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt
  
  if (wxWindow::gMouseWindow)
    {
      wxWindow* mouseWindow = wxWindow::gMouseWindow;
      int hitX = cCurrentEvent.where.h; // screen window c.s.
      int hitY = cCurrentEvent.where.v; // screen window c.s.
      mouseWindow->ScreenToClient(&hitX, &hitY); // mouseWindow client c.s.
      mouseWindow->ClientToLogical(&hitX, &hitY); // mouseWindow logical c.s.
      theMouseEvent.x = hitX;
      theMouseEvent.y = hitY;
      
      /* Grab is now only used for grabbing on mouse-down for canvases */
      if (wxSubType(mouseWindow->__type, wxTYPE_CANVAS) && !isMouseDown)
	mouseWindow->ReleaseMouse();
      
      if (!doCallPreMouseEvent(mouseWindow, mouseWindow, &theMouseEvent))		  
	if (!mouseWindow->IsGray())
	  mouseWindow->OnEvent(&theMouseEvent);
    }
  else
    {
      wxFrame* macWxFrame = findMacWxFrame(::FrontWindow());
      if (macWxFrame)
	{
	  int hitX = cCurrentEvent.where.h; // screen window c.s.
	  int hitY = cCurrentEvent.where.v; // screen window c.s.
	  wxArea* frameParentArea = macWxFrame->ParentArea();
	  frameParentArea->ScreenToArea(&hitX, &hitY);
	  theMouseEvent.x = hitX; // frame parent area c.s.
	  theMouseEvent.y = hitY; // frame parent area c.s.

	  macWxFrame->SeekMouseEventArea(&theMouseEvent);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseLeave(void)
{
  // RightButton is cmdKey click on the mac platform for one-button mouse
  Bool isRightButton = cCurrentEvent.modifiers & cmdKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  Bool isMouseDown = (cCurrentEvent.modifiers & btnState);

  wxMouseEvent *_theMouseEvent = new wxMouseEvent(wxEVENT_TYPE_LEAVE_WINDOW);
  wxMouseEvent &theMouseEvent = *_theMouseEvent;
  theMouseEvent.leftDown = isMouseDown && !isRightButton;
  theMouseEvent.middleDown = FALSE;
  theMouseEvent.rightDown = isMouseDown && isRightButton;
  theMouseEvent.shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent.controlDown = cCurrentEvent.modifiers & controlKey;
  theMouseEvent.altDown = isAltKey;
  theMouseEvent.metaDown = FALSE;  // mflatt
  theMouseEvent.timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt
  
  wxWindow* win = (wxWindow*)cCurrentEvent.message;
  if (win->IsShown()) {
    theMouseEvent.x = cCurrentEvent.where.h;
    theMouseEvent.y = cCurrentEvent.where.v;

    if (!doCallPreMouseEvent(win, win, &theMouseEvent))
      if (!win->IsGray())
	win->OnEvent(&theMouseEvent);
  }
}

//-----------------------------------------------------------------------------
// mflatt writes:
// Probably, this should be moved into an abstracted function so that
//   doMacKeyUp can call the same code.
// john clements writes:
// Abstraction performed, 2002-01-29.
// Note that wxKeyEvent objects have two extra fields: timeStamp and
//   metaDown. (On the Mac, metaDown is really commandDown.)

static Bool doPreOnChar(wxWindow *in_win, wxWindow *win, wxKeyEvent *evt)
{
  wxWindow *p = win->GetParent();
  return ((p && doPreOnChar(in_win, p, evt)) || win->PreOnChar(in_win, evt));
}

short wxMacDisableMods; /* If a modifier key is here, handle it specially */

void wxApp::doMacKeyUpDown(Bool down)
{

  wxFrame* theMacWxFrame = findMacWxFrame(::FrontWindow());
  
  if (down) {
    if (!theMacWxFrame || theMacWxFrame->CanAcceptEvent())
      if (cCurrentEvent.modifiers & cmdKey) { // is menu command key equivalent ?
	if (cCurrentEvent.what == keyDown) { // ignore autoKey
	  char key = cCurrentEvent.message & charCodeMask;
	  long menuResult = MenuKey(key);
	  if (menuResult) {
	    if (doMacInMenuBar(menuResult, TRUE))
	      return;
	    else
	      HiliteMenu(0);
	  }
	}
      }
  }    
  
  if (!theMacWxFrame || !theMacWxFrame->IsEnable())
    return;	

  static Handle transH = NULL;
  static unsigned long transState = 0;
  static Handle ScriptH = NULL;
  static short region_code = 1;

  if (!ScriptH) { // tom: don't guess the regioncode!!!!
    struct ItlbRecord * r;
    ScriptH = GetResource('itlb',0);
    if (ScriptH) {
      HLock(ScriptH);
      r = (ItlbRecord*)*ScriptH;
      region_code = r->itlbKeys;  	
      HUnlock(ScriptH);
    }	
  }
  
  wxKeyEvent *_theKeyEvent = new wxKeyEvent(wxEVENT_TYPE_CHAR);
  wxKeyEvent &theKeyEvent = *_theKeyEvent;
  theKeyEvent.x = cCurrentEvent.where.h;
  theKeyEvent.y = cCurrentEvent.where.v;
  theKeyEvent.controlDown = Bool(cCurrentEvent.modifiers & controlKey);
  theKeyEvent.shiftDown = Bool(cCurrentEvent.modifiers & shiftKey);
  // altKey is optionKey on the mac platform:
  theKeyEvent.altDown = Bool(cCurrentEvent.modifiers & optionKey);
  theKeyEvent.metaDown = Bool(cCurrentEvent.modifiers & cmdKey);
  theKeyEvent.timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);

  int key = (cCurrentEvent.message & keyCodeMask) >> 8;
  /* Better way than to use hard-wired key codes? */
  switch (key) {
  case 0x7e:
  case 0x3e:
    key = WXK_UP;
    break;
  case 0x7d:
  case 0x3d:
    key = WXK_DOWN;
    break;
  case 0x7b:
  case 0x3b:
    key = WXK_LEFT;
    break;
  case 0x7c:
  case 0x3c:
    key = WXK_RIGHT;
    break;
  case 0x24:
  case 0x4c:
    key = WXK_RETURN;
    break;
  case 0x30:
    key = WXK_TAB;
    break;
  case 0x33:
    key = WXK_BACK;
    break;
  case 0x75:
    key = WXK_DELETE;
    break;
  case 0x73:
    key = WXK_HOME;
    break;
  case 0x77:
    key = WXK_END;
    break;   
  case 0x74:
    key = WXK_PRIOR;
    break;     
  case 0x79:
    key = WXK_NEXT;
    break;     
  default:
    if (!transH) {
      transH = GetResource('KCHR', region_code);
      HNoPurge(transH);
    }
    if (transH && (cCurrentEvent.modifiers & wxMacDisableMods)) {
      /* Remove effect of anything in wxMacDisableMods: */
      int mods = cCurrentEvent.modifiers - (cCurrentEvent.modifiers & wxMacDisableMods);
      HLock(transH);
      key = KeyTranslate(*transH, (key & 0x7F) | mods, &transState) & charCodeMask;
      HUnlock(transH);
    } else 
      key = cCurrentEvent.message & charCodeMask;
  } // end switch

  if (down) {
    theKeyEvent.keyCode = key;
    theKeyEvent.keyUpCode = WXK_PRESS;
  } else {
    theKeyEvent.keyCode = WXK_RELEASE;
    theKeyEvent.keyCode = key;
  }  

  wxWindow *in_win = theMacWxFrame->GetFocusWindow();

  if (!in_win || !doPreOnChar(in_win, in_win, &theKeyEvent))
    if (!theMacWxFrame->IsGray())
      theMacWxFrame->OnChar(&theKeyEvent);
}

void wxApp::doMacKeyDown(void)
{
  doMacKeyUpDown(true);
}

void wxApp::doMacKeyUp(void)
{
  doMacKeyUpDown(false);
}

void wxApp::doMacAutoKey(void)
{
  doMacKeyUpDown(true);
}

//-----------------------------------------------------------------------------
void wxApp::doMacActivateEvt(void)
{
  WindowPtr theMacWindow = WindowPtr(cCurrentEvent.message);
  wxFrame* theMacWxFrame = findMacWxFrame(theMacWindow);
  if (theMacWxFrame)
    {
      Bool becomingActive = cCurrentEvent.modifiers & activeFlag;
      theMacWxFrame->Activate(becomingActive);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacUpdateEvt(void)
{
  WindowPtr theMacWindow = (WindowPtr)cCurrentEvent.message;
  wxFrame* theMacWxFrame = findMacWxFrame(theMacWindow);
  if (theMacWxFrame)
    {
      theMacWxFrame->MacUpdateWindow();
    } else {
      BeginUpdate(theMacWindow);
      EndUpdate(theMacWindow);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacDiskEvt(void)
{ // based on "Programming for System 7" by Gary Little and Tim Swihart
#ifndef OS_X
  // OS X doesn't support the diskEvt event. Yay!
  if ((cCurrentEvent.message >> 16) != noErr)
    {
      const int kDILeft = 0x0050; // top coord for disk init dialog
      const int kDITop = 0x0070; // left coord for disk init dialog
      Point mountPoint;
      mountPoint.h = kDILeft;
      mountPoint.v = kDITop;
      int myError = DIBadMount(mountPoint, cCurrentEvent.message);
    }
#endif
}

//-----------------------------------------------------------------------------
void wxApp::doMacOsEvt(void)
{ // based on "Programming for System 7" by Gary Little and Tim Swihart
  switch ((cCurrentEvent.message >> 24) & 0x0ff)
    {
    case suspendResumeMessage:
      if (cCurrentEvent.message & resumeFlag)
	{
	  doMacResumeEvent();
	}
      else
	{
	  doMacSuspendEvent();
	}
      break;
    case mouseMovedMessage:
      doMacMouseMovedMessage();
      break;
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacHighLevelEvent(void)
{ // based on "Programming for System 7" by Gary Little and Tim Swihart
  ::AEProcessAppleEvent(&cCurrentEvent); // System 7 or higher
}

//-----------------------------------------------------------------------------
void wxApp::doMacResumeEvent(void)
{
  wxFrame* theMacWxFrame = findMacWxFrame(::FrontWindow());
  if (theMacWxFrame)
    {
#ifndef OS_X
      if (cCurrentEvent.message & convertClipboardFlag)
	::TEFromScrap();
#endif                        
      Bool becomingActive = TRUE;
      theMacWxFrame->Activate(becomingActive);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacSuspendEvent(void)
{
  wxFrame* theMacWxFrame = findMacWxFrame(::FrontWindow());
  if (theMacWxFrame)
    {
#ifdef OS_X
      ClearCurrentScrap();
#else                
      ::ZeroScrap();
#endif                
      ::TEToScrap();
      Bool becomingActive = TRUE;
      theMacWxFrame->Activate(!becomingActive);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseMovedMessage(void)
{ // based on "Programming for System 7" by Gary Little and Tim Swihart
  if (cMacCursorRgn) ::DisposeRgn(cMacCursorRgn);
  cMacCursorRgn = ::NewRgn();
  CheckMemOK(cMacCursorRgn);
  ::SetRectRgn(cMacCursorRgn, -32768, -32768, 32766, 32766);
}

//-----------------------------------------------------------------------------
Bool wxApp::doMacInMenuBar(long menuResult, Bool externOnly)
{
  int macMenuId = HiWord(menuResult);
  int macMenuItemNum = LoWord(menuResult); // counting from 1

  if (macMenuId == 0) 					// no menu item selected;
    return FALSE;
  if (macMenuId == 128) {
    if (macMenuItemNum != 1) {			// if not the "About" entry (or the separator)
#ifndef OS_X
      // these (apparently) don't happen under OS X                
      Str255		daName;
      GetMenuItemText(GetMenuHandle(128), macMenuItemNum, daName);
      (void) OpenDeskAcc(daName);
      HiliteMenu(0); // unhilite the hilited menu
#endif                        
      return TRUE;
    }
  }

  WindowPtr theMacWindow = ::FrontWindow();
  if (!theMacWindow) {
    if (macMenuId == 128) {
      // Must be "About..."
      DoDefaultAboutItem();
    } else {
      // Must be quit
      exit(0);
    }
    return TRUE;
  }
  
  wxFrame* theMacWxFrame = findMacWxFrame(theMacWindow);
  if (!theMacWxFrame) wxFatalError("No wxFrame for theMacWindow.");

  wxMenuBar* theWxMenuBar = theMacWxFrame->wx_menu_bar;
  if (!theWxMenuBar) {
    /* Must be the About or Close item. */
    if (macMenuId == 128) {
      DoDefaultAboutItem();
    } else {
      if (theMacWxFrame->OnClose())
	theMacWxFrame->Show(FALSE);
    }
    return TRUE;
    // wxFatalError("No wxMenuBar for wxFrame.");
  }
  
  if (externOnly) {
    // Don't handle other keybindings automatically; in MrEd,
    //  they'll be handled by a frame's PreOnChar method
    return FALSE;
  }

  wxMenu* theWxMenu;
  if (macMenuId == kHMHelpMenuID) {
    if (theWxMenuBar->wxHelpHackMenu) {
      theWxMenu = theWxMenuBar->wxHelpHackMenu;
      macMenuItemNum -= wxNumHelpItems;
    } else
      return TRUE;
  } else if (macMenuId == 128 && macMenuItemNum == 1) {
    // This will Help/About selection
    if ((theWxMenu = theWxMenuBar->wxHelpHackMenu)
	&& theWxMenuBar->iHelpMenuHackNum) {
      macMenuItemNum = theWxMenuBar->iHelpMenuHackNum;
    } else {
      DoDefaultAboutItem();
      return TRUE;
    }
  } else {
    theWxMenu = theWxMenuBar->wxMacFindMenu(macMenuId);
  }
  if (!theWxMenu) wxFatalError("No wxMenu for wxMenuBar.");

  wxNode* node = theWxMenu->menuItems.Nth(macMenuItemNum - 1); // counting from 0
  if (!node) wxFatalError("No wxNode for Nth menuItem.");

  wxMenuItem* theWxMenuItem = (wxMenuItem*) node->Data();
  if (!theWxMenuItem) wxFatalError("No wxMenuItem for wxNode.");

  theMacWxFrame->ProcessCommand(theWxMenuItem->itemId);
  
  return TRUE;
}

//-----------------------------------------------------------------------------
void wxApp::doMacInContent(WindowPtr window)
{
  wxFrame* theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame)
    {
      if (window != ::FrontWindow())
	{		
	  wxFrame* frontFrame = findMacWxFrame(::FrontWindow());
	  if (!frontFrame) wxFatalError("No wxFrame for frontWindow.");
	  if (0 && !frontFrame->IsModal())
	    {
	      ::SelectWindow(window); //WCH : should I be calling some wxMethod?
	    }
	  else ::SysBeep(3);
	}
      else
	{
	  doMacContentClick(theMacWxFrame);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacContentClick(wxFrame* frame)
{
  // RightButton is cmdKey click  on the mac platform for one-button mouse
  Bool rightButton = cCurrentEvent.modifiers & cmdKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;

  WXTYPE mouseEventType = rightButton ? wxEVENT_TYPE_RIGHT_DOWN : wxEVENT_TYPE_LEFT_DOWN;
  wxMouseEvent *_theMouseEvent = new wxMouseEvent(mouseEventType);
  wxMouseEvent &theMouseEvent = *_theMouseEvent;
  theMouseEvent.leftDown = !rightButton;
  theMouseEvent.middleDown = FALSE;
  theMouseEvent.rightDown = rightButton;
  theMouseEvent.shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent.controlDown = cCurrentEvent.modifiers & controlKey;
  theMouseEvent.altDown = isAltKey;
  theMouseEvent.metaDown = FALSE;  // mflatt
  theMouseEvent.timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt

  int hitX = cCurrentEvent.where.h; // screen window c.s.
  int hitY = cCurrentEvent.where.v; // screen window c.s.
  wxArea* frameParentArea = frame->ParentArea();
  frameParentArea->ScreenToArea(&hitX, &hitY);
  theMouseEvent.x = hitX; // frame parent area c.s.
  theMouseEvent.y = hitY; // frame parent area c.s.

  frame->SeekMouseEventArea(&theMouseEvent);
}

//-----------------------------------------------------------------------------
void wxApp::doMacInDrag(WindowPtr window)
{
  if (window != ::FrontWindow())
    {
      wxFrame* frontFrame = findMacWxFrame(::FrontWindow());
      if (!frontFrame) wxFatalError("No wxFrame for frontWindow.");
      if (0 && frontFrame->IsModal())
	{
	  ::SysBeep(3);
	  return;
	}
    }

  wxFrame* theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame)
    {
      BitMap screenBits;
      Rect dragBoundsRect;
      GetQDGlobalsScreenBits(&screenBits);
      dragBoundsRect = screenBits.bounds;
      InsetRect(&dragBoundsRect, 4, 4); // This is not really necessary
      DragWindow(window, cCurrentEvent.where, &dragBoundsRect);
      theMacWxFrame->wxMacRecalcNewSize(FALSE); // recalc new position only
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInGrow(WindowPtr window)
{
  wxFrame* theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame && theMacWxFrame->CanAcceptEvent())
    {
      BitMap screenBits;
      
      GetQDGlobalsScreenBits(&screenBits);
      
      Rect growSizeRect; // WCH: growSizeRect should be a member of wxFrame class
      growSizeRect.top = 1; // minimum window height
      growSizeRect.left = 1; // minimum window width
      growSizeRect.bottom = screenBits.bounds.bottom - screenBits.bounds.top;
      growSizeRect.right = screenBits.bounds.right - screenBits.bounds.left;
      long windSize = ::GrowWindow(window, cCurrentEvent.where, &growSizeRect);
      if (windSize != 0)
	{
	  wxArea* contentArea = theMacWxFrame->ContentArea();
	  int newContentWidth = LoWord(windSize);
	  int newContentHeight = HiWord(windSize);
	  if (newContentWidth == 0) newContentWidth = contentArea->Width(); // no change
	  if (newContentHeight == 0) newContentHeight = contentArea->Height(); // no change
	  contentArea->SetSize(newContentWidth, newContentHeight);

	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInGoAway(WindowPtr window)
{
  wxFrame* theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame && theMacWxFrame->CanAcceptEvent())
    {
      if ((!StillDown()) || (TrackGoAway(window, cCurrentEvent.where)))
	{
	  Bool okToDelete = theMacWxFrame->OnClose();
	  if (okToDelete) {
#if WXGARBAGE_COLLECTION_ON
	    theMacWxFrame->Show(FALSE);
#else
	    delete theMacWxFrame;
#endif
	  }
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInZoom(WindowPtr window, short windowPart)
{
  wxFrame* theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame && theMacWxFrame->CanAcceptEvent())
    {
      if (TrackBox(window, cCurrentEvent.where, windowPart))
	{
	  theMacWxFrame->Maximize(windowPart == inZoomOut);
	}
      
    }
}

//-----------------------------------------------------------------------------
wxFrame* wxApp::findMacWxFrame(WindowPtr theMacWindow)
{
#if 0
  wxFrame* result = NULL;
  wxNode* node = wxTopLevelWindows(NULL)->First();
  while (node && !result)
    {
      wxWindow* theWxWindow = (wxWindow*) node->Data();
      if (theWxWindow->IsMacWindow())
	{
	  if (((wxFrame*)theWxWindow)->macWindow() == theMacWindow)
	    {
	      result = (wxFrame*) theWxWindow;
	    }
	}
      if (!result) node = node->Next();
    }

  return result;
#else
  if (theMacWindow)
    return (wxFrame *)GetWRefCon(theMacWindow);
  else
    return NULL;
#endif
}

//-----------------------------------------------------------------------------
void wxApp::AdjustCursor(void)
{
  wxFrame* theMacWxFrame = findMacWxFrame(::FrontWindow());
  if (theMacWxFrame) {
    if (theMacWxFrame->cBusyCursor)
      wxSetCursor(wxHOURGLASS_CURSOR);
    else {
      /* 
	 if (cCurrentEvent.what != kHighLevelEvent)
	 {
	 cLastMousePos.h = cCurrentEvent.where.h;
	 cLastMousePos.v = cCurrentEvent.where.v;
	 }
	 */
      Point p;
      GetMouse(&p);
      LocalToGlobal(&p);
      int hitX = p.h; // screen window c.s.
      int hitY = p.v; // screen window c.s.
      wxArea* frameParentArea = theMacWxFrame->ParentArea();
      frameParentArea->ScreenToArea(&hitX, &hitY);
      if (!theMacWxFrame->AdjustCursor(hitX, hitY))
	wxSetCursor(wxSTANDARD_CURSOR);
    }
  } else
    wxSetCursor(wxSTANDARD_CURSOR);
}

//-----------------------------------------------------------------------------
char *wxApp::GetDefaultAboutItemName(void)
{
  return "About wxWindows...";
}

void wxApp::DoDefaultAboutItem(void)
{
  wxMessageBox("This application was implemented with wxWindows,\n"
	       "Copyright 1993-94, AIAI, University of Edinburgh.\n"
	       "All Rights Reserved.",
	       "wxWindows");
}
