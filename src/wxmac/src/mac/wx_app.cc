////////////////////////////////////////////////////////////////////////////////
// File:	wx_app.cc
// Purpose:	wxApp implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
////////////////////////////////////////////////////////////////////////////////

#include "wx_main.h"
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
#include "wx_dccan.h"
#include "wxTimeScale.h"
#include "wx_print.h"
#if (defined(powerc) || defined(__powerc)) && defined(MPW)
QDGlobals 	qd;
#endif

#include <stdlib.h>

extern void wxDoEvents();
extern void wxDoNextEvent();
extern int wxEventReady();

extern void Drop_Quit();
extern void wxDo_Pref();
extern void wxDo_About();

extern CGrafPtr gMacFontGrafPort;

extern int wxNumHelpItems;

extern Bool doCallPreMouseEvent(wxWindow *in_win, wxWindow *win, wxMouseEvent *evt);

int wxMenuBarHeight;

extern wxApp *wxTheApp;
//-----------------------------------------------------------------------------
wxApp::wxApp():wxbApp()
{
  OSErr myErr;
  long quickdrawVersion;
  long windowMgrAttributes;
  long dontcare;
  wxArea* menuArea;
  
  wxREGGLOB(wxTheApp);
  wxTheApp = this;

  wxREGGLOB(wxWindow::gMouseWindow);

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

  wxREGGLOB(wxScreen::gScreenWindow);
  {
    wxScreen *sc;
    sc = new wxScreen;
    wxScreen::gScreenWindow = sc;
  }
  menuArea = (wxScreen::gScreenWindow)->MenuArea();
  wxMenuBarHeight = GetMBarHeight();
  menuArea->SetMargin(wxMenuBarHeight, wxTop);

  wx_frame = NULL;
  death_processed = FALSE;
  work_proc = NULL;
  wx_class = NULL;
  cLastMousePos.v = cLastMousePos.h = -1;  
  
  wxInitGL();
}

//-----------------------------------------------------------------------------
wxApp::~wxApp(void)
{
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

static MenuHandle m129 = NULL;

void wxApp::doMacPreEvent()
{
  static Bool noWinMode = FALSE;
  WindowPtr w;

  w = FrontWindow();

  wxCheckFinishedSounds();

  if (!w && !noWinMode) {
    wxPrepareMenuDraw();
    ::ClearMenuBar();
    wxSetUpAppleMenu(NULL);
    {
      if (!m129)
	m129 = GetMenu(129);
      if (m129) {
	::InsertMenu(m129, 0);
      }
    }
    ::InvalMenuBar();
    wxDoneMenuDraw();
    wxSetCursor(wxSTANDARD_CURSOR);
    noWinMode = TRUE;
  } else if (w && noWinMode)
    noWinMode = FALSE;

  if (w) {
    wxFrame* macWxFrame;
    macWxFrame = findMacWxFrame(w);
    
    /* If this is the root frame, try to move it behind everything
       else.  If there is any other window, the root frame shouldn't
       be frontmost. */
    if (macWxFrame && (macWxFrame == wxRootFrame)) {
      wxMacDC *dc;
      CGrafPtr graf;
      dc = wxRootFrame->MacDC();
      graf = dc->macGrafPort();
      ::SendBehind(GetWindowFromPort(graf), NULL);
      w = FrontWindow();
      macWxFrame = findMacWxFrame(w);
    }

    if (macWxFrame) {
      wxWindow* focusWindow;
      
      if (oldFrontWindow != macWxFrame) {
	oldFrontWindow->NowFront(FALSE);
	macWxFrame->NowFront(TRUE);
	oldFrontWindow = macWxFrame;
      }
      
      focusWindow = macWxFrame->cFocusWindow;
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
    case mouseMenuDown:
    case mouseDown:
      doMacMouseDown(); break;
    case mouseUp:
      doMacMouseUp(); break;
    case keyDown:
    case wheelEvt:
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
    case leaveEvt:
      doMacMouseLeave(); break;
    case nullEvent:
      if (e->message)
	doMacMouseMotion();
      break;
    default:
      break;
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseDown(void)
{
  WindowPtr window;
  short windowPart;

  windowPart = FindWindow(cCurrentEvent.where, &window);
  if (!window)
    window = FrontWindow();

  /* Check whether this window is blocked by a modal dialog: */
  {
    wxFrame *f;
    
    f = findMacWxFrame(window);
    if (f) {
      wxFrame *modal;
      
      modal = (wxFrame *)wxGetModalWindow(f);
      if (modal && (modal != f)) {
	/* Make sure the modal window is frontmost: */
	WindowPtr w;

	w = modal->macWindow();
	if (w != FrontWindow()) {
	  ::SelectWindow(w);
	}

#ifdef OS_X
	/* It's possible that the modal window is a sheet
	   in f... allow move operations. */
	if (((windowPart == inDrag) || (windowPart == inCollapseBox))
	    && (f->sheet == modal))
	  modal = NULL;
#endif

	if (modal) {
	  SysBeep(0);
	  return;
	}
      }
    }
  }

  switch (windowPart)
    {
    case inMenuBar:
      /* This code used to be guarded by a StillDown() check, but the
	 check doesn't seem to be useful or necessary. In fact, it
	 interfered with Ctl-F2 menu activation. */
      {
	long menuResult;
	WindowPtr theMacWindow;

	theMacWindow = FrontWindow();

	/* Give the menu bar a chance to build on-demand items: */
	if (theMacWindow) {
	  wxFrame* theMacWxFrame;
	  theMacWxFrame = findMacWxFrame(theMacWindow);
	  if (theMacWxFrame)
	    theMacWxFrame->OnMenuClick();
	}

	wxTracking();
	wxPrepareMenuDraw();
	menuResult = MenuSelect(cCurrentEvent.where);
	wxDoneMenuDraw(!!menuResult);
	doMacInMenuBar(menuResult, FALSE);
      }
      break;
    case inContent:
      doMacInContent(window); 
      break;
    case inDrag:
      doMacInDrag(window); 
      break;
    case inGrow:
      doMacInGrow(window);
      break;
    case inGoAway:
      doMacInGoAway(window);
      break;
    case inCollapseBox:
      {
	wxTracking();
	if ((!StillDown()) || (TrackBox(window, cCurrentEvent.where, inCollapseBox)))
	  CollapseWindow(window, TRUE);
      }
      break;
#ifdef OS_X
    case inToolbarButton:
      {
	wxTracking();
	if ((!StillDown()) || (TrackBox(window, cCurrentEvent.where, inToolbarButton))) {
	  {
	    wxFrame* theMacWxFrame;
	    theMacWxFrame = findMacWxFrame(window);
	    if (theMacWxFrame)
	      theMacWxFrame->OnToolbarButton();
	  } 
	}
      }
      break;
#endif
    case inZoomIn:
    case inZoomOut:
      doMacInZoom(window, windowPart); 
      break;
    default:
      break;
    }
}

#define rightButtonKey 

//-----------------------------------------------------------------------------
void wxApp::doMacMouseUp(void)
{
  wxWindow* mouseWindow = wxWindow::gMouseWindow;
  if (mouseWindow)
    {
      int hitX = cCurrentEvent.where.h; // screen window c.s.
      int hitY = cCurrentEvent.where.v; // screen window c.s.
      wxMouseEvent *theMouseEvent;
      Bool rightButton = cCurrentEvent.modifiers & controlKey;
      int type = rightButton ? wxEVENT_TYPE_RIGHT_UP : wxEVENT_TYPE_LEFT_UP;

      mouseWindow->ScreenToClient(&hitX, &hitY); // mouseWindow client c.s.
      mouseWindow->ClientToLogical(&hitX, &hitY); // mouseWindow logical c.s.
      
      theMouseEvent = new wxMouseEvent(type);
      theMouseEvent->leftDown = FALSE;
      theMouseEvent->middleDown = FALSE;
      theMouseEvent->rightDown = FALSE;
      theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
      theMouseEvent->controlDown = FALSE;
      theMouseEvent->altDown = cCurrentEvent.modifiers & optionKey;
      theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
      theMouseEvent->x = hitX;
      theMouseEvent->y = hitY;
      theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt
      
      /* Grab is now only used for grabbing on mouse-down for canvases & panels: */
      if (wxSubType(mouseWindow->__type, wxTYPE_CANVAS) 
	  || wxSubType(mouseWindow->__type, wxTYPE_PANEL))
	mouseWindow->ReleaseMouse();
      
      if (!doCallPreMouseEvent(mouseWindow, mouseWindow, theMouseEvent))
	if (!mouseWindow->IsGray())
	  mouseWindow->OnEvent(theMouseEvent);
    }
  else
    {
      wxFrame* macWxFrame;
      macWxFrame = findMacWxFrame(FrontWindow());
      if (macWxFrame)
	{
	  int hitX = cCurrentEvent.where.h; // screen window c.s.
	  int hitY = cCurrentEvent.where.v; // screen window c.s.
	  wxArea* frameParentArea;
	  wxMouseEvent *theMouseEvent;
	  Bool rightButton;
	  int type;

	  frameParentArea = macWxFrame->ParentArea();
	  frameParentArea->ScreenToArea(&hitX, &hitY);

	  // RightButton is cmdKey click  on the mac platform for one-button mouse
	  rightButton = cCurrentEvent.modifiers & controlKey;
	  type = rightButton ? wxEVENT_TYPE_RIGHT_UP : wxEVENT_TYPE_LEFT_UP;
	  
	  theMouseEvent = new wxMouseEvent(type);
	  
	  theMouseEvent->leftDown = FALSE;
	  theMouseEvent->middleDown = FALSE;
	  theMouseEvent->rightDown = FALSE;
	  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
	  theMouseEvent->controlDown = FALSE;
	  // altKey is optionKey on the mac platform:
	  theMouseEvent->altDown = cCurrentEvent.modifiers & optionKey;
	  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
	  theMouseEvent->x = hitX;
	  theMouseEvent->y = hitY;
	  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt

	  macWxFrame->SeekMouseEventArea(theMouseEvent);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseMotion(void)
{
  // RightButton is controlKey click on the mac platform for one-button mouse
  Bool isRightButton = cCurrentEvent.modifiers & controlKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  Bool isMouseDown = (cCurrentEvent.modifiers & btnState);
  wxMouseEvent *theMouseEvent;

  theMouseEvent = new wxMouseEvent(wxEVENT_TYPE_MOTION);
  theMouseEvent->leftDown = isMouseDown && !isRightButton;
  theMouseEvent->middleDown = FALSE;
  theMouseEvent->rightDown = isMouseDown && isRightButton;
  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent->controlDown = FALSE;
  theMouseEvent->altDown = isAltKey;
  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt
  
  if (wxWindow::gMouseWindow)
    {
      int hitX, hitY;
      wxWindow* mouseWindow = wxWindow::gMouseWindow;
      
      hitX = cCurrentEvent.where.h; // screen window c.s.
      hitY = cCurrentEvent.where.v; // screen window c.s.
      mouseWindow->ScreenToClient(&hitX, &hitY); // mouseWindow client c.s.
      mouseWindow->ClientToLogical(&hitX, &hitY); // mouseWindow logical c.s.
      theMouseEvent->x = hitX;
      theMouseEvent->y = hitY;

      /* Grab is now only used for grabbing on mouse-down for canvases & panels: */
      if ((wxSubType(mouseWindow->__type, wxTYPE_CANVAS) 
	   || wxSubType(mouseWindow->__type, wxTYPE_PANEL))
	  && !isMouseDown)
	mouseWindow->ReleaseMouse();
      
      if (!doCallPreMouseEvent(mouseWindow, mouseWindow, theMouseEvent))		  
	if (!mouseWindow->IsGray())
	  mouseWindow->OnEvent(theMouseEvent);
    }
  else
    {
      wxFrame* macWxFrame;
      macWxFrame = findMacWxFrame(FrontWindow());
      if (macWxFrame)
	{
	  int hitX = cCurrentEvent.where.h; // screen window c.s.
	  int hitY = cCurrentEvent.where.v; // screen window c.s.
	  wxArea* frameParentArea;
	  frameParentArea = macWxFrame->ParentArea();
	  frameParentArea->ScreenToArea(&hitX, &hitY);
	  theMouseEvent->x = hitX; // frame parent area c.s.
	  theMouseEvent->y = hitY; // frame parent area c.s.

	  macWxFrame->SeekMouseEventArea(theMouseEvent);
	}
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseLeave(void)
{
  // RightButton is controlKey click on the mac platform for one-button mouse
  Bool isRightButton = cCurrentEvent.modifiers & controlKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  Bool isMouseDown = (cCurrentEvent.modifiers & btnState);
  wxMouseEvent *theMouseEvent;
  wxWindow* win;
  void *rc;

  theMouseEvent = new wxMouseEvent(wxEVENT_TYPE_LEAVE_WINDOW);
  theMouseEvent->leftDown = isMouseDown && !isRightButton;
  theMouseEvent->middleDown = FALSE;
  theMouseEvent->rightDown = isMouseDown && isRightButton;
  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent->controlDown = FALSE;
  theMouseEvent->altDown = isAltKey;
  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt
  
  rc = (void *)cCurrentEvent.message;
  win = (wxWindow*)GET_SAFEREF(rc);
  if (win->IsShown()) {
    theMouseEvent->x = cCurrentEvent.where.h;
    theMouseEvent->y = cCurrentEvent.where.v;

    if (!doCallPreMouseEvent(win, win, theMouseEvent))
      if (!win->IsGray())
	win->OnEvent(theMouseEvent);
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
  wxWindow *p;
  p = win->GetParent();
  return ((p && doPreOnChar(in_win, p, evt)) || win->PreOnChar(in_win, evt));
}

short wxMacDisableMods; /* If a modifier key is here, handle it specially */
static short t2u_ready = 0;
static TextToUnicodeInfo t2uinfo;

static Handle transH = NULL;
static unsigned long transState = 0;
static Handle ScriptH = NULL;
static short region_code = 1;

void wxApp::doMacKeyUpDown(Bool down)
{
  wxFrame* theMacWxFrame;
  wxKeyEvent *theKeyEvent;
  int key;

  theMacWxFrame = findMacWxFrame(FrontWindow());
  
  if (down) {
    if (!theMacWxFrame || theMacWxFrame->CanAcceptEvent())
      if (cCurrentEvent.modifiers & cmdKey) { // is menu command key equivalent ?
	if (cCurrentEvent.what == keyDown) { // ignore autoKey
	  long menuResult;
	  wxPrepareMenuDraw();
	  menuResult = MenuEvent(&cCurrentEvent);
	  wxDoneMenuDraw(!!menuResult);
	  if (menuResult) {
	    if (doMacInMenuBar(menuResult, TRUE)) {
	      return;
	    } else {
	      wxPrepareMenuDraw();
	      // HiliteMenu(0); // calling wxPrepareMenuDraw unhlites the menu
	      wxDoneMenuDraw();
	    }
	  }
	}
      }
  }    
  
  if (!theMacWxFrame || !theMacWxFrame->IsEnable())
    return;	

  if (!ScriptH) {
    struct ItlbRecord * r;
    ScriptH = GetResource('itlb',0);
    if (ScriptH) {
      HLock(ScriptH);
      r = (ItlbRecord*)*ScriptH;
      region_code = r->itlbKeys;  	
      HUnlock(ScriptH);
    }	
  }
  
  theKeyEvent = new wxKeyEvent(wxEVENT_TYPE_CHAR);
  theKeyEvent->x = cCurrentEvent.where.h;
  theKeyEvent->y = cCurrentEvent.where.v;
  theKeyEvent->controlDown = Bool(cCurrentEvent.modifiers & controlKey);
  theKeyEvent->shiftDown = Bool(cCurrentEvent.modifiers & shiftKey);
  // altKey is optionKey on the mac platform:
  theKeyEvent->altDown = Bool(cCurrentEvent.modifiers & optionKey);
  theKeyEvent->metaDown = Bool(cCurrentEvent.modifiers & cmdKey);
  theKeyEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when);

  if (cCurrentEvent.what == wheelEvt) {
    if (cCurrentEvent.message)
      key = WXK_WHEEL_UP;
    else
      key = WXK_WHEEL_DOWN;
  } else {
    key = (cCurrentEvent.message & keyCodeMask) >> 8;
    /* Better way than to use hard-wired key codes? */
    switch (key) {
#   define wxFKEY(code, wxk)  case code: key = wxk; break
      wxFKEY(122, WXK_F1);
      wxFKEY(120, WXK_F2);
      wxFKEY(99, WXK_F3);
      wxFKEY(118, WXK_F4);
      wxFKEY(96, WXK_F5);
      wxFKEY(97, WXK_F6);
      wxFKEY(98, WXK_F7);
      wxFKEY(100, WXK_F8);
      wxFKEY(101, WXK_F9);
      wxFKEY(109, WXK_F10);
      wxFKEY(103, WXK_F11);
      wxFKEY(111, WXK_F12);
      wxFKEY(105, WXK_F13);
      wxFKEY(107, WXK_F14);
      wxFKEY(113, WXK_F15);
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

      if ((key > 127) && (key < 256)) {
	/* Translate to Latin-1 */
	ByteCount ubytes, converted;
	unsigned char unicode[2], str[1];
	
	if (!t2u_ready) {
	  CreateTextToUnicodeInfoByEncoding(kTextEncodingMacRoman, &t2uinfo);
	  t2u_ready = 1;
	}
	
	str[0] = key;
	
	ConvertFromTextToUnicode(t2uinfo, 1, (char *)str, 0,
				 0, NULL,
				 NULL, NULL,
				 2, &converted, &ubytes,
				 (UniCharArrayPtr)unicode);
	
	/* Sleazy conversion from UCS16 to Latin-1: drop high byte */
	key = unicode[1];
      }
    } // end switch
  }

  if (down) {
    theKeyEvent->keyCode = key;
    theKeyEvent->keyUpCode = WXK_PRESS;
  } else {
    theKeyEvent->keyCode = WXK_RELEASE;
    theKeyEvent->keyUpCode = key;
  }  

  {
    wxWindow *in_win;
    
    in_win = theMacWxFrame->GetFocusWindow();
    
    if (!in_win || !doPreOnChar(in_win, in_win, theKeyEvent))
      if (!theMacWxFrame->IsGray())
	theMacWxFrame->OnChar(theKeyEvent);
  }
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
  WindowPtr theMacWindow;
  wxFrame* theMacWxFrame;
  theMacWindow = WindowPtr(cCurrentEvent.message);
  theMacWxFrame = findMacWxFrame(theMacWindow);
  if (theMacWxFrame) {
    Bool becomingActive = cCurrentEvent.modifiers & activeFlag;
    theMacWxFrame->Activate(becomingActive);
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacUpdateEvt(void)
{
  WindowPtr theMacWindow;
  wxFrame* theMacWxFrame;
  theMacWindow = (WindowPtr)cCurrentEvent.message;
  theMacWxFrame = findMacWxFrame(theMacWindow);
  if (theMacWxFrame) {
    theMacWxFrame->MacUpdateWindow();
  } else {
    BeginUpdate(theMacWindow);
    EndUpdate(theMacWindow);
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacDiskEvt(void)
{
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
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(FrontWindow());
  if (theMacWxFrame)
    {
#ifndef WX_CARBON
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
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(FrontWindow());
  if (theMacWxFrame)
    {
      Bool becomingActive = TRUE;
#ifdef WX_CARBON
      ClearCurrentScrap();
#else                
      ::ZeroScrap();
#endif                
      ::TEToScrap();
      theMacWxFrame->Activate(!becomingActive);
    }
}

//-----------------------------------------------------------------------------
void wxApp::doMacMouseMovedMessage(void)
{
}

//-----------------------------------------------------------------------------

static void UnhiliteMenu()
{
  wxPrepareMenuDraw();
  // HiliteMenu(0); // calling wxPrepareMenuDraw unhlites the menu
  wxDoneMenuDraw();
}

Bool wxApp::doMacInMenuBar(long menuResult, Bool externOnly)
{
  int macMenuId = HiWord(menuResult);
  int macMenuItemNum = LoWord(menuResult); // counting from 1
  WindowPtr theMacWindow;
  wxMenu* theWxMenu;
  wxFrame* theMacWxFrame;
  wxMenuBar* theWxMenuBar;
  wxNode* node;
  wxMenuItem* theWxMenuItem;
  
  if (macMenuId == 0) 					// no menu item selected;
    return FALSE;

  // Check for the standard menu items:
  {
    MenuRef mnu;
    MenuItemIndex idx;
    
    if (macMenuId == 128) {
      // Must be "About..."
      wxDo_About();
      UnhiliteMenu();
      return TRUE;
    }

    if (!GetIndMenuItemWithCommandID(NULL, 'quit', 1, &mnu, &idx)) {
      if ((macMenuId == GetMenuID(mnu)) && (macMenuItemNum == idx)) {
	Drop_Quit();
	UnhiliteMenu();
	return TRUE;
      }
    }
    
    if (!GetIndMenuItemWithCommandID(NULL, 'pref', 1, &mnu, &idx)) {
      if ((macMenuId == GetMenuID(mnu)) && (macMenuItemNum == idx)) {
	wxDo_Pref();
	UnhiliteMenu();
	return TRUE;
      }
    }
    
    if (!GetIndMenuItemWithCommandID(NULL, 'hide', 1, &mnu, &idx)) {
      if ((macMenuId == GetMenuID(mnu)) && (macMenuItemNum == idx)) {
	/* Hide application */
	
	UnhiliteMenu();
	return TRUE;
      }
    }
  }

  theMacWindow = FrontWindow();
  if (!theMacWindow) {
    // Must be quit
    exit(0);
    return TRUE;
  }

  theMacWxFrame = findMacWxFrame(theMacWindow);
  if (!theMacWxFrame) wxFatalError("No wxFrame for theMacWindow.");

  theWxMenuBar = theMacWxFrame->wx_menu_bar;
  if (!theWxMenuBar) {
    /* Must be the Close item. See wx_frame.cxx. */
    if (theMacWxFrame->IsModal()) {
      /* this is really a dialog */
      wxChildNode *node2;
      wxChildList *cl;
      cl = theMacWxFrame->GetChildren();
      node2 = cl->First();
      if (node2) {
	wxDialogBox *d;
	d = (wxDialogBox *)node2->Data();
	if (d) {
	  if (d->OnClose())
	    d->Show(FALSE);
	}
      }
    } else {
      if (theMacWxFrame->OnClose())
	theMacWxFrame->Show(FALSE);
    }
    UnhiliteMenu();
    return TRUE;
  }
  
  if (externOnly) {
    // Don't handle other keybindings automatically; in MrEd,
    //  they'll be handled by a frame's PreOnChar method
    return FALSE;
  }

  if (macMenuId == kHMHelpMenuID) {
    if (theWxMenuBar->wxHelpHackMenu) {
      theWxMenu = theWxMenuBar->wxHelpHackMenu;
      macMenuItemNum -= wxNumHelpItems;
    } else
      return TRUE;
  } else if (macMenuId == 128) {
    if (macMenuItemNum == 1) {
      // This will Help/About selection
      theWxMenu = theWxMenuBar->wxHelpHackMenu;
      if (theWxMenu && theWxMenuBar->iHelpMenuHackNum) {
	macMenuItemNum = theWxMenuBar->iHelpMenuHackNum;
      } else {
	wxDo_About();
	UnhiliteMenu();
	return TRUE;
      }
    } else
      return FALSE;
  } else {
    theWxMenu = theWxMenuBar->wxMacFindMenu(macMenuId);
  }
  if (!theWxMenu) wxFatalError("No wxMenu for wxMenuBar.");

  node = theWxMenu->menuItems->Nth(macMenuItemNum - 1); // counting from 0
  if (!node) wxFatalError("No wxNode for Nth menuItem.");

  theWxMenuItem = (wxMenuItem*) node->Data();
  if (!theWxMenuItem) wxFatalError("No wxMenuItem for wxNode.");

  if (theWxMenuItem->IsCheckable()) {
    theWxMenuItem->Check(!theWxMenuItem->IsChecked());
  }

  theMacWxFrame->ProcessCommand(theWxMenuItem->itemId);
  
  return TRUE;
}

//-----------------------------------------------------------------------------
void wxApp::doMacInContent(WindowPtr window)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame)
    {
      if (window != FrontWindow())
	{		
	  wxFrame* frontFrame;
	  frontFrame = findMacWxFrame(FrontWindow());
	  if (!frontFrame) wxFatalError("No wxFrame for frontWindow.");
	  ::SysBeep(3);
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
  // RightButton is controlKey click  on the mac platform for one-button mouse
  Bool rightButton = cCurrentEvent.modifiers & controlKey;
  // altKey is optionKey on the mac platform:
  Bool isAltKey = cCurrentEvent.modifiers & optionKey;
  wxMouseEvent *theMouseEvent;
  wxArea* frameParentArea;
  WXTYPE mouseEventType = rightButton ? wxEVENT_TYPE_RIGHT_DOWN : wxEVENT_TYPE_LEFT_DOWN;
  int hitX, hitY;

  theMouseEvent = new wxMouseEvent(mouseEventType);
  theMouseEvent->leftDown = !rightButton;
  theMouseEvent->middleDown = FALSE;
  theMouseEvent->rightDown = rightButton;
  theMouseEvent->shiftDown = cCurrentEvent.modifiers & shiftKey;
  theMouseEvent->controlDown = FALSE;
  theMouseEvent->altDown = isAltKey;
  theMouseEvent->metaDown = cCurrentEvent.modifiers & cmdKey;
  theMouseEvent->timeStamp = SCALE_TIMESTAMP(cCurrentEvent.when); // mflatt

  hitX = cCurrentEvent.where.h; // screen window c.s.
  hitY = cCurrentEvent.where.v; // screen window c.s.
  frameParentArea = frame->ParentArea();
  frameParentArea->ScreenToArea(&hitX, &hitY);
  theMouseEvent->x = hitX; // frame parent area c.s.
  theMouseEvent->y = hitY; // frame parent area c.s.

  // Sheets cause windows to move in lots of ways.
  // Best just to re-calculate the position before processing an event.
  frame->wxMacRecalcNewSize(FALSE);

  frame->SeekMouseEventArea(theMouseEvent);
}

//-----------------------------------------------------------------------------

static WindowPtr last_drag_click;
static unsigned long last_drag_click_time;

void wxApp::doMacInDrag(WindowPtr window)
{
  if (last_drag_click == window) {
    if ((cCurrentEvent.when != last_drag_click_time) // avoid bring-to-front redundancies
	&& (cCurrentEvent.when - last_drag_click_time <= GetDblTime())) {
      CollapseWindow(window, TRUE);
      return;
    }
  }
  last_drag_click = window;
  last_drag_click_time = cCurrentEvent.when;

  if (StillDown()) {
    wxFrame* theMacWxFrame;
    theMacWxFrame = findMacWxFrame(window);
    if (theMacWxFrame) {
      BitMap screenBits;
      long oldx, oldy;
      Rect dragBoundsRect;

      GetQDGlobalsScreenBits(&screenBits);
      dragBoundsRect = screenBits.bounds;
      InsetRect(&dragBoundsRect, 4, 4); // This is not really necessary

      oldx = theMacWxFrame->cWindowX;
      oldy = theMacWxFrame->cWindowY;

      wxTracking();

      DragWindow(window, cCurrentEvent.where, &dragBoundsRect);

      theMacWxFrame->wxMacRecalcNewSize(FALSE); // recalc new position only

      if ((oldx != theMacWxFrame->cWindowX)
	  || (oldy != theMacWxFrame->cWindowY))
	last_drag_click = NULL;
    }
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInGrow(WindowPtr window)
{
  if (StillDown()) {
    wxFrame* theMacWxFrame;
    theMacWxFrame = findMacWxFrame(window);
    if (theMacWxFrame && theMacWxFrame->CanAcceptEvent()) {
      Rect growSizeRect; // WCH: growSizeRect should be a member of wxFrame class
      long windSize;

      wxTracking();

      theMacWxFrame->GetSizeLimits(&growSizeRect);

      windSize = ::GrowWindow(window, cCurrentEvent.where, &growSizeRect);
      if (windSize != 0) {
	wxArea* contentArea;
	int newContentWidth = LoWord(windSize);
	int newContentHeight = HiWord(windSize);
	contentArea = theMacWxFrame->ContentArea();
	if (newContentWidth == 0) newContentWidth = contentArea->Width(); // no change
	if (newContentHeight == 0) newContentHeight = contentArea->Height(); // no change
	contentArea->SetSize(newContentWidth, newContentHeight);
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInGoAway(WindowPtr window)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(window);
  if (theMacWxFrame && theMacWxFrame->CanAcceptEvent()) {
    wxTracking();
    if ((!StillDown()) || (TrackGoAway(window, cCurrentEvent.where))) {
      Bool okToDelete;
      okToDelete = theMacWxFrame->OnClose();
      if (okToDelete) {
	theMacWxFrame->Show(FALSE);
      }
    }
  }
}

//-----------------------------------------------------------------------------
void wxApp::doMacInZoom(WindowPtr window, short windowPart)
{
  if (StillDown()) { 
    wxFrame* theMacWxFrame;
    theMacWxFrame = findMacWxFrame(window);
    if (theMacWxFrame && theMacWxFrame->CanAcceptEvent()) {
      wxTracking();
      if (TrackBox(window, cCurrentEvent.where, windowPart))
	theMacWxFrame->Maximize(windowPart == inZoomOut);
    }
  }
}

//-----------------------------------------------------------------------------
wxFrame* wxApp::findMacWxFrame(WindowPtr theMacWindow)
{
  wxFrame *fr;

  if (theMacWindow) {
    void *rc;
    rc = (void *)GetWRefCon(theMacWindow);
    if (rc)
      fr = (wxFrame *)GET_SAFEREF(rc);
    else
      fr = NULL;
  } else
    fr = NULL;

  return fr;
}

//-----------------------------------------------------------------------------
void wxApp::AdjustCursor(void)
{
  wxFrame* theMacWxFrame;
  theMacWxFrame = findMacWxFrame(FrontWindow());
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
      wxArea* frameParentArea;
      int hitX, hitY;

      GetMouse(&p);
      LocalToGlobal(&p);
      hitX = p.h; // screen window c.s.
      hitY = p.v; // screen window c.s.
      frameParentArea = theMacWxFrame->ParentArea();
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
