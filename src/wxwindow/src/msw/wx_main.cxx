/*
 * File:	wx_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_main.cxx,v 1.10 1999/07/09 17:34:26 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_mgstr.h"
#include "wx_frame.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_dc.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_wmgr.h"
#include "wx_setup.h"

#endif

#include <string.h>

// NT defines APIENTRY, 3.x not
#if !defined(APIENTRY)
#define APIENTRY FAR PASCAL
#endif
 
#ifdef WIN32
#define _EXPORT
#else
#define _EXPORT _export
#endif
 
#if !defined(WIN32)
#define DLGPROC FARPROC
#endif

#if FAFA_LIB
#include "fafa.h"
#endif

#if USE_GAUGE
#include "..\..\contrib\gauge\zyzgauge.h"
#endif

#if CTL3D
#include <ctl3d.h>
#endif

#if !defined(WIN32)
#include <penwin.h>
#endif

wxWindow *wxDefaultParent = NULL;

HINSTANCE wxhInstance = 0;

extern wxNonlockingHashTable *wxWinHandleList;
extern wxNonlockingHashTable *wxSliderList;
extern FARPROC wxGenericControlSubClassProc;

extern void wxSetKeyboardHook(Bool doIt);

long last_msg_time; /* MATTHEW: timeStamp implementation */

/* Hack to support Watcom 32-bit Windows, DLLs etc */
//#if defined(__WINDOWS_386__) || defined(_WINDLL)
//# define _MULTIPLE_INSTANCES
//#endif
//*** I'll recommend to define it in the compiler command line.
//*** Generally you use WATCOM for the big applications, 
//*** so multiple instances are not so necessary.
//*** If you still want to use multiple instances for WATCOM be careful:
//*** you should make unique all windows classes names in all modules!
//*** I did it for wxWindows itself and for fafa.lib. 
//*** I didn't improve anything else!!!
//*** D.Chubraev.

#ifdef _MULTIPLE_INSTANCES
# define ZZ	32
#else
# define ZZ	/**/
#endif
char wxFrameClassName[ZZ]         = "wxFrameClass";
char wxMDIFrameClassName[ZZ]      = "wxMDIFrameClass";
char wxMDIChildFrameClassName[ZZ] = "wxMDIChildFrameClass";
char wxPanelClassName[ZZ]         = "wxPanelClass";
char wxCanvasClassName[ZZ]        = "wxCanvasClass";
#undef ZZ

HICON wxSTD_FRAME_ICON = NULL;

HFONT wxSTATUS_LINE_FONT = NULL;
LRESULT APIENTRY wxWndProc(HWND, UINT, WPARAM, LPARAM);
#if 0
UINT WINAPI wxTimerProc(HWND, WORD, int, DWORD);
#endif

// PenWindows Support
HANDLE hPenWin = (HANDLE)NULL;
#ifndef __WINDOWS_386__
typedef VOID (CALLBACK * PENREGPROC)(WORD,BOOL);
#endif

// The routine below allows Windows applications (binaries) to
// support Pen input when running under Microsoft Windows for
// Pen Computing 1.0 without need of the PenPalete.
//
// Should masked edit functions be added to wxWindows we would
// be a new class of functions to support BEDIT controls.
//
// (The function is a NOOP for native Windows-NT)

// Moved outside the function by JACS to make this file compile
#ifndef WIN32
#ifdef __WINDOWS_386__
  static HINDIR RegPenAppHandle = 0;
# define RegPenApp(u, b) (void)InvokeIndirectFunction(RegPenAppHandle, u, b)
#else
  static  VOID (CALLBACK * RegPenApp) (WORD, BOOL) = NULL;
#endif /* Watcom 32-bit Windows supervisor */
#endif

static void EnablePenAppHooks (Bool hook)
{
#ifndef WIN32
//  if (wxGetOsVersion() == wxWINDOWS_NT) return;

  if (hook)
    {
      if (hPenWin)
	return;

      ///////////////////////////////////////////////////////////////////////
      // If running on a Pen Windows system, register this app so all
      // EDIT controls in dialogs are replaced by HEDIT controls.
      if ((hPenWin = (HANDLE)GetSystemMetrics (SM_PENWINDOWS)) != (HANDLE) NULL)
	{
	  // We do this fancy GetProcAddress simply because we don't
	  // know if we're running Pen Windows.
#ifdef __WINDOWS_386__
	  FARPROC addr;
	  if ((addr = GetProcAddress (hPenWin, "RegisterPenApp")) != NULL)
	    {
	      RegPenAppHandle = GetIndirectFunctionHandle (addr, INDIR_WORD, INDIR_WORD, INDIR_ENDLIST);
	      RegPenApp (RPA_DEFAULT, TRUE);
	    }
#else /* Normal DLL calling convention */
	  if ((RegPenApp = (PENREGPROC)GetProcAddress (hPenWin, "RegisterPenApp")) != NULL)
	    (*RegPenApp) (RPA_DEFAULT, TRUE);
#endif /* Watcom 32-bit Windows supervisor */
	}
    }
  else
    {
      ///////////////////////////////////////////////////////////////////////
      // If running on a Pen Windows system, unregister
      if (hPenWin)
	{
	  // Unregister this app 
#ifdef __WINDOWS_386__
	  if (RegPenAppHandle)
	    RegPenApp (RPA_DEFAULT, FALSE);
# undef RegPenApp
#else /* Normal DLL calling convention */
	  if (RegPenApp != NULL)
	    (*RegPenApp) (RPA_DEFAULT, FALSE);
#endif /* Watcom 32-bit Windows supervisor */
	  hPenWin = (HANDLE) NULL;
	}
    }
#endif	/* ! Windows-NT */
}

void RegisterNoCursor(HINSTANCE hInstance, char *src, char *dest)
{
  WNDCLASSEX c;

  c.cbSize = sizeof(c);
  if (!GetClassInfoEx(hInstance, src, &c))
    wxFatalError("Can't get info for cursorless class");
  c.lpszClassName = dest;
  c.hCursor = NULL;
  if (!RegisterClassEx(&c))
    wxFatalError("Can't register cursorless class");
}

void wxInitialize(HINSTANCE hInstance)
{
  wxCommonInit();

#if CTL3D
  if (!Ctl3dRegister(hInstance))                                       
    wxFatalError("Cannot register CTL3D");

  Ctl3dAutoSubclass(hInstance);
#endif
#if FAFA_LIB
  InitFafa(hInstance);
#endif
#if USE_GAUGE
  if (!gaugeInit(hInstance))
    wxFatalError("Cannot initalize Gauge library");
#endif

  wxSTD_FRAME_ICON = LoadIcon(hInstance, "wxSTD_FRAME");

  wxSTATUS_LINE_FONT = CreateFont(16, 0, 0, 0, FW_NORMAL, 0, 0, 0,
                    ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
                    PROOF_QUALITY, DEFAULT_PITCH | FF_SWISS,
                    "Arial");

///////////////////////////////////////////////////////////////////////
// Register the frame window class.
  WNDCLASS wndclass;   // Structure used to register Windows class.

  wndclass.style         = CS_HREDRAW | CS_VREDRAW;
  wndclass.lpfnWndProc   = (WNDPROC)wxWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = sizeof( DWORD ); // was 4
  wndclass.hInstance     = hInstance;
  wndclass.hIcon         = wxSTD_FRAME_ICON;
  wndclass.hCursor       = NULL /* LoadCursor( NULL, IDC_ARROW ) */;
#if FAFA_LIB
  wndclass.hbrBackground =  (HBRUSH)(COLOR_BTNFACE+1) ;
#else
  wndclass.hbrBackground = GetStockObject( WHITE_BRUSH );
#endif
  wndclass.lpszMenuName  = NULL;
#ifdef _MULTIPLE_INSTANCES
  sprintf( wxFrameClassName,"wxFrameClass%d", hInstance );
#endif
  wndclass.lpszClassName = wxFrameClassName;

  if (!RegisterClass(&wndclass))
    wxFatalError("Can't register Frame Window class");

///////////////////////////////////////////////////////////////////////
// Register the MDI frame window class.
  WNDCLASS wndclass1;   // Structure used to register Windows class.

  wndclass1.style         = CS_HREDRAW | CS_VREDRAW;
  wndclass1.lpfnWndProc   = (WNDPROC)wxWndProc;
  wndclass1.cbClsExtra    = 0;
  wndclass1.cbWndExtra    = sizeof( DWORD ); // was 4
  wndclass1.hInstance     = hInstance;
  wndclass1.hIcon         = wxSTD_FRAME_ICON;
  wndclass1.hCursor       = NULL /* LoadCursor( NULL, IDC_ARROW ) */;
#if FAFA_LIB
  wndclass1.hbrBackground =  (HBRUSH)(COLOR_APPWORKSPACE+1) ;
#else
  wndclass1.hbrBackground = NULL;
#endif
  wndclass1.lpszMenuName  = NULL;

#ifdef _MULTIPLE_INSTANCES
  sprintf( wxMDIFrameClassName,"wxMDIFrameClass%d", hInstance );
#endif
  wndclass1.lpszClassName = wxMDIFrameClassName;
  if (!RegisterClass( &wndclass1 ))
    wxFatalError("Can't register MDI Frame window class");

///////////////////////////////////////////////////////////////////////
// Register the MDI child frame window class.
  WNDCLASS wndclass4;   // Structure used to register Windows class.

  wndclass4.style         = CS_HREDRAW | CS_VREDRAW;
  wndclass4.lpfnWndProc   = (WNDPROC)wxWndProc;
  wndclass4.cbClsExtra    = 0;
  wndclass4.cbWndExtra    = sizeof( DWORD ); // was 4
  wndclass4.hInstance     = hInstance;
  wndclass4.hIcon         = wxSTD_FRAME_ICON;
  wndclass4.hCursor       = NULL /* LoadCursor( NULL, IDC_ARROW ) */;
#if FAFA_LIB
  wndclass4.hbrBackground =  (HBRUSH)(COLOR_BTNFACE+1) ;
#else
  wndclass4.hbrBackground = NULL;
#endif
  wndclass4.lpszMenuName  = NULL;
#ifdef _MULTIPLE_INSTANCES
  sprintf( wxMDIChildFrameClassName,"wxMDIChildFrameClass%d", hInstance );
#endif
  wndclass4.lpszClassName = wxMDIChildFrameClassName;

  if (!RegisterClass( &wndclass4 ))
   wxFatalError("Can't register MDI child frame window class");

///////////////////////////////////////////////////////////////////////
// Register the panel window class.
  WNDCLASS wndclass2;   // Structure used to register Windows class.
  memset(&wndclass2, 0, sizeof(WNDCLASS));   // start with NULL defaults
  // Use CS_OWNDC to avoid messing about restoring the context
  // for every graphic operation.
  wndclass2.style         = CS_HREDRAW | CS_VREDRAW;
  wndclass2.lpfnWndProc   = (WNDPROC)wxWndProc;
  wndclass2.cbClsExtra    = 0;
  wndclass2.cbWndExtra    = sizeof( DWORD ); // was 4
  wndclass2.hInstance     = hInstance;
  wndclass2.hIcon         = NULL;
  wndclass2.hCursor       = NULL;
#if (FAFA_LIB && !USE_GREY_BACKGROUND)
  // wndclass2.hbrBackground = GetStockObject( LTGRAY_BRUSH );
  // No no no... After investigations, I found that Ctl3d use BTNFACE color
  // (which is ALWAYS grey :-))
  // So, respect the behavior!
  wndclass2.hbrBackground = (HBRUSH)(COLOR_BTNFACE+1) ;
#else
  wndclass2.hbrBackground = (HBRUSH)GetStockObject( LTGRAY_BRUSH );
#endif
  wndclass2.lpszMenuName  = NULL;
#ifdef _MULTIPLE_INSTANCES
  sprintf( wxPanelClassName,"wxPanelClass%d", hInstance );
#endif
  wndclass2.lpszClassName = wxPanelClassName;
  if (!RegisterClass( &wndclass2 ))
   wxFatalError("Can't register Panel Window class");

///////////////////////////////////////////////////////////////////////
// Register the canvas and textsubwindow class name
  WNDCLASS wndclass3;   // Structure used to register Windows class.
  memset(&wndclass3, 0, sizeof(WNDCLASS));   // start with NULL defaults
  // Use CS_OWNDC to avoid messing about restoring the context
  // for every graphic operation.
  wndclass3.style         = CS_HREDRAW | CS_VREDRAW | CS_OWNDC | CS_DBLCLKS ; 
  wndclass3.lpfnWndProc   = (WNDPROC)wxWndProc;
  wndclass3.cbClsExtra    = 0;
  wndclass3.cbWndExtra    = sizeof( DWORD ); // was 4
  wndclass3.hInstance     = hInstance;
  wndclass3.hIcon         = NULL;
  wndclass3.hCursor       = NULL;
#if FAFA_LIB
  wndclass3.hbrBackground = (HBRUSH)(COLOR_WINDOW+1) ;
#else
  wndclass3.hbrBackground = NULL;
#endif
  wndclass3.lpszMenuName  = NULL;
#ifdef _MULTIPLE_INSTANCES
  sprintf( wxCanvasClassName,"wxCanvasClass%d", hInstance );
#endif
  wndclass3.lpszClassName = wxCanvasClassName;
  if (!RegisterClass( &wndclass3))
   wxFatalError("Can't register Canvas class");

  RegisterNoCursor(hInstance, "BUTTON", "wxBUTTON");
  RegisterNoCursor(hInstance, "COMBOBOX", "wxCOMBOBOX");
  RegisterNoCursor(hInstance, "LISTBOX", "wxLISTBOX");
  RegisterNoCursor(hInstance, "EDIT", "wxEDIT");
  RegisterNoCursor(hInstance, "STATIC", "wxSTATIC");

#ifndef __WATCOMC__
#if !defined(__win32s__) && !defined(WIN32)
///////////////////////////////////////////////////////////////////////
// If running on a Pen Windows system, register this app so all
// EDIT controls in dialogs are replaced by HEDIT controls.
// (Notice the CONTROL statement in the RC file is "EDIT",
// RegisterPenApp will automatically change that control to
// an HEDIT.
  if ((hPenWin = (HANDLE)GetSystemMetrics(SM_PENWINDOWS)) != (HANDLE)NULL) {
    // We do this fancy GetProcAddress simply because we don't
    // know if we're running Pen Windows.
//   if ( ((FARPROC)RegPenApp = GetProcAddress(hPenWin, "RegisterPenApp"))!= NULL)
// ADDED THIS CAST TO MAKE IT COMPILER UNDER VC++ -- JACS
   if ( (RegPenApp = (VOID (CALLBACK *)(WORD, BOOL))GetProcAddress(hPenWin, "RegisterPenApp"))!= NULL)
     (*RegPenApp)(RPA_DEFAULT, TRUE);
  }
///////////////////////////////////////////////////////////////////////
#endif
#endif

  wxWinHandleList = new wxNonlockingHashTable();
  wxSliderList = new wxNonlockingHashTable();

  wxSetKeyboardHook(TRUE);
}


// Cleans up any wxWindows internal structures left lying around
void wxCleanUp(void)
{
#if WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  extern wxList wxModelessWindows;
  wxNode *node;
  while (node = wxModelessWindows.First())
    ((wxWindow *)node->Data())->Show(FALSE);
  GC_gcollect();
#endif

  wxSetKeyboardHook(FALSE);
  wxCommonCleanUp();

#ifndef __WATCOMC__
#if !defined(__win32s__) && !defined(WIN32)
  if (hPenWin) {
    // Unregister this app 
    if (RegPenApp != NULL)
	(*RegPenApp)(RPA_DEFAULT, FALSE);
  }
#endif
#endif
  
  if (wxSTD_FRAME_ICON)
    DestroyIcon(wxSTD_FRAME_ICON);

  DeleteObject(wxSTATUS_LINE_FONT);
#if CTL3D
  Ctl3dUnregister(wxhInstance);
#endif
#if FAFA_LIB
  EndFafa() ;
#endif
  if (wxGenericControlSubClassProc)
    FreeProcInstance(wxGenericControlSubClassProc);
  
  if (wxWinHandleList)
	 delete wxWinHandleList ;
/* Is this necessary/possible? What if we unregister whilst another wxWin
 * app is running?
  // Unregister window classes
  UnregisterClass(wxFrameClassName, wxhInstance);
  UnregisterClass(wxMDIFrameClassName, wxhInstance);
  UnregisterClass(wxMDIChildFrameClassName, wxhInstance);
  UnregisterClass(wxPanelClassName, wxhInstance);
  UnregisterClass(wxCanvasClassName, wxhInstance);
*/
}

// Main windows entry point

extern void wxInitUserResource(char *s);

static int retValue = 0;

static int parse_command_line(int count, char **command, char *buf, int maxargs)
{
  char *parse, *created, *write;
  int findquote = 0;
  
  parse = created = write = buf;
  while (*parse) {
    while (*parse && isspace(*parse)) parse++;
    while (*parse && (!isspace(*parse) || findquote))	{
      if (*parse== '"') {
	findquote = !findquote;
      } else if (*parse== '\\') {
	char *next;
	for (next = parse; *next == '\\'; next++);
	if (*next == '"') {
	  /* Special handling: */
	  int count = (next - parse), i;
	  for (i = 1; i < count; i += 2)
	    *(write++) = '\\';
	  parse += (count - 1);
	  if (count & 0x1) {
	    *(write++) = '\"';
	    parse++;
	  }
	}	else
	  *(write++) = *parse;
      } else
	*(write++) = *parse;
      parse++;
    }
    if (*parse)
      parse++;
    *(write++) = 0;
    
    if (*created)	{
      command[count++] = created;
      if (count == maxargs)
	return count;
    }
    created = write;
  }
  
  return count;
}

extern "C" int main(int, char**);

#ifdef __WATCOMC__
//***It's really principal for Watcom that WinMain should be PASCAL,
//***not FAR PASCAL!!   D.Chubraev
extern "C" int PASCAL WinMain(HINSTANCEhInstance, HINSTANCE WXUNUSED(hPrevInstance), LPSTR m_lpCmdLine,
						  int nCmdShow )
#else
extern "C" int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE WXUNUSED(hPrevInstance), LPSTR m_lpCmdLine,
						  int nCmdShow )
#endif
{
  wxhInstance = hInstance;

  wxInitialize(hInstance);

  // Split command line into tokens, as in usual main(argc, argv)
  char **command = new char*[50];
  int count = 0;
  char *buf = new char[strlen(m_lpCmdLine) + 1];
  // Hangs around until end of app. in case
  // user carries pointers to the tokens

  /* Model independent strcpy */
  {
    int i;
    for (i = 0; (buf[i] = m_lpCmdLine[i]) != 0; i++)
    {
      /* loop */;
    }
  }

  // Get application name
  {
    char name[500];
    ::GetModuleFileName(hInstance, name, 499);

  // Is it only Borland that already copies the program name
  // to the first argv index?
#ifndef __BORLANDC__
    command[count++] = copystring(name);
#endif

    strcpy(name, wxFileNameFromPath(name));
    wxStripExtension(name);
    wxTheApp->SetAppName(name);

    char *d, *p;

    d = getenv("HOMEDRIVE");
    p = getenv("HOMEPATH");

    if (d && p) {
      char *s;
      int l;

      s = new char[strlen(d) + strlen(p) + 12];
      strcpy(s, d);
      strcat(s, p);

      l = strlen(s);
      if (l && (s[l - 1] != '\\')) {
	 s[l] = '\\';
	 s[l + 1] = 0;
      }
      strcat(s, "mred.ini");
    
      wxInitUserResource(s);
    } else {
      char name[500], *s;
      int i;
      ::GetModuleFileName(hInstance, name, 499);

       i = strlen(name) - 1;    
       while (i && (name[i] != '\\'))
         --i;
       if (i)
	 i++;

       s = new char[i + 12];
       memcpy(s, name, i);
       strcpy(s + i, "mred.ini");

       wxInitUserResource(s);
    }
  }

  count = parse_command_line(count, command, buf, 49);
  command[count] = NULL; /* argv[] is NULL terminated list! */

  wxTheApp->hInstance = hInstance;
  // store the show-mode parameter of MSW for (maybe) later use.
  // this can be used to inform the program about special show modes
  // under MSW
  wxTheApp->nCmdShow = nCmdShow; // added by steve, 27.11.94

  return main(count, command);
}

int wxEntry(int argc, char **argv)
{
  wxTheApp->argc = argc;
  wxTheApp->argv = argv;

  wxTheApp->OnInit();
  
  return 0;
}

IMPLEMENT_DYNAMIC_CLASS(wxApp, wxObject)

wxApp::wxApp(wxlanguage_t language):wxbApp(language)
{
  wx_frame = NULL;
  wxTheApp = this;
  death_processed = FALSE;
  work_proc = NULL ;
  wx_class = NULL;
  wxSetLanguage(language);
}

wxApp::~wxApp(void)
{
}

Bool wxApp::Initialized(void)
{
#ifndef _WINDLL
  if (wx_frame)
    return TRUE;
  else
    return FALSE;
#endif
#ifdef _WINDLL // Assume initialized if DLL (no way of telling)
  return TRUE;
#endif
}

/*
 * Get and process a message, returning FALSE if WM_QUIT
 * received.
 *
 */
MSG top_msg;
int top_use = 0;

BOOL wxApp::DoMessage(void)
{
#if 0
  int rv = TRUE;
  MSG *msg;
  
  if (top_use)
    msg = new MSG;
  else {
    top_use = 1;
    msg = &top_msg;
  }
  if (!wxwmGetMessage(msg)) {
    retValue = msg->wParam;
    rv = FALSE;
  } else if (msg->message == WM_TIMER) {
    wxTimerProc(0, 0, msg->wParam, 0);
    rv = TRUE;
  } else if (!ProcessMessage(msg)) {
    last_msg_time = msg->time; /* MATTHEW: timeStamp impl. */
    wxwmTranslateMessage(msg);
    wxwmDispatchMessage(msg);
  }
  
  if (msg == &top_msg)
    top_use = 0;
  else
    delete msg;
  return rv;
#endif
  return FALSE;
}

extern void wxDoEvents();
extern void wxDoNextEvent();
extern int wxEventReady();

/*
 * Keep trying to process messages until WM_QUIT
 * received
 */

int wxApp::MainLoop(void)
{
  keep_going = TRUE;
#if 1
  while (keep_going)
    wxDoEvents();
#else
  while (keep_going && DoMessage());
#endif

  return 1;
}

void wxApp::ExitMainLoop(void)
{
  keep_going = FALSE;
}

Bool wxApp::Pending(void)
{
#if 1
  return wxEventReady();
#else
  OnIdle() ;
  return (::PeekMessage(&current_msg, NULL, NULL, NULL, PM_NOREMOVE));
#endif
}

void wxApp::Dispatch(void)
{
#if 1
  wxDoNextEvent();
#else
  if (!DoMessage())
    keep_going = FALSE;
#endif
}

/*
 * Give all windows a chance to preprocess
 * the message. Some may have accelerator tables, or have
 * MDI complications.
 */
BOOL wxApp::ProcessMessage(MSG *msg)
{
  HWND hWnd;

  // Anyone for a message? Try youngest descendants first.
  for (hWnd = msg->hwnd; hWnd != NULL; hWnd = ::GetParent(hWnd))
  {
    wxWnd *wnd = wxFindWinFromHandle(hWnd);
    if (wnd)
    {
       if (wnd->ProcessMessage(msg))
         return TRUE;

       // STOP if we've reached the top of the hierarchy!
       if (wx_frame && (wnd == (wxWnd *)wx_frame->handle))
          return FALSE;
    }
  }

  if (wx_frame && ((wxWnd *)wx_frame->handle)->ProcessMessage(msg))
     return TRUE;
  else return FALSE;
}

wxWindow *wxHWNDtoWindow(HWND hwnd)
{
  wxWnd *wnd = wxFindWinFromHandle(hwnd);

  if (wnd)
    return wnd->wx_window;
  else
	return NULL;
}

BOOL wxApp::OnIdle(void)
{
  if (work_proc)
    (*work_proc)(this) ;
  return FALSE;
}

// Windows specific. Intercept keyboard input: by default,
// route it to the active frame or dialog box.
Bool wxApp::OnCharHook(wxKeyEvent *event)
{
  wxWindow *win = wxGetActiveWindow();
  if (win)
    return win->GetEventHandler()->OnCharHook(event);
  else
    return FALSE;
}

void wxExit(void)
{
  if (wxTheApp)
    (void)wxTheApp->OnExit();
  wxCleanUp();
  FatalAppExit(0, "Exiting");
}

// Yield to incoming messages
Bool wxYield(void)
{
#if 1
  while (wxTheApp->Pending())
    wxTheApp->Dispatch();
#else
  MSG msg;
  // We want to go back to the main message loop
  // if we see a WM_QUIT. (?)
  while (1) {
    current_msg_dest = &msg;
    wxwmMessage(WXM_PEEK_MESSAGE, 0);
    if (!evt_ready || (msg.message == WM_QUIT))
      break;
    if (!wxTheApp->DoMessage())
      break;
  }
#endif

  return TRUE;
}

// Reset background brushes
#if FAFA_LIB
HBRUSH SetupBackground(HWND wnd)
{
  char tmp[128];

  CreatePensBrushes();
  GetClassName(wnd,tmp,127);
  if (strncmp(tmp,wxCanvasClassName,127)==0
#if !USE_GREY_BACKGROUND
      || strncmp(tmp,wxPanelClassName,127)==0
#endif
      || strncmp(tmp,wxMDIChildFrameClassName,127)==0)
  {
#ifdef WIN32
    SetClassLong(wnd,GCL_HBRBACKGROUND,(LONG)NULL) ;
#else
    SetClassWord(wnd,GCW_HBRBACKGROUND,(WORD)NULL) ;
#endif
    return brushBack;
  } else if (strncmp(tmp,wxFrameClassName,127)==0
	     || strncmp(tmp,wxMDIFrameClassName,127)==0)
  {
#ifdef WIN32
    SetClassLong(wnd,GCL_HBRBACKGROUND,(LONG)NULL) ;
#else
    SetClassWord(wnd,GCW_HBRBACKGROUND,(WORD)NULL) ;
#endif
    return brushFrame;
  }

  return NULL;
}
#endif
 
