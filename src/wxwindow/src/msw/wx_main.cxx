/*
 * File:	wx_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#include "wx.h"

#include <string.h>

#include "fafa.h"

#include "..\..\contrib\gauge\zyzgauge.h"

HINSTANCE wxhInstance = 0;

extern wxNonlockingHashTable *wxWinHandleList;
extern wxNonlockingHashTable *wxSliderList;
extern FARPROC wxGenericControlSubClassProc;

extern void wxSetKeyboardHook(Bool doIt);
extern void wxWindowInit(void);

long last_msg_time;

char wxFrameClassName[]         = "wxFrameClass";
char wxMDIFrameClassName[]      = "wxMDIFrameClass";
char wxMDIChildFrameClassName[] = "wxMDIChildFrameClass";
char wxPanelClassName[]         = "wxPanelClass";
char wxCanvasClassName[]        = "wxCanvasClass";

HICON wxSTD_FRAME_ICON = NULL;

HFONT wxSTATUS_LINE_FONT = NULL;
LRESULT APIENTRY wxWndProc(HWND, UINT, WPARAM, LPARAM);

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
  wxWindowInit();

  InitFafa(hInstance);
  if (!gaugeInit(hInstance))
    wxFatalError("Cannot initalize Gauge library");

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
  wndclass.hbrBackground =  (HBRUSH)(COLOR_BTNFACE+1) ;
  wndclass.lpszMenuName  = NULL;
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
  wndclass1.hbrBackground =  (HBRUSH)(COLOR_APPWORKSPACE+1) ;
  wndclass1.lpszMenuName  = NULL;

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
  wndclass4.hbrBackground =  (HBRUSH)(COLOR_BTNFACE+1) ;
  wndclass4.lpszMenuName  = NULL;
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
  wndclass2.hbrBackground = (HBRUSH)GetStockObject( LTGRAY_BRUSH );
  wndclass2.lpszMenuName  = NULL;
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
  wndclass3.hbrBackground = (HBRUSH)(COLOR_WINDOW+1) ;
  wndclass3.lpszMenuName  = NULL;
  wndclass3.lpszClassName = wxCanvasClassName;
  if (!RegisterClass( &wndclass3))
   wxFatalError("Can't register Canvas class");

  RegisterNoCursor(hInstance, "BUTTON", "wxBUTTON");
  RegisterNoCursor(hInstance, "COMBOBOX", "wxCOMBOBOX");
  RegisterNoCursor(hInstance, "LISTBOX", "wxLISTBOX");
  RegisterNoCursor(hInstance, "EDIT", "wxEDIT");
  RegisterNoCursor(hInstance, "STATIC", "wxSTATIC");

  wxREGGLOB(wxWinHandleList);
  wxREGGLOB(wxSliderList);

  wxWinHandleList = new wxNonlockingHashTable();
  wxSliderList = new wxNonlockingHashTable();

  wxSetKeyboardHook(TRUE);
}


// Cleans up any wxWindows internal structures left lying around
void wxCleanUp(void)
{
  wxSetKeyboardHook(FALSE);
  wxCommonCleanUp();

  if (wxSTD_FRAME_ICON)
    DestroyIcon(wxSTD_FRAME_ICON);

  DeleteObject(wxSTATUS_LINE_FONT);
  EndFafa();

  if (wxGenericControlSubClassProc)
    FreeProcInstance(wxGenericControlSubClassProc);
  
  if (wxWinHandleList)
    delete wxWinHandleList;
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
extern void wxCreateApp(void);
extern "C" int GC_use_registered_statics;

extern "C" int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE WXUNUSED(hPrevInstance), 
				LPSTR m_lpCmdLine, int nCmdShow )
{
  GC_use_registered_statics = 1;

  wxhInstance = hInstance;

  wxInitialize(hInstance);

  wxCreateApp();

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

    command[count++] = copystring(name);

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

wxApp::wxApp() : wxbApp()
{
  wxREGGLOB(wxTheApp);
  wxTheApp = this;

  wx_frame = NULL;
  death_processed = FALSE;
  work_proc = NULL ;
  wx_class = NULL;
}

wxApp::~wxApp(void)
{
}

Bool wxApp::Initialized(void)
{
  if (wx_frame)
    return TRUE;
  else
    return FALSE;
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
  while (keep_going)
    wxDoEvents();

  return 1;
}

void wxApp::ExitMainLoop(void)
{
  keep_going = FALSE;
}

Bool wxApp::Pending(void)
{
  return wxEventReady();
}

void wxApp::Dispatch(void)
{
  wxDoNextEvent();
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
  while (wxTheApp->Pending())
    wxTheApp->Dispatch();

  return TRUE;
}

// Reset background brushes
HBRUSH SetupBackground(HWND wnd)
{
  char tmp[128];

  CreatePensBrushes();
  GetClassName(wnd,tmp,127);
  if (strncmp(tmp,wxCanvasClassName,127)==0
      || strncmp(tmp,wxMDIChildFrameClassName,127)==0)
  {
    SetClassLong(wnd,GCL_HBRBACKGROUND,(LONG)NULL) ;
    return brushBack;
  } else if (strncmp(tmp,wxFrameClassName,127)==0
	     || strncmp(tmp,wxMDIFrameClassName,127)==0)
  {
    SetClassLong(wnd,GCL_HBRBACKGROUND,(LONG)NULL) ;
    return brushFrame;
  }

  return NULL;
}
 
