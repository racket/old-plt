/*								-*- C++ -*-
 * $Id: AppMain.cc,v 1.6 1998/09/06 01:53:58 mflatt Exp $
 *
 * Purpose: wxWindows application and main loop
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "AppMain.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxApp
#define  Uses_wxGDI
#define  Uses_wxPrintSetup
#include "wx.h"
#include "widgets.h" // for X11/StringDefs.h

//-----------------------------------------------------------------------------
// application resources
//-----------------------------------------------------------------------------

typedef struct {
    Boolean debugEvents;
    Boolean debugOutput;
} TwxAPP_DATA;
TwxAPP_DATA wxAPP_DATA;

//- application resources and options -
static XtResource wxAppResources[] = {
    { "debugEvents", "DebugEvents", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(TwxAPP_DATA, debugEvents), XtRImmediate, (XtPointer)FALSE },
    { "debugOutput", "DebugOutput", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(TwxAPP_DATA, debugOutput), XtRImmediate, (XtPointer)FALSE },
};
static XrmOptionDescRec wxAppOptions[] = {
    { "-debugEvents", "*debugEvents", XrmoptionNoArg, "TRUE" },
    { "-debugOutput", "*debugOutput", XrmoptionNoArg, "TRUE" },
};

//-----------------------------------------------------------------------------
// wxApp implementation
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxApp, wxObject)

// references to global data for compatibility
Bool&  wxApp::wantDebugOutput = wxAPP_DEBUGOUTPUT;
char*& wxApp::wx_class        = wxAPP_CLASS;
char*& wxApp::appName         = wxAPP_NAME;

wxApp::wxApp(wxlanguage_t language)
{
    __type = wxTYPE_APP;

    // no application and/or application not initialized
    initialized = FALSE;
    wxTheApp = this;
    wxSetLanguage(language);
}

void wxApp::Dispatch(void)
{
#if 0
  /* MATTHEW */
  // The problem with XtAppNextEvent is that it automatically dispatches
  // timers and alternate inputs and then stalls for an X event. If the
  // only thing pending was alternate input, a wxYield() could stall.
  // XtAppProcessEvent does what we want:
  XtAppProcessEvent(wxAPP_CONTEXT, XtIMAll);
#endif

  wxDoNextEvent();
}

int wxApp::MainLoop(void)
{
    keep_going = TRUE;
    while (keep_going) {
#if 0
        XEvent event;
	XtAppNextEvent(wxAPP_CONTEXT, &event);
#if DEBUG
	if (wxAPP_DATA.debugEvents) {
	    // print widgetname and event number
	    Widget w; int type = event.xany.type;
	    static char* event_name[] = {
		"", "unknown(-)",                                         // 0-1
		"KeyPress", "KeyRelease", "ButtonPress", "ButtonRelease", // 2-5
		"MotionNotify", "EnterNotify", "LeaveNotify", "FocusIn",  // 6-9
		"FocusOut", "KeymapNotify", "Expose", "GraphicsExpose",   // 10-13
		"NoExpose", "VisibilityNotify", "CreateNotify",           // 14-16
		"DestroyNotify", "UnmapNotify", "MapNotify", "MapRequest",// 17-20
		"ReparentNotify", "ConfigureNotify", "ConfigureRequest",  // 21-23
		"GravityNotify", "ResizeRequest", "CirculateNotify",      // 24-26
		"CirculateRequest", "PropertyNotify", "SelectionClear",   // 27-29
		"SelectionRequest", "SelectionNotify", "ColormapNotify",  // 30-32
		"ClientMessage", "MappingNotify",                         // 33-34
		"unknown(+)"};                                            // 35
	    type = min(35, type); type = max(1, type);
	    w = XtWindowToWidget(event.xany.display, event.xany.window);
	    fprintf(stderr, "%40s:%s\n", (w ? XtName(w) : "unknown"), event_name[type]);
	    fflush(stdout);
	}
#endif
	XtDispatchEvent(&event);
#else
	wxDoEvents();
#endif
    }
    return 0;
}

Bool wxApp::Pending(void)
{
    XFlush(wxAPP_DISPLAY);
#if 0
    return (XtAppPending(wxAPP_CONTEXT));
#else
    return (wxEventReady());
#endif
}

//-----------------------------------------------------------------------------
// The main function 
//-----------------------------------------------------------------------------

#include <X11/Shell.h>

static void wxCommonInit(void);

void wxInitNewToplevel(void)
{
  wxPutAppToplevel(XtAppCreateShell(wxAPP_NAME, wxAPP_CLASS,
				    applicationShellWidgetClass,
				    wxAPP_DISPLAY, NULL, 0));
}

typedef struct {
  char *flag;
  int arg_count;
} X_flag_entry;

X_flag_entry X_flags[] = {
  { "-display", 1 },
  { "-geometry", 1 },
  { "-bg", 1 },
  { "-background", 1 },
  { "-fg", 1 },
  { "-foreground", 1 },
  { "-fn", 1 },
  { "-font", 1 },
  { "-iconic", 0 },
  { "-name", 1 },
  { "-rv", 0 },
  { "-reverse", 0 },
  { "+rv", 0 },
  { "-selectionTimeout", 1 },
  { "-synchronous", 0 },
  { "-title", 1 },
  { "-xnllanguage", 1 },
  { "-xrm", 1 },
  { NULL, 0 }
};

int filter_x_readable(char **argv, int argc)
{
  int pos = 1, i;

  while (pos < argc) {
    for (i = 0; X_flags[i].flag; i++)
      if (!strcmp(X_flags[i].flag, argv[pos]))
	break;

    if (!X_flags[i].flag)
      return pos;
    else {
      int newpos = pos + X_flags[i].arg_count + 1;
      if (newpos > argc) {
	printf("%s: X Window System flag \"%s\" expects %d arguments, %d provided\n",
	       argv[0], argv[pos], X_flags[i].arg_count, argc - pos - 1);
	exit(-1);
      }
      pos = newpos;
    }
  }

  return pos;
}

int wxEntry(int argc, char *argv[])
{
    if (!wxTheApp) {
	wxFatalError("You have to define an instance of wxApp!");
    }

    // init private and public data
    /* MATTHEW: Set if not set... */
    if (!wxAPP_CLASS) wxAPP_CLASS = wxFileNameFromPath(argv[0]);
    if (!wxAPP_NAME)  wxAPP_NAME  = wxFileNameFromPath(argv[0]);

    int xargc = filter_x_readable(argv, argc), ate;
    ate = xargc - 1;

    wxPutAppToplevel(XtAppInitialize(
	&wxAPP_CONTEXT, wxAPP_CLASS,
	wxAppOptions, XtNumber(wxAppOptions),	// resource options
	&xargc, argv,				// command line arguments
	NULL,					// fallback resources
	NULL, 0));				// argument override for app-shell

    if (xargc != 1) {
      printf("%s: standard X Window System flag \"%s\" was rejected\n",
	     argv[0], argv[1]);
      exit(-1);
    }

    for (int i = ate + 1; i < argc; i++)
      argv[i - ate] = argv[i];
    argc -= ate;

    XtGetApplicationResources(wxAPP_TOPLEVEL, 
			      &wxAPP_DATA, 
			      wxAppResources, 
			      XtNumber(wxAppResources), 
			      NULL, 
			      0);
    wxAPP_DEBUGOUTPUT = wxAPP_DATA.debugOutput;

    wxTheApp->argc = argc;
    wxTheApp->argv = argv;

    // initialize global data
    wxCommonInit();

    wxTheApp->OnInit();

    return 0;
}

// If the compiler really, really wants main() to be in the main program
// a macro (IMPLEMENT_WXWIN_MAIN) is provided.

#if 0

#ifndef WX_FORCE_APP_CREATION
#define WX_FORCE_APP_CREATION 0
#endif

#if !defined(AIX) && !defined(AIX4) && !WX_FORCE_APP_CREATION
int main(int argc, char *argv[]) {
    return wxEntry(argc, argv);
}
#endif

#endif

//-----------------------------------------------------------------------------
// initialize and destroy global data
//-----------------------------------------------------------------------------

void wxCommonInit(void)
{
    wxAPP_DISPLAY   = XtDisplay(wxAPP_TOPLEVEL);
    wxAPP_SCREEN    = XtScreen(wxAPP_TOPLEVEL);
    wxAPP_ROOT	    = RootWindow(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));

    wxAPP_COLOURMAP = DEBUG_NEW wxColourMap(FALSE); // default colourmap

    wxBuffer = new char[BUFSIZ+512];

    wxTheFontNameDirectory.Initialize(); /* MATTHEW */

    wxInitializePrintSetupData();
    wxThePrintPaperDatabase = DEBUG_NEW wxPrintPaperDatabase;
    wxThePrintPaperDatabase->CreateDatabase();

    wxTheColourDatabase = DEBUG_NEW wxColourDatabase();
    wxThePenList = DEBUG_NEW wxPenList();
    wxTheBrushList = DEBUG_NEW wxBrushList();
    wxTheFontList = DEBUG_NEW wxFontList();
    wxTheBitmapList = DEBUG_NEW wxGDIList();

    wxNORMAL_FONT = DEBUG_NEW wxFont (12, wxMODERN, wxNORMAL, wxNORMAL);
    wxSMALL_FONT = DEBUG_NEW wxFont (10, wxSWISS, wxNORMAL, wxNORMAL);
    wxITALIC_FONT = DEBUG_NEW wxFont (12, wxROMAN, wxITALIC, wxNORMAL);
    wxSWISS_FONT = DEBUG_NEW wxFont (12, wxSWISS, wxNORMAL, wxNORMAL);
    wxSYSTEM_FONT = DEBUG_NEW wxFont (12, wxSYSTEM, wxNORMAL, wxNORMAL);

    wxRED_PEN = DEBUG_NEW wxPen ("RED", 0, wxSOLID);
    wxCYAN_PEN = DEBUG_NEW wxPen ("CYAN", 0, wxSOLID);
    wxGREEN_PEN = DEBUG_NEW wxPen ("GREEN", 0, wxSOLID);
    wxBLACK_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxSOLID);
    wxWHITE_PEN = DEBUG_NEW wxPen ("WHITE", 0, wxSOLID);
    wxTRANSPARENT_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxTRANSPARENT);
    wxBLACK_DASHED_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxSHORT_DASH);
    wxGREY_PEN = DEBUG_NEW wxPen ("GRAY", 0, wxSOLID);
    wxMEDIUM_GREY_PEN = DEBUG_NEW wxPen ("MEDIUM GRAY", 0, wxSOLID);
    wxLIGHT_GREY_PEN = DEBUG_NEW wxPen ("LIGHT GRAY", 0, wxSOLID);

    wxBLACK_PEN->Lock(1);
    wxWHITE_PEN->Lock(1);

    wxBLUE_BRUSH = DEBUG_NEW wxBrush ("BLUE", wxSOLID);
    wxGREEN_BRUSH = DEBUG_NEW wxBrush ("GREEN", wxSOLID);
    wxWHITE_BRUSH = DEBUG_NEW wxBrush ("WHITE", wxSOLID);
    wxBLACK_BRUSH = DEBUG_NEW wxBrush ("BLACK", wxSOLID);
    wxTRANSPARENT_BRUSH = DEBUG_NEW wxBrush ("BLACK", wxTRANSPARENT);
    wxCYAN_BRUSH = DEBUG_NEW wxBrush ("CYAN", wxSOLID);
    wxRED_BRUSH = DEBUG_NEW wxBrush ("RED", wxSOLID);
    wxGREY_BRUSH = DEBUG_NEW wxBrush ("GRAY", wxSOLID);
    wxMEDIUM_GREY_BRUSH = DEBUG_NEW wxBrush ("MEDIUM GRAY", wxSOLID);
    wxLIGHT_GREY_BRUSH = DEBUG_NEW wxBrush ("LIGHT GRAY", wxSOLID);

    wxBLACK_BRUSH->Lock(1);
    wxWHITE_BRUSH->Lock(1);

    wxBLACK = DEBUG_NEW wxColour ("BLACK");
    wxWHITE = DEBUG_NEW wxColour ("WHITE");
    wxGREY = DEBUG_NEW wxColour ("GRAY");
    wxRED = DEBUG_NEW wxColour ("RED");
    wxBLUE = DEBUG_NEW wxColour ("BLUE");
    wxGREEN = DEBUG_NEW wxColour ("GREEN");
    wxCYAN = DEBUG_NEW wxColour ("CYAN");
    wxLIGHT_GREY = DEBUG_NEW wxColour ("LIGHT GRAY");

    wxSTANDARD_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_ARROW);
    wxHOURGLASS_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_WAIT);
    wxCROSS_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_CROSS);
    wxIBEAM_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_IBEAM);
}
