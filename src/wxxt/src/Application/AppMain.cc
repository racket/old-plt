/*								-*- C++ -*-
 * $Id: AppMain.cc,v 1.11 1999/11/19 22:02:37 mflatt Exp $
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
#define  Uses_wxTypeTree
#include "wx.h"
#include "widgets.h" // for X11/StringDefs.h

//-----------------------------------------------------------------------------
// wxApp implementation
//-----------------------------------------------------------------------------

wxApp::wxApp()
{
  __type = wxTYPE_APP;
  
  // no application and/or application not initialized
  initialized = FALSE;
  wxTheApp = this;
}

void wxApp::Dispatch(void)
{
  wxDoNextEvent();
}

int wxApp::MainLoop(void)
{
    keep_going = TRUE;
    while (keep_going) {
      wxDoEvents();
    }
    return 0;
}

Bool wxApp::Pending(void)
{
    XFlush(wxAPP_DISPLAY);
    return (wxEventReady());
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
    /* Set if not set... */
    if (!wxAPP_CLASS) wxAPP_CLASS = wxFileNameFromPath(argv[0]);
    if (!wxAPP_NAME)  wxAPP_NAME  = wxFileNameFromPath(argv[0]);

    int xargc = filter_x_readable(argv, argc), ate;
    ate = xargc - 1;

    wxPutAppToplevel(XtAppInitialize(
	&wxAPP_CONTEXT, wxAPP_CLASS,
	NULL, 0,
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

    wxTheApp->argc = argc;
    wxTheApp->argv = argv;

    // initialize global data
    wxCommonInit();

    wxTheApp->OnInit();

    return 0;
}

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

    wxResourceCache = new wxList(wxKEY_STRING);

    wxAllTypes = new wxTypeTree;

    wxInitializeFontNameDirectory();

    wxInitializePrintSetupData();
    wxThePrintPaperDatabase = DEBUG_NEW wxPrintPaperDatabase;
    wxThePrintPaperDatabase->CreateDatabase();

    wxTheColourDatabase = DEBUG_NEW wxColourDatabase();
    wxThePenList = DEBUG_NEW wxPenList();
    wxTheBrushList = DEBUG_NEW wxBrushList();
    wxTheFontList = DEBUG_NEW wxFontList();

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
