/*								-*- C++ -*-
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
  wxREGGLOB(wxTheApp);
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
  Widget tl;

  tl = XtAppCreateShell(wxAPP_NAME, wxAPP_CLASS,
		       applicationShellWidgetClass,
			wxAPP_DISPLAY, NULL, 0);
  wxPutAppToplevel(tl);
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
    for (i = 0; X_flags[i].flag; i++) {
      if (!strcmp(X_flags[i].flag, argv[pos]))
	break;
    }

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
  int xargc, ate;

  if (!wxTheApp) {
    wxFatalError("You have to define an instance of wxApp!");
  }

  // init private and public data
  /* Set if not set... */
  if (!wxAPP_CLASS)
    wxAPP_CLASS = wxFileNameFromPath(argv[0]);
  if (!wxAPP_NAME)
    wxAPP_NAME  = wxFileNameFromPath(argv[0]);

  xargc = filter_x_readable(argv, argc);
  ate = xargc - 1;
  
  wxPutAppToplevel(XtAppInitialize(&wxAPP_CONTEXT, wxAPP_CLASS,
				   NULL, 0,
				   &xargc, argv, // command line arguments
				   NULL,         // fallback resources
				   NULL, 0));    // argument override for app-shell

  if (xargc != 1) {
    printf("%s: standard X Window System flag \"%s\" was rejected\n",
	   argv[0], argv[1]);
    exit(-1);
  }
  
  for (int i = ate + 1; i < argc; i++) {
    argv[i - ate] = argv[i];
  }
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

    wxREGGLOB(wxAPP_COLOURMAP);
    wxAPP_COLOURMAP = DEBUG_NEW wxColourMap(FALSE); // default colourmap

    wxREGGLOB(wxBuffer);
    wxBuffer = new char[BUFSIZ+512];

    wxREGGLOB(wxResourceCache);
    wxResourceCache = new wxList(wxKEY_STRING);

    wxREGGLOB(wxAllTypes);
    wxAllTypes = new wxTypeTree;

    wxInitializeFontNameDirectory();

    wxInitializePrintSetupData();
    wxREGGLOB(wxThePrintPaperDatabase);
    wxThePrintPaperDatabase = DEBUG_NEW wxPrintPaperDatabase;
    wxThePrintPaperDatabase->CreateDatabase();

    wxREGGLOB(wxTheColourDatabase);
    wxTheColourDatabase = DEBUG_NEW wxColourDatabase();
    wxREGGLOB(wxThePenList);
    wxThePenList = DEBUG_NEW wxPenList();
    wxREGGLOB(wxTheBrushList);
    wxTheBrushList = DEBUG_NEW wxBrushList();
    wxREGGLOB(wxTheFontList);
    wxTheFontList = DEBUG_NEW wxFontList();

    wxREGGLOB(wxNORMAL_FONT);
    wxNORMAL_FONT = DEBUG_NEW wxFont (12, wxMODERN, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxSMALL_FONT);
    wxSMALL_FONT = DEBUG_NEW wxFont (10, wxSWISS, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxITALIC_FONT);
    wxITALIC_FONT = DEBUG_NEW wxFont (12, wxROMAN, wxITALIC, wxNORMAL);
    wxREGGLOB(wxSWISS_FONT);
    wxSWISS_FONT = DEBUG_NEW wxFont (12, wxSWISS, wxNORMAL, wxNORMAL);
    wxREGGLOB(wxSYSTEM_FONT);
    wxSYSTEM_FONT = DEBUG_NEW wxFont (12, wxSYSTEM, wxNORMAL, wxNORMAL);

    wxREGGLOB(wxRED_PEN);
    wxRED_PEN = DEBUG_NEW wxPen ("RED", 0, wxSOLID);
    wxREGGLOB(wxCYAN_PEN);
    wxCYAN_PEN = DEBUG_NEW wxPen ("CYAN", 0, wxSOLID);
    wxREGGLOB(wxGREEN_PEN);
    wxGREEN_PEN = DEBUG_NEW wxPen ("GREEN", 0, wxSOLID);
    wxREGGLOB(wxBLACK_PEN);
    wxBLACK_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxSOLID);
    wxREGGLOB(wxWHITE_PEN);
    wxWHITE_PEN = DEBUG_NEW wxPen ("WHITE", 0, wxSOLID);
    wxREGGLOB(wxTRANSPARENT_PEN);
    wxTRANSPARENT_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxTRANSPARENT);
    wxREGGLOB(wxBLACK_DASHED_PEN);
    wxBLACK_DASHED_PEN = DEBUG_NEW wxPen ("BLACK", 0, wxSHORT_DASH);
    wxREGGLOB(wxGREY_PEN);
    wxGREY_PEN = DEBUG_NEW wxPen ("GRAY", 0, wxSOLID);
    wxREGGLOB(wxMEDIUM_GREY_PEN);
    wxMEDIUM_GREY_PEN = DEBUG_NEW wxPen ("MEDIUM GRAY", 0, wxSOLID);
    wxREGGLOB(wxLIGHT_GREY_PEN);
    wxLIGHT_GREY_PEN = DEBUG_NEW wxPen ("LIGHT GRAY", 0, wxSOLID);

    wxBLACK_PEN->Lock(1);
    wxWHITE_PEN->Lock(1);

    wxREGGLOB(wxBLUE_BRUSH);
    wxBLUE_BRUSH = DEBUG_NEW wxBrush ("BLUE", wxSOLID);
    wxREGGLOB(wxGREEN_BRUSH);
    wxGREEN_BRUSH = DEBUG_NEW wxBrush ("GREEN", wxSOLID);
    wxREGGLOB(wxWHITE_BRUSH);
    wxWHITE_BRUSH = DEBUG_NEW wxBrush ("WHITE", wxSOLID);
    wxREGGLOB(wxBLACK_BRUSH);
    wxBLACK_BRUSH = DEBUG_NEW wxBrush ("BLACK", wxSOLID);
    wxREGGLOB(wxTRANSPARENT_BRUSH);
    wxTRANSPARENT_BRUSH = DEBUG_NEW wxBrush ("BLACK", wxTRANSPARENT);
    wxREGGLOB(wxCYAN_BRUSH);
    wxCYAN_BRUSH = DEBUG_NEW wxBrush ("CYAN", wxSOLID);
    wxREGGLOB(wxRED_BRUSH);
    wxRED_BRUSH = DEBUG_NEW wxBrush ("RED", wxSOLID);
    wxREGGLOB(wxGREY_BRUSH);
    wxGREY_BRUSH = DEBUG_NEW wxBrush ("GRAY", wxSOLID);
    wxREGGLOB(wxMEDIUM_GREY_BRUSH);
    wxMEDIUM_GREY_BRUSH = DEBUG_NEW wxBrush ("MEDIUM GRAY", wxSOLID);
    wxREGGLOB(wxLIGHT_GREY_BRUSH);
    wxLIGHT_GREY_BRUSH = DEBUG_NEW wxBrush ("LIGHT GRAY", wxSOLID);

    wxBLACK_BRUSH->Lock(1);
    wxWHITE_BRUSH->Lock(1);

    wxREGGLOB(wxBLACK);
    wxBLACK = DEBUG_NEW wxColour ("BLACK");
    wxREGGLOB(wxWHITE);
    wxWHITE = DEBUG_NEW wxColour ("WHITE");
    wxREGGLOB(wxGREY);
    wxGREY = DEBUG_NEW wxColour ("GRAY");
    wxREGGLOB(wxRED);
    wxRED = DEBUG_NEW wxColour ("RED");
    wxREGGLOB(wxBLUE);
    wxBLUE = DEBUG_NEW wxColour ("BLUE");
    wxREGGLOB(wxGREEN);
    wxGREEN = DEBUG_NEW wxColour ("GREEN");
    wxREGGLOB(wxCYAN);
    wxCYAN = DEBUG_NEW wxColour ("CYAN");
    wxREGGLOB(wxLIGHT_GREY);
    wxLIGHT_GREY = DEBUG_NEW wxColour ("LIGHT GRAY");

    wxWHITE_PIXEL = wxWHITE->GetPixel(wxAPP_COLOURMAP);
    wxBLACK_PIXEL = wxBLACK->GetPixel(wxAPP_COLOURMAP);;
    wxGREY_PIXEL = wxGREY->GetPixel(wxAPP_COLOURMAP);;

    wxREGGLOB(wxSTANDARD_CURSOR);
    wxSTANDARD_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_ARROW);
    wxREGGLOB(wxHOURGLASS_CURSOR);
    wxHOURGLASS_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_WAIT);
    wxREGGLOB(wxCROSS_CURSOR);
    wxCROSS_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_CROSS);
    wxREGGLOB(wxIBEAM_CURSOR);
    wxIBEAM_CURSOR = DEBUG_NEW wxCursor (wxCURSOR_IBEAM);
}
