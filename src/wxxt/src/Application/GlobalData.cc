/*								-*- C++ -*-
 * $Id: GlobalData.cc,v 1.3 1996/01/11 10:26:42 markus Exp $
 *
 * Purpose: global data for an application (UNSHARED)
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
#pragma implementation "GlobalData.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxList
#define  Uses_wxTypeTree
#include "wx.h"

//-- Application data wxWindows -----------------------------------------------

class wxApp;
wxApp* wxTheApp = 0;

char*  wxAPP_CLASS = 0;
char*  wxAPP_NAME = 0;
Bool   wxAPP_DEBUGOUTPUT = FALSE;

wxColourMap* wxAPP_COLOURMAP = 0;

//-- Application data wxWindows (Xt) ------------------------------------------

#if 0
Widget	     wxAPP_TOPLEVEL;
#endif
XtAppContext wxAPP_CONTEXT;

//-- Application data wxWindows (XLib) ----------------------------------------

Display* wxAPP_DISPLAY = 0;
Screen*	 wxAPP_SCREEN = 0;
Window   wxAPP_ROOT = 0;

//-- DialogBox ----------------------------------------------------------------

#if 0
// A stack of modal_showing flags, since we can't rely
// on accessing wxDialogBox::modal_showing within
// wxDialogBox::Show in case a callback has deleted the wxDialogBox.
wxList wxModalShowingStack;
wxList wxModalFrames;
#endif

//-- Events --------------------------------------------------------------------

wxList wxEventClassList(wxKEY_INTEGER);
wxList wxEventNameList(wxKEY_INTEGER);
wxList wxPrimaryEventHandlerList;
wxList wxPreEventHandlerList;
wxList wxPostEventHandlerList;

//-- Frames -------------------------------------------------------------------

#if 0
wxList wxTopLevelFrames;
#endif

//-- GDI collections ----------------------------------------------------------

wxColourDatabase* wxTheColourDatabase = 0;
wxPenList*        wxThePenList = 0;
wxBrushList*	  wxTheBrushList = 0;
wxFontList*	  wxTheFontList = 0;
wxGDIList*	  wxTheBitmapList = 0;

//-- IPC ----------------------------------------------------------------------

#if USE_IPC
Bool  wxIPCInitialized       = FALSE;
char* wxDefaultIPCBuffer     = NULL;
int   wxDefaultIPCBufferSize = 4000;
#endif

//-- misc ---------------------------------------------------------------------

char* wxBuffer = 0;
int   wxCursorBusy = 0;

//-- Printing -----------------------------------------------------------------

wxPrintPaperDatabase* wxThePrintPaperDatabase = 0;
wxPrintSetupData*     wxThePrintSetupData = 0;

//-- Prolog IO ----------------------------------------------------------------

#if USE_PROLOGIO
// Error handler function definition. If app returns TRUE,
// carry on processing.
proioErrorHandler currentProioErrorHandler;
// Temporary variable for communicating between read.cc and YACC/LEX
class PrologDatabase;
PrologDatabase* hyPrologDatabase = NULL;
#endif

//-- Resources ----------------------------------------------------------------

#if USE_RESOURCES
XrmDatabase wxResourceDatabase;
wxList wxResourceCache(wxKEY_STRING);
#endif

//-- RPC ----------------------------------------------------------------------

#if USE_RPC
class PrologDatabase;
PrologDatabase *rpcPrologDatabase = NULL;
#endif

//-- simple language support---------------------------------------------------

char **wx_msg_str = NULL;

//-- stock objects ------------------------------------------------------------

wxBrush* wxBLUE_BRUSH = 0;
wxBrush* wxGREEN_BRUSH = 0;
wxBrush* wxWHITE_BRUSH = 0;
wxBrush* wxBLACK_BRUSH = 0;
wxBrush* wxGREY_BRUSH = 0;
wxBrush* wxMEDIUM_GREY_BRUSH = 0;
wxBrush* wxLIGHT_GREY_BRUSH = 0;
wxBrush* wxTRANSPARENT_BRUSH = 0;
wxBrush* wxCYAN_BRUSH = 0;
wxBrush* wxRED_BRUSH = 0;

wxColour* wxBLACK = 0;
wxColour* wxWHITE = 0;
wxColour* wxGREY = 0;
wxColour* wxRED = 0;
wxColour* wxBLUE = 0;
wxColour* wxGREEN = 0;
wxColour* wxCYAN = 0;
wxColour* wxLIGHT_GREY = 0;

wxCursor* wxSTANDARD_CURSOR = 0;
wxCursor* wxHOURGLASS_CURSOR = 0;
wxCursor* wxCROSS_CURSOR = 0;

wxFont* wxNORMAL_FONT = 0;
wxFont* wxSMALL_FONT = 0;
wxFont* wxITALIC_FONT = 0;
wxFont* wxSWISS_FONT = 0;
wxFont* wxSYSTEM_FONT = 0;

wxPen* wxRED_PEN = 0;
wxPen* wxCYAN_PEN = 0;
wxPen* wxGREEN_PEN = 0;
wxPen* wxBLACK_PEN = 0;
wxPen* wxWHITE_PEN = 0;
wxPen* wxTRANSPARENT_PEN = 0;
wxPen* wxBLACK_DASHED_PEN = 0;
wxPen* wxGREY_PEN = 0;
wxPen* wxMEDIUM_GREY_PEN = 0;
wxPen* wxLIGHT_GREY_PEN = 0;

//-- Types --------------------------------------------------------------------

wxTypeTree wxAllTypes;

