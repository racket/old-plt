/*								-*- C++ -*-
 * $Id: GlobalData.h,v 1.2 1998/04/11 13:57:29 mflatt Exp $
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

#ifndef GlobalData_h
#define GlobalData_h

#ifdef __GNUG__
#pragma interface
#endif

class wxApp;
class wxBrush;
class wxBrushList;
class wxColour;
class wxColourDatabase;
class wxColourMap;
class wxCursor;
class wxFont;
class wxFontList;
class wxGDIList;
class wxList;
class wxChildList;
class wxPen;
class wxPenList;
class wxPrintPaperDatabase;
class wxPrintSetupData;
class wxTypeTree;
class wxWindow;

//-- Application --------------------------------------------------------------

extern wxApp* wxTheApp;

extern char* wxAPP_CLASS;
extern char* wxAPP_NAME;
extern Bool  wxAPP_DEBUGOUTPUT;

extern wxColourMap* wxAPP_COLOURMAP;

#if defined (Uses_XtIntrinsic) || defined (Uses_XtIntrinsicP)
extern Widget wxGetAppToplevel(void);
extern void wxPutAppToplevel(Widget);
#define wxAPP_TOPLEVEL (wxGetAppToplevel())
#if 0
extern Widget wxAPP_TOPLEVEL;
#endif
extern XtAppContext wxAPP_CONTEXT;
#endif

#if defined (Uses_XtIntrinsic) || defined (Uses_XtIntrinsicP) || defined (Uses_XLib)
extern Display*	wxAPP_DISPLAY;
extern Screen*	wxAPP_SCREEN;
extern Window   wxAPP_ROOT;
#endif

//-- DialogBox ----------------------------------------------------------------

#if 0
// A stack of modal_showing flags, since we can't rely
// on accessing wxDialogBox::modal_showing within
// wxDialogBox::Show in case a callback has deleted the wxDialogBox.
extern wxList wxModalShowingStack;
extern wxList wxModalFrames;
#else
wxWindow *wxGetModalWindow(wxObject*);
void wxPutModalWindow(wxObject*,wxWindow *);
#endif

//-- Events --------------------------------------------------------------------

extern wxList wxEventClassList;
extern wxList wxEventNameList;
extern wxList wxPrimaryEventHandlerList;
extern wxList wxPreEventHandlerList;
extern wxList wxPostEventHandlerList;

//-- Frame --------------------------------------------------------------------

extern wxChildList *wxGetTopLevelWindowsList(wxObject*);
#define wxTopLevelFrames(w) (wxGetTopLevelWindowsList(w))
#if 0
extern wxList wxTopLevelFrames;
#endif

extern void *wxGetContextForFrame();

//-- GDI collections ----------------------------------------------------------

extern wxColourDatabase* wxTheColourDatabase;
extern wxPenList*	 wxThePenList;
extern wxBrushList*	 wxTheBrushList;
extern wxFontList*	 wxTheFontList;
extern wxGDIList*	 wxTheBitmapList;

//-- IPC ----------------------------------------------------------------------

#if USE_IPC
extern Bool  wxIPCInitialized;
extern char* wxDefaultIPCBuffer;
extern int   wxDefaultIPCBufferSize;
#endif

//-- misc ---------------------------------------------------------------------

extern char* wxBuffer;
extern int   wxCursorBusy;

//-- Printing -----------------------------------------------------------------

extern wxPrintPaperDatabase* wxThePrintPaperDatabase;

//-- Prolog IO ----------------------------------------------------------------

#if USE_PROLOGIO
// Error handler function definition. If app returns TRUE,
// carry on processing.
typedef Bool (*proioErrorHandler) (int errorType, char *msg);
extern proioErrorHandler currentProioErrorHandler;
// Temporary variable for communicating between read.cc and YACC/LEX
class PrologDatabase;
extern PrologDatabase* hyPrologDatabase;
#endif

//-- Resources ----------------------------------------------------------------

#if USE_RESOURCES
#if defined (Uses_XtIntrinsic) || defined (Uses_XtIntrinsicP) || defined (Uses_XLib)
extern XrmDatabase wxResourceDatabase;
#endif
extern wxList wxResourceCache;
#endif

//-- RPC ----------------------------------------------------------------------

#if USE_RPC
class PrologDatabase;
extern PrologDatabase *rpcPrologDatabase;
#endif

//-- simple language support---------------------------------------------------

extern char **wx_msg_str;

//-- stock objects ------------------------------------------------------------

extern wxFont* wxNORMAL_FONT;
extern wxFont* wxSMALL_FONT;
extern wxFont* wxITALIC_FONT;
extern wxFont* wxSWISS_FONT;
extern wxFont* wxSYSTEM_FONT;
			
extern wxPen* wxRED_PEN;
extern wxPen* wxCYAN_PEN;
extern wxPen* wxGREEN_PEN;
extern wxPen* wxBLACK_PEN;
extern wxPen* wxWHITE_PEN;
extern wxPen* wxTRANSPARENT_PEN;
extern wxPen* wxBLACK_DASHED_PEN;
extern wxPen* wxGREY_PEN;
extern wxPen* wxMEDIUM_GREY_PEN;
extern wxPen* wxLIGHT_GREY_PEN;
			
extern wxBrush* wxBLUE_BRUSH;
extern wxBrush* wxGREEN_BRUSH;
extern wxBrush* wxWHITE_BRUSH;
extern wxBrush* wxBLACK_BRUSH;
extern wxBrush* wxGREY_BRUSH;
extern wxBrush* wxMEDIUM_GREY_BRUSH;
extern wxBrush* wxLIGHT_GREY_BRUSH;
extern wxBrush* wxTRANSPARENT_BRUSH;
extern wxBrush* wxCYAN_BRUSH;
extern wxBrush* wxRED_BRUSH;
			
extern wxColour* wxBLACK;
extern wxColour* wxWHITE;
extern wxColour* wxGREY;
extern wxColour* wxRED;
extern wxColour* wxBLUE;
extern wxColour* wxGREEN;
extern wxColour* wxCYAN;
extern wxColour* wxLIGHT_GREY;

extern wxCursor* wxSTANDARD_CURSOR;
extern wxCursor* wxHOURGLASS_CURSOR;
extern wxCursor* wxCROSS_CURSOR;
extern wxCursor* wxIBEAM_CURSOR;

//-- Types --------------------------------------------------------------------

extern wxTypeTree wxAllTypes;

#endif // GlobalData_h
