/*
 * File:	wb_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_main.cc,v 1.4 1999/11/22 17:23:15 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"

#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_dc.h"
#include "wx_dialg.h"
#include "wx_types.h"
#include "wx_dcps.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include "wx_sysev.h"

#include <string.h>

extern char *wxBuffer;

wxApp *wxTheApp = NULL;

wxbApp::wxbApp(wxlanguage_t )
{
  __type = wxTYPE_APP;
  wx_class = NULL;
  wantDebugOutput = TRUE ;
}

wxbApp::~wxbApp(void)
{
}

Bool wxbApp::Initialized(void)
{
  return FALSE;
}

wxFrame *wxbApp::OnInit(void)
{
  return NULL;
}

int wxbApp::OnExit(void)
{
  return 0;
}

// prototypes for wxREGGLOB'ing functions:
void wxRegisterSplinePointList();
void wxRegisterAbortWindow();
void wxRegisterEntered();
void wxRegisterOldFrontWindow();
void wxRegisterCurCursor();


void wxCommonInit(void)
{
  wxREGGLOB(wxBuffer);
  wxBuffer = new char[1500];
  wxREGGLOB(wxTheColourDatabase);
  wxTheColourDatabase = new wxColourDatabase(wxKEY_STRING);
  wxTheColourDatabase->Initialize();
  wxInitializeFontNameDirectory();
  wxInitializeStockObjects();
  wxInitStandardTypes();
  wxREGGLOB(wxThePrintPaperDatabase);
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
  wxREGGLOB(wxWindow::gMouseWindow);
  wxRegisterAbortWindow();
  wxRegisterSplinePointList();
  wxRegisterEntered();
  wxRegisterOldFrontWindow();
  wxRegisterCurCursor();
}

void wxCommonCleanUp(void)
{
  wxDeleteStockObjects() ;
  // Destroy all GDI lists, etc.
  delete wxTheBrushList;
  delete wxThePenList;
  delete wxTheFontList;

  delete wxThePrintPaperDatabase;

  delete wxTheColourDatabase;

  delete[] wxBuffer;
}

