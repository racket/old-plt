/*
 * File:	wb_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_main.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

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

#include "wx_sysev.h"
#include "wx_dcps.h"
#endif

#include <string.h>

extern char *wxBuffer;

wxApp *wxTheApp = NULL;

wxbApp::wxbApp(wxlanguage_t )
{
  __type = wxTYPE_APP;
  wx_class = NULL;
  wantDebugOutput = TRUE ;
  appName = NULL;
  argc = 0;
  argv = NULL;
  death_processed = FALSE;
#ifdef wx_msw
  printMode = wxPRINT_WINDOWS;
#else
  printMode = wxPRINT_POSTSCRIPT;
#endif
  work_proc = NULL;
  wx_frame = NULL;
}

wxbApp::~wxbApp(void)
{
  if (appName)
    delete[] appName;
  if (wx_class)
    delete[] wx_class;
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

char *wxbApp::GetAppName(void)
{
  if (appName)
    return appName;
  else if (wx_class)
    return wx_class;
  else return NULL;
}

void wxbApp::SetAppName(char *name)
{
  if (name == appName)
    return;
  if (appName)
    delete[] appName;
  if (name)
    appName = copystring(name);
}

char *wxbApp::GetClassName(void)
{
  return wx_class;
}

void wxbApp::SetClassName(char *name)
{
  if (name == wx_class)
    return;
  if (wx_class)
    delete[] wx_class;
  if (name)
    wx_class = copystring(name);
}

wxWindow *wxbApp::GetTopWindow(void)
{
  return wx_frame;
}

void wxCommonInit(void)
{
#ifdef wx_msw
  wxBuffer = new char[1500];
#else
  wxBuffer = new char[BUFSIZ + 512];
#endif
#if USE_DYNAMIC_CLASSES
  wxClassInfo::InitializeClasses();
#endif
  
  wxTheFontNameDirectory.Initialize(); /* MATTHEW: [5] (Forgot marker earlier) */

  wxTheColourDatabase = new wxColourDatabase(wxKEY_STRING);
  wxTheColourDatabase->Initialize();
  wxInitializeStockObjects();
  wxInitStandardTypes();
  wxInitStandardEvents();

  // For PostScript printing
#if USE_POSTSCRIPT
  wxInitializePrintSetupData();
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
  wxThePrintPaperDatabase->CreateDatabase();
#endif

}

void wxCommonCleanUp(void)
{
  wxDeleteStockObjects() ;
  // Destroy all GDI lists, etc.
  delete wxTheBrushList;
  delete wxThePenList;
  delete wxTheFontList;
  delete wxTheBitmapList;
  delete wxTheColourDatabase;
#if USE_POSTSCRIPT
  wxInitializePrintSetupData(FALSE);
  delete wxThePrintPaperDatabase;
  wxThePrintPaperDatabase = NULL;
#endif

  wxDeleteEventLists() ;

  delete[] wxBuffer;
}

