///////////////////////////////////////////////////////////////////////////////
// File:	wx_main.cc
// Purpose:	wxApp implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";
#ifndef OS_X
  #include <Windows.h>
  #include <Folders.h>
#endif
#include <stdlib.h>

#include "wx_item.h"
#include "wx_main.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wxMacDC.h"
#include "PSDC.h"

void wxCleanUp(void);

///////////////////////////////////////////////////////////////////////////////
// The procedure CreateApp initializes the whole application.
// You must declare and define it in the file that
// implements your derived class of the base class wxApp.
// It will look like this, where MyApp is the name of your derived class.
// You must use the name CreateApp for this procedure;
// you must use the name you chose for your derived class,
// which does not have to be MyApp.
//
//	void CreateApp(void);	// This procedure initializes the whole application
//	void CreateApp(void)
//	{
//		new MyApp;
//	}
//
///////////////////////////////////////////////////////////////////////////////
void CreateApp(void);

extern "C" {
  extern char *scheme_os_getcwd(char *buf, int buflen, int *actlen, int noexn);
  extern char *scheme_mac_spec_to_path(FSSpec *spec);
};

char *wxmac_startup_directory;

extern void wxInitResources(char *s);

//-----------------------------------------------------------------------------
int wxEntry(int argc, char* argv[])
{
	// CreateApp();	// This procedure initializes the whole application

	if (!wxTheApp)
	{
		exit(0);
	}

	wxTheApp->argc = argc;
	wxTheApp->argv = argv;

//	if (!wxTheApp->wx_class) wxTheApp->wx_class = macCopyString(argv[0]);

	wxmac_startup_directory = scheme_os_getcwd(NULL, 0, NULL, 1);
	wxREGGLOB(wxmac_startup_directory);
	
	FSSpec spec;
    SInt16 vRefNum;
    SInt32 dirID;
    const Str255 fileName = "\p";
#ifdef OS_X
        static char path_divider = '/';
#else
        static char path_divider = ':';
#endif
        
	if (FindFolder(kOnSystemDisk, 'pref', kCreateFolder, &vRefNum, &dirID) == noErr) {
	  char *home;
	  
      FSMakeFSSpec(vRefNum,dirID,fileName,&spec);
	  home = scheme_mac_spec_to_path(&spec);
	  int l = strlen(home);
	  char *s = new char[l + 15];
	  memcpy(s, home, l);
	  if (s[l - 1] != path_divider) {
	    s[l++] = path_divider;
	  }
	  strcpy(s + l, "mred.fnt");
      wxInitResources(s);
    }

	wxCommonInit();
	wxInitializePrintSetupData(1);

	wxTheApp->OnInit();
	
	return 0;
}


//-----------------------------------------------------------------------------
void wxCleanUp(void)
{// Cleans up any wxWindows internal structures left lying around
	wxCommonCleanUp();
	wxFlushResources();
}

//-----------------------------------------------------------------------------
Bool wxYield(void)
{ // Yield to incoming messages

    while (wxTheApp->Pending())
      wxTheApp->Dispatch();
      
	return TRUE;
}

//-----------------------------------------------------------------------------
void wxExit(void)
{
	int retValue = 0;
	if (wxTheApp) retValue = wxTheApp->OnExit();
	wxCleanUp();

	exit(retValue);
}
