/*
 * File:	wx_main.h
 * Purpose:	wxApp declaration and a few other functions.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_main.h	1.2 5/9/94" */

#ifndef wx_mainh
#define wx_mainh

#include "common.h"
#include "wx_obj.h"
#include "wx_stdev.h"
#include "wx_mgstr.h"
#include "wb_main.h"

#ifdef IN_CPROTO
typedef       void    *wxApp ;
#else

class wxFrame;
class wxKeyEvent;

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxApp: public wxbApp
{
  DECLARE_DYNAMIC_CLASS(wxApp)

 public:
  HANDLE hInstance;
  MSG current_msg;
  BOOL keep_going ;

  // this variable holds the display mode, which is
  // passed to the WinMain function.
  int nCmdShow;

  wxApp(wxlanguage_t language = wxLANGUAGE_ENGLISH);
  ~wxApp(void);

  virtual int MainLoop(void);
  void ExitMainLoop(void);
  Bool Initialized(void);
  virtual Bool Pending(void) ;
  virtual void Dispatch(void) ;

  virtual BOOL DoMessage(void);
  virtual BOOL ProcessMessage(MSG* pMsg);
  virtual BOOL OnIdle(void);

  // Windows specific. Intercept keyboard input.
  virtual Bool OnCharHook(wxKeyEvent& event);
};

extern HINSTANCE wxhInstance;

#endif // IN_CPROTO
#endif
