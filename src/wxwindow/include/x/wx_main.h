/*
 * File:	wx_main.h
 * Purpose:	wxApp declaration and a few other functions (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_main.h	1.2 5/9/94" */

#ifndef wx_mainh
#define wx_mainh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_mgstr.h"
#include "wb_main.h"

#ifdef wx_motif
#include <Xm/Xm.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxApp;
#else

class wxFrame;

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxApp: public wxbApp
{
  DECLARE_DYNAMIC_CLASS(wxApp)

 public:
#ifdef wx_motif
  XtAppContext appContext;
  Widget topLevel;
  Bool keep_going;
#endif

  wxApp(wxlanguage_t language = wxLANGUAGE_ENGLISH);
  ~wxApp(void);
  int MainLoop(void);
  void ExitMainLoop(void);
  Bool Initialized(void);
  virtual Bool Pending(void) ;
  virtual void Dispatch(void) ;
#ifdef wx_motif
  // This handler is called, when a property change event occurs
  virtual void HandlePropertyChange(XEvent *event); // added by steve, 29.10.94;
                                                    // to be redefined in subclass
#endif
};

// added by steve for VMS, see wx_main.cpp for details (Dez.1994)
#ifdef VMS
void wxPostDelete(wxObject *object);
#endif

#endif // IN_CPROTO
#endif // wx_mainh
