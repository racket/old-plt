/*
 * File:	wx_mnuit.h
 * Purpose:	Declares menu item class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_mnuit.h	1.2 5/9/94" */

#ifndef wx_mnuith
#define wx_mnuith

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_mnuit.h"

#ifdef IN_CPROTO
typedef       void    *wxMenuItem ;
#else

class wxMenuBar ;
class wxMenu ;

class wxMenuItem: public wbMenuItem
{
  DECLARE_DYNAMIC_CLASS(wxMenuItem)

 public:
  Bool checkable ;
  Bool isChecked ;
  Bool isEnabled ;
#ifdef wx_motif
  Widget buttonWidget; // The actual string, so we can grey it etc.
#endif
#ifdef wx_xview
  char *uncheckedString;
  char *checkedString;
#endif
  wxMenuItem(void)
  {
    isChecked = FALSE;
    isEnabled = TRUE;
    checkable = TRUE;
#ifdef wx_motif
    buttonWidget = 0;
#endif
#ifdef wx_xview
    uncheckedString = NULL;
    checkedString = NULL;
#endif
  }
  ~wxMenuItem(void)
  {
#ifdef wx_xview
    if (uncheckedString)
      delete[] uncheckedString;
    if (checkedString)
      delete[] checkedString;
#endif
  }
#ifdef wx_motif
  void CreateItem(Widget pulldown,wxMenuBar *menuBar, wxMenu *topMenu) ;
  /* MATTHEW: [6] */
  void DestroyItem(Bool full);  /* MATTHEW: [13] */
#endif
};

#endif // IN_CPROTO
#endif // wx_mnuith

