/*
 * File:	wx_mnuit.h
 * Purpose:	Declares menu item class (Windows)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_mnuit.h	1.2 5/9/94" */

#ifndef wx_mnuith
#define wx_mnuith

#include "common.h"
#include "wx_obj.h"
#include "wb_mnuit.h"

#ifdef IN_CPROTO
typedef       void    *wxMenuItem ;
#else

class wxMenuItem: public wbMenuItem
{
  DECLARE_DYNAMIC_CLASS(wxMenuItem)

 public:
  Bool checkable ;
  inline wxMenuItem(void) { }
  inline ~wxMenuItem(void) { }
};

#endif // IN_CPROTO
#endif // wx_mnuith

