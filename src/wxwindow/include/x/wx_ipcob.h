/*
 * File:	wx_ipcob.h
 * Purpose:	Interprocess communication implementation, wxIPCObject (X)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_ipcob.h	1.2 5/9/94" */

#ifndef wx_ipcobh
#define wx_ipcobh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_IPC

#include "wb_ipcob.h"

#ifdef IN_CPROTO
typedef       void    *wxIPCObject ;
#else

class wxIPCObject: public wxbIPCObject
{
  DECLARE_ABSTRACT_CLASS(wxIPCObject)

 public:
  wxIPCObject();
  ~wxIPCObject();
};

#endif // IN_CPROTO
#endif // USE_IPC
#endif // wx_ipcob.h
