/*
 * File:	wx_ipcob.h
 * Purpose:	IPC object
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_ipcob.h	1.2 5/9/94" */

#ifndef wx_ipcobh
#define wx_ipcobh

#include "wx_setup.h"
#if USE_IPC
#include "wb_ipcob.h"

#include <ddeml.h>

#ifdef IN_CPROTO
typedef       void    *wxIPCObject ;
#else

class wxConnection;
class wxIPCObject: public wxbIPCObject
{
  DECLARE_ABSTRACT_CLASS(wxIPCObject)

 public:
  wxIPCObject(void);
  ~wxIPCObject(void);

  // Find/delete wxConnection corresponding to the HCONV
  wxConnection *FindConnection(HCONV conv);
  Bool DeleteConnection(HCONV conv);
};

#endif // IN_CPROTO
#endif // USE_IPC
#endif // wx_ipcob.h
