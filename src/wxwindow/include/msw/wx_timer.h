/*
 * File:	wx_timer.h
 * Purpose:	wxTimer - provides simple timer functionality
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_timer.h	1.2 5/9/94" */

#ifndef wx_timerh
#define wx_timerh

#include "common.h"
#include "wx_obj.h"
#include "wb_timer.h"

#ifdef IN_CPROTO
typedef       void    *wxTimer ;
#else

class wxTimer: public wxbTimer
{
  DECLARE_DYNAMIC_CLASS(wxTimer)

#if 0
 public:
  long id;
#endif
 public:
  wxTimer(void);
  ~wxTimer(void);
  virtual Bool Start(int milliseconds = -1,Bool one_shot = FALSE); // Start timer
  virtual void Stop(void);                   // Stop timer

  void Dequeue(void);
};

#endif // IN_CPROTO
#endif // wx_timerh
