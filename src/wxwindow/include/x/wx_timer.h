/*
 * File:	wx_timer.h
 * Purpose:	wxTimer - provides simple timer functionality (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_timer.h	1.2 5/9/94" */

#ifndef wx_timerh
#define wx_timerh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_timer.h"
#include "sys/time.h"

#ifdef IN_CPROTO
typedef       void    *wxTimer ;
#else

class wxTimer: public wxbTimer
{
  DECLARE_DYNAMIC_CLASS(wxTimer)

 public:
#if 0
#ifdef wx_motif
  long timerId;
#endif
#ifdef wx_xview
  struct itimerval timerval;
#endif
#endif
 public:
  wxTimer(void);
  ~wxTimer(void);
  virtual Bool Start(int milliseconds = -1,Bool one_shot = FALSE ); // Start timer
  virtual void Stop(void);                   // Stop timer

  void Dequeue(void);
};

#endif // IN_CPROTO
#endif // wx_timerh
