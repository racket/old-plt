/*
 * File:	wb_timer.h
 * Purpose:	wxTimer - provides simple timer functionality
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_timer.h	1.2 5/9/94" */

#ifndef wxb_timerh
#define wxb_timerh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_obj.h"

#if (!defined(__SC__) && !defined(__sgi))
#include <sys/timeb.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxbTimer ;
#else

class wxTimer;

class wxbTimer: public wxObject
{
 public:
#if 0
  Bool oneShot ;
  int  milli ;
  int  lastMilli ;
#else
  Bool one_shot;
  int interval;
  unsigned long expiration;
  void *context;
  wxTimer *next, *prev;
#endif

  wxbTimer(void);
  ~wxbTimer(void);
  virtual Bool Start(int milliseconds = -1,Bool one_shot=FALSE) = 0; // Start timer
  virtual void Stop(void) = 0;                   // Stop timer
  virtual void Notify(void);                 // Override this member
  virtual int Interval(void) ; // Returns the current interval time (0 if stop)
};

// Timer functions (milliseconds)
void wxStartTimer(void);
// Gets time since last wxStartTimer or wxGetElapsedTime
long wxGetElapsedTime(Bool resetTimer = TRUE);

#endif // IN_CPROTO
#endif // wxb_timerh
