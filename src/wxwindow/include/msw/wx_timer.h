/*
 * File:	wx_timer.h
 * Purpose:	wxTimer - provides simple timer functionality
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_timerh
#define wx_timerh

#include "common.h"
#include "wx_obj.h"
#include "wb_timer.h"

class wxTimer: public wxbTimer
{
 public:
  wxTimer(void);
  ~wxTimer(void);
  virtual Bool Start(int milliseconds = -1,Bool one_shot = FALSE); // Start timer
  virtual void Stop(void);                   // Stop timer

  void Dequeue(void);
};

#endif // wx_timerh
