/*
 * File:	wx_timer.cc
 * Purpose:	wxTimer implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_timer.cc,v 1.1 1994/08/14 21:59:17 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#endif

/* Timers now implemented by MrEd */
#if 0

#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
#pragma hdrstop

#include "wx_setup.h"
#include "wx_list.h"
#include "wx_timer.h"
#include "wx_main.h"
/* MATTHEW: [11] */
#include "wx_wmgr.h"

#include <time.h>
#include <sys/types.h>
#ifndef __SC__
#include <sys/timeb.h>
#endif
#ifdef WIN32
#define _EXPORT /**/
#else
#define _EXPORT _export
#endif

wxList wxTimerList(wxKEY_INTEGER);
UINT WINAPI _EXPORT wxTimerProc(HWND hwnd, WORD, int idTimer, DWORD);

IMPLEMENT_DYNAMIC_CLASS(wxTimer, wxObject)

wxTimer::wxTimer(void)
{
  id = 0;
}

wxTimer::~wxTimer(void)
{
  Stop();

  wxTimerList.DeleteObject(this);
}

Bool wxTimer::Start(int milliseconds,Bool mode)
{
  oneShot = mode ;
  if (milliseconds < 0)
    milliseconds = lastMilli;

  if (milliseconds <= 0)
    return FALSE;

  lastMilli = milli = milliseconds;

  wxTimerList.DeleteObject(this);

/* MATTHEW: [11] */
#ifndef USE_SEP_WIN_MANAGER
  id = SetTimer(NULL, (UINT)(id ? id : 1), (UINT)milliseconds, NULL);
#else
  wxwmCreateTimer r;

  r.id = (UINT)(id ? id : 1);
  r.milliseconds = (UINT)milliseconds;
  r.proc = wxTimerProcInst;
  wxwmMessage(WXM_CREATE_TIMER, (LPARAM)&r);
  id = r.result;
#endif

  if (id > 0)
  {
	 wxTimerList.Append(id, this);
	 return TRUE;
  }
  else return FALSE;
}

void wxTimer::Stop(void)
{
  if (id) {
    KillTimer(NULL, (UINT)id);
    wxTimerList.DeleteObject(this); /* @@@@ */
  }
  id = 0 ;
  milli = 0 ;
}

UINT WINAPI _EXPORT wxTimerProc(HWND WXUNUSED(hwnd), WORD, int idTimer, DWORD)
{
  Bool c = wxwmCheckInMain();
  wxNode *node = wxTimerList.Find((long)idTimer);
  if (node)
  {
	 wxTimer *timer = (wxTimer *)node->Data();
	 if (timer->id != 0) { // Avoid to process spurious timer events
		if (timer->oneShot) /* MATTHEW: Stop before notify */
		  timer->Stop() ;
		timer->Notify();
	 }
  }

  wxwmCheckOutMain(c);

  return 0;
}

#endif
