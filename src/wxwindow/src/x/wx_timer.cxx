/*
 * File:      wx_timer.cc
 * Purpose:     wxTimer implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_timer.cc,v 1.3 1994/08/14 21:28:43 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_timer.cc	1.2 5/9/94";

/* re-implemented by MrEd */
#if 0

#ifdef __GNUG__
#pragma implementation
#endif

#include <time.h>
#include <sys/types.h>
#if defined(__sgi) || defined(__ultrix)
#include <sys/time.h>
#else
#include <sys/timeb.h>
#endif
#include "wx_timer.h"
#include "wx_list.h"
#include "wx_main.h"
#include "wx_frame.h"
#include "wx_utils.h"

#ifdef wx_motif
#include <Xm/Xm.h>
#endif

#ifdef wx_xview
#include <xview/notify.h>
#endif

// Can't find an include file with this in!!
// extern "C" int ftime(struct timeb *); // Not used!!

/* MATTHEW: [4] We have to have this for safety & GC */
static wxList wxTimerList(wxKEY_INTEGER);

#ifdef wx_motif
void 
wxTimerCallback (wxTimer * timer)
{
  /* MATTHEW: [4] Check to see if it's still on */
  if (!wxTimerList.Find((long)timer))
    return;

  if (timer->timerId == 0)
    return;			// Avoid to process spurious timer events

  if (!timer->oneShot)
    timer->timerId = XtAppAddTimeOut (wxTheApp->appContext, timer->milli,
		  (XtTimerCallbackProc) wxTimerCallback, (XtPointer) timer);
  else
    timer->timerId = 0;
  timer->Notify ();
}
#endif

#ifdef wx_xview
Notify_value wxTimerFunc (Notify_client client, int which);
#endif

IMPLEMENT_DYNAMIC_CLASS(wxTimer, wxObject)

wxTimer::wxTimer (void)
{
#ifdef wx_motif
  timerId = 0;
#endif
#ifdef wx_xview
  timerval.it_value.tv_usec = 0;
  timerval.it_interval.tv_usec = 0;
#endif
}

wxTimer::~wxTimer (void)
{
  Stop ();
}

Bool wxTimer::Start (int milliseconds, Bool mode)
{
  oneShot = mode;
  if (milliseconds < 0)
    milliseconds = lastMilli;

  if (milliseconds <= 0)
    return FALSE;

  lastMilli = milli = milliseconds;

  /* MATTHEW: [4] Add timer to list: */
  if (!wxTimerList.Find((long)this))
    wxTimerList.Append((long)this, this);

#ifdef wx_motif
  timerId = XtAppAddTimeOut (wxTheApp->appContext, milliseconds,
		   (XtTimerCallbackProc) wxTimerCallback, (XtPointer) this);
  return TRUE;
#endif
#ifdef wx_xview
  if (milliseconds > -1)
    {
      long secs = (long) (milliseconds / 1000);
      long usecs = (long) (milliseconds - 1000 * secs) * 1000;

      timerval.it_value.tv_sec = secs;
      timerval.it_interval.tv_sec = secs;

      timerval.it_value.tv_usec = usecs;
      timerval.it_interval.tv_usec = usecs;
    }

  notify_set_itimer_func ((Notify_client) this, (Notify_func) wxTimerFunc, ITIMER_REAL,
			  &timerval, NULL);
  return TRUE;
#endif
}

void wxTimer::Stop (void)
{
#ifdef wx_motif
  if (timerId > 0)
    {
      XtRemoveTimeOut (timerId);
      timerId = 0;
    }
#endif
#ifdef wx_xview
  notify_set_itimer_func ((Notify_client) this, NOTIFY_FUNC_NULL, ITIMER_REAL,
			  NULL, NULL);
#endif
  milli = 0;

  /* MATTHEW: [4] Delete from timer list */
  wxNode *node = wxTimerList.Find((long)this);
  if (node)
    wxTimerList.DeleteNode(node);
}

#ifdef wx_xview
Notify_value 
wxTimerFunc (Notify_client client, int which)
{
  wxTimer *timer = (wxTimer *) client;

  /* MATTHEW: [4] Check to see if it's still on */
  if (!wxTimerList.Find((long)timer))
    return NOTIFY_DONE;

  if (timer->milli == 0)
    return NOTIFY_DONE;		// Avoid to process spurious timer events

  if (timer->oneShot)
    timer->Stop ();

  timer->Notify ();
  return NOTIFY_DONE;
}
#endif

#endif
