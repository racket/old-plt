/*								-*- C++ -*-
 * $Id: Timer.cc,v 1.2 1996/01/11 10:27:08 markus Exp $
 *
 * Purpose: class to process time outs
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "Timer.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxTimer
#define  Uses_wxList
#include "wx.h"

#if 0
/* MrEd re-implements timers */

//-----------------------------------------------------------------------------
// wxTimer implementation
//-----------------------------------------------------------------------------

/* MATTHEW: We have to have this for safety & GC */
static wxList wxTimerList(wxKEY_INTEGER);

wxTimer::wxTimer(void)
{
    __type = wxTYPE_TIMER;

    id       = 0;
    interval = 0;
    one_shot = FALSE;
}

wxTimer::~wxTimer(void)
{
    Stop();
}

// start timer using XtAppAddTimeOut
Bool wxTimer::Start(int milliseconds, Bool mode)
{
    one_shot = mode;
    if (milliseconds > 0)
	interval = milliseconds;
    if (interval == 0)
	return FALSE;

  /* MATTHEW: [4] Add timer to list: */
  if (!wxTimerList.Find((long)this))
    wxTimerList.Append((long)this, this);

    id = XtAppAddTimeOut(wxAPP_CONTEXT,
			 (unsigned long)interval,
			 (XtTimerCallbackProc)wxTimer::EventCallback,
			 (XtPointer)this);
    return TRUE;
}

// stop timer using XtRemoveTimeOut
void wxTimer::Stop(void)
{
    if (id > 0) {
	XtRemoveTimeOut(id);
	id = 0;
    }
    interval = 0;

  /* MATTHEW: Delete from timer list */
  wxNode *node = wxTimerList.Find((long)this);
  if (node)
    wxTimerList.DeleteNode(node);

}

// callback for XtAppAddTimeOut
void wxTimer::EventCallback(wxTimer *timer, XtIntervalId *WXUNUSED(id))
{
  /* MATTHEW: Check to see if it's still on */
  if (!wxTimerList.Find((long)timer))
    return;

    if (timer->id == 0)
	return;	// Avoid to process spurious timer events

    if (!timer->one_shot)
	timer->id = XtAppAddTimeOut(wxAPP_CONTEXT,
				    (unsigned long)timer->interval,
				    (XtTimerCallbackProc)wxTimer::EventCallback,
				    (XtPointer)timer);
    else {
      timer->id = 0;
      timer->Stop(); /* MATTHEW: To remove from the active list */
    }

    timer->Notify();
}

#endif
