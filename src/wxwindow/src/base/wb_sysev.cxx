/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_sysev.cxx,v 1.1.1.1 1997/12/22 16:11:57 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation "wx_sysev.h"
#pragma implementation
#pragma interface
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_list.h"
#include "wx_sysev.h"

#endif

/*
 * A different kind of event from wxEvent: general wxWindows events, covering
 * all interesting things that might happen (button clicking, resizing,
 * setting text in widgets, etc.).
 *
 * For each completely new event type, derive a new event class.
 *
 */

IMPLEMENT_ABSTRACT_CLASS(wxEvent, wxObject)

wxEvent::wxEvent(void) : wxObject(WXGC_NO_CLEANUP)
{
  eventClass = 0;
  eventType = 0;
  eventHandle = NULL;
  timeStamp = 0;
}

wxEvent::~wxEvent(void)
{
}

void wxEvent::SetTimestamp(long ts)
{
  timeStamp = ts;
}
