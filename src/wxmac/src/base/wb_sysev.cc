/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_sysev.cc,v 1.4 2001/07/11 16:53:06 clements Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#ifdef __GNUG__
#pragma implementation "wx_sysev.h"
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_list.h"

#include "wx.h"

#include "wx_sysev.h"
 
wxEvent::wxEvent(void)  : wxObject(WXGC_NO_CLEANUP)
{
  eventClass = 0;
  eventType = 0;
  eventHandle = NULL;
}

wxEvent::~wxEvent(void)
{
}

