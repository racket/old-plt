/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_sysev.cc,v 1.3 1998/08/14 13:56:03 robby Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#pragma implementation "wx_sysev.h"

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

