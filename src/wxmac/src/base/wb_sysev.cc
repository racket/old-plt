/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_sysev.cc,v 1.2 1998/06/02 20:51:42 robby Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

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

