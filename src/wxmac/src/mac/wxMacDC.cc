///////////////////////////////////////////////////////////////////////////////
// File:	wxMacDC.cc
// Purpose:	MacDC (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////


#include "wx_obj.h"
#include "wxMacDC.h"

//-----------------------------------------------------------------------------
wxMacDC::wxMacDC(CGrafPtr port)
{
  cMacGrafPort = port;
  cCurrentUser = NULL;
  WXGC_IGNORE(this, cCurrentUser);
}

//-----------------------------------------------------------------------------
wxMacDC::~wxMacDC(void)	// destructor
{
}

//-----------------------------------------------------------------------------
Bool wxMacDC::isCurrentPort(void)
{
  return cMacGrafPort == GetQDGlobalsThePort();
}

//-----------------------------------------------------------------------------
CGrafPtr wxMacDC::macGrafPort(void)
{
  return cMacGrafPort;
}

//-----------------------------------------------------------------------------
wxObject* wxMacDC::currentUser(void)
{
  return cCurrentUser;
}

//-----------------------------------------------------------------------------
void wxMacDC::setCurrentUser(wxObject* user)
{
  cCurrentUser = user;
}
