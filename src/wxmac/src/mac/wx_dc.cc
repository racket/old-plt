///////////////////////////////////////////////////////////////////////////////
// File:	wx_dc.cc
// Purpose:	Device context implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";
#include <QuickDraw.h>
#include "wx_gdi.h"
#include "wx_dc.h"

//-----------------------------------------------------------------------------
wxDC::wxDC(void)
//-----------------------------------------------------------------------------
{
	cMacDC = NULL;
	cMacCurrentTool = kNoTool;
}

//-----------------------------------------------------------------------------
wxDC::~wxDC(void)
//-----------------------------------------------------------------------------
{
}

//-----------------------------------------------------------------------------
void wxDC::wxMacDrawPoint(int x1, int y1)
//-----------------------------------------------------------------------------
{
	MoveTo(x1, y1);
	Line(1, 1);
}

//-----------------------------------------------------------------------------
void wxDC::wxMacDrawLine(int x1, int y1, int x2, int y2)
//-----------------------------------------------------------------------------
{
	MoveTo(x1, y1);
	LineTo(x2, y2);
}