/*
 * File:	dummy.cc
 * Purpose:	See below
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)dummy.cc	1.2 5/9/94"; */

/* A dummy file to include wx.h. If precompiling wx.h, I
 * always start by compiling this and producing the PCH file.
 * Then subsequent source files use the PCH file.
 *
 * If precompiling wx.h for wxWindows and derived apps,
 * link dummy.obj with your program (the MSC 7 linker complains otherwise).
 *
 * This is the only convenient way I found to use precompiled headers
 * under MSC 7.
 *
 * This will produce a big PCH file.
 */

#ifdef __BORLANDC__
#pragma hdrfile "c:\wx\src\msw\wx.sym"
#pragma hdrstart
#endif
#include "wx.h"
#pragma hdrstop

// Foils optimizations in Visual C++ (see also wx_main.cc)
char wxDummyChar=0;
