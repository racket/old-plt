/*
 * File:	main.cc
 * Purpose:	Main stub for calling wxWindows entry point
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	March 1995
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#include "windows.h"
#include "wx.h"
#pragma hdrstop

#ifndef _WINDLL
extern int wxEntry(HINSTANCE hInstance, HANDLE hPrevInstance, LPSTR m_lpCmdLine, int nCmdShow);
#else
extern int wxEntry(HINSTANCE hInstance,
	WORD wDataSegment, WORD wHeapSize, LPSTR lpszCmdLine)
#endif

// NT defines APIENTRY, 3.x not
#if !defined(APIENTRY)
#define APIENTRY FAR PASCAL
#endif
 
#ifndef _WINDLL

// Main windows entry point

#ifdef __WATCOMC__
//***For Watcom, WinMain MUST be PASCAL,
//***not FAR PASCAL!!   D.Chubraev
int PASCAL WinMain(HANDLE hInstance, HANDLE hPrevInstance, LPSTR m_lpCmdLine,
                    int nCmdShow )
#else
int APIENTRY WinMain(HANDLE hInstance, HANDLE hPrevInstance, LPSTR m_lpCmdLine,
                    int nCmdShow )
#endif
{
  return wxEntry(hInstance, hPrevInstance, m_lpCmdLine, nCmdShow);
}

#else /*  _WINDLL  */

extern "C"
int APIENTRY LibMain(HINSTANCE hInstance,
	WORD wDataSegment, WORD wHeapSize, LPSTR lpszCmdLine)
{
  return wxEntry(hInstance, wDataSegment, wHeapSize, lpszCmdLine);
}
#endif // _WINDLL
