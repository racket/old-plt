///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan.h
// Purpose:	Canvas device context declaration (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dcprh
#define wx_dcprh

#include "wx_gdi.h"
#include "wb_dccan.h"
#include "wx_print.h"

#ifdef IN_CPROTO
typedef       void* wxPrinterDC ;
#else

//class wxCanvas;
class wxPrinterDC: public wxCanvasDC
{
 public:
  TPPrPort prPort;
  THPrint  prRecHandle;

  wxPrinterDC(void);
  wxPrinterDC(THPrint); // Create a DC corresponding to a canvas

  ~wxPrinterDC(void);

  Bool StartDoc(char* message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
};

#endif // IN_CPROTO
#endif // wx_dccanh
