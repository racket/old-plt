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

  wxPrintData *cPrintData;
#ifdef OS_X
  PMPrintContext cPrintContext;
#else
  TPPrPort prPort;
#endif  

  wxPrinterDC(wxPrintData *); // Create a DC corresponding to a canvas

  ~wxPrinterDC(void);

  Bool StartDoc(char *);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);
};

#endif // IN_CPROTO
#endif // wx_dccanh
