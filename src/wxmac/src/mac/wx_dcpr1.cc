///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr1.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 1)
// Author:	Lou Birk (copied from wx_dccan)
// Created:	1995
// Updated:	
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WX_CARBON
# include <Printing.h>
# include <QuickDraw.h>
#endif
#include "wx_dcpr.h"
#include "wx_privt.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
wxPrinterDC::wxPrinterDC(wxPrintData *printData) : wxCanvasDC()
{
  ok = true;

  __type = wxTYPE_DC_PRINTER;

  cPrintData = printData;
  if (!printData->cPrintSession) {
    PMCreateSession(&printData->cPrintSession);

    wxPrintDialog *dialog = new wxPrintDialog(NULL, printData);
    dialog->ShowSetupDialog(TRUE);
    if (!dialog->UseIt()) {
      PMRelease(printData->cPrintSession);
      printData->cPrintSession = NULL;
    }
    delete dialog;
  }

  cMacDoingDrawing = FALSE;

  clipping = FALSE;
  selected_pixmap = NULL;

  current_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0;
  max_x = 0; max_y = 0;

  PMRect pageRect;
  
  PMGetAdjustedPageRect(cPrintData->cPageFormat,&pageRect);
  pixmapWidth = (int)(pageRect.right - pageRect.left);
  pixmapHeight = (int)(pageRect.bottom - pageRect.top);

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;

  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;

  ok = (printData->cPrintSession ? TRUE : FALSE);
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_text_foreground = new wxColour(wxBLACK);
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

//-----------------------------------------------------------------------------
wxPrinterDC::~wxPrinterDC(void)
{
  if (current_phase == 1)
    EndDoc();
  if (current_phase == 2)
    EndPage();
  if (current_phase == 1)
    EndDoc();

  if (cPrintData->cPrintSession) {
    PMRelease(cPrintData->cPrintSession);
    cPrintData->cPrintSession = NULL;
  }
}

//-----------------------------------------------------------------------------
Bool wxPrinterDC::StartDoc(char *message) 
{ 
  if (current_phase != 0)
   return FALSE;

  OSErr rlt;
  rlt = PMSessionBeginDocument(cPrintData->cPrintSession, cPrintData->cPrintSettings,cPrintData->cPageFormat);
  if (rlt != noErr) {
    ok = false;
    return FALSE;
  } else
    current_phase = 1;

  return TRUE;
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndDoc(void)
{
  if (current_phase != 1)
    return;

  PMSessionEndDocument(cPrintData->cPrintSession);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::StartPage(void)
{
  if (current_phase != 1)
    return;

  if (ok) {
    CGrafPtr theGrafPtr;

    if (PMSessionBeginPage(cPrintData->cPrintSession,
			   cPrintData->cPageFormat,
			   NULL) != noErr) {
      PMSessionEndDocument(cPrintData->cPrintSession);
      current_phase = 0;
      return;
    } else {
      if (PMSessionGetGraphicsContext(cPrintData->cPrintSession, NULL, (void **)&theGrafPtr) != noErr) {
	ok = false;
	PMSessionEndPage(cPrintData->cPrintSession);
	PMSessionEndDocument(cPrintData->cPrintSession);
	current_phase = 0;
	return;
      }
    }
    
    current_phase = 2;
    cMacDC = new wxMacDC(theGrafPtr);
  }
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndPage(void)
{
  if (current_phase != 2)
    return;
  PMSessionEndPage(cPrintData->cPrintSession);
  cMacDC = NULL;
  current_phase = 1;
}
