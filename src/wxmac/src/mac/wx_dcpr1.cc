///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr1.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 1)
// Author:	Lou Birk (copied from wx_dccan)
// Created:	1995
// Updated:	
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef OS_X
#include <Printing.h>
#include <QuickDraw.h>
#endif
#include "wx_dcpr.h"
//#include "wx_canvs.h"
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

    GrafPtr theGrafPtr;
    __type = wxTYPE_DC_PRINTER;

    cPrintData = printData;

#ifdef OS_X
    if (PMBeginDocument(cPrintData->cPrintSettings,cPrintData->cPageFormat,&cPrintContext) != noErr) {
      ok = false;
      return;
    }  
    
    if (PMGetGrafPtr(cPrintContext, &theGrafPtr) != noErr) {
      ok = false;
      return;
    }

#else    
    prPort = PrOpenDoc(cPrintData->macPrData, 0, 0);

    if (PrError()) {
      PrCloseDoc(prPort);
      PrClose();
      ok = FALSE;
      return;
    }

    theGrafPtr = &(prPort->gPort);
#endif

    cMacDC = new wxMacDC((CGrafPtr)theGrafPtr);
	
    cMacDoingDrawing = FALSE;

    clipping = FALSE;
    selected_pixmap = NULL;

    current_reg = NULL ;
    onpaint_reg = NULL ;

    min_x = 0; min_y = 0;
    max_x = 0; max_y = 0;

#ifdef OS_X
  PMRect pageRect;
  
  PMGetAdjustedPageRect(cPrintData->cPageFormat,&pageRect);
  pixmapWidth = (int)(pageRect.right - pageRect.left);
  pixmapHeight = (int)(pageRect.bottom - pageRect.top);
#else
  pixmapWidth = (*cPrintData->macPrData)->prInfo.rPage.right;
  pixmapHeight = (*cPrintData->macPrData)->prInfo.rPage.bottom;
#endif

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


  ok = TRUE;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_text_foreground = new wxColour(wxBLACK);
//  current_text_background = NULL;
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  int clientWidth, clientHeight;
  //the_canvas->GetClientSize(&clientWidth, &clientHeight);
  clientWidth = pixmapWidth;
  clientHeight = pixmapHeight; // paper
  Rect paintRect = {0, 0, clientHeight, clientWidth};
  SetPaintRegion(&paintRect);

}

//-----------------------------------------------------------------------------
wxPrinterDC::~wxPrinterDC(void)
{
  if (ok) {
#ifdef OS_X
    PMEndDocument(cPrintContext);
#else    
    PrCloseDoc(prPort);
#endif
  }
}

//-----------------------------------------------------------------------------
Bool wxPrinterDC::StartDoc(char *message) 
{ 
  return TRUE; 
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndDoc(void) { }

//-----------------------------------------------------------------------------
void wxPrinterDC::StartPage(void)
{
#ifdef OS_X
    if (cPrintContext)
        PMBeginPage(cPrintContext,NULL);
#else
  if (prPort)
    PrOpenPage(prPort, 0); 
#endif
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndPage(void)
{
#ifdef OS_X
    if (cPrintContext)
        PMEndPage(cPrintContext);
#else
  if (prPort)
    PrClosePage(prPort);
#endif
}
