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
#include <QuickDraw.h>
#include <Printing.h>
#include "wx_dcpr.h"
//#include "wx_canvs.h"
#include "wx_privt.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"

// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

extern CGrafPtr wxMainColormap;

void InstallColor(wxColour &c, int fg, Bool Colour);

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------
wxPrinterDC::wxPrinterDC(void)
{
  // this should never happen since the cMacDC would not be set
  // to a printer port like below
}


//-----------------------------------------------------------------------------
wxPrinterDC::wxPrinterDC(THPrint pData) : wxCanvasDC()
{
	/* MATTHEW: [6] */
	__type = wxTYPE_DC_PRINTER;

	GrafPtr oldPort;
	::GetPort(&oldPort);
    prRecHandle = pData;
    PrOpen();
	if (PrError()) {
      PrClose();
      ok = FALSE;
      return;
    }

	prPort = PrOpenDoc(prRecHandle, 0, 0);

    if (PrError()) {
      PrCloseDoc(prPort);
      PrClose();
      ok = FALSE;
      return;
    }

    cMacDC = new wxMacDC((CGrafPtr)&(prPort->gPort));
	GrafPtr theMacGrafPort = (GrafPtr)cMacDC->macGrafPort();
	::SetPort((GrafPtr)theMacGrafPort);
	
	cMacDoingDrawing = FALSE;

  clipping = FALSE;
  selected_pixmap = NULL;

  current_reg = NULL ;
  user_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0;
  max_x = 0; max_y = 0;

  pixmapWidth = (**prRecHandle).prInfo.rPage.right;
  pixmapHeight = (**prRecHandle).prInfo.rPage.bottom;

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
  current_logical_function = wxCOPY;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = NULL;
  current_text_foreground = *wxBLACK;
//  current_text_background = NULL;
  SetBackground(wxWHITE_BRUSH);
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  //::SetPort(oldPort);

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
  if (ok)
    PrCloseDoc(prPort);
}

//-----------------------------------------------------------------------------
Bool wxPrinterDC::StartDoc(char *message) { return TRUE; }

//-----------------------------------------------------------------------------
void wxPrinterDC::EndDoc(void) { }

//-----------------------------------------------------------------------------
void wxPrinterDC::StartPage(void)
{
  if (prPort)
    PrOpenPage(prPort, 0); 
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndPage(void)
{
  if (prPort)
    PrClosePage(prPort);
}
