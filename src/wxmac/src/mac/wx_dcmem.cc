///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcmem.cc
// Purpose:	Memory device context implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <Quickdraw.h>
#include "wx_dcmem.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_privt.h"

/* 
	A wxMemoryDC is a pointer to a bitmap, which is an offscreen GWorld. 

	When the wxMemoryDC(void) constructor is used we don't know how large a pixmap
	(boundsRect) to create so we wait until SelectObject() is called. 
   mflatt:
    This is unlike X-Windows and Windows! These platforms allocate a bitmap
     anyway of some size. Since the manual says to select a bitmap first, I
     see no problem with this Mac implementation.
*/
extern CGrafPtr wxMainColormap;

wxMemoryDC::wxMemoryDC(Bool ro)
{
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_PIXMAP;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  read_only = ro;
  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  canvas = NULL;
  clipping = FALSE;
  
  ok = FALSE;
  title = NULL;

  font = wxNORMAL_FONT;
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  current_pen = NULL;
  current_brush = NULL;
  current_text_foreground = *wxBLACK;

  // mflatt: NOT ok
  // ok = TRUE;
  selected_pixmap = NULL;
  gworldH = NULL;

  Colour = wxColourDisplay();
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::~wxMemoryDC(void)
{
  if (selected_pixmap) {
    if (!read_only) {
	selected_pixmap->selectedInto = NULL;
	selected_pixmap->selectedIntoDC = 0;
     }
	gworldH = NULL;
  } else {
    if (gworldH) {
	  ::DisposeGWorld(gworldH);
	  gworldH = NULL;
    }
 }
 
 if (cMacDC) {
	delete cMacDC;
	cMacDC = NULL;
  }
}

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  if (selected_pixmap == bitmap) {
		// set cMacDC ??
		return;
  }
  if (!read_only) {
    if (bitmap && bitmap->selectedIntoDC)
	// This bitmap is selected into a different memoryDC
    return;
  }

  if (selected_pixmap) {
     if (!read_only) {
	selected_pixmap->selectedInto = NULL;
	selected_pixmap->selectedIntoDC = 0;
      }
      gworldH = NULL;
  } else {
    if (gworldH) {
  	  ::DisposeGWorld(gworldH);
      gworldH = NULL;
    }
  }

  if (cMacDC) {
	delete cMacDC;
	cMacDC = NULL;
  }
  ok = FALSE;
  selected_pixmap = bitmap;
  if (bitmap == NULL) {	// deselect a bitmap
    pixmapWidth = 0;
    pixmapHeight = 0;
	pixmap = NULL;
	return;
  }
  if (!read_only) {
    bitmap->selectedInto = this;
    bitmap->selectedIntoDC = -1;
  }
  pixmapWidth = bitmap->GetWidth();
  pixmapHeight = bitmap->GetHeight();
  if (bitmap->Ok()) {
    gworldH = bitmap->x_pixmap;
    // gworldH = MacCreateGWorld(pixmapWidth, pixmapHeight);
    if (gworldH) {
	  pixmap = ::GetGWorldPixMap(gworldH);
	
	  SetGWorld(gworldH, 0);
	  cMacDC = new wxMacDC(gworldH);
	  // bitmap->DrawMac(0, 0);
	  ok = TRUE;
    }
  }
}

wxBitmap* wxMemoryDC::GetObject()
{
  return selected_pixmap;
}

GWorldPtr wxMemoryDC::MacCreateGWorld(int width, int height)
{
	QDErr err;
	GWorldPtr	newGWorld;
	Rect	bounds = {0, 0, height, width};

	err = NewGWorld(&newGWorld, 0, &bounds, NULL, NULL, noNewDevice);
	if (err == noErr)
		return newGWorld;
	else
		return NULL;
}

