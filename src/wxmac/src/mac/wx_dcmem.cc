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
  Init(NULL);
  
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_PIXMAP;

  read_only = ro;
  
  ok = FALSE;
  title = NULL;

  selected_pixmap = NULL;
  gworldH = NULL;
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
	    
        SetCurrentDC();
  	InstallColor(current_background_color, FALSE);
	PenMode(patCopy);
	ToolChanged(kNoTool);
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

