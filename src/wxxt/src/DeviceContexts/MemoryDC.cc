/*								-*- C++ -*-
 * $Id: MemoryDC.cc,v 1.3 1998/10/18 12:04:09 mflatt Exp $
 *
 * Purpose: device context to draw into wxBitmaps
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "MemoryDC.h"
#endif

#define  Uses_XLib
#define  Uses_wxBitmap
#define  Uses_wxMemoryDC
#include "wx.h"

wxMemoryDC::wxMemoryDC(Bool ro) : wxCanvasDC()
{
    __type = wxTYPE_DC_MEMORY;

    device = wxDEVICE_MEMORY;

    read_only = ro;

    // the memory DC is at construction a unusable class because
    // there is no drawable. The initialization will be done with
    // wxMemoryDC::SelectObject(wxBitmap *bitmap)
}

wxMemoryDC::~wxMemoryDC(void)
{
  if (selected) {
    selected->selectedIntoDC = 0;
    selected->selectedTo = NULL;
    selected = NULL;
  }
}

#define FreeGetPixelCache() if (X->get_pixel_image_cache) DoFreeGetPixelCache()

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  if (bitmap == selected)
    return;

  EndSetPixel();
  FreeGetPixelCache();

  if (!read_only) {
    /* MATTHEW: [4] Bitmap selection memory and safety */
    if (bitmap && bitmap->selectedIntoDC)
      bitmap = NULL;

    if (selected) {
      selected->selectedIntoDC = 0;
      selected->selectedTo = NULL;
    }
  }

    // free all associated GCs
    Destroy();

    if (bitmap && bitmap->Ok()) {
	// The bitmap must use the display and screen of the application.
	// The drawable is the associated pixmap, width, height and depth
	// will be queried with XGetGeometry.
	wxWindowDC_Xinit init;
	init.dpy      = wxAPP_DISPLAY;
	init.scn      = wxAPP_SCREEN;
	init.drawable = GETPIXMAP(bitmap);
	Initialize(&init);
	// If another colourmap is associated with the bitmap,
	//  use it instead of the current colourmap.
	if (bitmap->GetColourMap() != current_cmap)
	    SetColourMap(bitmap->GetColourMap());
	selected = bitmap;
	if (!read_only) {
	  bitmap->selectedIntoDC = -1;
	  selected->selectedTo = this;
	}
    } else {
	DRAWABLE = 0;
	WIDTH = HEIGHT = 0;
	selected = NULL;
    }
}

wxBitmap *wxMemoryDC::GetObject()
{
  return selected;
}

void wxMemoryDC::GetSize(float *w, float *h)
{
  if (selected) {
    *w = selected->GetWidth();
    *h = selected->GetHeight();
  } else {
    *w = 0;
    *h = 0;
  }
}
