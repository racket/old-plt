///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr2.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 2)
// Author:	Lou Birk (copied from wx_dccan)
// Created:	1995
// Updated:	
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <Quickdraw.h>
#include <Printing.h>
#include "wx_dcpr.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_privt.h"

#include <QDOffScreen.h>


class wxCanvasDC;



//-----------------------------------------------------------------------------
Bool wxPrinterDC::Blit(float xdest, float ydest, float width, float height,
                wxCanvasDC *source, float xsrc, float ysrc, int rop)
{
	return 0; /* doesn't work */

	if (device != source->device) {
		// Switch Gworld to this
		SetGWorld(cMacDC->macGrafPort(), 0);
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC();
	}
	else {
		SetCurrentDC();
	}
	if (rop == wxCOLOR) {
	  wxMacSetCurrentTool(kColorBlitTool);
	  rop = wxCOPY;
	} else
	  wxMacSetCurrentTool(kBlitTool);
	Bool theResult = FALSE;

	if (source->pixmap)
	{
		int mode;
		switch (rop)
		{ // FIXME -  these modes have not be tested
		//	case wxCLEAR:  theMacPenMode = GXclear; break;
			case wxXOR:  
				mode = srcXor; 
				break;
		//	case wxINVERT: theMacPenMode = GXinvert; break;
		//	case wxOR_REVERSE: theMacPenMode = GXorReverse; break;
		//	case wxAND_REVERSE: theMacPenMode = GXandReverse; break;
			case wxAND:
				mode = srcBic;
				break;
			case wxOR: 
				mode = srcOr; 
				break;
			case wxAND_INVERT: 
				mode = notSrcBic; break;
		//	case wxNO_OP: theMacPenMode = GXnoop; break;
		//	case wxNOR: theMacPenMode = GXnor; break;
			case wxEQUIV: 
				mode = notSrcXor; break;
			case wxSRC_INVERT: 
				mode = notSrcCopy; break;
			case wxOR_INVERT: 
				mode = notSrcOr; break;
		//	case wxNAND: theMacPenMode = GXnand; break;
		//	case wxSET: theMacPenMode = GXset; break;
			case wxCOPY:
			default:
				mode = srcCopy;
				break;
		}
          // LJB Moonface, Inc. Changes to make wxmac stuff
          // work with the HTML viewer "Majestic"
          // for the mac, take the offscreen pixmap and blit
          // it to the screen as defined by the destination
          // rectangle information
		int w = width;
		int h = height;
		int x = XLOG2DEV(xdest);
		int y = YLOG2DEV(ydest);
		int ixsrc = source->LogicalToDeviceX(xsrc);
		int iysrc = source->LogicalToDeviceY(ysrc);
		Rect srcr = {iysrc, ixsrc, iysrc + h, ixsrc + w};
		Rect destr = {y, x, y+h, x+w };

        GrafPtr theMacGrafPort = (GrafPtr)cMacDC->macGrafPort();
        BitMap *dstbm;
        PixMapHandle destpixh;
        if ((((CGrafPtr)theMacGrafPort)->portVersion & 0xC000) != 0xC000) {
          destpixh = NULL;
          dstbm = (BitMap *) &((GrafPtr)(cMacDC->macGrafPort()))->portBits;
        } else {
          destpixh = ((CGrafPtr)theMacGrafPort)->portPixMap;
		  ::LockPixels(destpixh);
		  dstbm = (BitMap *)(* destpixh);
        }

		// Lock PixMaps
		PixMapHandle srpixh = source->pixmap;
		int rs = ::LockPixels(srpixh);

		CopyBits((BitMap *) (* srpixh),   (dstbm),	&srcr, &destr, mode, NULL);

		if (destpixh) ::UnlockPixels(destpixh);
		::UnlockPixels(srpixh);
		CalcBoundingBox(xdest, ydest);
		CalcBoundingBox(xdest + width, ydest + height);
		theResult = TRUE;
	}

  	return theResult;
}