/*
 * File:	wx_dccan.h
 * Purpose:	Canvas device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_dccanh
#define wx_dccanh

#include "wx_gdi.h"
#include "wb_dccan.h"

class wxCanvasDC: public wxbCanvasDC
{
 public:
  wxCanvasDC(void);

  void TryColour(wxColour *src, wxColour *dest);

  // Create a DC corresponding to a canvas
  wxCanvasDC(wxCanvas *canvas);
  void GetClippingBox(float *x,float *y,float *w,float *h) ;
  void GetClippingRegion(float *x, float *y, float *width, float *height);

  void GetSize(float *width, float *height);

  Bool GCBlit(float xdest, float ydest, float width, float height,
	      wxBitmap *source, float xsrc, float ysrc);

  ~wxCanvasDC(void);
};

#endif // wx_dccan

