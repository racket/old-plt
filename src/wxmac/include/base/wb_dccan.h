/*
 * File:	wb_dccan.h
 * Purpose:	Base canvas device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_dccanh
#define wxb_dccanh

#include "common.h"
#include "wx_dc.h"

#ifdef IN_CPROTO
typedef       void    *wxbCanvasDC ;
#else

class wxbCanvasDC: public wxDC
{
 public:
  inline wxbCanvasDC(void) { }
  inline wxbCanvasDC(wxCanvas *the_canvas) { }

  inline ~wxbCanvasDC(void) { }
};

#endif // IN_CPROTO
#endif // wxb_dccanh

