/*
 * File:	wx_dcmem.h
 * Purpose:	Memory device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_dcmem.h	1.2 5/9/94" */


#ifndef wx_dcmemh
#define wx_dcmemh

#ifndef IN_CPROTO
#include <fstream.h>
#endif
#include "common.h"
#include "wb_dcmem.h"

#ifdef IN_CPROTO
typedef       void    *wxMemoryDC ;
#else

class wxMemoryDC: public wxbMemoryDC
{
  DECLARE_DYNAMIC_CLASS(wxMemoryDC)

 public:
  wxMemoryDC(void);
  wxMemoryDC(wxCanvasDC *old_dc); // Create compatible DC

  ~wxMemoryDC(void);
  virtual void SelectObject(wxBitmap *bitmap);
  void GetSize(float *width, float *height);
};

#endif // IN_CPROTO
#endif // wx_dcmemh

