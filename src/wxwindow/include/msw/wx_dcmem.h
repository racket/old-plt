/*
 * File:	wx_dcmem.h
 * Purpose:	Memory device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_dcmemh
#define wx_dcmemh

#include "common.h"
#include "wb_dcmem.h"

class wxMemoryDC: public wxbMemoryDC
{
 public:
  Bool read_only;
  wxMemoryDC(Bool read_only = 0);
  wxMemoryDC(wxCanvasDC *old_dc); // Create compatible DC

  ~wxMemoryDC(void);
  virtual void SelectObject(wxBitmap *bitmap);
  virtual wxBitmap* GetObject();
  void GetSize(float *width, float *height);
};

#endif // wx_dcmemh

