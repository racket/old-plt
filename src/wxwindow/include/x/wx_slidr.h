/*
 * File:	wx_slidr.h
 * Purpose:	Declares slider panel item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_slidr.h	1.2 5/9/94" */

#ifndef wx_slidrh
#define wx_slidrh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_slidr.h"

#ifdef IN_CPROTO
typedef       void    *wxSlider ;
#else

class wxPanel;
// Slider
class wxSlider: public wxbSlider
{
  DECLARE_DYNAMIC_CLASS(wxSlider)

 public:
  wxSlider(void);
  wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = wxHORIZONTAL, char *name = "slider");
  ~wxSlider(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = wxHORIZONTAL, char *name = "slider");
  int GetValue(void);
  void SetValue(int);
  void GetSize(int *x, int *y);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }

  void ChangeColour(void) ;
};

#endif // IN_CPROTO
#endif // wx_slidrh
