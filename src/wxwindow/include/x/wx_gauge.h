/*
 * File:	wx_gauge.h
 * Purpose:	Gauge panel item
 * Author:	Julian Smart
 * Created:	1994
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_gauge.h	1.2 5/9/94" */

#ifndef wx_gaugeh
#define wx_gaugeh

#include "wb_gauge.h"

#ifdef IN_CPROTO
typedef       void    *wxGauge;
#else

// Group box
class wxGauge: public wxbGauge
{
  DECLARE_DYNAMIC_CLASS(wxGauge)

 public:
  wxGauge(void);
  wxGauge(wxPanel *panel, char *label, int range, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "gauge");
  ~wxGauge(void);
  Bool Create(wxPanel *panel, char *label, int range, int x=-1, int y=-1,
           int width=-1, int height=-1, long style=0, char *name="gauge");
/*
  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);
*/
  void SetShadowWidth(int w);
  void SetBezelFace(int w);
  void SetRange(int r);
  void SetValue(int pos);

  void ChangeColour(void);

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }
};

#endif // IN_CPROTO
#endif // wx_gaugeh
