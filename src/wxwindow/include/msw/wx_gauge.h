/*
 * File:	wx_gauge.h
 * Purpose:	Gauge panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_gaugeh
#define wx_gaugeh

#include "wb_gauge.h"

// Group box
class wxGauge: public wxbGauge
{
 private:
  HWND static_label;
 public:
  wxGauge(wxPanel *panel, char *label, int range, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "gauge");
  ~wxGauge(void);
  Bool Create(wxPanel *panel, char *label, int range, int x=-1, int y=-1,
           int width=-1, int height=-1, long style=0, char *name="gauge");

  void SetShadowWidth(int w);
  void SetBezelFace(int w);
  void SetRange(int r);
  void SetValue(int pos);
  void SetButtonColour(wxColour *col);
  void SetBackgroundColour(wxColour *col);

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetSize(int *width, int *height);
  void GetPosition(int *x, int *y);

  void SetLabel(char *s);

  virtual void ChangeToGray(Bool gray);
  
  Bool Show(Bool);
};

#endif // wx_gaugeh
