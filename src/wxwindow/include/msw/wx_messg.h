/*
 * File:	wx_messg.h
 * Purpose:	Message panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_messgh
#define wx_messgh

#include "wb_messg.h"

// Message item
class wxMessage: public wxbMessage
{
 public:
  wxMessage(wxPanel *panel, char *message, int x = -1, int y = -1,
            long style = 0, char *name = "message");
  wxMessage(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1,
            long style = 0, char *name = "message");
  ~wxMessage(void);

  Bool Create(wxPanel *panel, char *message, int x = -1, int y = -1,
            long style = 0, char *name = "message");
  Bool Create(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1,
            long style = 0, char *name = "message");
  void SetLabel(wxBitmap *bitmap);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetLabel(char *label);

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;

  wxBitmap *bm_label;
};

#endif // wx_messgh
