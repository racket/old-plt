/*
 * File:	wx_messg.h
 * Purpose:	Message panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_messg.h	1.2 5/9/94" */

#ifndef wx_messgh
#define wx_messgh

#include "wb_messg.h"

#ifdef IN_CPROTO
typedef       void    *wxMessage ;
#else

// Message item
class wxMessage: public wxbMessage
{
  DECLARE_DYNAMIC_CLASS(wxMessage)

 public:
  wxMessage(void);
  wxMessage(wxPanel *panel, char *message, int x = -1, int y = -1,
            long style = 0, char *name = "message");
#if USE_BITMAP_MESSAGE
  wxMessage(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1,
            long style = 0, char *name = "message");
#endif
  ~wxMessage(void);

  Bool Create(wxPanel *panel, char *message, int x = -1, int y = -1,
            long style = 0, char *name = "message");
#if USE_BITMAP_MESSAGE
  Bool Create(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1,
            long style = 0, char *name = "message");
  void SetLabel(wxBitmap *bitmap);
#endif
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetLabel(char *label);

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;

  wxBitmap *bm_label;
};

#endif // IN_CPROTO
#endif // wx_messgh
