/*
 * File:	wx_messg.h
 * Purpose:	Declares message panel item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_messg.h	1.2 5/9/94" */

#ifndef wx_messgh
#define wx_messgh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_messg.h"

#ifdef IN_CPROTO
typedef       void    *wxMessage ;
#else

class wxPanel;

// Message item
class wxMessage: public wxbMessage
{
  DECLARE_DYNAMIC_CLASS(wxMessage)

 public:
  wxBitmap *bm_label;

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
  void SetLabel(char *label);
  void SetLabel(wxBitmap *image);
#endif

  void ChangeColour(void) ;

#ifdef wx_motif
  inline Bool CanAddEventHandler(void)
#if !USE_GADGETS
   { return TRUE; }
#else
   { return FALSE; }
#endif
#endif
};

#endif // IN_CPROTO
#endif // wx_messg
