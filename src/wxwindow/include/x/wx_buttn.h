/*
 * File:	wx_buttn.h
 * Purpose:	wxButton (X implementation)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_buttn.h	1.2 5/9/94" */

#ifndef wx_buttnh
#define wx_buttnh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_buttn.h"

#ifdef IN_CPROTO
typedef       void    *wxButton ;
#else

// Pushbutton
class wxBitmap;
class wxPanel;
class wxButton: public wxbButton
{
  DECLARE_DYNAMIC_CLASS(wxButton)

 public:
  wxBitmap *buttonBitmap ;
  wxButton(void);
  wxButton(wxPanel *panel, wxFunction func, char *label, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");
  wxButton(wxPanel *panel, wxFunction func, wxBitmap *bitmap, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");
  ~wxButton(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");
  Bool Create(wxPanel *panel, wxFunction func, wxBitmap *bitmap, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }
  void SetDefault(void);
  void SetLabel(char *);
  void SetLabel(wxBitmap *bitmap);
  char *GetLabel(void);
  void Command(wxCommandEvent& event);

  void ChangeColour(void) ;
};

#endif // IN_CPROTO
#endif // wx_buttnh
