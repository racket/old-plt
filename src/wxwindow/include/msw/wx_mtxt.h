/*
 * File:	wx_mtxt.h
 * Purpose:	Multi-line text panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_mtxt.h	1.2 5/9/94" */

#ifndef wx_mtxth
#define wx_mtxth

#include "wb_mtxt.h"

#ifdef IN_CPROTO
typedef       void    *wxMultiText ;
#else

// Multi-line text item
class wxMultiText: public wxbMultiText
{
  DECLARE_DYNAMIC_CLASS(wxMultiText)

 public:
  wxMultiText(void);
  wxMultiText(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "multiText");

  Bool Create(wxPanel *panel, wxFunction func, char *label, char *value="",
         int x=-1, int y=-1, int width=-1, int height=-1,
         long style=0, char *name = "multiText");
  char *GetValue(void);
  virtual void GetValue(char *buffer, int maxLen);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;
};

#endif // IN_CPROTO
#endif // wx_mtxth
