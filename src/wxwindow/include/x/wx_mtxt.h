/*
 * File:	wx_mtxt.h
 * Purpose:	Declares multi-line text panel item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_mtxt.h	1.2 5/9/94" */

#ifndef wx_mtxth
#define wx_mtxth

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_mtxt.h"

#ifdef IN_CPROTO
typedef       void    *wxMultiText ;
#else

class wxPanel;

// Multi-line text item
class wxMultiText: public wxbMultiText
{
  DECLARE_DYNAMIC_CLASS(wxMultiText)

 public:
  wxMultiText(void);
  wxMultiText(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "multiText");

  Bool Create(wxPanel *panel, wxFunction func, char *label, char *value,
         int x=-1, int y=-1, int width=-1, int height=-1,
         long style=0, char *name="multiText");
  char *GetValue(void);
  virtual void GetValue(char *buffer, int maxLen);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxText::SetSize(width, height); }

  void ChangeColour(void) ;
};

#endif // IN_CPROTO
#endif // wx_mtxth
