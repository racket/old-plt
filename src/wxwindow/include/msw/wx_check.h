/*
 * File:	wx_check.h
 * Purpose:	Check box
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_check.h	1.2 5/9/94" */

#ifndef wx_checkh
#define wx_checkh

#include "wb_check.h"

#ifdef IN_CPROTO
typedef       void    *wxCheckBox ;
#else

// Checkbox item (single checkbox)
class wxBitmap;
class wxCheckBox: public wxbCheckBox
{
  DECLARE_DYNAMIC_CLASS(wxCheckBox)

 public:
  int checkWidth ;
  int checkHeight ;
  wxCheckBox(void);
  wxCheckBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  wxCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  ~wxCheckBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  Bool Create(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  void SetValue(Bool);
  Bool GetValue(void);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  virtual BOOL MSWCommand(UINT param, WORD id);
  void SetLabel(char *);
  void SetLabel(wxBitmap *bitmap);
  char *GetLabel(void) ;

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;

  wxBitmap *bm_label;
};

#endif // IN_CPROTO
#endif // wx_checkh
