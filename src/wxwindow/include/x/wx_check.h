/*
 * File:	wx_check.h
 * Purpose:	Declares wxCheckBox panel item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_check.h	1.2 5/9/94" */

#ifndef wx_checkh
#define wx_checkh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_check.h"

#ifdef IN_CPROTO
typedef       void    *wxCheckBox ;
#else

// Checkbox item (single checkbox)
class wxBitmap;
class wxPanel;
class wxCheckBox: public wxbCheckBox
{
  DECLARE_DYNAMIC_CLASS(wxCheckBox)

 public:
  wxBitmap *buttonBitmap ;
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
  virtual void SetValue(Bool);
  virtual Bool GetValue(void);
  void SetLabel(char *);
  void SetLabel(wxBitmap *bitmap);
  char *GetLabel(void) ;

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
#endif // wx_checkh
