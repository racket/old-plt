/*
 * File:	wx_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_dialgh
#define wx_dialgh

#include "common.h"
#include "wx_item.h"
#include "wx_check.h"
#include "wx_messg.h"
#include "wb_dialg.h"

#ifdef IN_CPROTO
typedef void    *wxDialogBox ;
#else

// Dialog boxes
class wxDialogBox: public wxbDialogBox
{
  DECLARE_DYNAMIC_CLASS(wxDialogBox)

 public:
  Bool modal_showing;

  wxDialogBox(void);
  wxDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = 0,
              char *name = "dialogBox");
  ~wxDialogBox(void);

  Bool Create(wxWindow *parent, char *title, Bool modal=FALSE,
              int x=-1, int y=-1,
              int width=-1, int height=-1, long style=0,
              char *name="dialogBox");
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetClientSize(int width, int height);
  void GetPosition(int *x, int *y);
  Bool Show(Bool show);
  void Iconize(Bool iconize);
  Bool Iconized(void);
  void Fit(void);

  void SetTitle(char *title);
  char *GetTitle(void);

  virtual void ChangeToGray(Bool gray);

  Bool OnCharHook(wxKeyEvent& event);

  void SystemMenu(void);
};

#endif // IN_CPROTO
#endif // wx_dialgh
