/*
 * File:	wb_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_dialg.h	1.2 5/9/94" */

#ifndef wxb_dialgh
#define wxb_dialgh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"

#ifdef IN_CPROTO
typedef       void    *wxbDialogBox ;
#else

// Dialog boxes
class wxbDialogBox: public wxPanel
{
 protected:
  Bool modal;
  Bool is_show;
 public:

  void *context;

  wxbDialogBox(void);
  wxbDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = 0, char *name = "panel");
  ~wxbDialogBox();

  Bool Create(wxWindow *window, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = 0, char *name = "panel");

  virtual void Iconize(Bool iconize) = 0;
  virtual Bool Iconized(void) = 0;

  void Centre(int direction = wxBOTH);
  virtual Bool IsModal(void) { return modal; }
  virtual void SetShowing(Bool show) { is_show = show; }
};

#endif // IN_CPROTO
#endif // wxb_dialgh
