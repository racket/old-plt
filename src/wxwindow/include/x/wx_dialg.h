/*
 * File:	wx_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_dialgh
#define wx_dialgh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_item.h"
#include "wb_dialg.h"

#ifdef wx_xview
#include <xview/frame.h>
#endif

#ifdef IN_CPROTO
typedef void    *wxDialogBox ;
#else

// Dialog boxes
class wxDialogBox: public wxbDialogBox
{
  DECLARE_DYNAMIC_CLASS(wxDialogBox)

 private:
  char *dialogTitle;
#ifdef wx_xview
  Frame xFrame;
#endif
 public:
  Bool modal_showing;
#ifdef wx_motif
  Bool invisibleResize ;
  Widget dialogShell;
  void PostDestroyChildren(void);
#endif
  wxDialogBox(void);
  wxDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = wxDEFAULT_DIALOG_STYLE,
              char *name = "dialogBox");
  ~wxDialogBox();

  Bool Create(wxWindow *parent, char *title, Bool modal=FALSE,
              int x=-1, int y=-1,
              int width=-1, int height=-1, long style=wxDEFAULT_DIALOG_STYLE,
              char *name="dialogBox");
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxPanel::SetSize(width, height); }
  void SetClientSize(int width, int height);
  void GetSize(int *w, int *h);
  void GetPosition(int *x, int *y);
  Bool Show(Bool show);
  void Iconize(Bool iconize);
  Bool Iconized(void);
  void Fit(void);
  void SetTitle(char *title);
  char *GetTitle(void);

  virtual void OnSize(int w, int h);

#ifdef wx_xview
  inline Frame GetXFrame(void) { return xFrame; }
  inline void SetXFrame(Frame f) { xFrame = f; }
#endif
};

#endif // IN_CPROTO
#endif // wx_dialgh
