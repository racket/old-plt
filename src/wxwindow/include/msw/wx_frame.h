/*
 * File:	wx_frame.h
 * Purpose:	wxFrame declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_frameh
#define wx_frameh

#include "wb_frame.h"

class wxFrame: public wxbFrame
{
 public:
  wxStatusWnd *status_window[wxMAX_STATUS];
  short hiddenmax, client_dw, client_dh;

  wxFrame(wxFrame *parent, char *title,
          int x=-1, int y=-1, int width=-1, int height=-1,
          long style = 0, char *name = "frame");

  ~wxFrame(void);

  Bool Create(wxFrame *parent, char *title,
          int x=-1, int y=-1, int width=-1, int height=-1,
          long style = 0, char *name = "frame");

  void SetClientSize(int width, int height);
  void GetClientSize(int *width, int *height);

  void GetSize(int *width, int *height);
  void GetPosition(int *x, int *y);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  Bool Show(Bool show);

  // Set menu bar
  void SetMenuBar(wxMenuBar *menu_bar);

  // Set title
  void SetTitle(char *title);
  char *GetTitle(void);

  // Set icon
  void SetIcon(wxBitmap *icon, wxBitmap *bg = NULL, int kind = 0);

  // Create status line
  void CreateStatusLine(int number=1, char *name = "status_line");

  // Set status line text
  void SetStatusText(char *text, int number = 0);

  // Fit frame around subwindows
  void Fit(void);
  virtual void ChangeToGray(Bool gray);

  // Iconize
  virtual void Iconize(Bool iconize);
  virtual Bool Iconized(void);
  // Windos 3.x maximize/restore
  virtual void Maximize(Bool maximize);

  void PositionStatusWindow(void);
  HMENU GetWinMenu(void);

  void DrawMenuBar(void);
  void SystemMenu(void);
};

#endif // wx_frameh
