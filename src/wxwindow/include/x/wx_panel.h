/*
 * File:	wx_panel.h
 * Purpose:	wxPanel subwindow, for panel items (widgets/controls)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_panel.h	1.2 5/9/94" */

#ifndef wx_panelh
#define wx_panelh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_panel.h"

#ifdef wx_motif
#include <Xm/Xm.h>
#endif

#ifdef wx_motif
extern void wxPanelRepaintProc(Widget w, XtPointer c_data, XEvent *event, char *);
#endif

#define PANEL_HSPACING  14
#define PANEL_VSPACING  12
#define PANEL_LEFT_MARGIN 4
#define PANEL_TOP_MARGIN  4

#ifdef IN_CPROTO
typedef       void    *wxPanel ;
#else

class wxItem;
class wxFrame;
class wxPanel: public wxbPanel
{
  DECLARE_DYNAMIC_CLASS(wxPanel)

 protected:
  // A brush created especially for the
  // background of this panel.
  wxBrush *panelBackgroundBrush;
 public:
  // For panel item positioning.
  int cursor_x;
  int cursor_y;
  int max_width;
  int max_height;
  int max_line_height;
  wxWindow *last_created ;

  Widget panelWidget;
  Widget lastWidget;
  Widget firstRowWidget;
  Bool allRelative;  // If TRUE, all widgets are relatively placed so we can do
                     // some Motif-specific things to space them better
  int currentRow;
  int currentCol;

  // To help with focus processing
  Bool manualChange;
  wxItem *previousFocus;

  wxPanel(void);
  wxPanel(wxWindow *parent,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");

  ~wxPanel(void);

  Bool Create(wxWindow *parent,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  // Avoid compiler warning
  void SetSize(int w, int h) { wxCanvas::SetSize(w, h); }
  void SetClientSize(int w, int h);
  void GetPosition(int *x, int *y);
  void Centre(int direction) ;
  virtual void Enable(Bool Flag) ;
  void AddChild(wxObject *child) ;

  // Start a new line
  void NewLine(void);
  void NewLine(int pixels);
  void RealNewLine(void) ;
  // Tab specified number of pixels
  void Tab(void);
  void Tab(int pixels);

  void GetCursor(int *x, int *y);
  void SetItemCursor(int x, int y);

  // Set/get horizontal spacing
  void SetHorizontalSpacing(int sp);
  int GetHorizontalSpacing(void);

  // Set/get vertical spacing
  void SetVerticalSpacing(int sp);
  int GetVerticalSpacing(void);

  // Fits the panel around the items
  void Fit(void);

  // Update next cursor position
  void AdvanceCursor(wxWindow *item);
  void RealAdvanceCursor(void);

  // If x or y are not specified (i.e. < 0), supply
  // values based on left to right, top to bottom layout.
  // Internal use only.
  void GetValidPosition(int *x, int *y);

  void OptimizeLayout(void);
  void AttachWidget(wxPanel *panel, Widget formWidget,
                    int x, int y, int width, int height) ;
  void AttachWidget(wxWindow *item, Widget formWidget,
                    int x, int y, int width, int height) ;

  void SetBackgroundColour(wxColour*col)
          { backColour = col; ChangeColour(); }
  void SetLabelColour(wxColour*col)
          { labelColour = col ; ChangeColour(); }
  void SetButtonColour(wxColour*col)
          { buttonColour = col ; ChangeColour(); }
  void ChangeColour(void) ;

  virtual void DoPaint(XRectangle *xrect, int n);
  virtual void OnPaint(void);

  Window GetXWindow(void);

  // Override edit mode so we can remove translations etc.
  void SetUserEditMode(Bool edit);
};

#endif // IN_CPROTO
#endif // wx_panelh
