/*
 * File:	wb_panel.h
 * Purpose:	wxPanel subwindow, for panel items (widgets/controls)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_panelh
#define wxb_panelh

#include "common.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_canvs.h"

#define wxKEY_SHIFT     1
#define wxKEY_CTRL      2

class wxItem;
class wxButton;
class wxPanel;
class wxColour;
class wxBrush;
class wxPanelDC;
class wxStaticItem;

class wxbPanel: public wxCanvas
{
 public:
  Bool new_line;
  int label_position;
  wxButton *defaultItem;


  int hSpacing;
  int vSpacing;

  int current_hspacing ;
  int current_vspacing ;

  int initial_hspacing ;
  int initial_vspacing ;
  Bool has_child ;

  wxFont *labelFont ;
  wxFont *buttonFont;
  wxColour *backColour ;
  wxColour *labelColour;
  wxColour *buttonColour;

  wxbPanel(void);
  wxbPanel(wxWindow *window,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");
  ~wxbPanel(void);

  wxPanelDC *GetPanelDC(void) { return (wxPanelDC *)wx_dc; } ;
  // Set current label position, i.e. will label be on top or to the left
  virtual void SetLabelPosition(int pos);  // wxHORIZONTAL or wxVERTICAL
  int GetLabelPosition(void);

  virtual void SetButtonFont(wxFont *font);
  virtual void SetLabelFont(wxFont *font);
  virtual void SetButtonColour(wxColour *col);
  virtual void SetLabelColour(wxColour *col);
  virtual void SetBackgroundColour(wxColour *col);

  inline virtual wxFont  *GetLabelFont(void)        { return labelFont ; }
  inline virtual wxFont  *GetButtonFont(void)       { return buttonFont ; }
  inline virtual wxColour*GetBackgroundColour(void) { return backColour ; }
  inline virtual wxColour*GetLabelColour(void)      { return labelColour ; }
  inline virtual wxColour*GetButtonColour(void)     { return buttonColour ; }

  // Start a new line
  virtual void NewLine(void) = 0;
  virtual void NewLine(int pixels) = 0;

  // Tab specified number of pixels
  virtual void Tab(void) = 0;
  virtual void Tab(int pixels) = 0;

  virtual void GetCursor(int *x, int *y) = 0;

  // Set/get horizontal spacing
  virtual void SetHorizontalSpacing(int sp) = 0;
  virtual int GetHorizontalSpacing(void) = 0;

  // Set/get vertical spacing
  virtual void SetVerticalSpacing(int sp) = 0;
  virtual int GetVerticalSpacing(void) = 0;

  // Update next cursor position
  virtual void AdvanceCursor(wxWindow *item) = 0;

  // If x or y are not specified (i.e. < 0), supply
  // values based on left to right, top to bottom layout.
  // Internal use only.
  virtual void GetValidPosition(int *x, int *y) = 0;

  inline virtual wxButton *GetDefaultItem(void) { return defaultItem; }

  wxObject *GetChild(int number) ;

  // Override to define new behaviour for default action (e.g. double clicking
  // on a listbox)
  virtual void OnDefaultAction(wxItem *initiatingItem);
  virtual void OnChangeFocus(wxItem *from, wxItem *to);
  virtual Bool OnFunctionKey(wxKeyEvent &event);

  void OnEvent(wxMouseEvent *event);
  virtual void OnItemEvent(wxItem *item, wxMouseEvent& event);
  virtual void ProcessItemEvent(wxItem *item, wxMouseEvent& event, int selectionHandle);

  // Calls DrawSelectionHandles for all items if
  // edit mode is on.
  virtual void PaintSelectionHandles(void);
};

#endif // wxb_panelh
