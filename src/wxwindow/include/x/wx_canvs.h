/*
 * File:	wx_canvs.h
 * Purpose:	wxCanvas subwindow declarations (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_canvs.h	1.2 5/9/94" */

#ifndef wx_canvsh
#define wx_canvsh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_canvs.h"

#ifdef wx_xview
#include <xview/canvas.h>
#include <xview/scrollbar.h>
#include <xview/xv_xrect.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxCanvas ;
#else

// Canvas subwindow for drawing on
class wxCanvas: public wxbCanvas
{
  DECLARE_DYNAMIC_CLASS(wxCanvas)

 public:
  int units_per_page_x;
  int units_per_page_y;
  int units_x ;
  int units_y ;
  Bool requiresRetention; // Not the same as IsRetained()!
  Bool scrolls_set_size;
  wxList updateRects;     // List of wxRectangles representing damaged region
#ifdef wx_motif
  Bool requiresBackingStore ;
  Widget scrolledWindow;
  Bool PreResize(void);
  Bool hScroll;
  Bool vScroll;
  int  hExtent;   // Actual extent of virtual scrolled canvas
  int  vExtent;
  int hStart;
  int vStart;
  Widget hScrollBar;
  Widget vScrollBar;
  Widget borderWidget;
  Bool allowRepainting;
  Bool hScrollingEnabled;
  Bool vScrollingEnabled;
  Pixmap backingPixmap;
  int pixmapWidth;
  int pixmapHeight;
  int pixmapOffsetX;
  int pixmapOffsetY;

  // Call Refresh (Motif) or OnPaint (XView). Overriden
  // by wxPanel so XView can do something different.
  virtual void DoPaint(XRectangle *xrect, int n);
  void DoRefresh(Bool paint);
  void PhysicalScroll(int, int, int, int, int, int);
#endif
#ifdef wx_xview
  int DRAG_MAX;
  int drag_count;
//  Xv_xrectlist *xrects;
  Xv_opaque selectionRequestor;

// Every time a callback happens, these are set to point to the right values
// for drawing calls to work
  Xv_Window paint_window;
  Scrollbar horiz_scroll;
  Scrollbar vert_scroll;

  // Temporary data needed for scrollbar notification
  Notify_client xviewClient;
  Event *xviewEvent;
  Notify_event_type xviewEventType;
  Notify_value xviewReturnValue;
#endif
  wxCanvas(void);
  wxCanvas(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
           long style = wxRETAINED, char *name = "canvas");
  virtual ~wxCanvas(void);

  Bool Create(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
           long style = wxRETAINED, char *name = "canvas");
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  // Avoid compiler warning
  void SetSize(int w, int h) { wxbCanvas::SetSize(w, h); }
  void SetClientSize(int width, int height);
  void GetSize(int *width, int *height);
  void GetClientSize(int *width, int *height);
  void GetPosition(int *x, int *y);

  // Number of pixels per user unit (0 or -1 for no scrollbar)
  // Length of virtual canvas in user units
  // Length of page in user units
  void SetScrollbars(int horizontal, int vertical,
                             int xLength, int yLength,
                             int xPage, int yPage,
                             int xPos = 0, int yPos = 0,
                             Bool setVirtualSize = TRUE);

  // Scroll the canvas
  void Scroll(int x_pos, int y_pos);
  void GetScrollUnitsPerPage(int *x_page, int *y_page);

  void OnScroll(wxCommandEvent& event);
  void SetScrollPos(int orient, int pos);
  void SetScrollRange(int orient, int range);
  void SetScrollPage(int orient, int page);
  int GetScrollPos(int orient);
  int GetScrollRange(int orient);
  int GetScrollPage(int orient);

  void DoScroll(wxCommandEvent &event);

#ifdef wx_xview
  virtual void DragAcceptFiles(Bool accept = TRUE);
#endif

  Display *display;
  Window xwindow;

  void ViewStart(int *x, int *y);

  // Actual size in pixels when scrolling is taken into account
  void GetVirtualSize(int *x, int *y);

  void SetColourMap(wxColourMap *cmap);
  wxCursor *SetCursor(wxCursor *cursor);

  // Enable/disable Windows 3.1 scrolling in either direction.
  // If TRUE, wxWindows scrolls the canvas and only a bit of
  // the canvas is invalidated; no Clear() is necessary.
  // If FALSE, the whole canvas is invalidated and a Clear() is
  // necessary. Disable for when the scroll increment is used
  // to actually scroll a non-constant distance
  void EnableScrolling(Bool x_scrolling, Bool y_scrolling);

  Bool IsRetained(void) { return is_retained; }

  virtual void WarpPointer(int x_pos, int y_pos) ;

  Window GetXWindow(void);
};

// Allows iteration through damaged rectangles in OnPaint
class wxUpdateIterator
{
 private:
  int current;					        // Current rectangle index
  wxWindow *win;
 public:
  wxUpdateIterator(wxWindow* wnd);
  ~wxUpdateIterator(void);

  operator int (void);
  wxUpdateIterator* operator ++(int);
  void GetRect(wxRectangle *rect);
  int GetX();
  int GetY();
  int GetW();
  int GetH();
};

#endif // IN_CPROTO
#endif // wc_canvsh
