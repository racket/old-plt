/*
 * File:	wb_canvs.h
 * Purpose:	wxCanvas subwindow declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_canvs.h	1.2 5/9/94" */

#ifndef wxb_canvsh
#define wxb_canvsh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef IN_CPROTO
#include <iostream.h>
#endif
#include "common.h"
#include "wx_frame.h"
#include "wx_gdi.h"
#include "wx_dccan.h"
#include "wx_stdev.h"

// Convenience macros for accessing scroll event parameters
#define WXSCROLLPOS(event) event.commandInt
#define WXSCROLLORIENT(event) event.extraLong

#ifdef IN_CPROTO
typedef void *wxbCanvas ;
#else

// Canvas subwindow for drawing on
class wxbCanvas: public wxWindow
{
 public:
  wxCanvasDC *wx_dc;    // The canvas's device context

  Bool is_retained;
  int horiz_units;
  int vert_units;

  wxbCanvas(void);
  wxbCanvas(wxWindow *frame, int x=-1, int y=-1, int width=-1, int height=-1,
           long style = wxRETAINED, char *name = "canvas");
  virtual ~wxbCanvas(void);

  virtual void AllowDoubleClick(int value) ;

  void OnChar(wxKeyEvent& event);

  // Number of pixels per user unit (0 or -1 for no scrollbar)
  // Length of virtual canvas in user units
  // Length of page in user units
  virtual void SetScrollbars(int horizontal, int vertical,
                             int x_length, int y_length,
                             int x_page, int y_page,
                             int x_pos = 0, int y_pos = 0, Bool setVirtualSize = TRUE) = 0;

  // Scroll the canvas
  virtual void Scroll(int x_pos, int y_pos) = 0;
  virtual void ViewStart(int *x, int *y) = 0;
  virtual void GetScrollUnitsPerPage(int *x_page, int *y_page) = 0;

  virtual void Clear(void);

  // Actual size in pixels when scrolling is taken into account
  virtual void GetVirtualSize(int *x, int *y) = 0;
  virtual float GetCharHeight(void);
  virtual float GetCharWidth(void);
  /* MATTHEW: [2] 16-bit fonts */
  virtual void GetTextExtent(const char *string, float *x, float *y,
                             float *descent = NULL, 
                             float *externalLeading = NULL, 
                             wxFont *theFont = NULL, Bool use16 = FALSE);

  // Gets 'context' member
  virtual wxCanvasDC *GetDC(void);

  virtual void SetColourMap(wxColourMap *cmap) = 0;

  // Enable/disable Windows 3.1 scrolling in either direction.
  // If TRUE, wxWindows scrolls the canvas and only a bit of
  // the canvas is invalidated; no Clear() is necessary.
  // If FALSE, the whole canvas is invalidated and a Clear() is
  // necessary. Disable for when the scroll increment is used
  // to actually scroll a non-constant distance
  virtual void EnableScrolling(Bool x_scrolling, Bool y_scrolling) = 0;

  Bool IsRetained(void) { return is_retained; }

  virtual void WarpPointer(int x_pos, int y_pos) = 0 ;
};

#endif // IN_CPROTO
#endif // wxb_canvsh
