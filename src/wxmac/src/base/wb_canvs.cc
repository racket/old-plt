/*
 * File:	wb_canvs.cc
 * Purpose:	wxbCanvas implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_canvs.cc,v 1.3 1998/09/20 22:32:21 robby Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_canvs.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#include "common.h"
#include "wx_setup.h"
#include "wx_dc.h"
#include "wx_canvs.h"

#include "wx.h"

class wxFrame;

#ifndef wx_mac
wxbCanvas::wxbCanvas(void)
{
  __type = wxTYPE_CANVAS;
}
#endif // wx_mac

#ifndef wx_mac
wxbCanvas::wxbCanvas(wxWindow *window, int x, int y, int width, int height, long style,
                     char *name)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}
#endif // wx_mac
#ifdef wx_mac
// Constructor (given parentArea)
wxbCanvas::wxbCanvas (char* windowName, wxArea* parentArea, int x, int y,
		int width, int height, long style)
  : wxWindow ( windowName, parentArea, x, y, width, height, style),
		is_retained (FALSE), // Can only be retained after scrollbars have been set
		horiz_units (0),
		vert_units (0)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}

// Constructor (given parentWindow)
wxbCanvas::wxbCanvas(char* windowName, wxWindow* parentWindow, int x, int y,
		int width, int height, long style) 
  : wxWindow ( windowName, parentWindow, x, y, width, height, style),
		is_retained (FALSE), // Can only be retained after scrollbars have been set
		horiz_units (0),
		vert_units (0)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}
#endif // wx_mac

wxbCanvas::~wxbCanvas(void)
{
}

void wxbCanvas::AllowDoubleClick(int value)
{
  doubleClickAllowed = value ;
}


wxCanvasDC *wxbCanvas::GetDC(void)
{
  return wx_dc;
}

// Default input behaviour for a scrolling canvas should be to scroll
// according to the cursor keys pressed
void wxbCanvas::OnChar(wxKeyEvent* event)
{
  int x_page = 0;
  int y_page = 0;
  int start_x = 0;
  int start_y = 0;
  GetScrollUnitsPerPage(&x_page, &y_page);
  ViewStart(&start_x, &start_y);

  switch (event->keyCode)
  {
    case WXK_PRIOR:
    {
      if ((y_page > 0) && (start_y >= y_page))
        Scroll(start_x, start_y - y_page);
      break;
    }
    case WXK_NEXT:
    {
      if (y_page > 0)
        Scroll(start_x, start_y + y_page);
      break;
    }
    case WXK_UP:
    {
      if ((y_page > 0) && (start_y >= 1))
        Scroll(start_x, start_y - 1);
      break;
    }
    case WXK_DOWN:
    {
      if (y_page > 0)
        Scroll(start_x, start_y + 1);
      break;
    }
    case WXK_LEFT:
    {
      if ((x_page > 0) && (start_x >= 1))
        Scroll(start_x - 1, start_y);
      break;
    }
    case WXK_RIGHT:
    {
      if (x_page > 0)
        Scroll(start_x + 1, start_y);
      break;
    }
    case WXK_HOME:
    {
      Scroll(0, 0);
      break;
    }
  }
}

float wxbCanvas::GetCharHeight(void)
{
  return wx_dc->GetCharHeight();
}

float wxbCanvas::GetCharWidth(void)
{
  return wx_dc->GetCharWidth();
}

#ifdef wx_mac
void wxbCanvas::GetTextExtent(const char* string, float* x, float* y, float* descent,
  						float* externalLeading, wxFont* the_font, Bool use16)
{
  wx_dc->GetTextExtent(string, x, y, descent, externalLeading, the_font, use16);
}
#else // wx_mac
void wxbCanvas::GetTextExtent(const char *string, float *x, float *y,
                              float *descent, float *externalLeading)
{
  wx_dc->GetTextExtent(string, x, y, descent, externalLeading);
}
#endif // wx_mac

