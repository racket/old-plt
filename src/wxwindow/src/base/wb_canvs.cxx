/*
 * File:	wb_canvs.cc
 * Purpose:	wxbCanvas implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_canvs.cxx,v 1.3 1998/09/21 05:21:14 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_canvs.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_dc.h"
#include "wb_canvs.h"

#endif

class wxFrame;

wxbCanvas::wxbCanvas(void)
{
  __type = wxTYPE_CANVAS;
}

wxbCanvas::wxbCanvas(wxWindow *WXUNUSED(window), int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height),
   long style, char *WXUNUSED(name))
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}

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

void wxbCanvas::Clear(void)
{
  if (wx_dc)
    wx_dc->Clear();
}

// Default input behaviour for a scrolling canvas should be to scroll
// according to the cursor keys pressed
void wxbCanvas::OnChar(wxKeyEvent *event)
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
  if (wx_dc)
    return wx_dc->GetCharHeight();
  else
    return 0.0;
}

float wxbCanvas::GetCharWidth(void)
{
  if (wx_dc)
    return wx_dc->GetCharWidth();
  else
    return 0.0;
}

void wxbCanvas::GetTextExtent(const char *string, float *x, float *y,
                              float *descent, float *externalLeading,
			      wxFont *theFont,
			      Bool use16)
{
  if (wx_dc)
    wx_dc->GetTextExtent(string, x, y, descent, externalLeading, theFont, 
			 use16);
  else
    wxWindow::GetTextExtent(string, x, y, descent, externalLeading, theFont);
}
