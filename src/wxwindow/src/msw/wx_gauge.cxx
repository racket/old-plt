/*
 * File:	wx_gauge.cc
 * Purpose:	Gauge implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "..\..\contrib\gauge\zyzgauge.h"

wxGauge::wxGauge(wxPanel *panel, char *label,
		 int range, int x, int y, int width, int height,
		 long style, char *name):
  wxbGauge(panel, label, range, x, y, width, height, style, name)
{
  Create(panel, label, range, x, y, width, height, style, name);
}

Bool wxGauge::Create(wxPanel *panel, char *label,
		     int range, int x, int y, int width, int height,
		     long style, char *name)
{
  panel->AddChild(this);

  static_label = 0;
  wxWinType = wxTYPE_HWND;
  windowStyle = style;

  wxWnd *cparent = (wxWnd *)(panel->handle);

  labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

  // If label exists, create a static control for it.
  if (label) {
    static_label = wxwmCreateWindowEx(0, STATIC_CLASS, label,
				      STATIC_FLAGS | WS_CLIPSIBLINGS,
				      0, 0, 0, 0, cparent->handle, (HMENU)NewId(this),
				      wxhInstance, NULL);
    HDC the_dc = GetWindowDC(static_label) ;
    if (labelFont && labelFont->GetInternalFont(the_dc))
      SendMessage(static_label,WM_SETFONT,
                  (WPARAM)labelFont->GetInternalFont(the_dc),0L);
    ReleaseDC(static_label,the_dc) ;
  } else
    static_label = NULL;

  windows_id = (int)NewId(this);
  
  HWND wx_button =
    wxwmCreateWindowEx(0, "zYzGauge", label, 
		       WS_CHILD | WS_TABSTOP | WS_CLIPSIBLINGS,
		       0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
		       wxhInstance, NULL);
  
  ms_handle = (HANDLE)wx_button;
  
  SubclassControl(wx_button);

  int wOrient = 0;

  if (windowStyle & wxHORIZONTAL)
    wOrient = ZYZG_ORIENT_LEFTTORIGHT;
  else
    wOrient = ZYZG_ORIENT_BOTTOMTOTOP;
  
  SendMessage(wx_button, ZYZG_SETORIENTATION, wOrient, 0);
  SendMessage(wx_button, ZYZG_SETRANGE, range, 0);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;

  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  SetSize(x, y, width, height, wxSIZE_AUTO);

  if (!(style & wxINVISIBLE)) {
    ShowWindow(wx_button, SW_SHOW);
    if (static_label)
      ShowWindow(static_label, SW_SHOW);
  }

  panel->AdvanceCursor(this);

  if (style & wxINVISIBLE)
    Show(FALSE);

  return TRUE;
}

wxGauge::~wxGauge(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
    wxwmDestroyWindow(static_label);
  static_label = NULL;
}

void wxGauge::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  int clx; // label font dimensions
  int cly;

  float label_width, label_height, label_x, label_y;
  float control_width, control_height, control_x, control_y;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
    GetSize(&width, &height);

  int defwidth, defheight;
  defwidth = ((windowStyle & wxHORIZONTAL) ? 100 : 24);
  defheight = ((windowStyle & wxHORIZONTAL) ? 24 : 100);

  if (static_label) {
    // Find size of label
    char buf[300];
    wxGetCharSize((HWND)ms_handle, &clx, &cly, labelFont);
    GetWindowText(static_label, buf, 300);
    GetTextExtent(wxStripMenuCodes(buf), &label_width, &label_height, NULL, NULL, labelFont);

    // Given size is total label + edit size, find individual
    // control sizes on that basis.
    if (labelPosition == wxHORIZONTAL) {
      label_x = (float)x;
      label_y = (float)y;

      control_x = label_x + label_width + clx;
      control_y = (float)y;
      if (width <= 0)
	control_width = defwidth;
      else
	control_width = width - (control_x - label_x);
      if (height <= 0)
	control_height = defheight;
      else
	control_height = (float)height;
    } else { // wxVERTICAL
      label_x = (float)x;
      label_y = (float)y;

      control_x = (float)x;
      control_y = label_y + label_height + 3;
      if (width <= 0)
	control_width = defwidth;
      else
	control_width = (float)width;
      if (height <= 0)
	control_height = defheight;
      else
	control_height = height - (label_height + 3);
    }

    MoveWindow(static_label, (int)label_x, (int)label_y,
               (int)label_width, (int)label_height, TRUE);
  } else {
    // Deal with default size (using -1 values)
    if (width <= 0)
      width = defwidth;
    if (height <= 0)
      height = defheight;

    control_x = (float)x;
    control_y = (float)y;
    control_width = (float)width;
    control_height = (float)height;
  }

  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y, 
	     (int)control_width, (int)control_height, TRUE);
  
  OnSize(width, height);
}

void wxGauge::GetSize(int *width, int *height)
{
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
  {
    wxFindMaxSize(static_label, &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxGauge::GetPosition(int *x, int *y)
{
  wxWindow *parent = GetParent();
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);
  if (static_label)
    wxFindMaxSize(static_label, &rect);

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  POINT point;
  point.x = rect.left;
  point.y = rect.top;
  if (parent)
  {
    wxWnd *cparent = (wxWnd *)(parent->handle);
    ::ScreenToClient(cparent->handle, &point);
  }

  *x = point.x;
  *y = point.y;
}

Bool wxGauge::Show(Bool show)
{
  wxWindow::Show(show);
  if (static_label) 
    ShowWindow(static_label, show ? SW_SHOW : SW_HIDE);
  return TRUE;
}

void wxGauge::SetRange(int r)
{
  SendMessage((HWND)ms_handle, ZYZG_SETRANGE, r, 0);
}

void wxGauge::SetValue(int pos)
{
  SendMessage((HWND)ms_handle, ZYZG_SETPOSITION, pos, 0);
}

void wxGauge::SetLabel(char *label)
{
  if (static_label)
  {
    float w, h;
    RECT rect;

    wxWindow *parent = GetParent();
    GetWindowRect(static_label, &rect);

    // Since we now have the absolute screen coords,
    // if there's a parent we must subtract its top left corner
    POINT point;
    point.x = rect.left;
    point.y = rect.top;
    if (parent)
    {
      wxWnd *cparent = (wxWnd *)(parent->handle);
      ::ScreenToClient(cparent->handle, &point);
    }

    GetTextExtent(label, &w, &h, NULL, NULL,labelFont);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowText(static_label, label);
  }
}

void wxGauge::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  if (static_label)
    ::EnableWindow(static_label, !gray);
}
