/*
 * File:	wx_win.cc
 * Purpose:	wxWindow class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994     
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <math.h>
#include <shellapi.h>
#include <windowsx.h>

#include "fafa.h"

extern HBRUSH SetupBackground(HWND wnd); // in wx_main.cc

// Global variables
wxMenu **wxCurrentPopupMenu = NULL;
static wxWindow *current_mouse_wnd = NULL;
static void *current_mouse_context = NULL;

// Hook for new window just as it's being created,
// when the window isn't yet associated with the handle
wxWnd *wxWndHook = NULL;

extern void wxQueueLeaveEvent(void *ctx, wxWindow *wnd, int x, int y, int flags);

extern long last_msg_time; /* timeStamp implementation */

static void wxDoOnMouseLeave(wxWindow *wx_window, int x, int y, UINT flags);
static void wxDoOnMouseEnter(wxWindow *wx_window, int x, int y, UINT flags);

void wxWindowInit(void)
{
  wxREGGLOB(wxCurrentPopupMenu);
  wxREGGLOB(current_mouse_wnd);
  wxREGGLOB(current_mouse_context);
  wxREGGLOB(wxWndHook);
}

// Find an item given the MS Windows id
wxWindow *wxWindow::FindItem(int id)
{
  if (!children)
    return NULL;
  wxChildNode *current = children->First();
  while (current)
  {
    wxObject *obj = (wxObject *)current->Data();
    if (wxSubType(obj->__type, wxTYPE_PANEL)) {
      // Do a recursive search.
      wxPanel *panel = (wxPanel*)obj;
      wxWindow *wnd = panel->FindItem(id);
      if (wnd)
        return wnd;
    } else if (wxSubType(obj->__type, wxTYPE_CANVAS)
	     || wxSubType(obj->__type, wxTYPE_TEXT_WINDOW)) {
      // Do nothing
    } else {
      wxItem *item = (wxItem *)current->Data();
      if (item->windows_id == id)
        return item;
      else {
	// In case it's a 'virtual' control (e.g. radiobox)
	if (item->subControls
	    && item->subControls->Member((wxObject *)id))
          return item;
      }
    }
    current = current->Next();
  }
  return NULL;
}

// Find an item given the MS Windows handle
wxWindow *wxWindow::FindItemByHWND(HWND hWnd)
{
  if (!children)
    return NULL;
  wxChildNode *current = children->First();
  while (current) {
    wxObject *obj = (wxObject *)current->Data();
    if (wxSubType(obj->__type,wxTYPE_PANEL)) {
      // Do a recursive search.
      wxPanel *panel = (wxPanel*)obj;
      wxWindow *wnd = panel->FindItemByHWND(hWnd);
      if (wnd)
        return wnd;
    } else {
      wxItem *item = (wxItem *)current->Data();
      if ((HWND)(item->ms_handle) == hWnd)
        return item;
      else {
        // In case it's a 'virtual' control (e.g. radiobox)
        if (item->__type == wxTYPE_RADIO_BOX) {
          wxRadioBox *rbox = (wxRadioBox *)item;
          int i;
          for (i = 0; i < rbox->no_items; i++)
            if (rbox->radioButtons[i] == hWnd)
              return item;
        }
      }
    }
    current = current->Next();
  }
  return NULL;
}

// Default command handler
BOOL wxWindow::MSWCommand(UINT WXUNUSED(param), WORD WXUNUSED(id))
{
  return FALSE;
}

void wxWindow::PreDelete(HDC WXUNUSED(dc))
{
  mouseInWindow = FALSE;
}

HWND wxWindow::GetHWND(void)
{
  HWND hWnd = 0;

  switch (wxWinType)
  {
    case wxTYPE_XWND:
    case wxTYPE_MDICHILD:
    {
      wxWnd *wnd = (wxWnd *)handle;
	  if (wnd)
        hWnd = (HWND)wnd->handle;
      break;
    }
    default:
    {
      hWnd = (HWND)ms_handle;
      break;
    }
  }
  return hWnd;
}

// Constructor

wxWindow::wxWindow(void)
{
  ms_handle = 0;
  handle = NULL;
  mouseInWindow = FALSE;
  winEnabled = TRUE;
  cxChar = 0; cyChar = 0;
  windows_id = 0;
  mouseInWindow = FALSE;
  winEnabled = TRUE;

  focusWindow = NULL;
}

// Destructor
wxWindow::~wxWindow(void)
{
  if (current_mouse_wnd == this)
    current_mouse_wnd = NULL;

  if (window_parent)
    window_parent->RemoveChild(this);

  wxbWindow::DestroyChildren();
  switch (wxWinType)
  {
    case wxTYPE_XWND:
    {
      if (handle)
      {
        wxWnd *wnd = (wxWnd *)handle;
		  HDC dc = wxwmGetDC(wnd->handle);
		  PreDelete(dc);
		  wxwmReleaseDC(wnd->handle, dc);

        wnd->DestroyWindow();
        delete wnd;
        handle = NULL;
      }
      break;
    }
    case wxTYPE_MDICHILD:
    {
      wxMDIChild *child = (wxMDIChild *)handle;
      child->DestroyWindow();
      delete child;
      handle = NULL;
      break;
    }
    case wxTYPE_HWND:
    {
      if (ms_handle)
	wxwmDestroyWindow((HWND)ms_handle);
      handle = NULL;
      
      if (wxControlHandleList)
        wxControlHandleList->DeleteObject(this);

      break;
    }
    default:
      break;
    }

  delete children;
  children = NULL;
}

wxWindow *wxWindow::GetTopLevel()
{
  wxWindow *p = this;
  while (p && !(wxSubType(p->__type, wxTYPE_FRAME)
		|| wxSubType(p->__type, wxTYPE_DIALOG_BOX)))
    p = p->GetParent();
  
  return p;
}

void wxWindow::SetFocus(void)
{
  if (!IsShownTree())
    return;

  wxWindow *p = GetTopLevel();
  
  if (p && wxSubType(p->__type, wxTYPE_FRAME)
      && (((wxFrame *)p)->frame_type == wxMDI_CHILD)) {
    wxWindow *mdip = p->GetParent();
    DWORD r;

    if (mdip && (GetActiveWindow() != mdip->GetHWND()))
      r = 0;
    else
      r = ::SendMessage(((wxMDIFrame *)mdip->handle)->client_hwnd, WM_MDIGETACTIVE,
			(WPARAM)NULL, (LPARAM)NULL);
 
    if ((HWND)r != p->GetHWND()) {
      /* This frame not active within parent; remember local focus */
      p->focusWindow = this;
      return;
    }
    p = NULL;
  }
  
  // If the frame/dialog is not active, just set the focus
  //  locally.
  if (p) {
    p->focusWindow = this;
    
    if (GetActiveWindow() == p->GetHWND()) {
      HWND hWnd = GetHWND();
      if (hWnd)
	wxwmSetFocus(hWnd);
    }
  }
}

void wxWindow::ChangeToGray(Bool gray)
{
  /* Nothing extra to do over enabling */
}

Bool wxWindow::IsGray(void)
{
  return !winEnabled || internal_gray_disabled;
}

void wxWindow::DoEnableWindow(int on)
{
  HWND hWnd = GetHWND();
  if (hWnd)
    ::EnableWindow(hWnd, (BOOL)on); 
  if (!on) {
    wxWindow *p = GetTopLevel();
    if (p->focusWindow == this)
      p->SetFocus();
  }
}

void wxWindow::InternalEnable(Bool enable, Bool gray)
{
  Bool do_something;
  short start_igd = internal_gray_disabled;

  if (!enable) {
    do_something = !internal_disabled;
    internal_disabled++;
    if (gray)
      internal_gray_disabled++;
  } else { 
    --internal_disabled;
    do_something = !internal_disabled;
    if (gray)
      --internal_gray_disabled;
  }

  if (do_something && winEnabled) {
    DoEnableWindow((BOOL)enable);
  }

  if ((!!internal_gray_disabled != !!start_igd) && winEnabled)
    ChangeToGray(!!internal_gray_disabled);
}

void wxWindow::Enable(Bool enable)
{
  if (winEnabled == !!enable)
    return;

  winEnabled = enable;
  
  if (!internal_disabled) {
    DoEnableWindow((BOOL)enable);
  }

  /* Doing handle sensitive makes it gray: */
  if (!internal_gray_disabled)
    ChangeToGray(!enable);
}

void wxWindow::InternalGrayChildren(Bool gray)
{
  wxChildNode *cn;
  for (cn = GetChildren()->First(); cn; cn = cn->Next()) {
    wxWindow *w = (wxWindow *)cn->Data();
    w->InternalEnable(!gray, TRUE);
  }
}

void wxWindow::CaptureMouse(void)
{
  HWND hWnd = GetHWND();
  if (hWnd && !winCaptured) {
    SetCapture(hWnd);
    winCaptured = TRUE;
  }
}

void wxWindow::ReleaseMouse(void)
{
  if (winCaptured) {
    ReleaseCapture();
    winCaptured = FALSE;
  }
}

void wxWindow::DragAcceptFiles(Bool accept)
{
  HWND hWnd = GetHWND();
  if (hWnd)
    ::DragAcceptFiles(hWnd, (BOOL)accept);
}

// Get total size
void wxWindow::GetSize(int *x, int *y)
{
  HWND hWnd = GetHWND();
  RECT rect;
  GetWindowRect(hWnd, &rect);
  *x = rect.right - rect.left;
  *y = rect.bottom - rect.top;
}

void wxWindow::GetPosition(int *x, int *y)
{
  HWND hWnd = GetHWND();
  HWND hParentWnd = 0;
  if (GetParent())
    hParentWnd = GetParent()->GetHWND();
  
  RECT rect;
  GetWindowRect(hWnd, &rect);

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  POINT point;
  point.x = rect.left;
  point.y = rect.top;
  if (hParentWnd)
    ::ScreenToClient(hParentWnd, &point);
  *x = point.x;
  *y = point.y;

  if (*x < -10000)
    *x = -10000;
  if (*y < -10000)
    *y = -10000;
}

void wxWindow::ScreenToClient(int *x, int *y)
{
  HWND hWnd = GetHWND();
  POINT pt;
  pt.x = *x;
  pt.y = *y;
  ::ScreenToClient(hWnd, &pt);

  *x = pt.x;
  *y = pt.y;
}

void wxWindow::ClientToScreen(int *x, int *y)
{
  HWND hWnd = GetHWND();
  POINT pt;
  pt.x = *x;
  pt.y = *y;
  ::ClientToScreen(hWnd, &pt);

  *x = pt.x;
  *y = pt.y;
}

HCURSOR wxMSWSetCursor(HCURSOR c)
{
  return SetCursor(c);
}

wxWindow *wxLocationToWindow(int x, int y)
{
  POINT p;
  HWND hwnd;

  p.x = x;
  p.y = y;

  hwnd = WindowFromPoint(p);

  if (hwnd) {
    wxWnd *wnd = NULL;
    while (hwnd) {
      wnd = wxFindWinFromHandle(hwnd);
      if (wnd)
	break;
      else
	hwnd = GetParent(hwnd);
    }

    if (wnd && wnd->wx_window)
      return wnd->wx_window->GetTopLevel();
  }

  return NULL;
}

static wxWnd *wxCurrentWindow(int in_content)
{
  HWND hwnd;

  hwnd = GetCapture();
  if (!hwnd) {
    POINT pos;
    if (!GetCursorPos(&pos))
      return NULL;
    
    hwnd = WindowFromPoint(pos);
  } else
    /* Grab => always considered inside: */
    in_content = 0;
  if (!hwnd)
    return NULL;

  wxWnd *wnd = NULL;
  while (hwnd) {
    wnd = wxFindWinFromHandle(hwnd);
    if (wnd)
      break;
    else
      hwnd = GetParent(hwnd);
  }

  if (in_content && wnd) {
    /* Check content vs. non-content area: */
    POINT pos;
    GetCursorPos(&pos);
    ScreenToClient(wnd->handle, &pos);

    RECT wind;
    GetClientRect(wnd->handle, &wind);

    if (!PtInRect(&wind, pos))
      return NULL;
  }

  return wnd;
}

void wxResetCurrentCursor(void)
{
  wxWnd *wnd = wxCurrentWindow(0);
  if (!wnd) return;

  if (wxCurrentPopupMenu)
    return;

  wxWindow *w = wnd->wx_window;
  if (!w) return;

  wxCursor *cursor = wxSTANDARD_CURSOR;
  while (w) {
    if (w->wx_cursor) {
      cursor = w->wx_cursor;
      break;
    }
    w = w->GetParent();
  }

  wxMSWSetCursor(cursor->ms_cursor);
}

wxCursor *wxWindow::SetCursor(wxCursor *cursor)
{
  wxCursor *old_cursor = wx_cursor;

  if (cursor && !cursor->Ok())
    return old_cursor;

  wx_cursor = cursor;

  if (!wxIsBusy())
    wxResetCurrentCursor();

  return old_cursor;
}

// Get size *available for subwindows* i.e. excluding menu bar etc.
// For XView, this is the same as GetSize
void wxWindow::GetClientSize(int *x, int *y)
{
  HWND hWnd = GetHWND();
  RECT rect;
  GetClientRect(hWnd, &rect);
  *x = rect.right;
  *y = rect.bottom;
}

void wxWindow::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  int actualWidth = width;
  int actualHeight = height;
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  int currentW,currentH;
  GetSize(&currentW, &currentH);
  if (width == -1)
    actualWidth = currentW;
  if (height == -1)
    actualHeight = currentH;

  HWND hWnd = GetHWND();
  if (hWnd)
    MoveWindow(hWnd, x, y, actualWidth, actualHeight, (BOOL)TRUE);

  ((wxWnd *)handle)->OnSize(actualWidth, actualHeight, 0);
}

void wxWindow::SetClientSize(int width, int height)
{
  wxWindow *parent = GetParent();
  HWND hWnd = GetHWND();
  HWND hParentWnd = parent->GetHWND();

  RECT rect;
  GetClientRect(hWnd, &rect);

  RECT rect2;
  GetWindowRect(hWnd, &rect2);

  // Find the difference between the entire window (title bar and all)
  // and the client area; add this to the new client size to move the
  // window
  int actual_width = rect2.right - rect2.left - rect.right + width;
  int actual_height = rect2.bottom - rect2.top - rect.bottom + height;

  // If there's a parent, must subtract the parent's top left corner
  // since MoveWindow moves relative to the parent

  POINT point;
  point.x = rect2.left;
  point.y = rect2.top;
  if (parent)
  {
    ::ScreenToClient(hParentWnd, &point);
  }

  MoveWindow(hWnd, point.x, point.y, actual_width, actual_height, (BOOL)TRUE);
  OnSize(actual_width, actual_height);
}

Bool wxWindow::Show(Bool show)
{
  SetShown(show);

  if (window_parent)
    window_parent->GetChildren()->Show(this, show);

  HWND hWnd = GetHWND();
  int cshow;
  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
  ShowWindow(hWnd, (BOOL)cshow);
  if (show)
    BringWindowToTop(hWnd);

  {
    wxWindow *p = GetTopLevel();
    if (p->focusWindow == this)
      p->focusWindow = NULL;
  }

  return TRUE;
}

void wxWindow::GetTextExtent(const char *string, float *x, float *y,
			     float *descent, float *externalLeading, 
			     wxFont *theFont, Bool use16bit)
{
  wxFont *fontToUse = theFont;
  if (!fontToUse)
    fontToUse = font;
    
  HWND hWnd = GetHWND();
  HDC dc = wxwmGetDC(hWnd);

  HFONT fnt = 0; 
  HFONT was = 0;
  if (fontToUse && (fnt = fontToUse->GetInternalFont(dc))) 
    was = (HFONT)SelectObject(dc, fnt); 
  else {
    fnt = (HFONT)SendMessage(hWnd, WM_GETFONT, 0, 0L);
    if (fnt)
      was = (HFONT)SelectObject(dc, fnt);
  }

  SIZE sizeRect;
  TEXTMETRIC tm;
  int len = (int)strlen(string);
  GetTextExtentPoint(dc, len ? string : " ", len ? len : 1, &sizeRect);
  GetTextMetrics(dc, &tm);

  if (fontToUse && fnt && was) 
    SelectObject(dc,was); 

  wxwmReleaseDC(hWnd, dc);

  *x = (len ? (float)sizeRect.cx : (float)0.0);
  *y = (float)sizeRect.cy;
  if (descent) *descent = (float)tm.tmDescent;
  if (externalLeading) *externalLeading = (float)tm.tmExternalLeading;
}

void wxWindow::Refresh(void)
{
  HWND hWnd = GetHWND();
  if (hWnd)
  {
    ::InvalidateRect(hWnd, NULL, TRUE);
  }
}

wxWindow *wxWindow::FindFocusWindow()
{
  if (IsShown()) {
    wxChildNode *cn;
    for (cn = GetChildren()->First(); cn; cn = cn->Next()) {
      wxWindow *w = (wxWindow *)cn->Data();
      w = w->FindFocusWindow();
      if (w)
	return w;
    }
  }

  return NULL;
}

// Main window proc
LRESULT APIENTRY wxWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  wxWnd *wnd = (wxWnd *)GetWindowLong(hWnd, 0);
  if (!wnd) {
    if (wxWndHook) {
      wnd = wxWndHook;
      wnd->handle = hWnd;
    } else
      wnd = wxFindWinFromHandle(hWnd);
    
    if (!wnd)
      return DefWindowProc(hWnd, message, wParam, lParam);
  }

  // Stop right here if we don't have a valid handle
  // in our wxWnd object.
  if (!wnd->handle) {
    wnd->handle = hWnd;
    LONG res = wnd->DefWindowProc(message, wParam, lParam);
    wnd->handle = NULL;
    return res;
  }

  wnd->last_msg = message;
  wnd->last_wparam = wParam;
  wnd->last_lparam = lParam;
  
  if (message == WM_SETFONT)
    return 0;
  else if (message == WM_INITDIALOG)
    return TRUE;

  int retval = 0;

  switch (message) {
  case WM_ACTIVATE:
    {
      WORD state = LOWORD(wParam);
      WORD minimized = HIWORD(wParam);
      HWND hwnd = (HWND)lParam;
      wnd->OnActivate(state, minimized, hwnd);
      retval = 0;
      break;
    }
  case WM_SETFOCUS:
    {
      HWND hwnd = (HWND)wParam;

      if (wnd->OnSetFocus(hwnd))
	retval = 0;
      else 
	retval = wnd->DefWindowProc(message, wParam, lParam );
      break;
    }
  case WM_KILLFOCUS:
    {
      HWND hwnd = (HWND)lParam;
      if (wnd->OnKillFocus(hwnd))
	retval = 0;
      else
	retval = wnd->DefWindowProc(message, wParam, lParam );
      break;
    }
  case WM_CREATE:
    {
      wnd->OnCreate((LPCREATESTRUCT)lParam);
      retval = 0;
      break;
    }
  case WM_PAINT:
    {
      if (wnd->OnPaint())
	retval = 0;
      else retval = wnd->DefWindowProc(message, wParam, lParam );
      break;
    }
  case WM_QUERYDRAGICON:
    {
      HICON hIcon = 0;
      if (hIcon = wnd->OnQueryDragIcon())
	retval = (LONG)hIcon;
      else 
	retval = wnd->DefWindowProc(message, wParam, lParam );
      break;
    }

  case WM_SIZE:
  case WM_MOVE:
    {
      /* w & h ignored... */
      wnd->OnSize(0, 0, wParam);
      break;
    }

  case WM_RBUTTONDOWN:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_RIGHT_DOWN);
      break;
    }
  case WM_RBUTTONUP:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_RIGHT_UP);
      break;
    }
  case WM_RBUTTONDBLCLK:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_RIGHT_DOWN);
      break;
    }
  case WM_MBUTTONDOWN:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_MIDDLE_DOWN);
      break;
    }
  case WM_MBUTTONUP:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_MIDDLE_UP);
      break;
    }
  case WM_MBUTTONDBLCLK:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_MIDDLE_DOWN);
      break;
    }
  case WM_LBUTTONDOWN:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_LEFT_DOWN);
      break;
    }
  case WM_LBUTTONUP:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_LEFT_UP);
      break;
    }
  case WM_LBUTTONDBLCLK:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnButton(x, y, wParam, wxEVENT_TYPE_LEFT_DOWN);
      break;
    }
  case WM_MOUSEMOVE:
    {
      int x = (short)LOWORD(lParam);
      int y = (short)HIWORD(lParam);
      wnd->OnMouseMove(x, y, wParam);
      break;
    }
  case WM_DESTROY:
    {
      if (wnd->OnDestroy())
	retval = 0;
      else 
	retval = wnd->DefWindowProc(message, wParam, lParam );
      break;
    }
  case WM_COMMAND:
    {
      WORD id = LOWORD(wParam);
      HWND hwnd = (HWND)lParam;
      WORD cmd = HIWORD(wParam);
      if (!wnd->OnCommand(id, cmd, hwnd))
	retval = wnd->DefWindowProc(message, wParam, lParam );
      break;
    }
  case WM_INITMENU:
    {
      wnd->OnMenuClick();
      break;
    }
  case WM_MENUSELECT:
    {
      // WORD id = LOWORD(wParam);
      WORD flags = HIWORD(wParam);
      HMENU sysmenu = (HMENU)lParam;
      wnd->OnMenuSelect((WORD)wParam, flags, sysmenu);
      break;
    }
  case WM_SYSKEYDOWN:
    if ((wParam == VK_MENU) || (wParam == VK_F4)) { /* F4 is close */
      goto default_action;
    }
  case WM_KEYDOWN:
    {
      // Avoid duplicate messages to OnChar
      if ((wParam != VK_ESCAPE) 
	  && (wParam != VK_SHIFT) 
	  && (wParam != VK_CONTROL) 
	  && (wParam != VK_SPACE) 
	  && (wParam != VK_RETURN) 
	  && (wParam != VK_TAB) 
	  && (wParam != VK_BACK))
	wnd->OnChar((WORD)wParam, lParam);
      break;
    }
  case WM_KEYUP:
    {
      break;
    }
  case WM_SYSCHAR:
    if (wParam == VK_MENU) {
      goto default_action;
    }
  case WM_CHAR:
    {
      wnd->OnChar((WORD)wParam, lParam, TRUE);
      break;
    }
  case WM_HSCROLL:
    {
      WORD code = LOWORD(wParam);
      WORD pos = HIWORD(wParam);
      HWND control = (HWND)lParam;
      wnd->OnHScroll(code, pos, control);
      break;
    }
  case WM_VSCROLL:
    {
      WORD code = LOWORD(wParam);
      WORD pos = HIWORD(wParam);
      HWND control = (HWND)lParam;
      wnd->OnVScroll(code, pos, control);
      break;
    }
  case WM_CTLCOLORBTN:
    {
      int nCtlColor = CTLCOLOR_BTN;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);
      break;
    }
  case WM_CTLCOLORDLG:
    {
      int nCtlColor = CTLCOLOR_DLG;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);\
								 break;
    }
  case WM_CTLCOLORLISTBOX:
    {
      int nCtlColor = CTLCOLOR_LISTBOX;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);
      break;
    }
  case WM_CTLCOLORMSGBOX:
    {
      int nCtlColor = CTLCOLOR_MSGBOX;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);
      break;
    }
  case WM_CTLCOLORSCROLLBAR:
    {
      int nCtlColor = CTLCOLOR_SCROLLBAR;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);
      break;
    }
  case WM_CTLCOLORSTATIC:
    {
      int nCtlColor = CTLCOLOR_STATIC;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);
      break;
    }
  case WM_CTLCOLOREDIT:
    {
      int nCtlColor = CTLCOLOR_EDIT;
      HWND control = (HWND)lParam;
      HDC pDC = (HDC)wParam;
      retval = (DWORD)wnd->OnCtlColor(pDC, control, nCtlColor,
				      message, wParam, lParam);
      break;
    }
  case WM_SYSCOLORCHANGE:
    {
      retval = (DWORD)wnd->OnColorChange(hWnd, message, wParam, lParam);
      break;
    }
  case WM_ERASEBKGND:
    {
      // Prevents flicker when dragging
      if (IsIconic(hWnd))
	retval = 1;
      else if (!wnd->OnEraseBkgnd((HDC)wParam))
	retval = wnd->DefWindowProc(message, wParam, lParam );
      else
	retval = 1;
      break;
    }
  case WM_MDIACTIVATE:
    {
      HWND hWndActivate = GET_WM_MDIACTIVATE_HWNDACTIVATE(wParam,lParam);
      HWND hWndDeactivate = GET_WM_MDIACTIVATE_HWNDDEACT(wParam,lParam);
      BOOL activate = GET_WM_MDIACTIVATE_FACTIVATE(hWnd,wParam,lParam);
      retval = wnd->OnMDIActivate(activate, hWndActivate, hWndDeactivate);
      break;
    }
  case WM_DROPFILES:
    {
      wnd->OnDropFiles(wParam);
      break;
    }
  case WM_QUERYENDSESSION:
    {
      // Same as WM_CLOSE, but inverted results. Thx Microsoft :-)
      if (wnd->OnClose()) {
	if (wnd->wx_window)
	  wnd->wx_window->Show(FALSE);
      }
      retval = 0L;
      break;
    }
  case WM_CLOSE:
    {
      if (wnd->OnClose()) {
	if (wnd->wx_window) 
	  wnd->wx_window->Show(FALSE);
      }
      retval = 1L;
      break;
    }
	
  default_action:
  default:
    retval = wnd->DefWindowProc(message, wParam, lParam);
  }
  
  return retval;
}

// Dialog window proc
LONG APIENTRY wxDlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  return wxWndProc(hWnd, message, wParam, lParam);
}

wxNonlockingHashTable *wxWinHandleList = NULL;
extern wxNonlockingHashTable *wxSliderList;

wxWnd *wxFindWinFromHandle(HWND hWnd)
{
  return (wxWnd *)wxWinHandleList->Find((long)hWnd);
}

/* wxWnd class used to implement all Windows 3 windows
 */
wxWnd::wxWnd(void)
{
  x_scrolling_enabled = TRUE;
  y_scrolling_enabled = TRUE;
  calcScrolledOffset = TRUE;
  last_msg = 0;
  last_wparam = 0;
  last_lparam = 0;
  accelerator_table = NULL;
  hMenu = 0;

  xscroll_pixels_per_line = 0;
  yscroll_pixels_per_line = 0;
  xscroll_lines = 0;
  yscroll_lines = 0;
  xscroll_lines_per_page = 0;
  yscroll_lines_per_page = 0;
  xscroll_position = 0;
  yscroll_position = 0;
  canDeleteBackgroundBrush = FALSE;
  userColours = FALSE;
  background_colour = GetSysColor(COLOR_BTNFACE);
  background_transparent = FALSE;
  
  //background_brush = GetStockObject( LTGRAY_BRUSH );
  // No no no... After investigations, I found that Ctl3d use BTNFACE color
  // (which is ALWAYS grey :-))
  // So, respect the behavior!
  SetBackgroundBrush(CreateSolidBrush(GetSysColor(COLOR_BTNFACE)), TRUE);

  last_x_pos = -1.0;
  last_y_pos = -1.0;
  last_event = -1;
  is_canvas = FALSE;
  cdc = NULL;
  ldc = NULL;
  dc_count = 0;
}

wxWnd::~wxWnd(void)
{
  wxWinHandleList->DeleteObject(this);

  if (background_brush && canDeleteBackgroundBrush)
    ::DeleteObject(background_brush);

  if (wx_window) {
    wxWindow *p = wx_window->GetTopLevel();
    if (p->focusWindow == wx_window)
      p->focusWindow = NULL;
  }
}

HDC wxWnd::GetHDC(void)
{
  if (cdc)
    return(cdc);
  if (dc_count==0)
    ldc = wxwmGetDC(handle);
  dc_count++;
  return ldc;
}

void wxWnd::ReleaseHDC(void)
{
  if (cdc)
    return;
  dc_count--;
  if (dc_count==0)
    wxwmReleaseDC(handle,ldc);
  if (dc_count < 0)
    dc_count = 0;
}

// Default destroyer - override if you destroy it in some other way
// (e.g. with MDI child windows)
void wxWnd::DestroyWindow(void)
{
  DetachWindowMenu();
  SetWindowLong(handle, 0, (long)0);
  HWND oldHandle = handle;
  handle = NULL;

  wxwmDestroyWindow(oldHandle);
}

extern HICON wxSTD_FRAME_ICON;

void wxWnd::Create(wxWnd *parent, char *wclass, wxWindow *wx_win, char *title,
		   int x, int y, int width, int height,
		   DWORD style, char *dialog_template, DWORD extendedStyle)
{
  WXGC_IGNORE(this, wx_window);

  wx_window = wx_win;
  if (wx_window)
    wx_window->handle = (char *)this;
    
  is_dialog = (dialog_template != NULL);
  int x1 = 0;
  int y1 = 0;
  int w2 = 5;
  int h2 = 5;
  if (wx_window && wx_window->GetWindowStyleFlag() & wxUSER_COLOURS)
    userColours = TRUE;
  else
    userColours = FALSE;

  if (!parent) {
    x1 = y1 = CW_USEDEFAULT;
  }

  // Find parent's size, if it exists, to set up a possible default
  // panel size the size of the parent window
  RECT parent_rect;
  if (parent) {
    // Was GetWindowRect: JACS 5/5/95
    GetClientRect(parent->handle, &parent_rect);

    // Convert from screen coordinates to parent coordinates
    w2 = parent_rect.right - parent_rect.left;
    h2 = parent_rect.bottom - parent_rect.top;
  }

  if (x > -1) x1 = x;
  if (y > -1) y1 = y;
  if (width > -1) w2 = width;
  if (height > -1) h2 = height;

  HWND hParent = NULL;
  if (parent)
    hParent = parent->handle;

  wxWndHook = this;

  if (is_dialog)
  {
    handle = ::CreateDialog(wxhInstance, dialog_template, hParent,
			    (DLGPROC)wxDlgProc);
    
    if (handle == 0)
      MessageBox(NULL, "Can't find dummy dialog template!\nCheck resource include path for finding wx.rc.",
		 "wxWindows Error", MB_ICONEXCLAMATION | MB_OK);
    else
      MoveWindow(handle, x1, y1, w2, h2, FALSE);
    
    if (!parent) {
      /* Install PLT icon: */
      if (wxTheApp->wx_frame)
	SendMessage(handle, WM_SETICON, (WORD)TRUE, (DWORD)wxSTD_FRAME_ICON);
    }
  } else {
    handle = wxwmCreateWindowEx(extendedStyle, wclass,
				title,
				style,
				x1, y1,
				w2, h2,
				hParent, NULL, wxhInstance,
				NULL);
    
    if (handle == 0) {
      char buf[300];
      sprintf(buf, "Can't create window of class %s (%u)!",
	      wclass, GetLastError());
      wxFatalError(buf, "Fatal wxWindows Error");
    }
  }
  wxWndHook = NULL;
  wxWinHandleList->Append((long)handle, this);

  // Can't do this for dialogs!!!!
  if (!is_dialog)
    SetWindowLong(handle, 0, (long)this);
}

void wxWnd::OnCreate(LPCREATESTRUCT WXUNUSED(cs))
{
}

BOOL wxWnd::OnPaint(void)
{
  return 1;
}

BOOL wxWnd::OnClose(void)
{
  return FALSE;
}

BOOL wxWnd::OnDestroy(void)
{
  return TRUE;
}

void wxWnd::OnSize(int WXUNUSED(x), int WXUNUSED(y), UINT WXUNUSED(flag))
{
}

// Deal with child commands from buttons etc.

BOOL wxWnd::OnCommand(WORD WXUNUSED(id), WORD WXUNUSED(cmd), HWND WXUNUSED(control))
{
  return FALSE;
}

void wxWnd::OnMenuSelect(WORD WXUNUSED(item), WORD WXUNUSED(flags), HMENU WXUNUSED(sysmenu))
{
}

void wxWnd::OnMenuClick()
{
}

BOOL wxWnd::OnActivate(BOOL state, BOOL minimized, HWND WXUNUSED(activate))
{
  if (wx_window)
  {
    if ((state == WA_ACTIVE) || (state == WA_CLICKACTIVE)) {
      if (minimized) return TRUE; /* Ignore spurious activate while iconized */

      if (!wx_window->focusWindow) {
	/* Try to find one... */
	wx_window->focusWindow = wx_window->FindFocusWindow();
      }

      if (wx_window->focusWindow) {
	wxWindow *win = wx_window->focusWindow;
	wx_window->focusWindow = NULL;
	win->SetFocus();
      }
    }

    wx_window->OnActivate(((state == WA_ACTIVE) 
					      || (state == WA_CLICKACTIVE)));

    // If this window is an MDI parent, we must also send an OnActivate message
    // to the current child.
    if (wxSubType(wx_window->__type, wxTYPE_FRAME)) {
      wxFrame *frame = (wxFrame *)wx_window;
      if (frame->frame_type == wxMDI_PARENT) {
        wxMDIFrame *mdiFrame = (wxMDIFrame *)this;
        if (mdiFrame->current_child) {
          mdiFrame->current_child->OnActivate(state, 0, 0);
	}
      }
    }
    return 0;
  } else 
    return TRUE;
}

BOOL wxWnd::OnSetFocus(HWND WXUNUSED(hwnd))
{
  if (wx_window) {
    if (wx_window->IsShownTree()) {
      wxWindow *p = wx_window->GetTopLevel();
      p->focusWindow = wx_window;
      
      wx_window->OnSetFocus();
    }
    
    return TRUE;
  } else 
    return FALSE;
}

BOOL wxWnd::OnKillFocus(HWND WXUNUSED(hwnd))
{
  if (wx_window) {
    wx_window->OnKillFocus();
    return TRUE;
  } else 
    return FALSE;
}

void wxWnd::OnDropFiles(WPARAM wParam)
{
  HDROP hFilesInfo = (HDROP)wParam;
  POINT dropPoint;
  DragQueryPoint(hFilesInfo, (LPPOINT) &dropPoint);

  // Get the total number of files dropped
  WORD gwFilesDropped = (WORD)DragQueryFile ((HDROP)hFilesInfo,
				   (UINT)-1,
                                   (LPSTR)0,
                                   (UINT)0);

  char **files = new char *[gwFilesDropped];
  int wIndex;
  for (wIndex=0; wIndex < (int)gwFilesDropped; wIndex++)
  {
    DragQueryFile (hFilesInfo, wIndex, (LPSTR) wxBuffer, 1000);
    files[wIndex] = copystring(wxBuffer);
  }
  DragFinish (hFilesInfo);

  if (wx_window)
    for (wIndex=0; wIndex < (int)gwFilesDropped; wIndex++) 
      wx_window->OnDropFile(files[wIndex]);
}

void wxWnd::OnVScroll(WORD WXUNUSED(code), WORD WXUNUSED(pos), HWND WXUNUSED(control))
{
}

void wxWnd::OnHScroll(WORD WXUNUSED(code), WORD WXUNUSED(pos), HWND WXUNUSED(control))
{
}

void wxWnd::CalcScrolledPosition(int x, int y, int *xx, int *yy)
{
  *xx = (calcScrolledOffset ? (x - xscroll_position * xscroll_pixels_per_line) : x);
  *yy = (calcScrolledOffset ? (y - yscroll_position * yscroll_pixels_per_line) : y);
}

void wxWnd::CalcUnscrolledPosition(int x, int y, float *xx, float *yy)
{
  *xx = (float)(calcScrolledOffset ? (x + xscroll_position * xscroll_pixels_per_line) : x);
  *yy = (float)(calcScrolledOffset ? (y + yscroll_position * yscroll_pixels_per_line) : y);
}

HBRUSH wxWnd::OnCtlColor(HDC pDC, HWND pWnd, UINT nCtlColor,
                         UINT message, WPARAM wParam, LPARAM lParam)
{
  // Ignores CTL3D and FAFA settings of background colour,
  // uses current background colour for background,
  // and COLOR_BTNFACE for foreground.
  if (userColours && wx_window)
  {
    wxWindow *item = wx_window->FindItemByHWND((HWND)LOWORD(lParam));
    if ((nCtlColor == CTLCOLOR_STATIC || nCtlColor == CTLCOLOR_BTN) && background_brush)
    {
      if (background_transparent)
      {
        SetBkMode(pDC, TRANSPARENT);
      }
      else
      {
        SetBkMode(pDC, OPAQUE);
        // Radio boxes, group boxes and check boxes are different kinds of buttons:
        // the text should be the same colour as the background, not button-coloured.
        if ((nCtlColor == CTLCOLOR_BTN) && (!item ||
             ((item->__type != wxTYPE_RADIO_BOX) && (item->__type != wxTYPE_GROUP_BOX) &&
              (item->__type != wxTYPE_CHECK_BOX))))
          SetBkColor(pDC, GetSysColor(COLOR_BTNFACE));
        else
          SetBkColor(pDC, background_colour);
      }
      return background_brush;
    }
    else return NULL;
  }

  if ((nCtlColor == CTLCOLOR_STATIC || nCtlColor == CTLCOLOR_BTN) && background_brush)
  {
    // After investigations, I found that Ctl3d use BTNFACE color
    // (which is ALWAYS grey :-))
    // So, respect the behavior!
    SetBkColor(pDC, GetSysColor(COLOR_BTNFACE));
    return background_brush;
  }
  else return NULL;
}

// Set background brush, possibly deleting old one and
// noting whether we can delete the current one.
void wxWnd::SetBackgroundBrush(HBRUSH br, Bool canDelete, wxBrush *anchor)
{
  if (background_brush && canDeleteBackgroundBrush)
    ::DeleteObject(background_brush);
  canDeleteBackgroundBrush = canDelete;
  background_brush = br;
  backgroundBrushAnchor = anchor;
}

BOOL wxWnd::OnColorChange(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  if (userColours)
    return (BOOL)::DefWindowProc(hWnd, message, wParam, lParam);

  HBRUSH br = SetupBackground(hWnd);
  // We processed this message.
  return 0;
}

BOOL wxWnd::OnEraseBkgnd(HDC WXUNUSED(pDC))
{
  return FALSE;
}

LONG wxWnd::DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  return ::DefWindowProc(handle, nMsg, wParam, lParam);
}

BOOL wxWnd::ProcessMessage(MSG* WXUNUSED(pMsg))
{
  return FALSE;
}

BOOL wxWnd::OnMDIActivate(BOOL WXUNUSED(flag), HWND WXUNUSED(activate), HWND WXUNUSED(deactivate))
{
  return 1;
}

void wxWnd::DetachWindowMenu(void)
{
  if (hMenu)
  {
    int N = GetMenuItemCount(hMenu);
    int i;
    for (i = 0; i < N; i++)
    {
      char buf[100];
      int chars = GetMenuString(hMenu, i, buf, 100, MF_BYPOSITION);
      if ((chars > 0) && (strcmp(buf, "&Window") == 0))
      {
        RemoveMenu(hMenu, i, MF_BYPOSITION);
        break;
      }
    }
  }
}

/*
 * Subwindow - used for panels and canvases
 *
 */

wxSubWnd::wxSubWnd(wxWnd *parent, char *wclass, wxWindow *wx_win,
		   int x, int y, int width, int height,
		   DWORD style, char *dialog_template,
		   DWORD extendedStyle)
{
  Create(parent, wclass, wx_win, NULL, x, y, width, height, style, 
	 dialog_template, extendedStyle);
}

wxSubWnd::~wxSubWnd(void)
{
}


BOOL wxSubWnd::OnPaint(void)
{
  HRGN	tRgn=CreateRectRgn(0,0,0,0);	//Dummy call to get a handle!
  if (GetUpdateRgn(handle, tRgn, FALSE))
  {
    PAINTSTRUCT ps;
    // Hold a pointer to the dc so long as the OnPaint() message
    // is being processed
    cdc = BeginPaint(handle, &ps);
    Bool isPanel = (wx_window && wxSubType(wx_window->__type, wxTYPE_PANEL));
    if (wx_window)
    {
      wx_window->updateRect = ps.rcPaint;
      
      wx_window->OnPaint();
    }
    cdc = NULL;
    EndPaint(handle, &ps);
    DeleteObject(tRgn);

    if (isPanel)
      // Do default processing
      return FALSE;
    else
      return TRUE;
  }

  DeleteObject(tRgn);
  return FALSE;
}

void wxSubWnd::OnSize(int bad_w, int bad_h, UINT WXUNUSED(flag))
{
  if (!handle)
    return;

  if (calcScrolledOffset) {
    if ((xscroll_lines > 0) || (yscroll_lines > 0)) {
      wxCanvas * c= (wxCanvas *)wx_window;
      if (c) {
	c->SetScrollbars(c->horiz_units, c->vert_units,
			 xscroll_lines, yscroll_lines,
			 xscroll_lines_per_page, yscroll_lines_per_page,
			 xscroll_position, yscroll_position, TRUE);
      }
    }
  }

  if (wx_window)
    wx_window->OnSize(bad_w, bad_h);
}

// Deal with child commands from buttons etc.
BOOL wxSubWnd::OnCommand(WORD id, WORD cmd, HWND WXUNUSED(control))
{
  if (wxCurrentPopupMenu)
  {
    wxMenu *popupMenu = *wxCurrentPopupMenu;
    *wxCurrentPopupMenu = NULL;
    wxCurrentPopupMenu = NULL;
    BOOL succ = popupMenu->MSWCommand(cmd, id);
    return succ;
  }
  wxWindow *item = wx_window->FindItem(id);
  if (item)
  {
    BOOL value = item->MSWCommand(cmd, id);
    return value;
  }
  else
    return FALSE;
}

void wxWnd::OnButton(int x, int y, UINT flags, int evttype)
{
  wxMouseEvent *event = new wxMouseEvent(evttype);

  float px = (float)x;
  float py = (float)y;

  DeviceToLogical(&px, &py);

  CalcUnscrolledPosition((int)px, (int)py, &event->x, &event->y);

  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  event->SetTimestamp(last_msg_time);

  if (wx_window && is_canvas) {
    if ((evttype == wxEVENT_TYPE_LEFT_DOWN)
	|| (evttype == wxEVENT_TYPE_MIDDLE_DOWN)
	|| (evttype == wxEVENT_TYPE_RIGHT_DOWN))
      wx_window->CaptureMouse();
    if ((evttype == wxEVENT_TYPE_LEFT_UP)
	|| (evttype == wxEVENT_TYPE_MIDDLE_UP)
	|| (evttype == wxEVENT_TYPE_RIGHT_UP))
      wx_window->ReleaseMouse();
  }

  last_x_pos = event->x; last_y_pos = event->y; last_event = evttype;
  if (wx_window)
    if (!wx_window->CallPreOnEvent(wx_window, event))
      if (!wx_window->IsGray())
	wx_window->OnEvent(event);
}

static wxWindow *el_PARENT(wxWindow *w)
{
  /* Don't follow frame-parent hierarchy: */
  if (wxSubType(w->__type, wxTYPE_FRAME)
      || wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    return NULL;

  return w->GetParent();
}

int wxCheckMousePosition()
{
  if (current_mouse_wnd && !wxCurrentWindow(1)) {
    wxWindow *imw;

    for (imw = current_mouse_wnd; imw; imw = el_PARENT(imw))
      wxQueueLeaveEvent(current_mouse_context, imw, -10, -10, 0);

    current_mouse_wnd = NULL;
    current_mouse_context = NULL;

    return 1;
  }

  return 0;
}

void wxDoLeaveEvent(wxWindow *w, int x, int y, int flags)
{
  wxDoOnMouseLeave(w, x, y, flags);
}

extern void wxEntered(wxWindow *mw, int x, int y, int flags)
{
  wxWindow *imw, *join, *nextw;
  void *curr_context = wxGetContextForFrame();
  POINT glob, pos;

  glob.x = x;
  glob.y = y;
  ::ClientToScreen(mw->GetHWND(), &glob);
  
  wxWindow *mouse_wnd = current_mouse_wnd;
  void *mouse_context = current_mouse_context;

  current_mouse_wnd = NULL;
  current_mouse_context = NULL;

  join = mouse_wnd;
  while (join) {
    for (imw = mw; imw; imw = el_PARENT(imw)) {
      if (join == imw)
	break;
    }
    if (join == imw)
      break;
    join = el_PARENT(join);
  }
  
  /* Leave old window(s) */
  for (imw = mouse_wnd; imw != join; imw = el_PARENT(imw)) {
    pos = glob;
    ::ScreenToClient(imw->GetHWND(), &pos);
    if (mouse_context == curr_context)
      wxDoOnMouseLeave(imw, pos.x, pos.y, flags);
    else
      wxQueueLeaveEvent(mouse_context, imw, pos.x, pos.y, flags);
  }
  
  /* Enter new window(s) - outside to inside */
  while (join != mw) {
    imw = mw;
    for (nextw = el_PARENT(imw); nextw != join; nextw = el_PARENT(nextw)) {
      imw = nextw;
    }
    pos = glob;
    ::ScreenToClient(imw->GetHWND(), &pos);
    wxDoOnMouseEnter(imw, pos.x, pos.y, flags);
    join = imw;
  }

  if (!current_mouse_wnd) {
    current_mouse_wnd = mw;
    current_mouse_context = curr_context;
  }
}

void wxWnd::OnMouseMove(int x, int y, UINT flags)
{
  if (wxIsBusy())
    wxMSWSetCursor(wxHOURGLASS_CURSOR->ms_cursor);
  else
    wxResetCurrentCursor();

  /* Check mouse-position based stuff */
  if (wx_window)
    wxEntered(wx_window, x, y, flags);
  else {
    /* Could be status line... */
    /* We'd like to re-dispatch to the frame... */
  }

  wxMouseEvent *event = new wxMouseEvent(wxEVENT_TYPE_MOTION);
  float px = (float)x;
  float py = (float)y;

  DeviceToLogical(&px, &py);

  CalcUnscrolledPosition((int)px, (int)py, &event->x, &event->y);

  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  event->SetTimestamp(last_msg_time);

  // Window gets a click down message followed by a mouse move
  // message even if position isn't changed!  We want to discard
  // the trailing move event if x and y are the same.
  if ((last_event == wxEVENT_TYPE_RIGHT_DOWN || last_event == wxEVENT_TYPE_LEFT_DOWN ||
       last_event == wxEVENT_TYPE_MIDDLE_DOWN) &&
      (last_x_pos == event->x && last_y_pos == event->y)) {
    last_x_pos = event->x; last_y_pos = event->y;
    last_event = wxEVENT_TYPE_MOTION;
    return;
  }

  last_event = wxEVENT_TYPE_MOTION;
  last_x_pos = event->x; last_y_pos = event->y;
  if (wx_window) 
    if (!wx_window->CallPreOnEvent(wx_window, event))
      if (!wx_window->IsGray())
	wx_window->OnEvent(event);
}

void wxWnd::OnMouseEnter(int x, int y, UINT flags)
{
  if (wx_window)
    wxDoOnMouseEnter(wx_window, x, y, flags);
}

static void wxDoOnMouseEnter(wxWindow *wx_window, int x, int y, UINT flags)
{
  wxMouseEvent *event = new wxMouseEvent(wxEVENT_TYPE_ENTER_WINDOW);
  float px = (float)x;
  float py = (float)y;

  if (wx_window->handle) {
    wxWnd *wnd = (wxWnd *)wx_window->handle;
    wnd->DeviceToLogical(&px, &py);
    wnd->CalcUnscrolledPosition((int)px, (int)py, &event->x, &event->y);
  }

  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  event->SetTimestamp(last_msg_time); /* MATTHEW: timeStamp */

  if (!wx_window->CallPreOnEvent(wx_window, event))
    if (!wx_window->IsGray())
      wx_window->OnEvent(event);
}

void wxWnd::OnMouseLeave(int x, int y, UINT flags)
{
  if (wx_window)
    wxDoOnMouseLeave(wx_window, x, y, flags);
}

static void wxDoOnMouseLeave(wxWindow *wx_window, int x, int y, UINT flags)
{
  wxMouseEvent *event = new wxMouseEvent(wxEVENT_TYPE_LEAVE_WINDOW);
  float px = (float)x;
  float py = (float)y;

  if (wx_window->handle) {
    wxWnd *wnd = (wxWnd *)wx_window->handle;
    wnd->DeviceToLogical(&px, &py);
    wnd->CalcUnscrolledPosition((int)px, (int)py, &event->x, &event->y);
  }
  
  event->shiftDown = (flags & MK_SHIFT);
  event->controlDown = (flags & MK_CONTROL);
  event->leftDown = (flags & MK_LBUTTON);
  event->middleDown = (flags & MK_MBUTTON);
  event->rightDown = (flags & MK_RBUTTON);
  event->SetTimestamp(last_msg_time); /* MATTHEW: timeStamp */

  if (!wx_window->CallPreOnEvent(wx_window, event))
    if (!wx_window->IsGray())
      wx_window->OnEvent(event);
}

static int numpad_scan_codes[10];
static int plus_scan_code;
static int minus_scan_code;
static int times_scan_code;
static int divide_scan_code;
static int dot_scan_code;

#define THE_SCAN_CODE(lParam) ((((unsigned long)lParam) >> 16) & 0x1FF)

void wxWnd::OnChar(WORD wParam, LPARAM lParam, Bool isASCII)
{
  int id;
  Bool tempControlDown = (::GetKeyState(VK_CONTROL) >> 1);

  if (isASCII) {
    // If 1 -> 26, translate to CTRL plus a letter.
    id = wParam;
    if ((id > 0) && (id < 27) && tempControlDown) {
      id = id + 96;
    }
    
    /* Ignore character created by numpad, since it's
       already handled as WM_KEYDOWN */
    int sc = THE_SCAN_CODE(lParam);
    if (sc) {
      if ((id >= '0') && (id <= '9')) {
	if (numpad_scan_codes[id - '0'] == sc)
	  id = -1;
      } else if (id == '+') {
	if (sc == plus_scan_code)
	  id = -1;
      } else if (id == '-') {
	if (sc == minus_scan_code)
	  id = -1;
      } else if (id == '*') {
	if (sc == times_scan_code)
	  id = -1;
      } else if (id == '/') {
	if (sc == divide_scan_code)
	  id = -1;
      } else if (id == '.') {
	if (sc == dot_scan_code)
	  id = -1;
      }
    }
  } else {
    if ((id = wxCharCodeMSWToWX(wParam)) == 0)
      id = -1;
    if ((id >= WXK_NUMPAD0) && (id <= WXK_NUMPAD9)) {
      /* remember scan code so we can ignore the WM_CHAR part */
      numpad_scan_codes[id - WXK_NUMPAD0] = THE_SCAN_CODE(lParam);
    } else if (id == WXK_ADD) {
      plus_scan_code = THE_SCAN_CODE(lParam);
    } else if (id == WXK_SUBTRACT) {
      minus_scan_code = THE_SCAN_CODE(lParam);
    } else if (id == WXK_MULTIPLY) {
      times_scan_code = THE_SCAN_CODE(lParam);
    } else if (id == WXK_DIVIDE) {
      divide_scan_code = THE_SCAN_CODE(lParam);
    } else if (id == WXK_DECIMAL) {
      dot_scan_code = THE_SCAN_CODE(lParam);
    }
  } 

  if ((id > -1) && wx_window) {
    wxKeyEvent *event = new wxKeyEvent(wxEVENT_TYPE_CHAR);

    if (::GetKeyState(VK_SHIFT) >> 1)
      event->shiftDown = TRUE;
    if (tempControlDown)
      event->controlDown = TRUE;
    if ((HIWORD(lParam) & KF_ALTDOWN) == KF_ALTDOWN)
      event->metaDown = TRUE;

    event->keyCode = id;
    event->SetTimestamp(last_msg_time); /* MATTHEW: timeStamp */

    POINT pt;
    GetCursorPos(&pt);
    RECT rect;
    GetWindowRect(handle,&rect);
    pt.x -= rect.left;
    pt.y -= rect.top;
    float fx,fy;
    fx = (float)pt.x;
    fy = (float)pt.y;
    DeviceToLogical(&fx,&fy);
    CalcUnscrolledPosition((int)fx,(int)fy,&event->x,&event->y);

    if (!wx_window->CallPreOnChar(wx_window, event))
      if (!wx_window->IsGray())
	wx_window->OnChar(event);
  }
}

void wxSubWnd::OnVScroll(WORD wParam, WORD pos, HWND control)
{
  if (control) {
    wxSlider *slider = (wxSlider *)wxSliderList->Find((long)control);
    if (slider)
      wxSliderEvent(control, wParam, pos);
    return;
  }

  wxScrollEvent *event = new wxScrollEvent;
  
  event->pos = pos;
  event->direction = wxVERTICAL;
  switch (wParam) {
  case SB_TOP:
    event->moveType = wxEVENT_TYPE_SCROLL_TOP;
    break;
    
  case SB_BOTTOM:
    event->moveType = wxEVENT_TYPE_SCROLL_BOTTOM;
    break;
    
  case SB_LINEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEUP;
    break;
    
  case SB_LINEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEDOWN;
    break;
    
  case SB_PAGEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEUP;
    break;
    
  case SB_PAGEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEDOWN;
    break;
    
  case SB_THUMBTRACK:
    event->moveType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
    break;
    
  default:
    return;
    break;
  }
  if (wx_window)
    wx_window->DoScroll(event);
}

void wxSubWnd::OnHScroll( WORD wParam, WORD pos, HWND control)
{
  if (control) {
    wxSlider *slider = (wxSlider *)wxSliderList->Find((long)control);
    if (slider)
      wxSliderEvent(control, wParam, pos);
    return;
  }

  wxScrollEvent *event = new wxScrollEvent;
  
  event->pos = pos;
  event->direction = wxHORIZONTAL;
  switch (wParam) {
  case SB_TOP:
    event->moveType = wxEVENT_TYPE_SCROLL_TOP;
    break;
    
  case SB_BOTTOM:
    event->moveType = wxEVENT_TYPE_SCROLL_BOTTOM;
    break;
    
  case SB_LINEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEUP;
    break;
    
  case SB_LINEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_LINEDOWN;
    break;
    
  case SB_PAGEUP:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEUP;
    break;
    
  case SB_PAGEDOWN:
    event->moveType = wxEVENT_TYPE_SCROLL_PAGEDOWN;
    break;
    
  case SB_THUMBTRACK:
    event->moveType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
    break;
    
  default:
    return;
    break;
  }
  if (wx_window)
    wx_window->DoScroll(event);
}

void wxGetCharSize(HWND wnd, int *x, int *y,wxFont *the_font)
{
  TEXTMETRIC tm;
  HDC dc = wxwmGetDC(wnd);
  HFONT fnt =0;
  HFONT was = 0;
  if (the_font&&(fnt=the_font->GetInternalFont(dc)))
  {
    was = (HFONT)SelectObject(dc,fnt);
  }
  GetTextMetrics(dc, &tm);
  if (the_font && fnt && was)
  {
    SelectObject(dc,was);
  }
  wxwmReleaseDC(wnd, dc);
  *x = tm.tmAveCharWidth;
  *y = tm.tmHeight + tm.tmExternalLeading;
}

// Returns 0 if was a normal ASCII value, not a special key. This indicates that
// the key should be ignored by WM_KEYDOWN and processed by WM_CHAR instead.
int wxCharCodeMSWToWX(int keySym)
{
  int id = 0;
  switch (keySym)
  {
    case VK_CANCEL:             id = WXK_CANCEL; break;
    case VK_BACK:               id = WXK_BACK; break;
    case VK_TAB:	        id = WXK_TAB; break;
    case VK_CLEAR:		id = WXK_CLEAR; break;
    case VK_RETURN:		id = WXK_RETURN; break;
    case VK_SHIFT:		id = WXK_SHIFT; break;
    case VK_CONTROL:		id = WXK_CONTROL; break;
    case VK_MENU :		id = WXK_MENU; break;
    case VK_PAUSE:		id = WXK_PAUSE; break;
    case VK_SPACE:		id = WXK_SPACE; break;
    case VK_ESCAPE:		id = WXK_ESCAPE; break;
    case VK_PRIOR:		id = WXK_PRIOR; break;
    case VK_NEXT :		id = WXK_NEXT; break;
    case VK_END:		id = WXK_END; break;
    case VK_HOME :		id = WXK_HOME; break;
    case VK_LEFT :		id = WXK_LEFT; break;
    case VK_UP:		        id = WXK_UP; break;
    case VK_RIGHT:		id = WXK_RIGHT; break;
    case VK_DOWN :		id = WXK_DOWN; break;
    case VK_SELECT:		id = WXK_SELECT; break;
    case VK_PRINT:		id = WXK_PRINT; break;
    case VK_EXECUTE:		id = WXK_EXECUTE; break;
    case VK_INSERT:		id = WXK_INSERT; break;
    case VK_DELETE:		id = WXK_DELETE; break;
    case VK_HELP :		id = WXK_HELP; break;
    case VK_NUMPAD0:		id = WXK_NUMPAD0; break;
    case VK_NUMPAD1:		id = WXK_NUMPAD1; break;
    case VK_NUMPAD2:		id = WXK_NUMPAD2; break;
    case VK_NUMPAD3:		id = WXK_NUMPAD3; break;
    case VK_NUMPAD4:		id = WXK_NUMPAD4; break;
    case VK_NUMPAD5:		id = WXK_NUMPAD5; break;
    case VK_NUMPAD6:		id = WXK_NUMPAD6; break;
    case VK_NUMPAD7:		id = WXK_NUMPAD7; break;
    case VK_NUMPAD8:		id = WXK_NUMPAD8; break;
    case VK_NUMPAD9:		id = WXK_NUMPAD9; break;
    case VK_MULTIPLY:		id = WXK_MULTIPLY; break;
    case VK_ADD:		id = WXK_ADD; break;
    case VK_SUBTRACT:		id = WXK_SUBTRACT; break;
    case VK_DECIMAL:		id = WXK_DECIMAL; break;
    case VK_DIVIDE:		id = WXK_DIVIDE; break;
    case VK_F1:		id = WXK_F1; break;
    case VK_F2:		id = WXK_F2; break;
    case VK_F3:		id = WXK_F3; break;
    case VK_F4:		id = WXK_F4; break;
    case VK_F5:		id = WXK_F5; break;
    case VK_F6:		id = WXK_F6; break;
    case VK_F7:		id = WXK_F7; break;
    case VK_F8:		id = WXK_F8; break;
    case VK_F9:		id = WXK_F9; break;
    case VK_F10:		id = WXK_F10; break;
    case VK_F11:		id = WXK_F11; break;
    case VK_F12:		id = WXK_F12; break;
    case VK_F13:		id = WXK_F13; break;
    case VK_F14:		id = WXK_F14; break;
    case VK_F15:		id = WXK_F15; break;
    case VK_F16:		id = WXK_F16; break;
    case VK_F17:		id = WXK_F17; break;
    case VK_F18:		id = WXK_F18; break;
    case VK_F19:		id = WXK_F19; break;
    case VK_F20:		id = WXK_F20; break;
    case VK_F21:		id = WXK_F21; break;
    case VK_F22:		id = WXK_F22; break;
    case VK_F23:		id = WXK_F23; break;
    case VK_F24:		id = WXK_F24; break;
    case VK_NUMLOCK:		id = WXK_NUMLOCK; break;
    case VK_SCROLL:		id = WXK_SCROLL; break;
    default:
    {
      return 0;
    }
  }
  return id;
}

int wxCharCodeWXToMSW(int id, Bool *isVirtual)
{
  *isVirtual = TRUE;
  int keySym = 0;
  switch (id)
  {
    case WXK_CANCEL:            keySym = VK_CANCEL; break;
    case WXK_CLEAR:		keySym = VK_CLEAR; break;
    case WXK_SHIFT:		keySym = VK_SHIFT; break;
    case WXK_CONTROL:		keySym = VK_CONTROL; break;
    case WXK_MENU :		keySym = VK_MENU; break;
    case WXK_PAUSE:		keySym = VK_PAUSE; break;
    case WXK_PRIOR:		keySym = VK_PRIOR; break;
    case WXK_NEXT :		keySym = VK_NEXT; break;
    case WXK_END:		keySym = VK_END; break;
    case WXK_HOME :		keySym = VK_HOME; break;
    case WXK_LEFT :		keySym = VK_LEFT; break;
    case WXK_UP:		keySym = VK_UP; break;
    case WXK_RIGHT:		keySym = VK_RIGHT; break;
    case WXK_DOWN :		keySym = VK_DOWN; break;
    case WXK_SELECT:		keySym = VK_SELECT; break;
    case WXK_PRINT:		keySym = VK_PRINT; break;
    case WXK_EXECUTE:		keySym = VK_EXECUTE; break;
    case WXK_INSERT:		keySym = VK_INSERT; break;
    case WXK_DELETE:		keySym = VK_DELETE; break;
    case WXK_HELP :		keySym = VK_HELP; break;
    case WXK_NUMPAD0:		keySym = VK_NUMPAD0; break;
    case WXK_NUMPAD1:		keySym = VK_NUMPAD1; break;
    case WXK_NUMPAD2:		keySym = VK_NUMPAD2; break;
    case WXK_NUMPAD3:		keySym = VK_NUMPAD3; break;
    case WXK_NUMPAD4:		keySym = VK_NUMPAD4; break;
    case WXK_NUMPAD5:		keySym = VK_NUMPAD5; break;
    case WXK_NUMPAD6:		keySym = VK_NUMPAD6; break;
    case WXK_NUMPAD7:		keySym = VK_NUMPAD7; break;
    case WXK_NUMPAD8:		keySym = VK_NUMPAD8; break;
    case WXK_NUMPAD9:		keySym = VK_NUMPAD9; break;
    case WXK_MULTIPLY:		keySym = VK_MULTIPLY; break;
    case WXK_ADD:		keySym = VK_ADD; break;
    case WXK_SUBTRACT:		keySym = VK_SUBTRACT; break;
    case WXK_DECIMAL:		keySym = VK_DECIMAL; break;
    case WXK_DIVIDE:		keySym = VK_DIVIDE; break;
    case WXK_F1:		keySym = VK_F1; break;
    case WXK_F2:		keySym = VK_F2; break;
    case WXK_F3:		keySym = VK_F3; break;
    case WXK_F4:		keySym = VK_F4; break;
    case WXK_F5:		keySym = VK_F5; break;
    case WXK_F6:		keySym = VK_F6; break;
    case WXK_F7:		keySym = VK_F7; break;
    case WXK_F8:		keySym = VK_F8; break;
    case WXK_F9:		keySym = VK_F9; break;
    case WXK_F10:		keySym = VK_F10; break;
    case WXK_F11:		keySym = VK_F11; break;
    case WXK_F12:		keySym = VK_F12; break;
    case WXK_F13:		keySym = VK_F13; break;
    case WXK_F14:		keySym = VK_F14; break;
    case WXK_F15:		keySym = VK_F15; break;
    case WXK_F16:		keySym = VK_F16; break;
    case WXK_F17:		keySym = VK_F17; break;
    case WXK_F18:		keySym = VK_F18; break;
    case WXK_F19:		keySym = VK_F19; break;
    case WXK_F20:		keySym = VK_F20; break;
    case WXK_F21:		keySym = VK_F21; break;
    case WXK_F22:		keySym = VK_F22; break;
    case WXK_F23:		keySym = VK_F23; break;
    case WXK_F24:		keySym = VK_F24; break;
    case WXK_NUMLOCK:		keySym = VK_NUMLOCK; break;
    case WXK_SCROLL:		keySym = VK_SCROLL; break;
    default:
    {
      *isVirtual = FALSE;
      keySym = id;
      break;
    }
  }
  return keySym;
}

void wxWindow::DoScroll(wxScrollEvent *event)
{
  long orient = event->direction;

  int nScrollInc = CalcScrollInc(event);
  if (nScrollInc == 0)
    return;

  wxWnd *wnd = (wxWnd *)handle;
  HWND hWnd = GetHWND();

  if (orient == wxHORIZONTAL) {
    int newPos = wnd->xscroll_position + nScrollInc;
    ::SetScrollPos(hWnd, SB_HORZ, newPos, TRUE);
    newPos = ::GetScrollPos(hWnd, SB_HORZ);
    nScrollInc = newPos - wnd->xscroll_position;
    wnd->xscroll_position = newPos;
  } else {
    int newPos = wnd->yscroll_position + nScrollInc;
    ::SetScrollPos(hWnd, SB_VERT, newPos, TRUE );
    newPos = ::GetScrollPos(hWnd, SB_VERT);
    nScrollInc = newPos - wnd->yscroll_position;
    wnd->yscroll_position = newPos;
  }

  if (!wnd->calcScrolledOffset) {
    OnScroll(event);
  } else {
    if (orient == wxHORIZONTAL)
      ::ScrollWindow(hWnd, nScrollInc, 0, NULL, NULL);
    else
      ::ScrollWindow(hWnd, 0, nScrollInc, NULL, NULL);      
  
    InvalidateRect(hWnd, NULL, FALSE);
  }
}


int wxWindow::CalcScrollInc(wxScrollEvent *event)
{
  int pos = event->pos;
  long orient = event->direction;

  int nScrollInc = 0;
  wxWnd *wnd = (wxWnd *)handle;

  switch (event->moveType)
  {
    case wxEVENT_TYPE_SCROLL_TOP:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = - wnd->xscroll_position;
      else
        nScrollInc = - wnd->yscroll_position;
      break;
    }
    case wxEVENT_TYPE_SCROLL_BOTTOM:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = wnd->xscroll_lines - wnd->xscroll_position;
      else
        nScrollInc = wnd->yscroll_lines - wnd->yscroll_position;
      break;
    }
    case wxEVENT_TYPE_SCROLL_LINEUP:
    {
      nScrollInc = -1;
      break;
    }
    case wxEVENT_TYPE_SCROLL_LINEDOWN:
    {
      nScrollInc = 1;
      break;
    }
    case wxEVENT_TYPE_SCROLL_PAGEUP:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = -wnd->xscroll_lines_per_page;
      else
        nScrollInc = -wnd->yscroll_lines_per_page;
      break;
    }
    case wxEVENT_TYPE_SCROLL_PAGEDOWN:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = wnd->xscroll_lines_per_page;
      else
        nScrollInc = wnd->yscroll_lines_per_page;
      break;
    }
    case wxEVENT_TYPE_SCROLL_THUMBTRACK:
    {
      if (orient == wxHORIZONTAL)
        nScrollInc = pos - wnd->xscroll_position;
      else
        nScrollInc = pos - wnd->yscroll_position;
      break;
    }
    default:
    {
      break;
    }
  }
  HWND hWnd = GetHWND();
  if (orient == wxHORIZONTAL) {
    if (wnd->calcScrolledOffset) {
      // We're scrolling automatically
      int w;
      RECT rect;
      GetClientRect(hWnd, &rect);
      w = rect.right - rect.left;
      int nMaxWidth = wnd->xscroll_lines*wnd->xscroll_pixels_per_line;
      
      int nHscrollMax = (int)ceil((nMaxWidth - w)/(float)wnd->xscroll_pixels_per_line);
      nHscrollMax = max(0, nHscrollMax);

      nScrollInc = max(-wnd->xscroll_position,
		       min(nScrollInc, nHscrollMax - wnd->xscroll_position));
      return nScrollInc;
    } else {
      // We're not scrolling automatically so we don't care about pixel-per-line
      int newPosition = wnd->xscroll_position + nScrollInc;
      if (newPosition < 0)
	return -wnd->xscroll_position;
      else if (newPosition > wnd->xscroll_lines)
	return wnd->xscroll_lines - wnd->xscroll_position;
      else
        return nScrollInc;
    }
  } else {
    if (wnd->calcScrolledOffset) {
      // We're scrolling automatically
      RECT rect;
      GetClientRect(hWnd, &rect);
      int h = rect.bottom - rect.top;
      
      int nMaxHeight = wnd->yscroll_lines*wnd->yscroll_pixels_per_line;
      
      int nVscrollMax = (int)ceil((nMaxHeight - h)/(float)wnd->yscroll_pixels_per_line);
      nVscrollMax = max(0, nVscrollMax);
      
      nScrollInc = max(-wnd->yscroll_position,
		       min(nScrollInc, nVscrollMax - wnd->yscroll_position));
      return nScrollInc;
    } else {
      // We're not scrolling automatically so we don't care about pixel-per-line
      int newPosition = wnd->yscroll_position + nScrollInc;
      if (newPosition < 0)
	return -wnd->yscroll_position;
      else if (newPosition > wnd->yscroll_lines)
	return wnd->yscroll_lines - wnd->yscroll_position;
      else
	return nScrollInc;
    }
  }
}

void wxWindow::OnScroll(wxScrollEvent *event)
{
}

void wxWindow::SetScrollPos(int orient, int pos)
{
  wxWnd *wnd = (wxWnd *)handle;

  if (orient < 0) {
    /* Hack to avoid calcScrolledOffset check */
    orient = -orient;
  } else {
    if (wnd->calcScrolledOffset)
      return;
  }

  int wOrient;
  if (orient == wxHORIZONTAL)
    wOrient = SB_HORZ;
  else
    wOrient = SB_VERT;
    
  HWND hWnd = GetHWND();
  if (hWnd) {
    ::SetScrollPos(hWnd, wOrient, pos, TRUE);

    if (orient == wxHORIZONTAL)
      wnd->xscroll_position = ::GetScrollPos(hWnd, SB_HORZ);
    else
      wnd->yscroll_position = ::GetScrollPos(hWnd, SB_VERT);
  }
}

void wxWindow::SetScrollRange(int orient, int range)
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return;

  int wOrient, page;

  if (orient == wxHORIZONTAL)
    wOrient = SB_HORZ;
  else
    wOrient = SB_VERT;
    
  if (orient == wxHORIZONTAL) {
    page = wnd->xscroll_lines_per_page;
  } else {
    page = wnd->yscroll_lines_per_page;
  }

  SCROLLINFO info;
  info.cbSize = sizeof(SCROLLINFO);
  info.nPage = page;
  info.nMin = 0;
  info.nMax = range + page - 1;

  info.fMask = SIF_PAGE | SIF_RANGE | SIF_DISABLENOSCROLL;

  HWND hWnd = GetHWND();

  if (hWnd) {
    ::SetScrollInfo(hWnd, wOrient, &info, TRUE);
  }

  if (orient == wxHORIZONTAL)
    wnd->xscroll_lines = range;
  else
    wnd->yscroll_lines = range;
}

void wxWindow::SetScrollPage(int orient, int page)
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return;

  SCROLLINFO info;

  int dir, range;

  if (orient == wxHORIZONTAL) {
    dir = SB_HORZ;    
    range = wnd->xscroll_lines;
    if (page > range + 1)
      page = range + 1;
    wnd->xscroll_lines_per_page = page;
  } else {
    dir = SB_VERT;
    range = wnd->yscroll_lines;

    if (page > range + 1)
      page = range + 1;
    
    wnd->yscroll_lines_per_page = page;
  }

  info.cbSize = sizeof(SCROLLINFO);
  info.nPage = page;
  info.nMin = 0;
  info.nMax = range + page - 1;
  info.fMask = SIF_PAGE | SIF_RANGE | SIF_DISABLENOSCROLL;

  HWND hWnd = GetHWND();
  if (hWnd) {
    ::SetScrollInfo(hWnd, dir, &info, TRUE);
  }
}

int wxWindow::GetScrollPos(int orient)
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return 0;

  int wOrient;
  if (orient == wxHORIZONTAL)
    wOrient = SB_HORZ;
  else
    wOrient = SB_VERT;
  HWND hWnd = GetHWND();
  if (hWnd)
    return ::GetScrollPos(hWnd, wOrient);
  else
    return 0;
}

int wxWindow::GetScrollRange(int orient)
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return 0;

  if (orient == wxHORIZONTAL)
    return max(0, wnd->xscroll_lines);
  else
    return max(0, wnd->yscroll_lines);
}

int wxWindow::GetScrollPage(int orient)
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd->calcScrolledOffset) return 0;

  wxCanvas *c = (wxCanvas *)this;

  if (orient == wxHORIZONTAL) {
    if (c->horiz_units <= 0)
      return 0;
    return wnd->xscroll_lines_per_page;
  } else {
    if (c->vert_units <= 0)
      return 0;
    return wnd->yscroll_lines_per_page;
  }
}

// Default OnSize resets scrollbars, if any
void wxWindow::OnSize(int bad_w, int bad_h)
{
  if (wxWinType != wxTYPE_XWND)
    return;
  wxWnd *wnd = (wxWnd *)handle;
    
  if (wxSubType(__type, wxTYPE_DIALOG_BOX)) {
    wxChildNode* node = GetChildren()->First(); 

    if (node && !node->Next()) {
      wxWindow *win = (wxWindow *)node->Data();
      Bool hasSubPanel = ((wxSubType(win->__type, wxTYPE_PANEL)
			   && !wxSubType(win->__type, wxTYPE_DIALOG_BOX))
			  || wxSubType(win->__type, wxTYPE_CANVAS)
			  || wxSubType(win->__type, wxTYPE_TEXT_WINDOW));
      
      if (hasSubPanel) {
	int w, h;
	GetClientSize(&w, &h);
	win->SetSize(0, 0, w, h);
      }
    }
  }
}


Bool wxWindow::CallPreOnEvent(wxWindow *win, wxMouseEvent *evt)
{
  wxWindow *p;
  if (wxSubType(win->__type, wxTYPE_FRAME)
      || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
    p = NULL;
  else
    p = win->GetParent();

  return ((p && CallPreOnEvent(p, evt)) || win->PreOnEvent(this, evt));
}

Bool wxWindow::CallPreOnChar(wxWindow *win, wxKeyEvent *evt)
{
  wxWindow *p;
  if (wxSubType(win->__type, wxTYPE_FRAME)
      || wxSubType(win->__type, wxTYPE_DIALOG_BOX))
    p = NULL;
  else
    p = win->GetParent();

  return ((p && CallPreOnChar(p, evt)) || win->PreOnChar(this, evt));
}

Bool wxWindow::PreOnEvent(wxWindow *, wxMouseEvent *)
{
  return FALSE;
}

Bool wxWindow::PreOnChar(wxWindow *, wxKeyEvent *)
{
  return FALSE;
}

wxWindow *wxGetActiveWindow(void)
{
  HWND hWnd = GetActiveWindow();
  if (hWnd != 0)
  {
    wxWnd *wnd = wxFindWinFromHandle(hWnd);
    if (wnd && wnd->wx_window)
    {
      return wnd->wx_window;
    }
  }
  return NULL;
}
