/*
 * File:	wx_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#if USE_COMMON_DIALOGS
# include <commdlg.h>
#endif

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

class wxDialogWnd : public wxSubWnd
{
public:
  wxDialogWnd(wxWnd *parent, wxWindow *wx_win,
              int x, int y, int width, int height,
              char *dialog_template);

  // Handlers
  LONG DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam);
  BOOL ProcessMessage(MSG* pMsg);
  BOOL OnEraseBkgnd(HDC pDC);
  BOOL OnClose(void);
};

wxDialogWnd::wxDialogWnd(wxWnd *parent, wxWindow *wx_win,
			 int x, int y, int width, int height,
			 char *dialog_template):
  wxSubWnd(parent, NULL, wx_win, x, y, width, height, 0, dialog_template)
{
}
 
LONG wxDialogWnd::DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
  return FALSE; /* Not processed */
}

BOOL wxDialogWnd::ProcessMessage(MSG* pMsg)
{
  return FALSE;
}

BOOL wxDialogWnd::OnClose(void)
{
  if (wx_window) {
    wxWindow *modal = wxGetModalWindow(wx_window);
    if (modal && (modal != wx_window))
      return FALSE;
    
    if (wx_window->OnClose())
      return TRUE;
    else 
      return FALSE;
  }
  return FALSE;
}

BOOL wxDialogWnd::OnEraseBkgnd(HDC pDC)
{
  return FALSE;
}

wxDialogBox::wxDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
               int x, int y, int width, int height, long style, char *name):
  wxbDialogBox((wxWindow *)Parent, Title, Modal, x, y, width, height, style, name)
{
  Create(Parent, Title, Modal, x, y, width, height, style, name);
}
  
Bool wxDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name)
{
  // Do anything that needs to be done in the generic base class
  wxbDialogBox::Create(Parent, Title, Modal, x, y, width, height, style, name);

  has_child = FALSE;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;

  current_hspacing = hSpacing;
  current_vspacing = vSpacing;
  
  labelFont = wxTheFontList->FindOrCreateFont(8, wxSYSTEM, wxNORMAL, wxNORMAL, FALSE);
  buttonFont = wxTheFontList->FindOrCreateFont(8, wxSYSTEM, wxNORMAL, wxNORMAL, FALSE);

  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

  wxWinType = wxTYPE_XWND;
  windowStyle = style;
  wxWnd *cparent = NULL;
  if (Parent)
    cparent = (wxWnd *)Parent->handle;

  modal_showing = FALSE;

  if (width < 0)
    width = 0;
  if (height < 0)
    height = 0;

  // Allows creation of dialogs with & without captions under MSWindows
  wxDialogWnd *wnd;
  wnd = new wxDialogWnd(cparent, this, x, y, width, height,
			!(style & wxNO_CAPTION)
			? ((style & wxMAXIMIZE) ? "wxCaptionResizeDialog" : "wxCaptionDialog")
			: ((style & wxMAXIMIZE) ? "wxNoCaptionResizeDialog" : "wxNoCaptionDialog"));

  handle = (char *)wnd;
  SetWindowText(wnd->handle, Title);

  wx_cursor = wxSTANDARD_CURSOR;  

  modal = Modal;
  return TRUE;
}

wxDialogBox::~wxDialogBox()
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd && modal_showing)
    Show(FALSE);

  modal_showing = FALSE;
  if (wnd) {
    ShowWindow(wnd->handle, SW_HIDE);
  }
}

void wxDialogBox::Fit(void)
{
  wxPanel::Fit();
}

void wxDialogBox::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  InternalGrayChildren(gray);
}


void wxDialogBox::Iconize(Bool WXUNUSED(iconize))
{
  // Windows dialog boxes can't be iconized
}

Bool wxDialogBox::Iconized(void)
{
  return FALSE;
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  wxWindow::SetSize(x, y, width, height);
}

void wxDialogBox::SetClientSize(int width, int height)
{
  wxWnd *wnd = (wxWnd *)handle;
  RECT rect;
  GetClientRect(wnd->handle, &rect);

  RECT rect2;
  GetWindowRect(wnd->handle, &rect2);

  // Find the difference between the entire window (title bar and all)
  // and the client area; add this to the new client size to move the
  // window
  int actual_width = rect2.right - rect2.left - rect.right + width;
  int actual_height = rect2.bottom - rect2.top - rect.bottom + height;

  MoveWindow(wnd->handle, rect2.left, rect2.top, actual_width, actual_height, TRUE);
  OnSize(actual_width, actual_height);
}

void wxDialogBox::GetPosition(int *x, int *y)
{
  HWND hWnd = GetHWND();
  RECT rect;
  GetWindowRect(hWnd, &rect);

  *x = rect.left;
  *y = rect.top;

  if (*x < -10000)
    *x = -10000;
  if (*y < -10000)
    *y = -10000;
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static int CheckDialogShowing(void *data)
{
  return !((wxDialogBox *)data)->modal_showing;
}

Bool wxDialogBox::Show(Bool show)
{
  wxWnd *dialog = (wxWnd *)handle;

  if (!!show == !!IsShown()) {
    if (show)
      wxwmBringWindowToTop(dialog->handle);
    return TRUE;
  }

  SetShown(show);

  wxTopLevelWindows(this)->Show(this, show);
  if (window_parent)
    window_parent->GetChildren()->Show(this, show);

  if (modal) {
    if (show) {
      if (modal_showing) {
	wxwmBringWindowToTop(dialog->handle);
	return TRUE;
      }
      
      modal_showing = TRUE;

      disabled_windows = new wxList();

      wxPushModalWindow(this, this);

      ShowWindow(dialog->handle, SW_SHOW); /* can involve callbacks? */

      if (disabled_windows)
 	wxwmBringWindowToTop(dialog->handle);

      if (disabled_windows) {
	wxChildNode *cnode;
	
	// Make list of windows to disable:
	for (cnode = wxTopLevelWindows(this)->First(); cnode; cnode = cnode->Next()) {
	  wxWindow *w = (wxWindow *)cnode->Data();
	  if (w && cnode->IsShown() && w != this) {
	    disabled_windows->Append(w);
	  }
	}
	
	// Disable them (may involve callbacks):
	{
	  wxNode *node;
	  
	  for (node = disabled_windows->First(); node; node = node->Next()) {
	    wxWindow *w = (wxWindow *)node->Data();
	    w->InternalEnable(FALSE);
	    if (!disabled_windows) /* means the dialog was closed while we were disabling */
	      break;
	  } 
	}
	
	if (disabled_windows)
	  wxDispatchEventsUntil(CheckDialogShowing, (void *)this);
      }
    } else {
      if (modal_showing) {
	modal_showing = FALSE;

	wxPopModalWindow(this, this);
	
	if (disabled_windows) {
	  wxNode *node;

	  node = disabled_windows->First();
	  disabled_windows = NULL;

	  for (; node; node = node->Next()) {
	    wxWindow *w = (wxWindow *)node->Data();
	    w->InternalEnable(TRUE);
	  } 
	}
	
	ShowWindow(dialog->handle, SW_HIDE);
	
	if (GetParent())
	  wxwmBringWindowToTop(GetParent()->GetHWND());
      }
    }
  } else {
    if (show) {
      ShowWindow(dialog->handle, SW_SHOW);
      wxwmBringWindowToTop(dialog->handle);
    } else {
      // Try to highlight the correct window (the parent)
      HWND hWndParent = 0;
      if (GetParent()) {
        hWndParent = GetParent()->GetHWND();
        if (hWndParent)
	  wxwmBringWindowToTop(hWndParent);
      }
      ShowWindow(dialog->handle, SW_HIDE);
    }
  }

  return TRUE;
}

void wxDialogBox::SetTitle(char *title)
{
  wxWnd *wnd = (wxWnd *)handle;
  SetWindowText(wnd->handle, title);
}

char *wxDialogBox::GetTitle(void)
{
  wxWnd *wnd = (wxWnd *)handle;
  GetWindowText(wnd->handle, wxBuffer, 1000);
  return wxBuffer;
}

void wxDialogBox::SystemMenu(void)
{
  ::DefWindowProc(GetHWND(), WM_SYSKEYDOWN, ' ', 1 << 29);
  ::DefWindowProc(GetHWND(), WM_SYSCHAR, ' ', 1 << 29);
}
