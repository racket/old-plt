/*
 * File:	wx_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_dialg.cxx,v 1.4 1998/08/09 20:55:21 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wx_main.h"

#include "wx_setup.h"
#include "wx_wmgr.h"
#endif

#if USE_COMMON_DIALOGS
#include <commdlg.h>
#endif

#if CTL3D
#include <ctl3d.h>
#endif

#if FAFA_LIB
#include "fafa.h"
#endif

#if !defined(APIENTRY)	// NT defines APIENTRY, 3.x not
#define APIENTRY far pascal
#endif
 
#ifdef WIN32
#define _EXPORT /**/
#else
#define _EXPORT _export
typedef signed short int SHORT ;
#endif
 
#if !defined(WIN32)	// 3.x uses FARPROC for dialogs
#define DLGPROC FARPROC
#endif

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

// Lists to keep track of windows, so we can disable/enable them
// for modal dialogs
#if 0
wxList wxModalDialogs;
#endif
wxList wxModelessWindows;  // Frames and modeless dialogs

extern void wxCreatedWindow(wxWindow *w);

extern void wxDestroyedWindow(void *context, wxWindow *w);



class wxDialogWnd : public wxSubWnd
{
public:
  wxDialogWnd(wxWnd *parent, wxWindow *wx_win,
              int x, int y, int width, int height,
              char *dialog_template);

  // Handlers
  LONG DefWindowProc(UINT nMsg, UINT wParam, LONG lParam);
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

LONG wxDialogWnd::DefWindowProc(UINT nMsg, UINT wParam, LONG lParam)
{
  return ::DefWindowProc(handle, nMsg, wParam, lParam);
}

BOOL wxDialogWnd::ProcessMessage(MSG* pMsg)
{
  wxWindow *w = wx_window->FindItemByHWND(::GetFocus());



  if (w && !wxSubType(w->__type, wxTYPE_CANVAS))

    return ::IsDialogMessage(handle, pMsg);

  else

    return FALSE;
}

BOOL wxDialogWnd::OnClose(void)
{
  if (wx_window) {
    wxWindow *modal = wxGetModalWindow(wx_window);
    if (modal && (modal != wx_window))
      return FALSE;
    
    if (wx_window->GetEventHandler()->OnClose()) {
      /* MATTHEW: [11] */
#if !WXGARBAGE_COLLECTION_ON
      ((wxDialogBox *)wx_window)->Show(FALSE);
      delete wx_window;
#endif
      return TRUE;
    } else return FALSE;
  }
  return FALSE;
}

BOOL wxDialogWnd::OnEraseBkgnd(HDC pDC)
{
  if (background_brush)
  {
    RECT rect;
    GetClientRect(handle, &rect);
    int mode = SetMapMode(pDC, MM_TEXT);
    FillRect(pDC, &rect, background_brush);
    SetMapMode(pDC, mode);
    return TRUE;
  }
  else return FALSE;
}

IMPLEMENT_DYNAMIC_CLASS(wxDialogBox, wxPanel)

wxDialogBox::wxDialogBox(void)
{
  window_parent = NULL;
  handle = NULL;
  modal = FALSE;
  modal_showing = FALSE;
  has_child = FALSE ;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing ;
  initial_vspacing = vSpacing ;

  current_hspacing = hSpacing ;
  current_vspacing = vSpacing ;
}

// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal
wxDialogBox::wxDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
               int x, int y, int width, int height, long style, char *name):
  wxbDialogBox((wxWindow *)Parent, Title, Modal, x, y, width, height, style, name)
{
  Create(Parent, Title, Modal, x, y, width, height, style, name);
}
  
Bool wxDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name)
{
  SetName(name);

  if (!Parent) Parent = wxDefaultParent;

  // Do anything that needs to be done in the generic base class
  wxbDialogBox::Create(Parent, Title, Modal, x, y, width, height, style, name);

  has_child = FALSE ;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing ;
  initial_vspacing = vSpacing ;

  current_hspacing = hSpacing ;
  current_vspacing = vSpacing ;
  
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
    width = 500;
  if (height < 0)
    height = 500;

  // Allows creation of dialogs with & without captions under MSWindows
  wxDialogWnd *wnd;
  if (!(style & wxNO_CAPTION)) {
    wnd = new wxDialogWnd(cparent, this, x, y, width, height,
                          "wxCaptionDialog");
  }
  else{
    wnd = new wxDialogWnd(cparent, this, x, y, width, height,
                          "wxNoCaptionDialog");
  }

  handle = (char *)wnd;
  SetWindowText(wnd->handle, Title);

#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  if (!Modal)
    wxModelessWindows.Append(this);
#endif

  wxCreatedWindow(this);

  wx_cursor = wxSTANDARD_CURSOR;  

  wx_dc = new wxPanelDC (this);

  modal = Modal;
  return TRUE;
}

wxDialogBox::~wxDialogBox()
{
  wxWnd *wnd = (wxWnd *)handle;
  if (wnd && modal_showing)
    Show(FALSE);

  modal_showing = FALSE;
  if (wnd)
  {
    ShowWindow(wnd->handle, SW_HIDE);
  }

  if (!modal)
    wxModelessWindows.DeleteObject(this);



  wxDestroyedWindow(context, this);
}

// By default, pressing escape quits the dialog
Bool wxDialogBox::OnCharHook(wxKeyEvent& event)
{
  if (handle)
  {
    if (event.keyCode == WXK_ESCAPE)
    {
      if (OnClose())
      {
        Show(FALSE);
        // delete this;
      }
      return TRUE;
    }
  }
  return FALSE;
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
  GetEventHandler()->OnSize(actual_width, actual_height);
}

void wxDialogBox::GetPosition(int *x, int *y)
{
  HWND hWnd = GetHWND();
  RECT rect;
  GetWindowRect(hWnd, &rect);

  *x = rect.left;
  *y = rect.top;
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static int CheckDialogShowing(void *data)
{
  return !((wxDialogBox *)data)->modal_showing;
}

Bool wxDialogBox::Show(Bool show)
{
  wxWnd *dialog = (wxWnd *)handle;



  if (show == IsShown()) {
    if (show)
      wxwmBringWindowToTop(dialog->handle);
    return TRUE;
  }

  SetShown(show);

#if WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  if (!modal) {
    if (show) {
      if (!wxModelessWindows.Member(this))
	wxModelessWindows.Append(this);
    } else
      wxModelessWindows.DeleteObject(this);
  }
  if (!window_parent) {
    wxTopLevelWindows(this)->Show(this, show);
  } else
    window_parent->GetChildren()->Show(this, show);
#endif

  if (modal)
  {
    if (show)
    {
      if (modal_showing)
      {
	wxwmBringWindowToTop(dialog->handle);
	return TRUE;
      }
      
      modal_showing = TRUE;

      ShowWindow(dialog->handle, SW_SHOW);

      wxwmBringWindowToTop(dialog->handle);

      wxWindow *saveModal;
      saveModal = wxGetModalWindow(this);
      wxPutModalWindow(this, this);
      
      wxList *disabled_windows;
      wxChildNode *cnode;
      wxNode *node;

      disabled_windows = new wxList();
      
      for (cnode = wxTopLevelWindows(this)->First(); cnode; cnode = cnode->Next()) {
	wxWindow *w = (wxWindow *)cnode->Data();
	if (w && cnode->IsShown() && w != this) {
	  disabled_windows->Append(w);
	  w->InternalEnable(FALSE);
	}
      }
      
      wxDispatchEventsUntil(CheckDialogShowing, (void *)this);
      
      wxPutModalWindow(this, saveModal);
      
      for (node = disabled_windows->First(); node; node = node->Next()) {
	wxWindow *w = (wxWindow *)node->Data();
	w->InternalEnable(TRUE);
      } 

      ShowWindow(dialog->handle, SW_HIDE);
    }
    else
    {
      modal_showing = FALSE;
    }
  }
  else
  {
    if (show)
    {
      ShowWindow(dialog->handle, SW_SHOW);
		wxwmBringWindowToTop(dialog->handle);
	 }
    else
    {
      // Try to highlight the correct window (the parent)
      HWND hWndParent = 0;
      if (GetParent())
      {
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
