/*
 * File:	wx_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_frame.cxx,v 1.3 1998/08/09 20:55:21 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_setup.h"
#include "wx_privt.h"
#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_gdi.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_wmgr.h"

#endif

#if FAFA_LIB
# include "fafa.h"
#endif

#if USE_ITSY_BITSY
# include "..\..\contrib\itsybits\itsybits.h"
#endif


extern wxList wxModelessWindows;

wxPen *wxStatusGreyPen = NULL;
wxPen *wxStatusWhitePen = NULL;

#define IDM_WINDOWTILE  4001
#define IDM_WINDOWCASCADE 4002
#define IDM_WINDOWICONS 4003
#define IDM_WINDOWNEXT 4004
// This range gives a maximum of 500
// MDI children. Should be enough :-)
#define wxFIRST_MDI_CHILD 4100
#define wxLAST_MDI_CHILD 4600

// Status border dimensions
#define         wxTHICK_LINE_BORDER 3
#define         wxTHICK_LINE_WIDTH  1

extern char wxFrameClassName[];
extern char wxMDIFrameClassName[];
extern char wxMDIChildFrameClassName[];
extern char wxPanelClassName[];

IMPLEMENT_DYNAMIC_CLASS(wxFrame, wxWindow)

wxFrame::wxFrame(void)
{
  frame_type = 0;
  wx_menu_bar = NULL;
  status_line_exists = FALSE;
  icon = NULL;
  modal_showing = FALSE;
  window_parent = NULL;
  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    status_window[i] = NULL;
  wx_iconized = FALSE;
  wxWinType = 0;
  handle = NULL;
}

wxFrame::wxFrame(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name):
  wxbFrame(Parent, title, x, y, width, height, style, name)
{
  Create(Parent, title, x, y, width, height, style, name);
}


extern void wxCreatedWindow(wxWindow *w);

extern void wxDestroyedWindow(void *, wxWindow *w);


Bool wxFrame::Create(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name)
{
  wxbFrame::Create(Parent, title, x, y, width, height, style, name);
  
  SetName(name);
  frame_type = style & (wxSDI | wxMDI_PARENT | wxMDI_CHILD);
  if (!frame_type) frame_type = wxSDI;
  windowStyle = style;
  wx_menu_bar = NULL;
  status_line_exists = FALSE;
  icon = NULL;
  modal_showing = FALSE;
  handle = NULL;
  
  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    status_window[i] = NULL;

  wx_iconized = FALSE;
  wxWnd *cparent = NULL;
  if (Parent)
    cparent = (wxWnd *)Parent->handle;

  switch (frame_type)
  {
    case wxMDI_PARENT:
      wxWinType = wxTYPE_XWND;
      handle = (char *)new wxMDIFrame(NULL, this, title, x, y, width, height, style);
      break;
    case wxMDI_CHILD:
      wxWinType = wxTYPE_MDICHILD;
      handle = (char *)new wxMDIChild((wxMDIFrame *)cparent, this, title, x, y, width, height, style);
      break;
    default:
    case wxSDI:
      wxWinType = wxTYPE_XWND;
      handle = (char *)new wxFrameWnd(cparent, wxFrameClassName, this, title,
                   x, y, width, height, style);
      break;
  }

#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  wxModelessWindows.Append(this);
#endif

  wx_cursor = wxSTANDARD_CURSOR;  

  wxCreatedWindow(this);


  return TRUE;
}

wxFrame::~wxFrame(void)
{
  if (wx_menu_bar)
    delete wx_menu_bar;

#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  if (icon)
    delete icon;
#endif

  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    if (status_window[i])
    {
      status_window[i]->DestroyWindow();
      delete status_window[i];
    }

  if (this == wxTheApp->wx_frame)
  {
     wxTheApp->wx_frame = NULL;
     PostQuitMessage(0);
  }


  wxDestroyedWindow(context, this);


  wxModelessWindows.DeleteObject(this);
}

HMENU wxFrame::GetWinMenu(void)
{
  if (handle)
    return ((wxWnd *)handle)->hMenu;
  else return 0;
}


void wxFrame::ChangeToGray(Bool gray)

{

  wxWindow::ChangeToGray(gray);

  InternalGrayChildren(gray);

}




// Get size *available for subwindows* i.e. excluding menu bar etc.
// For XView, this is the same as GetSize
void wxFrame::GetClientSize(int *x, int *y)
{
  RECT rect;
  GetClientRect(GetHWND(), &rect);

  switch (frame_type)
  {
    case wxMDI_PARENT:
    {
      int cwidth = rect.right;
      int cheight = rect.bottom;
      int ctop = 0;

      if (frameToolBar)
      {
        int tw, th;
        frameToolBar->GetSize(&tw, &th);
        ctop = th;
        cheight -= th;
      }

      if (status_window[0])
        cheight -= status_window[0]->height;
      *x = cwidth;
      *y = cheight;
      break;
    }
    default:
    case wxMDI_CHILD:
    case wxSDI:
    {
      if (status_window[0])
        rect.bottom -= status_window[0]->height;

      *x = rect.right;
      *y = rect.bottom;
      break;
    }
  }
}

// Set the client size (i.e. leave the calculation of borders etc.
// to wxWindows)
void wxFrame::SetClientSize(int width, int height)
{
  wxFrame *parent = (wxFrame *)GetParent();
  HWND hWnd = GetHWND();
  HWND hParentWnd = 0;
  if (parent)
    hParentWnd = parent->GetHWND();

  RECT rect;
  GetClientRect(hWnd, &rect);

  RECT rect2;
  GetWindowRect(hWnd, &rect2);

  // Find the difference between the entire window (title bar and all)
  // and the client area; add this to the new client size to move the
  // window
  int actual_width = rect2.right - rect2.left - rect.right + width;
  int actual_height = rect2.bottom - rect2.top - rect.bottom + height;

  if (status_window[0])
    actual_height += status_window[0]->height;

  POINT point;
  point.x = rect2.left;
  point.y = rect2.top;

  // If there's an MDI parent, must subtract the parent's top left corner
  // since MoveWindow moves relative to the parent
  if (parent && (wxWinType == wxTYPE_MDICHILD))
  {
    ::ScreenToClient(hParentWnd, &point);
  }

  MoveWindow(hWnd, point.x, point.y, actual_width, actual_height, (BOOL)TRUE);
  GetEventHandler()->OnSize(actual_width, actual_height);
}

void wxFrame::GetSize(int *width, int *height)
{
  RECT rect;
  GetWindowRect(GetHWND(), &rect);
  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxFrame::GetPosition(int *x, int *y)
{
  wxWindow *parent = GetParent();

  RECT rect;
  GetWindowRect(GetHWND(), &rect);
  POINT point;
  point.x = rect.left;
  point.y = rect.top;

  // Since we now have the absolute screen coords,
  // if there's a parent we must subtract its top left corner
  if (parent)
  {
    wxWnd *cparent = (wxWnd *)(parent->handle);
    if (wxWinType == wxTYPE_MDICHILD)
    {
      wxMDIFrame *mdiParent = (wxMDIFrame *)cparent;
      ::ScreenToClient(mdiParent->client_hwnd, &point);
    }
  }
  *x = point.x;
  *y = point.y;
}

void wxFrame::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  int ww,hh ;
  GetSize(&ww,&hh) ;
  if (width == -1) width = ww ;
  if (height==-1) height = hh ;

  if (handle)
  {
    MoveWindow(GetHWND(), x, y, width, height, (BOOL)TRUE);
    GetEventHandler()->OnSize(width, height);
  }
}

Bool wxFrame::Show(Bool show)
{
  SetShown(show);

  int cshow;
  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
  
#if WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  if (show)  {
    if (!wxModelessWindows.Member(this))
      wxModelessWindows.Append(this);
  } else {
    wxModelessWindows.DeleteObject(this);
  }

  if (window_parent) {
    window_parent->GetChildren()->Show(this, show);
  } else {
# if 0
    if (show) {
      if (!wxTopLevelWindows(this)->Member(this))
	wxTopLevelWindows(this)->Append(this);
    } else
      wxTopLevelWindows(this)->DeleteObject(this);
# else
    wxTopLevelWindows(this)->Show(this, show);
# endif
  }
#endif  

  if (!show)
    {
      // Try to highlight the correct window (the parent)
      HWND hWndParent = 0;
      if (GetParent())
	{
	  hWndParent = GetParent()->GetHWND();
	  if (hWndParent)
		 wxwmBringWindowToTop(hWndParent);
	}
    }
  
  ShowWindow(GetHWND(), cshow);
  if (show)
  {
	 wxwmBringWindowToTop(GetHWND());
    OnActivate(TRUE);
  }
  return TRUE;
}

void wxFrame::Iconize(Bool iconize)
{
  if (!iconize)
    Show(TRUE);

  int cshow;
  if (iconize)
    cshow = SW_MINIMIZE;
  else
    cshow = SW_RESTORE;
  ShowWindow(GetHWND(), (BOOL)cshow);
  wx_iconized = iconize;
}

// Equivalent to maximize/restore in Windows
void wxFrame::Maximize(Bool maximize)
{
  Show(TRUE);
  int cshow;
  if (maximize)
    cshow = SW_MAXIMIZE;
  else
    cshow = SW_RESTORE;
  ShowWindow(GetHWND(), cshow);
  wx_iconized = FALSE;
}

Bool wxFrame::Iconized(void)
{
  wx_iconized = (Bool)::IsIconic(GetHWND());
  return wx_iconized;
}

void wxFrame::SetTitle(char *title)
{
  SetWindowText(GetHWND(), title);
}

char *wxFrame::GetTitle(void)
{
  GetWindowText(GetHWND(), wxBuffer, 1000);
  return wxBuffer;
}

void wxFrame::SetIcon(wxIcon *wx_icon)
{
  icon = wx_icon;
    
  wxFrameWnd *wnd = (wxFrameWnd *)handle;
  if (!wx_icon)
    wnd->icon = 0;
  else
    wnd->icon = wx_icon->ms_icon;
}

void wxFrame::CreateStatusLine(int number, char *WXUNUSED(name))
{
  if (status_line_exists)
    return;

  status_line_exists = TRUE;

  nb_status = number;

  wxFrameWnd *cframe = (wxFrameWnd *)handle;

  TEXTMETRIC tm;
  HDC dc = GetDC(cframe->handle);
  SelectObject(dc, wxSTATUS_LINE_FONT);
  GetTextMetrics(dc, &tm);
  ReleaseDC(cframe->handle, dc);
  int char_height = tm.tmHeight + tm.tmExternalLeading;
  int status_window_height =
     (int)((char_height * 4.0/3.0) + 2*wxTHICK_LINE_BORDER);

  if (!wxStatusGreyPen)
  {
    wxStatusGreyPen = new wxPen("DIM GREY", wxTHICK_LINE_WIDTH, wxSOLID);
    wxStatusWhitePen = new wxPen("WHITE", wxTHICK_LINE_WIDTH, wxSOLID);
  }

  int i;
  for (i = 0; i < number; i++)
    status_window[i] = new wxStatusWnd(cframe, status_window_height);
  PositionStatusWindow();
}

void wxFrame::SetStatusText(char *text, int number)
{
  if (!status_line_exists)
    return;

  if ((number < 0) || (number >= nb_status))
    return;

#if FAFA_LIB
  // Microsoft standard: use button colors for status line
  status_window[number]->light_grey_brush = brushFace ;
#endif

  if (status_window[number]->status_text)
    delete[] status_window[number]->status_text;

  if (text)
    status_window[number]->status_text = copystring(text);
  else status_window[number]->status_text = NULL;

  HDC dc = GetDC(status_window[number]->handle);
  SelectObject(dc, wxSTATUS_LINE_FONT);

  RECT rect;
  GetClientRect(status_window[number]->handle, &rect );

  int width = rect.right;
  int height = rect.bottom;

  SetBkMode(dc, TRANSPARENT);

  ::SetTextColor(dc, ::GetSysColor( COLOR_BTNTEXT ) );

  TEXTMETRIC tm;
  GetTextMetrics(dc, &tm);
  int cy = tm.tmHeight + tm.tmExternalLeading;
  int y = (int)((rect.bottom - cy)/2);

  rect.left += wxTHICK_LINE_BORDER + 1;
  rect.top += wxTHICK_LINE_BORDER + 1;
  rect.right -= (wxTHICK_LINE_BORDER + 1);
  rect.bottom -= (wxTHICK_LINE_BORDER + 1);
  FillRect(dc, &rect, status_window[number]->light_grey_brush);

  IntersectClipRect(dc, wxTHICK_LINE_BORDER + 3, y-1,
                            width - wxTHICK_LINE_BORDER - 1, height);

  if (status_window[number]->status_text)
    TextOut(dc, wxTHICK_LINE_BORDER + 4, y,
                status_window[number]->status_text, strlen(status_window[number]->status_text));

  SelectClipRgn(dc, NULL);
  ReleaseDC(status_window[number]->handle, dc);
}

void wxFrame::PositionStatusWindow(void)
{
  // We will assume that in a multi status line, all fields have the
  //  same width.
  RECT rect;
  GetClientRect(GetHWND(), &rect);
  int cwidth = rect.right;
  int cheight = rect.bottom;
  int i;
  for (i = 0; i < nb_status; i++)
  {
    int real_width = (int)(cwidth/nb_status);
    MoveWindow(status_window[i]->handle, i*real_width, cheight - status_window[0]->height,
                            real_width, status_window[0]->height, TRUE);
  }
}

void wxFrame::LoadAccelerators(char *table)
{
  wxFrameWnd *cframe = (wxFrameWnd *)handle;
  cframe->accelerator_table = ::LoadAccelerators(wxhInstance, table);
}

void wxFrame::Fit(void)
{
  // Work out max. size
  wxChildNode *node = children->First();
  int max_width = 0;
  int max_height = 0;
  while (node)
  {
    // Find a child that's a subwindow, but not a dialog box.
    wxWindow *win = (wxWindow *)node->Data();

    if ((wxSubType(win->__type, wxTYPE_PANEL) &&
         !wxSubType(win->__type, wxTYPE_DIALOG_BOX)) ||
        wxSubType(win->__type, wxTYPE_TEXT_WINDOW) ||
        wxSubType(win->__type, wxTYPE_CANVAS))
    {
      int width, height;
      int x, y;
      win->GetSize(&width, &height);
      win->GetPosition(&x, &y);

      if ((x + width) > max_width)
        max_width = x + width;
      if ((y + height) > max_height)
        max_height = y + height;
    }
    node = node->Next();
  }
  SetClientSize(max_width, max_height);
}

/*
 * Windows 3 specific windows
 *
 */

wxStatusWnd::wxStatusWnd(wxFrameWnd *parent, int the_height)
{
  status_text = NULL;
  height = the_height;
#if FAFA_LIB
  // Microsoft standard: use button colors for status line
  light_grey_brush = brushFace ;
#else
  light_grey_brush = GetStockObject(LTGRAY_BRUSH);
#endif

  Create(parent, wxPanelClassName, NULL, NULL, 0, 0, 100, 100, WS_CHILD);
  ShowWindow(handle, SW_SHOW);
}

wxStatusWnd::~wxStatusWnd(void)
{
  if (status_text)
    delete[] status_text;
}

BOOL wxStatusWnd::OnPaint()
{
#if DEBUG > 1
  wxDebugMsg("wxStatusWnd::OnPaint %d\n", handle);
#endif
  RECT rect;
  if (GetUpdateRect(handle, &rect, FALSE))
  {
#if FAFA_LIB
  // Microsoft standard: use button colors for status line
    light_grey_brush = brushFace ;
#endif
    PAINTSTRUCT ps;
    // Hold a pointer to the dc so long as the OnPaint() message
    // is being processed
    cdc = BeginPaint(handle, &ps);
    SelectObject(cdc, wxSTATUS_LINE_FONT);

    ::GetClientRect(handle, &rect);

    int width = rect.right;
    int height = rect.bottom;

    ::SetBkMode(cdc, TRANSPARENT);
    ::FillRect(cdc, &rect, light_grey_brush);

#if !FAFA_LIB
    wxGREY_BRUSH->ChangeBrush() ;
    HBRUSH old_brush = wxGREY_BRUSH->SelectBrush(cdc);

    // Draw border
    // Have grey background, plus 3-d border -
    // One black rectangle.
    // Inside this, left and top sides - dark grey. Bottom and right -
    // white.

    // Right and bottom white lines
    wxStatusWhitePen->ChangePen() ;
    HPEN old_pen = wxStatusWhitePen->SelectPen(cdc);
    MoveToEx(cdc, width-wxTHICK_LINE_BORDER,
                  wxTHICK_LINE_BORDER, NULL);
    LineTo(cdc, width-wxTHICK_LINE_BORDER,
                height-wxTHICK_LINE_BORDER);
    LineTo(cdc, wxTHICK_LINE_BORDER,
              height-wxTHICK_LINE_BORDER);

    // Left and top grey lines
    wxStatusGreyPen->ChangePen() ;
    wxStatusGreyPen->SelectPen(cdc);
    LineTo(cdc, wxTHICK_LINE_BORDER, wxTHICK_LINE_BORDER);
    LineTo(cdc, width-wxTHICK_LINE_BORDER, wxTHICK_LINE_BORDER);
#else
    HBRUSH old_brush = (HBRUSH)::SelectObject(cdc,brushFace) ;

    // Draw border
    // Have grey background, plus 3-d border -
    // One black rectangle.
    // Inside this, left and top sides - dark grey. Bottom and right -
    // white.

    // Right and bottom white lines
    HPEN old_pen = (HPEN)::SelectObject(cdc,penLight) ;
    MoveToEx(cdc, width-wxTHICK_LINE_BORDER,
                  wxTHICK_LINE_BORDER, NULL);
    LineTo(cdc, width-wxTHICK_LINE_BORDER,
                height-wxTHICK_LINE_BORDER);
    LineTo(cdc, wxTHICK_LINE_BORDER,
              height-wxTHICK_LINE_BORDER);

    // Left and top grey lines
    ::SelectObject(cdc,penShadow) ;
    LineTo(cdc, wxTHICK_LINE_BORDER, wxTHICK_LINE_BORDER);
    LineTo(cdc, width-wxTHICK_LINE_BORDER, wxTHICK_LINE_BORDER);
#endif

    SetTextColor(cdc, ::GetSysColor( COLOR_BTNTEXT ) );

    TEXTMETRIC tm;
    ::GetTextMetrics(cdc, &tm);
    int cy = tm.tmHeight + tm.tmExternalLeading;
    int y = (int)((rect.bottom - cy)/2);

    ::IntersectClipRect(cdc, wxTHICK_LINE_BORDER + 3, y-1,
                            rect.right - wxTHICK_LINE_BORDER - 1, rect.bottom);

    if (status_text)
      ::TextOut(cdc, wxTHICK_LINE_BORDER + 4, y,
                  status_text, strlen(status_text));

    ::SelectClipRgn(cdc, NULL);
    if (old_pen)
      SelectObject(cdc, old_pen);
    old_pen = NULL ;
    if (old_brush)
      SelectObject(cdc, old_brush);
    old_brush = NULL ;

    EndPaint(handle, &ps);
    cdc = NULL;
    return 0;
  }
  return 1;
}


/*
 * Frame window
 *
 */

wxFrameWnd::wxFrameWnd(void)
{
}
		   
wxFrameWnd::wxFrameWnd(wxWnd *parent, char *WXUNUSED(wclass), wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  defaultIcon = (wxSTD_FRAME_ICON ? wxSTD_FRAME_ICON : wxDEFAULT_FRAME_ICON);

//  DWORD msflags = WS_OVERLAPPED;
  DWORD msflags = WS_POPUP;
//  if (((style & wxCAPTION) == 0) && (style & wxTHICK_FRAME))
//    msflags = WS_DLGFRAME;
  
  DWORD extendedStyle = 0;
  if (!(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_MINIMIZEBOX;
    msflags |= WS_MAXIMIZEBOX;
  }
  if (!(style & wxNO_THICK_FRAME))
    msflags |= WS_THICKFRAME | WS_BORDER;
  if (!(style & wxNO_SYSTEM_MENU))
    msflags |= WS_SYSMENU;
  if (style & wxICONIZE)
    msflags |= WS_MINIMIZE;
  if (style & wxMAXIMIZE)
    msflags |= WS_MAXIMIZE;
  if (!(style & wxNO_CAPTION))
    msflags |= WS_CAPTION;

  if (style & wxSTAY_ON_TOP)
    extendedStyle |= WS_EX_TOPMOST;

  icon = NULL;
  iconized = FALSE;
  Create(parent, wxFrameClassName, wx_win, title, x, y, width, height,
         msflags, NULL, extendedStyle);
  // Seems to be necessary if we use WS_POPUP
  // style instead of WS_OVERLAPPED
  if (width > -1 && height > -1)
    ::PostMessage(handle, WM_SIZE, SIZE_RESTORED, MAKELPARAM(width, height));
#if DEBUG > 1
  wxDebugMsg("wxFrameWnd::wxFrameWnd %d\n", handle);
#endif
}

wxFrameWnd::~wxFrameWnd(void)
{
}

BOOL wxFrameWnd::OnPaint(void)
{
#if DEBUG > 1
  wxDebugMsg("wxFrameWnd::OnPaint %d\n", handle);
#endif
  RECT rect;
  if (GetUpdateRect(handle, &rect, FALSE))
  {
    PAINTSTRUCT ps;
    // Hold a pointer to the dc so long as the OnPaint() message
    // is being processed
    cdc = BeginPaint(handle, &ps);
      
    if (iconized)
    {
      HICON the_icon = icon;
      if (the_icon == 0)
        the_icon = defaultIcon;

      // Erase background before painting or we get white background
      this->DefWindowProc(WM_ICONERASEBKGND,(WORD)ps.hdc,0L);
      
      if (the_icon)
      {
        RECT rect;
        GetClientRect(handle, &rect);
        int icon_width = 32;
        int icon_height = 32;
        int icon_x = (int)((rect.right - icon_width)/2);
        int icon_y = (int)((rect.bottom - icon_height)/2);
        DrawIcon(cdc, icon_x, icon_y, the_icon);
      }
    }

    if (!iconized && wx_window)
      wx_window->GetEventHandler()->OnPaint();

    EndPaint(handle, &ps);
    cdc = NULL;
    return 0;
  }
  return 1;
}

HICON wxFrameWnd::OnQueryDragIcon(void)
{
  if (icon != 0)
    return icon;
  else
    return defaultIcon;
}

void wxFrameWnd::OnSize(int x, int y, UINT id)
{
#if DEBUG > 1
  wxDebugMsg("wxFrameWnd::OnSize %d\n", handle);
#endif
  switch (id)
  {
    case SIZEFULLSCREEN:
    case SIZENORMAL:
      iconized = FALSE;
    break;
    case SIZEICONIC:
      iconized = TRUE;
    break;
  }

 if (!iconized)
 {
  wxFrame *frame = (wxFrame *)wx_window;
  if (frame && frame->status_window[0])
    frame->PositionStatusWindow();

  if (wx_window && wx_window->handle)
    wx_window->GetEventHandler()->OnSize(x, y);
 }
}

BOOL wxFrameWnd::OnClose(void)
{
#if DEBUG > 1
  wxDebugMsg("wxFrameWnd::OnClose %d\n", handle);
#endif
  if (wx_window) {
    wxWindow *modal = wxGetModalWindow(wx_window);
    if (modal && (modal != wx_window))
      return FALSE;

    if (wx_window->GetEventHandler()->OnClose()) {
      /* MATTHEW: [11] */
#if !WXGARBAGE_COLLECTION_ON
      delete wx_window;
#endif
      return TRUE;
    } else return FALSE;
  }
  return FALSE;
}

BOOL wxFrameWnd::OnCommand(WORD id, WORD cmd, HWND WXUNUSED(control))
{
#if DEBUG > 1
  wxDebugMsg("wxFrameWnd::OnCommand %d\n", handle);
#endif
  if (cmd == 0 || cmd == 1 ) // Can be either a menu command or an accelerator.
  {
    wxFrame *frame = (wxFrame *)wx_window;
    if (frame->GetMenuBar() && frame->GetMenuBar()->FindItemForId(id))
    {
      ((wxFrame *)wx_window)->Command(id);
      return TRUE;
    }
    else
      return FALSE;
  }
  else
    return FALSE;
}

void wxFrameWnd::OnMenuSelect(WORD nItem, WORD nFlags, HMENU hSysMenu)
{
  wxFrame *frame = (wxFrame *)wx_window;
  if (nFlags == 0xFFFF && hSysMenu == NULL)
    frame->GetEventHandler()->OnMenuSelect(-1);
  else if (nFlags != MF_SEPARATOR)
    frame->GetEventHandler()->OnMenuSelect(nItem);
}

BOOL wxFrameWnd::ProcessMessage(MSG* pMsg)
{
  if (accelerator_table != NULL &&
          ::TranslateAccelerator(handle, (HACCEL)accelerator_table, pMsg))
    return TRUE;
	
  return FALSE;
}

/*
 * Windows MDI stuff
 */

wxMDIFrame::wxMDIFrame(wxWnd *parent, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  defaultIcon = (wxSTD_MDIPARENTFRAME_ICON ? wxSTD_MDIPARENTFRAME_ICON : wxDEFAULT_MDIPARENTFRAME_ICON);
  icon = NULL;
  iconized = FALSE;
  parent_frame_active = TRUE;
  current_child = NULL;

  window_menu = ::LoadMenu(wxhInstance, "wxDefaultMenu");
#if DEBUG > 1
  wxDebugMsg("Loaded window_menu %d\n", window_menu);
#endif
  
  DWORD msflags = WS_OVERLAPPED;
  if (!(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_MINIMIZEBOX;
    msflags |= WS_MAXIMIZEBOX;
  }
  if (!(style & wxNO_THICK_FRAME))
    msflags |= WS_THICKFRAME;
  if (!(style & wxNO_SYSTEM_MENU))
    msflags |= WS_SYSMENU;
  if (style & wxMINIMIZE)
    msflags |= WS_MINIMIZE;
  if (style & wxMAXIMIZE)
    msflags |= WS_MAXIMIZE;
  if (!(style & wxNO_CAPTION))
    msflags |= WS_CAPTION;

  Create(parent, wxMDIFrameClassName, wx_win, title, x, y, width, height,
         msflags);
#if DEBUG > 1
  wxDebugMsg("End of wxMDIFrame::wxMDIFrame %d\n", handle);
#endif
}

wxMDIFrame::~wxMDIFrame(void)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::~wxMDIFrame %d\n", handle);
#endif
  wxwmDestroyMenu(window_menu); // Destroy dummy "Window" menu
}

BOOL wxMDIFrame::OnDestroy(void)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::OnDestroy %d\n", handle);
#endif
  return FALSE;
}

void wxMDIFrame::OnCreate(LPCREATESTRUCT WXUNUSED(cs))
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::OnCreate %d\n", handle);
#endif
  CLIENTCREATESTRUCT ccs;
	
  ccs.hWindowMenu = window_menu;
  ccs.idFirstChild = wxFIRST_MDI_CHILD;

  client_hwnd = wxwmCreateWindowEx(0, "mdiclient", NULL,
					 WS_VISIBLE | WS_CHILD | WS_CLIPCHILDREN, 0, 0, 0, 0, handle, NULL,
					 wxhInstance, (LPSTR)(LPCLIENTCREATESTRUCT)&ccs);
}

void wxMDIFrame::OnSize(int x, int y, UINT id)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::OnSize %d\n", handle);
#endif
  switch (id)
  {
    case SIZEFULLSCREEN:
    case SIZENORMAL:
      iconized = FALSE;
    break;
    case SIZEICONIC:
      iconized = TRUE;
    break;
  }

 if (!iconized)
 {
  wxFrame *frame = (wxFrame *)wx_window;

  if (frame && (frame->status_window[0] || frame->frameToolBar))
  {
    RECT rect;
    GetClientRect(handle, &rect);
    int cwidth = rect.right;
    int cheight = rect.bottom;
    int ctop = 0;
    int tw, th;

    if (frame->frameToolBar)
    {
      frame->frameToolBar->GetSize(&tw, &th);
      ctop = th;
      cheight -= th;
    }

    if (frame->status_window[0])
      cheight -= frame->status_window[0]->height;

    MoveWindow(client_hwnd, 0, ctop, cwidth, cheight, TRUE);

    if (frame->frameToolBar)
      frame->frameToolBar->SetSize(0, 0, cwidth, th);
    if (frame->status_window[0])
      frame->PositionStatusWindow();
  }
  else (void)DefWindowProc(last_msg, last_wparam, last_lparam);

  if (wx_window && wx_window->handle)
    wx_window->GetEventHandler()->OnSize(x, y);
  }
}

BOOL wxMDIFrame::OnCommand(WORD id, WORD cmd, HWND control)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::OnCommand %d, id = %d\n", handle, id);
#endif
  if (cmd == 0)
  {
    switch (id)
    {
      case IDM_WINDOWCASCADE:
        SendMessage(client_hwnd, WM_MDICASCADE, MDITILE_SKIPDISABLED, 0);
        return TRUE;
      case IDM_WINDOWTILE:
        SendMessage(client_hwnd, WM_MDITILE, MDITILE_HORIZONTAL, 0);
        return TRUE;
      case IDM_WINDOWICONS:
        SendMessage(client_hwnd, WM_MDIICONARRANGE, 0, 0);
        return TRUE;
      case IDM_WINDOWNEXT:
//        SendMessage(client_hwnd, WM_MDINEXT, current_child->handle, 0);
        SendMessage(client_hwnd, WM_MDINEXT, 0, 0);
        return TRUE;
      default:
        break;
     }
    if (id >= 0xF000)
    {
#if DEBUG > 1
      wxDebugMsg("wxMDIFrame::OnCommand %d: system command: calling default window proc\n", handle);
#endif
      return FALSE; // Get WndProc to call default proc
    }
    
    if (parent_frame_active && (id < wxFIRST_MDI_CHILD || id > wxLAST_MDI_CHILD))
    {
      ((wxFrame *)wx_window)->Command(id);
      return TRUE;
    }
    else if (current_child && (id < wxFIRST_MDI_CHILD || id > wxLAST_MDI_CHILD))
    {
/*
      ((wxFrame *)(current_child->wx_window))->Command(id);
      return TRUE;
*/
#if DEBUG > 1
      wxDebugMsg("wxMDIFrame::OnCommand %d: calling child OnCommand\n", handle);
#endif
      return current_child->OnCommand(id, cmd, control);
    }
  }
  return FALSE;
}

void wxMDIFrame::OnMenuSelect(WORD nItem, WORD nFlags, HMENU hSysMenu)
{
  if (parent_frame_active)
  {
    wxFrame *frame = (wxFrame *)wx_window;
    if (nFlags == 0xFFFF && hSysMenu == NULL)
      frame->GetEventHandler()->OnMenuSelect(-1);
    else if (nFlags != MF_SEPARATOR)
      frame->GetEventHandler()->OnMenuSelect(nItem);
  }
  else if (current_child)
  {
    current_child->OnMenuSelect(nItem, nFlags, hSysMenu);
  }
}

long wxMDIFrame::DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::DefWindowProc %d, message = %d\n", handle, message);
#endif
  return DefFrameProc(handle, client_hwnd, message, wParam, lParam);
}

BOOL wxMDIFrame::ProcessMessage(MSG* pMsg)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIFrame::ProcessMessage %d\n", handle);
#endif
  if ((current_child != NULL) && (current_child->handle != NULL) && current_child->ProcessMessage(pMsg))
     return TRUE;
	
  if (accelerator_table != NULL &&
          ::TranslateAccelerator(handle, (HACCEL)accelerator_table, pMsg))
    return TRUE;
	
  if (pMsg->message == WM_KEYDOWN || pMsg->message == WM_SYSKEYDOWN)
  {
    if (::TranslateMDISysAccel(client_hwnd, pMsg))
      return TRUE;
  }

  return FALSE;
}

BOOL wxMDIFrame::OnEraseBkgnd(HDC WXUNUSED(pDC))
{
  return TRUE;
}

extern wxWnd *wxWndHook;
#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
extern wxList *wxWinHandleList;
#else
extern wxNonlockingHashTable *wxWinHandleList;
#endif

wxMDIChild::wxMDIChild(wxMDIFrame *parent, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  defaultIcon = (wxSTD_MDICHILDFRAME_ICON ? wxSTD_MDICHILDFRAME_ICON : wxDEFAULT_MDICHILDFRAME_ICON);
  icon = NULL;
  iconized = FALSE;
  wx_window = wx_win;
  active = FALSE;
  is_dialog = FALSE;

  wxWndHook = this;

  MDICREATESTRUCT mcs;
	
  mcs.szClass = wxMDIChildFrameClassName;
  mcs.szTitle = title;
  mcs.hOwner = wxhInstance;
  if (x > -1) mcs.x = x;
  else mcs.x = CW_USEDEFAULT;

  if (y > -1) mcs.y = y;
  else mcs.y = CW_USEDEFAULT;

  if (width > -1) mcs.cx = width;
  else mcs.cx = CW_USEDEFAULT;

  if (height > -1) mcs.cy = height;
  else mcs.cy = CW_USEDEFAULT;

  DWORD msflags = WS_OVERLAPPED;
  if (!(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_MINIMIZEBOX;
    msflags |= WS_MAXIMIZEBOX;
  }
  if (!(style & wxNO_THICK_FRAME))
    msflags |= WS_THICKFRAME;
  if (!(style & wxNO_SYSTEM_MENU))
    msflags |= WS_SYSMENU;
  if (style & wxMINIMIZE)
    msflags |= WS_MINIMIZE;
  if (style & wxMAXIMIZE)
    msflags |= WS_MAXIMIZE;
  if (!(style & wxNO_CAPTION))
    msflags |= WS_CAPTION;

  mcs.style = msflags;

  mcs.lParam = 0;

  DWORD Return = SendMessage(parent->client_hwnd,
		WM_MDICREATE, 0, (LONG)(LPSTR)&mcs);

  //handle = (HWND)LOWORD(Return);
  // Must be the DWORRD for WIN32. And in 16 bits, HIWORD=0 (says Microsoft)
  handle = (HWND)Return;

  wxWndHook = NULL;
  wxWinHandleList->Append((long)handle, this);

  SetWindowLong(handle, 0, (long)this);
#if DEBUG > 1
  wxDebugMsg("End of wxMDIChild::wxMDIChild %d\n", handle);
#endif
}

static HWND invalidHandle = 0;
void wxMDIChild::OnSize(int x, int y, UINT id)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIChild::OnSize %d\n", handle);
#endif
  if (!handle) return;

  if (invalidHandle == handle)
  {
#if DEBUG > 1
  wxDebugMsg("wxMDIChild::OnSize %d: invalid, so returning.\n", handle);
#endif
    return;
  }
  
#if DEBUG > 1
  wxDebugMsg("wxMDIChild::OnSize %d: calling DefWindowProc\n", handle);
#endif
  (void)DefWindowProc(last_msg, last_wparam, last_lparam);
#if DEBUG > 1
  wxDebugMsg("wxMDIChild::OnSize %d: called DefWindowProc\n", handle);
#endif
  
  switch (id)
  {
    case SIZEFULLSCREEN:
    case SIZENORMAL:
      iconized = FALSE;
    break;
    case SIZEICONIC:
      iconized = TRUE;
    break;
  }

 if (!iconized)
 {
  wxFrame *frame = (wxFrame *)wx_window;
  if (frame && frame->status_window[0])
    frame->PositionStatusWindow();

  if (wx_window && wx_window->handle)
    wx_window->GetEventHandler()->OnSize(x, y);
 }
}

BOOL wxMDIChild::OnClose(void)
{
#if DEBUG > 1
  wxDebugMsg("wxMDIChild::OnClose %d\n", handle);
#endif
  if (wx_window && handle)
  {
    if (wx_window->GetEventHandler()->OnClose())
    {
      delete wx_window;
      return TRUE;
    } else return FALSE;
  }
  return FALSE;
}

BOOL wxMDIChild::OnCommand(WORD id, WORD cmd, HWND WXUNUSED(control))
{
#if DEBUG > 1
  wxDebugMsg("wxMDIChild::OnCommand %d\n", handle);
#endif
//  (void)DefWindowProc(last_msg, last_wparam, last_lparam);
  if ((cmd == 0) && handle)
  {
    ((wxFrame *)wx_window)->Command(id);
    return TRUE;
  }
  else
    return FALSE;
}

long wxMDIChild::DefWindowProc(UINT message, UINT wParam, LONG lParam)
{
  if (handle)
    return DefMDIChildProc(handle, message, wParam, lParam);
  else return 0;
}

BOOL wxMDIChild::ProcessMessage(MSG *msg)
{
  if (accelerator_table && handle)
  {
    wxFrame *parent = (wxFrame *)wx_window->GetParent();
    HWND parent_hwnd = parent->GetHWND();
    return ::TranslateAccelerator(parent_hwnd, (HACCEL)accelerator_table, msg);
  }
  return FALSE;
}

BOOL wxMDIChild::OnMDIActivate(BOOL bActivate, HWND WXUNUSED(one), HWND WXUNUSED(two))
{
  wxFrame *parent = (wxFrame *)wx_window->GetParent();
  wxFrame *child = (wxFrame *)wx_window;
  HMENU parent_menu = parent->GetWinMenu();
#if DEBUG > 1
  wxDebugMsg("Parent menu is %d\n", parent_menu);
#endif
  HMENU child_menu = child->GetWinMenu();
#if DEBUG > 1
  wxDebugMsg("Child menu is %d\n", child_menu);
#endif

  wxMDIFrame *cparent = (wxMDIFrame *)parent->handle;
  if (bActivate)
  {
    active = TRUE;
    cparent->current_child = this;
    if (child_menu)
    {
      cparent->parent_frame_active = FALSE;
      HMENU subMenu = GetSubMenu(cparent->window_menu, 0);
#if DEBUG > 1
      wxDebugMsg("Window submenu is %d\n", subMenu);
#endif
//      HMENU subMenu = 0;
#ifdef WIN32
      ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
                    (WPARAM)child_menu,
                    (LPARAM)subMenu);
#else
      ::SendMessage(cparent->client_hwnd, WM_MDISETMENU, 0,
                  MAKELONG(child_menu, subMenu));
#endif

      ::DrawMenuBar(cparent->handle);
    }
    if (child)
      child->GetEventHandler()->OnActivate(TRUE);
  }
  else
  {
    if (cparent->current_child == this)
      cparent->current_child = NULL;
    if (child)
      child->GetEventHandler()->OnActivate(FALSE);

    active = FALSE;
    if (parent_menu)
    {
      cparent->parent_frame_active = TRUE;
      HMENU subMenu = GetSubMenu(cparent->window_menu, 0);
#if DEBUG > 1
      wxDebugMsg("Window submenu is %d\n", subMenu);
#endif
//      HMENU subMenu = 0;
#ifdef WIN32
      ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
                    (WPARAM)parent_menu,
                    (LPARAM)subMenu);
#else
      ::SendMessage(cparent->client_hwnd, WM_MDISETMENU, 0,
                  MAKELONG(parent_menu, subMenu));
#endif

      ::DrawMenuBar(cparent->handle);
    }
  }
  wx_window->GetEventHandler()->OnActivate(bActivate);
#if DEBUG > 1
  wxDebugMsg("Finished (de)activating\n");
#endif
  return 0;
}

wxMDIChild::~wxMDIChild(void)
{
}

void wxMDIChild::DestroyWindow(void)
{
#if DEBUG > 1
  wxDebugMsg("Start of wxMDIChild::DestroyWindow %d\n", handle);
#endif
  DetachWindowMenu();
  invalidHandle = handle;

  wxFrame *parent = (wxFrame *)wx_window->GetParent();
  wxMDIFrame *cparent = (wxMDIFrame *)parent->handle;

  // Must make sure this handle is invalidated (set to NULL)
  // since all sorts of things could happen after the
  // child client is destroyed, but before the wxFrame is
  // destroyed.

  HWND oldHandle = (HWND)handle;
#if DEBUG > 1
  wxDebugMsg("*** About to DestroyWindow MDI child %d\n", oldHandle);
#endif
#ifdef WIN32
  SendMessage(cparent->client_hwnd, WM_MDIDESTROY, (WPARAM)oldHandle, (LPARAM)0);
#else
  SendMessage(cparent->client_hwnd, WM_MDIDESTROY, (HWND)oldHandle, 0);
#endif
#if DEBUG > 1
  wxDebugMsg("*** Finished DestroyWindow MDI child %d\n", oldHandle);
#endif
  invalidHandle = 0;

  if (hMenu)
  {
	 wxwmDestroyMenu(hMenu);
	 hMenu = 0;
  }
}



