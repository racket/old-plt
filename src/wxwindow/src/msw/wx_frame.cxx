/*
 * File:	wx_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "fafa.h"

static HMENU emptyMenu;

wxPen *wxStatusGreyPen = NULL;
wxPen *wxStatusWhitePen = NULL;

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

wxFrame::wxFrame(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name):
  wxbFrame(Parent, title, x, y, width, height, style, name)
{
  Create(Parent, title, x, y, width, height, style, name);
}

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
  modal_showing = FALSE;
  handle = NULL;
  
  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    status_window[i] = NULL;

  hiddenmax = 0;
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

  wx_cursor = wxSTANDARD_CURSOR;  

  {
    /* Initialize client_d{w,h}, needed when GetCLientSize()
       is called while the frame is iconized. */
    int w, h, cw, ch;
    GetSize(&w, &h);
    GetClientSize(&cw, &ch);
    client_dh = h - ch;
    client_dw = w - cw;
  }

  return TRUE;
}

wxFrame::~wxFrame(void)
{
  if (wx_menu_bar)
    DELETE_OBJ wx_menu_bar;

  int i;
  for (i = 0; i < wxMAX_STATUS; i++) {
    if (status_window[i]) {
      status_window[i]->DestroyWindow();
      delete status_window[i];
    }
  }
}

HMENU wxFrame::GetWinMenu(void)
{
  if (handle)
    return ((wxWnd *)handle)->hMenu;
  else
    return 0;
}

void wxFrame::DrawMenuBar(void)
{
  wxFrame *frame;

  switch (frame_type) {
  case wxMDI_CHILD:
    frame = (wxFrame *)GetParent();
    break;
  default:
    frame = this;
    break;
  }

  wxWnd *cframe = (wxWnd*)frame->handle;
  HWND hand = (HWND)cframe->handle;
  ::DrawMenuBar(hand);
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

  if (Iconized()) {
    /* Iconized window's client size is always (0,0).
       Use stored client_d{w,h} to calculate the client
       size for the window as shown. */
    GetSize(x, y);
    rect.top = rect.left = 0;
    rect.right = *x - client_dw;
    rect.bottom = *y - client_dh;
  } else {
    GetClientRect(GetHWND(), &rect);
  }

  switch (frame_type)
  {
    case wxMDI_PARENT:
    {
      int cwidth = rect.right;
      int cheight = rect.bottom;
      int ctop = 0;

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
  OnSize(actual_width, actual_height);
}

void wxFrame::GetSize(int *width, int *height)
{
  HWND hwnd = GetHWND();

  if (::IsIconic(hwnd)) {
    WINDOWPLACEMENT wp;
    wp.length = sizeof(wp);
    GetWindowPlacement(hwnd, &wp);
    *width = wp.rcNormalPosition.right - wp.rcNormalPosition.left;
    *height = wp.rcNormalPosition.bottom - wp.rcNormalPosition.top;
  } else {
    RECT rect;
    GetWindowRect(hwnd, &rect);
    *width = rect.right - rect.left;
    *height = rect.bottom - rect.top;
  }
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

  if (*x < -10000)
    *x = -10000;
  if (*y < -10000)
    *y = -10000;
}

void wxFrame::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  /* Can't set size of an iconized frame. (We could actually play games
     with SetWindowPlacement, but it doesn't seem worthwhile.) */
  if (Iconized())
    Iconize(FALSE);

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
    OnSize(width, height);
  }
}

Bool wxFrame::Show(Bool show)
{
  int skipShow = (!show == !IsShown());

  SetShown(show);

  int cshow;
  if (show) {
    if (hiddenmax) {
      hiddenmax = 0;
      cshow = SW_SHOWMAXIMIZED;
    } else
      cshow = SW_RESTORE; /* Show */
  } else {
    if (!skipShow)
      hiddenmax = ::IsZoomed(GetHWND());
    cshow = SW_HIDE;
  }
  
  wxTopLevelWindows(this)->Show(this, show);
  if (window_parent)
    window_parent->GetChildren()->Show(this, show);

  if (!show) {
    // Try to highlight the correct window (the parent)
    HWND hWndParent = 0;
    if (GetParent()) {
      hWndParent = GetParent()->GetHWND();
      if (hWndParent)
	wxwmBringWindowToTop(hWndParent);
    }
  }
  
  if (!skipShow) {
    ShowWindow(GetHWND(), cshow);
  }
  if (show) {
    wxwmBringWindowToTop(GetHWND());
    /* OnActivate(TRUE); */
  }

  if (!skipShow) {
    if (frame_type == wxMDI_CHILD) {
      wxMDIFrame *cparent = (wxMDIFrame *)GetParent()->handle;
      if (cparent->current_child == (wxFrameWnd *)handle) {
	if (cshow == SW_HIDE) {
	  cparent->parent_frame_active = TRUE;
	  HMENU new_menu = ((wxFrame *)GetParent())->GetWinMenu();
	  
	  if (!new_menu) {
	    if (!emptyMenu)
	      emptyMenu = wxwmCreateMenu();
	    new_menu = emptyMenu;
	  }
	  
	  ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
			(WPARAM)new_menu,
			(LPARAM)NULL);
	  
	  ::DrawMenuBar(cparent->handle);
	}
      }
    }
  }

  return TRUE;
}

void wxFrame::Iconize(Bool iconize)
{
  if (!IsShown())
    return;

  if (!iconize && !Iconized())
    return; /* Otherwise we'd mess up maximizations */

  int cshow;
  HWND hwnd = GetHWND();
  if (iconize) {
    cshow = SW_MINIMIZE;
  } else {
    cshow = SW_RESTORE;
  }

  ShowWindow(GetHWND(), cshow);
}

// Equivalent to maximize/restore in Windows
void wxFrame::Maximize(Bool maximize)
{
  if (Iconized())
    return;

  if (IsShown()) {
    int cshow;
    if (maximize)
      cshow = SW_MAXIMIZE;
    else
      cshow = SW_RESTORE;
    ShowWindow(GetHWND(), cshow);
  } else
    hiddenmax = maximize;
}

Bool wxFrame::Iconized(void)
{
  return (Bool)::IsIconic(GetHWND());
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

static wxBitmap *black_bg = NULL;

void wxFrame::SetIcon(wxBitmap *icon, wxBitmap *bg, int kind)
/* kind: 1 = small, 2 = large, 0 = both */
{
  HICON wnd_icon;
  wxFrameWnd *wnd = (wxFrameWnd *)handle;

  int bigP = ((kind == 2) || (kind == 0));
  int smallP = ((kind == 1) || (kind == 0));

  if (bigP && wnd->bigIcon)
    DestroyIcon(wnd->bigIcon);
  if (smallP && wnd->icon)
    DestroyIcon(wnd->icon);

  if (!icon || !icon->Ok())
    wnd_icon = 0;
  else {
    ICONINFO info;

    if (!bg || !bg->Ok()) {
      if (!black_bg || (black_bg->GetWidth() != icon->GetWidth())
	  || (black_bg->GetHeight() != icon->GetHeight())) {
	black_bg = new wxBitmap(icon->GetWidth(), icon->GetHeight());
	wxMemoryDC *mdc = new wxMemoryDC();
	mdc->SelectObject(black_bg);
	mdc->SetBackground(wxBLACK);
	mdc->Clear();
	mdc->SelectObject(NULL);
      }
      bg = black_bg;
    }

    if (bg->Ok()) {
      info.fIcon = TRUE;
      info.hbmMask = bg->ms_bitmap;
      info.hbmColor = icon->ms_bitmap;
      wnd_icon = CreateIconIndirect(&info);
    } else
      wnd_icon = NULL;
  }

  if (bigP)
    wnd->bigIcon = wnd_icon;
  if (smallP)
    wnd->icon = wnd_icon;

  if (bigP)
    SendMessage(GetHWND(), WM_SETICON, (WORD)TRUE, (DWORD)wnd_icon);
  if (smallP)
    SendMessage(GetHWND(), WM_SETICON, (WORD)FALSE, (DWORD)wnd_icon);
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
    wxREGGLOB(wxStatusGreyPen);
    wxREGGLOB(wxStatusWhitePen);
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

  // Microsoft standard: use button colors for status line
  status_window[number]->light_grey_brush = brushFace ;

  if (text)
    status_window[number]->status_text = copystring(text);
  else 
    status_window[number]->status_text = NULL;

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

void wxFrame::SystemMenu(void)
{
  wxWnd *wnd = (wxWnd *)handle;
  wnd->DefWindowProc(WM_SYSKEYDOWN, ' ', 1 << 29);
  wnd->DefWindowProc(WM_SYSCHAR, ' ', 1 << 29);
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
  // Microsoft standard: use button colors for status line
  light_grey_brush = brushFace;

  Create(parent, wxPanelClassName, NULL, NULL, 0, 0, 100, 100, WS_CHILD);
  ShowWindow(handle, SW_SHOW);
}

wxStatusWnd::~wxStatusWnd(void)
{
}

BOOL wxStatusWnd::OnPaint()
{
  RECT rect;
  if (GetUpdateRect(handle, &rect, FALSE)) {

    // Microsoft standard: use button colors for status line
    light_grey_brush = brushFace ;

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
  defaultIcon = wxSTD_FRAME_ICON;

  DWORD msflags = WS_POPUP;
  
  DWORD extendedStyle = 0;
  if (!(style & wxNO_THICK_FRAME) && !(style & wxNO_RESIZE_BORDER)) {
    msflags |= WS_THICKFRAME | WS_BORDER;
    msflags |= WS_MINIMIZEBOX;
    msflags |= WS_MAXIMIZEBOX;
  }
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
}

wxFrameWnd::~wxFrameWnd(void)
{
  if (icon)
    DestroyIcon(icon);
  if (bigIcon)
    DestroyIcon(bigIcon);
}

BOOL wxFrameWnd::OnPaint(void)
{
  RECT rect;
  if (GetUpdateRect(handle, &rect, FALSE)) {
    PAINTSTRUCT ps;
    // Hold a pointer to the dc so long as the OnPaint() message
    // is being processed
    cdc = BeginPaint(handle, &ps);
      
    if (iconized) {
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
      wx_window->OnPaint();

    EndPaint(handle, &ps);
    cdc = NULL;
    return 0;
  }

  return 1;
}

HICON wxFrameWnd::OnQueryDragIcon(void)
{
  return NULL;
}

void wxFrameWnd::OnSize(int bad_x, int bad_y, UINT id)
{
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
    wx_window->OnSize(bad_x, bad_y);
 }
}

BOOL wxFrameWnd::OnClose(void)
{
  if (wx_window) {
    wxWindow *modal = wxGetModalWindow(wx_window);
    if (modal && (modal != wx_window))
      return FALSE;

    if (wx_window->OnClose()) {
      return TRUE;
    } else return FALSE;
  }
  return FALSE;
}

BOOL wxFrameWnd::OnCommand(WORD menuId, WORD cmd, HWND WXUNUSED(control))
{
  if (cmd == 0 || cmd == 1 ) { // Can be either a menu command or an accelerator.
    wxFrame *frame = (wxFrame *)wx_window;

    wxMenuBar *mb;
    if (mb = frame->GetMenuBar()) {
      wxMenuItem *i = mb->FindItemForMenuId(menuId);
      if (i) {
	if (i->checkable)
	  mb->Check(i->itemId, !mb->Checked(i->itemId));
    
	((wxFrame *)wx_window)->Command(i->itemId);
	return TRUE;
      }
    }
  }

  return FALSE;
}

void wxFrameWnd::OnMenuClick()
{
  wxFrame *frame = (wxFrame *)wx_window;
  frame->OnMenuClick();
}

void wxFrameWnd::OnMenuSelect(WORD nItem, WORD nFlags, HMENU hSysMenu)
{
  wxFrame *frame = (wxFrame *)wx_window;
  if (nFlags == 0xFFFF && hSysMenu == NULL)
    frame->OnMenuSelect(-1);
  else if (nFlags != MF_SEPARATOR)
    frame->OnMenuSelect(nItem);
}

BOOL wxFrameWnd::ProcessMessage(MSG* pMsg)
{
  return FALSE;
}

/*
 * Windows MDI stuff
 */

wxMDIFrame::wxMDIFrame(wxWnd *parent, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  defaultIcon = wxSTD_FRAME_ICON;
  icon = NULL;
  iconized = FALSE;
  parent_frame_active = TRUE;
  current_child = NULL;

  window_menu = ::LoadMenu(wxhInstance, "wxDefaultMenu");
  
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
}

wxMDIFrame::~wxMDIFrame(void)
{
  wxwmDestroyMenu(window_menu); // Destroy dummy "Window" menu
}

BOOL wxMDIFrame::OnDestroy(void)
{
  return FALSE;
}

void wxMDIFrame::OnCreate(LPCREATESTRUCT WXUNUSED(cs))
{
  CLIENTCREATESTRUCT ccs;
	
  ccs.hWindowMenu = window_menu;
  ccs.idFirstChild = wxFIRST_MDI_CHILD;

  client_hwnd = wxwmCreateWindowEx(0, "mdiclient", NULL,
				   WS_VISIBLE | WS_CHILD | WS_CLIPCHILDREN, 0, 0, 0, 0, 
				   handle, NULL,
				   wxhInstance, (LPSTR)(LPCLIENTCREATESTRUCT)&ccs);
}

void wxMDIFrame::OnSize(int bad_x, int bad_y, UINT id)
{
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

  if (frame && (frame->status_window[0]))
  {
    RECT rect;
    GetClientRect(handle, &rect);
    int cwidth = rect.right;
    int cheight = rect.bottom;
    int ctop = 0;

    cheight -= frame->status_window[0]->height;

    MoveWindow(client_hwnd, 0, ctop, cwidth, cheight, TRUE);

    frame->PositionStatusWindow();
  }
  else (void)DefWindowProc(last_msg, last_wparam, last_lparam);

  if (wx_window && wx_window->handle)
    wx_window->OnSize(bad_x, bad_y);
  }
}

BOOL wxMDIFrame::OnCommand(WORD id, WORD cmd, HWND control)
{
  if (parent_frame_active) {
    return wxFrameWnd::OnCommand(id, cmd, control);
  } else if (current_child) {
    return current_child->OnCommand(id, cmd, control);
  }
  
  return FALSE;
}

void wxMDIFrame::OnMenuSelect(WORD nItem, WORD nFlags, HMENU hSysMenu)
{
  if (parent_frame_active) {
    wxFrameWnd::OnMenuSelect(nItem, nFlags, hSysMenu);
  } else if (current_child) {
    current_child->OnMenuSelect(nItem, nFlags, hSysMenu);
  }
}

long wxMDIFrame::DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam)
{
  return ::DefFrameProc(handle, client_hwnd, message, wParam, lParam);
}

BOOL wxMDIFrame::ProcessMessage(MSG* pMsg)
{
  if ((current_child != NULL) 
      && (current_child->handle != NULL)
      && current_child->ProcessMessage(pMsg))
    return TRUE;

#if 0	
  if (accelerator_table != NULL &&
      ::TranslateAccelerator(handle, (HACCEL)accelerator_table, pMsg))
    return TRUE;
	
  if (pMsg->message == WM_KEYDOWN || pMsg->message == WM_SYSKEYDOWN)
  {
    if (::TranslateMDISysAccel(client_hwnd, pMsg))
      return TRUE;
  }
#endif

  return FALSE;
}

BOOL wxMDIFrame::OnEraseBkgnd(HDC WXUNUSED(pDC))
{
  return TRUE;
}

extern wxWnd *wxWndHook;
extern wxNonlockingHashTable *wxWinHandleList;

wxMDIChild::wxMDIChild(wxMDIFrame *parent, wxWindow *wx_win, char *title,
                   int x, int y, int width, int height, long style)
{
  defaultIcon = wxSTD_FRAME_ICON;
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
  else mcs.cx = 1;

  if (height > -1) mcs.cy = height;
  else mcs.cy = 1;

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

  HWND o;
  o = (HWND)::SendMessage(parent->client_hwnd, WM_MDIGETACTIVE, 0, 0);

  // turn off redrawing in the MDI client window
  SendMessage(parent->client_hwnd, WM_SETREDRAW, FALSE, 0L);
 
  DWORD Return = SendMessage(parent->client_hwnd,
			     WM_MDICREATE, 0, (LONG)(LPSTR)&mcs);
  
  //handle = (HWND)LOWORD(Return);
  // Must be the DWORRD for WIN32. And in 16 bits, HIWORD=0 (says Microsoft)
  handle = (HWND)Return;

  wxWndHook = NULL;
  wxWinHandleList->Append((long)handle, this);
  SetWindowLong(handle, 0, (long)this);
  
  ShowWindow(handle, SW_HIDE);

  // turn redrawing in the MDI client back on,
  // and force an immediate update
  SendMessage(parent->client_hwnd, WM_SETREDRAW, TRUE, 0L);
  InvalidateRect(parent->client_hwnd, NULL, TRUE);
  UpdateWindow(parent->client_hwnd);

  if (o) {
    InvalidateRect(o, NULL, TRUE); /* Because focus moved. */
    wxwmBringWindowToTop(o);
  }
}

static HWND invalidHandle = 0;
void wxMDIChild::OnSize(int bad_x, int bad_y, UINT id)
{
  if (!handle) return;

  if (invalidHandle == handle)
    return;
  
  (void)DefWindowProc(last_msg, last_wparam, last_lparam);
  
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
    wx_window->OnSize(bad_x, bad_y);
 }
}

BOOL wxMDIChild::OnClose(void)
{
  if (wx_window && handle) {
    if (wx_window->OnClose()) {
      return TRUE;
    } else 
      return FALSE;
  }
  return FALSE;
}

BOOL wxMDIChild::OnCommand(WORD id, WORD cmd, HWND control)
{
  return wxFrameWnd::OnCommand(id, cmd, control);
}

long wxMDIChild::DefWindowProc(UINT message, UINT wParam, LONG lParam)
{
  if (handle)
    return ::DefMDIChildProc(handle, message, wParam, lParam);
  else
    return 0;
}

BOOL wxMDIChild::ProcessMessage(MSG *msg)
{
  return FALSE;
}

BOOL wxMDIChild::OnMDIActivate(BOOL bActivate, HWND WXUNUSED(one), HWND WXUNUSED(two))
{
  wxFrame *parent = (wxFrame *)wx_window->GetParent();
  wxFrame *child = (wxFrame *)wx_window;

  HMENU new_menu, child_menu;

  wxMDIFrame *cparent = (wxMDIFrame *)parent->handle;
  if (bActivate) {
    active = TRUE;
    cparent->current_child = this;
    if (child)
      child_menu = child->GetWinMenu();
    else
      child_menu = NULL;
  } else {
    if (cparent->current_child == this)
      cparent->current_child = NULL;
    active = FALSE;
    child_menu = NULL;
  }

  if (child_menu) {
    cparent->parent_frame_active = FALSE;
    new_menu = child_menu;
  } else {
    cparent->parent_frame_active = TRUE;
    new_menu = parent->GetWinMenu();

    if (!new_menu) {
      if (!emptyMenu)
	emptyMenu = wxwmCreateMenu();
      new_menu = emptyMenu;
    }
  }

  ::SendMessage(cparent->client_hwnd, WM_MDISETMENU,
		(WPARAM)new_menu,
		(LPARAM)NULL);

  ::DrawMenuBar(cparent->handle);
  
  wxFrameWnd::OnActivate(bActivate, 0, 0);

  return 0;
}

wxMDIChild::~wxMDIChild(void)
{
}

void wxMDIChild::DestroyWindow(void)
{
  DetachWindowMenu();
  invalidHandle = handle;

  wxFrame *parent = (wxFrame *)wx_window->GetParent();
  wxMDIFrame *cparent = (wxMDIFrame *)parent->handle;

  // Must make sure this handle is invalidated (set to NULL)
  // since all sorts of things could happen after the
  // child client is destroyed, but before the wxFrame is
  // destroyed.

  HWND oldHandle = (HWND)handle;
  SendMessage(cparent->client_hwnd, WM_MDIDESTROY, (WPARAM)oldHandle, (LPARAM)0);

  invalidHandle = 0;

  if (hMenu) {
    wxwmDestroyMenu(hMenu);
    hMenu = 0;
  }
}
