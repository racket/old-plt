/*
 * File:	wx_buttn.cc
 * Purpose:	Button implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_panel.h"
#include "wx_buttn.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

#define BUTTON_HEIGHT_FACTOR (EDIT_CONTROL_FACTOR * 1.1)

// Buttons

BOOL wxButton::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == BN_CLICKED)
  {
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
    ProcessCommand(*event);
    return TRUE;
  }
  else return FALSE;
}

IMPLEMENT_DYNAMIC_CLASS(wxButton, wxItem)

wxButton::wxButton(void)
{
  wxWinType = wxTYPE_HWND;
  windows_id = 0;
  ms_handle = 0;
}

wxButton::wxButton(wxPanel *panel, wxFunction Function, char *label,
		   int x, int y, int width, int height,
                   long style, char *name):
  wxbButton(panel, Function, label, x, y, width, height, style, name)
{
  Create(panel, Function, label, x, y, width, height, style, name);
}

wxButton::wxButton(wxPanel *panel, wxFunction Function, wxBitmap *bitmap,
		   int x, int y, int width, int height,
                   long style, char *name):
  wxbButton(panel, Function, bitmap, x, y, width, height, style, name)
{
  Create(panel, Function, bitmap, x, y, width, height, style, name);
}

Bool wxButton::Create(wxPanel *panel, wxFunction Function, char *label,
		   int x, int y, int width, int height,
                   long style, char *name)
{
  SetName(name);
  
  if (panel) panel->AddChild(this);
  buttonFont = panel->buttonFont ;
  labelFont = panel->labelFont ;
  backColour = panel->backColour ;
  labelColour = panel->labelColour ;
  buttonColour = panel->buttonColour ;

  wxWinType = wxTYPE_HWND;
  windowStyle = style;

  wxWnd *cparent = NULL;
  if (panel)
    cparent = (wxWnd *)(panel->handle);

  panel->GetValidPosition(&x, &y);

  windows_id = (int)NewId();

  HWND wx_button =
    wxwmCreateWindowEx(0, "wxBUTTON", label, 
		       BS_PUSHBUTTON | WS_TABSTOP | WS_CHILD
		       | ((style & 1) ? WS_BORDER : 0),
		       0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
		       wxhInstance, NULL);

#if CTL3D
  Ctl3dSubclassCtl(wx_button);
#endif

  ms_handle = (HANDLE)wx_button;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(wx_button);

  if (buttonFont) {
    HDC the_dc = GetWindowDC((HWND)ms_handle);
    if (buttonFont->GetInternalFont(the_dc))
      SendMessage((HWND)ms_handle,WM_SETFONT,
		  (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
    ReleaseDC((HWND)ms_handle,the_dc);
  }

  SetSize(x, y, width, height);
  ShowWindow(wx_button, SW_SHOW);

  Callback(Function);

  panel->AdvanceCursor(this);
  return TRUE;
}

Bool wxButton::Create(wxPanel *panel, wxFunction Function, wxBitmap *bitmap,
		   int x, int y, int width, int height,
                   long style, char *name)
{
  if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return Create(panel, Function, "<bad-image>", x, y, width, height, style, name);
  
  bitmap->selectedIntoDC++;
  bm_label = bitmap;

  SetName(name);

  if (panel) panel->AddChild(this);
  buttonFont = panel->buttonFont ;
  labelFont = panel->labelFont ;
  backColour = panel->backColour ;
  labelColour = panel->labelColour ;
  buttonColour = panel->buttonColour ;
  wxWinType = wxTYPE_HWND;
  windowStyle = style;

  wxWnd *cparent = NULL;
  if (panel)
    cparent = (wxWnd *)(panel->handle);

  panel->GetValidPosition(&x, &y);

  windows_id = (int)NewId();

#if FAFA_LIB
  if (width<0)
	 width = bitmap->GetWidth() ;
  if (height<0)
    height = bitmap->GetHeight() ;
  width += FB_MARGIN ;
  height+= FB_MARGIN ;

  HWND wx_button =
    wxwmCreateWindowEx(0, FafaButt, "?", FB_BITMAP | WS_TABSTOP | WS_CHILD,
		       0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
		       wxhInstance, NULL);
  SetBitmapDimensionEx(bitmap->ms_bitmap,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)wx_button, WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
	      (LPARAM)bitmap->ms_bitmap);
#else
  HWND wx_button =
    wxwmCreateWindowEx(0, "wxBUTTON", "not implemented", 
		       BS_PUSHBUTTON | WS_TABSTOP | WS_CHILD,
		       0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
		       wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(wx_button);
#endif
#endif

  ms_handle = (HANDLE)wx_button;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(wx_button);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  SetSize(x, y, width, height);
  ShowWindow(wx_button, SW_SHOW);

  Callback(Function);

  panel->AdvanceCursor(this);
  return TRUE;
}

wxButton::~wxButton(void)
{
  if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label = NULL;
  }
}

void wxButton::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxButton::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxButton::SetButtonColour(wxColour* WXUNUSED(col))
{
}

void wxButton::SetLabel(char *label)
{
  if (bm_label)
      return;

#if FAFA_LIB
    // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
    SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
                (WPARAM)0,
                (LPARAM)NULL);
#endif
  SetWindowText((HWND)ms_handle, label);
}

void wxButton::SetLabel(wxBitmap *bitmap)
{
  if (!bm_label || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label = bitmap;
  bm_label->selectedIntoDC++;

#if FAFA_LIB
    SetBitmapDimensionEx(bitmap->ms_bitmap,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
    SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
                (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
                (LPARAM)bitmap->ms_bitmap);
#endif
}

char *wxButton::GetLabel(void)
{
  GetWindowText((HWND)ms_handle, wxBuffer, 300);
  return wxBuffer;
}

void wxButton::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  HWND button = (HWND)ms_handle;

  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  int actualWidth = width;
  int actualHeight = height;
  int ww, hh;
  GetSize(&ww, &hh);

  float current_width;
  float cyf;
  char buf[300];
  GetWindowText(button, buf, 300);
  GetTextExtent(buf, &current_width, &cyf,NULL,NULL,buttonFont);

  // If we're prepared to use the existing width, then...
  if (width == -1 && ((sizeFlags & wxSIZE_AUTO_WIDTH) != wxSIZE_AUTO_WIDTH))
    actualWidth = ww;
  else if (width == -1)
  {
    int cx;
    int cy;
    wxGetCharSize(button, &cx, &cy,buttonFont);
    actualWidth = (int)(current_width + 3*cx) ;
  }
  
  // If we're prepared to use the existing height, then...
  if (height == -1 && ((sizeFlags & wxSIZE_AUTO_HEIGHT) != wxSIZE_AUTO_HEIGHT))
    actualHeight = hh;
  else if (height == -1)
  {
    actualHeight = (int)(cyf*BUTTON_HEIGHT_FACTOR) ;
  }

  MoveWindow(button, x, y, actualWidth, actualHeight, TRUE);

  if (!((width == -1) && (height == -1)))
    GetEventHandler()->OnSize(width, height);
}

void wxButton::SetFocus(void)
{
/*
  wxPanel *panel = (wxPanel *)GetParent();
  if (panel)
  {
    wxWnd *wnd = (wxWnd *)panel->handle;
    SendMessage(wnd->handle, DM_SETDEFID, windows_id, 0L);
  }
*/
  wxItem::SetFocus();
}

void wxButton::SetDefault(void)
{
  wxPanel *panel = (wxPanel *)GetParent();
  if (panel)
    panel->defaultItem = this;

  if (panel)
  {
    wxWnd *wnd = (wxWnd *)panel->handle;
    SendMessage(wnd->handle, DM_SETDEFID, windows_id, 0L);
  }
}
