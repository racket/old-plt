/*
 * File:	wx_check.cc
 * Purpose:	Check box implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

BOOL wxCheckBox::MSWCommand(UINT WXUNUSED(param), WORD WXUNUSED(id))
{
  wxCommandEvent *event;
  event = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);
  ProcessCommand(event);
  return TRUE;
}

// Single check box item
wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction func, char *Title,
                       int x, int y, int width, int height, long style, char *name):
  wxbCheckBox(panel, func, Title, x, y, width, height, style, name)
{
  Create(panel, func, Title, NULL, x, y, width, height, style, name);
}

wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
                       int x, int y, int width, int height, long style, char *name):
  wxbCheckBox(panel, func, bitmap, x, y, width, height, style, name)
{
  Create(panel, func, NULL, bitmap, x, y, width, height, style, name);
}

Bool wxCheckBox::Create(wxPanel *panel, wxFunction func, char *Title, wxBitmap *bitmap,
                       int x, int y, int width, int height, long style, char *name)
{
  if (bitmap) {
    if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
      return Create(panel, func, "<bad-image>", NULL, x, y, width, height, style, name);
    
    bitmap->selectedIntoDC++;
    bm_label = bitmap;
  } else if (!Title)
    Title = " "; // Apparently needed or checkbox won't show

  SetName(name);
  panel->AddChild(this);
  wxWinType = wxTYPE_HWND;
  windowStyle = style;
  wxWnd *cparent = NULL;
  cparent = (wxWnd *)panel->handle;

  panel->GetValidPosition(&x, &y);

  windows_id = (int)NewId(this);

  HWND wx_button;

  if (bitmap) {
    isFafa = TRUE;
    if (width < 0)
      width = bitmap->GetWidth();
    if (height < 0)
      height = bitmap->GetHeight();
    checkWidth = width;
    checkHeight = height;
    width += FB_MARGIN;
    height += FB_MARGIN;

    wx_button = wxwmCreateWindowEx(0, FafaChck, "toggle",
				   BITCHECK_FLAGS | WS_CLIPSIBLINGS,
				   0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				   wxhInstance, NULL);
    SetBitmapDimensionEx(bitmap->ms_bitmap,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
    SendMessage((HWND)wx_button,WM_CHANGEBITMAP,
		(WPARAM)0xFFFF,
		(LPARAM)bitmap->ms_bitmap);
  } else {
    isFafa = FALSE;
    checkWidth = -1;
    checkHeight = -1;
    wx_button = wxwmCreateWindowEx(0, CHECK_CLASS, Title,
				   CHECK_FLAGS | WS_CLIPSIBLINGS,
				   0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				   wxhInstance, NULL);
#if CTL3D
    Ctl3dSubclassCtl(wx_button);
#endif
  }

  SubclassControl(wx_button);

  ms_handle = (HANDLE)wx_button;

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (labelFont && labelFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)labelFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  SetSize(x, y, width, height);

  ShowWindow(wx_button, SW_SHOW);
  panel->AdvanceCursor(this);
  Callback(func);
  return TRUE;
}

wxCheckBox::~wxCheckBox(void)
{
 if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label = NULL;
  }
}

void wxCheckBox::SetLabel(char *label)
{
  if (bm_label)
    return;

  checkWidth = checkHeight = -1 ;
  // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
	      (WPARAM)0,
	      (LPARAM)NULL);

  SetWindowText((HWND)ms_handle, label);
}

char *wxCheckBox::GetLabel()
{
  char buf[300];
  GetWindowText((HWND)ms_handle, buf, 300);
  return copystring(buf);
}

void wxCheckBox::SetLabel(wxBitmap *bitmap)
{
  if (!bm_label || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label = bitmap;
  bm_label->selectedIntoDC++;

  checkWidth = bitmap->GetWidth() ;
  checkHeight = bitmap->GetHeight() ;
  SetBitmapDimensionEx(bitmap->ms_bitmap,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF,
	      (LPARAM)bitmap->ms_bitmap);
}

void wxCheckBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  char buf[300];

  float current_width;

  int cx;
  int cy;
  float cyf;

  HWND button = (HWND)ms_handle;
  if (checkWidth < 0) {
    wxGetCharSize(button, &cx, &cy, labelFont);

    GetWindowText(button, buf, 300);
    GetTextExtent(wxStripMenuCodes(buf), &current_width, &cyf,NULL,NULL,labelFont);
    if (width < 0)
      width = (int)(current_width + RADIO_SIZE);
    if (height<0)
      height = (int)(cyf);
  } else {
    if (width<0)
      width = checkWidth + FB_MARGIN;
    if (height<0)
      height = checkHeight + FB_MARGIN;
  }

  MoveWindow(button, x, y, width, height, TRUE);

  OnSize(width, height);
}


void wxCheckBox::SetValue(Bool val)
{
  SendMessage((HWND)ms_handle, isFafa ? FAFA_SETCHECK : BM_SETCHECK, val, 0);
}

Bool wxCheckBox::GetValue(void)
{
  return (Bool)(0x003 & SendMessage((HWND)ms_handle,
				    isFafa ? FAFA_GETCHECK : BM_GETCHECK, 
				    0, 0));
}
