/*
 * File:	wx_check.cc
 * Purpose:	Check box implementation
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
#include "wx_check.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

BOOL wxCheckBox::MSWCommand(UINT WXUNUSED(param), WORD WXUNUSED(id))
{
  wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);
  ProcessCommand(*event);
  return TRUE;
}

IMPLEMENT_DYNAMIC_CLASS(wxCheckBox, wxItem)

// Single check box item
wxCheckBox::wxCheckBox(void)
{
  wxWinType = wxTYPE_HWND;
  windows_id = 0;
  ms_handle = 0;
  isFafa = CHECK_IS_FAFA ;
}

// Single check box item
wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction func, char *Title,
                       int x, int y, int width, int height, long style, char *name):
  wxbCheckBox(panel, func, Title, x, y, width, height, style, name)
{
  Create(panel, func, Title, x, y, width, height, style, name);
}

wxCheckBox::wxCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
                       int x, int y, int width, int height, long style, char *name):
  wxbCheckBox(panel, func, bitmap, x, y, width, height, style, name)
{
  Create(panel, func, bitmap, x, y, width, height, style, name);
}

// Single check box item
Bool wxCheckBox::Create(wxPanel *panel, wxFunction func, char *Title,
                       int x, int y, int width, int height, long style, char *name)
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
    cparent = (wxWnd *)panel->handle;

  if (!Title)
    Title = " "; // Apparently needed or checkbox won't show

  panel->GetValidPosition(&x, &y);

  windows_id = (int)NewId();

  isFafa = CHECK_IS_FAFA ;
  checkWidth = -1 ;
  checkHeight = -1 ;
  HWND wx_button = wxwmCreateWindowEx(0, CHECK_CLASS, Title,
                    CHECK_FLAGS,
                    0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
                    wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(wx_button);
#endif

  ms_handle = (HANDLE)wx_button;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(wx_button);
  
  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
/*
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
*/
  // I think on reflection the font for a checkbox should be the
  // label font, because it _is_ effectively a label, not a button.
  // N'est ce pas? -- JACS 2/1/95
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

Bool wxCheckBox::Create(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
                       int x, int y, int width, int height, long style, char *name)
{
  if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return Create(panel, func, "<bad-image>", x, y, width, height, style, name);
  
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
    cparent = (wxWnd *)panel->handle;

  panel->GetValidPosition(&x, &y);

  windows_id = (int)NewId();

#if FAFA_LIB // && !CTL3D
  if (width<0)
    width = bitmap->GetWidth() ;
  if (height<0)
    height = bitmap->GetHeight() ;
  checkWidth = width ;
  checkHeight = height ;
  width += FB_MARGIN ;
  height += FB_MARGIN ;
  HWND wx_button = wxwmCreateWindowEx(0, FafaChck, "toggle",
                    BITCHECK_FLAGS,
                    0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
                    wxhInstance, NULL);
      SetBitmapDimensionEx(bitmap->ms_bitmap,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
      SendMessage((HWND)wx_button,WM_CHANGEBITMAP,
                  (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
                  (LPARAM)bitmap->ms_bitmap);
  isFafa = TRUE;
#else
  isFafa = CHECK_IS_FAFA;
  checkWidth = -1 ;
  checkHeight = -1 ;
  HWND wx_button = wxwmCreateWindowEx(0, CHECK_CLASS, "toggle",
                    CHECK_FLAGS,
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
/*
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
*/
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

void wxCheckBox::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxCheckBox::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxCheckBox::SetButtonColour(wxColour* WXUNUSED(col))
{
}

void wxCheckBox::SetLabel(char *label)
{
  if (bm_label)
    return;

#if FAFA_LIB && !CTL3D
    checkWidth = checkHeight = -1 ;
    // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
    SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
                (WPARAM)0,
                (LPARAM)NULL);
#endif
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

#if FAFA_LIB // && !CTL3D
    checkWidth = bitmap->GetWidth() ;
    checkHeight = bitmap->GetHeight() ;
    SetBitmapDimensionEx(bitmap->ms_bitmap,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
    SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
                (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
                (LPARAM)bitmap->ms_bitmap);
#endif
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
#if FAFA_LIB && !CTL3D
  if (checkWidth<0)
#endif
  {
    wxGetCharSize(button, &cx, &cy, labelFont);

    GetWindowText(button, buf, 300);
    GetTextExtent(wxStripMenuCodes(buf), &current_width, &cyf,NULL,NULL,labelFont);
    if (width < 0)
      width = (int)(current_width + RADIO_SIZE) ;
    if (height<0)
      height = (int)(cyf) ;
  }
#if FAFA_LIB && !CTL3D
  else
  {
    if (width<0)
      width = checkWidth + FB_MARGIN ;
    if (height<0)
      height = checkHeight + FB_MARGIN ;
  }
#endif
  MoveWindow(button, x, y, width, height, TRUE);

  GetEventHandler()->OnSize(width, height);
}


void wxCheckBox::SetValue(Bool val)
{
/*
// Following necessary for Win32s, because Win32s translate BM_SETCHECK
#if FAFA_LIB && !CTL3D
  SendMessage((HWND)ms_handle, FAFA_SETCHECK, val, 0);
#else
  SendMessage((HWND)ms_handle, BM_SETCHECK, val, 0);
#endif
*/
#if FAFA_LIB
  SendMessage((HWND)ms_handle, isFafa?FAFA_SETCHECK:BM_SETCHECK, val, 0);
#else
  SendMessage((HWND)ms_handle, BM_SETCHECK, val, 0);
#endif
}

Bool wxCheckBox::GetValue(void)
{
/*
// Following necessary for Win32s, because Win32s translate BM_SETCHECK
#if FAFA_LIB && !CTL3D
  return (Bool)(0x003 & SendMessage((HWND)ms_handle, FAFA_GETCHECK, 0, 0));
#else
  return (Bool)(0x003 & SendMessage((HWND)ms_handle, BM_GETCHECK, 0, 0));
#endif
*/
#if FAFA_LIB
  return (Bool)(0x003 & SendMessage((HWND)ms_handle,
                isFafa?FAFA_GETCHECK:BM_GETCHECK, 0, 0));
#else
  return (Bool)(0x003 & SendMessage((HWND)ms_handle, BM_GETCHECK, 0, 0));
#endif
}
