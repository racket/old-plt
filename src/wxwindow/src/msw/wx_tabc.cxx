/*
 * File:	wx_tabc.cc
 * Purpose:	Tab choice implementation
 * Author:	Matthew
 * Created:	2002
 * Copyright:	(c) 2002, PLT
 */

#include "wx.h"

#include "wx_tabc.h"

#include <commctrl.h>

BOOL wxTabChoice::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == 64985 /* (UINT)TCN_SELCHANGE  ? */) {
    wxCommandEvent *event;
    event = new wxCommandEvent(wxEVENT_TYPE_TAB_CHOICE_COMMAND);
    ProcessCommand(event);
    return TRUE;
  } else
    return FALSE;
}


wxTabChoice::wxTabChoice(wxPanel *panel, wxFunction func, char *label,
			 int n, char **choices, int style)
  : wxItem(panel)
{
  int x = 0, y = 0, i;
  wxWnd *cparent = NULL;
  TCITEM tie;
  INITCOMMONCONTROLSEX icex;
  HWND hwndTab;
  int width, height, nid;
  RECT prc;

  __type = wxTYPE_TAB_CHOICE;

  icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icex.dwICC  = ICC_TAB_CLASSES;
  InitCommonControlsEx(&icex);

  panel->AddChild(this);
  wxWinType = wxTYPE_HWND;
  windowStyle = (style & wxBORDER);
  cparent = (wxWnd *)panel->handle;

  panel->GetValidPosition(&x, &y);

  windows_id = NewId(this);
  
  {
    int cx, cy;
    float current_width, cyf, total_width = 0;

    wxGetCharSize(cparent->handle, &cx, &cy, buttonFont);
    
    for (i = 0; i < n; i++) {
      GetTextExtent(wxStripMenuCodes(choices[i]), &current_width, &cyf, NULL, NULL, buttonFont);
      if (current_width < 40)
	current_width = 40;
      total_width += current_width + cy;
    }

    width = (int)total_width;
    height = 2 * cy;
  }

  hwndTab = CreateWindow(WC_TABCONTROL, "", 
			 WS_CHILD | WS_CLIPSIBLINGS,
			 0, 0, width ? width : 40, height,
			 cparent->handle, (HMENU)windows_id, wxhInstance, NULL);

  nid = NewId(this);

  /* For comctl32 version6, makes the panel background gray: */
  bgStatic = wxwmCreateWindowEx(0, STATIC_CLASS, "",
				STATIC_FLAGS | WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE,
				0, 0, 200, 200, hwndTab, (HMENU)nid,
				wxhInstance, NULL);

  tie.mask = TCIF_TEXT;
 
  if (n) {
    for (i = 0; i < n; i++) { 
      tie.pszText = choices[i];
      TabCtrl_InsertItem(hwndTab, i, &tie);
    } 
  } else {
    /* for height-meausing purposes, add one: */
    tie.pszText = "Dummy";
    TabCtrl_InsertItem(hwndTab, 0, &tie);
  }
    

  SubclassControl(hwndTab);

  ms_handle = (HANDLE)hwndTab;

  wxSetWinFont(buttonFont, ms_handle);

  prc.left = prc.top = prc.right = prc.bottom = 0;
  TabCtrl_AdjustRect(hwndTab, TRUE, &prc);
    
  if (!n)
    Delete(0);

  SetSize(x, y, width, prc.bottom - prc.top);

  if (style & wxINVISIBLE)
    Show(FALSE);
  else
    ShowWindow(hwndTab, SW_SHOW);

  panel->AdvanceCursor(this);
  Callback(func);
}

wxTabChoice::~wxTabChoice()
{
  wxwmDestroyWindow(bgStatic);
}

int wxTabChoice::GetSelection(void) {
  return TabCtrl_GetCurSel((HWND)ms_handle);
}

int wxTabChoice::Number(void) { 
  return TabCtrl_GetItemCount((HWND)ms_handle);
}

void wxTabChoice::SetSelection(int n) { 
  if ((n >= 0) && (n < Number()))
    TabCtrl_SetCurSel((HWND)ms_handle, n);
}

void wxTabChoice::Enable(Bool enable) { 
  wxItem::Enable(enable);
}

void wxTabChoice::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  if (width < 0)
    GetSize(&width, &height);

  if (!(windowStyle & wxBORDER)) {
    x -= 2;
    width += 4;
    height += 4;
  }

  if (!orig_height)
    orig_height = height;

  MoveWindow((HWND)ms_handle, x, y, width, height, TRUE);
  MoveWindow((HWND)bgStatic, 2, orig_height-4, width-5, height-orig_height, TRUE);

  OnSize(width, height);
}

void wxTabChoice::Append(char *s)
{
  TCITEM tie;
  int shownhide = 0, nv;

  /* The control misupdates when going from 0 to non-zero
     tabs. Hide before making the transition, then show
     after. */
  if (!Number() && IsShown())
    shownhide = 1;

  if (shownhide)
    ShowWindow((HWND)ms_handle, SW_HIDE);

  tie.mask = TCIF_TEXT;
  tie.pszText = s;
  nv = Number();
  TabCtrl_InsertItem((HWND)ms_handle, nv, &tie);

  if (shownhide)
    ShowWindow((HWND)ms_handle, SW_SHOW);
}

void wxTabChoice::Delete(int i)
{
  if ((i >= 0) && (i < Number()))
    TabCtrl_DeleteItem((HWND)ms_handle, i);
}

Bool wxTabChoice::Show(Bool show) 
{
  wxWindow::Show(show);
  return TRUE;
}
