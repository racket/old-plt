/*
 * File:	wx_lbox.cc
 * Purpose:	List box implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_panel.h"
#include "wx_lbox.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

BOOL wxListBox::MSWCommand(UINT param, WORD WXUNUSED(id))
{
  if (param == LBN_SELCHANGE) {
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
    ProcessCommand(*event);
    return TRUE;
  } else if (param == LBN_DBLCLK) {
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND);
    ProcessCommand(*event);
    return TRUE;
  }

  return FALSE;
}

// Listbox item
IMPLEMENT_DYNAMIC_CLASS(wxListBox, wxItem)

wxListBox::wxListBox(void)
{
  wxWinType = wxTYPE_HWND;
  static_label = NULL;
  windows_id = 0;
  no_items = 0;
  ms_handle = 0;
}

wxListBox::wxListBox(wxPanel *panel, wxFunction func,
                       char *Title, Bool Multiple,
                       int x, int y, int width, int height,
                       int N, char **Choices, long style, char *name):
  wxbListBox(panel, func, Title, Multiple, x, y, width, height, N, Choices,
             style, name)
{
  Create(panel, func, Title, Multiple, x, y, width, height, N, Choices,
         style, name);
}

Bool wxListBox::Create(wxPanel *panel, wxFunction func,
                       char *Title, Bool Multiple,
                       int x, int y, int width, int height,
                       int N, char **Choices, long style, char *name)
{
  SetName(name);
  if (panel) panel->AddChild(this);
  buttonFont = panel->buttonFont ;
  labelFont = panel->labelFont ;
  backColour = panel->backColour ;
  labelColour = panel->labelColour ;
  buttonColour = panel->buttonColour ;
  multiple = Multiple & wxMULTIPLE_MASK;
  wxWinType = wxTYPE_HWND;
  windowStyle = style;
  wxWnd *cparent = NULL;
  if (panel)
    cparent = (wxWnd *)(panel->handle);

  labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

  char *the_label = NULL ;

  if (Title)
  {
    the_label = new char[strlen(Title)+1] ;
    if (style&wxFIXED_LENGTH)
    {
      int i;
      for (i=0;i<(int)strlen(Title);i++)
        the_label[i]=MEANING_CHARACTER ;
    }
    else
      strcpy(the_label,Title) ;
    the_label[strlen(Title)] = '\0' ;
  }

  // If label exists, create a static control for it.
  if (Title) {
    static_label = wxwmCreateWindowEx(0, STATIC_CLASS, the_label,
				      STATIC_FLAGS,
				      0, 0, 0, 0, cparent->handle, (HMENU)NewId(this),
				      wxhInstance, NULL);
#if CTL3D
    Ctl3dSubclassCtl(static_label);
#endif
    HDC the_dc = GetWindowDC(static_label) ;
    if (labelFont && labelFont->GetInternalFont(the_dc))
      SendMessage(static_label,WM_SETFONT,
                  (WPARAM)labelFont->GetInternalFont(the_dc),0L);
    ReleaseDC(static_label,the_dc) ;

  }
  else
    static_label = NULL;


  DWORD wstyle;
  // Windows sense of MULTIPLE & EXTENDED is backwards from ours.
  if (multiple == wxEXTENDED)
    wstyle = WS_VSCROLL | WS_BORDER | LBS_MULTIPLESEL | LBS_NOTIFY | WS_TABSTOP;
  else if (multiple == wxMULTIPLE)
    wstyle = WS_VSCROLL | WS_BORDER | LBS_EXTENDEDSEL | LBS_NOTIFY | WS_TABSTOP ;
  else
    wstyle = WS_VSCROLL | WS_BORDER | LBS_NOTIFY | WS_TABSTOP;
  if ((Multiple&wxALWAYS_SB) || (style & wxALWAYS_SB))
    wstyle |= LBS_DISABLENOSCROLL ;
  if (style & wxHSCROLL)
    wstyle |= WS_HSCROLL;

  windows_id = (int)NewId(this);

  HWND wx_list = wxwmCreateWindowEx(0, "wxLISTBOX", NULL,
				    wstyle | WS_CHILD,
				    0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
				    wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(wx_list);
#endif

  int i;

  user_data = new char*[N];
  for (i = 0; i < N; i++)
    user_data[i] = NULL;

  for (i = 0; i < N; i++)
    SendMessage(wx_list, LB_ADDSTRING, 0, (LONG)Choices[i]);
  if (!Multiple)
    SendMessage(wx_list, LB_SETCURSEL, 0, 0);

  ShowWindow(wx_list, SW_SHOW);
  no_items = N;

  ms_handle = (HANDLE)wx_list;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(wx_list);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);
  Callback(func);

  if (Title) {
    if (style&wxFIXED_LENGTH)
      SetLabel(Title);
  }

  return TRUE;
}

wxListBox::~wxListBox(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
    wxwmDestroyWindow(static_label);
  static_label = NULL;
}

void wxListBox::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxListBox::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxListBox::SetButtonColour(wxColour* WXUNUSED(col))
{
}

void wxListBox::SetFirstItem(int N)
{
  SendMessage((HWND)ms_handle,LB_SETTOPINDEX,(WPARAM)N,(LPARAM)0) ;
}

void wxListBox::SetFirstItem(char *s)
{
  int N = FindString(s);

  if (N>=0)
    SetFirstItem(N);
}

int wxListBox::NumberOfVisibleItems(void)
{
  int h = SendMessage((HWND)ms_handle,LB_GETITEMHEIGHT,(WPARAM)0,(LPARAM)0);
  int cw, ch;
  
  GetClientSize(&cw, &ch);
  ch = ch / h;

  return max(ch, 1);
}

int wxListBox::GetFirstItem(void)
{
  return SendMessage((HWND)ms_handle,LB_GETTOPINDEX,(WPARAM)0,(LPARAM)0);
}

void wxListBox::Delete(int N)
{
  int i;

  SendMessage((HWND)ms_handle, LB_DELETESTRING, N, 0);
  no_items --;
  for (i = N; i < no_items; i++) {
    user_data[i] = user_data[i + 1];
  }
  SetHorizontalExtent(NULL);
}

void wxListBox::Append(char *Item)
{
  Append(Item, NULL);
}

void wxListBox::Append(char *Item, char *Client_data)
{
  char **old = user_data;
  int i;

  user_data = new char*[no_items + 1];
  for (i = no_items; i--; )
    user_data[i] = old[i];
  user_data[no_items] = Client_data;

  int index = (int)SendMessage((HWND)ms_handle, LB_ADDSTRING, 0, (LONG)Item);
  no_items++;
  SetHorizontalExtent(Item);
}

void wxListBox::Set(int n, char *choices[])
{
  int i;

  Clear();

  ShowWindow((HWND)ms_handle, SW_HIDE);
  for (i = 0; i < n; i++)
    SendMessage((HWND)ms_handle, LB_ADDSTRING, 0, (LONG)choices[i]);
  no_items = n;
  SetHorizontalExtent(NULL);
  ShowWindow((HWND)ms_handle, SW_SHOW);
}

int wxListBox::FindString(char *s)
{
  int pos = (int)SendMessage((HWND)ms_handle, LB_FINDSTRINGEXACT, -1, (LONG)s);
  if (pos == LB_ERR)
    return -1;
  else
    return pos;
}

void wxListBox::Clear(void)
{
  SendMessage((HWND)ms_handle, LB_RESETCONTENT, 0, 0);

  no_items = 0;
  user_data = NULL;
  SendMessage((HWND)ms_handle, LB_SETHORIZONTALEXTENT, LOWORD(0), 0L);
}

void wxListBox::SetSelection(int N, Bool select)
{
  if ((N < 0) || (N >= Number()))
    return;

  if (multiple != wxSINGLE) {
    SendMessage((HWND)ms_handle, LB_SETSEL, select, N);
  } else {
    N = -1;
    SendMessage((HWND)ms_handle, LB_SETCURSEL, N, 0);
  }
}

Bool wxListBox::Selected(int N)
{
  return (Bool)SendMessage((HWND)ms_handle, LB_GETSEL, N, 0);
}

void wxListBox::Deselect(int N)
{
  SetSelection(N, 0);
}

char *wxListBox::GetClientData(int N)
{
  return user_data[N];
}

void wxListBox::SetClientData(int N, char *Client_data)
{
  user_data[N] = Client_data;
}

// Return number of selections and an array of selected integers
int wxListBox::GetSelections(int **list_selections)
{
  HWND listbox = (HWND)ms_handle;

  if (multiple == wxSINGLE) {
    int sel = (int)SendMessage(listbox, LB_GETCURSEL, 0, 0);
    if (sel == LB_ERR)
      return 0;
    selections = new int[1];
    selections[0] = sel;
    *list_selections = selections;
    return 1;
  } else {
    int no_sel = (int)SendMessage(listbox, LB_GETSELCOUNT, 0, 0);
    if (no_sel == 0)
      return 0;
    selections = new int[no_sel];
    SendMessage(listbox, LB_GETSELITEMS, no_sel, (LONG)selections);
    *list_selections = selections;
    return no_sel;
  }
}

// Get single selection, for single choice list items
int wxListBox::GetSelection(void)
{
  int c, *l;
  c = GetSelections(&l);
  if (!c)
    return -1;
  else
    return l[0];
}

// Find string for position
char *wxListBox::GetString(int N)
{
  /* MATTHEW: [6] Check bounds */
  int len;

  if (N < 0 || N >= no_items)
    return NULL;
  else
    len = (int)SendMessage((HWND)ms_handle, LB_GETTEXT, N, (LONG)wxBuffer);

  wxBuffer[len] = 0;

  return copystring(wxBuffer);
}

void wxListBox::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
    GetSize(&width, &height);

  char buf[300];

  int cx; // button font dimensions
  int cy;
  int clx; // label font dimensions
  int cly;

  wxGetCharSize((HWND)ms_handle, &cx, &cy, buttonFont);

  float label_width, label_height, label_x, label_y;
  float control_width, control_height, control_x, control_y;

  // Deal with default size (using -1 values)
  if (width <= 0)
    width = DEFAULT_ITEM_WIDTH;

  if (height <= 0)
    height = DEFAULT_ITEM_HEIGHT;

  if (static_label) {
    // Find size of label
    wxGetCharSize((HWND)ms_handle, &clx, &cly,labelFont);
    GetWindowText(static_label, buf, 300);
    GetTextExtent(wxStripMenuCodes(buf), &label_width, &label_height, NULL, NULL,labelFont);

    // Given size is total label + edit size, find individual
    // control sizes on that basis.
    if (labelPosition == wxHORIZONTAL) {
      label_x = (float)x;
      label_y = (float)y;
      label_width += (float)clx;

      control_x = label_x + label_width + clx;
      control_y = (float)y;
      control_width = width - (control_x - label_x);
      control_height = (float)height;
    } else { // wxVERTICAL
      label_x = (float)x;
      label_y = (float)y;

      control_x = (float)x;
      control_y = label_y + label_height + 3; // Allow for 3D border
      control_width = (float)width;
      control_height = height - label_height - 3;
    }

    MoveWindow(static_label, (int)label_x, (int)label_y,
               (int)label_width, (int)label_height, TRUE);
  } else {
    control_x = (float)x;
    control_y = (float)y;
    control_width = (float)width;
    control_height = (float)height;
  }

  // Calculations may have made size too small
  if (control_height <= 0)
    control_height = DEFAULT_ITEM_HEIGHT;

  if (control_width <= 0)
    control_width = DEFAULT_ITEM_WIDTH;

  //  wxDebugMsg("About to set the listbox height to %d", (int)control_height);
  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y,
	     (int)control_width, (int)control_height, TRUE);
  
  GetEventHandler()->OnSize(width, height);
}

void wxListBox::GetSize(int *width, int *height)
{
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label) {
    wxFindMaxSize(static_label, &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxListBox::GetPosition(int *x, int *y)
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

char *wxListBox::GetLabel(void)
{
  if (static_label)
  {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  }
  else return NULL;
}

void wxListBox::SetLabel(char *label)
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

    GetTextExtent((LPSTR)label, &w, &h, NULL, NULL,labelFont);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowText(static_label, label);
  }
}

// Windows-specific code to set the horizontal extent of
// the listbox, if necessary. If s is non-NULL, it's
// used to calculate the horizontal extent.
// Otherwise, all strings are used.
void wxListBox::SetHorizontalExtent(char *s)
{
  // Only necessary if we want a horizontal scrollbar
  if (!(windowStyle & wxHSCROLL))
    return;
  TEXTMETRIC lpTextMetric;

  HWND hWnd = GetHWND();

  if (s)
  {
    int existingExtent = (int)SendMessage(hWnd, LB_GETHORIZONTALEXTENT, 0, 0L);
    HDC dc = GetWindowDC(hWnd);
    GetTextMetrics(dc, &lpTextMetric);
    SIZE extentXY;
    ::GetTextExtentPoint(dc, (LPSTR)s, strlen(s), &extentXY);
    int extentX = (int)(extentXY.cx + lpTextMetric.tmAveCharWidth);
    ReleaseDC(hWnd, dc);
    if (extentX > existingExtent)
      SendMessage(hWnd, LB_SETHORIZONTALEXTENT, LOWORD(extentX), 0L);
    return;
  }
  else
  {
    int largestExtent = 0;
    HDC dc = GetWindowDC(hWnd);
    GetTextMetrics(dc, &lpTextMetric);
    int i;
    for (i = 0; i < no_items; i++)
    {
      int len = (int)SendMessage(hWnd, LB_GETTEXT, i, (LONG)wxBuffer);
      wxBuffer[len] = 0;
      SIZE extentXY;
      ::GetTextExtentPoint(dc, (LPSTR)wxBuffer, len, &extentXY);
      int extentX = (int)(extentXY.cx + lpTextMetric.tmAveCharWidth);
      if (extentX > largestExtent)
        largestExtent = extentX;
    }
    ReleaseDC(hWnd, dc);
    SendMessage(hWnd, LB_SETHORIZONTALEXTENT, LOWORD(largestExtent), 0L);
  }
}

void
wxListBox::InsertItems(int nItems, char **Items, int pos)
{
#if 0
  int i;
  for (i = 0; i < nItems; i++)
    SendMessage((HWND)ms_handle, LB_INSERTSTRING, i + pos, (LPARAM)Items[i]);
  no_items += nItems;
  SetHorizontalExtent(NULL);
  no_items += nItems;
#endif
}

void wxListBox::SetString(int N, char *s)
{
  int sel = GetSelection();
  
  char *oldData = (char *)wxListBox::GetClientData(N);
  
  SendMessage((HWND)ms_handle, LB_DELETESTRING, N, 0);

  int newN = N;
  if (N == (no_items - 1))
    newN = -1;
    
  SendMessage((HWND)ms_handle, LB_INSERTSTRING, newN, (LPARAM)s);
  if (oldData)
    wxListBox::SetClientData(N, oldData);

  // Selection may have changed if last one deleted, ugh.
  if (sel == N && newN == -1)
    SetSelection(sel);
}


Bool wxListBox::Show(Bool show)
{
  HWND wnd = (HWND)ms_handle;
  int cshow;

  SetShown(show);
  window_parent->GetChildren()->Show(this, show);
  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;

  ShowWindow(wnd, cshow);

  if (static_label)
	ShowWindow(static_label, cshow);

  return TRUE;
}

void wxListBox::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);

  if (static_label)
    ::EnableWindow(static_label, !gray);
}

