/*
 * File:	wx_item.cc
 * Purpose:	Panel item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

// The MakeProcInstance version of the function
FARPROC wxGenericControlSubClassProc = 0;

wxNonlockingHashTable *wxControlHandleList = NULL;
wxNonlockingHashTable *wxItemIdList = NULL;

extern int wx_choice_dropped;

extern void wxEntered(wxWindow *w, int x, int y, int flags);

extern HCURSOR wxMSWSetCursor(HCURSOR c);

extern long last_msg_time; /* MATTHEW: timeStamp implementation */

long NewId(wxItem *item)
{
  WORD id;

  if (!wxItemIdList) {
    wxREGGLOB(wxItemIdList);
    wxItemIdList = new wxNonlockingHashTable;
  }

  do {
    id = (WORD)rand();
  } while (wxItemIdList->Get((long)id));

  wxItemIdList->Put(id, item);

  return id;
}

void DoneIds(wxItem *item)
{
  if (wxItemIdList)
    wxItemIdList->DeleteObject(item);
}

// Item members
wxItem::wxItem(wxPanel *pnl) : wxbItem(pnl)
{
  isFafa = FALSE ;
  oldWndProc = 0;
  isBeingDeleted = FALSE;
}

wxItem::~wxItem(void)
{
  DoneIds(this);

  isBeingDeleted = TRUE;
  
  // item may be a menu, so check.
  wxObject *obj = (wxObject *)GetParent();
  if (!obj || !wxSubType(obj->__type, wxTYPE_PANEL)) return;

  // If we delete an item, we should initialize the parent panel,
  // because it could now be invalid.
  wxPanel *panel = (wxPanel *)GetParent();
  if (panel)
  {
    panel->last_created = NULL;
    panel->cursor_x = PANEL_LEFT_MARGIN;
    panel->cursor_y = PANEL_TOP_MARGIN;
    panel->max_height = 0;
    panel->max_line_height = 0;
    panel->max_width = 0;
    panel->hSpacing = PANEL_HSPACING;
    panel->vSpacing = PANEL_VSPACING;
    panel->initial_hspacing = panel->hSpacing ;
    panel->initial_vspacing = panel->vSpacing ;
    panel->current_hspacing = panel->hSpacing ;
    panel->current_vspacing = panel->vSpacing ;

    panel->new_line = FALSE;
    panel->label_position = wxHORIZONTAL;
    panel->has_child = FALSE ;
    panel->last_created = 0 ;
  }
 
  UnsubclassControl((HWND)ms_handle);
}

void wxItem::GetSize(int *width, int *height)
{
  HWND wnd = (HWND)ms_handle;
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize(wnd, &rect);

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxItem::GetPosition(int *x, int *y)
{
  HWND wnd = (HWND)ms_handle;
  wxWindow *parent = GetParent();
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize(wnd, &rect);

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

void wxItem::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  wxWindow::SetSize(x, y, width, height, sizeFlags);
}

void wxItem::SetClientSize(int width, int height)
{
  SetSize(-1, -1, width, height);
}

void wxItem::SetLabel(char *label)
{
  SetWindowText((HWND)ms_handle, label);
}

char *wxItem::GetLabel(void)
{
  GetWindowText((HWND)ms_handle, wxBuffer, 1000);
  return wxBuffer;
}

void wxItem::SetFocus(void)
{
  wxWindow::SetFocus();
}

Bool wxItem::Show(Bool show)
{
  HWND wnd = (HWND)ms_handle;
  int cshow;

  SetShown(show);

  window_parent->GetChildren()->Show(this, show);

  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
  ShowWindow(wnd, (BOOL)cshow);
  if (show)
    BringWindowToTop(wnd);
  return TRUE;
}

void wxItem::SubclassControl(HWND hWnd)
{
  // Subclass again for purposes of dialog editing mode
  wxAddControlHandle(hWnd, this);
  oldWndProc = (FARPROC) GetWindowLong(hWnd, GWL_WNDPROC);
  if (!wxGenericControlSubClassProc)
    wxGenericControlSubClassProc = MakeProcInstance((FARPROC)wxSubclassedGenericControlProc,
						    wxhInstance);
  SetWindowLong(hWnd, GWL_WNDPROC, (LONG)wxGenericControlSubClassProc);
}

void wxItem::UnsubclassControl(HWND hWnd)
{
  if (oldWndProc) {
    wxRemoveControlHandle(hWnd);
    SetWindowLong(hWnd, GWL_WNDPROC, (LONG)oldWndProc);
  }
}

// Call this repeatedly for several wnds to find the overall size
// of the widget.
// Call it initially with -1 for all values in rect.
// Keep calling for other widgets, and rect will be modified
// to calculate largest bounding rectangle.
void wxFindMaxSize(HWND wnd, RECT *rect)
{
  int left = rect->left;
  int right = rect->right;
  int top = rect->top;
  int bottom = rect->bottom;

  GetWindowRect(wnd, rect);

  if (left < 0)
    return;

  if (left < rect->left)
    rect->left = left;

  if (right > rect->right)
    rect->right = right;

  if (top < rect->top)
    rect->top = top;

  if (bottom > rect->bottom)
    rect->bottom = bottom;

}

static int skip_next_return;

int wxDoItemPres(wxItem *item, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam,
		 long *result)
{
  *result = 0;
  
  // If not in edit mode (or has been removed from parent), call the default proc.
  wxPanel *panel = (wxPanel *)item->GetParent();
  
  if (panel && !item->isBeingDeleted) {
    /* Check PreOnChar or PreOnEvent */
    switch (message) {
    case WM_NCHITTEST:
      /* For messages, override hittest to claim it's always in the client area */
      if (wxSubType(item->__type, wxTYPE_MESSAGE)) {
	*result = HTCLIENT;
	return FALSE;
      }
      break;
    case WM_SETFOCUS:
      {
	if (item->IsShownTree()) {
	  wxWindow *p = item->GetTopLevel();
	  p->focusWindow = item;
	  
	  item->OnSetFocus();
	}
      }
      break;
    case WM_KILLFOCUS:
      item->OnKillFocus();
      break;
    case WM_MOUSEMOVE:
      if (!wxIsBusy()) {
	/* Set local cursor */
	wxWindow *w = item;
	while (w) {
	  if (w->wx_cursor) {
	    wxMSWSetCursor(w->wx_cursor->ms_cursor);
	    break;
	  }
	  w = w->GetParent();
	}
      }
    case WM_RBUTTONDOWN: /** ^^ move falls though ^^ */
    case WM_RBUTTONUP:
    case WM_RBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_LBUTTONDBLCLK:
      {
	int et;
	switch(message) {
        case WM_RBUTTONDOWN:
	  et = wxEVENT_TYPE_RIGHT_DOWN; break;
        case WM_RBUTTONUP:
	  et = wxEVENT_TYPE_RIGHT_UP; break;
        case WM_RBUTTONDBLCLK:
	  et = wxEVENT_TYPE_RIGHT_DCLICK; break;    
        case WM_MBUTTONDOWN:
	  et = wxEVENT_TYPE_MIDDLE_DOWN; break;
        case WM_MBUTTONUP:
	  et = wxEVENT_TYPE_MIDDLE_UP; break;
        case WM_MBUTTONDBLCLK:
	  et = wxEVENT_TYPE_MIDDLE_DCLICK; break;
        case WM_LBUTTONDOWN:
	  et = wxEVENT_TYPE_LEFT_DOWN; break;
        case WM_LBUTTONUP:
	  et = wxEVENT_TYPE_LEFT_UP; break;
        case WM_LBUTTONDBLCLK:
	  et = wxEVENT_TYPE_LEFT_DCLICK; break;
	case WM_MOUSEMOVE:
	  et = wxEVENT_TYPE_MOTION; break;
	}
	
	int x = (int)LOWORD(lParam);
	int y = (int)HIWORD(lParam);
	int flags = wParam;
	
	wxMouseEvent *_event = new wxMouseEvent(et);
	wxMouseEvent &event = *_event;
	
	event.x = (float)x;
	event.y = (float)y;
	
	event.shiftDown = (flags & MK_SHIFT);
	event.controlDown = (flags & MK_CONTROL);
	event.leftDown = (flags & MK_LBUTTON);
	event.middleDown = (flags & MK_MBUTTON);
	event.rightDown = (flags & MK_RBUTTON);
	event.SetTimestamp(last_msg_time); /* MATTHEW: timeStamp */

	wxEntered(item, x, y, wParam);

	if (item->CallPreOnEvent(item, &event))
	  return 0;
      }

      break;

    case WM_SYSKEYDOWN:
      if ((wParam == VK_MENU) || (wParam == VK_F4)) { /* F4 is close */
	return 1;
      }
    case WM_KEYDOWN:  /* ^^^ fallthrough */
      if (!((wParam != VK_ESCAPE) 
	    && (wParam != VK_SPACE) 
	    && (wParam != VK_RETURN)
	    && (wParam != VK_TAB)
	    && (wParam != VK_DELETE))) {
	/* Don't call pre-on-char for a ENTER press when
	   a choice menu is dropped-down */
	if (wx_choice_dropped)
	  if (wParam == VK_RETURN) {
	    skip_next_return = 1;
	    return 1;
	  }
	
	/* Otherwise, already covered by WM_CHAR */
	return 0;
      }

    case WM_SYSCHAR: /* ^^^ fallthrough */
      if (message == WM_SYSCHAR) {
	if (wParam == VK_MENU)
	  return 1;
      }
    case WM_CHAR:  /* ^^^ fallthrough */
      {
	int id = wParam;
	int tempControlDown = FALSE;

	if ((message == WM_CHAR) || (message == WM_SYSCHAR)) {
	  if ((id > 0) && (id < 27)) {
	    switch (id) {
	    case 13:
	      if (skip_next_return) {
		skip_next_return = 0;
		return 0; /* Return already consumes to close popup */
	      }
	      id = WXK_RETURN;
	      break;
	    case 8:
	      id = WXK_BACK;
	      break;
	    case 9:
	      id = WXK_TAB;
	      break;
	    default:
	      tempControlDown = TRUE;
	      id = id + 96;
	    } 
	  }
	} else
	  if ((id = wxCharCodeMSWToWX(wParam)) == 0)
	    id = -1;

	if (id >= 0) {
	  wxKeyEvent *_event = new wxKeyEvent(wxEVENT_TYPE_CHAR);
	  wxKeyEvent &event = *_event;
	  
	  if (::GetKeyState(VK_SHIFT) >> 1)
	    event.shiftDown = TRUE;
	  if (tempControlDown || (::GetKeyState(VK_CONTROL) >> 1))
	    event.controlDown = TRUE;
	  if ((HIWORD(lParam) & KF_ALTDOWN) == KF_ALTDOWN)
	    event.metaDown = TRUE;
	  
	  event.keyCode = id;
	  event.SetTimestamp(last_msg_time); /* MATTHEW: timeStamp */
	  
	  POINT pt;
	  GetCursorPos(&pt);
	  RECT rect;
	  GetWindowRect((HWND)item->handle,&rect);
	  pt.x -= rect.left;
	  pt.y -= rect.top;
	  event.x = (float)pt.x;
	  event.y = (float)pt.y;

	  /* Don't call pre-on-char for a ENTER press when
	     a choice menu is dropped-down */
	  if (wx_choice_dropped)
	    if (event.keyCode == 13)
	      return 1;

	  if (item->CallPreOnChar(item, &event))
	    return 0;
	  else if (event.metaDown)
	    return 0;
	}
      }
    }
  }

  return 1;
}

// Sub-classed generic control proc
LONG APIENTRY _EXPORT
  wxSubclassedGenericControlProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT res;

  /* See mredmsw.cxx: */
  if (wxEventTrampoline(hWnd, message, wParam, lParam, &res, wxSubclassedGenericControlProc))
    return res;

  if (message == WM_GETDLGCODE)
    return DLGC_WANTMESSAGE;

  wxItem *item = wxFindControlFromHandle(hWnd);

  if (!item) {
    wxDebugMsg("Panic! Cannot find wxItem for this HWND in wxSubclassedGenericControlProc.\n");
    return NULL;
  }

  long r;
  if (!wxDoItemPres(item, hWnd, message, wParam, lParam, &r))
    return r;

  return CallWindowProc((WNDPROC)item->oldWndProc, hWnd, message, wParam, lParam);
}

wxItem *wxFindControlFromHandle(HWND hWnd)
{
  if (!wxControlHandleList)
    return NULL;

  return (wxItem *)wxControlHandleList->Find((long)hWnd);
}

void wxAddControlHandle(HWND hWnd, wxItem *item)
{
  if (!wxControlHandleList) {
    wxREGGLOB(wxControlHandleList);
    wxControlHandleList = new wxNonlockingHashTable;
  }
  wxControlHandleList->Append((long)hWnd, item);
}

void wxRemoveControlHandle(HWND hWnd)
{
  wxControlHandleList->Delete((long)hWnd);
}

