/*
 * File:	wx_item.cc
 * Purpose:	Panel item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_item.cc,v 1.1 1994/08/14 21:59:17 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_panel.h"
#include "wx_privt.h"
#include "wx_itemp.h"
#include "wx_txt.h"

#endif

// The MakeProcInstance version of the function
FARPROC wxGenericControlSubClassProc = 0;

#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
wxList *wxControlHandleList = NULL;
#else
wxNonlockingHashTable *wxControlHandleList = NULL;
#endif

IMPLEMENT_ABSTRACT_CLASS(wxItem, wxWindow)

// Item members
wxItem::wxItem(void)
{
  isFafa = FALSE ;
  oldWndProc = 0;
  mswLastXPos = 0;
  mswLastYPos = 0;
  mswLastEvent = 0;
  isBeingDeleted = FALSE;
}

wxItem::~wxItem(void)
{
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

float wxItem::GetCharHeight(void)
{
  TEXTMETRIC lpTextMetric;
  HWND wnd = (HWND)ms_handle;
  HDC dc = GetDC(wnd);

  GetTextMetrics(dc, &lpTextMetric);
  ReleaseDC(wnd, dc);

  return (float)lpTextMetric.tmHeight;
}

float wxItem::GetCharWidth(void)
{
  TEXTMETRIC lpTextMetric;
  HWND wnd = (HWND)ms_handle;
  HDC dc = GetDC(wnd);

  GetTextMetrics(dc, &lpTextMetric);
  ReleaseDC(wnd, dc);

  return (float)lpTextMetric.tmAveCharWidth;
}

void wxItem::SubclassControl(HWND hWnd)
{
  // Subclass again for purposes of dialog editing mode
  wxAddControlHandle(hWnd, this);
  oldWndProc = (FARPROC) GetWindowLong(hWnd, GWL_WNDPROC);
  if (!wxGenericControlSubClassProc)
    wxGenericControlSubClassProc = MakeProcInstance((FARPROC) wxSubclassedGenericControlProc, wxhInstance);
  SetWindowLong(hWnd, GWL_WNDPROC, (LONG) wxGenericControlSubClassProc);
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

/*
// Not currently used
void wxConvertDialogToPixels(wxWindow *control, int *x, int *y)
{
  if (control->window_parent && control->window_parent->is_dialog)
  {
    DWORD word = GetDialogBaseUnits();
    int xs = LOWORD(word);
    int ys = HIWORD(word);
    *x = (int)(*x * xs/4);
    *y = (int)(*y * ys/8);
  }
  else
  {
    *x = *x;
    *y = *y;
  }
}
*/

// Sub-classed generic control proc
LONG APIENTRY _EXPORT
  wxSubclassedGenericControlProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  wxItem *item = wxFindControlFromHandle(hWnd);

  if (!item)
  {
    wxDebugMsg("Panic! Cannot find wxItem for this HWND in wxSubclassedGenericControlProc.\n");
    return FALSE;
  }

  // If not in edit mode (or has been removed from parent), call the default proc.
  wxPanel *panel = (wxPanel *)item->GetParent();

  if (!panel || item->isBeingDeleted)
    return CallWindowProc(item->oldWndProc, hWnd, message, wParam, lParam);
   
  // Special edit control processing
  if (!panel->GetUserEditMode() && (item->__type == wxTYPE_TEXT))
  {
    switch (message)
    {
      case WM_GETDLGCODE:
      {
        if (item->GetWindowStyleFlag() & wxPROCESS_ENTER)
          return DLGC_WANTALLKEYS;
        break;
      }
      case WM_CHAR: // Always an ASCII character
      {
        if (wParam == VK_RETURN)
        {
          wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_TEXT_ENTER_COMMAND);
          event->commandString = ((wxText *)item)->GetValue();
          event->eventObject = item;
          item->ProcessCommand(*event);
          return FALSE;
        }
      }
      break;
    }
  }

  if (!panel || !panel->GetUserEditMode())
    return CallWindowProc(item->oldWndProc, hWnd, message, wParam, lParam);

  int x = (int)LOWORD(lParam);
  int y = (int)HIWORD(lParam);
  unsigned int flags = wParam;

  // If a mouse message, must convert X and Y to item coordinates
  // from HWND coordinates (the HWND may be part of the composite wxItem)
  if ((message == WM_RBUTTONDOWN) || (message == WM_LBUTTONDOWN) || (message == WM_MBUTTONDOWN) || 
      (message == WM_RBUTTONUP) || (message == WM_LBUTTONUP) || (message == WM_MBUTTONUP) || 
      (message == WM_RBUTTONUP) || (message == WM_LBUTTONUP) || (message == WM_MBUTTONUP) || 
      (message == WM_RBUTTONDBLCLK) || (message == WM_LBUTTONDBLCLK) || (message == WM_MBUTTONDBLCLK) || 
      (message == WM_RBUTTONDBLCLK) || (message == WM_LBUTTONDBLCLK) || (message == WM_MBUTTONDBLCLK) || 
      (message == WM_MOUSEMOVE))
  {
    int ix, iy;
    item->GetPosition(&ix, &iy);
    RECT rect;
    GetWindowRect(hWnd, &rect);

    // Since we now have the absolute screen coords,
    // convert to panel coordinates.
    POINT point;
    point.x = rect.left;
    point.y = rect.top;
    ::ScreenToClient(panel->GetHWND(), &point);

    x += (point.x - ix);
    y += (point.y - iy);
  }

  switch (message)
  {
    case WM_KILLFOCUS:
      item->GetEventHandler()->OnSetFocus();
      break;
    case WM_SETFOCUS:
      item->GetEventHandler()->OnKillFocus();
      break;
    case WM_RBUTTONDOWN:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_RIGHT_DOWN);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_RIGHT_DOWN;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_RBUTTONUP:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_RIGHT_UP);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_RIGHT_UP;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_RBUTTONDBLCLK:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_RIGHT_DCLICK);
      wxMouseEvent &event = *_event;

      event.x = x; event.y = y;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_RIGHT_DCLICK;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_MBUTTONDOWN:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_MIDDLE_DOWN);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_MIDDLE_DOWN;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_MBUTTONUP:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_MIDDLE_UP);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_MIDDLE_UP;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_MBUTTONDBLCLK:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_MIDDLE_DCLICK);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_MIDDLE_DCLICK;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_LBUTTONDOWN:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_LEFT_DOWN);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_LEFT_DOWN;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_LBUTTONUP:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_LEFT_UP);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_LEFT_UP;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_LBUTTONDBLCLK:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_LEFT_DCLICK);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_LEFT_DCLICK;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    case WM_MOUSEMOVE:
    {
      wxMouseEvent *_event = new wxMouseEvent(wxEVENT_TYPE_MOTION);
      wxMouseEvent &event = *_event;

      event.shiftDown = (flags & MK_SHIFT);
      event.controlDown = (flags & MK_CONTROL);
      event.leftDown = (flags & MK_LBUTTON);
      event.middleDown = (flags & MK_MBUTTON);
      event.rightDown = (flags & MK_RBUTTON);

      event.x = x; event.y = y;

      if ((item->mswLastEvent == wxEVENT_TYPE_RIGHT_DOWN || item->mswLastEvent == wxEVENT_TYPE_LEFT_DOWN ||
           item->mswLastEvent == wxEVENT_TYPE_MIDDLE_DOWN) &&
          (item->mswLastXPos == event.x && item->mswLastYPos == event.y))
      {
        item->mswLastXPos = x; item->mswLastYPos = y;
        item->mswLastEvent = wxEVENT_TYPE_MOTION;
        return TRUE;
      }
      
      item->mswLastXPos = x; item->mswLastYPos = y;
      item->mswLastEvent = wxEVENT_TYPE_MOTION;
      item->GetEventHandler()->OnEvent(event);
      return TRUE;
    }
    // Ensure that static items get messages
    case WM_NCHITTEST:
    {
      return (long)HTCLIENT;
    }
    default:
    {
      return CallWindowProc(item->oldWndProc, hWnd, message, wParam, lParam);
    }
  }
  return CallWindowProc(item->oldWndProc, hWnd, message, wParam, lParam);
}

wxItem *wxFindControlFromHandle(HWND hWnd)
{
  if (!wxControlHandleList)
    return NULL;
#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
  wxNode *node = wxControlHandleList->Find((long)hWnd);
  if (!node)
    return NULL;
  return (wxItem *)node->Data();
#else
  return (wxItem *)wxControlHandleList->Find((long)hWnd);
#endif
}

void wxAddControlHandle(HWND hWnd, wxItem *item)
{
  if (!wxControlHandleList) {
#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
    wxControlHandleList = new wxList(wxKEY_INTEGER);
#else
    wxControlHandleList = new wxNonlockingHashTable;
#endif
  }
  wxControlHandleList->Append((long)hWnd, item);
}


