/*
 * File:	wx_messg.cc
 * Purpose:	Message item implementation
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
#include "wx_messg.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

// Message
IMPLEMENT_DYNAMIC_CLASS(wxMessage, wxItem)

wxMessage::wxMessage(void)
{
  wxWinType = wxTYPE_HWND;
  ms_handle = 0;
}

wxMessage::wxMessage(wxPanel *panel, char *label, int x, int y, long style, char *name):
  wxbMessage(panel, label, x, y, style, name)
{
  Create(panel, label, x, y, style, name);
}

#if USE_BITMAP_MESSAGE
wxMessage::wxMessage(wxPanel *panel, wxBitmap *image, int x, int y, long style, char *name):
  wxbMessage(panel, image, x, y, style, name)
{
  Create(panel, image, x, y, style, name);
}
#endif
  
Bool wxMessage::Create(wxPanel *panel, char *label, int x, int y, long style, char *name)
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

  HWND static_item = wxwmCreateWindowEx(0, STATIC_CLASS, label,
								 STATIC_FLAGS,
                         0, 0, 0, 0, cparent->handle, (HMENU)NewId(),
                         wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(static_item);
#endif

  ms_handle = (HANDLE)static_item;

  SubclassControl(static_item);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (labelFont && labelFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)labelFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  panel->GetValidPosition(&x, &y);

  SetSize(x, y, -1, -1);
  panel->AdvanceCursor(this);
  return TRUE;
}

#if USE_BITMAP_MESSAGE
Bool wxMessage::Create(wxPanel *panel, wxBitmap *image, int x, int y, long style, char *name)
{
  if (!image->Ok() || (image->selectedIntoDC < 0))
    return Create(panel, "<bad-image>", x, y, style, name);
  
  image->selectedIntoDC++;
  bm_label = image;

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

  HWND static_item = wxwmCreateWindowEx(0, FafaStat, NULL,
//                         FS_BITMAP | FS_Y4 | FS_X4 | WS_CHILD | WS_VISIBLE | WS_GROUP,
                         FS_BITMAP | FS_X2 | FS_Y2 | WS_CHILD | WS_VISIBLE | WS_GROUP,
                         0, 0, 0, 0, cparent->handle, (HMENU)NewId(),
                         wxhInstance, NULL);
  if (image) {
    SetBitmapDimensionEx(image->ms_bitmap,
			 image->GetWidth(),
			 image->GetHeight(),
			 NULL);
    SendMessage((HWND)static_item,WM_CHANGEBITMAP,
                  (WPARAM)0xFFFF/*((image->GetHeight()<<8)+image->GetWidth())*/,
                  (LPARAM)image->ms_bitmap);
  }
/*
#if CTL3D
  Ctl3dSubclassCtl(static_item);
#endif
*/
  ms_handle = (HANDLE)static_item;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(static_item);

/*
  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (labelFont && labelFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)labelFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;
*/
  panel->GetValidPosition(&x, &y);

  SetSize(x, y, image ? image->GetWidth() : 0, image ? image->GetHeight() : 0);
  panel->AdvanceCursor(this);
  return TRUE;
}
#endif

wxMessage::~wxMessage(void)
{
 if (bm_label) {
    --bm_label->selectedIntoDC;
    bm_label = NULL;
  }
}

void wxMessage::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxMessage::SetLabelColour(wxColour* WXUNUSED(col) )
{
}

void wxMessage::SetButtonColour(wxColour* WXUNUSED(col))
{
}

void wxMessage::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  int actualWidth = width;
  int actualHeight = height;

  char buf[300];
  float current_width;
  float cyf;
  GetWindowText((HWND)ms_handle, buf, 300);
  GetTextExtent(buf, &current_width, &cyf, NULL, NULL,labelFont);

  int ww, hh;
  GetSize(&ww, &hh);

  // If we're prepared to use the existing width, then...
  if (width == -1 && ((sizeFlags & wxSIZE_AUTO_WIDTH) != wxSIZE_AUTO_WIDTH))
    actualWidth = ww;
  else if (width == -1)
  {
    int cx;
    int cy;
    wxGetCharSize((HWND)ms_handle, &cx, &cy,labelFont);
    actualWidth = (int)(current_width + cx) ;
  }

  // If we're prepared to use the existing height, then...
  if (height == -1 && ((sizeFlags & wxSIZE_AUTO_HEIGHT) != wxSIZE_AUTO_HEIGHT))
    actualHeight = hh;
  else if (height == -1)
  {
    actualHeight = (int)(cyf) ;
  }

  MoveWindow((HWND)ms_handle, x, y, actualWidth, actualHeight, TRUE);

  if (!((width == -1) && (height == -1)))
    GetEventHandler()->OnSize(actualWidth, actualHeight);
}

void wxMessage::SetLabel(char *label)
{
  if (bm_label)
    return;

  float w, h;
  RECT rect;

  wxWindow *parent = GetParent();
  GetWindowRect((HWND)ms_handle, &rect);

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

  GetTextExtent(label, &w, &h, NULL, NULL, labelFont);
  MoveWindow((HWND)ms_handle, point.x, point.y, (int)(w + 10), (int)h,
             TRUE);
  SetWindowText((HWND)ms_handle, label);
}

#if USE_BITMAP_MESSAGE
void wxMessage::SetLabel(wxBitmap *bitmap)
{
  if (!bm_label || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label = bitmap;
  bm_label->selectedIntoDC++;

#if FAFA_LIB
  int x, y;
  int w, h;
  GetPosition(&x, &y);
  GetSize(&w, &h);
  RECT rect;
  rect.left = x; rect.top = y; rect.right = x + w; rect.bottom = y + h;
  
  MoveWindow((HWND)ms_handle, x, y, bitmap->GetWidth(), bitmap->GetHeight(),
             FALSE);
  
  SetBitmapDimensionEx(bitmap->ms_bitmap,
			 bitmap->GetWidth(),
			 bitmap->GetHeight(),
			 NULL);
  SendMessage((HWND)ms_handle,WM_CHANGEBITMAP,
                (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
                (LPARAM)bitmap->ms_bitmap);
  
  InvalidateRect(GetParent()->GetHWND(), &rect, TRUE);
#endif
}
#endif
