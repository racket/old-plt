/*
 * File:	wx_slidr.cc
 * Purpose:	Slider implementation
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
#include "wx_slidr.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

wxList wxScrollBarList;

// Slider
IMPLEMENT_DYNAMIC_CLASS(wxSlider, wxItem)

wxSlider::wxSlider(void)
{
  wxWinType = wxTYPE_HWND;
  static_label = NULL;
  edit_value = NULL;
  static_min = NULL;
  windows_id = 0;
  page_size = 0;
  s_max = 0;
  s_min = 0;
  ms_handle = 0;
  static_max = 0;
}

wxSlider::wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y,
           long style, char *name):
  wxbSlider(panel, func, label, value, min_value, max_value, width, x, y,
            style, name)
{
  Create(panel, func, label, value, min_value, max_value, width, x, y,
         style, name);
}

Bool wxSlider::Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y, long style, char *name)
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

  labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

  int cx;
  int cy;
  wxGetCharSize(cparent->handle, &cx, &cy,buttonFont);

  char *the_label = NULL ;

  if (label)
  {
    the_label = new char[strlen(label)+1] ;
    if (style&wxFIXED_LENGTH)
    {
      int i;
      for (i=0;i<(int)strlen(label);i++)
        the_label[i]=MEANING_CHARACTER ;
    }
    else
      strcpy(the_label,label) ;
    the_label[strlen(label)] = '\0' ;
  }

  // If label exists, create a static control for it.
  if (label)
  {
	 static_label = wxwmCreateWindowEx(0, STATIC_CLASS, the_label,
								 STATIC_FLAGS,
								 0, 0, 0, 0, cparent->handle, (HMENU)NewId(),
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

  edit_value = wxwmCreateWindowEx(0, "EDIT", NULL,
									ES_AUTOHSCROLL | ES_LEFT | WS_VISIBLE | WS_CHILD |
									WS_TABSTOP | ES_READONLY,
									0, 0, 0, 0, cparent->handle, (HMENU)NewId(),
									wxhInstance, NULL);
#if CTL3D
	 Ctl3dSubclassCtl(edit_value);
#endif

  // Now create min static control
  sprintf(wxBuffer, "%d", min_value);
  static_min = wxwmCreateWindowEx(0, STATIC_CLASS, wxBuffer,
								 STATIC_FLAGS,
								 0, 0, 0, 0, cparent->handle, (HMENU)NewId(),
								 wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(static_min);
#endif

  // Now create slider
  windows_id = (int)NewId();

  long msStyle = 0;
  if (windowStyle & wxVERTICAL)
    msStyle = SBS_VERT | WS_CHILD | WS_VISIBLE;
  else
    msStyle = SBS_HORZ | WS_CHILD | WS_VISIBLE;
    
  HWND scroll_bar = wxwmCreateWindowEx(0, "SCROLLBAR", wxBuffer,
                         msStyle,
                         0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
                         wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(scroll_bar);
#endif

  wxScrollBarList.Append((long)scroll_bar, this);

  page_size = (int)((max_value-min_value)/10);
  s_max = max_value;
  s_min = min_value;

  ::SetScrollRange(scroll_bar, SB_CTL, min_value, max_value, FALSE);
  ::SetScrollPos(scroll_bar, SB_CTL, value, FALSE);
  ShowWindow(scroll_bar, SW_SHOW);

  ms_handle = (HANDLE)scroll_bar;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(scroll_bar);

  // Finally, create max value static item
  sprintf(wxBuffer, "%d", max_value);
  static_max = wxwmCreateWindowEx(0, STATIC_CLASS, wxBuffer,
								 STATIC_FLAGS,
                         0, 0, 0, 0, cparent->handle, (HMENU)NewId(),
                         wxhInstance, NULL);
#if CTL3D
  Ctl3dSubclassCtl(static_max);
#endif

  HDC the_dc = GetWindowDC((HWND)static_max) ;
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
  {
    SendMessage((HWND)static_min,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
    SendMessage((HWND)static_max,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
    if (edit_value)
      SendMessage((HWND)edit_value,WM_SETFONT,
                  (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
  }
  ReleaseDC((HWND)static_max,the_dc) ;

  SetSize(x, y, width, -1);
  SetValue(value);

  panel->AdvanceCursor(this);
  Callback(func);

  if (label)
  {
    if (style&wxFIXED_LENGTH)
      SetLabel(label) ;
    if (the_label)
      delete [] the_label ;
  }

  return TRUE;
}

// Called from wx_win.cc: wxWnd::OnHScroll, wxWnd::OnVScroll
void wxSliderEvent(HWND bar, WORD wParam, WORD pos)
{
    wxNode *node = (wxNode *)wxScrollBarList.Find((long)bar);
    if (!node)
      return;

    wxSlider *slider = (wxSlider *)node->Data();
    int position = GetScrollPos(bar, SB_CTL);

    int nScrollInc;
    switch ( wParam )
    {
            case SB_LINEUP:
                    nScrollInc = -1;
                    break;

            case SB_LINEDOWN:
                    nScrollInc = 1;
                    break;

            case SB_PAGEUP:
                    nScrollInc = -slider->page_size;
                    break;

            case SB_PAGEDOWN:
                    nScrollInc = slider->page_size;;
                    break;

            case SB_THUMBTRACK:
#ifdef WIN32
                    nScrollInc = (signed short)pos - position;
#else
                    nScrollInc = pos - position;
#endif
                    break;

            default:
                    nScrollInc = 0;
    }

    if (nScrollInc != 0)
    {
      int new_pos = position + nScrollInc;
      if (!(new_pos < slider->s_min || new_pos > slider->s_max))
      {
        slider->SetValue(new_pos);
        wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
        event->commandInt = new_pos;
        event->eventObject = slider;
        slider->ProcessCommand(*event);
      }
    }
}

wxSlider::~wxSlider(void)
{
  isBeingDeleted = TRUE;
  
    if (static_min)
    {
		wxwmDestroyWindow(static_min);
	 }
	 if (static_max)
	 {
		wxwmDestroyWindow(static_max);
	 }
	 if (edit_value)
	 {
		wxwmDestroyWindow(edit_value);
	 }
	 if (static_label)
	 {
		wxwmDestroyWindow(static_label);
	 }
	 wxScrollBarList.DeleteObject(this);
}

void wxSlider::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxSlider::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxSlider::SetButtonColour(wxColour* WXUNUSED(col))
{
}

Bool wxSlider::Show(Bool show) 
{
  wxWindow::Show(show);

  int cshow;
  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;
  if (static_label) ShowWindow(static_label, (BOOL)cshow);
  if (static_min) ShowWindow(static_min, (BOOL)cshow);
  if (static_max) ShowWindow(static_max, (BOOL)cshow);
  if (edit_value) ShowWindow(edit_value, (BOOL)cshow);

  return TRUE;
}

int wxSlider::GetValue(void)
{
  return ::GetScrollPos((HWND)ms_handle, SB_CTL);
}

char *wxSlider::GetLabel(void)
{
  if (static_label)
  {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  }
  else return NULL;
}

void wxSlider::SetValue(int value)
{
  ::SetScrollPos((HWND)ms_handle, SB_CTL, value, TRUE);

  if (edit_value)
  {
    sprintf(wxBuffer, "%d", value);
    SetWindowText(edit_value, wxBuffer);
  }
}

void wxSlider::SetLabel(char *label)
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

    GetTextExtent(label, &w, &h, NULL, NULL,labelFont);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowText(static_label, label);
  }
}

void wxSlider::GetSize(int *width, int *height)
{
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
  {
    wxFindMaxSize(static_label, &rect);
  }
  if (static_min)
  {
    wxFindMaxSize(static_min, &rect);
  }
  if (static_max)
  {
    wxFindMaxSize(static_max, &rect);
  }
  if (edit_value)
  {
    wxFindMaxSize(edit_value, &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxSlider::GetPosition(int *x, int *y)
{
  wxWindow *parent = GetParent();
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
  {
    wxFindMaxSize(static_label, &rect);
  }
  if (static_min)
  {
    wxFindMaxSize(static_min, &rect);
  }
  if (static_max)
  {
    wxFindMaxSize(static_max, &rect);
  }
  if (edit_value)
  {
    wxFindMaxSize(edit_value, &rect);
  }

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

void wxSlider::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  char buf[300];

  int x_offset = x;
  int y_offset = y;
  float current_width;

  int cx;     // slider,min,max sizes
  int cy;
  float cyf;
  int cxs;    // label sizes
  int cys;
  int dy,dys ;  // Adjustment values (vertical) if font sizes differs.

  wxGetCharSize((HWND)ms_handle, &cx, &cy,buttonFont);

  if ((windowStyle & wxVERTICAL) != wxVERTICAL)
  {
    // Horizontal
    if (static_label)
    {
      wxGetCharSize((HWND)ms_handle, &cxs, &cys,labelFont);
      GetWindowText(static_label, buf, 300);
      GetTextExtent(buf, &current_width, &cyf,NULL,NULL, labelFont);
      if (cys>cy)
      {
        dys = 0 ;
        dy = (cys-cy)/2 ; // center slider
      }
      else
      {
        dys = (cy-cys)/2; // center label
        dy = 0 ;
      }
      MoveWindow(static_label, x_offset, y+dys, (int)(current_width + 2*cxs), (int)cyf,
                 TRUE);
      if (labelPosition==wxHORIZONTAL)
      {
        x_offset += (int)(current_width + 2*cxs + cxs);
        y_offset += dy ;
      }
      else
        y_offset += cys ;
    }

    float min_len;
    GetWindowText(static_min, buf, 300);
    GetTextExtent(buf, &min_len, &cyf,NULL,NULL, buttonFont);

    float max_len;
    GetWindowText(static_max, buf, 300);
    GetTextExtent(buf, &max_len, &cyf,NULL,NULL, buttonFont);

    if (edit_value)
    {
      int new_width = (int)(max(min_len, max_len));
      MoveWindow(edit_value, x_offset, y_offset, new_width, (int)cyf, TRUE);
      x_offset += new_width + cx;
    }

    MoveWindow(static_min, x_offset, y_offset, (int)min_len, cy, TRUE);
    x_offset += (int)(min_len + cx);

    int slider_length = (int)(width - (x_offset - x) - max_len - cx);

    // Slider must have a minimum/default length
    if (slider_length < 0)
      slider_length = 100;

    MoveWindow((HWND)ms_handle, x_offset, y_offset, slider_length, cy, TRUE);
    x_offset += slider_length + cx;

    MoveWindow(static_max, x_offset, y_offset, (int)max_len, cy, TRUE);
  }
  else
  {
    if (static_label)
    {
      wxGetCharSize((HWND)ms_handle, &cxs, &cys,labelFont);
      GetWindowText(static_label, buf, 300);
      GetTextExtent(buf, &current_width, &cyf,NULL,NULL, labelFont);
      MoveWindow(static_label, x_offset, y, (int)(current_width + 2*cxs), (int)cyf, TRUE);
      y_offset += cys ;
    }

    float min_len;
    GetWindowText(static_min, buf, 300);
    GetTextExtent(buf, &min_len, &cyf,NULL,NULL, buttonFont);

    float max_len;
    GetWindowText(static_max, buf, 300);
    GetTextExtent(buf, &max_len, &cyf,NULL,NULL, buttonFont);

    if (edit_value)
    {
      int new_width = (int)(max(min_len, max_len));
      MoveWindow(edit_value, x_offset, y_offset, new_width, (int)cyf, TRUE);
      y_offset += (int)cyf;
    }

    MoveWindow(static_min, x_offset, y_offset, (int)min_len, cy, TRUE);
    y_offset += cy;

    int slider_length = (int)(height - y_offset - cy - cy);

    // Slider must have a minimum/default length
    if (slider_length < 0)
      slider_length = 100;

    // Use character height as an estimate of slider width (yes, width)
    MoveWindow((HWND)ms_handle, x_offset, y_offset, cy, slider_length, TRUE);
    y_offset += slider_length;

    MoveWindow(static_max, x_offset, y_offset, (int)max_len, cy, TRUE);
  }
  OnSize(width, height);
}


void wxSlider::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  if (static_label) ::EnableWindow(static_label, !gray);
  if (static_min) ::EnableWindow(static_min, !gray);
  if (static_max) ::EnableWindow(static_max, !gray);
  if (edit_value) ::EnableWindow(edit_value, !gray);
}
