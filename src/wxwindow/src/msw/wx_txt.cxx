/*
 * File:	wx_txt.cc
 * Purpose:	Single-line text item implementation
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
#include "wx_txt.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

BOOL wxText::MSWCommand(UINT param, WORD WXUNUSED(id))
{
/*
  // Debugging
  wxDebugMsg("Edit control %d: ", (int)id);
  switch (param)
  {
    case EN_SETFOCUS:
      wxDebugMsg("EN_SETFOCUS\n");
      break;
    case EN_KILLFOCUS:
      wxDebugMsg("EN_KILLFOCUS\n");
      break;
    case EN_CHANGE:
      wxDebugMsg("EN_CHANGE\n");
      break;
    case EN_UPDATE:
      wxDebugMsg("EN_UPDATE\n");
      break;
    case EN_ERRSPACE:
      wxDebugMsg("EN_ERRSPACE\n");
      break;
    case EN_MAXTEXT:
      wxDebugMsg("EN_MAXTEXT\n");
      break;
    case EN_HSCROLL:
      wxDebugMsg("EN_HSCROLL\n");
      break;
    case EN_VSCROLL:
      wxDebugMsg("EN_VSCROLL\n");
      break;
    default:
      wxDebugMsg("Unknown EDIT notification\n");
      break;
  }
*/
  WXTYPE eventTyp = 0;
  switch (param)
  {
    case EN_SETFOCUS:
      eventTyp = wxEVENT_TYPE_SET_FOCUS;
      break;
    case EN_KILLFOCUS:
      eventTyp = wxEVENT_TYPE_KILL_FOCUS;
      break;
    case EN_CHANGE:
      break;
    case EN_UPDATE:
      eventTyp = wxEVENT_TYPE_TEXT_COMMAND;
      break;
    case EN_ERRSPACE:
      break;
    case EN_MAXTEXT:
      break;
    case EN_HSCROLL:
       break;
    case EN_VSCROLL:
      break;
    default:
      break;
  }
  if (eventTyp != 0)
  {
    wxCommandEvent *event = new wxCommandEvent(eventTyp);
    event->commandString = GetValue();
    event->eventObject = this;
    ProcessCommand(*event);
    return TRUE;
  }
  else
    return FALSE;
}

// Text item
IMPLEMENT_DYNAMIC_CLASS(wxText, wxItem)

wxText::wxText(void)
{
  wxWinType = wxTYPE_HWND;
  static_label = NULL;
  windows_id = 0;
  ms_handle = 0;
  globalHandle = 0;
}

wxText::wxText(wxPanel *panel, wxFunction Function, char *label, char *value,
               int x, int y, int width, int height, long style, char *name):
  wxbText(panel, Function, label, value, x, y, width, height, style, name)
{
  Create(panel, Function, label, value, x, y, width, height, style, name);
}
  
Bool wxText::Create(wxPanel *panel, wxFunction Function, char *label, char *value,
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
    cparent = (wxWnd *)(panel->handle);

  labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

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

  windows_id = (int)NewId();

#if 1
  globalHandle = wxhInstance;
#else
  // Obscure method from the MS Developer's Network Disk for
  // using global memory instead of the local heap, which
  // runs out far too soon. Solves the problem with
  // failing to appear.
  globalHandle=GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT,
								 256L);
#endif

  long msStyle = ES_AUTOHSCROLL | ES_LEFT | WS_BORDER |
                      WS_VISIBLE | WS_CHILD | WS_TABSTOP;

  if (windowStyle & wxREADONLY)
    msStyle |= ES_READONLY;

  if (windowStyle & wxPASSWORD) // hidden input
    msStyle |= ES_PASSWORD;

  HWND edit = wxwmCreateWindowEx(0, "EDIT", NULL,
                        msStyle,
                        0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
								globalHandle, NULL);
#if CTL3D
  Ctl3dSubclassCtl(edit);
#endif
/* The following code subclasses the EDIT control (again! -- CTL3D may
 * already have done it) to intercept the ENTER key, which only
 * works if the style wxPROCESS_ENTER has been used.
 */
  ms_handle = (HANDLE)edit;
  
  SubclassControl(edit);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  if ((width < 0) && value && strlen(value))
  {
  char *the_value ;
    the_value = new char[strlen(value)+1] ;
    if (style&wxFIXED_LENGTH)
    {
      int i;
      for (i=0;i<(int)strlen(value);i++)
        the_value[i]=MEANING_CHARACTER ;
    }
    else
      strcpy(the_value,value) ;
    the_value[strlen(value)] = '\0' ;

    int cx,cy ;
    wxGetCharSize((HWND)ms_handle,&cx,&cy,buttonFont) ;
    float current_width,cyf ;
    GetTextExtent(the_value,&current_width,&cyf, NULL, NULL,buttonFont) ;
    width = (int)current_width + 2*cx ;
    if (label && strlen(label) && labelPosition==wxHORIZONTAL)
    {
      GetTextExtent(the_label,&current_width,&cyf, NULL, NULL,labelFont) ;
      width += (int)current_width + cx ;
    }
    delete [] the_value ;
  }

  SetSize(x, y, width, height);

  if (value)
    SetWindowText(edit, value);

  panel->AdvanceCursor(this);
  Callback(Function);

  if (label)
  {
    if (style&wxFIXED_LENGTH)
      SetLabel(label) ;
    if (the_label)
      delete [] the_label ;
  }

  return TRUE;
}

wxText::~wxText(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
	 wxwmDestroyWindow(static_label);
}

void wxText::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxText::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxText::SetButtonColour(wxColour* WXUNUSED(col))
{
}

char *wxText::GetLabel(void)
{
  if (static_label)
  {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  }
  else return NULL;
}

void wxText::SetLabel(char *label)
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

char *wxText::GetValue(void)
{
/* Causes problems with Borland C++
  *(LPINT)wxBuffer = 500;
  int n = (int)SendMessage((HWND)ms_handle, EM_GETLINE, 0, (LONG)wxBuffer);
  wxBuffer[n] = 0;
  return wxBuffer;
*/
  GetWindowText((HWND)ms_handle, wxBuffer, 500);
  return wxBuffer;
}

void wxText::SetValue(char *value)
{
  if (!value)
    value = "";
    
  // If newlines are denoted by just 10, must stick 13 in front.
  int singletons = 0;
  int len = strlen(value);
  int i;
  for (i = 0; i < len; i ++)
  {
    if ((i > 0) && (value[i] == 10) && (value[i-1] != 13))
      singletons ++;
  }
  if (singletons > 0)
  {
    char *tmp = new char[len + singletons + 1];
    int j = 0;
    for (i = 0; i < len; i ++)
    {
      if ((i > 0) && (value[i] == 10) && (value[i-1] != 13))
      {
        tmp[j] = 13;
        j ++;
      }
      tmp[j] = value[i];
      j ++;
    }
    tmp[j] = 0;
    SetWindowText((HWND)ms_handle, tmp);
    delete[] tmp;
  }
  else
    SetWindowText((HWND)ms_handle, value);
}

void wxText::GetSize(int *width, int *height)
{
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  wxFindMaxSize((HWND)ms_handle, &rect);

  if (static_label)
  {
    wxFindMaxSize(static_label, &rect);
  }

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxText::GetPosition(int *x, int *y)
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

void wxText::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  char buf[300];
  int cx; // button font dimensions
  int cy;
  int clx; // label font dimensions
  int cly;

  wxGetCharSize((HWND)ms_handle, &cx, &cy,buttonFont);

  float label_width, label_height, label_x, label_y;
  float control_width, control_height, control_x, control_y;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
  {
    GetSize(&width, &height);
  }

  // Deal with default size (using -1 values)
  if (width<=0)
    width = DEFAULT_ITEM_WIDTH;

  if (static_label)
  {
    // Find size of label
    wxGetCharSize((HWND)ms_handle, &clx, &cly,labelFont);
    GetWindowText(static_label, buf, 300);
    GetTextExtent(buf, &label_width, &label_height, NULL, NULL,labelFont);

    // Given size is total label + edit size, so find individual
    // control sizes on that basis.
    if (labelPosition == wxHORIZONTAL)
    {
      if (height<=0)
        height = (int)((max(cly, cy))*EDIT_CONTROL_FACTOR) ;

      label_x = (float)x;
      label_y = (float)y + 2;
      label_width += (float)clx;

      control_x = label_x + label_width + cx;
      control_y = (float)y;
      control_width = width - (control_x - label_x);
      control_height = (float)height;
    }
    else // wxVERTICAL
    {
      label_x = (float)x;
      label_y = (float)y;

      control_x = (float)x;
      control_y = label_y + label_height + 3; // Allow for 3D border
      control_width = (float)width;

      if (height<=0)
        control_height = (float)(int)(cy*EDIT_CONTROL_FACTOR) ;
      else
        control_height = height - label_height - 3;
    }

    MoveWindow(static_label, (int)label_x, (int)label_y,
               (int)label_width, (int)label_height, TRUE);
  }
  else
  {
    control_x = (float)x;
    control_y = (float)y;
    control_width = (float)width;
    control_height = (float)height;
  }

  // Calculations may have made text size too small
  if (control_height <= 0)
    control_height = (float)(int)(cy*EDIT_CONTROL_FACTOR) ;

  if (control_width <= 0)
    control_width = DEFAULT_ITEM_WIDTH;

  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y,
                              (int)control_width, (int)control_height, TRUE);
  GetEventHandler()->OnSize(width, height);
}

void wxText::SetFocus(void)
{
  wxItem::SetFocus();
}

// Clipboard operations
void wxText::Copy(void)
{
  HWND hWnd = GetHWND();
  SendMessage(hWnd, WM_COPY, 0, 0L);
}

void wxText::Cut(void)
{
  HWND hWnd = GetHWND();
  SendMessage(hWnd, WM_CUT, 0, 0L);
}

void wxText::Paste(void)
{
  HWND hWnd = GetHWND();
  SendMessage(hWnd, WM_PASTE, 0, 0L);
}

void wxText::SetEditable(Bool editable)
{
  HWND hWnd = GetHWND();
  SendMessage(hWnd, EM_SETREADONLY, (WPARAM)!editable, (LPARAM)0L);
}

void wxText::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
  if (static_label)
    ::EnableWindow(static_label, !gray);
}
