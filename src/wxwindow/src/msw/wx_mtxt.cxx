/*
 * File:	wx_mtxt.cc
 * Purpose:	Multi-line text item implementation
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
#include "wx_mtxt.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

// Multi-line Text item
IMPLEMENT_DYNAMIC_CLASS(wxMultiText, wxText)

wxMultiText::wxMultiText(void)
{
}

wxMultiText::wxMultiText(wxPanel *panel, wxFunction Function, char *label, char *value,
               int x, int y, int width, int height, long style, char *name):
  wxbMultiText(panel, Function, label, value, x, y, width, height, style, name)
{
  Create(panel, Function, label, value, x, y, width, height, style, name);
}

Bool wxMultiText::Create(wxPanel *panel, wxFunction Function, char *label, char *value,
               int x, int y, int width, int height, long style, char *name)
{
  SetName(name);
  if (panel) panel->AddChild(this);
  buttonFont = panel->buttonFont ;
  labelFont = panel->labelFont ;
  backColour = panel->backColour ;
  labelColour = panel->labelColour ;
  buttonColour = panel->buttonColour ;
  if (height == -1) height = 50;

  window_parent = panel;
  labelPosition = panel->label_position;
  
  wxWinType = wxTYPE_HWND;
  windowStyle = style;
  wxWnd *cparent = NULL;
  if (panel)
    cparent = (wxWnd *)(panel->handle);

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

  long msStyle = ES_MULTILINE | ES_LEFT | ES_WANTRETURN |
					WS_BORDER | WS_VISIBLE | WS_CHILD | WS_TABSTOP |
               WS_VSCROLL;

  if (windowStyle & wxREADONLY)
    msStyle |= ES_READONLY;

  if (windowStyle & wxHSCROLL)
    msStyle |= (WS_HSCROLL | ES_AUTOHSCROLL) ;

  if (windowStyle & wxPASSWORD) // hidden input
    msStyle |= ES_PASSWORD;

  HWND edit = wxwmCreateWindowEx(0, "EDIT", label,
               msStyle,
               0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
               globalHandle, NULL);
#if CTL3D
  Ctl3dSubclassCtl(edit);
#endif

  ms_handle = (HANDLE)edit;

  // Subclass again for purposes of dialog editing mode
  SubclassControl(edit);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (buttonFont && buttonFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

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

void wxMultiText::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxMultiText::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxMultiText::SetButtonColour(wxColour* WXUNUSED(col))
{
}

char *wxMultiText::GetValue(void)
{
  // Fix by /Roffe for Borland 4.0.
  int TL = GetWindowTextLength((HWND)ms_handle);
  if (TL > 999)
    TL = 999;

  // Calling SendMessage this way is equivalent with a GetWindowText call.
  // Note that (some of) the parameters have to be casted appropriately...
  SendMessage((HWND)ms_handle, WM_GETTEXT, TL+1, (LPARAM)((LPSTR) wxBuffer));

  if(TL == 999)
    wxBuffer[999] = '\0';

  return wxBuffer;

/* Not sure if this is still needed.

// Julian! I can't see why this job below should have to be done -
// the edit control has the es_wantreturn attribute byte set,
// and will thus have the cr/lf characters inserted appropriately... /Roffe

  int buf_len = 1000;
  int no_chars = 0;
  int no_lines = (int)SendMessage((HWND)ms_handle, EM_GETLINECOUNT, 0, 0L);
  int i;
  for (i = 0; i < no_lines; i++)
  {
    *(LPINT)(wxBuffer+no_chars) = buf_len;
    int n = (int)SendMessage((HWND)ms_handle, EM_GETLINE, i,
                         (LONG)(wxBuffer+no_chars));
    no_chars += n;
    buf_len -= (n + 2);
    if (i < (no_lines - 1))
    {
      wxBuffer[no_chars] = 13;
      no_chars ++;
      wxBuffer[no_chars] = 10;
      no_chars ++;
    }
  }
  wxBuffer[no_chars] = 0;
  return wxBuffer;
*/
}

void wxMultiText::GetValue(char *buffer, int maxSize)
{
  // Fix by /Roffe for Borland 4.0.
  int TL = GetWindowTextLength((HWND)ms_handle);

  // We still want to keep within the limits for an edit control, won't we?
  if (maxSize > 32766)
    maxSize = 32766;

  if (TL > maxSize)
    TL = maxSize;

  // Here we use GetWindowText instead.
  GetWindowText((HWND)ms_handle, (LPSTR) buffer, TL);

  if(TL == maxSize)
    buffer[maxSize] = '\0';

/*
  buffer[0] = 0;
  int no_chars = 0;
  int no_lines = (int)SendMessage((HWND)ms_handle, EM_GETLINECOUNT, 0, 0L);
  int i;
  for (i = 0; i < no_lines; i++)
  {
    *(LPINT)(buffer+no_chars) = maxSize;
    int n = (int)SendMessage((HWND)ms_handle, EM_GETLINE, i,
                         (LONG)(buffer+no_chars));
    no_chars += n;
    maxSize -= (n + 2);
    if (i < (no_lines - 1))
    {
      buffer[no_chars] = 13;
      no_chars ++;
      buffer[no_chars] = 10;
      no_chars ++;
    }
  }
  buffer[no_chars] = 0;
*/
}

void wxMultiText::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  char buf[300];

  int cx;
  int cy;
  int clx;
  int cly;

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
  {
    GetSize(&width, &height);
  }

  if (height <= 0)
    height = DEFAULT_ITEM_HEIGHT;

  wxGetCharSize((HWND)ms_handle, &cx, &cy,buttonFont);

  float label_width, label_height, label_x, label_y;
  float control_width, control_height, control_x, control_y;

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
      label_x = (float)x;
      label_y = (float)y;
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
    control_height = DEFAULT_ITEM_HEIGHT;

  if (control_width <= 0)
    control_width = DEFAULT_ITEM_WIDTH;

//  wxDebugMsg("About to set the multitext height to %d", (int)control_height);

  MoveWindow((HWND)ms_handle, (int)control_x, (int)control_y,
                              (int)control_width, (int)control_height, TRUE);
  GetEventHandler()->OnSize(width, height);
}
