/*
 * File:	wx_rbox.cc
 * Purpose:	Radio box item implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#define HAS_LABEL 1

BOOL wxRadioBox::MSWCommand(UINT param, WORD id)
{
  if (param == BN_CLICKED)  {
    return TRUE;
  } else 
    return FALSE;
}

extern int wxDoItemPres(wxItem *item, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam,
			long *r);

typedef struct {
  FARPROC old;
  wxItem *item;
  int which;
} wxRBInfo;

// Sub-classed generic control proc
LONG APIENTRY _EXPORT
wxRadioItemProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  if (message == WM_GETDLGCODE)
    return DLGC_WANTMESSAGE;

  wxRBInfo *i = (wxRBInfo *)wxFindControlFromHandle(hWnd);
  
  if (!i) return FALSE;

  long r;
  if (!wxDoItemPres(i->item, hWnd, message, wParam, lParam, &r))
    return r;

  if (message == WM_LBUTTONDOWN) {
    wxRadioBox *rb = (wxRadioBox *)i->item;
    if (rb->buttonEnabled[i->which]) {
      rb->SetSelection(i->which);
      rb->ButtonFocus(i->which);
      wxCommandEvent *event;
	  event = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
      i->item->ProcessCommand(event);
    }
    return TRUE;
  } else
    return CallWindowProc((WNDPROC)i->old, hWnd, message, wParam, lParam);
}

static FARPROC wxGenericRIProc;

static void SubclassRadioButton(HWND hWnd, wxItem *item, int which)
{
  wxRBInfo *i = (wxRBInfo *)malloc(sizeof(wxRBInfo));
  
  i->item = item;
  i->which = which;

  // Subclass again for purposes of dialog editing mode
  wxAddControlHandle(hWnd, (wxItem *)i);
  i->old = (FARPROC) GetWindowLong(hWnd, GWL_WNDPROC);
  if (!wxGenericRIProc)
    wxGenericRIProc = MakeProcInstance((FARPROC) wxRadioItemProc, wxhInstance);
  SetWindowLong(hWnd, GWL_WNDPROC, (LONG) wxGenericRIProc);
}

void UnsubclassRadioButton(HWND hWnd)
{
  wxRBInfo *i = (wxRBInfo *)wxFindControlFromHandle(hWnd);
  
  if (i) {
    wxRemoveControlHandle(hWnd);
    SetWindowLong(hWnd, GWL_WNDPROC, (LONG)i->old);
    free(i);
  }
}

// Radio box item

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, char **Choices,
                       int MajorDim,long style, char *name):
  wxbRadioBox(panel, func, Title, x, y, width, height, N, Choices,
              MajorDim, style, name)
{
  Create(panel, func, Title, x, y, width, height, N, Choices, NULL, MajorDim, style, name);
}

/*
 * Causes problems for the Turbo C++ for Windows linker
 *
 */
wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, wxBitmap **Choices,
                       int MajorDim,long style, char *name):
  wxbRadioBox(panel, func, Title, x, y, width, height, N, Choices,
              MajorDim, style, name)
{
  Create(panel, func, Title, x, y, width, height, N, NULL, Choices, MajorDim, style, name);
}

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, char **Choices, wxBitmap **bmChoices,
                       int MajorDim, long _style, char *name)
{
  panel->AddChild(this);
  if (MajorDim==0)
    MajorDim = N ;
  majorDim = MajorDim;
  selected = -1;
  ms_handle = 0;
  wxWinType = wxTYPE_HWND;
  isFafa = !!bmChoices;
  static_label = NULL;

  wxWnd *cparent = (wxWnd *)(panel->handle);

  panel->GetValidPosition(&x, &y);

  char *the_label;

  the_label = copystring(Title ? Title : "");

  HWND the_handle = cparent->handle;

  ms_handle = wxwmCreateWindowEx(0, GROUP_CLASS, the_label,
				 GROUP_FLAGS,
				 0, 0, 0, 0,
				 cparent->handle, (HMENU)NewId(this),
				 wxhInstance, NULL);

  HDC the_dc = GetWindowDC((HWND)ms_handle) ;
  if (labelFont && labelFont->GetInternalFont(the_dc))
    SendMessage((HWND)ms_handle,WM_SETFONT,
                (WPARAM)labelFont->GetInternalFont(the_dc),0L);
  ReleaseDC((HWND)ms_handle,the_dc) ;

  the_handle = cparent->handle;

  SubclassControl((HWND)ms_handle);

  radioButtons = new HWND[N];
  radioWidth = new int[N];
  radioHeight = new int[N];
  if (bmChoices)
    bm_labels = new wxBitmap*[N];
  else
    bm_labels = NULL;
  buttonEnabled = new Bool[N];

  subControls = new wxList();

  int i;
  for (i = 0; i < N; i++) {
    long newId;
    long groupStyle = 0;

    if (i == 0 && _style==0)
      groupStyle = WS_GROUP;

    newId = NewId(this);
    buttonEnabled[i] = TRUE;
      
    if (bmChoices) {
      radioWidth[i]  = bmChoices[i]->GetWidth()  + FB_MARGIN;
      radioHeight[i] = bmChoices[i]->GetHeight() + FB_MARGIN;
      char tmp[32];
      sprintf(tmp, "Toggle%d", i);
      
      bm_labels[i] = bmChoices[i];
      bm_labels[i]->selectedIntoDC++;
      radioButtons[i] = wxwmCreateWindowEx(0, FafaChck, tmp,
					   groupStyle | BITRADIO_FLAGS,
					   0, 0, 0, 0,
					   the_handle, (HMENU)newId, wxhInstance, NULL);
	
      SetBitmapDimensionEx(bmChoices[i]->ms_bitmap,
			   bmChoices[i]->GetWidth(),
			   bmChoices[i]->GetHeight(),
			   NULL);
      SendMessage((HWND)radioButtons[i],WM_CHANGEBITMAP,
		  (WPARAM)0xFFFF,
		  (LPARAM)bmChoices[i]->ms_bitmap);
    } else {
      radioWidth[i] = radioHeight[i] = -1 ;
      radioButtons[i] = wxwmCreateWindowEx(0, RADIO_CLASS, Choices[i],
					   groupStyle | RADIO_FLAGS, 0, 0, 0, 0,
					   the_handle, (HMENU)newId, wxhInstance, NULL);
    }

    SubclassRadioButton(radioButtons[i], this, i);
    HDC the_dc = GetWindowDC((HWND)radioButtons[i]) ;
    if (buttonFont && buttonFont->GetInternalFont(the_dc))
      SendMessage((HWND)radioButtons[i],WM_SETFONT,
		  (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
    ReleaseDC((HWND)radioButtons[i],the_dc) ;
    subControls->Append((wxObject *)newId);
  }

  // Create a dummy radio control to end the group.
  (void)wxwmCreateWindowEx(0, RADIO_CLASS, "", 
			   WS_GROUP | RADIO_FLAGS, 0, 0, 0, 0, 
			   the_handle, (HMENU)NewId(this), wxhInstance, NULL);

  no_items = N;
  SetSelection(0);
  
  style = Title ? HAS_LABEL : 0;

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);
  Callback(func);

  return TRUE;
}

wxRadioBox::~wxRadioBox(void)
{
  isBeingDeleted = TRUE;
  
  if (static_label)
    wxwmDestroyWindow(static_label);
  static_label = NULL;
  if (radioButtons) {
    int i;
    for (i = 0; i < no_items; i++) {
	  UnsubclassRadioButton(radioButtons[i]);
      wxwmDestroyWindow(radioButtons[i]);
	}
    delete[] radioButtons;
  }

  if (bm_labels) {
    int i;

    for (i = 0; i < no_items; i++)
      if (bm_labels[i])
	--bm_labels[i]->selectedIntoDC;

    bm_labels = NULL;
  }
  if (ms_handle)
    wxwmDestroyWindow((HWND)ms_handle) ;
  ms_handle = NULL ;

}

char *wxRadioBox::GetLabel(int item)
{
  char buf[300];
  GetWindowText((HWND)radioButtons[item], buf, 300);
  return copystring(buf);
}

void wxRadioBox::SetLabel(int item, char *label)
{
  if (bm_labels && bm_labels[item])
    return;
  
  // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
  SendMessage((HWND)radioButtons[item],WM_CHANGEBITMAP,
	      (WPARAM)0,
	      (LPARAM)NULL);

  radioWidth[item] = radioHeight[item] = -1 ;
  SetWindowText((HWND)radioButtons[item], label);
}

void wxRadioBox::SetLabel(int item,wxBitmap *bitmap)
{
  if (!bm_labels || !bm_labels[item]
      || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --bm_labels[item]->selectedIntoDC;
  bm_labels[item] = bitmap;
  bm_labels[item]->selectedIntoDC++;

  SetBitmapDimensionEx(bitmap->ms_bitmap,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)radioButtons[item],WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
	      (LPARAM)bitmap->ms_bitmap);
  radioWidth[item] = bitmap->GetWidth() + FB_MARGIN ;
  radioHeight[item] = bitmap->GetHeight() + FB_MARGIN ;
}

int wxRadioBox::FindString(char *s)
{
  int i;
  for (i = 0; i < no_items; i++) {
    GetWindowText(radioButtons[i], wxBuffer, 1000);
    if (strcmp(wxBuffer, s) == 0)
      return i;
  }
  return -1;
}


void wxRadioBox::SetButton(int which, int value)
{
  int reset = 0;

  if (IsGray() || !buttonEnabled[which]) {
    ::EnableWindow(radioButtons[which], TRUE);
    reset = 1;
  }

  SendMessage(radioButtons[which], 
	      isFafa ? FAFA_SETCHECK : BM_SETCHECK, 
	      value, 0L);

  if (reset)
    ::EnableWindow(radioButtons[which], FALSE);
}


void wxRadioBox::SetSelection(int N)
{
  if ((N < 0) || (N >= no_items))
    return;

  if (N == selected)
    return;

  if (selected >= 0 && selected < no_items)
    SetButton(selected, 0);
    
  SetButton(N, 1);

  selected = N;
}

// Get single selection, for single choice list items
int wxRadioBox::GetSelection(void)
{
  return selected;
}

// Find string for position
char *wxRadioBox::GetString(int N)
{
  GetWindowText(radioButtons[N], wxBuffer, 1000);
  return wxBuffer;
}

void wxRadioBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;
  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;

  char buf[400];

  int y_offset = y;
  int x_offset = x;
  float current_width;

  float cyf;
  HWND wnd = (HWND)ms_handle;

  int cx1, cy1;

  // The layout looks like this:
  //  - Title --------
  //  | * item 1     |
  //  | * item 2     |
  //  ----------------
  // Each item is actually its own window, and we're
  //  responsible for putting each on in its place.
  // We find the height of the largest item, and then
  //  put maxHeight/2 space between each radio button.
  //  The label can be a different font from the rest.

  wxGetCharSize(wnd, &cx1, &cy1, buttonFont);
  int maxWidth =  -1;
  int maxHeight = -1;

  float label_width = 0;
  float label_height = 0;

  if (style & HAS_LABEL) {
    GetWindowText((HWND)ms_handle, buf, 300);
    GetTextExtent(wxStripMenuCodes(buf), &label_width, &label_height, NULL, NULL, labelFont);
    int char_width, ignored;
    wxGetCharSize(wnd, &char_width, &ignored, labelFont);
    label_width += 3 * char_width; /* space before & after label */
  } else {
    int cw, ch;
    wxGetCharSize(wnd, &cw, &ch, buttonFont);
    label_height = ch;
    label_width = cw;
  }

  int i;
  for (i = 0 ; i < no_items; i++) {
    int eachWidth;
    int eachHeight ;
    if (radioWidth[i] < 0) {
      // It's a labelled toggle
      GetWindowText(radioButtons[i], buf, 300);
      GetTextExtent(wxStripMenuCodes(buf), &current_width, &cyf,NULL,NULL, buttonFont);
      eachWidth = (int)(current_width + RADIO_SIZE);
      eachHeight = (int)cyf;
    } else {
      eachWidth = radioWidth[i];
      eachHeight = radioHeight[i];
    }
    if (maxWidth < eachWidth) maxWidth = eachWidth;
    if (maxHeight < eachHeight) maxHeight = eachHeight;
  }

  if (ms_handle) {
    int totWidth;
    int totHeight;

    int nbHor,nbVer;

    if (windowStyle & wxHORIZONTAL) {
      nbHor = majorDim;
      nbVer = (no_items + majorDim - 1) / majorDim;
    } else {
      nbVer = majorDim;
      nbHor = (no_items + majorDim - 1) / majorDim;
    }

    // hieght =       label         + items           + between & ends space
    totHeight = (int)(label_height + nbVer*maxHeight + nbVer*maxHeight/2.0
		      // + a little more at the end
		      + maxHeight/4.0);
    totWidth  = nbHor * (maxWidth + cx1)+ 2 * cx1;
    if (totWidth < label_width)
      totWidth = label_width;

    MoveWindow((HWND)ms_handle, x_offset, y_offset, totWidth, totHeight, TRUE);

    x_offset += cx1;
    y_offset += label_height + maxHeight/4;
  }

  int startX = x_offset;
  int startY = y_offset;

  for ( i = 0 ; i < no_items; i++) {
    // Bidimensional radio adjustment
    if (i && !(i % majorDim)) {
      if (windowStyle & wxHORIZONTAL) {
        x_offset = startX;
        y_offset += maxHeight + maxHeight/2;
      } else {
        y_offset = startY;
        x_offset += maxWidth + cx1;
      }
    }

    int eachWidth;
    int eachHeight;
    if (radioWidth[i] < 0) {
      // It's a labeled item
      GetWindowText(radioButtons[i], buf, 300);
      GetTextExtent(wxStripMenuCodes(buf), &current_width, &cyf, NULL, NULL, buttonFont);
      eachWidth = (int)(current_width + RADIO_SIZE);
      eachHeight = (int)cyf;
    } else {
      eachWidth = radioWidth[i];
      eachHeight = radioHeight[i];
    }

    MoveWindow(radioButtons[i], x_offset, y_offset, eachWidth, eachHeight, TRUE);
    if (windowStyle & wxHORIZONTAL)
      x_offset += maxWidth + cx1;
    else
      y_offset += maxHeight + maxHeight/2;
  }
  OnSize(width, height);
}

void wxRadioBox::GetSize(int *width, int *height)
{
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  if (static_label)
    wxFindMaxSize(static_label, &rect);

  if (ms_handle)
    wxFindMaxSize((HWND)ms_handle, &rect);

  int i;
  for (i = 0; i < no_items; i++)
    wxFindMaxSize((HWND)radioButtons[i], &rect);

  *width = rect.right - rect.left;
  *height = rect.bottom - rect.top;
}

void wxRadioBox::GetPosition(int *x, int *y)
{
  wxWindow *parent = GetParent();
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  int i;
  for (i = 0; i < no_items; i++)
    wxFindMaxSize(radioButtons[i], &rect);

  if (static_label)
    wxFindMaxSize(static_label, &rect);

  if (ms_handle)
    wxFindMaxSize((HWND)ms_handle, &rect);

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

char *wxRadioBox::GetLabel(void)
{
  if (static_label) {
    GetWindowText(static_label, wxBuffer, 300);
    return wxBuffer;
  } else if (ms_handle) {

    if (!GetWindowText((HWND)ms_handle, wxBuffer, 300))

      return NULL;

    return wxBuffer;

  }
  else return NULL;
}

void wxRadioBox::SetLabel(char *label)
{
  if (static_label) {
    float w, h;
    RECT rect;

    wxWindow *parent = GetParent();
    GetWindowRect(static_label, &rect);

    // Since we now have the absolute screen coords,
    // if there's a parent we must subtract its top left corner
    POINT point;
    point.x = rect.left;
    point.y = rect.top;
    if (parent) {
      wxWnd *cparent = (wxWnd *)(parent->handle);
      ::ScreenToClient(cparent->handle, &point);
    }

    GetTextExtent((LPSTR)label, &w, &h, NULL, NULL,labelFont);
    MoveWindow(static_label, point.x, point.y, (int)(w + 10), (int)h,
               TRUE);
    SetWindowText(static_label, label);
  } else if (ms_handle) {
    if (GetWindowText((HWND)ms_handle, wxBuffer, 300))
      SetWindowText((HWND)ms_handle, label);
  }
}

void wxRadioBox::SetFocus(void)
{
  if (no_items > 0)
   ::SetFocus(radioButtons[0]);
}

Bool wxRadioBox::Show(Bool show)
{
  int cshow;

  SetShown(show);

  window_parent->GetChildren()->Show(this, show);

  if (show)
    cshow = SW_SHOW;
  else
    cshow = SW_HIDE;

  ShowWindow((HWND)ms_handle, cshow);

  if (static_label)
    ShowWindow(static_label, cshow);

  int i;
  for (i = 0; i < no_items; i++)
    ShowWindow(radioButtons[i], cshow);

  return TRUE;
}

// Enable a specific button
void wxRadioBox::Enable(int item, Bool enable)
{
  if (item >= 0 && item < no_items) {
    buttonEnabled[item] = enable;
    if (!IsGray())
      ::EnableWindow(radioButtons[item], enable);
  }
}

void wxRadioBox::Enable(Bool enable)
{
  wxWindow::Enable(enable);
}

void wxRadioBox::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);

  int i;
  for (i = 0; i < no_items; i++) {
    if (gray)
      ::EnableWindow(radioButtons[i], FALSE);
    else
      ::EnableWindow(radioButtons[i], buttonEnabled[i]);
  }
}

// Show a specific button
void wxRadioBox::Show(int item, Bool show)
{
  if (item < 0)
    wxRadioBox::Show(show);
  else if (item < no_items) {
    int cshow;
    if (show)
      cshow = SW_SHOW;
    else
      cshow = SW_HIDE;
    ShowWindow(radioButtons[item], cshow);
  }
}

int wxRadioBox::ButtonFocus(int which)
{
  if (which < 0) {
    int i;
    HWND fw = ::GetFocus();
    for (i = no_items; i--; )
      if (fw == radioButtons[i])
	return i;
  } else if (which < no_items)
    ::SetFocus(radioButtons[which]);

  return -1;
}
