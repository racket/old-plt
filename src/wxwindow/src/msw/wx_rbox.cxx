/*
 * File:	wx_rbox.cc
 * Purpose:	Radio box item implementation
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
#include "wx_rbox.h"
#include "wx_itemp.h"
#include "wx_wmgr.h"

#endif

#define HAS_LABEL 1

BOOL wxRadioBox::MSWCommand(UINT param, WORD id)
{
  if (param == BN_CLICKED)  {
    selected = id;
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
    ProcessCommand(*event);
    return TRUE;
  } else 
    return FALSE;
}

extern int wxDoItemPres(wxItem *item, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam,
			long *r);

typedef struct {
  FARPROC old;
  wxItem *item;
} wxRBInfo;

// Sub-classed generic control proc
LONG APIENTRY _EXPORT
wxRadioItemProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  wxRBInfo *i = (wxRBInfo *)wxFindControlFromHandle(hWnd);
  
  if (!i) return FALSE;

  long r;
  if (!wxDoItemPres(i->item, hWnd, message, wParam, lParam, &r))
    return r;

  return CallWindowProc(i->old, hWnd, message, wParam, lParam);
}

static FARPROC wxGenericRIProc;

void SubclassRadioButton(HWND hWnd, wxItem *item)
{
  wxRBInfo *i = (wxRBInfo *)malloc(sizeof(wxRBInfo));
  
  i->item = item;

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
IMPLEMENT_DYNAMIC_CLASS(wxRadioBox, wxItem)

wxRadioBox::wxRadioBox(void)
{
  wxWinType = wxTYPE_HWND;
  static_label = NULL;
  windows_id = 0;
  no_items = 0;
  ms_handle = 0;
  radioButtons = NULL;
  majorDim = 0 ;
  selected = -1;
  radioWidth = NULL ;
  radioHeight = NULL ;
  isFafa = RADIO_IS_FAFA ;
}

wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, char **Choices,
                       int MajorDim,long style, char *name):
  wxbRadioBox(panel, func, Title, x, y, width, height, N, Choices,
              MajorDim, style, name)
{
  Create(panel, func, Title, x, y, width, height, N, Choices, MajorDim, style, name);
}

/*
 * Causes problems for the Turbo C++ for Windows linker
 *
 */
// #ifndef __BORLANDC__
wxRadioBox::wxRadioBox(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, wxBitmap **Choices,
                       int MajorDim,long style, char *name):
  wxbRadioBox(panel, func, Title, x, y, width, height, N, Choices,
              MajorDim, style, name)
{
  Create(panel, func, Title, x, y, width, height, N, Choices, MajorDim, style, name);
}
// #endif

Bool wxRadioBox::Create(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, char **Choices,
                       int MajorDim, long _style, char *name)
{
  SetName(name);
  if (panel) panel->AddChild(this);
  buttonFont = panel->buttonFont ;
  labelFont = panel->labelFont ;
  backColour = panel->backColour ;
  labelColour = panel->labelColour ;
  buttonColour = panel->buttonColour ;
  if (MajorDim == 0)
    MajorDim = N;
  majorDim = MajorDim;
  windowStyle = _style;
  selected = -1;
  ms_handle = 0;

  bm_labels = NULL;
  isFafa = RADIO_IS_FAFA ;
  static_label = NULL;
  wxWinType = wxTYPE_HWND;
  wxWnd *cparent = NULL;
  if (panel)
    cparent = (wxWnd *)(panel->handle);

//  labelPosition = panel->label_position;
  panel->GetValidPosition(&x, &y);

  char *the_label = NULL ;

  if (Title) {
    the_label = new char[strlen(Title)+1] ;
    if (_style & wxFIXED_LENGTH) {
      int i;
      for (i=0;i<(int)strlen(Title);i++)
        the_label[i]=MEANING_CHARACTER ;
    } else
      strcpy(the_label,Title);
    the_label[strlen(Title)] = '\0';
  } else 
    the_label = copystring("");

  static_label = NULL;
  HWND the_handle;

  ms_handle = wxwmCreateWindowEx(0, GROUP_CLASS, the_label,
				 GROUP_FLAGS,
				 0, 0, 0, 0,
				 cparent->handle, (HMENU)NewId(),
				 wxhInstance, NULL);

  the_handle = cparent->handle ;

#if CTL3D
  Ctl3dSubclassCtl(ms_handle);
#endif
  HDC the_dc = GetWindowDC(ms_handle) ;
  if (labelFont && labelFont->GetInternalFont(the_dc))
    SendMessage(ms_handle,WM_SETFONT,
                (WPARAM)labelFont->GetInternalFont(the_dc),0L);
  ReleaseDC(ms_handle,the_dc) ;

  the_handle = cparent->handle;

  // Subclass again for purposes of dialog editing mode
  SubclassControl((HWND)ms_handle);

  // Some radio boxes test consecutive id.
  (void)NewId();
  radioButtons = new HWND[N];
  radioWidth = new int[N];
  radioHeight = new int[N];

  buttonEnabled = new Bool[N];
  int i;
  for (i = 0; i < N; i++) {
    buttonEnabled[i] = TRUE;
    radioWidth[i] = radioHeight[i] = -1 ;
    long groupStyle = 0;
    if (i == 0 && _style==0)
      groupStyle = WS_GROUP;
    long newId = NewId();
    radioButtons[i] = wxwmCreateWindowEx(0, RADIO_CLASS, Choices[i],
					 groupStyle | RADIO_FLAGS, 0, 0, 0, 0,
					 the_handle, (HMENU)newId, wxhInstance, NULL);
#if CTL3D
    Ctl3dSubclassCtl(radioButtons[i]);
#endif
    SubclassRadioButton(radioButtons[i], this);
    HDC the_dc = GetWindowDC((HWND)radioButtons[i]) ;
    if (buttonFont && buttonFont->GetInternalFont(the_dc))
      SendMessage((HWND)radioButtons[i],WM_SETFONT,
                  (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
    ReleaseDC((HWND)radioButtons[i],the_dc) ;
    subControls.Append((wxObject *)newId);
  }
  //  (void)NewId() ;
  // Create a dummy radio control to end the group.
  (void)wxwmCreateWindowEx(0, RADIO_CLASS, "", 
			   WS_GROUP | RADIO_FLAGS, 0, 0, 0, 0, 
			   the_handle, (HMENU)NewId(), wxhInstance, NULL);

  no_items = N;
  SetSelection(0);
  
  style = Title ? HAS_LABEL : 0;

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);
  Callback(func);

  if (Title) {
    if (_style & wxFIXED_LENGTH)
      SetLabel(Title);
  }


  if (the_label)
    delete [] the_label ;

  return TRUE;
}

// #ifndef __BORLANDC__
Bool wxRadioBox::Create(wxPanel *panel, wxFunction func,
                       char *Title,
                       int x, int y, int width, int height,
                       int N, wxBitmap **Choices,
                       int MajorDim, long _style, char *name)
{
  SetName(name);
  if (panel) panel->AddChild(this);
  buttonFont = panel->buttonFont ;
  labelFont = panel->labelFont ;
  backColour = panel->backColour ;
  labelColour = panel->labelColour ;
  buttonColour = panel->buttonColour ;
  if (MajorDim==0)
    MajorDim = N ;
  majorDim = MajorDim;
  windowStyle = _style;
  selected = -1;
  ms_handle = 0;
  wxWinType = wxTYPE_HWND;
  wxWnd *cparent = NULL;
  if (panel)
    cparent = (wxWnd *)(panel->handle);

  panel->GetValidPosition(&x, &y);

  char *the_label = NULL ;

  if (Title)
  {
    the_label = new char[strlen(Title)+1] ;
    if (_style&wxFIXED_LENGTH)
    {
      int i;
      for (i=0;i<(int)strlen(Title);i++)
        the_label[i]=MEANING_CHARACTER ;
    }
    else
      strcpy(the_label,Title) ;
    the_label[strlen(Title)] = '\0' ;
  }
  else
    the_label = copystring("") ;

  static_label = NULL;
  HWND the_handle ;

  ms_handle = wxwmCreateWindowEx(0, GROUP_CLASS, the_label,
				 GROUP_FLAGS,
				 0, 0, 0, 0,
				 cparent->handle,(HMENU)NewId(),wxhInstance,NULL) ;

  HDC the_dc = GetWindowDC(ms_handle) ;
  if (labelFont && labelFont->GetInternalFont(the_dc))
	 SendMessage(ms_handle,WM_SETFONT,
					 (WPARAM)labelFont->GetInternalFont(the_dc),0L);
  ReleaseDC(ms_handle,the_dc) ;
  the_handle = cparent->handle;

  // Subclass again for purposes of dialog editing mode
  SubclassControl((HWND)ms_handle);

  (void)NewId() ;
  radioButtons = new HWND[N];

  bm_labels = new wxBitmap*[N];
  radioWidth = new int[N] ;
  radioHeight = new int[N] ;

  buttonEnabled = new Bool[N];
#if FAFA_LIB && !CTL3D
  isFafa = TRUE ; //always !!
#else
  isFafa = RADIO_IS_FAFA ;
#endif
  int i;
  for (i = 0; i < N; i++)
  {

    buttonEnabled[i] = TRUE;

    
    long groupStyle = 0;
    if (i == 0 && _style==0)
      groupStyle = WS_GROUP;
    long newId = NewId();
#if FAFA_LIB && !CTL3D
    radioWidth[i]  = Choices[i]->GetWidth()  + FB_MARGIN ;
    radioHeight[i] = Choices[i]->GetHeight() + FB_MARGIN ;
#else
    radioWidth[i]  = Choices[i]->GetWidth();
    radioHeight[i] = Choices[i]->GetHeight();
#endif
    char tmp[32] ;
    sprintf(tmp,"Toggle%d",i) ;
#if FAFA_LIB && !CTL3D

    if (Choices[i]->Ok()

        && (Choices[i]->selectedIntoDC >= 0)) {

	 bm_labels[i] = Choices[i];

	 bm_labels[i]->selectedIntoDC++;
	 radioButtons[i] = wxwmCreateWindowEx(0, FafaChck, tmp,
					      groupStyle | BITRADIO_FLAGS,
					      0, 0, 0, 0,
					      the_handle, (HMENU)newId, wxhInstance, NULL);
     SubclassRadioButton(radioButtons[i], this);
    
    SetBitmapDimensionEx(Choices[i]->ms_bitmap,

			 Choices[i]->GetWidth(),

			 Choices[i]->GetHeight(),

			 NULL);
    SendMessage((HWND)radioButtons[i],WM_CHANGEBITMAP,
                  (WPARAM)0xFFFF/*((Choices[i]->GetHeight()<<8)+Choices[i]->GetWidth())*/,
                  (LPARAM)Choices[i]->ms_bitmap);
    } else 
#else

      {

	 bm_labels[i] = NULL;
	 radioButtons[i] = wxwmCreateWindowEx(0, RADIO_CLASS, tmp,
					      groupStyle | RADIO_FLAGS, 0, 0, 0, 0,
					      the_handle, (HMENU)newId, wxhInstance, NULL);

    }
#if CTL3D
    Ctl3dSubclassCtl(radioButtons[i]);
#endif
	SubclassRadioButton(radionButtons[i]);
#endif
    HDC the_dc = GetWindowDC((HWND)radioButtons[i]) ;
    if (buttonFont && buttonFont->GetInternalFont(the_dc))
      SendMessage((HWND)radioButtons[i],WM_SETFONT,
                  (WPARAM)buttonFont->GetInternalFont(the_dc),0L);
    ReleaseDC((HWND)radioButtons[i],the_dc) ;
    subControls.Append((wxObject *)newId);
  }
//  (void)NewId() ;
  // Create a dummy radio control to end the group.
  (void)wxwmCreateWindowEx(0, RADIO_CLASS, "", 
			   WS_GROUP|RADIO_FLAGS, 0, 0, 0, 0,
			   the_handle, (HMENU)NewId(), wxhInstance, NULL);

  no_items = N;
  SetSelection(0);

  style = Title ? HAS_LABEL : 0;

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);
  Callback(func);

  if (Title) {
    if (_style & wxFIXED_LENGTH)
      SetLabel(Title);
  }
  if (the_label)
    delete [] the_label ;

  return TRUE;
}
// #endif

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

    delete[] bm_labels;

    bm_labels = NULL;

  }
  if (radioWidth)
    delete[] radioWidth ;
  if (radioHeight)
    delete[] radioHeight ;
  if (ms_handle)
    wxwmDestroyWindow(ms_handle) ;
  ms_handle = NULL ;

}

void wxRadioBox::SetBackgroundColour(wxColour* WXUNUSED(col))
{
}

void wxRadioBox::SetLabelColour(wxColour* WXUNUSED(col))
{
}

void wxRadioBox::SetButtonColour(wxColour* WXUNUSED(col))
{
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
  
#if FAFA_LIB && !CTL3D
  // This message will switch from FB_BITMAP style to FB_TEXT, if needed.
  SendMessage((HWND)radioButtons[item],WM_CHANGEBITMAP,
	      (WPARAM)0,
	      (LPARAM)NULL);
#endif
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

#if FAFA_LIB // && !CTL3D
  SetBitmapDimensionEx(bitmap->ms_bitmap,
		       bitmap->GetWidth(),
		       bitmap->GetHeight(),
		       NULL);
  SendMessage((HWND)radioButtons[item],WM_CHANGEBITMAP,
	      (WPARAM)0xFFFF/*((bitmap->GetHeight()<<8)+bitmap->GetWidth())*/,
	      (LPARAM)bitmap->ms_bitmap);
  radioWidth[item] = bitmap->GetWidth() + FB_MARGIN ;
  radioHeight[item] = bitmap->GetHeight() + FB_MARGIN ;
#endif
}

int wxRadioBox::FindString(char *s)
{
 int i;
 for (i = 0; i < no_items; i++)
 {
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

#if FAFA_LIB

	      isFafa?FAFA_SETCHECK:BM_SETCHECK, 

#else

	      BM_SETCHECK,

#endif

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
    GetWindowText(ms_handle, buf, 300);
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

    MoveWindow(ms_handle, x_offset, y_offset, totWidth, totHeight, TRUE);

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
  GetEventHandler()->OnSize(width, height);
}

void wxRadioBox::GetSize(int *width, int *height)
{
  RECT rect;
  rect.left = -1; rect.right = -1; rect.top = -1; rect.bottom = -1;

  if (static_label)
    wxFindMaxSize(static_label, &rect);

  if (ms_handle)
    wxFindMaxSize(ms_handle, &rect);

  int i;
  for (i = 0; i < no_items; i++)
    wxFindMaxSize(radioButtons[i], &rect);

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
    wxFindMaxSize(ms_handle, &rect);

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

    if (!GetWindowText(ms_handle, wxBuffer, 300))

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
    if (GetWindowText(ms_handle, wxBuffer, 300))
      SetWindowText(ms_handle, label);
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

  

  ShowWindow(ms_handle, cshow);
  

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
  if (item<0)
    wxRadioBox::Show(show) ;
  else if (item < no_items)
  {
    int cshow;
    if (show)
      cshow = SW_SHOW;
    else
      cshow = SW_HIDE;
    ShowWindow(radioButtons[item], cshow);
  }
}
