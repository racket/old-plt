///////////////////////////////////////////////////////////////////////////////
// File:	wx_tabc.cc
// Purpose:	Panel item tab choice implementation (Macintosh version)
// Author:	Matthew
// Created:	2002
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 2002, PLT
///////////////////////////////////////////////////////////////////////////////

#include "wx_tabc.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"

static int OS_103 = -1;

/* constants from Aqua interface guidelines */
#define TAB_TOP_SPACE (OS_103 ? 7 : 12)
#define TAB_CONTROL_HEIGHT (OS_103 ? 25 : 30)
#define TAB_CONTENT_MARGIN 2
#define TAB_BOTTOM_EXTRA_MARGIN 3
#define TAB_TITLE_SPACE 24
#define TAB_BASE_SIDE_SPACE 16
#define TAB_PANE_OVERLAP (OS_103 ? 6 : 7)
#define TAB_PANE_CLIP_OVERLAP (OS_103 ? 9 : 3)

static ControlHandle MakeTabs(CGrafPtr theMacGrafPort, int N, char **Choices, Rect *boundsRect)
{
  ControlTabEntry *array;
  ControlHandle cMacControl;
  int i;

  if (OS_103 < 0) { 
    long r;
    Gestalt(gestaltSystemVersion, &r);
    OS_103 = (((r >> 4) & 0xF) > 2);
  }

#ifdef MZ_PRECISE_GC
  array = (ControlTabEntry *)GC_malloc_atomic(sizeof(ControlTabEntry) * N);
#else
  array = new ControlTabEntry[N];
#endif
  for (i = 0; i < N; i++) {
    CFStringRef cfstr;
    array[i].icon = NULL;
    cfstr = wxCFString(wxItemStripLabel(Choices[i]));
    array[i].name = cfstr;
    array[i].enabled = TRUE;
  }

  cMacControl = NULL;
  CreateTabsControl(GetWindowFromPort(theMacGrafPort), boundsRect, 
		    kControlTabSizeLarge, kControlTabDirectionNorth,
		    N, array, &cMacControl);
  for (i = 0; i < N; i++) {
    CFRelease(array[i].name);
  }

  return cMacControl;
}

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxTabChoice::wxTabChoice(wxPanel *panel, wxFunction function, char *label, 
			 int N, char **Choices, int style)
 : wxItem (panel, -1, -1, -1, -1, style,  "tab-choice")
{
  int i;
  CGrafPtr theMacGrafPort;
  Rect boundsRect = {0, 0, 10, 10};

  Callback(function);

  tab_count = N;
  tab_labels = Choices;

  font = buttonFont;
  
  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
  OffsetRect(&boundsRect, SetOriginX, SetOriginY + TAB_TOP_SPACE);

  /* FIXME: style & wxBORDER */

  cMacControl = MakeTabs(theMacGrafPort, N, Choices, &boundsRect);
   
  CheckMemOK(cMacControl);

#if 0
  /* #^%$^&!!! GetBestControlRect doesn't work for tab widgets.
     And why should it? That would be entriely too helpful. */
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  OSErr err;
  err = ::GetBestControlRect(cMacControl,&r,&baselineOffset);

  cWindowWidth = r.right - r.left;
  cWindowHeight = r.bottom - r.top;
#else
  cWindowHeight = TAB_TOP_SPACE + TAB_CONTROL_HEIGHT + TAB_CONTENT_MARGIN + TAB_BOTTOM_EXTRA_MARGIN + 5;
  cWindowWidth = TAB_TITLE_SPACE + TAB_BASE_SIDE_SPACE;
  for (i = 0; i < N; i++) {
    double x, y;
    font->GetTextExtent(wxItemStripLabel(Choices[i]), 0, &x, &y, NULL, NULL, TRUE);
    cWindowWidth += TAB_TITLE_SPACE + (int)x;
  }
  padTop = TAB_TOP_SPACE;
#endif

  padLeft = padRight = padBottom = 2;

  phantom_height = -1;

  ::SizeControl(cMacControl, 
		cWindowWidth - (padLeft + padRight), 
		(style & wxBORDER) ? (cWindowHeight - padBottom) : TAB_CONTROL_HEIGHT);

  ::EmbedControl(cMacControl, GetRootControl());
  
  {
    wxWindow*p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxTabChoice::~wxTabChoice(void)
{
  if (cMacControl) {
    ::DisposeControl(cMacControl);
    cMacControl = NULL;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Item methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


//-----------------------------------------------------------------------------
void wxTabChoice::SetSelection(Bool value)
{
  if (cMacControl) {
    ::SetControlValue(cMacControl, value + 1);
  }
}

//-----------------------------------------------------------------------------
Bool wxTabChoice::GetSelection(void)
{
  if (cMacControl) {
    short value;
    value = ::GetControlValue(cMacControl);
    return value - 1;
  } else
    return -1;
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxTabChoice::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (!cMacControl)
    return;
  
  if (dW || dH) {
    int clientWidth, clientHeight;

    GetClientSize(&clientWidth, &clientHeight);

    if (cStyle & wxBORDER) {
      wxWindow *parent;
      int pw, ph;

      if (phantom_height > 0) {
	ph = phantom_height;
	pw = 0;
      } else {
	parent = GetParent();
	parent->GetClientSize(&pw, &ph);
      }
      ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		    ph - (padTop + padBottom));
    } else {
      ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		    TAB_CONTROL_HEIGHT);
    }
  }

  if (dX || dY) {
    MaybeMoveControls();
  }
}

void wxTabChoice::MaybeMoveControls(void)
{
  wxItem::MaybeMoveControls();
}

void wxTabChoice::Refresh(void)
{
  wxItem::Refresh();
}

void wxTabChoice::SetPhantomSize(int w, int h)
{
  phantom_height = h;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxTabChoice::Paint(void)
{
}

//-----------------------------------------------------------------------------
void wxTabChoice::DoShow(Bool show)
{
  if (show) {
    ::ShowControl(cMacControl);
  } else {
    ::HideControl(cMacControl);
  }
  wxItem::DoShow(show);
}

void wxTabChoice::ChangeToGray(Bool gray)
{
  wxItem::ChangeToGray(gray);
}

void wxTabChoice::Activate(Bool on)
{
  wxItem::Activate(on);
}

//-----------------------------------------------------------------------------
void wxTabChoice::OnEvent(wxMouseEvent *event)
{
  if (event->LeftDown()) {
    int startH;
    int startV;
    Point startPt;
    int trackResult;

    SetCurrentDC();
      
    event->Position(&startH, &startV); // client c.s.
      
    startPt.v = startV - padTop;
    startPt.h = startH - padLeft;

    wxTracking();
    if (cMacControl)
      trackResult = ::TrackControl(cMacControl, startPt, NULL);
    else
      trackResult = Track(startPt);

    if (trackResult) {
      wxCommandEvent *commandEvent;
      commandEvent = new wxCommandEvent(wxEVENT_TYPE_TAB_CHOICE_COMMAND);
      ProcessCommand(commandEvent);
    }
  }
}


int wxTabChoice::Number(void) { 
  return tab_count;
}

void wxTabChoice::Append(char *s, int new_sel)
{
  char **new_choices;
  int i;  
  Rect r;
  ControlHandle naya;
  int ox, oy;

  if (new_sel < 0)
    new_sel = GetSelection();

  if (s) {
    new_choices = new char*[tab_count + 1];
    for (i = 0; i < tab_count; i++) {
      new_choices[i] = tab_labels[i];
    }
    new_choices[i] = s;
    tab_labels = new_choices;
    tab_count++;
  }

  GetWinOrigin(&ox, &oy);
  
  r.top = padTop + ox;
  r.bottom = r.top + TAB_CONTROL_HEIGHT;
  r.left = oy + padLeft;
  r.right = r.left + cWindowWidth - (padLeft + padRight);

  naya = MakeTabs(cMacDC->macGrafPort(), tab_count, tab_labels, &r);

  if (cMacControl) {
    ::DisposeControl(cMacControl);
  }
  cMacControl = naya;

  ::EmbedControl(cMacControl, GetRootControl());
  
  if (cHidden) {
    ::HideControl(cMacControl);
  }
  if (!cActive)
    DeactivateControl(cMacControl);
  if (!OS_Active()) {
#ifdef OS_X
    DisableControl(cMacControl);
#else
    HiliteControl(cMacControl, 255);
#endif
  }

  if (new_sel >= 0)
    SetSelection(new_sel);

  OnClientAreaDSize(1, 1, 1, 1);
}

void wxTabChoice::Delete(int i)
{
  if ((i >= 0) && (i < tab_count)) {
    int sel;

    sel = GetSelection();
    sel = ((sel <= i) ? sel : (sel ? sel - 1 : 0));

    for (i++; i < tab_count; i++) {
      tab_labels[i - 1] = tab_labels[i];
    }
    --tab_count;

    Append(NULL, sel); /* refreshes the control */
  }
}

void wxTabChoice::SetLabel(int i, char *s)
{
  if ((i >= 0) && (i < tab_count)) {
    tab_labels[i] = s;
    Append(NULL); /* refreshes the control */
  }
}

char *wxTabChoice::GetLabel()
{
  return "tab choice";
}

void wxTabChoice::Set(int N, char **Choices)
{
  int sel;
  sel = GetSelection();
  tab_count = N;
  tab_labels = Choices;
  if (sel >= N)
    sel = N - 1;
  Append(NULL, sel); /* refreshes the control */
}
