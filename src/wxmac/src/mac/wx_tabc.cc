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
#define TAB_TITLE_SPACE 20
#define TAB_PANE_OVERLAP (OS_103 ? 6 : 7)
#define TAB_PANE_CLIP_OVERLAP (OS_103 ? 9 : 3)

static pascal void userPaneDrawFunction(ControlRef controlRef, SInt16 thePart);
static ControlUserPaneDrawUPP userPaneDrawFunctionUPP = NewControlUserPaneDrawUPP(userPaneDrawFunction); 

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
    cfstr = CFStringCreateWithCString(NULL, wxItemStripLabel(Choices[i]), kCFStringEncodingISOLatin1);
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

  if (style & wxBORDER) {
    Rect pRect;
    
    pRect.left = boundsRect.left;
    pRect.right = boundsRect.right;
    pRect.top = boundsRect.bottom;
    pRect.right = boundsRect.bottom + 20;
    pane = NULL;
    CreateUserPaneControl(GetWindowFromPort(theMacGrafPort), &pRect, 0, &pane);
    CheckMemOK(pane);
  } else
    pane = NULL;

  SetControlData(pane, kControlEntireControl, kControlUserPaneDrawProcTag | kControlSupportsEmbedding, 
		 sizeof(userPaneDrawFunctionUPP), (Ptr)&userPaneDrawFunctionUPP);

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
  cWindowWidth = TAB_TITLE_SPACE;
  for (i = 0; i < N; i++) {
    float x, y;
    font->GetTextExtent(wxItemStripLabel(Choices[i]), 0, &x, &y, NULL, NULL, 0, 1.0);
    cWindowWidth += TAB_TITLE_SPACE + (int)x;
  }
  padTop = TAB_TOP_SPACE;
#endif

  ::SizeControl(cMacControl, cWindowWidth, TAB_CONTROL_HEIGHT);

  ::EmbedControl(cMacControl, GetRootControl());

  if (pane) {
    ::EmbedControl(pane, GetRootControl());

    padBottom = TAB_CONTENT_MARGIN + TAB_BOTTOM_EXTRA_MARGIN;
    padLeft = padRight = TAB_CONTENT_MARGIN;
  }
  
  {
    wxWindow*p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
    if (pane)
      ((wxPanel *)p)->paneControl = pane;
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
  if (pane) {
    ::DisposeControl(pane);
    pane = NULL;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Item methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


//-----------------------------------------------------------------------------
void wxTabChoice::SetSelection(Bool value)
{
  if (cMacControl) {
    SetCurrentDC();
    ::SetControlValue(cMacControl, value - 1);
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
  
  SetCurrentDC();

  if (dW || dH) {
    wxWindow *parent;
    int clientWidth, clientHeight;
    int pClientWidth, pClientHeight;

    GetClientSize(&clientWidth, &clientHeight);

    ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), TAB_CONTROL_HEIGHT);
    if (pane) {
      parent = GetParent();
      parent->GetClientSize(&pClientWidth, &pClientHeight);
      ::SizeControl(pane, clientWidth - (padLeft + padRight), 
		    pClientHeight
		    - (padTop + padBottom) 
		    - (TAB_CONTROL_HEIGHT - TAB_PANE_OVERLAP - TAB_CONTENT_MARGIN));
    }
  }

  if (dX || dY) {
    MaybeMoveControls();
  }

  if (!cHidden && (dW || dH || dX || dY)) {
    Refresh();
  }
}

void wxTabChoice::MaybeMoveControls(void)
{
  SetCurrentDC();
  if (pane)
    MoveControl(pane, SetOriginX + padLeft, SetOriginY + padTop + TAB_CONTROL_HEIGHT - TAB_PANE_OVERLAP);
  wxItem::MaybeMoveControls();
  Refresh();
}

void wxTabChoice::Refresh(void)
{
  if (cHidden) return;

  if (SetCurrentMacDC()) {
    int clientWidth, clientHeight;
    Rect clientRect;

    GetClientSize(&clientWidth, &clientHeight);
    clientHeight += 4;
    ::SetRect(&clientRect, 0, 0, clientWidth, clientHeight);
    OffsetRect(&clientRect, SetOriginX, SetOriginY);
    
    if (pane) {
      wxWindow *parent;
      int pClientWidth, pClientHeight;
      parent = GetParent();
      parent->GetClientSize(&pClientWidth, &pClientHeight);
      
      clientRect.left -= 2;
      clientRect.bottom = clientRect.top + pClientHeight + 4;
      clientRect.right += 2;
    }
    
    ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()), &clientRect);
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

#ifndef OS_X
# define IsControlEnabled(x) 1
#endif

static void userPaneDrawFunction(ControlRef controlRef, SInt16 thePart)
{
  Rect itemRect;

  GetControlBounds(controlRef, &itemRect);
  if (OS_103)
    DrawThemePrimaryGroup(&itemRect, (IsControlEnabled(controlRef) 
				      && IsControlActive(controlRef)));
  else
    DrawThemeTabPane(&itemRect, (IsControlEnabled(controlRef) 
				 && IsControlActive(controlRef)));
} 

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

//-----------------------------------------------------------------------------
void wxTabChoice::Paint(void)
{
  if (cHidden) return;
  if (SetCurrentDC()) {
    if (cMacControl) {
      ::Draw1Control(cMacControl);
    }
    if (pane) {
      RgnHandle clipRgn, innerRgn, oldClipRgn;
      Rect itemRect;

      clipRgn = NewRgn();
      oldClipRgn = NewRgn();
      innerRgn = NewRgn();

      GetClip(oldClipRgn);

      /* Clip midway through overlap to avoid drawing on
         the tab, and clip out the inside to avoid
         drawing on contained items. */
      GetControlBounds(pane, &itemRect);
      itemRect.top += TAB_PANE_CLIP_OVERLAP;
      itemRect.left -= 2;
      itemRect.right += 2;
      itemRect.bottom += 3;
      RectRgn(clipRgn, &itemRect);
      itemRect.top += 11 - 3;
      itemRect.left += 3 + 2;
      itemRect.right -= 2 + 3;
      itemRect.bottom -= 2 + 3;
      RectRgn(innerRgn, &itemRect);
      DiffRgn(clipRgn, innerRgn, clipRgn);
      SetClip(clipRgn);

      ::EraseRgn(clipRgn);
      ::Draw1Control(pane);

      SetClip(oldClipRgn);

      DisposeRgn(clipRgn);
      DisposeRgn(oldClipRgn);
      DisposeRgn(innerRgn);
    }
  }
}

//-----------------------------------------------------------------------------
void wxTabChoice::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (cMacControl) {
    SetCurrentDC();
    if (show) {
      ::ShowControl(cMacControl);
      if (pane) {
	::ShowControl(pane);
      }
    } else {
      ::HideControl(cMacControl);
      if (pane) {
	::HideControl(pane);
      }
    }
  }
  
  wxWindow::DoShow(show);
}

void wxTabChoice::ChangeToGray(Bool gray)
{
  if (cMacControl) {
    SetCurrentDC();
    if (gray) {
#ifdef OS_X
      DisableControl(cMacControl);
      if (pane)
	DisableControl(pane);
#else
      HiliteControl(cMacControl, 255);
      if (pane)
	HiliteControl(pane, 255);
#endif
    } else {
#ifdef OS_X
      EnableControl(cMacControl);
      if (pane)
	EnableControl(pane);
#else
      HiliteControl(cMacControl, 0);
      if (pane)
	HiliteControl(pane, 0);
#endif
    }
  }
  
  Paint(); /* to paint custom control */
  Refresh(); /* in case an update is in progress */

  wxWindow::ChangeToGray(gray);
}

void wxTabChoice::Activate(Bool on)
{
  if (cMacControl) {
    SetCurrentDC();
    if (!on) {
      DeactivateControl(cMacControl);
      if (pane)
	DeactivateControl(pane);
    } else {
      ActivateControl(cMacControl);
      if (pane)
	ActivateControl(pane);
    }
  }

  Paint(); /* to paint custom control */
  Refresh(); /* in case an update is in progress */

  wxWindow::Activate(on);
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
      
    startPt.v = startV + SetOriginY; // port c.s.
    startPt.h = startH + SetOriginX;

    if (::StillDown()) {
      wxTracking();
      if (cMacControl)
	trackResult = ::TrackControl(cMacControl, startPt, NULL);
      else
	trackResult = Track(startPt);
      Paint(); /* This is the handler thread; can't be in update */
    } else
      trackResult = 1;
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

void wxTabChoice::Append(char *s)
{
  char **new_choices;
  int i;  
  Rect r;
  ControlHandle naya;

  if (s) {
    new_choices = new char*[tab_count + 1];
    for (i = 0; i < tab_count; i++) {
      new_choices[i] = tab_labels[i];
    }
    new_choices[i] = s;
    tab_labels = new_choices;
    tab_count++;
  }

  SetCurrentMacDC();
  
  r.top = padTop + SetOriginY;
  r.bottom = r.top + TAB_CONTROL_HEIGHT;
  r.left = SetOriginX;
  r.right = r.left + cWindowWidth;

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

  if (s && !cHidden) {
    Paint();
    Refresh(); /* in case an update is in progress */
  }
}

void wxTabChoice::Delete(int i)
{
  if ((i >= 0) && (i < tab_count)) {
    for (i++; i < tab_count; i++) {
      tab_labels[i - 1] = tab_labels[i];
    }
    --tab_count;
    Append(NULL); /* refreshes the control */
  }
}

char *wxTabChoice::GetLabel()
{
  return "tab choice";
}

