///////////////////////////////////////////////////////////////////////////////
// File:	wx_tabc.cc
// Purpose:	Panel item tab choice implementation (Macintosh version)
// Author:	Matthew
// Created:	2002
// Copyright:  (c) 2002, PLT
///////////////////////////////////////////////////////////////////////////////

#include "wx_tabc.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"

/* constants from Aqua interface guidelines */
#define TAB_TOP_SPACE 12
#define TAB_CONTROL_HEIGHT 30
#define TAB_CONTENT_MARGIN 2
#define TAB_TITLE_SPACE 20

static ControlHandle MakeTabs(CGrafPtr theMacGrafPort, int N, char **Choices, Rect *boundsRect)
{
  ControlTabEntry *array;
  ControlHandle cMacControl;
  int i;

  array = new ControlTabEntry[N];
  for (i = 0; i < N; i++) {
    array[i].icon = NULL;
    array[i].name = CFStringCreateWithCString(NULL, wxItemStripLabel(Choices[i]), kCFStringEncodingISOLatin1);
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
			 int N, char **Choices)
 : wxItem (panel, -1, -1, -1, -1, 0,  "tab-choice")
{
  int i;

  Callback(function);

  tab_count = N;
  tab_labels = Choices;

  font = buttonFont;
  
  SetCurrentMacDC();
  CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
  Rect boundsRect = {0, 0, 10, 10};
  OffsetRect(&boundsRect, SetOriginX, SetOriginY + TAB_TOP_SPACE);

  cMacControl = MakeTabs(theMacGrafPort, N, Choices, &boundsRect);
   
  CheckMemOK(cMacControl);
  
#if 0
  /* #^%$^&!!! GetBestControlrect doesn't work for tab widgets.
     And why should it? That would be entriely too helpful. */
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  OSErr err;
  err = ::GetBestControlRect(cMacControl,&r,&baselineOffset);

  cWindowWidth = r.right - r.left;
  cWindowHeight = r.bottom - r.top;
#else
  cWindowHeight = TAB_TOP_SPACE + TAB_CONTROL_HEIGHT + (2 * TAB_CONTENT_MARGIN);
  cWindowWidth = TAB_TITLE_SPACE;
  for (i = 0; i < N; i++) {
    float x, y;
    font->GetTextExtent(wxItemStripLabel(Choices[i]), &x, &y, NULL, NULL, 0, 1.0);
    cWindowWidth += TAB_TITLE_SPACE + (int)x;
  }
  padTop = TAB_TOP_SPACE;
  padBottom = cWindowHeight - TAB_TOP_SPACE - TAB_CONTROL_HEIGHT;
#endif

  ::SizeControl(cMacControl, cWindowWidth, TAB_CONTROL_HEIGHT);

  ::EmbedControl(cMacControl, GetRootControl());
  
  if (GetParent()->IsHidden())
    DoShow(FALSE);
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
    SetCurrentDC();
    ::SetControlValue(cMacControl, value - 1);
  }
}

//-----------------------------------------------------------------------------
Bool wxTabChoice::GetSelection(void)
{
  if (cMacControl) {
    short value = ::GetControlValue(cMacControl);
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

  padBottom = cWindowHeight - TAB_TOP_SPACE - TAB_CONTROL_HEIGHT;

  if (dW || dH) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), clientHeight - (padTop + padBottom));
  }

  if (dX || dY)
    MaybeMoveControls();

  if (!cHidden && (dW || dH || dX || dY)) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    Rect clientRect = {0, 0, clientHeight, clientWidth};
    OffsetRect(&clientRect,SetOriginX,SetOriginY);
    ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&clientRect);
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxTabChoice::Paint(void)
{
  if (cHidden) return;
  SetCurrentDC();
  Rect r = { 0, 0, cWindowHeight, cWindowWidth};
  ::OffsetRect(&r,SetOriginX,SetOriginY);
  if (cMacControl)
    ::Draw1Control(cMacControl);
}

//-----------------------------------------------------------------------------
void wxTabChoice::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (cMacControl) {
    SetCurrentDC();
    if (show)
      ::ShowControl(cMacControl);
    else
      ::HideControl(cMacControl);
  }
  
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxTabChoice::OnEvent(wxMouseEvent *event)
{
  if (event->LeftDown()) {
      SetCurrentDC();
      
      int startH;
      int startV;
      event->Position(&startH, &startV); // client c.s.
      
      Point startPt = {startV + SetOriginY, startH + SetOriginX}; // port c.s.
      int trackResult;
      if (::StillDown()) {
	if (cMacControl)
	  trackResult = ::TrackControl(cMacControl, startPt, NULL);
	else
	  trackResult = Track(startPt);
      } else
	trackResult = 1;
      if (trackResult) {
	  wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_TAB_CHOICE_COMMAND);
	  // SetValue(!GetValue()); // toggle checkbox
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
    for (i = 0; i < tab_count; i++)
      new_choices[i] = tab_labels[i];
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

  if (cMacControl)
    ::DisposeControl(cMacControl);
  cMacControl = naya;

  ::EmbedControl(cMacControl, GetRootControl());
  
  if (cHidden)
    ::HideControl(cMacControl);
  if (!cActive)
    DeactivateControl(cMacControl);
  if (!OS_Active())
    DisableControl(cMacControl);

  if (s && !cHidden) {
    /* for some reason, the toolbox is stupid about drawing
       the new control; force a redraw */
    Paint();
  }
}

void wxTabChoice::Delete(int i)
{
  if ((i >= 0) && (i < tab_count)) {
    for (i++; i < tab_count; i++) {
      tab_labels[i - 1] = tab_labels[i];
    }
    --tab_count;
    Append(NULL); /* refreshs the control */
  }
}

char *wxTabChoice::GetLabel()
{
  return "tab choice";
}

