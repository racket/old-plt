///////////////////////////////////////////////////////////////////////////////
// File:	wx_gbox.cc
// Purpose:	Panel item tab choice implementation (Macintosh version)
// Author:	Matthew
// Created:	2002
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 2002, PLT
///////////////////////////////////////////////////////////////////////////////

#include "wx_gbox.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"

#define GBOX_EXTRA_SPACE 8
#define GBOX_EXTRA_H_SPACE 32

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxGroupBox::wxGroupBox(wxPanel *panel, char *label, int style)
 : wxItem (panel, -1, -1, -1, -1, style,  "group-box")
{
  CGrafPtr theMacGrafPort;
  Rect boundsRect = {0, 0, 10, 10};
  CFStringRef title;

  font = buttonFont;
  
  SetCurrentMacDC();
  theMacGrafPort = cMacDC->macGrafPort();
  OffsetRect(&boundsRect, SetOriginX, SetOriginY);
  
  if (label)
    title = CFStringCreateWithCString(NULL, label, kCFStringEncodingISOLatin1);
  else
    title = NULL;

  cMacControl = NULL;
  CreateGroupBoxControl(GetWindowFromPort(theMacGrafPort), &boundsRect, title, TRUE, &cMacControl);

  if (title)
    CFRelease(title);
   
  CheckMemOK(cMacControl);

#if 0
  /* #^%$^&!!! GetBestControlRect doesn't work for group widgets.
     And why should it? That would be entriely too helpful. */
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  OSErr err;
  err = ::GetBestControlRect(cMacControl, &r, &baselineOffset);
  cWindowWidth = r.right - r.left;
  cWindowHeight = (r.bottom - r.top) + GBOX_EXTRA_SPACE;
#else
  {
    float x, y;
    wxFont *bold;
    bold = new wxFont(font->GetPointSize(), font->GetFontId(), font->GetStyle(), wxBOLD, 0);
    bold->GetTextExtent(wxItemStripLabel(label), 0, &x, &y, NULL, NULL, 0, 1.0);
    cWindowWidth = (int)x + GBOX_EXTRA_H_SPACE;
    cWindowHeight = (int)y + GBOX_EXTRA_SPACE;
  }
#endif

  orig_height = cWindowHeight;

  ::SizeControl(cMacControl, cWindowWidth, cWindowHeight);

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
wxGroupBox::~wxGroupBox(void)
{
  if (cMacControl) {
    ::DisposeControl(cMacControl);
    cMacControl = NULL;
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxGroupBox::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
  if (!cMacControl)
    return;
  
  SetCurrentDC();

  if (dW || dH) {
    wxWindow *parent;
    int clientWidth, clientHeight;
    int pClientWidth, pClientHeight;

    GetClientSize(&clientWidth, &clientHeight);

    parent = GetParent();
    parent->GetClientSize(&pClientWidth, &pClientHeight);
    ::SizeControl(cMacControl, clientWidth - (padLeft + padRight), 
		  pClientHeight - (padTop + padBottom));
  }

  if (dX || dY) {
    MaybeMoveControls();
  }

  if (!cHidden && (dW || dH || dX || dY)) {
    Refresh();
  }
}

void wxGroupBox::MaybeMoveControls(void)
{
  wxItem::MaybeMoveControls();
}

void wxGroupBox::Refresh(void)
{
  wxWindow::Refresh();
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxGroupBox::Paint(void)
{
  /* Shouldn't get here */
}

//-----------------------------------------------------------------------------
void wxGroupBox::DoShow(Bool show)
{
  wxItem::DoShow(show);
  Paint(); /* to paint custom control */
  Refresh(); /* in case an update is in progress */
}

void wxGroupBox::ChangeToGray(Bool gray)
{
  wxItem::ChangeToGray(gray);
  Paint(); /* to paint custom control */
  Refresh(); /* in case an update is in progress */
}

void wxGroupBox::Activate(Bool on)
{
  wxItem::Activate(on);
}

char *wxGroupBox::GetLabel()
{
  return "group box"; /* not right, but I don't think anyone cares */
}

void wxGroupBox::SetLabel(char *label)
{
  if (cMacControl) {
    SetCurrentDC();
    {
      CFStringRef llabel;
      llabel = CFStringCreateWithCString(NULL, wxItemStripLabel(label), kCFStringEncodingISOLatin1);
      SetControlTitleWithCFString(cMacControl, llabel);
      CFRelease(llabel);

      Paint(); /* to paint custom control */
      Refresh(); /* in case an update is in progress */
    }
  }
}

