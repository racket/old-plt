///////////////////////////////////////////////////////////////////////////////
// File:	wx_messg.cc
// Purpose:	Panel item message implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_item.h"
#include "wx_gdi.h"
#include "wx_messg.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxLabelArea.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
# include <TextEdit.h>
#endif

extern FSSpec wx_app_spec;

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentArea)
(
 wxArea*		parentArea,
 char*		label,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentArea, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, labelFont);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentArea, font)
(
 wxArea*		parentArea,
 char*		label,
 wxFont*		theFont,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentArea, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, theFont);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentPanel)
(
 wxPanel*	parentPanel,
 char*		label,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, labelFont);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentPanel, font)
(
 wxPanel*	parentPanel,
 char*		label,
 wxFont*		theFont,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  CreateWxMessage(label, theFont);
}

wxMessage::wxMessage // Constructor (given parentPanel and bitmap)
(
 wxPanel*	parentPanel,
 wxBitmap*	bitmap,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
    sBitmap = bitmap;
    sBitmap->selectedIntoDC++;
    cMessage = NULL;
    if (cStyle & wxBORDER) new wxBorderArea(this);
    SetClientSize(sBitmap->GetWidth(), sBitmap->GetHeight());
    if (GetParent()->IsHidden())
      DoShow(FALSE);
  } else
    CreateWxMessage("<bad-image>");
}

IconRef msg_icons[3];

wxMessage::wxMessage // Constructor (given parentPanel and icon id)
(
 wxPanel*	parentPanel,
 int            iconID,
 int 		x,
 int			y,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
  wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);
  switch (iconID) {
  case wxMSGICON_APP:
    if (!msg_icons[wxMSGICON_APP]) {
      SInt16 lbl;
      GetIconRefFromFile(&wx_app_spec, msg_icons + wxMSGICON_APP, &lbl);
    }
    break;
  case wxMSGICON_WARNING:
    if (!msg_icons[iconID]) {
      GetIconRef(kOnSystemDisk, 'macs', kAlertCautionIcon, msg_icons + iconID);
    }
    break;
  case wxMSGICON_ERROR:
    if (!msg_icons[iconID]) {
      GetIconRef(kOnSystemDisk, 'macs', kAlertStopIcon, msg_icons + iconID);
    }
    break;
    /* kAlertNoteIcon */
  default:
    CreateWxMessage("<bad-icon>");
    return;
  }

  if (msg_icons[iconID]) {
    icon_id = iconID;
    SetClientSize(64, 64);
    if (GetParent()->IsHidden())
      DoShow(FALSE);
  } else
    CreateWxMessage("<icon-missing>");
}


//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxMessage::~wxMessage(void)
{
  if (cMessage) delete [] cMessage;
  if (sBitmap) --sBitmap->selectedIntoDC;
  if (cParentArea->__type == wxTYPE_LABEL_AREA) {
    // CJC hack? clean up label area so it does point to us, since its 
    // about to go away.
    wxLabelArea *pa = (wxLabelArea *)cParentArea;
    pa->cLabelText = NULL;
  }
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxMessage::CreateWxMessage(char* label, wxFont* theFont) // common constructor initialization
{
  if (cStyle & wxBORDER) new wxBorderArea(this);
  sBitmap = NULL;
  cMessage = wxItemStripLabel(label);
	
  SetEraser(wxCONTROL_BACKGROUND_BRUSH);

  float clientWidth = 20;
  float clientHeight = 14;
  if (theFont) 
    font = theFont;
  if (!font) 
    font = wxNORMAL_FONT; // WCH: kludge
  if (font) {
    font->GetTextExtent(cMessage, &clientWidth, &clientHeight, NULL, NULL);
    if (font->GetStyle() != wxNORMAL)
      clientWidth += 5; //cjc - try hello.cc italic labels are truncated
  }
  SetClientSize((int)floor(clientWidth) + 3, (int)floor(clientHeight)); // mflatt: +3 is needed (even for plain)
	
  if (GetParent()->IsHidden())
    DoShow(FALSE);
}


//-----------------------------------------------------------------------------
void wxMessage::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxMessage::GetLabel(void)
{
  if (cMessage) {
    strcpy(wxBuffer, cMessage);
    return wxBuffer;
  } else
    return "";
}

//-----------------------------------------------------------------------------
void wxMessage::SetLabel(wxBitmap *bitmap)
{
  if (!sBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --sBitmap->selectedIntoDC;
  sBitmap = bitmap;
  sBitmap->selectedIntoDC++;
  
  // erase the old
  if (SetCurrentDC()) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    Rect clientRect = {0, 0, clientHeight, clientWidth};
    OffsetRect(&clientRect,SetOriginX,SetOriginY);
    ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&clientRect);

    //FIXME CJC	SetClientSize(sBitmap->GetWidth(), sBitmap->GetHeight());
    sBitmap->DrawMac();

    FlushDisplay();
  }
}

//-----------------------------------------------------------------------------
void wxMessage::SetLabel(char* label)
{
  if (sBitmap || icon_id) return;
  if (cMessage) delete [] cMessage;
  cMessage = macCopyString0(wxItemStripLabel(label));
  if (!cHidden) {
    Paint();
    FlushDisplay();
  }
}

//-----------------------------------------------------------------------------
void wxMessage::Paint(void)
{
  if (cHidden) return;

  if (SetCurrentDC()) {
    int clientWidth, clientHeight;
    GetClientSize(&clientWidth, &clientHeight);
    Rect clientRect = {0, 0, clientHeight, clientWidth};
    OffsetRect(&clientRect,SetOriginX,SetOriginY);
    ::EraseRect(&clientRect);
	  
    if (sBitmap) {
      sBitmap->DrawMac();
    } else if (icon_id) {
      Rect r = { SetOriginY, SetOriginX, 
		 SetOriginY + clientHeight, SetOriginX + clientWidth };
      PlotIconRef(&r, kAlignNone, kTransformNone, 0 /* kIconServicesDefaultUsageFlags */, msg_icons[icon_id]);
    } else if (font && (font != wxNORMAL_FONT)) {
      FontInfo fontInfo;
      ::GetFontInfo(&fontInfo);
      MoveTo(SetOriginX, fontInfo.ascent + SetOriginY); // move pen to start drawing text
      
      DrawLatin1Text(cMessage, 0, -1, 0);
    } else {
      Rect r = { SetOriginY, SetOriginX, 
		 SetOriginY + clientHeight, SetOriginX + clientWidth };
      CFStringRef str = CFStringCreateWithCString(NULL, cMessage, kCFStringEncodingISOLatin1);

      DrawThemeTextBox(str, kThemeSystemFont, kThemeStateActive,
		       0, &r, teJustLeft, NULL);

      CFRelease(str);
    }
  }
}

//-----------------------------------------------------------------------------
void wxMessage::DoShow(Bool show) 
{
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxMessage::ChangeToGray(Bool gray) 
{
  Refresh();
  wxWindow::ChangeToGray(gray);
}
