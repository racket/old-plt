/*
 * File:        wx_choic.cc
 * Purpose:     Choice item implementation (Mac version)
 * Author:      Julian Smart/Cecil Coupe
 * Created:     1993
 * Updated:	April 1995
 *   July 22, 1995 - First Mac version - Cecil Coupe
 *
 * Copyright:   (c) 2004 PLT Scheme, Inc.
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */


#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#ifdef __GNUG__
#pragma implementation "wx_choic.h"
#endif
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_choic.h"
#include "wx_menu.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#ifndef WX_CARBON
# include <Controls.h>
#endif

// Because I never get this right and t,l,b,r makes sense to me - CJC
//
#define SetBounds(rect, top, left, bottom, right) ::SetRect(rect, left, top, right, bottom)
#define VSLOP 0
#define HSLOP 4
#define PAD_Y 4
#define PAD_X 2
#define MSPACEY 1


wxChoice::wxChoice()
{
  /* dummy - not used */
}

wxChoice::wxChoice (wxPanel * panel, wxFunction func, char *Title,
		    int x, int y, int width, int height, int N, char **Choices,
		    long style, char *name
		    ):
		    wxbChoice (panel, func, Title, x, y, width, height, N, Choices, style, name)
{
  Create (panel, func, Title, x, y, width, height, N, Choices, style, name);
}

#define max(x, y) ((x > y) ? x : y)

Bool wxChoice::
Create (wxPanel * panel, wxFunction func, char *Title,
	int x, int y, int width, int height, int N, char **Choices,
	long style, char *name)
{
  float fWidth, fHeight, fDescent, fLeading;
  int lblw, lblh;
  int maxdfltw;
  int maxdflth;
  int n,w,h;
  Str255 temp;
  OSErr err;
  Rect r = {0,0,0,0};
  SInt16 baselineOffset; // ignored
  
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  window_parent = panel;
  labelPosition = panel->label_position;
  windowStyle = style;
  valueFont = buttonFont ? buttonFont : wxNORMAL_FONT;
  Callback (func);
  padLeft = padRight = PAD_X;
  padTop = padBottom = PAD_Y;
  
  SetCurrentMacDC();
  /*	if (!buttonFont)
	buttonFont = wxNORMAL_FONT;
	*/
  if (!labelFont)
    labelFont = wxNORMAL_FONT;

  if (Title)
    Title = wxItemStripLabel(Title);

  fWidth = 50.0;
  fHeight = 12.0;
  fDescent = 0.0;
  fLeading = 0.0;
  labelbase = 9;
  if (Title) {
    int n;
    char *naya_s;
    GetTextExtent(Title, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
    if (fHeight < 12) fHeight = 12; 
    n = strlen(Title);
    naya_s = new char[n+1];
    sTitle = (StringPtr)naya_s;
    sTitle[0] = n;
    memcpy(&sTitle[1], Title, n);
    labelbase = (int)(fDescent + fLeading);
  } else  {
    sTitle = NULL;
    fWidth = fHeight = 0;
  }
  lblw = (int)(fWidth + (Title ? HSLOP : 0));
  lblh = (int)(fHeight + (Title ? 2 : 0));
    
  if (labelPosition == wxVERTICAL) {
    padTop += lblh + VSLOP;
  } else{
    padLeft += lblw;
  }
  
  if (N) {
    maxdfltw = 30;
  } else {
    // No strings - pick something reasonable - say 60 pixels
    maxdfltw = 60;
  }

  GetTextExtent("Test", &fWidth, &fHeight, &fDescent, &fLeading, buttonFont);
  maxdflth = (int)fHeight;
  valuebase = (int)(fDescent + fLeading);

  // Build the menu and find the width of the longest string
  PopUpID = wxMenu::gMenuIdCounter++;
  hDynMenu = NewMenu(PopUpID, "\p");
  CheckMemOK(hDynMenu);
  for (n = 0; n < N; n++) {
    // attempt to size control by width of largest string
    GetTextExtent(Choices[n], &fWidth, &fHeight, &fDescent, &fLeading, buttonFont);
    w = (int)fWidth;
    h = (int)fHeight;
    maxdfltw = max(w, maxdfltw);
    maxdflth = max(h, maxdflth);
    CopyCStringToPascal(Choices[n],temp);
    ::AppendMenu(hDynMenu, "\ptemp");
    ::SetMenuItemText(hDynMenu, n + 1, temp);
    ::SetMenuItemTextEncoding(hDynMenu, n + 1, kCFStringEncodingISOLatin1);
  }
  no_strings = N;

  // First, create the control with a bogus rectangle;
  err = ::CreatePopupButtonControl(GetWindowFromPort(cMacDC->macGrafPort()), &r, NULL, -12345,
				   FALSE, 0, 0, normal, &cMacControl);
  err = ::SetControlData(cMacControl, kControlNoPart, kControlPopupButtonOwnedMenuRefTag,
			 sizeof(MenuRef), (void *)(&hDynMenu));
  ::SetControlMinimum(cMacControl, 1);
  ::SetControlMaximum(cMacControl, no_strings);
  
  // Now, ignore the font data and let the control find the "best" size 
  err = ::GetBestControlRect(cMacControl, &r, &baselineOffset);
  maxdfltw = r.right - r.left + (PAD_X * 2);
  maxdflth = r.bottom - r.top + (PAD_Y * 2);

  // compute the Rects that contain everything.
  // note valuebase and labelbase are changed from font descents
  // to number of pixels to substract from the rect bottom.
  if (labelPosition == wxVERTICAL) {
    w = max(lblw, maxdfltw);
    if (Title) {
      SetBounds(&TitleRect, 1, 1, lblh+1, w);
    } else {
      SetBounds(&TitleRect, 1, 1, 1, 1);
    }
    SetBounds(&ValueRect, TitleRect.bottom + VSLOP, 1,
	      TitleRect.bottom + maxdflth + 1, maxdfltw);
    valuebase += MSPACEY;
    SetBounds(&CtlRect, 0, 0, ValueRect.bottom+2, w+1);
  } else {
    int bot;
    h = max(lblh, maxdflth);
    SetBounds(&ValueRect, 1, lblw+1, h-1, maxdfltw+lblw+1);
    valuebase += MSPACEY;
    SetBounds(&CtlRect, 0, 0, h+1, lblw+maxdfltw+2);
    bot = (h - valuebase) + labelbase;
    SetBounds(&TitleRect, bot-lblh, 1, bot, lblw);
  }

  if (width < 0 && height < 0) {
    // use the sizes we just calced
    cWindowWidth = CtlRect.right;
    cWindowHeight = CtlRect.bottom;
  } else {
    OnClientAreaDSize((width == -1 ? 0 : width),
		      (height == -1 ? 0 : height), 
		      (x == -1 ? 0 : x), 
		      (y == -1 ? 0 : y));
  }

  SetSelection(0);

  ::EmbedControl(cMacControl, GetRootControl());

  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();

  return TRUE;
}

int wxChoice::Number(void)
{
  return no_strings;
}

wxChoice::~wxChoice (void)
{
  if (cMacControl) {
    ::DisposeControl(cMacControl);
  }
  cMacControl = NULL;
}

// ---------Draw the Choice Control -----------
void wxChoice::DrawChoice(Bool active)
{
  SetCurrentDC();
  
  if (sTitle) {
    Rect r = { SetOriginY + TitleRect.top - 2, SetOriginX + TitleRect.left, 
	       SetOriginY + TitleRect.bottom - 2, SetOriginX + TitleRect.right };

    ::EraseRect(&r);

    if (font && (font != wxNORMAL_FONT)) {
      FontInfo fontInfo;
      ::GetFontInfo(&fontInfo);
      MoveTo(SetOriginX, fontInfo.ascent + SetOriginY); // move pen to start drawing text
      DrawLatin1Text((char *)sTitle, 1, sTitle[0], 0);
    } else {
      CFStringRef str;
      str = CFStringCreateWithCString(NULL, wxP2C(sTitle), kCFStringEncodingISOLatin1);
      
      DrawThemeTextBox(str, kThemeSystemFont, kThemeStateActive,
		       0, &r, teJustLeft, NULL);
      
      CFRelease(str);
    }
  }
  
  ::Draw1Control(cMacControl);
}

void wxChoice::ChangeToGray(Bool gray)
{
  wxItem::ChangeToGray(gray);
  DrawChoice(TRUE);
}

// --------- Event Handling -------------------
void wxChoice::Paint(void)
{
  if (cHidden) return;
  DrawChoice(TRUE);
  wxWindow::Paint();
}

// Resize and/or Move the Control
void wxChoice::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  int clientWidth, clientHeight;

  SetCurrentDC();

  if (1) {
    GetClientSize(&clientWidth, &clientHeight);
    if (clientWidth != CtlRect.right) {
      int needw = CtlRect.right - clientWidth;
      if (labelPosition == wxVERTICAL) {
	// Shrink width of both Title and Value
	if (sTitle)
	  TitleRect.right -= needw;
	ValueRect.right -= needw;
      } else {
	ValueRect.right -= needw;
      }
      CtlRect.right = clientWidth;
    } 
    if (clientHeight != CtlRect.bottom) {
      int needh = CtlRect.bottom - clientHeight;
      if (labelPosition == wxVERTICAL) {
	// Shrink heights equally
	if (sTitle) {
	  TitleRect.bottom -= needh/2;
	  ValueRect.top -= needh/2;
	}
	ValueRect.bottom -= needh;
      } else {
	// Shrink heights eqally
	if (sTitle)
	  TitleRect.bottom -= needh;
	ValueRect.bottom -= needh;
      }
      CtlRect.bottom = clientHeight;
    }

    ::SizeControl(cMacControl, 
		  ValueRect.right - ValueRect.left, 
		  ValueRect.bottom - ValueRect.top);
  }

  padTop = ValueRect.top+1;
  padLeft = ValueRect.left;
  MaybeMoveControls();
}

//-----------------------------------------------------------------------------
void wxChoice::DoShow(Bool show)
{
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------


void wxChoice::OnEvent(wxMouseEvent *event) // mac platform only
{
  if (event->LeftDown() && (no_strings > 0))
    {
      int startH, startV;
      Point startPt;

      SetCurrentDC();
      
      event->Position(&startH, &startV); // client c.s.

      startPt.v = startV + SetOriginY; // port c.s.
      startPt.h = startH + SetOriginX;

      if (::StillDown()) {
	int trackResult;
	wxTracking();
	trackResult = TrackControl(cMacControl,startPt,(ControlActionUPP)-1);
	if (trackResult) {
	  wxCommandEvent *commandEvent;
	  selection = GetControlValue(cMacControl);
	  selection -= 1;
	  commandEvent = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
	  ProcessCommand(commandEvent);
	}
      }
    }
}


// ------------ Methods available to user ------------

void wxChoice::Append (char *Item)
{
  Str255 s;
  int n;
  n = strlen(Item);
  memcpy(&s[1], Item, n);
  s[0] = n;
  ::InsertMenuItem(hDynMenu, "\ptemp", no_strings);
  ::SetMenuItemText(hDynMenu, no_strings + 1, s);
  ::SetMenuItemTextEncoding(hDynMenu, no_strings + 1, kCFStringEncodingISOLatin1);
  no_strings++;
  ::SetControlMinimum(cMacControl,1);
  ::SetControlMaximum(cMacControl,no_strings);
  Paint();
  Refresh(); /* in case an update is in progress */
}

void wxChoice::Clear (void)
{
  int n;
  for (n = 0; n < no_strings; n++){
    ::DeleteMenuItem(hDynMenu, 1);
  }
  no_strings = 0;
  selection = 0;
  ::SetControlMinimum(cMacControl,0);
  ::SetControlMaximum(cMacControl,0);        
  Paint();
  Refresh(); /* in case an update is in progress */
}


int wxChoice::GetSelection (void)
{
  return selection;
}

void wxChoice::SetSelection (int n)
{
  if ((n < 0) || (n >= no_strings))
    return;

  ::SetControlValue(cMacControl,n+1);
  selection = n;
  DrawChoice(TRUE);
}

int wxChoice::FindString (char *s)
{
  int i;
  Str255 ps;
  Str255 temp;
  CopyCStringToPascal(s,temp);
  for (i = 0; i < no_strings; i++) {
    ::GetMenuItemText(hDynMenu, i+1, ps);
    if (!CompareString(ps,temp,NULL))
      return i;
  }
  return -1;
}

char *wxChoice::GetString (int n)
{
  Str255 s;
  if (n < 0 || n >= no_strings)
    return NULL; // dummy
  ::GetMenuItemText(hDynMenu, n+1, s);
  CopyPascalStringToC(s, wxBuffer);
  return copystring(wxBuffer);
}


void wxChoice::SetBackgroundColour(wxColour*col)
{
} 

void wxChoice::SetLabelColour(wxColour*col)
{
}

void wxChoice::SetButtonColour(wxColour*col) 
{
}

char* wxChoice::GetLabel(void)
{
  int n;
  if (sTitle && (n = sTitle[0])) {
    CopyPascalStringToC(sTitle, wxBuffer);
    return wxBuffer;
  }
  else
    return NULL;
}

void wxChoice::SetLabel(char *label)
{
  char *s;
  long len;

  label = wxItemStripLabel(label);
  len = strlen(label);
  s = new char[len + 1];
  sTitle = (StringPtr)s;
  CopyCStringToPascal(label, sTitle);
  
  if (SetCurrentDC()) {
    Rect r = TitleRect;
    OffsetRect(&r,SetOriginX,SetOriginY);
    EraseRect(&r);
    Paint();
    Refresh(); /* in case an update is in progress */
  }
}
