///////////////////////////////////////////////////////////////////////////////
// File:	wx_canvs.cc
// Purpose:	wxCanvas implementation
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_canvs.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_area.h"
#include "wx_sbar.h"
#include "wxScroll.h"
#include "wx_frame.h"
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif 
#include "wxScrollArea.h"
#include "wxBorderArea.h"


//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxCanvas::wxCanvas // Constructor (given parentFrame)
(
 wxFrame*	parentFrame,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbCanvas (windowName, parentFrame, x, y, width, height, style)
{
  InitDefaults();
}


//-----------------------------------------------------------------------------
wxCanvas::wxCanvas // Constructor (given parentArea)
(
 wxArea*		parentArea,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbCanvas (windowName, parentArea, x, y, width, height, style)
{
  InitDefaults();

  wx_dc = new wxCanvasDC(this);
}

//-----------------------------------------------------------------------------
wxCanvas::wxCanvas // Constructor (given parentWindow)
(
 wxWindow*	parentWindow,
 int 		x,
 int			y,
 int			width,
 int			height,
 long		style,
 char*		windowName,
 WXTYPE		objectType
 ) :
 wxbCanvas (windowName, parentWindow, x, y, width, height, style)
{
  InitDefaults();
}


//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxCanvas::~wxCanvas(void)
{
  if (wx_dc) delete wx_dc;
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxCanvas::InitDefaults(void)
{
  units_x = 0;
  units_y = 0;
  units_per_page_x = 0;
  units_per_page_y = 0;
  hExtent = 0;
  vExtent = 0;
  hScrollingEnabled = TRUE;
  vScrollingEnabled = TRUE;
  scrollAutomanaged = TRUE;

  wx_dc = new wxCanvasDC(this);

  if (cStyle & wxBORDER) {
    int direction = Direction::wxAll;
    if (cStyle & wxVSCROLL)
      direction -= Direction::wxRight;
    if (cStyle & wxHSCROLL)
      direction -= Direction::wxBottom;

    wxBorderArea *border = new wxBorderArea(this, 1, direction);
  }

  if (cStyle & wxVSCROLL || cStyle & wxHSCROLL)
    {
      wxScrollData* scrollData = new wxScrollData;
      cScroll = new wxScroll(this, scrollData);
      new wxScrollArea(this, this, (cStyle & wxVSCROLL) | (cStyle & wxHSCROLL));
    }
  
  if (GetParent()->IsHidden())
    DoShow(FALSE);
}

void wxCanvas::AddWhiteRgn(RgnHandle rgn) 
{
  if (wxSubType(__type, wxTYPE_PANEL))
    wxWindow::AddWhiteRgn(rgn);
  else {
    int theRootX, theRootY, w, h;
    RgnHandle wrgn;
    cClientArea->FrameContentAreaOffset(&theRootX, &theRootY);
    GetClientSize(&w, &h);
    if ((wrgn = NewRgn())) {
      SetRectRgn(wrgn, theRootX, theRootY, theRootX + w, theRootY + h);
      UnionRgn(rgn, wrgn, rgn);
      DisposeRgn(wrgn);
    }
  }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxCanvas::SetColourMap(wxColourMap* cmap)
{
}

#define max(x, y) ((x > y) ? x : y)

//-----------------------------------------------------------------------------
void wxCanvas::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  // update deviceContext
  if (wx_dc) {
    int clientWidth = ClientArea()->Width();
    int clientHeight= ClientArea()->Height();
    Rect paintRect = {0, 0, clientHeight, clientWidth};
    wx_dc->SetPaintRegion(&paintRect);
  }
  
  if (cScroll && scrollAutomanaged
      && (requested_x_step_size > 0 || requested_y_step_size > 0))
    SetScrollbars(requested_x_step_size, requested_y_step_size,
		  hExtent / max(1, requested_x_step_size), vExtent / max(1, requested_y_step_size),
		  1, 1,
		  cScroll->GetScrollData()->GetValue(wxWhatScrollData::wxPositionH),
		  cScroll->GetScrollData()->GetValue(wxWhatScrollData::wxPositionV));
}

//-----------------------------------------------------------------------------
// horizontal/vertical: number of pixels per unit (e.g. pixels per text line)
// x/y_length:        : no. units per scrollbar
// x/y_page:          : no. units per page scrolled
//-----------------------------------------------------------------------------
void wxCanvas::SetScrollbars(int horizontal, int vertical,
                             int x_length, int y_length,
                             int x_page, int y_page,
                             int x_pos, int y_pos,
			     Bool automgmt)
{
  if (!cScroll) 
    return;
  
  if (!(cStyle & wxHSCROLL) || (x_length <= 0))
    horizontal = -1;
  if (!(cStyle & wxVSCROLL) || (y_length <= 0))
    vertical = -1;
  
  if (automgmt) {
    if (horizontal >= 0) {
      horizontal = max(horizontal, 1);
      hExtent = horizontal * x_length;
    } else
      hExtent = 0;
    if (vertical >= 0) {
      vertical = max(vertical, 1);
      vExtent = vertical * y_length;
    } else
      vExtent = 0;
  } else {
    hExtent = 0;
    vExtent = 0;
  }
  
  if (automgmt) {
    requested_x_step_size = horizontal;
    requested_y_step_size = vertical;
    
    int tw, th, w, h, dw, dh;
    GetClientSize(&w, &h);
    if (hExtent) {
      tw = hExtent;
      dw = tw - w;
      if (dw <= 0) {
	horizontal = -1;
      } else {
	x_length = (int)ceil((double)dw / horizontal);
	x_page = (int)floor((double)w / horizontal);
      }
    }
    if (vExtent) {
      th = vExtent;
      dh = th - h;
      if (dh <= 0) {
	vertical = -1;
      } else {
	y_length = (int)ceil((double)dh / vertical);
	y_page = (int)floor((double)h / vertical);
      }
    }
  }
  
  if (x_pos > x_length) x_pos = x_length;
  if (y_pos > y_length) y_pos = y_length;

  scrollAutomanaged = automgmt; //mflatt

  wxWhatScrollData whatScrollData; // track what scrolldata changes
  wxScrollData* oldScrollData = cScroll->GetScrollData();
  wxScrollData scrollData;
  if (oldScrollData) scrollData = *oldScrollData;

  int sizeH = (vertical > 0 ? max(y_length, 1) : 0);
  if (sizeH != scrollData.GetValue(wxWhatScrollData::wxSizeH))
    {
      scrollData.SetValue(sizeH, wxWhatScrollData::wxSizeH);
      whatScrollData |= wxWhatScrollData::wxSizeH;
    }

  int unitH = (vertical > 0 ? vertical : 0);
  if (unitH != scrollData.GetValue(wxWhatScrollData::wxUnitH))
    {
      scrollData.SetValue(unitH, wxWhatScrollData::wxUnitH);
      whatScrollData |= wxWhatScrollData::wxUnitH;
    }

  if (vertical < 0) y_page = 1;
  if (y_page != scrollData.GetValue(wxWhatScrollData::wxPageH))
    {
      scrollData.SetValue(y_page, wxWhatScrollData::wxPageH);
      whatScrollData |= wxWhatScrollData::wxPageH;
    }

  if (vertical < 0) y_pos = 0;
  if (y_pos != scrollData.GetValue(wxWhatScrollData::wxPositionV))
    {
      scrollData.SetValue(y_pos, wxWhatScrollData::wxPositionV);
      whatScrollData |= wxWhatScrollData::wxPositionV;
    }

  int sizeW = (horizontal > 0 ? max(x_length, 1) : 0);
  if (sizeW != scrollData.GetValue(wxWhatScrollData::wxSizeW))
    {
      scrollData.SetValue(sizeW, wxWhatScrollData::wxSizeW);
      whatScrollData |= wxWhatScrollData::wxSizeW;
    }

  int unitW = (horizontal > 0 ? horizontal : 0);
  if (unitW != scrollData.GetValue(wxWhatScrollData::wxUnitW))
    {
      scrollData.SetValue(unitW, wxWhatScrollData::wxUnitW);
      whatScrollData |= wxWhatScrollData::wxUnitW;
    }

  if (horizontal < 0) x_page = 1;
  if (x_page != scrollData.GetValue(wxWhatScrollData::wxPageW))
    {
      scrollData.SetValue(x_page, wxWhatScrollData::wxPageW);
      whatScrollData |= wxWhatScrollData::wxPageW;
    }

  if (horizontal < 0) x_pos = 0;
  if (x_pos != scrollData.GetValue(wxWhatScrollData::wxPositionH))
    {
      scrollData.SetValue(x_pos, wxWhatScrollData::wxPositionH);
      whatScrollData |= wxWhatScrollData::wxPositionH;
    }

  if ((long)whatScrollData != 0)
    cScroll->SetScrollData(&scrollData, whatScrollData, NULL);	
}

//-----------------------------------------------------------------------------
void wxCanvas::SetScrollData
(
 wxScrollData*		scrollData,
 wxWhatScrollData	whatScrollData,
 wxScrollEvent*		evnt
 )
{
  // if (iniatorWindow == this) return;

  if ((long)whatScrollData & wxWhatScrollData::wxSizeW)
    units_x = scrollData->GetValue(wxWhatScrollData::wxSizeW);

  if ((long)whatScrollData & wxWhatScrollData::wxSizeH)
    units_y = scrollData->GetValue(wxWhatScrollData::wxSizeH);

  if ((long)whatScrollData & wxWhatScrollData::wxUnitW)
    horiz_units = scrollData->GetValue(wxWhatScrollData::wxUnitW);

  if ((long)whatScrollData & wxWhatScrollData::wxUnitH)
    vert_units = scrollData->GetValue(wxWhatScrollData::wxUnitH);

  if ((long)whatScrollData & wxWhatScrollData::wxPageW)
    units_per_page_x = scrollData->GetValue(wxWhatScrollData::wxPageW);

  if ((long)whatScrollData & wxWhatScrollData::wxPageH)
    units_per_page_y = scrollData->GetValue(wxWhatScrollData::wxPageH);

  wxDC* theDC = GetDC();

  if (!scrollAutomanaged) {
    if (theDC) {
      theDC->device_origin_x = 0;
      theDC->device_origin_y = 0;
      // ((wxCanvasDC *)theDC)->SetCanvasClipping();
    }
    if (evnt) {
      OnScroll(evnt);
    }
    return;
  }

  if (theDC)
    {
      int dH = 0;
      {
	int newH = scrollData->GetValue(wxWhatScrollData::wxPositionH) *
	  scrollData->GetValue(wxWhatScrollData::wxUnitW);
	dH = (int)(newH - (-theDC->device_origin_x));
      }
      
      int dV = 0;
      {
	int newV = scrollData->GetValue(wxWhatScrollData::wxPositionV) *
	  scrollData->GetValue(wxWhatScrollData::wxUnitH);
	dV = (int)(newV - (-theDC->device_origin_y));
      }
      
      if (dH != 0 || dV != 0)
	{
	  wxArea* clientArea = ClientArea();
	  RgnHandle theUpdateRgn = ::NewRgn();
	  CheckMemOK(theUpdateRgn);
	  theDC->BeginDrawing();
	  if (!IsHidden()) {
	    Rect scrollRect = {0, 0, clientArea->Height(), clientArea->Width()};
	    OffsetRect(&scrollRect,SetOriginX,SetOriginY);
	    ::ScrollRect(&scrollRect, -dH, -dV, theUpdateRgn);
	    ::InvalWindowRgn(GetWindowFromPort(cMacDC->macGrafPort()),theUpdateRgn);
	  }
	  theDC->device_origin_x += -dH;
	  theDC->device_origin_y += -dV;
	  // ((wxCanvasDC *)theDC)->SetCanvasClipping();
	  theDC->EndDrawing();
	  ::DisposeRgn(theUpdateRgn);
	}

      Refresh();
    }
}

//-----------------------------------------------------------------------------
void wxCanvas::GetScrollUnitsPerPage(int* x_page, int* y_page)
{
  *x_page = units_per_page_x;
  *y_page = units_per_page_y;
}

//-----------------------------------------------------------------------------
// Scroll to given position (scroll position, not pixel position)
//-----------------------------------------------------------------------------
void wxCanvas::Scroll(int xPos, int yPos)
{
  if (!cScroll) return;

  wxWhatScrollData whatScrollData; // track what scrolldata changes
  wxScrollData* oldScrollData = cScroll->GetScrollData();
  wxScrollData scrollData;
  if (oldScrollData) scrollData = *oldScrollData;

  if (xPos != -1)
    {
      if (xPos != scrollData.GetValue(wxWhatScrollData::wxPositionH))
	{
	  scrollData.SetValue(xPos, wxWhatScrollData::wxPositionH);
	  whatScrollData |= wxWhatScrollData::wxPositionH;
	}
    }

  if (yPos != -1)
    {
      if (yPos != scrollData.GetValue(wxWhatScrollData::wxPositionV))
	{
	  scrollData.SetValue(yPos, wxWhatScrollData::wxPositionV);
	  whatScrollData |= wxWhatScrollData::wxPositionV;
	}
    }

  if ((long)whatScrollData != 0)
    cScroll->SetScrollData(&scrollData, whatScrollData, NULL);
}

void wxCanvas::ScrollPercent(float x, float y)
{
  if (!scrollAutomanaged) {
    /* Not managing  - do nothing */
  } else {
    /* Managing */
    int xp, yp, vw, vh, cw, ch;
    GetVirtualSize(&vw, &vh);
    GetClientSize(&cw, &ch);

    if (vw > cw)
      vw -= cw;
    else
      vw = 0;
    if (vh > ch)
      vh -= ch;
    else
      vh = 0;

    if (x >= 0)
      xp = (int)floor(x * vw);
    else
      xp = -1;
    
    if (y >= 0)
      yp = (int)floor(y * vh);
    else
      yp = -1;
    
    Scroll(xp, yp);
  }

}

//-----------------------------------------------------------------------------
void wxCanvas::EnableScrolling(Bool x_scroll, Bool y_scroll)
{
  hScrollingEnabled = x_scroll;
  vScrollingEnabled = y_scroll;
}

//-----------------------------------------------------------------------------
int wxCanvas::GetScrollsPerPage(int orientation) // mac platform only
{
  return (orientation == wxHSCROLL ? units_per_page_x : units_per_page_y);
}

//-----------------------------------------------------------------------------
void wxCanvas::GetVirtualSize(int* x, int* y)
{
  int x1, y1;
  GetClientSize(&x1, &y1);
  if (hExtent == 0)
    *x = x1;
  else
    *x = hExtent;

  if (vExtent == 0)
    *y = y1;
  else
    *y = vExtent;
}

//-----------------------------------------------------------------------------
// Where the current view starts from
//-----------------------------------------------------------------------------
void wxCanvas::ViewStart(int* x, int* y)
{
  wxDC* theDC = GetDC();
  if (theDC) {
    *x = (int)(-(theDC->device_origin_x));
    *y = (int)(-(theDC->device_origin_y));
  } else
    *x = *y = 0;
}

//-----------------------------------------------------------------------------
void wxCanvas::WarpPointer(int x_pos, int y_pos)
{
  // Move the pointer to (x_pos,y_pos) coordinates. They are expressed in
  // pixel coordinates, relatives to the canvas -- So, we only need to
  // substract origin of the window.

  if (GetDC())
    {
      x_pos += (int)(GetDC()->device_origin_x) ;
      y_pos += (int)(GetDC()->device_origin_y) ;
    }
}

//-----------------------------------------------------------------------------
void wxCanvas::DoShow(Bool show)
{
  if (!CanShow(show)) return;

  if (show)
    wxWindow::DoShow(show);

  wxChildNode* node = GetChildren()->First();
  while (node) {
    wxWindow* theChildWindow = (wxWindow*)node->Data();
    theChildWindow->DoShow(show);
    node = node->Next();
  }

  if (!show)
    wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxCanvas::ClientToLogical(int* x, int* y) // mac platform only; testing
{ // Transform point from client c.s. to logical c.s. (virtual canvas, scrolling)
  // trying without all this gunk:
  wxDC* theDC = GetDC();
  if (theDC)
    {
      float fX = theDC->DeviceToLogicalX(*x);
      float fY = theDC->DeviceToLogicalY(*y);
      *x = (int)fX;
      *y = (int)fY;
    }
}

Bool wxCanvas::WantsFocus(void)
{
  return !cHidden;
}


void wxCanvas::SetScrollPage(int dir, int val)
{
  if (scrollAutomanaged) return;

  wxCanvas::SetScrollbars(horiz_units, vert_units, 
			  units_x, units_y,
			  (dir == wxHORIZONTAL) ? val : units_per_page_x,
			  (dir == wxVERTICAL) ? val : units_per_page_y,
			  GetScrollPos(wxHORIZONTAL), GetScrollPos(wxVERTICAL), scrollAutomanaged);
}

void wxCanvas::SetScrollRange(int dir, int val)
{
  if (scrollAutomanaged) return;

  wxCanvas::SetScrollbars((dir == wxHORIZONTAL) ? (val > 0) : horiz_units,
			  (dir == wxVERTICAL) ? (val > 0) : vert_units,
			  (dir == wxHORIZONTAL) ? val : units_x,
			  (dir == wxVERTICAL) ? val : units_y,
			  units_per_page_x, units_per_page_y, 
			  GetScrollPos(wxHORIZONTAL), GetScrollPos(wxVERTICAL), scrollAutomanaged);
}

void wxCanvas::SetScrollPos(int dir, int val)
{
  if (scrollAutomanaged) return;

  wxCanvas::SetScrollbars(horiz_units, vert_units, 
			  units_x, units_y,
			  units_per_page_x, units_per_page_y,
			  (dir == wxHORIZONTAL) ? val : GetScrollPos(wxHORIZONTAL),
			  (dir == wxVERTICAL) ? val : GetScrollPos(wxVERTICAL),
			  scrollAutomanaged);
}

int wxCanvas::GetScrollPos(int dir)
{
  if (scrollAutomanaged) return 0;

  if (!cScroll) return 0;
  
  return cScroll->GetScrollData()->GetValue((dir == wxHORIZONTAL) ? wxWhatScrollData::wxPositionH
					    : wxWhatScrollData::wxPositionV);
}

int wxCanvas::GetScrollPage(int dir)
{
  if (scrollAutomanaged) return 0;

  if (!cScroll) return 1;
  
  if (!GetScrollRange(dir)) return 0;
  
  return cScroll->GetScrollData()->GetValue((dir == wxHORIZONTAL) ? wxWhatScrollData::wxPageW
					    : wxWhatScrollData::wxPageH);
}
int wxCanvas::GetScrollRange(int dir)
{
  if (scrollAutomanaged) return 0;

  if (!cScroll) return 0;
  
  return cScroll->GetScrollData()->GetValue((dir == wxHORIZONTAL) ? wxWhatScrollData::wxSizeW
					    : wxWhatScrollData::wxSizeH);
}



void wxCanvas::Paint(void)
{
  wxWindow::Paint();
  OnPaint();
}

void wxCanvas::OnPaint(void)
{
  /* Do nothing */
}

void wxCanvas::CanvasSwapBuffers(void)
{
}

void wxCanvas::ThisContextCurrent(void)
{
}
