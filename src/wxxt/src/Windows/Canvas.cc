/*								-*- C++ -*-
 *
 * Purpose: canvas panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "Canvas.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxCanvas
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_ScrollWinWidget
#define  Uses_CanvasWidget
#include "widgets.h"

#include "wxgl.h"
#ifdef USE_GL
extern Visual *wxGetGLWindowVisual();
Visual *wx_common_use_visual;
#endif

//-----------------------------------------------------------------------------
// create and destroy canvas
//-----------------------------------------------------------------------------

wxCanvas::wxCanvas(wxWindow *parent, int x, int y, int width, int height,
		   int style, char *name) : wxItem()
{
    __type = wxTYPE_CANVAS;

    h_size = h_units = v_size = v_units = 1;
    h_units_per_page = v_units_per_page = 50;

    Create((wxPanel *)parent, x, y, width, height, style, name);
}

Bool wxCanvas::Create(wxPanel *panel, int x, int y, int width, int height,
		      int style, char *name)
{
    wxWindow_Xintern *ph;
    Widget wgt;
    ChainToPanel(panel, style, name);

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateWidget
	(name, xfwfEnforcerWidgetClass, ph->handle,
	 XtNbackground,  wxGREY_PIXEL,
	 XtNforeground,  wxBLACK_PIXEL,
	 XtNfont,        label_font->GetInternalFont(),
	 XtNtraversalTranslationDone, TRUE,
	 XtNhighlightThickness, 0,
	 XtNframeWidth, 0,
	 NULL);
    if (!(style & wxINVISIBLE))
      XtManageChild(wgt);
    X->frame = wgt;
    // create scrolled area
    wgt = XtVaCreateManagedWidget
	("viewport", xfwfScrolledWindowWidgetClass, X->frame,
	 XtNhideHScrollbar, TRUE,
	 XtNhideVScrollbar, TRUE,
	 XtNtraversalTranslationDone, TRUE,
	 XtNframeWidth, ((style & wxBORDER) ? 1 : 0),
	 XtNframeType, XfwfPlain,
	 XtNshadowWidth, 0,
	 XtNshadowScheme, XfwfBlack,
	 XtNhighlightThickness, ((style & wxNO_CAPTION) ? 2 : 0),
	 XtNspacing, 0,
	 XtNbackground,  wxGREY_PIXEL,
	 NULL);
    X->scroll = wgt;
    // create canvas
#ifdef USE_GL
    wx_common_use_visual = wxGetGLWindowVisual();
#endif
    wgt = XtVaCreateManagedWidget
      ("canvas", xfwfCanvasWidgetClass, X->scroll,
       XtNbackingStore, (style & wxBACKINGSTORE) ? Always : NotUseful,
       XtNborderWidth,  0,
       XtNbackground,  wxWHITE_PIXEL,
       XtNhighlightThickness, 0,
       XtNframeWidth, 0,
       XtNtraversalOn, FALSE,
       NULL);
#ifdef USE_GL
    wx_common_use_visual = NULL;
#endif
    X->handle = wgt;
    // In case this window or the parent is hidden; we
    // need windows to create DCs
    XtRealizeWidget(X->frame);
    XtRealizeWidget(X->scroll);
    XtRealizeWidget(X->handle);

    // Initialize CanvasDC
    CreateDC();
    dc->SetBackground(wxWHITE); // white brush as default for canvas background
    // position in panel
    panel->PositionItem(this, x, y,
			(width  > -1 ? width  : /* wxCANVAS_WIDTH */ 0),
			(height > -1 ? height : /* wxCANVAS_HEIGHT */ 0));

#if 0
    // resize canvas
    Position xx, yy; int ww, hh;
    XfwfCallComputeInside(X->scroll, &xx, &yy, &ww, &hh);
    h_size = ww; v_size = hh; SetScrollArea(h_size, v_size);
#endif

    if (style & (wxHSCROLL | wxVSCROLL)) {
      EnableScrolling(style & wxHSCROLL, style & wxVSCROLL);
      SetScrollbars(style & wxHSCROLL, style & wxVSCROLL,
		    0, 0, 1, 1, 0, 0, FALSE);
    }

    // add event handlers
    AddEventHandlers();
    /* MATTHEW */
    // propagate key events from frame to canvas widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);

    if (style & wxINVISIBLE)
      Show(FALSE);

    // ready
    return TRUE;
}

wxCanvas::~wxCanvas(void)
{
}

void wxCanvas::SetBackgroundToGray(void)
{
  XtVaSetValues(X->handle, XtNbackground,  wxGREY_PIXEL, NULL);

}

//-----------------------------------------------------------------------------
// handle scrollbars, pointer, and virtual size
//-----------------------------------------------------------------------------

void wxCanvas::GetVirtualSize(int *x, int *y)
{
    Dimension ww, hh;
    XtVaGetValues(X->handle, XtNwidth, &ww, XtNheight, &hh, NULL);
    *x = ww; *y = hh;
}

void wxCanvas::GetRefreshSize(int *w, int *h)
{
  GetVirtualSize(w, h);
}

void wxCanvas::Scroll(int x_pos, int y_pos)
{
  if (misc_flags & 8) {
    /* Not managing */
    wxItem::Scroll(x_pos, y_pos);
  } else {
    /* Managing */
    /* Get the actual scroll step, which is the client size, rather
       than h_units/v_unit */
    int cw, ch;
    GetClientSize(&cw, &ch);
    wxItem::Scroll(x_pos * cw, y_pos * ch);
  }
}

void wxCanvas::ScrollPercent(float x, float y)
{
  if (misc_flags & 8) {
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
    
    wxItem::Scroll(xp, yp);
  }
}

void wxCanvas::SetScrollbars(int h_pixels, int v_pixels, int x_len, int y_len,
			     int x_page, int y_page, int x_pos, int y_pos,
			     Bool setVirtualSize)
{
    if (!(GetWindowStyleFlag() & wxHSCROLL))
      h_pixels = -1;
    if (!(GetWindowStyleFlag() & wxVSCROLL))
      v_pixels = -1;

    if (x_len < 1) h_pixels = -1;
    if (y_len < 0) v_pixels = -1;

    if (setVirtualSize) {
      Arg a[4];

      XtVaSetValues(X->scroll, XtNautoAdjustScrollbars, 1, NULL);

      misc_flags -= (misc_flags & 8);

      if (h_pixels > 0) {
	h_units          = h_pixels;
	h_size           = h_units * x_len;
	h_units_per_page = hs_page = x_page;
	hs_width         = x_len;
      } else
	hs_width = 0;
      if (v_pixels > 0) {
	v_units          = v_pixels;
	v_size           = v_units * y_len;
	v_units_per_page = vs_page = y_page;
	vs_width         = y_len;
      } else
	vs_width = 0;
       
      /* MATTHEW: [5] size = 0 safety: */
      if (!h_size)
	h_size = 1;
      if (!v_size)
	v_size = 1;

      // adjust size and position of canvas
      a[0].name = XtNabs_height;
      a[0].value = v_pixels > 0 ? ((Dimension)v_size) : 0;
      a[1].name = XtNrel_height;
      *(float *)&(a[1].value) = (float)(v_pixels > 0 ? 0.0 : 1.0);
      a[2].name = XtNabs_width;
      a[2].value = h_pixels > 0 ? ((Dimension)h_size) : 0;
      a[3].name = XtNrel_width;
      *(float *)&(a[3].value) = (float)(h_pixels > 0 ? 0.0 : 1.0);

      XtSetValues(X->handle, a, 4);

      wxItem::Scroll(x_pos, y_pos);
      // set scroll steps of scrollbars
      if (X->scroll) {
	XtVaSetValues(X->scroll,
		      XtNhScrollAmount, h_units,
		      XtNvScrollAmount, v_units,
		      0);
      }
    } else {
      Arg a[8];

      XtVaSetValues(X->scroll, XtNautoAdjustScrollbars, 0, NULL);

      a[0].name = XtNabs_height;
      a[0].value = 0;
      a[1].name = XtNrel_height;
      *(float *)&(a[1].value) = 1.0;
      a[2].name = XtNabs_width;
      a[2].value = 0;
      a[3].name = XtNrel_width;
      *(float *)&(a[3].value) = 1.0;
      a[4].name = XtNabs_x;
      a[4].value = 0;
      a[5].name = XtNabs_y;
      a[5].value = 0;
      a[6].name = XtNrel_x;
      *(float *)&(a[6].value) = 0.0;
      a[7].name = XtNrel_y;
      *(float *)&(a[7].value) = 0.0;

      XtSetValues(X->handle, a, 8);

      misc_flags |= 8;
      if (h_pixels > 0) {
	hs_width = x_len;
	hs_page = x_page;
	SetScrollPos(wxHORIZONTAL, x_pos);
      } else {
	hs_width = 0;
	hs_page = 1;
	SetScrollPos(wxHORIZONTAL, 0);
      }

      if (v_pixels > 0) {
	vs_width = y_len;
	vs_page = y_page;
	SetScrollPos(wxVERTICAL, y_pos);
      } else {
	vs_width = 0;
	vs_page = 1;
	SetScrollPos(wxVERTICAL, 0);
      }
    }
}

void wxCanvas::ViewStart(int *x, int *y)
{
    Position xx, yy;

    XtVaGetValues(X->handle, XtNx, &xx, XtNy, &yy, NULL);
    *x = -xx; *y = -yy;
}

void wxCanvas::WarpPointer(int x, int y)
{
  /* MATTHEW: [5] scroll => handle */
  XWarpPointer(XtDisplay(X->handle), None, XtWindow(X->handle), 0, 0, 0, 0, x, y);
}

//-----------------------------------------------------------------------------
// handle scrolling with keys
//-----------------------------------------------------------------------------

void wxCanvas::OnChar(wxKeyEvent *event)
{
    int start_x, start_y;

    ViewStart(&start_x, &start_y);

    switch (event->KeyCode()) {
    case WXK_PRIOR:
	Scroll(start_x, max(0, start_y-v_units_per_page));
	break;
    case WXK_NEXT:
	Scroll(start_x, start_y + v_units_per_page);
	break;
    case WXK_UP:
	if (start_y >= 1)
	    Scroll(start_x, start_y - 1);
	break;
    case WXK_DOWN:
	Scroll(start_x, start_y + 1);
	break;
    case WXK_LEFT:
	if (event->ControlDown()) {
	    Scroll(max(0, start_x-h_units_per_page), start_y);
	} else {
	    if (start_x >= 1)
		Scroll(start_x - 1, start_y);
	}
	break;
    case WXK_RIGHT:
	if (event->ControlDown())
	    Scroll(start_x + h_units_per_page, start_y);
	else
	    Scroll(start_x + 1, start_y);
	break;
    case WXK_HOME:
	Scroll(0, 0);
	break;
    }
}

