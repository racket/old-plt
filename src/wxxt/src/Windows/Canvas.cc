/*								-*- C++ -*-
 * $Id: Canvas.cc,v 1.1 1996/01/10 14:57:03 markus Exp $
 *
 * Purpose: canvas panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
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

//-----------------------------------------------------------------------------
// create and destroy canvas
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxCanvas, wxItem)

wxCanvas::wxCanvas(void) : wxItem()
{
    __type = wxTYPE_CANVAS;

    h_size = h_units = v_size = v_units = 1;
    h_units_per_page = v_units_per_page = 50;
}

wxCanvas::wxCanvas(wxPanel *parent, int x, int y, int width, int height,
		   int style, char *name) : wxItem()
{
    __type = wxTYPE_CANVAS;

    h_size = h_units = v_size = v_units = 1;
    h_units_per_page = v_units_per_page = 50;

    Create(parent, x, y, width, height, style, name);
}

Bool wxCanvas::Create(wxPanel *panel, int x, int y, int width, int height,
		      int style, char *name)
{
    ChainToPanel(panel, style, name);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNtraversalTranslationDone, TRUE,
	 XtNhighlightThickness, 0,
	 XtNframeWidth, 0,
	 NULL);
    // create scrolled area
    X->scroll = XtVaCreateManagedWidget
	("viewport", xfwfScrolledWindowWidgetClass, X->frame,
	 XtNhideHScrollbar, TRUE,
	 XtNhideVScrollbar, TRUE,
	 XtNtraversalTranslationDone, TRUE,
	 XtNframeWidth, 0,
	 XtNshadowWidth, 0,
	 XtNhighlightThickness, ((style & wxBORDER) ? 2 : 0),
	 XtNspacing, 0,
	 XtNbackground,  bg->GetPixel(cmap),
	 NULL);
    // create canvas
    X->handle = XtVaCreateManagedWidget
	("canvas", xfwfCanvasWidgetClass, X->scroll,
	 XtNbackingStore, (style & wxBACKINGSTORE) ? Always : NotUseful,
	 XtNborderWidth,  0,
	 XtNbackground,  wxWHITE->GetPixel(),
	 XtNhighlightThickness, 0,
	 XtNframeWidth, 0,
	 XtNtraversalOn, FALSE,
	 NULL);
    // Initialize CanvasDC
    CreateDC();
    dc->SetBackground(wxWHITE_BRUSH); // white brush as default for canvas background
    // position in panel
    panel->PositionItem(this, x, y,
			(width  > -1 ? width  : wxCANVAS_WIDTH),
			(height > -1 ? height : wxCANVAS_HEIGHT));

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

    
    // ready
    return TRUE;
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

void wxCanvas::Scroll(int x_pos, int y_pos)
{
    wxItem::Scroll(x_pos*h_units, y_pos*v_units);
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
      XtVaSetValues(X->scroll, XtNautoAdjustScrollbars, 1, NULL);

      misc_flags -= (misc_flags & 8);

      if (h_pixels > 0) {
	h_units          = h_pixels;
	h_size           = h_units * x_len;
	h_units_per_page = x_page;
      }
      if (v_pixels > 0) {
	v_units          = v_pixels;
	v_size           = v_units * y_len;
	v_units_per_page = y_page;
      }
       
      /* MATTHEW: [5] size = 0 safety: */
      if (!h_size)
	h_size = 1;
      if (!v_size)
	v_size = 1;

      // adjust size and position of canvas
      Arg a[4];
      a[0].name = XtNabs_height;
      a[0].value = v_pixels > 0 ? Dimension(v_size) : 0;
      a[1].name = XtNrel_height;
      *(float *)&(a[1].value) = (float)(v_pixels > 0 ? 0.0 : 1.0);
      a[2].name = XtNabs_width;
      a[2].value = h_pixels > 0 ? Dimension(h_size) : 0;
      a[3].name = XtNrel_width;
      *(float *)&(a[3].value) = (float)(h_pixels > 0 ? 0.0 : 1.0);

      XtSetValues(X->handle, a, 4);

      Scroll(x_pos, y_pos);
      // set scroll steps of scrollbars
      if (X->scroll) {
	XtVaSetValues(X->scroll,
		      XtNhScrollAmount, h_units,
		      XtNvScrollAmount, v_units,
		      0);
      }
    } else {
      XtVaSetValues(X->scroll, XtNautoAdjustScrollbars, 0, NULL);

      Arg a[4];
      a[0].name = XtNabs_height;
      a[0].value = 0;
      a[1].name = XtNrel_height;
      *(float *)&(a[1].value) = 1.0;
      a[2].name = XtNabs_width;
      a[2].value = 0;
      a[3].name = XtNrel_width;
      *(float *)&(a[3].value) = 1.0;

      XtSetValues(X->handle, a, 4);

      misc_flags |= 8;
      if (h_pixels > 0) {
	SetScrollRange(wxHORIZONTAL, x_len);
	SetScrollPage(wxHORIZONTAL, x_page);
	SetScrollPos(wxHORIZONTAL, x_pos);
      } else {
	SetScrollRange(wxHORIZONTAL, 0);
	SetScrollPage(wxHORIZONTAL, 1);
	SetScrollPos(wxHORIZONTAL, 0);
      }

      if (v_pixels > 0) {
	SetScrollRange(wxVERTICAL, y_len);
	SetScrollPage(wxVERTICAL, y_page);
	SetScrollPos(wxVERTICAL, y_pos);
      } else {
	SetScrollRange(wxVERTICAL, 0);
	SetScrollPage(wxVERTICAL, 1);
	SetScrollPos(wxVERTICAL, 0);
      }
    }
}

void wxCanvas::ViewStart(int *x, int *y)
{
    Position xx, yy;

    XtVaGetValues(X->handle, XtNx, &xx, XtNy, &yy, NULL);
    *x = -xx/h_units; *y = -yy/v_units;
}

void wxCanvas::WarpPointer(int x, int y)
{
  /* MATTHEW: [5] scroll => handle */
  XWarpPointer(XtDisplay(X->handle), None, XtWindow(X->handle), 0, 0, 0, 0, x, y);
}

//-----------------------------------------------------------------------------
// misc
//-----------------------------------------------------------------------------

void wxCanvas::ChangeColours(void)
{
    wxItem::ChangeColours();
    if (X->scroll) {
	if (parent->GetBackgroundColour())
	    XtVaSetValues(X->scroll, XtNbackground,
			  parent->GetBackgroundColour()->GetPixel(cmap), NULL);
	if (label_fg)
	    XtVaSetValues(X->scroll, XtNforeground,
			  label_fg->GetPixel(cmap), NULL);
    }
}

//-----------------------------------------------------------------------------
// handle scrolling with keys
//-----------------------------------------------------------------------------

void wxCanvas::OnChar(wxKeyEvent &event)
{
    int start_x, start_y;

    ViewStart(&start_x, &start_y);

    switch (event.KeyCode()) {
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
	if (event.ControlDown()) {
	    Scroll(max(0, start_x-h_units_per_page), start_y);
	} else {
	    if (start_x >= 1)
		Scroll(start_x - 1, start_y);
	}
	break;
    case WXK_RIGHT:
	if (event.ControlDown())
	    Scroll(start_x + h_units_per_page, start_y);
	else
	    Scroll(start_x + 1, start_y);
	break;
    case WXK_HOME:
	Scroll(0, 0);
	break;
    }
}

//-----------------------------------------------------------------------------
// compatibility, I like the typesave constructor more
//-----------------------------------------------------------------------------

wxCanvas::wxCanvas(wxWindow *parent, int x, int y, int width, int height,
		   int style, char *name) : wxItem()
{
    __type = wxTYPE_CANVAS;

    h_size = h_units = v_size = v_units = 1;
    h_units_per_page = v_units_per_page = 50;

    Create(parent, x, y, width, height, style, name);
}

/* MATTHEW */
wxCanvas::wxCanvas(wxFrame *parent, int x, int y, int width, int height,
		   int style, char *name) : wxItem()
{
    __type = wxTYPE_CANVAS;

    h_size = h_units = v_size = v_units = 1;
    h_units_per_page = v_units_per_page = 50;

    Create((wxPanel *)parent, x, y, width, height, style, name);
}

Bool wxCanvas::Create(wxWindow *panel, int x, int y, int width, int height,
		      int style, char *name)
{
  /* MATTHEW: [6] */
  if (!wxSubType(panel->__type, wxTYPE_PANEL))
    wxFatalError("parent has to be a wxFrame, wxPanel, or any subtype", "wxCanvas");

  return Create((wxPanel*)panel, x, y, width, height, style, name);
}

