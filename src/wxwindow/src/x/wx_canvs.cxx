/*
 * File:        wx_canvs.cc
 * Purpose:     wxCanvas implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_canvs.cxx,v 1.1.1.1 1997/12/22 16:12:03 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_canvs.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

#include <math.h>
#include <iostream.h>
#include <stdlib.h>
#include "common.h"
#include "wx_frame.h"
#include "wx_dccan.h"
#include "wx_dcpan.h"
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_stdev.h"
#include "wx_utils.h"
#include "wx_privt.h"

#include <X11/Xutil.h>
#include <X11/keysym.h>

#ifdef wx_motif
#include "wx_main.h"

#include <Xm/DrawingA.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/BulletinB.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>

#define SCROLL_MARGIN 4
void wxCanvasRepaintProc (Widget, XtPointer, XmDrawingAreaCallbackStruct * cbs);
void wxCanvasInputEvent (Widget drawingArea, XtPointer data, XmDrawingAreaCallbackStruct * cbs);
void wxCanvasMotionEvent (Widget, XButtonEvent * event);
void wxCanvasEnterLeave (Widget drawingArea, XtPointer clientData, XCrossingEvent * event);
#endif

//#define NSTATIC_RECTS	20
//static wxRectangle wxRectArray[NSTATIC_RECTS];

IMPLEMENT_DYNAMIC_CLASS(wxCanvas, wxWindow)

wxCanvas::wxCanvas (void)
{
  is_retained = FALSE;
  horiz_units = 0;
  vert_units = 0;
  wx_dc = NULL;
  units_per_page_x = 0;
  units_per_page_y = 0;
  scrolls_set_size = TRUE;
  updateRects.DeleteContents(TRUE);
  requiresBackingStore = FALSE;
  hScroll = FALSE;
  vScroll = FALSE;
  hScrollBar = NULL;
  vScrollBar = NULL;
  allowRepainting = TRUE;
  hScrollingEnabled = TRUE;
  vScrollingEnabled = TRUE;
  backingPixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  hExtent = 0;
  vExtent = 0;
  pixmapOffsetX = 0;
  pixmapOffsetY = 0;
  scrolledWindow = 0;
  borderWidget = 0;
  handle = NULL;
  display = NULL;
  xwindow = 0;
}

wxCanvas::wxCanvas (wxWindow * parent, int x, int y, int width, int height,
	  long style, char *name):
wxbCanvas (parent, x, y, width, height, style, name)
{
  Create (parent, x, y, width, height, style, name);
}

Bool wxCanvas::
Create (wxWindow * parent, int x, int y, int width, int height,
	long style, char *name)
{
  requiresRetention = 0 && ((style & wxRETAINED) == wxRETAINED);
  windowStyle = style;
  is_retained = FALSE;		// Can only be retained after scrollbars have been set
  scrolls_set_size = TRUE;

  units_per_page_x = 0;
  units_per_page_y = 0;
  SetName(name);
  updateRects.DeleteContents(TRUE);

  requiresBackingStore = ((style & wxBACKINGSTORE) == wxBACKINGSTORE);
  hScroll = FALSE;
  vScroll = FALSE;
  hScrollBar = NULL;
  vScrollBar = NULL;
  allowRepainting = TRUE;
  borderWidget = 0;
  hScrollingEnabled = TRUE;
  vScrollingEnabled = TRUE;
  backingPixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  hExtent = 0;
  vExtent = 0;
  pixmapOffsetX = 0;
  pixmapOffsetY = 0;

  // New translations for getting mouse motion feedback
  String translations =
  "<Btn1Motion>: wxCanvasMotionEvent() DrawingAreaInput() ManagerGadgetButtonMotion()\n\
     <Btn2Motion>: wxCanvasMotionEvent() DrawingAreaInput() ManagerGadgetButtonMotion()\n\
     <Btn3Motion>: wxCanvasMotionEvent() DrawingAreaInput() ManagerGadgetButtonMotion()\n\
     <BtnMotion>: wxCanvasMotionEvent() DrawingAreaInput() ManagerGadgetButtonMotion()\n\
     <Btn1Down>: DrawingAreaInput() ManagerGadgetArm()\n\
     <Btn2Down>: DrawingAreaInput() ManagerGadgetArm()\n\
     <Btn3Down>: DrawingAreaInput() ManagerGadgetArm()\n\
     <Btn1Up>: DrawingAreaInput() ManagerGadgetActivate()\n\
     <Btn2Up>: DrawingAreaInput() ManagerGadgetActivate()\n\
     <Btn3Up>: DrawingAreaInput() ManagerGadgetActivate()\n\
     <Motion>: wxCanvasMotionEvent() DrawingAreaInput()\n\
     <EnterWindow>: wxCanvasMotionEvent() DrawingAreaInput()\n\
     <LeaveWindow>: wxCanvasMotionEvent() DrawingAreaInput()\n\
     <Key>: DrawingAreaInput()";

  XtActionsRec actions[1];
  actions[0].string = "wxCanvasMotionEvent";
  actions[0].proc = (XtActionProc) wxCanvasMotionEvent;
  XtAppAddActions (wxTheApp->appContext, actions, 1);

  Widget parentWidget = 0;
  if (wxSubType(parent->__type, wxTYPE_FRAME))
    parentWidget = ((wxFrame *)parent)->clientArea;
  else if (wxSubType(parent->__type, wxTYPE_PANEL))
    parentWidget = (Widget)parent->handle;
  else
  {
    wxError("Canvas subwindow must be a child of either a frame or panel!");
    return FALSE;
  }

  if (style & wxBORDER)
    borderWidget = XtVaCreateManagedWidget ("canvasBorder",
				      xmFrameWidgetClass, parentWidget,
					    XmNshadowType, XmSHADOW_IN,
					    NULL);

  scrolledWindow = XtVaCreateManagedWidget ("scrolledWindow",
					    xmScrolledWindowWidgetClass, 
					    borderWidget ? borderWidget : parentWidget,
					    XmNspacing, 0,
					    XmNscrollingPolicy, XmAPPLICATION_DEFINED,
					    NULL);

  XtTranslations ptr;
  Widget drawingArea;

  if (0 && wxSubType(__type, wxTYPE_PANEL)) {
    drawingArea = XtVaCreateWidget(windowName,
				   xmBulletinBoardWidgetClass, scrolledWindow,
				   XmNmarginWidth, 0,
				   XmNmarginHeight, 0,
				   XmNresizePolicy, XmRESIZE_GROW,
				   XmNallowOverlap, (Boolean)TRUE,
				   NULL);
  } else {
    drawingArea = XtVaCreateWidget (windowName,
				    xmDrawingAreaWidgetClass, 
				    scrolledWindow,
				    XmNunitType, XmPIXELS,
				    XmNresizePolicy, XmRESIZE_GROW,
				    // XmNresizePolicy, XmRESIZE_NONE,
				    XmNmarginHeight, 0,
				    XmNmarginWidth, 0,
				    XmNtranslations, ptr = XtParseTranslationTable (translations),
				    NULL);

    if (!wxSubType(__type, wxTYPE_PANEL)) {
      Display *d = XtDisplay (scrolledWindow);
      XtVaSetValues(drawingArea, 
		    XmNbackground, WhitePixel(d, DefaultScreen(d)),
		    NULL);
    }
  }

  /* Must add this window style before uncommenting */
  /* MATTHEW: added flag */
  if (ptr) {
    XtFree((char *)ptr);
    if (windowStyle & wxOVERRIDE_KEY_TRANSLATIONS) {
      ptr = XtParseTranslationTable ("<Key>: DrawingAreaInput()");
      XtOverrideTranslations (drawingArea, ptr);
      XtFree ((char *) ptr);
    }
  }

  if (wxWidgetHashTable->Get ((long) drawingArea))
    {
      wxError ("Widget table clash in wx_canvs.cc: drawingArea");
    }
  if (wxWidgetHashTable->Get ((long) scrolledWindow))
    {
      wxError ("Widget table clash in wx_canvs.cc: scrolledWindow");
    }
  wxWidgetHashTable->Put ((long) drawingArea, this);
  wxWidgetHashTable->Put ((long) scrolledWindow, this);

  /*
   * This order is very important in Motif 1.2.1
   *
   */

  XtRealizeWidget (scrolledWindow);
  XtRealizeWidget (drawingArea);
  XtManageChild (drawingArea);

  XtOverrideTranslations (drawingArea,
		   ptr = XtParseTranslationTable ("<Configure>: resize()"));
  XtFree ((char *) ptr);

  XtAddCallback (drawingArea, XmNexposeCallback, (XtCallbackProc) wxCanvasRepaintProc, (XtPointer) this);
  XtAddCallback (drawingArea, XmNinputCallback, (XtCallbackProc) wxCanvasInputEvent, (XtPointer) this);

  handle = (char *) drawingArea;

  display = XtDisplay (scrolledWindow);
  xwindow = XtWindow (drawingArea);

  XtAddEventHandler (drawingArea, PointerMotionHintMask | EnterWindowMask | LeaveWindowMask | FocusChangeMask,
    False, (XtEventHandler) wxCanvasEnterLeave, (XtPointer) this);

  if (parent)
    parent->AddChild (this);
  window_parent = parent;

  horiz_units = 0;
  vert_units = 0;

  if (wxSubType(__type, wxTYPE_PANEL))
    wx_dc = new wxPanelDC ((wxPanel *)this);
  else
    wx_dc = new wxCanvasDC (this);

  if (wxSubType(parent->__type, wxTYPE_PANEL))
    ((wxPanel *)parent)->AttachWidget(this, 0, x, y, width, height);
  else
    SetSize (x, y, width, height);

  if (GetWindowStyleFlag() & (wxHSCROLL |  wxVSCROLL))
    SetScrollbars(1, 1, 0, 0, 1, 1, 0, 0, FALSE);

  return TRUE;
}

wxCanvas::~wxCanvas (void)
{
  if (backingPixmap)
    XFreePixmap (XtDisplay ((Widget) handle), backingPixmap);

  // This should be the right ordering now.
  // The (potential) children of a panel-canvas are deleted
  // in ~wxPanel. Now delete the canvas widget.
  wxWidgetHashTable->Delete((long)handle);
  Widget w = (Widget)handle;
  if (w)
    XtDestroyWidget(w);
  handle = NULL;

  // Only if we're _really_ a canvas (not a dialog box/panel)
  if (scrolledWindow)
  {
    wxWidgetHashTable->Delete ((long) handle);
    wxWidgetHashTable->Delete ((long) scrolledWindow);
  }

  if (hScrollBar)
    {
      XtUnmanageChild (hScrollBar);
      XtDestroyWidget (hScrollBar);
    }
  if (vScrollBar)
    {
      XtUnmanageChild (vScrollBar);
      XtDestroyWidget (vScrollBar);
    }
  if (scrolledWindow)
  {
    XtUnmanageChild (scrolledWindow);
    XtDestroyWidget (scrolledWindow);
    handle = NULL;
  }

  if (borderWidget)
  {
    XtDestroyWidget (borderWidget);
    borderWidget = 0;
  }

  if (wx_dc)
  {
    delete wx_dc;
    wx_dc = NULL;
  }
}

Bool wxCanvas:: PreResize (void)
{
//  cout << "Canvas PreResize\n";
  //  OnPaint();
  return TRUE;
}

void wxCanvas:: SetColourMap (wxColourMap * cmap)
{
  if (wx_dc)
    wx_dc->SetColourMap(cmap);
}


void wxCanvas:: SetClientSize (int w, int h)
{
#ifdef _____wx_motif
//  SetSize(-1, -1, w, h);
  /* ALTERNATIVE CODE SUPPLIED BY ALS, NOT TESTED
   * IS THIS BETTER AND IF SO WHY!
   */
  Widget drawingArea = (Widget) handle;

  XtVaSetValues(drawingArea, XmNresizePolicy, XmRESIZE_ANY, NULL);

  if (w > -1)
    XtVaSetValues (drawingArea, XmNwidth, w, NULL);
  if (h > -1)
    XtVaSetValues (drawingArea, XmNheight, h, NULL);
  allowRepainting = FALSE;

  XSync (XtDisplay (drawingArea), FALSE);
  XEvent event;
  while (XtAppPending (wxTheApp->appContext))
    {
      XFlush (XtDisplay (drawingArea));
      XtAppNextEvent (wxTheApp->appContext, &event);
      XtDispatchEvent (&event);
    }
  XtVaSetValues(drawingArea, XmNresizePolicy, XmRESIZE_NONE, NULL);

  allowRepainting = TRUE;
  DoRefresh(FALSE);
  GetEventHandler()->OnSize(w, h);
#else
  wxWindow::SetClientSize (w, h);
#endif
}

void wxCanvas:: GetClientSize (int *w, int *h)
{
#ifdef wx_motif
  Dimension xx, yy;
  if (handle) {
    XtVaGetValues ((Widget)handle,
		   XmNwidth, &xx, 
		   XmNheight, &yy, 
		   NULL);
  } else
    xx = yy = 0;

  *w = xx;
  *h = yy;
#else
  wxWindow::GetClientSize (w, h);
#endif
}

void wxCanvas:: SetSize (int x, int y, int w, int h, int sizeFlags)
{
  Position cx = 0, cy = 0;
  Dimension cw = 0, ch = 0;
  Widget topWidget = borderWidget ? borderWidget : scrolledWindow;
  Widget focusWidget = 0;
  Bool isShow = XtIsManaged(topWidget);
  int pchanged = 0, schanged = 0;

#ifdef _SIZE_TRACE
  printf("assign C: %lx %d %d\n", this, w, h);
#endif

  XtVaGetValues(topWidget, 
		XmNx, &cx, 
		XmNy, &cy, 
		XmNwidth, &cw, 
		XmNheight, &ch, 
		NULL);
    
  if ((x > -1) && (x != cx)) {
    cx = x;
    pchanged = 1;
  }
  if ((y > -1) && (y != cy)) {
    cy = y;
    pchanged = 1;
  }
  if ((w > -1) && (w != cw)) {
    cw = w;
    schanged = 1;
  }
  if ((h > -1) && (h != ch)) {
    ch = h;
    schanged = 1;
  }

  if (!pchanged && !schanged)
    return;

  int pw, ph;
  GetParent()->GetSize(&pw, &ph);

  if (isShow) {
    focusWidget = XmGetFocusWidget(topWidget);
    XtUnmanageChild(topWidget);
    /* printf("unmanage: %lx\n", topWidget); */
  }

  XtVaSetValues(topWidget, XmNx, cx, XmNy, cy,
		XmNwidth, cw, XmNheight, ch, NULL);
  
  if (schanged && borderWidget) {
    Dimension thick, margin;
    XtVaGetValues (borderWidget,
		   XmNshadowThickness, &thick,
		   XmNmarginWidth, &margin,
		   NULL);
    cw -= 2 * (thick + margin);
    ch -= 2 * (thick + margin);

    XtVaSetValues (scrolledWindow, XmNwidth, cw, XmNheight, ch, NULL);

    cw += 2 * (thick + margin);
    ch += 2 * (thick + margin);
  }
  
  if (isShow) {
    XtManageChild(topWidget);
    if (focusWidget != XmGetFocusWidget(topWidget))
      XmProcessTraversal(focusWidget, XmTRAVERSE_CURRENT);
    /* printf("manage: %lx\n", topWidget); */
  }

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

  if (!(sizeFlags & 0x100)) {
    int ww, hh;
    GetSize(&ww, &hh);
    sr_width = ww;
    sr_height = hh;
    GetEventHandler()->OnSize(ww, hh);
  }
}

void wxCanvas:: GetSize (int *w, int *h)
{
  Dimension xx, yy;
//  XtVaGetValues(scrolledWindow, XmNwidth, &xx, XmNheight, &yy, NULL);
  if (borderWidget)
    XtVaGetValues (borderWidget, XmNwidth, &xx, XmNheight, &yy, NULL);
  else if (scrolledWindow)
    XtVaGetValues (scrolledWindow, XmNwidth, &xx, XmNheight, &yy, NULL);
  else
    XtVaGetValues ((Widget)handle, XmNwidth, &xx, XmNheight, &yy, NULL);

  *w = xx;
  *h = yy;

#ifdef _SIZE_TRACE
  printf("report C: %lx %d %d\n", this, *w, *h);
#endif
}

Window wxCanvas::GetXWindow(void)
{
  return (Window)XtWindow((Widget)(borderWidget ? borderWidget : scrolledWindow));
}

void wxCanvas:: GetPosition (int *x, int *y)
{
  Dimension xx, yy;
  XtVaGetValues (borderWidget ? borderWidget : scrolledWindow, XmNx, &xx, XmNy, &yy, NULL);
  *x = xx;
  *y = yy;
}

void wxCanvas::PhysicalScroll(int x, int y, int w, int h,
		int deltax, int deltay)
{
//  cerr << "Scrolling. delta = " << deltax << ", " << deltay << endl;
  Widget drawingArea = (Widget) handle;

  int x1 = deltax >= 0 ? x : x - deltax ;
  int y1 = deltay >= 0 ? y : y - deltay;
  int w1 = w - abs(deltax);
  int h1 = h - abs(deltay);
  int x2 = deltax >= 0 ? x + deltax : x;
  int y2 = deltay >= 0 ? y + deltay : y;
/*
  cerr << "Copying " << x1 << ", " << y1 << ", " << "width = " << w1;
  cerr << ", height = " << h1 << ", to " << x2 << ", " << y2 << endl;
*/
  XCopyArea(XtDisplay(drawingArea), XtWindow(drawingArea),
    XtWindow(drawingArea), GetDC()->gc,
    x1, y1,
    w1, h1,
    x2, y2);

  GetDC()->autoSetting = TRUE;
  SetBrush(GetDC()->current_background_brush);
//  SetBrush(wxRED_BRUSH);

  // We'll add rectangles to the list of update rectangles
  // according to which bits we've exposed.
  updateRects.Clear();
	
  if (deltax > 0)
  {
    wxRectangle *rect = new wxRectangle;
    rect->x = x;
    rect->y = y;
    rect->width = deltax;
    rect->height = h;

//    cerr << "Filling rectangle " << rect->x << ", " << rect->y << ", ";
//    cerr << rect->width << ", " << rect->height << endl;

    XFillRectangle(XtDisplay(drawingArea), XtWindow(drawingArea),
     GetDC()->gc, rect->x, rect->y, rect->width, rect->height);

    updateRects.Append(rect);
  }
  else if (deltax < 0)
  {
    wxRectangle *rect = new wxRectangle;

    rect->x = x + w + deltax;
    rect->y = y;
    rect->width = -deltax;
    rect->height = h;

//    cerr << "Filling rectangle " << rect->x << ", " << rect->y << ", ";
//    cerr << rect->width << ", " << rect->height << endl;

    XFillRectangle(XtDisplay(drawingArea), XtWindow(drawingArea),
      GetDC()->gc, rect->x, rect->y, rect->width,
      rect->height);

    updateRects.Append(rect);
  }
  if (deltay > 0)
  {
    wxRectangle *rect = new wxRectangle;

    rect->x = x;
    rect->y = y;
    rect->width = w;
    rect->height = deltay;
//    cerr << "Filling rectangle " << rect->x << ", " << rect->y << ", ";
//    cerr << rect->width << ", " << rect->height << endl;

    XFillRectangle(XtDisplay(drawingArea), XtWindow(drawingArea),
      GetDC()->gc, rect->x, rect->y, rect->width, rect->height);

    updateRects.Append(rect);
  }
  else if (deltay < 0)
  {
    wxRectangle *rect = new wxRectangle;

    rect->x = x;
    rect->y = y + h + deltay;
    rect->width = w;
    rect->height = -deltay;
//    cerr << "Filling rectangle " << rect->x << ", " << rect->y << ", ";
//    cerr << rect->width << ", " << rect->height << endl;

    XFillRectangle(XtDisplay(drawingArea), XtWindow(drawingArea),
      GetDC()->gc, rect->x, rect->y, rect->width, rect->height);

    updateRects.Append(rect);
  }
//  cerr << "About to paint" << endl;
  GetEventHandler()->OnPaint();
  updateRects.Clear();
}

void wxCanvas::DoPaint(XRectangle *WXUNUSED(xrect), int WXUNUSED(n))
{
  DoRefresh(TRUE);
}

/* Calls OnPaint or uses retained pixmap,
 * as necessary
 */
void wxCanvas:: DoRefresh (Bool paint)
{
  int canvasWidth1;
  int canvasHeight1;
  GetClientSize (&canvasWidth1, &canvasHeight1);

  // Following test assure that callback is not called repeatedly.
  if (hScroll && scrolls_set_size) {
    int old_size, old_max, old_pos;
    XtVaGetValues (hScrollBar,
		   XmNsliderSize, &old_size,
		   XmNmaximum, &old_max, 
		   XmNvalue, &old_pos,
		   NULL);
    int new_size =
      (int) (max (min (canvasWidth1 / horiz_units, hExtent / horiz_units), 1));
    if (old_size != new_size) {
      int mx = max(new_size, old_max);
      XtVaSetValues (hScrollBar,
		     XmNmaximum, mx,
		     XmNsliderSize, new_size,
		     XmNvalue, min(old_pos, mx - new_size),
		     NULL);
    }
  }

  if (vScroll && scrolls_set_size) {
    int old_size, old_max, old_pos;
    XtVaGetValues (vScrollBar, 
		   XmNsliderSize, &old_size,
		   XmNmaximum, &old_max, 
		   XmNvalue, &old_pos,
		   NULL);
    int new_size =
      (int) (max (min (canvasHeight1 / vert_units, vExtent / vert_units), 1));
    if (old_size != new_size) {
      int mx = max(new_size, old_max);
      XtVaSetValues (vScrollBar,
		     XmNmaximum, mx,
		     XmNsliderSize, new_size, 
		     XmNvalue, min(old_pos, mx - new_size),
		     NULL);
    }
  }
  if (paint) {
    int x, y;
    ViewStart (&x, &y);
    if (is_retained && backingPixmap)
      {
	Widget drawingArea = (Widget) handle;
	XCopyArea (XtDisplay (drawingArea), backingPixmap, XtWindow (drawingArea), GetDC ()->gc,
		   pixmapOffsetX, pixmapOffsetY,
		   pixmapWidth, pixmapHeight,
		   0, 0);
      }
    else
      {
	GetEventHandler()->OnPaint ();
      }
  }
}

void 
wxScrollCallback (Widget scrollbar, int orientation, XmScrollBarCallbackStruct * cbs)
{
  Widget scrolledWindow = XtParent (scrollbar);
  wxCanvas *canvas = (wxCanvas *) wxWidgetHashTable->Get ((long) scrolledWindow);
  if (canvas)
  {
    wxCommandEvent *_event = new wxCommandEvent;
    wxCommandEvent &event = *_event;

    WXSCROLLPOS(event) = cbs->value;
    if (orientation == XmHORIZONTAL)
      WXSCROLLORIENT(event) = wxHORIZONTAL;
    else
      WXSCROLLORIENT(event) = wxVERTICAL;

    switch (cbs->reason)
    {
      case XmCR_INCREMENT:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_LINEDOWN;
        break;
      }
      case XmCR_DECREMENT:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_LINEUP;
        break;
      }
      case XmCR_PAGE_INCREMENT:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_PAGEDOWN;
        break;
      }
      case XmCR_PAGE_DECREMENT:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_PAGEUP;
        break;
      }
      case XmCR_TO_TOP:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_TOP;
        break;
      }
      case XmCR_TO_BOTTOM:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_BOTTOM;
        break;
      }
      case XmCR_DRAG:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
        break;
      }
      case XmCR_VALUE_CHANGED:
      {
        event.eventType = wxEVENT_TYPE_SCROLL_THUMBTRACK;
        break;
      }
      default:
      {
        return;
      }
    }
    canvas->DoScroll(event);
    canvas->GetEventHandler()->OnScroll(event);
  }
}


/*
 * horizontal/vertical: number of pixels per unit (e.g. pixels per text line)
 * x/y_length:        : no. units per scrollbar
 * x/y_page:          : no. units per page scrolled
 */

void wxCanvas::SetScrollbars (int horizontal, int vertical,
			      int x_length, int y_length,
			      int x_page, int y_page,
			      int x_pos, int y_pos, Bool setVirtualSize)
{
  if (!(GetWindowStyleFlag() & wxHSCROLL))
    horizontal = -1;
  if (!(GetWindowStyleFlag() & wxVSCROLL))
    vertical = -1;

  int xp, yp;
  xp = -1;
  yp = -1;

  if (hScrollBar)
    XtVaGetValues (hScrollBar, XmNvalue, &xp, NULL);
  if (vScrollBar)
    XtVaGetValues (vScrollBar, XmNvalue, &yp, NULL);

  Bool needRepaint = TRUE;
  if (horizontal == horiz_units &&
      vertical == vert_units &&
      x_pos == xp &&
      y_pos == yp &&
      x_page == units_per_page_x &&
      y_page == units_per_page_y) {
    if (x_length == units_x &&
	y_length == units_y) {
      //DebugMsg("No change\n") ;
      return;			/* Nothing changed */
    }
    /*
       This flag is to avoid repainting (which causes
       flickering) when all we are doing
       is changing the virtual size of the canvas.
       Hernan Otero (hernan@isoft.com.ar)
       */
    needRepaint = FALSE;
  }
  
  horiz_units = horizontal;
  vert_units = vertical;
  units_per_page_x = max(x_page, 1);
  units_per_page_y = max(y_page, 1);
  units_x = max(x_length, 0);
  units_y = max(y_length, 0);
  x_pos = min(max(x_pos, 0), x_length);
  y_pos = min(max(y_pos, 0), y_length);

  int w, h, x, y;
  GetSize (&w, &h);
  GetPosition (&x, &y);

  int canvasWidth1;
  int canvasHeight1;
  GetClientSize (&canvasWidth1, &canvasHeight1);
  scrolls_set_size = setVirtualSize;

  Widget drawingArea = (Widget) handle;
  if (horizontal > 0) {
    if (setVirtualSize)
      hExtent = horizontal * x_length;
    else 
      hExtent = 0;
    
    if (!hScrollBar) {
      hScrollBar = XtVaCreateManagedWidget ("hsb",
					    xmScrollBarWidgetClass, scrolledWindow,
					    XmNorientation, XmHORIZONTAL,
					    NULL);
      XtAddCallback (hScrollBar, XmNvalueChangedCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNdragCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNincrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNdecrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNpageIncrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNpageDecrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNtoTopCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
      XtAddCallback (hScrollBar, XmNtoBottomCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmHORIZONTAL);
    }
    
    int sliderSize, extra;
    if (setVirtualSize) {
      sliderSize = max(min(canvasWidth1/horizontal, x_length), 1);
      x_page = min(max(x_page, 1), sliderSize);
      x_pos = min(x_pos, x_length - sliderSize);
      extra = 0;
    } else {
      sliderSize = x_page;
      extra = x_page;
    }

    XtVaSetValues (hScrollBar,
		   XmNincrement, 1,
		   XmNpageIncrement, x_page,
		   XmNmaximum, x_length + extra,
		   XmNvalue, x_pos,
		   XmNsliderSize, sliderSize,
		   NULL);

    if (setVirtualSize) {
      hStart = x_pos;
      if (GetDC ())
	GetDC ()->device_origin_x = -(x_pos * horiz_units);
      if (requiresRetention)
	pixmapOffsetX = (x_pos * horiz_units);
    }

    hScroll = TRUE;
  } else {
    hStart = 0;
    hExtent = 0;
    hScroll = FALSE;
    if (GetWindowStyleFlag() & wxHSCROLL) {
      SetScrollRange(wxHORIZONTAL, 0);
      SetScrollPage(wxHORIZONTAL, 1);
      SetScrollPos(wxHORIZONTAL, 0);
    } else {    
      if (hScrollBar) {
	XtUnmanageChild (hScrollBar);
	XtDestroyWidget(hScrollBar);
	hScrollBar = NULL;
      }
    }
  }

  if (vertical > 0) {
    if (setVirtualSize)
      vExtent = vertical * y_length;
    else
      vExtent = 0;
    
    if (!vScrollBar) {
      vScrollBar = XtVaCreateManagedWidget ("vsb",
					    xmScrollBarWidgetClass, scrolledWindow,
					    XmNorientation, XmVERTICAL,
					    NULL);
      XtAddCallback (vScrollBar, XmNvalueChangedCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNdragCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNincrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNdecrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNpageIncrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNpageDecrementCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNtoTopCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
      XtAddCallback (vScrollBar, XmNtoBottomCallback, 
		     (XtCallbackProc) wxScrollCallback, (XtPointer) XmVERTICAL);
    }

    int sliderSize, extra;
    if (setVirtualSize) {
      sliderSize = max(min(canvasHeight1/vertical, y_length), 1);
      y_page = min(max(y_page, 0), sliderSize);
      y_pos = min(y_pos, y_length - sliderSize);
      extra = 0;
    } else {
      sliderSize = y_page;
      extra = y_page;
    }

    XtVaSetValues (vScrollBar,
		   XmNincrement, 1,
		   XmNpageIncrement, y_page,
		   XmNmaximum, y_length + extra,
		   XmNvalue, y_pos,
		   XmNsliderSize, sliderSize,
		   NULL);

    if (setVirtualSize) {
      vStart = y_pos;
      if (GetDC ())
	GetDC ()->device_origin_y = -(y_pos * vert_units);
      if (requiresRetention)
	pixmapOffsetY = (y_pos * vert_units);
    }

    vScroll = TRUE;
  } else {
    vStart = 0;
    vExtent = 0;
    vScroll = FALSE;
    if (GetWindowStyleFlag() & wxVSCROLL) {
      SetScrollRange(wxVERTICAL, 0);
      SetScrollPage(wxVERTICAL, 1);
      SetScrollPos(wxVERTICAL, 0);
    } else {
      if (vScrollBar) {
	XtUnmanageChild (vScrollBar);
	XtDestroyWidget(vScrollBar);
	vScrollBar = NULL;
      }
    }
  }

  XmScrolledWindowSetAreas (scrolledWindow, hScrollBar, vScrollBar, drawingArea);
  if (hScrollBar)
    XtRealizeWidget (hScrollBar);
  if (vScrollBar)
    XtRealizeWidget (vScrollBar);

  EnableScrolling(hScroll, vScroll);

  if (!setVirtualSize) {
    /* No, really, I mean it ... */
    if (hScroll) {
      SetScrollRange(wxHORIZONTAL, x_length);
      SetScrollPage(wxHORIZONTAL, x_page);
      SetScrollPos(wxHORIZONTAL, x_pos);
    }
    if (vScroll) {
      SetScrollRange(wxVERTICAL, y_length);
      SetScrollPage(wxVERTICAL, y_page);
      SetScrollPos(wxVERTICAL, y_pos);
    }
  }

  Dimension cw, ch;
  XtVaGetValues(drawingArea, XtNwidth, &cw, XtNheight, &ch, NULL);
  if (wx_dc && ((hExtent && (cw > hExtent)) || (vExtent && (ch > vExtent)))) {
    float x, y, w, h;
    wx_dc->GetClippingRegion(&x, &y, &w, &h);
    if (hExtent && (cw > hExtent)) {
      wx_dc->SetClippingRegion(hExtent, 0, cw, ch);
      wx_dc->Clear();
    }
    if (vExtent && (ch > vExtent)) {
      wx_dc->SetClippingRegion(0, vExtent, cw, ch);
      wx_dc->Clear();
    }
    if (w >= 0)
      wx_dc->SetClippingRegion(x, y, w, h);
    else
      wx_dc->DestroyClippingRegion();
  }

  /*
   * Retained pixmap stuff
   *
   */

  if (requiresRetention && (hExtent > 0) && (vExtent > 0))
    {
      if ((hExtent != pixmapWidth) || (vExtent != pixmapHeight))
	{
	  pixmapWidth = hExtent;
	  pixmapHeight = vExtent;

	  if (backingPixmap)
	    XFreePixmap (XtDisplay (drawingArea), backingPixmap);

	  backingPixmap = XCreatePixmap (XtDisplay (drawingArea),
	      RootWindowOfScreen (XtScreen (drawingArea)), hExtent, vExtent,
			     DefaultDepthOfScreen (XtScreen (drawingArea)));

	  if (backingPixmap)
	    is_retained = TRUE;
	  else
	    is_retained = FALSE;

	  if (needRepaint) {
	    Clear ();
	    GetEventHandler()->OnPaint ();
	  }
	}
    }

  if (needRepaint) {
    // This necessary to make scrollbars appear, for some reason!
    SetSize (x, y, w, h);
  }
}

void wxCanvas:: GetScrollUnitsPerPage (int *x_page, int *y_page)
{
  *x_page = units_per_page_x;
  *y_page = units_per_page_y;
}

/*
 * Scroll to given position (scroll position, not pixel position)
 */

void wxCanvas:: Scroll (int x_pos, int y_pos)
{
  int old_x, old_y;
  ViewStart (&old_x, &old_y);
  if (((x_pos == -1) || (x_pos == old_x)) && ((y_pos == -1) || (y_pos == old_y)))
    return;

  Bool clearCanvas = FALSE;
  if (hScroll)
    {
      XtVaSetValues (hScrollBar, XmNvalue, x_pos, NULL);
      hStart = x_pos;
      if (hScrollingEnabled && !is_retained)
		clearCanvas = TRUE;

      if (GetDC ())
	GetDC ()->device_origin_x = -(x_pos * horiz_units);
      if (is_retained)
	pixmapOffsetX = (x_pos * horiz_units);
    }
  if (vScroll)
    {
      XtVaSetValues (vScrollBar, XmNvalue, y_pos, NULL);
      vStart = y_pos;

      if (vScrollingEnabled && !is_retained)
		clearCanvas = TRUE;

      if (GetDC ())
	GetDC ()->device_origin_y = -(y_pos * vert_units);
      if (is_retained)
	pixmapOffsetY = (y_pos * vert_units);
    }

	if (clearCanvas) {
		int new_x, new_y;
		int width, height;
		ViewStart(&new_x, &new_y);
		GetClientSize(&width, &height);
		PhysicalScroll(0, 0, width, height,
				(old_x - new_x) * horiz_units,
				(old_y - new_y) * vert_units);
	} else
	  DoRefresh (FALSE);
}

void wxCanvas:: EnableScrolling (Bool x_scroll, Bool y_scroll)
{
  hScrollingEnabled = x_scroll;
  vScrollingEnabled = y_scroll;
}

void wxCanvas:: GetVirtualSize (int *x, int *y)
{
  int x1, y1;
  GetClientSize (&x1, &y1);
  if (hExtent == 0)
    *x = x1;
  else
    *x = hExtent;

  if (vExtent == 0)
    *y = y1;
  else
    *y = vExtent;
}


/* There follows a fix to a X event sequencing problem, contributed
 * by Scott Maxwell (maxwell@natasha.jpl.nasa.giv).
 */

// Maps wxCanvases to lists of XRectangles that need updating.
// wxHash is unfortunately not usable for the purpose since the
// wxCanvas * -> long conversion could lead to bogus matches.
// JACS, how about a third key type for wxHash: wxObject *?
typedef wxList inherited;
class wxRectMap: public wxList
{
    // Prevent copying.
    wxRectMap(const wxRectMap &);
    wxRectMap & operator = (const wxRectMap &);

    // Our map consists of Pair *s.
    class Pair
    {
	// Prevent copying.
	Pair(const Pair &);
	Pair & operator = (const Pair &);

    public:
	const wxCanvas & canv;
	wxList rectQ;

	Pair(const wxCanvas & canv_): canv(canv_)  {}
	~Pair(void)  {  rectQ.Clear();  }  // Needed?
    };

    wxNode * Append(wxObject * obj)  {  return inherited::Append(obj);  }
/*
    // Give linker error if we mistakenly try to use these.
    wxNode * Append(long key, wxObject * obj);
    wxNode * Append(char * key, wxObject * obj);
*/
    // Get the list node that maps from the specified canvas to its rectangle
    // list, or return 0 if canvas not found.
    wxNode * getPair(const wxCanvas & canvas)
    {
	for (wxNode * node = First(); node; node = node->Next())
	{
	    Pair * p = (Pair *) (node->Data());
	    
	    if (&(p->canv) == &canvas)
		return node;
	}

	return 0;
    }

public:
    wxRectMap(void)  {}
    ~wxRectMap(void)
    {
	// Drain the list if necessary -- should always be empty, though.
	while (Number())
	{
	    int bogus;

	    wxNode * node = First();

	    // As a side effect, this deletes the underlying Pair.
	    delete [] GiveArray(((Pair *) (node->Data()))->canv, bogus);
	}
    }

    // Appends the given rect to the list of rects waiting to be updated
    // by canvas.  If necessary, create a new canvas/rectQ pair.
    // We take over ownership of rect but not of canvas.
    void Append(const wxCanvas & canvas, XRectangle & rect)
    {
	wxNode * node = getPair(canvas);

	if (!node)
	    node = Append((wxObject *) new Pair(canvas));

	Pair * pair = (Pair *) (node->Data());
	pair->rectQ.Append((wxObject *) &rect);
    }

    // Convert the list of rectangles stored for canvas into an array;
    // return a pointer to this array, and set n to its size.  Deletes
    // the corresponding Pair, so that further Appends for the same canvas
    // will start afresh.  Caller owns the returned array.
    XRectangle * GiveArray(const wxCanvas & canvas, int & n)
    {
	wxNode * node = getPair(canvas);

	if (!node)
	{
	    n = 0;
	    return 0;
	}

	Pair * p = (Pair *) (node->Data());
	wxList & rectQueue = p->rectQ;
	n = rectQueue.Number();

	XRectangle * xrects = new XRectangle[n];
	XRectangle * rectp = xrects;

	// Copy XRectangles into the array, freeing the old copies as we go.
	for (wxNode * iter = rectQueue.First(); iter; iter = iter->Next())
	{
	    XRectangle * xrect = (XRectangle *) (iter->Data());

	    *rectp++ = *xrect;
	    delete xrect;  xrect = 0;
	}

	delete p;  p = 0;
	DeleteNode(node);

	return xrects;
    }
};


/*
void indent(int nLevels)  // Debugging only.
{
    if (nLevels < 0)
    {
	cerr << "!!! nLevels == " << nLevels << endl;
	return;
    }

    while (nLevels--)
	cerr << '\t';
    cerr << flush;
}
*/


void 
wxCanvasRepaintProc (Widget drawingArea, XtPointer clientData, XmDrawingAreaCallbackStruct * cbs)
// void wxCanvasRepaintProc(Widget w, XtPointer c_data, XEvent *event, char *)
{
    if (!wxWidgetHashTable->Get ((long) drawingArea))
	return;

    static wxRectMap rectMap;
//    static int depth = -1;  // Debugging only.

    XEvent * event = cbs->event;
    wxCanvas * canvas = (wxCanvas *) clientData;
    Display * display = (Display *) canvas->GetXDisplay();
    GC gc = (GC) canvas->GetDC()->gc;

    XRectangle * xrect;

    switch (event->type)
    {
    case Expose:
	xrect = new XRectangle;

	xrect->x = event->xexpose.x;
	xrect->y = event->xexpose.y;
	xrect->width = event->xexpose.width;
	xrect->height = event->xexpose.height;

	/*
	++depth;
	indent(depth);
	
	cerr << "Appending to " << hex << canvas << dec
	     << " (" << event->xexpose.count << " to go)." << endl;
	*/
	
	// Append this rectangle to the queue of rectangles to be updated
	// by this canvas.  After this operation, rectMap owns the memory
	// pointed to by xrect, so do not delete it.
	rectMap.Append(*canvas, *xrect);

	if (!event->xexpose.count)
	{
	    int n;
	    XRectangle * xrects = rectMap.GiveArray(*canvas, n);

	    /*
	    indent(depth);
	    cerr << "Servicing " << hex << canvas << dec
		 << " (" << n << " queued) ... " << endl;
	    */
	    
	    XSetClipRectangles(display, gc, 0, 0, xrects, n, Unsorted);
	    canvas->DoPaint(xrects, n);

	    /*
	    indent(depth);
	    cerr << "Done servicing " << n << " for " << hex << canvas << dec << "." << endl;
	    */

	    // We own the array pointed to by xrects, so delete it.
	    delete [] xrects;  xrects = 0;

#if 1
	    XGCValues gc_val;
	    gc_val.clip_mask = None;
	    XChangeGC(display, gc, GCClipMask, &gc_val);
#else
	    XSetClipMask(display, gc, None);
#endif
	}
//	--depth;
	break;

    default:
	cout << "\n\nNew Event ! is = " << event -> type << "\n";
	break;
    }
}

// Code with X event sequencing problems
#if 0
void 
wxCanvasRepaintProc (Widget drawingArea, XtPointer clientData, XmDrawingAreaCallbackStruct * cbs)
// void wxCanvasRepaintProc(Widget w, XtPointer c_data, XEvent *event, char *)
   {
  if (!wxWidgetHashTable->Get ((long) drawingArea))
    return;

  wxCanvas *canvas = (wxCanvas *) clientData;

//     wxCanvas *canvas;
//     Window window;
     static XRectangle *xrect;
     Display *display;
     GC gc;
     int llp = 0;
//     int ppl;
     static int last_count = 0;
     static int draw_count = 0;
     XEvent *event = cbs->event;

//     canvas = (wxCanvas *) c_data;

     switch(event -> type)
        {
          case Expose :
               // window = XtWindow((Widget)canvas->handle); /* MATTHEW: [15] */
               display = (Display *) canvas -> GetXDisplay();
               gc = (GC) canvas -> GetDC() -> gc;
               
               llp = event -> xexpose.count;
               
               if ((last_count == 0) && (llp == 0))
                  {
                    xrect = new XRectangle[1];
                    xrect[0].x = event -> xexpose.x;
                    xrect[0].y = event -> xexpose.y;
                    xrect[0].width = event -> xexpose.width;
                    xrect[0].height = event -> xexpose.height;

                    XSetClipRectangles(display,gc,0,0,xrect,1,Unsorted);
                    canvas -> DoPaint(xrect,1);
                    delete[] xrect;

                    // This line is an attempt to restore canvas to no clipping: JACS
//                    XSetClipMask (display, gc, None);

                    // Didn't work; try this instead. JACS.
#if 1
                    XGCValues gc_val;
                    gc_val.clip_mask = None;
                    XChangeGC (display, gc, GCClipMask, &gc_val);
#else
		    XSetClipMask (display, gc, None);
#endif

                  }

               if ((last_count == 0) && (llp != 0))
                  {
                    xrect = new XRectangle[llp + 1];
                    draw_count = llp + 1;
                    
                    xrect[draw_count - llp - 1].x = event -> xexpose.x;
                    xrect[draw_count - llp - 1].y = event -> xexpose.y;
                    xrect[draw_count - llp - 1].width = event -> xexpose.width;
                    xrect[draw_count - llp - 1].height = event -> xexpose.height;
                  }

               if ((last_count != 0) && (llp != 0))
                  {
                    xrect[draw_count - llp - 1].x = event -> xexpose.x;
                    xrect[draw_count - llp - 1].y = event -> xexpose.y;
                    xrect[draw_count - llp - 1].width = event -> xexpose.width;
                    xrect[draw_count - llp - 1].height = event -> xexpose.height;
                  }
               
               if ((last_count != 0) && (llp == 0))
                  {
                    xrect[draw_count - llp - 1].x = event -> xexpose.x;
                    xrect[draw_count - llp - 1].y = event -> xexpose.y;
                    xrect[draw_count - llp - 1].width = event -> xexpose.width;
                    xrect[draw_count - llp - 1].height = event -> xexpose.height;

                    XSetClipRectangles(display,gc,0,0,xrect,draw_count,Unsorted);
                    canvas -> DoPaint(xrect,draw_count);
                    delete[] xrect;

                    // This line is an attempt to restore canvas to no clipping: JACS
//                    XSetClipMask (display, gc, None);
                    // Didn't work; try this instead
#if 1
                    XGCValues gc_val;
                    gc_val.clip_mask = None;
                    XChangeGC (display, gc, GCClipMask, &gc_val);
#else
		    XSetClipMask (display, gc, None);
#endif
                  }
               last_count = event -> xexpose.count;
               break;
          default :
               cout << "\n\nNew Event ! is = " << event -> type << "\n";
               break;
        }
   }
#endif

// Unable to deal with Enter/Leave without a separate EventHandler (Motif 1.1.4)
void 
wxCanvasEnterLeave (Widget drawingArea, XtPointer, XCrossingEvent * event)
{
  XmDrawingAreaCallbackStruct cbs;
  XEvent ev;

  //if (event->mode!=NotifyNormal)
  //  return ;
  ev = *((XEvent *) event);
  cbs.reason = XmCR_INPUT;
  cbs.event = &ev;

  wxCanvasInputEvent (drawingArea, (XtPointer) NULL, &cbs);
}

// Fix to make it work under Motif 1.0 (!)
void wxCanvasMotionEvent(Widget drawingArea, XButtonEvent * event)
{
#if   XmVersion<=1000
  XmDrawingAreaCallbackStruct cbs;
  XEvent ev;

  //ev.xbutton = *event;
  ev = *((XEvent *) event);
  cbs.reason = XmCR_INPUT;
  cbs.event = &ev;

  wxCanvasInputEvent (drawingArea, (XtPointer) NULL, &cbs);
#endif
}

// I know that I use global var. to handle double-click, but after
// examining all counterparts, I really think that this is OK.
static Bool wait_dclick = FALSE;
static Bool dclick = FALSE;
static lose_up = FALSE;

static long timerId;

static void wxDClickCallback(XtPointer)
{
  dclick = FALSE;
  wait_dclick = FALSE;
}

void 
wxCanvasInputEvent (Widget drawingArea, XtPointer data, XmDrawingAreaCallbackStruct * cbs)
{
  wxCanvas *canvas = (wxCanvas *) wxWidgetHashTable->Get ((long) drawingArea);
  XEvent local_event;

  if (canvas==NULL)
    return ;

  if (cbs->reason != XmCR_INPUT)
    return;

  local_event = *(cbs->event);	// We must keep a copy!

  if (wait_dclick)
    {
      // If we are waiting for a double-click, we only handle Button events
      // in a special fashion.
      if (local_event.xany.type == ButtonPress)
	{
	  wait_dclick = FALSE;
	  dclick = TRUE;
	  lose_up = FALSE;
	  XtRemoveTimeOut (timerId);
	  return;
	}
      if (local_event.xany.type == ButtonRelease)
	{
	  lose_up = TRUE;
	  return;
	}
    }

  switch (local_event.xany.type)
    {
    case EnterNotify:
    case LeaveNotify:
    case ButtonPress:
    case ButtonRelease:
    case MotionNotify:
      {
	WXTYPE eventType = 0;

	if (local_event.xany.type == EnterNotify)
	  {
	    //if (local_event.xcrossing.mode!=NotifyNormal)
	    //  return ; // Ignore grab events
	    eventType = wxEVENT_TYPE_ENTER_WINDOW;
//            canvas->GetEventHandler()->OnSetFocus();
	  }
	else if (local_event.xany.type == LeaveNotify)
	  {
	    //if (local_event.xcrossing.mode!=NotifyNormal)
	    //  return ; // Ignore grab events
	    eventType = wxEVENT_TYPE_LEAVE_WINDOW;
//            canvas->GetEventHandler()->OnKillFocus();
	  }
	else if (local_event.xany.type == MotionNotify)
	  {
	    eventType = wxEVENT_TYPE_MOTION;
	    if (local_event.xmotion.is_hint == NotifyHint)
	      {
		Window root, child;
		Display *dpy = XtDisplay (drawingArea);

		XQueryPointer (dpy, XtWindow (drawingArea),
			       &root, &child,
			       &local_event.xmotion.x_root,
			       &local_event.xmotion.y_root,
			       &local_event.xmotion.x,
			       &local_event.xmotion.y,
			       &local_event.xmotion.state);
//fprintf(stderr,"*") ; fflush(stderr) ;
	      }
	    else
	      {
//fprintf(stderr,".") ; fflush(stderr) ;
	      }
	  }

	else if (local_event.xany.type == ButtonPress)
	  {

	    // Not reached if we are already waiting a double click.
	    // @@@ Double Clicks allowed ONLY for left button!
	    // [JACS: if the right button is used for PopUp menus it seems
	    // to interfere with this code and the button seems to be down
	    // always]
	    if (canvas->doubleClickAllowed && local_event.xbutton.button == Button1)
	      {
		timerId = XtAppAddTimeOut (wxTheApp->appContext,
					   canvas->doubleClickAllowed,
				     (XtTimerCallbackProc) wxDClickCallback,
					   (XtPointer) 0);

		wait_dclick = TRUE;
		lose_up = FALSE;
		dclick = FALSE;
		// Not so trivial code... I've carefully looked Xt code to find
		// that THIS seq. is the good one!!
		do
		  {
		    if (XtAppPending (wxTheApp->appContext))
		      XtAppProcessEvent (wxTheApp->appContext, XtIMAll);
		  }
		while (wait_dclick);
		// So here, dclick&lose_up have correct values.
	      }
	    else
	      {
		lose_up = FALSE;
		dclick = FALSE;
	      }

	    // Setup correct event type, depending on dclick.
	    if (local_event.xbutton.button == Button1)
	      {
		eventType = dclick ? wxEVENT_TYPE_LEFT_DCLICK : wxEVENT_TYPE_LEFT_DOWN;
	      }
	    else if (local_event.xbutton.button == Button2)
	      {
		eventType = dclick ? wxEVENT_TYPE_MIDDLE_DCLICK : wxEVENT_TYPE_MIDDLE_DOWN;
	      }
	    else if (local_event.xbutton.button == Button3)
	      {
		eventType = dclick ? wxEVENT_TYPE_RIGHT_DCLICK : wxEVENT_TYPE_RIGHT_DOWN;
	      }
	  }
	else if (local_event.xany.type == ButtonRelease)
	  {
	    // Not reached if we are already waiting a double click.
	    if (local_event.xbutton.button == Button1)
	      {
		eventType = wxEVENT_TYPE_LEFT_UP;
	      }
	    else if (local_event.xbutton.button == Button2)
	      {
		eventType = wxEVENT_TYPE_MIDDLE_UP;
	      }
	    else if (local_event.xbutton.button == Button3)
	      {
		eventType = wxEVENT_TYPE_RIGHT_UP;
	      }
	  }

	wxMouseEvent *_event = new wxMouseEvent(eventType);
	wxMouseEvent &wxevent = *_event;

	wxevent.eventHandle = (char *) &local_event;

	if (canvas->GetDC ()) {
	  wxevent.x = canvas->GetDC ()->DeviceToLogicalX (local_event.xbutton.x);
	  wxevent.y = canvas->GetDC ()->DeviceToLogicalY (local_event.xbutton.y);
	}

#define event_left_is_down(x) (x.xbutton.state & Button1Mask)
#define event_middle_is_down(x) (x.xbutton.state & Button2Mask)
#define event_right_is_down(x) (x.xbutton.state & Button3Mask)

	wxevent.leftDown = ((eventType == wxEVENT_TYPE_LEFT_DOWN)
			    || (event_left_is_down (local_event) 
				&& (eventType != wxEVENT_TYPE_LEFT_UP)));
	wxevent.middleDown = ((eventType == wxEVENT_TYPE_MIDDLE_DOWN)
			      || (event_middle_is_down (local_event) 
				  && (eventType != wxEVENT_TYPE_MIDDLE_UP)));
	wxevent.rightDown = ((eventType == wxEVENT_TYPE_RIGHT_DOWN)
			     || (event_right_is_down (local_event) 
				 && (eventType != wxEVENT_TYPE_RIGHT_UP)));

	wxevent.shiftDown = local_event.xbutton.state & ShiftMask;
	wxevent.controlDown = local_event.xbutton.state & ControlMask;
        wxevent.altDown = /* local_event.xbutton.state & Mod3Mask */ FALSE;
        wxevent.metaDown = local_event.xbutton.state & Mod1Mask;
	wxevent.eventObject = canvas;
        wxevent.SetTimestamp(local_event.xbutton.time);

	if (!canvas->CallPreOnEvent(canvas, &wxevent))
	  canvas->GetEventHandler()->OnEvent (wxevent);

	if (eventType == wxEVENT_TYPE_ENTER_WINDOW ||
	    eventType == wxEVENT_TYPE_LEAVE_WINDOW ||
	    eventType == wxEVENT_TYPE_MOTION
	  )
	  return;

	if (lose_up)		// Simple click, but ButtonRelease event was losed.

	  {
	    if (local_event.xbutton.button == Button1)
	      {
		eventType = wxEVENT_TYPE_LEFT_UP;
	      }
	    else if (local_event.xbutton.button == Button2)
	      {
		eventType = wxEVENT_TYPE_MIDDLE_UP;
	      }
	    else if (local_event.xbutton.button == Button3)
	      {
		eventType = wxEVENT_TYPE_RIGHT_UP;
	      }
	    wxevent.eventType = eventType;
	    if (!canvas->CallPreOnEvent(canvas, &wxevent))
	      canvas->GetEventHandler()->OnEvent (wxevent);
	  }
	wait_dclick = FALSE;
	dclick = FALSE;
	lose_up = FALSE;
	break;
      }
    case KeyPress:
      {
	KeySym keySym;
	XComposeStatus compose;
	(void) XLookupString ((XKeyEvent *) & local_event, wxBuffer, 20, &keySym, &compose);
	int id = CharCodeXToWX (keySym);

	wxKeyEvent *_event = new wxKeyEvent(wxEVENT_TYPE_CHAR);
	wxKeyEvent &event  = *_event;

	if (local_event.xkey.state & ShiftMask)
	  event.shiftDown = TRUE;
	if (local_event.xkey.state & ControlMask)
	  event.controlDown = TRUE;
	/*
	if (local_event.xkey.state & Mod3Mask)
	  event.altDown = TRUE;
	  */
	if (local_event.xkey.state & Mod1Mask)
	  event.metaDown = TRUE;
	event.eventObject = canvas;
	event.keyCode = id;
        event.SetTimestamp(local_event.xkey.time);

	if (canvas->GetDC ())
	  {
	    event.x = canvas->GetDC ()->DeviceToLogicalX (local_event.xbutton.x);
	    event.y = canvas->GetDC ()->DeviceToLogicalY (local_event.xbutton.y);
	  }

	if (id > -1) {
	  if (!canvas->CallPreOnChar(canvas, &event))
	    canvas->GetEventHandler()->OnChar(event);
	}
	break;
      }
    case FocusIn:
      {
	/* MATTHEW: [2] detail check was needed after all */
	if (local_event.xfocus.detail != NotifyPointer)
          canvas->GetEventHandler()->OnSetFocus ();
	break;
      }
    case FocusOut:
      {
	/* MATTHEW: [2] detail check was needed after all */
	if (local_event.xfocus.detail != NotifyPointer)
          canvas->GetEventHandler()->OnKillFocus ();
        break;
      }
    default:
      break;
    }
}

// Where the current view starts from
void wxCanvas:: ViewStart (int *x, int *y)
{
  int xx, yy;
  if (hScroll)
    XtVaGetValues (hScrollBar, XmNvalue, &xx, NULL);
  else
    xx = 0;
  if (vScroll)
    XtVaGetValues (vScrollBar, XmNvalue, &yy, NULL);
  else
    yy = 0;
  *x = xx;
  *y = yy;
}


void wxCanvas:: WarpPointer (int x_pos, int y_pos)
{
  // Move the pointer to (x_pos,y_pos) coordinates. They are expressed in
  // pixel coordinates, relatives to the canvas -- So, we only need to
  // substract origin of the window.

  if (GetDC ())
    {
      x_pos += (int) (GetDC ()->device_origin_x);
      y_pos += (int) (GetDC ()->device_origin_y);
    }

  XWarpPointer (display, None, xwindow, 0, 0, 0, 0, x_pos, y_pos);
}

wxCursor *wxCanvas:: SetCursor (wxCursor * cursor)
{
  return wxWindow::SetCursor (cursor);
}

// Default scrolling behaviour
void wxCanvas::OnScroll(wxCommandEvent& event)
{
  DoRefresh(TRUE);
}

void wxCanvas::DoScroll(wxCommandEvent &event)
{
  if (!scrolls_set_size)
    return;

  int newx, newy;
  ViewStart(&newx, &newy);
  int oldScrollX = hStart;
  int oldScrollY = vStart;

  Bool doScroll = FALSE;
  int value = WXSCROLLPOS(event);

  if (WXSCROLLORIENT(event) == wxHORIZONTAL)
  {
    if (hScrollingEnabled && !is_retained)
      doScroll = TRUE;

    hStart = value;
    if (GetDC ())
      GetDC ()->device_origin_x = -(value * horiz_units);
    if (is_retained)
      pixmapOffsetX = (value * horiz_units);
  }
  else
  {
    if (vScrollingEnabled && !is_retained)
      doScroll = TRUE;

     vStart = value;
     if (GetDC ())
       GetDC ()->device_origin_y = -(value * vert_units);
     if (is_retained)
       pixmapOffsetY = (value * vert_units);
  }

  if (doScroll)
  {
    int width, height;
    GetClientSize(&width, &height);
    PhysicalScroll(0, 0, width, height,
		(oldScrollX - newx) * horiz_units

    // This is an OBOB correction.... I don't know who's fault
    // it is, but if this is not here... the horizontal
    // scrolling is offset by 1.
    // Hernan Otero (hernan@isoft.com.ar)
/* Taken out by JACS 21/5/95: Alexey Iskhakov's new wx_dc.h macros
 * should cure the problem.
        + (oldx == 0 && newx != 0 ? 1 :
	  (newx == 0 && oldx != 0 ? -1 : 0))
*/
      ,
      
      (oldScrollY - newy) * vert_units);
  }
  else
  {
    DoRefresh(FALSE);
  }
}

/* MATTHEW: Fix [Set/Get]Scroll[Pos/Range/Page] */

void wxCanvas::SetScrollPos(int orient, int pos)
{
  Widget bar = ((orient == wxHORIZONTAL) ? hScrollBar : vScrollBar);
  
  int w = ((orient == wxHORIZONTAL) ? units_x : units_y);
  pos = min(max(pos, 0), w);

  if (bar)
    XtVaSetValues (bar,
		   XmNvalue, pos,
		   NULL);
}

void wxCanvas::SetScrollRange(int orient, int range)
{
  Widget bar = ((orient == wxHORIZONTAL) ? hScrollBar : vScrollBar);
  int page ((orient == wxHORIZONTAL) ? units_per_page_x : units_per_page_y);

  if (bar)
    XtVaSetValues (bar,
		   XmNsliderSize, page,
		   XmNpageIncrement, page,
		   XmNmaximum, range + page,
		   NULL);

  if (orient == wxHORIZONTAL)
    units_x = range;
  else
    units_y = range;
}

int wxCanvas::GetScrollPos(int orient)
{
  Widget bar = ((orient == wxHORIZONTAL) ? hScrollBar : vScrollBar);
  int d;
  if (bar) {
    XtVaGetValues (bar,
		   XmNvalue, &d,
		   NULL);
    return (int)d;
  }

  return 0;
}

int wxCanvas::GetScrollRange(int orient)
{
  if (orient == wxHORIZONTAL) {
    if (hScroll)
      return units_x;
  } else if (vScroll)
    return units_y;

  return 0;
}

void wxCanvas::SetScrollPage(int orient, int page)
{
  if (orient == wxHORIZONTAL)
    units_per_page_x = page;
  else
    units_per_page_y = page;

  Widget bar = ((orient == wxHORIZONTAL) ? hScrollBar : vScrollBar);

  if (bar) {
    XtVaSetValues (bar,
		   XmNsliderSize, page,
		   XmNpageIncrement, page,
		   XmNmaximum, page + ((orient == wxHORIZONTAL) ? units_x : units_y),
		   NULL);
  }
}

int wxCanvas::GetScrollPage(int orient)
{
  if (orient == wxHORIZONTAL) {
    if (hScroll)
      return units_per_page_x;
  } else if (vScroll) 
    return units_per_page_y;

  return 0;
}


/*
 * Update iterator. Use from within OnPaint.
 */
 
wxUpdateIterator::wxUpdateIterator(wxWindow* wnd)
{
  current = 0;					//start somewhere...
  win = wnd;
}

wxUpdateIterator::~wxUpdateIterator(void)
{
}

wxUpdateIterator::operator int (void)
{
  wxCanvas *can = (wxCanvas *)win;
  if (current < can->updateRects.Number())
  {
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}

wxUpdateIterator* wxUpdateIterator::operator ++(int)
{
  current++;
  return this;
}

void wxUpdateIterator::GetRect(wxRectangle *rect)
{
  wxCanvas *can = (wxCanvas *)win;
  if (current < can->updateRects.Number())
  {
    wxRectangle *canRect = (wxRectangle *)can->updateRects.Nth(current)->Data();
    rect->x = canRect->x;
    rect->y = canRect->y;
    rect->width = canRect->width;
    rect->height = canRect->height;
  }
}

int wxUpdateIterator::GetX()
{
  wxCanvas *can = (wxCanvas *)win;
  if (current < can->updateRects.Number())
  {
    wxRectangle *canRect = (wxRectangle *)can->updateRects.Nth(current)->Data();
    return canRect->x;
  }
  else return 0;
}

int wxUpdateIterator::GetY()
{
  wxCanvas *can = (wxCanvas *)win;
  if (current < can->updateRects.Number())
  {
    wxRectangle *canRect = (wxRectangle *)can->updateRects.Nth(current)->Data();
    return canRect->y;
  }
  else return 0;
}

int wxUpdateIterator::GetW()
{
  wxCanvas *can = (wxCanvas *)win;
  if (current < can->updateRects.Number())
  {
    wxRectangle *canRect = (wxRectangle *)can->updateRects.Nth(current)->Data();
    return canRect->width;
  }
  else return 0;
}

int wxUpdateIterator::GetH()
{
  wxCanvas *can = (wxCanvas *)win;
  if (current < can->updateRects.Number())
  {
    wxRectangle *canRect = (wxRectangle *)can->updateRects.Nth(current)->Data();
    return canRect->height;
  }
  else return 0;
}

