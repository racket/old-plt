/*
 * File:	wx_frame.cc
 * Purpose:	wxFrame implementation (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_frame.cxx,v 1.12 1998/12/11 01:07:22 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_frame.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

// #include <iostream.h>

#include "common.h"
#include "wx_frame.h"
#include "wx_dialg.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_menu.h"
#include "wx_dcmem.h"

#include <stdlib.h>

#ifdef wx_motif
#if defined(__ultrix) || defined(__sgi)
#include <Xm/Frame.h>
#endif
#include <Xm/Xm.h>
#include <X11/Shell.h>
#if XmVersion >= 1002
#include <Xm/XmAll.h>
#else
#include <Xm/Frame.h>
#endif
#include <Xm/MwmUtil.h>
#include <Xm/BulletinB.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/AtomMgr.h>
#include <Xm/LabelG.h>
#include <Xm/Frame.h>
#if   XmVersion > 1000
#include <Xm/Protocols.h>
#endif
void wxCloseFrameCallback(Widget, XtPointer, XmAnyCallbackStruct *cbs);
#endif


extern void wxRegisterFrameWidget(Widget);
extern void wxUnregisterFrameWidget(Widget);

static Bool wxTopLevelUsed = FALSE;

#define INACTIVE_FRAME_MARKER ((wxWindow *)-1)

static void wxSetFrameFocus(wxWindow *frame, wxWindow *fw)
{
  wxWindow *ofw;
  int is_frame;

  is_frame = wxSubType(frame->__type, wxTYPE_FRAME);

  if (is_frame) {
    ofw = ((wxFrame *)frame)->activeItem;
    ((wxFrame *)frame)->activeItem = fw;
  } else {
    ofw = ((wxDialogBox *)frame)->activeItem;
    ((wxDialogBox *)frame)->activeItem = fw;
  }
  
  if (ofw != fw) {
    if (ofw && (ofw != INACTIVE_FRAME_MARKER))
      ofw->OnKillFocus();
    
    if (fw && (fw != INACTIVE_FRAME_MARKER))
      fw->OnSetFocus();
  }
}

static void wxFrameFocusProc(Widget workArea, XtPointer, 
			     XmAnyCallbackStruct *)
{
  wxWindow *fw = NULL, *frame;

  frame = (wxWindow *)wxWidgetHashTable->Get((long)workArea);
  
  if (!frame)
    return;

  Widget w = XmGetFocusWidget(workArea);

  while (w) {
    fw = (wxWindow *)wxWidgetHashTable->Get((long)w);
    if (fw)
      break;

    w = XtParent(w);
  }

  wxSetFrameFocus(frame, fw);
}

void wxFrameCheckFocus(wxWindow *w)
{
  Widget wa;
  wxWindow *ofw;

  while (w && !wxSubType(w->__type, wxTYPE_FRAME) && !wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    w = w->GetParent();

  if (wxSubType(w->__type, wxTYPE_FRAME)) {
    wa = ((wxFrame *)w)->workArea;
    ofw = ((wxFrame *)w)->activeItem;
  } else {
    wa = ((wxDialogBox *)w)->dialogShell;
    ofw = ((wxDialogBox *)w)->activeItem;
  }

  if (ofw != INACTIVE_FRAME_MARKER)
    wxFrameFocusProc(wa, NULL, NULL);
}

/* MATTEW: Used to insure that hide-&-show within an event cycle works */
static void wxFrameMapProc(Widget, XtPointer clientData, XEvent *e)
{
  wxFrame *frame = (wxFrame *)wxWidgetHashTable->Get((long)clientData);

  if (frame) {
    if (e->xany.type == MapNotify) {
      if (!frame->visibleStatus) {
	/* We really wanted this to be hidden! */
	XtUnmapWidget(frame->frameShell);
      }
    }
  }
}

/* MATTEW: Call OnActivate */
static void wxWindowFocusProc(Widget, XtPointer clientData, XEvent *xev)
{
# define ACTIVE_VIA_POINTER_FLAG 0x1
  wxWindow *win = (wxWindow *)wxWidgetHashTable->Get((long)clientData);

  if (win) {
    int Enter = FALSE;

    switch (xev->xany.type) {
    case EnterNotify:
      Enter = TRUE;
    case LeaveNotify: 
      /* If Focus == PointerRoot, manage activation */
      if (xev->xcrossing.detail != NotifyInferior) {
	Window current;
	int old_revert;
	XGetInputFocus(XtDisplay((Widget)win->handle), &current, &old_revert);
	if (current == PointerRoot) {
	  if (Enter)
	    win->filler |= ACTIVE_VIA_POINTER_FLAG;
	  else
	    win->filler -= (win->filler & ACTIVE_VIA_POINTER_FLAG);
	  win->GetEventHandler()->OnActivate(Enter);
	  if (!Enter)
	    wxSetFrameFocus(win, INACTIVE_FRAME_MARKER);
	}
      }
      break;

    case FocusIn:
      Enter = TRUE;
    case FocusOut:
      if (xev->xfocus.detail != NotifyInferior) {
	Window current;
	if (xev->xfocus.detail == NotifyPointer) {
	  /* NotifyPointer is meaningful if the focus is PointerRoot
	     or we're active via the pointer */
	  if (!Enter && (win->filler & ACTIVE_VIA_POINTER_FLAG)) {
	    current = PointerRoot;
	  } else {
	    int old_revert;
	    XGetInputFocus(XtDisplay((Widget)win->handle), &current, &old_revert);
	  }
	} else
	  current = PointerRoot;
	
	if (current == PointerRoot) {
	  if (xev->xfocus.detail == NotifyPointer) {
	    if (Enter)
	      win->filler |= ACTIVE_VIA_POINTER_FLAG;
	    else
	      win->filler -= (win->filler & ACTIVE_VIA_POINTER_FLAG);
	  }
	  win->GetEventHandler()->OnActivate(Enter);
	  if (!Enter)
	    wxSetFrameFocus(win, INACTIVE_FRAME_MARKER);
	}
      }
    }
  }
}

void wxInstallOnActivate(Widget frameShell, Widget workArea)
{
  XtAddEventHandler(frameShell, 
		    FocusChangeMask | EnterWindowMask | LeaveWindowMask,
		    False, (XtEventHandler)wxWindowFocusProc,
		    (XtPointer)workArea);

  XtAddCallback(workArea, XmNfocusCallback, 
                (XtCallbackProc)wxFrameFocusProc, 
		(XtPointer)NULL);
}


IMPLEMENT_DYNAMIC_CLASS(wxFrame, wxWindow)

wxFrame::wxFrame(void)
{
  frame_type = 0;
  wx_menu_bar = NULL;
  status_line_exists = FALSE;
  icon = NULL;
  modal_showing = FALSE;
  window_parent = NULL;
  frameTitle = NULL;
  lastWidth = -1 ;
  lastHeight = -1 ;
  /* MATTHEW: intialize visibility flag: */
  visibleStatus = FALSE;
  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    statusTextWidget[i] = 0;
  statusLineForm = 0;
  frameShell = NULL;
  menuBarWidget = NULL;
  statusLineWidget = NULL;
  frameWidget = NULL;
  workArea = NULL;
  clientArea = NULL;
  handle = NULL;
}

wxFrame::wxFrame(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name):
  wxbFrame(Parent, title, x, y, width, height, style, name)
{
  Create(Parent, title, x, y, width, height, style, name);
}

Bool wxFrame::Create(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name)
{
  wxbFrame::Create(Parent, title, x, y, width, height, style, name);

  SetName(name);
  if (title)
    frameTitle = copystring(title);

  frame_type = style & (wxSDI | wxMDI_PARENT | wxMDI_CHILD);
  windowStyle = style;
  wx_menu_bar = NULL;
  status_line_exists = FALSE;
  icon = NULL;
  modal_showing = FALSE;
  if (Parent) Parent->AddChild(this);
  window_parent = Parent;
  lastWidth = -1 ;
  lastHeight = -1 ;
  statusLineForm = 0;
  statusLineWidget = 0;
  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    statusTextWidget[i] = 0;

  if (wxTopLevelUsed)
    frameShell = XtAppCreateShell(windowName, wxTheApp->wx_class, topLevelShellWidgetClass, wxGetDisplay(), NULL, 0);
  else {
    frameShell = wxTheApp->topLevel;
    wxTopLevelUsed = TRUE;
  }

  wxRegisterFrameWidget(frameShell);

  XtVaSetValues(frameShell, 
                 // Allows menu to resize
                 XmNallowShellResize, (style & wxRESIZE_BORDER) ? False : True,
                 XmNdeleteResponse, XmDO_NOTHING,
                 XmNmappedWhenManaged, False,
                 XmNiconic, (style & wxICONIZE) ? TRUE : FALSE,
                 NULL);

  if (title)
    XtVaSetValues(frameShell, 
                  XmNtitle, title,
                  NULL);

  menuBarWidget = NULL;
  statusLineWidget = NULL;

  frameWidget = XtVaCreateManagedWidget("main_window",
                    xmMainWindowWidgetClass, frameShell,
                    XmNresizePolicy, /* (style & wxPUSH_PIN) ? XmRESIZE_GROW : XmRESIZE_NONE */ XmRESIZE_GROW,
                    NULL);

  workArea = XtVaCreateWidget("form",
                    xmFormWidgetClass, frameWidget,
                    XmNresizePolicy, /* (style & wxPUSH_PIN) ? XmRESIZE_GROW : XmRESIZE_NONE */ XmRESIZE_GROW,
                    NULL);

  clientArea = XtVaCreateWidget("client",
                    xmBulletinBoardWidgetClass, workArea,
                    XmNmarginWidth, 0,
                    XmNmarginHeight, 0,
                    XmNrightAttachment, XmATTACH_FORM,
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNtopAttachment, XmATTACH_FORM,
                    XmNbottomAttachment, XmATTACH_FORM,
//                    XmNresizePolicy, XmRESIZE_ANY,
                    NULL);

  XtVaSetValues(frameWidget,
		XmNworkWindow, workArea,
		NULL);


  XtManageChild(clientArea);
//  XtManageChild(statusLineWidget);
  XtManageChild(workArea);

  if (wxWidgetHashTable->Get((long)workArea))
    wxError("Widget table clash in wx_frame.cc");
  wxWidgetHashTable->Put((long)workArea, this);

  XtTranslations ptr ;

  XtOverrideTranslations(workArea,
              ptr = XtParseTranslationTable("<Configure>: resize()"));

  XtFree((char *)ptr);

  /* MATTHEW: part of show-&-hide fix */
  XtAddEventHandler(frameShell, StructureNotifyMask,
		    False, (XtEventHandler)wxFrameMapProc,
		    (XtPointer)workArea);
  
  wxInstallOnActivate(frameShell, workArea);

  if (x > -1)
    XtVaSetValues(frameShell, XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues(frameShell, XmNy, y, NULL);
  if (width > -1)
    XtVaSetValues(frameShell, XmNwidth, width, NULL);
  if (height > -1)
    XtVaSetValues(frameShell, XmNheight, height, NULL);

  handle = (char *)frameWidget;

  // This patch comes from Torsten Liermann lier@lier1.muc.de
  if (XmIsMotifWMRunning(wxTheApp->topLevel)) {
    int decor = 0 ;
    if (!(style & wxNO_RESIZE_BORDER)) {
      decor |= MWM_DECOR_RESIZEH;
      decor |= MWM_DECOR_MINIMIZE;
      decor |= MWM_DECOR_MAXIMIZE;
    }
    if (!(style & wxNO_SYSTEM_MENU))
      decor |= MWM_DECOR_MENU;
    if (!(style & wxNO_CAPTION))
      decor |= MWM_DECOR_TITLE;
    if (!(style & wxNO_THICK_FRAME))
      decor |= MWM_DECOR_BORDER;
    XtVaSetValues(frameShell,XmNmwmDecorations,decor,NULL) ;
  }

  XtRealizeWidget(frameShell);

  // Intercept CLOSE messages from the window manager
  Atom WM_DELETE_WINDOW = XmInternAtom(XtDisplay(frameShell), "WM_DELETE_WINDOW", False);

#if (XmREVISION > 1 || XmVERSION > 1)
  XmAddWMProtocolCallback(frameShell, WM_DELETE_WINDOW, (XtCallbackProc) wxCloseFrameCallback, (XtPointer)this);
#else
#if XmREVISION == 1
  XmAddWMProtocolCallback(frameShell, WM_DELETE_WINDOW, (XtCallbackProc) wxCloseFrameCallback, (caddr_t)this);
#else
  XmAddWMProtocolCallback(frameShell, WM_DELETE_WINDOW, (void (*)())wxCloseFrameCallback, (caddr_t)this);
#endif
#endif

  PreResize();
  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize(width, height);

  AddPreHandlers(workArea);

  return TRUE;
}

Bool wxFrame::PreResize(void)
{
  return TRUE;

#if 0
  // Set status line, if any
  if (status_line_exists)
  {
    Dimension clientW, clientH;
    XtVaGetValues(clientArea, XmNwidth, &clientW, XmNheight, &clientH, NULL);
    Dimension xx, yy;
    XtVaGetValues(statusLineWidget, XmNwidth, &xx, XmNheight, &yy, NULL);

    if (!clientW)
      clientW = 1;

    XtUnmanageChild(statusLineWidget);
    XtVaSetValues(statusLineWidget, XmNx, 0, XmNy, clientH - yy, XmNwidth, clientW, NULL);

    if (statusLineForm)
      XtVaSetValues(statusLineForm,  XmNwidth, clientW, NULL);

    XtManageChild(statusLineWidget);
  }

  int width, height;
  GetSize(&width, &height);

  if (width == lastWidth && height == lastHeight)
    return FALSE;
  else
    return TRUE;
#endif
}

// Get size *available for subwindows* i.e. excluding menu bar etc.
// For XView, this is the same as GetSize
void wxFrame::GetClientSize(int *x, int *y)
{
#if 1
  Dimension xx, yy;
  XtVaGetValues(clientArea, XmNwidth, &xx, XmNheight, &yy, NULL);
#else
  Dimension xx, yy;
  XtVaGetValues(workArea, XmNwidth, &xx, XmNheight, &yy, NULL);

  if (status_line_exists)
  {
    Dimension ys;
    XtVaGetValues(statusLineWidget, XmNheight, &ys, NULL);
    yy -= ys;
  }

  if (wx_menu_bar != NULL)
  {
    // it seems that if a frame holds a panel, the menu bar size
    // gets automatically taken care of --- grano@cs.helsinki.fi 4.4.95
    Bool hasSubPanel = FALSE;
    for(wxChildNode* node = GetChildren()->First(); node; node = node->Next())
    {
     wxWindow *win = (wxWindow *)node->Data();
     hasSubPanel = (wxSubType(win->__type, wxTYPE_PANEL && !wxSubType(win->__type, wxTYPE_DIALOG_BOX)) ||
                     wxSubType(win->__type, wxTYPE_CANVAS) ||
                     wxSubType(win->__type, wxTYPE_TEXT_WINDOW));

      if (hasSubPanel)
          break;
    }
    if (! hasSubPanel) {
      Dimension ys;
      XtVaGetValues(GetMenuBarWidget(), XmNheight, &ys, NULL);
      yy -= ys;
    }
  }

#endif

  *x = xx; *y = yy;
}

// Set the client size (i.e. leave the calculation of borders etc.
// to wxWindows)
void wxFrame::SetClientSize(int width, int height)
{
  // Calculate how large the new main window should be
  // by finding the difference between the client area and the
  // main window area, and adding on to the new client area
/*
  Dimension current_frame_width, current_frame_height;
  Dimension current_form_width, current_form_height;
  XtVaGetValues(frameShell, XmNwidth, &current_frame_width, XmNheight, &current_frame_height, NULL);
  XtVaGetValues(workArea, XmNwidth, &current_form_width, XmNheight, &current_form_height, NULL);
  int diffX = current_frame_width - current_form_width;
  int diffY = current_frame_height - current_form_height;

  if (width > -1)
    XtVaSetValues(frameShell, XmNwidth, width + diffX, NULL);
  if (height > -1)
  {
    int real_height = height + diffY;
#if 0
    if (status_line_exists)
    {
      Dimension ys;
      XtVaGetValues(statusLineWidget, XmNheight, &ys, NULL);
      real_height += ys;
    }
#endif
    XtVaSetValues(frameShell, XmNheight, real_height, NULL);
  }
*/
  if (width > -1)
    XtVaSetValues(workArea, XmNwidth, width, NULL);

  if (height > -1)
  {
    if (status_line_exists)
    {
      Dimension ys;
      XtVaGetValues(statusLineWidget, XmNheight, &ys, NULL);
      height += ys;
    }
    XtVaSetValues(workArea, XmNheight, height, NULL);
  }
  PreResize();

  GetSize(&lastWidth, &lastHeight);
  sr_width = lastWidth;
  sr_height = lastHeight;
  GetEventHandler()->OnSize(lastWidth, lastHeight);
}

void wxFrame::GetSize(int *width, int *height)
{
  Dimension xx, yy;
  XtVaGetValues(frameShell, XmNwidth, &xx, XmNheight, &yy, NULL);
  *width = xx; *height = yy;
}

void wxFrame::GetPosition(int *x, int *y)
{
//  Widget widget = (Widget)handle;
  Dimension xx, yy;
//  XtVaGetValues(widget, XmNx, &xx, XmNy, &yy, NULL);
  XtVaGetValues(frameShell, XmNx, &xx, XmNy, &yy, NULL);
  *x = xx; *y = yy;
}

void wxFrame::SetSize(int x, int y, int width, int height, int sizeFlags)
{
  Position cx, cy;
  Dimension cw, ch, oh, ow;
  int changed;
  Widget w = frameShell;

  XtVaGetValues(w, 
		XmNx, &cx, 
		XmNy, &cy, 
		XmNwidth, &cw,
		XmNheight, &ch,
		NULL);

  ow = cw;
  oh = ch;

  changed = 0;

  if ((x > -1) && (x != cx)) {
    cx = x;
    changed = 1;
  }
  if ((y > -1) && (y != cy)) {
    cy = y;
    changed = 1;
  }
  if ((width > -1) && (width != cw)) {
    cw = width;
    changed = 1;
  }
  if ((height > -1) && (height != ch)) {
    ch = height;
    changed = 1;
  }

  if (!changed)
    return;

#if 0
  if (status_line_exists && ((oh > ch) || (ow > cw))) {
    Dimension yy, ww, hh;

    XtVaGetValues(statusLineWidget, XmNheight, &yy, NULL);

    if (ow < cw)
      ww = ow;
    else
      ww = cw;
    if (oh < ch)
      hh = oh;
    else
      hh = ch;

    XtUnmanageChild(statusLineWidget);
    XtVaSetValues(statusLineWidget, XmNx, 0, XmNy, hh - yy, XmNwidth, ww, NULL);
    if (statusLineForm)
      XtVaSetValues(statusLineForm,  XmNwidth, ww, NULL);
    XtManageChild(statusLineWidget);
  }
#endif

  XtVaSetValues(w, 
		XmNx, cx, 
		XmNy, cy, 
		XmNwidth, cw,
		XmNheight, ch,
		NULL);

  if (!(height == -1 && width == -1) && !(sizeFlags & 0x100))
  {
    int ww, hh;
    PreResize();
    GetSize(&ww, &hh);
    sr_width = ww;
    sr_height = hh;
    GetEventHandler()->OnSize(ww, hh);
  }

  GetSize(&lastWidth, &lastHeight);
}

void FrameForceFocus(Widget frame)
{
  static int force_focus = 0;

  if (!force_focus) {
    wxGetResource(wxTheApp->wx_class, "forceFocus", &force_focus);
    force_focus = !force_focus ? -1 : 1;
  }

  if (force_focus > 0) {
    Window current;
    int old_revert;
    XGetInputFocus(XtDisplay(frame), &current, &old_revert);
    if (current != PointerRoot) {
      XFlush(XtDisplay(frame));
      XGrabServer(XtDisplay(frame));
      
      XWindowAttributes attrib;
      XGetWindowAttributes(XtDisplay(frame), XtWindow(frame), &attrib);
      if (attrib.map_state == IsViewable)
	XSetInputFocus(XtDisplay(frame), XtWindow(frame),
		       RevertToNone, CurrentTime);
    }
    XUngrabServer(XtDisplay(frame));
  }
}

Bool wxFrame::Show(Bool show)
{
  SetShown(show);

  visibleStatus = show; /* MATTHEW: show-&-hide fix */


  wxTopLevelWindows(this)->Show(this, show);
  if (window_parent)
    window_parent->GetChildren()->Show(this, show);

  if (show) {
    XtMapWidget(frameShell);
    XRaiseWindow(XtDisplay(frameShell), XtWindow(frameShell));
    FrameForceFocus(frameShell);
  } else {
    /* XWithdrawWindow does the right thing for iconified windows */
    XWithdrawWindow(XtDisplay(frameShell), XtWindow(frameShell), 
		    XScreenNumberOfScreen(XtScreen(frameShell)));

    XtUnmapWidget(frameShell);
//    XmUpdateDisplay(wxTheApp->topLevel); // Experimental: may be responsible for crashes
  }

  return TRUE;
}

void wxCloseFrameCallback(Widget, XtPointer client_data, XmAnyCallbackStruct *)
{
  wxFrame *frame = (wxFrame *)client_data;
  wxWindow *modal;

  modal = wxGetModalWindow(frame);
  if (modal && (modal != frame))
    return;

  /* MATTHEW: [8] GC */
  if (frame->GetEventHandler()->OnClose()) {
#if !WXGARBAGE_COLLECTION_ON
    delete frame;
#else
    frame->Show(FALSE);
#endif
  }
}

void wxFrame::Iconize(Bool iconize)
{
  if (!IsShown())
    return;

  if (!iconize) {
    XtMapWidget(frameShell);
  } else {
    XIconifyWindow(XtDisplay(frameShell),
		   XtWindow(frameShell),
		   XScreenNumberOfScreen(XtScreen(frameShell)));
  }
}

// Equivalent to maximize/restore in Windows
void wxFrame::Maximize(Bool maximize)
{
  Show(TRUE);

  if (maximize)
    XtVaSetValues(frameShell, XmNiconic, FALSE, NULL);
}

Bool wxFrame::Iconized(void)
{
  if (!IsShown())
    return FALSE;

  XWindowAttributes wa;

  XGetWindowAttributes(XtDisplay(frameShell), XtWindow(frameShell), &wa);

  return (wa.map_state == IsUnmapped);
}


void wxFrame::SetTitle(char *title)
{
  if (frameTitle)
    delete[] frameTitle;

  frameTitle = copystring(title);

  if (title)
    XtVaSetValues(frameShell, 
                  XmNtitle, title,
                  XmNiconName, title,
                  NULL);
}

char *wxFrame::GetTitle(void)
{
  return frameTitle;
}


void wxFrame::SetIcon(wxBitmap *icon)
{
  if (icon->Ok()) {
    wxBitmap *bm = new wxBitmap(icon->GetWidth(), icon->GetHeight());
    if (bm->Ok()) {
      wxMemoryDC *mdc = new wxMemoryDC();
      mdc->SelectObject(bm);
      mdc->Blit(0, 0, icon->GetWidth(), icon->GetHeight(), icon, 0, 0);
      mdc->SelectObject(NULL);

      XtVaSetValues(frameShell, XtNiconPixmap, bm->x_pixmap, NULL);
      
      frame_icon = bm;
    }
  }
}

void wxFrame::CreateStatusLine(int number, char *name)
{
  if (status_line_exists)
    return;

  nb_status = number;
  status_line_exists = TRUE;

  int i;
  statusLineWidget = XtVaCreateManagedWidget(name,
					     xmFrameWidgetClass,      workArea,
					     XmNshadowType,           XmSHADOW_IN,
					     XmNrightAttachment, XmATTACH_FORM,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNbottomAttachment, XmATTACH_FORM,
					     NULL);

  if (number == 1)
  {
    sprintf(wxBuffer,"status_text%d",0) ;
    statusTextWidget[0] = XtVaCreateManagedWidget(wxBuffer,
#if USE_GADGETS
        xmLabelGadgetClass,      statusLineWidget,
#else
        xmLabelWidgetClass,      statusLineWidget,
#endif
        XmNalignment,            XmALIGNMENT_BEGINNING,
        NULL);
  }
  else
  {
    statusLineForm = XtVaCreateManagedWidget("form",
                xmFormWidgetClass, statusLineWidget,NULL) ;
    for (i=0;i<number;i++)
    {
      sprintf(wxBuffer,"status_text%d",i) ;
      statusTextWidget[i] = XtVaCreateManagedWidget(wxBuffer,
#if USE_GADGETS
          xmLabelGadgetClass,      statusLineForm,
#else
          xmLabelWidgetClass,      statusLineForm,
#endif
          XmNalignment,            XmALIGNMENT_BEGINNING,
          NULL);
    }
    Widget attach_me = NULL  ;
    for (i=0;i<number/2;i++)
    {
      XtVaSetValues(statusTextWidget[i],
          XmNleftAttachment ,  i==0? XmATTACH_FORM:XmATTACH_WIDGET,
          XmNleftWidget ,      attach_me ,
          XmNrightAttachment , XmATTACH_NONE ,
          NULL) ;
      attach_me = statusTextWidget[i] ;
    }
    for (i=number-1;i>=number/2;i--)
    {
      XtVaSetValues(statusTextWidget[i],
          XmNrightAttachment ,i==number-1? XmATTACH_FORM : XmATTACH_WIDGET,
          XmNrightWidget ,    attach_me ,
          XmNleftAttachment , XmATTACH_NONE ,
          NULL) ;
      attach_me = statusTextWidget[i] ;
    }
  }

  XtVaSetValues(clientArea,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, statusLineWidget,
		NULL);


  XtRealizeWidget(statusLineWidget);
//  XtRealizeWidget(statusTextWidget);
  if (statusLineForm)
    XtRealizeWidget(statusLineForm);
  for(i=0;i<number;i++)
    XtRealizeWidget(statusTextWidget[i]);

  for (i=0;i<number;i++)
    SetStatusText(" ",i);

/*
  // Initialize it to set it to the desired height
  XmString str = XmStringCreateSimple(" ");
  XtVaSetValues(statusTextWidget,
                  XmNrecomputeSize, True,
                  XmNlabelString, str,
                  NULL);
  XmStringFree(str);
*/

  PreResize();

  int ww, hh;
  GetSize(&ww, &hh);
  sr_width = ww;
  sr_height = hh;
  GetEventHandler()->OnSize(ww, hh);
}

void wxFrame::SetStatusText(char *text, int number)
{
  if (!status_line_exists)
    return;

  if (!text) text = " ";
  sprintf(wxBuffer, " %s", text);

  // First check if the string is the same as what's there.
  XmString textString;
  char *s = NULL;
  XtVaGetValues(statusTextWidget[number],
                  XmNlabelString, &textString,
                  NULL);

  if (XmStringGetLtoR(textString, XmSTRING_DEFAULT_CHARSET, &s))
  {
    Bool theSame = (strcmp(s, wxBuffer) == 0);
    XtFree(s);
    XmStringFree(textString) ;
    if (theSame)
      return;
  }
  else
  {
    if (s) XtFree(s);
    XmStringFree(textString) ;
  }

  // Not the same, so carry on

  XmString str = XmStringCreateSimple(wxBuffer);
  XtVaSetValues(statusTextWidget[number],
//                  XmNrecomputeSize, False,
                  XmNlabelString, str,
                  NULL);
  XmStringFree(str);
  PreResize();  // Stretch it back to full width!
}

void wxFrame::LoadAccelerators(char *)
{
}

void wxFrame::Fit(void)
{
  int maxX = 0;
  int maxY = 0;
  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    wxWindow *win = (wxWindow *)node->Data();
    if ((wxSubType(win->__type, wxTYPE_PANEL) &&
         !wxSubType(win->__type, wxTYPE_DIALOG_BOX)) ||
        wxSubType(win->__type, wxTYPE_TEXT_WINDOW) ||
        wxSubType(win->__type, wxTYPE_CANVAS))
    {
      int x, y, w, h;
      win->GetPosition(&x, &y);
      win->GetSize(&w, &h);
      if ((x + w) > maxX)
        maxX = x + w;
      if ((y + h) > maxY)
        maxY = y + h;
    }
  }
  SetClientSize(maxX, maxY);
}

Widget wxFrame::GetMenuBarWidget (void)
{
  return (Widget) wx_menu_bar->handle;
}

void wxFrame::SetMenuBar (wxMenuBar * menu_bar)
{
  /* MATTHEW: [6] Protect against resetting the menu bar or using 
     the same bar twice */
  if (wx_menu_bar || menu_bar->menu_bar_frame)
    return;

  Widget MenuBar = XmCreateMenuBar (frameWidget, "MenuBar", NULL, 0);
  menu_bar->handle = (char *) MenuBar;

  int i;
  for (i = 0; i < menu_bar->n; i++)
    {
      wxMenu *menu = menu_bar->menus[i];
      menu->buttonWidget = menu->CreateMenu (menu_bar, MenuBar, menu, menu_bar->titles[i], TRUE);

      wxStripMenuCodes (menu_bar->titles[i], wxBuffer);

      /*
       * COMMENT THIS OUT IF YOU DON'T LIKE A RIGHT-JUSTIFIED HELP MENU
       */
      if (strcmp (wxBuffer, wxSTR_MENU_HELP) == 0)
	XtVaSetValues(MenuBar, XmNmenuHelpWidget, menu->buttonWidget, NULL);
    }

  XtRealizeWidget (MenuBar);
  XtManageChild (MenuBar);

  wx_menu_bar = menu_bar;
  menu_bar->menu_bar_frame = this;
}

wxFrame::~wxFrame (void)
{
  if (handle)
    Show (FALSE);

  if (wx_menu_bar)
  {
// Hack to stop core dump on Ultrix, OSF, for some strange reason.
#if MOTIF_MENUBAR_DELETE_FIX
    wx_menu_bar->handle = 0;
#endif
    delete wx_menu_bar;
  }
#if !WXGARBAGE_COLLECTION_ON
  if (icon)
    delete icon;
#endif

  if (frameTitle)
    delete[]frameTitle;

  DestroyChildren();

  int i;
  for (i = 0; i < wxMAX_STATUS; i++)
    if (statusTextWidget[i])
      XtDestroyWidget (statusTextWidget[i]);

  if (statusLineForm)
    XtDestroyWidget (statusLineForm);

  if (statusLineWidget)
    XtDestroyWidget (statusLineWidget);

  wxWidgetHashTable->Delete ((long) workArea);

  XtDestroyWidget(workArea);
  XtDestroyWidget(frameWidget);
  wxWidgetHashTable->Delete((long)frameWidget);
  XtDestroyWidget(frameShell);

  wxUnregisterFrameWidget(frameShell);

  handle = NULL;
}

void wxFrame::CaptureMouse(void)
{
  if (winCaptured)
    return;
    
  if (handle)
    XtAddGrab(frameShell, TRUE, FALSE);

  winCaptured = TRUE;
}

void wxFrame::ReleaseMouse(void)
{
  if (!winCaptured)
    return;
    
  if (handle)
    XtRemoveGrab(frameShell);

  winCaptured = FALSE;
}


void wxFrame::GrowReady()
{
  XtVaSetValues(frameWidget,
		XmNresizePolicy, XmRESIZE_GROW,
		NULL);
  XtVaSetValues(workArea,
		XmNresizePolicy, XmRESIZE_GROW,
		NULL);
}

void wxFrame::GrowDone()
{
  XtVaSetValues(frameWidget,
		XmNresizePolicy, XmRESIZE_NONE,
		NULL);
  XtVaSetValues(workArea,
		XmNresizePolicy, XmRESIZE_NONE,
		NULL);
}

