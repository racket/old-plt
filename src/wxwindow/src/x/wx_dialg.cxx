/*
 * File:	wx_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_dialg.cxx,v 1.11 1998/11/17 21:40:45 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// $Log: wx_dialg.cxx,v $
// Revision 1.11  1998/11/17 21:40:45  mflatt
// .
//
// Revision 1.10  1998/10/16 18:19:41  mflatt
// .
//
// Revision 1.9  1998/09/23 01:11:14  mflatt
// .
//
// Revision 1.8  1998/08/21 00:31:40  mflatt
// .
//
// Revision 1.7  1998/08/09 20:55:25  mflatt
// .
//
// Revision 1.6  1998/07/13 19:08:21  mflatt
// .
//
// Revision 1.5  1998/04/08 00:09:12  mflatt
// beginning of fixing pre-on-char
//
// Revision 1.4  1998/03/07 14:23:45  mflatt
// dialog transience
//
// Revision 1.3  1998/03/07 00:37:48  mflatt
// dialog parents
//
// Revision 1.2  1998/02/10 02:50:15  mflatt
// dialog fixes
//
// Revision 1.1.1.1  1997/12/22 16:12:04  mflatt
// import
//
// Revision 1.62  1994/11/02  22:18:47  edz
// Minor changes.
//
// Revision 1.61  1994/11/02  12:07:36  edz
// Cleaned up the X file dialog box. Also made the current working
// directory follow the selection like the file manager--- this
// is better HCI logic.
//
// Revision 1.60  1994/11/02  11:13:15  edz
// Fixed the push-pin problem/close modal window problem
// --- still no definitive answer.
//
//
//

static const char sccsid[] = "%W% %G%";

#ifdef __GNUG__
#pragma implementation
#pragma interface
#endif

#include <iostream.h>
#include <stdio.h>
#include "common.h"
#include "wx_lbox.h"
#include "wx_buttn.h"
#include "wx_choic.h"
#include "wx_check.h"
#include "wx_messg.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wx_privt.h"
#include "wx_main.h"

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

#include <X11/Shell.h>
#if XmVersion >= 1002
#include <Xm/XmAll.h>
#endif
#include <Xm/MwmUtil.h>
#include <Xm/Label.h>
#include <Xm/BulletinB.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/AtomMgr.h>
#if   XmVersion > 1000
#include <Xm/Protocols.h>
#endif
void wxCloseDialogCallback(Widget widget, XtPointer client_data, XmAnyCallbackStruct *cbs);
void wxDialogBoxRepaintProc(Widget w, XtPointer c_data, XEvent *event, char *);
void wxDialogBoxEventHandler (Widget    wid,
                              XtPointer client_data,
                              XEvent*   event,
                              Boolean *continueToDispatch);

char *wxMotifFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y);

extern void wxRegisterFrameWidget(Widget);
extern void wxUnregisterFrameWidget(Widget);

// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal

IMPLEMENT_DYNAMIC_CLASS(wxDialogBox, wxPanel)

wxDialogBox::wxDialogBox(void)
{
  dialogTitle = NULL;

  borderWidget = 0;
  wxType = 1;
  invisibleResize = FALSE ;

  has_child = FALSE ;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing ;
  initial_vspacing = vSpacing ;

  current_hspacing = hSpacing ;
  current_vspacing = vSpacing ;
  new_line = FALSE; // @@@@@
  firstRowWidget = NULL;
  lastWidget = NULL;
  dialogShell = NULL;
  panelWidget = NULL;
  handle = NULL;
  modal_showing = FALSE;

  modal = FALSE;
}

void UnmapBulletinBoard(Widget w)
{
  wxDialogBox *client = (wxDialogBox *)wxWidgetHashTable->Get((long)w);

  if (client) {
    client->modal_showing = FALSE;
    client->SetShowing(FALSE);
  }
}

void UnmapShell(Widget, Widget w, XEvent *e)
{
  wxDialogBox *client = (wxDialogBox *)wxWidgetHashTable->Get((long)w);

  if (client) {
    if (e->xany.type == UnmapNotify) {
      if (client->IsShown()) {
	/* Deiconize if it was iconized... */
	client->Show(TRUE);
      }
    }
  }
}

wxDialogBox::wxDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height,
                         long style, char *name):
  wxbDialogBox((wxWindow *)Parent, Title, Modal, x, y, width, height, style, name)
{
  Create(Parent, Title, Modal, x, y, width, height, style, name);
}

extern void wxInstallOnActivate(Widget frameShell, Widget workArea);

Bool wxDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height,
                         long style, char *name)
{
  wxbDialogBox::Create(Parent, Title, Modal, x, y, width, height, style, name);

  SetName(name);
  has_child = FALSE;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;

  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  labelFont = wxNORMAL_FONT;
  buttonFont = wxNORMAL_FONT;
  
  if (Title) dialogTitle = copystring(Title);

  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  modal_showing = FALSE;

  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

  if (width == -1)
    width = 10;
  if (height == -1)
    height = 10;

  invisibleResize = TRUE;

  borderWidget = 0;
  wxType = 1;
  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;

  new_line = FALSE; // @@@@
  firstRowWidget = NULL;
  lastWidget = NULL;

  // Create dialog box as BulletinBoard. It seems better to me (Patrick), Dialog are
  // always over parent's frame.
  // Side effect: no Minimize/Maximize buttons
  Widget parentShell = 0;

  if (Parent) {
    if (wxSubType(Parent->__type, wxTYPE_FRAME))
      parentShell = ((wxFrame *)Parent)->frameWidget;
    else if (wxSubType(Parent->__type, wxTYPE_DIALOG_BOX))
      parentShell = ((wxDialogBox *)Parent)->dialogShell;
    else
      Parent = NULL;
  }

  if (!parentShell) {
    parentShell = XtAppCreateShell(wxTheApp->appName, wxTheApp->wx_class,
				   applicationShellWidgetClass,
				   wxGetDisplay(), NULL, 0);
    wxRegisterFrameWidget(parentShell);
    localParentShell = parentShell;

    XtVaSetValues(parentShell, XmNmappedWhenManaged, False, NULL);
    XtRealizeWidget(parentShell);
  } else
    localParentShell = 0;

#if 0
  // Make sure the parent is not iconized,
  // or the dialog will not appear!
  if (Parent && Parent->Iconized())
    Parent->Iconize(FALSE);
#endif

  // Force dialog box to be positioned correctly
  Arg args[1];
  XtSetArg(args[0], XmNdefaultPosition, False);
  dialogShell = XmCreateBulletinBoardDialog(parentShell, windowName, args, 1);

  // We don't want margins, since there are enough elsewhere.
  XtVaSetValues(dialogShell,
		XmNmarginHeight, 0,
		XmNmarginWidth, 0,
		XmNresizePolicy, /* (style & wxPUSH_PIN) ? XmRESIZE_GROW : XmRESIZE_NONE */ XmRESIZE_NONE,
		NULL);

  panelWidget = dialogShell;

  Widget shell = XtParent(dialogShell);
  if (Title) {
    XmString str = XmStringCreateSimple(Title);
    XtVaSetValues(dialogShell,
                  XmNdialogTitle, str,
                  NULL);
    XmStringFree(str);
  }

  if (wxWidgetHashTable->Get((long)dialogShell))
    wxError("Widget table clash in wx_dialg.cc");

  wxWidgetHashTable->Put((long)dialogShell, this);

  // Intercept CLOSE messages from the window manager
  Atom WM_DELETE_WINDOW = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);

  /* MATTHEW: [8] Remove and add WM_DELETE_WINDOW so ours is only handler */
  /* Why do we have to do this for wxDialog, but not wxFrame? */
  XmRemoveWMProtocols(shell, &WM_DELETE_WINDOW, 1);
  XmAddWMProtocols(shell, &WM_DELETE_WINDOW, 1);
  XmActivateWMProtocol(shell, WM_DELETE_WINDOW);

  // Modified Steve Hammes for Motif 2.0
#if (XmREVISION > 1 || XmVERSION > 1)
  XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW, (XtCallbackProc) wxCloseDialogCallback, (XtPointer)this);
#elif XmREVISION == 1
  XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW, (XtCallbackProc) wxCloseDialogCallback, (caddr_t)this);
#else
  XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW, (void (*)())wxCloseDialogCallback, (caddr_t)this);
#endif

  XtTranslations ptr;
  XtOverrideTranslations(dialogShell,
			 ptr = XtParseTranslationTable("<Configure>: resize()"));
  XtFree((char *)ptr);
  
  if (invisibleResize) {
    if (x > -1)
      XtVaSetValues(dialogShell, XmNx, x, NULL);
    if (y > -1)
      XtVaSetValues(dialogShell, XmNy, y, NULL);

    if (width > -1)
      XtVaSetValues(panelWidget, XmNwidth, width, NULL);
    if (height > -1)
      XtVaSetValues(panelWidget, XmNheight, height, NULL);
  }

  // This patch come from Torsten Liermann lier@lier1.muc.de
  if (XmIsMotifWMRunning(wxTheApp->topLevel)) {
    int decor = 0 ;
    if (!(style & wxNO_SYSTEM_MENU))
      decor |= MWM_DECOR_MENU;
    if (!(style & wxNO_CAPTION))
      decor |= MWM_DECOR_TITLE;
    decor |= MWM_DECOR_BORDER;
    
    XtVaSetValues(shell,XmNmwmDecorations,decor,NULL);
  }

  XtRealizeWidget(dialogShell);
  
  XtAddCallback(dialogShell, XmNunmapCallback,
		(XtCallbackProc)UnmapBulletinBoard, 
		NULL);

  {
    wxWindow *p;

    for (p = Parent; p; p = p->GetParent()) {
      if (!wxSubType(p->__type, wxTYPE_DIALOG_BOX))
	break;
    }

    if (!p)
      XtInsertEventHandler(XtParent(dialogShell),
			   StructureNotifyMask,
			   TRUE,
			   (XtEventHandler)UnmapShell,
			   (XtPointer)dialogShell,
			   XtListHead);
  }
  
  // Positioning of the dialog doesn't work properly unless the dialog
  // is managed, so we manage without mapping to the screen.
  // To show, we map the shell (actually it's parent).

  if (!invisibleResize)
    XtVaSetValues(XtParent(dialogShell), XmNmappedWhenManaged, FALSE, NULL);

  handle = (char *)panelWidget;
  if (!invisibleResize) {
    XtManageChild(dialogShell);
    SetSize(x, y, width, height);
  }
  XtAddEventHandler(panelWidget, ExposureMask, FALSE,
		    wxDialogBoxRepaintProc, (XtPointer) this);

  wxInstallOnActivate(dialogShell, dialogShell);

  XtAddEventHandler(panelWidget,
		    ButtonPressMask | ButtonReleaseMask | PointerMotionMask | KeyPressMask,
		    FALSE,
		    wxDialogBoxEventHandler,
		    (XtPointer)this);

  wx_dc = new wxPanelDC(this);

  AddPreHandlers(panelWidget, dialogShell);

  modal = Modal;
  return TRUE;
}

wxDialogBox::~wxDialogBox()
{
  if (dialogTitle)
    delete[] dialogTitle;
  modal_showing = FALSE;

  if (!invisibleResize && dialogShell)
    XtUnmapWidget(dialogShell);

  if (localParentShell)
    wxUnregisterFrameWidget(localParentShell);
}

void wxDialogBox::OnSize(int, int)
{
  wxChildNode* node = GetChildren()->First(); 

  if (node && !node->Next()) {
    wxWindow *win = (wxWindow *)node->Data();
    Bool hasSubPanel = (wxSubType(win->__type, wxTYPE_PANEL && !wxSubType(win->__type, wxTYPE_DIALOG_BOX)) ||
			wxSubType(win->__type, wxTYPE_CANVAS) ||
			wxSubType(win->__type, wxTYPE_TEXT_WINDOW));
    
    if (hasSubPanel) {
      int w, h;
      GetClientSize(&w, &h);
      win->SetSize(0, 0, w, h);
    }
  }
}

void wxDialogBox::PostDestroyChildren(void)
{
}

Window wxDialogBox::GetXWindow(void)
{
  return (Window)XtWindow((Widget)handle);
}

/*
 * It's necessary to put this dialog-specific stuff in the base class
 * because for some reason wxDialogBox::PostDestroyChildren
 * doesn't get called from ~wxWindow. Perhaps this is because C++
 * is crap. Why should the virtual member function calling rules
 * be changed just because we're in a destructor?!!
 *
 */

void wxWindow::PostDestroyChildren(void)
{
  if (wxType == 1) {
    wxDialogBox *box = (wxDialogBox *)this;
    XtDestroyWidget(box->dialogShell);

    if (box->localParentShell)
      XtDestroyWidget(box->localParentShell);

    // Now process all events, because otherwise
    // this might remain on the screen
    XSync(XtDisplay(wxTheApp->topLevel), FALSE);
    while (wxTheApp->Pending()) {
      XFlush(XtDisplay(wxTheApp->topLevel));
      wxTheApp->Dispatch();
    }
  }
}

void wxCloseDialogCallback(Widget, XtPointer client_data, XmAnyCallbackStruct *)
{
  wxDialogBox *dialog = (wxDialogBox *)client_data;
  wxWindow *modal;

  modal = wxGetModalWindow(dialog);
  if (modal && (modal != dialog))
    return;

  /* MATTHEW: [8] GC */
  if (dialog && dialog->GetEventHandler()->OnClose()) {
    dialog->Show(FALSE);
#if !WXGARBAGE_COLLECTION_ON
    delete dialog;
#endif
  }
}

void wxDialogBoxRepaintProc(Widget w, XtPointer, XEvent *event, char *)
{
  wxPanel *panel;
  // Window window;
  static XRectangle *xrect;
  Display *display;
  GC gc;
  int llp = 0;
  static int last_count = 0;
  static int draw_count = 0;
  
  panel = (wxPanel *)wxWidgetHashTable->Get((long)w);

  if (panel)
    switch(event -> type) {
    case Expose :
      // window = (Window) panel -> GetXWindow();
      display = (Display *) panel -> GetXDisplay();
      gc = (GC) panel -> GetDC() -> gc;
      
      llp = event -> xexpose.count;
      
      if ((last_count == 0) && (llp == 0))
	{
	  xrect = new XRectangle[1];
	  xrect[0].x = event -> xexpose.x;
	  xrect[0].y = event -> xexpose.y;
	  xrect[0].width = event -> xexpose.width;
	  xrect[0].height = event -> xexpose.height;
	  
	  XSetClipRectangles(display,gc,0,0,xrect,1,Unsorted);
	  //                    panel->DoPaint(xrect, 1);
	  panel->GetEventHandler()->OnPaint();

	  delete xrect;
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
	  //                    panel->DoPaint(xrect,draw_count);
	  panel->GetEventHandler()->OnPaint();

	  delete xrect;
	}
      last_count = event -> xexpose.count;
      break;
      default :
	cout << "\n\nNew Event ! is = " << event -> type << "\n";
      break;
    }
}

void wxDialogBoxEventHandler(Widget wid,
			     XtPointer,
			     XEvent*   event,
			     Boolean *continueToDispatch)
{
  wxDialogBox *dialog = (wxDialogBox *)wxWidgetHashTable->Get((long)wid);
  if (dialog) {
    wxMouseEvent *wxevent = new wxMouseEvent(0);
    if (wxTranslateMouseEvent(*wxevent, dialog, event))
      dialog->GetEventHandler()->OnEvent(*wxevent);
  }
  *continueToDispatch = True;
}

void wxDialogBox::Fit(void)
{
  wxPanel::Fit(); // Doesn't work properly for dialog boxes, Motif 1.2
}

void wxDialogBox::Iconize(Bool)
{
  /* Not implemented for MrEd dialogs; show uniconizes, though */
}

Bool wxDialogBox::Iconized(void)
{
  /* Not implemented for MrEd dialogs */
  return FALSE;
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int sizeFlags)
{
//  Widget shell = XtParent((Widget)handle) ;
  Widget shell = (Widget)handle ;
  XtVaSetValues(shell, XmNresizePolicy, XmRESIZE_ANY, NULL);

//  Widget shellParent = XtParent(shell);

/*
  if (width > -1)
  {
    XtVaSetValues((Widget)handle, XmNwidth, width, NULL);
//    XtVaSetValues(panelWidget, XmNwidth, width, NULL);
  }
  if (height > -1)
  {
    XtVaSetValues((Widget)handle, XmNheight, height, NULL);
//    XtVaSetValues(panelWidget, XmNheight, height, NULL);
  }

  if (invisibleResize)
  {
    if (width > -1)
      XtVaSetValues(dialogShell, XmNwidth, width, NULL);
    if (height > -1)
      XtVaSetValues(dialogShell, XmNheight, height, NULL);
  }
*/
  if (x > -1)
    XtVaSetValues(shell, XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues(shell, XmNy, y, NULL);
//  if (!invisibleResize)
  {
    if (width > -1)
      XtVaSetValues(shell, XmNwidth, width, NULL);
    if (height > -1)
      XtVaSetValues(shell, XmNheight, height, NULL);
  }

  XtVaSetValues(shell, 
		XmNresizePolicy, 
		/* (windowStyle & wxPUSH_PIN) ? XmRESIZE_GROW : XmRESIZE_NONE */ XmRESIZE_NONE, 
		NULL);

  if (!(sizeFlags & 0x100)) {
    sr_width = width;
    sr_height = height;
    GetEventHandler()->OnSize(width, height);
  }
}

void wxDialogBox::GetSize(int *w, int *h)
{
//  Widget shell = XtParent((Widget)handle) ;
  Widget shell = (Widget)handle ;
  Dimension ww, hh;
  XtVaGetValues(shell, XmNwidth, &ww, XmNheight, &hh, NULL);
  *w = ww;
  *h = hh;
}

void wxDialogBox::SetClientSize(int width, int height)
{
//  Widget shell = XtParent((Widget)handle) ;
  Widget shell = (Widget)handle ;
  XtVaSetValues(shell, XmNresizePolicy, XmRESIZE_ANY, NULL);
  if (!invisibleResize)
  {
    int xx, yy;
    GetPosition(&xx, &yy);
    SetSize(xx, yy, width, height);
  } else {
    if (width > -1)
      XtVaSetValues((Widget)handle, XmNwidth, width, NULL);
    if (height > -1)
      XtVaSetValues((Widget)handle, XmNheight, height, NULL);
  }
  XtVaSetValues(shell, 
		XmNresizePolicy, 
		/* (windowStyle & wxPUSH_PIN) ? XmRESIZE_GROW : XmRESIZE_NONE */ XmRESIZE_NONE, 
		NULL);
  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize(width, height);
}

void wxDialogBox::GetPosition(int *x, int *y)
{
  Dimension xx, yy;
  XtVaGetValues(dialogShell, XmNx, &xx, XmNy, &yy, NULL);
  *x = xx; *y = yy;
}

// A stack of modal_showing flags, since we can't rely
// on accessing wxDialogBox::modal_showing within
// wxDialogBox::Show in case a callback has deleted the wxDialogBox.
static wxList wxModalShowingStack;

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static int CheckDialogShowing(void *data)
{
  return !((wxDialogBox *)data)->modal_showing;
}

extern void FrameForceFocus(Widget frame);

Bool wxDialogBox::Show(Bool show)
{
  if (show == IsShown()) {
    if (show) {
      Widget top;
      
      top = XtParent(dialogShell);
      
      if (!invisibleResize)
	XtMapWidget(top);
      else
	XtManageChild(dialogShell);

      XtMapWidget(top); /* to uniconify */
      
      XRaiseWindow(XtDisplay(top), XtWindow(top));
      FrameForceFocus(top);
    }
    return TRUE;
  }

  SetShown(show);

  wxTopLevelWindows(this)->Show(this, show);
  if (window_parent)
    window_parent->GetChildren()->Show(this, show);

  if (show) {
    Widget top;

    top = XtParent(dialogShell);

    if (!invisibleResize)
      XtMapWidget(top);
    else
      XtManageChild(dialogShell);

    XtMapWidget(top); /* to uniconify */
    XRaiseWindow(XtDisplay(top), XtWindow(top));
    FrameForceFocus(top);

    if (modal) {
      if (modal_showing)
        return TRUE;

      modal_showing = TRUE;      

      wxWindow *saveModal;
      saveModal = wxGetModalWindow(this);
      wxPutModalWindow(this, this);
      
      wxList disabled_windows;
      wxChildNode *cnode;
      wxNode *node;
      
      for (cnode = wxTopLevelWindows(this)->First(); cnode; cnode = cnode->Next()) {
	wxWindow *w = (wxWindow *)cnode->Data();
	if (w && cnode->IsShown() && w != this) {
	  disabled_windows.Append(w);
	  w->InternalEnable(FALSE);
	}
      }
      
      wxDispatchEventsUntil(CheckDialogShowing, (void *)this);
      
      wxPutModalWindow(this, saveModal);
      
      for (node = disabled_windows.First(); node; node = node->Next()) {
	wxWindow *w = (wxWindow *)node->Data();
	w->InternalEnable(TRUE);
      } 

      // Now process all events, to ensure widget destruction
      XSync(XtDisplay(wxTheApp->topLevel), FALSE);
      while (wxTheApp->Pending()) {
        XFlush(XtDisplay(wxTheApp->topLevel));
	wxTheApp->Dispatch();
      }
    }
    else
      modal_showing = FALSE;
  } else {
    /* XWithdrawWindow does the right thing for iconified windows */
    XWithdrawWindow(XtDisplay(dialogShell), XtWindow(dialogShell), 
		    XScreenNumberOfScreen(XtScreen(dialogShell)));

    if (!invisibleResize)
      XtUnmapWidget(XtParent(dialogShell));
    else
      XtUnmanageChild(dialogShell);

    modal_showing = FALSE;

    XFlush(XtDisplay(wxTheApp->topLevel));

    XSync(XtDisplay(wxTheApp->topLevel), FALSE);
  }

  return TRUE;
}


void wxDialogBox::SetTitle(char *title)
{
  if (dialogTitle)
    delete[] dialogTitle;

  dialogTitle = copystring(title);

  if (title)
    XtVaSetValues(dialogShell, 
                  XmNtitle, title,
                  XmNiconName, title,
                  NULL);
}

char *wxDialogBox::GetTitle(void)
{
  return dialogTitle;
}

