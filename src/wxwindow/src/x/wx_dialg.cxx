/*
 * File:	wx_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_dialg.cc,v 1.63 1994/11/02 22:18:47 edz Exp edz $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// $Log: wx_dialg.cc,v $
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

#ifdef wx_motif
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
#endif

#ifdef wx_xview
#include <dirent.h>
#include <unistd.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/notice.h>

#define USE_DEFAULT_DIALOG_ICON

#ifdef  USE_DEFAULT_DIALOG_ICON
static short closed_wxdialog_bits[] =  {
#if 1
/* Format_version=1, Width=64, Height=64, Depth=1, Valid_bits_per_item=16
 */
	0x0055,	0x5555,	0x5555,	0x5000, 0x002A,	0xAAAA,	0xAAAA,	0xA800,
	0x0040,	0x0000,	0x0000,	0x0800, 0x0020,	0xA000,	0x0000,	0x1000,
	0x0045,	0xFAAA,	0xAAAA,	0xB000, 0x0023,	0x8155,	0x5555,	0x5800,
	0x0045,	0x12AA,	0xAAAA,	0xB000, 0x0023,	0xF2AA,	0xAAAA,	0xB800,
	0x0045,	0x1155,	0x5555,	0x5000, 0x0022,	0x82AA,	0xAAAA,	0xB800,
	0x0047,	0xFFFF,	0xFFFF,	0xF000, 0x0026,	0xAAAA,	0xAAAA,	0x9800,
	0x0040,	0x0000,	0x0000,	0x1000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAFA,	0xB000, 0x0025,	0x5555,	0x55DD,	0x5800,
	0x0042,	0xAAAA,	0xAAAE,	0xB000, 0x0025,	0x5555,	0x555D,	0x5800,
	0x0042,	0xAAAA,	0xAABA,	0xB000, 0x0025,	0x5555,	0x5575,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5575,	0x5800,
	0x0042,	0xFFFF,	0xFFBA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x0042,	0xAAAA,	0xAAAA,	0xB000, 0x0025,	0x5555,	0x5555,	0x5800,
	0x004F,	0xFFFF,	0xFFFF,	0xF000, 0x002A,	0xAAAA,	0xAAAA,	0xA800,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
	0x0000,	0x0000,	0x0000,	0x0000, 0x0000,	0x0000,	0x0000,	0x0000,
#else 
/* Format_version=1, Width=64, Height=64, Depth=1, Valid_bits_per_item=16
 */
	0xFFFF,	0xFFFF,	0xFFFF,	0xF89C, 0xFFFF,	0xFFFF,	0xFFFF,	0xF8A2,
	0xE18C,	0x4216,	0x8439,	0x1A61, 0xC9CF,	0xD6CC,	0x71E8,	0x5A91,
	0xC31C,	0xCDED,	0x9B72,	0x1989, 0xD38C,	0xF9CD,	0xDB30,	0x9A46,
	0xC739,	0x99DB,	0x99E4,	0x3E26, 0xE319,	0xFBBB,	0x9621,	0x1B1A,
	0xCFE3,	0xDDDC,	0xE3C8,	0x5498, 0xC210,	0x8421,	0x0FE2,	0x3968,
	0xD084,	0x2108,	0x4A70,	0xD262, 0xC421,	0x0842,	0x17C4,	0xA4A2,
	0xE108,	0x4210,	0x8421,	0x4988, 0xC842,	0x1084,	0x210A,	0xD288,
	0xC210,	0x8421,	0x0847,	0x3622, 0xD0E6,	0xEFCF,	0x7BFA,	0xCA22,
	0xC4F7,	0xEFEF,	0xFDF4,	0xB888, 0xE108,	0x4210,	0x8429,	0x3888,
	0xC842,	0x1084,	0x215A,	0x5A22, 0xC7B9,	0xEFBD,	0x9DE6,	0x9A22,
	0xD7FD,	0xFFDF,	0xDF59,	0x9888, 0xC421,	0x0842,	0x1296,	0x3888,
	0xE108,	0x4210,	0x8525,	0x1A22, 0xCF7F,	0x3CEC,	0xFB48,	0x5A22,
	0xC7BF,	0xBEFF,	0xFCD2,	0x1888, 0xD084,	0x2108,	0x6B30,	0x9888,
	0xC421,	0x0842,	0x52C4,	0x3A22, 0xE7EF,	0x67F7,	0xA4E1,	0x1A22,
	0xCFF7,	0xF7F7,	0x69F8,	0x5888, 0xC210,	0x8423,	0x9A42,	0x1888,
	0xD084,	0x210D,	0x6610,	0x9A22, 0xC7BD,	0x99EA,	0x5F84,	0x3A22,
	0xE7DF,	0xDFF4,	0x9FA1,	0x1888, 0xC842,	0x10AD,	0x2108,	0x5888,
	0xC210,	0x8473,	0x4842,	0x1A22, 0xD7BC,	0x21EC,	0xC210,	0x9A22,
	0xC7BD,	0x096B,	0x1084,	0x3888, 0xE108,	0x4312,	0x8421,	0x1888,
	0xC842,	0x128C,	0x2108,	0x5A22, 0xC210,	0x8639,	0x0842,	0x1A22,
	0xD084,	0x2768,	0x4210,	0x9888, 0xC421,	0x0FC2,	0x1084,	0x3888,
	0xE108,	0x4E10,	0x8421,	0x1A22, 0xC842,	0x1884,	0x2108,	0x5A22,
	0xC210,	0x8421,	0x0842,	0x1888, 0xD084,	0x2108,	0x4210,	0x9888,
	0xCF6F,	0x09C3,	0x9084,	0x3A22, 0xE76C,	0x4399,	0x8421,	0x1A22,
	0xCE66,	0x109C,	0x210E,	0x5888, 0xC6F8,	0xFFBF,	0xBE7E,	0x1888,
	0xD6ED,	0xB999,	0xDB74,	0x9A22, 0xC731,	0xBB73,	0x1BFC,	0x3A22,
	0xE339,	0xF333,	0xB621,	0x1888, 0xCB72,	0xDBBF,	0xB77C,	0x5888,
	0xC210,	0x8421,	0x08CE,	0x1A22, 0xD084,	0x2108,	0x42F8,	0x9A22,
	0xC421,	0x0842,	0x1084,	0x3888, 0xE108,	0x4210,	0x8421,	0x1888,
	0xC842,	0x1084,	0x2108,	0x5A22, 0xC210,	0x8421,	0x0842,	0x1A22,
	0xD084,	0x2108,	0x4210,	0x9888, 0xC421,	0x0842,	0x1084,	0x3888,
	0xFFFF,	0xFFFF,	0xFFFF,	0xFA22, 0xFFFF,	0xFFFF,	0xFFFF,	0xFA22,
#endif
};
#endif

extern "C" Frame xv_window_loop(Frame);
extern "C" void xv_window_return(Frame);

extern void wxPanelEventProc(Panel panel, Event *event); // See wx_item.cc
extern void wxPanelInterposeProc(Panel x_panel, Event *event, Notify_arg arg, Notify_event_type type);
Notify_value wxDialogInterposer(Frame x_frame, Event *x_event, Notify_arg arg,
                               Notify_event_type type);
Notify_value wxDialogCloseInterposer(Notify_client client, Destroy_status status);

// Sometimes calls notify function after frame has died, so set this when frame
// is deleted, to do nothing.
Notify_value wxDummyDialogInterposer(Frame x_frame, Event *x_event, Notify_arg arg,
                               Notify_event_type type);
char *wxXFileSelector(wxWindow *parent, char *path, char *file, char *message, int flags, char *wild_card, int x, int y);
#endif

// The Maximum length of a file path
#define _MAXPATHLEN 1024 /* @@@@ */

// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal

IMPLEMENT_DYNAMIC_CLASS(wxDialogBox, wxPanel)

wxDialogBox::wxDialogBox(void)
{
  dialogTitle = NULL;
  panelBackgroundBrush = NULL;

#ifdef wx_motif
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
#endif
#ifdef wx_xview
  xFrame = XV_NULL;
  handle = NULL;
  has_child = FALSE; // @@@
#endif
  modal = FALSE;
}

#ifdef wx_motif
void UnmapBulletinBoard(Widget dialog,wxDialogBox *client,XtPointer call)
{
  client->modal_showing = FALSE ;
  client->SetShowing(FALSE) ;
}
#endif

wxDialogBox::wxDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height,
                         long style, char *name):
  wxbDialogBox((wxWindow *)Parent, Title, Modal, x, y, width, height, style, name)
{
  Create(Parent, Title, Modal, x, y, width, height, style, name);
}

#ifdef wx_xview

void Done_proc(Panel x_panel)
{
  ; // Block quit
}


// Hack function to handle XView push pins
// [Handles both mapped and unmapped frames]

void PopUp_done_proc(Frame subframe)
{
  // If the push-pin is pulled out...
  if (xv_get(subframe, FRAME_CMD_PIN_STATE) != FRAME_CMD_PIN_IN) {
    Panel x_panel = (Panel) xv_get (subframe, FRAME_CMD_PANEL);

    // Make sure the panel is still useable
    if (! xv_get(x_panel, WIN_CLIENT_DATA)) {
	// Looks like an old frame so...
	xv_set(subframe, XV_SHOW, FALSE, NULL);
	return;
    }

    // Find the default item, if any, and execute its callback
    Panel_item item = (Panel_item) xv_get (x_panel, PANEL_DEFAULT_ITEM);
    if (item) {
      wxButton *wx_item = (wxButton *) xv_get (item, PANEL_CLIENT_DATA);
      if (wx_item) {
	// Push the default button....
        wxCommandEvent event = new wxCommandEvent (wxEVENT_TYPE_BUTTON_COMMAND);
        event->eventObject = wx_item;
        wx_item->ProcessCommand (*event);
      }
     }
    /* we should push the pin back in */
    xv_set(subframe, FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN, NULL); 
  }
}
#endif

extern void wxInstallOnActivate(Widget frameShell, Widget workArea);

Bool wxDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height,
                         long style, char *name)
{
  wxbDialogBox::Create(Parent, Title, Modal, x, y, width, height, style, name);

  SetName(name);
  has_child = FALSE ;

  panelBackgroundBrush = NULL;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  
  initial_hspacing = hSpacing ;
  initial_vspacing = vSpacing ;

  current_hspacing = hSpacing ;
  current_vspacing = vSpacing ;

  if (Title) dialogTitle = copystring(Title);

  if (Parent) Parent->AddChild(this);
  window_parent = Parent;

  modal_showing = FALSE;

  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

#ifdef wx_motif
  if (width == -1)
    width = 600;
  if (height == -1)
    height = 600;

//  invisibleResize = (style & wxMOTIF_RESIZE)!=0 ;
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
    {
      parentShell = ((wxFrame *)Parent)->frameShell;

     // Make sure the parent is not iconized,
     // or the dialog will not appear!
     if (((wxFrame *)Parent)->Iconized())
       ((wxFrame *)Parent)->Iconize(FALSE);
    }
    else if (wxSubType(Parent->__type, wxTYPE_DIALOG_BOX))
      parentShell = ((wxDialogBox *)Parent)->dialogShell;
    else
    {
       parentShell = wxTheApp->topLevel;
       // Make sure the parent is not iconized,
       // or the dialog will not appear!
       if (wxTheApp->wx_frame && wxTheApp->wx_frame->Iconized())
         wxTheApp->wx_frame->Iconize(FALSE);
    }
  } else
  {
     parentShell = wxTheApp->topLevel;

     // Make sure the parent is not iconized,
     // or the dialog will not appear!
     if (wxTheApp->wx_frame && wxTheApp->wx_frame->Iconized())
       wxTheApp->wx_frame->Iconize(FALSE);
  }

  // Force dialog box to be positioned correctly
  Arg args[1];
  XtSetArg (args[0], XmNdefaultPosition, False);
  dialogShell = XmCreateBulletinBoardDialog(parentShell, windowName, args, 1);

  // We don't want margins, since there is enough elsewhere.
  XtVaSetValues(dialogShell,
          XmNmarginHeight,   0,
          XmNmarginWidth,    0,
          XmNresizePolicy, /* (style & wxPUSH_PIN) ? XmRESIZE_GROW : XmRESIZE_NONE */ XmRESIZE_NONE,
          NULL) ;

  panelWidget = dialogShell;

  Widget shell = XtParent(dialogShell) ;
  if (Title)
  {
    XmString str = XmStringCreateSimple(Title);
    XtVaSetValues(dialogShell,
                  XmNdialogTitle, str,
                  NULL);
    XmStringFree(str);
  }

  if (wxWidgetHashTable->Get((long)dialogShell))
  {
    wxError("Widget table clash in wx_dialg.cc");
  }

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

  XtTranslations ptr ;
  XtOverrideTranslations(dialogShell,
              ptr = XtParseTranslationTable("<Configure>: resize()"));
  XtFree((char *)ptr);

  if (invisibleResize)
  {
    if (x > -1)
      XtVaSetValues(dialogShell, XmNx, x,
                    NULL);
    if (y > -1)
      XtVaSetValues(dialogShell, XmNy, y,
                    NULL);

    if (width > -1)
      XtVaSetValues(panelWidget, XmNwidth, width, NULL);
    if (height > -1)
      XtVaSetValues(panelWidget, XmNheight, height, NULL);
  }

  // This patch come from Torsten Liermann lier@lier1.muc.de
  if (XmIsMotifWMRunning(wxTheApp->topLevel))
  {
    int decor = 0 ;
    if (style & wxRESIZE_BORDER)
      decor |= MWM_DECOR_RESIZEH ;
    if (style & wxSYSTEM_MENU)
      decor |= MWM_DECOR_MENU;
    if ((style & wxCAPTION) ||
        (style && wxTINY_CAPTION_HORIZ) ||
        (style && wxTINY_CAPTION_VERT))
      decor |= MWM_DECOR_TITLE;
    if (style & wxTHICK_FRAME)
      decor |= MWM_DECOR_BORDER;
    if (style & wxMINIMIZE_BOX)
      decor |= MWM_DECOR_MINIMIZE;
    if (style & wxMAXIMIZE_BOX)
      decor |= MWM_DECOR_MAXIMIZE;

    XtVaSetValues(shell,XmNmwmDecorations,decor,NULL) ;
  }

  XtRealizeWidget(dialogShell);

  XtAddCallback(dialogShell,XmNunmapCallback,
               (XtCallbackProc)UnmapBulletinBoard,this) ;

  // Positioning of the dialog doesn't work properly unless the dialog
  // is managed, so we manage without mapping to the screen.
  // To show, we map the shell (actually it's parent).

  if (!invisibleResize)
    XtVaSetValues(XtParent(dialogShell), XmNmappedWhenManaged, FALSE, NULL);

  handle = (char *)panelWidget;
  if (!invisibleResize)
  {
    XtManageChild(dialogShell);
    SetSize(x, y, width, height);
  }
  XtAddEventHandler(panelWidget,ExposureMask,FALSE,
                          wxDialogBoxRepaintProc, (XtPointer) this);

  XtAddEventHandler(panelWidget,ExposureMask,FALSE,
                          wxDialogBoxRepaintProc, (XtPointer) this);

  wxInstallOnActivate(dialogShell, dialogShell);

  XtAddEventHandler(panelWidget,
     ButtonPressMask | ButtonReleaseMask | PointerMotionMask | KeyPressMask,
     FALSE,
     wxDialogBoxEventHandler,
     (XtPointer)this);

  wx_dc = new wxPanelDC(this);

  // Construct a new brush that takes on the
  // real background colour of this panel.
  panelBackgroundBrush = new wxBrush;
  Pixel thePix;
  XtVaGetValues(panelWidget, XmNbackground, &thePix, NULL);
  panelBackgroundBrush->colour.pixel = thePix;
  GetPanelDC()->SetBackground(panelBackgroundBrush);
#endif	/* Motif */

#ifdef wx_xview
  Frame parent;
  parent = ((Parent==NULL || !wxSubType(Parent->__type, wxTYPE_FRAME)) ?
            ROOT_FRAME : (Frame)(Parent->GetHandle()));


  // The kind of frame created is given by style, so you can
  // create frame with pushpin -- but note that on other platforms,
  // you must probably emulate pushpin with a checkbox.
  // Currently, wxEnhDialogBox provides this.
  // default style is to create FRAME, except for wxEnhDialogBox.
  //
  Xv_pkg *Frame_style;

  Bool scrollable = (style & wxVSCROLL) != 0;

  // System Menu or Resize Corners
  if (style & wxSYSTEM_MENU || style & wxRESIZE_BOX || scrollable) {
    Frame_style = FRAME;	// Normal frame
    xFrame = (Frame) xv_create (parent,
	Frame_style,
	FRAME_SHOW_RESIZE_CORNER, (style & wxRESIZE_BOX) ? TRUE : FALSE,
	FRAME_DONE_PROC, Done_proc,
//	FRAME_DEFAULT_DONE_PROC, done_proc,
	FRAME_LABEL, Title,
	FRAME_SHOW_LABEL,
(style & (wxCAPTION|wxTINY_CAPTION_HORIZ|wxTINY_CAPTION_VERT)) ? TRUE : FALSE,
	XV_X, x, XV_Y, y,
	WIN_CLIENT_DATA, (Xv_opaque) this,
	NULL);
  } else {
    Frame_style = FRAME_CMD;	// Push-Pin frame
    xFrame = (Frame) xv_create (parent,
	Frame_style,
	FRAME_DONE_PROC, PopUp_done_proc,	/* @@@@ 941102 */
	FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
//	FRAME_DEFAULT_DONE_PROC, done_proc,
	FRAME_LABEL, Title,
	FRAME_SHOW_LABEL,
(style & (wxCAPTION|wxTINY_CAPTION_HORIZ|wxTINY_CAPTION_VERT)) ? TRUE : FALSE,
	XV_X, x, XV_Y, y,
	WIN_CLIENT_DATA, (Xv_opaque) this,
	NULL);
   }

  Panel x_panel;
  if (Frame_style == FRAME_CMD)
    {
      x_panel = (Panel) xv_get (xFrame, FRAME_CMD_PANEL);
    }
  else
    {
      x_panel = (Panel) xv_create (xFrame, scrollable ? SCROLLABLE_PANEL : PANEL, NULL);
#ifdef USE_DEFAULT_DIALOG_ICON
      Server_image closed_image = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
        XV_WIDTH,               64,
        XV_HEIGHT,              64,
        SERVER_IMAGE_BITS,      closed_wxdialog_bits,
        NULL);

      Icon icon = (Icon)xv_create(xFrame, ICON,
        ICON_IMAGE,             closed_image,
	ICON_LABEL,		Title,
        XV_X,                   64,
        XV_Y,                   64,
        NULL);
      xv_set(xFrame, FRAME_ICON, icon, NULL);
#endif
    }

  xv_set (x_panel,
	PANEL_BACKGROUND_PROC, wxPanelEventProc,
	PANEL_ACCEPT_KEYSTROKE, TRUE,
	WIN_CLIENT_DATA, (Xv_opaque) this,
	NULL);

  notify_interpose_event_func(x_panel, (Notify_func)wxPanelInterposeProc, NOTIFY_SAFE);

  Scrollbar sb; 
  if (scrollable)
    sb = xv_create(x_panel, SCROLLBAR, NULL);


  if (width > -1) {
    xv_set(xFrame, XV_WIDTH, width, NULL);
    xv_set(x_panel, XV_WIDTH, width, NULL);
  }

  if (height > -1) {
    xv_set(xFrame, XV_HEIGHT, height, NULL);
    xv_set(x_panel, XV_HEIGHT, height, NULL);
  }

//  xv_set(xFrame, XV_SHOW, TRUE, NULL); // EXPERIMENT


  handle = (char *)x_panel;
  // Have to do this interposition to receive frame resize events
  (void)notify_interpose_event_func(xFrame, (Notify_func)wxDialogInterposer, NOTIFY_SAFE);
  (void)notify_interpose_destroy_func(xFrame, (Notify_func)wxDialogCloseInterposer);

  wx_dc = new wxPanelDC(this);
#endif
  modal = Modal;
  return TRUE;
}

wxDialogBox::~wxDialogBox()
{
// cerr << "Deleting dialog box\n";
  if (dialogTitle)
    delete[] dialogTitle;
#ifdef wx_motif
  modal_showing = FALSE;
  if (!invisibleResize && dialogShell)
    XtUnmapWidget(dialogShell);
//  wxWidgetHashTable->Delete((long)dialogShell);
#endif
#ifdef wx_xview
  modal_showing = FALSE;
  if (xFrame) {
    (void)notify_interpose_event_func(xFrame, (Notify_func)wxDummyDialogInterposer, NOTIFY_SAFE);
    // Zap the data attached to the frame
    xv_set(xFrame, WIN_CLIENT_DATA, NULL, NULL);
    xv_destroy_safe(xFrame);
  }
#endif
}

void wxDialogBox::OnSize(int w, int h)
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

#ifdef wx_xview
Notify_value wxDummyDialogInterposer(Frame x_frame, Event *x_event, Notify_arg arg,
                                     Notify_event_type type)
{
  return NOTIFY_DONE;
}

Notify_value wxDialogInterposer(Frame x_frame, Event *x_event, Notify_arg arg,
                               Notify_event_type type)
{
  wxDialogBox *dialog = (wxDialogBox *)xv_get(x_frame, WIN_CLIENT_DATA);

  if (dialog && event_action(x_event) == WIN_RESIZE) {
       int width, height;

       dialog->GetSize(&width, &height);
       dialog->GetEventHandler()->OnSize(width, height);

  }
  return notify_next_event_func(x_frame, (Notify_event)x_event, arg, type);
}

Notify_value wxDialogCloseInterposer(Notify_client client, Destroy_status status)
{
  wxDialogBox *dialog = (wxDialogBox *)xv_get(client, WIN_CLIENT_DATA);

  if (status == DESTROY_CHECKING) {
    if (dialog && ( dialog->GetEventHandler()->OnClose() == FALSE || dialog->IsModal()))
        notify_veto_destroy(client);
  } else if (status == DESTROY_CLEANUP) {
    // Try to delete the wxDialogBox without allowing the Frame
    // to be deleted, since this will be done by XView
    if (dialog) {
      dialog->Show(FALSE);
      (void)notify_interpose_event_func(dialog->GetXFrame(), (Notify_func)wxDummyDialogInterposer, NOTIFY_SAFE);
      xv_set(dialog->GetXFrame(), WIN_CLIENT_DATA, NULL, NULL);
      dialog->SetXFrame(XV_NULL);

      dialog->DestroyChildren();
      delete dialog;
    }
    return notify_next_destroy_func(client, status);

  } else if (status == DESTROY_SAVE_YOURSELF) {
    ; // Do nothing - this is an Open Look specific feature

  } else  if (status == DESTROY_PROCESS_DEATH) { 
    ; // Nothing here
  };

  return NOTIFY_DONE;
}

#endif

#ifdef wx_motif
void wxDialogBox::PostDestroyChildren(void)
{
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

    // Now process all events, because otherwise
    // this might remain on the screen
    XSync(XtDisplay(wxTheApp->topLevel), FALSE);
    XEvent event;
    while (
#if 0
	   XtAppPending(wxTheApp->appContext)
#else
	   wxTheApp->Pending()
#endif
	   ) {
      XFlush(XtDisplay(wxTheApp->topLevel));
#if 0
      XtAppNextEvent(wxTheApp->appContext, &event);
      XtDispatchEvent(&event);
#else
      wxTheApp->Dispatch();
#endif
    }
  }
}

void wxCloseDialogCallback(Widget widget, XtPointer client_data, XmAnyCallbackStruct *cbs)
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

void wxDialogBoxRepaintProc(Widget w, XtPointer c_data, XEvent *event, char *)
   {
     wxPanel *panel;
     // Window window;
     static XRectangle *xrect;
     Display *display;
     GC gc;
     int llp = 0;
     static int last_count = 0;
     static int draw_count = 0;

//     panel = (wxPanel *) c_data;
    panel = (wxPanel *)wxWidgetHashTable->Get((long)w);

    if (panel)
     switch(event -> type)
        {
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

void wxDialogBoxEventHandler (Widget    wid,
                              XtPointer client_data,
                              XEvent*   event,
                              Boolean *continueToDispatch)
{
//  wxDialogBox *dialog = (wxDialogBox *)client_data;
  wxDialogBox *dialog = (wxDialogBox *)wxWidgetHashTable->Get((long)wid);
  if (dialog)
  {
    wxMouseEvent *wxevent = new wxMouseEvent(0);
    if (wxTranslateMouseEvent(*wxevent, dialog, event))
      dialog->GetEventHandler()->OnEvent(*wxevent);
  }
  *continueToDispatch = True;
}
#endif

void wxDialogBox::Fit(void)
{
#ifdef wx_motif
  wxPanel::Fit(); // Doesn't work properly for dialog boxes, Motif 1.2
#endif
#ifdef wx_xview
  Panel panel = (Panel)handle;
  window_fit(panel);

  // Make sure that panel isn't too big too fit on the screen.
  // Assume that we'll be using a scrolling panel if it's really
  // big.
  int y = (int)xv_get(panel, XV_HEIGHT);
  int dx, dy;
  wxDisplaySize(&dx, &dy);
  if ((y + 40) > dy) {
    xv_set(panel, XV_HEIGHT, dy-40, NULL);
  }

  window_fit(xFrame);
#endif
}

void wxDialogBox::Iconize(Bool iconize)
{
#ifdef wx_motif
  // Can't iconize a dialog in Motif
//  XtVaSetValues(dialogShell, XmNiconic, iconize, NULL);
#endif
#ifdef wx_xview
  xv_set(xFrame, FRAME_CLOSED, iconize, NULL);
#endif
}

Bool wxDialogBox::Iconized(void)
{
#ifdef wx_motif
/*
  Bool iconic;
  XtVaGetValues(dialogShell, XmNiconic, &iconic, NULL);

  return iconic;
*/
  return FALSE;
#endif
#ifdef wx_xview
  return xv_get(xFrame, FRAME_CLOSED);
#endif
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int sizeFlags)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  if (x == -1 || y == -1)
  {
    int xx,yy ;
    GetPosition(&xx, &yy);
    if (x == -1) x = xx ;
    if (y == -1) y = yy ;
  }
  (void)xv_set(xFrame, XV_X, x, XV_Y, y, NULL);

  if (width == -1 || height == -1)
  {
    int ww,hh ;
    GetSize(&ww, &hh);
    if (width == -1) width = ww ;
    if (height == -1) height = hh ;
  }
  (void)xv_set(xFrame, XV_WIDTH, width, XV_HEIGHT, height, NULL);

  GetEventHandler()->OnSize(width, height);
#endif
}

void wxDialogBox::GetSize(int *w, int *h)
{
#ifdef wx_motif
//  Widget shell = XtParent((Widget)handle) ;
  Widget shell = (Widget)handle ;
  Dimension ww, hh;
  XtVaGetValues(shell, XmNwidth, &ww, XmNheight, &hh, NULL);
  *w = ww;
  *h = hh;
#endif
#ifdef wx_xview
  Xv_opaque object = (Xv_opaque) handle;
  *w = (int)xv_get(object, XV_WIDTH, NULL);
  *h = (int)xv_get(object, XV_HEIGHT, NULL);
#endif
}

void wxDialogBox::SetClientSize(int width, int height)
{
#ifdef wx_motif
//  Widget shell = XtParent((Widget)handle) ;
  Widget shell = (Widget)handle ;
  XtVaSetValues(shell, XmNresizePolicy, XmRESIZE_ANY, NULL);
  if (!invisibleResize)
  {
    int xx, yy;
    GetPosition(&xx, &yy);
    SetSize(xx, yy, width, height);
  }
  else
  {
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
#endif
#ifdef wx_xview
  if (width > -1 && height > -1)
    (void)xv_set(xFrame, XV_WIDTH, width, XV_HEIGHT, height, NULL);
#endif
}

void wxDialogBox::GetPosition(int *x, int *y)
{
#ifdef wx_motif
  Dimension xx, yy;
  XtVaGetValues(dialogShell, XmNx, &xx, XmNy, &yy, NULL);
  *x = xx; *y = yy;
#endif
#ifdef wx_xview
  *x = (int)xv_get(xFrame, XV_X);
  *y = (int)xv_get(xFrame, XV_Y);
#endif
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

Bool wxDialogBox::Show(Bool show)
{
  if (show == IsShown())
    return TRUE;

  SetShown(show);

#ifdef WXGARBAGE_COLLECTION_ON
  if (!window_parent) {
# if 0    
    if (show) {
      if (!wxTopLevelWindows(this)->Member(this))
	wxTopLevelWindows(this)->Append(this);
    } else
      wxTopLevelWindows(this)->DeleteObject(this);
# else
    wxTopLevelWindows(this)->Show(this, show);
# endif
  } else
    window_parent->GetChildren()->Show(this, show);
#endif
#ifdef wx_motif
  if (show)
  {
    if (!invisibleResize)
      XtMapWidget(XtParent(dialogShell));
    else
      XtManageChild(dialogShell) ; 

    XRaiseWindow(XtDisplay(dialogShell), XtWindow(dialogShell));
/*    XtManageChild(dialogShell) ; */ // Don't manage, map!
    if (modal)
    {
      if (modal_showing)
        return TRUE;

      modal_showing = TRUE;      

#if 1
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
#else
      wxModalShowingStack.Insert((wxObject *)TRUE);
        
      XtAddGrab(dialogShell, TRUE, FALSE);
      XEvent event;

//      printf("+++ Entering modal dialog loop\n");
      while ((wxModalShowingStack.Number() > 0) && (Bool)wxModalShowingStack.First()->Data())
      {
	/* MATTHEW: [6] See comment in wxApp::Dispatch */
	XtAppProcessEvent(wxTheApp->appContext, XtIMAll);
        // XtAppNextEvent(wxTheApp->appContext, &event);
        // XtDispatchEvent(&event);
//        printf("*");
//        fflush(stdout);
      }
      // Remove modal dialog flag from stack
      wxNode *node = wxModalShowingStack.First();
      if (node)
        delete node;

//      printf("\n--- Exiting modal dialog loop\n");

      // Question: do we have to avoid destroying the widget
      // in the destructor, or not?
/*
      // Destroy widget
      if (wxWidgetHashTable->Get((long)theWidget))
        XtDestroyWidget(theWidget);
*/
//      printf("+++ Processing remaining events\n");

#endif

      // Now process all events, to ensure widget destruction
      XSync(XtDisplay(wxTheApp->topLevel), FALSE);
      while (
#if 0
	     XtAppPending(wxTheApp->appContext)
#else
	     wxTheApp->Pending()
#endif
	     )
      {
        XFlush(XtDisplay(wxTheApp->topLevel));
#if 0
        /* MATTHEW: [6] See comment in wxApp::Dispatch */
	XtAppProcessEvent(wxTheApp->appContext, XtIMAll);
        // XtAppNextEvent(wxTheApp->appContext, &event);
        // XtDispatchEvent(&event);
#else
	wxTheApp->Dispatch();
#endif
      }
//      printf("--- Processed remaining events\n");
    }
    else
      modal_showing = FALSE;
  }
  else
  {
#if 0
    if (modal_showing)
      XtRemoveGrab(dialogShell);
#endif

    if (!invisibleResize)
      XtUnmapWidget(XtParent(dialogShell));
    else
      XtUnmanageChild(dialogShell) ;

//    XmUpdateDisplay(wxTheApp->topLevel); // Experimental: may be responsible for crashes

    modal_showing = FALSE;

#if 0
    wxNode *node = wxModalShowingStack.First();
    if (node)
      node->SetData((wxObject *)FALSE);
#endif

//    printf("About to flush and sync\n");
    XFlush(XtDisplay(wxTheApp->topLevel));

//    printf("Number of events in queue is %d\n", (int)XEventsQueued(XtDisplay(wxTheApp->topLevel), QueuedAfterFlush));
    XSync(XtDisplay(wxTheApp->topLevel), FALSE);
//    printf("After flush and sync\n");
  }
#endif
#ifdef wx_xview
  // Note this bit not code is not working
  // recursively!! XView limitation? Yes!
  // Requires patched version of xv_win_lp.c
  if (show)
    {
      if (modal)
	{
	  if (modal_showing)
	    return TRUE;

	  modal_showing = TRUE;

	  xv_set (xFrame, WIN_GRAB_ALL_INPUT, FALSE, NULL);
	  if (XV_OK != xv_window_loop (xFrame))
	    printf("Internal Error: %s(%d)\n", __FILE__, __LINE__);

	  modal_showing = FALSE;
	}
      else
	xv_set(xFrame, XV_SHOW, TRUE, NULL);
    }
  else // Hide the Frame
    {
      xv_set (xFrame, WIN_GRAB_ALL_INPUT, FALSE, NULL);
      if (modal)
	xv_window_return (XV_OK);
      else
	xv_set(xFrame, XV_SHOW, FALSE, NULL);
    }
#endif
  return TRUE;
}


void wxDialogBox::SetTitle(char *title)
{
  if (dialogTitle)
    delete[] dialogTitle;

  dialogTitle = copystring(title);

#ifdef wx_motif
  if (title)
    XtVaSetValues(dialogShell, 
                  XmNtitle, title,
                  XmNiconName, title,
                  NULL);
#endif
#ifdef wx_xview
  xv_set(xFrame, FRAME_LABEL, title, NULL);
/*
  if (icon)
  {
    xv_set(icon->x_icon, XV_LABEL, title, NULL);
    xv_set(xFrame, FRAME_ICON, icon->x_icon, NULL);
  }
*/
#endif
}

char *wxDialogBox::GetTitle(void)
{
  return dialogTitle;
}

