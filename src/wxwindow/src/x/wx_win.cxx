/*
 * File:	wx_win.cc
 * Purpose:	wxWindow class implementation (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_win.cxx,v 1.1.1.1 1997/12/22 16:12:05 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_win.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

#include <X11/keysym.h>
// #include <iostream.h>

#include "common.h"
#include "wx_win.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_gdi.h"
#include "wx_canvs.h"
#include "wx_menu.h"
#include "wx_panel.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"

#include "wx_dialg.h"
#include <Xm/RowColumn.h>

// Constructor
IMPLEMENT_DYNAMIC_CLASS(wxWindow, wxEvtHandler)

wxWindow::wxWindow(void)
{
  currentWindowCursor = 0;
  wxType = 0;
  sr_width = sr_height = -1;
}

// Destructor
wxWindow::~wxWindow(void)
{
  if (window_parent)
  {
    window_parent->RemoveChild(this);

    if (wxSubType(window_parent->__type, wxTYPE_PANEL))
    {
      wxPanel *panel = (wxPanel *) GetParent ();
      // Must reset the panel since we may have dangling pointers, etc.

      panel->new_line = FALSE;
      panel->label_position = wxHORIZONTAL;

      panel->hSpacing = PANEL_HSPACING;
      panel->vSpacing = PANEL_VSPACING;
      panel->initial_hspacing = panel->hSpacing;
      panel->initial_vspacing = panel->vSpacing;
      panel->current_hspacing = panel->hSpacing;
      panel->current_vspacing = panel->vSpacing;

      panel->cursor_x = PANEL_LEFT_MARGIN;
      panel->cursor_y = PANEL_TOP_MARGIN;
      panel->max_height = 0;
      panel->max_line_height = 0;
      panel->max_width = 0;
      panel->firstRowWidget = 0;
      panel->currentRow = 0;
      panel->currentCol = 0;
      panel->last_created = 0;
    }
  }
  if (handle)
  {
    DestroyChildren();

    wxWidgetHashTable->Delete((long)handle);
    Widget w = (Widget)handle;
    if (w)
      XtDestroyWidget(w);

    PostDestroyChildren();

  }
  delete children;
  children = NULL;
}

void wxWindow::SetFocus(void)
{
  XmProcessTraversal((Widget)handle, XmTRAVERSE_CURRENT);
  XmProcessTraversal((Widget)handle, XmTRAVERSE_CURRENT);
  
  wxWindow *w = GetParent();
  while (w 
	 && !wxSubType(w->__type, wxTYPE_FRAME) 
	 && !wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    w = w->GetParent();
}

void wxWindow::CaptureMouse(void)
{
  if (winCaptured)
    return;
    
  if (handle)
    XtAddGrab((Widget)handle, TRUE, FALSE);

  winCaptured = TRUE;
}

void wxWindow::ReleaseMouse(void)
{
  if (!winCaptured)
    return;
    
  if (handle)
    XtRemoveGrab((Widget)handle);

  winCaptured = FALSE;
}

#define USER_DISABLED 0x08000000

extern void wxSetSensitive(Widget, Bool);

void wxWindow::InternalEnable(Bool enable)
{
  int do_something;

  if (enable) {
    --internal_disabled;
    do_something = !internal_disabled;
  } else {
    do_something = !internal_disabled;
    internal_disabled++;
  }

  if (do_something && !(windowStyle & USER_DISABLED))
    wxSetSensitive((Widget)handle, enable);
}

void wxWindow::Enable(Bool enable)
{
  if (handle && !internal_disabled)
  {
    XtSetSensitive((Widget)handle, enable);
    XmUpdateDisplay((Widget)handle);
  }
  if (!enable)
    windowStyle |= USER_DISABLED;
  else if (windowStyle & USER_DISABLED)
    windowStyle -= USER_DISABLED;
}

Window wxWindow::GetXWindow(void)
{
  return XtWindow((Widget)handle);
}

Display *wxWindow::GetXDisplay(void)
{
  return XtDisplay((Widget)handle);
}

void wxWindow::Refresh(void)
{
  Display *display = XtDisplay((Widget)handle);
  Window thisWindow = XtWindow((Widget)handle);

  XExposeEvent dummyEvent;
  int width, height;
  GetSize(&width, &height);

  dummyEvent.type = Expose;
  dummyEvent.display = display;
  dummyEvent.send_event = True;
  dummyEvent.window = thisWindow;
  dummyEvent.x = 0;
  dummyEvent.y = 0;
  dummyEvent.width = width;
  dummyEvent.height = height;
  dummyEvent.count = 0;

  XSendEvent(display, thisWindow, False, ExposureMask, (XEvent *)&dummyEvent);
}

void wxWindow::DragAcceptFiles(Bool WXUNUSED(accept))
{
}

// Get total size
void wxWindow::GetSize(int *x, int *y)
{
  Widget widget = (Widget)handle;
  Dimension xx, yy;
  XtVaGetValues(widget, XmNwidth, &xx, XmNheight, &yy, NULL);
  *x = xx; *y = yy;
}

void wxWindow::GetPosition(int *x, int *y)
{
  Widget widget = (Widget)handle;
  Dimension xx, yy;
  XtVaGetValues(widget, XmNx, &xx, XmNy, &yy, NULL);
  *x = xx; *y = yy;
}

void wxWindow::ClientToScreen(int *x, int *y)
{
  Display *display = XtDisplay((Widget)handle);
  Window rootWindow = RootWindowOfScreen(XtScreen((Widget)handle));
  Window thisWindow;
  if (wxSubType(__type, wxTYPE_FRAME))
  {
    wxFrame *fr = (wxFrame *)this;
    thisWindow = XtWindow(fr->clientArea);
  }
  else
    thisWindow = XtWindow((Widget)handle);

  Window childWindow;
  int xx = *x;
  int yy = *y;
  XTranslateCoordinates(display, thisWindow, rootWindow, xx, yy, x, y, &childWindow);
}

void wxWindow::ScreenToClient(int *x, int *y)
{
  Display *display = XtDisplay((Widget)handle);
  Window rootWindow = RootWindowOfScreen(XtScreen((Widget)handle));
  Window thisWindow;
  if (wxSubType(__type, wxTYPE_FRAME))
  {
    wxFrame *fr = (wxFrame *)this;
    thisWindow = XtWindow(fr->clientArea);
  }
  else
    thisWindow = XtWindow((Widget)handle);

  Window childWindow;
  int xx = *x;
  int yy = *y;
  XTranslateCoordinates(display, rootWindow, thisWindow, xx, yy, x, y, &childWindow);
}

wxCursor *wxWindow::SetCursor(wxCursor *cursor)
{
  wxCursor *old_cursor = wx_cursor;

  /* MATTHEW: [4] Get cursor for this display */
  if (cursor && !currentWindowCursor)
  {
    Display *dpy = GetXDisplay();
    Cursor x_cursor = cursor->GetXCursor(dpy);

    Widget w = (Widget)handle;
    Window win = XtWindow(w);
    XDefineCursor(dpy, win, x_cursor);
  }

  wx_cursor = cursor;
 
  return old_cursor;
}

void wxWindow::SetColourMap(wxColourMap *cmap)
{
  Display *display = GetXDisplay();
  Window win = GetXWindow();
  /* MATTHEW: [4] Use display-specific colormap */
  XSetWindowColormap(display, win, cmap->GetXColormap(display));
}

// Get size *available for subwindows* i.e. excluding menu bar etc.
// For XView, this is the same as GetSize
void wxWindow::GetClientSize(int *x, int *y)
{
  Widget widget = (Widget)handle;
  Dimension xx, yy;
  XtVaGetValues(widget, XmNwidth, &xx, XmNheight, &yy, NULL);
  *x = xx; *y = yy;
}

void wxWindow::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  Widget widget = (Widget)handle;

  if (x > -1)
    XtVaSetValues(widget, XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues(widget, XmNy, y, NULL);
  if (width > -1)
    XtVaSetValues(widget, XmNwidth, width, NULL);
  if (height > -1)
    XtVaSetValues(widget, XmNheight, height, NULL);
  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize(width, height);
}

void wxWindow::SetClientSize(int width, int height)
{
  Widget widget = (Widget)handle;

  if (width > -1)
    XtVaSetValues(widget, XmNwidth, width, NULL);
  if (height > -1)
    XtVaSetValues(widget, XmNheight, height, NULL);
  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize(width, height);
}

Bool wxWindow::Show(Bool show)
{
  if (show == IsShown())
    return TRUE;
  SetShown(show);

  if (window_parent)
    window_parent->GetChildren()->Show(this, show);

  if (wxSubType(__type, wxTYPE_CANVAS)) {
    wxCanvas *c = (wxCanvas *)this;
    Widget w = c->borderWidget ? c->borderWidget : c->scrolledWindow;

    if (show)
      XtManageChild(w);
    else
      XtUnmanageChild (w);

    /* printf("%smanage: %lx\n", show ? "" : "un", w); */
  } else {
    Window xwin=GetXWindow();        //*** all this part
    Display *xdisp=GetXDisplay();    //*** added by
    if (show)                         //*** chubraev
      XMapWindow(xdisp,xwin);  //***
    else                             //***
      XUnmapWindow(xdisp,xwin);//***
  }

  return TRUE;
}

float wxWindow::GetCharHeight(void)
{
  return 0.0;
}

float wxWindow::GetCharWidth(void)
{
  return 0.0;
}

/* MATTHEW: [2] Helper function for 16-bit fonts */
static int str16len(const char *s)
{
  int count = 0;

  while (s[0] && s[1]) {
    count++;
    s += 2;
  }

  return count;
}

void wxWindow::GetTextExtent(const char *string, float *x, float *y,
			     float *descent, float *externalLeading, wxFont *theFont, Bool use16)
{
  wxFont *fontToUse = theFont;
  XFontStruct *fontStruct;

  if (!fontToUse) {
    if (font)
      (void)font->GetInternalFont(GetXDisplay(), &fontStruct);
    else {
      XmFontList list;
      XmFontContext context;
      XmStringCharSet ignored;

      XtVaGetValues((Widget)handle, XmNfontList, &list, NULL);
      if (!XmFontListInitFontContext(&context, list))
	return;
      if (!XmFontListGetNextFont(context, &ignored, &fontStruct))
	fontStruct = 0;
      XmFontListFreeFontContext(context);
    }
  } else
    (void)fontToUse->GetInternalFont(GetXDisplay(), &fontStruct);

  /* MATTHEW: [4] Use GetInternalFont */
  if (!fontStruct) {
    cerr << "wxWindows warning - set a font before calling GetTextExtent!\n";
    *x = -1;
    *y = -1;
    return;
  }


  int direction, ascent, descent2;
  XCharStruct overall;

  /* MATTHEW: [2] Helper 16-bit fonts */
  if (use16)
    XTextExtents16(fontStruct, (XChar2b *)string, str16len(string), 
		   &direction, &ascent, &descent2, &overall);
  else
    XTextExtents(fontStruct, string, strlen(string), &direction, &ascent,
		 &descent2, &overall);
  *x = overall.width;
  *y = ascent + descent2;
  if (descent)
    *descent = (float)descent2;
  if (externalLeading)
    *externalLeading = 0.0;
}

Bool wxWindow:: PopupMenu (wxMenu * menu, float x, float y)
{
  /* MATTHEW: [11] safety (revised) */
  /* Always popup menus in the frame. Dunno why, but this seems
     to solve the problem described below for FakePopupMenu(). */
  /* HACK: The menuId field seems to be usused, so we'll use it to
     indicate whether a menu is popped up or not:
         0: Not currently created as a popup
       < 0: Created as a popup, but not active
       > 0: Active popup.
   */

  if (menu->window_parent && (menu->menuId > -1))
    return FALSE;

  if (menu->handle) {
    wxChildList *list = menu->window_parent->children;

    list->DeleteObject(menu);

    menu->DestroyMenu(TRUE);
  }

  wxWindow *parent = this;
  while (parent->window_parent)
    parent = parent->window_parent;

  menu->menuId = 1; /* Mark as popped-up */
  menu->CreateMenu(NULL, (Widget)parent->handle, menu);
  menu->SetParent(parent);
  parent->children->Append(menu);  // Store menu for later deletion

  Widget menuWidget = (Widget) menu->handle;

  int rootX = 0;
  int rootY = 0;

  int deviceX = (int) x;
  int deviceY = (int) y;

  if (wxSubType (__type, wxTYPE_CANVAS))
    {
      wxCanvas *canvas = (wxCanvas *) this;
/*
   int vs_x, vs_y;
   canvas->ViewStart(&vs_x, &vs_y);
   if (canvas->horiz_units > 0)
   scrollOffsetX = (int)(- vs_x*canvas->horiz_units);
   if (canvas->vert_units > 0)
   scrollOffsetY = (int)(- vs_y*canvas->vert_units);
 */
      deviceX = canvas->GetDC ()->LogicalToDeviceX (x);
      deviceY = canvas->GetDC ()->LogicalToDeviceY (y);
    }

  Display *display = XtDisplay ((Widget)handle);
  Window rootWindow = RootWindowOfScreen (XtScreen((Widget)handle));
  Window thisWindow = XtWindow ((Widget)handle);
  Window childWindow;
  XTranslateCoordinates (display, thisWindow, rootWindow, (int) deviceX, (int) deviceY,
			 &rootX, &rootY, &childWindow);

  unsigned int state;
  {
    Window rw, cw;
    int rx, ry, wx, wy;
    XQueryPointer(display, thisWindow, &rw, &cw, &rx, &ry, &wx, &wy, &state);
  }

  XButtonPressedEvent event;
  event.type = ButtonPress;

  event.display = display;
  event.window = thisWindow;
  event.root = rootWindow;

  event.x = (int) deviceX;
  event.y = (int) deviceY;

  event.x_root = rootX;
  event.y_root = rootY;
  
  if (state & Button3Mask)
    event.button = Button3;
  else if (state & Button2Mask)
    event.button = Button2;
  else
    event.button = Button1;

  event.state = state;

  XmMenuPosition (menuWidget, &event);
  XtManageChild (menuWidget);

  return TRUE;
}

// Sometimes in Motif there are problems popping up a menu
// (for unknown reasons); use this instead when this happens.
// Works for one-level popups only (ignores submenus).
Bool wxWindow::FakePopupMenu(wxMenu *menu, float x, float y)
{
  // Find true dialog box parent
  wxWindow *trueParent = NULL;
  if (wxSubType(__type, wxTYPE_CANVAS) ||
      wxSubType(__type, wxTYPE_PANEL) ||
      wxSubType(__type, wxTYPE_TEXT_WINDOW))
    trueParent = GetParent();
  else
    trueParent = this;

  // Find true screen x and y position
  int trueX = (int)x;
  int trueY = (int)y;

  ClientToScreen(&trueX, &trueY);

  // Create array of strings to pass to choose dialog
  int noStrings = menu->menuItems.Number();
  char **theStrings = new char *[noStrings];
  // Have an array of menu ids so can map position in
  // chooser list to menu id
  int *menuIds = new int[noStrings];

  int i = 0;
  
  for (wxNode * node = menu->menuItems.First (); node; node = node->Next ())
    {
      wxMenuItem *item = (wxMenuItem *) node->Data ();
      if ((item->itemId != -2) && (item->itemName && !item->subMenu) &&
          (item->isEnabled))
      {
        theStrings[i] = item->itemName;
        menuIds[i] = item->itemId;
        i ++;
      }
    }
  int stringIndex = wxGetSingleChoiceIndex("Please choose an option",
   "Menu", i, theStrings, trueParent, trueX, trueY);

  int menuId = 0;
  if (stringIndex > -1)
    menuId = menuIds[stringIndex];

  delete[] theStrings;
  delete[] menuIds;
  if (stringIndex > -1)
  {
    wxCommandEvent *event  = new wxCommandEvent(wxEVENT_TYPE_MENU_COMMAND);
    event->eventObject = menu;
    event->commandInt = menuId;
    menu->ProcessCommand (*event);
  }
  return TRUE;
}

int CharCodeXToWX(KeySym keySym)
{
  int id;
  switch (keySym) {
    case XK_Shift_L:
    case XK_Shift_R:
      id = WXK_SHIFT; break;
    case XK_Control_L:
    case XK_Control_R:
      id = WXK_CONTROL; break;
    case XK_BackSpace:
      id = WXK_BACK; break;
    case XK_Delete:
      id = WXK_DELETE; break;
    case XK_Clear:
      id = WXK_CLEAR; break;
    case XK_Tab:
      id = WXK_TAB; break;
    case XK_numbersign:
      id = '#'; break;
    case XK_Return:
      id = WXK_RETURN; break;
    case XK_Escape:
      id = WXK_ESCAPE; break;
    case XK_Pause:
    case XK_Break:
      id = WXK_PAUSE; break;
    case XK_Num_Lock:
      id = WXK_NUMLOCK; break;
    case XK_Scroll_Lock:
      id = WXK_SCROLL; break;

    case XK_Home:
      id = WXK_HOME; break;
    case XK_End:
      id = WXK_END; break;
    case XK_Left:
      id = WXK_LEFT; break;
    case XK_Right:
      id = WXK_RIGHT; break;
    case XK_Up:
      id = WXK_UP; break;
    case XK_Down:
      id = WXK_DOWN; break;
    case XK_Next:
      id = WXK_NEXT; break;
    case XK_Prior:
      id = WXK_PRIOR; break;
    case XK_Menu:
      id = WXK_MENU; break;
    case XK_Select:
      id = WXK_SELECT; break;
    case XK_Cancel:
      id = WXK_CANCEL; break;
    case XK_Print:
      id = WXK_PRINT; break;
    case XK_Execute:
      id = WXK_EXECUTE; break;
    case XK_Insert:
      id = WXK_INSERT; break;
    case XK_Help:
      id = WXK_HELP; break;

    case XK_KP_Multiply:
      id = WXK_MULTIPLY; break;
    case XK_KP_Add:
      id = WXK_ADD; break;
    case XK_KP_Subtract:
      id = WXK_SUBTRACT; break;
    case XK_KP_Divide:
      id = WXK_DIVIDE; break;
    case XK_KP_Decimal:
      id = WXK_DECIMAL; break;
    case XK_KP_Equal:
      id = '='; break;
    case XK_KP_Space:
      id = ' '; break;
    case XK_KP_Tab:
      id = WXK_TAB; break;
    case XK_KP_Enter:
      id = WXK_RETURN; break;
    case XK_KP_0:
      id = WXK_NUMPAD0; break;
    case XK_KP_1:
      id = WXK_NUMPAD1; break;
    case XK_KP_2:
      id = WXK_NUMPAD2; break;
    case XK_KP_3:
      id = WXK_NUMPAD3; break;
    case XK_KP_4:
      id = WXK_NUMPAD4; break;
    case XK_KP_5:
      id = WXK_NUMPAD5; break;
    case XK_KP_6:
      id = WXK_NUMPAD6; break;
    case XK_KP_7:
      id = WXK_NUMPAD7; break;
    case XK_KP_8:
      id = WXK_NUMPAD8; break;
    case XK_KP_9:
      id = WXK_NUMPAD9; break;
    case XK_F1:
      id = WXK_F1; break;
    case XK_F2:
      id = WXK_F2; break;
    case XK_F3:
      id = WXK_F3; break;
    case XK_F4:
      id = WXK_F4; break;
    case XK_F5:
      id = WXK_F5; break;
    case XK_F6:
      id = WXK_F6; break;
    case XK_F7:
      id = WXK_F7; break;
    case XK_F8:
      id = WXK_F8; break;
    case XK_F9:
      id = WXK_F9; break;
    case XK_F10:
      id = WXK_F10; break;
    case XK_F11:
      id = WXK_F11; break;
    case XK_F12:
      id = WXK_F12; break;
    case XK_F13:
      id = WXK_F13; break;
    case XK_F14:
      id = WXK_F14; break;
    case XK_F15:
      id = WXK_F15; break;
    case XK_F16:
      id = WXK_F16; break;
    case XK_F17:
      id = WXK_F17; break;
    case XK_F18:
      id = WXK_F18; break;
    case XK_F19:
      id = WXK_F19; break;
    case XK_F20:
      id = WXK_F20; break;
    case XK_F21:
      id = WXK_F21; break;
    case XK_F22:
      id = WXK_F22; break;
    case XK_F23:
      id = WXK_F23; break;
    case XK_F24:
      id = WXK_F24; break;
    default:
      id = (keySym <= 255) ? (int)keySym : -1;
  } // switch
  return id;
}

KeySym CharCodeWXToX(int id)
{
  KeySym keySym;

  switch (id) {
    case WXK_CANCEL:            keySym = XK_Cancel; break;
    case WXK_BACK:              keySym = XK_BackSpace; break;
    case WXK_TAB:	        keySym = XK_Tab; break;
    case WXK_CLEAR:		keySym = XK_Clear; break;
    case WXK_RETURN:		keySym = XK_Return; break;
    case WXK_SHIFT:		keySym = XK_Shift_L; break;
    case WXK_CONTROL:		keySym = XK_Control_L; break;
    case WXK_MENU :		keySym = XK_Menu; break;
    case WXK_PAUSE:		keySym = XK_Pause; break;
    case WXK_ESCAPE:		keySym = XK_Escape; break;
    case WXK_SPACE:		keySym = ' '; break;
    case WXK_PRIOR:		keySym = XK_Prior; break;
    case WXK_NEXT :		keySym = XK_Next; break;
    case WXK_END:		keySym = XK_End; break;
    case WXK_HOME :		keySym = XK_Home; break;
    case WXK_LEFT :		keySym = XK_Left; break;
    case WXK_UP:		keySym = XK_Up; break;
    case WXK_RIGHT:		keySym = XK_Right; break;
    case WXK_DOWN :		keySym = XK_Down; break;
    case WXK_SELECT:		keySym = XK_Select; break;
    case WXK_PRINT:		keySym = XK_Print; break;
    case WXK_EXECUTE:		keySym = XK_Execute; break;
    case WXK_INSERT:		keySym = XK_Insert; break;
    case WXK_DELETE:		keySym = XK_Delete; break;
    case WXK_HELP :		keySym = XK_Help; break;
    case WXK_NUMPAD0:		keySym = XK_KP_0; break;
    case WXK_NUMPAD1:		keySym = XK_KP_1; break;
    case WXK_NUMPAD2:		keySym = XK_KP_2; break;
    case WXK_NUMPAD3:		keySym = XK_KP_3; break;
    case WXK_NUMPAD4:		keySym = XK_KP_4; break;
    case WXK_NUMPAD5:		keySym = XK_KP_5; break;
    case WXK_NUMPAD6:		keySym = XK_KP_6; break;
    case WXK_NUMPAD7:		keySym = XK_KP_7; break;
    case WXK_NUMPAD8:		keySym = XK_KP_8; break;
    case WXK_NUMPAD9:		keySym = XK_KP_9; break;
    case WXK_MULTIPLY:		keySym = XK_KP_Multiply; break;
    case WXK_ADD:		keySym = XK_KP_Add; break;
    case WXK_SUBTRACT:		keySym = XK_KP_Subtract; break;
    case WXK_DECIMAL:		keySym = XK_KP_Decimal; break;
    case WXK_DIVIDE:		keySym = XK_KP_Divide; break;
    case WXK_F1:		keySym = XK_F1; break;
    case WXK_F2:		keySym = XK_F2; break;
    case WXK_F3:		keySym = XK_F3; break;
    case WXK_F4:		keySym = XK_F4; break;
    case WXK_F5:		keySym = XK_F5; break;
    case WXK_F6:		keySym = XK_F6; break;
    case WXK_F7:		keySym = XK_F7; break;
    case WXK_F8:		keySym = XK_F8; break;
    case WXK_F9:		keySym = XK_F9; break;
    case WXK_F10:		keySym = XK_F10; break;
    case WXK_F11:		keySym = XK_F11; break;
    case WXK_F12:		keySym = XK_F12; break;
    case WXK_F13:		keySym = XK_F13; break;
    case WXK_F14:		keySym = XK_F14; break;
    case WXK_F15:		keySym = XK_F15; break;
    case WXK_F16:		keySym = XK_F16; break;
    case WXK_F17:		keySym = XK_F17; break;
    case WXK_F18:		keySym = XK_F18; break;
    case WXK_F19:		keySym = XK_F19; break;
    case WXK_F20:		keySym = XK_F20; break;
    case WXK_F21:		keySym = XK_F21; break;
    case WXK_F22:		keySym = XK_F22; break;
    case WXK_F23:		keySym = XK_F23; break;
    case WXK_F24:		keySym = XK_F24; break;
    case WXK_NUMLOCK:		keySym = XK_Num_Lock; break;
    case WXK_SCROLL:		keySym = XK_Scroll_Lock; break;
    default:                    keySym = id <= 255 ? (KeySym)id : 0;
  } // switch
  return keySym;
}

Bool wxWindow::PreResize(void)
{
  return TRUE;
}

// All widgets should have this as their resize proc.
// OnSize sent to wxWindow via client data.
void wxWidgetResizeProc(Widget w, XConfigureEvent *WXUNUSED(event), 
			String WXUNUSED(args)[], int *WXUNUSED(num_args))
{
  wxWindow *win = (wxWindow *)wxWidgetHashTable->Get((long)w);
  if (!win)
    return;
  if (win->PreResize()) {
    int width, height;
    win->GetSize(&width, &height);
    if (win->sr_width != width || win->sr_height != height) {
      win->sr_width = width;
      win->sr_height = height;
      win->GetEventHandler()->OnSize(width, height);
    }
  }
}

void wxWindow::SetFont(wxFont *f)
{
  if (f) 
    XtVaSetValues ((Widget)handle,
		   XmNfontList, 
		   /* MATTHEW: [4] Provide display */
		   f->GetInternalFont(XtDisplay((Widget)handle)),
		   NULL);
}

Bool wxTranslateMouseEvent(wxMouseEvent& wxevent, wxWindow *win, XEvent *xevent)
{
  switch (xevent->xany.type)
  {
    case ButtonPress:
    case ButtonRelease:
    case MotionNotify:
      {
	WXTYPE eventType = 0;

        if (xevent->xany.type == LeaveNotify)
	{
          return FALSE;
	}
	else if (xevent->xany.type == MotionNotify)
	  {
	    eventType = wxEVENT_TYPE_MOTION;
	  }
	else if (xevent->xany.type == ButtonPress)
	  {
	    if (xevent->xbutton.button == Button1)
	      {
		eventType = wxEVENT_TYPE_LEFT_DOWN;
	      }
	    else if (xevent->xbutton.button == Button2)
	      {
		eventType = wxEVENT_TYPE_MIDDLE_DOWN;
	      }
	    else if (xevent->xbutton.button == Button3)
	      {
		eventType = wxEVENT_TYPE_RIGHT_DOWN;
	      }
	  }
	else if (xevent->xany.type == ButtonRelease)
	  {
	    if (xevent->xbutton.button == Button1)
	      {
		eventType = wxEVENT_TYPE_LEFT_UP;
	      }
	    else if (xevent->xbutton.button == Button2)
	      {
		eventType = wxEVENT_TYPE_MIDDLE_UP;
	      }
	    else if (xevent->xbutton.button == Button3)
	      {
		eventType = wxEVENT_TYPE_RIGHT_UP;
	      }
            else return FALSE;
	  }
          else return FALSE;

	wxevent.eventHandle = (char *)xevent;
        wxevent.eventType = eventType;

        wxevent.x = xevent->xbutton.x;
	wxevent.y = xevent->xbutton.y;

#define event_left_is_down(x) (x->xbutton.state & Button1Mask)
#define event_middle_is_down(x) (x->xbutton.state & Button2Mask)
#define event_right_is_down(x) (x->xbutton.state & Button3Mask)

	wxevent.leftDown = ((eventType == wxEVENT_TYPE_LEFT_DOWN)
			    || (event_left_is_down (xevent) 
				&& (eventType != wxEVENT_TYPE_LEFT_UP)));
	wxevent.middleDown = ((eventType == wxEVENT_TYPE_MIDDLE_DOWN)
			      || (event_middle_is_down (xevent) 
				  && (eventType != wxEVENT_TYPE_MIDDLE_UP)));
	wxevent.rightDown = ((eventType == wxEVENT_TYPE_RIGHT_DOWN)
			     || (event_right_is_down (xevent) 
				 && (eventType != wxEVENT_TYPE_RIGHT_UP)));

	wxevent.shiftDown = xevent->xbutton.state & ShiftMask;
	wxevent.controlDown = xevent->xbutton.state & ControlMask;

	wxevent.eventObject = win;

        return TRUE;
    }
  }
  return FALSE;
}

void wxWindow::AddPreHandlers(Widget w, Widget hash_w)
{
  if (!hash_w)
    hash_w = w;

  XtInsertEventHandler(w, (KeyPressMask
			   | ButtonPressMask
			   | ButtonReleaseMask
			   | ButtonMotionMask
			   | PointerMotionMask
			   | PointerMotionHintMask),
		       FALSE,
		       (XtEventHandler)wxWindow::WindowEventHandler,
		       (XtPointer)hash_w,
		       XtListHead);
}

void wxWindow::WindowEventHandler(Widget WXUNUSED(w),
				  Widget hash_w,
				  XEvent *xev,
				  Boolean *continue_to_dispatch_return)
{
  wxWindow *win = (wxWindow *)wxWidgetHashTable->Get((long)hash_w);

  if (!win)
    return;

  switch (xev->xany.type) {
  case ButtonPress:
  case ButtonRelease:
  case MotionNotify:
    {
      wxMouseEvent *e = new wxMouseEvent(0);
      wxTranslateMouseEvent(*e, win, xev);
      if (win->CallPreOnEvent(win, e)) {
	*continue_to_dispatch_return = 0;
	return;
      }
    }
  break;
  case KeyPress:
    {
      KeySym keySym;
      XComposeStatus compose;
      XLookupString((XKeyEvent *)xev, wxBuffer, 20, &keySym, &compose);
      int id = CharCodeXToWX (keySym);
      
      wxKeyEvent *e = new wxKeyEvent(wxEVENT_TYPE_CHAR);
      
      if (xev->xkey.state & ShiftMask)
	e->shiftDown = TRUE;
      if (xev->xkey.state & ControlMask)
	e->controlDown = TRUE;
      if (xev->xkey.state & Mod1Mask)
	e->metaDown = TRUE;
      e->eventObject = win;
      e->keyCode = id;
      e->SetTimestamp(xev->xkey.time);

      if (id > -1) {
	if (win->CallPreOnChar(win, e)) {
	  *continue_to_dispatch_return = 0;
	  return;
	}
      }
    }
  break;
  }  
}

Bool wxWindow::PreOnChar(wxWindow *, wxKeyEvent *)
{
  return FALSE;
}

Bool wxWindow::CallPreOnChar(wxWindow *win, wxKeyEvent *event)
{
  wxWindow *p = win->GetParent();

  return ((p && CallPreOnChar(p, event)) || win->PreOnChar(this, event));
}

Bool wxWindow::PreOnEvent(wxWindow *, wxMouseEvent *)
{
  return FALSE;
}

Bool wxWindow::CallPreOnEvent(wxWindow *win, wxMouseEvent *event)
{
  wxWindow *p = win->GetParent();

  return ((p && CallPreOnEvent(p, event)) || win->PreOnEvent(this, event));
}

