/*								-*- C++ -*-
 * $Id: Frame.cc,v 1.1.1.1 1997/12/22 17:28:58 mflatt Exp $
 *
 * Purpose: base class for all frames
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
#pragma implementation "Frame.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxApp
#define  Uses_wxFrame
#define  Uses_wxGDI
#define  Uses_wxLayout
#define  Uses_wxList
#define  Uses_wxMenuBar
#define  Uses_wxMessage
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_ShellWidget
#define  Uses_BoardWidget
#include "widgets.h"

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

//-----------------------------------------------------------------------------
// create and destroy frame
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxFrame, wxPanel)

wxFrame::wxFrame(void) : wxPanel()
{
    __type = wxTYPE_FRAME;

    menubar         = NULL;
    status          = NULL;
    num_status      = 0;
    being_destroyed = FALSE;

    SetShown(FALSE);
}

wxFrame::wxFrame(wxFrame *parent, char *title,
		 int x, int y, int width, int height, int style, char *name)
    : wxPanel()
{
    __type = wxTYPE_FRAME;

    menubar         = NULL;
    status          = NULL;
    num_status      = 0;
    being_destroyed = FALSE;

    Create(parent, title, x, y, width, height, style, name);

    SetShown(FALSE);
}

wxFrame::~wxFrame(void)
{
    being_destroyed = TRUE;
    // hide frame
    Show(FALSE);
    // destroy children first to popdown child frames
    DestroyChildren();
    // adjust list of top level frames
    wxTopLevelFrames(this)->DeleteObject(this);
#if !WXGARBAGE_COLLECTION_ON
    if (wxTopLevelFrames.Number() == 0) {
	wxTheApp->ExitMainLoop();
    }
#endif
}

/* MATTHEW: [3] Used to insure that hide-&-show within an event cycle works */
static void wxFrameMapProc(Widget w, XtPointer clientData, 
			   XCrossingEvent * event)
{
  wxFrame *frame = *(wxFrame **)clientData;

  if (frame) {
    XEvent *e = (XEvent *)event;

    if (e->xany.type == MapNotify && e->xmap.window == XtWindow(w)) {
      if (!frame->IsShown()) {
	/* We really wanted this to be hidden! */
	XtUnmapWidget(w);
      }
    }
  }
}

Bool wxFrame::Create(wxFrame *frame_parent, char *title,
		     int x, int y, int width, int height,
		     int _style, char *name)
{
  context = wxGetContextForFrame();
  WXGC_IGNORE(context);

    Widget parent_widget;

    // chain child <-> parent
    if ((parent = frame_parent)) {
 	parent_widget = frame_parent->GetHandle()->frame;
	parent->AddChild(this);
    } else {
	parent_widget = wxAPP_TOPLEVEL;
	wxTopLevelFrames(this)->Append(this);
	wxTopLevelFrames(this)->Show(this, FALSE);
    }
    // create top level or transient shell
    if ( (style = _style) & wxTRANSIENT ) {
	// create transient shell with WM_TRANSIENT_FOR property
	wxWindow *p=parent;
	for (/*wxWindow *p = parent*/; p; p = p->GetParent())
	    if (wxSubType(p->__type, wxTYPE_FRAME)
	    && !(p->GetWindowStyleFlag() & wxTRANSIENT)) // frame must not be transient
		break;
        /* MATTHEW: Use name */
	X->frame = XtVaCreatePopupShell
	    (name ? name : "shell", transientShellWidgetClass, parent_widget,
	     XtNsaveUnder, FALSE,
	     XtNtransientFor, (p ? p->GetHandle()->frame : wxAPP_TOPLEVEL),
	     NULL);
    } else {
	// create top level shell
        /* MATTHEW: Use name */
	X->frame = XtVaCreatePopupShell
	  (name ? name : "shell", topLevelShellWidgetClass, parent_widget, NULL);
    }
    // set common data
    SetSize(x, y, width, height, wxSIZE_AUTO);
    SetTitle(title);
    // create board widget
    X->handle = XtVaCreateManagedWidget(
	name, xfwfBoardWidgetClass, X->frame,
	XtNhighlightThickness, 0,
	XtNbackground, bg->GetPixel(cmap),
	NULL);
    AddEventHandlers();

    /* MATTHEW */
    XtRealizeWidget(X->frame);
    // make a WM_PROTOCOLS atom if necessary
    XInternAtom(XtDisplay(X->frame), "WM_PROTOCOLS", False);
    // make a WM_DELETE_WINDOW atom
    Atom WM_DELETE_WINDOW = XInternAtom(XtDisplay(X->frame),
					"WM_DELETE_WINDOW",
					False);
    XSetWMProtocols(XtDisplay(X->frame),
		    XtWindow(X->frame),
		    &WM_DELETE_WINDOW,
		    1);

    /* MATTHEW: [3] part of show-&-hide fix */
    XtAddEventHandler(X->frame, StructureNotifyMask,
		      False, (XtEventHandler)wxFrameMapProc,
		      (XtPointer)saferef);

    return TRUE;
}

//-----------------------------------------------------------------------------
// leave place for menubar and statusline
//-----------------------------------------------------------------------------

void wxFrame::Fit(void)
{
    int hsize=0, vsize=0;

    if (children) {
	for (wxChildNode *node = children->First(); node; node = node->Next()) {
	    wxWindow *child = (wxWindow*)(node->Data());
	    if (child) {
		// skip menubar and status line for computation
		int i=0;
		for ( /* i=0 */; i<num_status; ++i)
		    if (child == status[i])
			break;
		if (child == menubar || i < num_status) {
		    continue;
		}
		// compute maximal size
		int x, y, w, h;
		child->GetPosition(&x, &y); child->GetSize(&w, &h);
		hsize = max(hsize, x + w);
		vsize = max(vsize, y + h);
	    }
	}
	hsize -= xoff; vsize -= yoff;
    } else {
	hsize = PANEL_HMARGIN;
	vsize = PANEL_VMARGIN;
    }
    hsize += /* PANEL_HMARGIN + */ (style & wxBORDER ? 4 : 0 );
    vsize += /* PANEL_VMARGIN + */ (style & wxBORDER ? 4 : 0 );
    SetClientSize(hsize, vsize);
}

// void wxFrame::Layout(void)
// --> wxLayout.cc

void wxFrame::GetClientSize(int *width, int *height)
{
    int dummy, h1=0, h2=0, i;

    wxWindow::GetClientSize(width, height);
    if (menubar)  menubar->GetSize(&dummy, &h1);   // get menubar's height
    for (i = 0; i < num_status; i++) {
      status[i]->GetSize(&dummy, &h2); // get status lines's height
      h1 += h2;
    }
    *height -= h1;                            // adjust height
}

void wxFrame::SetClientSize(int width, int height)
{
    int dummy, h1=0, h2=0;

    if (menubar)  menubar->GetSize(&dummy, &h1);   // get menubar's height
    if (status)   status[0]->GetSize(&dummy, &h2); // get status lines's height
    height += h1 + h2;		                   // adjust height
    wxWindow::SetClientSize(width, height);
}

//-----------------------------------------------------------------------------
// iconize, maximize
//-----------------------------------------------------------------------------

void wxFrame::Iconize(Bool iconize)
{
  if (!IsShown())
    return;
  
  if (iconize) {
    XIconifyWindow(XtDisplay(X->frame), 
		   XtWindow(X->frame), 
		   XScreenNumberOfScreen(XtScreen(X->frame)));
#if 0
    /* MATTHEW: [5] cast to Boolean */
    XtVaSetValues(X->frame, XtNiconic, (Boolean)iconize, NULL);
#endif
  } else {
    XtMapWidget(X->frame);
  }
}

Bool wxFrame::Iconized(void)
{
  if (!IsShown())
    return FALSE;

  XWindowAttributes wa;

  XGetWindowAttributes(XtDisplay(X->frame), XtWindow(X->frame), &wa);

  return (wa.map_state == IsUnmapped);
  
#if 0
  Boolean iconized; /* MATTHEW: [5] from Bool */
  XtVaGetValues(X->frame, XtNiconic, &iconized, NULL);
  return iconized;
#endif
}

void wxFrame::Maximize(Bool WXUNUSED(maximize))
{
}

//-----------------------------------------------------------------------------
// status line
//-----------------------------------------------------------------------------

void wxFrame::CreateStatusLine(int number, char *)
{
    if (StatusLineExists())
	return;

    status = new wxMessage* [num_status = min(number, wxMAX_STATUS)];
    for (int i = 0; i < num_status; ++i) {
	wxLayoutConstraints *constr;
	int ww, hh;

	status[i] = DEBUG_NEW wxMessage(this, "", 0, 0, wxBORDER, "status");
	status[i]->AllowResize(FALSE);
	status[i]->SetAlignment(wxALIGN_LEFT);
	status[i]->GetSize(&ww, &hh);
	constr = DEBUG_NEW wxLayoutConstraints;
	constr->left.PercentOf(this, wxWidth, i*(100/num_status));
	constr->top.Below(this, 0); // wxBottom of client area
	constr->height.Absolute(hh);
	if (i != num_status-1) {
	    constr->width.PercentOf(this, wxWidth, 100 / num_status);
	} else {
	    constr->right.SameAs(this, wxRight, 0);
	    constr->width.Unconstrained();
	}
	status[i]->SetConstraints(constr);
    }
}

void wxFrame::SetStatusText(char *text, int number)
{
    if (number < num_status)
	status[number]->SetLabel(text ? text : "");
}

Bool wxFrame::StatusLineExists(void)
{
    return (num_status != 0);
}

//-----------------------------------------------------------------------------
// associated GDI objects
//-----------------------------------------------------------------------------

wxMenuBar *wxFrame::GetMenuBar(void)
{
    return menubar;
}

void wxFrame::SetIcon(wxIcon *icon)
{
    if (icon->Ok())
	XtVaSetValues(X->frame, XtNiconPixmap, GETPIXMAP(icon), NULL);
}

void wxFrame::SetMenuBar(wxMenuBar *new_menubar)
{
  /* MATTHEW: Enforce safety */
  if (new_menubar->GetParent())
    return;

    if (menubar)
	menubar->Destroy();	// destroy X internal representation
    if ((menubar = new_menubar)) {
	int ww, hh;

	menubar->Create(this);
	menubar->GetSize(&ww, &hh);
	yoff = hh; // offset off client area inside frame
    }
}

//-----------------------------------------------------------------------------
// miscellaneous
//-----------------------------------------------------------------------------

void wxFrame::Command(int id)
{
    GetEventHandler()->OnMenuCommand(id);
}

static void ForceFocus(Widget frame)
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
  if (parent)
    parent->GetChildren()->Show(this, show);
  
  if (show == IsShown()) { // do nothing if state doesn't change
    if (show) {
      /* Make sure window isn't iconized: */
      Iconize(FALSE);
      XRaiseWindow(XtDisplay(X->frame), XtWindow(X->frame));
      ForceFocus(X->frame);      
    }
    return TRUE;
  }

  wxTopLevelFrames(this)->Show(this, show);

  SetShown(show);
  if (show) {
    XtMapWidget(X->frame);
    XRaiseWindow(XtDisplay(X->frame), XtWindow(X->frame));
    ForceFocus(X->frame);
  } else {
    /* XWithdrawWindow does the right thing for iconified windows */
    XWithdrawWindow(XtDisplay(X->frame), XtWindow(X->frame), 
		    XScreenNumberOfScreen(XtScreen(X->frame)));
    XtUnmapWidget(X->frame);
  }

#if 0
  // Now process all events, to ensure focus out before anything else
  if (!being_destroyed)
    wxFlushEvents();
#else
  XFlush(XtDisplay(wxAPP_TOPLEVEL));
  XSync(XtDisplay(wxAPP_TOPLEVEL), FALSE);
#endif

  return TRUE;
}

//-----------------------------------------------------------------------------
// virtual event functions
//-----------------------------------------------------------------------------

void wxFrame::OnMenuSelect(int id)
{
  SetStatusText(menubar->GetHelpString(id));
}



void wxFrame::SetToolBar(wxToolBar *){
}

wxToolBar *wxFrame::GetToolBar(void){
  return NULL;
}
