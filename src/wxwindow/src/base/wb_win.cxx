/*
 * File:	wb_win.cc
 * Purpose:	wxWindow class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_win.cxx,v 1.3 1998/10/16 18:19:39 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_win.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_win.h"
#include "wx_gdi.h"
#include "wx_utils.h"

#endif

#if USE_CONSTRAINTS
#include "wx_lay.h"
#endif

IMPLEMENT_DYNAMIC_CLASS(wxRectangle, wxObject)

// Constructor
wxbWindow::wxbWindow(void)
{
  __type = wxTYPE_WINDOW;
  windowStyle = 0;
  window_parent = NULL;
  windowEventHandler = this;
  font = NULL;
  handle = NULL;
  windowName = NULL;
  callback = 0;
  wx_cursor = NULL /* wxSTANDARD_CURSOR */;
  children = new wxChildList;
  paintingEnabled = TRUE;
  doubleClickAllowed = 0 ;
  winCaptured = FALSE;
  editUIMode = FALSE;
  internal_disabled = 0;
  is_shown = 1;
#if USE_CONSTRAINTS
  constraints = NULL;
  constraintsInvolvedIn = NULL;
  windowSizer = NULL;
  sizerParent = NULL;
  autoLayout = FALSE;
#endif
  WXGC_IGNORE(window_parent);
}

// Destructor
wxbWindow::~wxbWindow(void)
{
  if (windowName) delete[] windowName;
#if USE_CONSTRAINTS
  DeleteRelatedConstraints();
  if (constraints)
  {
    // This removes any dangling pointers to this window
    // in other windows' constraintsInvolvedIn lists.
    UnsetConstraints(constraints);
    delete constraints;
    constraints = NULL;
  }
  if (windowSizer)
  {
    delete windowSizer;
    windowSizer = NULL;
  }
#endif
}

char *wxbWindow::GetHandle(void)
{
  return handle;
}

void wxbWindow::SetEventHandler(wxEvtHandler *handler)
{
  windowEventHandler = handler;
}

wxEvtHandler *wxbWindow::GetEventHandler(void)
{
  return windowEventHandler;
}

// General callback setting
void wxbWindow::Callback(wxFunction Function)
{
  if (Function)
    callback = Function;
}

wxWindow *wxbWindow::GetParent(void)
{
  return window_parent;
}

wxWindow *wxbWindow::GetGrandParent(void)
{
  if (GetParent())
    return GetParent()->GetParent();
  else
    return NULL;
}

void wxbWindow::AddChild(wxObject *child)
{
  children->Append(child);
}

void wxbWindow::RemoveChild(wxObject *child)
{
  if (children)
    children->DeleteObject(child);
}

void wxbWindow::DestroyChildren(void)
{
  if (children) {
    wxChildNode *node;
    while ((node = children->First()) != NULL) {
      wxWindow *child;
      if ((child = (wxWindow *)node->Data()) != (wxWindow *)NULL) {
//      child->DestroyChildren();
        delete child;
      }
    } /* while */
  }
}

Bool wxbWindow::IsShown()
{
  return is_shown;
}

void wxbWindow::SetShown(Bool s)
{
  is_shown = s;
}

void wxbWindow::MakeModal(Bool modal)
{
  // Disable all other windows
  if (wxSubType(__type, wxTYPE_DIALOG_BOX) || wxSubType(__type, wxTYPE_FRAME))
  {
    wxChildNode *node = wxTopLevelWindows(this)->First();
    while (node)
    {
      wxWindow *win = (wxWindow *)node->Data();
      if (win != this)
        win->Enable(!modal);

      node = node->Next();
    }
  }
}

void wxbWindow::SetName(char *name)
{
  if (windowName)
    delete[] windowName;
  if (name)
    windowName = copystring(name);
  else
    windowName = NULL;
}

// If nothing defined for this, try the parent.
// E.g. we may be a button loaded from a resource, with no callback function
// defined.
void wxbWindow::OnCommand(wxWindow *win, wxCommandEvent *event)
{
  if (window_parent)
  {
    window_parent->GetEventHandler()->OnCommand(win, event);
  }
}

void wxbWindow::OnSize(int WXUNUSED(width), int WXUNUSED(height))
{
}


/*
 * Event handler
 */

IMPLEMENT_DYNAMIC_CLASS(wxEvtHandler, wxObject)

wxEvtHandler::wxEvtHandler(void)
{
  nextHandler = NULL;
  previousHandler = NULL;
}

wxEvtHandler::~wxEvtHandler(void)
{
  // Takes itself out of the list of handlers
  if (previousHandler)
    previousHandler->nextHandler = nextHandler;

  if (nextHandler)
    nextHandler->previousHandler = previousHandler;
}

wxEvtHandler *wxEvtHandler::GetNextHandler(void)
{
  return nextHandler;
}

wxEvtHandler *wxEvtHandler::GetPreviousHandler(void)
{
  return previousHandler;
}

void wxEvtHandler::SetNextHandler(wxEvtHandler *handler)
{
  nextHandler = handler;
}

void wxEvtHandler::SetPreviousHandler(wxEvtHandler *handler)
{
  previousHandler = handler;
}

void wxbWindow::ForEach(void (*foreach)(wxWindow *w, void *data), void *data)
{
  wxChildNode *node = GetChildren()->First();
  while (node) {
    wxChildNode *next = node->Next();
    wxWindow *win = (wxWindow *)node->Data();
    win->ForEach(foreach, data);
    node = next;
  }

  foreach((wxWindow *)this, data);
}

Bool wxbWindow::GetsFocus()
{
  return TRUE;
}
