/*
 * File:	wb_win.cc
 * Purpose:	wxWindow class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_win.cxx,v 1.2 1998/04/11 13:58:19 mflatt Exp $
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
void wxbWindow::OnCommand(wxWindow& win, wxCommandEvent& event)
{
  if (window_parent)
  {
    window_parent->GetEventHandler()->OnCommand(win, event);
  }
}

void wxbWindow::OnSize(int WXUNUSED(width), int WXUNUSED(height))
{
#if USE_CONSTRAINTS
  if (GetAutoLayout())
    Layout();
#endif
}


#if USE_CONSTRAINTS
void wxbWindow::SetConstraints(wxLayoutConstraints *c)
{
  if (constraints)
  {
    UnsetConstraints(constraints);
    delete constraints;
  }
  constraints = c;
  if (constraints)
  {
    // Make sure other windows know they're part of a 'meaningful relationship'
    if (constraints->left.GetOtherWindow() && (constraints->left.GetOtherWindow() != this))
      constraints->left.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->top.GetOtherWindow() && (constraints->top.GetOtherWindow() != this))
      constraints->top.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->right.GetOtherWindow() && (constraints->right.GetOtherWindow() != this))
      constraints->right.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->bottom.GetOtherWindow() && (constraints->bottom.GetOtherWindow() != this))
      constraints->bottom.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->width.GetOtherWindow() && (constraints->width.GetOtherWindow() != this))
      constraints->width.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->height.GetOtherWindow() && (constraints->height.GetOtherWindow() != this))
      constraints->height.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->centreX.GetOtherWindow() && (constraints->centreX.GetOtherWindow() != this))
      constraints->centreX.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
    if (constraints->centreY.GetOtherWindow() && (constraints->centreY.GetOtherWindow() != this))
      constraints->centreY.GetOtherWindow()->AddConstraintReference((wxWindow *)this);
  }
}

// This removes any dangling pointers to this window
// in other windows' constraintsInvolvedIn lists.
void wxbWindow::UnsetConstraints(wxLayoutConstraints *c)
{
  if (c)
  {
    if (c->left.GetOtherWindow() && (c->top.GetOtherWindow() != this))
      c->left.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->top.GetOtherWindow() && (c->top.GetOtherWindow() != this))
      c->top.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->right.GetOtherWindow() && (c->right.GetOtherWindow() != this))
      c->right.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->bottom.GetOtherWindow() && (c->bottom.GetOtherWindow() != this))
      c->bottom.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->width.GetOtherWindow() && (c->width.GetOtherWindow() != this))
      c->width.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->height.GetOtherWindow() && (c->height.GetOtherWindow() != this))
      c->height.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->centreX.GetOtherWindow() && (c->centreX.GetOtherWindow() != this))
      c->centreX.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
    if (c->centreY.GetOtherWindow() && (c->centreY.GetOtherWindow() != this))
      c->centreY.GetOtherWindow()->RemoveConstraintReference((wxWindow *)this);
  }
}

// Back-pointer to other windows we're involved with, so if we delete
// this window, we must delete any constraints we're involved with.
void wxbWindow::AddConstraintReference(wxWindow *otherWin)
{
  if (!constraintsInvolvedIn)
    constraintsInvolvedIn = new wxList;
  if (!constraintsInvolvedIn->Member(otherWin))
    constraintsInvolvedIn->Append(otherWin);
}

// REMOVE back-pointer to other windows we're involved with.
void wxbWindow::RemoveConstraintReference(wxWindow *otherWin)
{
  if (constraintsInvolvedIn)
    constraintsInvolvedIn->DeleteObject(otherWin);
}

// Reset any constraints that mention this window
void wxbWindow::DeleteRelatedConstraints(void)
{
  if (constraintsInvolvedIn)
  {
    wxNode *node = constraintsInvolvedIn->First();
    while (node)
    {
      wxWindow *win = (wxWindow *)node->Data();
      wxNode *next = node->Next();
      wxLayoutConstraints *constr = win->GetConstraints();

      // Reset any constraints involving this window
      if (constr)
      {
        constr->left.ResetIfWin((wxWindow *)this);
        constr->top.ResetIfWin((wxWindow *)this);
        constr->right.ResetIfWin((wxWindow *)this);
        constr->bottom.ResetIfWin((wxWindow *)this);
        constr->width.ResetIfWin((wxWindow *)this);
        constr->height.ResetIfWin((wxWindow *)this);
        constr->centreX.ResetIfWin((wxWindow *)this);
        constr->centreY.ResetIfWin((wxWindow *)this);
      }
      delete node;
      node = next;
    }
    delete constraintsInvolvedIn;
    constraintsInvolvedIn = NULL;
  }
}

void wxbWindow::SetSizer(wxSizer *sizer)
{
  windowSizer = sizer;
  if (sizer)
    sizer->SetSizerParent((wxWindow *)this);
}

wxSizer *wxbWindow::GetSizer(void)
{
  return windowSizer;
}

wxWindow *wxbWindow::GetSizerParent(void)
{
  return sizerParent;
}

/*
Bool wxbWindow::Layout(void)
{
  if (windowSizer)
  {
    windowSizer->Layout();
    return TRUE;
  }
  else
    return wxDoLayout((wxWindow *)this);
}
*/


/*
 * New version
 */

Bool wxbWindow::Layout(void)
{
  if (GetConstraints())
  {
    int w, h;
    GetClientSize(&w, &h);
    GetConstraints()->width.SetValue(w);
    GetConstraints()->height.SetValue(h);
  }
  
  // If top level (one sizer), evaluate the sizer's constraints.
  if (GetSizer())
  {
    int noChanges;
    GetSizer()->ResetConstraints();   // Mark all constraints as unevaluated
    GetSizer()->LayoutPhase1(&noChanges);
    GetSizer()->LayoutPhase2(&noChanges);
    GetSizer()->SetConstraintSizes(); // Recursively set the real window sizes
    return TRUE;
  }
  else
  {
    // Otherwise, evaluate child constraints
    ResetConstraints();   // Mark all constraints as unevaluated
    DoPhase(1);           // Just one phase need if no sizers involved
    DoPhase(2);
    SetConstraintSizes(); // Recursively set the real window sizes
  }
  return TRUE;
}


// Do a phase of evaluating constraints:
// the default behaviour. wxSizers may do a similar
// thing, but also impose their own 'constraints'
// and order the evaluation differently.
Bool wxbWindow::LayoutPhase1(int *noChanges)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    return constr->SatisfyConstraints((wxWindow *)this, noChanges);
  }
  else
    return TRUE;
}

Bool wxbWindow::LayoutPhase2(int *noChanges)
{
  *noChanges = 0;
  
  // Layout children
  DoPhase(1);
  DoPhase(2);
  return TRUE;
}

// Do a phase of evaluating child constraints
Bool wxbWindow::DoPhase(int phase)
{
  int noIterations = 0;
  int maxIterations = 500;
  int noChanges = 1;
  int noFailures = 0;
  wxList succeeded;
  while ((noChanges > 0) && (noIterations < maxIterations))
  {
    noChanges = 0;
    noFailures = 0;
    wxChildNode *node = GetChildren()->First();
    while (node)
    {
      wxWindow *child = (wxWindow *)node->Data();
      if (!wxSubType(child->__type, wxTYPE_FRAME) && !wxSubType(child->__type, wxTYPE_DIALOG_BOX))
      {
        wxLayoutConstraints *constr = child->GetConstraints();
        if (constr)
        {
          if (succeeded.Member(child))
          {
          }
          else
          {
            int tempNoChanges = 0;
            Bool success = ( (phase == 1) ? child->LayoutPhase1(&tempNoChanges) : child->LayoutPhase2(&tempNoChanges) ) ;
            noChanges += tempNoChanges;
            if (success)
            {
              succeeded.Append(child);
            }
          }
        }
      }
      node = node->Next();
    }
    noIterations ++;
  }
  return TRUE;
}

void wxbWindow::ResetConstraints(void)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    constr->left.SetDone(FALSE);
    constr->top.SetDone(FALSE);
    constr->right.SetDone(FALSE);
    constr->bottom.SetDone(FALSE);
    constr->width.SetDone(FALSE);
    constr->height.SetDone(FALSE);
    constr->centreX.SetDone(FALSE);
    constr->centreY.SetDone(FALSE);
  }
  wxChildNode *node = GetChildren()->First();
  while (node)
  {
    wxWindow *win = (wxWindow *)node->Data();
    if (!wxSubType(win->__type, wxTYPE_FRAME) && !wxSubType(win->__type, wxTYPE_DIALOG_BOX))
      win->ResetConstraints();
    node = node->Next();
  }
}

// Need to distinguish between setting the 'fake' size for
// windows and sizers, and setting the real values.
void wxbWindow::SetConstraintSizes(Bool recurse)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr && constr->left.GetDone() && constr->right.GetDone() &&
                constr->width.GetDone() && constr->height.GetDone())
  {
    int x = constr->left.GetValue();
    int y = constr->top.GetValue();
    int w = constr->width.GetValue();
    int h = constr->height.GetValue();

    // If we don't want to resize this window, just move it...
    if ((constr->width.GetRelationship() != wxAsIs) ||
        (constr->height.GetRelationship() != wxAsIs))
    {
      // Calls Layout() recursively. AAAGH. How can we stop that.
      // Simply take Layout() out of non-top level OnSizes.
      SizerSetSize(x, y, w, h);
    }
    else
    {
      SizerMove(x, y);
    }
  }
  else if (constr)
  {
    /* MATTHEW: use GetClassInfo() only when USE_DYNAMIC_CLASSES */
#if USE_DYNAMIC_CLASSES
    char *windowClass = this->GetClassInfo()->GetClassName();
#else
    char *windowClass = "unknown";
#endif
    char *winName = (GetName() ? GetName() : "unnamed");
    wxDebugMsg("Constraint(s) not satisfied for window of type %s, name %s:\n", windowClass, winName);
    if (!constr->left.GetDone())
      wxDebugMsg("  unsatisfied 'left' constraint.\n");
    if (!constr->right.GetDone())
      wxDebugMsg("  unsatisfied 'right' constraint.\n");
    if (!constr->width.GetDone())
      wxDebugMsg("  unsatisfied 'width' constraint.\n");
    if (!constr->height.GetDone())
      wxDebugMsg("  unsatisfied 'height' constraint.\n");
    wxDebugMsg("Please check constraints: try adding AsIs() constraints.\n");
  }

  if (recurse)
  {
    wxChildNode *node = GetChildren()->First();
    while (node)
    {
      wxWindow *win = (wxWindow *)node->Data();
      if (!wxSubType(win->__type, wxTYPE_FRAME) && !wxSubType(win->__type, wxTYPE_DIALOG_BOX))
        win->SetConstraintSizes();
      node = node->Next();
    }
  }
}

// This assumes that all sizers are 'on' the same
// window, i.e. the parent of this window.
void wxbWindow::TransformSizerToActual(int *x, int *y)
{
  if (!sizerParent)
    return;
    
  int xp, yp;
  sizerParent->GetPosition(&xp, &yp);
  sizerParent->TransformSizerToActual(&xp, &yp);
  *x += xp;
  *y += yp;
}

void wxbWindow::SizerSetSize(int x, int y, int w, int h)
{
  TransformSizerToActual(&x, &y);
  SetSize(x, y, w, h);
}

void wxbWindow::SizerMove(int x, int y)
{
  TransformSizerToActual(&x, &y);
  Move(x, y);
}

// Only set the size/position of the constraint (if any)
void wxbWindow::SetSizeConstraint(int x, int y, int w, int h)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    if (x != -1)
    {
      constr->left.SetValue(x);
      constr->left.SetDone(TRUE);
    }
    if (y != -1)
    {
      constr->top.SetValue(y);
      constr->top.SetDone(TRUE);
    }
    if (w != -1)
    {
      constr->width.SetValue(w);
      constr->width.SetDone(TRUE);
    }
    if (h != -1)
    {
      constr->height.SetValue(h);
      constr->height.SetDone(TRUE);
    }
  }
}

void wxbWindow::MoveConstraint(int x, int y)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    if (x != -1)
    {
      constr->left.SetValue(x);
      constr->left.SetDone(TRUE);
    }
    if (y != -1)
    {
      constr->top.SetValue(y);
      constr->top.SetDone(TRUE);
    }
  }
}

void wxbWindow::GetSizeConstraint(int *w, int *h)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    *w = constr->width.GetValue();
    *h = constr->height.GetValue();
  }
  else
    GetSize(w, h);
}

void wxbWindow::GetClientSizeConstraint(int *w, int *h)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    *w = constr->width.GetValue();
    *h = constr->height.GetValue();
  }
  else
    GetClientSize(w, h);
}

void wxbWindow::GetPositionConstraint(int *x, int *y)
{
  wxLayoutConstraints *constr = GetConstraints();
  if (constr)
  {
    *x = constr->left.GetValue();
    *y = constr->top.GetValue();
  }
  else
    GetPosition(x, y);
}
#endif

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
