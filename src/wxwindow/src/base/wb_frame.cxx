/*
 * File:	wb_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_frame.cxx,v 1.6 1999/02/23 18:27:45 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wb_frame.h"
#include "wx_stdev.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_stdev.h"

#endif

class wxFrame;

wxbFrame::wxbFrame(void)
{
  __type = wxTYPE_FRAME;
  nb_status = 0;
  frameToolBar = NULL ;
  SetShown(FALSE);
}

wxbFrame::wxbFrame(wxFrame *WXUNUSED(Parent), char *WXUNUSED(title), int WXUNUSED(x), int WXUNUSED(y),
                 int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_FRAME;
  windowStyle = style;
  frameToolBar = NULL ;
  SetShown(FALSE);
}

Bool wxbFrame::Create(wxFrame *Parent, char *WXUNUSED(title), int WXUNUSED(x), int WXUNUSED(y),
                 int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  windowStyle = style;

  context = (void *)wxGetContextForFrame();
  /* WXGC_IGNORE(context); - NO context itself is not finalized */

  wxTopLevelWindows(this)->Append(this);
  wxTopLevelWindows(this)->Show(this, FALSE);

  SetShown(FALSE);

  return TRUE;
}

wxbFrame::~wxbFrame(void)
{
  wxTopLevelWindows(this)->DeleteObject(this);
}

// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxbFrame::OnSize(int WXUNUSED(x), int WXUNUSED(y))
{
#if DEBUG > 1
  wxDebugMsg("wxbFrame::OnSize\n");
#endif
  if (frame_type == wxMDI_PARENT)
    return;

  // Search for a child which is a subwindow, not another frame.
  wxWindow *child = NULL;
  // Count the number of _subwindow_ children
  int noChildren = 0;
  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    wxWindow *win = (wxWindow *)node->Data();
    WXTYPE winType = win->__type;

    if (wxSubType(winType, wxTYPE_PANEL) ||
        wxSubType(winType, wxTYPE_TEXT_WINDOW) ||
        wxSubType(winType, wxTYPE_CANVAS))
    {
      child = win;
      noChildren ++;
    }
  }

  // If not one child, call the Layout function if compiled in
  if (!child || (noChildren > 1))
  {
#if USE_CONSTRAINTS
    if (GetAutoLayout())
      Layout();
#endif
    return;
  }

  int client_x, client_y;

#if DEBUG > 1
  wxDebugMsg("wxbFrame::OnSize: about to set the child's size.\n");
#endif

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y);
}

// Default activation behaviour - set the focus for the first child
// subwindow found.
void wxbFrame::OnActivate(Bool WXUNUSED(flag))
{
#if 0
#ifndef wx_msw
  if (!flag)
    return;

  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    // Find a child that's a subwindow, but not a dialog box.
    wxWindow *child = (wxWindow *)node->Data();
    if ((wxSubType(child->__type, wxTYPE_PANEL) &&
         !wxSubType(child->__type, wxTYPE_DIALOG_BOX)) ||
        wxSubType(child->__type, wxTYPE_TEXT_WINDOW) ||
        wxSubType(child->__type, wxTYPE_CANVAS))
    {
#if DEBUG > 1
      wxDebugMsg("wxbFrame::OnActivate: about to set the child's focus.\n");
#endif
      child->SetFocus();
      return;
    }
  }
#endif
#endif
}

// Default menu selection behaviour - display a help string
void wxbFrame::OnMenuSelect(long id)
{
  if (StatusLineExists()) {
    wxMenuBar *menuBar = GetMenuBar();
    if (menuBar) {
      char *helpString = GetMenuBar()->GetHelpString(id);
      if (helpString) {
	SetStatusText(helpString);
	return;
      }
    }

    SetStatusText("");
  }
}

void wxbFrame::OnMenuClick(void)
{
}

wxMenuBar *wxbFrame::GetMenuBar(void)
{
  return wx_menu_bar;
}

Bool wxbFrame::StatusLineExists(void)
{
  return status_line_exists;
}

void wxbFrame::Centre(int direction)
{
  int display_width, display_height, width, height, x, y;
  wxDisplaySize(&display_width, &display_height);

  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  SetSize(x, y, width, height);
}

// Call this to simulate a menu command
void wxbFrame::Command(long id)
{
  ProcessCommand(id);
}

void wxbFrame::ProcessCommand(long id)
{
  OnMenuCommand(id);
}
