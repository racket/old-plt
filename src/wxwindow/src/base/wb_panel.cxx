/*
 * File:	wb_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_panel.cxx,v 1.4 1999/03/09 19:53:23 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_panel.cc	1.2 5/9/94"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wb_panel.h"
#include "wx_buttn.h"
#include "wx_stdev.h"

#endif

#include <stdlib.h>
#include <math.h>

#if USE_EXTENDED_STATICS
#include "wx_stat.h"
#endif

class wxFrame;
class wxPanel;

// Constructors

wxbPanel::wxbPanel(void)
{
  __type = wxTYPE_PANEL;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;
  window_parent = NULL;
}

wxbPanel::wxbPanel(wxWindow *parent, int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), 
		   int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_PANEL;
  windowStyle = style;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;

  window_parent = parent;
}

wxbPanel::~wxbPanel(void)
{
#if USE_EXTENDED_STATICS
  wxStaticItem *s_item;
  wxNode *node = staticItems.First();
  while (node)
  {
    s_item = (wxStaticItem *) node -> Data();
    wxNode *next = node->Next();
    delete s_item;
    node = next ;
  }
#endif
}

void wxbPanel::OnChangeFocus(wxItem *, wxItem *)
{
}

Bool wxbPanel::OnFunctionKey(wxKeyEvent &)
{
	return FALSE;
}

wxObject* wxbPanel::GetChild(int number)
{
  // Return a pointer to the Nth object in the Panel
  if (!children)
    return(NULL) ;
  wxChildNode *node = GetChildren()->First();
  while (node && number--)
    node = node->Next() ;
  if (node)
  {
    wxObject *obj = (wxObject *)node->Data();
    return(obj) ;
  }
  else
    return NULL ;
}

void wxbPanel::SetLabelPosition(int pos)  // wxHORIZONTAL or wxVERTICAL
{
  label_position = pos;
}

int wxbPanel::GetLabelPosition(void)
{
  return label_position;
}

void wxbPanel::OnDefaultAction(wxItem *WXUNUSED(initiatingItem))
{
  wxButton *but = GetDefaultItem();
  if (but)
  {
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
    but->Command(event);
  }
}

void wxbPanel::SetLabelFont(wxFont *fnt)
{
  labelFont = fnt ;
  /* MATTHEW: [4] Label font resolution does not belong here. */
}

void wxbPanel::SetButtonFont(wxFont *theFont)
{
  buttonFont = theFont ;
}

void wxbPanel::SetBackgroundColour(wxColour *col)
{
  backColour = col ;
}

void wxbPanel::SetLabelColour(wxColour *col)
{
  labelColour = col ;
}

void wxbPanel::SetButtonColour(wxColour *col)
{
  buttonColour = col ;
}

/*
 * Called if in editing mode
 */

// An event outside any items: may be a drag event.
void wxbPanel::OnEvent(wxMouseEvent * /* event */)
{
}

void wxbPanel::OnItemEvent(wxItem *item, wxMouseEvent& event)
{
  // Not a selection handle event: just a normal item event.
  // Transform to panel coordinates.
  int x, y;
  item->GetPosition(&x, &y);

  event.x = (float)(event.x + x);
  event.y = (float)(event.y + y);
  ProcessItemEvent(item, event, 0);
}

void wxbPanel::ProcessItemEvent(wxItem * /* item */, wxMouseEvent& /* event */, int /* selectionHandle */)
{
}

// Calls DrawSelectionHandles for all items if
// edit mode is on.
void wxbPanel::PaintSelectionHandles(void)
{
}

#if USE_EXTENDED_STATICS
void wxbPanel::AddStaticItem(wxStaticItem *item)
{
   staticItems.Append((wxObject *) item);
}

void wxbPanel::RemoveStaticItem(wxStaticItem *item)
{
  wxNode *node = staticItems.Member((wxObject *)item);
  wxStaticItem *nitem;
  if (node) 
  {
     nitem = (wxStaticItem *) node -> Data();
     nitem -> Show(FALSE);
     delete node;
  }
}

void wxbPanel::DestroyStaticItem(wxStaticItem *item)
{
  wxNode *node = staticItems.Member((wxObject *)item);
  wxStaticItem *s_item;
  if (node)
  {
    s_item = (wxStaticItem *) node -> Data();
    item -> Show(FALSE);
    delete s_item;
    delete node;
  }
}
#endif
