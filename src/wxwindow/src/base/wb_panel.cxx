/*
 * File:	wb_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <stdlib.h>
#include <math.h>

// Constructors

wxbPanel::wxbPanel(void)
{
  __type = wxTYPE_PANEL;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;
  window_parent = NULL;
}

wxbPanel::wxbPanel(wxWindow *parent, int WXUNUSED(x),
		   int WXUNUSED(y), int WXUNUSED(width), 
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
}

wxObject* wxbPanel::GetChild(int number)
{
  // Return a pointer to the Nth object in the Panel
  if (!children)
    return(NULL) ;
  wxChildNode *node = GetChildren()->First();
  while (node && number--)
    node = node->Next() ;
  if (node) {
    wxObject *obj = (wxObject *)node->Data();
    return(obj) ;
  } else
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

void wxbPanel::SetLabelFont(wxFont *fnt)
{
  labelFont = fnt;
}

void wxbPanel::SetButtonFont(wxFont *theFont)
{
  buttonFont = theFont;
}

/*
 * Called if in editing mode
 */

// An event outside any items: may be a drag event.
void wxbPanel::OnEvent(wxMouseEvent * /* event */)
{
}
