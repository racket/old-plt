/*
 * File:	wb_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_panel.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
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

  dragSlow = TRUE;
  dragMode = wxDRAG_MODE_NONE;
  dragType = wxDRAG_TYPE_NONE;
  dragItem = NULL;
  firstDragX = 0;
  firstDragY = 0;
  oldDragX = 0;
  oldDragY = 0;
  dragTolerance = 3;
  checkTolerance = TRUE;
}

wxbPanel::wxbPanel(wxWindow *parent, int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height),
   long style, char *WXUNUSED(name))
{
  __type = wxTYPE_PANEL;
  windowStyle = style;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;

  window_parent = parent;

  dragSlow = TRUE;
  dragMode = wxDRAG_MODE_NONE;
  dragType = wxDRAG_TYPE_NONE;
  dragItem = NULL;
  firstDragX = 0;
  firstDragY = 0;
  oldDragX = 0;
  oldDragY = 0;
  dragTolerance = 3;
  checkTolerance = TRUE;
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
    but->Command(*event);
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
void wxbPanel::OnEvent(wxMouseEvent& event)
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
  ProcessItemEvent(item, event, dragType);
}

void wxbPanel::ProcessItemEvent(wxItem *item, wxMouseEvent& event, int selectionHandle)
{
}

// Calls DrawSelectionHandles for all items if
// edit mode is on.
void wxbPanel::PaintSelectionHandles(void)
{
}


#if USE_WX_RESOURCES
#include "wx_res.h"
#include "wx_buttn.h"
#include "wx_check.h"
#include "wx_choic.h"
#include "wx_group.h"
#include "wx_messg.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_rbox.h"
#include "wx_lbox.h"
#if USE_GAUGE
#include "wx_gauge.h"
#endif
#include "wx_slidr.h"
#include "wx_dialg.h"

/*
 * Optional resource loading facility
 */

Bool wxbPanel::LoadFromResource(wxWindow *parent, char *resourceName, wxResourceTable *table)
{
  if (!table)
    table = &wxDefaultResourceTable;
    
  wxItemResource *resource = table->FindResource(resourceName);
  if (!resource || (resource->GetType() != wxTYPE_DIALOG_BOX))
    return FALSE;
  char *title = resource->GetTitle();
  long theWindowStyle = resource->GetStyle();
  Bool isModal = (Bool)resource->GetValue1();
  int x = resource->GetX();
  int y = resource->GetY();
  int width = resource->GetWidth();
  int height = resource->GetHeight();
  char *name = resource->GetName();

  wxFont *theButtonFont = (wxFont *)resource->GetValue2();
  wxFont *theLabelFont = (wxFont *)resource->GetValue3();

  if (wxSubType(__type, wxTYPE_DIALOG_BOX))
  {
    wxDialogBox *dialogBox = (wxDialogBox *)this;
    if (!dialogBox->Create(parent, title, isModal, x, y, width, height, theWindowStyle, name))
      return FALSE;
  }
  else
  {
    if (!((wxPanel *)this)->Create(parent, x, y, width, height, theWindowStyle, name))
      return FALSE;
  }

  if (theButtonFont)
    SetButtonFont(theButtonFont);
  if (theLabelFont)
    SetLabelFont(theLabelFont);

  // Now create children
  wxNode *node = resource->GetChildren().First();
  while (node)
  {
    wxItemResource *childResource = (wxItemResource *)node->Data();
    if (childResource->GetStyle() & wxVERTICAL_LABEL)
      SetLabelPosition(wxVERTICAL);
    else
      SetLabelPosition(wxHORIZONTAL);
      
    switch (childResource->GetType())
    {
      case wxTYPE_BUTTON:
      {
        wxButton *control = NULL;
        if (childResource->GetValue4())
        {
          // Bitmap button
          wxBitmap *bitmap = childResource->GetBitmap();
          if (!bitmap)
          {
            bitmap = wxResourceCreateBitmap(childResource->GetValue4(), table);
            childResource->SetBitmap(bitmap);
          }
          if (bitmap)
           control = new wxButton((wxPanel *)this, (wxFunction)NULL, bitmap,
             childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
             childResource->GetStyle(), childResource->GetName());
        }
        else
          // Normal, text button
          control = new wxButton((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_MESSAGE:
      {
        wxMessage *control = NULL;
        if (childResource->GetValue4())
        {
          // Bitmap button
          wxBitmap *bitmap = childResource->GetBitmap();
          if (!bitmap)
          {
            bitmap = wxResourceCreateBitmap(childResource->GetValue4(), table);
            childResource->SetBitmap(bitmap);
          }
#if USE_BITMAP_MESSAGE
          if (bitmap)
           control = new wxMessage((wxPanel *)this, bitmap,
             childResource->GetX(), childResource->GetY(), // childResource->GetWidth(), childResource->GetHeight(),
             childResource->GetStyle(), childResource->GetName());
#endif
        }
        else
        {
           control = new wxMessage((wxPanel *)this, childResource->GetTitle(),
             childResource->GetX(), childResource->GetY(), // childResource->GetWidth(), childResource->GetHeight(),
             childResource->GetStyle(), childResource->GetName());
        }
        break;
      }
      case wxTYPE_TEXT:
      {
        (void)new wxText((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), childResource->GetValue4(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_MULTI_TEXT:
      {
        (void)new wxMultiText((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), childResource->GetValue4(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_CHECK_BOX:
      {
        wxCheckBox *control = new wxCheckBox((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        control->SetValue((Bool)childResource->GetValue1());
        break;
      }
#if USE_GAUGE
      case wxTYPE_GAUGE:
      {
        wxGauge *control = new wxGauge((wxPanel *)this, childResource->GetTitle(), (int)childResource->GetValue2(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        control->SetValue((int)childResource->GetValue1());
        break;
      }
#endif
      case wxTYPE_SLIDER:
      {
        (void)new wxSlider((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), (int)childResource->GetValue1(),
           (int)childResource->GetValue2(), (int)childResource->GetValue3(),
           childResource->GetWidth(), childResource->GetX(), childResource->GetY(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_GROUP_BOX:
      {
        (void)new wxGroupBox((wxPanel *)this, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_LIST_BOX:
      {
        wxStringList *stringList = childResource->GetStringValues();
        char **strings = NULL;
        int noStrings = 0;
        if (stringList && (stringList->Number() > 0))
        {
          noStrings = stringList->Number();
          strings = new char *[noStrings];
          wxNode *node = stringList->First();
          int i = 0;
          while (node)
          {
            strings[i] = (char *)node->Data();
            i ++;
            node = node->Next();
          }
        }
        (void)new wxListBox((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), (int)childResource->GetValue1(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           noStrings, strings, childResource->GetStyle(), childResource->GetName());

        if (strings)
          delete[] strings;

        break;
      }
      case wxTYPE_CHOICE:
      {
        wxStringList *stringList = childResource->GetStringValues();
        char **strings = NULL;
        int noStrings = 0;
        if (stringList && (stringList->Number() > 0))
        {
          noStrings = stringList->Number();
          strings = new char *[noStrings];
          wxNode *node = stringList->First();
          int i = 0;
          while (node)
          {
            strings[i] = (char *)node->Data();
            i ++;
            node = node->Next();
          }
        }
        (void)new wxChoice((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           noStrings, strings, childResource->GetStyle(), childResource->GetName());

        if (strings)
          delete[] strings;

        break;
      }
      case wxTYPE_RADIO_BOX:
      {
        wxStringList *stringList = childResource->GetStringValues();
        char **strings = NULL;
        int noStrings = 0;
        if (stringList && (stringList->Number() > 0))
        {
          noStrings = stringList->Number();
          strings = new char *[noStrings];
          wxNode *node = stringList->First();
          int i = 0;
          while (node)
          {
            strings[i] = (char *)node->Data();
            i ++;
            node = node->Next();
          }
        }
        (void)new wxRadioBox((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           noStrings, strings, 0, childResource->GetStyle(), childResource->GetName());

        if (strings)
          delete[] strings;

        break;
      }
    }
    node = node->Next();
  }
  return TRUE;
}
#endif

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
