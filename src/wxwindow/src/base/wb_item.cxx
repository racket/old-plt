/*
 * File:        wb_item.cc
 * Purpose:     Panel items implementation: base (platform-independent) code
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	March 1995
 * RCS_ID:      $Id: wb_item.cxx,v 1.6 1998/08/16 19:23:12 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation

#pragma implementation "wb_item.h"
#pragma implementation "wb_lbox.h"
#pragma implementation "wb_rbox.h"
#pragma implementation "wb_buttn.h"
#pragma implementation "wb_choic.h"
#pragma implementation "wb_check.h"
#pragma implementation "wb_messg.h"
#pragma implementation "wb_slidr.h"
#pragma implementation "wb_slidr.h"
#pragma implementation "wb_menu.h"
#pragma implementation "wb_mnuit.h"
#pragma implementation "wb_txt.h"
#pragma implementation "wb_mtxt.h"
#pragma implementation "wb_menu.h"
#pragma implementation "wb_group.h"
#pragma implementation "wb_gauge.h"
#endif

#include "common.h"
#include "wx_setup.h"

#include "wx_item.h"
#include "wx_lbox.h"
#include "wx_rbox.h"
#include "wx_buttn.h"
#include "wx_choic.h"
#include "wx_check.h"
#include "wx_messg.h"
#include "wx_slidr.h"
#include "wx_group.h"
#include "wx_menu.h"
#include "wx_txt.h"
#include "wx_mtxt.h"

#if USE_GAUGE
#include "wx_gauge.h"
#endif

#include "wx_stdev.h"
#include "wx_utils.h"

#endif

#include <math.h>
#include <stdlib.h>

/* When implementing a new item, be sure to:

 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

/*
   Motif notes

   A panel is a form.
   Each item is created on a RowColumn or Form of its own, to allow a label to
   be positioned. wxListBox and wxMultiText have forms, all the others have RowColumns.
   This is to allow labels to be positioned to the top left (can't do it with a
   RowColumn as far as I know).
   AttachWidget positions widgets relative to one another (left->right, top->bottom)
   unless the x, y coordinates are given (more than -1).
 */

// Item members
wxbItem::wxbItem (void)
{
  __type = wxTYPE_ITEM;

  handleSize = 6;
  handleMargin = 1;
  isSelected = FALSE;
  dragOffsetX = 0;
  dragOffsetY = 0;
  centreX = 0;
  centreY = 0;
}

wxbItem::~wxbItem (void)
{
  wxPanel *parent = (wxPanel *) GetParent ();
  if (parent)
  {
    // parent is not always a wxPanel: can be a wxMenu...
    if (wxSubType(parent->__type,wxTYPE_PANEL))
    {
      if (parent->defaultItem == this)
        parent->defaultItem = NULL;
    }
  }
}

void wxbItem::SetClientSize (int width, int height)
{
  SetSize (-1, -1, width, height);
}

int wxbItem::GetLabelPosition (void)
{
  return labelPosition;
}

void wxbItem::SetLabelPosition (int pos)
{
  labelPosition = pos;
}

void wxbItem::Centre (int direction)
{
  int x, y, width, height, panel_width, panel_height, new_x, new_y;

  wxPanel *panel = (wxPanel *) GetParent ();
  if (!panel)
    return;

  panel->GetClientSize (&panel_width, &panel_height);
  GetSize (&width, &height);
  GetPosition (&x, &y);

  new_x = x;
  new_y = y;

  if (direction & wxHORIZONTAL)
    new_x = (int) ((panel_width - width) / 2);

  if (direction & wxVERTICAL)
    new_y = (int) ((panel_height - height) / 2);

  SetSize (new_x, new_y, width, height);
  int temp_x, temp_y;
  GetPosition (&temp_x, &temp_y);
  GetPosition (&temp_x, &temp_y);
}

/*
 * Manipulation and drawing of items in Edit Mode
 */
 
void wxbItem::SelectItem(Bool select)
{
  isSelected = select;
}

// Returns TRUE or FALSE
Bool wxbItem::HitTest(int x, int y)
{
  int xpos, ypos, width, height;
  GetPosition(&xpos, &ypos);
  GetSize(&width, &height);

  return ((x >= xpos) && (x <= (xpos + width)) && (y >= ypos) && (y <= (ypos + height)));
}

// Calculate position of the 8 handles
void wxbItem::CalcSelectionHandles(int *hx, int *hy)
{
  int xpos, ypos, width, height;
  GetPosition(&xpos, &ypos);
  GetSize(&width, &height);
  int middleX = (int)(xpos + (width/2));
  int middleY = (int)(ypos + (height/2));

  // Start from top middle, clockwise.
/*
  7      0      1

  6             2

  5      4      3
*/

  hx[0] = (int)(middleX - (handleSize/2));
  hy[0] = ypos - handleSize - handleMargin;

  hx[1] = xpos + width + handleMargin;
  hy[1] = ypos - handleSize - handleMargin;

  hx[2] = xpos + width + handleMargin;
  hy[2] = (int)(middleY - (handleSize/2));

  hx[3] = xpos + width + handleMargin;
  hy[3] = ypos + height + handleMargin;

  hx[4] = (int)(middleX - (handleSize/2));
  hy[4] = ypos + height + handleMargin;

  hx[5] = xpos - handleSize - handleMargin;
  hy[5] = ypos + height + handleMargin;

  hx[6] = xpos - handleSize - handleMargin;
  hy[6] = (int)(middleY - (handleSize/2));

  hx[7] = xpos - handleSize - handleMargin;
  hy[7] = ypos - handleSize - handleMargin;
}

// Returns 0 (no hit), 1 - 8 for which selection handle
// (clockwise from top middle)
int wxbItem::SelectionHandleHitTest(int x, int y)
{
  // Handle positions
  int hx[8];
  int hy[8];
  CalcSelectionHandles(hx, hy);

  int i;
  for (i = 0; i < 8; i++)
  {
    if ((x >= hx[i]) && (x <= (hx[i] + handleSize)) && (y >= hy[i]) && (y <= (hy[i] + handleSize)))
      return (i + 1);
  }
  return 0;
}

void wxbItem::DrawSelectionHandles(wxPanelDC *dc, Bool WXUNUSED(erase))
{
//  wxPanel *panel = (wxPanel *)GetParent();
  
  dc->SetOptimization(FALSE);

  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxBLACK_BRUSH);

  dc->SetOptimization(TRUE);

  // Handle positions
  int hx[8];
  int hy[8];
  CalcSelectionHandles(hx, hy);

  int i;
  for (i = 0; i < 8; i++)
  {
    dc->DrawRectangle((float)hx[i], (float)hy[i], (float)handleSize, (float)handleSize);
  }
}

void wxbItem::DrawBoundingBox(wxPanelDC *dc, int x, int y, int w, int h)
{
  dc->DrawRectangle(x, y, w, h);
}

// If selectionHandle is zero, not dragging the selection handle.
void wxbItem::OnDragBegin(int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(keys), 
			  wxPanelDC *WXUNUSED(dc), int WXUNUSED(selectionHandle))
{
}

void wxbItem::OnDragContinue(Bool WXUNUSED(paintIt), int WXUNUSED(x), int WXUNUSED(y), 
			     int WXUNUSED(keys), wxPanelDC *WXUNUSED(dc), int WXUNUSED(selectionHandle))
{
}

void wxbItem::OnDragEnd(int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(keys), 
			wxPanelDC *WXUNUSED(dc), int WXUNUSED(selectionHandle))
{
}

// These functions call OnItemEvent, OnItemMove and OnItemSize
// by default.
void wxbItem::OnEvent(wxMouseEvent& event)
{
  if ((event.eventType == wxEVENT_TYPE_LEFT_DCLICK) ||
      (event.eventType == wxEVENT_TYPE_RIGHT_DCLICK))
    return;
    
  wxWindow *theParent = this->GetParent();
  ((wxPanel *)theParent)->GetEventHandler()->OnItemEvent((wxItem *)this, event);
}

void wxbItem::OnMove(int x, int y)
{
  wxWindow *theParent = GetParent();
  ((wxPanel *)theParent)->GetEventHandler()->OnItemMove((wxItem *)this, x, y);
}

void wxbItem::OnSize(int w, int h)
{
  wxWindow *theParent = GetParent();
  ((wxPanel *)theParent)->GetEventHandler()->OnItemSize((wxItem *)this, w, h);
}

void wxbItem::OnSelect(Bool select)
{
  wxWindow *theParent = GetParent();
  ((wxPanel *)theParent)->GetEventHandler()->OnItemSelect((wxItem *)this, select);
}

void wxbItem::OnLeftClick(int x, int y, int keys)
{
  wxWindow *theParent = GetParent();
  ((wxPanel *)theParent)->GetEventHandler()->OnItemLeftClick((wxItem *)this, x, y, keys);
}

void wxbItem::OnRightClick(int x, int y, int keys)
{
  wxWindow *theParent = GetParent();
  ((wxPanel *)theParent)->GetEventHandler()->OnItemRightClick((wxItem *)this, x, y, keys);
}

void wxbItem::Command (wxCommandEvent & event)
{
  ProcessCommand (event);
}

void wxbItem::ProcessCommand (wxCommandEvent & event)
{
  wxFunction fun = callback;
  if (fun)
    (void) (*(fun)) (*this, event);
}

/*
 * Button
 */
 
wxbButton::wxbButton (void)
{
  __type = wxTYPE_BUTTON;
  window_parent = NULL;
  labelPosition = wxHORIZONTAL;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbButton::wxbButton (wxPanel * panel, wxFunction WXUNUSED(Function), char *WXUNUSED(label),
	   int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_BUTTON;
  windowStyle = style;
  window_parent = panel;
  labelPosition = wxHORIZONTAL;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbButton::wxbButton (wxPanel * panel, wxFunction WXUNUSED(Function), wxBitmap * WXUNUSED(bitmap),
	   int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_BUTTON;
  windowStyle = style;
  window_parent = panel;
  labelPosition = wxHORIZONTAL;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbButton::~wxbButton (void)
{
}

/*
 * Menu
 */

// Construct a menu with optional title (then use append)
wxbMenu::wxbMenu (char *Title, wxFunction WXUNUSED(func))
{
  __type = wxTYPE_MENU;
  no_items = 0;
  menu_bar = NULL;
  WXGC_IGNORE(menu_bar);
  WXGC_IGNORE(top_level_menu);
  if (Title)
    title = copystring (Title);
  else
    title = NULL;
}

// The wxWindow destructor will take care of deleting the submenus.
wxbMenu::~wxbMenu (void)
{
  if (title)
    delete[]title;
}

// Finds the item id matching the given string, -1 if not found.
int wxbMenu::FindItem (char *itemString)
{
  char buf1[200];
  char buf2[200];
  wxStripMenuCodes (itemString, buf1);

  for (wxNode * node = menuItems.First (); node; node = node->Next ())
    {
      wxMenuItem *item = (wxMenuItem *) node->Data ();
      if (item->subMenu)
	{
	  int ans = item->subMenu->FindItem (itemString);
	  if (ans > -1)
	    return ans;
	}
      if ((item->itemId > -1) && item->itemName)
	{
	  wxStripMenuCodes (item->itemName, buf2);
	  if (strcmp (buf1, buf2) == 0)
	    return item->itemId;
	}
    }

  return -1;
}

wxMenuItem *wxbMenu::FindItemForId (long itemId, wxMenu ** itemMenu, int * pos)
{
  int i = 0;
  if (itemMenu)
    *itemMenu = NULL;
  for (wxNode * node = menuItems.First (); node; node = node->Next (), i++) {
    wxMenuItem *item = (wxMenuItem *) node->Data ();

    if (item->itemId == itemId) {
      if (itemMenu)
	*itemMenu = (wxMenu *) this;
      if (pos)
	*pos = i;
      return item;
    }

    if (item->subMenu) {
      wxMenuItem *ans = item->subMenu->FindItemForId (itemId, itemMenu, pos);
      if (ans)
	return ans;
    }
  }

  if (itemMenu)
    *itemMenu = NULL;
  return NULL;
}

void wxbMenu::SetHelpString (long itemId, char *helpString)
{
  wxMenuItem *item = FindItemForId (itemId);
  if (item)
    {
      if (item->helpString)
	delete[]item->helpString;
      if (helpString)
	item->helpString = copystring (helpString);
      else
	item->helpString = NULL;
    }
}

char *wxbMenu::GetHelpString (long itemId)
{
  wxMenuItem *item = FindItemForId (itemId);
  if (item)
    return item->helpString;
  else
    return NULL;
}

void wxbMenu::ProcessCommand (wxCommandEvent & event)
{
  wxFunction fun = callback;
  if (fun)
    (void) (*(fun)) (*this, event);
}

/*
 * Menu Bar
 */

wxbMenuBar::wxbMenuBar (void)
{
  __type = wxTYPE_MENU_BAR;
  n = 0;
  menus = NULL;
  titles = NULL;
  menu_bar_frame = NULL;
  WXGC_IGNORE(menu_bar_frame);
}

wxbMenuBar::wxbMenuBar (int N, wxMenu * Menus[], char *Titles[])
{
  __type = wxTYPE_MENU_BAR;
  n = N;
  menus = Menus;
  titles = Titles;
  menu_bar_frame = NULL;
  int i;
  for (i = 0; i < N; i++)
    menus[i]->menu_bar = (wxMenuBar *) this;
  WXGC_IGNORE(menu_bar_frame);
}

wxbMenuBar::~wxbMenuBar (void)
{
}

void wxbMenuBar::Append (wxMenu * menu, char *title)
{
  /* MATTHEW: [6] */
  if (!OnAppend(menu, title))
	 return;

  n++;
  wxMenu **new_menus = new wxMenu *[n];
  char **new_titles = new char *[n];
  int i;

  for (i = 0; i < n - 1; i++)
	 {
		new_menus[i] = menus[i];
		menus[i] = NULL;
		new_titles[i] = titles[i];
		titles[i] = NULL;
	 }
  if (menus)
	 {
		delete[]menus;
		delete[]titles;
	 }
  menus = new_menus;
  titles = new_titles;

  menus[n - 1] = menu;
  titles[n - 1] = copystring (title);

  menu->menu_bar = (wxMenuBar *) this;
  /* MATTHEW: [11] */
  menu->SetParent(this);
}

/* MATTHEW: [6] */
Bool wxbMenuBar::Delete(wxMenu * menu, int i)
{
  int j;

  if (menu) {
    for (i = 0; i < n; i++) {
      if (menus[i] == menu)
	break;
    }
    if (i >= n)
      return FALSE;
  } else {
    if (i < 0 || i >= n)
      return FALSE;
    menu = menus[i];
  }

  if (!OnDelete(menu, i))
    return FALSE;

  /* MATTHEW: [11] */
  menu->SetParent(NULL);

  --n;
  for (j = i; j < n; j++) {
    menus[j] = menus[j + 1];
    titles[j] = titles[j + 1];
  }

  return TRUE;
}

int wxbMenuBar::Number(void)
{
  return n;
}

// Find the menu menuString, item itemString, and return the item id.
// Returns -1 if none found.
int wxbMenuBar::FindMenuItem (char *menuString, char *itemString)
{
  char buf1[200];
  char buf2[200];
  wxStripMenuCodes (menuString, buf1);
  int i;
  for (i = 0; i < n; i++)
    {
      wxStripMenuCodes (titles[i], buf2);
      if (strcmp (buf1, buf2) == 0)
	return menus[i]->FindItem (itemString);
    }
  return -1;
}

wxMenuItem *wxbMenuBar::FindItemForId (long Id, wxMenu ** itemMenu)
{
  if (itemMenu)
    *itemMenu = NULL;

  wxMenuItem *item = NULL;
  int i;
  for (i = 0; i < n; i++)
    if ((item = menus[i]->FindItemForId (Id, itemMenu)))
      return item;
  return NULL;
}

void wxbMenuBar::SetHelpString (long Id, char *helpString)
{
  int i;
  for (i = 0; i < n; i++)
    if (menus[i]->FindItemForId (Id)) {
      menus[i]->SetHelpString (Id, helpString);
      return;
    }
}

char *wxbMenuBar::GetHelpString (long Id)
{
  int i;
  for (i = 0; i < n; i++) {
    if (menus[i]->FindItemForId(Id))
      return menus[i]->GetHelpString (Id);
  }
  return NULL;
}

/*
 * Single check box item
 */
 
wxbCheckBox::wxbCheckBox (void)
{
  __type = wxTYPE_CHECK_BOX;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbCheckBox::wxbCheckBox (wxPanel * panel, wxFunction WXUNUSED(func), char *WXUNUSED(Title),
	     int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_CHECK_BOX;
  windowStyle = style;
  window_parent = panel;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbCheckBox::wxbCheckBox (wxPanel * panel, wxFunction WXUNUSED(func), wxBitmap * WXUNUSED(bitmap),
	     int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_CHECK_BOX;
  windowStyle = style;
  window_parent = panel;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbCheckBox::~wxbCheckBox (void)
{
}

/*
 * Choice
 */
 
wxbChoice::wxbChoice (void)
{
  __type = wxTYPE_CHOICE;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbChoice::wxbChoice (wxPanel * panel, wxFunction WXUNUSED(func), char *WXUNUSED(Title),
	   int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), int N, char **WXUNUSED(Choices),
	   long style, char *WXUNUSED(name))
{
  __type = wxTYPE_CHOICE;
  windowStyle = style;
  window_parent = panel;
  labelPosition = panel->label_position;
  no_strings = N;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbChoice::~wxbChoice (void)
{
}

char *wxbChoice::GetStringSelection (void)
{
  int sel = GetSelection ();
  if (sel > -1)
    return this->GetString (sel);
  else
    return NULL;
}

Bool wxbChoice::SetStringSelection (char *s)
{
  int sel = FindString (s);
  if (sel > -1)
    {
      SetSelection (sel);
      return TRUE;
    }
  else
    return FALSE;
}


/*
 * Listbox
 */
 
wxbListBox::wxbListBox (void)
{
  __type = wxTYPE_LIST_BOX;
  selected = -1;
  selections = 0;
  no_items = 0;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbListBox::wxbListBox (wxPanel * panel, wxFunction WXUNUSED(func),
	    char *WXUNUSED(Title), Bool Multiple,
	    int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height),
	    int WXUNUSED(N), char **WXUNUSED(Choices), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_LIST_BOX;
  windowStyle = style;
  selected = -1;
  selections = 0;
  multiple = Multiple;
  window_parent = panel;
  no_items = 0;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbListBox::~wxbListBox (void)
{
}

int wxbListBox::Number (void)
{
  return no_items;
}

// For single selection items only
char *wxbListBox::GetStringSelection (void)
{
  int sel = GetSelection ();
  if (sel > -1)
    return this->GetString (sel);
  else
    return NULL;
}

Bool wxbListBox::SetStringSelection (char *s)
{
  int sel = FindString (s);
  if (sel > -1)
    {
      SetSelection (sel);
      return TRUE;
    }
  else
    return FALSE;
}


/*
 * Radiobox item
 */
 
wxbRadioBox::wxbRadioBox (void)
{
  __type = wxTYPE_RADIO_BOX;
  selected = -1;
  no_items = 0;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction WXUNUSED(func),
	     char *WXUNUSED(Title),
	     int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height),
	     int WXUNUSED(N), char **WXUNUSED(Choices),
	     int WXUNUSED(majorDim), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_RADIO_BOX;
  windowStyle = style;
  selected = -1;
  window_parent = panel;
  no_items = 0;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

// #ifndef __BORLANDC__
wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction WXUNUSED(func),
	     char *WXUNUSED(Title),
	     int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height),
	     int WXUNUSED(N), wxBitmap ** WXUNUSED(Choices),
	     int WXUNUSED(majorDim), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_RADIO_BOX;
  windowStyle = style;
  selected = -1;
  window_parent = panel;
  no_items = 0;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}
// #endif

wxbRadioBox::~wxbRadioBox (void)
{
}

int wxbRadioBox::Number (void)
{
  return no_items;
}

// For single selection items only
char *wxbRadioBox::GetStringSelection (void)
{
  int sel = GetSelection ();
  if (sel > -1)
    return this->GetString (sel);
  else
    return NULL;
}

Bool wxbRadioBox::SetStringSelection (char *s)
{
  int sel = FindString (s);
  if (sel > -1)
    {
      SetSelection (sel);
      return TRUE;
    }
  else
    return FALSE;
}


/*
 * Message
 */
 
wxbMessage::wxbMessage (void)
{
  __type = wxTYPE_MESSAGE;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbMessage::wxbMessage (wxPanel * panel, char *WXUNUSED(label), int WXUNUSED(x), int WXUNUSED(y),
   long style, char *WXUNUSED(name))
{
  __type = wxTYPE_MESSAGE;
  windowStyle = style;
  window_parent = panel;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

#if USE_BITMAP_MESSAGE
wxbMessage::wxbMessage (wxPanel * panel, wxBitmap *WXUNUSED(image), int WXUNUSED(x), int WXUNUSED(y),
 long style, char *WXUNUSED(name))
{
  __type = wxTYPE_MESSAGE;
  windowStyle = style;
  window_parent = panel;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}
#endif

wxbMessage::~wxbMessage (void)
{
}

/*
 * Slider
 */
 
wxbSlider::wxbSlider (void)
{
  __type = wxTYPE_SLIDER;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbSlider::wxbSlider (wxPanel * panel, wxFunction WXUNUSED(func), char *WXUNUSED(label), int WXUNUSED(value),
	   int WXUNUSED(min_value), int WXUNUSED(max_value), int WXUNUSED(width),
           int WXUNUSED(x), int WXUNUSED(y), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_SLIDER;
  windowStyle = style;
  window_parent = panel;
  labelPosition = panel->label_position;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbSlider::~wxbSlider (void)
{
}

/*
 * Gauge
 */
 
#if USE_GAUGE
wxbGauge::wxbGauge (void)
{
  __type = wxTYPE_GAUGE;
  window_parent = NULL;
  labelPosition = wxHORIZONTAL;
  buttonFont = NULL;
  labelFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
}

wxbGauge::wxbGauge (wxPanel * panel, char *WXUNUSED(label),
	   int WXUNUSED(range), int WXUNUSED(x), int WXUNUSED(y),
           int WXUNUSED(width), int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  __type = wxTYPE_GAUGE;
  windowStyle = style;
  window_parent = panel;
  labelPosition = wxHORIZONTAL;
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
}

wxbGauge::~wxbGauge (void)
{
}
#endif
