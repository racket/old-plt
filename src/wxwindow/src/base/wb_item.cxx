/*
 * File:        wb_item.cc
 * Purpose:     Panel items implementation: base (platform-independent) code
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	March 1995
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#include "wx.h"

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

void wxbItem::Command (wxCommandEvent *event)
{
  ProcessCommand (event);
}

void wxbItem::ProcessCommand (wxCommandEvent *event)
{
  wxFunction fun = callback;
  if (fun)
    fun(this, event);
}

/*
 * Button
 */
 
wxbButton::wxbButton (wxPanel * panel, wxFunction WXUNUSED(Function), 
		      char *WXUNUSED(label),
		      int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), 
		      int WXUNUSED(height), long style, char *WXUNUSED(name))
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

wxbButton::wxbButton (wxPanel * panel, wxFunction WXUNUSED(Function), 
		      wxBitmap * WXUNUSED(bitmap),
		      int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width),
		      int WXUNUSED(height), long style, char *WXUNUSED(name))
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
  menuItems = new wxList();
}

// The wxWindow destructor will take care of deleting the submenus.
wxbMenu::~wxbMenu (void)
{
  if (title)
    delete[]title;
}

wxMenuItem *wxbMenu::FindItemForId (long itemId, wxMenu ** itemMenu, int * pos)
{
  int i = 0;
  if (itemMenu)
    *itemMenu = NULL;
  for (wxNode * node = menuItems->First (); node; node = node->Next (), i++) {
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

void wxbMenu::ProcessCommand (wxCommandEvent *event)
{
  wxFunction fun = callback;
  if (fun)
    fun(this, event);
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
 
wxbListBox::wxbListBox(wxPanel * panel, wxFunction WXUNUSED(func),
		       char *WXUNUSED(Title), Bool Multiple,
		       int WXUNUSED(x), int WXUNUSED(y), 
		       int WXUNUSED(width), int WXUNUSED(height),
		       int WXUNUSED(N), char **WXUNUSED(Choices),
		       long style, char *WXUNUSED(name))
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
      SetOneSelection(sel);
      return TRUE;
    }
  else
    return FALSE;
}


/*
 * Radiobox item
 */
 
wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction WXUNUSED(func),
			  char *WXUNUSED(Title),
			  int WXUNUSED(x), int WXUNUSED(y),
			  int WXUNUSED(width), int WXUNUSED(height),
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

wxbRadioBox::wxbRadioBox (wxPanel * panel, wxFunction WXUNUSED(func),
			  char *WXUNUSED(Title),
			  int WXUNUSED(x), int WXUNUSED(y),
			  int WXUNUSED(width), int WXUNUSED(height),
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
 
wxbMessage::wxbMessage (wxPanel * panel, char *WXUNUSED(label),
			int WXUNUSED(x), int WXUNUSED(y),
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

wxbMessage::wxbMessage (wxPanel * panel, wxBitmap *WXUNUSED(image), 
			int WXUNUSED(x), int WXUNUSED(y),
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

wxbMessage::~wxbMessage (void)
{
}

/*
 * Slider
 */
 
wxbSlider::wxbSlider (wxPanel * panel, wxFunction WXUNUSED(func), 
		      char *WXUNUSED(label), int WXUNUSED(value),
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
 
wxbGauge::wxbGauge (wxPanel * panel, char *WXUNUSED(label),
		    int WXUNUSED(range), int WXUNUSED(x), int WXUNUSED(y),
		    int WXUNUSED(width), int WXUNUSED(height), 
		    long style, char *WXUNUSED(name))
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
