/*								-*- C++ -*-
 *
 * Purpose: simple menu class
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
#pragma implementation "Menu.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxGDI
#define  Uses_wxMenu
#include "wx.h"
#define  Uses_ShellWidget
#define  Uses_MenuWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// constructor and destructor
//-----------------------------------------------------------------------------

wxMenu::wxMenu(char *_title, wxFunction _func)
{
    __type = wxTYPE_MENU;

    // widgets are created by PopupMenu and destroyed by EventCallback
    X    = NULL;

    font = wxSYSTEM_FONT;
    fg   = wxBLACK;
    bg   = wxGREY;
    callback = _func;
    top = topdummy = title = last = 0;
    // if a title is associated with a menu, it may not be removed
    if (_title) {
	Append(-1, _title);
	title = top;
	((menu_item*)title)->type = MENU_TEXT;
	AppendSeparator();
	AppendSeparator();
    } else {
	Append(-1, NULL); // to have something if associated to another menu
	topdummy = top;
    }

    WXGC_IGNORE(owner);
}

wxMenu::~wxMenu(void)
{
    menu_item *item = (menu_item*)top;

    while (item) {
	menu_item *temp = item;
	item = item->next;
	DELETE_VAL temp->label;
	if (temp->contents) { 	// has submenu?
	  wxMenu *mnu;
	  mnu = ((wxMenu*)(temp->user_data));
	  DELETE_OBJ mnu;
	}
	DELETE_VAL temp;
    }
}

//-----------------------------------------------------------------------------
// create and popup menu, will be destroyed by wxMenuCallback
//-----------------------------------------------------------------------------

extern "C" {
  extern void wxAddGrab(Widget);
  extern void wxRemoveGrab(Widget);
};

Bool wxMenu::PopupMenu(Widget in_w, int root_x, int root_y)
{
    Widget wgt;
    Position x, y, new_root_x, new_root_y;
    XEvent xevent;

    if (X)
      return FALSE;

    while (XtParent(in_w)) {
      in_w = XtParent(in_w);
   }

    X = new wxMenu_Widgets;
    X->shell = XtVaCreatePopupShell
	("popup", overrideShellWidgetClass, in_w, NULL);
    wgt = XtVaCreateManagedWidget
	("menu", menuWidgetClass, X->shell,
	 XtNmenu,       top,
	 XtNfont,       font->GetInternalFont(),
	 XtNforeground, fg->GetPixel(wxAPP_COLOURMAP),
	 XtNbackground, bg->GetPixel(wxAPP_COLOURMAP),
	 NULL);
    X->menu = wgt;
    XtRealizeWidget(X->shell);
    XtAddCallback(X->menu, XtNonSelect, wxMenu::EventCallback, this);
    XtAddCallback(X->menu, XtNonNoSelect, wxMenu::EventCallback, this);
    Xaw3dPopupMenuAtPos((MenuWidget)(X->menu), root_x, root_y);

    /* Get the menu started: */
    XtVaGetValues(X->menu, XtNx, &x, XtNy, &y, NULL);
    XtTranslateCoords(X->menu, x, y, &new_root_x, &new_root_y);

    xevent.xmotion.x_root = new_root_x + 5;
    xevent.xmotion.x = 5;
    xevent.xmotion.y_root = new_root_y + 5;
    xevent.xmotion.y = 5;

    XtAddGrab(X->shell, TRUE, FALSE);
    wxAddGrab(X->shell);
    XtCallActionProc(X->menu, "start", &xevent, NULL, 0);

    return TRUE;
}

//-----------------------------------------------------------------------------
// add items to menu
//-----------------------------------------------------------------------------

void wxMenu::Append(long id, char *label, char *help, Bool checkable)
{
    menu_item *item;

    Stop();
  
    item = 0;
    // create new menu item or use topdummy
    if (topdummy) {
	item = (menu_item*)topdummy;
	DELETE_VAL item->label;
	topdummy = 0;
    } else {
#ifdef MZ_PRECISE_GC
      /* FIXME: needs a tag! Moves on Xt lib! */
      item = (menu_item *)GC_malloc(sizeof(menu_item));
#else
      item = new menu_item;
#endif
      // chain or initialize menu_item list
      if (last) {
	menu_item *prev = (menu_item*)last;
	prev->next = item;
	item->prev = prev;
	last = (wxMenuItem*)item;
      } else {
	top = last = (wxMenuItem*)item;
	item->prev = NULL;
      }
    }
    // initialize menu_item
    if ((long)help == -1) {
      /* Hack to avoid parse: */
      char *s;
      s = copystring(label);
      item->label= s;
      item->key_binding = NULL;
    } else {
      wxGetLabelAndKey(label, &item->label, &item->key_binding);
    }
    item->help_text = help;
    item->ID        = id; 
    item->enabled   = TRUE;
    item->set       = FALSE;
    item->contents  = NULL;
    item->next      = NULL;
    item->user_data = NULL;
    item->type      = checkable ? MENU_TOGGLE : MENU_BUTTON;
}

void wxMenu::Append(long id, char *label, wxMenu *submenu, char *help)
{
  menu_item *item;

  /* MATTHEW: enforce one-menu-owner: */
  if (submenu->owner)
    return;

  Stop();

  // do the same thing as if appending a "button"
  Append(id, label, help, FALSE);
  // change data for submenu
  item            = (menu_item*)last;
  item->type      = MENU_CASCADE;
  item->contents  = (menu_item*)submenu->top;
  item->user_data = (void*)submenu;
  /* MATTHEW */
  submenu->owner = (wxMenuItem **)&item->contents;
}

void wxMenu::AppendSeparator(void)
{
    menu_item * item;

    Stop();

    // do the same thing as if appending a "button"
    Append(-1, NULL, NULL, FALSE);
    // change data for separator
    item = (menu_item*)last;
    item->type      = MENU_SEPARATOR;
}

/* MATTHEW: */
Bool wxMenu::DeleteItem(long id, int pos)
{
  menu_item *found, *prev;

  if (id == -1)
    return FALSE;

  for (found = (menu_item*)top; found && pos--; found = found->next) {
    if ((pos < 0) && (found->ID == id))
      break;
  }

  prev = found->prev;

  if (found) {
    Stop();

    if (!prev) {
      top = (wxMenuItem*)found->next;
      if (found->next)
	found->next->prev = NULL;
      if (!top) {
	last = 0;
	Append(-1, NULL); /* Reinstate topdummy */
	topdummy = top;
      }
      if (owner)
	*owner = top;
    } else {
      prev->next = found->next;
      if (prev->next)
	prev->next->prev = prev;
      if (!found->next)
	last = (wxMenuItem*)prev;
    }

    DELETE_VAL found->label;
    /* If there's a submenu, let it go. */
    if (found->contents)
      ((wxMenu *)found->user_data)->owner = NULL;
    DELETE_VAL found;
    return TRUE;
  } else
    return FALSE;
}

Bool wxMenu::Delete(long id)
{
  return DeleteItem(id, -1);
}

Bool wxMenu::DeleteByPosition(int pos)
{
  if (pos > -1)
    return DeleteItem(0, pos);
  else
    return FALSE;
}

int wxMenu::Number()
{
  menu_item *found;
  int n = 0;

  for (found = (menu_item*)top; found; found = found->next) {
    n++;
  }

  if (n && topdummy)
    --n;

  return n;
}

//-----------------------------------------------------------------------------
// modify items
//-----------------------------------------------------------------------------

void wxMenu::Check(long id, Bool flag)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
	found->set = flag;
}

Bool wxMenu::Checked(long id)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
      return found->set;
    return FALSE;
}

void wxMenu::Enable(long id, Bool flag)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found) {
      if (!flag && found->enabled)
	Stop();
      found->enabled = flag;
    }
}

char *wxMenu::GetHelpString(long id)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
      return found->help_text;
    return NULL;
}

char *wxMenu::GetLabel(long id)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
      return found->label;
    return NULL;
}

char *wxMenu::GetTitle(void)
{
    if (title)
      return ((menu_item*)title)->label;
    return NULL;
}

void wxMenu::SetHelpString(long id, char *help)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
      found->help_text = help;
}

void wxMenu::SetLabel(long id, char *label)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found) {
      Stop();
      wxGetLabelAndKey(label, &found->label, &found->key_binding);
    }
}

void wxMenu::SetTitle(char *label)
{
    if (title) {
      menu_item *item;
      Stop();
      item = (menu_item*)title;
      wxGetLabelAndKey(label, &item->label, &item->key_binding);
    }
}

//-----------------------------------------------------------------------------
// find items by ID or by label
//-----------------------------------------------------------------------------

int wxMenu::FindItem(char *itemstring, int strip)
{
    char *label, *key;
    int  answer = -1;

    if (strip)
      wxGetLabelAndKey(itemstring, &label, &key);
    else
      label = itemstring;
    for (menu_item *item = (menu_item*)top; item; item=item->next) {
	if (!strcmp(label, item->label)) { // label found
	    answer = item->ID;
	    break; // found
	}
	if (item->contents) // has submenu => search in submenu
	    if ((answer = ((wxMenu*)item->user_data)->FindItem(label)) > -1)
		break; // found
    }
    return answer;
}

wxMenuItem *wxMenu::FindItemForId(long id, wxMenu **req_menu)
{
    menu_item *answer=NULL;

    for (menu_item *item = (menu_item*)top; item; item=item->next) {
	if (id == item->ID) { // id found
	    answer = item;
	    break; // found
	}
	if (item->contents) // has submenu => search in submenu
	    if ((answer =
		 (menu_item*)((wxMenu*)item->user_data)->FindItemForId(id)))
		break; // found
    }
    if (req_menu)
	*req_menu = (wxMenu*)answer->user_data;
    return ((wxMenuItem*)answer);
}

//-----------------------------------------------------------------------------
// callback for wxMenu::PopupMenu
//-----------------------------------------------------------------------------

void wxMenu::EventCallback(Widget WXUNUSED(w), XtPointer dclient, XtPointer dcall)
{
    wxMenu    *menu  = (wxMenu*)dclient;
    menu_item *item  = (menu_item*)dcall;

    /* MATTHEW: remove grab */
    XtRemoveGrab(menu->X->shell);
    wxRemoveGrab(menu->X->shell);

    // destroy widgets
    XtDestroyWidget(menu->X->shell);
    menu->X->shell = menu->X->menu = 0;
    DELETE_OBJ menu->X;
    menu->X=NULL;

    if (item && (item->ID != -1)) {
      wxPopupEvent *event;

      if (item->type == MENU_TOGGLE)
	item->set = (!item->set);
      
      event = new wxPopupEvent();
      
      event->menuId = item->ID;

      // call callback function
      if (menu->callback)
	menu->callback(menu, event);
    }
}

void wxMenu::Stop()
{
  /* No way to get to menu bar right now... */
}
