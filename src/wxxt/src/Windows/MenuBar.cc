/*								-*- C++ -*-
 * $Id: MenuBar.cc,v 1.4 1998/04/22 15:36:34 mflatt Exp $
 *
 * Purpose: menu bar class
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
#pragma implementation "MenuBar.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxLayout
#define  Uses_wxMenuBar
#include "wx.h"
#define  Uses_EnforcerWidget
#define  Uses_MenuWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// constructor and destructor
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxMenuBar, wxItem)

wxMenuBar::wxMenuBar(void) : wxItem()
{
    __type = wxTYPE_MENU_BAR;

    top = topdummy = help = last = 0;
    // if a title is associated with a menu, it may not be removed
    Append(NULL, NULL); // to have something if associated to frame
    topdummy = top;
}

wxMenuBar::wxMenuBar(int n, wxMenu *menus[], char *titles[]) : wxItem()
{
    __type = wxTYPE_MENU_BAR;

    top = topdummy = help = last = 0;
    // if a title is associated with a menu, it may not be removed
    if (n) {
	for (int i=0; i<n; ++i)
	    Append(menus[i], titles[i]);
    } else {
	Append(NULL, NULL); // to have something if associated to frame
	topdummy = top;
    }
}

wxMenuBar::~wxMenuBar(void)
{
    menu_item *item = (menu_item*)top;

    while (item) {
	menu_item *temp = item;
	item = item->next;
	if (temp->contents) { // has submenu?
	  delete temp->label;                  // delete label
	  delete ((wxMenu*)(temp->user_data)); // delete wxMenu
	}
	delete temp;		// delete menu_item
    }
}

//-----------------------------------------------------------------------------
// create and destroy menubar
//-----------------------------------------------------------------------------

Bool wxMenuBar::Create(wxPanel *panel)
{
    ChainToPanel(panel, 0, "menubar");

    // create widgets
    X->frame = XtVaCreateManagedWidget
	("menubar", xfwfEnforcerWidgetClass, panel->GetHandle()->handle,
	 XtNtraversalOn, FALSE, XtNhighlightThickness, 0,
	 NULL);
    X->handle = XtVaCreateWidget
	("menubar", menuWidgetClass, X->frame,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  fg->GetPixel(cmap),
	 XtNhMargin,     4,
	 XtNfont,        font->GetInternalFont(),
	 XtNmenu,        top,
	 XtNcursor,      None,
	 NULL);
    // callbacks
    XtAddCallback(X->handle, XtNonSelect,  wxMenuBar::CommandEventCallback, this);
    XtAddCallback(X->handle, XtNonNewItem, wxMenuBar::SelectEventCallback, this);
    // position menubar
    Dimension hh, ww;
    XtVaGetValues(X->handle, XtNheight, &hh, XtNwidth, &ww, NULL);
    XtVaSetValues(X->frame,  XtNheight,  hh, XtNwidth,  ww, NULL);
    constraints->top.Absolute(-hh);
    constraints->left.Absolute(0);
    constraints->width.SameAs(panel, wxWidth, 0);
    constraints->height.Absolute(hh);
    // menubar may now be managed
    XtManageChild(X->handle);
    AddEventHandlers();

    return TRUE;
}

void wxMenuBar::Destroy(void)
{
    if (parent)    parent->RemoveChild(this);
    if (X->frame)  XtDestroyWidget(X->frame);
    parent = NULL;
    X->frame = X->handle = 0;
}

//-----------------------------------------------------------------------------
// add items to menu
//-----------------------------------------------------------------------------

void wxMenuBar::Append(wxMenu *menu, char *title)
{
    if (!menu || !title) // I need menu and title
	return;

    /* MATTHEW: enforce safety */
    if (menu->owner)
      return;

    menu_item *item = 0;
    // create new menu item or use topdummy
    if (topdummy) {
	item = (menu_item*)topdummy;
	delete item->label;
	topdummy = 0;
    } else {
	item = new menu_item;
    }
    // initialize menu_item
    wxGetLabelAndKey(title, &item->label, &item->key_binding);
    item->help_text = NULL;
    item->ID        = -1; 
    item->enabled   = TRUE;
    item->set       = FALSE;
    item->contents  = (menu_item*)menu->top;
    menu->owner     = (wxMenuItem **)&item->contents; /* MATTHEW */
    item->next      = NULL;
    item->user_data = (void*)menu;
    item->type      = (!strcmp(item->label, "Help")) ? MENU_HELP : MENU_CASCADE;
    // chain or initialize menu_item list
    if (last) {
      menu_item *prev = (menu_item*)last;
      prev->next = item;
      last = (wxMenuItem*)item;
    } else {
      top = last = (wxMenuItem*)item;
    }
    if (X->handle) { // redisplay if menu added
      XtVaSetValues(X->handle, XtNmenu, top, XtNrefresh, True, NULL);
    }
}

/* MATTHEW: */
Bool wxMenuBar::Delete(wxMenu *menu, int pos)
{
  menu_item *i, *prev;
  int counter;

  if (!menu && (pos < 0))
    return FALSE;

  prev = NULL;
  for (i = (menu_item *)top, counter = 0; 
       i && ((menu && (i->user_data != (void *)menu))
	     || (!menu && (counter < pos)));
       counter++) {
    prev = i;
    i = i->next;
  }

  if (i) {
    if (i == (menu_item *)top)
      top = (wxMenuItem *)i->next;
    if (i == (menu_item *)last)
      last = (wxMenuItem *)prev;
    if (prev)
      prev->next = i->next;

    if (!top) {
      Append(NULL, NULL); // to have something if associated to frame
      topdummy = top;
    }

    if (i->contents) {
      delete i->label;
      /* Release menu: */
      ((wxMenu *)(i->user_data))->owner = NULL;
    }

    delete i;

    if (X->handle) { // redisplay
      XtVaSetValues(X->handle, XtNmenu, top, XtNrefresh, True, NULL);
    }

    return TRUE;
  } else
    return FALSE;
}

int wxMenuBar::Number()
{
  menu_item *i;
  int counter = 0;

  for (i = (menu_item *)top; i; i = i->next)
    counter++;

  if (counter && topdummy)
    --counter;

  return counter;
}

//-----------------------------------------------------------------------------
// modify items
//-----------------------------------------------------------------------------

void wxMenuBar::Check(int id, Bool flag)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found)
	found->set = flag;
}

Bool wxMenuBar::Checked(int id)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found)
	return found->set;
    return FALSE;
}

void wxMenuBar::Enable(int id, Bool flag)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found)
	found->enabled = flag;
}

void wxMenuBar::EnableTop(int pos, Bool flag)
{
    menu_item *item = (menu_item*)top;

    for (int i=0; item && i<pos; ++i)
	item = item->next;
    if (item) {
      if (X->handle) {
	item->enabled = flag;
	XtVaSetValues(X->handle, XtNmenu, top, XtNrefresh, True, NULL);
      }
    }
}

char *wxMenuBar::GetHelpString(int id)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found)
	return found->help_text;
    return NULL;
}

char *wxMenuBar::GetLabel(int id)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found)
	return found->label;
    return NULL;
}

char *wxMenuBar::GetLabelTop(int pos)
{
    menu_item *item = (menu_item*)top;

    for (int i=0; item && i<pos; ++i)
	item = item->next;
    if (item)
	return item->label;
    return NULL;
}

void wxMenuBar::SetHelpString(int id, char *help)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found)
	found->help_text = help;
}

void wxMenuBar::SetLabel(int id, char *label)
{
    menu_item *found = (menu_item*)FindItemForId(id);
    if (found) {
	delete found->label;
	wxGetLabelAndKey(label, &found->label, &found->key_binding);
    }
}

void wxMenuBar::SetLabelTop(int pos, char *label)
{
    menu_item *item = (menu_item*)top;

    for (int i=0; item && i<pos; ++i)
	item = item->next;
    if (item) {
	delete item->label;
	wxGetLabelAndKey(label, &item->label, &item->key_binding);
	if (X->handle) { // redisplay if menu added
	  XtVaSetValues(X->handle, XtNmenu, top, XtNrefresh, True, NULL);
	}
    }
}

//-----------------------------------------------------------------------------
// find items by ID or by label
//-----------------------------------------------------------------------------

int wxMenuBar::FindMenuItem(char *menu, char *itemstring)
{
    char *label, *key;
    int  answer = -1;

    wxGetLabelAndKey(menu, &label, &key);

    for (menu_item *item = (menu_item*)top; item; item=item->next)
	if (!strcmp(item->label, label) && item->contents) {
	    answer = ((wxMenu*)item->user_data)->FindItem(itemstring);
	    break;
	}
    delete label;
    return answer;
}

wxMenuItem *wxMenuBar::FindItemForId(int id, wxMenu **req_menu)
{
    menu_item *answer=NULL;

    for (menu_item *item = (menu_item*)top; item; item=item->next) {
	if (item->contents)
	    if ((answer = (menu_item*)((wxMenu*)item->user_data)->FindItemForId(id)))
		break; // found
    }
    if (req_menu)
	*req_menu = (wxMenu*)answer->user_data;
    return ((wxMenuItem*)answer);
}

//-----------------------------------------------------------------------------
// callbacks for wxMenuBar
//-----------------------------------------------------------------------------

void wxMenuBar::CommandEventCallback(Widget WXUNUSED(w),
				     XtPointer dclient, XtPointer dcall)
{
    wxMenuBar *menu  = (wxMenuBar*)dclient;
    menu_item *item  = (menu_item*)dcall;

    if (item->type == MENU_TOGGLE)
	item->set = (!item->set);

    // call OnMenuCommandt of parent (usually of a frame)
    if (menu->parent)
	menu->parent->GetEventHandler()->OnMenuCommand(item->ID);
}

void wxMenuBar::SelectEventCallback(Widget WXUNUSED(w),
				    XtPointer dclient, XtPointer dcall)
{
    wxMenuBar *menu  = (wxMenuBar*)dclient;
    menu_item *item  = (menu_item*)dcall;

    // call OnMenuSelect of parent (usually of a frame)
    if (menu->parent)
	menu->parent->GetEventHandler()->OnMenuSelect(item->ID);
}
