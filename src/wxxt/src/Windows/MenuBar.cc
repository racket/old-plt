/*								-*- C++ -*-
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
      for (int i=0; i<n; ++i) {
	Append(menus[i], titles[i]);
      }
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
	  wxMenu *mnu;
	  DELETE_VAL temp->label;
	  mnu = ((wxMenu*)(temp->user_data));
	  DELETE_OBJ mnu; 
	}
	DELETE_VAL temp;
    }
}

//-----------------------------------------------------------------------------
// create and destroy menubar
//-----------------------------------------------------------------------------

Bool wxMenuBar::Create(wxPanel *panel)
{
    int ph, pw;
    Dimension hh, ww;
    wxWindow_Xintern *parenth;
    Widget wgt;

    ChainToPanel(panel, 0, "menubar");

    parenth = panel->GetHandle();

    // create widgets
    wgt = XtVaCreateManagedWidget
	("menubar", xfwfEnforcerWidgetClass, parenth->handle,
	 XtNtraversalOn, FALSE, XtNhighlightThickness, 0,
	 NULL);
    X->frame = wgt;
    wgt = XtVaCreateWidget
	("menubar", menuWidgetClass, X->frame,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  fg->GetPixel(cmap),
	 XtNhMargin,     4,
	 XtNfont,        font->GetInternalFont(),
	 XtNmenu,        top,
	 XtNcursor,      None,
	 NULL);
    X->handle = wgt;
    // callbacks
    XtAddCallback(X->handle, XtNonSelect,  wxMenuBar::CommandEventCallback, this);
    XtAddCallback(X->handle, XtNonNewItem, wxMenuBar::SelectEventCallback, this);

    // Panel width needed
    panel->GetSize(&pw, &ph);

    // position menubar
    XtVaGetValues(X->handle, XtNheight, &hh, XtNwidth, &ww, NULL);
    ww = pw;
    XtVaSetValues(X->frame,  XtNheight,  hh, XtNwidth,  ww, NULL);
    wxLC_MEM(constraints->top, Absolute(-hh));
    wxLC_MEM(constraints->left, Absolute(0));
    wxLC_MEM(constraints->width, SameAs(panel, wxWidth, 0));
    wxLC_MEM(constraints->height, Absolute(hh));

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
    menu_item *item = 0;

    if (!menu || !title) // I need menu and title
	return;

    /* MATTHEW: enforce safety */
    if (menu->owner)
      return;

    Stop();

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
    {
      _e_menu_item_type t;
      t = (!strcmp(item->label, "Help")) ? MENU_HELP : MENU_CASCADE;
      item->type    = t;
    }
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
    if (X->handle) { // redisplay if menu added
      XtVaSetValues(X->handle, XtNmenu, top, XtNrefresh, True, NULL);
    }
}

/* MATTHEW: */
Bool wxMenuBar::Delete(wxMenu *menu, int pos)
{
  menu_item *i;
  int counter;

  if (!menu && (pos < 0))
    return FALSE;

  for (i = (menu_item *)top, counter = 0; 
       i && ((menu && (i->user_data != (void *)menu))
	     || (!menu && (counter < pos)));
       counter++) {
    i = i->next;
  }

  if (i) {
    if (i == (menu_item *)top)
      top = (wxMenuItem *)i->next;
    if (i == (menu_item *)last)
      last = (wxMenuItem *)i->prev;
    if (i->prev)
      i->prev->next = i->next;
    if (i->next)
      i->next->prev = i->prev;
    
    if (!top) {
      Append(NULL, NULL); // to have something if associated to frame
      topdummy = top;
    }

    if (i->contents) {
      DELETE_VAL i->label;
      /* Release menu: */
      ((wxMenu *)(i->user_data))->owner = NULL;
    }

    DELETE_VAL i;

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

  for (i = (menu_item *)top; i; i = i->next) {
    counter++;
  }

  if (counter && topdummy)
    --counter;

  return counter;
}

//-----------------------------------------------------------------------------
// modify items
//-----------------------------------------------------------------------------

void wxMenuBar::Check(long id, Bool flag)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
	found->set = flag;
}

Bool wxMenuBar::Checked(long id)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
	return found->set;
    return FALSE;
}

void wxMenuBar::Enable(long id, Bool flag)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
	found->enabled = flag;
}

void wxMenuBar::EnableTop(int pos, Bool flag)
{
    menu_item *item = (menu_item*)top;
    int i;

    for (i=0; item && i<pos; ++i) {
	item = item->next;
    }
    if (item) {
      Stop();
      if (X->handle) {
	item->enabled = flag;
	XtVaSetValues(X->handle, XtNmenu, top, XtNrefresh, True, NULL);
      }
    }
}

char *wxMenuBar::GetHelpString(long id)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
	return found->help_text;
    return NULL;
}

char *wxMenuBar::GetLabel(long id)
{
    menu_item *found;
    found = (menu_item*)FindItemForId(id);
    if (found)
	return found->label;
    return NULL;
}

char *wxMenuBar::GetLabelTop(int pos)
{
    menu_item *item = (menu_item*)top;

    for (int i=0; item && i<pos; ++i) {
	item = item->next;
    }
    if (item)
	return item->label;
    return NULL;
}

void wxMenuBar::SetHelpString(long id, char *help)
{
  menu_item *found;
  found = (menu_item*)FindItemForId(id);
  if (found)
    found->help_text = help;
}

void wxMenuBar::SetLabel(long id, char *label)
{
  menu_item *found;
  found = (menu_item*)FindItemForId(id);
  if (found) {
    DELETE_VAL found->label;
    wxGetLabelAndKey(label, &found->label, &found->key_binding);
  }
}

void wxMenuBar::SetLabelTop(int pos, char *label)
{
    menu_item *item = (menu_item*)top;
    int i;

    for (i=0; item && i<pos; ++i) {
      item = item->next;
    }
    if (item) {
        Stop();
	DELETE_VAL item->label;
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
    menu_item *item;

    wxGetLabelAndKey(menu, &label, &key);

    for (item = (menu_item*)top; item; item=item->next) {
      if (!strcmp(item->label, label) && item->contents) {
	answer = ((wxMenu*)item->user_data)->FindItem(itemstring);
	break;
      }
    }
    DELETE_VAL label;
    return answer;
}

wxMenuItem *wxMenuBar::FindItemForId(long id, wxMenu **req_menu)
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

    if (item->ID != -1) {
      if (item->type == MENU_TOGGLE)
	item->set = (!item->set);
      
      // call OnMenuCommandt of parent (usually of a frame)
      if (menu->parent)
	menu->parent->OnMenuCommand(item->ID);
    }
}

void wxMenuBar::SelectEventCallback(Widget WXUNUSED(w),
				    XtPointer dclient, XtPointer dcall)
{
    wxMenuBar *menu  = (wxMenuBar*)dclient;
    menu_item *item  = (menu_item*)dcall;

    // call OnMenuSelect of parent (usually of a frame)
    if (menu->parent)
	menu->parent->OnMenuSelect(item->ID);
}

void wxMenuBar::Stop(void)
{
  XtCallActionProc(X->handle, "select", NULL, NULL, 0);
}

void wxMenuBar::SelectAMenu()
{
  XEvent xevent;
  Position x, y, new_root_x, new_root_y;

  Stop();

  /* Get the menu started: */
  XtVaGetValues(X->handle, XtNx, &x, XtNy, &y, NULL);
  XtTranslateCoords(X->handle, x, y, &new_root_x, &new_root_y);
  
  xevent.xmotion.x_root = new_root_x + 5;
  xevent.xmotion.x = 5;
  xevent.xmotion.y_root = new_root_y + 5;
  xevent.xmotion.y = 5;
  
  XtCallActionProc(X->handle, "start", &xevent, NULL, 0);
}
