/*
 * File:        wx_menu.cc
 * Purpose:     Menu and menu bar implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_menu.h"
#pragma implementation "wx_mnuit.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wx_privt.h"
#include "wx_menu.h"

#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>

// Convenience dialogs, only needed for FakePopupMenu
#include "wx_dialg.h"

void wxMenuItemCallback (Widget w, XtPointer clientData,
			 XtPointer ptr);
void wxMenuItemArmCallback (Widget w, XtPointer clientData,
			    XtPointer ptr);
void wxMenuItemDisarmCallback (Widget w, XtPointer clientData,
			       XtPointer ptr);

// Menus

// Construct a menu with optional title (then use append)
IMPLEMENT_DYNAMIC_CLASS(wxMenu, wxItem)
IMPLEMENT_DYNAMIC_CLASS(wxMenuItem, wxObject)

wxMenu::wxMenu (char *Title, wxFunction func): wxbMenu (Title, func)
{
  no_items = 0;
  menu_bar = NULL;
  menuItems.DeleteContents (TRUE);
  numColumns = 1;
  buttonWidget = NULL;
  handle = NULL;
  top_menu = NULL;
  WXGC_IGNORE(top_menu);
  menuId = 0;

  if (title)
    {
      Append (-2, title);
      AppendSeparator ();
    }
  Callback (func);
}

// The wxWindow destructor will take care of deleting the submenus.
wxMenu::~wxMenu (void)
{
  if (handle) {
    if (!window_parent)
      DestroyMenu(TRUE);
    else
      DestroyMenu(FALSE);
    handle = NULL;
  }

  DestroyChildren();

  if (window_parent && wxSubType(window_parent->__type, wxTYPE_MENU_BAR))
    window_parent = NULL;
}

void wxMenu:: Break (void)
{
  numColumns++;
}

// Ordinary menu item
void wxMenu:: Append (long Id, char *Label, char *helpString, Bool checkable)
{
  wxMenuItem *item = new wxMenuItem;
  item->checkable = checkable;
  item->itemId = Id;
  item->itemName = copystring (Label);
  item->subMenu = NULL;
  if (helpString)
    item->helpString = copystring (helpString);

  menuItems.Append (item);

  item->buttonWidget = NULL;
  if (handle)
    item->CreateItem ((Widget) handle, menu_bar, top_menu);	// this is a dynamic Append
  no_items++;
}

void wxMenu:: AppendSeparator (void)
{
  wxMenuItem *item = new wxMenuItem;
  item->itemId = -1;
  item->checkable = FALSE;
  menuItems.Append (item);

  item->buttonWidget = NULL;
  if (handle)
    item->CreateItem ((Widget) handle, menu_bar, top_menu);	// this is a dynamic Append
}

// Pullright item
void wxMenu:: Append (long Id, char *Label, wxMenu * SubMenu, char *helpString)
{
  /* MATTHEW: [6] Safety */
  if (SubMenu->window_parent)
    return;

  SubMenu->top_level_menu = top_level_menu;
  SubMenu->window_parent = this;
  children->Append (SubMenu);	// Store submenu for later deletion

  wxMenuItem *item = new wxMenuItem;
  item->checkable = FALSE;
  item->itemId = Id;
  item->itemName = copystring (Label);
  if (helpString)
    item->helpString = copystring (helpString);

  item->subMenu = SubMenu;
  menuItems.Append (item);

  item->buttonWidget = NULL;
  if (handle)
    item->CreateItem ((Widget) handle, menu_bar, top_menu);	// this is a dynamic Append

  no_items++;
}

/* MATTHEW: [6] New method */
Bool wxMenu::DeleteItem(long Id, int  Pos)
{
  wxMenuItem *item = NULL;
  wxNode *node;
  int pos;

  for (pos = 0, node = menuItems.First(); node && Pos--; node = node->Next(), pos++) {
    item = (wxMenuItem *)node->Data();
    if ((Pos < 0) && (item->itemId == Id))
      break;
  }

  if (!node)
    return FALSE;

  item->DestroyItem(TRUE);

  if (item->subMenu) {
    item->subMenu->top_level_menu = item->subMenu;
    item->subMenu->window_parent = NULL;
    children->DeleteObject(item->subMenu);
  }

  menuItems.DeleteObject(item);

  --no_items;

  return TRUE;
}

Bool wxMenu::Delete(long Id)
{
  return DeleteItem(Id, -1);
}

Bool wxMenu::DeleteByPosition(int pos)
{
  return DeleteItem(0, pos);
}

int wxMenu::Number()
{
  return no_items;
}

void wxMenu:: Enable (long Id, Bool Flag)
{
  wxMenuItem *it = NULL;
  Widget w = FindMenuItem (Id, &it);

  if (it)
    it->isEnabled = Flag;

  if (w) {
    if (w)
      XtSetSensitive (w, Flag);
    return;
  }
}

void wxMenu:: Check (long Id, Bool Flag)
{
  wxMenuItem *it = NULL;
  Widget w = FindMenuItem (Id, &it);
  if (it) 
    it->isChecked = Flag;

  if (w && XtIsSubclass (w, xmToggleButtonGadgetClass)) {
    XtVaSetValues (w, XmNset, (Boolean) Flag, NULL);
  }
}

Bool wxMenu:: Checked (long Id)
{
  wxMenuItem *it;
  Widget w = FindMenuItem(Id, &it);
  if (it) {
    if (w) {
      Boolean Flag;
      XtVaGetValues(w, XmNset, &Flag, NULL);

      return Flag;
    }

    return it->isChecked;
  } else
    return FALSE;
}

void wxMenu:: SetTitle (char *label)
{
  if (title)
    delete title;
  if (label)
    title = copystring (label);
  else
    title = copystring (" ");

  wxNode *node = menuItems.First ();
  if (!node)
    return;
  wxMenuItem *item = (wxMenuItem *) node->Data ();
  Widget widget = item->buttonWidget;
  if (!widget)
    return;

  XmString title_str = XmStringCreateSimple (title);
  XtVaSetValues (widget,
                 XmNlabelString, title_str,
                 NULL);
}

char *wxMenu:: GetTitle ()
{
  return title;
}

void wxMenu:: SetLabel (long Id, char *label)
{
  char mnem = wxFindMnemonic (label);
  wxStripMenuCodes (label, wxBuffer);
  wxMenuItem *it = NULL;
  Widget w = FindMenuItem (Id, &it);
  if (it)
    it->itemName = copystring(label);
  if (w)
    {
      XmString label_str = XmStringCreateSimple (wxBuffer);
      XtVaSetValues (w,
                     XmNlabelString, label_str,
                     NULL);
      XmStringFree (label_str);
      if (mnem != 0)
        XtVaSetValues (w, XmNmnemonic, mnem, NULL);

      XmString accel_str = wxFindAcceleratorText (label);
      if (accel_str)
        {
          XtVaSetValues (w, XmNacceleratorText, accel_str, NULL);
          XmStringFree (accel_str);
        }
    }
}

char *wxMenu:: GetLabel (long Id)
{
  wxMenuItem *it = NULL;
  FindMenuItem (Id, &it);
  if (it) {
    wxStripMenuCodes (it->itemName, wxBuffer);
    return copystring(wxBuffer);
  }
  return NULL;
}

Widget wxMenu:: FindMenuItem (long Id, wxMenuItem ** it)
{
  if (it)
    *it = NULL;

  if (Id == menuId)
    {
      return buttonWidget;
    }

  for (wxNode * node = menuItems.First (); node; node = node->Next ())
    {
      wxMenuItem *item = (wxMenuItem *) node->Data ();
      if (item->itemId == Id)
	{
	  if (it)
	    *it = item;
	  return item->buttonWidget;
	}

      if (item->subMenu)
	{
	  Widget w = item->subMenu->FindMenuItem (Id, it);
	  if (w || (it && *it))
	    {
	      return w;
	    }
	}
    }				// for()

  if (it)
    *it = NULL;
  return NULL;
}


// Menu Bar

IMPLEMENT_DYNAMIC_CLASS(wxMenuBar, wxItem)

wxMenuBar::wxMenuBar (void)
{
}

wxMenuBar::wxMenuBar (int N, wxMenu * Menus[], char *Titles[]):
  wxbMenuBar (N, Menus, Titles)
{
}

wxMenuBar::~wxMenuBar (void)
{
  int i;
  for (i = 0; i < n; i++)
    {
      delete menus[i];
      delete[]titles[i];
    }
  delete[]menus;
  delete[]titles;
}

void wxMenuBar::Enable (long Id, Bool Flag)
{
  int j;
  for (j = 0; j < n; j++)
    {
      if (menus[j]->FindMenuItem(Id)) {
	menus[j]->Enable(Id, Flag);
	return;
      }
    }
}

void wxMenuBar::Check (long Id, Bool Flag)
{
  int j;
  for (j = 0; j < n; j++)
    {
      if (menus[j]->FindMenuItem(Id)) {
	menus[j]->Check(Id, Flag);
	return;
      }
    }
}

Bool wxMenuBar::Checked (long Id)
{
  int j;
  for (j = 0; j < n; j++)
    {
      if (menus[j]->FindMenuItem(Id))
	return menus[j]->Checked(Id);
    }
  return FALSE;
}

void wxMenuBar::SetLabel (long Id, char *label)
{
  int j;
  for (j = 0; j < n; j++)
    {
      if (menus[j]->FindMenuItem(Id)) {
	menus[j]->SetLabel(Id, label);
	return;
      }
    }
}

char *wxMenuBar::GetLabel (long Id)
{
  int j;
  for (j = 0; j < n; j++) {
    char *s = menus[j]->GetLabel(Id);
    if (s)
      return s;
  }
  return NULL;
}

void wxMenuBar::SetLabelTop (int pos, char *label)
{
  /* MATTHEW: [13] */
  if ((pos < 0) || (pos >= n))
    return;

  titles[pos] = copystring(label);

  Widget w = menus[pos]->buttonWidget;
  if (w)
    {
      XmString label_str = XmStringCreateSimple (label);
      XtVaSetValues (w,
		     XmNlabelString, label_str,
		     NULL);
      XmStringFree (label_str);
      return;
    }
}

char *wxMenuBar::GetLabelTop (int pos)
{
  /* MATTHEW: [13] */
  if ((pos < 0) || (pos >= n))
    return NULL;

  Widget w = menus[pos]->buttonWidget;
  if (w)
    {
      XmString text;
      char *s;
      XtVaGetValues (w,
		     XmNlabelString, &text,
		     NULL);

      if (XmStringGetLtoR (text, XmSTRING_DEFAULT_CHARSET, &s))
	{
	  strcpy (wxBuffer, s);
	  XtFree (s);
	  return wxBuffer;
	}
      else
	{
	  return NULL;
	}
    }
  else
    return (NULL);
}

void wxMenuBar::EnableTop (int pos, Bool flag)
{
  /* MATTHEW: [9] */
  if (pos < 0 || (pos >= n))
    return;

  Widget w = menus[pos]->buttonWidget;
  if (w)
    {
      XtSetSensitive (w, flag);
      return;
    }
}

/* MATTHEW: [6] */
Bool wxMenuBar::OnAppend(wxMenu *menu, char *title)
{
  if (!menu_bar_frame)
    return TRUE;
  if (menu->GetParent())
    return FALSE;

  if (menu->buttonWidget)
    return FALSE;

  menu->buttonWidget = menu->CreateMenu(this, (Widget)handle, menu, 
					title, TRUE);

  return TRUE;
}

Bool wxMenuBar::OnDelete(wxMenu *menu, int position)
{
  if (!menu_bar_frame)
    return TRUE;

  menu->DestroyMenu(TRUE);

  return TRUE;
}

static void KillPopup(void *m)
{
  wxMenu *menu = (wxMenu *)m;

  if (menu->GetParent()) {
    wxChildList *list = menu->GetParent()->GetChildren();

    list->DeleteObject(menu);

    menu->SetParent(NULL);
    menu->DestroyMenu(TRUE);

    menu->menuId = 0;
  }
}
extern void wxRegsiterIdleCallback(void (*f)(void *), void *data, wxWindow *w);

/* MATTHEW: [11] */
void 
wxMenuPopdownCallback(Widget w, XtPointer clientData,
		      XtPointer ptr)
{
  wxMenu *menu = (wxMenu *)clientData;

  /* Mark as no longer popped up */
  menu->menuId = -1;

  if (menu->GetParent())
    wxRegsiterIdleCallback(KillPopup, menu, menu->GetParent());
}

void 
wxMenuItemCallback (Widget w, XtPointer clientData,
		    XtPointer ptr)
{
  wxMenuItem *item = (wxMenuItem *) clientData;
  if (item)
    {
      if (item->menuBar && item->menuBar->menu_bar_frame)
	{
//       cout << "Id = " << item->itemId << "\n";
	  item->menuBar->menu_bar_frame->GetEventHandler()->OnMenuCommand (item->itemId);
	}
      else if (item->topMenu)
	{
	  wxPopupEvent *event = new wxPopupEvent();
	  event->menuId = item->itemId;
	  item->topMenu->ProcessCommand (*event);
	}
    }
}

void 
wxMenuItemArmCallback (Widget w, XtPointer clientData,
		       XtPointer ptr)
{
  wxMenuItem *item = (wxMenuItem *) clientData;
  if (item)
    {
      if (item->menuBar && item->menuBar->menu_bar_frame)
	{
	  item->menuBar->menu_bar_frame->GetEventHandler()->OnMenuSelect (item->itemId);
	}
    }
}

void 
wxMenuItemDisarmCallback (Widget w, XtPointer clientData,
			  XtPointer ptr)
{
  wxMenuItem *item = (wxMenuItem *) clientData;
  if (item)
    {
      if (item->menuBar && item->menuBar->menu_bar_frame)
	{
	  item->menuBar->menu_bar_frame->GetEventHandler()->OnMenuSelect (-1);
	}
    }
}

/*
 * Create a popup or pulldown menu.
 * Submenus of a popup will be pulldown.
 *
 */

Widget wxMenu::CreateMenu (wxMenuBar * menuBar, Widget parent, wxMenu * topMenu, char *Title, Bool pullDown)
{
  Widget menu;
  Widget ButtonWidget = 0;
  Arg args[5];
  XtSetArg (args[0], XmNnumColumns, numColumns);
  XtSetArg (args[1], XmNpacking, XmPACK_COLUMN);

  if (!pullDown)
    {
      menu = XmCreatePopupMenu (parent, "popup", args, 2);

      /* MATTHEW: [11] */
      XtAddCallback(menu,
		    XmNunmapCallback, 
		    (XtCallbackProc)wxMenuPopdownCallback,
		    (XtPointer)this);
    }
  else
    {
      char mnem = wxFindMnemonic (Title);
      wxStripMenuCodes (Title, wxBuffer);

      menu = XmCreatePulldownMenu (parent, "pulldown", args, 2);

      XmString label_str = XmStringCreateSimple (wxBuffer);
      ButtonWidget = XtVaCreateManagedWidget (wxBuffer,
#if USE_GADGETS
					 xmCascadeButtonGadgetClass, parent,
#else
					 xmCascadeButtonWidgetClass, parent,
#endif
					      XmNlabelString, label_str,
					      XmNsubMenuId, menu,
					      NULL);

      if (mnem != 0)
	XtVaSetValues (ButtonWidget, XmNmnemonic, mnem, NULL);

      XmStringFree (label_str);
    }

  // XtVaSetValues(menu, XmNnumColumns, numColumns, NULL) ;
  handle = (char *) menu;

  menu_bar = menuBar;
  top_menu = topMenu;

  for (wxNode * node = menuItems.First (); node; node = node->Next ())
    {
      wxMenuItem *item = (wxMenuItem *) node->Data ();
      item->CreateItem (menu, menu_bar, top_menu);
    }

  return ButtonWidget;
}

// Destroys the Motif implementation of the menu,
// but maintains the wxWindows data structures so we can
// do a CreateMenu again. 
void wxMenu::DestroyMenu(Bool full)
{
  for (wxNode * node = menuItems.First (); node; node = node->Next ())
    {
      wxMenuItem *item = (wxMenuItem *) node->Data ();
      item->menuBar = NULL;

      /* MATTHEW: [13] */
      item->DestroyItem(full);
    }				// for()

  if (buttonWidget)
    {
      /* MATTHEW: [13] */
      if (full) {
	XtVaSetValues(buttonWidget, XmNsubMenuId, NULL, NULL);
	XtDestroyWidget (buttonWidget);
	buttonWidget = 0;
      }
    }

  /* MATTHEW: [13] */
  if (handle && full)
    {
      XtDestroyWidget((Widget)handle);
      handle = NULL;
    }
}

void wxMenuItem::CreateItem (Widget menu, wxMenuBar * menu_bar, wxMenu * top_menu)
{
  menuBar = menu_bar;
  topMenu = top_menu;

  if (itemId == -2)
    {
      // Id=-2 identifies a Title item.
      wxStripMenuCodes (itemName, wxBuffer);
      buttonWidget = XtVaCreateManagedWidget (wxBuffer,
					    xmLabelGadgetClass, menu, NULL);
    }
  else if (itemName && (!subMenu))
    {
      wxStripMenuCodes (itemName, wxBuffer);
      if (checkable)
	{
	  buttonWidget = XtVaCreateManagedWidget (wxBuffer,
					    xmToggleButtonGadgetClass, menu,
						  NULL);
	  XtVaSetValues (buttonWidget, XmNset, (Boolean) isChecked, NULL);
	}
      else
	buttonWidget = XtVaCreateManagedWidget (wxBuffer,
					      xmPushButtonGadgetClass, menu,
						NULL);
      char mnem = wxFindMnemonic (itemName);
      if (mnem != 0)
	XtVaSetValues (buttonWidget, XmNmnemonic, mnem, NULL);

      /* MATTHEW: [15] Skip */
#if 0
      char *accel = wxFindAccelerator (itemName);
      if (accel)
	XtVaSetValues (buttonWidget, XmNaccelerator, accel, NULL);
#endif

      XmString accel_str = wxFindAcceleratorText (itemName);
      if (accel_str)
	{
	  XtVaSetValues (buttonWidget, XmNacceleratorText, accel_str, NULL);
	  XmStringFree (accel_str);
	}

      if (checkable)
	XtAddCallback (buttonWidget,
		       XmNvalueChangedCallback,
		       (XtCallbackProc) wxMenuItemCallback,
		       (XtPointer) this);
      else
	XtAddCallback (buttonWidget,
		       XmNactivateCallback,
		       (XtCallbackProc) wxMenuItemCallback,
		       (XtPointer) this);
      XtAddCallback (buttonWidget,
		     XmNarmCallback,
		     (XtCallbackProc) wxMenuItemArmCallback,
		     (XtPointer) this);
      XtAddCallback (buttonWidget,
		     XmNdisarmCallback,
		     (XtCallbackProc) wxMenuItemDisarmCallback,
		     (XtPointer) this);
    }
  else if (itemId == -1)
    {
      buttonWidget = XtVaCreateManagedWidget ("separator",
					xmSeparatorGadgetClass, menu, NULL);
    }
  else if (subMenu)
    {
      buttonWidget = subMenu->CreateMenu (menu_bar, menu, topMenu, itemName, TRUE);
      /* MATTHEW: [11] */
      subMenu->buttonWidget = buttonWidget;
      XtAddCallback (buttonWidget,
		     XmNcascadingCallback,
		     (XtCallbackProc) wxMenuItemArmCallback,
		     (XtPointer) this);
//      XtAddCallback(buttonWidget,
      //                    XmNdisarmCallback,
      //                    (XtCallbackProc)wxMenuItemDisarmCallback,
      //                    (XtPointer)this);
    }
  if (buttonWidget)
    XtSetSensitive (buttonWidget, isEnabled);
}

/* MATTHEW: [13] */
void wxMenuItem::DestroyItem(Bool full)
{
  if (itemId == -2)
    {
      ;			// Nothing
      
    }
  else if (itemName && !subMenu)
    {
      if (buttonWidget)
	{
	  if (checkable) {
	    Boolean Flag;
	    XtVaGetValues(buttonWidget, XmNset, &Flag, NULL);
	    isChecked = Flag;

	    XtRemoveCallback (buttonWidget, XmNvalueChangedCallback,
			      wxMenuItemCallback, (XtPointer) this);
	  } else
	    XtRemoveCallback (buttonWidget, XmNactivateCallback,
			      wxMenuItemCallback, (XtPointer) this);
	  XtRemoveCallback (buttonWidget, XmNarmCallback,
			    wxMenuItemArmCallback, (XtPointer) this);
	  XtRemoveCallback (buttonWidget, XmNdisarmCallback,
			    wxMenuItemDisarmCallback, (XtPointer) this);
	}
    }
  else if (itemId == -1)
    {
      ;			// Nothing
      
    }
  else if (subMenu)
    {
      if (buttonWidget)
	{
	  XtRemoveCallback (buttonWidget, XmNcascadingCallback,
			    wxMenuItemArmCallback, (XtPointer) this);
	  //XtRemoveCallback(buttonWidget, XmNdisarmCallback,
	  //                 wxMenuItemDisarmCallback, (XtPointer) this);
	}

      /* MATTHEW: [11] */
      subMenu->DestroyMenu(full);
      if (full)
	buttonWidget = NULL;
    }

  if (buttonWidget && full)
    {
      XtDestroyWidget (buttonWidget);
      buttonWidget = 0;
    }
}


