/*
 * File:	wx_menu.cc
 * Purpose:	Menu implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_itemp.h"
#include "wx_utils.h"
#include "wx_wmgr.h"

#endif

wxNonlockingHashTable *wxMenuItemIDs = NULL;

IMPLEMENT_DYNAMIC_CLASS(wxMenuItem, wxObject)

wxMenuItem::wxMenuItem(void)
{
}

wxMenuItem::~wxMenuItem(void)
{
  if (menuId) {
    wxMenuItemIDs->Delete(menuId);
    menuId = 0;
  }
}

// Menus

// Construct a menu with optional title (then use append)
IMPLEMENT_DYNAMIC_CLASS(wxMenu, wxItem)

wxMenu::wxMenu(char *Title, wxFunction func):wxbMenu(Title, func)
{
  mustBeBreaked = FALSE;
  no_items = 0;
  menu_bar = NULL;
  wxWinType = wxTYPE_HMENU;
  ms_handle = (HANDLE)wxwmCreatePopupMenu();
  save_ms_handle = NULL;
  top_level_menu = this;
  if (title)
  {
    Append(-2,title);
    AppendSeparator();
  }

  Callback(func);
}

// The wxWindow destructor will take care of deleting the submenus.
wxMenu::~wxMenu(void)
{
  if (ms_handle)
	 wxwmDestroyMenu((HMENU)ms_handle);
  ms_handle = NULL;

  wxNode *node = menuItems.First();
  while (node) {
    wxMenuItem *item = (wxMenuItem *)node->Data();
    item->menuBar = NULL;
    
    // Delete child menus.
    // Beware: they must not be appended to children list!!!
    // (because order of delete is significant)
    if (item->subMenu)
      delete item->subMenu;
    item->subMenu = NULL;
    
    wxNode *next = node->Next();
    delete item;
    delete node;
    node = next;
  }
}

void wxMenu::Break(void)
{
  mustBeBreaked = TRUE;
}

// Ordinary menu item
void wxMenu::Append(long Id, char *Label, char *helpString, Bool checkable)
{
  // 'checkable' parameter is useless for Windows.
  wxMenuItem *item = new wxMenuItem;
  item->checkable = checkable;
  item->itemId = Id;
  item->itemName = copystring(Label);
  item->subMenu = NULL;
  if (helpString)
    item->helpString = copystring(helpString);

  WORD menuId;

  if (!wxMenuItemIDs)
    wxMenuItemIDs = new wxNonlockingHashTable;

  do {
    menuId = (WORD)rand();
  } while (wxMenuItemIDs->Get((long)menuId));

  item->menuId = menuId;

  menuItems.Append(item);

  int ms_flags = mustBeBreaked? MF_MENUBREAK : 0;
  mustBeBreaked = FALSE;

  if (ms_handle)
    AppendMenu((HMENU)ms_handle, MF_STRING|ms_flags, menuId, Label);
  else if (save_ms_handle)
    AppendMenu((HMENU)save_ms_handle, MF_STRING|ms_flags, menuId, Label);

  if (Id == -2) {
    int ms_flag = MF_DISABLED;
    if (ms_handle)
      EnableMenuItem((HMENU)ms_handle, no_items, MF_BYPOSITION | ms_flag);
    else if (save_ms_handle) // For Dynamic Menu Append, Thx!!
      EnableMenuItem((HMENU)save_ms_handle, no_items, MF_BYPOSITION | ms_flag);
  }

  no_items++;
}

void wxMenu::AppendSeparator(void)
{
  int ms_flags = mustBeBreaked? MF_MENUBREAK : 0;
  mustBeBreaked = FALSE;

  if (ms_handle)
    AppendMenu((HMENU)ms_handle, MF_SEPARATOR|ms_flags, NULL, NULL);
  else if (save_ms_handle) // For Dynamic Manu Append, Thx!
    AppendMenu((HMENU)save_ms_handle, MF_SEPARATOR|ms_flags, NULL, NULL);

  wxMenuItem *item = new wxMenuItem;
  item->checkable = FALSE;
  item->itemId = -1;
  menuItems.Append(item);
  no_items++;
}

// Pullright item
void wxMenu::Append(long Id, char *Label, wxMenu *SubMenu, char *helpString)
{
  /* MATTHEW: [6] Safety */
  if (!SubMenu->ms_handle)
    return;

  SubMenu->top_level_menu = top_level_menu;

  wxMenuItem *item = new wxMenuItem;
  item->checkable = FALSE;
  item->itemId = Id;
  item->itemName = copystring(Label);
  if (helpString)
    item->helpString = copystring(helpString);
  item->subMenu = SubMenu;

  menuItems.Append(item);

  int ms_flags = mustBeBreaked? MF_MENUBREAK : 0;
  mustBeBreaked = FALSE;

  HMENU menu = (HMENU)(ms_handle ? ms_handle : save_ms_handle);
  HMENU child = (HMENU)SubMenu->ms_handle;
  SubMenu->save_ms_handle = (HANDLE)child;
  SubMenu->ms_handle = NULL;
  AppendMenu(menu, MF_POPUP | MF_STRING | ms_flags, (UINT)child, Label);

  no_items++;
}

/* MATTHEW: [6] Add method; does not delete removed submenus */
Bool wxMenu::DeleteItem(long Id, int Pos)
{
  wxNode *node;
  wxMenuItem *item;
  int pos;
  HMENU menu;

  for (pos = 0, node = menuItems.First(); node && Pos--; node = node->Next(), pos++) {
    item = (wxMenuItem *)node->Data();
    if ((Pos < 0) && (item->itemId == Id))
      break;
  }

  if (!node)
    return FALSE;
  
  item = (wxMenuItem *)node->Data();

  menu = (HMENU)(ms_handle ? ms_handle : save_ms_handle);

  if (item->subMenu) {
    RemoveMenu(menu, (UINT)pos, MF_BYPOSITION);
    item->subMenu->ms_handle = item->subMenu->save_ms_handle;
    item->subMenu->save_ms_handle = NULL;
    item->subMenu->top_level_menu = NULL;
  } else
    DeleteMenu(menu, (UINT)pos, MF_BYPOSITION);
  
  menuItems.DeleteNode(node);
  delete item;

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

void wxMenu::Enable(long Id, Bool Flag)
{
  int ms_flag;
  if (Flag)
    ms_flag = MF_ENABLED;
  else
    ms_flag = MF_GRAYED;
  
  int pos;
  wxMenuItem *item = FindItemForId(Id, NULL, &pos);
  if (item == NULL)
    return;

  HMENU mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;
  EnableMenuItem(mh, pos, MF_BYPOSITION | ms_flag);
}

void wxMenu::Check(long Id, Bool Flag)
{
  int pos;
  wxMenuItem *item = FindItemForId(Id, NULL, &pos);
  if (!item || !item->checkable)
    return;
  int ms_flag;
  HMENU mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;

  if (Flag)
    ms_flag = MF_CHECKED;
  else
    ms_flag = MF_UNCHECKED;

  if (mh)
    CheckMenuItem(mh, pos, MF_BYPOSITION | ms_flag);
}

Bool wxMenu::Checked(long Id)
{
  int Flag = -1;
  HMENU mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;

  int pos;
  wxMenuItem *item = FindItemForId(Id, NULL, &pos);

  if (mh && item)
    Flag = GetMenuState(mh, pos, MF_BYPOSITION);

  if (Flag == -1)
    return FALSE;
  
  if (Flag & MF_CHECKED)
    return TRUE;
  else
    return FALSE;
}

void wxMenu::SetTitle(char *label)
{
  if (title)
    delete[] title;
  if (label)
    title = copystring(label);
  else
    title = copystring(" ");

  HMENU mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;
  if (mh)
    ModifyMenu(mh, 0,
	       MF_BYPOSITION | MF_STRING | MF_DISABLED,
	       -2, title);
}

char *wxMenu::GetTitle()
{
  return(title);
}

void wxMenu::SetLabel(long Id,char *label)
{
  int pos;
  wxMenuItem *item = FindItemForId(Id, NULL, &pos);
  if (item==NULL)
    return;

  item->itemName = copystring(label);

  HMENU mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;
  if (mh) {
    if (!item->subMenu) {
      UINT was_flag = GetMenuState(mh,Id,MF_BYPOSITION);
      ModifyMenu(mh,pos,MF_BYPOSITION|MF_STRING|was_flag,Id,label);
    } else {
      ModifyMenu(mh, pos, MF_BYPOSITION|MF_STRING|MF_POPUP,
		 (UINT)item->subMenu->save_ms_handle, label);
    }
  }
}

char *wxMenu::GetLabel(long Id)
{
  static char tmp[128];
  int len, pos;

  wxMenuItem *item = FindItemForId(Id, NULL, &pos);
  if (!item)
    return NULL;

  HMENU mh = ms_handle ? (HMENU)ms_handle : (HMENU)save_ms_handle;

  if (mh)
    len = GetMenuString(mh,pos,tmp,127,MF_BYPOSITION);
  else
    len = 0;

  tmp[len] = '\0';
  return copystring(tmp);
}

BOOL wxMenu::MSWCommand(UINT WXUNUSED(param), WORD menuId)
{
  /* MATTHEW: auto-check checkable */
  wxMenuItem *item = FindItemForMenuId(menuId);

  if (item) {
    if (item->checkable)
      Check(item->itemId, !Checked(item->itemId));
    
    wxPopupEvent *event = new wxPopupEvent();
    event->menuId = item->itemId;
    ProcessCommand(*event);
    return TRUE;
  } else
    return FALSE;
}

extern wxMenu *wxCurrentPopupMenu;
Bool wxWindow::PopupMenu(wxMenu *menu, float x, float y)
{
  if (wxCurrentPopupMenu)
    return FALSE;

  HWND hWnd = GetHWND();
  HMENU hMenu = (HMENU)menu->ms_handle;
  if (!hMenu) return FALSE; /* MATTHEW: [11] */
  POINT point;
  point.x = (int)x;
  point.y = (int)y;
  ::ClientToScreen(hWnd, &point);
  wxCurrentPopupMenu = menu;
  wxwmTrackPopupMenu(hMenu, 0, point.x, point.y, 0, hWnd, NULL);
  wxYield();
  wxCurrentPopupMenu = NULL;
  return TRUE;
}

wxMenuItem *wxMenu::FindItemForMenuId(WORD menuId)
{
  wxNode *node;
  for (node = menuItems.First(); node; node = node->Next()) {
    wxMenuItem *item = (wxMenuItem *)node->Data();
    
    if (item->menuId == menuId)
      return item;
    
    if (item->subMenu) {
      wxMenuItem *ans = item->subMenu->FindItemForMenuId(menuId);
      if (ans)
	return ans;
    }
  }
  
  return NULL;
}

// Menu Bar
IMPLEMENT_DYNAMIC_CLASS(wxMenuBar, wxItem)

wxMenuBar::wxMenuBar(void)
{
  wxWinType = wxTYPE_HMENU;

  n = 0;
  menus = NULL;
  titles = NULL;
  menu_bar_frame = NULL;
}

wxMenuBar::wxMenuBar(int N, wxMenu *Menus[], char *Titles[]):wxbMenuBar(N, Menus, Titles)
{
  wxWinType = wxTYPE_HMENU;
}

wxMenuBar::~wxMenuBar(void)
{
  int i;

  for (i = 0; i < n; i++)
  {
    delete menus[i];
    delete[] titles[i];
  }
}

void wxMenuBar::Append(wxMenu *menu, char *title)
{
  wxbMenuBar::Append(menu, title);
}

Bool wxMenuBar::OnDelete(wxMenu *a_menu, int pos)
{
  if (!menu_bar_frame)
    return TRUE;
  
  if (RemoveMenu((HMENU)ms_handle, (UINT)pos, MF_BYPOSITION)) {
    menus[pos]->ms_handle = menus[pos]->save_ms_handle;
    menus[pos]->save_ms_handle = NULL;
    
    if (menu_bar_frame) {
      wxWnd *cframe = (wxWnd*)menu_bar_frame->handle;
      HWND hand = (HWND)cframe->handle;
      DrawMenuBar(hand);
    }
    
    return TRUE;
  }
  
  return FALSE;
}

Bool wxMenuBar::OnAppend(wxMenu *a_menu, char *title)
{
  if (!a_menu->ms_handle)
    return FALSE;

  if (!menu_bar_frame)
    return TRUE;

  a_menu->save_ms_handle = a_menu->ms_handle;
  a_menu->ms_handle = NULL;

  AppendMenu((HMENU)ms_handle, MF_POPUP | MF_STRING, (UINT)a_menu->save_ms_handle, title);

  wxWnd *cframe = (wxWnd*)menu_bar_frame->handle;
  HWND hand = (HWND)cframe->handle;
  DrawMenuBar(hand);

  return TRUE;
}

void wxMenuBar::Enable(long Id, Bool Flag)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item = FindItemForId(Id, &itemMenu);
  if (!item)
    return;

  itemMenu->Enable(Id, Flag);
}

void wxMenuBar::EnableTop(int pos,Bool flag)
{
  if (!menu_bar_frame)
    return;

  int ms_flag;
  if (flag)
    ms_flag = MF_ENABLED;
  else
    ms_flag = MF_GRAYED;
  
  EnableMenuItem((HMENU)ms_handle, pos, MF_BYPOSITION | ms_flag);
  wxWnd *cframe = (wxWnd*)menu_bar_frame->handle;
  HWND hand = (HWND)cframe->handle;
  DrawMenuBar(hand);
}

void wxMenuBar::Check(long Id, Bool Flag)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item = FindItemForId(Id, &itemMenu);
  if (!item)
   return;

  itemMenu->Check(Id, Flag);
}

Bool wxMenuBar::Checked(long Id)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item = FindItemForId(Id, &itemMenu);
  if (!item)
    return FALSE;

  return itemMenu->Checked(Id);
}

void wxMenuBar::SetLabel(long Id,char *label)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item = FindItemForId(Id, &itemMenu);

  if (!item)
    return;

  itemMenu->SetLabel(Id, label);
}

char *wxMenuBar::GetLabel(long Id)
{
  wxMenu *itemMenu = NULL;
  wxMenuItem *item = FindItemForId(Id, &itemMenu);

  if (!item)
    return NULL;

  return itemMenu->GetLabel(Id);
}

void wxMenuBar::SetLabelTop(int pos,char *label)
{
  if (pos < 0 || pos >= n)
    return;

  titles[pos] = copystring(label);
  
  UINT was_flag = GetMenuState((HMENU)ms_handle,pos,MF_BYPOSITION);
  if (was_flag & MF_POPUP) {
    was_flag &= 0xff;
    HMENU popup = GetSubMenu((HMENU)ms_handle,pos);
    ModifyMenu((HMENU)ms_handle,pos,MF_BYPOSITION|MF_STRING|was_flag,(UINT)popup,label);
  } else
    ModifyMenu((HMENU)ms_handle,pos,MF_BYPOSITION|MF_STRING|was_flag,pos,label);

  if (menu_bar_frame) {
    wxWnd *cframe = (wxWnd*)menu_bar_frame->handle;
    HWND hand = (HWND)cframe->handle;
    DrawMenuBar(hand);
  }
}

char *wxMenuBar::GetLabelTop(int pos)
{
  static char tmp[128];
  int len = GetMenuString((HMENU)ms_handle,pos,tmp,127,MF_BYPOSITION);

  if (!len)
    return 0L;
  tmp[len] = '\0';
  char *p = copystring(tmp);
  return(p);
}

void wxFrame::SetMenuBar(wxMenuBar *menu_bar)
{
  int i;
  HMENU menu = wxwmCreateMenu();

  if (menu_bar->menu_bar_frame)
    return;

  for (i = 0; i < menu_bar->n; i++) {
    HMENU popup = (HMENU)menu_bar->menus[i]->ms_handle;
    menu_bar->menus[i]->save_ms_handle = (HANDLE)popup;
    menu_bar->menus[i]->ms_handle = NULL;
    AppendMenu(menu, MF_POPUP | MF_STRING, (UINT)popup, menu_bar->titles[i]);
  }

  menu_bar->ms_handle = (HANDLE)menu;
  if (wx_menu_bar)
    delete wx_menu_bar;

  wxWnd *cframe = (wxWnd *)handle;
  cframe->hMenu = menu;

  switch (frame_type)
  {
    case wxMDI_PARENT:
	 {
      wxMDIFrame *mdi_frame = (wxMDIFrame *)cframe;
      HMENU subMenu = GetSubMenu(mdi_frame->window_menu, 0);

		// Try to insert Window menu in front of Help, otherwise append it.
      int N = GetMenuItemCount(menu);
      Bool success = FALSE;
      for (i = 0; i < N; i++)
      {
        char buf[100];
        int chars = GetMenuString(menu, i, buf, 100, MF_BYPOSITION);
		  if ((chars > 0) && (strcmp(buf, "&Help") == 0 ||
                            strcmp(buf, "Help") == 0))
        {
           success = TRUE;
           InsertMenu(menu, i, MF_BYPOSITION | MF_POPUP | MF_STRING,
                      (UINT)subMenu, "&Window");
           break;
        }
      }
      if (!success)
        AppendMenu(menu, MF_POPUP,
								 (UINT)subMenu,
                         "&Window");
		mdi_frame->parent_frame_active = TRUE;
#ifdef WIN32
      SendMessage(mdi_frame->client_hwnd, WM_MDISETMENU,
                  (WPARAM)menu,
						(LPARAM)subMenu);
#else
      SendMessage(mdi_frame->client_hwnd, WM_MDISETMENU, 0,
                  MAKELPARAM(menu, subMenu));
#endif
      DrawMenuBar(mdi_frame->handle);
      break;
	 }
    case wxMDI_CHILD:
    {
      wxMDIFrame *parent = (wxMDIFrame *)GetParent()->handle;
      parent->parent_frame_active = FALSE;
      HMENU subMenu = GetSubMenu(parent->window_menu, 0);

      // Try to insert Window menu in front of Help, otherwise append it.
      int N = GetMenuItemCount(menu);
      Bool success = FALSE;
      for (i = 0; i < N; i++)
		{
        char buf[100];
		  int chars = GetMenuString(menu, i, buf, 100, MF_BYPOSITION);
        if ((chars > 0) && (strcmp(buf, "&Help") == 0 ||
                            strcmp(buf, "Help") == 0))
        {
			  success = TRUE;
           InsertMenu(menu, i, MF_BYPOSITION | MF_POPUP | MF_STRING,
                      (UINT)subMenu, "&Window");
           break;
        }
      }
      if (!success)
		  AppendMenu(menu, MF_POPUP,
                         (UINT)subMenu,
                         "&Window");
#ifdef WIN32
      SendMessage(parent->client_hwnd, WM_MDISETMENU,
                  (WPARAM)menu,
                  (LPARAM)subMenu);
#else
      SendMessage(parent->client_hwnd, WM_MDISETMENU, 0,
                  MAKELPARAM(menu, subMenu));
#endif

      DrawMenuBar(parent->handle);
		break;
    }
    default:
    case wxSDI:
	 {
      SetMenu(cframe->handle, menu);
      break;
    }
  }
  wx_menu_bar = menu_bar;
  menu_bar->menu_bar_frame = this;
}

wxMenuItem *wxMenuBar::FindItemForMenuId(WORD menuId)
{
  int i;
  wxMenuItem *item;
  for (i = 0; i < n; i++)
    if ((item = menus[i]->FindItemForMenuId(menuId)))
      return item;
  return NULL;
}
