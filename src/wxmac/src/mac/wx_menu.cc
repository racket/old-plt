///////////////////////////////////////////////////////////////////////////////
// File:	wx_menu.cc (split from wx_item.cc)
// Purpose:	Menu objects implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

//#include <assert.h>

#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_main.h"
#include <Strings.h>	

int wxNumHelpItems;
MenuHandle wxHelpMenu;

void wxSetUpAppleMenu(wxMenuBar *mbar);

///////////////////////////////////////////////////////////////////////////////
// Menu Bar
///////////////////////////////////////////////////////////////////////////////


// mflatt: In a wxMenu object:
//          menu_bar is now only used by top-level menus in the menu bar
//          top_level_menu is not set or used at all
//          window_parent is set to the menu_bar for a top-level menu, or the
//           parent menu for a submenu
//         menuBar in a wxMenuItem is not used ever
//
//    When a menu bar is deleted, it deletes all menus it conatins
//    When a menu is deleted, it deletes all submenus it contains
//    The children list of a manu/menubar is not used
//
//    When inserting a test item into a menu, Special mac characters are ignored.
//
//    NOTE: A great deal of confusion is based on the fact that wxMenu and
//          wxMenuBar are derived from wxItem. They are not windows or items
//          and should not be derived from wxItem. Perhaps we can get this fixed...

//=============================================================================
// Public constructors
//=============================================================================

wxMenuBar::wxMenuBar // Constructor (given objectType)
	(
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMenuBar (windowName),
		wxHelpHackMenu(NULL),
		iHelpMenuHackNum(0)
{
	WXGC_IGNORE(menu_bar_frame);
}

wxMenuBar::wxMenuBar // Constructor (given parentPanel, label)
	(
		int 		N,
		wxMenu* 	Menus[],
		char* 		Titles[],
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMenuBar (N, Menus, Titles, windowName),
		wxHelpHackMenu(NULL),
		iHelpMenuHackNum(0)
{
	WXGC_IGNORE(menu_bar_frame);
}

//=============================================================================
// Public destructor
//=============================================================================

static wxMenuBar *last_installed_bar;

//-----------------------------------------------------------------------------
wxMenuBar::~wxMenuBar(void)
{
	if (last_installed_bar == this) {
		::ClearMenuBar();
		last_installed_bar = NULL;
	}

	if (menu_bar_frame) 
		menu_bar_frame->wx_menu_bar = 0;
	for (int i = 0; i < n; i++)
	{
		menus[i]->menu_bar = NULL;  // So menu doesn't try to remove itself
		menus[i]->window_parent = NULL; // So menu doesn't try to remove itself
		delete menus[i];
		delete[] titles[i];
	}
	delete[] menus;
	delete[] titles;
}

///////////////////////////////////////////////////////////////////////////////
// Menus
///////////////////////////////////////////////////////////////////////////////

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
// Construct a menu with optional title (then use append)
//-----------------------------------------------------------------------------
wxMenu::wxMenu // Constructor (given objectType)
	(
		char*		Title,
		wxFunction	function,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMenu(Title, windowName)
{
	Callback(function);

	cMacMenuId = gMenuIdCounter++; // get next unique menuID
	wxMacString1 theMacString1 = title;
	cMacMenu = ::NewMenu(cMacMenuId, theMacString1());
	CheckMemOK(cMacMenu);
	WXGC_IGNORE(menu_bar);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
// The wxWindow destructor will take care of deleting the submenus.
//-----------------------------------------------------------------------------
wxMenu::~wxMenu(void)
{
	if (menu_bar) // may have to remove menu from the current mac menu bar
	{
		menu_bar->Delete(this);
		// mflatt: The AboutMenu Hack stuff below is mysterious to me
#if 0
		// Deactive AboutMenu Hack - This is untested
		if (this == menu_bar->wxHelpHackMenu) {
			menu_bar->wxHelpHackMenu = NULL;
			menu_bar->iHelpMenuHackNum = 0;
		}
		wxFrame* frame = menu_bar->menu_bar_frame;
		if (frame)
		{
			if (frame->IsFrontWindow())
			{
				::DeleteMenu(cMacMenuId);
			}
		}
#endif
	} else if (window_parent)
		((wxMenu *)window_parent)->Delete(this, 0, -1);

	::DisposeMenu(cMacMenu); // WCH: does this dispose of submenus? I hope not.
                             // mflatt: it does not dispose of submenus

	// if (title) delete[] title; // WCH: ~wxbMenu should do this - (cjc - it does)
	wxNode* node = menuItems.First();
   // mflatt: The menuItems list is not DeleteContents(TRUE).
   //         Also, we need to delete any submenus attached to this menu
	while (node) 
	{
    	wxMenuItem* item = (wxMenuItem*)node->Data();
		if (item->subMenu) {
		   item->subMenu->window_parent = NULL; // so it doesn't try to delete itself
		   delete item->subMenu;
		}
		delete item;
    	node = node->Next();
	}
}

// Helper function - Convert a wxMenu String into one you can give
// to the Mac menu mgr AppendMenu();
// setupstr should be used with AppendItem; showstr should then be used with SetMenuItemText
// If stripCmds is TRUE, instead of replacing wxMenu string special chars, 
// we delete them. This is appropriate for the menu text of a pulldown Menu.
static void BuildMacMenuString(StringPtr setupstr, StringPtr showstr, char *itemName,
								Bool stripCmds)
{
	char *s, *d;
	char spc = '\0';

	d = (char *)&showstr[1];
	if (itemName[0] == '-') // Fix problem with leading hyphen
		*d++ = ' ';
	for (s = itemName; *s != '\0'; ) {
		if (*s == '&') {
			// spc = *s;
		}
		else if (*s == '\t') {
			s++;
			if (strncmp("Cmd+", s, 4) == 0)
				spc = s[4];
			break;
		} 
		else {
			*d++ = *s;
		}
		s++;
	}
	showstr[0] = (char)(d - (char *)setupstr)-1;
	setupstr[1] = 'X'; // temporary menu item name
	if (spc && !stripCmds) {
		setupstr[2] = '/';
		setupstr[3] = spc;
		setupstr[0] = 3;
	} else
		setupstr[0] = 1;
}

static void StripMacMenuString(StringPtr theMacItemString1, char *s)
{
	char *d;
	char spc = '\0';
	for (d = (char *)&theMacItemString1[1]; *s != '\0'; ) {
		if (*s == '&') {
			spc = *++s;
			if (*s == '\0') break;
			*d++ = *s;
		}
		else {
			*d++ = *s;
		}
		s++;
	}
	theMacItemString1[0] = (d - (char *)theMacItemString1)-1;
}

MenuHandle wxMenu::CreateCopy(char *title, Bool doabouthack, MenuHandle toHandle)
{
	char t[256], tmp[256];
	int i, offset;
	MenuHandle nmh;
	int helpflg;
	
	if (!toHandle)  {
		// Remove accel - not used in Mac Top Level Menus
		char *s = t;
		for (i = 0; title[i]; i++) {
			if (title[i] == '&') {
				if (title[i + 1])
					i++;
				else
					break;
			}
			*(s++) = title[i];
		}
		*s = 0;
		s = t;
		helpflg = strncmp("Help", s, 4) ? 0 : 1;
		C2PStr(s);
		nmh = ::NewMenu(cMacMenuId ,(ConstStr255Param)s);
		CheckMemOK(nmh);
		offset = 1;
		if (helpflg && menu_bar && menu_bar->n) {
		  if (menu_bar->menus[menu_bar->n - 1] == this) {
		    menu_bar->wxHelpHackMenu = this;
		    menu_bar->iHelpMenuHackNum = 0;
		  } else
		     helpflg = 0;
		}
	} else {
		nmh = toHandle;
		offset = CountMItems(nmh) + 1;
		helpflg = 0;
	}
	int cnt = menuItems.Number();
	// Create a new Mac Menu 
	wxNode* node = menuItems.First();
	for (i = 0; i < cnt; i++) {
		// Try to recreate from the wxMenuItem
		wxMenuItem* menuItem = (wxMenuItem*)node->Data(); 
		if (menuItem->itemId == -1) {
			// Separator
			tmp[0] = t[0] = 1;
			tmp[1] = t[1] = '-';			
		} 
		else if (menuItem->subMenu) {
			BuildMacMenuString((StringPtr)tmp, (StringPtr)t, menuItem->itemName, TRUE);
			wxMenu *subMenu = menuItem->subMenu;
			subMenu->wxMacInsertSubmenu();
			unsigned char menuadds[4] = {'/',0x1b,'!', ' ' };
			menuadds[3] = subMenu->cMacMenuId;
			int len = tmp[0];
			memcpy(&tmp[len+1], menuadds, 4);
			tmp[0] += 4;
		}
		else {
			BuildMacMenuString((StringPtr)tmp, (StringPtr)t, menuItem->itemName, FALSE);
			if (!i && doabouthack && helpflg && (!strncmp("About", &t[1], 5))) {
			    if (menu_bar) {
				  // This is an very sad hack !
				  menu_bar->iHelpMenuHackNum = 1;
				}
			}
		}
		::AppendMenu(nmh, (ConstStr255Param)tmp);
		::SetMenuItemText(nmh, i + offset, (ConstStr255Param)t);
		if (menuItem->IsChecked())
		  ::CheckItem(nmh, i + offset, TRUE);
		if (!menuItem->IsEnabled() || (toHandle && !cEnable))
		  ::DisableItem(nmh, i + offset);
		node = node->Next();						// watch for null?	
	}
	
    return nmh;
}

// another Helper function - change the Title/Text of a Menu 
// We Have to change the Title in the MenuBar, which is rebuilt in wxFrame::SetMenubar
// Unfortunately, The ToolBox does'nt let you get/set the ItemText for "Menu" (items yes)
// but not the Menu. So the menu will have to be rebuilt. Plenty of room for errors!
void wxMenu::MacChangeMenuText(wxMenu *menu, char *new_title)
{
	// Get Handle to the Menu
	MenuHandle omh = menu->cMacMenu;
	
	title = macCopyString1(new_title);

    // Create a new Mac Menu 
	menu->cMacMenu = menu->CreateCopy(new_title, TRUE);

	// Remove from system Menu List if it might be there and redraw with new menu:
    if (menu->menu_bar && menu->menu_bar == last_installed_bar)
	  menu->menu_bar->Install();

	// Dispose the old menu
	::DisposeMenu(omh);
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxMenuBar::Enable(int Id, Bool Flag)
{
	for (int j = 0; j < n; j++)
	{
    	wxMenuItem* theMenuItem = menus[j]->FindItemForId(Id);
    	if (theMenuItem)
    	{
			theMenuItem->Enable(Flag);
			return;
		}
	}
}

//-----------------------------------------------------------------------------
void wxMenuBar::Check(int Id, Bool Flag)
{
	for (int j = 0; j < n; j++)
	{
    	wxMenuItem* theMenuItem = menus[j]->FindItemForId(Id);
    	if (theMenuItem)
    	{
			theMenuItem->Check(Flag);
			return;
		}
	}
}

//-----------------------------------------------------------------------------
Bool wxMenuBar::Checked(int Id)
{
	for (int j = 0; j < n; j++)
	{
    	wxMenuItem* theMenuItem = menus[j]->FindItemForId(Id);
    	if (theMenuItem)
    	{
			return theMenuItem->IsChecked();
		}
	}

	return FALSE;
}

//-----------------------------------------------------------------------------
void wxMenuBar::SetLabel(int Id, char* label)
{
	for (int j = 0; j < n; j++)
	{
    	wxMenuItem* theMenuItem = menus[j]->FindItemForId(Id);
    	if (theMenuItem)
    	{
			theMenuItem->SetLabel(label);
			return;
		}
	}
}

//-----------------------------------------------------------------------------
char* wxMenuBar::GetLabel(int Id)
{
	for (int j = 0; j < n; j++)
	{
    	wxMenuItem* theMenuItem = menus[j]->FindItemForId(Id);
    	if (theMenuItem)
    	{
			return theMenuItem->GetLabel();
		}
	}

	return NULL;
}

//-----------------------------------------------------------------------------
void wxMenuBar::SetLabelTop(int pos, char* label)
{
	if ((pos >= 0) && (pos < n)) {
	  menus[pos]->SetTitle(label);
	  titles[pos] = copystring(label);
	}
}

//-----------------------------------------------------------------------------
char* wxMenuBar::GetLabelTop(int pos) { 
  if ((pos >= 0) && (pos < n))
    return menus[pos]->GetTitle();
  else
    return NULL;
}

//-----------------------------------------------------------------------------
void wxMenuBar::EnableTop(int pos, Bool flag)
{ 
    /* For consistency with other platforms, 
       disabling is not allowed to work unless the menu bar in installed: */
	if (!menu_bar_frame && !flag)
	  return;
	
	if (pos >= 0 && (pos < n))
		menus[pos]->Enable(flag);
}

//-----------------------------------------------------------------------------
void wxMenuBar::SetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenuBar::DoSetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenuBar::Enable(Bool Flag) { }

//-----------------------------------------------------------------------------
void wxMenuBar::Show(Bool show) { }

//-----------------------------------------------------------------------------
char* wxMenuBar::GetLabel(void) { return NULL; } // The menubar doesn't have a label

//-----------------------------------------------------------------------------
void wxMenuBar::SetLabel(char* label) { } // The menubar doesn't have a label

//-----------------------------------------------------------------------------
wxMenu* wxMenuBar::wxMacFindMenu(int macMenuId)
{
	wxMenu* result = NULL;
	int i = 0;
	while (i < n && !result) // try to find among main menus
	{
		if (menus[i]->GetMacMenuId() == macMenuId)
			result = menus[i];
		else i++;
	}

	if (!result)
	{
		i = 0;
		while (i < n && !result) // try to find among submenus
		{
			result = menus[i]->wxMacFindSubmenu(macMenuId);
			if (!result) i++;
		}
	}

	return result;
}

// Append menu to menubar and optionally, change title of menu item
Bool wxMenuBar::OnAppend (wxMenu * menu, char *title)
{
  Bool retval = TRUE, new_menu = FALSE;

  if (!menu->window_parent) {
    menu->Enable(TRUE); /* Force the menu to be enabled (for consistency) */
    menu->window_parent = this;
    menu->menu_bar = this;
    new_menu = TRUE;
  } else if (menu->window_parent == this) {
	// Don't add, but maybe change the menu title
	retval = FALSE;
  } else
	return FALSE;
	
  if (retval) {
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
  }  

  wxMenu *wasHack = wxHelpHackMenu;
  wxHelpHackMenu = NULL;

  if (title)
	menu->MacChangeMenuText(menu, title);

  if (new_menu && this == last_installed_bar) {
	menu->wxMacInsertSubmenu();
    if (wasHack || (menu == wxHelpHackMenu)) {
	  Install();
	} else {
	  ::InsertMenu(menu->cMacMenu, 0);
	  ::InvalMenuBar();
	}
  }

  return retval;
}

Bool wxMenuBar::OnDelete(wxMenu *menu, int pos)
{
  int j, was_last = (pos == n - 1);

  --n;
  for (j = pos; j < n; j++) {
    menus[j] = menus[j + 1];
    titles[j] = titles[j + 1];
  }
  
  menu->window_parent = NULL;
  menu->menu_bar = NULL;
  
  int was_hack = (menu == wxHelpHackMenu);
  if (was_hack) {
  	  wxHelpHackMenu = NULL;
  	  iHelpMenuHackNum = 0;
  }
  int new_hack = FALSE;
  if (was_last && n) {
    /* Check for new Help menu: */
    wxMenu *m = menus[n - 1];
    DisposeMenu(m->CreateCopy(titles[n - 1], TRUE, NULL));
    if (wxHelpHackMenu)
      new_hack = TRUE;
  }
  
  if (menu_bar_frame && menu_bar_frame->IsFrontWindow()) {
  	if (was_hack || new_hack) {
  	  Install();
  	} else {
  	  ::DeleteMenu(menu->cMacMenuId);
	  ::InvalMenuBar();
	}
  }

  return TRUE;
}

// Helper function - Add the Apple Menu 
void wxSetUpAppleMenu(wxMenuBar *mbar)
{
	MenuHandle appleMenuHandle = GetMHandle(128);

	if (appleMenuHandle == NULL) {
		char t[2] = {1, appleMark};
		appleMenuHandle = ::NewMenu(128, (StringPtr)t);
		CheckMemOK(appleMenuHandle);
		::AddResMenu(appleMenuHandle, 'DRVR');
	}
	if (mbar && mbar->wxHelpHackMenu && mbar->iHelpMenuHackNum) {
		Str255 t = "\pAboutÉ";
		wxNode *n = mbar->wxHelpHackMenu->menuItems.Nth(mbar->iHelpMenuHackNum - 1);
		if (n) {
		  wxMenuItem *i = (wxMenuItem *)n->Data();
		  if (i) {
		    char *s = i->GetLabel();
		    strcpy((char *)t, s);
		    CtoPstr((char *)t);
		  }
		}
		
		::InsertMenuItem(appleMenuHandle, t, 0);
	} else {
		char buffer[256];
		strcpy(buffer, wxTheApp->GetDefaultAboutItemName());
		::InsertMenuItem(appleMenuHandle, C2PStr(buffer), 0);
    }
	::InsertMenu(appleMenuHandle, 0);
	
	HMGetHelpMenuHandle(&wxHelpMenu);
	if (!wxNumHelpItems) {
	  /* Each ClearMenuBar() seems to create a new HelpMenu.
	     But just in case Apple changes its mind... */
	  wxNumHelpItems = CountMItems(wxHelpMenu);
	}
	if (wxHelpMenu) {
	  int i = CountMItems(wxHelpMenu);
	  while (i > wxNumHelpItems)
	    DeleteMenuItem(wxHelpMenu, i--);
	    
	  if (mbar && mbar->wxHelpHackMenu)
	    mbar->wxHelpHackMenu->CreateCopy(NULL, FALSE, wxHelpMenu);
	}
}

void wxMenuBar::Install(void)
{
	::ClearMenuBar();
	wxSetUpAppleMenu(this);
	for (int i = 0; i < n; i ++)
	{
		wxMenu* menu = menus[i];
		if (menu != wxHelpHackMenu) {
		    ::InsertMenu(menu->MacMenu(), 0);
		    menu->wxMacInsertSubmenu();
		    if (!menu->IsEnable() || (menu_bar_frame && !menu_bar_frame->CanAcceptEvent()))
		    	::DisableItem(menu->MacMenu(), 0);
		    else
				::EnableItem(menu->MacMenu(), 0);
		}
	}
	::InvalMenuBar();
	last_installed_bar = this;
}

///////////////////////////////////////////////////////////////////////////////
// Menus
///////////////////////////////////////////////////////////////////////////////

short wxMenu::gMenuIdCounter = 129; // mac platform (to give unique menuID's to mac menus)

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxMenu::Break(void) { } // Macintosh has just one line for the menu bar

void wxMenu::CheckHelpHack(void)
{
   if (menu_bar && this == menu_bar->wxHelpHackMenu)
	  MacChangeMenuText(this, title);
}

//-----------------------------------------------------------------------------
void wxMenu::AppendSeparator(void)
{
	wxMenuItem* item = new wxMenuItem(this);

	item->itemId = -1;
	item->itemName = copystring("-");
	menuItems.Append(item);
	no_items ++;

	wxMacString1 theMacItemString1 = item->itemName; // menu item string can't be empty
	AppendMenu(cMacMenu, theMacItemString1());
	
	CheckHelpHack();
}

//-----------------------------------------------------------------------------
// Ordinary menu item
//-----------------------------------------------------------------------------
void wxMenu::Append(int Id, char* Label, char* helpString, Bool checkable)
{
//	assert(Id >= 1);
	wxMenuItem* item = new wxMenuItem(this, checkable);

	item->itemId = Id;
	item->itemName = macCopyString(Label);
	item->helpString = macCopyString(helpString);
	menuItems.Append(item);
	no_items ++;

	// wxMacString1 theMacItemString1 = item->itemName; // menu item string can't be empty
	// AppendMenu(cMacMenu, theMacItemString1());
	Str255 menusetup, menustr;
	BuildMacMenuString(menusetup, menustr, item->itemName, FALSE);
	::AppendMenu(cMacMenu, (ConstStr255Param)menusetup);
	::SetMenuItemText(cMacMenu, no_items, (ConstStr255Param)menustr);

	CheckHelpHack();
}

//-----------------------------------------------------------------------------
// Pullright item
//-----------------------------------------------------------------------------
void wxMenu::Append(int Id, char* Label, wxMenu* SubMenu, char* helpString)
{
//	assert(Id >= 1);
//	assert(SubMenu->window_parent == NULL); // WCH : error if submenu is a child of another

	// mflatt: If this menu is used, give up
	if (SubMenu->window_parent)
		return;

	SubMenu->window_parent = this;

	wxMenuItem* item = new wxMenuItem(this);
	item->subMenu = SubMenu;

  	item->itemId = Id;
  	item->itemName = macCopyString(Label);
  	item->helpString = macCopyString(helpString);

	menuItems.Append(item);
	no_items++;

	Str255 pullrightSetup, pullrightLabel;
	BuildMacMenuString(pullrightSetup, pullrightLabel,item->itemName, TRUE);
	
	int theEnd = pullrightSetup[0] + 1; // mflatt: +1
	if (theEnd > 251) theEnd = 251; // mac allows only 255 characters
	pullrightSetup[theEnd++] = '/';
	pullrightSetup[theEnd++] = 0x1B; // mac item is a pullright menu
	pullrightSetup[theEnd++] = '!';
	pullrightSetup[theEnd++] = SubMenu->cMacMenuId; // id of mac pullright menu
	pullrightSetup[theEnd] = 0x00;
	pullrightSetup[0] = theEnd;
	::AppendMenu(cMacMenu, (ConstStr255Param)pullrightSetup);
	::SetMenuItemText(cMacMenu, no_items, (ConstStr255Param)pullrightLabel);

	wxMenu *ancestor = this;
	while (ancestor) {
		if (ancestor->menu_bar) {
			if (ancestor->menu_bar == last_installed_bar) {
				InsertMenu(SubMenu->cMacMenu, -1);
				SubMenu->wxMacInsertSubmenu();
			}
			break;
		}
		ancestor = (wxMenu *)ancestor->window_parent;
    }

	CheckHelpHack();
}

// mflatt
Bool wxMenu::Delete(wxMenu *menu, int Id, int delpos)
{
  int pos;
  wxMenuItem *item;
  wxNode *node;
	
  if ((Id == -1) && (delpos == -1))
    return FALSE;
  
  for (pos = 0, node = menuItems.First(); node; node = node->Next(), pos++) {
    item = (wxMenuItem *)node->Data();
    if ((menu && item->subMenu == menu) 
	|| (!menu && (delpos == -1) && item->itemId == Id)
	|| (delpos == pos)) {
      if (item->subMenu)
	item->subMenu->window_parent = NULL;
      ::DeleteMenuItem(cMacMenu, pos + 1);
      menuItems.DeleteNode(node);
      delete item;
      --no_items;
      CheckHelpHack();
      return TRUE;
    }
  }

  return FALSE;
}

Bool wxMenu::DeleteByPosition(int pos)
{
  return Delete(NULL, -1, pos);
}

// mflatt
Bool wxMenu::Delete(int Id)
{
  return Delete((wxMenu *)NULL, Id, -1);
}

int wxMenu::Number()
{
  return no_items;
}

//-----------------------------------------------------------------------------
void wxMenu::Enable(int Id, Bool Flag)
{
	wxMenuItem* theMenuItem = FindItemForId(Id);
	if (theMenuItem)
		theMenuItem->Enable(Flag);

	CheckHelpHack();
}

//-----------------------------------------------------------------------------
void wxMenu::Check(int Id, Bool Flag)
{
	wxMenuItem* theMenuItem = FindItemForId(Id);
	if (theMenuItem)
		theMenuItem->Check(Flag);

	CheckHelpHack();
}

//-----------------------------------------------------------------------------
Bool wxMenu::Checked(int Id)
{
	wxMenuItem* theMenuItem = FindItemForId(Id);
	if (theMenuItem)
		return theMenuItem->IsChecked();
	else return FALSE;
}

//-----------------------------------------------------------------------------
void wxMenu::SetTitle(char* label)
{
	if (menu_bar)
	{
		// WCH : must reset Macintosh menu label (not easy)
		// WCH : the duplicate title in menuBar object may now be out of sync
		MacChangeMenuText(this, label);
	} else {
	  if (title) delete title;
	  title = macCopyString1(label);
	}
}

//-----------------------------------------------------------------------------
char* wxMenu::GetTitle(void) { return title; }

//-----------------------------------------------------------------------------
char* wxMenu::GetLabel(int Id)
{
	wxMenuItem* theMenuItem = FindItemForId(Id);
	if (theMenuItem)
		return theMenuItem->GetLabel();
	else return NULL;
}

//-----------------------------------------------------------------------------
void wxMenu::SetLabel(int Id , char* label)
{
	wxMenuItem* theMenuItem = FindItemForId(Id);
	label = wxItemStripLabel(label);
	if (theMenuItem)
		theMenuItem->SetLabel(label);

	CheckHelpHack();
}

//-----------------------------------------------------------------------------
void wxMenu::SetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenu::DoSetSize(int x, int y, int width, int height) { }

//-----------------------------------------------------------------------------
void wxMenu::Enable(Bool Flag)
{
	if (cEnable != Flag)
	{
		cEnable = Flag;
		if (menu_bar)
		{
			wxFrame* frame = menu_bar->menu_bar_frame;
			if (frame)
			{
				if (frame->IsFrontWindow())
				{
					if (cEnable)
						EnableItem(cMacMenu, 0);
					else
						DisableItem(cMacMenu, 0);
					::InvalMenuBar();
					CheckHelpHack();
				}
			}
		}
	}
}

//-----------------------------------------------------------------------------
void wxMenu::Show(Bool show) { }

//-----------------------------------------------------------------------------
char* wxMenu::GetLabel(void)
{
	return title; // WCH: confusion with title and label; cf. GetTitle
}

//-----------------------------------------------------------------------------
void wxMenu::SetLabel(char* label) 
{
}

//-----------------------------------------------------------------------------
MenuHandle wxMenu::MacMenu(void) { return cMacMenu; }

//-----------------------------------------------------------------------------
short wxMenu::GetMacMenuId(void) { return cMacMenuId; }

//-----------------------------------------------------------------------------
wxMenu* wxMenu::wxMacFindSubmenu(int macMenuId)
{
	wxMenu* result = NULL;
	wxNode* node = menuItems.First();
	while (node && !result)
	{
		wxMenuItem* menuItem = (wxMenuItem*)node->Data();
		wxMenu* submenu = menuItem->subMenu;
		if (submenu)
		{
			if (submenu->cMacMenuId == macMenuId)
				result = submenu;
			else
			{
				result = submenu->wxMacFindSubmenu(macMenuId);
			}
		}

		if (!result) node = node->Next();
	}

	return result;
}

//-----------------------------------------------------------------------------
void wxMenu::wxMacInsertSubmenu(void)
{
	wxNode* node = menuItems.First();
	while (node)
	{
		wxMenuItem* menuItem = (wxMenuItem*)node->Data();
		wxMenu* submenu = menuItem->subMenu;
		if (submenu)
		{
			InsertMenu(submenu->cMacMenu, -1);
			submenu->wxMacInsertSubmenu();
		}
		node = node->Next();
	}
}
