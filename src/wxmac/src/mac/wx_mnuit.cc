///////////////////////////////////////////////////////////////////////////////
// File:	wx_mnuit.cc (split from wx_item.cc)
// Purpose:	Menu items implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_mnuit.h"
#include "wx_menu.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Constructors
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMenuItem::wxMenuItem
(
 void
 )
{
  checkable = FALSE;
  cIsChecked = FALSE;
  cIsEnabled = TRUE;
  parentMenu = NULL;
  WXGC_IGNORE(this, parentMenu);
}

wxMenuItem::wxMenuItem
(
 wxMenu* theParentMenu,
 Bool	isCheckable
 )
{
  checkable = isCheckable;
  cIsChecked = FALSE;
  cIsEnabled = TRUE;
  parentMenu = theParentMenu;
  if (!theParentMenu) wxFatalError("No parent menu for constructing menu item.");
  WXGC_IGNORE(this, parentMenu);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Destructor
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMenuItem::~wxMenuItem(void)
{
  if (parentMenu)
    {
      // Must detach this from parent menu
    }
  parentMenu = NULL;

  if (subMenu)
    {
      // Must detach submenu from this
      // Must delete submenu
    }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// tree methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMenu* wxMenuItem::ParentMenu(void) { return parentMenu; }

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxMenuItem::Check(Bool flag)
{
  if (checkable)
    {
      if (cIsChecked != flag)
	{
	  short macMenuItem;

	  cIsChecked = flag;
	  macMenuItem = GetMacItemNumber();
	  if (macMenuItem > 0) {
	    CheckMenuItem(parentMenu->MacMenu(), macMenuItem, cIsChecked);
	  }
	}
    }
}

//-----------------------------------------------------------------------------
Bool wxMenuItem::IsChecked(void) { return cIsChecked; }

//-----------------------------------------------------------------------------
Bool wxMenuItem::IsCheckable(void) { return checkable; }

//-----------------------------------------------------------------------------
void wxMenuItem::Enable(Bool flag)
{
  if (cIsEnabled != flag)
    {
      short macMenuItem;

      cIsEnabled = flag;
      macMenuItem = GetMacItemNumber();
      if (macMenuItem > 0)
	{
	  if (cIsEnabled)
	    EnableMenuItem(parentMenu->MacMenu(), macMenuItem);
	  else DisableMenuItem(parentMenu->MacMenu(), macMenuItem);
	}
    }
}

//-----------------------------------------------------------------------------
char* wxMenuItem::GetHelpString(void) { return helpString; }

//-----------------------------------------------------------------------------
void wxMenuItem::SetHelpString(char* theHelpString)
{
  helpString = macCopyString(theHelpString);
}

//-----------------------------------------------------------------------------
char* wxMenuItem::GetLabel(void) {  return itemName; }

//-----------------------------------------------------------------------------
void wxMenuItem::SetLabel(char* label)
{
  short macMenuItem;

  itemName = macCopyString(label);

  macMenuItem = GetMacItemNumber();
  if (macMenuItem > 0) {
    unsigned char *s;
    if (label[0])
      s = wxC2P(label);
    else
      s = (unsigned char *)"\p ";
    SetMenuItemText(parentMenu->MacMenu(), macMenuItem, s);
  }
}

//-----------------------------------------------------------------------------
short wxMenuItem::GetMacItemNumber(void) // mac platform only
{
  short result = 0;
  if (parentMenu) {
    long memberIndex;
    memberIndex = parentMenu->menuItems->MemberIndex(this);
    if (memberIndex >= 0) result = memberIndex + 1; // mac counts from one
  }

  return result; // zero result means not found
}
