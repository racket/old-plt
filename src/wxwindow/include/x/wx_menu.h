/*
 * File:	wx_menu.h
 * Purpose:	Declares menus and menu bars (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_menu.h	1.2 5/9/94" */

#ifndef wx_menuh
#define wx_menuh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_menu.h"
#include "wx_mnuit.h"

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/Form.h>
#endif
#ifdef wx_xview
#include <xview/openmenu.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxMenu ;
typedef       void    *wxMenuBar ;
#else

class wxMenuBar;

// Menu
class wxMenu: public wxbMenu
{
  DECLARE_DYNAMIC_CLASS(wxMenu)

 public:
#ifdef wx_motif
  int numColumns;
  Widget buttonWidget; // The actual string, so we can grey it etc.
  int menuId;
  wxMenu *top_menu ;
#endif
#ifdef wx_xview
  void *panelItem ;
#endif

  wxMenu(char *Title = NULL, wxFunction func = NULL);
  ~wxMenu(void);
  void AppendSeparator(void);
  void Append(int Id, char *Label, char *helpString=NULL, Bool checkable=FALSE);
  void Append(int Id, char *Label, wxMenu *SubMenu, char *helpString = NULL);
  void Enable(int Id, Bool Flag);
  // Avoid compiler warning
  void Enable(Bool Flag) { wxItem::Enable(Flag); }
  void Check(int Id, Bool Flag);
  Bool Checked(int Id);
  void SetTitle(char *label);
  char *GetTitle(void);
  void SetLabel(int id, char *label);
  // Avoid compiler warning
  void SetLabel(char *label) { wxItem::SetLabel(label); }
  char *GetLabel(int id);
  // Avoid compiler warning
  char *GetLabel(void) { return wxItem::GetLabel(); }
  void Break(void) ;

  /* MATTHEW: [6] */
  Bool DeleteItem(int id, int pos);
  Bool Delete(int id);
  Bool DeleteByPosition(int pos);

  int Number(void);

#ifdef wx_motif
  Widget CreateMenu(wxMenuBar *menuBar, Widget parent, wxMenu *topMenu,
            char *title = NULL, Bool isPulldown = FALSE);

  // For popups, need to destroy, then recreate menu for a different (or
  // possibly same) window, since the parent may change.
  void DestroyMenu(Bool full); /* MATTHEW: [13] */
  Widget FindMenuItem(int Id, wxMenuItem **it = NULL);
#endif
#ifdef wx_xview
  Menu_item FindMenuItem(int Id);
#endif
};

// Menu Bar (a la Windows)
#define MENU_BAR_PANEL_HEIGHT 30
class wxMenuBar:public wxbMenuBar
{
  DECLARE_DYNAMIC_CLASS(wxMenuBar)

 public:
  wxMenuBar(void);
  wxMenuBar(int n, wxMenu *menus[], char *Titles[]);
  ~wxMenuBar(void);

  // Must only be used AFTER menu has been attached to frame,
  // otherwise use individual menus to enable/disable items
  void Enable(int id, Bool flag);
  // Avoid compiler warning
  void Enable(Bool Flag) { wxItem::Enable(Flag); }
  void EnableTop(int pos, Bool flag);
  void Check(int id, Bool flag);
  Bool Checked(int Id);
  // Avoid compiler warning
  void SetLabel(char *label) { wxItem::SetLabel(label); }
  void SetLabel(int id, char *label) ;
  char *GetLabel(int id) ;
  // Avoid compiler warning
  char *GetLabel(void) { return wxItem::GetLabel(); }
  void SetLabelTop(int pos,char *label) ;
  char *GetLabelTop(int pos) ;
  /* MATTHEW: [6] */
  virtual Bool OnAppend(wxMenu *menu, char *title);
  virtual Bool OnDelete(wxMenu *menu, int index);
};

#endif // IN_CPROTO
#endif // wx_menuh
