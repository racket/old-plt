/*
 * File:	wx_menu.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_menu.h	1.2 5/9/94" */

#ifndef wx_menuh
#define wx_menuh

#include "common.h"
#include "wx_win.h"
#include "wx_panel.h"
#include "wb_menu.h"

#ifdef IN_CPROTO
typedef       void    *wxMenu ;
typedef       void    *wxMenuBar ;
#else

class wxMenuBar;

// Menu
class wxMenu: public wxbMenu
{
  DECLARE_DYNAMIC_CLASS(wxMenu)
 private:
  Bool mustBeBreaked ;
 public:
  HANDLE save_ms_handle ; // Used for Enable() on popup

  wxMenu(char *Title = NULL, wxFunction func = NULL);
  ~wxMenu(void);
  void AppendSeparator(void);
  void Append(int id, char *Label, char *helpString=NULL, Bool checkable=FALSE);
  void Append(int id, char *Label, wxMenu *SubMenu, char *helpString = NULL);
  void Enable(int id, Bool Flag);
  void Check(int id, Bool Flag);
  Bool Checked(int id);
  void SetTitle(char *label);
  char *GetTitle(void);
  inline void SetLabel(char *label) { wxItem::SetLabel(label); } ;
  void SetLabel(int id, char *label);
  inline char *GetLabel(void) {return wxItem::GetLabel(); };
  char *GetLabel(int id);
  void Break(void) ;

  /* MATTHEW: [6] */
  Bool DeleteItem(int, int);
  Bool Delete(int id);
  Bool DeleteByPosition(int pos);

  int Number(void);

  BOOL MSWCommand(UINT param, WORD id);
};

// Menu Bar (a la Windows)
#define MENU_BAR_PANEL_HEIGHT 30
class wxFrame;
class wxMenuBar:public wxbMenuBar
{
  DECLARE_DYNAMIC_CLASS(wxMenuBar)

 public:
  wxMenuBar(void);
  wxMenuBar(int n, wxMenu *menus[], char *Titles[]);
  ~wxMenuBar(void);

  void Append(wxMenu *menu, char *title);
  // Must only be used AFTER menu has been attached to frame,
  // otherwise use individual menus to enable/disable items
  void Enable(int Id, Bool Flag);
  void EnableTop(int pos, Bool Flag);
  void Check(int id, Bool Flag);
  Bool Checked(int id);
  inline void SetLabel(char *label) { wxItem::SetLabel(label); } ;
  void SetLabel(int id,char *label) ;
  inline char *GetLabel(void) {return wxItem::GetLabel(); };
  char *GetLabel(int id) ;
  void SetLabelTop(int pos,char *label) ;
  char *GetLabelTop(int pos) ;

  /* MATTHEW: [6] */
  virtual Bool OnAppend(wxMenu *menu, char *title);
  virtual Bool OnDelete(wxMenu *menu, int index);
};

#endif // IN_CPROTO
#endif // wx_menuh
