/*
 * File:	wb_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_frame.h"
#include "wx_panel.h"
#include "wx_item.h"
#include "wx_buttn.h"
#include "wx_messg.h"
#include "wx_choic.h"
#include "wx_check.h"
#include "wx_menu.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_slidr.h"
#include "wx_lbox.h"

#include <stdio.h>

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

// Generic file load/save dialog
// static inline char * // HP compiler complains
static char *
wxDefaultFileSelector(Bool load, const char *what, char *extension, char *default_name)
{
  char prompt[50];
  sprintf(prompt, load ? "Get File" : "Save File", what);

  if (*extension == '.') extension++;
  char wild[60];
  sprintf(wild, "*.%s", extension);
  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild, load ? wxOPEN : wxSAVE);
}


// Generic file load dialog
char *
wxLoadFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(TRUE, what, extension, default_name);
}


// Generic file save dialog
char *
wxSaveFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(FALSE, what, extension, default_name);
}


// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal
#ifndef wx_mac
wxbDialogBox::wxbDialogBox(void)
{
  __type = wxTYPE_DIALOG_BOX;
}
wxbDialogBox::wxbDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name):
  wxPanel()
{
  __type = wxTYPE_DIALOG_BOX;
  windowStyle = style;
}
#else
#endif


Bool wxbDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name)
{
  windowStyle = style;
#if 0
  /* Handled by dialog's frame. */
  wxTopLevelWindows(ContextWindow())->Append(this);
  wxTopLevelWindows(ContextWindow())->Show(this, FALSE);
#endif
  return TRUE;
}

wxbDialogBox::~wxbDialogBox()
{
  wxTopLevelWindows(ContextWindow())->DeleteObject(this);
}

void wxbDialogBox::Centre(int direction)
{
  ((wxDialogBox *)this)->cFrame->Centre(direction);
}

