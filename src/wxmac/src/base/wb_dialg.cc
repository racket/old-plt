/*
 * File:	wb_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_dialg.cc,v 1.5 1999/07/12 14:25:37 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_dialg.cc	1.2 5/9/94"; */

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
#include "wx_mgstr.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
// #include <iostream.h>
#include <stdio.h>

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

// Generic file load/save dialog
// static inline char * // HP compiler complains
static char *
wxDefaultFileSelector(Bool load, const char *what, char *extension, char *default_name)
{
  char prompt[50];
  sprintf(prompt, load ? wxSTR_LOAD_FILE : wxSTR_SAVE_FILE, what);

  if (*extension == '.') extension++;
  char wild[60];
  sprintf(wild, "*.%s", extension);
#ifdef wx_mac
  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild, load ? wxOPEN : wxSAVE);
#else
  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild);
#endif
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
#if 0
  int x_offset,y_offset ;
  int display_width, display_height;
  int  width, height, x, y;
  wxFrame *frame ;
  if (direction & wxCENTER_FRAME)
  {
    frame = (wxFrame*)GetParent() ;
    if (frame)
    {
      frame->GetPosition(&x_offset,&y_offset) ;
      frame->GetSize(&display_width,&display_height) ;
    }
  }
  else
    frame = NULL;

  if (frame==NULL)
  {
    wxDisplaySize(&display_width, &display_height);
    x_offset = 0 ;
    y_offset = 0 ;
  }


  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  SetSize(x+x_offset, y+y_offset, width, height, wxPOS_USE_MINUS_ONE);
#endif
  ((wxDialogBox *)this)->cFrame->Centre(direction);
}

