/*
 * File:	wx_cmdlg.h
 * Purpose:	Common dialogs: X declarations
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_cmdlgh
#define wx_cmdlgh

#ifdef __GNUG__
#pragma interface
#endif

#include "wx_setup.h"
#include "wb_cmdlg.h"

#define ORPHAN_PARENT NULL

int wxMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
  wxWindow *parent = ORPHAN_PARENT, int x = -1, int y = -1);

#endif
