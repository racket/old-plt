/*								-*- C++ -*-
 * $Id: Misc.cc,v 1.1.1.1 1997/12/22 17:28:56 mflatt Exp $
 *
 * Purpose: miscellaneous utilities
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

#define  Uses_XLib
#define  Uses_wxFrame
#define  Uses_wxList
#define  Uses_wxMenuBar
#define  Uses_wxWindow
#include "wx.h"

void wxBell(void)
{
    XBell(wxAPP_DISPLAY, 100);
}

long wxGetFreeMemory(void)
{
    return -1; // cannot determine amount (UNIX)
}

int wxGetOsVersion(int *majorVsn, int *minorVsn)
{
    if (majorVsn)  *majorVsn = X_PROTOCOL;
    if (minorVsn)  *minorVsn = XlibSpecificationRelease;
    return wxXT_X;
}

/*
 * wxWindows search function utilities
 */

static wxWindow *wxFindWindowByLabel1(char *title, wxWindow *parent);

wxWindow *wxFindWindowByLabel(char *title, wxWindow *parent)
{
    if (parent) {
	return wxFindWindowByLabel1(title, parent);
    } else {
	for (wxChildNode* node=wxTopLevelFrames(NULL)->First(); node; node=node->Next()) {
	    wxWindow *win = (wxWindow*)node->Data();
	    if (win && node->IsShown()) {
	      wxWindow *retwin = wxFindWindowByLabel1(title, win);
	      if (retwin)
		return retwin;
	    }
	}
    }
    return NULL;
}

static wxWindow *wxFindWindowByLabel1(char *title, wxWindow *parent)
{
    if (parent) {
	char *lab = parent->GetLabel();
	if (lab && StringMatch(title, lab))
	    return parent;
    }
    if (parent) {
	for (wxChildNode *node=parent->GetChildren()->First(); node; node=node->Next()) {
	    wxWindow *win = (wxWindow*)node->Data();
	    wxWindow *retwin = wxFindWindowByLabel1(title, win);
	    if (retwin)
		return retwin;
	}
    }
    return NULL;
}

static wxWindow *wxFindWindowByName1(char *title, wxWindow *parent);

wxWindow *wxFindWindowByName(char *title, wxWindow *parent)
{
    if (parent) {
	return wxFindWindowByName1(title, parent);
    } else {
	for (wxChildNode *node=wxTopLevelFrames(NULL)->First(); node; node=node->Next()) {
	    wxWindow *win = (wxWindow*)node->Data();
	    if (win && node->IsShown()) {
	      wxWindow *retwin = wxFindWindowByName1(title, win);
	      if (retwin)
		return retwin;
	    }
	}
    }
    // Failed? Try by label instead.
    return wxFindWindowByLabel(title, parent);
}

static wxWindow *wxFindWindowByName1(char *title, wxWindow *parent)
{
    if (parent) {
	char *lab = parent->GetName ();
	if (lab && StringMatch(title, lab))
	    return parent;
    }
    if (parent) {
	for (wxChildNode *node=parent->GetChildren()->First(); node; node=node->Next()) {
	    wxWindow *win = (wxWindow*)node->Data();
	    wxWindow *retwin = wxFindWindowByName1(title, win);
	    if (retwin)
		return retwin;
	}
    }
    return NULL;
}

int wxFindMenuItemId(wxFrame *frame, char *menuString, char *itemString)
{
    wxMenuBar *menuBar = frame->GetMenuBar();
    if (!menuBar)
	return -1;
    return menuBar->FindMenuItem(menuString, itemString);
}
