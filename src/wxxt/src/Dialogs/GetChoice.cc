/*								-*- C++ -*-
 * $Id: GetChoice.cc,v 1.2 1996/01/11 10:26:57 markus Exp $
 *
 * Purpose: dialogs to select or more selection from listboxes
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

#define  Uses_wxDialogBase
#define  Uses_wxListBox
#include "wx.h"

int wxGetSingleChoiceIndex(char *message, char *caption, int n,
			   char *choices[], wxWindow *parent,
			   int x, int y, Bool centre, int width, int height)
{
    int selection = -1;

    // create dialog box and listbox item
    wxDialogBase *box
	= DEBUG_NEW wxDialogBase(message, caption,
				 (centre ? wxCENTRE : 0) | wxOK | wxCANCEL,
				 parent, x, y);
    wxListBox *lbox
	= DEBUG_NEW wxListBox(box, (wxFunction)NULL, NULL, wxSINGLE|wxALWAYS_SB,
			      -1, -1, width, height, n, choices);
    // wait for input
    if (box->GetInput() == wxOK) {
	selection = lbox->GetSelection();
    }
    delete box;
    return selection;
}

char *wxGetSingleChoice(char *message, char *caption, int n, char *choices[],
			wxWindow *parent, int x, int y, Bool centre,
			int width, int height)
{
    int selection = wxGetSingleChoiceIndex(message, caption, n, choices,
					   parent, x, y, centre, width,height);
    if (selection > -1)
	return choices[selection];
    return NULL;
}

char *wxGetSingleChoiceData(char *message, char *caption, int n,
			    char *choices[], char *client_data[],
			    wxWindow *parent, int x, int y, Bool centre,
			    int width, int height)
{
    int selection = wxGetSingleChoiceIndex(message, caption, n, choices,
					   parent, x, y, centre, width,height);
    if (selection > -1)
	return client_data[selection];
    return NULL;
}

int wxGetMultipleChoice(char *message, char *caption, int n, char *choices[],
			int nsel, int *selection, wxWindow *parent,
			int x , int y, Bool centre, int width, int height)
{
    int num_selections = -1;

    // create dialog box and listbox item
    wxDialogBase *box
	= DEBUG_NEW wxDialogBase(message, caption,
				 (centre ? wxCENTRE : 0) | wxOK | wxCANCEL,
				 parent, x, y);
    wxListBox *lbox
	= DEBUG_NEW wxListBox(box, (wxFunction)NULL, NULL, wxMULTIPLE|wxALWAYS_SB,
			      -1, -1, width, height, n, choices);
    // initialize lbox
    for (int i=0; i < nsel; i++)
	lbox->SetSelection(selection[i], TRUE);
    // wait for input
    if (box->GetInput() == wxOK) {
	int *selections_internal;
	num_selections = lbox->GetSelections(&selections_internal);
	for(int i=0; i < num_selections; ++i)
	    selection[i] = selections_internal[i];
    }
    delete box;
    return num_selections;
}
