/*								-*- C++ -*-
 * $Id: GetText.cc,v 1.2 1996/01/11 10:26:57 markus Exp $
 *
 * Purpose: get single text input
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
#define  Uses_wxText
#include "wx.h"

char *wxGetTextFromUser(char *message, char *caption, char *default_value,
			wxWindow *parent, int x, int y, Bool centre)
{
    static char *return_text = NULL;

    // if there is an old text delete it
    if (return_text)
	delete return_text;
    return_text = NULL;

    // create dialog box and text item
    wxDialogBase *box
	= DEBUG_NEW wxDialogBase(message, caption,
				 (centre ? wxCENTRE : 0) | wxOK | wxCANCEL,
				 parent, x, y);
    wxText *txt
	= DEBUG_NEW wxText(box, (wxFunction)NULL, NULL, default_value,
			   -1, -1, 200, -1, wxPROCESS_ENTER);
    // wait for input
    if (box->GetInput() == wxOK)
	return_text = copystring(txt->GetValue());
    delete box;
    return return_text;
}
