/*								-*- C++ -*-
 * $Id: MessageBox.cc,v 1.2 1996/01/11 10:26:58 markus Exp $
 *
 * Purpose: message box
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
#include "wx.h"

int wxMessageBox(char *message, char *caption, long style, wxWindow *parent,
		 int x, int y)
{
    int selection;

    // create dialog box
    wxDialogBase *box
	= DEBUG_NEW wxDialogBase(message, caption, style, parent, x, y);
    // wait for input
    selection = box->GetInput();
    delete box;
    return selection;
}
