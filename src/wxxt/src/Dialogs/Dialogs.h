/*								-*- C++ -*-
 * $Id: Dialogs.h,v 1.2 1996/01/11 10:26:56 markus Exp $
 *
 * Purpose: common dialogs
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

#ifndef wxDialogs_h
#define wxDialogs_h

#ifdef __GNUG__
#pragma interface
#endif

class wxWindow;

int wxMessageBox(
    char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
    wxWindow *parent = NULL, int x = -1, int y = -1);
char *wxGetTextFromUser(
    char *message, char *caption = "Input text", char *default_value = "",
    wxWindow *parent = NULL, int x = -1, int y = -1, Bool centre = TRUE);
char *wxGetSingleChoice(
    char *message, char *caption, int n, char *choices[],
    wxWindow *parent = NULL, int x = -1, int y = -1, Bool centre = TRUE,
    int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);
int wxGetSingleChoiceIndex(
    char *message, char *caption, int n, char *choices[],
    wxWindow *parent = NULL, int x = -1, int y = -1, Bool centre = TRUE,
    int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);
char *wxGetSingleChoiceData(
    char *message, char *caption, int n, char *choices[], char *client_data[],
    wxWindow *parent = NULL, int x = -1, int y = -1, Bool centre = TRUE,
    int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);
int wxGetMultipleChoice(
    char *message, char *caption, int n, char *choices[],
    int nsel, int * selection, wxWindow *parent = NULL,
    int x = -1 , int y = -1, Bool centre = TRUE,
    int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);
char *wxLoadFileSelector(
    char *what = "Text", char *extension = "txt", char *default_name = NULL,
    wxWindow *parent=NULL);
char *wxSaveFileSelector(
    char *what = "Text", char *extension = "txt", char *default_name = NULL,
    wxWindow *parent=NULL);
char *wxFileSelector(
    char *message = "Select a file", char *default_path = NULL,
    char *default_filename = NULL, char *default_extension = NULL,
    char *wildcard = "*.*", int flags = 0,
    wxWindow *parent = NULL, int x = -1, int y = -1);

#endif // wxDialogs_h
