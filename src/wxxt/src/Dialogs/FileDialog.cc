/*								-*- C++ -*-
 * $Id: FileDialog.cc,v 1.1.1.1 1997/12/22 17:28:49 mflatt Exp $
 *
 * Purpose: file load and save dialogs
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

#include <limits.h> // for PATH_MAX
#include <sys/types.h>
#include <dirent.h> // for opendir, readdir, etc.
#include <unistd.h> // for getcwd
#include <string.h>

#define  Uses_wxDialogBox
#define  Uses_wxStringList
#define  Uses_wxButton
#define  Uses_wxLayout
#define  Uses_wxListBox
#define  Uses_wxMessage
#define  Uses_wxText
#include "wx.h"

//-----------------------------------------------------------------------------
// wxFileSelector
//-----------------------------------------------------------------------------
char *wxFileSelector(char *message, char *default_path,
		     char *default_filename, char *WXUNUSED(default_extension),
		     char *wildcard, int flags, wxWindow *parent, int x, int y)
{
  return NULL;
}

char *wxLoadFileSelector(char *WXUNUSED(what), char *extension, char *default_name,
			 wxWindow *parent)
{
    if (*extension == '.')  ++extension;
    char wild[60];
    sprintf(wild, "*.%s", extension);
    return wxFileSelector("Load file", NULL, default_name,
			  (char*)extension, wild, 0, parent);
}

char *wxSaveFileSelector(char *WXUNUSED(what), char *extension, char *default_name,
			 wxWindow *parent)
{
    if (*extension == '.')  ++extension;
    char wild[60];
    sprintf(wild, "*.%s", extension);
    return wxFileSelector("Save file", NULL, default_name,
			  (char*)extension, wild, 0, parent);
}
