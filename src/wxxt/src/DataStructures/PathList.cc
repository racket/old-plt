/*								-*- C++ -*-
 * $Id: PathList.cc,v 1.1 1996/01/10 14:55:31 markus Exp $
 *
 * Purpose: path list
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

#ifdef __GNUG__
#pragma implementation "PathList.h"
#endif

#define  Uses_wxPathList
#include "wx.h"

#include <string.h>
#include <limits.h>

IMPLEMENT_DYNAMIC_CLASS(wxPathList, wxList)

void wxPathList::Add(char *path)
{
    Append((wxObject*)path);
}

void wxPathList::AddEnvList(char *envVariable)
{
    static const char PATH_TOKS[] = ":;";

    char *val = getenv(envVariable);
    if (val && *val) {
	char *s = copystring(val);
	char *token = strtok(s, PATH_TOKS);
	if (token) {
	    Add(copystring(token));
	    while (token) {
		if ((token=strtok(NULL, PATH_TOKS)) != NULL)
		    Add(copystring(token));
	    }
	}
	delete[]s;
    }
}

void wxPathList::EnsureFileAccessible(char *path)
{
    char *path_only = wxPathOnly(path);
    if (path_only) {
	if (!Member(path_only))
	    Add (path_only);
    }
}

Bool wxPathList::Member(char *path)
{
    for (wxNode *node = First(); node != NULL; node = node->Next()) {
	char *path2 = (char*)node->Data();
	if (path2 && strcmp (path, path2) == 0)
	    return TRUE;
    }
    return FALSE;
}

char *wxPathList::FindValidPath(char *file)
{
  /* MATTHEW: [14] No file exists check */

  wxExpandPath(wxBuffer, file);
  
  char buf[PATH_MAX];
  strcpy(buf, wxBuffer);
  
  char *filename = IsAbsolutePath(buf) ? wxFileNameFromPath(buf) : buf;
  
  for (wxNode *node = First(); node; node = node->Next()) {
    char *path = (char*)node->Data();
    strcpy (wxBuffer, path);
    strcat (wxBuffer, "/");
    strcat (wxBuffer, filename);
    if (wxFileExists(wxBuffer))
      return wxBuffer;
  }
  return NULL;
}
