/*								-*- C++ -*-
 * $Id: FindFile.cc,v 1.1 1996/01/10 14:56:51 markus Exp $
 *
 * Purpose: find first/next file (DO NOT USE MULTIPLE TIMES SIMULTANEOUS!!)
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
// $Id: FindFile.cc,v 1.1 1996/01/10 14:56:51 markus Exp $

#include "wx.h"

#include <dirent.h>
#include <string.h>

// problems occure when used multiple times simultanously
// stock objects
static DIR  *wxDirStream = NULL;
static char *wxFileSpec = NULL;
static int  wxFindFileFlags = 0;

char *wxFindFirstFile(const char *spec, int flags)
{
    if (wxDirStream)
	closedir(wxDirStream);
    wxFindFileFlags = flags;
    if (wxFileSpec)
	delete[] wxFileSpec;
    wxFileSpec = copystring(spec);

    wxDirStream = opendir(wxPathOnly(wxFileSpec));

    return wxFindNextFile();
}

char *wxFindNextFile(void)
{
    static char buf[400];

    if (!wxDirStream)
	return NULL;

    // Find path only so we can concatenate
    // found file onto path
    char *p = wxPathOnly(wxFileSpec);
    char *n = wxFileNameFromPath(wxFileSpec);
    
    // Do the reading
    struct dirent *nextDir;
    for (nextDir=readdir(wxDirStream); nextDir!=NULL;
				       nextDir=readdir(wxDirStream)) {
	if ((strcmp(nextDir->d_name, ".") == 0)
	|| (strcmp(nextDir->d_name, "..") == 0)) {
	    if (wxFindFileFlags == wxDIR) {
		buf[0] = '\0';
		if (p && *p) {
		    strcpy(buf, p);
		    if (strcmp(p, "/") != 0)
			strcat(buf, "/");
		}
		strcat(buf, nextDir->d_name);
		return buf;
	    }
	} else if (!wxIsWild(n) || wxMatchWild(n, nextDir->d_name)) {
	    buf[0] = '\0';
	    if (p && *p) {
		strcpy(buf, p);
		if (strcmp(p, "/") != 0)
		    strcat(buf, "/");
	    }
	    strcat(buf, nextDir->d_name);
	    // Check for directory
	    if (wxFindFileFlags == wxDIR) {
		if (wxDirExists(buf))
		    return buf;
	    } else
		return buf;
	}
    }
    return NULL;
}
