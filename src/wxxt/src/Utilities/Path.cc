/*								-*- C++ -*-
 * $Id: Path.cc,v 1.2 1999/08/09 14:51:29 mflatt Exp $
 *
 * Purpose: path- and filename manipulations
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

#include "wx.h"

#include <string.h>

char *wxFileNameFromPath(char *path)
{
    if (path) {
      char *tcp;
      
      tcp = path+strlen(path);
      while (--tcp >= path) {
	if (*tcp == '/' || *tcp == '\\')
	  return tcp + 1;
      }
    }

    return path;
}

char *wxPathOnly(char *path)
{
    if (path) {
        char *p;
        char *last_slash = NULL;

        // copy path and keep the last slash or baskslash in mind
        for (p = wxBuffer; *path; ++path, ++p) {
            *p = *path;
            if (*p == '/' || *p == '\\')
                last_slash = p;
        }
        if (last_slash) {
            *last_slash = '\0';
            return wxBuffer;
        }
    }
    return NULL;
}
