/*								-*- C++ -*-
 * $Id: PathList.h,v 1.1 1996/01/10 14:55:31 markus Exp $
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

#ifndef PathList_h
#define PathList_h

#ifdef __GNUG__
#pragma interface
#endif

class wxPathList : public wxList {
DECLARE_DYNAMIC_CLASS(wxPathList)
public:
    wxPathList(void) : wxList() {}

    void AddEnvList(char *envVariable);
    void Add(char *path);
    char *FindValidPath(char *filename);
    void EnsureFileAccessible(char *path);
    Bool Member(char *path);
};

#endif // PathList_h
