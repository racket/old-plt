/*								-*- C++ -*-
 * $Id: StringList.h,v 1.1 1996/01/10 14:55:35 markus Exp $
 *
 * Purpose: string list
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

#ifndef StringList_h
#define StringList_h

#ifdef __GNUG__
#pragma interface
#endif

class wxStringList : public wxList {
DECLARE_DYNAMIC_CLASS(wxStringList)
public:
    wxStringList(void);
    wxStringList(char *first ...);
    ~wxStringList(void);

    virtual wxNode   *Add(const char *s);
    virtual void     Delete(const char *s);
    virtual char     **ListToArray(Bool new_copies = FALSE);
    virtual void     Sort(void);
    virtual Bool     Member(const char *s);
};

#endif // StringList_h
