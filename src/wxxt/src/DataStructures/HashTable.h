/*								-*- C++ -*-
 * $Id: HashTable.h,v 1.1 1996/01/10 14:55:27 markus Exp $
 *
 * Purpose: hash table
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

#ifndef HashTable_h
#define HashTable_h

#ifdef __GNUG__
#pragma interface
#endif

class wxList;
class wxNode;

class wxHashTable : public wxObject {
DECLARE_DYNAMIC_CLASS(wxHashTable)
public:
    size_t n;
    int current_position;
    wxNode *current_node;

    unsigned int key_type;
    wxList **hash_table;

    wxHashTable(int the_key_type = wxKEY_INTEGER, int size = 1000);
    ~wxHashTable(void);

    Bool Create(int the_key_type = wxKEY_INTEGER, int size = 1000);

    // Note that there are 2 forms of Put, Get.
    // With a key and a value, the *value* will be checked
    // when a collision is detected. Otherwise, if there are
    // 2 items with a different value but the same key,
    // we'll retrieve the WRONG ONE. So where possible,
    // supply the required value along with the key.
    // In fact, the value-only versions make a key, and still store
    // the value. The use of an explicit key might be required
    // e.g. when combining several values into one key.
    // When doing that, it's highly likely we'll get a collision,
    // e.g. 1 + 2 = 3, 2 + 1 = 3.

    // key and value are NOT necessarily the same
    void Put(long key, long value, wxObject *object);
    void Put(long key, char *value, wxObject *object);

    // key and value are the same
    void Put(long value, wxObject *object);
    void Put(const char *value, wxObject *object);

    // key and value not the same
    wxObject *Get(long key, long value);
    wxObject *Get(long key, char *value);

    // key and value are the same
    wxObject *Get(long value);
    wxObject *Get(const char *value);

    // Deletes entry and returns data if found
    wxObject *Delete(long key);
    wxObject *Delete(const char *key);

    wxObject *Delete(long key, int value);
    wxObject *Delete(long key, char *value);

    // Construct your own integer key from a string, e.g. in case
    // you need to combine it with something
    long MakeKey(const char *string);

    // Way of iterating through whole hash table (e.g. to delete everything)
    // Not necessary, of course, if you're only storing pointers to
    // objects maintained separately

    void BeginFind(void);
    wxNode *Next(void);

    void DeleteContents(Bool flag);
    void Clear(void);
};

#endif //HashTable_h
