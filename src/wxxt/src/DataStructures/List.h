/*								-*- C++ -*-
 * $Id: List.h,v 1.1 1996/01/10 14:55:28 markus Exp $
 *
 * Purpose: list and list nodes
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

#ifndef List_h
#define List_h

#ifdef __GNUG__
#pragma interface
#endif

class wxList;

class wxNode : public wxObject {
DECLARE_DYNAMIC_CLASS(wxNode)
public:
    wxNode(wxList *the_list=NULL, wxNode *last_one=NULL,
	   wxNode *next_one=NULL, wxObject *object=NULL);
    wxNode(wxList *the_list, wxNode *last_one, wxNode *next_one,
	   wxObject *object, long the_key);
    wxNode(wxList *the_list, wxNode *last_one, wxNode *next_one,
	   wxObject *object, const char *the_key);
    ~wxNode(void);

    inline wxNode   *Next(void)                 { return next; }
    inline wxNode   *Previous(void)             { return previous; }
    inline wxObject *Data(void)                 { return data; }
    inline void     SetData(wxObject *the_data) { data = the_data; }
    inline long     KeyInteger(void)		{ return key.integer; }
    inline char     *KeyString(void)		{ return key.string; }
 
    /* MATTHEW: [5] */
    inline void     DataGCIgnored(void)         { WXGC_IGNORED(data); }
private:
    friend class wxList;

    wxList   *list;
    wxObject *data;
    wxNode   *next;
    wxNode   *previous;
    // Optional key stuff
public: // for compatibility
    union {
	long integer;
	char *string;
    } key;
};

class wxList : public wxObject {
DECLARE_DYNAMIC_CLASS(wxList)
public:
    wxList(void);
    wxList(unsigned int the_key_type);
    wxList(int N, wxObject *Objects[]);
    wxList(wxObject *object, ...);
    ~wxList(void);

    inline int Number(void) { return n; } // number of list items
    inline wxNode *Append(wxObject *object) // Append to end of list
    {
	wxNode *node = new wxNode(this, last_node, NULL, object);
	if (!first_node)
	    first_node = node;
	last_node = node;
	n++;
	return node;
    }
    wxNode *Insert(wxObject *object);    // Insert at front of list
    wxNode *Insert(wxNode *position, wxObject *object); // Insert before node
    wxNode *Append(long key, wxObject *object);  // Keyed append
    wxNode *Append(const char *key, wxObject *object);
    Bool    DeleteNode(wxNode *node);
    Bool    DeleteObject(wxObject *object); // Finds object pointer and
				         // deletes node (and object if
				         // DeleteContents is on)
    void Clear(void);                    // Delete all nodes
    inline wxNode *First(void) { return first_node; }
    inline wxNode *Last(void) { return last_node; }
    wxNode *Nth(int i);                  // nth node counting from 0
    wxNode *Find(long key);              // Keyed search
    wxNode *Find(const char *key);
    wxNode *Member(wxObject *object);
    inline void DeleteContents(int destroy) { destroy_data = destroy; }
				         // Instruct it to destroy user data
				         // when deleting nodes
protected:
    friend class wxNode;

    int          n;
    int          destroy_data;
    wxNode       *first_node;
    wxNode       *last_node;
    unsigned int key_type;
};

#endif // List_h
