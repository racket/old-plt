/*								-*- C++ -*-
 * $Id: List.cc,v 1.1 1996/01/10 14:55:28 markus Exp $
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

#ifdef __GNUG__
#pragma implementation "List.h"
#endif

#define  Uses_wxList
#include "wx.h"

#include <stdarg.h>

/*
 *
 * wxNode
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxNode, wxObject)

wxNode::wxNode(wxList * the_list, wxNode * last_one, wxNode * next_one,
	       wxObject * object)
{
    __type = wxTYPE_NODE;

    data = object;
    previous = last_one;
    next = next_one;
    list = the_list;
    key.string = NULL;
    if (previous)
	previous->next = this;
    if (next)
	next->previous = this;

    WXGC_IGNORE(list);
    WXGC_IGNORE(next);
}

// Keyed constructor
wxNode::wxNode(wxList * the_list, wxNode * last_one, wxNode * next_one,
		wxObject * object, long the_key)
{
    __type = wxTYPE_NODE;

    data = object;
    previous = last_one;
    next = next_one;
    list = the_list;
    key.integer = the_key;
    if (previous)
	previous->next = this;
    if (next)
	next->previous = this;

    WXGC_IGNORE(list);
    WXGC_IGNORE(next);
}

wxNode::wxNode(wxList * the_list, wxNode * last_one, wxNode * next_one,
		wxObject * object, const char *the_key)
{
    __type = wxTYPE_NODE;

    data = object;
    previous = last_one;
    next = next_one;
    list = the_list;
    key.string = copystring (the_key);
    if (previous)
	previous->next = this;
    if (next)
	next->previous = this;

    WXGC_IGNORE(list);
    WXGC_IGNORE(next);
}

wxNode::~wxNode(void)
{
    if (list)
	list->n--;
    if (list && list->destroy_data)
	delete data;
    if (list && list->key_type == wxKEY_STRING && key.string)
	delete[]key.string;
    // Make next node point back to the previous node from here
    if (next)
	next->previous = previous;
    else if (list)
	// If there's a new end of list (deleting the last one)
	// make sure the list knows about it.
	list->last_node = previous;
    // Make the previous node point to the next node from here
    if (previous)
	previous->next = next;
    // Or if no previous node (start of list), make sure list points at
    // the next node which becomes the first!.
    else if (list)
	list->first_node = next;
}

/*
 *
 * wxList
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxList, wxObject)

wxList::wxList(void)
{
    __type = wxTYPE_LIST;

    first_node = NULL;
    last_node = NULL;
    n = 0;
    destroy_data = 0;
    key_type = wxKEY_NONE;
}

wxList::wxList(int N, wxObject * Objects[])
{
    __type = wxTYPE_LIST;

    wxNode *last = NULL;
    
    for (int i = 0; i < N; i++)	{
	wxNode *next = DEBUG_NEW wxNode (this, last, NULL, Objects[i]);
	last = next;
	if (i == 0)
	    first_node = next;
    }
    last_node = last;
    n = N;
    key_type = wxKEY_NONE;
}

wxList::wxList(unsigned int the_key_type)
{
    __type = wxTYPE_LIST;

    n = 0;
    destroy_data = 0;
    first_node = NULL;
    last_node = NULL;
    key_type = the_key_type;
}

// Variable argument list, terminated by a zero
wxList::wxList(wxObject * first_one...)
{
    __type = wxTYPE_LIST;

    va_list  ap;
    va_start(ap, first_one);
    wxNode *last = DEBUG_NEW wxNode (this, NULL, NULL, first_one);
    first_node = last;
    n = 1;

    for (;;) {
	wxObject *object = va_arg (ap, wxObject *);
	if (object == NULL)
	    break;
	else {
	    wxNode *node = DEBUG_NEW wxNode (this, last, NULL, object);
	    last = node;
	    n++;
	}
    }
    last_node = last;
    va_end (ap);

    destroy_data = 0;
    key_type = wxKEY_NONE;
}

wxList::~wxList(void)
{
    wxNode *each = first_node;
    while (each) {
	wxNode *next = each->Next ();
	delete each;
	each = next;
    }
}

wxNode *wxList::Nth(int i)
{
    int j = 0;
    for (wxNode * current = First (); current; current = current->Next ())
	if (j++ == i)
	    return current;
    return NULL;  // No such element
}

wxNode *wxList::Find(long key)
{
    for (wxNode * current = First (); current; current = current->Next ())
	if (current->key.integer == key)
	    return current;
    return NULL;  // Not found!
}

wxNode *wxList::Find(const char *key)
{
    for (wxNode * current = First (); current; current = current->Next ()) {
	if (!current->key.string)	{
	    wxFatalError ("wxList: string key not present, probably did not Append correctly!");
	    break;
	}
	if (strcmp (current->key.string, key) == 0)
	    return current;
    }			// for
    return NULL;	// Not found!

}

wxNode *wxList::Member (wxObject *object)
{
    for (wxNode * current = First (); current; current = current->Next ()) {
	wxObject *each = current->Data ();
	if (each == object)
	    return current;
    }
    return NULL;
}

Bool wxList::DeleteNode(wxNode *node)
{
    if (node) {
	delete node;
	return TRUE;
    }
    return FALSE;
}

Bool wxList::DeleteObject (wxObject *object)
{
    for (wxNode * current = first_node; current; current = current->Next ()) {
	if (current->Data () == object) {
	    delete current;
	    return TRUE;
	}
    }
    return FALSE;  // Did not find the object
}


// Insert new node at front of list
wxNode *wxList::Insert(wxObject *object)
{
    wxNode *node = DEBUG_NEW wxNode (this, NULL, First (), object);
    first_node = node;

    if (!(node->Next ()))
	last_node = node;
    n++;
    return node;
}

// Insert new node before given node.
wxNode *wxList::Insert(wxNode *position, wxObject *object)
{
    wxNode *prev = NULL;
    if (position)
	prev = position->Previous ();
    wxNode *node = DEBUG_NEW wxNode (this, prev, position, object);
    if (!first_node) {
	first_node = node;
	last_node = node;
    }
    if (!prev)
	first_node = node;
    n++;
    return node;
}

// Keyed append
wxNode *wxList::Append(long key, wxObject *object)
{
    wxNode *node = DEBUG_NEW wxNode(this, last_node, NULL, object, key);
    if (!first_node)
	first_node = node;
    last_node = node;
    n++;
    return node;
}

wxNode *wxList::Append(const char *key, wxObject *object)
{
    wxNode *node = DEBUG_NEW wxNode (this, last_node, NULL, object, key);
    if (!first_node)
	first_node = node;
    last_node = node;
    n++;
    return node;
}

void wxList::Clear(void)
{
    wxNode *current = first_node;
    while (current) {
	wxNode *next = current->Next ();
	delete current;
	current = next;
    }
    n = 0;
}
