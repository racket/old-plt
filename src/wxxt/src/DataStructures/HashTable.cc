/*								-*- C++ -*-
 * $Id: HashTable.cc,v 1.1 1996/01/10 14:55:26 markus Exp $
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

#ifdef __GNUG__
#pragma implementation "HashTable.h"
#endif

#define  Uses_wxHashTable
#include "wx.h"

IMPLEMENT_DYNAMIC_CLASS(wxHashTable, wxObject)

wxHashTable::wxHashTable(int the_key_type, int size)
{
    __type = wxTYPE_HASH_TABLE;

    n = size;
    current_position = -1;
    current_node = NULL;

    key_type = the_key_type;
    hash_table = new (wxList*)[size];
    for (int i = 0; i < size; i++)
	hash_table[i] = NULL;
}

wxHashTable::~wxHashTable(void)
{
    for (size_t i = 0; i < n; i++)
	if (hash_table[i])
	    delete hash_table[i];
    delete hash_table;
}

Bool wxHashTable::Create(int the_key_type, int size)
{
    __type = wxTYPE_HASH_TABLE;
    n = size;
    current_position = -1;
    current_node = NULL;

    key_type = the_key_type;
    if (hash_table)
	delete[] hash_table;
    hash_table = new (wxList*)[size];
    for (int i = 0; i < size; i++)
	hash_table[i] = NULL;
    return TRUE;
}

void wxHashTable::Put(long key, long value, wxObject * object)
{
    // Should NEVER be
    if (key < 0)
	key = -key;
    
    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	hash_table[position] = DEBUG_NEW wxList(wxKEY_INTEGER);

    hash_table[position]->Append(value, object);
}

void wxHashTable::Put(long key, char *value, wxObject * object)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	hash_table[position] = DEBUG_NEW wxList(wxKEY_INTEGER);
    
    hash_table[position]->Append(value, object);
}

void wxHashTable::Put(long key, wxObject * object)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	hash_table[position] = DEBUG_NEW wxList(wxKEY_INTEGER);
    
    hash_table[position]->Append(key, object);
}

void wxHashTable::Put(const char *key, wxObject * object)
{
    size_t position = (size_t)(MakeKey(key) % n);

    if (!hash_table[position])
	hash_table[position] = DEBUG_NEW wxList(wxKEY_STRING);

    hash_table[position]->Append(key, object);
}

wxObject *wxHashTable::Get(long key, long value)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(value);
	if (node)
	    return node->Data();
	else
	    return NULL;
    }
}

wxObject *wxHashTable::Get(long key, char *value)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(value);
	if (node)
	    return node->Data();
	else
	    return NULL;
    }
}

wxObject *wxHashTable::Get(long key)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(key);
	return node ? node->Data() : (wxNode*)NULL;
    }
}

wxObject *wxHashTable::Get(const char *key)
{
    size_t position = (size_t)(MakeKey(key) % n);

    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(key);
	return node ? node->Data() : (wxNode*)NULL;
    }
}

wxObject *wxHashTable::Delete(long key)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(key);
	if (node) {
	    wxObject *data = node->Data();
	    delete node;
	    return data;
	} else
	    return NULL;
    }
}

wxObject *wxHashTable::Delete(const char *key)
{
    size_t position = (size_t)(MakeKey(key) % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(key);
	if (node) {
	    wxObject *data = node->Data();
	    delete node;
	    return data;
	} else
	    return NULL;
    }
}

wxObject *wxHashTable::Delete(long key, int value)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(value);
	if (node) {
	    wxObject *data = node->Data();
	    delete node;
	    return data;
	} else
	    return NULL;
    }
}

wxObject *wxHashTable::Delete(long key, char *value)
{
    // Should NEVER be
    if (key < 0)
	key = -key;

    size_t position = (size_t)(key % n);
    if (!hash_table[position])
	return NULL;
    else {
	wxNode *node = hash_table[position]->Find(value);
	if (node) {
	    wxObject *data = node->Data();
	    delete node;
	    return data;
	} else
	    return NULL;
    }
}

long wxHashTable::MakeKey(const char *string)
{
    long int_key = 0;

    while (*string)
	int_key += (unsigned char)*string++;

    return int_key;
}

void wxHashTable::BeginFind(void)
{
    current_position = -1;
    current_node = NULL;
}

wxNode *wxHashTable::Next(void)
{
    wxNode *found = NULL;
    Bool end = FALSE;
    while (!end && !found) {
	if (!current_node) {
	    current_position++;
	    if (current_position >= (long)n) {
		current_position = -1;
		current_node = NULL;
		end = TRUE;
	    } else {
		if (hash_table[current_position]) {
		    current_node = hash_table[current_position]->First();
		    found = current_node;
		}
	    }
	} else {
	    current_node = current_node->Next();
	    found = current_node;
	}
    }
    return found;
}

void wxHashTable::DeleteContents(Bool flag)
{
    for (int i = 0; i < (long)n; i++) {
	if (hash_table[i])
	    hash_table[i]->DeleteContents(flag);
    }
}

void wxHashTable::Clear (void)
{
    for (int i = 0; i < (long)n; i++) {
	if (hash_table[i])
	    hash_table[i]->Clear();
    }
}
