/*
 * File:		wb_hash.cc
 * Purpose:	Hash table implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:	$Id: wb_hash.cxx,v 1.3 1999/11/13 02:30:26 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#if defined(_MSC_VER)
# include "wx.h"
#else
# ifdef wx_xt
#  define  Uses_wxHashTable
#  include "wx.h"
# else
#  include "common.h"
#  include "wx_list.h"
#  include "wx_hash.h"
#  include "wx_types.h"
# endif
#endif

#include <string.h>
#include <stdarg.h>

wxHashTable::wxHashTable (int, int size)
{
  int i;
  wxList **ll;

  __type = wxTYPE_HASH_TABLE;
  n = size;
  current_position = -1;
  current_node = NULL;

  ll = new wxList *[size];
  hash_table = ll;
  for (i = 0; i < size; i++) {
    hash_table[i] = NULL;
  }
}

wxHashTable::~wxHashTable (void)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i])
      delete hash_table[i];
  }
  delete[] hash_table;
}

void wxHashTable::Put (long key, long value, wxObject * object)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position]) {
    wxList *l;
    l = new wxList (wxKEY_INTEGER, FALSE);
    hash_table[position] = l;
  }

  hash_table[position]->Append (value, object);
}

void wxHashTable::Put (long key, char *value, wxObject * object)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position]) {
    wxList *l;
    l = new wxList (wxKEY_INTEGER, FALSE);
    hash_table[position] = l;
  }

  hash_table[position]->Append (value, object);
}

void wxHashTable::Put (long key, wxObject * object)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position]) {
    wxList *l;
    l = new wxList (wxKEY_INTEGER, FALSE);
    hash_table[position] = l;
  }

  hash_table[position]->Append (key, object);
}

void wxHashTable::Put (const char *key, wxObject * object)
{
  int position;

  position = (int) (MakeKey (key) % n);

  if (!hash_table[position]) {
    wxList *l;
    l = new wxList (wxKEY_STRING, FALSE);
    hash_table[position] = l;
  }

  hash_table[position]->Append (key, object);
}

wxObject *wxHashTable::Get (long key, long value)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node;
      node = hash_table[position]->Find (value);
      if (node)
	return node->Data ();
      else
	return NULL;
    }
}

wxObject *wxHashTable::Get (long key, char *value)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node;
      node = hash_table[position]->Find (value);
      if (node)
	return node->Data ();
      else
	return NULL;
    }
}

wxObject *wxHashTable::Get (long key)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node;
      node = hash_table[position]->Find (key);
      return node ? node->Data() : (wxObject *)NULL;
    }
}

wxObject *wxHashTable::Get (const char *key)
{
  int position;
  position = (int) (MakeKey (key) % n);

  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node;
      node = hash_table[position]->Find (key);
      return node ? node->Data() : (wxObject *)NULL;
    }
}

wxObject *wxHashTable::Delete (long key)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node;
      node = hash_table[position]->Find (key);
      if (node)
	{
	  wxObject *data;
	  data = node->Data ();
	  delete node;
	  return data;
	}
      else
	return NULL;
    }
}

wxObject *wxHashTable::Delete (const char *key)
{
  int position;
  position = (int) (MakeKey (key) % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node;
      node = hash_table[position]->Find (key);
      if (node) {
	  wxObject *data;
	  data = node->Data ();
	  delete node;
	  return data;
      } else
	return NULL;
    }
}

wxObject *wxHashTable::Delete (long key, int value)
{
  int position;

  // Should NEVER be
  if (key < 0)
    key = -key;

  position = (int) (key % n);
  if (!hash_table[position])
    return NULL;
  else {
    wxNode *node;
    node = hash_table[position]->Find (value);
    if (node) {
      wxObject *data;
      data = node->Data ();
      delete node;
      return data;
    } else
      return NULL;
  }
}

wxObject *wxHashTable::Delete (long key, char *value)
{
  int position = (int) (key % n);
  if (!hash_table[position])
    return NULL;
  else {
    wxNode *node;
    node = hash_table[position]->Find (value);
    if (node) {
      wxObject *data;
      data = node->Data ();
      delete node;
      return data;
    } else
      return NULL;
  }
}

long wxHashTable::MakeKey (const char *string)
{
  long int_key = 0;

  while (*string) {
    int_key += (unsigned char) *string++;
  }

/* // Don't need this since int_key >= 0) 
  if (int_key < 0)
    int_key = -int_key;
*/
  return int_key;
}

void wxHashTable::BeginFind (void)
{
  current_position = -1;
  current_node = NULL;
}

wxNode *wxHashTable::Next (void)
{
  wxNode *found = NULL;
  Bool end = FALSE;
  while (!end && !found) {
    if (!current_node) {
      current_position++;
      if (current_position >= n) {
	current_position = -1;
	current_node = NULL;
	end = TRUE;
      } else {
	if (hash_table[current_position]) {
	  current_node = hash_table[current_position]->First ();
	  found = current_node;
	}
      }
    } else {
      current_node = current_node->Next ();
      found = current_node;
    }
  }
  return found;
}

void wxHashTable::DeleteContents (Bool flag)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i])
      hash_table[i]->DeleteContents (flag);
    }
}

void wxHashTable::Clear (void)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i])
      hash_table[i]->Clear ();
  }
}



/* This is a hash table implementation which does not lock the objects
   from garbage collection.
 */

typedef struct Bucket {
  long widget;
  wxObject *object;
} Bucket;

/* because widgets are likely to be word-aligned */
#define HASH(w) ((((unsigned long)w) >> 2) % numbuckets)

#define FILL_FACTOR 2 /* inverted max fraction of hash table implying reash */

#ifdef USE_SENORA_GC
# define GC_MALLOC_ATOMIC GC_malloc_atomic
# define GC_FREE(x) /* empty */
#endif

wxNonlockingHashTable::wxNonlockingHashTable()
{
  long i;

  numbuckets = 1001;
  buckets = (Bucket *)GC_MALLOC_ATOMIC(sizeof(Bucket) * numbuckets);
  for (i = 0; i < numbuckets; i++) {
    buckets[i].widget = 0;
  }
  numwidgets = 0;
}

wxNonlockingHashTable::~wxNonlockingHashTable()
{
  GC_FREE(buckets);
}

void wxNonlockingHashTable::Put(long widget, wxObject *object)
{
  long i;

  if (FILL_FACTOR * numwidgets >= numbuckets) {
    /* Rehash */
    Bucket *oldbuckets = buckets;
    long oldnumbuckets = numbuckets;

    numbuckets = (numbuckets * FILL_FACTOR) + 1;
    buckets = (Bucket *)GC_MALLOC_ATOMIC(sizeof(Bucket) * numbuckets);

    numwidgets = 0;
    for (i = 0; i < oldnumbuckets; i++) {
      if (oldbuckets[i].widget && oldbuckets[i].object)
	Put(oldbuckets[i].widget, oldbuckets[i].object);
    }
    GC_FREE(oldbuckets);
  }

  i = HASH(widget);
  /* MATTHEW: [10] Added equality check (shouldn't happen, though). */
  while (buckets[i].widget && buckets[i].object
	 && (buckets[i].widget != widget)) {
    i = (i + 1) % numbuckets;
  }
  buckets[i].widget = widget;
  buckets[i].object = object;
  numwidgets++; /* Fix counter */
}

wxObject *wxNonlockingHashTable::Get(long widget)
{
  long i;

  i = HASH(widget);
  while ((buckets[i].widget != widget) && buckets[i].widget) {
    i = (i + 1) % numbuckets;
  }

  if (buckets[i].widget == widget)
    return buckets[i].object;

  return NULL;
}

void wxNonlockingHashTable::Delete(long widget)
{
  long i;

  i = HASH(widget);
  while ((buckets[i].widget != widget) && buckets[i].widget) {
    i = (i + 1) % numbuckets;
  }

  if (buckets[i].widget == widget)
  {
    buckets[i].object = NULL;
    --numwidgets; /* Fix counter */
  }
}

/* new method (not very fast) */
void wxNonlockingHashTable::DeleteObject(wxObject *o)
{
  long i;
  
  for (i = 0; i < numbuckets; i++) {
    if (buckets[i].widget && buckets[i].object == o)
      Delete(buckets[i].widget);
  }
}

