/*
 * File:		wb_list.cc
 * Purpose:	List implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:	$Id: wb_list.cc,v 1.3 1998/02/07 13:43:59 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation "wx_list.h"
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    /* static const char sccsid[] = "@(#)wb_list.cc	1.2 5/9/94"; */
    // For compilers that support precompilation, includes "wx.h".
// #   include "wx_prec.h"
#   ifdef __BORLANDC__
#	pragma hdrstop
#   endif
#   ifndef WX_PRECOMP
#	include "common.h"
#	include "wx_list.h"
#	include "wx_utils.h"
#   endif
    // Sun CC compatibility (interference with xview/pkg.h, apparently...)
#   if defined(SUN_CC) && defined(wx_xview)
#	undef va_start
#	undef va_end
#	undef va_arg
#	undef va_list
#   endif
#   include <stdlib.h>
#else // wx_xt
    // The Xt port uses another include mechanism
#   define  Uses_wxList
#   define  Uses_wxStringList
#   include "wx.h"
#endif // #ifndef wx_xt

#include <stdarg.h>
#include <string.h>

wxNode::wxNode (wxList *, wxNode * last_one, wxNode * next_one,
	wxObject * object)
{
  data = object;
  previous = last_one;
  next = next_one;
  key.string = NULL;

  if (previous)
    previous->next = this;

  if (next)
    next->previous = this;
}

// Keyed constructor
wxNode::wxNode (wxList *, wxNode * last_one, wxNode * next_one,
	wxObject * object, long the_key)
{
  data = object;
  previous = last_one;
  next = next_one;
  key.integer = the_key;

  if (previous)
    previous->next = this;

  if (next)
    next->previous = this;
}

wxNode::wxNode (wxList *, wxNode * last_one, wxNode * next_one,
	wxObject * object, const char *the_key)
{
  data = object;
  previous = last_one;
  next = next_one;
  key.string = copystring (the_key);

  if (previous)
    previous->next = this;

  if (next)
    next->previous = this;
}


void wxNode::Kill(wxList *list)
{
  if (list)
    list->n--;

#ifndef wx_mac
  if (list && list->destroy_data)
    delete data;
#endif

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

  next = previous = NULL;
  list = NULL;
  
#ifdef wx_mac
  // Put this here since data may try to remove itself as a child: kludge
  if (list && list->destroy_data)
    delete data;
#endif // wx_mac
}

#ifdef wx_mac
wxList::wxList(DestroyDataCode destroyData, Bool clean_up)
: wxObject(clean_up)
{
  __type = wxTYPE_LIST;
  first_node = NULL;
  last_node = NULL;
  n = 0;
  destroy_data = destroyData;
  key_type = wxKEY_NONE;
}
#else // wx_mac
wxList::wxList (void)
{
  __type = wxTYPE_LIST;
  first_node = NULL;
  last_node = NULL;
  n = 0;
  destroy_data = 0;
  key_type = wxKEY_NONE;
}
#endif // wx_mac

wxList::wxList (int N, wxObject * Objects[])
{
  __type = wxTYPE_LIST;
  wxNode *last = NULL;

  int i;
  for (i = 0; i < N; i++)
    {
      wxNode *next = new wxNode (this, last, NULL, Objects[i]);
      last = next;
      if (i == 0)
	first_node = next;
    }
  last_node = last;
  n = N;
  key_type = wxKEY_NONE;
}

wxList::wxList(KeyType the_key_type, Bool clean_up)
: wxObject(clean_up)
{
  __type = wxTYPE_LIST;
  n = 0;
  destroy_data = 0;
  first_node = NULL;
  last_node = NULL;
  key_type = the_key_type;
}

wxList::~wxList (void)
{
  wxNode *each = first_node;
  while (each)
    {
      wxNode *next = each->Next ();

      each->Kill(this);
      delete each;

      each = next;
    }

  first_node = last_node = NULL;
}

wxNode *wxList::Nth (int i)
{
  int j = 0;
  for (wxNode * current = First (); current; current = current->Next ())
    {
      if (j++ == i)
	return current;
    }
  return NULL;			// No such element

}

wxNode *wxList::Find (long key)
{
  wxNode *current = First();
  while (current)
  {
    if (current->key.integer == key)
      return current;
    current = current->Next();
  }
    
  return NULL;			// Not found!
}

wxNode *wxList::Find (const char *key)
{
  wxNode *current = First();
  while (current)
  {
      if (!current->key.string)
	{
	  wxFatalError ("wxList: string key not present, probably did not Append correctly!");
	  break;
	}
      if (strcmp (current->key.string, key) == 0)
	return current;
      current = current->Next();
  }

  return NULL;			// Not found!

}

wxNode *wxList::Member (wxObject * object)
{
  for (wxNode * current = First (); current; current = current->Next ())
    {
      wxObject *each = current->Data ();
      if (each == object)
	return current;
    }
  return NULL;
}

Bool wxList::DeleteNode (wxNode * node)
{
  if (node)
    {
      node->Kill(this);
      delete node;
      return TRUE;
    }
  return FALSE;
}

Bool wxList::DeleteObject (wxObject * object)
{
  // Search list for object
  for (wxNode * current = first_node; current; current = current->Next ())
    {
      if (current->Data () == object)
	{
	  current->Kill(this);
	  delete current;
	  return TRUE;
	}
    }
  return FALSE;			// Did not find the object

}


// Insert new node at front of list
wxNode *wxList::Insert (wxObject * object)
{
  wxNode *node = new wxNode (this, NULL, First (), object);
  first_node = node;

  if (!(node->Next ()))
    last_node = node;

  n++;
  return node;
}


// Insert new node before given node.
wxNode *wxList::Insert (wxNode * position, wxObject * object)
{
  wxNode *prev = NULL;
  if (position)
    prev = position->Previous ();

  wxNode *node = new wxNode (this, prev, position, object);
  if (!first_node)
    {
      first_node = node;
      last_node = node;
    }
  if (!prev)
    first_node = node;

  n++;
  return node;
}

// Keyed append
wxNode *wxList::Append (long key, wxObject * object)
{
  wxNode *node = new wxNode (this, last_node, NULL, object, key);
  if (!first_node)
    first_node = node;
  last_node = node;
  n++;
  return node;
}

wxNode *wxList::Append (const char *key, wxObject * object)
{
  wxNode *node = new wxNode (this, last_node, NULL, object, key);
  if (!first_node)
    first_node = node;
  last_node = node;
  n++;
  return node;
}

void wxList::Clear (void)
{
  wxNode *current = first_node;
  while (current)
    {
      wxNode *next = current->Next ();
      delete current;
      current = next;
    }
  first_node = NULL;
  last_node = NULL;
  n = 0;
}

/*
 * String list
 *
 */

wxStringList::wxStringList (void):
wxList ()
{
  __type = wxTYPE_STRING_LIST;
}

#ifdef MEMORY_USE_METHOD
long wxList::MemoryUse(void)
{
  wxNode *node;
  long s = 0;

  for (node = First(); node; node = node->Next())
    s += sizeof(wxNode);
  
  return s + wxObject::MemoryUse();
}
#endif

wxStringList::~wxStringList (void)
{
  wxNode *each = first_node;
  while (each)
    {
      char *s = (char *) each->Data ();
      delete[]s;
      wxNode *next = each->Next ();
      delete each;
      each = next;
    }
}

wxNode *wxStringList::Add (const char *s)
{
  return Append ((wxObject *) (copystring (s)));
}

void wxStringList::Delete (const char *s)
{
  for (wxNode * node = First (); node; node = node->Next ())
    {
      char *string = (char *) node->Data ();
      if (string == s || strcmp (string, s) == 0)
	{
	  delete[]string;
	  delete node;
	  break;		// Done!

	}
    }				// for

}

// Only makes new strings if arg is TRUE
char **wxStringList::ListToArray (Bool new_copies)
{
  char **string_array = new char *[Number ()];
  wxNode *node = First ();
  int i;
  for (i = 0; i < n; i++)
    {
      char *s = (char *) node->Data ();
      if (new_copies)
	string_array[i] = copystring (s);
      else
	string_array[i] = s;
      node = node->Next ();
    }
  return string_array;
}

// Checks whether s is a member of the list
Bool wxStringList::Member (const char *s)
{
  for (wxNode * node = First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) node->Data ();
      if (s == s1 || strcmp (s, s1) == 0)
	return TRUE;
    }
  return FALSE;
}

/****************************************************************************/

wxChildNode* wxChildNode::Next()
{
  return owner->FindNode(this);
}

wxObject* wxChildNode::Data()
{
  if (strong)
    return strong;
  else if (weak)
    return *weak;
  else
    return NULL;
}

Bool wxChildNode::IsShown()
{
  return strong ? TRUE : FALSE;
}

wxChildList::wxChildList()
{
  n = 0;
  size = 0;
  nodes = NULL;
}

wxChildList::~wxChildList()
{
  
}

void wxChildList::Append(wxObject *object)
{
  int i;
  wxChildNode *cn = new wxChildNode, **naya;

  cn->owner = this;
  cn->strong = object;
  cn->weak = NULL;
  
  for (i = 0; i < size; i++)
    if (!nodes[i]) {
      nodes[i] = cn;
      n++;
      return;
    }

  size = (size * 2) + 20;
  naya = new wxChildNode* [size];
  for (i = 0; i < n; i++)
    naya[i] = nodes[i];

  nodes = naya;
  nodes[n++] = cn;
}

Bool wxChildList::DeleteObject(wxObject *object)
{
  int i;

  for (i = 0; i < size; i++)
    if (nodes[i] && (nodes[i]->Data() == object)) {
      nodes[i]->strong = NULL;
      nodes[i]->weak = NULL;
      nodes[i] = NULL;
      n--;

      return TRUE;
    }

  return FALSE;
}

#ifdef wx_mac
long wxList::MemberIndex(wxObject *object) // WCH wx_mac added 8/12/94
{
  long result = 0;
  wxNode *current = First();
  wxNode *found = NULL;
  while (current && !found)
  {
    wxObject *each = current->Data();
    if (each == object)
      found = current;
    else
    {
    	current = current->Next();
    	result++;
    }
  }
  return (found ? result : -1);
}

Bool wxList::OnDeleteObject(wxObject *object)  // mac platform only
{
  Bool result;

  wxNode *current = first_node;
  while (current)
  {
    if (current->Data() == object)
      break;
    else current = current->Next();
  }

  if (current)
  {
	int destroy_data_saved = destroy_data; // kludge
	destroy_data = kNoDestroyData; // kludge

    delete current;
    
	destroy_data = destroy_data_saved; // kludge

    result = TRUE;
  }
  else result = FALSE;

  return result;
}
#endif // wx_mac

Bool wxChildList::DeleteNode(wxChildNode *node)
{
  int i;

  for (i = 0; i < size; i++)
    if (nodes[i] == node) {
      nodes[i]->strong = NULL;
      nodes[i]->weak = NULL;
      nodes[i] = NULL;
      n--;

      return TRUE;
    }

  return FALSE;
}

wxChildNode *wxChildList::FindNode(wxChildNode *after)
{
  int i;

  if (after) {
    for (i = 0; i < size; i++)
      if (nodes[i] == after)
	break;
    i++;
  } else
    i = 0;

  return NextNode(i);
}

wxChildNode *wxChildList::NextNode(int &pos)
{
  int i;

  for (i = pos; i < size; i++) {
    if (nodes[i]) {
      wxChildNode *node = nodes[i];
      
      if (node->Data()) {
	pos = i + 1;
	return node;
      }
      /* GC: */
      node->strong = NULL;
      node->weak = NULL;
      nodes[i] = NULL;
      n--;
    }
  }

  return NULL;
}

void wxChildList::Show(wxObject *object, int show)
{
  int i;

  for (i = 0; i < size; i++)
    if (nodes[i] && (nodes[i]->Data() == object)) {
      wxChildNode *node = nodes[i];

      if (show > 0) {
	if (node->strong)
	  return;
	node->strong = object;
	node->weak = NULL;
      } else {
	if (node->weak)
	  return;
	node->weak = new WXGC_ATOMIC wxObject*;
	*node->weak = object;
	if (show < 0)
	  GC_general_register_disappearing_link((void **)node->weak, object);
	node->strong = NULL;
      }
      return;
    }
}

Bool wxChildList::IsShown(wxObject *object)
{
  int i;

  for (i = 0; i < size; i++)
    if (nodes[i] && (nodes[i]->Data() == object)) {
      wxChildNode *node = nodes[i];

      return (node->strong) ? TRUE : FALSE;
    }

  return FALSE;
}

