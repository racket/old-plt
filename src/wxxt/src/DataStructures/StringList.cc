/*								-*- C++ -*-
 * $Id: StringList.cc,v 1.1 1996/01/10 14:55:35 markus Exp $
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

#ifdef __GNUG__
#pragma implementation "StringList.h"
#endif

#define  Uses_wxStringList
#include "wx.h"

#include <stdarg.h> // for variable arg list

IMPLEMENT_DYNAMIC_CLASS(wxStringList, wxList)

wxStringList::wxStringList(void) : wxList()
{
    __type = wxTYPE_STRING_LIST;
};

// Variable argument list, terminated by a zero
// Makes new storage for the strings
wxStringList::wxStringList(char *first...)
{
    __type = wxTYPE_STRING_LIST;

    n = 0;
    destroy_data = 0;
    key_type = wxKEY_NONE;
    first_node = NULL;
    last_node = NULL;

    if (!first)
	return;
    va_list ap;
    va_start (ap, first);

    wxNode *last = DEBUG_NEW wxNode(this, NULL, NULL, (wxObject*)copystring(first));
    first_node = last;
    n = 1;
    for (;;) {
	char *s = va_arg (ap, char *);
	if (s == NULL)
	    break;
	else {
	    wxNode *node = DEBUG_NEW wxNode(this, last, NULL,
					    (wxObject*)copystring(s));
	    last = node;
	    n++;
	}
    }
    last_node = last;
    va_end (ap);
}

wxStringList::~wxStringList(void)
{
    wxNode *each = first_node;
    while (each) {
	char *s = (char *) each->Data ();
	delete[]s;
	wxNode *next = each->Next ();
	delete each;
	each = next;
    }
}

wxNode *wxStringList::Add(const char *s)
{
    return Append((wxObject *)(copystring (s)));
}

void wxStringList::Delete(const char *s)
{
    for (wxNode *node = First(); node; node = node->Next()) {
	char *string = (char *) node->Data ();
	if (string == s || strcmp (string, s) == 0) {
	    delete[]string;
	    delete node;
	    break;		// Done!
	}
    }				// for
}

// Only makes new strings if arg is TRUE
char **wxStringList::ListToArray(Bool new_copies)
{
    char **string_array = new char *[Number ()];
    wxNode *node = First ();
    for (int i = 0; i < n; i++) {
	char *s = (char *) node->Data ();
	if (new_copies)
	    string_array[i] = copystring (s);
	else
	    string_array[i] = s;
	node = node->Next ();
    }
    return string_array;
}

static int wx_comparestrings(const void *arg1, const void *arg2)
{
  char **s1 = (char **)arg1;
  char **s2 = (char **)arg2;

  return strcmp (*s1, *s2);
}

// Sort a list of strings - deallocates old nodes, allocates new
void wxStringList::Sort (void)
{
    size_t N = n;
    char **array = new char *[N];
    size_t i = 0;
    for (wxNode * node = First (); node; node = node->Next ())
	array[i++] = (char *) node->Data ();
    qsort (array, N, sizeof (char *), wx_comparestrings);
    Clear ();
    for (i = 0; i < N; i++)
	Append ((wxObject *) (array[i]));
    delete[]array;
}

// Checks whether s is a member of the list
Bool wxStringList::Member (const char *s)
{
    for (wxNode * node = First (); node; node = node->Next ()) {
	const char *s1 = (const char *) node->Data ();
	if (s == s1 || strcmp (s, s1) == 0)
	    return TRUE;
    }
    return FALSE;
}
