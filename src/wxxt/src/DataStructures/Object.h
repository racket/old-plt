/*								-*- C++ -*-
 *
 * Purpose: Top level object and memory debugging for wxWindows
 *
 * Authors: Markus Holzem, Julian Smart and Arthur Seaton
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian, Arthur)
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

#ifndef Object_h
#define Object_h

#ifndef MZ_PRECISE_GC
# ifdef __GNUG__
# pragma interface
# endif
#endif

//-----------------------------------------------------------------------------
// wxObject: top level object
//-----------------------------------------------------------------------------

#ifdef MZ_PRECISE_GC
# define WXGC_IGNORE(ptr) GC_finalization_weak_ptr((void **)&(ptr))
# define WXGC_ATOMIC /* empty */
# define DELETE_OBJ delete_wxobject
# define DELETE_VAL delete
#else
# define WXGC_IGNORE(ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
# define WXGC_ATOMIC (AtomicGC)
# define DELETE_OBJ delete
# define DELETE_VAL delete
#endif
#define WXGC_NO_CLEANUP FALSE

class wxObject : public gc_cleanup
{
public:
  wxObject(void);
  wxObject(Bool cleanup);
  virtual ~wxObject(void);
  
  WXTYPE __type;
  
#ifdef MEMORY_USE_METHOD
  virtual long MemoryUse(void);
#endif
};


#define wxASSERT(ignore1, ignore2) ((void) 0)

#define DEBUG_NEW new

#endif // Object_h
