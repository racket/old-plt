/*************************************************************************
Based On:

Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
    Last modified on Sat Nov 19 19:31:14 PST 1994 by ellis
                  on Sat Jun  8 15:10:00 PST 1994 by boehm

Permission is hereby granted to copy this code for any purpose,
provided the above notices are retained on all copies.

Authors: John R. Ellis and Jesse Hull

**************************************************************************/
/* Boehm, December 20, 1994 7:26 pm PST */

#include "wxGC.h"

void *operator new(size_t size)
{
#ifdef USE_SENORA_GC
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);

  return GC_malloc_specific(size, cpp_objects);
#else
  return GC_malloc(size);
#endif
}

void operator delete(void *obj)
{
}

extern "C" {
  void GC_cleanup(void *obj, void *displ)
  {
    gc *clean = ((gc *)((char *)obj + (ptrdiff_t)displ));
    clean->~gc();
  }
}

/**********************************************************************/  

#ifdef OPERATOR_NEW_ARRAY

void* operator new[](size_t size)
{
#ifdef USE_SENORA_GC
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);
  
  return GC_malloc_specific(size, cpp_objects);
#else
  return GC_malloc(size);
#endif
}
  
void operator delete[](void *obj)
{
}

#endif

/**********************************************************************/

#ifdef USE_SENORA_GC

struct GC_Set *wx_objects, *cpp_objects;

# ifdef USE_WXOBJECT_TRACE_COUNTER
extern void wxTraceCount(void *, int);
extern void wxTracePath(void *, unsigned long, void *);
extern void wxTraceInit(void);
extern void wxTraceDone(void);
extern void wxObjectFinalize(void *);
# endif

void *GC_cpp_malloc(size_t size)
{
  if (!wx_objects)
    wx_objects = GC_new_set("wxObjects", 
# ifdef USE_WXOBJECT_TRACE_COUNTER
			    wxTraceInit,
			    wxTraceDone,
			    wxTraceCount,
			    wxTracePath,
			    wxObjectFinalize,
# else
			    NULL, NULL, NULL, NULL, NULL,
# endif
			    0);

  return GC_malloc_specific(size, wx_objects);
}

void GC_cpp_for_each(void (*f)(void *, int, void *), void *data)
{
  if (wx_objects)
    GC_for_each_element(wx_objects, f, data);
}

int GC_is_wx_object(void *v)
{
  return wx_objects && (GC_set(v) == wx_objects);
}

#endif
