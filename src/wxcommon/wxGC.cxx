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

void operator delete(void * /*obj*/)
{
}

void gc_cleanup::install_cleanup(void)
{
  GC_finalization_proc old_fn;
  void *old_data;

# ifdef MZ_PRECISE_GC
#  define ALLOW_NON_BASE 0
#  define CHECK_BASE 0
# else
#  ifdef wx_xt
#   define ALLOW_NON_BASE 0
#   define CHECK_BASE 1
#  else
#   define ALLOW_NON_BASE 1
#   define CHECK_BASE 0
#  endif
# endif

# if CHECK_BASE || ALLOW_NON_BASE
  if (GC_base(this) != (void *)this) {
#  if ALLOW_NON_BASE
    return;
#  else
    printf("Clean-up object is not the base object\n");
    abort();
#  endif
  }
# endif

  GC_register_finalizer_ignore_self(gcOBJ_TO_PTR(this), 
				    GC_cleanup, NULL, 
				    &old_fn, &old_data);

# if CHECK_BASE
  if (old_fn) {
    printf("Object already has a clean-up\n");
    abort();
  }
# endif
}

void GC_cleanup(void *obj, void *)
{
  gc *clean = (gc *)gcPTR_TO_OBJ(obj);

#ifdef MZ_PRECISE_GC
  GC_cpp_delete(clean);
#else
  clean->~gc();
#endif
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
  
void operator delete[](void * /*obj*/)
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

/**********************************************************************/

#ifdef MZ_PRECISE_GC

void gc::gcMark()
{
}

void gc::gcFixup()
{
}

void gc_cleanup::gcMark()
{
  gcMARK(__gc_external);
}

void gc_cleanup::gcFixup()
{
  gcFIXUP(__gc_external);
}

#include "scheme.h"

static int size_cpp_object(void *p)
{
  short size = ((short *)p)[1];
  return size + 1;
}

static int mark_cpp_object(void *p)
{
  short size = ((short *)p)[1];
  gc *obj = (gc *)gcPTR_TO_OBJ(p);

  obj->gcMark();

  return size + 1;
}

static int fixup_cpp_object(void *p)
{
  short size = ((short *)p)[1];
  gc *obj = (gc *)gcPTR_TO_OBJ(p);

  obj->gcFixup();

  return size + 1;
}

static int size_cpp_array_object(void *p)
{
  short orig_size = ((short *)p)[1];

  return orig_size + 1;
}

static int do_cpp_array_object(void *p, int fixup)
{
  short size, orig_size = ((short *)p)[1];
  void **pp = (void **)gcPTR_TO_OBJ(p);
  gc *obj;
  size_t s;

  size = orig_size;
  s = size / (*(long *)pp);
  
  /* FIXME: the count stuff is probably g++-specific: */
  // skip count
  pp++;
#if gcALIGN_DOUBLE
  // double alignment...
  pp++;
#endif

  while (size > 0) {
    obj = (gc *)pp;
    if (fixup)
      obj->gcFixup();
    else
      obj->gcMark();

    pp += s;
    size -= s;
  }

  return orig_size + 1;
}

static int mark_cpp_array_object(void *p)
{
  return do_cpp_array_object(p, 0);
}

static int fixup_cpp_array_object(void *p)
{
  return do_cpp_array_object(p, 1);
}

static int size_preallocated_object(void *p)
{
  short size = ((short *)p)[1];
  
  return size + 1;
}

static int is_initialized;

static void initize(void)
{
  /* Initialize: */
  GC_register_traversers(scheme_rt_cpp_object, 
			 size_cpp_object,
			 mark_cpp_object,
			 fixup_cpp_object);
  GC_register_traversers(scheme_rt_cpp_array_object,
			 size_cpp_array_object,
			 mark_cpp_array_object,
			 fixup_cpp_array_object);
  GC_register_traversers(scheme_rt_preallocated_object,
			 size_preallocated_object,
			 size_preallocated_object,
			 size_preallocated_object);
  
  is_initialized = 1;
}

void *GC_cpp_malloc(size_t size)
{
  void *p;

  if (!is_initialized) {
    initize();
  }

  p = GC_malloc_one_tagged(size + sizeof(AlignedType));

  ((short *)p)[0] = scheme_rt_cpp_object;
  ((short *)p)[1] = (short)gcBYTES_TO_WORDS(size);

  return gcPTR_TO_OBJ(p);
}

void GC_cpp_delete(gc *v)
{
  void *p;

  v->~gc();
  
  p = gcOBJ_TO_PTR(v);
  ((short *)p)[0] = scheme_rt_preallocated_object;
}

void *GC_cpp_malloc_array(size_t size)
{
  void *p;

  if (!is_initialized) {
    initize();
  }

  p = GC_malloc_one_tagged(size + sizeof(AlignedType));

  ((short *)p)[0] = scheme_rt_cpp_array_object;
  ((short *)p)[1] = (short)gcBYTES_TO_WORDS(size);

  return gcPTR_TO_OBJ(p);
}

#endif
