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

int gc::gcMark(Mark_Proc /* mark */)
{
  return gcBYTES_TO_WORDS(sizeof(gc));
}

int gc_cleanup::gcMark(Mark_Proc mark)
{
  if (mark) {
    gcMARK(__gc_external);
  }

  return gcBYTES_TO_WORDS(sizeof(gc_cleanup));
}

#include "scheme.h"

typedef struct AllocStackLink {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  void *data;
  void **var_stack;
  AllocStackLink *next;
} AllocStackLink;

AllocStackLink *new_stack;

void *GC_get_current_new()
{
  return new_stack->data;
}

void *GC_pop_current_new()
{
  void *p;
  p = new_stack->data;
  new_stack = new_stack->next;
  return p;
}

void GC_restore_current_new_var_stack()
{
  GC_variable_stack = new_stack->var_stack;
}

static void GC_push_current_new(void *p)
{
  AllocStackLink *link;
  void **vs;

  vs = GC_variable_stack;

  link = (AllocStackLink *)GC_malloc_one_tagged(sizeof(AllocStackLink));
  link->type = scheme_rt_stack_object;
  link->data = p;
  link->var_stack = vs;
  link->next = new_stack;

  new_stack = link;
}

static void *get_new_stack()
{
  return new_stack;
}

static void set_new_stack(void *p)
{
  new_stack = (AllocStackLink *)p;
}

static int mark_cpp_object(void *p, Mark_Proc mark)
{
  gc *obj = (gc *)gcPTR_TO_OBJ(p);

  return obj->gcMark(mark) + 1;
}

static int mark_cpp_array_object(void *p, Mark_Proc mark)
{
  short size, orig_size = ((short *)p)[1];
  void **pp = (void **)gcPTR_TO_OBJ(p);
  gc *obj;

  // skip count
  pp++;
  size = orig_size - 1;

  while (size > 0) {
    size_t s;

    obj = (gc *)pp;
    s = obj->gcMark(mark);

    pp += s;
    size -= s;
  }

  return orig_size + 1;
}

static int mark_stack_object(void *p, Mark_Proc mark)
{
  AllocStackLink *link = (AllocStackLink *)p;

  if (mark) {
    gcMARK_TYPED(void *, link->data);
    gcMARK_TYPED(AllocStackLink *, link->next);
  }

  return gcBYTES_TO_WORDS(sizeof(AllocStackLink));
}

static int mark_preallocated_object(void *p, Mark_Proc /* mark */)
{
  short size = ((short *)p)[1];
  
  return gcBYTES_TO_WORDS(size) + 1;
}

static void *park;
static void *preallocated;
static int use_pre;
static int is_initialized;

void GC_pre_allocate(size_t size)
{
  if (preallocated) {
    printf("ERROR: preallocated already waiting\n");
    abort();
  }

  preallocated = GC_malloc_one_tagged(size + sizeof(long));
  ((short *)preallocated)[0] = scheme_rt_preallocated_object;
  ((short *)preallocated)[1] = (short)size;
}

void GC_use_preallocated()
{
  use_pre = 1;
}

static void initize(void)
{
  /* Initialize: */
  wxREGGLOB(park);
  wxREGGLOB(new_stack);
  wxREGGLOB(preallocated);
  
  scheme_get_external_stack_val = get_new_stack;
  scheme_set_external_stack_val = set_new_stack;
  
  GC_register_traverser(scheme_rt_cpp_object, mark_cpp_object);
  GC_register_traverser(scheme_rt_cpp_array_object, mark_cpp_array_object);
  GC_register_traverser(scheme_rt_stack_object, mark_stack_object);
  GC_register_traverser(scheme_rt_preallocated_object, mark_preallocated_object);
  
  is_initialized = 1;
}

void *GC_cpp_malloc(size_t size)
{
  void *p;

  if (!is_initialized) {
    initize();
  }

  if (use_pre) {
    if (((short *)preallocated)[1] != (short)size) {
      printf("ERROR: preallocated wrong size\n");
      abort();
    }
    p = preallocated;
    use_pre = 0;
    preallocated = NULL;
  } else {
    p = GC_malloc_one_tagged(size + sizeof(long));
  }

  /* push_new might trigger a gc: */
  ((short *)p)[0] = scheme_rt_preallocated_object;
  ((short *)p)[1] = (short)size;
  park = p;

  GC_push_current_new(gcPTR_TO_OBJ(p));

  p = park;
  ((short *)p)[0] = scheme_rt_cpp_object;

  return gcPTR_TO_OBJ(p);
}

void GC_cpp_delete(gc *v)
{
  size_t size;
  void *p;

  size = v->gcMark(NULL);
  v->~gc();
  
  p = gcOBJ_TO_PTR(v);
  ((short *)p)[0] = scheme_rt_preallocated_object;
  ((short *)p)[1] = (short)gcWORDS_TO_BYTES(size);
}

void *GC_cpp_malloc_array(size_t size)
{
  void *p;

  if (!is_initialized) {
    initize();
  }

  p = GC_malloc_one_tagged(size + sizeof(long));

  ((short *)p)[0] = scheme_rt_cpp_array_object;
  ((short *)p)[1] = (short)gcBYTES_TO_WORDS(size);

  return gcPTR_TO_OBJ(p);
}

#endif
