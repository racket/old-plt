#ifndef GC_CPP_H
#define GC_CPP_H

/****************************************************************************
Based On:

C++ Interface to the Boehm Collector

    John R. Ellis and Jesse Hull 
    Last modified on Wed Jan  4 16:30:20 PST 1995 by ellis


Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
Permission is hereby granted to use or copy this program for any
purpose, provided the above notices are retained on all copies.
Permission to modify the code and to distribute modified code is
granted, provided the above notices are retained, and a notice that
the code was modified is included with the above copyright notice.
****************************************************************************/

enum GCPlacement {UseGC, AtomicGC};

typedef void (*GCCleanUpFunc)(void* obj, void* clientData);

extern "C" {
  void gc_mark_external_invalid(void *);
};
void GC_cleanup(void *obj, void *ignored);

#include "gc.h"

/******************** Special kinds of GC *********************/

#ifdef USE_SENORA_GC
extern void *GC_cpp_malloc(size_t);
#endif

#if defined(MZ_PRECISE_GC) || defined(USE_SENORA_GC)
# ifndef WX_REGISTER_GLOBAL_MEMORY
#  define WX_REGISTER_GLOBAL_MEMORY
# endif
#endif

#ifdef WX_REGISTER_GLOBAL_MEMORY
extern "C" {
  void scheme_register_extension_global(void *p, long size);
}
# define wxREGGLOB(x) scheme_register_extension_global((void *)&x, sizeof(x))
#else
# define wxREGGLOB(x) /* empty */
#endif

#ifdef MZ_PRECISE_GC
extern void *GC_cpp_malloc(size_t);
extern void *GC_get_current_new();
extern void *GC_pop_current_new();
# define GC_register_finalizer_ignore_self GC_register_finalizer
#endif


#ifdef MZ_PRECISE_GC
# define gcOBJ_TO_PTR(x) ((char *)x - 4)
# define gcPTR_TO_OBJ(x) ((char *)x + 4)
#else
# define gcOBJ_TO_PTR(x) x
# define gcPTR_TO_OBJ(x) x
#endif

/**** The `gc' and `gc_cleanup' class ************/

class gc
{
public:
  inline virtual ~gc();

  inline void *operator new(size_t size);
  inline void *operator new(size_t size, GCPlacement gcp);
  inline void operator delete(void *obj);
#ifdef OPERATOR_NEW_ARRAY
  inline void *operator new[](size_t size);
  inline void *operator new[](size_t size, GCPlacement gcp);
  inline void operator delete[](void *obj);
#endif

#ifdef MZ_PRECISE_GC
  /* Overridden in each subclass: */
  virtual int gcMark(Mark_Proc mp);
#endif
};

class gc_cleanup : public gc
{
public:
  void *__gc_external;

  inline gc_cleanup();
  inline gc_cleanup(int cleanup);
  inline virtual ~gc_cleanup();
  void install_cleanup();  

#ifdef MZ_PRECISE_GC
  int gcMark(Mark_Proc mp);
#endif
};

/***** Constructors and Destructors: ******/

inline gc_cleanup::gc_cleanup(void)
{
  __gc_external = NULL;
  install_cleanup();
}

inline gc_cleanup::gc_cleanup(int cleanup) {
  __gc_external = NULL;
  if (cleanup)
    install_cleanup();
}

inline gc_cleanup::~gc_cleanup(void)
{
  if (__gc_external)
    gc_mark_external_invalid(__gc_external);
  GC_register_finalizer_ignore_self(gcOBJ_TO_PTR(this), 0, 0, 0, 0);
}

inline gc::~gc(void)
{
}

/***** Allocators: ******/

inline void *gc::operator new(size_t size)
{
#if defined(USE_SENORA_GC) || defined(MZ_PRECISE_GC)
  return GC_cpp_malloc(size);
#else
  return GC_malloc(size);
#endif
}

inline void *gc::operator new(size_t size, GCPlacement gcp)
{
  if (gcp == AtomicGC) 
    return GC_malloc_atomic(size);
  else {
#if defined(USE_SENORA_GC) || defined(MZ_PRECISE_GC)
    return GC_cpp_malloc(size);
#else
    return GC_malloc(size);
#endif
  }
}

inline void gc::operator delete(void * /*obj*/) 
{
}


#ifdef OPERATOR_NEW_ARRAY
inline void *gc::operator new[](size_t size) {
  return gc::operator new(size);
}
    
inline void *gc::operator new[](size_t size, GCPlacement gcp) {
  return gc::operator new(size, gcp);
}

inline void gc::operator delete[](void *obj) {
  gc::operator delete(obj);
}
#endif


/*************** For objects not derived from `gc' ***********************/

inline void *operator new(size_t size, GCPlacement gcp)
{
  void *obj;
  
  if (gcp == AtomicGC)
    obj = GC_malloc_atomic(size);
  else
    obj = GC_malloc(size);

  return obj;
}
        

#ifdef OPERATOR_NEW_ARRAY
inline void *operator new[](size_t size, GCPlacement gcp)
{
  return ::operator new(size, gcp);
}
#endif

#endif /* GC_CPP_H */
