/*
 * File:	wx_obj.h
 * Purpose:	Top level object for wxWindows
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_obj.h	1.2 5/9/94" */

#ifndef wxb_objh
#define wxb_objh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"

#if WXGARBAGE_COLLECTION_ON
# include "gc_cpp.h"

# ifndef WXGC_CLEANUP_CLASS
#  define WXGC_CLEANUP_CLASS gc_cleanup
# endif
#endif

#ifdef IN_CPROTO
typedef       void    *wxObject ;
#else

class wxObject;

#if USE_DYNAMIC_CLASSES
class wxClassInfo;

/*
 * Dynamic object system declarations
 */

typedef wxObject * (*wxObjectConstructorFn) (void);

class wxClassInfo
{
 public:
   char *className;
   char *baseClassName1;
   char *baseClassName2;
   int objectSize;
   wxObjectConstructorFn objectConstructor;

   // Pointers to base wxClassInfos: set in InitializeClasses
   // called from wx_main.cc
   wxClassInfo *baseInfo1;
   wxClassInfo *baseInfo2;

   static wxClassInfo *first;
   wxClassInfo *next;

   wxClassInfo(char *cName, char *baseName1, char *baseName2, int sz, wxObjectConstructorFn fn);

   wxObject *CreateObject(void);
   inline char *GetClassName(void) { return className; }
   inline char *GetBaseClassName1(void) { return baseClassName1; }
   inline char *GetBaseClassName2(void) { return baseClassName2; }
   inline int GetSize(void) { return objectSize; }
   Bool IsKindOf(wxClassInfo *info);

   static wxClassInfo *FindClass(char *c);
   // Initializes parent pointers for fast searching.
   static void InitializeClasses(void);
};

wxObject *wxCreateDynamicObject(char *name);

#define DECLARE_DYNAMIC_CLASS(name) \
 public:\
  static wxClassInfo class##name;\
  wxClassInfo *GetClassInfo() \
   { return &name::class##name; }

#define DECLARE_ABSTRACT_CLASS(name) DECLARE_DYNAMIC_CLASS(name)
#define DECLARE_CLASS(name) DECLARE_DYNAMIC_CLASS(name)

//////
////// for concrete classes
//////

// Single inheritance with one base class
#define IMPLEMENT_DYNAMIC_CLASS(name, basename) \
wxObject *wxConstructorFor##name(void) \
   { return new name; }\
 wxClassInfo name::class##name(#name, #basename, NULL, sizeof(name), wxConstructorFor##name);

// Multiple inheritance with two base classes
#define IMPLEMENT_DYNAMIC_CLASS2(name, basename1, basename2) \
wxObject *wxConstructorFor##name(void) \
   { return new name; }\
 wxClassInfo name::class##name(#name, #basename1, #basename2, sizeof(name), wxConstructorFor##name);

//////
////// for abstract classes
//////

// Single inheritance with one base class
#define IMPLEMENT_ABSTRACT_CLASS(name, basename) \
 wxClassInfo name::class##name(#name, #basename, NULL, sizeof(name), NULL);

// Multiple inheritance with two base classes
#define IMPLEMENT_ABSTRACT_CLASS2(name, basename1, basename2) \
 wxClassInfo name::class##name(#name, #basename1, #basename2, sizeof(name), NULL);

#define IMPLEMENT_CLASS IMPLEMENT_ABSTRACT_CLASS
#define IMPLEMENT_CLASS2 IMPLEMENT_ABSTRACT_CLASS2

#define CLASSINFO(name) (&name::class##name)

#else

// No dynamic class system: so stub out the macros
#define DECLARE_DYNAMIC_CLASS(name)
#define DECLARE_ABSTRACT_CLASS(name)
#define DECLARE_CLASS(name)
#define IMPLEMENT_DYNAMIC_CLASS(name, basename)
#define IMPLEMENT_DYNAMIC_CLASS2(name, basename1, basename2)
#define IMPLEMENT_ABSTRACT_CLASS(name, basename)
#define IMPLEMENT_ABSTRACT_CLASS2(name, basename1, basename2)
#define IMPLEMENT_CLASS IMPLEMENT_ABSTRACT_CLASS
#define IMPLEMENT_CLASS2 IMPLEMENT_ABSTRACT_CLASS2

#endif

// Unfortunately Borland seems to need this include.
#ifdef __BORLANDC__
#include <iostream.h>
#endif

class istream;
class ostream;

#if WXGARBAGE_COLLECTION_ON
#define WXGC_IGNORE(ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
#define WXGC_ATOMIC (AtomicGC)
#define WXGC_NO_CLEANUP FALSE
class wxObject : public WXGC_CLEANUP_CLASS
#else
#define WXGC_IGNORE(ptr)
#define WXGC_ATOMIC
class wxObject
#endif
{
 public:
  WXTYPE __type;
#if USE_DYNAMIC_CLASSES
  static wxClassInfo classwxObject;
#endif

  wxObject(void);
  wxObject(Bool cleanup);
  virtual ~wxObject(void);

#if USE_DYNAMIC_CLASSES
  virtual wxClassInfo *GetClassInfo(void) { return &classwxObject; }
  virtual istream& LoadObject(istream&);
  virtual ostream& SaveObject(ostream&);

  Bool IsKindOf(wxClassInfo *info);
#endif
#if DEBUG && WX_USE_MEMORY_TRACING
  void * operator new (size_t size, char * fileName = NULL, int lineNum = 0);
  void operator delete (void * buf);
#endif

#if DEBUG
  virtual void Dump(ostream& str);
#endif
};


#endif // IN_CPROTO
#endif // wx_objh
