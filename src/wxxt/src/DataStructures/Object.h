/*								-*- C++ -*-
 * $Id: Object.h,v 1.2 1996/01/10 23:46:27 markus Exp $
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

#ifdef __GNUG__
#pragma interface
#endif

//-----------------------------------------------------------------------------
// wxClassInfo: for dynamic object system declarations
//-----------------------------------------------------------------------------

#if USE_DYNAMIC_CLASSES

class wxClassInfo;
class wxObject;

typedef wxObject* (*wxObjectConstructorFn)(void);

class wxClassInfo {
public:
    wxClassInfo(char *cName, char *baseName1, char *baseName2,
		int sz, wxObjectConstructorFn fn);

           wxObject*    CreateObject(void);
    static wxClassInfo* FindClass(char *c);
    inline char*        GetClassName(void) { return className; }
    inline char*        GetBaseClassName1(void) { return baseClassName1; }
    inline char*        GetBaseClassName2(void) { return baseClassName2; }
    inline int          GetSize(void) { return objectSize; }
    static void         InitializeClasses(void);
           Bool         IsKindOf(wxClassInfo *info);
private:
    friend wxObject *wxCreateDynamicObject(char *name);
    friend class wxDebugContext;

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
};

wxObject *wxCreateDynamicObject(char *name);

#endif // IF USE_DYNAMIC_CLASSES

//-----------------------------------------------------------------------------
// wxObject: top level object
//-----------------------------------------------------------------------------

/* MATTHEW */
#if WXGARBAGE_COLLECTION_ON
#include "gc_cpp.h"
#define WXGC_IGNORE(ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
#define WXGC_ATOMIC (AtomicGC)
#define WXGC_NOT (NoGC)
#define WXGC_NO_CLEANUP FALSE
class wxObject : public gc_cleanup
#else
#define WXGC_IGNORE(ptr)
#define WXGC_ATOMIC
#define WXGC_NOT
class wxObject
#endif
{
public:
    wxObject(void);
    wxObject(Bool cleanup);
    virtual ~wxObject(void);

    WXTYPE __type;

#if USE_DYNAMIC_CLASSES
    static  wxClassInfo  classwxObject;
    virtual wxClassInfo* GetClassInfo(void) { return &classwxObject; }
            Bool         IsKindOf(wxClassInfo *info);
    virtual istream&     LoadObject(istream&);
    virtual ostream&     SaveObject(ostream&);
#if DEBUG
    virtual void Dump(ostream& str);
#endif
#endif
#if DEBUG && WX_USE_MEMORY_TRACING
    void* operator new(size_t size, char* fileName=NULL, int lineNum=0);
    void* operator new[](size_t size, char* fileName=NULL, int lineNum=0);
    void  operator delete(void * buf);
    void  operator delete[](void * buf);
#endif

#ifdef MEMORY_USE_METHOD
  virtual long MemoryUse(void);
#endif
};

#if DEBUG && WX_USE_MEMORY_TRACING && USE_GLOBAL_MEMORY_OPERATORS
void *operator new(size_t size, char * fileName=NULL, int lineNum=0);
void *operator new[](size_t size, char * fileName=NULL, int lineNum=0);
void  operator delete(void * buf);
void  operator delete[](void * buf);
#endif

//-----------------------------------------------------------------------------
// debugging context
//-----------------------------------------------------------------------------

#if DEBUG && WX_USE_MEMORY_TRACING && USE_DYNAMIC_CLASSES

class wxMemStruct;
typedef void (wxMemStruct::*PmSFV)(void);

#ifdef Uses_wxDebugStreamBuf

class wxDebugStreamBuf : public streambuf {
public:
    wxDebugStreamBuf(void);

    int overflow(int i);
    inline int underflow(void) { return EOF; }
    int sync(void);
};

#endif

class wxDebugContext {

private:
    // Store these here to allow access to the list without
    // needing to have a wxMemStruct object.
    static wxMemStruct * _head;
    static wxMemStruct * _tail;

    // Set to FALSE if we're not checking all previous nodes when
    // we do a new. Set to TRUE when we are.
    static Bool _checkPrevious;
protected:
    // Used to set alignment for markers.
    static size_t CalcAlignment ();

    // Returns the amount of padding needed after something of the given
    // size. This is so that when we cast pointers backwards and forwards
    // the pointer value will be valid for a wxMarkerType.
    static size_t GetPadding (const size_t size);

    // Traverse the list.
    static void TraverseList (PmSFV, wxMemStruct *from = NULL);

    static streambuf *streamBuf;
    static ostream *debugStream;

    static int debugLevel;
    static Bool debugOn;

public:
    // Set a checkpoint to dump only the memory from
    // a given point
    static wxMemStruct *checkPoint;

    wxDebugContext(void);
    ~wxDebugContext(void);

    static Bool HasStream(void) { return (debugStream != NULL); };
    static ostream& GetStream(void) { return *debugStream; }
    static streambuf *GetStreamBuf(void) { return streamBuf; }
    static void SetStream(ostream *stream, streambuf *buf = NULL);
    static Bool SetFile(char *file);
    static Bool SetStandardError(void);
    
    static int GetLevel(void) { return debugLevel; }
    static void SetLevel(int level) { debugLevel = level; }

    static Bool GetDebugMode(void) { return debugOn; }
    static void SetDebugMode(Bool flag) { debugOn = flag; }

    static void SetCheckpoint(Bool all = FALSE);
    static wxMemStruct *GetCheckpoint(void) { return checkPoint; }
    
    // Calculated from the request size and any padding needed
    // before the final marker.
    static size_t PaddedSize (const size_t reqSize);

    // Calc the total amount of space we need from the system
    // to satisfy a caller request. This includes all padding.
    static size_t TotSize (const size_t reqSize);

    // Return valid pointers to offsets within the allocated memory.
    static char * StructPos (const char * buf);
    static char * MidMarkerPos (const char * buf);
    static char * CallerMemPos (const char * buf);
    static char * EndMarkerPos (const char * buf, const size_t size);

    // Given a pointer to the start of the caller requested area
    // return a pointer to the start of the entire alloc\'d buffer.
    static char * StartPos (const char * caller);

    // Access to the list.
    static wxMemStruct * GetHead () { return _head; }
    static wxMemStruct * GetTail () { return _tail; }

    // Set the list sentinals.
    static wxMemStruct * SetHead (wxMemStruct * st) { return (_head = st); }
    static wxMemStruct * SetTail (wxMemStruct * st) { return (_tail = st); }

    // If this is set then every new operation checks the validity
    // of the all previous nodes in the list.
    static Bool GetCheckPrevious () { return _checkPrevious; }
    static void SetCheckPrevious (Bool value) { _checkPrevious = value; }

    // Checks all nodes
    static int Check(void);

    // Print out the list of wxMemStruct nodes.
    static Bool PrintList(void);
    
    // Dump objects
    static Bool Dump(void);
    
    // Print statistics
    static Bool PrintStatistics(Bool detailed = TRUE);
    
    // Print out the classes in the application.
    static Bool PrintClasses(void);
};

#endif

//-----------------------------------------------------------------------------
// macros to store meta-information about classes
//-----------------------------------------------------------------------------

#if USE_DYNAMIC_CLASSES

#define DECLARE_DYNAMIC_CLASS(name) \
    public:\
    static wxClassInfo class##name;\
    wxClassInfo *GetClassInfo() { return (&name::class##name); }
#define DECLARE_ABSTRACT_CLASS(name) \
    DECLARE_DYNAMIC_CLASS(name)
#define DECLARE_CLASS(name) \
    DECLARE_DYNAMIC_CLASS(name)

#define IMPLEMENT_DYNAMIC_CLASS(name, basename) \
    wxObject *wxConstructorFor##name(void) { return (new name()); } \
    wxClassInfo name::class##name(#name, #basename, NULL, \
                                  sizeof(name), wxConstructorFor##name);
#define IMPLEMENT_DYNAMIC_CLASS2(name, basename1, basename2) \
    wxObject *wxConstructorFor##name(void) { return (new name()); } \
    wxClassInfo name::class##name(#name, #basename1, #basename2, \
                                  sizeof(name), wxConstructorFor##name);
#define IMPLEMENT_ABSTRACT_CLASS(name, basename) \
    wxClassInfo name::class##name(#name, #basename, NULL, sizeof(name), NULL);
#define IMPLEMENT_ABSTRACT_CLASS2(name, basename1, basename2) \
    wxClassInfo name::class##name(#name, #basename1, #basename2, sizeof(name), NULL);
#define IMPLEMENT_CLASS \
    IMPLEMENT_ABSTRACT_CLASS
#define IMPLEMENT_CLASS2 \
    IMPLEMENT_ABSTRACT_CLASS2

#define CLASSINFO(name) \
    (&name::class##name)

#else // NOT USE_DYNAMIC_CLASSES

#define DECLARE_DYNAMIC_CLASS(name)
#define DECLARE_ABSTRACT_CLASS(name)
#define DECLARE_CLASS(name)
#define IMPLEMENT_DYNAMIC_CLASS(name, basename)
#define IMPLEMENT_DYNAMIC_CLASS2(name, basename1, basename2)
#define IMPLEMENT_ABSTRACT_CLASS(name, basename)
#define IMPLEMENT_ABSTRACT_CLASS2(name, basename1, basename2)
#define IMPLEMENT_CLASS IMPLEMENT_ABSTRACT_CLASS
#define IMPLEMENT_CLASS2 IMPLEMENT_ABSTRACT_CLASS2

#endif // IF USE_DYNAMIC_CLASSES

#if DEBUG && WX_USE_MEMORY_TRACING

// Output a debug mess., in a system dependent fashion.
void wxTrace(const char *fmt ...);
void wxTraceLevel(int level, const char *fmt ...);

#define TRACE wxTrace
#define TRACELEVEL wxTrace
#define DEBUG_NEW new(__FILE__,__LINE__)

#else // else part for the #if DEBUG

inline void wxTrace(const char *WXUNUSED(fmt)) {}
inline void wxTraceLevel(int WXUNUSED(level), const char *WXUNUSED(fmt)) {}

#define TRACE TRUE ? (void)0 : wxTrace
#define TRACELEVEL TRUE ? (void)0 : wxTraceLevel
#define DEBUG_NEW new

#endif // DEBUG && WX_USE_MEMORY_TRACING

#ifdef DEBUG
#define wxASSERT(expression, reason)\
    ((void) ((expression) ? 0 :\
	      __wxASSERT(__FILE__, __LINE__, reason)))
    void __wxASSERT(char *file, unsigned lineno,
		    char *reason="assert failed");
#else
#define wxASSERT(ignore1, ignore2) \
    ((void) 0)
#endif // DEBUG

#endif // Object_h
