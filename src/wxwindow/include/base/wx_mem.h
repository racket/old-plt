/*
 * File:	wx_mem.h
 * Purpose:	Check for memory errors
 * Author:	Arthur Seaton
 * Created:	1994
 * Updated:	Julian Smart May 1995
 * Copyright:	(c) 1994, AIAI, University of Edinburgh
 */

 /*
 Changes by Julian Smart
 1/5/95
 - made all memDebug members static and thus removed need for
   a global memDebug variable.
 - all filling/checking codes are now #defines.
 - debugging wxDebugAlloc/wxDebugFree implemented as separate functions from
   new/delete operators.
 - implemented 'new' in terms of malloc, not another new.
 - removed vtable from memStruct by removing virtual keyword
   (otherwise get problems).
 - standard output replaced with wxDebugMsg calls.
 */

/* static const char sccsid[] = "%W% %G%"; */

#ifndef _WXMEM_H
#define _WXMEM_H

#include "common.h"

/*
  The macro which will be expanded to include the file and line number
  info, or to be a straight call to the new operator.
*/

#if DEBUG && WX_USE_MEMORY_TRACING

#include <stddef.h>
#include <iostream.h>

#define DEBUG_NEW new(__FILE__,__LINE__)

void * wxDebugAlloc(size_t size, char * fileName, int lineNum, Bool isObject);
void wxDebugFree(void * buf);

// Global versions of the new and delete operators.
// Currently, these merely call malloc and free; only the wxObject
// operators do something interesting. But this allows DEBUG_NEW to
// work for all 'new's in a file.
#if USE_GLOBAL_MEMORY_OPERATORS
void * operator new (size_t size, char * fileName, int lineNum);
void operator delete (void * buf);
#endif

typedef unsigned int wxMarkerType;

/*
  Define the struct which will be placed at the start of all dynamically
  allocated memory.
*/


class wxMemStruct {

friend class wxDebugContext; // access to the _next pointer for list traversal.

public:
    // Check for underwriting. There are 2 of these checks. This one
    // inside the struct and another right after the struct.
    wxMarkerType _firstMarker;

    // File name and line number are from cpp.
    char * _fileName;
    int _lineNum;

   // Total amount of memory malloc\'d. Includes all additional
    // structs, markers etc as well as the caller requested area.
//    size_t _totSize;
    // The amount of memory requested by the caller.
    size_t _reqSize;

    // Used to try to verify that we really are dealing with an object
    // of the required class. Can be 1 of 2 values these indicating a valid
    // wxMemStruct object, or a deleted wxMemStruct object.
    wxMarkerType _id;

    wxMemStruct * _prev;
    wxMemStruct * _next;

    void *actualData;
    Bool isObject;

    int AssertList ();

public:
//    size_t totalSize () { return _totSize; }
    size_t RequestSize () { return _reqSize; }
    wxMarkerType Marker () { return _firstMarker; }

    // When an object is deleted we set the id slot to a specific value.
    inline void SetDeleted ();
    inline int IsDeleted ();

    int Append ();
    int Unlink ();

    // Used to determine if the object is really a wxMemStruct.
    // Not a foolproof test by any means, but better than none I hope!
    int AssertIt ();

    // Do all validation on a node.
    int ValidateNode ();

    // Check the integrity of a node and of the list, node by node.
    int CheckBlock ();
    int CheckAllPrevious ();

    // Print a single node.
    void PrintNode ();

    // Called when the memory linking functions get an error.
    void ErrorMsg (const char *);
    void ErrorMsg ();

    inline void *GetActualData(void) { return actualData; }

    void Dump(void);
};


typedef void (wxMemStruct::*PmSFV) ();


/*
  Debugging class. This will only have a single instance, but it\'s
  a reasonable way to keep everything together and to make this
  available for change if needed by someone else.
  A lot of this stuff would be better off within the wxMemStruct class, but
  it\'s stuff which we need to access at times when there is no wxMemStruct
  object so we use this class instead. Think of it as a collection of
  globals which have to do with the wxMemStruct class.
*/

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

// Output a debug mess., in a system dependent fashion.
void wxTrace(const char *fmt ...);
void wxTraceLevel(int level, const char *fmt ...);

#define TRACE wxTrace
#define TRACELEVEL wxTrace

#else // else part for the #if DEBUG

inline void wxTrace(const char *WXUNUSED(fmt)) {}
inline void wxTraceLevel(int WXUNUSED(level), const char *WXUNUSED(fmt)) {}

#define TRACE TRUE ? (void)0 : wxTrace
#define TRACELEVEL TRUE ? (void)0 : wxTraceLevel
#define DEBUG_NEW new

#endif // DEBUG

#endif // _WMMEM_H
