/*
 * File:	wx_mem.cc
 * Purpose:	Check for memory errors
 * Author:	Arthur Seaton
 * Created:	1994
 * Updated:	Julian Smart June 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

// static const char sccsid[] = "%W% %G%";

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "common.h"
#include "wx_setup.h"

#endif

#if DEBUG

#if WX_USE_MEMORY_TRACING

#ifdef __GNUG__
#pragma implementation
#endif

#include "wx_utils.h"
#include "wx_main.h"

#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <memory.h>
#include <stdarg.h>
#include "wx_mem.h"

// wxDebugContext wxTheDebugContext;
/*
  Redefine new and delete so that we can pick up situations where:
	- we overwrite or underwrite areas of malloc'd memory.
	- we use uninitialise variables
  Only do this in debug mode.

  We change new to get enough memory to allocate a struct, followed
  by the caller's requested memory, followed by a tag. The struct
  is used to create a doubly linked list of these areas and also
  contains another tag. The tags are used to determine when the area
  has been over/under written.
*/


/*
  Values which are used to set the markers which will be tested for
  under/over write. There are 3 of these, one in the struct, one
  immediately after the struct but before the caller requested memory and
  one immediately after the requested memory.
*/
#define MemStartCheck  0x23A8
#define MemMidCheck  0xA328
#define MemEndCheck 0x8A32
#define MemFillChar 0xAF
#define MemStructId  0x666D

/*
  External interface for the wxMemStruct class. Others are
  defined inline within the class def. Here we only need to be able
  to add and delete nodes from the list and handle errors in some way.
*/

/*
  Used for internal "this shouldn't happen" type of errors.
*/
void wxMemStruct::ErrorMsg (const char * mesg)
{
  wxTrace("wxWindows memory checking error: %s\n", mesg);
  PrintNode ();

//	 << _fileName << ' ' << _lineNum << endl;
}

/*
  Used when we find an overwrite or an underwrite error.
*/
void wxMemStruct::ErrorMsg ()
{
  wxTrace("wxWindows over/underwrite memory error: \n");
  PrintNode ();
    
//    cerr << _fileName << ' ' << _lineNum << endl;
}


/*
  We want to find out if pointers have been overwritten as soon as is
  possible, so test everything before we dereference it. Of course it's still
  quite possible that, if things have been overwritten, this function will
  fall over, but the only way of dealing with that would cost too much in terms
  of time.
*/
int wxMemStruct::AssertList ()
{
    if (wxDebugContext::GetHead () != 0 && ! (wxDebugContext::GetHead ())->AssertIt () ||
	wxDebugContext::GetTail () != 0 && ! wxDebugContext::GetTail ()->AssertIt ()) {
	ErrorMsg ("Head or tail pointers trashed");
	return 0;
    }
    return 1;
}


/*
  Check that the thing we're pointing to has the correct id for a wxMemStruct
  object and also that it's previous and next pointers are pointing at objects
  which have valid ids.
  This is definitely not perfect since we could fall over just trying to access
  any of the slots which we use here, but I think it's about the best that I
  can do without doing something like taking all new wxMemStruct pointers and
  comparing them against all known pointer within the list and then only
  doing this sort of check _after_ you've found the pointer in the list. That
  would be safer, but also much more time consuming.
*/
int wxMemStruct::AssertIt ()
{
    return (_id == MemStructId &&
	    (_prev == 0 || _prev->_id == MemStructId) &&
	    (_next == 0 || _next->_id == MemStructId));
}


/*
  Additions are always at the tail of the list.
  Returns 0 on error, non-zero on success.
*/
int wxMemStruct::Append ()
{
    if (! AssertList ())
	return 0;

    if (wxDebugContext::GetHead () == 0) {
	if (wxDebugContext::GetTail () != 0) {
	    ErrorMsg ("Null list should have a null tail pointer");
	    return 0;
	}
	(void) wxDebugContext::SetHead (this);
	(void) wxDebugContext::SetTail (this);
    } else {
	wxDebugContext::GetTail ()->_next = this;
	this->_prev = wxDebugContext::GetTail ();
	(void) wxDebugContext::SetTail (this);
    }
    return 1;
}


/*
  Don't actually free up anything here as the space which is used
  by the node will be free'd up when the whole block is free'd.
  Returns 0 on error, non-zero on success.
*/
int wxMemStruct::Unlink ()
{
    if (! AssertList ())
	return 0;

    if (wxDebugContext::GetHead () == 0 || wxDebugContext::GetTail () == 0) {
	ErrorMsg ("Trying to remove node from empty list");
	return 0;
    }

    // Handle the part of the list before this node.
    if (_prev == 0) {
	if (this != wxDebugContext::GetHead ()) {
	    ErrorMsg ("No previous node for non-head node");
	    return 0;
	}
	(void) wxDebugContext::SetHead (_next);
    } else {
	if (! _prev->AssertIt ()) {
	    ErrorMsg ("Trashed previous pointer");
	    return 0;
	}
	
	if (_prev->_next != this) {
	    ErrorMsg ("List is inconsistent");
	    return 0;
	}
	_prev->_next = _next;
    }

    // Handle the part of the list after this node.
    if (_next == 0) {
	if (this != wxDebugContext::GetTail ()) {
	    ErrorMsg ("No next node for non-tail node");
	    return 0;
	}
	(void) wxDebugContext::SetTail (_prev);
    } else {
	if (! _next->AssertIt ()) {
	    ErrorMsg ("Trashed next pointer");
	    return 0;
	}

	if (_next->_prev != this) {
	    ErrorMsg ("List is inconsistent");
	    return 0;
	}
	_next->_prev = _prev;
    }

    return 1;
}



/*
  Checks a node and block of memory to see that the markers are still
  intact.
*/
int wxMemStruct::CheckBlock ()
{
    int nFailures = 0;

    if (_firstMarker != MemStartCheck) {
	nFailures++;
	ErrorMsg ();
    }
    
    char * pointer = wxDebugContext::MidMarkerPos ((char *) this);
    if (* (wxMarkerType *) pointer != MemMidCheck) {
	nFailures++;
	ErrorMsg ();
    }
    
    pointer = wxDebugContext::EndMarkerPos ((char *) this, RequestSize ());
    if (* (wxMarkerType *) pointer != MemEndCheck) {
	nFailures++;
	ErrorMsg ();
    }
    
    return nFailures;
}


/*
  Check the list of nodes to see if they are all ok.
*/
int wxMemStruct::CheckAllPrevious ()
{
    int nFailures = 0;
    
    for (wxMemStruct * st = this->_prev; st != 0; st = st->_prev) {
	if (st->AssertIt ())
	    nFailures += st->CheckBlock ();
	else
	    return -1;
    }

    return nFailures;
}


/*
  When we delete a node we set the id slot to a specific value and then test
  against this to see if a nodes have been deleted previously. I don't
  just set the entire memory to the fillChar because then I'd be overwriting
  useful stuff like the vtbl which may be needed to output the error message
  including the file name and line numbers. Without this info the whole point
  of this class is lost!
*/
void wxMemStruct::SetDeleted ()
{
    _id = MemFillChar;
}

int wxMemStruct::IsDeleted ()
{
    return (_id == MemFillChar);
}


/*
  Print out a single node. There are many far better ways of doing this
  but this will suffice for now.
*/
void wxMemStruct::PrintNode ()
{
  if (isObject)
  {
    wxObject *obj = (wxObject *)actualData;
    wxClassInfo *info = obj->GetClassInfo();

    if (info && info->GetClassName())
      wxTrace("%s", info->GetClassName());
    else
      wxTrace("Object");

    if (_fileName)
      wxTrace(" (%s %d)", _fileName, (int)_lineNum);

    wxTrace(" at $%lX, size %d\n", (long)GetActualData(), (int)RequestSize());
  }
  else
  {
    wxTrace("Non-object data");
    if (_fileName)
      wxTrace(" (%s %d)", _fileName, (int)_lineNum);
    wxTrace(" at $%lX, size %d\n", (long)GetActualData(), (int)RequestSize());
  }
}

void wxMemStruct::Dump ()
{
  if (!ValidateNode()) return;
  
  if (isObject)
  {
    wxObject *obj = (wxObject *)actualData;
    wxClassInfo *info = obj->GetClassInfo();

    if (_fileName)
      wxTrace("Item (%s %d)", _fileName, (int)_lineNum);
    else
      wxTrace("Item");

    wxTrace(" at $%lX, size %d: ", (long)GetActualData(), (int)RequestSize());
//    wxTrace(info->GetClassName());
    obj->Dump(wxDebugContext::GetStream());
    wxTrace("\n");
  }
  else
  {
    wxTrace("Non-object data");
    if (_fileName)
      wxTrace(" (%s %d)", _fileName, (int)_lineNum);
    wxTrace(" at $%lX, size %d\n", (long)GetActualData(), (int)RequestSize());
  }
}


/*
  Validate a node. Check to see that the node is "clean" in the sense
  that nothing has over/underwritten it etc.
*/
int wxMemStruct::ValidateNode ()
{
    char * startPointer = (char *) this;
    if (!AssertIt ()) {
	if (IsDeleted ())
	    ErrorMsg ("Object already deleted");
	else {
	    // Can't use the error routines as we have no recognisable object.
	    wxTrace("Can't verify memory struct - all bets are off!\n");
	}
	return 0;
    }

/*
    int i;
    for (i = 0; i < wxDebugContext::TotSize (requestSize ()); i++)
      cout << startPointer [i];
    cout << endl;
*/
    if (Marker () != MemStartCheck)
      ErrorMsg ();
    if (* (wxMarkerType *) wxDebugContext::MidMarkerPos (startPointer) != MemMidCheck)
      ErrorMsg ();
    if (* (wxMarkerType *) wxDebugContext::EndMarkerPos (startPointer,
					      RequestSize ()) !=
	MemEndCheck)
      ErrorMsg ();

    // Back to before the extra buffer and check that
    // we can still read what we originally wrote.
    if (Marker () != MemStartCheck ||
	* (wxMarkerType *) wxDebugContext::MidMarkerPos (startPointer)
	                 != MemMidCheck ||
	* (wxMarkerType *) wxDebugContext::EndMarkerPos (startPointer,
					      RequestSize ()) != MemEndCheck)
    {
	ErrorMsg ();
	return 0;
    }

    return 1;
}




/*
  The wxDebugContext class.
*/

wxMemStruct *wxDebugContext::_head = NULL;
wxMemStruct *wxDebugContext::_tail = NULL;
// ostream *wxDebugContext::debugStream = NULL;
// streambuf *wxDebugContext::streamBuf = NULL;
streambuf *wxDebugContext::streamBuf = new wxDebugStreamBuf;
ostream *wxDebugContext::debugStream = new ostream(wxDebugContext::streamBuf);
Bool wxDebugContext::_checkPrevious = FALSE;
int wxDebugContext::debugLevel = 1;
Bool wxDebugContext::debugOn = TRUE;
wxMemStruct *wxDebugContext::checkPoint = NULL;

wxDebugContext::wxDebugContext(void)
{
//  streamBuf = new wxDebugStreamBuf;
//  debugStream = new ostream(streamBuf);
}

wxDebugContext::~wxDebugContext(void)
{
  SetStream(NULL, NULL);
}

void wxDebugContext::SetStream(ostream *stream, streambuf *buf)
{
  if (debugStream)
  {
    debugStream->flush();
    delete debugStream;
  }
  if (streamBuf)
    delete streamBuf;
  streamBuf = buf;
  debugStream = stream;
}

Bool wxDebugContext::SetFile(char *file)
{
  ofstream *str = new ofstream(file);
  if (str->bad())
  {
    delete str;
    return FALSE;
  }
  else
  {
    SetStream(str);
    return TRUE;
  }
}

Bool wxDebugContext::SetStandardError(void)
{
  wxDebugStreamBuf *buf = new wxDebugStreamBuf;
  ostream *stream = new ostream(streamBuf);
  SetStream(stream, buf);
  return TRUE;
}


/*
  Work out the positions of the markers by creating an array of 2 markers
  and comparing the addresses of the 2 elements. Use this number as the
  alignment for markers.
*/
size_t wxDebugContext::CalcAlignment ()
{
    wxMarkerType ar[2];
    return (char *) &ar[1] - (char *) &ar[0];
}


char * wxDebugContext::StructPos (const char * buf)
{
    return (char *) buf;
}

char * wxDebugContext::MidMarkerPos (const char * buf)
{
    return StructPos (buf) + PaddedSize (sizeof (wxMemStruct));
}

char * wxDebugContext::CallerMemPos (const char * buf)
{
    return MidMarkerPos (buf) + PaddedSize (sizeof(wxMarkerType));
}


char * wxDebugContext::EndMarkerPos (const char * buf, const size_t size)
{
    return CallerMemPos (buf) + PaddedSize (size);
}


/*
  Slightly different as this takes a pointer to the start of the caller
  requested region and returns a pointer to the start of the buffer.
  */
char * wxDebugContext::StartPos (const char * caller)
{
    return ((char *) (caller - wxDebugContext::PaddedSize (sizeof(wxMarkerType)) -
	    wxDebugContext::PaddedSize (sizeof (wxMemStruct))));
}

/*
  We may need padding between various parts of the allocated memory.
  Given a size of memory, this returns the amount of memory which should
  be allocated in order to allow for alignment of the following object.

  I don't know how portable this stuff is, but it seems to work for me at
  the moment. It would be real nice if I knew more about this!
*/
size_t wxDebugContext::GetPadding (const size_t size)
{
    size_t pad = size % CalcAlignment ();
    return (pad) ? sizeof(wxMarkerType) - pad : 0;
}



size_t wxDebugContext::PaddedSize (const size_t size)
{
    return size + GetPadding (size);
}

/*
  Returns the total amount of memory which we need to get from the system
  in order to satisfy a caller request. This includes space for the struct
  plus markers and the caller's memory as well.
*/
size_t wxDebugContext::TotSize (const size_t reqSize)
{
    return (PaddedSize (sizeof (wxMemStruct)) + PaddedSize (reqSize) +
	    2 * sizeof(wxMarkerType));
}


/*
  Traverse the list of nodes executing the given function on each node.
*/
void wxDebugContext::TraverseList (PmSFV func, wxMemStruct *from)
{
  if (!from)
    from = wxDebugContext::GetHead ();
    
  for (wxMemStruct * st = from; st != 0; st = st->_next)
	(st->*func) ();
}


/*
  Print out the list.
  */
Bool wxDebugContext::PrintList (void)
{
  if (!HasStream())
    return FALSE;

  TraverseList ((PmSFV)&wxMemStruct::PrintNode, (checkPoint ? checkPoint->_next : NULL));

  return TRUE;
}

Bool wxDebugContext::Dump(void)
{
  if (!HasStream())
    return FALSE;

  char *appName = (wxTheApp ? wxTheApp->GetAppName() : NULL);
  if (!appName)
    appName = "application";
    
  wxTrace("Memory dump of %s at %s:\n", appName, wxNow());
  TraverseList ((PmSFV)&wxMemStruct::Dump, (checkPoint ? checkPoint->_next : NULL));

  return TRUE;
}

struct wxDebugStatsStruct
{
  long instanceCount;
  long totalSize;
  char *instanceClass;
  wxDebugStatsStruct *next;
};

static wxDebugStatsStruct *FindStatsStruct(wxDebugStatsStruct *st, char *name)
{
  while (st)
  {
    if (strcmp(st->instanceClass, name) == 0)
      return st;
    st = st->next;
  }
  return NULL;
}

static wxDebugStatsStruct *InsertStatsStruct(wxDebugStatsStruct *head, wxDebugStatsStruct *st)
{
  st->next = head;
  return st;
}

Bool wxDebugContext::PrintStatistics(Bool detailed)
{
  if (!HasStream())
    return FALSE;

  Bool currentMode = GetDebugMode();
  SetDebugMode(FALSE);
  
  long noNonObjectNodes = 0;
  long noObjectNodes = 0;
  long totalSize = 0;

  wxDebugStatsStruct *list = NULL;

  wxMemStruct *from = (checkPoint ? checkPoint->_next : NULL);
  if (!from)
    from = wxDebugContext::GetHead ();

  wxMemStruct *st;    
  for (st = from; st != 0; st = st->_next)
  {
    if (detailed)
    {
      char *className = "nonobject";
      if (st->isObject && st->GetActualData())
      {
        wxObject *obj = (wxObject *)st->GetActualData();
        if (obj->GetClassInfo()->GetClassName())
          className = obj->GetClassInfo()->GetClassName();
      }
      wxDebugStatsStruct *stats = FindStatsStruct(list, className);
      if (!stats)
      {
        stats = (wxDebugStatsStruct *)malloc(sizeof(wxDebugStatsStruct));
        stats->instanceClass = className;
        stats->instanceCount = 0;
        stats->totalSize = 0;
        list = InsertStatsStruct(list, stats);
      }
      stats->instanceCount ++;
      stats->totalSize += st->RequestSize();
    }

    totalSize += st->RequestSize();
    if (st->isObject)
      noObjectNodes ++;
    else
      noNonObjectNodes ++;
  }

  if (detailed)
  {
    while (list)
    {
      wxTrace("%ld objects of class %s, total size %ld\n",
          list->instanceCount, list->instanceClass, list->totalSize);
      wxDebugStatsStruct *old = list;
      list = old->next;
      free(old);
    }
    wxTrace("\n");
  }
  
  SetDebugMode(currentMode);

  wxTrace("Number of object items: %ld\n", noObjectNodes);
  wxTrace("Number of non-object items: %ld\n", noNonObjectNodes);
  wxTrace("Total allocated size: %ld\n", totalSize);

  return TRUE;
}    

Bool wxDebugContext::PrintClasses(void)
{
  if (!HasStream())
    return FALSE;

  char *appName = (wxTheApp ? wxTheApp->GetAppName() : NULL);
  if (!appName)
    appName = "application";

  wxTrace("Classes in %s:\n\n", appName);
  int n = 0;
  wxClassInfo *info = wxClassInfo::first;
  while (info)
  {
    if (info->GetClassName())
    {
      wxTrace("%s ", info->GetClassName());

      if (info->GetBaseClassName1() && !info->GetBaseClassName2())
        wxTrace("is a %s", info->GetBaseClassName1());
      else if (info->GetBaseClassName1() && info->GetBaseClassName2())
        wxTrace("is a %s, %s", info->GetBaseClassName1(), info->GetBaseClassName2());
      if (info->objectConstructor)
        wxTrace(": dynamic\n");
      else
        wxTrace("\n");
    }
    info = info->next;
    n ++;
  }
  wxTrace("\nThere are %d classes derived from wxObject.\n", n);
  return TRUE;
}    

void wxDebugContext::SetCheckpoint(Bool all)
{
  if (all)
    checkPoint = NULL;
  else
    checkPoint = _tail;
}

// Checks all nodes since checkpoint
int wxDebugContext::Check(void)
{
  int nFailures = 0;
    
  wxMemStruct *from = (checkPoint ? checkPoint->_next : NULL);
  if (!from)
    from = wxDebugContext::GetHead ();

  for (wxMemStruct * st = from; st != 0; st = st->_next)
  {
    if (st->AssertIt ())
      nFailures += st->CheckBlock ();
    else
      return -1;
  }

  return nFailures;
}

/*
  The global operator new used for everything apart from getting
  dynamic storage within this function itself.
*/

// We'll only do malloc and free for the moment: leave the interesting
// stuff for the wxObject versions.

#if USE_GLOBAL_MEMORY_OPERATORS
void * operator new (size_t size, char * fileName, int lineNum)
{
  return malloc(size);
//  return wxDebugAlloc(size, fileName, lineNum, FALSE);
}


void operator delete (void * buf)
{
  free(buf);
//  wxDebugFree(buf);
}
#endif

void * wxDebugAlloc(size_t size, char * fileName, int lineNum, Bool isObject)
{
  // If not in debugging allocation mode, do the normal thing
  // so we don't leave any trace of ourselves in the node list.
  if (!wxDebugContext::GetDebugMode())
  {
    return (void *)malloc(size);
  }
  
    char * buf = (char *) malloc(wxDebugContext::TotSize (size));
    if (!buf) {
	wxTrace("Call to malloc (%ld) failed.\n", (long)size);
	return 0;
    }
    wxMemStruct * st = (wxMemStruct *)buf;
    st->_firstMarker = MemStartCheck;
    st->_reqSize = size;
    st->_fileName = fileName;
    st->_lineNum = lineNum;
    st->_id = MemStructId;
    st->_prev = 0;
    st->_next = 0;
    st->isObject = isObject;

    // Errors from Append() shouldn't really happen - but just in case!
    if (st->Append () == 0) {
	st->ErrorMsg ("Trying to append new node");
	exit (1);
    }
    
    if (wxDebugContext::GetCheckPrevious ()) {
	if (st->CheckAllPrevious () < 0) {
	    st->ErrorMsg ("Checking previous nodes");
	    exit (1);
	}
    }
    
    // Set up the extra markers at the middle and end.
    char * ptr = wxDebugContext::MidMarkerPos (buf);
    * (wxMarkerType *) ptr = MemMidCheck;
    ptr = wxDebugContext::EndMarkerPos (buf, size);
    * (wxMarkerType *) ptr = MemEndCheck;

    // pointer returned points to the start of the caller's
    // usable area.
    void *actualData = (void *) wxDebugContext::CallerMemPos (buf);
    st->actualData = actualData;
    
    return actualData;
}


void wxDebugFree(void * buf)
{
  if (!buf)
    return;
    
  // If not in debugging allocation mode, do the normal thing
  // so we don't leave any trace of ourselves in the node list.
  if (!wxDebugContext::GetDebugMode())
  {
    free(buf);
    return;
  }

    // Points to the start of the entire allocated area.
    char * startPointer = wxDebugContext::StartPos ((char *) buf);
    // Find the struct and make sure that it's identifiable.
    wxMemStruct * st = (wxMemStruct *) wxDebugContext::StructPos (startPointer);

    if (! st->ValidateNode ())
	return;

    // If this is the current checkpoint, we need to
    // move the checkpoint back so it points to a valid
    // node.
    if (st == wxDebugContext::checkPoint)
      wxDebugContext::checkPoint = wxDebugContext::checkPoint->_prev;

    if (! st->Unlink ())
    {
      st->ErrorMsg ("Unlinking deleted node");
      exit (1);
    }
    
    // Now put in the fill char into the id slot and the caller requested
    // memory locations.
    st->SetDeleted ();
    (void) memset (wxDebugContext::CallerMemPos (startPointer), MemFillChar,
		   st->RequestSize ());

    // Don't allow delayed freeing of memory in this version
//    if (!wxDebugContext::GetDelayFree())
    free((void *)st);
}

// Trace: send output to the current debugging stream
void wxTrace(const char *fmt ...)
{
  va_list ap;
  static char buffer[512];

  va_start(ap, fmt);

#ifdef wx_msw
  wvsprintf(buffer,fmt,ap) ;
#else
  vsprintf(buffer,fmt,ap) ;
#endif

  va_end(ap);

  if (wxDebugContext::HasStream())
  {
    wxDebugContext::GetStream() << buffer;
    wxDebugContext::GetStream().flush();
  }
  else
#ifdef wx_msw
    OutputDebugString((LPCSTR)buffer) ;
#else
    fprintf(stderr, buffer);
#endif
}

// Trace with level
void wxTraceLevel(int level, const char *fmt ...)
{
  if (wxDebugContext::GetLevel() < level)
    return;
    
  va_list ap;
  static char buffer[512];

  va_start(ap, fmt);

#ifdef wx_msw
  wvsprintf(buffer,fmt,ap) ;
#else
  vsprintf(buffer,fmt,ap) ;
#endif

  va_end(ap);

  if (wxDebugContext::HasStream())
  {
    wxDebugContext::GetStream() << buffer;
    wxDebugContext::GetStream().flush();
  }
  else
#ifdef wx_msw
    OutputDebugString((LPCSTR)buffer) ;
#else
    fprintf(stderr, buffer);
#endif
}

#endif // WX_USE_MEMORY_TRACING
#endif // DEBUG
