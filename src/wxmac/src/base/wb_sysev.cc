/*
 * File:	wb_sysev.cc
 * Purpose:	System event implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_sysev.cc,v 1.1.1.1 1998/01/13 17:54:58 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation "wx_sysev.h"
#pragma implementation
#pragma interface
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_list.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include "wx_sysev.h"

wxList wxEventClassList(wxKEY_INTEGER);
wxList wxEventNameList(wxKEY_INTEGER);
wxList wxPrimaryEventHandlerList;
wxList wxPreEventHandlerList;
wxList wxPostEventHandlerList;

class wxEventHandlerStruc: public wxObject
{
 public:
   wxEventHandler handler;
   Bool override;
};

/*
 * A different kind of event from wxEvent: general wxWindows events, covering
 * all interesting things that might happen (button clicking, resizing,
 * setting text in widgets, etc.).
 *
 * For each completely new event type, derive a new event class.
 *
 */
 
wxEvent::wxEvent(void)  : wxObject(WXGC_NO_CLEANUP)
{
  eventClass = 0;
  eventType = 0;
  objectType = 0;
  eventObject = NULL;
  eventHandle = NULL;
}

wxEvent::~wxEvent(void)
{
}


static Bool wxSendEvent1(wxEvent& event, Bool external, wxList& list, Bool *stop)
{
  Bool handled = FALSE;

  for(wxNode *node = list.First(); node; node = node->Next())
  {
    wxEventHandlerStruc *thing = (wxEventHandlerStruc *)node->Data();
    if ((handled = (*thing->handler)(&event, external)) == TRUE)
    {
      if (thing->override)
      {
        *stop = TRUE;
        break;
      } else *stop = FALSE;
    }
  }
  return handled;
}

// Send an event to the system, usually will be external, but specify
// external = FALSE if calling from within the main application in response
// to other events.
// Returns TRUE iff the event was processed.
Bool wxSendEvent(wxEvent& event, Bool external)
{
  Bool stop = FALSE;
  return wxSendEvent1(event, external, wxPrimaryEventHandlerList, &stop);
}

// Notify the system of the event you are about to execute/have just executed.
// If TRUE is returned and pre = TRUE, calling code should not execute the event
// (i.e. it's been intercepted by a handler and vetoed).
// These events are always internal, because they're generated from within
// the main application code.
Bool wxNotifyEvent(wxEvent& event, Bool pre)
{
  Bool stop = FALSE;
  wxList *theList = NULL;
  if (pre)
    theList = &wxPreEventHandlerList;
  else
    theList = &wxPostEventHandlerList;

  return wxSendEvent1(event, FALSE, *theList, &stop);
}

// Add a primary event handler. pre = TRUE iff it should be called before the
// Returns TRUE if succeeds.
Bool wxAddPrimaryEventHandler(wxEventHandler handlerFunc)
{
  wxEventHandlerStruc *struc = new wxEventHandlerStruc;
  struc->handler = handlerFunc;
  struc->override = FALSE;

  wxPrimaryEventHandlerList.Append(struc);
  return TRUE;
}

// Add a secondary event handler, pre = TRUE iff it should be called before the
// event is executed. override = TRUE iff the handler is allowed to override
// all subsequent events by returning TRUE.
// Returns TRUE if succeeds.
Bool wxAddSecondaryEventHandler(wxEventHandler handlerFunc,
                                Bool pre, Bool override, Bool append)
{
  wxEventHandlerStruc *struc = new wxEventHandlerStruc;
  struc->handler = handlerFunc;
  struc->override = override;

  if (pre)
  {
    if (append)
      wxPreEventHandlerList.Append(struc);
    else
      wxPreEventHandlerList.Insert(struc);
  }
  else
  {
    if (append)
      wxPostEventHandlerList.Append(struc);
    else
      wxPostEventHandlerList.Insert(struc);
  }

  return TRUE;
}

// Remove secondary event handler. Returns TRUE if succeeds.
Bool wxRemoveSecondaryEventHandler(wxEventHandler handlerFunc, Bool pre)
{
  wxList *theList;
  if (pre)
    theList = &wxPreEventHandlerList;
  else
    theList = &wxPostEventHandlerList;
  
  for(wxNode *node = theList->First(); node; node = node->Next())
  {
    wxEventHandlerStruc *thing = (wxEventHandlerStruc *)node->Data();
    if (thing->handler == handlerFunc)
    {
      delete thing;
      delete node;
      return TRUE;
    }
  }
  return FALSE;
}

// Register a new event class (derived from wxEvent),
// giving the new event class type, its superclass, a function for creating
// a new event object of this class, and an optional description.
void wxRegisterEventClass(WXTYPE eventClassId, WXTYPE superClassId,
                          wxEventConstructor constructor, char *description)
{
  wxSystemEventClassStruc *struc = new wxSystemEventClassStruc;
  struc->eventClass = eventClassId;
  struc->eventConstructor = constructor;
  if (description)
    struc->eventDescription = copystring(description);
  else
    struc->eventDescription = NULL;

  wxEventClassList.Append((long)eventClassId, struc);

//  wxAllTypes.AddType(eventClassId, superClassId, description);
}

// Register the name of the event. This will allow a simple command language
// where giving the event type name and some arguments will cause
// a new event of class eventClassId to be created, with given event type,
// and some arguments, allows an event to be dynamically constructed and sent.
void wxRegisterEventName(WXTYPE eventTypeId, WXTYPE eventClassId, char *eventName)
{
  wxSystemEventNameStruc *struc = new wxSystemEventNameStruc;
  struc->eventClass = eventClassId;
  struc->eventType = eventTypeId;
  struc->eventName = copystring(eventName);

  wxEventNameList.Append((long)eventTypeId, struc);
}

// Define this and link before wxWindows library to allow
// registering events from 'outside' the main application
void wxRegisterExternalEventHandlers(void)
{
}

void wxDeleteEventLists(void)
{
  wxNode *node ;

  node = wxEventClassList.First();
  while (node)
  {
    wxSystemEventClassStruc *struc = (wxSystemEventClassStruc*)node->Data();
    wxNode *next = node->Next();
    delete struc;
    node = next;
  }

  node = wxEventNameList.First();
  while (node)
  {
    wxSystemEventNameStruc *struc = (wxSystemEventNameStruc*)node->Data();
    wxNode *next = node->Next();
    delete struc;
    node = next;
  }

  node = wxPrimaryEventHandlerList.First();
  while (node)
  {
    wxEventHandlerStruc *struc = (wxEventHandlerStruc*)node->Data();
    wxNode *next = node->Next();
    delete struc;
    node = next;
  }

  node = wxPreEventHandlerList.First();
  while (node)
  {
    wxEventHandlerStruc *struc = (wxEventHandlerStruc*)node->Data();
    wxNode *next = node->Next();
    delete struc;
    node = next;
  }

  node = wxPostEventHandlerList.First();
  while (node)
  {
    wxEventHandlerStruc *struc = (wxEventHandlerStruc*)node->Data();
    wxNode *next = node->Next();
    delete struc;
    node = next;
  }

}
