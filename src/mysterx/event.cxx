// event.cxx -- event-related functions

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "myspage.h"
#include "mysterx.h"

// number of elts should be same as in EVENT_TYPE enumeration
static WCHAR *eventNames[11]; 

Scheme_Object *hash_table_get;
Scheme_Object *hash_table_put;
Scheme_Object *hash_table_remove;
Scheme_Object *make_hash_table;

/* DWORD WINAPI doHandleEvents(LPVOID doc) {
  while (1) {
    MX_Event *eventObj;
    IEvent *pEvent;
    BSTR id;

    eventObj = (MX_Event *)mx_get_event(1,(Scheme_Object **)doc);
    pEvent = eventObj->pEvent;
    pEvent->get_srcId(&id);
    wprintf(L"tag: %s\n",id);
  }

  return 0;
}

Scheme_Object *mx_event_handle(int argc,Scheme_Object **argv) {
  DWORD threadId;

   if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("event-handle","hash-table",0,argc,argv) ;
  }

  if (SCHEME_HASHTP(argv[1]) == FALSE) {
    scheme_wrong_type("event-handle","hash-table",0,argc,argv) ;
  }

  CreateThread(NULL,0,doHandleEvents,argv,0,&threadId);

  return scheme_void;
}
*/

/* BOOL eventAvailable(Scheme_Object *pDocument) {
  VARIANT_BOOL val;
  
  ((MX_Document_Object *)pDocument)->pIEventQueue->get_EventAvailable(&val);

  return (BOOL)val;
}
*/

Scheme_Object *mx_event_available(int argc,Scheme_Object **argv) {
  VARIANT_BOOL val;

  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("block-until-event","mx-document",0,argc,argv) ;
  }

  ((MX_Document_Object *)argv[0])->pIEventQueue->get_EventAvailable(&val);

  return val ? scheme_true : scheme_false;    
}

void initEventNames(void) {
  eventNames[click] = L"click";
  eventNames[dblclick] = L"dblclick";
  eventNames[error] = L"error";
  eventNames[keydown] = L"keydown";
  eventNames[keypress] = L"keypress";
  eventNames[keyup] = L"keyup";
  eventNames[mousedown] = L"mousedown";
  eventNames[mousemove] = L"mousemove";
  eventNames[mouseout] = L"mouseout";
  eventNames[mouseover] = L"mouseover";
  eventNames[mouseup] = L"mouseup";
}

IEvent *getEventInterface(Scheme_Object *ev,char *fname) {
  if (MX_EVENTP(ev) == FALSE) {
    scheme_wrong_type(fname,"com-event",-1,0,&ev) ;
  }

  return MX_EVENT_VAL(ev);
}


Scheme_Object *mx_event_tag(int argc,Scheme_Object **argv) {
  BSTR tag;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-tag");

  pEvent->get_srcTag(&tag);

  return BSTRToSchemeString(tag);
}

Scheme_Object *mx_event_id(int argc,Scheme_Object **argv) {
  BSTR id;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-id");
  
  pEvent->get_srcId(&id);

  return BSTRToSchemeString(id);
}

Scheme_Object *mx_event_from_tag(int argc,Scheme_Object **argv) {
  BSTR tag;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-from-tag");

  pEvent->get_fromTag(&tag);

  return BSTRToSchemeString(tag);
}

Scheme_Object *mx_event_from_id(int argc,Scheme_Object **argv) {
  BSTR id;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-from-id");
  
  pEvent->get_fromId(&id);

  return BSTRToSchemeString(id);
}

Scheme_Object *mx_event_to_tag(int argc,Scheme_Object **argv) {
  BSTR tag;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-to-tag");
  
  pEvent->get_toTag(&tag);

  return BSTRToSchemeString(tag);
}

Scheme_Object *mx_event_to_id(int argc,Scheme_Object **argv) {
  BSTR id;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-to-id");
  
  pEvent->get_toId(&id);

  return BSTRToSchemeString(id);
}

Scheme_Object *mx_event_x(int argc,Scheme_Object **argv) {
  long x;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-x");
  
  pEvent->get_x(&x);

  return scheme_make_integer(x);
}

Scheme_Object *mx_event_y(int argc,Scheme_Object **argv) {
  long y;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-y");
  
  pEvent->get_y(&y);

  return scheme_make_integer(y);
}

Scheme_Object *mx_event_type_pred(int argc,Scheme_Object **argv,WCHAR *evType) {
  EVENT_TYPE actualType;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"com-event-<event-type>?");

  pEvent->get_eventType(&actualType);

  if (wcscmp(evType,eventNames[actualType]) == 0) {
      return scheme_true;
  }
  
  return scheme_false;
}
  
Scheme_Object *mx_event_keypress_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keypress");
}

Scheme_Object *mx_event_keydown_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keydown");
}

Scheme_Object *mx_event_keyup_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keyup");
}

Scheme_Object *mx_event_mousedown_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mousedown");
}

Scheme_Object *mx_event_mouseover_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseover");
}

Scheme_Object *mx_event_mousemove_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mousemove");
}

Scheme_Object *mx_event_mouseout_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseout");
}

Scheme_Object *mx_event_mouseup_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseup");
}

Scheme_Object *mx_event_click_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"click");
}

Scheme_Object *mx_event_dblclick_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"dblclick");
}

Scheme_Object *mx_event_error_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"error");
}


Scheme_Object *mx_get_event(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IEvent *pEvent;
  IEventQueue *pEventQueue;
  MX_Event *event_object;
  
  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("mx-get-event","mx-document",0,argc,argv) ;
  }

  pEventQueue = MX_DOCUMENT_EVENTQUEUE(argv[0]);

  pEvent = NULL; // DCOM requires this for some reason

  hr = pEventQueue->GetEvent(&pEvent);  // blocking call

  if (hr != S_OK || pEvent == NULL) {
    scheme_signal_error("Error retrieving event: %X",hr);
  }

  event_object = (MX_Event *)scheme_malloc(sizeof(MX_Event));

  event_object->type = mx_event_type;
  event_object->pEvent = pEvent;

  mx_register_com_object((Scheme_Object *)event_object,pEvent);

  return (Scheme_Object *)event_object;
}
