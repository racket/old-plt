/*
 * File:        mredmac.cc
 * Purpose:     MrEd MacOS event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */

#define SELF_SUSPEND_RESUME

#ifdef SELF_SUSPEND_RESUME
/* Note on handling Suspend/Resume events:
    Something in the handling of events messes up the sending of 
    suspend and resume events. So, we ignore these events if they happen 
    to occur, but notice suspension and resumption ourselves (by testing 
    for the current process).
*/
static int last_was_front;
#else
# define last_was_front 1
#endif

#include "wx_main.h"
#include "wx_media.h"
#include "scheme.h"
#include "wx_macevents.h"

#include "mred.h"

#ifndef OS_X
# include <Events.h>
# include <Processes.h>
# include <Sound.h>
#endif

#define FG_SLEEP_TIME 0
#define BG_SLEEP_TIME 30
#define DELAY_TIME 5

static long resume_ticks;

static int dispatched = 1;

#define leaveEvt 42

#if defined(OS_X)
# define XX_USE_OS_X_EVENTHANDLER
#endif

#ifdef USE_OS_X_EVENTHANDLER
static OSErr InstallAEventHandler();
typedef EventRef MrQueueRef;
static EventQueueRef mainQueue = NULL;
#else
static int QueueTransferredEvent(EventRecord *e);
typedef struct MrQueueElem *MrQueueRef;
#endif


typedef struct {
  int check_only;
  MrEdContext *c;
  MrEdContext *keyOk;
  EventRecord *event;
  MrEdContext **which;
  int (*checker)(EventRecord *evt, MrQueueRef q, int check_only, 
                 MrEdContext *c, MrEdContext *keyOk, 
	         EventRecord *event, MrEdContext **which);
} EventFinderClosure;


void MrEdInitFirstContext(MrEdContext *)
{
#ifdef USE_OS_X_EVENTHANDLER
  // result ignored:
  InstallAEventHandler();
  
  mainQueue = GetMainEventQueue();
#endif
#ifdef MACINTOSH_EVENTS
  scheme_handle_aewait_event = QueueMrEdEvent;
#endif
}

void MrEdInitNewContext(MrEdContext *)
{
} 

void MrEdDestroyContext(MrEdFinalizedContext *)
{
}

int MrEdGetDoubleTime(void)
{
  return (int)(GetDblTime() * 16.67);
}

static wxFrame *_wxWindowPtrToFrame(WindowPtr w, wxChildList *l)
{
  wxChildNode *n;

  for (n = l->First(); n; n = n->Next()) {
    wxFrame *f = (wxFrame *)n->Data();
    if (f->macWindow() == w)
      return f;
  }

  return NULL;
}

static wxFrame *wxWindowPtrToFrame(WindowPtr w, MrEdContext *c)
{
  if (c)
    return _wxWindowPtrToFrame(w, c->topLevelWindowList);
  else {
    for (c = mred_contexts; c; c = c->next) {
      wxFrame *f;
      if ((f = _wxWindowPtrToFrame(w, c->topLevelWindowList)))
	return f;
    }
  }

  return NULL;
}

/* Note on Carbon Events: Under classic, there's no way to handle the event queue
 * non-sequentially.  That is, we want to handle certain kinds of events before
 * handling other kinds of events.  The only solution is to suck all of the events
 * into a queue of our own, and deal with them there.  This causes certain problems,
 * but not horrible ones.  Under Carbon, however, there's no need for this trickery,
 * because we can scan the event queue for certain kinds of events.  So this file
 * has a severe personality split. Of course, there's always the possibility of 
 * carbonizing the classic version.  This would probably simplify things in the 
 * long run.
 */

/* question: does the carbon event manager call ReleaseEvent() for you, even if you 
 * handle the event yourself? I believe it does.
 */
 
#ifdef USE_OS_X_EVENTHANDLER

UInt32 kEventClassMrEd = 'MrEd';
UInt32 kEventMrEdLeave = 'LEEV';

UInt32 typeWxWindowPtr = FOUR_CHAR_CODE('WinP'); /* wxWindow * */

static OSErr QueueMrEdCarbonEvent(EventRef e)
{
  OSErr err;
  
  err = PostEventToQueue(mainQueue, e, kEventPriorityStandard);
  if (err != noErr) {
  	return err;
  }
}

/* after installing this Apple Event Handler, the main event loop should never even see
 * the apple events coming in.
 */
 
static OSStatus myAEventHandler(EventHandlerCallRef inHandlerCallRef, EventRef inEvent, void *inUserData)
{
	EventRecord e;
	
	// result ignored:
	ConvertEventRefToEventRecord(inEvent,&e);
	
	// dump the apple event on the default handler
	AEProcessAppleEvent(&e);
}	
	
static OSErr InstallAEventHandler()
{
    EventTypeSpec typeSpec = {kEventClassAppleEvent, kEventAppleEvent};
    EventTargetRef target = GetApplicationEventTarget();
    char *name = "Apple Events";
    
    return InstallEventHandler(target,NewEventHandlerUPP(myAEventHandler),1,&typeSpec,NULL,NULL);
}

enum {
	kActionRemove = 0,
	kActionAccept
};

static Boolean EventFinder(EventRef inEvent, EventFinderClosure *closure)
{
  EventRecord evt;

  if (GetEventClass(inEvent) == kEventClassMrEd) {
    if (GetEventKind(inEvent) == kEventMrEdLeave) {
      wxWindow *win;
      
      // result ignored:
      GetEventParameter(inEvent,kEventParamDirectObject,typeWxWindowPtr,
      					NULL, sizeof(wxWindow *), NULL, &win);
      			
      
      evt.what = leaveEvt;
      evt.message = (long)win;
    } else
      return FALSE; // unknown event
    } else
       ConvertEventRefToEventRecord(inEvent,&evt);


  return closure->checker(&evt, inEvent, closure->check_only, 
                 closure->c, closure->keyOk, 
	         closure->event, closure->which);
}          


EventComparatorUPP eventFinderUPP = NewEventComparatorUPP(EventFinder);

static MrQueueRef Find(EventFinderClosure *closure)
{
  return FindSpecificEventInQueue(mainQueue,eventFinderUPP,closure);
}

static void MrDequeue(MrQueueRef e) {
  RemoveEventFromQueue(mainQueue, e);
  ReleaseEvent(e);
}

void QueueMrEdEvent(EventRecord *e)
{
}
 
#else

#include "mredmacclassic.inc"

static MrQueueRef Find(EventFinderClosure *closure)
{
  MrQueueRef osq, next;

  osq = first;
  while (osq) {
    next = osq->next;

    if (closure->checker(&osq->event, osq, closure->check_only, 
                 closure->c, closure->keyOk, 
	         closure->event, closure->which)) {
       return osq;
    }

    osq = next;
 }

 return NULL;
}

#endif


static MrEdContext *KeyOk(int current_only)
{
  WindowPtr w;
  wxFrame *fr;
  MrEdContext *c;
  
  c = current_only ? MrEdGetContext() : NULL;
  
  w = FrontWindow();
  fr = wxWindowPtrToFrame(w, c);
  if (!fr || (c && (fr->context != (void *)c)) 
      || (!c && !((MrEdContext *)fr->context)->ready))
    return NULL;
  
  return (fr ? (MrEdContext *)fr->context : c);
}

static int WindowStillHere(WindowPtr win)
{
  WindowPtr f = FrontWindow();

  while (f) {
    if (f == win)
      return TRUE;
    f = GetNextWindow(f);
  }

  return FALSE;
}

#if defined(SELF_SUSPEND_RESUME)
static int WeAreFront()
{
  static int inited;
  static ProcessSerialNumber us;
  ProcessSerialNumber front;
  Boolean r;
  
  if (!inited) {
    GetCurrentProcess(&us);
    inited = 1;
  }
  GetFrontProcess(&front);
  SameProcess(&us, &front, &r);
  
  return r;
}
#endif

static int GetMods(void)
{
  KeyMap km;
  int mods = 0;
	  
  GetKeys(km);
  if (km[1] & 32768)
    mods |= cmdKey;
  if (km[1] & 1)
    mods |= shiftKey;
  if (km[1] & 4)
    mods |= optionKey;
  if (km[1] & 8)
    mods |= controlKey;
  
  return mods;
}

/* the cont_event_context is used to keep information about mouse-downs around so
 * that later mouse-ups can be properly handled.
 */
 
static MrEdContext *cont_event_context;
static WindowPtr cont_event_context_window;
static Point last_mouse;
static WindowPtr last_front_window;

#ifdef RECORD_HISTORY
FILE *history;
#endif

static int CheckForLeave(EventRecord *evt, MrQueueRef q, int check_only, MrEdContext *c,   MrEdContext *keyOk, 
	          EventRecord *event, MrEdContext **which) {
    switch (evt->what) {
    case leaveEvt:
      {
        wxWindow *win = (wxWindow *)event->message;
	wxFrame *fr;
	MrEdContext *fc;

        if ((win->__type != -1) && win->IsShown()) {
          fr = (wxFrame *)win->GetRootFrame();
	      fc = fr ? (MrEdContext *)fr->context : NULL;
	      if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	        if (which)
	          *which = fc;

#ifdef RECORD_HISTORY
	        fprintf(history, "leave\n");
	        fflush(history);
#endif

	        if (check_only)
	          return TRUE;
	
	        MrDequeue(q);
	        memcpy(event, evt, sizeof(EventRecord));
	        return TRUE;
	      }
        } else {
          MrDequeue(q);
        }
      }
    }

  return FALSE;
}

static int saw_mup = 0, saw_mdown = 0, saw_kdown = 0, kill_context = 0;

static int CheckForMouseOrKey(EventRecord *e, MrQueueRef osq, int check_only, MrEdContext *c,  MrEdContext *keyOk, 
	          EventRecord *event, MrEdContext **foundc) {
    int found = 0;
  wxFrame *fr;
   MrEdContext *fc;

    switch (e->what) {
      case mouseDown:
      {
	WindowPtr window, front;
	int part;

	saw_mdown = 1;
	
        part = FindWindow(e->where, &window);
	front = FrontWindow();
	if (part == inMenuBar)
	  window = front;

	if (!window) {
	  MrDequeue(osq);
	  found = 1;
	  *foundc = keyOk;
	  cont_event_context = NULL;
        } else if (window != front) {
          /* Handle bring-window-to-front click immediately */
          if (!WindowStillHere(window)) {
            MrDequeue(osq);
          } else {
	    fr = wxWindowPtrToFrame(window, NULL);
	    fc = fr ? (MrEdContext *)fr->context : NULL;
	    if (fc && (!fc->modal_window || (fr == fc->modal_window))) {
	      SelectWindow(window);
	      MrDequeue(osq);
	      cont_event_context = NULL;
	    } else if (fc && fc->modal_window) {
	      SysBeep(0);
	      MrDequeue(osq);
	      cont_event_context = NULL;
	      SelectWindow(((wxFrame *)fc->modal_window)->macWindow());
	    }
	  }
	} else if (resume_ticks > e->when) {
	  /* Clicked MrEd into foreground - toss the event */
	  MrDequeue(osq);
	} else {
	  *foundc = keyOk;
	  if (*foundc) {
	    last_mouse.h = -1;
	    found = 1;
	    if (!check_only && (part != inMenuBar)) {
	      cont_event_context = *foundc;
	      cont_event_context_window = window;
	      kill_context = 0;
	    } else
	      cont_event_context = NULL;
	  }
	}
      }
      break;
    case mouseUp:
      saw_mup = 1;
      if (!cont_event_context) {
      	if (!saw_mdown) {
	  MrDequeue(osq);
        }
      } else if (keyOk == cont_event_context) {
	*foundc = keyOk;
	if (*foundc) {
	  found = 1;
	  if (!check_only)
	    cont_event_context = NULL;
	}
      }
      break;
    case keyDown:
    case autoKey:
      *foundc = keyOk;
      if (*foundc) {
	found = 1;
      }
      break;
    case keyUp:
      if (!cont_event_context) {
        if (!saw_kdown) {
	  MrDequeue(osq);
        }
      } else if (keyOk == cont_event_context) {
	*foundc = keyOk;
	if (*foundc)
	  found = 1;
	if (!check_only)
	  cont_event_context = NULL;
      }
      break;
    }

  if (found)
    memcpy(event, e, sizeof(EventRecord));

  return found;
}

static int CheckForActivate(EventRecord *evt, MrQueueRef q, int check_only, MrEdContext *c,   MrEdContext *keyOk, 
	          EventRecord *event, MrEdContext **which)
 {
   WindowPtr window;

    switch (evt->what) {
#ifndef OS_X    
    // OS X does not support the diskEvt event. Yay!
    case diskEvt:
#endif    
    case kHighLevelEvent:
    {
	MrEdContext *fc;
       fc = NULL;
	   if ((!c && !fc) || (!c && fc->ready) || (fc == c)) {
	    if (which)
	      *which = fc;
        if (check_only)
          return TRUE;
	    memcpy(event, evt, sizeof(EventRecord));
        MrDequeue(q);
	    return TRUE;
	  }
}
	  break;
    case activateEvt:
      window = (WindowPtr)evt->message;
      if (WindowStillHere(window)) {
	wxFrame *fr;
	MrEdContext *fc;

        fr = wxWindowPtrToFrame(window, c);
        fc = fr ? (MrEdContext *)fr->context : NULL;
        if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	  if (which)
	    *which = fc;

#ifdef RECORD_HISTORY
	  fprintf(history, "activate\n");
	  fflush(history);
#endif

	  if (check_only)
	    return TRUE;
	
	  memcpy(event, evt, sizeof(EventRecord));
	  MrDequeue(q);
	  return TRUE;
        }
      } else
	MrDequeue(q);
      break;
    }

  return FALSE;
}

static int CheckForUpdate(EventRecord *evt, MrQueueRef q, int check_only, MrEdContext *c,  MrEdContext *keyOk, 
	          EventRecord *event, MrEdContext **which)
 {
    WindowPtr window;

    switch (evt->what) {
    case updateEvt:
      window = (WindowPtr)evt->message;
      if (WindowStillHere(window)) {
	wxFrame *fr;
	MrEdContext *fc;

	fr = wxWindowPtrToFrame(window, c);
	fc = fr ? (MrEdContext *)fr->context : NULL;
	if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	  if (which)
	    *which = fc;

#ifdef RECORD_HISTORY
	  fprintf(history, "update\n");
	  fflush(history);
#endif

	  if (check_only)
	    return TRUE;
	
	  memcpy(event, evt, sizeof(EventRecord));
	  // MrDequeue(q);
	  return TRUE;
	}
      } else {
#ifndef USE_OS_X_EVENTHANDLER
	DisposeRgn(q->rgn);
#endif
	MrDequeue(q);
      }
      break;
    }

 return FALSE;
}

int MrEdGetNextEvent(int check_only, int current_only,
		     EventRecord *event, MrEdContext **which)
{
  /* Search for an event. Handle clicks in non-frontmost windows
     immediately. */
  MrQueueRef osq;
  EventFinderClosure closure;
  EventRecord *e, ebuf;
  MrEdContext *c, *keyOk, *foundc;
  WindowPtr window;
  int found = 0;
  int we_are_front;

  saw_mup = 0; saw_mdown = 0; saw_kdown = 0;
  kill_context = 0;

  if (!event)
    event = &ebuf;
  
  c = current_only ? MrEdGetContext() : NULL;
    
  keyOk = KeyOk(current_only);
  
#ifdef RECORD_HISTORY
  if (!history) history = fopen("history3", "w");
  fprintf(history, "%lx %lx %lx\n",
  	  c, keyOk, cont_event_context);
#endif

  if (cont_event_context)
    if (!StillDown())
      kill_context = 1;

#ifdef USE_OS_X_EVENTHANDLER  
  // just to give the event manager a little time:
  EventRecord ignored;
  WaitNextEvent(0, // no events
  				&ignored, // will never get filled in
  				0, // don't wait at all
  				NULL); // empty mouse region
#else
  if (!TransferQueue(0))
    kill_context = 0;
#endif
    
  if (cont_event_context)
    if (!WindowStillHere(cont_event_context_window))
      cont_event_context = NULL;
  
#ifdef SELF_SUSPEND_RESUME 
  /* Do fg/bg ourselves. See note at top. */
  we_are_front = WeAreFront();
  if (we_are_front != last_was_front) {
     last_was_front = we_are_front;

     if (we_are_front) {     
       TEFromScrap();
       resume_ticks = TickCount();
     } else {
#ifdef OS_X
       ClearCurrentScrap();
#else
       ZeroScrap();
#endif
       TEToScrap();
     }

#ifndef USE_OS_X_EVENTHANDLER  
	 /* for OS_X, activate events are automatically generated for the frontmost
	  * window in an application when that application comes to the front.
	  */
	       
     WindowPtr front = FrontWindow();
  
     if (front) {
      MrQueueElem *q;

      q = new MrQueueElem;
      q->next = NULL;
      q->prev = last;
      if (last)
	last->next = q;
      else
	first = q;
      last = q;
      
      q->rgn = NULL;
      
    
      q->event.what = activateEvt;
      q->event.modifiers = we_are_front ? activeFlag : 0;
      q->event.message = (long)front;
      
      if (we_are_front)
        wxSetCursor(NULL); /* reset cursor */
    }
#endif
  }
#endif
  
  closure.c = c;
  closure.check_only = check_only;
  closure.keyOk = keyOk;
  closure.event = event;
  closure.which = which;

  /* First, service leave events: */
  closure.checker = CheckForLeave;
  if (Find(&closure))
     return TRUE; 
  
  /* Next, service mouse & key events: */
  closure.checker = CheckForMouseOrKey;
  closure.which = &foundc;
  if (osq = Find(&closure)) {
    found = 1;
  }
  closure.which = which;
  
  if (kill_context && !saw_mup)
    cont_event_context = NULL;
  
  if (found) {
#ifndef USE_OS_X_EVENTHANDLER  
    /* Remove intervening mouse/key events: */
    MrQueueElem *qq, *next;
    for (qq = first; qq && (qq != osq); qq = next) {
      next = qq->next;
      switch (qq->event.what) {
        case mouseDown:
        case mouseUp:
        case keyDown:
        case keyUp:
        case autoKey:
          MrDequeue(qq);
          break;
      }
    }
#endif

    if (which)
      *which = foundc;

#ifdef RECORD_HISTORY
    fprintf(history, "mouse or key\n");
    fflush(history);
#endif

    if (check_only)
      return TRUE;
        
    MrDequeue(osq);
    
    return TRUE;
  }
  
  // TransferQueue(0);
    
  /* Try activate and high-level events: */
  closure.checker = CheckForActivate;
  if (Find(&closure))
     return TRUE; 
  
  /* Update events: */
  closure.checker = CheckForUpdate;
  if (Find(&closure))
     return TRUE; 

  /* Generate a motion event? */
  if (keyOk) {
      GetMouse(&event->where);
      LocalToGlobal(&event->where);
      
      if (((event->where.v != last_mouse.v)
           || (event->where.h != last_mouse.h)
           || last_front_window != FrontWindow())
          && (!cont_event_context || (cont_event_context == keyOk))) {
          
        if (which)
          *which = (cont_event_context ? cont_event_context : keyOk);
	
        if (check_only) {
#ifdef RECORD_HISTORY
	  fprintf(history, "move or drag\n");
	  fflush(history);
#endif
          return TRUE;
        }

        last_mouse.v = event->where.v;
        last_mouse.h = event->where.h;
        last_front_window = FrontWindow();
        
        event->what = nullEvent;
        event->when = TickCount();
        if (cont_event_context) {
	  /* Dragging... */
	  event->modifiers = GetMods() | btnState;
	  event->message = 1;
#ifdef RECORD_HISTORY
	  fprintf(history, "drag\n");
  	  fflush(history);
#endif
        } else {
          event->modifiers = (keyOk ? GetMods() : 0);
	  event->message = (keyOk ? 1 : 0);
#ifdef RECORD_HISTORY
	  fprintf(history, "move\n");
  	  fflush(history);
#endif
        }
        return TRUE;
     }
  }
  
#ifdef RECORD_HISTORY
  fprintf(history, "no event\n");
  fflush(history);
#endif
  
  return FALSE;
}

extern void wxCheckFinishedSounds(void);


void MrEdDispatchEvent(EventRecord *e)
{
  dispatched = 1;

#ifndef USE_OS_X_EVENTHANDLER
  if (e->what == updateEvt) {
    /* Find the update event for this window: */
    RgnHandle rgn;
    MrQueueElem *q;
    WindowPtr w;

    w = (WindowPtr)e->message;

    for (q = first; q; q = q->next) {
      if ((q->event.what == updateEvt)
	  && (w == ((WindowPtr)q->event.message))) {
	rgn = q->rgn;
	MrDequeue(q);
	break;
      }
    }
    
#ifdef OS_X
    RgnHandle copied = NewRgn();
    Rect windowBounds;
    CopyRgn(rgn,copied);
    GetWindowBounds(w,kWindowContentRgn,&windowBounds);
    OffsetRgn(copied,-1 * windowBounds.left,-1 * windowBounds.top);
    InvalWindowRgn(w,copied);
#else
 	if (!((WindowRecord *)w)->updateRgn)
 	  ((WindowRecord *)w)->updateRgn = rgn;
 	else {
       RgnHandle update = ((WindowRecord *)w)->updateRgn;
 	   UnionRgn(update, rgn, update);
       DisposeRgn(rgn);
    }
#endif
  }
#endif
    
  wxTheApp->doMacPreEvent();
  wxTheApp->doMacDispatch(e);
  wxTheApp->doMacPostEvent();
  
  wxCheckFinishedSounds();
}

int MrEdCheckForBreak(void)
{
#ifndef USE_OS_X_EVENTHANDLER  
  MrQueueElem *q;
  
  if (!KeyOk(TRUE))
    return 0;
  
  TransferQueue(0);

  for (q = first; q; q = q->next) {
    if (q->event.what == keyDown) {
      if ((((q->event.message & charCodeMask) == '.') 
	   && (q->event.modifiers & cmdKey))
      	  || (((q->event.message & charCodeMask) == 3) 
	      && (q->event.modifiers & controlKey))) {
        MrDequeue(q);
        return TRUE;
      }
    }
  }
#endif
  
  return FALSE;
}

void MrEdMacSleep(float secs)
{
  secs = 0;
  
  EventRecord e;
  
#ifndef USE_OS_X_EVENTHANDLER
  /* This is right only if there is no TCP blocking */
  RgnHandle rgn;
  rgn = ::NewRgn();
  if (rgn) {
    Point pt;
    GetMouse(&pt);
    LocalToGlobal(&pt);
    ::SetRectRgn(rgn, pt.h - 1, pt.v - 1, pt.h + 1, pt.v + 1); 
  }
  if (WaitNextEvent(everyEvent, &e, secs ? secs * 60 : BG_SLEEP_TIME, rgn))
    QueueTransferredEvent(&e);
#else
  RgnHandle rgn = NULL;
#endif    
}

/**********************************************************************/

wxWindow *wxLocationToWindow(int x, int y)
{
  WindowPtr win;
  Point pt;
  wxFrame *frame;
  
  pt.v = x;
  pt.h = y;
  FindWindow(pt,&win);
 
  if (win == NULL) {
	return NULL;
  }
  
  frame = (wxFrame *)GetWRefCon(win);  
  
  /* Mac: some frames really represent dialogs. Any modal frame is
	 a dialog, so extract its only child. */
  if (frame->IsModal()) {
	wxChildNode *node2;
	wxChildList *cl;
	cl = frame->GetChildren();
	node2 = cl->First();
	if (node2)
	  return (wxWindow *)node2->Data();
  } else {
    return frame;
  }
  
  return NULL;
}
