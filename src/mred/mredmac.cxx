/*
 * File:        mredmac.cc
 * Purpose:     MrEd MacOS event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */

#ifndef OS_X
# define SELF_SUSPEND_RESUME
#endif

#ifdef SELF_SUSPEND_RESUME
/* Note on handling Suspend/Resume events:
    Before OS X, something in the handling of events messes up the sending of 
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

#ifndef WX_CARBON
# include <Events.h>
# include <Processes.h>
# include <Sound.h>
#endif

#define DELAY_TIME 5
#define FG_SLEEP_TIME 0
#define BG_SLEEP_TIME DELAY_TIME

static long resume_ticks;

static int dispatched = 1;

extern "C" {
  typedef void (*HANDLE_AE)(EventRecord *e);
}

static void QueueTransferredEvent(EventRecord *e);
typedef struct MrQueueElem *MrQueueRef;

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

static int queue_size, max_queue_size;

void MrEdInitFirstContext(MrEdContext *)
{
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

/***************************************************************************/
/*                            shadow event queue                           */
/***************************************************************************/

/*
   We need two things from the event queue:

    * We need to handle the event queue non-sequentially.  That is, we
      want to handle certain kinds of events before handling other
      kinds of events.

    * We need to be able to sleep until a new (potentially ready to
      handle) event arrives in the queue.

   The only solution appears to be sucking all of the events into a
   queue of our own, and dealing with them there.  This causes certain
   problems, but not horrible ones.
*/


typedef struct MrQueueElem {
  EventRecord event;
  RgnHandle rgn;
  struct MrQueueElem *next, *prev;
} MrQueueElem;

static MrQueueElem *first, *last;

/* QueueTransferredEvent takes an event and puts it
 * in the MrEd queue, with several exceptions.
 * 1. Update events.  Update events are sent by the OS
 *    whenever the OS queue does not contain an update
 *    event and the update region is not empty.  That is,
 *    the OS will keep poking you until the update region
 *    is empty.  To get around this, QTE clears the update
 *    region manually (and then must reinstate it when it's
 *    time to handle the event.  ick.
 * 2. high level events. Dispatched immediately, and the
 *    handlers queue work in MzScheme threads.
 * 3. suspendResumeMessage. See comment at top.
 */

static void QueueTransferredEvent(EventRecord *e)
{
  MrQueueElem *q;
  int done;
  
  dispatched = 0;
  
  done = 0;
  if (e->what == updateEvt) {
    WindowPtr w = (WindowPtr)e->message;
    for (q = first; q; q = q->next) {
      if ((q->event.what == updateEvt)
	  && (w == ((WindowPtr)q->event.message))) {
        RgnHandle updateRegionHandle = NewRgn();

        GetWindowRegion(w,kWindowUpdateRgn,updateRegionHandle);	
        UnionRgn(updateRegionHandle, q->rgn, q->rgn);
	DisposeRgn(updateRegionHandle);

        BeginUpdate(w);
        EndUpdate(w);
        return;
      }
    }
  }
    
  if (e->what == kHighLevelEvent) {
    /* We have to dispatch the event immediately */
    AEProcessAppleEvent(e);
    return;
  }

  if ((e->what == osEvt) && !(((e->message >> 24) & 0x0ff) == suspendResumeMessage))
    return;

  q = new MrQueueElem;
  memcpy(&q->event, e, sizeof(EventRecord));
  q->next = NULL;
  q->prev = last;
  if (last)
    last->next = q;
  else
    first = q;
  last = q;
  
  queue_size++;
  if (queue_size > max_queue_size) {
    max_queue_size = queue_size;
  }

  q->rgn = NULL;
  
  if (e->what == updateEvt) {
    WindowPtr w = (WindowPtr)e->message;
    q->rgn = NewRgn();
#ifdef WX_CARBON
    GetWindowRegion(w,kWindowUpdateRgn,q->rgn);
#else      
    CopyRgn(((WindowRecord *)w)->updateRgn, q->rgn);
#endif      
    BeginUpdate(w);
    EndUpdate(w);
  } else if (e->what == osEvt) {
    /* Must be a suspend/resume event */

#ifdef SELF_SUSPEND_RESUME
    /* Forget it; we do fg/bg ourselves. See note at top. */
    last = q->prev;
    if (last)
      last->next = NULL;
    else
      first = NULL;
#else
    int we_are_front = e->message & resumeFlag;
    WindowPtr front = FrontWindow();
    
    /* This code generates activate events; under classic MacOS, returning an 
     * application to the foreground does not generate (de)activate events.
     */
    
    q->event.what = activateEvt;
    q->event.modifiers = we_are_front ? activeFlag : 0;
    q->event.message = (long)front;
#endif
  }
}

/* Called by wxWindows to queue leave events: */
 
void QueueMrEdEvent(EventRecord *e)
{
  QueueTransferredEvent(e);
}

static void GetSleepTime(int *sleep_time, int *delay_time)
{
#ifdef OS_X
  /* No need to cooperate: */
  *sleep_time = 0;
  *delay_time = 0;
#else
# if FG_SLEEP_TIME
  if (last_was_front && Button())
    *sleep_time = 0;
  else
# endif
    *sleep_time = last_was_front ? FG_SLEEP_TIME : BG_SLEEP_TIME;
   
  *delay_time = DELAY_TIME;
#endif
}

static int no_modifiers_last_time = 1;
static int last_was_option_down;
static int WeAreFront(); /* forward decl */

/* WNE: a replacement for WaitNextEvent so we can get things like
   wheel events. */
int WNE(EventRecord *e, double sleep_secs)
{
#if 0
  return WaitNextEvent(everyEvent, e, sleep_secs * 60, NULL);
#else
  EventRef ref;

  if (noErr == ReceiveNextEvent(0, NULL, sleep_secs, TRUE, &ref)) {
    Boolean ok, need_click = FALSE;

    if ((GetEventClass(ref) == kEventClassKeyboard)
	&& (GetEventKind(ref) == kEventRawKeyModifiersChanged)) {
      /* Watch for down-up of option => convert to click at 10, 10 */
      UInt32 modifiers;

      GetEventParameter(ref, kEventParamKeyModifiers, typeUInt32, 
			NULL, sizeof(modifiers), NULL, &modifiers);
      
      switch (modifiers & (shiftKey | cmdKey | controlKey | optionKey)) {
      case optionKey:
	if (no_modifiers_last_time)
	  last_was_option_down = 1;
	break;
      case 0:
	no_modifiers_last_time = 1;
	if (last_was_option_down) {
	  last_was_option_down = 0;
	  need_click = TRUE;
	}
	last_was_option_down = 0;
	break;
      default:
	no_modifiers_last_time = 0;
	last_was_option_down = 0;
	break;
      } 
    } else {
      no_modifiers_last_time = 0;
      last_was_option_down = 0;
    }

    ok = ConvertEventRefToEventRecord(ref, e);

    if (!ok) {
      if (need_click) {
	e->what = mouseMenuDown;
	e->message = 0;
	e->modifiers = 0;
	e->where.h = 20;
	e->where.v = 20;
	ok = TRUE;
      } else if ((GetEventClass(ref) == kEventClassMouse)
		 && (GetEventKind(ref) == kEventMouseWheelMoved)) {
	UInt32 modifiers;
	EventMouseWheelAxis axis;
	SInt32 delta;
	Point pos;
	
	GetEventParameter(ref, kEventParamKeyModifiers, typeUInt32, 
			  NULL, sizeof(modifiers), NULL, &modifiers);
	GetEventParameter(ref, kEventParamMouseWheelAxis, 
			  typeMouseWheelAxis, NULL, sizeof(axis), NULL, &axis);
	GetEventParameter(ref, kEventParamMouseWheelDelta, 
			  typeLongInteger, NULL, sizeof(delta), NULL, &delta);
	GetEventParameter(ref, kEventParamMouseLocation,
			  typeQDPoint, NULL, sizeof(Point), NULL, &pos);

	if (axis == kEventMouseWheelAxisY) {
	  e->what = wheelEvt;
	  e->message = (delta > 0);
	  e->modifiers = modifiers;
	  e->where.h = pos.h;
	  e->where.v = pos.v;
	  ok = TRUE;
	}
      } else {
	SendEventToEventTarget(ref, GetEventDispatcherTarget());
      }
    }

    if (ok && (e->what == mouseDown)) {
      /* For bring-to-front: */
      if (!WeAreFront()) {
	SendEventToEventTarget(ref, GetEventDispatcherTarget());
	/* (Maybe sending the event is actually always ok. But
	   we'll just do it here where it's necessary.) */
      }
    }

    ReleaseEvent(ref);

    if (ok)
      no_modifiers_last_time = !(e->modifiers & (shiftKey | cmdKey | controlKey | optionKey));

    return ok;
  }
  return FALSE;
#endif
}

/* TransferQueue sucks all of the pending events out of the
   Application queue, sticks them in the MrEd queue, and returns 1,
   unless it was called less than delay_time ago, in which case do
   nothing and return 0. */
 
static int TransferQueue(int all)
{
  EventRecord e;
  int sleep_time;
  int delay_time;
  
  GetSleepTime(&sleep_time, &delay_time);
  
  /* Don't call WaitNextEvent too often. */
  static unsigned long lastTime;
  if (TickCount() <= lastTime + delay_time)
    return 0;
  
  while (WNE(&e, dispatched ? ((double)sleep_time/60.0) : 0)) {
    QueueTransferredEvent(&e);
  }
  
  lastTime = TickCount();
  
  return 1;
}

static void MrDequeue(MrQueueElem *q)
{
  if (q->prev)
    q->prev->next = q->next;
  else
    first = q->next;
  if (q->next)
    q->next->prev = q->prev;
  else
    last = q->prev;

  --queue_size;
}

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

/***************************************************************************/
/*                               state finder                              */
/***************************************************************************/

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

/***************************************************************************/
/*                                event finders                            */
/***************************************************************************/

static int CheckForLeave(EventRecord *evt, MrQueueRef q, int check_only, 
			 MrEdContext *c, MrEdContext *keyOk, 
			 EventRecord *event, MrEdContext **which) {
  switch (evt->what) {
  case leaveEvt:
    {
      wxWindow *win = (wxWindow *)evt->message;
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

static int CheckForMouseOrKey(EventRecord *e, MrQueueRef osq, int check_only, 
			      MrEdContext *c, MrEdContext *keyOk, 
			      EventRecord *event, MrEdContext **foundc) {
  int found = 0;
  wxFrame *fr;
  MrEdContext *fc;

  switch (e->what) {
  case mouseMenuDown:
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
      } else if (!WindowStillHere(window)) {
	MrDequeue(osq);
      } else {
	MrEdContext *clickOk;

	fr = wxWindowPtrToFrame(window, c);
	fc = fr ? (MrEdContext *)fr->context : NULL;
	
	if (!fr || (c && (fr->context != (void *)c)) 
	    || (!c && !((MrEdContext *)fr->context)->ready))
	  clickOk = NULL;
	else
	  clickOk = fc;

	if (window != front) {
	  /* Handle bring-window-to-front click immediately */
	  if (fc && (!fc->modal_window || (fr == fc->modal_window))) {
	    SelectWindow(window);
	    cont_event_context = NULL;
	  } else if (fc && fc->modal_window) {
	    SysBeep(0);
	    cont_event_context = NULL;
	    SelectWindow(((wxFrame *)fc->modal_window)->macWindow());
	  }
	}

	*foundc = clickOk;
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
  case wheelEvt:
  case keyDown:
  case autoKey:
    *foundc = keyOk;
    if (*foundc) {
      found = 1;
    }
    break;
  case keyUp:
    {
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
    }
    break;
  }

  if (found)
    memcpy(event, e, sizeof(EventRecord));

  return found;
}

static int CheckForActivate(EventRecord *evt, MrQueueRef q, int check_only, 
			    MrEdContext *c, MrEdContext *keyOk, 
			    EventRecord *event, MrEdContext **which)
{
  WindowPtr window;

  switch (evt->what) {
#ifndef OS_X    
    // OS X does not support the diskEvt event.
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

static int CheckForUpdate(EventRecord *evt, MrQueueRef q, int check_only, 
			  MrEdContext *c, MrEdContext *keyOk, 
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
	/* don't dequeue... done in the dispatcher */
	return TRUE;
      }
    } else {
      DisposeRgn(q->rgn);
      MrDequeue(q);
    }
    break;
  }

  return FALSE;
}

/***************************************************************************/
/*                             get next event                              */
/***************************************************************************/

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

  if (!TransferQueue(0))
    kill_context = 0;
    
  if (cont_event_context)
    if (!WindowStillHere(cont_event_context_window))
      cont_event_context = NULL;
  
#ifdef SELF_SUSPEND_RESUME 
  /* Do fg/bg ourselves. See note at top. */
  we_are_front = WeAreFront();
  if (we_are_front != last_was_front) {
    last_was_front = we_are_front;

# ifndef WX_CARBON
    if (we_are_front) {     
      TEFromScrap();
      resume_ticks = TickCount();
    } else {
      ZeroScrap();
      TEToScrap();
    }
# endif

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

      queue_size++;
      
      if (we_are_front)
        wxSetCursor(NULL); /* reset cursor */
    }
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
    /* Remove intervening mouse/key events: */
    MrQueueElem *qq, *next;
    for (qq = first; qq && (qq != osq); qq = next) {
      next = qq->next;
      switch (qq->event.what) {
      case mouseMenuDown:
      case mouseDown:
      case mouseUp:
      case wheelEvt:
      case keyDown:
      case keyUp:
      case autoKey:
	MrDequeue(qq);
	break;
      }
    }

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
    
# ifdef WX_CARBON
    {
      Rect windowBounds;
      RgnHandle contentRgn = NewRgn();

      GetWindowBounds(w, kWindowContentRgn, &windowBounds);
      GetWindowRegion(w, kWindowContentRgn, contentRgn);
      SectRgn(contentRgn, rgn, rgn);
      OffsetRgn(rgn, -1 * windowBounds.left, -1 * windowBounds.top);
      InvalWindowRgn(w, rgn);
      DisposeRgn(rgn);
      DisposeRgn(contentRgn);
    }
# else
    if (!((WindowRecord *)w)->updateRgn)
      ((WindowRecord *)w)->updateRgn = rgn;
    else {
      RgnHandle update = ((WindowRecord *)w)->updateRgn;
      UnionRgn(update, rgn, update);
      DisposeRgn(rgn);
    }
# endif
  }

  wxTheApp->doMacPreEvent();
  wxTheApp->doMacDispatch(e);
  wxTheApp->doMacPostEvent();
  
  wxCheckFinishedSounds();
}

int MrEdCheckForBreak(void)
{
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
  
  return FALSE;
}

/***************************************************************************/
/*                                 sleep                                   */
/***************************************************************************/

#ifdef OS_X
#include <pthread.h>
static volatile int thread_running, need_post;
static SLEEP_PROC_PTR mzsleep;
static pthread_t watcher;
static volatile float sleep_secs;
static ProcessSerialNumber psn;


/* These file descriptors act as semaphores: */
static int watch_read_fd, watch_write_fd;
static int watch_done_read_fd, watch_done_write_fd;

static void *do_watch(void *fds)
{
  while (1) {
    char buf[1];

    read(watch_read_fd, buf, 1);

    mzsleep(sleep_secs, fds);
    if (need_post) {
      need_post = 0;
      WakeUpProcess(&psn);
    }

    write(watch_done_write_fd, "y", 1);
  }

  return NULL;
}

static int StartFDWatcher(void (*mzs)(float secs, void *fds), float secs, void *fds)
{
  if (!watch_write_fd) {
    int fds[2];
    if (!pipe(fds)) {
      watch_read_fd = fds[0];
      watch_write_fd = fds[1];
    } else {
      return 0;
    }
  }

  if (!watch_done_write_fd) {
    int fds[2];
    if (!pipe(fds)) {
      watch_done_read_fd = fds[0];
      watch_done_write_fd = fds[1];
    } else {
      return 0;
    }
  }

  if (!watcher) {
    GetCurrentProcess(&psn);
    if (pthread_create(&watcher, NULL,  do_watch, fds)) {
      return 0;
    }
  }

  mzsleep = mzs;
  sleep_secs = secs;
  thread_running = 1; 
  need_post = 1;
  write(watch_write_fd, "x", 1);
}

static void EndFDWatcher(void)
{
  char buf[1];

  if (thread_running) {
    void *val;

    if (need_post) {
      need_post = 0;
      scheme_signal_received();
    }

    read(watch_done_read_fd, buf, 1);
    thread_running = 0;
  }
}
#endif

static RgnHandle msergn;

void MrEdMacSleep(float secs, void *fds, SLEEP_PROC_PTR mzsleep)
{
  /* If we're asked to sleep less than 1/60 of a second, then don't
     bother with WaitNextEvent. */
  if ((secs > 0) && (secs < 1.0/60)) {
    mzsleep(secs, fds);
  } else {
    EventRecord e;

#ifdef OS_X
    if (!StartFDWatcher(mzsleep, secs, fds)) {
      secs = 0;
    }
#endif

    if (WNE(&e, secs ? secs : kEventDurationForever))
      QueueTransferredEvent(&e);

#ifdef OS_X
    EndFDWatcher();
#endif
  }
}

/***************************************************************************/
/*               location->window (used for send-message)                  */
/***************************************************************************/

wxWindow *wxLocationToWindow(int x, int y)
{
  WindowPtr f;
  Rect bounds;

  f = FrontWindow();
  while (f) {
    GetWindowBounds(f, kWindowContentRgn, &bounds);
    if (IsWindowVisible(f)
	&& (bounds.left <= x)
	&& (bounds.right >= x)
	&& (bounds.top <= y)
	&& (bounds.bottom >= y)) {
      /* Found it */
      wxFrame *frame;

      frame = (wxFrame *)GetWRefCon(f);  

      if (frame) {
	/* Mac: some frames really represent dialogs. Any modal frame is
	   a dialog, so extract its only child. */
	if (frame->IsModal()) {
	  wxChildNode *node2;
	  wxChildList *cl;
	  cl = frame->GetChildren();
	  node2 = cl->First();
	  if (node2)
	    return (wxWindow *)node2->Data();
	} else
	  return frame;
      } else
	return NULL;
    }
    f = GetNextWindow(f);
  }
  
  return NULL;
}

/***************************************************************************/
/*                                gc                                       */
/***************************************************************************/

void wxmac_reg_globs(void)
{
  wxREGGLOB(first);
  wxREGGLOB(last);
  wxREGGLOB(cont_event_context);
}

/***************************************************************************/
/*                            AppleEvents                                  */
/***************************************************************************/

static Scheme_Object *record_symbol, *file_symbol;

static long check_four(char *name, int which, int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[which];

  if (!SCHEME_STRINGP(o) || (SCHEME_STRTAG_VAL(o) != 4))
    scheme_wrong_type(name, "MacOS type/creator 4-character string", which, argc, argv);
  
  return *(long *)SCHEME_STR_VAL(o);
}

static int has_null(const char *s, long l)
{
  if (!l)
    return 1;

  while (l--) {
    if (!s[l])
      return 1;
  }

  return 0;
}

#ifdef OS_X

/* Provided by MzScheme for Classic: */

int scheme_mac_path_to_spec(const char *filename, FSSpec *spec)
{
  FSRef fsref;
  OSErr err;
	
  // first, convert to an FSRef
	
  err = FSPathMakeRef((const UInt8 *)filename,&fsref,NULL);
	
  if (err != noErr) {
    return 0;
  }
	
  // then, convert to an FSSpec
  err = FSGetCatalogInfo(&fsref, kFSCatInfoNone, NULL, NULL, spec, NULL);
	
  if (err != noErr) {
    return 0;
  }
	
  return 1;
}	

char *scheme_mac_spec_to_path(FSSpec *spec)
{
  FSRef fileRef;
  int longEnough = FALSE;
  int strLen = 256;
  char *str;
    
  str = (char *)scheme_malloc_atomic(strLen);
    
  // first, convert to an FSRef
  if (FSpMakeFSRef(spec,&fileRef) != noErr) {
    return NULL;
  }
    
  while (! longEnough) {
    if (FSRefMakePath(&fileRef,(unsigned char *)str,strLen) == pathTooLongErr) {
      strLen *= 2;
      str = (char *)scheme_malloc_atomic(strLen);
    } else {
      longEnough = TRUE;
    }
  }
    
  return str;
}

#endif // OS_X

static int ae_marshall(AEDescList *ae, AEDescList *list_in, AEKeyword kw, Scheme_Object *v, 
		       char *name, OSErr *err, char **stage)
{
  DescType type;
  Ptr data;
  Size size;
  Boolean x_b;
  long x_i;
  double x_d;
  FSSpec x_fss;
  Handle alias = NULL;
  int retval = 1;
    
  switch (SCHEME_TYPE(v)) {
  case scheme_true_type:
  case scheme_false_type:
    x_b = SCHEME_TRUEP(v) ? TRUE : FALSE;
    type = typeBoolean;
    data = (char *)&x_b;
    size = sizeof(Boolean);
    break;
  case scheme_integer_type:
    x_i = SCHEME_INT_VAL(v);
    type = typeLongInteger;
    data = (char *)&x_i;
    size = sizeof(long);
    break;
  case scheme_string_type:
    type = typeChar;
    data = SCHEME_STR_VAL(v);
    size = SCHEME_STRTAG_VAL(v);
    break;
  case scheme_float_type:
  case scheme_double_type:
    x_d = SCHEME_FLOAT_VAL(v);
    type = typeFloat;
    data = (char *)&x_d;
    size = sizeof(double);
    break;
  case scheme_vector_type: /* vector => record */
    if ((SCHEME_VEC_SIZE(v) >= 1)
	&& ((SCHEME_VEC_ELS(v)[0] == record_symbol)
	    || (SCHEME_VEC_ELS(v)[0] == file_symbol))) {
      if (SCHEME_VEC_ELS(v)[0] == file_symbol) {
	if ((SCHEME_VEC_SIZE(v) == 2)
	    && SCHEME_STRINGP(SCHEME_VEC_ELS(v)[1]))  {
	  char *s = SCHEME_STR_VAL(SCHEME_VEC_ELS(v)[1]);
	  long l = SCHEME_STRTAG_VAL(SCHEME_VEC_ELS(v)[1]);
	  if (!has_null(s, l)) {
	    if (scheme_mac_path_to_spec(s, &x_fss)) {
	      *err = NewAliasMinimal(&x_fss, (AliasHandle *)&alias);
	      if (*err == -43) {
	        /* Can't make alias; make FSSpec, instead */
	        type = typeFSS;
	        data = (char *)&x_fss;
	        size = sizeof(FSSpec);
	        break;
	      } else if (*err) {
		*stage = "converting file to alias: ";
		return NULL;
	      }
	      type = typeAlias;
	      HLock(alias);
	      data = (char *)*alias;
	      size = GetHandleSize(alias);
	      break;
	    }
	  }
	}
	scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
			 "%s: cannot interpret vector as a file specification: %V",
			 name,
			 v);
      }
      /* record case falls through to list */
    } else {
      scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		       "%s: cannot convert ill-tagged or untagged vector: %V",
		       name,
		       v);
    }
  case scheme_pair_type: /* /\ falls through */
  case scheme_null_type:
    {
      int l;
      int isrec = SCHEME_VECTORP(v);
        
      if (isrec)
	v = SCHEME_CDR(scheme_vector_to_list(v));
        
      l = scheme_proper_list_length(v);
      if (l >= 0) {
	AEDescList *list;
	list = new WXGC_ATOMIC AEDescList;
          
        list->descriptorType = typeNull;
        list->dataHandle = NULL;
	*err = AECreateList(NULL, 0, isrec, list);
	if (*err) {
	  *stage = "cannot create list/record: ";
	  return 0;
	}
		  
	while (!SCHEME_NULLP(v)) {
	  Scheme_Object *a = SCHEME_CAR(v);
	  AEKeyword rkw;
	  if (isrec) {
	    Scheme_Object *k;
	    if (!SCHEME_PAIRP(a)
		|| !SCHEME_PAIRP(SCHEME_CDR(a))
		|| !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(a)))
		|| !SCHEME_STRINGP(SCHEME_CAR(a))) {
	      /* Bad record form. */
	      scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
			       "%s: cannot interpret vector part as a record field: %s",
			       name,
			       scheme_make_provided_string(a, 1, NULL));
	    }
	    k = SCHEME_CAR(a);
	    a = SCHEME_CADR(a);
	    rkw = check_four(name, 0, 1, &k);
	  } else
	    rkw = 0;
	  if (!ae_marshall(NULL, list, rkw, a, name, err, stage)) {
	    AEDisposeDesc(list);
	    return 0;
	  }
	  v = SCHEME_CDR(v);
	}
		  
	if (list_in) {
	  if (kw)
	    *err = AEPutKeyDesc(list_in, kw, list);
	  else
	    *err = AEPutDesc(list_in, 0, list);
	  if (*err) {
	    *stage = "cannot add list item: ";
	    AEDisposeDesc(list);
	    return 0;
	  }
	} else {
	  if (kw)
	    *err = AEPutParamDesc(ae, kw, list);
	  else
	    *err = AEPutParamDesc(ae, keyDirectObject, list);
	  if (*err) {
	    *stage = "cannot install argument: ";
	    AEDisposeDesc(list);
	    return 0;
	  }
	}
		
	AEDisposeDesc(list);
		  
	return 1;
      }
    }
  default:
    /* Don't know how to marshall */
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "%s: cannot convert value for sending: %s",
		     name,
		     scheme_make_provided_string(v, 1, NULL));
    return 0;
  }
    
  if (list_in) {
    if (kw)
      *err = AEPutKeyPtr(list_in, kw, type, data, size);
    else
      *err = AEPutPtr(list_in, 0, type, data, size);
    if (*err) {
      *stage = "cannot add list item: ";
      retval = 0;
    }
  } else {
    if (kw)
      *err = AEPutParamPtr(ae, kw, type, data, size);
    else
      *err = AEPutParamPtr(ae, keyDirectObject, type, data, size);
    if (*err) {
      *stage = "cannot install argument: ";
      retval = 0;
    }
  }

  if (alias)
    DisposeHandle(alias);
	
  return retval;
}

static Scheme_Object *ae_unmarshall(AppleEvent *reply, AEDescList *list_in, int pos,
                                    OSErr *err, char **stage, Scheme_Object **record)
{

  DescType rtype;
  long sz;
  AEKeyword kw;
  Scheme_Object *result;

  if (list_in) {
    if (AEGetNthPtr(list_in, pos, typeWildCard, &kw, &rtype, NULL, 0, &sz))
      return scheme_void;
  } else {
    if (AEGetParamPtr(reply, keyDirectObject, typeWildCard, &rtype, NULL, 0, &sz))
      return scheme_void;
  }
  
  {
    Boolean x_b;
    long x_i;
    double x_d;
    char *x_s;
    FSSpec x_f;
    Ptr data;
    
    switch (rtype) {
    case typeBoolean:
      data = (char *)&x_b;
      break;
    case typeLongInteger:
    case typeShortInteger:
      rtype = typeLongInteger;
      data = (char *)&x_i;
      sz = sizeof(long);
      break;
    case typeLongFloat:
    case typeShortFloat:
    case typeExtended:
      rtype = typeFloat;
      data = (char *)&x_d;
      sz = sizeof(double);
      break;
    case typeChar:
      data = x_s = (char *)scheme_malloc_atomic(sz + 1);
      x_s[0] = 0;
      break;
    case typeAlias:
    case typeFSS:
      rtype = typeFSS;
      data = (char *)&x_f;
      sz = sizeof(FSSpec);
      break;
    case typeAEList:
    case typeAERecord:
      {
	AEDescList *list;
	Scheme_Object *first = scheme_null, *last = NULL, *v, *rec, **recp;
	int i;
         
	list = new WXGC_ATOMIC AEDescList;
          
	if (list_in) {
	  if (AEGetNthDesc(list_in, pos, rtype, &kw, list))
	    return NULL;
	  if (record)
	    *record = scheme_make_sized_string((char *)&kw, sizeof(long), 1);
	} else {
	  if (AEGetParamDesc(reply, keyDirectObject, rtype, list))
	    return NULL;
	}
         
	if (rtype == typeAERecord)
	  recp = &rec;
	else
	  recp = NULL;
         
	for (i = 1; (v = ae_unmarshall(NULL, list, i, err, stage, recp)); i++) {
	  if (v == scheme_void)
	    break;
	  else if (!v) {
	    AEDisposeDesc(list);
	    return NULL;
	  } else {
	    Scheme_Object *pr;

	    pr = scheme_make_pair(v, scheme_null);
	    if (recp) {
	      pr = scheme_make_pair(rec, pr);
	      pr = scheme_make_pair(pr, scheme_null);
	    }
	           
	    if (last)
	      SCHEME_CDR(last) = pr;
	    else
	      first = pr;
	    last = pr;
	  }
	}
         
	if (recp)
	  first = scheme_list_to_vector(scheme_make_pair(record_symbol, first));
         
	AEDisposeDesc(list);
	return first;
      }
    default:
      /* Don't know how to un-marshall */
      *err = -1;
      *stage = "error translating the reply to a Scheme value: ";
      return NULL;
    }
    
    if (list_in) {
      *err = AEGetNthPtr(list_in, pos, rtype, &kw, &rtype, data, sz, &sz);
      if (record)
        *record = scheme_make_sized_string((char *)&kw, sizeof(long), 1);
      if (*err) {
        *stage = "lost a list value: ";
        return NULL;
      }
    } else {
      *err = AEGetParamPtr(reply, keyDirectObject, rtype, &rtype, data, sz, &sz);
      if (*err) {
        *stage = "lost the return value: ";
        return NULL;
      }
    }
    
    switch (rtype) {
    case typeBoolean:
      result = (x_b ? scheme_true : scheme_false);
      break;
    case typeLongInteger:
      result = scheme_make_integer(x_i);
      break;
    case typeFloat:
      result = scheme_make_double(x_d);
      break;
    case typeChar:
      result = scheme_make_sized_string(x_s, sz, 0);
      break;
    case typeFSS:
      result = scheme_make_sized_string(scheme_mac_spec_to_path(&x_f), -1, 0);
      break;      
    }
  }
  
  return result;
}

/* Single-threaded ok: */
static mz_jmp_buf escape_while_waiting;
static int escaped = 0;

static int handlerInstalled = 0;
typedef struct ReplyItem {
  long id;
  AppleEvent ae;
  struct ReplyItem *next;
} ReplyItem;
static ReplyItem *reply_queue;

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#define MARKS_FOR_FILE_C
#include "mzmark.c"
END_XFORM_SKIP;
#endif

static pascal Boolean while_waiting(EventRecord *e, long *sleeptime, RgnHandle *rgn)
{
   mz_jmp_buf save;

   if (escaped) return TRUE;

   QueueTransferredEvent(e);
   
   memcpy(&save, &scheme_error_buf, sizeof(mz_jmp_buf));
   if (scheme_setjmp(scheme_error_buf)) {
     memcpy(&escape_while_waiting, &save, sizeof(mz_jmp_buf));
     escaped = 1;
     return TRUE; /* Immediately return to AESend */
   } else {
     scheme_thread_block(0);
     scheme_current_thread->ran_some = 1;
     memcpy(&scheme_error_buf, &save, sizeof(mz_jmp_buf));
   }
   
   return FALSE;
}

static pascal OSErr HandleAnswer(const AppleEvent *evt, AppleEvent *rae, long k)
{
  ReplyItem *r;
  DescType rtype;
  long sz;

  r = new ReplyItem;
  
  AEGetAttributePtr(evt, keyReturnIDAttr, typeLongInteger, &rtype, &r->id, sizeof(long), &sz);
  
  AEDuplicateDesc(evt, &r->ae);

#ifdef MZTAG_REQUIRED
  r->type = scheme_rt_reply_item;
#endif
  r->next = reply_queue;
  reply_queue = r;
  
  return 0;
}

static void wait_for_reply(AppleEvent *ae, AppleEvent *reply)
{
  EventRecord e;
  DescType rtype;
  long id, sz;
  ReplyItem *r, *prev;
  
  if (!handlerInstalled) {
    handlerInstalled = TRUE;
    AEInstallEventHandler(kCoreEventClass, kAEAnswer, NewAEEventHandlerUPP(HandleAnswer), 0, 0);
    wxREGGLOB(reply_queue);
  }
  
  AEGetAttributePtr(ae, keyReturnIDAttr, typeLongInteger, &rtype, &id, sizeof(long), &sz);
  
  while (1) {
    WaitNextEvent(everyEvent, &e, 60, 0L);
    if (e.what == kHighLevelEvent)
      AEProcessAppleEvent(&e);
    else {
      if (while_waiting(&e, NULL, NULL))
	break;
    }
	
    prev = NULL;
    for (r = reply_queue; r; r = r->next) {
      if (r->id == id) {
	/* Got the reply */
	memcpy(reply, &r->ae, sizeof(AppleEvent));
	if (prev)
	  prev->next = r->next;
	else
	  reply_queue = r->next;
	return;
      }
      prev = r;
    }
  }
}

int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, 
			  Scheme_Object **result, int *err, char **stage)
{
  OSErr oerr;
  AEEventClass classid;
  AEEventID eventid;
  AppleEvent *ae = NULL, *reply = NULL;
  AEAddressDesc *target = NULL;
  DescType rtype;
  int retval;
  long ret, sz, dst;

  if (!record_symbol) {
    wxREGGLOB(record_symbol);
    wxREGGLOB(file_symbol);

    record_symbol = scheme_intern_symbol("record");
    file_symbol = scheme_intern_symbol("file");
  }

  dst = check_four(name, 0, argc, argv);
  classid = check_four(name, 1, argc, argv);
  eventid = check_four(name, 2, argc, argv);

  target = new WXGC_ATOMIC AEAddressDesc;
  oerr = AECreateDesc(typeApplSignature, &dst, sizeof(long), target);
  if (oerr) {
    *err = (int)oerr;
    *stage = "application not found: ";
    goto fail;
  }
    
  ae = new WXGC_ATOMIC AppleEvent;
  oerr = AECreateAppleEvent(classid, eventid, target, kAutoGenerateReturnID, 
                            kAnyTransactionID, ae);
  if (oerr) {
    *err = (int)oerr;
    *stage = "cannot create event: ";
    ae = NULL;    
    goto fail;
  }
  
  if ((argc > 3) && !SCHEME_VOIDP(argv[3])) {
    if (!ae_marshall(ae, NULL, 0, argv[3], name, &oerr, stage)) {
      *err = (int)oerr;
      goto fail;
    }
  }
  
  if (argc > 4) {
    Scheme_Object *l = argv[4];
    char *expected = "list of pairs containing a type-string and a value";
    while (SCHEME_PAIRP(l)) {
      Scheme_Object *a = SCHEME_CAR(l), *k, *v;
      AEKeyword kw;
      /* Must be a list of 2-item lists: keyword and value */
      if (!SCHEME_PAIRP(a) 
          || !SCHEME_PAIRP(SCHEME_CDR(a))
          || !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(a)))
          || !SCHEME_STRINGP(SCHEME_CAR(a)))
        break; /* => type error */
      k = SCHEME_CAR(a);
      v = SCHEME_CADR(a);
      kw = check_four(name, 0, 1, &k);
      if (!ae_marshall(ae, NULL, kw, v, name, &oerr, stage)) {
	*err = (int)oerr;
        goto fail;
      }
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_type(name, expected, 4, argc, argv);
  }
  
  reply = new WXGC_ATOMIC AppleEvent;
  oerr = AESend(ae, reply, kAEQueueReply | kAECanInteract, kAENormalPriority, kNoTimeOut, NULL, NULL);
  if (oerr) {
    *err = (int)oerr;
    *stage = "send failed: ";
    reply = NULL;
    goto fail;
  }
  wait_for_reply(ae, reply);
  if (escaped) {
     reply = NULL;
     escaped = 0;
     memcpy(&scheme_error_buf, &escape_while_waiting, sizeof(mz_jmp_buf));
     memset(&escape_while_waiting, 0, sizeof(mz_jmp_buf));
     goto escape;
  }
  
  if (!AEGetParamPtr(reply, keyErrorString, typeChar, &rtype, NULL, 0, &sz) && sz) {
    *err = -1;
    if (sz > 256) sz = 256;
    *stage = (char *)scheme_malloc_atomic(sz + 1);
    (*stage)[sz] = 0;
    AEGetParamPtr(reply, keyErrorString, typeChar, &rtype, *stage, sz, &sz);
    goto fail;
  }
  if (!AEGetParamPtr(reply, keyErrorNumber, typeLongInteger, &rtype, &ret, sizeof(long), &sz)
      && ret) {
    *err = (int)ret;
    
    *stage = "application replied with error: ";
    goto fail;
  }
  
  *result = ae_unmarshall(reply, NULL, 0, &oerr, stage, NULL);
  if (!*result) {
    *err = (int)oerr;
    goto fail;
  }
  
succeed:
  retval = 1;
  goto done;
escape:
  retval = -1;
  goto done;
fail:
  retval = 0;
   
done:
  if (ae) AEDisposeDesc(ae);
  if (reply) AEDisposeDesc(reply);
  if (target) AEDisposeDesc(target);
  
  if (retval < 0) {
    scheme_longjmp(scheme_error_buf, 1);
  }
  
  return retval;
}

