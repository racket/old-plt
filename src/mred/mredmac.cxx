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

static int QueueTransferredEvent(EventRecord *e);
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


void MrEdInitFirstContext(MrEdContext *)
{
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

static int QueueTransferredEvent(EventRecord *e)
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
#ifdef OS_X	  
        RgnHandle updateRegionHandle = NewRgn();
        GetWindowRegion(w,kWindowUpdateRgn,updateRegionHandle);	
        UnionRgn(updateRegionHandle, q->rgn, q->rgn);
#else		
        UnionRgn(((WindowRecord *)w)->updateRgn, q->rgn, q->rgn);
#endif		
        BeginUpdate(w);
        EndUpdate(w);
        done = 1;
      }
    }
  }
    
  if (e->what == kHighLevelEvent) {
    /* We have to dispatch the event immediately */
    AEProcessAppleEvent(e);
    done = 1;
  }

  if (!done) {
    q = new MrQueueElem;
    memcpy(&q->event, e, sizeof(EventRecord));
    q->next = NULL;
    q->prev = last;
    if (last)
      last->next = q;
    else
      first = q;
    last = q;
      
    q->rgn = NULL;
      
    if (e->what == updateEvt) {
      WindowPtr w = (WindowPtr)e->message;
      q->rgn = NewRgn();
#ifdef OS_X
      RgnHandle updateRegion = NewRgn();
      GetWindowRegion(w,kWindowUpdateRgn,updateRegion);
      CopyRgn(updateRegion,q->rgn);
#else      
      CopyRgn(((WindowRecord *)w)->updateRgn, q->rgn);
#endif      
      BeginUpdate(w);
      EndUpdate(w);
    } else if ((e->what == osEvt)
	       && ((e->message >> 24) & 0x0ff) == suspendResumeMessage) {
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
      
      /* This code generates activate events; under classic MacOS, returning an 
       * application to the foreground does not generate (de)activate events.
       */
        
      q->event.what = activateEvt;
      q->event.modifiers = we_are_front ? activeFlag : 0;
      q->event.message = (long)front;
#endif
    }
  }

  return 1;
}

/* Called by wxWindows to queue leave events: */
 
void QueueMrEdEvent(EventRecord *e)
{
  QueueTransferredEvent(e);
}

static void GetSleepTime(int *sleep_time, int *delay_time)
{
#if FG_SLEEP_TIME
  if (last_was_front && Button())
    *sleep_time = 0;
  else
#endif
    *sleep_time = last_was_front ? FG_SLEEP_TIME : BG_SLEEP_TIME;
   
  *delay_time = last_was_front ? DELAY_TIME : 0;
}

/* TransferQueue sucks all of the pending events out of the
   Application queue, sticks them in the MrEd queue, and returns 1,
   unless it was called less than delay_time ago, in which case do
   nothing and return 0. */
 
static int TransferQueue(int all)
{
  EventRecord e;
  short mask;
  int sleep_time, delay_time;
  
  GetSleepTime(&sleep_time, &delay_time);
  
  /* Don't call WaitNextEvent too often. */
  static unsigned long lastTime;
  if (TickCount() <= lastTime + delay_time)
    return 0;

  mask = everyEvent;
  
  while (WaitNextEvent(mask, &e, dispatched ? sleep_time : 0, NULL)) {
    if (!QueueTransferredEvent(&e)) {
      // this never happens: QueueTransferredEvent always returns 1
      break;
    }
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

/***************************************************************************/
/*                                event finders                            */
/***************************************************************************/

static int CheckForLeave(EventRecord *evt, MrQueueRef q, int check_only, MrEdContext *c,   MrEdContext *keyOk, 
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

    if (we_are_front) {     
      TEFromScrap();
      resume_ticks = TickCount();
    } else {
# ifdef OS_X
      ClearCurrentScrap();
# else
      ZeroScrap();
# endif
      TEToScrap();
    }

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
      case mouseDown:
      case mouseUp:
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
    
# ifdef OS_X
    {
      Rect windowBounds;
      GetWindowBounds(w, kWindowContentRgn, &windowBounds);
      OffsetRgn(rgn, -1 * windowBounds.left, -1 * windowBounds.top);
      InvalWindowRgn(w, rgn);
      DisposeRgn(rgn);
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
static void (*mzsleep)(float secs, void *fds);
static pthread_t watcher;
static volatile float sleep_secs;
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
      PostEvent(mouseUp, 0);
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

void MrEdMacSleep(float secs, void *fds, void (*mzsleep)(float secs, void *fds))
{
  EventRecord e;

  if (!msergn)
    msergn = ::NewRgn();
  if (msergn) {
    Point pt;
    GetMouse(&pt);
    LocalToGlobal(&pt);
    ::SetRectRgn(msergn, pt.h - 1, pt.v - 1, pt.h + 1, pt.v + 1); 
  }

#ifdef OS_X
  if (!StartFDWatcher(mzsleep, secs, fds)) {
    secs = 0;
  }
#else 
  /* No way to know when fds are ready: */
  secs = 0;
#endif

  printf("%f\n", secs);
  if (WaitNextEvent(everyEvent, &e, secs ? (long)(secs * 60) : 0x7FFFFFFF, msergn))
    QueueTransferredEvent(&e);

#ifdef OS_X
  EndFDWatcher();
#endif
}

/***************************************************************************/
/*               location->window (used for send-message)                  */
/***************************************************************************/

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
