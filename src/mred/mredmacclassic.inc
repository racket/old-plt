/* mredmacclassic.inc : non-carbon event handling stuff.
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
 * 2. high level events.  I believe that high level events
 *    actually do not occur any more under OS X.  I could be
 *    wrong.  in any case, they're dispatched immediately.
 *    I'm not sure why.
 * 3. suspendResumeMessage.  Per matthew's comment above, 
 *    these messages evidently don't work somehow.  So 
 *    MrEd detects them manually.  Maybe we can get rid
 *    of this too.
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
 * Application queue, sticks them in the MrEd queue, and returns 1,
 * unless it was called less than delay_time ago, in which case do
 * nothing and return 0.
 */
 
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
 
#define leaveEvt 42

