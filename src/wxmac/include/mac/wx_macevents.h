/* wx_macevents.h : mac-specific declarations to handle 'leave' events
 */
 

#if 0 //#ifdef OS_X
extern OSErr QueueMrEdCarbonEvent(EventRef e);

extern UInt32 kEventClassMrEd;
extern UInt32 kEventMrEdLeave;
extern UInt32 typeWxWindowPtr;

#else

extern void QueueMrEdEvent(EventRecord *e);

#endif

