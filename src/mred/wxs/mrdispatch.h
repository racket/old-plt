
#ifndef MR_DISPATCH_H
#define MR_DISPATCH_H

typedef int (*wxDispatch_Check_Fun)(void *);
typedef int (*wxDispatch_Check_Fun_FPC)(void *, Scheme_Schedule_Info *sinfo);
typedef void (*wxDispatch_Needs_Wakeup_Fun)(void *, void *);

extern void wxDispatchEventsUntil(wxDispatch_Check_Fun f, void *data);
extern void wxDispatchEventsUntilWakeable(wxDispatch_Check_Fun_FPC f, wxDispatch_Needs_Wakeup_Fun wu, void *data);

#endif
