/*
 * File:        mred.cc
 * Purpose:     MrEd main file, including a hodge-podge of global stuff
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995-98, Matthew Flatt
 */

#define WINDOW_STDIO 1

/* wx_xt: */
#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib

/* wx_motif, for wxTimer: */
#ifdef __GNUG__
# pragma implementation "wx_timer.h"
#endif

#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_buttn.h"
#include "wx_messg.h"
#include "wx_timer.h"
#include "wx_media.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_menu.h"
#include "wx_dcps.h"
#ifdef USE_SENORA_GC
# include "wx_types.h"
#endif
#ifdef wx_mac
# include "simpledrop.h"
#endif
#ifdef wx_msw
# include "wx_wmgr.h"
#endif
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

/* Solaris: getdtablesize sometimes not available */
#if !defined(USE_ULIMIT) && defined(sun) && defined(__svr4__)
# define USE_ULIMIT
#endif

#if defined(wx_motif) || defined(wx_xt)
# include <X11/Xlib.h>
# include <X11/keysymdef.h>
#endif

#ifdef wx_x
# include <sys/types.h>
# include <sys/time.h>
# include <unistd.h>
#  if defined(_IBMR2)
#   include <sys/select.h>
#  endif
# include <signal.h>
#endif

#ifdef wx_msw
# ifdef _MSC_VER
#  include <direct.h>
# else
#  include <dir.h>
# endif
#endif

#ifdef wx_mac
# include <unistd.h>
# include <Events.h>
#endif

#if defined(wx_x) || defined(wx_msw)
# define ADD_OBJ_DUMP 1
#else
# define ADD_OBJ_DUMP 0
#endif

#define INTERRUPT_CHECK_ON 0

#ifdef INCLUDE_WITHOUT_PATHS
# include "wxscheme.h"
# include "wxsmred.h"
# include "wxs_fram.h"
# include "wxs_obj.h"
#else
# include "wxs/wxscheme.h"
# include "wxs/wxsmred.h"
# include "wxs/wxs_fram.h"
# include "wxs/wxs_obj.h"
#endif

wxFrame *mred_real_main_frame;

extern void wxMediaIOCheckLSB(void);

#include "mred.h"

#ifndef USE_SENORA_GC
/* Force initialization of the garbage collector (currently needed
   only when supporting Irix sprocs) */
class GCInit {
public:
  GCInit() {
    GC_INIT();
  }
};
static GCInit _gcinit;
#endif

static Scheme_Env *global_env;

class MrEdApp: public wxApp
{
public:
  Bool initialized;
  int xargc;
  char **xargv;

  MrEdApp();
  wxFrame *OnInit(void);
  void RealInit(void);
#ifdef wx_mac
  char *GetDefaultAboutItemName();
  void DoDefaultAboutItem();
#endif
  int OnExit(void);
};

#if !defined(wx_msw)
MrEdApp *TheMrEdApp;
#else
MrEdApp _TheMrEdApp;
# define TheMrEdApp (&_TheMrEdApp)
#endif

#ifdef wx_mac
DialogPtr startup_dial;
#endif

#ifdef LIBGPP_REGEX_HACK
/* Fixes weirdness with libg++ and the compiler: it tries to
   destroy global regexp objects that were never created. Calling
   the constructor forces the other global values to be initialized. */
# include <Regex.h>
#endif

#ifdef wx_x
extern Display *MrEdGetXDisplay(void);
#endif

/****************************************************************************/
/*                               Contexts                                   */
/****************************************************************************/

MrEdContext *mred_contexts;
static MrEdContext *mred_main_context;
static MrEdContext *mred_only_context;
static MrEdContextFrames *mred_frames;
static wxTimer *mred_timers;
int mred_eventspace_param;
int mred_event_dispatch_param;
Scheme_Type mred_eventspace_type;
static Scheme_Object *def_dispatch;
int mred_ps_setup_param;

typedef struct Context_Manager_Hop {
  Scheme_Type type;
  MrEdContext *context;
} Context_Manager_Hop;


MrEdContext *MrEdGetContext(wxObject *w)
{
  if (w) {
#if !defined(wx_xt) && !defined(wx_mac)
    if (wxSubType(w->__type, wxTYPE_FRAME)) {
#endif
      MrEdContext *c;
      c = (MrEdContext *)((wxFrame *)w)->context;
      if (c) 
	return c;
#if !defined(wx_xt) && !defined(wx_mac)
    } else {
      MrEdContext *c;
      c = (MrEdContext *)((wxDialogBox *)w)->context;
      if (c) 
	return c;
    }
#endif
  }

  if (mred_only_context)
    return mred_only_context;
  else
    return (MrEdContext *)scheme_get_param(scheme_config, mred_eventspace_param);
}

void *wxGetContextForFrame()
{
  if (!TheMrEdApp)
    return NULL;
  else
    return (void *)MrEdGetContext();
}

wxChildList *wxGetTopLevelWindowsList(wxObject *w)
{
  return MrEdGetContext(w)->topLevelWindowList;
}

wxWindow *wxGetModalWindow(wxObject *w)
{
  MrEdContext *c = MrEdGetContext(w);

  return c->modal_window;
}

void wxPutModalWindow(wxObject *w, wxWindow *win)
{
  MrEdContext *c = MrEdGetContext(w);

  c->modal_window = win;
}

wxStandardSnipClassList *wxGetTheSnipClassList()
{
  return MrEdGetContext()->snipClassList;
}

wxBufferDataClassList *wxGetTheBufferDataClassList()
{
  return MrEdGetContext()->bufferDataClassList;
}

int wxGetBusyState(void)
{
  MrEdContext *c = MrEdGetContext();

  return c->busyState;
}

void wxSetBusyState(int state)
{
  MrEdContext *c = MrEdGetContext();

  c->busyState = state;
}

Bool wxIsPrimEventspace()
{
  return MrEdGetContext() == mred_main_context;
}

static int ps_ready = 0;
static wxPrintSetupData *orig_ps_setup;

wxPrintSetupData *wxGetThePrintSetupData()
{
  if (ps_ready) {
    Scheme_Object *o = scheme_get_param(scheme_config, mred_ps_setup_param);
    if (o)
      return wxsUnbundlePSSetup(o);
  }
  return orig_ps_setup;
}

void wxSetThePrintSetupData(wxPrintSetupData *d)
{
  if (ps_ready) {
    scheme_set_param(scheme_config, mred_ps_setup_param, wxsBundlePSSetup(d));
  }
  orig_ps_setup = d;
}


static int num_contexts = 0;

/* Forward decl: */
static int MrEdSameContext(void *c, void *testc);

static void destroy_wxObject(wxWindow *w, void *)
{
  if (w->__gc_external) {
    objscheme_destroy(w, (Scheme_Object *)w->__gc_external);
    ((Scheme_Class_Object *)w->__gc_external)->primflag = -2; /* -2 => shutdown */
    w->__gc_external = NULL;
  }
}

static void kill_eventspace(Scheme_Object *ec, void *)
{
  MrEdContext *c = ((Context_Manager_Hop *)ec)->context;

  if (!c)
    return; /* must not have had any frames or timers */

  {
    wxChildNode *node, *next;
    for (node = c->topLevelWindowList->First(); node; node = next) {
      wxWindow *w = (wxWindow *)node->Data();
      next = node->Next();
      if (w) {
	if (node->IsShown())
	  w->Show(FALSE);
	w->ForEach(destroy_wxObject, NULL);
      }
    }
  }

  {
    wxTimer *t, *next;
    for (t = mred_timers; t; t = next) {
      next = t->next;
      if (t->context == (void *)c)
	t->Stop();
    }
  }

  scheme_remove_sema_callbacks(MrEdSameContext, c);
}

static void CollectingContext(void *cfx, void *)
{
  MrEdFinalizedContext *cf = (MrEdFinalizedContext *)cfx;

  if (cf->frames->next)
    cf->frames->next->prev = cf->frames->prev;
  if (cf->frames->prev)
    cf->frames->prev->next = cf->frames->next;
  else
    mred_frames = cf->frames->next;

  /* Must explicitly delete frames now because their context
     is going away. (The frame would certainly have been finalized
     later during this set of finalizations, but that would be
     too late.) */
  wxChildNode *cnode, *next;
  for (cnode = cf->frames->list->First(); cnode; cnode = next) {
    next = cnode->Next();
    wxFrame *fr = (wxFrame *)cnode->Data();
    if (fr)
      delete fr;
  }

  MrEdDestroyContext(cf);

  delete cf->frames->list;
  cf->frames = NULL;

  --num_contexts;
}

static MrEdContext *MakeContext(MrEdContext *c, Scheme_Config *config)
{
  if (!c) {
    c = (MrEdContext *)scheme_malloc_tagged(sizeof(MrEdContext));

    c->topLevelWindowList = new wxChildList();
    c->snipClassList = wxMakeTheSnipClassList();
    c->bufferDataClassList = wxMakeTheBufferDataClassList();
    c->finalized = new MrEdFinalizedContext;
  }

  c->ready = 1;

  c->handler_running = NULL;

  c->busyState = 0;

  MrEdContextFrames *frames;
  frames = c->finalized->frames = new MrEdContextFrames;
  frames->next = mred_frames;
  frames->prev = NULL;
  frames->list = c->topLevelWindowList;
  if (mred_frames)
    mred_frames->prev = frames;
  mred_frames = frames;

  c->modal_window = NULL;

  if (!config) {
    config = (Scheme_Config *)scheme_branch_config();
    scheme_set_param(config, mred_eventspace_param, (Scheme_Object *)c);
  }

  c->main_config = config;

  scheme_register_finalizer((void *)c->finalized,
			    CollectingContext, NULL,
			    NULL, NULL);

  num_contexts++;

  c->type = mred_eventspace_type;

  Context_Manager_Hop *mr_hop = (Context_Manager_Hop *)scheme_malloc_atomic(sizeof(Context_Manager_Hop));
  mr_hop->type = 0;
  mr_hop->context = c;
  c->mr_hop = mr_hop;
  scheme_weak_reference((void **)&mr_hop->context);

  c->mref = scheme_add_managed(NULL, (Scheme_Object *)mr_hop, kill_eventspace, NULL, 0);

  return c;
}

static void ChainContextsList()
{
  MrEdContextFrames *f = mred_frames;
  wxChildNode *first;
  
  mred_contexts = NULL;

  while (f) {
    first = f->list->First();

    while (first && !first->IsShown())
      first = first->Next();

    if (first) {
      wxObject *o = first->Data();
      MrEdContext *c;
      c = MrEdGetContext(o);
      c->next = mred_contexts;
      mred_contexts = c;
    }
    f = f->next;
  }
}

static void UnchainContextsList()
{
  while (mred_contexts) {
    MrEdContext *next = mred_contexts->next;
    mred_contexts->next = NULL;
    mred_contexts = next;
  }
}

Scheme_Object *MrEdMakeEventspace(Scheme_Config *config)
{
  MrEdContext *c;

  c = MakeContext(NULL, config);

  MrEdInitNewContext(c);

  return (Scheme_Object *)c;
}

Scheme_Object *MrEdEventspaceConfig(Scheme_Object *e)
{
  return (Scheme_Object *)((MrEdContext *)e)->main_config;
}

Scheme_Object *MrEdGetFrameList(void)
{
  MrEdContext *c = MrEdGetContext();
  Scheme_Object *l = scheme_null;

  if (c) {
    wxChildNode *node;
    for (node = c->topLevelWindowList->First(); node; node = node->Next()) {
      wxObject *o = node->Data();
      if (node->IsShown())
	l = scheme_make_pair(objscheme_bundle_wxObject(o), l);
    }
  }

  return l;
}

/****************************************************************************/
/*                               Events                                     */
/****************************************************************************/

static wxTimer *TimerReady(MrEdContext *c)
{
  wxTimer *timer = mred_timers;
  
  if (c) {
    while (timer && (timer->context != (void *)c))
      timer = timer->next;
  } else {
    while (timer && !((MrEdContext *)timer->context)->ready)
      timer = timer->next;
  }

  if (timer) {
    unsigned long now = (unsigned long)scheme_get_milliseconds();
    unsigned long goal = timer->expiration;
    
    return ((now >= goal)
	    ? timer
	    : (wxTimer *)NULL);
  } else
    return NULL;
}

static void DoTimer(wxTimer *timer)
{
  int once;

  if (timer->interval == -1)
    return;

  once = timer->one_shot;
  timer->one_shot = -1;

  timer->Notify();

  if (!once && (timer->one_shot == -1) && (timer->interval != -1))
    timer->Start(timer->interval, FALSE);
}

static int check_for_nested_event(Scheme_Object *cx)
{
  MrEdContext *c = (MrEdContext *)cx;

  return (!c->waiting_for_nested
	  || (c->alternate
	      && c->alternate(c->alt_data)));
}

static int MrEdSameContext(void *c, void *testc)
{
  return (c == testc);
}

static void GoAhead(MrEdContext *c)
{
  c->ready_to_go = 0;

  if (c->sema_callback) {
    c->sema_callback = 0;
    (void)scheme_check_sema_callbacks(MrEdSameContext, c, 0);
  } else if (c->timer) {
    wxTimer *timer;
    timer = c->timer;
    c->timer = NULL;
    DoTimer(timer);
  } else {
    MrEdEvent e;
    
    memcpy(&e, &c->event, sizeof(MrEdEvent));
    
    MrEdDispatchEvent(&e);
  }
}

static Scheme_Object *def_event_dispatch_handler(int argc, Scheme_Object *argv[])
{
  MrEdContext *c;

  c = (MrEdContext *)argv[0];
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), mred_eventspace_type)
      || !c->ready_to_go) {
    scheme_wrong_type("default-event-dispatch-handler",
		      "eventspace (with ready event)",
		      0, argc, argv);
    return NULL;
  }

  GoAhead(c);

  return scheme_void;
}

static void DoTheEvent(MrEdContext *c)
{
  Scheme_Object *a[1], *p;

  c->ready_to_go = 1;

  p = scheme_get_param(scheme_config, mred_event_dispatch_param);
  if (p != def_dispatch) {
    a[0] = (Scheme_Object *)c;
    _scheme_apply_multi(p, 1, a);
  }
  
  if (c->ready_to_go)
    GoAhead(c);
}

void MrEdDoNextEvent(MrEdContext *c, int (*alt)(void *), void *altdata)
{
  wxTimer *timer;
  Scheme_Config *save_config;

  save_config = scheme_config;
  scheme_config = c->main_config;

  if (scheme_check_sema_callbacks(MrEdSameContext, c, 1)) {
    c->sema_callback = 1;
    DoTheEvent(c);
  } else if ((timer = TimerReady(c))) {
    timer->Dequeue();
    c->timer = timer;
    DoTheEvent(c);
  } else if (MrEdGetNextEvent(0, 1, &c->event, NULL)) {
    DoTheEvent(c);
  } else if (c != mred_main_context) {
    c->ready = 1;
    c->waiting_for_nested = 1;

    c->alternate = alt;
    c->alt_data = altdata;

    /* Temp restore config: */
    scheme_config = save_config;

    scheme_current_process->block_descriptor = -1;
    scheme_current_process->blocker = (Scheme_Object *)c;
    scheme_current_process->block_check = check_for_nested_event;
    do {
      scheme_process_block(0);
    } while (!check_for_nested_event((Scheme_Object *)c));
    scheme_current_process->block_descriptor = 0;
    scheme_current_process->ran_some = 1;

    /* un-'Temp restore config': */
    scheme_config = c->main_config;

    c->alternate = NULL;
    c->alt_data = NULL;

    if (c->waiting_for_nested) {
      /* Alternate condition fired. Do nothing. */
      c->ready = 0;
      c->waiting_for_nested = 0;
    } else
      DoTheEvent(c);
  }

  scheme_config = save_config;
}

void wxDoNextEvent()
{
  MrEdContext *c = MrEdGetContext();

  if (!c->ready_to_go)
    if (c->handler_running == scheme_current_process)
      MrEdDoNextEvent(c, NULL, NULL);
}

int MrEdEventReady(MrEdContext *c)
{
  return (TimerReady(c) || MrEdGetNextEvent(1, 1, NULL, NULL)
	  || scheme_check_sema_callbacks(MrEdSameContext, c, 1));
}

int wxEventReady()
{
  MrEdContext *c = MrEdGetContext();

  return (!c->ready_to_go
	  && (c->handler_running == scheme_current_process)
	  && MrEdEventReady(c));
}

static void WaitForAnEvent_OrDie(MrEdContext *c)
{
  c->ready = 1;
  c->waiting_for_nested = 1;
  c->alternate = NULL;

  /* Suspend the thread. If another event is found for the eventspace, the
     thread will be resumed. */
  c->suspended = 1;
  while (1) {
    scheme_weak_suspend_thread(c->handler_running); /* suspend self */

    if (c->waiting_for_nested) {
      /* we were resumed for a break signal, or some such: */
      c->suspended = 0;
      c->ready = 0;
      c->waiting_for_nested = 0;
      
      scheme_process_block(0);
      
      /* Go back to sleep: */
      c->ready = 1;
      c->waiting_for_nested = 1;
      c->suspended = 1;
    } else
      break;
  }

  /* An event has been found. Do it. */
  DoTheEvent(c);

  /* Return to loop and look for more events... */
}

static void on_handler_killed(Scheme_Process *p)
{
  MrEdContext *c = (MrEdContext *)p->kill_data;

  p->on_kill = NULL;
  p->kill_data = NULL;

  /* The thread is forever not ready: */
  c->handler_running = NULL;
  c->ready = 0;
  c->waiting_for_nested = 0;
  c->sema_callback = 0;
  c->timer = NULL;
  c->alternate = NULL;
  c->alt_data = NULL;
  c->ready_to_go = 0;
}

static Scheme_Object *handle_events(void *cx, int, Scheme_Object **)
{
  MrEdContext *c = (MrEdContext *)cx;
  Scheme_Process *this_thread;

#if SGC_STD_DEBUGGING
  fprintf(stderr, "new thread\n");
#endif

  scheme_config = c->main_config;

  this_thread = scheme_current_process;
  c->handler_running = this_thread;
  this_thread->on_kill = on_handler_killed;
  this_thread->kill_data = c;
  c->suspended = 0;
  c->ready = 0;

  if (!scheme_setjmp(scheme_error_buf)) {
    if (!TheMrEdApp->initialized)
      TheMrEdApp->RealInit();
    else {
      DoTheEvent(c);

      while(1) {
	while (MrEdEventReady(c)) {
	  /* reset parameterization in case the last event handler 
	     changed it */
	  scheme_config = c->main_config;
	  
	  MrEdDoNextEvent(c, NULL, NULL);
	}

	WaitForAnEvent_OrDie(c);
      }
    }
  }
   
  /* We should never get here, now. */
  c->ready = 1;
  c->handler_running = NULL;
  this_thread->on_kill = NULL;
  this_thread->kill_data = NULL;

  return scheme_void;
}

static int main_loop_exited = 0;

static int MrEdContextReady(void *, void *c)
{
  return ((MrEdContext *)c)->ready;
}

static void event_found(MrEdContext *c)
{
  c->ready = 0;
  
  if (c->waiting_for_nested) {
    if (c->suspended) {
      c->suspended = 0;
      scheme_weak_resume_thread(c->handler_running);
    }
    c->waiting_for_nested = 0;
  } else
    scheme_thread_w_manager(scheme_make_closed_prim(handle_events, c), 
			    c->main_config,
			    (Scheme_Manager *)scheme_get_param(c->main_config, 
							       MZCONFIG_MANAGER));
}

static int try_dispatch(Scheme_Object *do_it)
{
  MrEdContext *c;
  MrEdEvent e;
  wxTimer *timer;
  int got_one;

  if (main_loop_exited)
    return 1;

  if ((c = (MrEdContext *)scheme_check_sema_callbacks(MrEdContextReady, NULL, 1))) {
    if (!do_it)
      return 1;

    if (SCHEME_FALSEP(do_it))
      scheme_current_process->ran_some = 1;

    if (c == mred_main_context)
      scheme_check_sema_callbacks(MrEdSameContext, c, 0);
    else {
      c->sema_callback = 1;
      event_found(c);
    }

    return 1;
  }

  if ((timer = TimerReady(NULL))) {
    if (!do_it)
      return 1;
    if (SCHEME_FALSEP(do_it))
      scheme_current_process->ran_some = 1;

    c = (MrEdContext *)timer->context;

    timer->Dequeue();

    if (c == mred_main_context)
      timer->Notify();
    else {
      c->timer = timer;
      event_found(c);
    }

    return 1;
  }

  ChainContextsList();

  got_one = MrEdGetNextEvent(!do_it, 0, &e, &c);

  UnchainContextsList();

  if (!got_one)
    return 0;

  if (!do_it)
    return 1;
  if (SCHEME_FALSEP(do_it))
    scheme_current_process->ran_some = 1;

  if (c) {
    memcpy(&c->event, &e, sizeof(MrEdEvent));
    event_found(c);
  } else
    /* Event with unknown context: */
    MrEdDispatchEvent(&e);

  return 1;
}

static void wakeup_on_dispatch(Scheme_Object *, void *fds)
{
#ifdef wx_x
# ifdef wx_xt
  Display *d = XtDisplay(mred_main_context->finalized->toplevel);
# else
  Display *d = XtDisplay(wxTheApp->topLevel);
# endif
  int fd;
  
  fd = ConnectionNumber(d);
  
  MZ_FD_SET(fd, (fd_set *)fds);
#endif
}

static int check_initialized(Scheme_Object *)
{
  return TheMrEdApp->initialized;
}

# define KEEP_GOING wxTheApp->keep_going

void wxDoEvents()
{
  /* When we get here, we are in the main dispatcher thread */
  if (!TheMrEdApp->initialized) {
    MrEdContext *c;

    c = (MrEdContext *)MrEdMakeEventspace(NULL);
    
    scheme_thread(scheme_make_closed_prim(handle_events,
					  c), 
		  c->main_config);

    /* Block until initialized: */
    scheme_current_process->block_descriptor = -1;
    scheme_current_process->blocker = NULL;
    scheme_current_process->block_check = check_initialized;
    scheme_current_process->block_needs_wakeup = NULL;
    do {
      scheme_process_block(0);
    } while (!TheMrEdApp->initialized);
    scheme_current_process->block_descriptor = 0;
  }

  if (!try_dispatch(scheme_true)) {
    do {
      scheme_current_process->block_descriptor = -1;
      scheme_current_process->blocker = NULL;
      scheme_current_process->block_check = try_dispatch;
      scheme_current_process->block_needs_wakeup = wakeup_on_dispatch;

      scheme_process_block(0);

      scheme_current_process->block_descriptor = 0;
      /* Sets ran_some if it succeeds: */
      if (try_dispatch(scheme_false))
	break;
    } while (KEEP_GOING);
  }
}

void wxDispatchEventsUntil(int (*f)(void *), void *data)
{
  MrEdContext *c = MrEdGetContext();

  if (c->ready_to_go
      || (c->handler_running != scheme_current_process)) {
    /* This is not the handler thread or an event still hasn't been
       dispatched. Wait. */
    do {
      scheme_current_process->block_descriptor = -1;
      scheme_current_process->blocker = (Scheme_Object *)data;
      scheme_current_process->block_check = (int (*)(Scheme_Object *))f;
      do {
	scheme_process_block(0);
      } while (!f(data));
      scheme_current_process->block_descriptor = 0;
      scheme_current_process->ran_some = 1;
    } while (!f(data));
  } else {
    /* This is the main process. Handle events */
    do
      MrEdDoNextEvent(c, f, data);
    while (!f(data));
  }
}

static void (*mzsleep)(float secs, void *fds);

static void MrEdSleep(float secs, void *fds)
{
  MrEdContext *c;
  unsigned long now;

  if (!(KEEP_GOING))
    return;

  ChainContextsList();
  
  now = (unsigned long)scheme_get_milliseconds();
  for (c = mred_contexts; c; c = c->next) {
    wxTimer *timer = mred_timers;
    
    while (timer && !((MrEdContext *)timer->context)->ready)
      timer = timer->next;
    
    if (timer) {
      unsigned long done = timer->expiration;
      float diff;
      diff = done - now;
      
      diff /= 1000;
      if (diff <= 0)
	secs = 0.00001;
      else if (!secs || (secs > diff))
	secs = diff;
    }
  }
  
  UnchainContextsList();
  
#ifdef wx_msw
  MrEdMSWSleep(secs, fds);
#else
#ifdef wx_mac
  MrEdMacSleep(secs);
#else
  mzsleep(secs, fds);
#endif
#endif
}

/****************************************************************************/
/*                                wxTimer                                   */
/****************************************************************************/

wxTimer::wxTimer(void)
{
  __type = wxTYPE_TIMER;

  next = prev = NULL;

  context = (void *)MrEdGetContext();

  WXGC_IGNORE(context);
}

wxTimer::~wxTimer(void)
{
}

Bool wxTimer::Start(int millisec, Bool _one_shot)
{
  if (prev || next || (mred_timers == this))
    return FALSE;

  interval = millisec;
  if (interval <= 0)
    interval = 1;
  one_shot = !!_one_shot;

  unsigned long now = (unsigned long)scheme_get_milliseconds();
  expiration = now + interval;

  if (mred_timers) {
    wxTimer *t = mred_timers;

    while (1) {
      int later;

      later = (expiration >= t->expiration);

      if (!later) {
	prev = t->prev;
	t->prev = this;
	next = t;
	if (prev)
	  prev->next = this;
	else
	  mred_timers = this;
	return TRUE;
      }

      if (!t->next) {
	t->next = this;
	prev = t;

	return TRUE;
      }
      t = t->next;
    } 
  } else
    mred_timers = this;

  return TRUE;
}

void wxTimer::Dequeue(void)
{
  if (!prev) {
    if (mred_timers == this)
      mred_timers = next;
  }

  if (prev)
    prev->next = next;
  if (next)
    next->prev = prev;

  next = prev = NULL;
}

void wxTimer::Stop(void)
{
  Dequeue();

  interval = -1;
}

/****************************************************************************/
/*                        Redirected Standard I/O                           */
/****************************************************************************/

#ifndef WINDOW_STDIO
/* Removing "|| defined(wx_msw)" below uses the Windows console.
   The danger is that closing that console kills MrEd without
   any chance of cancelling the kill. */
# if defined(wx_mac) || defined(wx_msw)
#  define WINDOW_STDIO 1
# else
#  define WINDOW_STDIO 0
# endif
#endif

#ifndef WCONSOLE_STDIO
# if defined(wx_msw) && !WINDOW_STDIO
#  define WCONSOLE_STDIO 1
# else
#  define WCONSOLE_STDIO 0
# endif
#endif

#ifndef REDIRECT_STDIO
# if (defined(wx_msw) || defined(wx_mac)) && !WINDOW_STDIO && !WCONSOLE_STDIO
#  define REDIRECT_STDIO 1
# else
#  define REDIRECT_STDIO 0
# endif
#endif

#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
static void MrEdSchemeMessages(char *, ...);
#endif

#if WINDOW_STDIO

static int have_stdio = 0;
static int stdio_kills_prog = 0;

class IOFrame : public wxFrame
{
public:
  wxMediaCanvas *display;
  wxMediaEdit *media;
  Bool hidden, beginEditSeq;

  IOFrame() : wxFrame(NULL, "Standard Output", -1, -1, 600, 400, 0, "stdout")
    {
      display = new wxMediaCanvas(this);
      
      media = new wxMediaEdit();
      display->SetMedia(media);
      media->Lock(1);
      hidden = FALSE;

      /* Map copy keys: */
      wxKeymap *km = media->GetKeymap();
      media->AddBufferFunctions(km);
      media->AddEditorFunctions(km);
# ifdef wx_msw
      km->MapFunction("c:c", "copy-clipboard");
      km->MapFunction("c:x", "copy-clipboard");
# else
      km->MapFunction("d:c", "copy-clipboard");
      km->MapFunction("d:x", "copy-clipboard");
# endif

      /* Fixed-width font: */
      wxStyle *style = media->GetStyleList()->FindNamedStyle("Standard");
      style->SetDelta(*(new wxStyleDelta(wxCHANGE_FAMILY, wxMODERN)));

#ifdef wx_mac
      OnSize(600, 400);
#endif

      wxMenuBar *mb = new wxMenuBar();
      wxMenu *m = new wxMenu();
      m->Append(79, "Copy");
      mb->Append(m, "Edit");
      SetMenuBar(mb);
      
      have_stdio = 1;
      Show(TRUE);

      beginEditSeq = 0;
    }

  void OnSize(int x, int y)
    {
      GetClientSize(&x, &y);
      if (display)
	display->SetSize(0, 0, x, y);
    }

  Bool OnClose(void) 
    { 
      hidden = TRUE;
      if (stdio_kills_prog)
	exit(0);
      else
	have_stdio = 0;
      return TRUE; 
    }

  void OnMenuCommand(int id) 
    {
      if (id == 79)
	media->Copy();
    }
};

static IOFrame *ioFrame = NULL;

#else  /* !WINDOW_STDIO */

#if WCONSOLE_STDIO

static HANDLE console_out;

#else  /* !WCONSOLE_STDIO */

#if REDIRECT_STDIO
static FILE *mrerr = NULL;
#else
#define mrerr stderr
#endif

#endif /* WCONSOLE_STDIO */

#endif /* WINDOW_STDIO */

#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
static void MrEdSchemeMessages(char *msg, ...)
{
  va_list args;

#if WINDOW_STDIO
  static opening = 0;
  if (opening)
	return;
  opening = 1;
  if (!ioFrame) {
    if (mred_only_context)
      ioFrame = new IOFrame;
    else {
      /* Set eventspace ... */
      mred_only_context = mred_main_context;
      ioFrame = new IOFrame;
      mred_only_context = NULL;
    }
  }
  opening = 0;
  if (ioFrame->hidden) {
    ioFrame->hidden = FALSE;
    have_stdio = 1;
    ioFrame->Show(TRUE);
  }
#endif
#if WCONSOLE_STDIO
  if (!console_out) {
    AllocConsole();
    console_out = GetStdHandle(STD_OUTPUT_HANDLE);
  }
#endif
#if REDIRECT_STDIO
  if (!mrerr)
    mrerr = fopen("mrstderr.txt", "w");
  if (!mrerr)
    return;
#endif

  va_start(args, msg);
#if WINDOW_STDIO
  if (!msg) {
    char *s;
    long l;
    
    s = va_arg(args, char*);
    l = va_arg(args, long);

    if (!ioFrame->beginEditSeq) {
      ioFrame->media->BeginEditSequence();
      ioFrame->beginEditSeq = 1;
    }
    ioFrame->media->Lock(0);
    ioFrame->media->Insert(l, s, ioFrame->media->LastPosition());
    ioFrame->media->Lock(1);

    if (l != 1 || s[0] == '\n') {
      ioFrame->media->EndEditSequence();
      ioFrame->beginEditSeq = 0;
    }
  } else {
    char buffer[2048];
    vsprintf(buffer, msg, args);
    ioFrame->media->Lock(0);
    ioFrame->media->Insert((char *)buffer, ioFrame->media->LastPosition());
    ioFrame->media->Lock(1);
    if (ioFrame->beginEditSeq) {
      ioFrame->media->EndEditSequence();
      ioFrame->beginEditSeq = 0;
    }
  }
#endif
#if WCONSOLE_STDIO
  if (!msg) {
    char *s;
    long l;
	DWORD wrote;
    
    s = va_arg(args, char*);
    l = va_arg(args, long);

	WriteConsole(console_out, s, l, &wrote, NULL);
  } else {
	char buffer[2048];
	DWORD wrote;
    vsprintf(buffer, msg, args);
	WriteConsole(console_out, buffer, strlen(buffer), &wrote, NULL);
  }
#endif
#if !WINDOW_STDIO && !WCONSOLE_STDIO
  vfprintf(mrerr, msg, args);
#endif
  va_end(args);
}
#endif

#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO

static int stdin_getc(Scheme_Input_Port*)
{
#ifdef WINDOW_STDIO
  static int printed_input_warning = 0;
  if (!printed_input_warning) {
    printed_input_warning = 1;
    MrEdSchemeMessages("WARNING: no standard input on this platform\n");
  }
#endif
  return EOF;
}

static int stdin_char_ready(Scheme_Input_Port*)
{
  return TRUE;
}

static Scheme_Object *MrEdMakeStdIn(void)
{
  Scheme_Object *intype = scheme_make_port_type("stdin");

  return (Scheme_Object *)scheme_make_input_port(intype, NULL,
						 stdin_getc,
						 stdin_char_ready,
						 NULL, NULL, 0);
}

static void stdout_write(char *s, long l, Scheme_Output_Port*)
{
#if WINDOW_STDIO || WCONSOLE_STDIO
  MrEdSchemeMessages(NULL, s, l);
#else
  static FILE *out = NULL;

  if (!out)
    out = fopen("mrstdout.txt", "w");
  
  if (out)
    fwrite(s, l, 1, out);
#endif
}

static Scheme_Object *MrEdMakeStdOut(void)
{
  Scheme_Object *outtype = scheme_make_port_type("stdout");

  return (Scheme_Object *)scheme_make_output_port(outtype, NULL,
						  stdout_write,
						  NULL, 0);
}

static void stderr_write(char *s, long l, Scheme_Output_Port*)
{
#if WINDOW_STDIO || WCONSOLE_STDIO
  MrEdSchemeMessages(NULL, s, l);
#else
  if (!mrerr)
    mrerr = fopen("mrstderr.txt", "w");
  
  if (mrerr)
    fwrite(s, l, 1, mrerr);
#endif
}

static Scheme_Object *MrEdMakeStdErr(void)
{
  Scheme_Object *errtype = scheme_make_port_type("stderr");

  return (Scheme_Object *)scheme_make_output_port(errtype, NULL,
						  stderr_write,
						  NULL, 0);
}
#endif

/****************************************************************************/
/*                               Debugging                                  */
/****************************************************************************/

#if ADD_OBJ_DUMP
extern int wx_object_count;

# ifndef USE_SENORA_GC
extern "C" GC_PTR GC_changing_list_start, GC_changing_list_current;
# else
# define GC_word int
# endif
extern "C" GC_word GC_dl_entries;
extern "C" GC_word GC_fo_entries;

Scheme_Object *OBJDump(int, Scheme_Object *[])
{
# if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE
# define PRINT_IT MrEdSchemeMessages
# else
# define PRINT_IT scheme_console_printf
# endif
  int c;

  PRINT_IT("Objects: %d\n", wx_object_count);
# ifndef USE_SENORA_GC
  PRINT_IT("Memory: %d\n", GC_get_heap_size());
# endif
  PRINT_IT("FO: %d\n", GC_fo_entries);
  PRINT_IT("DL: %d\n", GC_dl_entries);
# ifndef USE_SENORA_GC
  PRINT_IT("Changing: %d\n",
	 (long)GC_changing_list_current - (long)GC_changing_list_start);
# endif

  wxTimer *timer;
  for (c = 0, timer = mred_timers; timer; timer = timer->next)
    c++;
  PRINT_IT("Timers: %d\n", c);

  Scheme_Process *p;
  for (c = 0, p = scheme_first_process; p; p = p->next)
    c++;

  PRINT_IT("Threads: %d\n", c);

  return scheme_make_integer(wx_object_count);
}
#endif

#ifdef SGC_STD_DEBUGGING
extern "C" {
  void (*scheme_external_dump_info)(void);
  void (*scheme_external_dump_arg)(Scheme_Object *);
  char *(*scheme_external_dump_type)(void *);
};
extern void GC_cpp_for_each(void (*f)(void *, int, void *), void *data);
extern int GC_is_wx_object(void *v);

#define NUM_OBJ_KIND (wxTYPE_SNIP_CLASS_LIST + 1)
static int cpp_count[NUM_OBJ_KIND], cpp_sch_count[NUM_OBJ_KIND], cpp_size[NUM_OBJ_KIND];
static int cpp_actual_count[NUM_OBJ_KIND], cpp_actual_size[NUM_OBJ_KIND];
static unsigned long cpp_lo[NUM_OBJ_KIND], cpp_hi[NUM_OBJ_KIND];

static int trace_path_type;

#ifdef USE_WXOBJECT_TRACE_COUNTER

void wxTraceCount(void *o, int size)
{
  wxObject *obj = (wxObject *)o;
  int type = obj->__type;

  if ((type >= 0) && (type < NUM_OBJ_KIND)) {
    cpp_actual_count[type]++;
    cpp_actual_size[type] += size;

    unsigned long s = (unsigned long)o;
    if (!cpp_lo[type] || (s < cpp_lo[type]))
      cpp_lo[type] = s;
    if (!cpp_hi[type] || (s > cpp_hi[type]))
      cpp_hi[type] = s;
  }
}

void wxTracePath(void *o, unsigned long src, void *pd)
{
  if (trace_path_type > 0) {
    wxObject *obj = (wxObject *)o;
    int type = obj->__type;
    
    if (type == trace_path_type)
      GC_store_path(o, src, pd);
  }
}

void wxTraceInit(void)
{
  int i;

  for (i = 0; i < NUM_OBJ_KIND; i++) {
    cpp_actual_count[i] = cpp_actual_size[i] = 0;
    cpp_lo[i] = cpp_hi[i] = 0;
  }
}

void wxTraceDone(void)
{
  /* nothing */
}

void wxObjectFinalize(void *o)
{
  if (((wxObject *)o)->__type != -1) {
#if 0
    /* New non-cleanup flag makes this incorrect: */
    fprintf(stderr, "ERROR: free wxObject had non-deleted type value!");
#else
    ((wxObject *)o)->__type = -1;
#endif
  }
}

static void set_trace_arg(Scheme_Object *a)
{
  trace_path_type = -1;
  if (a && SCHEME_SYMBOLP(a)) {
    char *s = SCHEME_SYM_VAL(a);
    int i;

    for (i = 0; i < NUM_OBJ_KIND; i++) {
      char *tn = wxGetTypeName(i);
      if (tn && !strcmp(tn, s)) {
	trace_path_type = i;
	return;
      }
    }
  }
}

static char *object_type_name(void *v)
{
  if (GC_is_wx_object(v)) {
    int t = ((wxObject *)v)->__type;
    if ((t >= 0) && (t < NUM_OBJ_KIND)) {
      char *c = wxGetTypeName(t);
      if (c)
	return c;
      else
	return "wxUNKNOWN";
    } else
      return "wxBAD";
  } else
    return "";
}

#endif

static void count_obj(void *o, int s, void *)
{
  wxObject *obj = (wxObject *)o;
  int type = obj->__type;

  if ((type >= 0) && (type < NUM_OBJ_KIND)) {
    cpp_count[type]++;
    if (obj->__gc_external)
      cpp_sch_count[type]++;
#ifdef MEMORY_USE_METHOD
    cpp_size[type] += s + (obj->MemoryUse());
#endif
  }
}

static void dump_cpp_info()
{
  int i, total_count = 0, total_size = 0, total_actual_size = 0;
  
  for (i = 0; i < NUM_OBJ_KIND; i++)
    cpp_count[i] = cpp_sch_count[i] = cpp_size[i] = 0;

  GC_cpp_for_each(count_obj, NULL);

  scheme_console_printf("\nBegin wxWindows\n");

  for (i = 0; i < NUM_OBJ_KIND; i++) {
    if (cpp_count[i] || cpp_actual_count[i]) {
      char buffer[50];
      char *name = wxGetTypeName(i);

      if (!name) {
	sprintf(buffer, "#%d", i);
	name = buffer;
      }

      scheme_console_printf("%30.30s %4ld %5ld %10ld %10ld %8lx - %8lx\n",
			    name,
			    cpp_sch_count[i],
			    cpp_count[i],
			    cpp_size[i],
			    cpp_actual_size[i],
			    cpp_lo[i],
			    cpp_hi[i]);
#ifdef USE_WXOBJECT_TRACE_COUNTER
      if (cpp_count[i] != cpp_actual_count[i])
	scheme_console_printf("%30.30s actual count: %10ld\n",
			      "", cpp_actual_count[i]);
#endif
      total_count += cpp_count[i];
      total_size += cpp_size[i];
      total_actual_size += cpp_actual_size[i];
    }
  }
    
  scheme_console_printf("%30.30s %10ld %10ld %10ld\n",
			"total", total_count, total_size, total_actual_size);
  
  scheme_console_printf("End wxWindows\n");

#if ADD_OBJ_DUMP
  scheme_console_printf("\n");
  OBJDump(0, NULL);
#endif
}

#endif

/****************************************************************************/
/*                           AIX DANGER signal                              */
/****************************************************************************/

#if defined(_IBMR2)
#define DANGER_ALARM
#endif

#ifdef DANGER_ALARM

static int danger_signal_received = 0;
static wxDialogBox *dangerFrame = NULL;

class DangerThreadTimer : public wxTimer
{
 public:
  void Notify(void);
};

void DismissDanger(wxObject &o, wxEvent &e)
{
  dangerFrame->Show(FALSE);
  dangerFrame = NULL;
  danger_signal_received = 0;
}

void DangerThreadTimer::Notify(void)
{
  if (danger_signal_received) {
    if (!dangerFrame) {
      dangerFrame = new wxDialogBox((wxWindow *)NULL, "Danger", FALSE, 0, 0, 300, 200);

      (void) new wxMessage(dangerFrame, "Warning: Paging space is low.");

      dangerFrame->NewLine();

      wxButton *b = new wxButton(dangerFrame, DismissDanger, "Ok");

      dangerFrame->Fit();
      b->Centre(wxHORIZONTAL);

      dangerFrame->Centre(wxBOTH);
      dangerFrame->Show(TRUE);
    }
  }
}

#endif

/****************************************************************************/
/*                             Application                                  */
/****************************************************************************/

MrEdApp::MrEdApp()
{
#ifndef wx_xt
  if (!wx_class)
    wx_class = "mred";
#endif
}

extern "C" void (*GC_out_of_memory)(void);

static void MrEdOutOfMemory(void)
{
#ifdef wx_mac
  Alert(101, NULL);
  ExitToShell();
#else
  _exit(-1);
#endif
}

void *wxOutOfMemory()
{
  MrEdOutOfMemory();
  return NULL;
}

static const char *CallSchemeExpand(const char *filename)
{
  char *s;

  s = scheme_expand_filename((char *)filename, strlen(filename), NULL, 0);
  
  return s ? s : filename;
}

#ifndef USE_SENORA_GC
static void MrEdIgnoreWarnings(char *, GC_word)
{
}
#endif

#include "../mzscheme/src/schvers.h"

#ifdef wx_x
# define INIT_FILENAME "~/.mredrc"
#else
# define INIT_FILENAME "mred.rc"
#endif
#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
# define PRINTF scheme_console_printf
#else
# define PRINTF printf
#endif
#define PROGRAM "MrEd"
#define PROGRAM_LC "mred"
#define BANNER "MrEd version " VERSION ", Copyright (c) 1995-98 PLT (Matthew Flatt and Robby Findler)\n"

#include "../mzscheme/cmdline.inc"

static FinishArgs *xfa;

static int do_main_loop(FinishArgs *fa)
{
  xfa = fa;

  TheMrEdApp->MainLoop();

  return 0;
}

static Scheme_Env *setup_basic_env()
{
  global_env = scheme_basic_env();

  scheme_no_dumps("the graphics library is running");

  wxmeExpandFilename = CallSchemeExpand;

#ifdef DANGER_ALARM
  {
    DangerThreadTimer *t = new DangerThreadTimer();
    t->Start(10000);
  }
#endif

  wxsScheme_setup(global_env);

  mred_eventspace_type = scheme_make_type("<eventspace>");

  scheme_set_param(scheme_config, mred_eventspace_param, (Scheme_Object *)mred_main_context);

  def_dispatch = scheme_make_prim_w_arity(def_event_dispatch_handler,
					  "default-event-dispatch-handler",
					  1, 1);
  scheme_set_param(scheme_config, mred_event_dispatch_param, def_dispatch);

  /* Make sure ps-setup is installed in the parameterization */
  ps_ready = 1;
  wxSetThePrintSetupData(wxGetThePrintSetupData());

  MakeContext(mred_main_context, NULL);

  mred_only_context = NULL;

  mred_main_context->handler_running = scheme_current_process;

  mzsleep = scheme_sleep;
  scheme_sleep = MrEdSleep;

#if ADD_OBJ_DUMP
  scheme_add_global("dump-object-stats", 
		    scheme_make_prim(OBJDump), global_env);
#endif

  return global_env;
}

wxFrame *MrEdApp::OnInit(void)
{
  initialized = 0;

#ifdef LIBGPP_REGEX_HACK
  new Regex("a", 0);
#endif

#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
  scheme_make_stdin = MrEdMakeStdIn;
  scheme_make_stdout = MrEdMakeStdOut;
  scheme_make_stderr = MrEdMakeStdErr;
#endif

#ifndef USE_SENORA_GC
  GC_set_warn_proc(MrEdIgnoreWarnings);
#endif
  GC_out_of_memory = MrEdOutOfMemory;

#ifdef SGC_STD_DEBUGGING
  scheme_external_dump_info = dump_cpp_info;
# ifdef USE_WXOBJECT_TRACE_COUNTER
  scheme_external_dump_type = object_type_name;
  scheme_external_dump_arg = set_trace_arg;
# endif
#endif

#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
  scheme_console_printf = MrEdSchemeMessages;
#endif
  scheme_get_sema_callback_context = wxGetContextForFrame;

  mred_eventspace_param = scheme_new_param();
  mred_event_dispatch_param = scheme_new_param();
  mred_ps_setup_param = scheme_new_param();

  mred_main_context = new MrEdContext;
  mred_main_context->topLevelWindowList = new wxChildList();
  mred_main_context->snipClassList = wxMakeTheSnipClassList();
  mred_main_context->bufferDataClassList = wxMakeTheBufferDataClassList();

  mred_main_context->finalized = new MrEdFinalizedContext;

  mred_only_context = mred_main_context;

  MrEdInitFirstContext(mred_main_context);

  mred_real_main_frame = new wxFrame(NULL, "MrEd"); /* Just in case wxWindows needs an initial frame */

  wxInitMedia();

  run_from_cmd_line(argc, argv, setup_basic_env, do_main_loop);

  return NULL;
}

static void do_graph_repl(void)
{
  scheme_eval_string("(graphical-read-eval-print-loop)", global_env);
}

static void on_main_killed(Scheme_Process *p)
{
  on_handler_killed(p);
  
#ifdef WINDOW_STDIO
  if (have_stdio) {
    
    stdio_kills_prog = 1;
    return;
  }
#endif

  exit(0);
}

void MrEdApp::RealInit(void)
{
  initialized = 1;

  wxMediaIOCheckLSB(/* scheme_console_printf */);

  scheme_current_process->on_kill = on_main_killed;
  
  finish_cmd_line_run(xfa, do_graph_repl);

  scheme_kill_thread(scheme_current_process);
}

#ifdef wx_mac
char *MrEdApp::GetDefaultAboutItemName()
{
  return "About MrEd...";
}

void MrEdApp::DoDefaultAboutItem()
{
  DialogPtr dial;
  short hit;
  GrafPtr port;
 
  dial = GetNewDialog(129, NULL, (WindowRef)-1);
  GetPort(&port);
  SetPort(dial);
  TextFont(kFontIDGeneva);
  TextSize(10);
  SetPort(port);

  ModalDialog(NULL, &hit);
  
  DisposeDialog(dial);
}

#endif

int MrEdApp::OnExit(void)
{
  return 0;
}

#ifdef wx_mac
extern Scheme_Object *wxs_app_file_proc;

void Drop_Runtime(char **argv, int argc)
{
  int i;
  mz_jmp_buf savebuf;
  
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

  if (scheme_setjmp(scheme_error_buf)) {
    /* give up on rest */
  } else {
    for (i = 0; i < argc; i++) {
      Scheme_Object *p[1];
      p[0] = scheme_make_string(argv[0]);
      scheme_apply(wxs_app_file_proc, 1, p);
    }
  }

  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
}

void Drop_Quit()
{
  Scheme_Object *sym, *quit;

  sym = scheme_intern_symbol("mred:exit");
  quit = scheme_lookup_global(sym, scheme_get_env(scheme_config));

  scheme_apply(quit, 0, NULL);
}
#endif

#ifdef wx_x
#if INTERRUPT_CHECK_ON
static int interrupt_signal_received;

static void interrupt(int)
{
  interrupt_signal_received = 1;

  signal(SIGINT, interrupt);
}
#endif
#endif

#if defined(_IBMR2)
static void dangerdanger(int)
{
  if (danger_signal_received) {
    fprintf(stderr, "mred: Danger - paging space STILL low - exiting\n");
    exit(-1);
  } else {
    fprintf(stderr, "mred: Danger - paging space low\n");
    scheme_collect_garbage();
    danger_signal_received = 1;
  }
  
  signal(SIGDANGER, dangerdanger);
}
#endif

#ifdef wx_mac
extern short wxMacDisableMods;
extern long wxMediaCreatorId;
#endif

extern int wxEntry(int, char **);

extern "C" {
int actual_main(int argc, char **argv)
{
#ifndef wx_msw
  TheMrEdApp = new MrEdApp;
#endif

  int r = wxEntry(argc, argv);

  return r;
}
};

int main(int argc, char *argv[])
{
#if defined(_IBMR2)
  signal(SIGDANGER, dangerdanger);
#endif
#ifdef wx_x
#if INTERRUPT_CHECK_ON
  signal(SIGINT, interrupt);
#endif
#endif

#ifdef USE_SENORA_GC
  {
    int dummy;
    GC_set_stack_base(&dummy);
  }

  fprintf(stderr, "Starting MrEd sgc\n");
#endif

#ifdef wx_mac
  wxMacDisableMods = 4096;

  scheme_creator_id = 'MrEd';
  wxMediaCreatorId = 'MrEd';

#if !defined(__powerc)
  long calcLimit;
  THz zone;
	
  zone = GetZone();
  calcLimit = ((long)LMGetCurStackBase()-(*(long *)zone)-sizeof(Zone))*3/4;
  if (calcLimit % 2)
    calcLimit++;
  SetApplLimit((Ptr)((*(long *)zone)+sizeof(Zone)+calcLimit));
#endif
#endif

#ifdef wx_mac
  /* initialize Mac stuff */
  MaxApplZone();
  InitGraf(&qd.thePort);		
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  MoreMasters();
  MoreMasters();
  
  Drop_GetArgs(&argc, &argv);
#endif
  
  scheme_actual_main = actual_main;

  return scheme_image_main(argc, argv);
}

/****************************************************************************/
/*                              wxFlushDisplay                              */
/****************************************************************************/

void wxFlushDisplay(void)
{
#ifdef wx_x
  Display *d;

#ifdef wx_motif
    d = XtDisplay(wxTheApp->topLevel);
#endif
#ifdef wx_xview
    d = (Display*)xv_get((Frame)(wxTheApp->wx_frame->GetHandle()), XV_DISPLAY);
#endif
#ifdef wx_xt
    d = XtDisplay(wxAPP_TOPLEVEL);
#endif

  XFlush(d);
  XSync(d, FALSE);
  XFlush(d);
  XSync(d, FALSE);
#endif
}

#ifdef DEFINE_DUMMY_PURE_VIRTUAL
/* Weird hack to avoid linking to libg++ */
extern "C" {
 void __pure_virtual(void) {  }
}
#endif
