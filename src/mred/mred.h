
#define BREAKING_REQUIRES_SHIFT 1

#ifndef MRED_EXTERN
# define MRED_EXTERN extern
#endif

#ifdef wx_x
typedef XEvent MrEdEvent;
#else
#ifdef wx_msw
typedef MSG MrEdEvent;
#else
typedef EventRecord MrEdEvent;
#endif
#endif

class wxTimer;

class MrEdContextFrames {
 public:
  wxChildList *list;
  MrEdContextFrames *next, *prev;
};

class MrEdFinalizedContext;
class MrEd_Saved_Modal;

typedef struct MrEdContext {
  Scheme_Type type;
  MZ_HASH_KEY_EX

  Scheme_Thread *handler_running;
  int suspended;

  MrEdFinalizedContext *finalized;

  wxChildList *topLevelWindowList;
  wxStandardSnipClassList *snipClassList;
  wxBufferDataClassList *bufferDataClassList;
  wxWindow *modal_window;
  MrEd_Saved_Modal *modal_stack;

  Scheme_Config *main_config;

  short ready_to_go;

  short ready, waiting_for_nested;
  short q_callback;
  wxTimer *timer;
  MrEdEvent event;

  /* Alternate condition for nested event loop pending some condition */
  int (*alternate)(void *);
  void *alt_data;

  /* Used to chain active contexts while reading events: */
  struct MrEdContext *next;

  int busyState;
  int killed;

#ifdef wx_msw
  struct LeaveEvent *queued_leaves;
#endif

  struct Context_Custodian_Hop *mr_hop;
  Scheme_Custodian_Reference *mref;
} MrEdContext;

class MrEdFinalizedContext {
 public:
#ifdef wx_xt
  Widget toplevel;
#endif
  MrEdContextFrames *frames;
  MrEdContext **real_context; /* atomic ptr to actual context */
};

extern MrEdContext *mred_contexts;

MrEdContext *MrEdGetContext(wxObject *w = NULL);

extern int MrEdGetNextEvent(int peek, int current_only, MrEdEvent *, MrEdContext **);
void MrEdDispatchEvent(MrEdEvent *);

void MrEdInitFirstContext(MrEdContext *c);
void MrEdInitNewContext(MrEdContext *c);
void MrEdDestroyContext(MrEdFinalizedContext *c);

#ifdef wx_msw
void MrEdMSWSleep(float secs, void *fds);
MRED_EXTERN void mred_clean_up_gdi_objects(void);
#endif

#ifdef wx_mac
void MrEdMacSleep(float secs);
#endif

typedef void *(*ForEachFrameProc)(wxObject *, void *);
void *MrEdForEachFrame(ForEachFrameProc fp, void *data);

/* Startup: */
MRED_EXTERN void wxCreateApp(void);
MRED_EXTERN void wxDoMainLoop();

typedef int (*MrEd_Finish_Cmd_Line_Run_Proc)(void);
typedef void (*MrEd_Run_From_Cmd_Line_Proc)(int argc, char **argv, Scheme_Env *(*mk_basic_env)(void));

MRED_EXTERN MrEd_Finish_Cmd_Line_Run_Proc mred_finish_cmd_line_run;
MRED_EXTERN MrEd_Run_From_Cmd_Line_Proc mred_run_from_cmd_line;

#ifdef INCLUDE_WITHOUT_PATHS
# include "schvers.h"
#else
# include "../mzscheme/src/schvers.h"
#endif

#ifdef MZ_PRECISE_GC
# define MRED3M "3m"
#else
# define MRED3M ""
#endif
#define BANNER "MrEd" MRED3M " version " MZSCHEME_VERSION ", Copyright (c) 1995-2001 PLT\n"
