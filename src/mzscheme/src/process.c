/*
  MzScheme
  Copyright (c) 1995-98 Matthew Flatt
 
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#define SCHEME_NO_GC_PROTO

/* Irix SPROCS needs to load some files first, so find out if we're SPROCS. */
#ifdef INCLUDE_WITHOUT_PATHS
# include "sconfig.h"
#else
# include "../sconfig.h"
#endif

#ifdef MZ_USE_IRIX_SPROCS
/* Don't include anything else before this */
# include "../gc/gc.h"
# include "../gc/semaphores.h"
# include "../gc/sproc.h"
#endif

#include "schpriv.h"
#include "schmach.h"
#include "schgc.h"
#include <time.h>
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
# ifdef USE_BEOS_SOCKET_INCLUDE
#  include <be/net/socket.h>
# endif
#endif
#ifdef USE_WINSOCK_TCP
# ifdef USE_TCP
#  include <winsock.h>
# endif
#endif
#ifdef USE_BEOS_PORT_THREADS
# include <be/net/socket.h>
#endif
#ifdef USE_STACKAVAIL
# include <malloc.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#if defined(MZ_USE_WIN32_THREADS)
# include <windows.h>
# include <process.h>
#endif
#ifdef MZ_USE_SOLARIS_THREADS
# include <thread.h>
# include <synch.h>
#endif
#ifdef MZ_USE_PTHREADS
# include <semaphore.h>
#endif
#ifdef WIN32_THREADS
# include <process.h>
#endif

#if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)
# ifndef NO_STDIO_THREADS
#  include <windows.h>
extern HANDLE scheme_break_semaphore;
# endif
#endif

#if defined(FILES_HAVE_FDS) \
     || defined(USE_BEOS_PORT_THREADS) \
     || (defined(USE_WINSOCK_TCP) && defined(USE_TCP)) \
     || ((defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)) \
	     && !defined(NO_STDIO_THREADS))
# define USING_FDS
# if (!defined(USE_WINSOCK_TCP) || !defined(USE_TCP)) && !defined(FILES_HAVE_FDS)
#  include <sys/types.h>
# endif
#endif

#include "schfd.h"

#define INIT_SCHEME_STACK_SIZE SCHEME_STACK_SIZE

#ifdef SGC_STD_DEBUGGING
# define SENORA_GC_NO_FREE
#endif

typedef struct ActiveWill {
  Scheme_Object *o;
  Scheme_Object *proc;
  struct WillExecutor *w;  /* Set to will executor when executed */
  struct ActiveWill *next;
} ActiveWill;

typedef struct WillExecutor {
  Scheme_Type type;
  short running;
  ActiveWill *first, *last;
} WillExecutor;

typedef struct WillRegistration {
  Scheme_Object *proc;
  WillExecutor *w;
} WillRegistration;

#define INIT_TB_SIZE  20

static int buffer_init_size = INIT_TB_SIZE;

#ifndef MZ_REAL_THREADS
Scheme_Process *scheme_current_process = NULL;
#endif
Scheme_Process *scheme_main_process = NULL;
Scheme_Process *scheme_first_process = NULL;
#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Process **scheme_current_process_ptr;
int *scheme_fuel_counter_ptr;
#endif
#ifndef MZ_REAL_THREADS
static int swap_no_setjmp = 0;
#endif

#ifdef RUNSTACK_IS_GLOBAL
Scheme_Object **scheme_current_runstack_start;
Scheme_Object **scheme_current_runstack;
MZ_MARK_STACK_TYPE scheme_current_cont_mark_stack;
MZ_MARK_POS_TYPE scheme_current_cont_mark_pos;
#endif

static Scheme_Manager *main_manager;

long scheme_total_gc_time;
static long start_this_gc_time;

void (*scheme_sleep)(float seconds, void *fds);
void (*scheme_notify_multithread)(int on);
void (*scheme_wakeup_on_input)(void *fds);
int (*scheme_check_for_break)(void);

#ifndef MZ_REAL_THREADS
static int do_atomic = 0;
static int missed_context_switch = 0;
static int have_activity = 0;
int scheme_active_but_sleeping = 0;
static int process_ended_with_activity;
#endif

static int tls_pos = 0;

extern void (*GC_collect_start_callback)(void);
extern void (*GC_collect_end_callback)(void);
static void get_ready_for_GC(void);
static void done_with_GC(void);

extern long GC_get_memory_use();

static Scheme_Object *keywords_symbol, *no_keywords_symbol;
static Scheme_Object *callcc_is_callec_symbol, *callcc_is_not_callec_symbol;
static Scheme_Object *hash_percent_syntax_symbol, *all_syntax_symbol, *empty_symbol;

static Scheme_Object *collect_garbage(int argc, Scheme_Object *args[]);
static Scheme_Object *current_memory_use(int argc, Scheme_Object *args[]);

#ifndef NO_SCHEME_THREADS
static Scheme_Object *sch_thread(int argc, Scheme_Object *args[]);
#endif
static Scheme_Object *sch_sleep(int argc, Scheme_Object *args[]);
#ifndef NO_SCHEME_THREADS
static Scheme_Object *process_weight(int argc, Scheme_Object *args[]);
static Scheme_Object *processp(int argc, Scheme_Object *args[]);
static Scheme_Object *process_running_p(int argc, Scheme_Object *args[]);
static Scheme_Object *process_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_current(int argc, Scheme_Object *args[]);
static Scheme_Object *kill_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *break_thread(int argc, Scheme_Object *args[]);
#endif

static Scheme_Object *make_manager(int argc, Scheme_Object *argv[]);
static Scheme_Object *manager_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *manager_close_all(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_manager(int argc, Scheme_Object *argv[]);

static Scheme_Object *current_namespace(int argc, Scheme_Object *args[]);
static Scheme_Object *namespace_p(int argc, Scheme_Object *args[]);

static Scheme_Object *parameter_p(int argc, Scheme_Object *args[]);
static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object *args[]);
static Scheme_Object *make_parameter(int argc, Scheme_Object *args[]);

static Scheme_Object *make_new_config(Scheme_Config *base);

static void adjust_manager_family(void *pr, void *ignored);

static Scheme_Object *current_will_executor(int argc, Scheme_Object *args[]);
static Scheme_Object *make_will_executor(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_p(int argc, Scheme_Object *args[]);
static Scheme_Object *register_will(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_try(int argc, Scheme_Object *args[]);

static Scheme_Object *do_next_will(WillExecutor *w, int tail);
static Scheme_Config *make_initial_config(void);
static int do_kill_thread(Scheme_Process *p);

static void remove_process(Scheme_Process *r);

static Scheme_Config *initial_config;

static Scheme_Object **config_map;

typedef struct {
  unsigned long key;
  Scheme_Object *guard;
  Scheme_Object *defval;
} ParamData;

typedef struct ParamExtensionRecData {
  Scheme_Object *p;
  unsigned long key;
} ParamExtensionRecData;

typedef struct ParamExtensionRec {
  ParamExtensionRecData *data; /* atomic */
  struct ParamExtensionRec *next;
} ParamExtensionRec;

static ParamExtensionRec *param_ext_recs;

enum {
  CONFIG_DIRECT,
  CONFIG_INDIRECT
};

typedef struct Scheme_Process_Manager_Hop {
  Scheme_Type type;
  Scheme_Process *p;
} Scheme_Process_Manager_Hop;

typedef struct {
  Scheme_Object *key;
  void (*f)(Scheme_Env *);
} Scheme_NSO;
static int num_nsos = 0;
static Scheme_NSO *namespace_options = NULL;

#if defined(MZ_REAL_THREADS)
# define SETJMP(p) 1
# define LONGJMP(p) 0
# define RESETJMP(p)
#else /* not USE_REAL_THREADS */
# define SETJMP(p) scheme_setjmpup(&p->jmpup_buf, p->stack_start)
# define LONGJMP(p) scheme_longjmpup(&p->jmpup_buf)
# define RESETJMP(p) scheme_reset_jmpup_buf(&p->jmpup_buf)
#endif

#ifdef MZ_REAL_THREADS
void *make_namespace_mutex;
#endif

#ifndef MZ_REAL_THREADS
#define GET_WILL_LOCK() /* empty */
#define RELEASE_WILL_LOCK() /* empty */

#define GET_CUST_LOCK() /* empty */
#define RELEASE_CUST_LOCK() /* empty */
#else
static void *will_mutex;
# ifdef MZ_KEEP_LOCK_INFO
static int will_lock_c;
# endif
# define GET_WILL_LOCK() (SCHEME_LOCK_MUTEX(will_mutex) _MZ_LOCK_INFO(will_lock_c++))
# define RELEASE_WILL_LOCK()  (MZ_LOCK_INFO_(--will_lock_c) SCHEME_UNLOCK_MUTEX(will_mutex))

static void *cust_mutex;
# ifdef MZ_KEEP_LOCK_INFO
static int cust_lock_c;
# endif
# define GET_CUST_LOCK() (SCHEME_LOCK_MUTEX(cust_mutex) _MZ_LOCK_INFO(cust_lock_c++))
# define RELEASE_CUST_LOCK()  (MZ_LOCK_INFO_(--cust_lock_c) SCHEME_UNLOCK_MUTEX(cust_mutex))
#endif

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
# define GC_THINKS_ITS_A_DLL_BUT_ISNT

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
# endif
#endif

void scheme_init_process(Scheme_Env *env)
{
  scheme_add_global_constant("dump-memory-stats",
			     scheme_make_prim_w_arity(scheme_dump_gc_stats,
						      "dump-memory-stats",
						      0, 1), 
			     env);

#if 0
#ifdef MEMORY_COUNTING_ON
  scheme_add_global_constant("dump-memory-count",
			     scheme_make_prim_w_arity(scheme_dump_memory_count,
						      "dump-memory-count",
						      0, 1),
			     env);
#endif
#endif

  scheme_add_global_constant("make-namespace",
			     scheme_make_prim_w_arity(scheme_make_namespace,
						      "make-namespace",
						      0, -1),
			     env);
#ifndef NO_SCHEME_THREADS
  scheme_add_global_constant("thread",
			     scheme_make_prim_w_arity(sch_thread,
						      "thread",
						      1, 1),
			     env);
#endif
  
  scheme_add_global_constant("sleep",
			     scheme_make_prim_w_arity(sch_sleep,
						      "sleep",
						      0, 1),
			     env);

#ifndef NO_SCHEME_THREADS
  scheme_add_global_constant("thread-weight",
			     scheme_make_prim_w_arity(process_weight,
						      "thread-weight",
						      1, 2),
			     env);
  scheme_add_global_constant("thread?",
			     scheme_make_folding_prim(processp,
						      "thread?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("thread-running?",
			     scheme_make_prim_w_arity(process_running_p,
						      "thread-running?",
						      1, 1),
			     env);
  scheme_add_global_constant("thread-wait",
			     scheme_make_prim_w_arity(process_wait,
						      "thread-wait",
						      1, 1),
			     env);
  
  scheme_add_global_constant("current-thread", 
			     scheme_make_prim_w_arity(sch_current,
						      "current-thread", 
						      0, 0), 
			     env);

  scheme_add_global_constant("kill-thread", 
			     scheme_make_prim_w_arity(kill_thread,
						      "kill-thread", 
						      1, 1), 
			     env);
  scheme_add_global_constant("break-thread", 
			     scheme_make_prim_w_arity(break_thread,
						      "break-thread", 
						      1, 1), 
			     env);
#endif

  scheme_add_global_constant("make-custodian",
			     scheme_make_prim_w_arity(make_manager,
						      "make-custodian",
						      0, 1),
			     env);
  scheme_add_global_constant("custodian?",
			     scheme_make_folding_prim(manager_p,
						      "custodian?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("custodian-shutdown-all",
			     scheme_make_prim_w_arity(manager_close_all,
						      "custodian-shutdown-all",
						      0, 1),
			     env);
  scheme_add_global_constant("current-custodian", 
			     scheme_register_parameter(current_manager,
						       "current-custodian",
						       MZCONFIG_MANAGER),
			     env);

  scheme_add_global_constant("current-namespace", 
			     scheme_register_parameter(current_namespace,
						       "current-namespace",
						       MZCONFIG_ENV),
			     env);

  scheme_add_global_constant("namespace?", 
			     scheme_make_prim_w_arity(namespace_p,
						      "namespace?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("parameter?", 
			     scheme_make_prim_w_arity(parameter_p,
						      "parameter?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("make-parameter", 
			     scheme_make_prim_w_arity(make_parameter,
						      "make-parameter", 
						      1, 2), 
			     env);
  scheme_add_global_constant("parameter-procedure=?", 
			     scheme_make_prim_w_arity(parameter_procedure_eq,
						      "parameter-procedure=?", 
						      2, 2), 
			     env);

  scheme_add_global_constant("make-will-executor", 
			     scheme_make_prim_w_arity(make_will_executor,
						      "make-will-executor", 
						      0, 0), 
			     env);
  scheme_add_global_constant("will-executor?", 
			     scheme_make_prim_w_arity(will_executor_p,
						      "will-executor?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("register-will", 
			     scheme_make_prim_w_arity(register_will,
						      "register-will", 
						      2, 3), 
			     env);
  scheme_add_global_constant("will-executor-try", 
			     scheme_make_prim_w_arity(will_executor_try,
						      "will-executor-try", 
						      1, 1), 
			     env);
  scheme_add_global_constant("current-will-executor", 
			     scheme_register_parameter(current_will_executor,
						       "current-will-executor",
						       MZCONFIG_WILL_EXECUTOR),
			     env);

  scheme_add_global_constant("collect-garbage", 
			     scheme_make_prim_w_arity(collect_garbage, 
						      "collect-garbage",
						      0, 0), 
			     env);
  scheme_add_global_constant("current-memory-use", 
			     scheme_make_prim_w_arity(current_memory_use, 
						      "current-memory-use",
						      0, 0), 
			     env);

  if (scheme_starting_up) {
    REGISTER_SO(config_map);
    REGISTER_SO(namespace_options);
    REGISTER_SO(param_ext_recs);

#ifdef MZ_REAL_THREADS
    make_namespace_mutex = SCHEME_MAKE_MUTEX();
    will_mutex = SCHEME_MAKE_MUTEX();
#endif

    REGISTER_SO(keywords_symbol);
    REGISTER_SO(no_keywords_symbol);
    REGISTER_SO(callcc_is_callec_symbol);
    REGISTER_SO(callcc_is_not_callec_symbol);
    REGISTER_SO(hash_percent_syntax_symbol);
    REGISTER_SO(all_syntax_symbol);
    REGISTER_SO(empty_symbol);

    keywords_symbol = scheme_intern_symbol("keywords");
    no_keywords_symbol = scheme_intern_symbol("no-keywords");
    callcc_is_callec_symbol = scheme_intern_symbol("call/cc=call/ec");
    callcc_is_not_callec_symbol = scheme_intern_symbol("call/cc!=call/ec");
    hash_percent_syntax_symbol = scheme_intern_symbol("hash-percent-syntax");
    all_syntax_symbol = scheme_intern_symbol("all-syntax");
    empty_symbol = scheme_intern_symbol("empty");

    GC_collect_start_callback = get_ready_for_GC;
    GC_collect_end_callback = done_with_GC;
  }
}

static Scheme_Object *collect_garbage(int c, Scheme_Object *p[])
{
  scheme_collect_garbage();

  return scheme_void;
}

static Scheme_Object *current_memory_use(int argc, Scheme_Object *args[])
{
  return scheme_make_integer(GC_get_memory_use());
}

#define ATSTEP(x) /* printf(x "\n") */

static Scheme_Process *make_process(Scheme_Process *after, Scheme_Config *config, 
				    Scheme_Manager *mgr)
{
  Scheme_Process *process;
  int prefix = 0;

  ATSTEP("making process");

  process = MALLOC_ONE_TAGGED(Scheme_Process);

  process->type = scheme_process_type;

  process->stack_start = 0;

  if (!scheme_main_process) {
#ifndef MZ_REAL_THREADS
    REGISTER_SO(scheme_current_process);
#endif
    REGISTER_SO(scheme_main_process);
    REGISTER_SO(scheme_first_process);

#ifdef MZ_REAL_THREADS
    process->thread = SCHEME_INIT_THREADS();
    SCHEME_SET_CURRENT_PROCESS(process);
#else
    scheme_current_process = process;
#endif
    scheme_first_process = scheme_main_process = process;
    process->prev = NULL;
    process->next = NULL;

#ifndef MZ_REAL_THREADS
#ifdef LINK_EXTENSIONS_BY_TABLE
    scheme_current_process_ptr = &scheme_current_process;
    scheme_fuel_counter_ptr = &scheme_fuel_counter;
#endif
#endif

#ifdef MZ_REAL_THREADS
    scheme_init_stack_check();
#endif
  } else {
    prefix = 1;
  }

  process->engine_weight = 10000;

  ATSTEP("making config");

  if (!config) {
    config = process->config = make_initial_config();

    if (scheme_starting_up) {
      REGISTER_SO(initial_config);
      initial_config = process->config;
    }
  } else
    process->config = config;

  ATSTEP("initing jmpbuf");

  scheme_init_jmpup_buf(&process->jmpup_buf);

  process->running = MZTHREAD_RUNNING;

  process->dw = NULL;

  process->block_descriptor = NOT_BLOCKED;
  process->block_check = NULL;
  process->block_needs_wakeup = NULL;
  process->sleep_time = 0;

  process->current_local_env = NULL;

  process->suspend_break = 0;
  process->external_break = 0;

  process->print_buffer = NULL;
  process->print_allocated = 0;

#ifndef NO_SCHEME_EXN
  process->exn_raised = 0;
#endif
  process->error_invoked = 0;
  process->err_val_str_invoked = 0;

  process->list_stack = NULL;

  SCHEME_GET_LOCK();
  if (prefix) {
    if (after) {
      process->prev = after;
      process->next = after->next;
      process->next->prev = process;
      process->prev->next = process;
    } else {
      process->next = scheme_first_process;
      process->prev = NULL;
      process->next->prev = process;
      scheme_first_process = process;
    }
  }

  ATSTEP("initing tailbuf");

  process->tail_buffer = (Scheme_Object **)scheme_malloc(buffer_init_size 
							 * sizeof(Scheme_Object *));
  process->tail_buffer_size = buffer_init_size;
#ifdef AGRESSIVE_ZERO_TB
  process->tail_buffer_set = 0;
#endif
  SCHEME_RELEASE_LOCK();

  process->runstack_size = INIT_SCHEME_STACK_SIZE;
  process->runstack_start = MALLOC_N(Scheme_Object*, INIT_SCHEME_STACK_SIZE);
  process->runstack = process->runstack_start + INIT_SCHEME_STACK_SIZE;
  process->runstack_saved = NULL;

  process->cont_mark_pos = (MZ_MARK_POS_TYPE)1;
  process->cont_mark_stack = 0;
  process->cont_mark_stack_segments = NULL;
  process->cont_mark_seg_count = 0;

#ifdef RUNSTACK_IS_GLOBAL
  if (!prefix) {
    REGISTER_SO(MZ_RUNSTACK);
    REGISTER_SO(MZ_RUNSTACK_START);

    MZ_RUNSTACK = process->runstack;
    MZ_RUNSTACK_START = process->runstack_start;
    MZ_CONT_MARK_STACK = process->cont_mark_stack;
    MZ_CONT_MARK_POS = process->cont_mark_pos;
  }
#endif

#ifdef MZ_REAL_THREADS
  process->done_sema = scheme_make_sema(0);
#endif

  process->on_kill = NULL;

  process->user_tls = NULL;
  process->user_tls_size = 0;

  ATSTEP("initing manager");

  /* A thread points to a lot of stuff, so it's bad to put a finalization
     on it, which is what registering with a manager does. Instead, we
     register a weak indirection with the manager. That way, the thread
     (and anything it points to) can be collected one GC cycle earlier. */
  process->mr_hop = MALLOC_ONE_ATOMIC(Scheme_Process_Manager_Hop);
  process->mr_hop->type = scheme_process_type;
  process->mr_hop->p = process;

  process->mref = scheme_add_managed(mgr
				     ? mgr
				     : (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER),
				     (Scheme_Object *)process->mr_hop, NULL, NULL, 0);

  scheme_weak_reference((void **)&process->mr_hop->p);

  return process;
}

static void ensure_manage_space(Scheme_Manager *m, int k)
{
  int i;

  if (m->count + k >= m->alloc) {
    Scheme_Object ***naya_boxes;
    Scheme_Manager_Reference **naya_mrefs;
    Scheme_Close_Manager_Client **naya_closers;
    void **naya_data;

    m->alloc = (m->alloc ? (2 * m->alloc) : 4);
    if (m->alloc < k)
      m->alloc += k;
    
    naya_boxes = MALLOC_N(Scheme_Object**, m->alloc);
    naya_closers = MALLOC_N(Scheme_Close_Manager_Client*, m->alloc);
    naya_data = MALLOC_N(void*, m->alloc);
    naya_mrefs = MALLOC_N(Scheme_Manager_Reference*, m->alloc);

    for (i = m->count; i--; ) {
      naya_boxes[i] = m->boxes[i];
      m->boxes[i] = NULL;
      naya_closers[i] = m->closers[i];
      m->closers[i] = NULL;
      naya_data[i] = m->data[i];
      m->data[i] = NULL;
      naya_mrefs[i] = m->mrefs[i];
      m->mrefs[i] = NULL;
    }

    m->boxes = naya_boxes;
    m->closers = naya_closers;
    m->data = naya_data;
    m->mrefs = naya_mrefs;
  }
}

static void add_managed_box(Scheme_Manager *m, 
			    Scheme_Object **box, Scheme_Manager_Reference *mref,
			    Scheme_Close_Manager_Client *f, void *data)
{
  int i;

  for (i = m->count; i--; ) {
    if (!m->boxes[i]) {
      m->boxes[i] = box;
      m->closers[i] = f;
      m->data[i] = data;
      m->mrefs[i] = mref;

      return;
    }
  }

  ensure_manage_space(m, 1);

  m->boxes[m->count] = box;
  m->closers[m->count] = f;
  m->data[m->count] = data;
  m->mrefs[m->count] = mref;

  m->count++;
}

static void remove_managed(Scheme_Manager_Reference *mr, Scheme_Object *o,
			   Scheme_Close_Manager_Client **old_f, void **old_data)
{
  Scheme_Manager *m;
  int i;

  GET_CUST_LOCK();

  m = *mr;
  if (!m) {
    RELEASE_CUST_LOCK();
    return;
  }

  for (i = m->count; i--; )
    if (m->boxes[i] && SAME_OBJ((*(m->boxes[i])),  o)) {
      *(m->boxes[i]) = 0;
      m->boxes[i] = NULL;
      *(m->mrefs[i]) = 0;
      m->mrefs[i] = NULL;
      if (old_f)
	*old_f = m->closers[i];
      if (old_data)
	*old_data = m->data[i];
      m->data[i] = NULL;
      break;
    }

  while (m->count && !m->boxes[m->count - 1])
    --m->count;

  RELEASE_CUST_LOCK();
}

static void adjust_manager_family(void *mgr, void *ignored)
{
  /* Threads note: because this function is only called as a
     finalization callback, it is automatically syncronized by the GC
     locks. And it is synchronized against all finalizations, so a
     managee can't try to unregister while we're shuffling its
     manager. */
  Scheme_Manager *r = (Scheme_Manager *)mgr, *parent, *m;
  int i;

  parent = *r->parent;

  GET_CUST_LOCK();

  if (parent) {
    /* Remove from parent's list of children: */
    if (*parent->children == r) {
      *parent->children = *(r->sibling);
    } else {
      m = *parent->children;
      while (m && *m->sibling != r)
	m = *m->sibling;
      if (m)
	*m->sibling = *r->sibling;
    }

    /* Add children to parent's list: */
    for (m = *r->children; m; ) {
      Scheme_Manager *next = *m->sibling;
      
      *m->parent = parent;
      *m->sibling = *parent->children;
      *parent->children = m;

      m = next;
    }

    /* Add remaining managed items to parent: */
    for (i = 0; i < r->count; i++) {
      if (r->boxes[i]) {
	*(r->mrefs[i]) = parent;
	add_managed_box(parent, r->boxes[i], r->mrefs[i], r->closers[i], r->data[i]);
      }
    }
  }

  *r->parent = NULL;
  *r->sibling = NULL;
  *r->children = NULL;

  RELEASE_CUST_LOCK();
}

Scheme_Manager *scheme_make_manager(Scheme_Manager *parent) 
{
  Scheme_Manager *m;

  m = MALLOC_ONE_TAGGED(Scheme_Manager);

  m->type = scheme_manager_type;

  m->alloc = m->count = 0;

  m->parent = MALLOC_ONE_ATOMIC(Scheme_Manager*);
  m->children = MALLOC_ONE_ATOMIC(Scheme_Manager*);
  m->sibling = MALLOC_ONE_ATOMIC(Scheme_Manager*);

  *(m->children) = NULL;
  *(m->sibling) = NULL;

  *(m->parent) = parent;
  if (parent) {
    *m->sibling = *parent->children;
    *parent->children = m;
  } else
    *(m->sibling) = NULL;

  scheme_add_finalizer(m, adjust_manager_family, NULL);

  return m;
}

static void rebox_willdone_object(void *o, void *mr)
{
  Scheme_Manager *m = *(Scheme_Manager **)mr;
  Scheme_Close_Manager_Client *f;
  void *data;

  /* Still needs management? */
  if (m) {
    Scheme_Object **b;

    remove_managed(mr, o, &f, &data);

    b = MALLOC_ONE(Scheme_Object*); /* not atomic this time */
    *b = o;
    
    /* Put the manager back: */
    *(Scheme_Manager **)mr = m;

    add_managed_box(m, b, mr, f, data);
  }
}

static void managed_object_gone(void *o, void *mr)
{
  Scheme_Manager *m = *(Scheme_Manager **)mr;

  /* Still has management? */
  if (m)
    remove_managed(mr, o, NULL, NULL);
}


Scheme_Manager_Reference *scheme_add_managed(Scheme_Manager *m, Scheme_Object *o, 
					     Scheme_Close_Manager_Client *f, void *data, int must_close)
{
  Scheme_Object **b;
  Scheme_Manager_Reference *mr;

  b = MALLOC_ONE_ATOMIC(Scheme_Object*);
  *b = o;

  mr = MALLOC_ONE_ATOMIC(Scheme_Manager_Reference);

  if (!m)
    m = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);

  *mr = m;

  /* The atomic link via the box `b' allows the execution of wills for
     o. After this, we should either drop the object or we have to
     hold on to the object strongly (for when manager-close-all is
     called). */
  if (must_close)
    scheme_add_finalizer(o, rebox_willdone_object, mr);
  else
    scheme_add_finalizer(o, managed_object_gone, mr);

#ifdef MZ_REAL_THREADS
  /* GCing while we have the lock would be bad: */
  ensure_manage_space(m, 1);

  if (!cust_mutex)
    cust_mutex = SCHEME_MAKE_MUTEX();
#endif

  GET_CUST_LOCK();
  add_managed_box(m, b, mr, f, data);
  RELEASE_CUST_LOCK();

  return mr;
}

void scheme_remove_managed(Scheme_Manager_Reference *mr, Scheme_Object *o)
{
  remove_managed(mr, o, NULL, NULL);
}

static Scheme_Process *do_close_managed(Scheme_Manager *m)
{
  Scheme_Process *kill_self = NULL, *ks;
  Scheme_Manager *c, *next;

  /* Kill children first: */
  for (c = *(m->children); c; c = next) {
    next = *(c->sibling);
    ks = do_close_managed(c);
    if (ks)
      kill_self = ks;
  }

  while (m->count) {
    int i = m->count - 1;
    if (m->boxes[i]) {
      Scheme_Object *o;
      Scheme_Close_Manager_Client *f;
      void *data;

      o = *(m->boxes[i]);

      f = m->closers[i];
      data = m->data[i];
      *(m->boxes[i]) = NULL;
      m->boxes[i] = NULL;
      *(m->mrefs[i]) = NULL;
      m->mrefs[i] = NULL;
      m->data[i] = NULL;
      --m->count;

      if (SCHEME_PROCESSP(o)) {
#ifndef NO_SCHEME_THREADS
	/* We've added an indirection and made it weak. See mr_hop note above. */
	Scheme_Process *p = ((Scheme_Process_Manager_Hop *)o)->p;

	if (p)
	  if (do_kill_thread(p))
	    kill_self = p;
#endif
      } else {
	f(o, data);
      }
    } else {
      --m->count;
    }
  }

  return kill_self;
}

void scheme_close_managed(Scheme_Manager *m)
/* The trick is that we may need to kill the thread
   that is running us. If so, delay it to the very
   end. */
{
  Scheme_Process *p;

#ifndef NO_SCHEME_THREADS
  if ((p = do_close_managed(m))) {
    /* Kill self */
# ifdef MZ_REAL_THREADS
    remove_process(scheme_current_process);
    SCHEME_EXIT_THREAD();
# else
    scheme_kill_thread(p);
# endif
  }
#endif
}

void scheme_set_tail_buffer_size(int s)
{
  SCHEME_GET_LOCK();
  if (s > buffer_init_size) {
    Scheme_Process *p;

    buffer_init_size = s;

    for (p = scheme_first_process; p; p = p->next)
      if (p->tail_buffer_size < s) {
	p->tail_buffer = (Scheme_Object **)scheme_malloc(buffer_init_size 
							 * sizeof(Scheme_Object *));
	p->tail_buffer_size = buffer_init_size;
      }
  }
  SCHEME_RELEASE_LOCK();  
}

Scheme_Process *scheme_make_process()
{
  return make_process(NULL, NULL, NULL);
}

int scheme_in_main_thread(void)
{
  return !scheme_current_process->next;
}

#ifdef MZ_REAL_THREADS
Scheme_Process *scheme_get_current_process()
{
  return scheme_current_process;
}
#endif

#ifndef MZ_REAL_THREADS

void scheme_swap_process(Scheme_Process *new_process)
{
  scheme_zero_unneeded_rands(scheme_current_process);

  if (!swap_no_setjmp && SETJMP(scheme_current_process)) {
    /* We're back! */
#ifdef RUNSTACK_IS_GLOBAL
    MZ_RUNSTACK = scheme_current_process->runstack;
    MZ_RUNSTACK_START = scheme_current_process->runstack_start;
    MZ_CONT_MARK_STACK = scheme_current_process->cont_mark_stack;
    MZ_CONT_MARK_POS = scheme_current_process->cont_mark_pos;
#endif
    RESETJMP(scheme_current_process);
  } else {
    swap_no_setjmp = 0;

    /* We're leaving... */
#ifdef RUNSTACK_IS_GLOBAL
    scheme_current_process->runstack = MZ_RUNSTACK;
    scheme_current_process->runstack_start = MZ_RUNSTACK_START;
    scheme_current_process->cont_mark_stack = MZ_CONT_MARK_STACK;
    scheme_current_process->cont_mark_pos = MZ_CONT_MARK_POS;
#endif
    scheme_current_process = new_process;
    LONGJMP(scheme_current_process);
  }
}

#endif

static void remove_process(Scheme_Process *r)
{
  Scheme_Saved_Stack *saved;

  r->running = 0;

#ifdef MZ_REAL_THREADS
  scheme_post_sema(r->done_sema);
#endif

  if (r->prev) {
    r->prev->next = r->next;
    r->next->prev = r->prev;
  } else {
    if (r->next)
      r->next->prev = NULL;
    scheme_first_process = r->next;
  }
  r->next = r->prev = NULL;

#ifdef RUNSTACK_IS_GLOBAL
  if (r == scheme_current_process) {
    r->runstack = MZ_RUNSTACK;
    r->runstack_start = MZ_RUNSTACK_START;
    r->cont_mark_stack = MZ_CONT_MARK_STACK;
    r->cont_mark_pos = MZ_CONT_MARK_POS;
  }
#endif

#ifdef SENORA_GC_NO_FREE
  memset(r->runstack_start, 0, r->runstack_size * sizeof(Scheme_Object*));
#else
  GC_free(r->runstack_start);
#endif
  r->runstack_start = NULL;
  for (saved = r->runstack_saved; saved; saved = saved->prev) {
#ifdef SENORA_GC_NO_FREE
    memset(saved->runstack_start, 0, saved->runstack_size * sizeof(Scheme_Object*));
#else
    GC_free(saved->runstack_start);
#endif
    saved->runstack_start = NULL;
  }

#ifndef SENORA_GC_NO_FREE
  if (r->list_stack)
    GC_free(r->list_stack);
#endif
  r->list_stack = NULL;

#ifndef MZ_REAL_THREADS
  if (r == scheme_current_process) {
    /* We're going to be swapped out immediately. */
    swap_no_setjmp = 1;
  } else
    RESETJMP(r);
#endif

  scheme_remove_managed(r->mref, (Scheme_Object *)r->mr_hop);
}

static void start_child(Scheme_Process *child,
			Scheme_Process *return_to_process,
			Scheme_Object *child_eval)
{
  if (SETJMP(child)) {
#ifdef RUNSTACK_IS_GLOBAL
    MZ_RUNSTACK = scheme_current_process->runstack;
    MZ_RUNSTACK_START = scheme_current_process->runstack_start;
    MZ_CONT_MARK_STACK = scheme_current_process->cont_mark_stack;
    MZ_CONT_MARK_POS = scheme_current_process->cont_mark_pos;
#endif

    RESETJMP(child);

#ifndef MZ_REAL_THREADS
    if (return_to_process)
      scheme_swap_process(return_to_process);
#endif    

    if (!scheme_setjmp(scheme_error_buf)) 
      scheme_apply_multi(child_eval, 0, NULL);
    
    SCHEME_GET_LOCK();
    
    remove_process(scheme_current_process);
    SCHEME_RELEASE_LOCK();
    
#ifndef MZ_REAL_THREADS
    process_ended_with_activity = 1;
    
    if (scheme_notify_multithread && !scheme_first_process->next) {
      scheme_notify_multithread(0);
      have_activity = 0;
    }
#endif
    
#ifndef MZ_REAL_THREADS
    if (scheme_current_process->next)
      scheme_swap_process(scheme_current_process->next);
    else
      scheme_swap_process(scheme_first_process);
    
    /* Shouldn't get here! */
    scheme_signal_error("bad process switch");
#endif
  }
}

#if defined(MZ_REAL_THREADS)

typedef struct {
  Scheme_Process *sc_child, *sc_rtp;
  Scheme_Object *sc_eval;
} ThreadStartData;

static void really_start_child(void *data)
{
  long dummy;
  ThreadStartData *th = (ThreadStartData *)data;

#ifdef MZ_REAL_THREADS
  SCHEME_SET_CURRENT_PROCESS(th->sc_child);
  th->sc_child->stack_start = (void *)&dummy;
#endif

  start_child(th->sc_child, th->sc_rtp, th->sc_eval);
}

#endif /* defined(MZ_REAL_THREADS) */

#if defined(MZ_REAL_THREADS)

static void do_start_child(Scheme_Process *child, Scheme_Process *rtp,
			   Scheme_Object *child_eval)
{
  Scheme_Process *sc_child, *sc_rtp;
  Scheme_Object *sc_eval;

  sc_child = child;
  sc_rtp = rtp;
  sc_eval = child_eval;

  {
    ThreadStartData *th = scheme_malloc(sizeof(ThreadStartData));
    
    th->sc_child = sc_child;
    th->sc_rtp = sc_rtp;
    th->sc_eval = sc_eval;

    SCHEME_CREATE_THREAD(really_start_child, (void *)th, 
			 (unsigned long *)&sc_child->stack_end,
			 &sc_child->thread);
  }
}

#endif

static Scheme_Object *make_subprocess(Scheme_Object *child_thunk,
				      void *child_start, 
				      Scheme_Config *config,
				      Scheme_Manager *mgr)
{
  Scheme_Process *child, *return_to_process;
  int turn_on_multi;
 
  turn_on_multi = !scheme_first_process->next;
  
  scheme_ensure_stack_start(scheme_current_process, child_start);
  
  child = make_process(NULL, config, mgr);

  scheme_init_error_escape_proc(child);

  child->stack_start = child_start;
  
#ifndef MZ_REAL_THREADS
  if (do_atomic)
    return_to_process = scheme_current_process;
  else
#endif
    return_to_process = NULL;

#if defined(MZ_REAL_THREADS)
  do_start_child(child, return_to_process, child_thunk);
#else
  start_child(child, return_to_process, child_thunk);
#endif

#ifndef MZ_REAL_THREADS
  if (scheme_notify_multithread && turn_on_multi) {
    scheme_notify_multithread(1);
    have_activity = 1;
  }

  SCHEME_USE_FUEL(1000);
#endif
  
  return (Scheme_Object *)child;
}

Scheme_Object *scheme_thread(Scheme_Object *thunk, Scheme_Config *config)
{
  return scheme_thread_w_manager(thunk, config, NULL);
}

/* Stuff for ensuring that a new thread has a reasonable starting stack: */
#ifndef MZ_REAL_THREADS
# ifdef DO_STACK_CHECK
#  define THREAD_STACK_SPACE (STACK_SAFETY_MARGIN / 2)
void scheme_check_stack_ok(char *s) {
#  include "mzstkchk.h"
  {
    s[THREAD_STACK_SPACE] = 1;
  } else {
    s[THREAD_STACK_SPACE] = 0;
  }
}

static int is_stack_too_shallow2(void)
{
  char s[THREAD_STACK_SPACE+1];
  
  scheme_check_stack_ok(s);
  return s[THREAD_STACK_SPACE];
}

static int is_stack_too_shallow(void)
{
#  include "mzstkchk.h"
  {
    return 1;
  }
  return is_stack_too_shallow2();
}

static Scheme_Object *thread_k()
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *thunk;
  Scheme_Config *config;
  Scheme_Manager *mgr;
  long dummy;
  
  thunk = (Scheme_Object *)p->ku.k.p1;
  config = (Scheme_Config *)p->ku.k.p2;
  mgr = (Scheme_Manager *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  
  return make_subprocess(thunk, (void *)&dummy, config, mgr);
}
# endif
#endif

Scheme_Object *scheme_thread_w_manager(Scheme_Object *thunk, Scheme_Config *config, 
				       Scheme_Manager *mgr)
{
  long dummy;

#ifndef MZ_REAL_THREADS
# ifdef DO_STACK_CHECK
  /* Make sure the thread starts out with a reasonable stack size, so
     it doesn't thrash right away: */
  if (is_stack_too_shallow()) {
    Scheme_Process *p = scheme_current_process;

    p->ku.k.p1 = thunk;
    p->ku.k.p2 = config;
    p->ku.k.p3 = mgr;

    return scheme_handle_stack_overflow(thread_k);
  }
# endif
#endif

  return make_subprocess(thunk, (void *)&dummy, config, mgr);
}

void scheme_break_thread(Scheme_Process *p)
{
  if (!p) {
    p = scheme_main_process;
    if (!p) return;
    if (p->external_break) /* needed for real threads */
      return;
  }

  p->external_break = 1;

#ifndef MZ_REAL_THREADS
  if (p == scheme_current_process)
    scheme_fuel_counter = 0;
  scheme_weak_resume_thread(p);
# if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)
#  ifndef NO_STDIO_THREADS
  if (!p->next)
    ReleaseSemaphore(scheme_break_semaphore, 1, NULL);
#  endif
# endif
#else
  p->fuel_counter = 0;
  SCHEME_BREAK_THREAD(p->thread);
#endif
}

void scheme_add_namespace_option(Scheme_Object *key, void (*f)(Scheme_Env *))
{
  Scheme_NSO *old = namespace_options;
  
  namespace_options = (Scheme_NSO *)scheme_malloc((num_nsos + 1) 
						  * sizeof(Scheme_NSO));

  memcpy(namespace_options, old, num_nsos * sizeof(Scheme_NSO));

  namespace_options[num_nsos].key = key;
  namespace_options[num_nsos].f = f;
  
  num_nsos++;
}

Scheme_Object *scheme_make_namespace(int argc, Scheme_Object *argv[])
{
  int save_no_key, save_ec_only, save_hp_syntax;
  int i, with_nso = 0;
  int empty = 0;
  Scheme_Object *v;
  Scheme_Env *env;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(make_namespace_mutex);
#endif

  save_no_key = scheme_no_keywords;
  save_ec_only = scheme_escape_continuations_only;
  save_hp_syntax = scheme_hash_percent_syntax_only;

  for (i = 0; i < argc; i++) {
    v = argv[i];
    if (v == keywords_symbol)
      scheme_no_keywords = 0;
    else if (v == no_keywords_symbol)
      scheme_no_keywords = 1;
    else if (v == callcc_is_callec_symbol)
      scheme_escape_continuations_only = 1;
    else if (v == callcc_is_not_callec_symbol)
      scheme_escape_continuations_only = 0;
    else if (v == hash_percent_syntax_symbol)
      scheme_hash_percent_syntax_only = 1;
    else if (v == all_syntax_symbol)
      scheme_hash_percent_syntax_only = 0;
    else if (v == empty_symbol)
      empty = 1;
    else {
      int j;
      for (j = 0; j < num_nsos; j++)
	if (namespace_options[j].key == v) {
	  with_nso = 1;
	  break;
	}
      
      if (j >= num_nsos)
	scheme_wrong_type("make-namespace", "symbol-flag", i, argc, argv);
    }
  }
  
#ifndef MZ_REAL_THREADS
  do_atomic++;
#endif
  env = (empty ? scheme_make_empty_env() : scheme_top_level_env());
#ifndef MZ_REAL_THREADS
  --do_atomic;
#endif

  scheme_no_keywords = save_no_key;   
  scheme_escape_continuations_only = save_ec_only;
  scheme_hash_percent_syntax_only = save_hp_syntax;

  if (with_nso && !empty) {
    int j;

    for (i = 0; i < argc; i++) {
      v = argv[i];
      for (j = 0; j < num_nsos; j++)
	if (namespace_options[j].key == v) {
	  namespace_options[j].f(env);
	  break;
	}
    }
  }

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(make_namespace_mutex);
#endif

  return (Scheme_Object *)env;
}

#ifndef MZ_REAL_THREADS
static int check_sleep(int need_activity, int sleep_now)
{
  Scheme_Process *p, *p2;
  int end_with_act;
  
#if defined(USING_FDS)
  DECL_FDSET(set, 3);
  fd_set *set1, *set2;
#endif
  void *fds;

  /* Is everything blocked? */
  if (!do_atomic) {
    p = scheme_first_process;
    while (p) {
      if (p->ran_some || p->block_descriptor == NOT_BLOCKED)
	break;
      p = p->next;
    }
  } else
    p = NULL;
  
  p2 = scheme_first_process;
  while (p2) {
    p2->ran_some = 0;
    p2 = p2->next;
  }
  
  end_with_act = process_ended_with_activity;
  process_ended_with_activity = 0;
  
  if (need_activity && !end_with_act && 
      (do_atomic 
       || (!p && ((!sleep_now && scheme_wakeup_on_input)
		  || (sleep_now && scheme_sleep))))) {
    float max_sleep_time = 0;

    /* Poll from top-level process, and all subprocesses are blocked. */
    /* So, everything is blocked pending external input. */
    /* Build a list of file descriptors that we're waiting on */
    /* and turn off polling. */
    if (have_activity)
      scheme_active_but_sleeping = 1;
    if (have_activity && scheme_notify_multithread)
      scheme_notify_multithread(0);
    
#if defined(USING_FDS)
    INIT_DECL_FDSET(set, 3);
    set1 = (fd_set *) MZ_GET_FDSET(set, 1);
    set2 = (fd_set *) MZ_GET_FDSET(set, 2);

    fds = (void *)set;
    MZ_FD_ZERO(set);
    MZ_FD_ZERO(set1);
    MZ_FD_ZERO(set2);
#else
    fds = NULL;
#endif
    
    p = scheme_first_process;
    while (p) {
      int merge_time = 0;

      if (p->block_descriptor == -1) {
	if (p->block_needs_wakeup)
	  (p->block_needs_wakeup)(p->blocker, fds);
	merge_time = (p->sleep_time > 0.0);
      } else if (p->block_descriptor == PORT_BLOCKED)
	scheme_need_wakeup(p->blocker, fds);
      else if (p->block_descriptor == SLEEP_BLOCKED) {
	merge_time = 1;
      }

      if (merge_time) {
	long d = (long)p->block_start_sleep;
	float t;

	d = (scheme_get_milliseconds() - d);

	if (d < 0)
	  d = -d;
	
	t = p->sleep_time - (((float)d) / 1000);
	if (t <= 0)
	  t = 0.00001;
	if (!max_sleep_time || (t < max_sleep_time))
	  max_sleep_time = t;
      } 
      p = p->next;
    }
    
    if (sleep_now)
      scheme_sleep(max_sleep_time, fds);
    else
      scheme_wakeup_on_input(fds);
    
    return 1;
  }
  
  return 0;
}
#endif

void scheme_check_threads(void)
{
#ifndef MZ_REAL_THREADS
  scheme_current_process->suspend_break = 1;
  scheme_process_block((float)0);
  scheme_current_process->suspend_break = 0;

  check_sleep(have_activity, 0);
#endif
}

void scheme_wake_up(void)
{
#ifndef MZ_REAL_THREADS
  scheme_active_but_sleeping = 0;
  if (have_activity && scheme_notify_multithread)
    scheme_notify_multithread(1);
#endif
}

void scheme_out_of_fuel(void)
{
#ifndef MZ_REAL_THREADS
  scheme_process_block((float)0);
  scheme_current_process->ran_some = 1;
#endif
}

void scheme_zero_unneeded_rands(Scheme_Process *p)
{
  /* Call this procedure before GC or before copying out
     a thread's stack. */
}

static void get_ready_for_GC()
{
  start_this_gc_time = scheme_get_process_milliseconds();

  scheme_zero_unneeded_rands(scheme_current_process);

#ifdef RUNSTACK_IS_GLOBAL
  scheme_current_process->runstack = MZ_RUNSTACK;
  scheme_current_process->runstack_start = MZ_RUNSTACK_START;
  scheme_current_process->cont_mark_stack = MZ_CONT_MARK_STACK;
  scheme_current_process->cont_mark_pos = MZ_CONT_MARK_POS;
#endif

#ifndef MZ_REAL_THREADS
# define RUNSTACK_TUNE(x) /* x   - Used for performance tuning */
  if (scheme_fuel_counter) {
    Scheme_Process *p;

    /* zero ununsed part of env stack in each thread */
    for (p = scheme_first_process; p; p = p->next) {
      Scheme_Object **o, **e, **e2;
      Scheme_Saved_Stack *saved;
      RUNSTACK_TUNE( long size; );

      o = p->runstack_start;
      e = p->runstack;
      e2 = p->runstack_tmp_keep;

      while (o < e && (o != e2))
	*(o++) = NULL;

      RUNSTACK_TUNE( size = p->runstack_size - (p->runstack - p->runstack_start); );

      for (saved = p->runstack_saved; saved; saved = saved->prev) {
	o = saved->runstack_start;
	e = saved->runstack_start;
	RUNSTACK_TUNE( size += saved->runstack_size; );
	while (o < e)
	  *(o++) = NULL;
      }

      RUNSTACK_TUNE( printf("%ld\n", size); );

# ifdef AGRESSIVE_ZERO_TB
      {
	int i;
	for (i = p->tail_buffer_set; i < p->tail_buffer_size; i++)
	  p->tail_buffer[i] = NULL;
      }
# endif

      /* release unused cont mark stack segments */
      {
	int segcount, i;
	if (p->cont_mark_stack)
	  segcount = ((long)(p->cont_mark_stack - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;
	else
	  segcount = 0;
	for (i = segcount; i < p->cont_mark_seg_count; i++)
	  p->cont_mark_stack_segments[i] = NULL;
	if (segcount < p->cont_mark_seg_count)
	  p->cont_mark_seg_count = segcount;
      }
      
      /* zero unused part of last mark stack segment */
      {
	int segpos = ((long)p->cont_mark_stack >> SCHEME_LOG_MARK_SEGMENT_SIZE);
	
	if (segpos < p->cont_mark_seg_count) {
	  Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[segpos];
	  int stackpos = ((long)p->cont_mark_stack & SCHEME_MARK_SEGMENT_MASK), i;
	  for (i = stackpos; i < SCHEME_MARK_SEGMENT_SIZE; i++) {
	    seg[i].key = NULL;
	    seg[i].val = NULL;
	  }
	}
      }
    }
  }
#endif
   
  scheme_fuel_counter = 0;
}

extern int GC_words_allocd;

static void done_with_GC()
{
  scheme_total_gc_time += (scheme_get_process_milliseconds() - start_this_gc_time);
}

int scheme_can_break(Scheme_Process *p, Scheme_Config *config)
{
  return (SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ENABLE_BREAK))
	  && (!p->exn_raised 
	      || SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ENABLE_EXCEPTION_BREAK))));
}

int scheme_block_until(int (*f)(Scheme_Object *), void (*fdf)(Scheme_Object *,void*), 
		       void *data, float delay)
{
  int result;
  Scheme_Process *p = scheme_current_process;

  p->block_descriptor = -1;
  p->blocker = (Scheme_Object *)data;
  p->block_check = f;
  p->block_needs_wakeup = fdf;
  do
    scheme_process_block(delay);
  while (!(result = f((Scheme_Object *)data)));
  p->block_descriptor = NOT_BLOCKED;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
  p->ran_some = 1;

  return result;
}

static void make_unblocked(Scheme_Process *p)
{
  p->block_descriptor = 0;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
  p->ran_some = 1;
}

#ifndef MZ_REAL_THREADS
void scheme_process_block(float sleep_time)
#else
void scheme_process_block_w_process(float sleep_time, Scheme_Process *p)
#endif
/* Auto-resets p's blocking info if an escape occurs. */
{
  long start, d;
#ifndef MZ_REAL_THREADS
  Scheme_Process *next, *p = scheme_current_process;
#endif
  Scheme_Config *config = p->config;

#ifndef MZ_REAL_THREADS
  if (p->running & MZTHREAD_KILLED) {
    /* This thread is dead! Give up now. */
    remove_process(p);
    scheme_swap_process(scheme_first_process);
  }

  if (scheme_active_but_sleeping)
    scheme_wake_up();
#else
  if (!p->running || (p->running & MZTHREAD_KILLED)) {
    remove_process(p);
    SCHEME_EXIT_THREAD();
  }
#endif

  if (sleep_time > 0)
    start = scheme_get_milliseconds();
  else
    start = 0; /* compiler-friendly */

  if (!p->external_break && !p->next && scheme_check_for_break && scheme_check_for_break())
    p->external_break = 1;

  if (p->external_break && !p->suspend_break && scheme_can_break(p, config)) {
    p->external_break = 0;
    make_unblocked(p);
    p->ran_some = 1;
    scheme_raise_exn(MZEXN_MISC_USER_BREAK, "user break");
  }
  
 swap_or_sleep:
  
  /* Check for active wills: */
  {
    WillExecutor *w;
    
    w = (WillExecutor *)scheme_get_param(config, MZCONFIG_WILL_EXECUTOR);
    while (w->first && !w->running && MZTHREAD_STILL_RUNNING(p->running)) {
      p->ran_some = 1;
      do_next_will(w, 0);
      p->ran_some = 1;
    }
  }

#ifndef MZ_REAL_THREADS
  if (!do_atomic && (sleep_time >= 0.0)) {
    /* Find the next process. Skip processes that are definitely
       blocked. */
    
    next = p;
    while (1) {
      next = next->next ? next->next : scheme_first_process;
      if (SAME_PTR(next, p)) {
	next = NULL;
	break;
      }
      
      if (next->running & MZTHREAD_KILLED) {
	/* This one has been terminated. */
	if (next->running & MZTHREAD_NEED_KILL_CLEANUP) {
	  /* The thread needs to clean up. Swap it in so it can die. */
	  break;
	} else
	  remove_process(next);
	break;
      } else if (next->external_break && scheme_can_break(next, next->config)) {
	break;
      } else {
	if (next->block_descriptor == -1) {
	  if (next->block_check)
	    if ((next->block_check)(next->blocker))
	      break;
	} else if (next->block_descriptor == EVENTLOOP_BLOCKED) {
	  /* Can't use it. */
	} else if (next->block_descriptor == SEMA_BLOCKED) {
	  Scheme_Sema *sema = (Scheme_Sema *)next->blocker;
	  if (sema->value)
	    break;
	} else if ((next->block_descriptor == PORT_BLOCKED)
		   || (next->block_descriptor == PIPE_BLOCKED)) {
	  int ready;
	  scheme_internal_checking_char = 1;
	  ready = scheme_char_ready(next->blocker);
	  scheme_internal_checking_char = 0;
	  if (ready)
	    break;
	} else if (next->block_descriptor == SLEEP_BLOCKED) {
	  d = (scheme_get_milliseconds() - (long)next->block_start_sleep);
	  if (d < 0)
	    d = -d;
	  if (d >= (next->sleep_time * 1000))
	    break;
	} else
	  break;

	/* If the will executor has something, the thread can work: */
	{
	  WillExecutor *w;

	  w = (WillExecutor *)scheme_get_param(next->config, MZCONFIG_WILL_EXECUTOR);
	  if (w->first && !w->running)
	    break;
	}
      }
    }
  } else
    next = NULL;
  
  if ((sleep_time > 0.0) && (p->block_descriptor == NOT_BLOCKED)) {
    p->block_descriptor = SLEEP_BLOCKED;
    p->block_start_sleep = start;
    p->sleep_time = sleep_time;
  } else if ((sleep_time > 0.0) && (p->block_descriptor == -1)) {
    p->block_start_sleep = start;
    p->sleep_time = sleep_time;
  }

  if (next && (!next->running || (next->running & MZTHREAD_SUSPENDED))) {
    /* In the process of selecting another thread, it was suspended or
       removed. Very unusual, but possible if a block checker does
       stange things??? */
    next = NULL;
  }

#if 0
  /* Debugging: next must be in the chain of processes */
  if (next) {
    Scheme_Process *p = scheme_first_process;
    while (p != next) {
      p = p->next;
      if (!p) {
	printf("error: tried to switch to bad thread\n");
	exit(-1);
      }
    }
  }
#endif

  if (next) {
    if (!p->next) {
      /* This is the main process */
      scheme_ensure_stack_start(p, (void *)&start);
    }
    
    scheme_swap_process(next);
  } else {
    /* If all processes are blocked, check for total process sleeping: */
    if (p->block_descriptor != NOT_BLOCKED)
      check_sleep(1, 1);
  }

  if (p->block_descriptor == SLEEP_BLOCKED) {
    p->block_descriptor = NOT_BLOCKED;
    p->sleep_time = 0.0;
  } else if (p->block_descriptor == -1) {
    p->sleep_time = 0.0;
  }
#else
  if ((sleep_time > 0) && scheme_sleep)
    scheme_sleep(sleep_time, NULL);
#endif

#ifndef MZ_REAL_THREADS
  /* Killed while I was asleep? */
  if (p->running & MZTHREAD_KILLED) {
    /* This thread is dead! Give up now. */
    if (p->running & MZTHREAD_NEED_KILL_CLEANUP) {
      /* The thread needs to clean up. It will block immediately to die. */
      return;
    } else {
      remove_process(p);
      scheme_swap_process(scheme_first_process);
    }
  }
#else
  if (!p->running || (p->running & MZTHREAD_KILLED)) {
    remove_process(p);
    SCHEME_EXIT_THREAD();
  }
#endif

  /* Check for external break again after swap or sleep */
  if (p->external_break && !p->suspend_break && scheme_can_break(p, config)) {
    p->external_break = 0;
    make_unblocked(p);
    p->ran_some = 1;
    scheme_raise_exn(MZEXN_MISC_USER_BREAK, "user break");
  }
  
  if (sleep_time > 0) {
    d = (scheme_get_milliseconds() - start);
    if (d < 0)
      d = -d;
    if (d < (sleep_time * 1000))
      goto swap_or_sleep;
  }

#if defined(MZ_REAL_THREADS) && (defined(USING_FDS))
  if ((p->block_descriptor == PORT_BLOCKED)
      || (p->block_descriptor == -1)) {
    DECL_FDSET(set, 3);
    fd_set *set1, *set2;
    void *fds;
    float sleep_time = 0;

    INIT_DECL_FDSET(set, 3);
    set1 = (fd_set *) MZ_GET_FDSET(set, 1);
    set2 = (fd_set *) MZ_GET_FDSET(set, 2);

    fds = (void *)set;
    MZ_FD_ZERO(set);
    MZ_FD_ZERO(set1);
    MZ_FD_ZERO(set2);
    
    if (p->block_descriptor == -1) {
      if (p->block_needs_wakeup)
	(p->block_needs_wakeup)(p->blocker, fds);
      sleep_time = p->sleep_time;
    } else
      scheme_need_wakeup(p->blocker, fds);
    
    if (scheme_sleep)
      scheme_sleep(sleep_time, fds);
  }
#endif

#ifndef MZ_REAL_THREADS
  if (do_atomic)
    missed_context_switch = 1;
#endif

  MZTHREADELEM(p, fuel_counter) = p->engine_weight;
}

#ifndef MZ_REAL_THREADS
void scheme_start_atomic(void)
{
  if (!do_atomic)
    missed_context_switch = 0;
  do_atomic++;
}

void scheme_end_atomic(void)
{
  --do_atomic;
  if (!do_atomic && missed_context_switch) {
    scheme_process_block(0.0);
    scheme_current_process->ran_some = 1;    
  }
}
#endif

void scheme_weak_suspend_thread(Scheme_Process *r)
{
#ifndef MZ_REAL_THREADS
  Scheme_Process *swap_to = r->next;

  if (r->prev) {
    r->prev->next = r->next;
    r->next->prev = r->prev;
  } else {
    if (r->next)
      r->next->prev = NULL;
    scheme_first_process = r->next;
  }

  r->next = r->prev = NULL;

  r->running |= MZTHREAD_SUSPENDED;

  if (r == scheme_current_process) {
    if (!swap_to)
      swap_to = scheme_first_process;
    
    scheme_swap_process(swap_to);

    /* Killed while suspended? */
    if ((r->running & MZTHREAD_KILLED) && !(r->running & MZTHREAD_NEED_KILL_CLEANUP))
      scheme_process_block(0);
  }
#endif
}

void scheme_weak_resume_thread(Scheme_Process *r)
{
#ifndef MZ_REAL_THREADS
  if (r->running & MZTHREAD_SUSPENDED) {
    r->running -= MZTHREAD_SUSPENDED;
    r->next = scheme_first_process;
    r->prev = NULL;
    scheme_first_process = r;
    r->next->prev = r;
    r->ran_some = 1;
  }
#endif
}

Scheme_Object *scheme_branch_config(void)
{
  return scheme_make_config(scheme_config);
}

static Scheme_Object *sch_thread(int argc, Scheme_Object *args[])
{
  scheme_check_proc_arity("thread", 0, 0, argc, args);

  return scheme_thread(args[0], (Scheme_Config *)scheme_branch_config());
}

static Scheme_Object *
sch_sleep(int argc, Scheme_Object *args[])
{
  float t;

  if (argc && !SCHEME_REALP(args[0]))
    scheme_wrong_type("sleep", "non-negative real number", 0, argc, args);

  if (argc) {
    t = scheme_real_to_double(args[0]);
    if (t < 0)
      scheme_wrong_type("sleep", "non-negative real number", 0, argc, args);
  } else
    t = 0;

  scheme_process_block(t);
  scheme_current_process->ran_some = 1;

  return scheme_void;
}

static Scheme_Object *sch_current(int argc, Scheme_Object *args[])
{
  return (Scheme_Object *)scheme_current_process;
}

static Scheme_Object *break_thread(int argc, Scheme_Object *args[])
{
  Scheme_Process *p;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_process_type))
    scheme_wrong_type("break-thread", "thread", 0, argc, args);

  p = (Scheme_Process *)args[0];

  scheme_break_thread(p);

  return scheme_void;
}

static int do_kill_thread(Scheme_Process *p)
{
  int kill_self = 0;

  scheme_weak_resume_thread(p);

  if (!p->next) {
    if (scheme_exit)
      scheme_exit(0);

    /* We really have to exit: */
    exit(0);
  }

#ifdef MZ_REAL_THREADS
  SCHEME_GET_LOCK();
#endif

  if (!p->running || (p->running & MZTHREAD_KILLED)) {
#ifdef MZ_REAL_THREADS
    SCHEME_RELEASE_LOCK();
#endif
    return 0;
  }

  if (p->on_kill)
    p->on_kill(p);

  scheme_remove_managed(p->mref, (Scheme_Object *)p->mr_hop);

#ifdef MZ_REAL_THREADS
  p->running |= MZTHREAD_KILLED;
  if (p == scheme_current_process)
    kill_self = 1;
  else {
    p->fuel_counter = 0;
    /* in case it's blocked on a semaphore: */
    SCHEME_BREAK_THREAD(p->thread);
  }
  SCHEME_RELEASE_LOCK();
#else
  if (p->running) {
    p->running |= MZTHREAD_KILLED;
    if (p->running & MZTHREAD_NEED_KILL_CLEANUP)
      scheme_weak_resume_thread(p);
  }
  if (p == scheme_current_process)
    kill_self = 1;
#endif

  return kill_self;
}

#ifndef NO_SCHEME_THREADS
void scheme_kill_thread(Scheme_Process *p)
{
  if (do_kill_thread(p)) {
#ifdef MZ_REAL_THREADS
    /* Kill self: */
    remove_process(scheme_current_process);
    SCHEME_EXIT_THREAD();
#endif
  }

#ifndef MZ_REAL_THREADS
  scheme_process_block(0);
#endif
}

static Scheme_Object *kill_thread(int argc, Scheme_Object *argv[])
{
  Scheme_Manager *m, *current;
  Scheme_Process *p = (Scheme_Process *)argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_process_type))
    scheme_wrong_type("kill-thread", "thread", 0, argc, argv);

  if (!MZTHREAD_STILL_RUNNING(p->running))
    return scheme_void;

  /* Check management of the thread: */
  current = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);
  m = *p->mref;
  while (NOT_SAME_OBJ(m, current)) {
    m = *m->parent;
    if (!m) {
      scheme_raise_exn(MZEXN_MISC,
		       "kill-thread: the current custodian does not "
		       "manage the specified thread");
      return NULL;
    }
  }

  scheme_kill_thread(p);

  return scheme_void;
}
#endif

static Scheme_Object *process_weight(int argc, Scheme_Object *args[])
{
  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_process_type))
    scheme_wrong_type("thread-weight", "thread", 0, argc, args);

  if (argc > 1) {
    long v;
    Scheme_Process *p;

    if (!SCHEME_INTP(args[1])) {
      if (!SCHEME_BIGNUMP(args[1]) || !SCHEME_BIGPOS(args[1]))
	scheme_wrong_type("thread-weight", "non-negative exact integer", 1, argc, args);
    }

    if (!scheme_get_int_val(args[1], &v)) {
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       args[1],
		       "thread-weight: %s is too large",
		       scheme_make_provided_string(args[1], 0, NULL));
    } else if (v < 0) {
      scheme_wrong_type("thread-weight", "non-negative exact integer", 1, argc, args);
    }

    p = (Scheme_Process *)args[0];
    
    p->engine_weight = v;
    if (p == scheme_current_process && scheme_fuel_counter > v)
      scheme_fuel_counter = v;
    
    return scheme_void;
  } else
    return scheme_make_integer(((Scheme_Process *)args[0])->engine_weight);
}

static Scheme_Object *processp(int argc, Scheme_Object *args[])
{
  return SCHEME_PROCESSP(args[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *process_running_p(int argc, Scheme_Object *args[])
{
  int running;

  if (!SCHEME_PROCESSP(args[0]))
    scheme_wrong_type("thread-running?", "thread", 0, argc, args);

  running = ((Scheme_Process *)args[0])->running;

  return MZTHREAD_STILL_RUNNING(running) ? scheme_true : scheme_false;
}

#ifndef MZ_REAL_THREADS
static int thread_wait_done(Scheme_Object *p)
{
  int running = ((Scheme_Process *)p)->running;
  return !MZTHREAD_STILL_RUNNING(running);
}
#endif

static Scheme_Object *process_wait(int argc, Scheme_Object *args[])
{
  Scheme_Process *p;

  if (!SCHEME_PROCESSP(args[0]))
    scheme_wrong_type("thread-wait", "thread", 0, argc, args);

  p = (Scheme_Process *)args[0];

  if (MZTHREAD_STILL_RUNNING(p->running)) {
#ifndef MZ_REAL_THREADS
    scheme_block_until(thread_wait_done, NULL, p, 0);
#else
    scheme_wait_sema(p->done_sema, 0);
    scheme_post_sema(p->done_sema);
#endif
  }

  return scheme_void;
}

int scheme_tls_allocate()
{
  return tls_pos++;
}

void scheme_tls_set(int pos, void *v)
{
  Scheme_Process *p = scheme_current_process;

  if (p->user_tls_size <= pos) {
    int oldc = p->user_tls_size;
    void **old_tls = p->user_tls;

    p->user_tls_size = tls_pos;
    p->user_tls = MALLOC_N(void*, tls_pos);
    while (oldc--)
      p->user_tls[oldc] = old_tls[oldc];
  }

  p->user_tls[pos] = v;
}

void *scheme_tls_get(int pos)
{
  Scheme_Process *p = scheme_current_process;

  if (p->user_tls_size <= pos)
    return NULL;
  else
    return p->user_tls[pos];
}

static Scheme_Object *make_manager(int argc, Scheme_Object *argv[])
{
  Scheme_Manager *m;

  if (argc) {
    if (!SCHEME_MANAGERP(argv[0]))
      scheme_wrong_type("make-custodian", "custodian", 0, argc, argv);
    m = (Scheme_Manager *)argv[0];
  } else
    m = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);

  return (Scheme_Object *)scheme_make_manager(m);
}

static Scheme_Object *manager_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_MANAGERP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *manager_close_all(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MANAGERP(argv[0]))
    scheme_wrong_type("custodian-shutdown-all", "custodian", 0, argc, argv);

  scheme_close_managed((Scheme_Manager *)argv[0]);

  return scheme_void;
}

static Scheme_Object *current_manager(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-custodian", MZCONFIG_MANAGER,
			     argc, argv,
			     -1, manager_p, "custodian", 0);
}

static Scheme_Object *parameter_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v = argv[0];

  return (((SCHEME_PRIMP(v) || SCHEME_CLSD_PRIMP(v))
	   && (((Scheme_Primitive_Proc *)v)->flags & SCHEME_PRIM_IS_PARAMETER))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_param(void *data, int argc, Scheme_Object *argv[])
{
  Scheme_Object *guard, **argv2;

  if (argc && argv[0]) {
    guard = ((ParamData *)data)->guard;
    if (guard) {
      Scheme_Object *v;
      long l;
      
      v = _scheme_apply(guard, 1, argv);
      argv2 = (Scheme_Object **)scheme_malloc(l = (argc * sizeof(Scheme_Object **)));
      memcpy(argv2, argv, l);
      argv2[0] = v;
    } else
      argv2 = argv;
  } else
    argv2 = argv;    

  return scheme_param_config("parameter-procedure", 
			     (long)scheme_make_pair((Scheme_Object *)((ParamData *)data)->key,
						    ((ParamData *)data)->defval),
			     argc, argv2,
			     -2, NULL, NULL, 0);
}

static Scheme_Object *make_parameter(int argc, Scheme_Object **argv)
{
  Scheme_Object *p;
  ParamData *data;
  ParamExtensionRec *erec, *prev;
  void *k = scheme_malloc_atomic(1); /* generates a key */

  if (argc > 1)
    scheme_check_proc_arity("make-parameter", 1, 1, argc, argv);

  data = MALLOC_ONE(ParamData);
  data->key = (unsigned long)k;
  data->defval = argv[0];
  data->guard = ((argc > 1) ? argv[1] : NULL);

  p = scheme_make_closed_prim_w_arity(do_param, (void *)data, 
				      "parameter-procedure", 0, 1);
  ((Scheme_Primitive_Proc *)p)->flags |= SCHEME_PRIM_IS_PARAMETER;

  /* Throw away expired ext recs: */
  prev = NULL;
  for (erec = param_ext_recs; erec; erec = erec->next) {
    if (!erec->data->p) {
      if (prev)
	prev->next = erec->next;
      else
	param_ext_recs = erec->next;
    } else
      prev = erec;
  }

  erec = MALLOC_ONE(ParamExtensionRec);
  erec->next = param_ext_recs;
  param_ext_recs = erec;
  erec->data = MALLOC_ONE_ATOMIC(ParamExtensionRecData);
  erec->data->p = p;
  erec->data->key = (unsigned long)k;

  scheme_weak_reference((void **)&erec->data->p);

  return p;
}

static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object **argv)
{
  Scheme_Object *a, *b;

  a = argv[0];
  b = argv[1];

  if (!((SCHEME_PRIMP(a) || SCHEME_CLSD_PRIMP(a))
	&& (((Scheme_Primitive_Proc *)a)->flags & SCHEME_PRIM_IS_PARAMETER)))
    scheme_wrong_type("parameter-procedure=?", "parameter-procedure", 0, argc, argv);
  if (!((SCHEME_PRIMP(b) || SCHEME_CLSD_PRIMP(b))
	&& (((Scheme_Primitive_Proc *)b)->flags & SCHEME_PRIM_IS_PARAMETER)))
    scheme_wrong_type("parameter-procedure=?", "parameter-procedure", 1, argc, argv);

  return (SAME_OBJ(a, b)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *namespace_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_namespace_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_namespace(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-namespace", MZCONFIG_ENV,
			     argc, argv,
			     -1, namespace_p, "namespace", 0);
}

/****************************************/

static int max_configs = __MZCONFIG_BUILTIN_COUNT__;

int scheme_new_param(void)
{
  return max_configs++;
}

static Scheme_Config *make_initial_config(void)
{
  Scheme_Config *config;

  config = (Scheme_Config *)scheme_malloc_tagged(sizeof(Scheme_Config) + 
						 (max_configs - 1) * sizeof(Scheme_Object*));
  
  scheme_set_param(config, MZCONFIG_ENABLE_BREAK, scheme_true);
  scheme_set_param(config, MZCONFIG_ENABLE_EXCEPTION_BREAK, scheme_false);
  scheme_set_param(config, MZCONFIG_CAN_READ_GRAPH, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_COMPILED, scheme_false);
  scheme_set_param(config, MZCONFIG_CAN_READ_BOX, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE, scheme_true);

  scheme_set_param(config, MZCONFIG_PRINT_GRAPH, scheme_false);
  scheme_set_param(config, MZCONFIG_PRINT_STRUCT, scheme_false);
  scheme_set_param(config, MZCONFIG_PRINT_BOX, scheme_true);
  scheme_set_param(config, MZCONFIG_PRINT_VEC_SHORTHAND, scheme_true);

  scheme_set_param(config, MZCONFIG_CASE_SENS, (scheme_case_sensitive ? scheme_true : scheme_false));
  scheme_set_param(config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, (scheme_square_brackets_are_parens
								 ? scheme_true : scheme_false));
  scheme_set_param(config, MZCONFIG_CURLY_BRACES_ARE_PARENS, (scheme_curly_braces_are_parens
							      ? scheme_true : scheme_false));

  scheme_set_param(config, MZCONFIG_ERROR_PRINT_WIDTH, scheme_make_integer(40));

  REGISTER_SO(main_manager);
  main_manager = scheme_make_manager(NULL);
  scheme_set_param(config, MZCONFIG_MANAGER, (Scheme_Object *)main_manager);

  scheme_set_param(config, MZCONFIG_WILL_EXECUTOR, make_will_executor(0, NULL));

  scheme_set_param(config, MZCONFIG_ALLOW_SET_UNDEFINED, (scheme_allow_set_undefined
							  ? scheme_true
							  : scheme_false));
  scheme_set_param(config, MZCONFIG_COND_AUTO_ELSE, (scheme_allow_cond_auto_else
						     ? scheme_true
						     : scheme_false));

  scheme_set_param(config, MZCONFIG_REQUIRE_COLLECTION, scheme_false);

  scheme_set_param(config, MZCONFIG_CURRENT_DIRECTORY, scheme_make_string(scheme_os_getcwd(NULL, 0, NULL, 1)));

  scheme_set_param(config, MZCONFIG_RANDOM_STATE, scheme_make_random_state(scheme_get_milliseconds()));
  
  config->extensions = NULL;

  return config;
}

static Scheme_Object *make_new_config(Scheme_Config *base)
{
  Scheme_Config *config;
  int i;
  
  if (!base)
    base = scheme_config;

  config = (Scheme_Config *)scheme_malloc_tagged(sizeof(Scheme_Config) + 
						 (max_configs - 1) * sizeof(Scheme_Object*));

  config->type = scheme_config_type;
  config->extensions = NULL;
  
  for (i = 0; i < max_configs; i++)
    config->configs[i] = base->configs[i];

  if (base->extensions) {
    Scheme_Bucket **bs = base->extensions->buckets;
    int i = base->extensions->size;
    
    if (!config->extensions)
      config->extensions = scheme_hash_table(2, SCHEME_hash_weak_ptr, 0, 0);
    
    while (i--) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val && b->key && *(void **)b->key)
	scheme_add_to_table(config->extensions, *(const char **)b->key, b->val, 0);
    }
  }

  return (Scheme_Object *)config;
}

Scheme_Object *scheme_make_config(Scheme_Config *base)
{
  return make_new_config(base);
}

Scheme_Object *scheme_register_parameter(Scheme_Prim *function, char *name, int which)
{
  Scheme_Object *o;

  if (!config_map)
    config_map = MALLOC_N(Scheme_Object*, max_configs);

  if (config_map[which])
    return config_map[which];

  o = scheme_make_prim_w_arity(function, name, 0, 1);
  ((Scheme_Primitive_Proc *)o)->flags |= SCHEME_PRIM_IS_PARAMETER;

  config_map[which] = o;

  return o;
}

typedef Scheme_Object *(*PCheck_Proc)(int, Scheme_Object **, Scheme_Config *);

Scheme_Object *scheme_param_config(char *name, long pos,
				   int argc, Scheme_Object **argv,
				   int arity,
				   /* -3 => like -1, plus use check to unmarshall the value
                                      -2 => user parameter; pos is (cons key defval)
				      -1 => use check; if isboolorfilter, check is a filter
                                            (and expected is ignored)
				      0+ => check argument for this arity */
				   Scheme_Object *(*check)(int, Scheme_Object **), 
				   /* Actually called with (int, S_O **, Scheme_Config *) */
				   char *expected,
				   int isboolorfilter)
{
  int set = (argc == 1);
  Scheme_Config *config = scheme_config;

  if (!set) {
    if (arity == -2) {
      Scheme_Object *defval = SCHEME_CDR((Scheme_Object *)pos);
      if (config->extensions) {
	const char *key = (const char *)SCHEME_CAR((Scheme_Object *)pos);
	Scheme_Bucket *b;

	b = scheme_bucket_or_null_from_table(config->extensions, key, 0);
	if (b)
	  return (Scheme_Object *)b->val;
	else
	  return defval;
      }
      return defval;
    } else {
      Scheme_Object *s = scheme_get_param(config, pos);
      if (arity == -3) {
	Scheme_Object *a[1];
	a[0] = s;
	s = ((PCheck_Proc)check)(1, a, config);
      }
      return s;
    }
  } else {
    Scheme_Object *naya = argv[0];

    if (arity != -2) {
      if (arity < 0) {
	if (check) {
	  Scheme_Object *r = ((PCheck_Proc)check)(1, argv, config);
	  
	  if (!isboolorfilter && SCHEME_FALSEP(r))
	    r = NULL;
	  
	  if (!r) {
	    scheme_wrong_type(name, expected, 0, argc, argv);
	    return NULL;
	  }
	  
	  if (isboolorfilter)
	    naya = r;
	}
      } else 
	scheme_check_proc_arity(name, arity, 0, argc, argv);

      if (isboolorfilter && !check)
	scheme_set_param(config, pos, ((SCHEME_TRUEP(naya)) ? scheme_true : scheme_false));
      else
	scheme_set_param(config, pos, naya);
    } else {
      const char *key = (const char *)SCHEME_CAR((Scheme_Object *)pos);
      Scheme_Bucket *b;

      if (!config->extensions)
	config->extensions = scheme_hash_table(2, SCHEME_hash_weak_ptr, 0, 0);

      b = scheme_bucket_from_table(config->extensions, key);
      b->val = naya;
    }
  
    return scheme_void;
  }
}

Scheme_Env *scheme_get_env(Scheme_Config *c)
{
  Scheme_Object *o = scheme_get_param(c, MZCONFIG_ENV);
  return (Scheme_Env *)o;
}

/****************************************/

static void activate_will(void *o, void *data) 
{
  WillRegistration *r = (WillRegistration *)data;
  ActiveWill *a;
  WillExecutor *w;
    
  a = MALLOC_ONE(ActiveWill);
  a->o = (Scheme_Object *)o;
  a->proc = r->proc;
  
  GET_WILL_LOCK();
  w = r->w;
  if (w->last)
    w->last->next = a;
  else
    w->first = a;
  w->last = a;
  RELEASE_WILL_LOCK();
}

static Scheme_Object *now_do_will(void *v)
{
  ActiveWill *a = (ActiveWill *)v;
  Scheme_Object *o[1];

  o[0] = a->o;
  a->o = NULL;

  return scheme_apply_multi(a->proc, 1, o);
}

static void post_will(void *v)
{
  ActiveWill *a = (ActiveWill *)v;

  a->w->running = 0;
}

static Scheme_Object *do_next_will(WillExecutor *w, int tail)
{
  ActiveWill *a;

  GET_WILL_LOCK();
  if (w->running || !w->first) {
    RELEASE_WILL_LOCK();
    return scheme_false;
  }
  a = w->first;
  w->first = a->next;
  if (!w->first)
    w->last = NULL;
  w->running = 1;
  RELEASE_WILL_LOCK();
  
  a->w = w;

  return scheme_dynamic_wind(NULL, now_do_will, post_will, NULL, a);
}

static Scheme_Object *current_will_executor(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-will-executor", MZCONFIG_WILL_EXECUTOR,
			     argc, argv,
			     -1, will_executor_p, "will-executor", 0);
}

static Scheme_Object *make_will_executor(int argc, Scheme_Object **argv)
{
  WillExecutor *w = MALLOC_ONE_TAGGED(WillExecutor);

  w->type = scheme_will_executor_type;
  w->first = NULL;
  w->last = NULL;
  w->running = 0;

  return (Scheme_Object *)w;
}

static Scheme_Object *will_executor_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *register_will(int argc, Scheme_Object **argv)
{
  WillRegistration *r;

  scheme_check_proc_arity("register-will", 1, 1, argc, argv);
  if ((argc > 2) && NOT_SAME_TYPE(SCHEME_TYPE(argv[2]), scheme_will_executor_type))
    scheme_wrong_type("register-will", "will-executor", 2, argc, argv);

  r = MALLOC_ONE(WillRegistration);
  r->proc = argv[1];
  r->w = (WillExecutor *)((argc > 2)
			  ? argv[2]
			  : scheme_get_param(scheme_config, MZCONFIG_WILL_EXECUTOR));

  scheme_add_scheme_finalizer(argv[0], activate_will, (void *)r);

  return scheme_void;
}

static Scheme_Object *will_executor_try(int argc, Scheme_Object **argv)
{
  WillExecutor *w;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_type("will-executor-try", "will-executor", 0, argc, argv);
  
  w = (WillExecutor *)argv[0];

  if (w->first && !w->running)
    return do_next_will(w, 1);
  else
    return scheme_false;
}

/****************************************/

#ifdef MZ_REAL_THREADS

/* Implementing support for new thread systems:

   MzScheme needs implementations for the following functions (usually
   these are macros mapped to actual function names in sconfig.h):

   void* SCHEME_INIT_THREADS(void) - initializes thread system, returns
     the id of the initial (main) thread

   void SCHEME_CREATE_THREAD(void (*f)(void*), void* data, 
     unsigned long *slimit, void** thp) - starts a new thread, applying `f' 
     to `data' in the new thread. BEFORE `f' is applied to `data', the id 
     for the new thread must be in `*thp'. If Unix-style stack-checking is 
     used, the hottest allowable stack address (with the safely margin 
     already removed) must be put into `slimit' before `f' is applied; 
     `slimit' may be NULL, in which case the stack limit is not needed.

   void SCHEME_EXIT_THREAD() - exits the current thread

   void SCHEME_BREAK_THREAD(void* th) - signals a break in the thread `th'.
     If `th' is waiting on a semaphore or selecting a file descriptor, this
     breaking signal must cause that wait or select to break.

   void SCHEME_SET_CURRENT_PROCESS(Scheme_Process* p) - stores `p' as the
     Scheme thread pointer for the current thread.

   Scheme_Process* SCHEME_GET_CURRENT_PROCESS() - retrieves the Scheme thread
     pointer for the current thread.

   void* SCHEME_MAKE_MUTEX() - creates a new mutex.

   void SCHEME_FREE_MUTEX(void* m) - destroys the mutex `m'.

   void SCHEME_LOCK_MUTEX(void* m) - locks the mutex `m', blocking until
     it is available.

   void SCHEME_UNLOCK_MUTEX(void* m) - unlocks the mutex `m'.

   int SCHEME_SEMA_DOWN_BREAKABLE(void* s) - waits on `s' but allows the
     wait to be terminated by a break, returing 1 if the wait was sucessful
     (i.e., no break occurred) or 0 if unsuccessful.

   void* SCHEME_MAKE_SEMA(int init) - creates a new semaphore with initial
     count `init'.

   void SCHEME_FREE_SEMA(void* s) - destroys the semaphore `s'.

   int SCHEME_SEMA_UP(void* s) - posts to the semaphore `s', returning 0 if
     an error occurred.

   int SCHEME_SEMA_DOWN_BREAKABLE(void* s) - waits on `s' but allows the
     wait to be terminated by a break, returing 1 if the wait was sucessful
     (i.e., no break occurred) or 0 if unsuccessful.

   int SCHEME_SEMA_TRY_DOWN(void* s) - attempts a non-blocking wait on the
     semaphore `s', immediately returning 1 if the wait was successful or
     0 if unsuccessful.
*/

void scheme_real_sema_down(void *sema)
{
  if (!SCHEME_SEMA_DOWN_BREAKABLE(sema)) {
    Scheme_Process *p = scheme_current_process;
    do {
      scheme_process_block_w_process(0, p);
    } while (!SCHEME_SEMA_DOWN_BREAKABLE(sema));
  }
}

#endif

/****************************************/

#ifdef MZ_USE_PTHREADS

static pthread_key_t cp_key;
typedef struct {
  void (*f)(void *);
  void *data;
  void *stack;
  long *stackend;
  sem_t go_sema;
  sem_t stackset_sema;
} pthread_closure;

void *scheme_pthread_init_threads(void)
{
  if (pthread_key_create(&cp_key, NULL)) {
    printf("init failed\n");
    exit(-1);
  }

  return (void *)pthread_self();
}

static void do_nothing(int ignored)
{
# ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGINT, do_nothing);
# endif

# ifdef ____MZ_USE_LINUX_PTHREADS
  {
    Scheme_Process *p;
    p = scheme_current_process;
    if (p->jump_on_signal) {
      p->jump_on_signal = 0;
      scheme_longjmp(p->signal_buf, 1);
    }
  }
#endif
}

static void *start_pthread_thread(void *_cl)
{
  pthread_closure *cl = (pthread_closure *)_cl;

  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

  sem_wait(&cl->go_sema);
  sem_destroy(&cl->go_sema);

  MZ_SIGSET(SIGINT, do_nothing);

#ifdef MZ_USE_LINUX_PTHREADS
  {
    /* LinuxThreads: thread stacks are allocated on 2M boundaries and grow
       to no more than 2M. */
    int dummy;
    unsigned long loc;

    loc = ((unsigned long)&dummy);
    loc -= (loc & 0x1FFFFF);
    *(cl->stackend) = loc + STACK_SAFETY_MARGIN;
  }
#else
  >>> Need something more specific than ``pthreads'' <<<
#endif

  sem_post(&cl->stackset_sema);

  cl->f(((pthread_closure *)cl)->data);

  return NULL;
}

#ifdef MZ_KEEP_LOCK_INFO
extern int scheme_fin_lock_c;
extern int scheme_port_lock_c;
static int sem_wait_c;
static int made_printer = 0;
static void print_lock_info(void *ignored)
{
  {
    long start = scheme_get_milliseconds();
    
    do {
      sleep(5);
    } while ((scheme_get_milliseconds() - start) < 30000);
  }

  while (1) {
    Scheme_Process *p;
    int c = 0;
    long start = scheme_get_milliseconds();
    
    do {
      sleep(5);
    } while ((scheme_get_milliseconds() - start) < 5000);
    
    for (p = scheme_first_process; p; p = p->next)
      c++;
    
    printf("gl: %d cl: %d wl: %d fnl: %d pl: %d sw: %d  tc: %d fpb: %d\n", 
	   scheme_global_lock_c,
	   cust_lock_c, will_lock_c, scheme_fin_lock_c,
	   scheme_port_lock_c, sem_wait_c, c,
	   scheme_first_process->block_descriptor);
    for (p = scheme_first_process; p; p = p->next)
      printf("[%d] ", p->block_descriptor);
    printf("\n");
  }
}
#endif

void scheme_pthread_create_thread(void (*f)(void *), void *data, 
				  unsigned long *stackend, void **thp)
{
  pthread_t naya;
  pthread_closure *cl = scheme_malloc(sizeof(pthread_closure));

  cl->f = f;
  cl->data = data;
  cl->stackend = stackend;

  sem_init(&cl->go_sema, 0, 0);
  sem_init(&cl->stackset_sema, 0, 0);

  /* weird trick: as long as the thread's stack is needed, cl will be
     on it, so let GC decide whether the stack is active and therefore
     whether "stack" is still active. */

  pthread_create(&naya, NULL, start_pthread_thread, (void *)cl);

  pthread_detach(naya);

  *thp = (void *)naya;

  sem_post(&cl->go_sema);

  sem_wait(&cl->stackset_sema);
  sem_destroy(&cl->stackset_sema);

#ifdef MZ_KEEP_LOCK_INFO
  if (!made_printer) {
    void *th;
    unsigned long e;
    made_printer = 1;
    scheme_pthread_create_thread(print_lock_info, NULL, &e, &th);
  }
#endif

}

void scheme_pthread_exit_thread()
{
  pthread_exit(0);
}

void scheme_pthread_break_thread(void *th)
{
  pthread_kill((pthread_t)th, SIGINT);
}

Scheme_Process *scheme_pthread_get_current_process()
{
  return (Scheme_Process *)pthread_getspecific(cp_key);
}

void scheme_pthread_set_current_process(Scheme_Process *p)
{
  pthread_setspecific(cp_key, (void *)p);
}

void *scheme_pthread_make_mutex()
{
  pthread_mutex_t *m;
  m = (pthread_mutex_t *)scheme_malloc_atomic(sizeof(pthread_mutex_t));
  pthread_mutex_init(m, NULL);

  return (void *)m;
}

void scheme_pthread_free_mutex(void *m)
{
  pthread_mutex_destroy((pthread_mutex_t *)m);
}

void scheme_pthread_lock_mutex(void *m)
{
  pthread_mutex_lock((pthread_mutex_t *)m);
}

void scheme_pthread_unlock_mutex(void *m)
{
  pthread_mutex_unlock((pthread_mutex_t *)m);
}

void *scheme_pthread_make_semaphore(int v)
{
  sem_t *s;
  s = (sem_t *)scheme_malloc_atomic(sizeof(sem_t));
  sem_init(s, 0, v);

  return (void *)s;
}

void scheme_pthread_free_semaphore(void *s)
{
  sem_destroy((sem_t *)s);
}

int scheme_pthread_semaphore_up(void *s)
{
  return !sem_post((sem_t *)s);
}

int scheme_pthread_semaphore_down_breakable(void *s)
{
  int v;

#ifdef MZ_KEEP_LOCK_INFO
  SCHEME_GET_LOCK();
  sem_wait_c++;
  SCHEME_RELEASE_LOCK();
#endif

#ifdef ___MZ_USE_LINUX_PTHREADS
  {
    Scheme_Process *p = scheme_current_process;

    if (!scheme_setjmp(p->signal_buf)) {
      p->jump_on_signal = 1;
#endif

      
      v = !sem_wait((sem_t *)s);


#ifdef ___MZ_USE_LINUX_PTHREADS
      p->jump_on_signal = 0;
    } else {
      /* Somehow, the post was consumed, anyway; restore it. */
      sem_post((sem_t *)s); 
      v = 0;
    }
  }
#endif

#ifdef MZ_KEEP_LOCK_INFO
  SCHEME_GET_LOCK();
  --sem_wait_c;
  SCHEME_RELEASE_LOCK();
#endif

  return v;
}

int scheme_pthread_semaphore_try_down(void *s)
{
  return !sem_trywait((sem_t *)s);
}

#endif /* MZ_USE_PTHREADS */

/****************************************************************/

#ifdef MZ_USE_SOLARIS_THREADS

static thread_key_t cp_key;
typedef struct {
  void (*f)(void *);
  void *data;
  void *stack;
} solaris_closure;

void *scheme_solaris_init_threads(void)
{
  if (thr_keycreate(&cp_key, NULL)) {
    printf("init failed\n");
    exit(-1);
  }

  return (void *)thr_self();
}

static void do_nothing(int ignored)
{
}

static void *start_solaris_thread(void *cl)
{
  sigset(SIGINT, do_nothing);

  ((solaris_closure *)cl)->f(((solaris_closure *)cl)->data);

  return NULL;
}

void scheme_solaris_create_thread(void (*f)(void *), void *data, 
				  unsigned long *stackend, void **thp)
{
  void *stack;
  size_t size;
  thread_t naya;
  solaris_closure *cl = scheme_malloc(sizeof(solaris_closure));

  cl->f = f;
  cl->data = data;

  if (stackend) {
    size = 2 * STACK_SAFETY_MARGIN;
    if (thr_min_stack() > size)
      size = thr_min_stack();
    stack = scheme_malloc_atomic(size);
  
    *stackend = ((unsigned long)stack) + STACK_SAFETY_MARGIN;
  } else {
    stack = NULL;
    size = 0;
  }

  cl->stack = stack; /* weird trick: as long as the stack is needed,
			cl will be on it, so let GC decide whether 
			the stack is active and therefore whether
			"stack" is still active. */

  thr_create(stack, size,
	     start_solaris_thread, (void *)cl, 
	     THR_SUSPENDED | THR_DETACHED, &naya);

  *thp = (void *)naya;
  
  thr_continue(naya);
}

void scheme_solaris_exit_thread()
{
  thr_exit(0);
}

void scheme_solaris_break_thread(void *th)
{
  thr_kill((thread_t)th, SIGINT);
}

Scheme_Process *scheme_solaris_get_current_process()
{
  Scheme_Process *p;

  thr_getspecific(cp_key, (void **)&p);

  return p;
}

void scheme_solaris_set_current_process(Scheme_Process *p)
{
  thr_setspecific(cp_key, (void *)p);
}

void *scheme_solaris_make_mutex()
{
  mutex_t *m;
  m = (mutex_t *)scheme_malloc_atomic(sizeof(mutex_t));
  mutex_init(m, USYNC_THREAD, NULL);

  return (void *)m;
}

void scheme_solaris_free_mutex(void *m)
{
  mutex_destroy(m);
}

void scheme_solaris_lock_mutex(void *m)
{
  mutex_lock(m);
}

void scheme_solaris_unlock_mutex(void *m)
{
  mutex_unlock(m);
}

void *scheme_solaris_make_semaphore(int v)
{
  sema_t *s;
  s = (sema_t *)scheme_malloc_atomic(sizeof(sema_t));
  sema_init(s, v, USYNC_THREAD, NULL);

  return (void *)s;
}

void scheme_solaris_free_semaphore(void *s)
{
  sema_destroy(s);
}

int scheme_solaris_semaphore_up(void *s)
{
  return !sema_post((sema_t *)s);
}

int scheme_solaris_semaphore_down_breakable(void *s)
{
  return !sema_wait((sema_t *)s);
}

int scheme_solaris_semaphore_try_down(void *s)
{
  return !sema_trywait((sema_t *)s);
}

#endif /* MZ_USE_SOLARIS_THREADS */

/****************************************************************/

#ifdef MZ_USE_WIN32_THREADS

typedef struct {
  long th;
  HANDLE break_sema;
} Win32SchemeThread;

typedef struct {
  void (*f)(void *);
  void *data;
  void *stack;
  HANDLE stack_set;
  HANDLE thread_go;
} thread_closure;

static DWORD tls;

void *scheme_win32_init_threads(void)
{
  Win32SchemeThread *th = MALLOC_ONE_ATOMIC(Win32SchemeThread);

  tls = TlsAlloc();

  th->th = 0;
  th->break_sema = NULL; /* use scheme_break_semaphore */

  return th;
}

static unsigned __stdcall run_win32_thread(void *data)
{
  long dummy;
  thread_closure *cl = (thread_closure *)data;

  cl->stack = (void *)&dummy;
  
# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_ATTACH, NULL);
# endif

  ReleaseSemaphore(cl->stack_set, 1, NULL);
  WaitForSingleObject(cl->thread_go, INFINITE);

  cl->f(cl->data);

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_DETACH, NULL);
# endif

  return 0;
}

static void free_win32_break_sema(void *th, void *ignored)
{
  CloseHandle(((Win32SchemeThread *)th)->break_sema);
}

void scheme_win32_create_thread(void (*f)(void *), void *data, 
				unsigned long *stackend, void **thp)
{
  Win32SchemeThread *th = MALLOC_ONE_ATOMIC(Win32SchemeThread);
  thread_closure *cl = MALLOC_ONE(thread_closure);
  unsigned id;

  cl->f = f;
  cl->data = data;

  th->break_sema = CreateSemaphore(NULL, 0, 1, NULL);
  cl->thread_go = th->break_sema;
  cl->stack_set = stackend ? CreateSemaphore(NULL, 0, 1, NULL) : NULL;

  th->th = _beginthreadex(NULL, 0, run_win32_thread, cl, 0, &id);

  *thp = (void *)th;

  if (stackend) {
    WaitForSingleObject(cl->stack_set, INFINITE);

# ifdef WINDOWS_FIND_STACK_BOUNDS
#  ifndef STACK_SAFETY_MARGIN
#   define STACK_SAFETY_MARGIN 50000
#  endif
    *stackend = ((unsigned long)cl->stack + (STACK_SAFETY_MARGIN - 0x100000));
# else
    >> not implemented <<
# endif

    CloseHandle(cl->stack_set);
  }

  ReleaseSemaphore(cl->thread_go, 1, NULL);

  ResumeThread((HANDLE)th->th);

  scheme_add_finalizer(th, free_win32_break_sema, NULL);
}

void scheme_win32_exit_thread()
{
# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_DETACH, NULL);
# endif

  _endthreadex(0);
}

void *scheme_win32_get_break_semaphore(void *th)
{
  HANDLE s;

  s = ((Win32SchemeThread *)th)->break_sema;
  if (!s)
    s = scheme_break_semaphore;

  return (void *)s;
}

void scheme_win32_break_thread(void *th)
{
  HANDLE s = (HANDLE)scheme_win32_get_break_semaphore(th);

  ReleaseSemaphore(s, 1, NULL);
}

struct Scheme_Process *scheme_win32_get_current_process()
{
  return (Scheme_Process *)TlsGetValue(tls);
}

void scheme_win32_set_current_process(struct Scheme_Process *p)
{
  TlsSetValue(tls, (LPVOID)p);
}

void *scheme_win32_make_mutex()
{
  CRITICAL_SECTION *m;
  m = (CRITICAL_SECTION *)scheme_malloc_atomic(sizeof(CRITICAL_SECTION));
  InitializeCriticalSection(m);

  return (void *)m;
}

void scheme_win32_free_mutex(void *m)
{
  DeleteCriticalSection((CRITICAL_SECTION *)m);
}

void scheme_win32_lock_mutex(void *m)
{
  EnterCriticalSection((CRITICAL_SECTION *)m);
}

void scheme_win32_unlock_mutex(void *m)
{
  LeaveCriticalSection((CRITICAL_SECTION *)m);
}

void *scheme_win32_make_semaphore(int init)
{
  return (void *)CreateSemaphore(NULL, init, 256, NULL);
}

void scheme_win32_free_semaphore(void *s)
{
  CloseHandle((HANDLE)s);
}

int scheme_win32_semaphore_up(void *s)
{
  return ReleaseSemaphore((HANDLE)s, 1, NULL);
}

int scheme_win32_semaphore_down_breakable(void *s)
{
  int c;
  HANDLE a[2];

  a[0] = (HANDLE)s;
  a[1] = scheme_win32_get_break_semaphore((Win32SchemeThread *)scheme_current_process->thread);

  c = !a[1] ? 1 : 2;

  return WaitForMultipleObjects(c, a, FALSE, INFINITE) == WAIT_OBJECT_0;
}

int scheme_win32_semaphore_try_down(void *s)
{
  return !WaitForSingleObject((HANDLE)s, 0);
}

#endif /* MZ_USE_WIN32_THREADS */

/****************************************************************/

#ifdef MZ_USE_IRIX_SPROCS

/* Irix sprocs implementation contributed by Fernando D. Mato Mira */
/* 
   NOTE TO IMPLEMENTORS:
   IRIX semaphores are awfully slow to awake a process. 
   It's better to use ulocks or abilocks, if a count is not really needed.
   There're are macros in semaphores.h that homogeneize the interfaces
   for locking, eg:

   SLOW:   slock --> usema_t
   MEDIUM: ulock --> ulock_t   (good response)
   QUICK:  qlock --> abilock_t (eats a bit of CPU time, 'cause it spins/nanosleeps)

   --fdmm
*/

#include <ulocks.h>

static thread_key_t cp_key;
typedef struct {
  void (*f)(void *);
  void *data;
#ifdef MZ_PRIVATE_SPAWN_MUTEX
  mutex_t *mutex;
#endif
  void *stack;
} sproc_closure;

void *scheme_sproc_init_threads(void)
{
  if (thr_keycreate(&cp_key, NULL)) {
    printf("init failed\n");
    exit(-1);
  }

  return (void *)getpid(); /* retrurn the main thread (= current thread) */
}

static void do_nothing(int ignored)
{
}

static void start_sproc_thread(void *cl, size_t ignored)
{
  sproc_closure *scl = (sproc_closure *)cl;

  sigset(SIGINT, do_nothing);

  /* wait until `*stackend' and `*thp' have been set. */
#ifdef MZ_PRIVATE_SPAWN_MUTEX
  qlock_1(scl->mutex, "scheme_sproc_mutex_down");
  freemutex(scl->mutex);
#else
  /* (parent does not release the allocation lock */
  /*  until that is done) */
  sproc_lock();
  sproc_unlock();
#endif

  scl->f(scl->data);
}

void scheme_sproc_create_thread(void (*f)(void *), void *data, 
				unsigned long *stackend, void **thp)
{
  int naya;
  void *stack;
  size_t size;
  sproc_closure *cl = scheme_malloc(sizeof(sproc_closure));

  cl->f = f;
  cl->data = data;

  if (stackend) {
    size = 2 * STACK_SAFETY_MARGIN;
    stack = scheme_malloc(size); /*scheme_malloc_atomic(size);*/
    
    *stackend = ((unsigned long)stack) + STACK_SAFETY_MARGIN;
  } else {
    stack = NULL;
    size = 0;
  }

  cl->stack = stack; /* weird trick: as long as the stack is needed,
			cl will be on it, so let GC decide whether 
			the stack is active and therefore whether
			"stack" is still active. */
#ifdef MZ_PRIVATE_SPAWN_MUTEX
#define SPROCSP sprocsp
  cl->mutex = newmutex();
#else
#define SPROCSP GC_sprocsp_
  /* I'll deadlock if I call GC-safe unblockproc() before   */
  /* GC-safe sproc()  completes its housekeeping.           */
  /* Worse, because of sproc_lock() a process that tries to */
  /* GC-safely blockproc() before it gets completely        */
  /* unblockproc'ed will also cause a deadlock.             */

  /* Solution: we call sprocsp_(), which is GC_safe, but   */
  /*   does not release the allocation lock.               */
  /*   We could use _GC_unblockproc() then, but it's less  */
  /*   redundant to just wait for this lock in the child   */ 
  /*   Hopefully, this will not cause much more contention */
  /*   with other threads than if we used a separate lock  */
  /*   just for this.      */
#endif

  naya = SPROCSP (start_sproc_thread, PR_SALL, (void *)cl, 
		 (caddr_t)(((char *)stack) + size), 0);

  *thp = (void *)naya;

  /* let thread run, now */
#ifdef MZ_PRIVATE_SPAWN_MUTEX
  qunlock_1(cl->mutex, "scheme_sproc_mutex_up");
#else
  sproc_unlock();
#endif

}

void scheme_sproc_exit_thread()
{
  exit(0);
}

void scheme_sproc_break_thread(void *th)
{
  kill((int)th, SIGINT);
}

Scheme_Process *scheme_sproc_get_current_process()
{
  Scheme_Process *p;

  thr_getspecific(cp_key, (void **)&p);

  return p;
}

void scheme_sproc_set_current_process(Scheme_Process *p)
{
  thr_setspecific(cp_key, (void *)p);
}

void *scheme_sproc_make_semaphore(int v)
{
  usema_t *s = usnewsema(semArena, v);

  return (void *)s;
}

void scheme_sproc_free_semaphore(void *s)
{
  usfreesema(s, semArena);
}

#  ifdef MZ_TASKS
--> Semaphore/lock routines should be revised
#  endif

int scheme_sproc_semaphore_up(void *s)
{

  return !seminc((usema_t *)s,"scheme_sproc_semaphore_up");
}

int scheme_sproc_semaphore_down_breakable(void *s)
{
  return (semdec((sema_t *)s,"scheme_sproc_semaphore_down") == 1);
}

int scheme_sproc_semaphore_try_down(void *s)
{
  return (uscpsema((usema_t *)s) == 1);
}

void *scheme_sproc_make_mutex()
{
  mutex_t *s = usnewlock(semArena);

  return (void *)s;
}

void scheme_sproc_free_mutex(void *s)
{
  usfreelock(s, semArena);
}

int scheme_sproc_mutex_up(void *s)
{

  return (!uunlock((mutex_t *)s));
}

int scheme_sproc_mutex_down_breakable(void *s)
{
  return (ulock((mutex_t *)s));
}

int scheme_sproc_mutex_try_down(void *s)
{
  return (utrylock((mutex_t *)s));
}

#endif /* MZ_USE_IRIX_SPROCS */

