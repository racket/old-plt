/*
  MzScheme
  Copyright (c) 1995-2001 Matthew Flatt
 
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

/* This file implements MzScheme threads.

   Usually, MzScheme threads are implemented by copying the stack.
   The scheme_thread_block() function is called occassionally by the
   evaluator so that the current thread can be swapped out.
   scheme_swap_thread() performs the actual swap. Threads can also be
   implemented by the OS; the bottom part of this file contains
   OS-specific thread code.

   Much of the work in thread management is knowning when to go to
   sleep, to be nice to the OS outside of MzScheme. The rest of the
   work is implementing custodians (called "custodians" in the code),
   parameters, and wills. */

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#define SCHEME_NO_GC_PROTO

/* Irix SPROCS needs to load some files first, so find out if we're SPROCS. */
# include "../sconfig.h"

#ifdef MZ_USE_IRIX_SPROCS
/* Don't include anything else before this */
# include "../gc/gc.h"
# include "../gc/semaphores.h"
# include "../gc/sproc.h"
#endif

#include "schpriv.h"
#include "schmach.h"
#include "schgc.h"
#ifndef PALMOS_STUFF
# include <time.h>
#endif
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
#ifdef USE_ITIMER
# include <sys/types.h>
# include <sys/time.h>
# include <signal.h>
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

#ifndef SIGNMZTHREAD
# define SIGMZTHREAD SIGUSR2
#endif

#if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
# include <windows.h>
extern HANDLE scheme_break_semaphore;
#endif

#if defined(FILES_HAVE_FDS) \
     || defined(USE_BEOS_PORT_THREADS) \
     || (defined(USE_WINSOCK_TCP) && defined(USE_TCP)) \
     || (defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES))
# define USING_FDS
# if (!defined(USE_WINSOCK_TCP) || !defined(USE_TCP)) && !defined(FILES_HAVE_FDS)
#  include <sys/types.h>
# endif
#endif

#include "schfd.h"

#define INIT_SCHEME_STACK_SIZE 1000

#ifdef SGC_STD_DEBUGGING
# define SENORA_GC_NO_FREE
#endif

/* If a finalization callback in MrEd invokes Scheme code,
   we can end up with a thread swap in the middle of a thread
   swap (where the outer swap was interrupted by GC). The
   following is a debugging flag to help detect and fix
   such problems. */
#define WATCH_FOR_NESTED_SWAPS 0

#if WATCH_FOR_NESTED_SWAPS
static int swapping = 0;
#endif

/*========================================================================*/
/*                    local variables and proptypes                       */
/*========================================================================*/

#define INIT_TB_SIZE  20

#ifndef MZ_THREAD_QUANTUM_USEC
# define MZ_THREAD_QUANTUM_USEC 10000
#endif

static int buffer_init_size = INIT_TB_SIZE;

Scheme_Thread *scheme_current_thread = NULL;
Scheme_Thread *scheme_main_thread = NULL;
Scheme_Thread *scheme_first_thread = NULL;
#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Thread **scheme_current_thread_ptr;
volatile int *scheme_fuel_counter_ptr;
#endif
static int swap_no_setjmp = 0;

#ifdef RUNSTACK_IS_GLOBAL
Scheme_Object **scheme_current_runstack_start;
Scheme_Object **scheme_current_runstack;
MZ_MARK_STACK_TYPE scheme_current_cont_mark_stack;
MZ_MARK_POS_TYPE scheme_current_cont_mark_pos;
#endif

static Scheme_Custodian *main_custodian;

static Scheme_Object *scheduled_kills;

long scheme_total_gc_time;
static long start_this_gc_time;
extern MZ_DLLIMPORT void (*GC_collect_start_callback)(void);
extern MZ_DLLIMPORT void (*GC_collect_end_callback)(void);
static void get_ready_for_GC(void);
static void done_with_GC(void);

static short delay_breaks = 0, delayed_break_ready = 0;

void (*scheme_sleep)(float seconds, void *fds);
void (*scheme_notify_multithread)(int on);
void (*scheme_wakeup_on_input)(void *fds);
int (*scheme_check_for_break)(void);

static int do_atomic = 0;
static int missed_context_switch = 0;
static int have_activity = 0;
int scheme_active_but_sleeping = 0;
static int thread_ended_with_activity;

static int tls_pos = 0;

#ifdef MZ_PRECISE_GC
extern long GC_get_memory_use(void *c);
#else
extern MZ_DLLIMPORT long GC_get_memory_use();
#endif

static Scheme_Object *empty_symbol, *initial_symbol;

static Scheme_Object *read_symbol, *write_symbol, *execute_symbol, *delete_symbol, *exists_symbol;
static Scheme_Object *client_symbol, *server_symbol;

static Scheme_Object *nested_exn_handler;

static Scheme_Object *closers;

static Scheme_Object *thread_swap_callbacks;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static Scheme_Object *custodian_require_mem(int argc, Scheme_Object *args[]);
static Scheme_Object *custodian_limit_mem(int argc, Scheme_Object *args[]);

static Scheme_Object *collect_garbage(int argc, Scheme_Object *args[]);
static Scheme_Object *current_memory_use(int argc, Scheme_Object *args[]);

#ifndef NO_SCHEME_THREADS
static Scheme_Object *sch_thread(int argc, Scheme_Object *args[]);
#endif
static Scheme_Object *sch_sleep(int argc, Scheme_Object *args[]);
#ifndef NO_SCHEME_THREADS
static Scheme_Object *thread_p(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_running_p(int argc, Scheme_Object *args[]);
static Scheme_Object *thread_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_current(int argc, Scheme_Object *args[]);
static Scheme_Object *kill_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *break_thread(int argc, Scheme_Object *args[]);
static void register_thread_wait();
#endif

static Scheme_Object *object_waitable_p(int argc, Scheme_Object *args[]);
static Scheme_Object *object_wait_break(int argc, Scheme_Object *args[]);

static Scheme_Object *make_custodian(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *custodian_close_all(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_custodian(int argc, Scheme_Object *argv[]);
static Scheme_Object *call_as_nested_thread(int argc, Scheme_Object *argv[]);

static Scheme_Object *current_namespace(int argc, Scheme_Object *args[]);
static Scheme_Object *namespace_p(int argc, Scheme_Object *args[]);

static Scheme_Object *parameter_p(int argc, Scheme_Object *args[]);
static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object *args[]);
static Scheme_Object *make_parameter(int argc, Scheme_Object *args[]);

static Scheme_Object *make_security_guard(int argc, Scheme_Object *argv[]);
static Scheme_Object *security_guard_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_security_guard(int argc, Scheme_Object *argv[]);

static void adjust_custodian_family(void *pr, void *ignored);

static Scheme_Object *make_will_executor(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_p(int argc, Scheme_Object *args[]);
static Scheme_Object *register_will(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_try(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_go(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_sema(Scheme_Object *w, int *repost);

static Scheme_Config *make_initial_config(void);
static int do_kill_thread(Scheme_Thread *p);

static int check_sleep(int need_activity, int sleep_now);

static void remove_thread(Scheme_Thread *r);
static void exit_or_escape(Scheme_Thread *p);

static Scheme_Object **config_map;

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *key;
  Scheme_Object *guard;
  Scheme_Object *defval;
} ParamData;

enum {
  CONFIG_DIRECT,
  CONFIG_INDIRECT
};

typedef struct Scheme_Thread_Custodian_Hop {
  Scheme_Type type;
  Scheme_Thread *p; /* really an indirection with precise gc */
} Scheme_Thread_Custodian_Hop;

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *key;
  void (*f)(Scheme_Env *);
} Scheme_NSO;
static int num_nsos = 0;
static Scheme_NSO *namespace_options = NULL;

#define SETJMP(p) scheme_setjmpup(&p->jmpup_buf, p, p->stack_start)
#define LONGJMP(p) scheme_longjmpup(&p->jmpup_buf)
#define RESETJMP(p) scheme_reset_jmpup_buf(&p->jmpup_buf)

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
# define GC_THINKS_ITS_A_DLL_BUT_ISNT

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
# endif
#endif

#ifndef MZ_PRECISE_GC
# define scheme_thread_hop_type scheme_thread_type
#endif

#ifdef MZ_PRECISE_GC
static unsigned long get_current_stack_start(void);
#endif

typedef int (*Block_Check_Procedure)(Scheme_Object *blocker);
typedef void (*Block_Needs_Wakeup_Procedure)(Scheme_Object *blocker, void *fds);

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void scheme_init_thread(Scheme_Env *env)
{
  scheme_add_global_constant("dump-memory-stats",
			     scheme_make_prim_w_arity(scheme_dump_gc_stats,
						      "dump-memory-stats",
						      0, -1), 
			     env);

  scheme_add_global_constant("make-namespace",
			     scheme_make_prim_w_arity(scheme_make_namespace,
						      "make-namespace",
						      0, 1),
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
  scheme_add_global_constant("thread?",
			     scheme_make_folding_prim(thread_p,
						      "thread?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("thread-running?",
			     scheme_make_prim_w_arity(thread_running_p,
						      "thread-running?",
						      1, 1),
			     env);
  scheme_add_global_constant("thread-wait",
			     scheme_make_prim_w_arity(thread_wait,
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

  register_thread_wait();
#endif

  scheme_add_global_constant("make-custodian",
			     scheme_make_prim_w_arity(make_custodian,
						      "make-custodian",
						      0, 1),
			     env);
  scheme_add_global_constant("custodian?",
			     scheme_make_folding_prim(custodian_p,
						      "custodian?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("custodian-shutdown-all",
			     scheme_make_prim_w_arity(custodian_close_all,
						      "custodian-shutdown-all",
						      1, 1),
			     env);
  scheme_add_global_constant("current-custodian", 
			     scheme_register_parameter(current_custodian,
						       "current-custodian",
						       MZCONFIG_CUSTODIAN),
			     env);
  scheme_add_global_constant("call-in-nested-thread",
			     scheme_make_prim_w_arity(call_as_nested_thread,
						      "call-in-nested-thread",
						      1, 2),
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

  scheme_add_global_constant("security-guard?", 
			     scheme_make_prim_w_arity(security_guard_p,
						      "security-guard?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("make-security-guard", 
			     scheme_make_prim_w_arity(make_security_guard,
						      "make-security-guard", 
						      3, 3), 
			     env);
  scheme_add_global_constant("current-security-guard", 
			     scheme_register_parameter(current_security_guard,
						       "current-security-guard",
						       MZCONFIG_SECURITY_GUARD),
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
  scheme_add_global_constant("will-register", 
			     scheme_make_prim_w_arity(register_will,
						      "will-register", 
						      3, 3), 
			     env);
  scheme_add_global_constant("will-try-execute", 
			     scheme_make_prim_w_arity(will_executor_try,
						      "will-try-execute", 
						      1, 1), 
			     env);
  scheme_add_global_constant("will-execute", 
			     scheme_make_prim_w_arity(will_executor_go,
						      "will-execute", 
						      1, 1), 
			     env);
  
  scheme_add_waitable_through_sema(scheme_will_executor_type, will_executor_sema, NULL);


  scheme_add_global_constant("collect-garbage", 
			     scheme_make_prim_w_arity(collect_garbage, 
						      "collect-garbage",
						      0, 0), 
			     env);
  scheme_add_global_constant("current-memory-use", 
			     scheme_make_prim_w_arity(current_memory_use, 
						      "current-memory-use",
						      0, 1),
			     env);

  scheme_add_global_constant("custodian-require-memory",
			     scheme_make_prim_w_arity(custodian_require_mem,
						      "custodian-require-memory",
						      2, 2),
			     env);
  scheme_add_global_constant("custodian-limit-memory",
			     scheme_make_prim_w_arity(custodian_limit_mem,
						      "custodian-limit-memory",
						      3, 3),
			     env);
  

  scheme_add_global_constant("object-waitable?", 
			     scheme_make_folding_prim(object_waitable_p,
						      "object-waitable?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("object-wait-multiple", 
			     scheme_make_prim_w_arity(scheme_object_wait_multiple,
						      "object-wait-multiple", 
						      2, -1), 
			     env);
  scheme_add_global_constant("object-wait-multiple/enable-break", 
			     scheme_make_prim_w_arity(object_wait_break,
						      "object-wait-multiple/enable-break", 
						      2, -1),
			     env);

  REGISTER_SO(namespace_options);

  REGISTER_SO(empty_symbol);
  REGISTER_SO(initial_symbol);
  
  empty_symbol = scheme_intern_symbol("empty");
  initial_symbol = scheme_intern_symbol("initial");
}

static Scheme_Object *collect_garbage(int c, Scheme_Object *p[])
{
  scheme_collect_garbage();

  return scheme_void;
}

static Scheme_Object *current_memory_use(int argc, Scheme_Object *args[])
{
  Scheme_Object *cust = NULL;
  long retval = 0;

  if (argc) {
    if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_custodian_type))
      scheme_wrong_type("current-memory-use", "custodian", 0, argc, args);
    cust = args[0];
  }

#ifdef MZ_PRECISE_GC
  retval = GC_get_memory_use(args[0]);    
#else
  retval = GC_get_memory_use();
#endif
  
  return scheme_make_integer(retval);
}


/*========================================================================*/
/*                              custodians                                */
/*========================================================================*/

static Scheme_Object *custodian_require_mem(int argc, Scheme_Object *args[])
{
  long lim;

  if (SCHEME_INTP(args[0]) && (SCHEME_INT_VAL(args[0]) > 0)) {
    lim = SCHEME_INT_VAL(args[0]);
  } else if (SCHEME_BIGNUMP(args[0]) && SCHEME_BIGPOS(args[0])) {
    lim = 0x3fffffff; /* more memory than we actually have */
  } else {
    scheme_wrong_type("custodian-require-memory", "positive exact integer", 0, argc, args);
    return NULL;
  }

  if(NOT_SAME_TYPE(SCHEME_TYPE(args[1]), scheme_custodian_type)) {
    scheme_wrong_type("custodian-require-memory", "custodian", 1, argc, args);
    return NULL;
  }

#ifdef MZ_PRECISE_GC
  if (GC_set_account_hook(MZACCT_REQUIRE, NULL, lim, args[1]))
    return scheme_void;
#endif

  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "custodian-require-memory: not supported");
  return NULL; /* doesn't get here */
}

static Scheme_Object *custodian_limit_mem(int argc, Scheme_Object *args[])
{
  long lim;
  
  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_custodian_type)) {
    scheme_wrong_type("custodian-limit-memory", "custodian", 0, argc, args);
    return NULL;
  }

  if (SCHEME_INTP(args[1]) && (SCHEME_INT_VAL(args[1]) > 0)) {
    lim = SCHEME_INT_VAL(args[1]);
  } else if (SCHEME_BIGNUMP(args[1]) && SCHEME_BIGPOS(args[1])) {
    lim = 0x3fffffff; /* more memory than we actually have */
  } else {
    scheme_wrong_type("custodian-limit-memory", "positive exact integer", 1, argc, args);
  }

  if(NOT_SAME_TYPE(SCHEME_TYPE(args[2]), scheme_custodian_type)) {
    scheme_wrong_type("custodian-require-memory", "custodian", 2, argc, args);
    return NULL;
  }

#ifdef MZ_PRECISE_GC
  if (GC_set_account_hook(MZACCT_LIMIT, args[0], SCHEME_INT_VAL(args[1]), args[2]))
    return scheme_void;
#endif

  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "custodian-limit-memory: not supported");
  return NULL; /* doesn't get here */
}

static void ensure_custodian_space(Scheme_Custodian *m, int k)
{
  int i;

  if (m->count + k >= m->alloc) {
    Scheme_Object ***naya_boxes;
    Scheme_Custodian_Reference **naya_mrefs;
    Scheme_Close_Custodian_Client **naya_closers;
    void **naya_data;

    m->alloc = (m->alloc ? (2 * m->alloc) : 4);
    if (m->alloc < k)
      m->alloc += k;
    
    naya_boxes = MALLOC_N(Scheme_Object**, m->alloc);
    naya_closers = MALLOC_N(Scheme_Close_Custodian_Client*, m->alloc);
    naya_data = MALLOC_N(void*, m->alloc);
    naya_mrefs = MALLOC_N(Scheme_Custodian_Reference*, m->alloc);

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

static void add_managed_box(Scheme_Custodian *m, 
			    Scheme_Object **box, Scheme_Custodian_Reference *mref,
			    Scheme_Close_Custodian_Client *f, void *data)
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

  ensure_custodian_space(m, 1);

  m->boxes[m->count] = box;
  m->closers[m->count] = f;
  m->data[m->count] = data;
  m->mrefs[m->count] = mref;

  m->count++;
}

#ifdef MZ_PRECISE_GC
/* This is a trick to get the types right. Note that 
   the layout of the weak box is defined by the
   GC spec. */
typedef struct {
  short type;
  short hash_key;
  Scheme_Custodian *val;
} Scheme_Custodian_Weak_Box;

# define MALLOC_MREF() (Scheme_Custodian_Reference *)scheme_make_weak_box(NULL)
# define CUSTODIAN_FAM(x) ((Scheme_Custodian_Weak_Box *)x)->val
# define xCUSTODIAN_FAM(x) SCHEME_BOX_VAL(x)
#else
# define MALLOC_MREF() MALLOC_ONE_WEAK(Scheme_Custodian_Reference)
# define CUSTODIAN_FAM(x) *(x)
# define xCUSTODIAN_FAM(x) *(x)
#endif

static void remove_managed(Scheme_Custodian_Reference *mr, Scheme_Object *o,
			   Scheme_Close_Custodian_Client **old_f, void **old_data)
{
  Scheme_Custodian *m;
  int i;

  m = CUSTODIAN_FAM(mr);
  if (!m) {
    return;
  }

  for (i = m->count; i--; ) {
    if (m->boxes[i] && SAME_OBJ((xCUSTODIAN_FAM(m->boxes[i])),  o)) {
      CUSTODIAN_FAM(m->boxes[i]) = 0;
      m->boxes[i] = NULL;
      CUSTODIAN_FAM(m->mrefs[i]) = 0;
      m->mrefs[i] = NULL;
      if (old_f)
	*old_f = m->closers[i];
      if (old_data)
	*old_data = m->data[i];
      m->data[i] = NULL;
      break;
    }
  }

  while (m->count && !m->boxes[m->count - 1]) {
    --m->count;
  }
}

static void adjust_custodian_family(void *mgr, void *ignored)
{
  /* Threads note: because this function is only called as a
     finalization callback, it is automatically syncronized by the GC
     locks. And it is synchronized against all finalizations, so a
     managee can't try to unregister while we're shuffling its
     custodian. */
  Scheme_Custodian *r = (Scheme_Custodian *)mgr, *parent, *m;
  int i;

  parent = CUSTODIAN_FAM(r->parent);

  if (parent) {
    /* Remove from parent's list of children: */
    if (CUSTODIAN_FAM(parent->children) == r) {
      CUSTODIAN_FAM(parent->children) = CUSTODIAN_FAM(r->sibling);
    } else {
      m = CUSTODIAN_FAM(parent->children);
      while (m && CUSTODIAN_FAM(m->sibling) != r) {
	m = CUSTODIAN_FAM(m->sibling);
      }
      if (m)
	CUSTODIAN_FAM(m->sibling) = CUSTODIAN_FAM(r->sibling);
    }

    /* Add children to parent's list: */
    for (m = CUSTODIAN_FAM(r->children); m; ) {
      Scheme_Custodian *next = CUSTODIAN_FAM(m->sibling);
      
      CUSTODIAN_FAM(m->parent) = parent;
      CUSTODIAN_FAM(m->sibling) = CUSTODIAN_FAM(parent->children);
      CUSTODIAN_FAM(parent->children) = m;

      m = next;
    }

    /* Add remaining managed items to parent: */
    for (i = 0; i < r->count; i++) {
      if (r->boxes[i]) {
	CUSTODIAN_FAM(r->mrefs[i]) = parent;
	add_managed_box(parent, r->boxes[i], r->mrefs[i], r->closers[i], r->data[i]);
      }
    }
  }

  CUSTODIAN_FAM(r->parent) = NULL;
  CUSTODIAN_FAM(r->sibling) = NULL;
  CUSTODIAN_FAM(r->children) = NULL;
}

Scheme_Custodian *scheme_make_custodian(Scheme_Custodian *parent) 
{
  Scheme_Custodian *m;
  Scheme_Custodian_Reference *mw;

  m = MALLOC_ONE_TAGGED(Scheme_Custodian);

  m->type = scheme_custodian_type;

  m->alloc = m->count = 0;

  mw = MALLOC_MREF();
  m->parent = mw;
  mw = MALLOC_MREF();
  m->children = mw;
  mw = MALLOC_MREF();
  m->sibling = mw;

  CUSTODIAN_FAM(m->children) = NULL;
  CUSTODIAN_FAM(m->sibling) = NULL;

  CUSTODIAN_FAM(m->parent) = parent;
  if (parent) {
    CUSTODIAN_FAM(m->sibling) = CUSTODIAN_FAM(parent->children);
    CUSTODIAN_FAM(parent->children) = m;
  } else
    CUSTODIAN_FAM(m->sibling) = NULL;

  scheme_add_finalizer(m, adjust_custodian_family, NULL);

  return m;
}

static void rebox_willdone_object(void *o, void *mr)
{
  Scheme_Custodian *m = CUSTODIAN_FAM((Scheme_Custodian_Reference *)mr);
  Scheme_Close_Custodian_Client *f;
  void *data;

  /* Still needs management? */
  if (m) {
#ifdef MZ_PRECISE_GC
    Scheme_Object *b;
#else
    Scheme_Object **b;
#endif

    remove_managed(mr, o, &f, &data);

#ifdef MZ_PRECISE_GC
    b = scheme_box(NULL);
#else
    b = MALLOC_ONE(Scheme_Object*); /* not atomic this time */
#endif
    xCUSTODIAN_FAM(b) = o;
    
    /* Put the custodian back: */
    CUSTODIAN_FAM((Scheme_Custodian_Reference *)mr) = m;

    add_managed_box(m, (Scheme_Object **)b, (Scheme_Custodian_Reference *)mr, f, data);
  }
}

static void managed_object_gone(void *o, void *mr)
{
  Scheme_Custodian *m = CUSTODIAN_FAM((Scheme_Custodian_Reference *)mr);

  /* Still has management? */
  if (m)
    remove_managed(mr, o, NULL, NULL);
}


Scheme_Custodian_Reference *scheme_add_managed(Scheme_Custodian *m, Scheme_Object *o, 
					     Scheme_Close_Custodian_Client *f, void *data, int must_close)
{
#ifdef MZ_PRECISE_GC
    Scheme_Object *b;
#else
    Scheme_Object **b;
#endif
  Scheme_Custodian_Reference *mr;

#ifdef MZ_PRECISE_GC
  b = scheme_make_weak_box(NULL);
#else
  b = MALLOC_ONE_WEAK(Scheme_Object*);
#endif
  xCUSTODIAN_FAM(b) = o;

  mr = MALLOC_MREF();

  if (!m)
    m = (Scheme_Custodian *)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);

  CUSTODIAN_FAM(mr) = m;

  /* The atomic link via the box `b' allows the execution of wills for
     o. After this, we should either drop the object or we have to
     hold on to the object strongly (for when custodian-close-all is
     called). */
  if (must_close)
    scheme_add_finalizer(o, rebox_willdone_object, mr);
  else
    scheme_add_finalizer(o, managed_object_gone, mr);

  add_managed_box(m, (Scheme_Object **)b, mr, f, data);

  return mr;
}

void scheme_remove_managed(Scheme_Custodian_Reference *mr, Scheme_Object *o)
{
  remove_managed(mr, o, NULL, NULL);
}

Scheme_Thread *scheme_do_close_managed(Scheme_Custodian *m, Scheme_Exit_Closer_Func cf)
{
  Scheme_Thread *kill_self = NULL, *ks;
  Scheme_Custodian *c, *next;
  int cx = 0;

  if (!m)
    m = main_custodian;

  /* Kill children first. */
  /* FIXME: we might overflow the stack here! */
  for (c = CUSTODIAN_FAM(m->children); c; c = next) {
    next = CUSTODIAN_FAM(c->sibling);
    ks = scheme_do_close_managed(c, cf);
    if (ks)
      kill_self = ks;
  }

  while (m->count) {
    int i = m->count - 1;
    if (m->boxes[i]) {
      Scheme_Object *o;
      Scheme_Close_Custodian_Client *f;
      void *data;

      o = xCUSTODIAN_FAM(m->boxes[i]);

      f = m->closers[i];
      data = m->data[i];
      CUSTODIAN_FAM(m->boxes[i]) = NULL;
      m->boxes[i] = NULL;
      CUSTODIAN_FAM(m->mrefs[i]) = NULL;
      m->mrefs[i] = NULL;
      m->data[i] = NULL;
      --m->count;

      if (cf) {
	cf(o, f, data);
      } else {
	if (SAME_TYPE(SCHEME_TYPE(o), scheme_thread_hop_type)) {
#ifndef NO_SCHEME_THREADS
	  /* We've added an indirection and made it weak. See mr_hop note above. */
	  Scheme_Thread *p = (Scheme_Thread *)WEAKIFIED(((Scheme_Thread_Custodian_Hop *)o)->p);
	  
	  if (p)
	    if (do_kill_thread(p))
	      kill_self = p;
#endif
	} else {
	  cx++;
	  f(o, data);
	}
      }
    } else {
      --m->count;
    }
  }

  return kill_self;
}

typedef void (*Scheme_For_Each_Func)(Scheme_Object *);

static void for_each_managed(Scheme_Type type, Scheme_Custodian *m, Scheme_For_Each_Func cf)
     /* This function must not allocate. */
{
  Scheme_Custodian *c, *next;
  int i;

  if (SAME_TYPE(type, scheme_thread_type))
    type = scheme_thread_hop_type;
  
  /* Children first. */
  /* FIXME: we might overflow the stack here! */
  for (c = CUSTODIAN_FAM(m->children); c; c = next) {
    next = CUSTODIAN_FAM(c->sibling);
    for_each_managed(type, c, cf);
  }

  for (i = m->count; i--; ) {
    if (m->boxes[i]) {
      Scheme_Object *o;

      o = xCUSTODIAN_FAM(m->boxes[i]);
      
      if (SAME_TYPE(SCHEME_TYPE(o), type)) {
	if (SAME_TYPE(type, scheme_thread_hop_type)) {
	  /* We've added an indirection and made it weak. See mr_hop note above. */
	  o = (Scheme_Object *)WEAKIFIED(((Scheme_Thread_Custodian_Hop *)o)->p);
	}

	cf(o);
      }
    }
  }
}

void scheme_close_managed(Scheme_Custodian *m)
/* The trick is that we may need to kill the thread
   that is running us. If so, delay it to the very
   end. */
{
#ifndef NO_SCHEME_THREADS
  if (scheme_do_close_managed(m, NULL)) {
    /* Kill self */
    scheme_thread_block(0.0);
  }

  /* Give killed threads time to die: */
  scheme_thread_block(0);
  scheme_current_thread->ran_some = 1;
#endif
}

static Scheme_Object *make_custodian(int argc, Scheme_Object *argv[])
{
  Scheme_Custodian *m;

  if (argc) {
    if (!SCHEME_CUSTODIANP(argv[0]))
      scheme_wrong_type("make-custodian", "custodian", 0, argc, argv);
    m = (Scheme_Custodian *)argv[0];
  } else
    m = (Scheme_Custodian *)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);

  return (Scheme_Object *)scheme_make_custodian(m);
}

static Scheme_Object *custodian_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_CUSTODIANP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *custodian_close_all(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CUSTODIANP(argv[0]))
    scheme_wrong_type("custodian-shutdown-all", "custodian", 0, argc, argv);

  scheme_close_managed((Scheme_Custodian *)argv[0]);

  return scheme_void;
}

static Scheme_Object *current_custodian(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-custodian", 
			     scheme_make_integer(MZCONFIG_CUSTODIAN),
			     argc, argv,
			     -1, custodian_p, "custodian", 0);
}


static void run_closers(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  Scheme_Object *l;

  for (l = closers; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    Scheme_Exit_Closer_Func cf;
    cf = (Scheme_Exit_Closer_Func)SCHEME_CAR(l);
    cf(o, f, data);
  }
}

static void run_atexit_closers(void)
{
  scheme_start_atomic();
  scheme_do_close_managed(NULL, run_closers);
}

void scheme_add_atexit_closer(Scheme_Exit_Closer_Func f)
{
  if (!closers) {
#ifdef USE_ON_EXIT_FOR_ATEXIT
    on_exit(run_atexit_closers, NULL);
#else
    atexit(run_atexit_closers);
#endif

    REGISTER_SO(closers);
    closers = scheme_null;
  }

  closers = scheme_make_pair((Scheme_Object *)f, closers);
}

void scheme_schedule_custodian_close(Scheme_Custodian *c)
{
  /* This procedure might be called by a garbage collector to register
     a resource-based kill. */

  if (!scheduled_kills) {
    REGISTER_SO(scheduled_kills);
    scheduled_kills = scheme_null;
  }

  scheduled_kills = scheme_make_pair((Scheme_Object *)c, scheduled_kills);
  scheme_fuel_counter = 0;
}

static void check_scheduled_kills()
{
  while (scheduled_kills && !SCHEME_NULLP(scheduled_kills)) {
    Scheme_Object *k;
    k = SCHEME_CAR(scheduled_kills);
    scheduled_kills = SCHEME_CDR(scheduled_kills);
    scheme_close_managed((Scheme_Custodian *)k);
  }
}

/*========================================================================*/
/*                      thread record creation                            */
/*========================================================================*/

static Scheme_Thread *make_thread(Scheme_Thread *after, Scheme_Config *config, 
				    Scheme_Custodian *mgr)
{
  Scheme_Thread *process;
  int prefix = 0;

  process = MALLOC_ONE_TAGGED(Scheme_Thread);

  process->type = scheme_thread_type;

#ifdef MZ_PRECISE_GC
  GC_register_thread(process);
#endif

  process->stack_start = 0;

  if (!scheme_main_thread) {
    /* Creating the first thread... */
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif
    REGISTER_SO(scheme_current_thread);
    REGISTER_SO(scheme_main_thread);
    REGISTER_SO(scheme_first_thread);
    REGISTER_SO(thread_swap_callbacks);

    scheme_current_thread = process;
    scheme_first_thread = scheme_main_thread = process;
    process->prev = NULL;
    process->next = NULL;

    thread_swap_callbacks = scheme_null;

    GC_collect_start_callback = get_ready_for_GC;
    GC_collect_end_callback = done_with_GC;

#ifdef LINK_EXTENSIONS_BY_TABLE
    scheme_current_thread_ptr = &scheme_current_thread;
    scheme_fuel_counter_ptr = &scheme_fuel_counter;
#endif

#ifdef MZ_PRECISE_GC
    {
      void *ss;
      ss = (void *)GC_get_stack_base();
      process->stack_start = ss;
    }
    GC_get_thread_stack_base = get_current_stack_start;
#endif
  } else {
    prefix = 1;
  }

  process->engine_weight = 10000;

  if (!config) {    
    config = make_initial_config();
    process->config = config;
  } else
    process->config = config;

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

  process->ran_some = 1;

  process->list_stack = NULL;

  if (prefix) {
    if (after) {
      process->prev = after;
      process->next = after->next;
      process->next->prev = process;
      process->prev->next = process;
    } else {
      process->next = scheme_first_thread;
      process->prev = NULL;
      process->next->prev = process;
      scheme_first_thread = process;
    }
  }

  {
    Scheme_Object **tb;
    tb = MALLOC_N(Scheme_Object *, buffer_init_size);
    process->tail_buffer = tb;
  }
  process->tail_buffer_size = buffer_init_size;

  process->runstack_size = INIT_SCHEME_STACK_SIZE;
  {
    Scheme_Object **sa;
    sa = scheme_malloc_allow_interior(sizeof(Scheme_Object*) * INIT_SCHEME_STACK_SIZE);
    process->runstack_start = sa;
  }
  process->runstack = process->runstack_start + INIT_SCHEME_STACK_SIZE;
  process->runstack_saved = NULL;

  process->cont_mark_pos = (MZ_MARK_POS_TYPE)1;
  process->cont_mark_stack = 0;
  process->cont_mark_stack_segments = NULL;
  process->cont_mark_seg_count = 0;

#ifdef RUNSTACK_IS_GLOBAL
  if (!prefix) {
# ifndef MZ_PRECISE_GC
    /* Precise GC: we intentionally don't register MZ_RUNSTACK. See done_with_GC() */
    REGISTER_SO(MZ_RUNSTACK);
# endif
    REGISTER_SO(MZ_RUNSTACK_START);

    MZ_RUNSTACK = process->runstack;
    MZ_RUNSTACK_START = process->runstack_start;
    MZ_CONT_MARK_STACK = process->cont_mark_stack;
    MZ_CONT_MARK_POS = process->cont_mark_pos;
  }
#endif

  process->on_kill = NULL;

  process->user_tls = NULL;
  process->user_tls_size = 0;
  
  process->nester = process->nestee = NULL;

  /* A thread points to a lot of stuff, so it's bad to put a finalization
     on it, which is what registering with a custodian does. Instead, we
     register a weak indirection with the custodian. That way, the thread
     (and anything it points to) can be collected one GC cycle earlier. */
  {
    Scheme_Thread_Custodian_Hop *hop;
    Scheme_Custodian_Reference *mref;
    hop = MALLOC_ONE_WEAK_RT(Scheme_Thread_Custodian_Hop);
    process->mr_hop = hop;
    hop->type = scheme_thread_hop_type;
    {
      Scheme_Thread *wp;
      wp = (Scheme_Thread *)WEAKIFY((Scheme_Object *)process);
      hop->p = wp;
    }

    mref = scheme_add_managed(mgr
			      ? mgr
			      : (Scheme_Custodian *)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN),
			      (Scheme_Object *)hop, NULL, NULL, 0);
    process->mref = mref;

#ifndef MZ_PRECISE_GC
    scheme_weak_reference((void **)&hop->p);
#endif
  }

  return process;
}

Scheme_Thread *scheme_make_thread()
{
  /* Makes the initial process. */
  return make_thread(NULL, NULL, NULL);
}

void scheme_set_tail_buffer_size(int s)
{
  if (s > buffer_init_size) {
    Scheme_Thread *p;

    buffer_init_size = s;

    for (p = scheme_first_thread; p; p = p->next) {
      if (p->tail_buffer_size < s) {
	Scheme_Object **tb;
	tb = MALLOC_N(Scheme_Object *, buffer_init_size);
	p->tail_buffer = tb;
	p->tail_buffer_size = buffer_init_size;
      }
    }
  }
}

int scheme_tls_allocate()
{
  return tls_pos++;
}

void scheme_tls_set(int pos, void *v)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->user_tls_size <= pos) {
    int oldc = p->user_tls_size;
    void **old_tls = p->user_tls, **va;

    p->user_tls_size = tls_pos;
    va = MALLOC_N(void*, tls_pos);
    p->user_tls = va;
    while (oldc--) {
      p->user_tls[oldc] = old_tls[oldc];
    }
  }

  p->user_tls[pos] = v;
}

void *scheme_tls_get(int pos)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->user_tls_size <= pos)
    return NULL;
  else
    return p->user_tls[pos];
}

/*========================================================================*/
/*                     thread creation and swapping                       */
/*========================================================================*/

int scheme_in_main_thread(void)
{
  return !scheme_current_thread->next;
}


void scheme_swap_thread(Scheme_Thread *new_thread)
{
  scheme_zero_unneeded_rands(scheme_current_thread);

#if WATCH_FOR_NESTED_SWAPS
  if (swapping)
    printf("death\n");
  swapping = 1;
#endif
  if (!swap_no_setjmp && SETJMP(scheme_current_thread)) {
    /* We're back! */
#ifdef RUNSTACK_IS_GLOBAL
    MZ_RUNSTACK = scheme_current_thread->runstack;
    MZ_RUNSTACK_START = scheme_current_thread->runstack_start;
    MZ_CONT_MARK_STACK = scheme_current_thread->cont_mark_stack;
    MZ_CONT_MARK_POS = scheme_current_thread->cont_mark_pos;
#endif
    RESETJMP(scheme_current_thread);
#if WATCH_FOR_NESTED_SWAPS
    swapping = 0;
#endif
    scheme_reset_locale();
    {
      Scheme_Object *l, *o;
      Scheme_Closure_Func f;
      for (l = thread_swap_callbacks; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	o = SCHEME_CAR(l);
	f = SCHEME_CLOS_FUNC(o);
	o = SCHEME_CLOS_DATA(o);
	f(o);
      }
    }
  } else {
    swap_no_setjmp = 0;

    /* We're leaving... */
#ifdef RUNSTACK_IS_GLOBAL
    scheme_current_thread->runstack = MZ_RUNSTACK;
    scheme_current_thread->runstack_start = MZ_RUNSTACK_START;
    scheme_current_thread->cont_mark_stack = MZ_CONT_MARK_STACK;
    scheme_current_thread->cont_mark_pos = MZ_CONT_MARK_POS;
#endif
    scheme_current_thread = new_thread;
    LONGJMP(scheme_current_thread);
  }
}

static void select_thread(Scheme_Thread *start_thread)
{
  Scheme_Thread *new_thread = start_thread;

  do {
    if (!new_thread)
      new_thread = scheme_first_thread;
    
    /* Can't swap in processes with a nestee: */
    while (new_thread && new_thread->nestee) {
      new_thread = new_thread->next;
    }

    if (!new_thread && !start_thread) {
      /* The main thread must be blocked on a nestee, and everything
	 else is suspended. But we have to go somewhere.  Weakly
	 resume the main thread's innermost nestee. */
      new_thread = scheme_main_thread;
      while (new_thread->nestee) {
	new_thread = new_thread->nestee;
      }
      scheme_weak_resume_thread(new_thread);
      break;
    } 
    start_thread = NULL;
  } while (!new_thread);

  scheme_swap_thread(new_thread);
}

static void remove_thread(Scheme_Thread *r)
{
  Scheme_Saved_Stack *saved;

  r->running = 0;

  if (r->prev) {
    r->prev->next = r->next;
    r->next->prev = r->prev;
  } else {
    if (r->next)
      r->next->prev = NULL;
    scheme_first_thread = r->next;
  }
  r->next = r->prev = NULL;

#ifdef RUNSTACK_IS_GLOBAL
  if (r == scheme_current_thread) {
    r->runstack = MZ_RUNSTACK;
    MZ_RUNSTACK = NULL;
    r->runstack_start = MZ_RUNSTACK_START;
    MZ_RUNSTACK_START = NULL;
    r->cont_mark_stack = MZ_CONT_MARK_STACK;
    r->cont_mark_pos = MZ_CONT_MARK_POS;
  }
#endif

#if defined(SENORA_GC_NO_FREE) || defined(MZ_PRECISE_GC)
  memset(r->runstack_start, 0, r->runstack_size * sizeof(Scheme_Object*));
#else
  GC_free(r->runstack_start);
#endif
  r->runstack_start = NULL;
  for (saved = r->runstack_saved; saved; saved = saved->prev) {
#if defined(SENORA_GC_NO_FREE) || defined(MZ_PRECISE_GC)
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

  r->dw = NULL;
  r->config = NULL;
  r->cont_mark_stack_segments = NULL;
  r->overflow = NULL;

  if (r == scheme_current_thread) {
    /* We're going to be swapped out immediately. */
    swap_no_setjmp = 1;
  } else
    RESETJMP(r);

  scheme_remove_managed(r->mref, (Scheme_Object *)r->mr_hop);
}

static void start_child(Scheme_Thread * volatile child,
			Scheme_Thread * volatile return_to_thread,
			Scheme_Object * volatile child_eval)
{
  if (SETJMP(child)) {
#ifdef RUNSTACK_IS_GLOBAL
    MZ_RUNSTACK = scheme_current_thread->runstack;
    MZ_RUNSTACK_START = scheme_current_thread->runstack_start;
    MZ_CONT_MARK_STACK = scheme_current_thread->cont_mark_stack;
    MZ_CONT_MARK_POS = scheme_current_thread->cont_mark_pos;
#endif

    RESETJMP(child);

#if WATCH_FOR_NESTED_SWAPS
    swapping = 0;
#endif

    if (return_to_thread)
      scheme_swap_thread(return_to_thread);

    if (scheme_current_thread->running & MZTHREAD_KILLED) {
      /* This thread is dead! Give up now. */
      exit_or_escape(scheme_current_thread);
    }

    if (!scheme_setjmp(scheme_error_buf)) 
      scheme_apply_multi(child_eval, 0, NULL);
    
    remove_thread(scheme_current_thread);

    thread_ended_with_activity = 1;
    
    if (scheme_notify_multithread && !scheme_first_thread->next) {
      scheme_notify_multithread(0);
      have_activity = 0;
    }

    select_thread(NULL);
    
    /* Shouldn't get here! */
    scheme_signal_error("bad process switch");
  }
}

static Scheme_Object *make_subprocess(Scheme_Object *child_thunk,
				      void *child_start, 
				      Scheme_Config *config,
				      Scheme_Custodian *mgr)
{
  Scheme_Thread *child, *return_to_thread;
  int turn_on_multi;
 
  turn_on_multi = !scheme_first_thread->next;
  
  scheme_ensure_stack_start(scheme_current_thread, child_start);
  
  child = make_thread(NULL, config, mgr);

  scheme_init_error_escape_proc(child);
  scheme_set_param(child->config, MZCONFIG_EXN_HANDLER,
		   scheme_get_param(child->config, MZCONFIG_INIT_EXN_HANDLER));

  child->stack_start = child_start;
  
  if (do_atomic)
    return_to_thread = scheme_current_thread;
  else
    return_to_thread = NULL;

  start_child(child, return_to_thread, child_thunk);

  if (scheme_notify_multithread && turn_on_multi) {
    scheme_notify_multithread(1);
    have_activity = 1;
  }

  SCHEME_USE_FUEL(1000);
  
  return (Scheme_Object *)child;
}

Scheme_Object *scheme_thread(Scheme_Object *thunk, Scheme_Config *config)
{
  return scheme_thread_w_custodian(thunk, config, NULL);
}

static Scheme_Object *sch_thread(int argc, Scheme_Object *args[])
{
  Scheme_Config *c;

  scheme_check_proc_arity("thread", 0, 0, argc, args);

  c = (Scheme_Config *)scheme_branch_config();
  return scheme_thread(args[0], c);
}

static Scheme_Object *sch_current(int argc, Scheme_Object *args[])
{
  return (Scheme_Object *)scheme_current_thread;
}

static Scheme_Object *thread_p(int argc, Scheme_Object *args[])
{
  return SCHEME_THREADP(args[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *thread_running_p(int argc, Scheme_Object *args[])
{
  int running;

  if (!SCHEME_THREADP(args[0]))
    scheme_wrong_type("thread-running?", "thread", 0, argc, args);

  running = ((Scheme_Thread *)args[0])->running;

  return MZTHREAD_STILL_RUNNING(running) ? scheme_true : scheme_false;
}

static int thread_wait_done(Scheme_Object *p)
{
  int running = ((Scheme_Thread *)p)->running;
  return !MZTHREAD_STILL_RUNNING(running);
}

static Scheme_Object *thread_wait(int argc, Scheme_Object *args[])
{
  Scheme_Thread *p;

  if (!SCHEME_THREADP(args[0]))
    scheme_wrong_type("thread-wait", "thread", 0, argc, args);

  p = (Scheme_Thread *)args[0];

  if (MZTHREAD_STILL_RUNNING(p->running)) {
    scheme_block_until(thread_wait_done, NULL, (Scheme_Object *)p, 0);
  }

  return scheme_void;
}

static void register_thread_wait()
{
  scheme_add_waitable(scheme_thread_type, thread_wait_done, NULL, NULL);
}

void scheme_add_swap_callback(Scheme_Closure_Func f, Scheme_Object *data)
{
  Scheme_Object *p;

  p = scheme_make_pair((Scheme_Object *)f, data);
  thread_swap_callbacks = scheme_make_pair(p, thread_swap_callbacks);
}

/**************************************************************************/
/* Ensure that a new thread has a reasonable starting stack */

#ifdef DO_STACK_CHECK
# define THREAD_STACK_SPACE (STACK_SAFETY_MARGIN / 2)
void scheme_check_stack_ok(char *s); /* prototype, needed for PalmOS */

void scheme_check_stack_ok(char *s) {
# include "mzstkchk.h"
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

static Scheme_Object *thread_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *thunk, *result;
  Scheme_Config *config;
  Scheme_Custodian *mgr;
#ifndef MZ_PRECISE_GC
  long dummy;
#endif
  
  thunk = (Scheme_Object *)p->ku.k.p1;
  config = (Scheme_Config *)p->ku.k.p2;
  mgr = (Scheme_Custodian *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  
  result = make_subprocess(thunk,
#ifdef MZ_PRECISE_GC
			   (void *)&__gc_var_stack__,
#else
			   (void *)&dummy, 
#endif
			   config, mgr);

  /* Don't get rid of `result'; it keeps the
     Precise GC xformer from "optimizing" away
     the __gc_var_stack__ frame. */
  return result;
}

#endif /* DO_STACK_CHECK */

Scheme_Object *scheme_thread_w_custodian(Scheme_Object *thunk, Scheme_Config *config, 
				       Scheme_Custodian *mgr)
{
  Scheme_Object *result;
#ifndef MZ_PRECISE_GC
  long dummy;
#endif

#ifdef DO_STACK_CHECK
  /* Make sure the thread starts out with a reasonable stack size, so
     it doesn't thrash right away: */
  if (is_stack_too_shallow()) {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = thunk;
    p->ku.k.p2 = config;
    p->ku.k.p3 = mgr;

    return scheme_handle_stack_overflow(thread_k);
  }
#endif

  result = make_subprocess(thunk, 
#ifdef MZ_PRECISE_GC
			   (void *)&__gc_var_stack__,
#else
			   (void *)&dummy, 
#endif
			   config, mgr);

  /* Don't get rid of `result'; it keeps the
     Precise GC xformer from "optimizing" away
     the __gc_var_stack__ frame. */
  return result;
}

/**************************************************************************/
/* Nested threads */

static Scheme_Object *def_nested_exn_handler(int argc, Scheme_Object *argv[])
{
  if (scheme_current_thread->nester) {
    Scheme_Thread *p = scheme_current_thread;
    p->cjs.jumping_to_continuation = (struct Scheme_Escaping_Cont *)scheme_current_thread;
    p->cjs.u.val = argv[0];
    p->cjs.is_kill = 0;
    scheme_longjmp(p->error_buf, 1);
  }

  return scheme_void; /* misuse of exception handler */
}

static Scheme_Object *call_as_nested_thread(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Thread * volatile np;
  Scheme_Custodian *mgr;
  Scheme_Object * volatile v;
  volatile int failure;

  scheme_check_proc_arity("call-in-nested-thread", 0, 0, argc, argv);
  if (argc > 1) {
    if (SCHEME_CUSTODIANP(argv[1]))
      mgr = (Scheme_Custodian *)argv[1];
    else {
      scheme_wrong_type("call-in-nested-thread", "custodian", 1, argc, argv);
      return NULL;
    }
  } else
    mgr = (Scheme_Custodian *)scheme_get_param(p->config, MZCONFIG_CUSTODIAN);

  SCHEME_USE_FUEL(25);

  np = MALLOC_ONE_TAGGED(Scheme_Thread);
  np->type = scheme_thread_type;
#ifdef MZ_PRECISE_GC
  GC_register_thread(np);
#endif
  np->running = MZTHREAD_RUNNING;
  np->ran_some = 1;

#ifdef RUNSTACK_IS_GLOBAL
  p->runstack = MZ_RUNSTACK;
  p->runstack_start = MZ_RUNSTACK_START;
  p->cont_mark_stack = MZ_CONT_MARK_STACK;
  p->cont_mark_pos = MZ_CONT_MARK_POS;
#endif

  np->runstack = p->runstack;
  np->runstack_start = p->runstack_start;
  np->runstack_size = p->runstack_size;
  np->runstack_saved = p->runstack_saved;
  np->stack_start = p->stack_start;
  np->stack_end = p->stack_end;
  np->current_local_env = p->current_local_env;
  np->engine_weight = p->engine_weight;
  {
    Scheme_Object **tb;
    tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
    np->tail_buffer = tb;
  }
  np->tail_buffer_size = p->tail_buffer_size;

  np->overflow_set = p->overflow_set;
  np->cc_start = p->cc_start;
  memcpy(&np->overflow_buf, &p->overflow_buf, sizeof(mz_jmp_buf));

  /* In case it's not yet set in the main thread... */
  scheme_ensure_stack_start((Scheme_Thread *)np, (int *)&failure);
  
  np->list_stack = p->list_stack;
  np->list_stack_pos = p->list_stack_pos;

  /* np->prev = NULL; - 0ed by allocation */
  np->next = scheme_first_thread;
  scheme_first_thread->prev = np;
  scheme_first_thread = np;

  {
    Scheme_Config *nconfig;
    nconfig = (Scheme_Config *)scheme_make_config(p->config);
    np->config = nconfig;
  }
  np->cont_mark_pos = (MZ_MARK_POS_TYPE)1;
  /* others 0ed already by allocation */

  if (!nested_exn_handler) {
    REGISTER_SO(nested_exn_handler);
    nested_exn_handler = scheme_make_prim_w_arity(def_nested_exn_handler,
						  "nested-thread-exception-handler",
						  1, 1);
  }

  scheme_init_error_escape_proc(np);
  scheme_set_param(np->config, MZCONFIG_EXN_HANDLER, nested_exn_handler);

  np->nester = p;
  p->nestee = np;
  np->external_break = p->external_break;
  p->external_break = 0;

  {
    Scheme_Thread_Custodian_Hop *hop;
    Scheme_Custodian_Reference *mref;
    hop = MALLOC_ONE_WEAK_RT(Scheme_Thread_Custodian_Hop);
    np->mr_hop = hop;
    hop->type = scheme_thread_hop_type;
    {
      Scheme_Thread *wp;
      wp = (Scheme_Thread *)WEAKIFY((Scheme_Object *)np);
      hop->p = wp;
    }
    mref = scheme_add_managed(mgr, (Scheme_Object *)hop, NULL, NULL, 0);
    np->mref = mref;
#ifndef MZ_PRECISE_GC
    scheme_weak_reference((void **)&hop->p);
#endif
  }

#ifdef RUNSTACK_IS_GLOBAL
  MZ_CONT_MARK_STACK = np->cont_mark_stack;
  MZ_CONT_MARK_POS = np->cont_mark_pos;
#endif

  scheme_current_thread = np;

  /* Call thunk, catch escape: */
  if (scheme_setjmp(np->error_buf)) {
    if (!np->cjs.is_kill)
      v = np->cjs.u.val;
    else
      v = NULL;
    failure = 1;
  } else {
    v = scheme_apply(argv[0], 0, NULL);
    failure = 0;
  }

  scheme_remove_managed(np->mref, (Scheme_Object *)np->mr_hop);
#ifdef MZ_PRECISE_GC
  WEAKIFIED(np->mr_hop->p) = NULL;
#else
  scheme_unweak_reference((void **)&np->mr_hop->p);
#endif
  scheme_remove_all_finalization(np->mr_hop);

  if (np->prev)
    np->prev->next = np->next;
  else
    scheme_first_thread = np->next;
  np->next->prev = np->prev;

  np->running = 0;

  p->external_break = np->external_break;
  p->nestee = NULL;
  np->nester = NULL;

  np->runstack_start = NULL;
  np->runstack_saved = NULL;
  np->list_stack = NULL;
  np->config = NULL;
  np->dw = NULL;

  scheme_current_thread = p;

#ifdef RUNSTACK_IS_GLOBAL
  MZ_CONT_MARK_STACK = p->cont_mark_stack;
  MZ_CONT_MARK_POS = p->cont_mark_pos;
#endif

  if (p->running & MZTHREAD_KILLED)
    scheme_thread_block(0.0);

  if (failure) {
    if (!v)
      scheme_raise_exn(MZEXN_THREAD, "call-in-nested-thread: the thread was killed, or it exited via the default error escape handler");
    else
      scheme_raise(v);
  }

  /* May have just moved a break to a breakable thread: */
    /* Check for external break again after swap or sleep */
  if (p->external_break && !p->suspend_break && scheme_can_break(p, p->config)) {
    scheme_thread_block(0.0);
    p->ran_some = 1;
  }

  return v;
}

/*========================================================================*/
/*                     thread scheduling and termination                  */
/*========================================================================*/

static int check_sleep(int need_activity, int sleep_now)
/* Signals should be suspended */
{
  Scheme_Thread *p, *p2;
  int end_with_act;
  
#if defined(USING_FDS)
  DECL_FDSET(set, 3);
  fd_set *set1, *set2;
#endif
  void *fds;

  /* Is everything blocked? */
  if (!do_atomic) {
    p = scheme_first_thread;
    while (p) {
      if (!p->nestee && (p->ran_some || p->block_descriptor == NOT_BLOCKED))
	break;
      p = p->next;
    }
  } else
    p = NULL;
  
  p2 = scheme_first_thread;
  while (p2) {
    p2->ran_some = 0;
    p2 = p2->next;
  }
  
  end_with_act = thread_ended_with_activity;
  thread_ended_with_activity = 0;
  
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
    
    p = scheme_first_thread;
    while (p) {
      int merge_time = 0;

      if (p->nestee) {
	/* nothing */
      } else if (p->block_descriptor == -1) {
	if (p->block_needs_wakeup) {
	  Block_Needs_Wakeup_Procedure f = p->block_needs_wakeup;
	  f(p->blocker, fds);
	}
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

void scheme_check_threads(void)
/* Signals should be suspended. */
{
  scheme_current_thread->suspend_break++;
  scheme_thread_block((float)0);
  --scheme_current_thread->suspend_break;

  check_sleep(have_activity, 0);
}

void scheme_wake_up(void)
{
  scheme_active_but_sleeping = 0;
  if (have_activity && scheme_notify_multithread)
    scheme_notify_multithread(1);
}

void scheme_out_of_fuel(void)
{
  scheme_thread_block((float)0);
  scheme_current_thread->ran_some = 1;
}

#ifdef USE_ITIMER
static int itimer_handler_installed = 0;

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void timer_expired(int ignored)
{
  scheme_fuel_counter = 0;
#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGPROF, timer_expired);
#  endif
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif

int scheme_can_break(Scheme_Thread *p, Scheme_Config *config)
{
  return (!p->suspend_break
	  && SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ENABLE_BREAK))
	  && !p->exn_raised);
}

static Scheme_Object *raise_user_break(int argc, Scheme_Object **argv)
{
  scheme_raise_exn(MZEXN_BREAK, argv[0], "user break");

  return scheme_void;
}

static void raise_break(Scheme_Thread *p)
{
  int block_descriptor;
  Scheme_Object *blocker; /* semaphore or port */
  Block_Check_Procedure block_check;
  Block_Needs_Wakeup_Procedure block_needs_wakeup;
  Scheme_Object *a[1];

  p->external_break = 0;

  block_descriptor = p->block_descriptor;
  blocker = p->blocker;
  block_check = p->block_check;
  block_needs_wakeup = p->block_needs_wakeup;
  
  p->block_descriptor = 0;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
  p->ran_some = 1;
  
  a[0] = scheme_make_prim(raise_user_break);

  scheme_call_ec(1, a);

  /* Continue from break... */
  p->block_descriptor = block_descriptor;
  p->blocker = blocker;
  p->block_check = block_check;
  p->block_needs_wakeup = block_needs_wakeup;
}

static void exit_or_escape(Scheme_Thread *p)
{
  /* Maybe this killed thread is nested: */
  if (p->nester) {
    if (p->running & MZTHREAD_KILLED)
      p->running -= MZTHREAD_KILLED;
    p->cjs.jumping_to_continuation = (struct Scheme_Escaping_Cont *)p;
    p->cjs.is_kill = 1;
    scheme_longjmp(p->error_buf, 1);
  }

  if (!p->next) {
    /* Hard exit: */
    if (scheme_exit)
      scheme_exit(0);
    
    /* We really have to exit: */
    exit(0);
  }

  remove_thread(p);
  select_thread(NULL);
}

void scheme_break_thread(Scheme_Thread *p)
{
  if (delay_breaks) {
    delayed_break_ready = 1;
    return;
  }

  if (!p) {
    p = scheme_main_thread;
    if (!p)
      return;
  }

  /* Propagate breaks: */
  while (p->nestee) {
    p = p->nestee;
  }

  p->external_break = 1;

  if (p == scheme_current_thread) {
    if (scheme_can_break(p, p->config))
      scheme_fuel_counter = 0;
  }
  scheme_weak_resume_thread(p);
# if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)
  if (!p->next)
    ReleaseSemaphore(scheme_break_semaphore, 1, NULL);
# endif
}

void scheme_thread_block(float sleep_time)
     /* If we're blocked, `sleep_time' is a max sleep time,
	not a min sleep time. Otherwise, it's a min & max sleep time.
	This proc auto-resets p's blocking info if an escape occurs. */
{
  long start, d;
  Scheme_Thread *next, *p = scheme_current_thread;
  Scheme_Config *config = p->config;

  if (p->running & MZTHREAD_KILLED) {
    /* This thread is dead! Give up now. */
    exit_or_escape(p);
  }

  /* Check scheduled_kills early and often. */
  check_scheduled_kills();

  if (scheme_active_but_sleeping)
    scheme_wake_up();

  if (sleep_time > 0)
    start = scheme_get_milliseconds();
  else
    start = 0; /* compiler-friendly */

 start_sleep_check:

  if (!p->external_break && !p->next && scheme_check_for_break && scheme_check_for_break())
    p->external_break = 1;

  if (p->external_break && !p->suspend_break && scheme_can_break(p, config)) {
    raise_break(p);
    goto start_sleep_check;
  }
  
 swap_or_sleep:

#ifdef USE_OSKIT_CONSOLE
  scheme_check_keyboard_input();
#endif

  /* Check scheduled_kills early and often. */
  check_scheduled_kills();

  if (!do_atomic && (sleep_time >= 0.0)) {
    /* Find the next process. Skip processes that are definitely
       blocked. */
    
    next = p;
    while (1) {
      next = next->next ? next->next : scheme_first_thread;
      if (SAME_PTR(next, p)) {
	next = NULL;
	break;
      }
      
      if (next->nestee) {
	/* Blocked on nestee */
      } else if (next->running & MZTHREAD_KILLED) {
	/* This one has been terminated. */
	if ((next->running & MZTHREAD_NEED_KILL_CLEANUP) 
	    || next->nester
	    || !next->next) {
	  /* The thread needs to clean up. Swap it in so it can die. */
	  break;
	} else
	  remove_thread(next);
	break;
      } else if (next->external_break && scheme_can_break(next, next->config)) {
	break;
      } else {
	if (next->block_descriptor == -1) {
	  if (next->block_check) {
	    Block_Check_Procedure f = next->block_check;
	    if (f(next->blocker))
	      break;
	  }
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
    Scheme_Thread *p = scheme_first_thread;
    while (p != next) {
      p = p->next;
      if (!p) {
	printf("error: tried to switch to bad thread\n");
	exit(1);
      }
    }
  }
#endif

  if (next) {
    if (!p->next) {
      /* This is the main process */
      scheme_ensure_stack_start(p, (void *)&start);
    }
    
    scheme_swap_thread(next);
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

  /* Killed while I was asleep? */
  if (p->running & MZTHREAD_KILLED) {
    /* This thread is dead! Give up now. */
    if (p->running & MZTHREAD_NEED_KILL_CLEANUP) {
      /* The thread needs to clean up. It will block immediately to die. */
      return;
    } else {
      exit_or_escape(p);
    }
  }

  /* Check for external break again after swap or sleep */
  if (p->external_break && !p->suspend_break && scheme_can_break(p, config)) {
    raise_break(p);
  }
  
  if (sleep_time > 0) {
    d = (scheme_get_milliseconds() - start);
    if (d < 0)
      d = -d;
    if (d < (sleep_time * 1000)) {
      /* Still have time to sleep if necessary, but make sure we're
	 not ready (because maybe that's why we were swapped back in!) */
      if (p->block_descriptor == -1) {
	if (p->block_check) {
	  Block_Check_Procedure f = p->block_check;
	  if (f(p->blocker)) {
	    sleep_time = 0;
	  }
	}
      }

      if (sleep_time > 0)
	goto swap_or_sleep;
    }
  }

  if (do_atomic)
    missed_context_switch = 1;

#ifdef USE_ITIMER
  {
    struct itimerval t, old;

    if (!itimer_handler_installed) {
      itimer_handler_installed = 1;
      MZ_SIGSET(SIGPROF, timer_expired);
    }

    t.it_value.tv_sec = 0;
    t.it_value.tv_usec = MZ_THREAD_QUANTUM_USEC;
    t.it_interval.tv_sec = 0;
    t.it_interval.tv_usec = 0;

    setitimer(ITIMER_PROF, &t, &old);
  }
#endif
#ifdef USE_WIN32_THREAD_TIMER
  scheme_start_itimer_thread(MZ_THREAD_QUANTUM_USEC);
#endif

  /* Check scheduled_kills early and often. */
  check_scheduled_kills();

  MZTHREADELEM(p, fuel_counter) = p->engine_weight;
}

void scheme_making_progress()
{
  scheme_current_thread->ran_some = 1;
}

int scheme_block_until(Scheme_Ready_Fun f, Scheme_Needs_Wakeup_Fun fdf, 
		       Scheme_Object *data, float delay)
{
  int result;
  Scheme_Thread *p = scheme_current_thread;

  p->block_descriptor = -1;
  p->blocker = (Scheme_Object *)data;
  p->block_check = f;
  p->block_needs_wakeup = fdf;
  do {
    scheme_thread_block(delay);
  } while (!(result = f((Scheme_Object *)data)));
  p->block_descriptor = NOT_BLOCKED;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
  p->ran_some = 1;

  return result;
}

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
    scheme_thread_block(0.0);
    scheme_current_thread->ran_some = 1;    
  }
}

void scheme_weak_suspend_thread(Scheme_Thread *r)
{
  Scheme_Thread *swap_to = r->next;

  if (r->prev) {
    r->prev->next = r->next;
    r->next->prev = r->prev;
  } else {
    if (r->next)
      r->next->prev = NULL;
    scheme_first_thread = r->next;
  }

  r->next = r->prev = NULL;

  r->running |= MZTHREAD_SUSPENDED;

  if (r == scheme_current_thread) {
    /* NOTE: swap_to might not be a good choice, because it
       might be blocked. If it's a bad choice, we waste
       time swapping in swap_to. */
    select_thread(swap_to);

    /* Killed while suspended? */
    if ((r->running & MZTHREAD_KILLED) && !(r->running & MZTHREAD_NEED_KILL_CLEANUP))
      scheme_thread_block(0);
  }
}

void scheme_weak_resume_thread(Scheme_Thread *r)
{
  if (r->running & MZTHREAD_SUSPENDED) {
    r->running -= MZTHREAD_SUSPENDED;
    r->next = scheme_first_thread;
    r->prev = NULL;
    scheme_first_thread = r;
    r->next->prev = r;
    r->ran_some = 1;
  }
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

  scheme_thread_block(t);
  scheme_current_thread->ran_some = 1;

  return scheme_void;
}

static Scheme_Object *break_thread(int argc, Scheme_Object *args[])
{
  Scheme_Thread *p;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_thread_type))
    scheme_wrong_type("break-thread", "thread", 0, argc, args);

  p = (Scheme_Thread *)args[0];

  scheme_break_thread(p);

  /* In case p == scheme_current_thread */
  if (!scheme_fuel_counter) {
    scheme_thread_block(0.0);
    scheme_current_thread->ran_some = 1;
  }

  return scheme_void;
}

static int do_kill_thread(Scheme_Thread *p)
{
  int kill_self = 0;

  if (!p->running || (p->running & MZTHREAD_KILLED)) {
    return 0;
  }

  {
    Scheme_Thread *nestee;

    nestee = p->nestee;

    if (nestee)
      scheme_break_thread(nestee);
  }

  scheme_weak_resume_thread(p);

  while (p->private_on_kill) {
    p->private_on_kill(p->private_kill_data);
    if (p->private_kill_next) {
      p->private_on_kill = (Scheme_Kill_Action_Func)p->private_kill_next[0];
      p->private_kill_data = p->private_kill_next[1];
      p->private_kill_next = (void **)p->private_kill_next[2];
    } else {
      p->private_on_kill = NULL;
      p->private_kill_data = NULL;
    }
  }

  if (p->on_kill)
    p->on_kill(p);

  scheme_remove_managed(p->mref, (Scheme_Object *)p->mr_hop);

  if (p->running) {
    p->running |= MZTHREAD_KILLED;
    if (p->running & MZTHREAD_NEED_KILL_CLEANUP)
      scheme_weak_resume_thread(p);
  }
  if (p == scheme_current_thread)
    kill_self = 1;

  return kill_self;
}

#ifndef NO_SCHEME_THREADS
void scheme_kill_thread(Scheme_Thread *p)
{
  if (do_kill_thread(p)) {
    /* Kill self: */
    scheme_thread_block(0.0);
  }

  /* Give killed threads time to die: */
  scheme_thread_block(0.0);
  scheme_current_thread->ran_some = 1;
}

static Scheme_Object *kill_thread(int argc, Scheme_Object *argv[])
{
  Scheme_Custodian *m, *current;
  Scheme_Thread *p = (Scheme_Thread *)argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_thread_type))
    scheme_wrong_type("kill-thread", "thread", 0, argc, argv);

  if (!MZTHREAD_STILL_RUNNING(p->running))
    return scheme_void;

  /* Check management of the thread: */
  current = (Scheme_Custodian *)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
  m = CUSTODIAN_FAM(p->mref);

  while (NOT_SAME_OBJ(m, current)) {
    m = CUSTODIAN_FAM(m->parent);
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

void scheme_push_kill_action(Scheme_Kill_Action_Func f, void *d)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->private_on_kill) {
    /* Pretty unlikely that these get nested. An exception handler
       would have to block on and within operations that need special
       kill handling. But it could happen. */
    void **next;
    next = MALLOC_N(void *, 3);
    next[0] = (void *)p->private_on_kill;
    next[1] = p->private_kill_data;
    next[2] = (void *)p->private_kill_next;
    p->private_kill_next = next;
  }

  p->private_on_kill = f;
  p->private_kill_data = d;
}

void scheme_pop_kill_action()
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->private_kill_next) {
    p->private_on_kill = (Scheme_Kill_Action_Func)p->private_kill_next[0];
    p->private_kill_data = p->private_kill_next[1];
    p->private_kill_next = (void **)p->private_kill_next[2];
  } else {
    p->private_on_kill = NULL;
    p->private_kill_data = NULL;
  }
}

/*========================================================================*/
/*                              waiting                                   */
/*========================================================================*/

typedef struct Waitable {
  MZTAG_IF_REQUIRED
  Scheme_Type wait_type;
  Scheme_Ready_Fun ready;
  Scheme_Needs_Wakeup_Fun needs_wakeup;
  Scheme_Wait_Sema_Fun get_sema;
  Scheme_Wait_Filter_Fun filter;
  struct Waitable *next;
} Waitable;

static Waitable *waitables;

void scheme_add_waitable(Scheme_Type type,
			 Scheme_Ready_Fun ready, 
			 Scheme_Needs_Wakeup_Fun wakeup, 
			 Scheme_Wait_Filter_Fun filter)
{
  Waitable *next = waitables;

  if (!waitables) {
    REGISTER_SO(waitables);
  }

  waitables = MALLOC_ONE_RT(Waitable);
#ifdef MZTAG_REQUIRED
  waitables->type = scheme_rt_waitable;
#endif
  waitables->wait_type = type;
  waitables->ready = ready;
  waitables->needs_wakeup = wakeup;
  waitables->filter = filter;
  waitables->next = next;
}

void scheme_add_waitable_through_sema(Scheme_Type type,
				      Scheme_Wait_Sema_Fun get_sema, 
				      Scheme_Wait_Filter_Fun filter)
{
  scheme_add_waitable(type, NULL, NULL, filter);
  waitables->get_sema = get_sema;
}

static Waitable *find_waitable(Scheme_Object *o)
{
  Scheme_Type t;
  Waitable *w;

  t = SCHEME_TYPE(o);

  for (w = waitables; w; w = w->next) {
    if (SAME_TYPE(t, w->wait_type)) {
      if (w->filter) {
	Scheme_Wait_Filter_Fun filter;
	filter = w->filter;
	if (!filter(o))
	  return NULL;
      }
      return w;
    }
  }

  return NULL;
}

int scheme_is_waitable(Scheme_Object *o)
{
  return !!find_waitable(o);
}

static int block_on_waitable(Waitable *w, Scheme_Object *o, int just_try)
{
  if (w->ready) {
    if (just_try) {
      Scheme_Ready_Fun r;
      r = w->ready;
      return r(o);
    } else {
      scheme_block_until(w->ready, w->needs_wakeup, o, 0.0);
      return 1;
    }
  } else if (w->get_sema) {
    int repost = 0, ok;
    Scheme_Wait_Sema_Fun get_sema = w->get_sema;
    Scheme_Object *sema;
    
    sema = get_sema(o, &repost);
    ok = scheme_wait_sema(sema, just_try);
    if (ok && repost)
      scheme_post_sema(sema);

    return ok;
  } else
    return 1;
}

void scheme_waitable_needs_wakeup(Scheme_Object *o, void *fds)
{
  Waitable *w;

  w = find_waitable(o);
  if (w && w->needs_wakeup) {
    w->needs_wakeup(o, fds);
  }
}

int scheme_wait_on_waitable(Scheme_Object *o, int just_try)
{
  Waitable *w;

  w = find_waitable(o);
  if (w)
    return block_on_waitable(w, o, just_try);
  else
    return 0;
}

typedef struct Waiting {
  MZTAG_IF_REQUIRED
  int argc;
  Scheme_Object **argv;
  Waitable **ws;
  Scheme_Object *result;
  long start_time;
  float timeout;
} Waiting;

static int waiting_ready(Scheme_Object *s)
{
  int i;
  Waiting *waiting = (Waiting *)s;
  Waitable *w;

  if (waiting->result)
    return 1;

  /* Anything ready? */
  for (i = 0; i < waiting->argc; i++) {
    w = waiting->ws[i];
    if (w->ready) {
      Scheme_Ready_Fun ready = w->ready;
      
      if (ready(waiting->argv[i])) {
	waiting->result = waiting->argv[i];
	return 1;
      }
    } else if (w->get_sema) {
      int repost = 0;
      Scheme_Wait_Sema_Fun get_sema = w->get_sema;
      Scheme_Object *sema;
      
      sema = get_sema(waiting->argv[i], &repost);
      if (scheme_wait_sema(sema, 1)) {
	if (repost)
	  scheme_post_sema(sema);
	waiting->result = waiting->argv[i];
	return 1;
      }
    }
  }

  if (waiting->timeout >= 0.0) {
    long d;
    d = (scheme_get_milliseconds() - waiting->start_time);
    if (d < 0)
      d = -d;
    if (d >= (waiting->timeout * 1000))
      return 1;
  }

  return 0;
}

static void waiting_needs_wakeup(Scheme_Object *s, void *fds)
{
  int i;
  Waiting *waiting = (Waiting *)s;
  Waitable *w;

  for (i = 0; i < waiting->argc; i++) {
    w = waiting->ws[i];
    if (w->needs_wakeup) {
      Scheme_Needs_Wakeup_Fun nw = w->needs_wakeup;
      
      nw(waiting->argv[i], fds);
    }
  }
}

static Scheme_Object *object_waitable_p(int argc, Scheme_Object *argv[])
{
  return (scheme_is_waitable(argv[0])
	  ? scheme_true
	  : scheme_false);
}

Scheme_Object *scheme_object_wait_multiple(int argc, Scheme_Object *argv[])
{
  Waitable *w, **ws, *qws[1];
  Waiting *waiting;
  Scheme_Object **args;
  int i;
  float timeout = -1.0;
  long start_time;

  if (!SCHEME_FALSEP(argv[0])) {
    if (SCHEME_REALP(argv[0]))
      timeout = scheme_real_to_double(argv[0]);

    if (timeout < 0.0) {
      scheme_wrong_type("object-wait-multiple", "non-negative real number", 0, argc, argv);
      return NULL;
    }

    start_time = scheme_get_milliseconds();
  } else
    start_time = 0;

  if (argc == 2 && SCHEME_FALSEP(argv[0]) && SCHEME_SEMAP(argv[1])) {
    scheme_wait_sema(argv[1], 0);
    return scheme_void;
  }

  if ((argc == 2) && SCHEME_FALSEP(argv[0]))
    ws = qws;
  else
    ws = MALLOC_N(Waitable*, argc-1);

  /* Find Waitable record for each argument: */
  for (i = 0; i < argc-1; i++) {
    w = find_waitable(argv[i+1]);
    if (!w) {
      scheme_arg_mismatch("object-wait-multiple",
			  "argument is not waitable: ",
			  argv[i+1]);
    }
    ws[i] = w;
  }

  /* Special case: one argument, no timeout */
  if ((argc == 2) && SCHEME_FALSEP(argv[0])) {
    block_on_waitable(ws[0], argv[1], 0);
    return argv[1];
  }

  /* Normal case: wait for more than one thing */
  waiting = MALLOC_ONE_RT(Waiting);
#ifdef MZTAG_REQUIRED
  waiting->type = scheme_rt_waiting;
#endif
  waiting->argc = argc - 1;
  waiting->ws = ws;
  args = MALLOC_N(Scheme_Object*, argc-1);
  for (i = 1; i < argc; i++) {
    args[i - 1] = argv[i];
  }
  waiting->argv = args;
  waiting->timeout = timeout;
  waiting->start_time = start_time;
  
  if (timeout < 0.0)
    timeout = 0.0; /* means "no timeout" to block_until */

  if (!waiting_ready((Scheme_Object *)waiting)) {
    scheme_block_until(waiting_ready, waiting_needs_wakeup, (Scheme_Object *)waiting, timeout);
  }

  if (waiting->result)
    return waiting->result;
  else
    return scheme_false;
}

static Scheme_Object *object_wait_break(int argc, Scheme_Object *argv[])
{
  if (argc == 2 && SCHEME_FALSEP(argv[0]) && SCHEME_SEMAP(argv[1])) {
    scheme_wait_sema(argv[1], -1);
    return scheme_void;
  }

  return scheme_call_enable_break(scheme_object_wait_multiple, argc, argv);
}

/*========================================================================*/
/*                              parameters                                */
/*========================================================================*/

Scheme_Object *scheme_branch_config(void)
{
  return scheme_make_config(scheme_config);
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
  Scheme_Object *guard, **argv2, *kv;

  if (argc && argv[0]) {
    guard = ((ParamData *)data)->guard;
    if (guard) {
      Scheme_Object *v;
      
      v = _scheme_apply(guard, 1, argv);
      argv2 = MALLOC_N(Scheme_Object *, argc);
      memcpy(argv2, argv, argc * sizeof(Scheme_Object *));
      argv2[0] = v;
    } else
      argv2 = argv;
  } else
    argv2 = argv;    

  kv = scheme_make_pair(((ParamData *)data)->key,
			((ParamData *)data)->defval);
  
  return scheme_param_config("parameter-procedure", 
			     kv,
			     argc, argv2,
			     -2, NULL, NULL, 0);
}

static Scheme_Object *make_parameter(int argc, Scheme_Object **argv)
{
  Scheme_Object *p;
  ParamData *data;
  void *k;

  k = scheme_make_pair(scheme_true, scheme_false); /* generates a key */

  if (argc > 1)
    scheme_check_proc_arity("make-parameter", 1, 1, argc, argv);

  data = MALLOC_ONE_RT(ParamData);
#ifdef MZTAG_REQUIRED
  data->type = scheme_rt_param_data;
#endif
  data->key = (Scheme_Object *)k;
  data->defval = argv[0];
  data->guard = ((argc > 1) ? argv[1] : NULL);

  p = scheme_make_closed_prim_w_arity(do_param, (void *)data, 
				      "parameter-procedure", 0, 1);
  ((Scheme_Primitive_Proc *)p)->flags |= SCHEME_PRIM_IS_PARAMETER;

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
  config->type = scheme_config_type;
  
  scheme_set_param(config, MZCONFIG_ENABLE_BREAK, scheme_false);
  scheme_set_param(config, MZCONFIG_CAN_READ_GRAPH, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_COMPILED, scheme_false);
  scheme_set_param(config, MZCONFIG_CAN_READ_BOX, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_DOT, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_QUASI, scheme_true);
  scheme_set_param(config, MZCONFIG_READ_DECIMAL_INEXACT, scheme_true);

  scheme_set_param(config, MZCONFIG_PRINT_GRAPH, scheme_false);
  scheme_set_param(config, MZCONFIG_PRINT_STRUCT, scheme_false);
  scheme_set_param(config, MZCONFIG_PRINT_BOX, scheme_true);
  scheme_set_param(config, MZCONFIG_PRINT_VEC_SHORTHAND, scheme_true);

  {
    Scheme_Object *s;
    s = scheme_make_immutable_sized_string("", 0, 0);
    scheme_set_param(config, MZCONFIG_LOCALE, s);
  }

  scheme_set_param(config, MZCONFIG_CASE_SENS, (scheme_case_sensitive ? scheme_true : scheme_false));
  scheme_set_param(config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, (scheme_square_brackets_are_parens
								 ? scheme_true : scheme_false));
  scheme_set_param(config, MZCONFIG_CURLY_BRACES_ARE_PARENS, (scheme_curly_braces_are_parens
							      ? scheme_true : scheme_false));

  scheme_set_param(config, MZCONFIG_ERROR_PRINT_WIDTH, scheme_make_integer(100));
  scheme_set_param(config, MZCONFIG_ERROR_PRINT_SRCLOC, scheme_true);

  REGISTER_SO(main_custodian);
  main_custodian = scheme_make_custodian(NULL);
  scheme_set_param(config, MZCONFIG_CUSTODIAN, (Scheme_Object *)main_custodian);

  scheme_set_param(config, MZCONFIG_ALLOW_SET_UNDEFINED, (scheme_allow_set_undefined
							  ? scheme_true
							  : scheme_false));

  scheme_set_param(config, MZCONFIG_COLLECTION_PATHS,  scheme_null);

  {
    Scheme_Object *s;
    s = scheme_make_string(scheme_os_getcwd(NULL, 0, NULL, 1));
    scheme_set_param(config, MZCONFIG_CURRENT_DIRECTORY, s);
  }

  {
    Scheme_Object *rs;
    rs = scheme_make_random_state(scheme_get_milliseconds());
    scheme_set_param(config, MZCONFIG_RANDOM_STATE, rs);
  }

  {
    Scheme_Object *eh;
    eh = scheme_make_prim_w_arity2(scheme_default_eval_handler,
				   "default-eval-handler",
				   1, 1,
				   0, -1);
    scheme_set_param(config, MZCONFIG_EVAL_HANDLER, eh);
  }
  
  {
    Scheme_Object *ph, *prh;

    ph = scheme_make_prim_w_arity(scheme_default_print_handler,
				  "default-print-handler",
				  1, 1);
    scheme_set_param(config, MZCONFIG_PRINT_HANDLER, ph);

    prh = scheme_make_prim_w_arity(scheme_default_prompt_read_handler,
				   "default-prompt-read-handler",
				   0, 0);
    scheme_set_param(config, MZCONFIG_PROMPT_READ_HANDLER, prh);
  }

  {
    Scheme_Object *lh;
    lh = scheme_make_prim_w_arity2(scheme_default_load_extension,
				   "default-load-extension-handler",
				   2, 2,
				   0, -1);
    scheme_set_param(config, MZCONFIG_LOAD_EXTENSION_HANDLER, lh);
  }

  {
    Scheme_Object *ins;
    ins = scheme_make_initial_inspectors();
    scheme_set_param(config, MZCONFIG_INSPECTOR, ins);
  }
  
  {
    Scheme_Object *zlv;
    zlv = scheme_make_vector(0, NULL);
    scheme_set_param(config, MZCONFIG_CMDLINE_ARGS, zlv);
  }

  {
    Scheme_Security_Guard *sg;

    sg = MALLOC_ONE_TAGGED(Scheme_Security_Guard);
    sg->type = scheme_security_guard_type;
    scheme_set_param(config, MZCONFIG_SECURITY_GUARD, (Scheme_Object *)sg);
  }

  config->use_count = NULL;
  config->extensions = NULL;

  return config;
}

Scheme_Object *scheme_make_config(Scheme_Config *base)
{
  Scheme_Config *config;
  int i;
  
  if (!base)
    base = scheme_config;

  config = (Scheme_Config *)scheme_malloc_tagged(sizeof(Scheme_Config) + 
						 (max_configs - 1) * sizeof(Scheme_Object*));

  config->type = scheme_config_type;
  
  for (i = 0; i < max_configs; i++) {
    config->configs[i] = base->configs[i];
  }


  if (base->extensions) {
    config->extensions = base->extensions;
    config->use_count = base->use_count;
    *(base->use_count) += 1;
  } else {
    config->extensions = NULL;
    config->use_count = NULL;
  }

  return (Scheme_Object *)config;
}

Scheme_Object *scheme_register_parameter(Scheme_Prim *function, char *name, int which)
{
  Scheme_Object *o;

  if (!config_map) {
    REGISTER_SO(config_map);
    config_map = MALLOC_N(Scheme_Object*, max_configs);
  }

  if (config_map[which])
    return config_map[which];

  o = scheme_make_prim_w_arity(function, name, 0, 1);
  ((Scheme_Primitive_Proc *)o)->flags |= SCHEME_PRIM_IS_PARAMETER;

  config_map[which] = o;

  return o;
}

typedef Scheme_Object *(*PCheck_Proc)(int, Scheme_Object **, Scheme_Config *);

Scheme_Object *scheme_param_config(char *name, Scheme_Object *pos,
				   int argc, Scheme_Object **argv,
				   int arity,
				   /* -3 => like -1, plus use check to unmarshall the value
                                      -2 => user parameter; pos is (cons key defval)
				      -1 => use check; if isboolorfilter, check is a filter
                                            (and expected is ignored), and if check is NULL,
                                            parameter is boolean-valued
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
      Scheme_Object *defval = SCHEME_CDR(pos);
      if (config->extensions) {
	const char *key = (const char *)SCHEME_CAR(pos);
	Scheme_Bucket *b;

	b = scheme_bucket_or_null_from_table(config->extensions, key, 0);
	if (b)
	  return (Scheme_Object *)b->val;
	else
	  return defval;
      }
      return defval;
    } else {
      Scheme_Object *s = scheme_get_param(config, SCHEME_INT_VAL(pos));
      if (arity == -3) {
	Scheme_Object *a[1];
	PCheck_Proc checkp = (PCheck_Proc)check;
	a[0] = s;
	s = checkp(1, a, config);
      }
      return s;
    }
  } else {
    Scheme_Object *naya = argv[0];

    if (arity != -2) {
      if (arity < 0) {
	if (check) {
	  PCheck_Proc checkp = (PCheck_Proc)check;
	  Scheme_Object *r;

	  r = checkp(1, argv, config);
	  
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
	scheme_set_param(config, SCHEME_INT_VAL(pos), ((SCHEME_TRUEP(naya)) ? scheme_true : scheme_false));
      else
	scheme_set_param(config, SCHEME_INT_VAL(pos), naya);
    } else {
      const char *key = (const char *)SCHEME_CAR(pos);
      Scheme_Bucket *b;

      if (!config->extensions) {
	Scheme_Bucket_Table *ht;
	int *use_count;
	ht = scheme_make_bucket_table(2, SCHEME_hash_weak_ptr);
	config->extensions = ht;
	use_count = MALLOC_ONE_ATOMIC(int);
	*use_count = 0;
	config->use_count = use_count;
      }

      if (*(config->use_count)) {
	/* Copy-on-write! */
	int *use_count;
	Scheme_Bucket **bs = config->extensions->buckets;
	int i = config->extensions->size;
	Scheme_Bucket_Table *ht;
	
	ht = scheme_make_bucket_table(2, SCHEME_hash_weak_ptr);
	
	config->extensions = ht;
	
	while (i--) {
	  b = bs[i];
	  if (b && b->val && b->key && HT_EXTRACT_WEAK(b->key))
	    scheme_add_to_table(config->extensions, (const char *)HT_EXTRACT_WEAK(b->key), b->val, 0);
	}
      
	use_count = MALLOC_ONE_ATOMIC(int);
	*use_count = 0;
	config->use_count = use_count;
      }

      b = scheme_bucket_from_table(config->extensions, key);
      b->val = naya;
    }
  
    return scheme_void;
  }
}

/*========================================================================*/
/*                              namespaces                                */
/*========================================================================*/

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

Scheme_Env *scheme_get_env(Scheme_Config *c)
{
  Scheme_Object *o = scheme_get_param(c, MZCONFIG_ENV);
  return (Scheme_Env *)o;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void scheme_add_namespace_option(Scheme_Object *key, void (*f)(Scheme_Env *))
{
  Scheme_NSO *old = namespace_options;
  
  namespace_options = MALLOC_N_RT(Scheme_NSO, (num_nsos + 1));

  memcpy(namespace_options, old, num_nsos * sizeof(Scheme_NSO));

#ifdef MZTAG_REQUIRED
  namespace_options[num_nsos].type = scheme_rt_namespace_option;
#endif
  namespace_options[num_nsos].key = key;
  namespace_options[num_nsos].f = f;
  
  num_nsos++;
}

Scheme_Object *scheme_make_namespace(int argc, Scheme_Object *argv[])
{
  int empty = 0;
  Scheme_Env *env;

  if (argc) {
    if (SAME_OBJ(argv[0], empty_symbol))
      empty = 1;
    else if (SAME_OBJ(argv[0], initial_symbol))
      empty = 0;
    else
      scheme_wrong_type("make-namespace", "'empty or 'initial", 0, argc, argv);
  }
  
  env = scheme_make_empty_env();
  if (!empty) {
    /* Copy from initial namespace: */
    scheme_install_initial_module_set(env);
  }

  return (Scheme_Object *)env;
}

static Scheme_Object *namespace_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_namespace_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_namespace(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-namespace", 
			     scheme_make_integer(MZCONFIG_ENV),
			     argc, argv,
			     -1, namespace_p, "namespace", 0);
}

/*========================================================================*/
/*                           security guards                              */
/*========================================================================*/

static Scheme_Object *make_security_guard(int argc, Scheme_Object *argv[])
{
  Scheme_Security_Guard *sg;

  if (!(SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_security_guard_type)))
    scheme_wrong_type("make-security-guard", "security-guard", 0, argc, argv);
  scheme_check_proc_arity("make-security-guard", 3, 1, argc, argv);
  scheme_check_proc_arity("make-security-guard", 4, 2, argc, argv);

  sg = MALLOC_ONE_TAGGED(Scheme_Security_Guard);
  sg->type = scheme_security_guard_type;
  sg->parent = (Scheme_Security_Guard *)argv[0];
  sg->file_proc = argv[1];
  sg->network_proc = argv[2];

  return (Scheme_Object *)sg;
}

static Scheme_Object *security_guard_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_security_guard_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_security_guard(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-security-guard", 
			     scheme_make_integer(MZCONFIG_SECURITY_GUARD),
			     argc, argv,
			     -1, security_guard_p, "security-guard", 0);
}


void scheme_security_check_file(const char *who, const char *filename, int guards)
{
  Scheme_Security_Guard *sg;

  sg = (Scheme_Security_Guard *)scheme_get_param(scheme_config, MZCONFIG_SECURITY_GUARD);

  if (sg->file_proc) {
    Scheme_Object *l = scheme_null, *a[3];

    if (!read_symbol) {
      REGISTER_SO(read_symbol);
      REGISTER_SO(write_symbol);
      REGISTER_SO(execute_symbol);
      REGISTER_SO(delete_symbol);
      REGISTER_SO(exists_symbol);

      read_symbol = scheme_intern_symbol("read");
      write_symbol = scheme_intern_symbol("write");
      execute_symbol = scheme_intern_symbol("execute");
      delete_symbol = scheme_intern_symbol("delete");
      exists_symbol = scheme_intern_symbol("exists");
    }

    if (guards & SCHEME_GUARD_FILE_EXISTS)
      l = scheme_make_immutable_pair(exists_symbol, l);
    if (guards & SCHEME_GUARD_FILE_DELETE)
      l = scheme_make_immutable_pair(delete_symbol, l);
    if (guards & SCHEME_GUARD_FILE_EXECUTE)
      l = scheme_make_immutable_pair(execute_symbol, l);
    if (guards & SCHEME_GUARD_FILE_WRITE)
      l = scheme_make_immutable_pair(write_symbol, l);
    if (guards & SCHEME_GUARD_FILE_READ)
      l = scheme_make_immutable_pair(read_symbol, l);

    a[0] = scheme_intern_symbol(who);
    a[1] = (filename ? scheme_make_immutable_sized_string((char *)filename, -1, 1) : scheme_false);
    a[2] = l;

    while (sg->parent) {
      scheme_apply(sg->file_proc, 3, a);
      sg = sg->parent;
    }
  }
}

void scheme_security_check_network(const char *who, const char *host, int port, int client)
{
  Scheme_Security_Guard *sg;

  sg = (Scheme_Security_Guard *)scheme_get_param(scheme_config, MZCONFIG_SECURITY_GUARD);

  if (sg->network_proc) {
    Scheme_Object *a[4];

    if (!client_symbol) {
      REGISTER_SO(client_symbol);
      REGISTER_SO(server_symbol);

      client_symbol = scheme_intern_symbol("client");
      server_symbol = scheme_intern_symbol("server");
    }

    a[0] = scheme_intern_symbol(who);
    a[1] = (host ? scheme_make_immutable_sized_string((char *)host, -1, 1) : scheme_false);
    a[2] = scheme_make_integer(port);
    a[3] = (client ? client_symbol : server_symbol);

    while (sg->parent) {
      scheme_apply(sg->network_proc, 4, a);
      sg = sg->parent;
    }
  }
}

/*========================================================================*/
/*                         wills and will executors                       */
/*========================================================================*/

typedef struct ActiveWill {
  MZTAG_IF_REQUIRED
  Scheme_Object *o;
  Scheme_Object *proc;
  struct WillExecutor *w;  /* Set to will executor when executed */
  struct ActiveWill *next;
} ActiveWill;

typedef struct WillExecutor {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *sema;
  ActiveWill *first, *last;
} WillExecutor;

typedef struct WillRegistration {
  MZTAG_IF_REQUIRED
  Scheme_Object *proc;
  WillExecutor *w;
} WillRegistration;

static void activate_will(void *o, void *data) 
{
  WillRegistration *r = (WillRegistration *)data;
  ActiveWill *a;
  WillExecutor *w;
    
  a = MALLOC_ONE_RT(ActiveWill);
#ifdef MZTAG_REQUIRED
  a->type = scheme_rt_will;
#endif
  a->o = (Scheme_Object *)o;
  a->proc = r->proc;
  
  w = r->w;
  if (w->last)
    w->last->next = a;
  else
    w->first = a;
  w->last = a;
  scheme_post_sema(w->sema);
}

static Scheme_Object *do_next_will(WillExecutor *w)
{
  ActiveWill *a;
  Scheme_Object *o[1];

  a = w->first;
  w->first = a->next;
  if (!w->first)
    w->last = NULL;
  
  o[0] = a->o;
  a->o = NULL;

  return scheme_apply_multi(a->proc, 1, o);
}

static Scheme_Object *make_will_executor(int argc, Scheme_Object **argv)
{
  WillExecutor *w;
  Scheme_Object *sema;

  w = MALLOC_ONE_TAGGED(WillExecutor);
  sema = scheme_make_sema(0);

  w->type = scheme_will_executor_type;
  w->first = NULL;
  w->last = NULL;
  w->sema = sema;

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

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_type("will-register", "will-executor", 0, argc, argv);
  scheme_check_proc_arity("will-register", 1, 2, argc, argv);

  r = MALLOC_ONE_RT(WillRegistration);
#ifdef MZTAG_REQUIRED
  r->type = scheme_rt_will_registration;
#endif
  r->proc = argv[2];
  r->w = (WillExecutor *)argv[0];

  scheme_add_scheme_finalizer(argv[1], activate_will, (void *)r);

  return scheme_void;
}

static Scheme_Object *will_executor_try(int argc, Scheme_Object **argv)
{
  WillExecutor *w;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_type("will-try-execute", "will-executor", 0, argc, argv);
  
  w = (WillExecutor *)argv[0];

  if (scheme_wait_sema(w->sema, 1))
    return do_next_will(w);
  else
    return scheme_false;
}

static Scheme_Object *will_executor_go(int argc, Scheme_Object **argv)
{
  WillExecutor *w;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_type("will-execute", "will-executor", 0, argc, argv);
  
  w = (WillExecutor *)argv[0];

  scheme_wait_sema(w->sema, 0);

  return do_next_will(w);
}

static Scheme_Object *will_executor_sema(Scheme_Object *w, int *repost)
{
  *repost = 1;
  return ((WillExecutor *)w)->sema;
}

/*========================================================================*/
/*                         GC preparation and timing                      */
/*========================================================================*/

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

void scheme_zero_unneeded_rands(Scheme_Thread *p)
{
  /* Call this procedure before GC or before copying out
     a thread's stack. */
}

static void prepare_thread_for_GC(Scheme_Object *t)
{
  Scheme_Thread *p = (Scheme_Thread *)t;

  /* zero ununsed part of env stack in each thread */

  if (!p->nestee) {
    Scheme_Object **o, **e, **e2;
    Scheme_Saved_Stack *saved;
# define RUNSTACK_TUNE(x) /* x   - Used for performance tuning */
    RUNSTACK_TUNE( long size; );

    o = p->runstack_start;
    e = p->runstack;
    e2 = p->runstack_tmp_keep;

    while (o < e && (o != e2)) {
      *(o++) = NULL;
    }

    RUNSTACK_TUNE( size = p->runstack_size - (p->runstack - p->runstack_start); );

    for (saved = p->runstack_saved; saved; saved = saved->prev) {
      o = saved->runstack_start;
      e = saved->runstack;
      RUNSTACK_TUNE( size += saved->runstack_size; );
      while (o < e) {
	*(o++) = NULL;
      }
    }

    RUNSTACK_TUNE( printf("%ld\n", size); );

    if (p->tail_buffer && (p->tail_buffer != p->runstack_tmp_keep)) {
      int i;
      for (i = 0; i < p->tail_buffer_size; i++) {
	p->tail_buffer[i] = NULL;
      }
    }
  }
      
  /* release unused cont mark stack segments */
  {
    int segcount, i;
    if (p->cont_mark_stack)
      segcount = ((long)(p->cont_mark_stack - 1) >> SCHEME_LOG_MARK_SEGMENT_SIZE) + 1;
    else
      segcount = 0;
    for (i = segcount; i < p->cont_mark_seg_count; i++) {
      p->cont_mark_stack_segments[i] = NULL;
    }
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

  if (p->values_buffer)
    memset(p->values_buffer, 0, sizeof(Scheme_Object*) * p->values_buffer_size);

  /* zero ununsed part of list stack */
  scheme_clean_list_stack(p);
}

static void get_ready_for_GC()
{
  start_this_gc_time = scheme_get_process_milliseconds();

  scheme_zero_unneeded_rands(scheme_current_thread);

  scheme_clear_modidx_cache();

#ifdef RUNSTACK_IS_GLOBAL
  scheme_current_thread->runstack = MZ_RUNSTACK;
  scheme_current_thread->runstack_start = MZ_RUNSTACK_START;
  scheme_current_thread->cont_mark_stack = MZ_CONT_MARK_STACK;
  scheme_current_thread->cont_mark_pos = MZ_CONT_MARK_POS;
#endif

  if (scheme_fuel_counter) {
    for_each_managed(scheme_thread_type, main_custodian, prepare_thread_for_GC);
  }
   
  scheme_fuel_counter = 0;

#ifdef WINDOWS_PROCESSES
  scheme_suspend_remembered_threads();
#endif
#ifdef UNIX_PROCESSES
  scheme_block_child_signals(1);
#endif

  delayed_break_ready = 0;
  delay_breaks = 1;
}

extern int GC_words_allocd;

static void done_with_GC()
{
#ifdef RUNSTACK_IS_GLOBAL
# ifdef MZ_PRECISE_GC
  MZ_RUNSTACK = scheme_current_thread->runstack;
# endif
#endif
#ifdef WINDOWS_PROCESSES
  scheme_resume_remembered_threads();
#endif
#ifdef UNIX_PROCESSES
  scheme_block_child_signals(0);
#endif

  delay_breaks = 0;
  if (delayed_break_ready)
    scheme_break_thread(NULL);

  scheme_total_gc_time += (scheme_get_process_milliseconds() - start_this_gc_time);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

/*========================================================================*/
/*                               precise GC                               */
/*========================================================================*/

#ifdef MZ_PRECISE_GC
static unsigned long get_current_stack_start(void)
{
  return (unsigned long)scheme_current_thread->stack_start;
}
#endif

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_THREAD_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_config_type, mark_config_val);
  GC_REG_TRAV(scheme_will_executor_type, mark_will_executor_val);
  GC_REG_TRAV(scheme_custodian_type, mark_custodian_val);
  GC_REG_TRAV(scheme_thread_hop_type, mark_thread_hop);

  GC_REG_TRAV(scheme_rt_namespace_option, mark_namespace_option);
  GC_REG_TRAV(scheme_rt_param_data, mark_param_data);
  GC_REG_TRAV(scheme_rt_will, mark_will);
  GC_REG_TRAV(scheme_rt_will_registration, mark_will_registration);
  GC_REG_TRAV(scheme_rt_waitable, mark_waitable);
  GC_REG_TRAV(scheme_rt_waiting, mark_waiting);
}

END_XFORM_SKIP;

#endif
