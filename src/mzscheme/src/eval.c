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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file contains 

        * the main eval-apply loop, in scheme_do_eval()

        * the main compile loop, int scheme_compile_expand_expr()

        * compilation and bytecode [un]marshaling for
             - applications
             - sequences (along with code in syntax.c)
             - branches (along with code in syntax.c)
             - with-continuation-mark
           [These are here instead of syntax.c because they are
            tightly integrated into the evaluation loop.]

        * C and Scheme stack management routines

   Evaluation:

   The bytecode evaluator uses the C stack for continuations, and a
   separate Scheme stack for activation-frame variables and collecting
   application arguments. Closures are flat, so mutable variables are
   boxed. A third stack is used for continuation marks, only as
   needed.

   Tail calls are, for the most part, gotos within scheme_do_eval(). A
   C function called y the main evaluation loop can perform a
   trampoling tail call via scheme_tail_apply.

   The apply half of the eval-apply loop branches on all possible
   application types. All primitive functions (including cons) are
   implemented by C functions outside the loop. Continuation
   applications are handled directly in scheme_do_eval(). That leaves
   calls to closures, which are also performed within scheme_do_eval()
   (so that most tail calls avoid the trampoline).

   The eval half of the loop detects a limited set of core syntactic
   forms, such as application and letrecs. Otherwise, it dispatches to
   external functions to implement elaborate syntactic forms, such as
   class and unit expressions.

   When collecting the arguments for an application, scheme_do_eval()
   avoids recursive C calls to evaluate arguments by recogzining
   easily-evaluated expressions, such as constrants and variable
   lookups. This can be viewed as a kind of half-way A-normalization.

   Bytecodes are not linear, but actually trees of expression nodes.

   Compilation:

   Compilation works in two passes. The first pass, called "compile",
   performs most of the work and tracks variable usage (including
   whether a variable is mutated or not). The second pass, called
   "resolve", finishes compilation by computing variable offsets and
   indirections (often mutating the records produced by the first
   pass).

   Linking:

   Linking matches compiled code to a top-level environment. It never
   mutates records produced by the first pass. */

#include "schpriv.h"
#include "schrunst.h"

#ifdef USE_STACKAVAIL
#include <malloc.h>
#endif
#ifdef UNIX_FIND_STACK_BOUNDS
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef BEOS_FIND_STACK_BOUNDS
# include <be/kernel/OS.h>
#endif
#ifdef OSKIT_FIXED_STACK_BOUNDS
# include <oskit/machine/base_stack.h>
#endif
#include "schmach.h"
#ifdef MACOS_STACK_LIMIT
#include <Memory.h>
#endif

#define EMBEDDED_DEFINES_START_ANYWHERE 1

/* globals */
int scheme_allow_cond_auto_else = 1;

Scheme_Object *scheme_eval_waiting;
Scheme_Object *scheme_multiple_values;

#ifndef MZ_REAL_THREADS
volatile int scheme_fuel_counter;
#endif

int scheme_stack_grows_up;

static Scheme_Object *app_symbol;
static Scheme_Object *datum_symbol;
static Scheme_Object *unbound_symbol;

static Scheme_Object *stop_expander;

/* locals */
static Scheme_Object *eval(int argc, Scheme_Object *argv[]);
static Scheme_Object *compile(int argc, Scheme_Object *argv[]);
static Scheme_Object *compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand(int argc, Scheme_Object **argv);
static Scheme_Object *expand_once(int argc, Scheme_Object **argv);
static Scheme_Object *enable_break(int, Scheme_Object *[]);
static Scheme_Object *current_eval(int argc, Scheme_Object *[]);

static Scheme_Object *allow_auto_cond_else(int argc, Scheme_Object **argv);
static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv);

static Scheme_Object *app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *app_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *unbound_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *unbound_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *write_application(Scheme_Object *obj);
static Scheme_Object *read_application(Scheme_Object *obj);
static Scheme_Object *write_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence_save_first(Scheme_Object *obj);
static Scheme_Object *write_branch(Scheme_Object *obj);
static Scheme_Object *read_branch(Scheme_Object *obj);
static Scheme_Object *write_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *read_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *write_syntax(Scheme_Object *obj);
static Scheme_Object *read_syntax(Scheme_Object *obj);

static Scheme_Object *define_values_symbol, *letrec_values_symbol, *lambda_symbol;
static Scheme_Object *unknown_symbol, *void_link_symbol, *quote_symbol;
static Scheme_Object *letmacro_symbol, *begin_symbol;
static Scheme_Object *let_symbol;

static Scheme_Object *zero_rands_ptr; /* &zero_rands_ptr is dummy rands pointer */

static Scheme_Object *scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
						 Scheme_Compile_Info *rec, int drec, 
						 int depth, Scheme_Object *boundname,
						 int app_position);

#define cons(x,y) scheme_make_pair(x,y)

typedef void (*DW_PrePost_Proc)(void *);

#define TAIL_COPY_THRESHOLD 5

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) \
    || defined(MACOS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE) \
    || defined(BEOS_FIND_STACK_BOUNDS) || defined(OSKIT_FIXED_STACK_BOUNDS) \
    || defined(PALM_FIND_STACK_BOUNDS)
# ifndef MZ_REAL_THREADS
unsigned long scheme_stack_boundary;
# endif
#endif

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* Lookahead types for evaluating application arguments. */
/* 4 cases + else => magic number for some compilers doing a switch */
enum {
  SCHEME_EVAL_CONSTANT = 0,
  SCHEME_EVAL_GLOBAL,
  SCHEME_EVAL_LOCAL,
  SCHEME_EVAL_LOCAL_UNBOX,
  SCHEME_EVAL_GENERAL
};

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_eval (Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
  scheme_eval_waiting = MZ_EVAL_WAITING_CONSTANT;
#else
  REGISTER_SO(scheme_eval_waiting);
  scheme_eval_waiting = scheme_alloc_eternal_object();
  scheme_eval_waiting->type = scheme_eval_waiting_type;
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
  scheme_multiple_values = MZ_MULTIPLE_VALUES_CONSTANT;
#else
  REGISTER_SO(scheme_multiple_values);
  scheme_multiple_values = scheme_alloc_eternal_object();
  scheme_multiple_values->type = scheme_multiple_values_type;
#endif

  REGISTER_SO(define_values_symbol);
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(lambda_symbol);
  REGISTER_SO(unknown_symbol);
  REGISTER_SO(void_link_symbol);
  REGISTER_SO(quote_symbol);
  REGISTER_SO(letmacro_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(let_symbol);
  
  define_values_symbol = scheme_intern_symbol("define-values");
  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_symbol = scheme_intern_symbol("let");
  lambda_symbol = scheme_intern_symbol("lambda");
  unknown_symbol = scheme_intern_symbol("unknown");
  void_link_symbol = scheme_intern_symbol("-v");
  quote_symbol = scheme_intern_symbol("quote");
  letmacro_symbol = scheme_intern_symbol("letrec-syntax");
  begin_symbol = scheme_intern_symbol("begin");
  
  scheme_install_type_writer(scheme_application_type, write_application);
  scheme_install_type_reader(scheme_application_type, read_application);
  scheme_install_type_writer(scheme_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_sequence_type, read_sequence);
  scheme_install_type_writer(scheme_branch_type, write_branch);
  scheme_install_type_reader(scheme_branch_type, read_branch);
  scheme_install_type_writer(scheme_with_cont_mark_type, write_with_cont_mark);
  scheme_install_type_reader(scheme_with_cont_mark_type, read_with_cont_mark);
  scheme_install_type_writer(scheme_syntax_type, write_syntax);
  scheme_install_type_reader(scheme_syntax_type, read_syntax);
  
  scheme_install_type_writer(scheme_begin0_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_begin0_sequence_type, read_sequence_save_first);
  
  scheme_add_global_constant("eval", 
			     scheme_make_prim_w_arity2(eval, 
						       "eval", 
						       1, 2,
						       0, -1), 
			     env);
  scheme_add_global_constant("compile", 
			     scheme_make_prim_w_arity(compile, 
						      "compile", 
						      1, 1), 
			     env);
  scheme_add_global_constant("compiled-expression?",
			     scheme_make_prim_w_arity(compiled_p, 
						      "compiled-expression?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("expand", 
			     scheme_make_prim_w_arity(expand, 
						      "expand",
						      1, 1), 
			     env);
  scheme_add_global_constant("local-expand", 
			     scheme_make_prim_w_arity(local_expand, 
						      "local-expand",
						      2, 2), 
			     env);
  scheme_add_global_constant("expand-once", 
			     scheme_make_prim_w_arity(expand_once, 
						      "expand-once", 
						      1, 1), 
			     env);
  scheme_add_global_constant("break-enabled", 
			     scheme_register_parameter(enable_break, 
						       "break-enabled",
						       MZCONFIG_ENABLE_BREAK), 
			     env);
  scheme_add_global_constant("current-eval",
			     scheme_register_parameter(current_eval, 
						       "current-eval",
						       MZCONFIG_EVAL_HANDLER),
			     env);

  scheme_add_global_constant("compile-allow-cond-fallthrough", 
			     scheme_register_parameter(allow_auto_cond_else, 
						       "compile-allow-cond-fallthrough",
						       MZCONFIG_COND_AUTO_ELSE), 
			     env);
  scheme_add_global_constant("compile-allow-set!-undefined", 
			     scheme_register_parameter(allow_set_undefined, 
						       "compile-allow-set!-undefined",
						       MZCONFIG_ALLOW_SET_UNDEFINED), 
			     env);

  REGISTER_SO(app_symbol);
  REGISTER_SO(datum_symbol);
  REGISTER_SO(unbound_symbol);

  app_symbol = scheme_intern_symbol("#%app");
  datum_symbol = scheme_intern_symbol("#%datum");
  unbound_symbol = scheme_intern_symbol("#%unbound");

  scheme_add_global_keyword("#%app", 
			    scheme_make_compiled_syntax(app_syntax,
							app_expand), 
			    env);
  scheme_add_global_keyword("#%datum", 
			    scheme_make_compiled_syntax(datum_syntax,
							datum_expand), 
			    env);
  scheme_add_global_keyword("#%unbound", 
			    scheme_make_compiled_syntax(unbound_syntax,
							unbound_expand), 
			    env);
}

/*========================================================================*/
/*                   C stack and Scheme stack handling                    */
/*========================================================================*/

#ifndef MZ_REAL_THREADS
# define DO_CHECK_FOR_BREAK(p, e) \
	if (DECREMENT_FUEL(scheme_fuel_counter, 1) <= 0) { \
	  e scheme_process_block(0); \
          (p)->ran_some = 1; \
	}
#else
# define DO_CHECK_FOR_BREAK(p, e) \
	if (DECREMENT_FUEL((p)->fuel_counter, 1) <= 0) { \
	  e scheme_process_block_w_process(0, p); \
	}
#endif

Scheme_Object *
scheme_handle_stack_overflow(Scheme_Object *(*k)(void))
{
  scheme_overflow_k = k;
  scheme_init_jmpup_buf(&scheme_overflow_cont);
  scheme_zero_unneeded_rands(scheme_current_process);
  if (scheme_setjmpup(&scheme_overflow_cont, scheme_current_process,
		      scheme_current_process->cc_start)) {
    scheme_init_jmpup_buf(&scheme_overflow_cont);
    if (!scheme_overflow_reply) {
      scheme_longjmp(scheme_error_buf, 1);
    } else
      return scheme_overflow_reply;
  } else
    scheme_longjmp(scheme_current_process->overflow_buf, 1);
  return NULL; /* never gets here */
}

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE)
extern unsigned long GC_get_stack_base();
#endif

void scheme_init_stack_check()
{
  int *v;
  unsigned long deeper;
#ifdef UNIX_FIND_STACK_BOUNDS
  struct rlimit rl;
#endif
  
  deeper = scheme_get_deeper_address();
  scheme_stack_grows_up = (deeper > (unsigned long)&v);

#ifdef STACK_GROWS_UP
  if (!scheme_stack_grows_up) {
    if (scheme_console_printf)
      scheme_console_printf("Stack grows DOWN, not UP.\n");
    else
      printf("Stack grows DOWN, not UP.\n");
    exit(1);
  }
#endif
#ifdef STACK_GROWS_DOWN
  if (scheme_stack_grows_up) {
    if (scheme_console_printf)
      scheme_console_printf("Stack grows UP, not DOWN.\n");
    else
      printf("Stack grows UP, not DOWN.\n");
    exit(1);
  }
#endif

#ifdef ASSUME_FIXED_STACK_SIZE
  scheme_stack_boundary = GC_get_stack_base();
  if (scheme_stack_grows_up)
    scheme_stack_boundary += (FIXED_STACK_SIZE - STACK_SAFETY_MARGIN);
  else
    scheme_stack_boundary += (STACK_SAFETY_MARGIN - FIXED_STACK_SIZE);
#endif

#ifdef WINDOWS_FIND_STACK_BOUNDS
  scheme_stack_boundary = GC_get_stack_base();
  scheme_stack_boundary += (STACK_SAFETY_MARGIN - 0x100000);
#endif

#ifdef MACOS_FIND_STACK_BOUNDS
  scheme_stack_boundary = (unsigned long)&v +  STACK_SAFETY_MARGIN - StackSpace();
#endif

#ifdef PALMOS_FIND_STACK_BOUNDS
  {
    Ptr s, e;
    SysGetStackInfo(Ptr &s, &e);
    scheme_stack_boundary = (unsigned long)e + STACK_SAFETY_MARGIN;
  }
#endif

#ifdef BEOS_FIND_STACK_BOUNDS
  {
    thread_info info;
    get_thread_info(find_thread(NULL), &info);
    scheme_stack_boundary = (unsigned long)info.stack_base + STACK_SAFETY_MARGIN;
  }
#endif

#ifdef OSKIT_FIXED_STACK_BOUNDS
  scheme_stack_boundary = (unsigned long)base_stack_start + STACK_SAFETY_MARGIN;
#endif

#ifdef UNIX_FIND_STACK_BOUNDS
  getrlimit(RLIMIT_STACK, &rl);

  {
    unsigned long bnd;
    bnd = (unsigned long)GC_get_stack_base();

    if (scheme_stack_grows_up)
      bnd += ((unsigned long)rl.rlim_cur - STACK_SAFETY_MARGIN);
    else
      bnd += (STACK_SAFETY_MARGIN - (unsigned long)rl.rlim_cur);

# ifndef MZ_REAL_THREADS
    scheme_stack_boundary = bnd;
# else
    scheme_current_process->stack_end = (void *)bnd;
# endif
  }
#endif
}


int scheme_check_runstack(long size)
{
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Process *p = scheme_current_process;
#endif

  return ((MZ_RUNSTACK - MZ_RUNSTACK_START) >= (size + TAIL_COPY_THRESHOLD));
}

void *scheme_enlarge_runstack(long size, void *(*k)())
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Saved_Stack *saved;
  void *v;

  saved = MALLOC_ONE_RT(Scheme_Saved_Stack);

#ifdef MZTAG_REQUIRED
  saved->type = scheme_rt_saved_stack;
#endif
  saved->prev = p->runstack_saved;
  saved->runstack = MZ_RUNSTACK;
  saved->runstack_start = MZ_RUNSTACK_START;
  saved->runstack_size = p->runstack_size;
  
  size += TAIL_COPY_THRESHOLD;
  if (size < SCHEME_STACK_SIZE)
    size = SCHEME_STACK_SIZE;

  p->runstack_saved = saved;
  p->runstack_size = size;
  MZ_RUNSTACK_START = scheme_malloc_allow_interior(sizeof(Scheme_Object*) * size);
  MZ_RUNSTACK = MZ_RUNSTACK_START + size;
  
  v = k();
  
  p->runstack_saved = saved->prev;
  MZ_RUNSTACK = saved->runstack;
  MZ_RUNSTACK_START = saved->runstack_start;
  p->runstack_size = saved->runstack_size;

  return v;
}

/*========================================================================*/
/*           compiling applications, sequences, and branches              */
/*========================================================================*/

int scheme_omittable_expr(Scheme_Object *o)
{
  Scheme_Type vtype;

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_compiled_values_types_) 
      || (vtype == scheme_local_type)
      || (vtype == scheme_local_unbox_type)
      || (vtype == scheme_unclosed_procedure_type)
      || (vtype == scheme_compiled_unclosed_procedure_type))
    return 1;

  if ((vtype == scheme_branch_type)) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test)
	    && scheme_omittable_expr(b->tbranch)
	    && scheme_omittable_expr(b->fbranch));
  }

  return 0;
}

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    if (!can_be_closed) {
      Scheme_Closure_Compilation_Data *data;
      data = (Scheme_Closure_Compilation_Data *)o;
      /* Because == 0 is like a constant */
      return (data->closure_size > 0);
    } else
      return 1;
  } else
    return 0;
}

int scheme_get_eval_type(Scheme_Object *obj)
{
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  if (type > _scheme_values_types_)
    return SCHEME_EVAL_CONSTANT;
  else if (SAME_TYPE(type, scheme_local_type))
    return SCHEME_EVAL_LOCAL;
  else if (SAME_TYPE(type, scheme_local_unbox_type))
    return SCHEME_EVAL_LOCAL_UNBOX;
  else if (SAME_TYPE(type, scheme_variable_type)
	   || SAME_TYPE(type, scheme_module_variable_type))
    return SCHEME_EVAL_GLOBAL;
  else
    return SCHEME_EVAL_GENERAL;
}    

static Scheme_Object *try_apply(Scheme_Object *f, Scheme_Object *args)
{
  Scheme_Object * volatile result;
  mz_jmp_buf savebuf;
  scheme_current_process->error_invoked = 5;
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

  if (scheme_setjmp(scheme_error_buf))
    result = NULL;
  else
    result = _scheme_apply_to_list(f, args);
  
  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
  scheme_current_process->error_invoked = 0;  

  return result;
}

static Scheme_Object *make_application(Scheme_Object *v, int final)
{
  Scheme_Object *o;
  Scheme_App_Rec *app;
  int i, nv, size, devals;
  volatile int n;

  o = v;
  n = 0;
  nv = 0;
  while (!SCHEME_NULLP(o)) {
    Scheme_Type type;
    
    n++;
    if (!SCHEME_LISTP(o))
      scheme_wrong_syntax("application", NULL, NULL, NULL);
    type = SCHEME_TYPE(SCHEME_CAR(o));
    if (type < _scheme_compiled_values_types_)
      nv = 1;
    o = SCHEME_CDR(o);
  }

  if (!nv) {
    /* They're all values. Applying folding prim or closure? */
    Scheme_Object *f;

    f = SCHEME_CAR(v);

    if ((SCHEME_PRIMP(f) && (((Scheme_Primitive_Proc *)f)->flags & SCHEME_PRIM_IS_FOLDING))
	|| (SCHEME_CLSD_PRIMP(f) 
	    && (((Scheme_Closed_Primitive_Proc *)f)->flags & SCHEME_PRIM_IS_FOLDING))
	|| (SAME_TYPE(SCHEME_TYPE(f), scheme_linked_closure_type)
	    && (((Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(f))->flags
		& CLOS_FOLDABLE))) {
      f = try_apply(f, SCHEME_CDR(v));
      
      if (f)
	return f;
    }
  }

  size = (sizeof(Scheme_App_Rec) 
	  + ((n - 1) * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  app = (Scheme_App_Rec *)scheme_malloc_tagged(size);

  app->type = scheme_application_type;

  app->num_args = n - 1;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));

  for (i = 0; i < n; i++, v = SCHEME_CDR(v)) {
    app->args[i] = SCHEME_CAR(v);
  }

  if (final) {
    for (i = 0; i < n; i++) {
      char etype;
      etype = scheme_get_eval_type(app->args[i]);
      ((char *)app + devals)[i] = etype;
    }
  }

  return (Scheme_Object *)app;
}

static Scheme_Object *link_application(Scheme_Object *o, Link_Info *info)
{
  Scheme_App_Rec *oapp = (Scheme_App_Rec *)o;
  Scheme_App_Rec *napp;
  Scheme_Object *e;
  int n, i, size;

  n = oapp->num_args + 1;
  
  size = (sizeof(Scheme_App_Rec) 
	  + ((n - 1) * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  napp = (Scheme_App_Rec *)scheme_malloc_tagged(size);
  memcpy(napp, oapp, size);

  for (i = 0; i < n; i++) {
    e = scheme_link_expr(oapp->args[i], info);
    napp->args[i] = e;
  }

  return (Scheme_Object *)napp;
}

static Scheme_Object *resolve_application(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_App_Rec *app;
  int i, n, devals;

  app = (Scheme_App_Rec *)o;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));
  
  n = app->num_args + 1;

  info = scheme_resolve_info_extend(info, n - 1, 0, 0);

  for (i = 0; i < n; i++) {
    Scheme_Object *le;
    le = scheme_resolve_expr(app->args[i], info);
    app->args[i] = le;
  }

  for (i = 0; i < n; i++) {
    char et;
    et = scheme_get_eval_type(app->args[i]);
    ((char *)app + devals)[i] = et;
  }

  return o;
}

Scheme_Object *
scheme_make_branch(Scheme_Object *test, Scheme_Object *thenp,
		   Scheme_Object *elsep)
{
  Scheme_Branch_Rec *b;

  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    if (SCHEME_FALSEP(test))
      return elsep;
    else
      return thenp;
  }

  b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  b->type = scheme_branch_type;

#if 0
  /* Try optimize: (if (not x) y z) => (if x z y) */
  if (SAME_TYPE(SCHEME_TYPE(test), scheme_application_type)) {
    Scheme_App_Rec *app;

    app = (Scheme_App_Rec *)test;
    if ((app->num_args == 1) && SAME_PTR(scheme_not_prim, app->args[0])) {
      test = thenp;
      thenp = elsep;
      elsep = test;
      test = app->args[1];
    }
  }
#endif

  b->test = test;
  b->tbranch = thenp;
  b->fbranch = elsep;

  return (Scheme_Object *)b;
}

static Scheme_Object *link_branch(Scheme_Object *o, Link_Info *info)
{
  Scheme_Branch_Rec *ob = (Scheme_Branch_Rec *)o;
  Scheme_Branch_Rec *nb;
  Scheme_Object *e;

  nb = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  nb->type = scheme_branch_type;

  e = scheme_link_expr(ob->test, info);
  nb->test = e;
  e = scheme_link_expr(ob->tbranch, info);
  nb->tbranch = e;
  e = scheme_link_expr(ob->fbranch, info);
  nb->fbranch = e;

  return (Scheme_Object *)nb;
}

static Scheme_Object *resolve_branch(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;

  b = (Scheme_Branch_Rec *)o;

  t = scheme_resolve_expr(b->test, info);
  tb = scheme_resolve_expr(b->tbranch, info);
  fb = scheme_resolve_expr(b->fbranch, info);
  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return o;
}

static Scheme_Object *link_wcm(Scheme_Object *o, Link_Info *info)
{
  Scheme_With_Continuation_Mark *owcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_With_Continuation_Mark *nwcm;
  Scheme_Object *k, *v, *b;

  nwcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  nwcm->type = scheme_with_cont_mark_type;

  k = scheme_link_expr(owcm->key, info);
  v = scheme_link_expr(owcm->val, info);
  b = scheme_link_expr(owcm->body, info);
  nwcm->key = k;
  nwcm->val = v;
  nwcm->body = b;

  return (Scheme_Object *)nwcm;
}

static Scheme_Object *resolve_wcm(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = scheme_resolve_expr(wcm->key, info);
  v = scheme_resolve_expr(wcm->val, info);
  b = scheme_resolve_expr(wcm->body, info);
  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return (Scheme_Object *)wcm;
}

static Scheme_Sequence *malloc_sequence(int count)
{
  return (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
						 + (count - 1) 
						 * sizeof(Scheme_Object *));
}

Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *seq, int opt)
{
  Scheme_Object **array, *list, *v, *good;
  Scheme_Sequence *o;
  int count, i, k, total, last, first, setgood, addconst;
  Scheme_Type type;

  type = scheme_sequence_type;

  list = seq;
  count = i = 0;
  good = NULL;
  total = 0;
  first = 1;
  setgood = 1;
  while (!SCHEME_NULLP(list)) {
    v = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
    last = SCHEME_NULLP(list);

    if (((opt > 0) || !first) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      /* "Inline" nested begins */
      count += ((Scheme_Sequence *)v)->count;
      total++;
    } else if (opt 
	       && (((opt > 0) && !last) || ((opt < 0) && !first))
	       && scheme_omittable_expr(v)) {
      /* A value that is not the result. We'll drop it. */
      total++;
    } else {
      if (setgood)
	good = v;
      count++;
      total++;
    }
    i++;
    if (first) {
      if (opt < 0)
	setgood = 0;
      first = 0;
    }
  }

  if (!count)
    return scheme_compiled_void();
  
  if (count == 1) {
    if ((opt < 0) && !scheme_omittable_expr(SCHEME_CAR(seq))) {
      /* We can't optimize (begin expr cont) to expr because
	 exp is not in tail position in the original (so we'd mess
	 up continuation marks. */
      addconst = 1;
    } else
      return good;
  } else
    addconst = 0;

  o = malloc_sequence(count + addconst);

  o->type = ((opt < 0) ? scheme_begin0_sequence_type : scheme_sequence_type);
  o->count = count + addconst;
  array = o->array;
  
  --total;
  for (i = k = 0; i < count; k++) {
    v = SCHEME_CAR(seq);
    seq = SCHEME_CDR(seq);

    if (((opt > 0) || k) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      int c, j;
      Scheme_Object **a;

      c = ((Scheme_Sequence *)v)->count;
      a = ((Scheme_Sequence *)v)->array;
      for (j = 0; j < c; j++) {
	array[i++] = a[j];
      }
    } else if (opt 
	       && ((opt > 0 && (k < total))
		   || ((opt < 0) && k))
	       && scheme_omittable_expr(v)) {
      /* Value not the result. Do nothing. */
    } else
      array[i++] = v;
  }

  if (addconst)
    array[i] = scheme_make_integer(0);
  
  return (Scheme_Object *)o;
}

static Scheme_Object *look_for_letv_change(Scheme_Sequence *s)
{
  int i;

  for (i = 0; i < s->count - 1; i++) {
    Scheme_Object *v;
    v = s->array[i];
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_let_value_type)) {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)v;
      if (scheme_omittable_expr(lv->body)) {
	int esize = s->count - (i + 1);
	int nsize = i + 1;
	Scheme_Object *nv, *ev;

	if (nsize > 1) {
	  Scheme_Sequence *naya;

	  naya = malloc_sequence(nsize);
	  naya->type = scheme_sequence_type;
	  naya->count = nsize;
	  nv = (Scheme_Object *)naya;

	  for (i = 0; i < nsize; i++) {
	    naya->array[i] = s->array[i];
	  }
	} else
	  nv = (Scheme_Object *)lv;

	if (esize > 1) {
	  Scheme_Sequence *e;
	  e = malloc_sequence(esize);
	  e->type = scheme_sequence_type;
	  e->count = esize;

	  for (i = 0; i < esize; i++) {
	    e->array[i] = s->array[i + nsize];
	  }

	  ev = (Scheme_Object *)look_for_letv_change(e);
	} else
	  ev = s->array[nsize]; 

	lv->body = ev;

	return nv;
      }
    }
  }

  return (Scheme_Object *)s;
}

static Scheme_Object *link_sequence(Scheme_Object *o, Link_Info *info)
{
  Scheme_Sequence *os = (Scheme_Sequence *)o;
  Scheme_Sequence *ns;
  Scheme_Object *e;
  int i, count;

  count = os->count;
  ns = malloc_sequence(count);
  ns->type = scheme_sequence_type;
  ns->count = count;

  for (i = 0; i < count; i++) {
    e = scheme_link_expr(os->array[i], info);
    ns->array[i] = e;
  }

  return (Scheme_Object *)ns;
}

static Scheme_Object *resolve_sequence(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  int i;

  for (i = s->count; i--; ) {
    Scheme_Object *le;
    le = scheme_resolve_expr(s->array[i], info);
    s->array[i] = le;
  }
  
  return look_for_letv_change(s);
}

Scheme_Object *scheme_make_syntax_resolved(int idx, Scheme_Object *data)
{
  Scheme_Object *v;

  v = scheme_alloc_object();
  v->type = scheme_syntax_type;
  SCHEME_PINT_VAL(v) = idx;
  SCHEME_IPTR_VAL(v) = (void *)data;

  return v;
}

Scheme_Object *scheme_make_syntax_compiled(int idx, Scheme_Object *data)
{
  Scheme_Object *v;

  v = scheme_alloc_object();
  v->type = scheme_compiled_syntax_type;
  SCHEME_PINT_VAL(v) = idx;
  SCHEME_IPTR_VAL(v) = (void *)data;

  return v;  
}

static Scheme_Object *link_module_variable(Scheme_Object *modidx,
					   Scheme_Object *varname,
					   Scheme_Object *rootname,
					   Link_Info *info)
{
  Scheme_Object *modname;
  Scheme_Env *menv, *root;

  /* If it's a name id, resolve the name. */
  modname = scheme_module_resolve(modidx);

  if (rootname && info->val_env)
    root = (Scheme_Env *)scheme_module_syntax(rootname, info->val_env, NULL);
  else 
    root = NULL;

  menv = scheme_module_access(modname, root ? root : info);
  
  if (!menv) {
    scheme_wrong_syntax("link", NULL, varname,
			"broken compiled code (phase %d), no declaration for module"
			": %S", info->phase, modname);
    return NULL;
  }

  if (!SAME_OBJ(menv, info))
    scheme_check_accessible_in_module(menv, varname, NULL);
      
  return (Scheme_Object *)scheme_global_bucket(varname, menv);
}

Scheme_Object *scheme_link_expr(Scheme_Object *expr, Link_Info *info)
{
  Scheme_Type type = SCHEME_TYPE(expr);

  switch (type) {
  case scheme_variable_type:
    {
      Scheme_Bucket_With_Home *b = (Scheme_Bucket_With_Home *)expr;

      if (!info || !b->home->module)
	return (Scheme_Object *)b;
      else
	return link_module_variable(b->home->module->modname,
				    (Scheme_Object *)b->bucket.bucket.key,
				    (b->home->for_syntax_of
				     ? b->home->for_syntax_of->module->modname
				     : NULL),
				    info);
    }
  case scheme_module_variable_type:
    {
      Scheme_Object *mod;

      mod = SCHEME_PTR1_VAL(expr);
      return link_module_variable(SCHEME_PAIRP(mod) ? SCHEME_CAR(mod) : mod,
				  SCHEME_PTR2_VAL(expr),
				  SCHEME_PAIRP(mod) ? SCHEME_CDR(mod) : NULL,
				  info);
    }
  case scheme_syntax_type:
    {
      Scheme_Syntax_Linker f;
      Scheme_Object *o;
	  
      f = scheme_syntax_linkers[SCHEME_PINT_VAL(expr)];
      o = f((Scheme_Object *)SCHEME_IPTR_VAL(expr), info);
      if (SAME_OBJ(o, (Scheme_Object *)SCHEME_IPTR_VAL(expr)))
	return expr;
      else
	return o;
    }
  case scheme_application_type:
    return link_application(expr, info);
  case scheme_sequence_type:
    return link_sequence(expr, info);
  case scheme_branch_type:
    return link_branch(expr, info);
  case scheme_with_cont_mark_type:
    return link_wcm(expr, info);
  case scheme_unclosed_procedure_type:
    return scheme_link_closure_compilation(expr, info);
  case scheme_let_value_type:
    return scheme_link_let_value(expr, info);
  case scheme_let_void_type:
    return scheme_link_let_void(expr, info);
  case scheme_let_one_type:
    return scheme_link_let_one(expr, info);
  case scheme_letrec_type:
    return scheme_link_letrec(expr, info);
  default:
    return expr;
  }
}

Scheme_Object *scheme_link_list(Scheme_Object *expr, Link_Info *info)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  while (SCHEME_PAIRP(expr)) {
    Scheme_Object *pr;

    pr = scheme_make_pair(scheme_link_expr(SCHEME_CAR(expr), info),
			  scheme_null);

    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    expr = SCHEME_CDR(expr);
  }

  return first;
}

Scheme_Object *scheme_resolve_expr(Scheme_Object *expr, Resolve_Info *info)
{
  Scheme_Type type = SCHEME_TYPE(expr);

  switch (type) {
  case scheme_local_type:
    {
      int pos, flags;
      
      pos = scheme_resolve_info_lookup(info, SCHEME_LOCAL_POS(expr), &flags);
      return scheme_make_local((flags & SCHEME_INFO_BOXED) 
			       ? scheme_local_unbox_type
			       : scheme_local_type,
			       pos);
    }
  case scheme_compiled_syntax_type:
    {
      Scheme_Syntax_Resolver f;
	  
      f = scheme_syntax_resolvers[SCHEME_PINT_VAL(expr)];
      return f((Scheme_Object *)SCHEME_IPTR_VAL(expr), info);
    }
  case scheme_application_type:
    return resolve_application(expr, info);
  case scheme_sequence_type:
    return resolve_sequence(expr, info);
  case scheme_branch_type:
    return resolve_branch(expr, info);
  case scheme_with_cont_mark_type:
    return resolve_wcm(expr, info);
  case scheme_compiled_unclosed_procedure_type:
    return scheme_resolve_closure_compilation(expr, info);
  case scheme_compiled_let_void_type:
    return scheme_resolve_lets(expr, info);
  default:
    return expr;
  }
}

Scheme_Object *scheme_resolve_list(Scheme_Object *expr, Resolve_Info *info)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  while (SCHEME_PAIRP(expr)) {
    Scheme_Object *pr;

    pr = scheme_make_pair(scheme_resolve_expr(SCHEME_CAR(expr), info),
			  scheme_null);

    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    expr = SCHEME_CDR(expr);
  }

  return first;
}

/*========================================================================*/
/*                       compilation info management                      */
/*========================================================================*/

void scheme_default_compile_rec(Scheme_Compile_Info *rec, int drec)
{
  rec[drec].max_let_depth = 0;
}

void scheme_init_compile_recs(Scheme_Compile_Info *src, int drec, 
			      Scheme_Compile_Info *dest, int n)
{
  int i;

  for (i = 0; i < n; i++) {
#ifdef MZTAG_REQUIRED
    dest[i].type = scheme_rt_compile_info;
#endif
    dest[i].max_let_depth = 0;
    dest[i].dont_mark_local_use = src[drec].dont_mark_local_use;
    dest[i].resolve_module_ids = src[drec].resolve_module_ids;
    dest[i].value_name = NULL;
  }
}

void scheme_merge_compile_recs(Scheme_Compile_Info *src, int drec, 
			       Scheme_Compile_Info *dest, int n)
{
  int i;

  if (!n) {
    src[drec].max_let_depth = 0;
    return;
  }
  
  src[drec].max_let_depth = dest[0].max_let_depth;

  for (i = 1; i < n; i++) {
    if (dest[i].max_let_depth > src[drec].max_let_depth)
      src[drec].max_let_depth = dest[i].max_let_depth;
  }
}

void scheme_init_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec)
{
  lam[dlrec].max_let_depth = 0;
  lam[dlrec].dont_mark_local_use = src[drec].dont_mark_local_use;
  lam[dlrec].resolve_module_ids = src[drec].resolve_module_ids;
  lam[dlrec].value_name = NULL;
}

void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			     Scheme_Compile_Info *lam, int dlrec)
{
}

void scheme_compile_rec_done_local(Scheme_Compile_Info *rec, int drec)
{
  rec[drec].value_name = NULL;
}

/*========================================================================*/
/*                         compilation dispatcher                         */
/*========================================================================*/

static Scheme_Object *
scheme_inner_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
			  Scheme_Compile_Info *rec, int drec, int start_app_position)
{
  if (SCHEME_STX_NULLP(form)) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_null;
  } else if (SCHEME_STX_PAIRP(form)) {
    Scheme_Compile_Info recs[2];
    Scheme_Object *c1, *c2, *name, *first, *rest;

    name = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);
    scheme_init_compile_recs(rec, drec, recs, 2);

    first = SCHEME_STX_CAR(form);
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_NULLP(rest))
      recs[0].value_name = name;
    else
      recs[1].value_name = name;

    c1 = scheme_compile_expand_expr(first, env, recs, 0,
				    1, scheme_false, start_app_position);
    /* if (start_app_position)
      recs[0].is_proc_closure = 0; */
    c2 = scheme_inner_compile_list(rest, env, recs, 1, 0);

    scheme_merge_compile_recs(rec, drec, recs, 2);

    return scheme_make_pair(c1, c2);
  } else
    return scheme_compile_expr(form, env, rec, drec);
}

static Scheme_Object *compile_application(Scheme_Object *form, Scheme_Comp_Env *env,
					  Scheme_Compile_Info *rec, int drec)
{
  int len;

  len = scheme_stx_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax("application", NULL, form, NULL);
  
  scheme_compile_rec_done_local(rec, drec);
  form = scheme_inner_compile_list(form, scheme_no_defines(env), rec, drec, 1);

  rec[drec].max_let_depth += (len - 1);

  return make_application(form, 0);
}

Scheme_Object *
scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  return scheme_inner_compile_list(form, env, rec, drec, 0);
}

static void *compile_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *form;
  int writeable;
  Scheme_Comp_Env *env;
  Scheme_Compile_Info rec;
  Scheme_Object *o;
  Scheme_Compilation_Top *top;

  form = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;
  writeable = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  rec.dont_mark_local_use = 0;
  rec.resolve_module_ids = !writeable;
  rec.value_name = NULL;

  if (!SCHEME_STXP(form))
    form = scheme_datum_to_syntax(form, scheme_false, scheme_false, 1);

  /* Renamings for imports: */
  if (env->genv->rename)
    form = scheme_add_rename(form, env->genv->rename);
  if (env->genv->exp_env && env->genv->exp_env->rename)
    form = scheme_add_rename(form, env->genv->exp_env->rename);

  o = scheme_compile_expr(form, env, &rec, 0);
  o = scheme_resolve_expr(o, scheme_resolve_info_create());

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  top->type = scheme_compilation_top_type;
  top->max_let_depth = rec.max_let_depth;
  top->code = o;

  return (void *)top;
}

static Scheme_Object *_compile(Scheme_Object *form, Scheme_Env *env, int writeable, int eb)
{
  Scheme_Process *p = scheme_current_process;

  if (SAME_TYPE(SCHEME_TYPE(form), scheme_compilation_top_type))
    return form;

  if (SCHEME_STXP(form)) {
    if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_compilation_top_type))
      return SCHEME_STX_VAL(form);
  }

  p->ku.k.p1 = form;
  p->ku.k.p2 = env->init;
  p->ku.k.i1 = writeable;

  return (Scheme_Object *)scheme_top_level_do(compile_k, eb);
}

Scheme_Object *scheme_compile(Scheme_Object *form, Scheme_Env *env, int writeable)
{
  return _compile(form, env, writeable, 1);
}

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first, 
					    Scheme_Comp_Env *env, 
					    Scheme_Compile_Info *rec, int drec,
					    int depth, Scheme_Object *boundname,
					    Scheme_Object **current_val)
{
  Scheme_Object *name, *val, *orig;

  orig = first;

 check_top:
  *current_val = NULL;

  name = SCHEME_STX_CAR(first);
  if (!SCHEME_STX_SYMBOLP(name))
    return first;

  val = scheme_static_distance(name, env, 
			       SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + ((rec && rec[drec].dont_mark_local_use) 
				  ? SCHEME_DONT_MARK_USE 
				  : 0)
			       + ((rec && rec[drec].resolve_module_ids)
				  ? SCHEME_RESOLVE_MODIDS
				  : 0));

  *current_val = val;

  if (!val) {
    return first;
  } else if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type)
      || SAME_TYPE(SCHEME_TYPE(val), scheme_id_macro_type)) {
    /* Yep, it's a macro; expand once */
    first = scheme_expand_expr(first, env, 1, boundname);
  } else {
#if 0
    if (SAME_OBJ(val, scheme_define_values_syntax)) {
      /* Check the form of the definition: can't shadow syntax bindings. */
      /* Only check identifier if the definition is well-formed. */
      Scheme_Object *binding, *rest;
      rest = SCHEME_STX_CDR(first);
      if (SCHEME_STX_PAIRP(rest)) {
	binding = SCHEME_STX_CAR(rest);
	rest = SCHEME_STX_CDR(rest);
	if (SCHEME_STX_PAIRP(rest) && SCHEME_NULLP(SCHEME_STX_CDR(rest))) {
	  if (SCHEME_STX_SYMBOLP(binding)) {
	    /* Binding part is ok */
	  } else if (SCHEME_STX_PAIRP(binding)) {
	    rest = SCHEME_STX_CDR(binding);
	    binding = SCHEME_STX_CAR(binding);
	    while (SCHEME_STX_PAIRP(rest)) {
	      if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(rest)))
		break;
	      rest = SCHEME_STX_CDR(rest);
	    }
	    if (!SCHEME_STX_NULLP(rest) && !SCHEME_STX_SYMBOLP(rest))
	      binding = NULL;
	  } else
	    binding = NULL;
	  
	  if (binding) {
	    /* Check binding id for shadowing syntax */
	    Scheme_Object *sdval;
	    sdval = scheme_static_distance(binding, env, 
					   SCHEME_NULL_FOR_UNBOUND
					   + SCHEME_DONT_MARK_USE 
					   + SCHEME_ENV_CONSTANTS_OK);
	    if (sdval
		&& (SAME_TYPE(SCHEME_TYPE(sdval), scheme_macro_type)
		    || SAME_TYPE(SCHEME_TYPE(sdval), scheme_id_macro_type)
		    || SAME_TYPE(SCHEME_TYPE(sdval), scheme_syntax_compiler_type))) {
	      scheme_wrong_syntax("define-values (in unit or internal)",
				  binding, orig,
				  "unit/internal binding cannot shadow syntax names");
	      return NULL;
	    }
	  }
	}
      }
    }
#endif

    return first;
  }

  if (SCHEME_STX_PAIRP(first))
    goto check_top;
  
  return first;
}

Scheme_Object *
scheme_compile_expand_macro_app(Scheme_Object *name, Scheme_Object *macro,
				Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Compile_Info *rec, int drec, 
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *xformer;

  if (!depth)
    return form; /* We've gone as deep as requested */

  xformer = (Scheme_Object *)SCHEME_PTR_VAL(macro);

  if (SAME_TYPE(SCHEME_TYPE(xformer), scheme_id_macro_type)) {
    xformer = SCHEME_PTR_VAL(xformer);
  } else if (!scheme_check_proc_arity(NULL, 1, 0, -1, &xformer)) {
    Scheme_Object *sname;
    const char *name;

    if (SCHEME_STX_SYMBOLP(form))
      sname = SCHEME_STX_SYM(form);
    else
      sname = SCHEME_STX_SYM(SCHEME_STX_CAR(form));
    name = scheme_symbol_name(sname);
    
    scheme_wrong_syntax(name, NULL, form, "illegal use of syntax");
    return NULL;
  }

  if (rec)
    boundname = rec[drec].value_name;
  if (!boundname)
    boundname = scheme_false;

  form = scheme_apply_macro(name, xformer, form, env, boundname);

  if (rec)
    return scheme_compile_expr(form, env, rec, drec);
  else {
    if (depth > 0)
      --depth;
    if (depth)
      return scheme_expand_expr(form, env, depth, scheme_false);
    else
      return form;
  }
}

static Scheme_Object *compile_expand_expr_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;
  Scheme_Object *boundname = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return scheme_compile_expand_expr(form, 
				    env,
				    rec,
				    p->ku.k.i3,
				    p->ku.k.i1,
				    boundname,
				    p->ku.k.i2);
}

static Scheme_Object *
scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
			   Scheme_Compile_Info *rec, int drec, 
			   int depth, Scheme_Object *boundname,
			   int app_position)
{
  Scheme_Object *name, *var, *rest, *stx;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Process *p = scheme_current_process;
    p->ku.k.p1 = (void *)form;
    p->ku.k.p2 = (void *)env;
    p->ku.k.p3 = (void *)rec;
    p->ku.k.p4 = (void *)boundname;
    p->ku.k.i3 = drec;
    p->ku.k.i1 = depth;
    p->ku.k.i2 = app_position;
    return scheme_handle_stack_overflow(compile_expand_expr_k);
  }
#endif

  DO_CHECK_FOR_BREAK(scheme_current_process, ;);

#if 1
  if (!SCHEME_STXP(form))
    scheme_signal_error("not syntax");
#endif

  if (rec)
    scheme_default_compile_rec(rec, drec);

  if (SCHEME_STX_NULLP(form)) {
    stx = app_symbol;
  } else if (!SCHEME_STX_PAIRP(form)) {
    if (SCHEME_STX_SYMBOLP(form)) {
      var = scheme_static_distance(form, env, 
				   SCHEME_NULL_FOR_UNBOUND
				   + SCHEME_ENV_CONSTANTS_OK
				   + (rec
				      ? SCHEME_ELIM_CONST 
				      : 0)
				   + (app_position 
				      ? SCHEME_APP_POS 
				      : 0)
				   + ((rec && drec[rec].dont_mark_local_use) ? 
				      SCHEME_DONT_MARK_USE 
				      : 0)
				   + ((rec && rec[drec].resolve_module_ids)
				      ? SCHEME_RESOLVE_MODIDS
				      : 0));
      
      if (!var) {
	/* Unbound variable */
	stx = unbound_symbol;
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (var == stop_expander)
	    return form;
	  else {
	    scheme_wrong_syntax("compile", NULL, form, "bad syntax");
	    return NULL;
	  }
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
		   || SAME_TYPE(SCHEME_TYPE(var), scheme_id_macro_type))
	  return scheme_compile_expand_macro_app(form, var, form, env, rec, drec, depth, boundname);
	
	if (rec) {
	  scheme_compile_rec_done_local(rec, drec);
	  return var;
	} else
	  return form;
      }
    } else
      stx = datum_symbol;
  } else {
    name = SCHEME_STX_CAR(form);
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_SYMBOLP(name)) {
      /* Check for macros: */
      var = scheme_static_distance(name, env, 
				   SCHEME_APP_POS
				   + SCHEME_NULL_FOR_UNBOUND
				   + SCHEME_ENV_CONSTANTS_OK
				   + (rec
				      ? SCHEME_ELIM_CONST
				      : 0)
				   + ((rec && rec[drec].dont_mark_local_use)
				      ? SCHEME_DONT_MARK_USE 
				      : 0)
				   + ((rec && rec[drec].resolve_module_ids)
				      ? SCHEME_RESOLVE_MODIDS
				      : 0));
      if (!var) {
	/* apply to global variable: compile it normally */
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)
	  || SAME_TYPE(SCHEME_TYPE(var), scheme_local_unbox_type)) {
	/* apply to local variable: compile it normally */
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	    || SAME_TYPE(SCHEME_TYPE(var), scheme_id_macro_type)) {
	  return scheme_compile_expand_macro_app(name, var, form, env, rec, drec, depth, boundname);
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (rec) {
	    Scheme_Syntax *f;
	    f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	    return f(form, env, rec, drec);
	  } else {
	    Scheme_Syntax_Expander *f;
	    f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	    return f(form, env, depth, boundname);
	  }
	}
	
	/* Else: unknown global - must be a function: compile as application */
      }
    }

    stx = app_symbol;
  }

  /* Compile/expand as application, datum, or unbound: */
  stx = scheme_datum_to_syntax(stx, scheme_false, form, 0);
  var = scheme_static_distance(stx, env,
			       SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + SCHEME_DONT_MARK_USE);
  if (var && (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_id_macro_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type))) {
    if (SAME_OBJ(var, stop_expander)) {
      /* Return original: */
      return form;
    } else {
      form = scheme_datum_to_syntax(scheme_make_pair(stx, form), form, form, 0);
      if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	if (rec) {
	  Scheme_Syntax *f;
	  f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	  return f(form, env, rec, drec);
	} else {
	  Scheme_Syntax_Expander *f;
	  f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	  return f(form, env, depth, boundname);
	}
      } else {
	return scheme_compile_expand_macro_app(stx, var, form, env, rec, drec, depth, boundname);
      }
    }
  } else {
    /* Not allowed this context! */
    scheme_wrong_syntax("compile", NULL, form, "bad syntax (no %S syntax expander)",
			SCHEME_STX_VAL(stx));
    return NULL;
  }
}

static Scheme_Object *
compile_expand_app(Scheme_Object *forms, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec, int drec, 
		   int depth, Scheme_Object *boundname)
{
  Scheme_Object *form, *naya;

  form = SCHEME_STX_CDR(forms);
  form = scheme_datum_to_syntax(form, forms, forms, 0);
  
  if (SCHEME_STX_NULLP(form)) {
    /* Compile/expand empty application to null list: */
    if (rec)
      return scheme_null;
    else
      return forms;
  } else if (SCHEME_STX_SYMBOLP(SCHEME_CAR(form))) {
#if 0
    /* Optimize (void) to just the value void */
    if (rec && SAME_OBJ(var, scheme_void_func) && SCHEME_NULLP(rest)) {
      scheme_compile_rec_done_local(rec, drec);
      return scheme_void;
    }
#endif

    if (rec)
      return compile_application(form, env, rec, drec);
    else {
      naya = scheme_expand_list(form, scheme_no_defines(env), depth, scheme_false);
      /* naya will be prefixed and returned... */
    }
  } else if (rec) {
    Scheme_Object *name;
    name = SCHEME_STX_CAR(form);
    /* look for ((lambda (x) ...) ...) */
    if (SCHEME_STX_PAIRP(name) && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(name))) {
      Scheme_Object *gval, *origname = name;

      name = scheme_check_immediate_macro(name, env, rec, drec, depth, boundname, &gval);
      
      if (SAME_OBJ(gval, scheme_lambda_syntax)) {
	Scheme_Object *argsnbody;
	
	argsnbody = SCHEME_STX_CDR(name);
	if (SCHEME_STX_PAIRP(argsnbody)) {
	  Scheme_Object *args, *body;

	  args = SCHEME_STX_CAR(argsnbody);
	  body = SCHEME_STX_CDR(argsnbody);
	  
	  if (SCHEME_STX_PAIRP(body)) {
	    int pl;
	    pl = scheme_stx_proper_list_length(args);
	    if (pl >= 0) {
	      Scheme_Object *bindings = scheme_null, *last = NULL;
	      Scheme_Object *rest;
	      int al;
	      rest = SCHEME_STX_CDR(form);
	      al = scheme_stx_proper_list_length(rest);
	      
	      if (al == pl) {	      
		while (!SCHEME_STX_NULLP(args)) {
		  Scheme_Object *v, *n;
		  
		  n = SCHEME_STX_CAR(args);
		  scheme_check_identifier("lambda", n, NULL, env, form);
		  
		  v = SCHEME_STX_CAR(rest);
		  v = cons(cons(n, cons(v, scheme_null)), scheme_null);
		  if (last)
		    SCHEME_CDR(last) = v;
		  else
		    bindings = v;
		  
		  last = v;
		  args = SCHEME_STX_CDR(args);
		  rest = SCHEME_STX_CDR(rest);
		}
		
		return scheme_compile_expand_expr(scheme_datum_to_syntax(cons(let_symbol,
									      cons(bindings,
										   body)),
									 form, 
									 scheme_sys_wraps(env), 
									 0),
						  env, rec, drec, depth, boundname, 0);
	      } else {
#if 0
 		scheme_wrong_syntax("application", NULL, form, 
				    "procedure application: bad ((lambda (...) ...) ...) syntax");
		return NULL
#endif
	      }
	    }
	  }
	}
      }

      if (NOT_SAME_OBJ(name, origname)) {
	form = SCHEME_STX_CDR(form);
	form = scheme_datum_to_syntax(scheme_make_pair(name, form), forms, forms, 0);
      }
    }
    
    return compile_application(form, env, rec, drec);
  } else {
    naya = scheme_expand_list(form, scheme_no_defines(env), depth, scheme_false);
    /* naya will be prefixed returned... */
  }

  if (SAME_OBJ(form, naya))
    return forms;

  /* Add #%app prefix back: */
  {
    Scheme_Object *first;
    first = SCHEME_STX_CAR(forms);
    return scheme_datum_to_syntax(scheme_make_pair(first, naya),
				  forms,
				  forms, 0);
  }
}

static Scheme_Object *
app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_app(form, env, rec, drec, 0, scheme_false);
}

static Scheme_Object *
app_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return compile_expand_app(form, env, NULL, 0, depth, boundname);
}

static Scheme_Object *
datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c;

  c = SCHEME_STX_CDR(form);

  if (SCHEME_NULLP(c))
    scheme_wrong_syntax("#%datum", NULL, form, NULL);

  return SCHEME_STX_VAL(c);
}

static Scheme_Object *
datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *c;

  c = SCHEME_STX_CDR(form);

  if (SCHEME_NULLP(c))
    scheme_wrong_syntax("#%datum", NULL, form, NULL);

  return form;
}

static void check_unbound(char *when, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *c;

  c = SCHEME_STX_CDR(form);

  if (!SCHEME_STX_SYMBOLP(c))
    scheme_wrong_syntax("#%unbound", NULL, form, NULL);

  if (env->genv->module) {
    Scheme_Object *modidx, *symbol = c;
    Scheme_Env *home;

    modidx = scheme_stx_module_name(&symbol, env->genv->phase, &home);
    if (modidx) {
      /* If it's an access path, resolve it: */
      if (env->genv->module
	  && SAME_OBJ(scheme_module_resolve(modidx), env->genv->module->modname))
	modidx = NULL;
    }

    if (modidx || !scheme_lookup_in_table(env->genv->toplevel, (const char *)SCHEME_STX_SYM(c)))
      scheme_wrong_syntax(when, NULL, c, "unbound variable in module");
  }
}

static Scheme_Object *
unbound_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c;

  check_unbound("compile", form, env);

  c = SCHEME_STX_CDR(form);

  return (Scheme_Object *)scheme_global_bucket(SCHEME_STX_SYM(c), env->genv);
}

static Scheme_Object *
unbound_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *c;

  check_unbound("expand", form, env);

  c = SCHEME_STX_CDR(form);

  return form;
}

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				   Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_expr(form, env, rec, drec, 1, scheme_false, 0);
}

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				  int depth, Scheme_Object *boundname)
{
  return scheme_compile_expand_expr(form, env, NULL, 0, depth, boundname, 0);
}

static Scheme_Object *
scheme_compile_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
			    Scheme_Compile_Info *rec, int drec, 
			    int depth, Scheme_Object *boundname)
/* This ugly code parses a block of code, transforming embedded
   define-values and define-syntax into letrec and letrec-syntax.
   It is espcailly ugly because we have to expand macros
   before deciding what we have. */
{
  Scheme_Object *first;
  Scheme_Compile_Info recs[2];

  if (rec)
    scheme_default_compile_rec(rec, drec);

  if (SCHEME_STX_NULLP(forms)) {
    if (rec)
      scheme_compile_rec_done_local(rec, drec);
    return scheme_null;
  }

 try_again:

  first = SCHEME_STX_CAR(forms);

  if (SCHEME_STX_PAIRP(first)) {
    Scheme_Object *name, *gval, *result;

    result = forms;

    /* Check for macro expansion, which could mask the real
       define-values, define-syntax, etc.: */
    first = scheme_check_immediate_macro(first, env, rec, drec, depth, scheme_false, &gval);

    name = SCHEME_STX_PAIRP(first) ? SCHEME_STX_CAR(first) : scheme_void;
    if (SAME_OBJ(gval, scheme_begin_syntax)) {
      /* Inline content */
      Scheme_Object *orig_forms = forms;
      Scheme_Object *content;

      content = SCHEME_STX_CDR(first);

      if (scheme_stx_proper_list_length(content) < 0)
	scheme_wrong_syntax("begin", NULL, first, 
			    "bad syntax (" IMPROPER_LIST_FORM ")");

      forms = SCHEME_STX_CDR(forms);
      forms = scheme_append(scheme_flatten_syntax_list(content, NULL),
			    forms);

      if (SCHEME_STX_NULLP(forms)) {
	scheme_wrong_syntax("begin", NULL, first, 
			    "bad syntax (empty form)");
      }

      forms = scheme_datum_to_syntax(forms, orig_forms, orig_forms, 0);

      goto try_again;
    } else if (SAME_OBJ(gval, scheme_define_values_syntax)
	       || SAME_OBJ(gval, scheme_defmacro_syntax)) {
      /* Turn defines into a letrec: */
      Scheme_Object *var, *vars, *v, *link, *l = scheme_null, *start = NULL;
      int values = SAME_OBJ(gval, scheme_define_values_syntax);
      GC_CAN_IGNORE char *dname = (values 
				   ? "define-values (internal)" 
				   : "define-syntax (internal)");

      while (1) {
	v = SCHEME_STX_CDR(first);
	
	if (!SCHEME_STX_PAIRP(v))
	  scheme_wrong_syntax(dname, NULL, forms, 
			      "bad syntax (" IMPROPER_LIST_FORM ")");

	var = NULL;
	vars = SCHEME_STX_CAR(v);
	while (SCHEME_STX_PAIRP(vars)) {
	  var = SCHEME_STX_CAR(vars);
	  if (!SCHEME_STX_SYMBOLP(var))
	    scheme_wrong_syntax(dname, var, forms, 
				"name must be an identifier");
	  vars = SCHEME_STX_CDR(vars);
	}
	
	link = scheme_make_pair(v, scheme_null);
	if (!start)
	  start = link;
	else
	  SCHEME_CDR(l) = link;
	l = link;
	result = SCHEME_STX_CDR(result);
	if (!SCHEME_STX_NULLP(result) && !SCHEME_STX_PAIRP(result))
	  scheme_wrong_syntax(dname, NULL, forms, NULL);

      define_try_again:
	if (!SCHEME_STX_NULLP(result)) {
	  first = SCHEME_STX_CAR(result);
	  if (SCHEME_STX_PAIRP(first)) {
	    first = scheme_datum_to_syntax(first, forms, forms, 0);
	    first = scheme_check_immediate_macro(first, env, rec, drec, depth, scheme_false, &gval);
	    name = SCHEME_STX_CAR(first);
	    if ((values && NOT_SAME_OBJ(gval, scheme_define_values_syntax))
		|| (!values && NOT_SAME_OBJ(gval, scheme_defmacro_syntax))) {
	      if (SAME_OBJ(gval, scheme_begin_syntax)) {
		/* Inline content */
		Scheme_Object *content;

		content = SCHEME_STX_CDR(first);
		
		if (scheme_stx_proper_list_length(content) < 0)
		  scheme_wrong_syntax("begin", NULL, first, 
				      "bad syntax (" IMPROPER_LIST_FORM ")");
		
		result = scheme_append(scheme_flatten_syntax_list(content, NULL), 
				       SCHEME_CDR(result));
		goto define_try_again;
	      } else
		break;
	    }
	  } else
	    break;
	} else
	  break;
      }

      if (SCHEME_STX_PAIRP(result)) {
	result = scheme_make_pair(values ? letrec_values_symbol : letmacro_symbol, 
				  scheme_make_immutable_pair(start, result));
	result = scheme_datum_to_syntax(result, forms, scheme_sys_wraps(env), 0);

	name = NULL;
      } else {
	/* Empty body: illegal. */
	scheme_wrong_syntax("begin (possibly implicit)", NULL, forms, 
			    "no expression after a sequence of internal definitions");
      }
    }

    if (!name) {
      if (rec)
	result = scheme_compile_expr(result, env, rec, drec);
      else {
	if (depth > 0)
	  --depth;
	if (depth)
	  result = scheme_expand_expr(result, env, depth, boundname);
      }
      
      return scheme_make_pair(result, scheme_null);
    }
  }

  if (rec) {
    Scheme_Object *vname, *rest;

    vname = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);
    scheme_init_compile_recs(rec, drec, recs, 2);

    rest = SCHEME_STX_CDR(forms);
    if (SCHEME_STX_NULLP(rest))
      recs[0].value_name = vname;
    else
      recs[1].value_name = vname;

    rest = scheme_datum_to_syntax(rest, forms, forms, 0);

    first = scheme_compile_expr(first, env, recs, 0);
#if EMBEDDED_DEFINES_START_ANYWHERE
    forms = scheme_compile_expand_block(rest, env, recs, 1, 1, boundname);
#else
    forms = scheme_compile_list(rest, env, recs, 1);
#endif
    
    scheme_merge_compile_recs(rec, drec, recs, 2);
  } else {
    Scheme_Object *rest;

    first = scheme_expand_expr(first, env, depth, scheme_false);
    rest = SCHEME_STX_CDR(forms);
    rest = scheme_datum_to_syntax(rest, forms, forms, 0);
#if EMBEDDED_DEFINES_START_ANYWHERE
    forms = scheme_compile_expand_block(scheme_datum_to_syntax(rest, forms, forms, 0), 
					env, rec, drec, depth, boundname);
#else
    forms = scheme_expand_list(rest, env, depth, boundname);
#endif
  }

  return scheme_make_pair(first, forms);
}

Scheme_Object *
scheme_compile_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
		     Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_block(forms, env, rec, drec, 1, scheme_false);
}

Scheme_Object *
scheme_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return scheme_compile_expand_block(forms, env, NULL, 0, depth, boundname);
}

Scheme_Object *
scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *first = NULL, *last = NULL, *fm;

  if (SCHEME_STX_NULLP(form))
    return scheme_null;

  fm = form;
  while (SCHEME_STX_PAIRP(fm)) {
    Scheme_Object *r, *p;

    r = SCHEME_STX_CAR(fm);
    p = SCHEME_STX_CDR(fm);
    r = scheme_expand_expr(r, env, depth, 
			   (SCHEME_STX_NULLP(p)
			    ? boundname
			    : scheme_false));
    p = scheme_make_pair(r, scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;

    fm = SCHEME_STX_CDR(fm);
  }

  if (!SCHEME_STX_NULLP(fm))
    scheme_wrong_syntax("expand", NULL, form, 
			"bad syntax (" IMPROPER_LIST_FORM ")");

  return scheme_datum_to_syntax(first, form, form, 0);
}

/*========================================================================*/
/*                          continuation marks                            */
/*========================================================================*/

void scheme_push_continuation_frame(Scheme_Cont_Frame_Data *d)
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  d->cont_mark_pos = MZ_CONT_MARK_POS;
  d->cont_mark_stack = MZ_CONT_MARK_STACK;

  MZ_CONT_MARK_POS += 2;
}

void scheme_pop_continuation_frame(Scheme_Cont_Frame_Data *d)
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  MZ_CONT_MARK_POS = d->cont_mark_pos;
  MZ_CONT_MARK_STACK = d->cont_mark_stack;
}


void scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Cont_Mark *cm = NULL;
  long findpos;
  
  findpos = (long)MZ_CONT_MARK_STACK;
  while (findpos--) {
    Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    long pos = findpos & SCHEME_MARK_SEGMENT_MASK;
    Scheme_Cont_Mark *find = seg + pos;

    if ((long)find->pos < (long)MZ_CONT_MARK_POS) {
      break;
    } else {
      if (find->key == key) {
	cm = find;
	break;
      } else {
	/* Assume that we'll mutate rather than allocate a new mark record. */
	/* This is a bad assumption for a nasty program that repeatedly
	   creates a new key for the same frame, but it's good enough. */
	find->cached_chain = NULL;
      }
    }
  }
  
  if (!cm) {
    /* Allocate a new mark record: */
    long segpos = ((long)MZ_CONT_MARK_STACK) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
    long pos = ((long)MZ_CONT_MARK_STACK) & SCHEME_MARK_SEGMENT_MASK;
    Scheme_Cont_Mark *seg;

    if (segpos >= p->cont_mark_seg_count) {
      /* Need a new segment */
      int c = p->cont_mark_seg_count;
      Scheme_Cont_Mark **segs, *seg;

      /* Note: we perform allocations before changing p to avoid GC trouble,
	 since MzScheme adjusts a thread's cont_mark_stack_segments on GC. */
      segs = MALLOC_N(Scheme_Cont_Mark *, c + 1);
      seg = scheme_malloc_allow_interior(sizeof(Scheme_Cont_Mark) * SCHEME_MARK_SEGMENT_SIZE);
      segs[c] = seg;

      memcpy(segs, p->cont_mark_stack_segments, c * sizeof(Scheme_Cont_Mark *));
	      
      p->cont_mark_seg_count++;
      p->cont_mark_stack_segments = segs;
    }

    seg = p->cont_mark_stack_segments[segpos];
    cm = seg + pos;
    MZ_CONT_MARK_STACK++;
  }

  cm->key = key;
  cm->val = val;
  cm->pos = MZ_CONT_MARK_POS; /* always odd */
  cm->cached_chain = NULL;
}

void scheme_temp_dec_mark_depth()
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  MZ_CONT_MARK_POS -= 2;
}

void scheme_temp_inc_mark_depth()
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  MZ_CONT_MARK_POS += 2;
}

/*========================================================================*/
/*                         eval-apply helpers                             */
/*========================================================================*/

/* called in schapp.h */

static Scheme_Object *do_apply_known_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p2 = NULL;

  return _scheme_apply_known_closed_prim_multi((Scheme_Object *)p->ku.k.p1, 
					       p->ku.k.i1, 
					       argv);
}

#if 0
# define DEBUG_CHECK_TYPE(v) \
  if ((v != SCHEME_MULTIPLE_VALUES) \
      && (v != SCHEME_TAIL_CALL_WAITING) \
      && (v != SCHEME_EVAL_WAITING) \
      && (SCHEME_TYPE(v) > (_scheme_last_type_ + 5))) \
  { Scheme_Object *o = *(Scheme_Object **)(v); \
    if (SCHEME_TYPE(o) > (_scheme_last_type_ + 5))\
       scheme_signal_error("bad type"); }
#else
# define DEBUG_CHECK_TYPE(v) /**/
#endif

Scheme_Object *_scheme_apply_known_closed_prim_multi(Scheme_Object *rator,
						     int argc,
						     Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 0
#define PRIM_CHECK_MULTI 0
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_closed_prim_multi(Scheme_Object *rator,
					       int argc,
					       Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 1
#define PRIM_CHECK_MULTI 0
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_known_closed_prim(Scheme_Object *rator,
					       int argc,
					       Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 0
#define PRIM_CHECK_MULTI 1
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_closed_prim(Scheme_Object *rator,
					 int argc,
					 Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 1
#define PRIM_CHECK_MULTI 1
#include "schapp.inc"
}

Scheme_Object *scheme_check_one_value(Scheme_Object *v)
{
  if (v == SCHEME_MULTIPLE_VALUES)
    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
  return v;
}

static Scheme_Object *do_eval_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *obj = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_do_eval(obj, 
			p->ku.k.i1, 
			argv,
			p->ku.k.i2);
}

#ifdef REGISTER_POOR_MACHINE
# define USE_LOCAL_RUNSTACK 0
# define DELAY_THREAD_RUNSTACK_UPDATE 0
#else
# define USE_LOCAL_RUNSTACK 1
# define DELAY_THREAD_RUNSTACK_UPDATE 1
#endif

/*========================================================================*/
/*                        main eval-apply loop                            */
/*========================================================================*/

/* This is the main evaluator loop. It's roughly of the form:

   while (1) {
     if (now-applying) {
       if (apply-type-1)
         ...
       else if (apply-type-2)
         ...
       else ...
     } else {
       switch (eval-type) {
         case eval-type-1:
           ...
         case eval-type-2:
           ...
         ...
       }
     }
   }

   The main use of #ifdefs is whether to use global variables for the
   Scheme stack pointer or to use local variables. Different
   architectures work best with different choices.

 */

#ifdef MZ_REAL_THREADS
Scheme_Object *
scheme_do_eval_w_process(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
			 int get_value, Scheme_Process *p)
#else
Scheme_Object *
scheme_do_eval(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
		int get_value)
#endif
{
  Scheme_Type type;
  Scheme_Object *v;
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **old_runstack;
  GC_MAYBE_IGNORE_INTERIOR MZ_MARK_STACK_TYPE old_cont_mark_stack;
#if USE_LOCAL_RUNSTACK
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **runstack;
#endif
#ifndef MZ_REAL_THREADS
# ifdef REGISTER_POOR_MACHINE
#  define p scheme_current_process
# else
  Scheme_Process *p = scheme_current_process;
# endif
#endif

#ifdef DO_STACK_CHECK
# define SCHEME_CURRENT_PROCESS p
# ifdef MZ_REAL_THREADS
#  define SCHEME_STACK_BOUNDARY ((unsigned long)p->stack_end)
# endif
# include "mzstkchk.h"
  {
    p->ku.k.p1 = (void *)obj;
    p->ku.k.i1 = num_rands;
    if (num_rands >= 0) {
      /* Copy rands: */
      GC_CAN_IGNORE void *ra;
      ra = (void *)MALLOC_N(Scheme_Object*, num_rands);
      p->ku.k.p2 = ra;
      {
	int i;
	for (i = num_rands; i--; ) {
	  ((Scheme_Object **)p->ku.k.p2)[i] = rands[i];
	}
      }
    } else
      p->ku.k.p2 = (void *)rands;
    p->ku.k.i2 = get_value;
    return scheme_handle_stack_overflow(do_eval_k);
  }
#endif

#if USE_LOCAL_RUNSTACK
# define RUNSTACK runstack
# if DELAY_THREAD_RUNSTACK_UPDATE
#  define UPDATE_THREAD_RSPTR() (MZ_RUNSTACK = runstack)
#  define RUNSTACK_CHANGED() /**/
# else
#  define UPDATE_THREAD_RSPTR() /**/
#  define RUNSTACK_CHANGED() (MZ_RUNSTACK = runstack)
# endif
  runstack = MZ_RUNSTACK;
# define RESET_LOCAL_RUNSTACK() (runstack = MZ_RUNSTACK)
#else
# define RUNSTACK MZ_RUNSTACK
# define UPDATE_THREAD_RSPTR() /**/
# define RUNSTACK_CHANGED() /**/
# define RESET_LOCAL_RUNSTACK() /**/
#endif

#define RUNSTACK_START MZ_RUNSTACK_START

#define UPDATE_THREAD_RSPTR_FOR_GC() UPDATE_THREAD_RSPTR()
#define UPDATE_THREAD_RSPTR_FOR_ERROR() UPDATE_THREAD_RSPTR()

  MZ_CONT_MARK_POS += 2;
  old_runstack = RUNSTACK;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;

  if (num_rands >= 0) {

    if ((RUNSTACK - RUNSTACK_START) < TAIL_COPY_THRESHOLD) {
      /* It's possible that a sequence of primitive _scheme_tail_apply()
	 calls will exhaust the Scheme stack. Watch out for that. */
      p->ku.k.p1 = (void *)obj;
      p->ku.k.i1 = num_rands;
      p->ku.k.p2 = (void *)rands;
      p->ku.k.i2 = -1;
      
      UPDATE_THREAD_RSPTR();
      MZ_CONT_MARK_POS -= 2;
      return scheme_enlarge_runstack(100 * TAIL_COPY_THRESHOLD, (void *(*)(void))do_eval_k);
    }

  apply_top:

    /* DANGER: if rands == p->tail_buffer, we have to be careful to
       get the arguments out of the buffer before a GC occurs.
       (Otherwise, they'll be zeroed.) One way to make things safe for
       GC is to let rands have the buffer and create a new one. */

# define MAKE_TAIL_BUFFER_SAFE()                               \
       {                                                       \
	  GC_CAN_IGNORE Scheme_Object **tb;                    \
	  p->tail_buffer = NULL; /* so args aren't zeroed */   \
	  tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size); \
	  p->tail_buffer = tb;                                 \
	}

    if (SCHEME_INTP(obj)) {
      UPDATE_THREAD_RSPTR();
      scheme_wrong_rator(obj, num_rands, rands);
      return NULL; /* doesn't get here */
    }

    type = _SCHEME_TYPE(obj);

    if (type == scheme_prim_type) {
      GC_CAN_IGNORE Scheme_Primitive_Proc *prim;
      
      if (rands == p->tail_buffer) {
	if (num_rands < TAIL_COPY_THRESHOLD) {
	  int i;
	  Scheme_Object **quick_rands;

	  quick_rands = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();

	  for (i = num_rands; i--; ) {
	    quick_rands[i] = rands[i];
	  }
	  rands = quick_rands;
	} else {
	  UPDATE_THREAD_RSPTR_FOR_GC();
	  MAKE_TAIL_BUFFER_SAFE();
	}
      } 

      UPDATE_THREAD_RSPTR();

      prim = (Scheme_Primitive_Proc *)obj;

      if (num_rands < prim->mina 
	  || (num_rands > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count(prim->name, prim->mina, prim->maxa, 
			   num_rands, rands);
	return NULL; /* Shouldn't get here */
      }

      v = prim->prim_val(num_rands, rands);

      DEBUG_CHECK_TYPE(v);
    } else if (type == scheme_linked_closure_type) {
      Scheme_Closure_Compilation_Data *data;
      GC_CAN_IGNORE Scheme_Object **stack, **src;
      int i, has_rest, num_params;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(););
      
      data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(obj);

      if ((RUNSTACK - RUNSTACK_START) < data->max_let_depth) {
	if (rands == p->tail_buffer) {
	  UPDATE_THREAD_RSPTR_FOR_GC();
	  MAKE_TAIL_BUFFER_SAFE();
	}

	p->ku.k.p1 = (void *)obj;
	p->ku.k.i1 = num_rands;
	p->ku.k.p2 = (void *)rands;
	p->ku.k.i2 = -1;

	UPDATE_THREAD_RSPTR();
	MZ_CONT_MARK_POS -= 2;
	v = (Scheme_Object *)scheme_enlarge_runstack(data->max_let_depth, (void *(*)(void))do_eval_k);
	MZ_CONT_MARK_POS += 2;

	goto returnv;
      }

      num_params = data->num_params;
      has_rest = (data->flags & CLOS_HAS_REST);
      
      if (num_params) {
	if (has_rest) {
	  int extra, n;

	  if (num_rands < (num_params - 1)) {
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1), 
			       num_params - 1, -1,
			       num_rands, rands);
	    return NULL; /* Doesn't get here */
	  }

	  n = num_params - has_rest;
	  
	  RUNSTACK = old_runstack - num_params;
	  CHECK_RUNSTACK(p, RUNSTACK);
	  RUNSTACK_CHANGED();

	  extra = num_rands - n;
	  if (extra) {
	    Scheme_Object *rest_vals, *pairs;

	    /* This is a special case: GC may be triggered, but
	       p->runstack does not point at everything that needs
	       to be kept if args are lower on the stack. 
	       That's what runstack_tmp_keep is for. Also, if
	       runstack_tmp_keep == tail_buffer, then the buffer
	       won't be zeroed. */
	    UPDATE_THREAD_RSPTR_FOR_GC();
	    p->runstack_tmp_keep = rands;

	    rest_vals = scheme_null;
	    for (i = num_rands - 1; i >= n; --i) {
	      pairs = scheme_alloc_object();
	      pairs->type = scheme_pair_type;
	      SCHEME_CDR(pairs) = rest_vals;
	      SCHEME_CAR(pairs) = rands[i];
	      rest_vals = pairs;
	    }

	    p->runstack_tmp_keep = NULL;

	    stack = RUNSTACK;
	    stack[n] = rest_vals;
	    while (n--) {
	      stack[n] = rands[n];
	    }
	  } else {
	    stack = RUNSTACK;
	    /* Possibly, but not necessarily, rands > stack: */
	    if ((unsigned long)rands > (unsigned long)stack) {
	      int i;
	      for (i = 0; i < n; i++) {
		stack[i] = rands[i];
	      }
	      stack[n] = scheme_null;
	    } else {
	      stack[n] = scheme_null;
	      while (n--) {
		stack[n] = rands[n];
	      }
	    }
	  }
	} else {
	  if (num_rands != num_params) {
	    if (num_rands < num_params) {
	      UPDATE_THREAD_RSPTR_FOR_ERROR();
	      scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1), 
				 num_params, num_params,
				 num_rands, rands);
	      return NULL; /* Doesn't get here */
	    } else {
	      UPDATE_THREAD_RSPTR_FOR_ERROR();
	      scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1), 
				 num_params, num_params,
				 num_rands, rands);
	      return NULL; /* Doesn't get here */
	    }
	  }
	
	  stack = RUNSTACK = old_runstack - num_params;
	  CHECK_RUNSTACK(p, RUNSTACK);
	  RUNSTACK_CHANGED();

	  if (rands != stack) {
	    int n = num_params; 
	    while (n--) {
	      stack[n] = rands[n];
	    }
	  }
	}
      } else {
	if (num_rands) {
	  UPDATE_THREAD_RSPTR_FOR_ERROR();
	  scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1),
			     0, 0, num_rands, rands);
	  return NULL; /* Doesn't get here */
	}
	RUNSTACK = old_runstack;
	RUNSTACK_CHANGED();
      }
      
      {
	int n = data->closure_size;
      
	if (n) {
	  src = SCHEME_COMPILED_CLOS_ENV(obj);
	  stack = PUSH_RUNSTACK(p, RUNSTACK, n);
	  RUNSTACK_CHANGED();

	  while (n--) {
	    stack[n] = src[n];
	  }
	}
      }

      obj = data->code;

      goto eval_top;
    } else if (type == scheme_closed_prim_type) {
      GC_CAN_IGNORE Scheme_Closed_Primitive_Proc *prim;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(););
      
      if (rands == p->tail_buffer) {
	if (num_rands < TAIL_COPY_THRESHOLD) {
	  int i;
	  Scheme_Object **quick_rands;

	  quick_rands = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();

	  for (i = num_rands; i--; ) {
	    quick_rands[i] = rands[i];
	  }
	  rands = quick_rands;
	} else {
	  UPDATE_THREAD_RSPTR_FOR_GC();
	  MAKE_TAIL_BUFFER_SAFE();
	}
      }

      UPDATE_THREAD_RSPTR();

      prim = (Scheme_Closed_Primitive_Proc *)obj;
      
      if (num_rands < prim->mina 
	  || (num_rands > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count(prim->name, prim->mina, prim->maxa, 
			   num_rands, rands);
	return NULL; /* Shouldn't get here */
      }
      
      v = prim->prim_val(prim->data, num_rands, rands);

      DEBUG_CHECK_TYPE(v);
    } else if (type == scheme_case_closure_type) {
      Scheme_Case_Lambda *seq;
      Scheme_Closure_Compilation_Data *data;
      
      int i;
      
      seq = (Scheme_Case_Lambda *)obj;
      for (i = 0; i < seq->count; i++) {
	data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(seq->array[i]);
	if ((!(data->flags & CLOS_HAS_REST) 
	     && (data->num_params == num_rands))
	    || ((data->flags & CLOS_HAS_REST)
		&& (data->num_params - 1 <= num_rands))) {
	  obj = seq->array[i];
	  goto apply_top;
	}
      }
      
      UPDATE_THREAD_RSPTR_FOR_ERROR();
      scheme_wrong_count((char *)seq, -1, -1, num_rands, rands);

      return NULL; /* Doesn't get here. */
    } else if (type == scheme_cont_type) {
      Scheme_Cont *c;
      Scheme_Dynamic_Wind *dw, *common;
      Scheme_Object *value;
      
      if (num_rands != 1) {
	GC_CAN_IGNORE Scheme_Object **vals;
	int i;

	UPDATE_THREAD_RSPTR_FOR_GC();

	if (rands == p->tail_buffer)
	  MAKE_TAIL_BUFFER_SAFE();

	vals = MALLOC_N(Scheme_Object *, num_rands);
	for (i = num_rands; i--; ) {
	  vals[i] = rands[i];
	}

	value = (Scheme_Object *)vals;
      } else
	value = rands[0];
      
      c = (Scheme_Cont *)obj;
      
      DO_CHECK_FOR_BREAK(p, ;);

      if (NOT_SAME_OBJ(c->home, p)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 c,
			 "continuation application: attempted to apply foreign continuation"
			 " (created in another thread)");
      }
      if (c->ok && !*c->ok) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 c,
			 "continuation application: attempted to cross a continuation boundary");
      }
      
      p->suspend_break = 1; /* restored at call/cc destination */

      /* Find `common', then intersection of dynamic-wind chain for 
	 the current continuation and the given continuation */
      common = p->dw;
      while (common) {
	dw = c->dw;
	while (dw && dw != common) {
	  dw = dw->prev;
	}
	if (dw)
	  break;
	common = common->prev;
      }
	
      c->common = common;
      /* For dynamaic-winds after `common' in this
	 continuation, execute the post-thunks */
      for (dw = p->dw; dw != common; dw = dw->prev) {
	if (dw->post) {
	  DW_PrePost_Proc post = dw->post;
	  p->dw = dw->prev;
	  post(dw->data);
	}
      }
      
      if (num_rands == 1)
	c->value = value;
      else {
	GC_CAN_IGNORE Scheme_Object *vals;
	vals = scheme_values(num_rands, (Scheme_Object **)value);
	c->value = vals;
      }
      scheme_longjmpup(&c->buf);
      
      return NULL;
    } else if (type == scheme_escaping_cont_type) {
      Scheme_Object *value;

      if (num_rands != 1) {
	GC_CAN_IGNORE Scheme_Object **vals;
	int i;

	UPDATE_THREAD_RSPTR_FOR_GC();
	if (rands == p->tail_buffer)
	  MAKE_TAIL_BUFFER_SAFE();

	vals = MALLOC_N(Scheme_Object *, num_rands);
	for (i = num_rands; i--; ) {
	  vals[i] = rands[i];
	}
	
	value = (Scheme_Object *)vals;
	p->cjs.num_vals = num_rands;
      } else {
	value = rands[0];
	p->cjs.num_vals = 1;
      }

      if (!SCHEME_CONT_HOME(obj)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 obj,
			 "continuation application: attempt to jump into an escape continuation");
      }
      if (NOT_SAME_OBJ(SCHEME_CONT_HOME(obj), p)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 obj,
			 "continuation application: attempted to apply foreign escape continuation"
			 " (created in another thread)");
      }
      if (SCHEME_CONT_OK(obj) && !*SCHEME_CONT_OK(obj)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 obj,
			 "continuation application: attempted to cross an escape continuation boundary");
      }
      p->cjs.u.val = value;
      p->cjs.jumping_to_continuation = (Scheme_Escaping_Cont *)obj;
      scheme_longjmp(MZTHREADELEM(p, error_buf), 1);
    } else {
      UPDATE_THREAD_RSPTR_FOR_ERROR();
      scheme_wrong_rator(obj, num_rands, rands);
      return NULL; /* Doesn't get here. */
    }
  } else {

  eval_top:

    if (SCHEME_INTP(obj)) {
      v = obj;
      goto returnv;
    }

    type = _SCHEME_TYPE(obj);
    switch (type)
      {
      case scheme_variable_type:
	{
#define global_lookup(prefix, _obj, tmp)                                \
	  tmp = (Scheme_Object *)(SCHEME_VAR_BUCKET(_obj))->val;        \
	  if (!tmp) {                                                   \
            UPDATE_THREAD_RSPTR_FOR_ERROR();                            \
	    scheme_unbound_global((Scheme_Bucket *)_obj);               \
            return NULL;                                                \
	  }                                                             \
	  prefix tmp

	  global_lookup(v = , obj, v);  
	  goto returnv;
	}
      case scheme_local_type:
	{
	  v = RUNSTACK[SCHEME_LOCAL_POS(obj)];
	  goto returnv;
	}
      case scheme_local_unbox_type:
	{
	  v = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(obj)]);
	  goto returnv;
	}
      case scheme_syntax_type:
	{
	  Scheme_Syntax_Executer f;

	  UPDATE_THREAD_RSPTR();
	  f = scheme_syntax_executers[SCHEME_PINT_VAL(obj)];
	  v = f((Scheme_Object *)SCHEME_IPTR_VAL(obj));
	  break;
	}
      case scheme_application_type:
	{
	  Scheme_App_Rec *app;
	  GC_CAN_IGNORE Scheme_Object *tmpv;
	  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **randsp;
	  Scheme_Object **stack;
	  int k;
	  int d_evals;
#ifdef MZ_PRECISE_GC
# define GET_FIRST_EVAL ((char *)app)[d_evals]
#else
	  char *evals;	  
	  Scheme_Object **args;
# define GET_FIRST_EVAL evals[0]
#endif

	  app = (Scheme_App_Rec *)obj;
	  num_rands = app->num_args;
	  
	  d_evals = sizeof(Scheme_App_Rec) + (num_rands * sizeof(Scheme_Object *));
#ifndef MZ_PRECISE_GC
	  evals = ((char *)obj) + d_evals;
#endif

	  obj = app->args[0];
	  
	  stack = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();

	  /* Inline local & global variable lookups for speed */
	  switch (GET_FIRST_EVAL) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(obj =, obj, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    obj = stack[SCHEME_LOCAL_POS(obj)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    obj = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(obj)]);
	    break;
	  default:
	    obj = _scheme_eval_linked_expr_wp(obj, p);
	    break;
	  }

	  if (num_rands) {
#ifdef MZ_PRECISE_GC
	    int evalpos = 1;
#endif

	    rands = stack;
	
	    /* Inline local & global variable lookups for speed */
#ifdef MZ_PRECISE_GC
# define GET_NEXT_EVAL ((char *)app)[d_evals + evalpos++]	    
# define GET_NEXT_ARG app->args[evalpos]
#else
	    evals++;
	    args = app->args + 1;
# define GET_NEXT_EVAL *(evals++)
# define GET_NEXT_ARG *(args++)
#endif
	    randsp = rands;
	    for (k = num_rands; k--; ) {
	      v = GET_NEXT_ARG;
	      switch (GET_NEXT_EVAL) {
	      case SCHEME_EVAL_CONSTANT:
		*(randsp++) = v;
		break;
	      case SCHEME_EVAL_GLOBAL:
		global_lookup(*(randsp++) =, v, tmpv);
		break;
	      case SCHEME_EVAL_LOCAL:
		*(randsp++) = stack[SCHEME_LOCAL_POS(v)];
		break;
	      case SCHEME_EVAL_LOCAL_UNBOX:
		*(randsp++) = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(v)]);
		break;
	      default:
		{
		  GC_CAN_IGNORE Scheme_Object *er;
		  er = _scheme_eval_linked_expr_wp(v, p);
		  *(randsp++) = er;
		}
		break;
	      }

	      DEBUG_CHECK_TYPE(randsp[-1]);
	    }
	  } else
	    rands = &zero_rands_ptr;
      
	  goto apply_top;
	}
      
      case scheme_sequence_type:
	{
	  int cnt;
	  int i;
	  
	  cnt = ((Scheme_Sequence *)obj)->count - 1;
	  
	  UPDATE_THREAD_RSPTR();
	  for (i = 0; i < cnt; i++) {
	    (void)_scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[i], p);
	  }

	  obj = ((Scheme_Sequence *)obj)->array[cnt];
	  goto eval_top;
	}
      case scheme_branch_type:
	{
	  UPDATE_THREAD_RSPTR();
	  obj = (NOT_SAME_OBJ(_scheme_eval_linked_expr_wp(((Scheme_Branch_Rec *)obj)->test, p), 
			      scheme_false)
		 ? ((Scheme_Branch_Rec *)obj)->tbranch 
		 : ((Scheme_Branch_Rec *)obj)->fbranch);

	  goto eval_top;
	}
      case scheme_unclosed_procedure_type:
	UPDATE_THREAD_RSPTR();
	v = scheme_make_linked_closure(p, obj, 1);
	goto returnv;

      case scheme_let_value_type:
	{
	  GC_CAN_IGNORE Scheme_Let_Value *lv;
	  GC_CAN_IGNORE Scheme_Object *value, **values;
	  int i, c, ab;

	  lv = (Scheme_Let_Value *)obj;

	  c = lv->count;

	  i = lv->position;
	  ab = lv->autobox;
	  value = lv->value;
	  obj = lv->body;

	  UPDATE_THREAD_RSPTR();

	  if (c == 1) {
	    value = _scheme_eval_linked_expr_wp(value, p);
	    if (ab)
	      SCHEME_ENVBOX_VAL(RUNSTACK[i]) = value;
	    else
	      RUNSTACK[i] = value;
	  } else {
	    int c2;
	    GC_CAN_IGNORE Scheme_Object **stack;

	    value = _scheme_eval_linked_expr_multi_wp(value, p);
	    c2 = (SAME_OBJ(value, SCHEME_MULTIPLE_VALUES) ? p->ku.multiple.count : 1);
	    if (c2 != c) {
	      scheme_wrong_return_arity(NULL, c, c2, 
					(c2 == 1) ? (Scheme_Object **)value : p->ku.multiple.array,
					"lexical binding");
	      return NULL;
	    }

	    /* Precise GC: values++ is ok because we exit the block
	       before any GC can happen. */

	    values = p->ku.multiple.array;
	    p->ku.multiple.array = NULL;
	    stack = RUNSTACK;
	    if (ab) {
	      while (c--) {
		SCHEME_ENVBOX_VAL(stack[i]) = *values;
		values++;
		i++;
	      }
	    } else {
	      while (c--) {
		stack[i] = *values;
		values++;
		i++;
	      }
	    }
	  }

	  goto eval_top;
	}

      case scheme_let_void_type:
	{
	  GC_CAN_IGNORE Scheme_Let_Void *lv;
	  int c;

	  lv = (Scheme_Let_Void *)obj;
	  c = lv->count;
	  obj = lv->body;

	  PUSH_RUNSTACK(p, RUNSTACK, c);
	  RUNSTACK_CHANGED();

	  if (lv->autobox) {
	    Scheme_Object **stack = RUNSTACK;

	    UPDATE_THREAD_RSPTR_FOR_GC();

	    while (c--) {
	      GC_CAN_IGNORE Scheme_Object *ub;
	      ub = scheme_make_envunbox(scheme_undefined);
	      stack[c] = ub;
	    }
	  }

	  goto eval_top;
	}

      case scheme_letrec_type:
	{
	  Scheme_Letrec *l = (Scheme_Letrec *)obj;
	  Scheme_Object **a, **dest, **stack;
	  GC_CAN_IGNORE short *map;
	  int i;

	  stack = RUNSTACK;
	  a = l->procs;
	  i = l->count;
	  
	  UPDATE_THREAD_RSPTR_FOR_GC();

	  /* Create unfinished closures */
	  while (i--) {
	    Scheme_Object *uc;
	    uc = scheme_make_linked_closure(p, a[i], 0);
	    stack[i] = uc;
	  }

	  /* Close them: */
	  i = l->count;
	  while (i--) {
	    GC_CAN_IGNORE Scheme_Closed_Compiled_Procedure *closure;
	    GC_CAN_IGNORE Scheme_Closure_Compilation_Data *data;
	    int j;

	    closure = (Scheme_Closed_Compiled_Procedure *)stack[i];
	    data = (Scheme_Closure_Compilation_Data *)a[i];

	    map = data->closure_map;
	    j = data->closure_size;
	    dest = closure->vals;

	    while (j--) {
	      dest[j] = stack[map[j]];
	    }
	  }

	  obj = l->body;
	  goto eval_top;
	}

      case scheme_let_one_type:
	{
	  /* Macro instead of var for efficient precise GC conversion */
# define lo ((Scheme_Let_One *)obj)

	  PUSH_RUNSTACK(p, RUNSTACK, 1);
	  RUNSTACK_CHANGED();

	  switch (lo->eval_type) {
	  case SCHEME_EVAL_CONSTANT:
	    RUNSTACK[0] = lo->value;
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    {
	      GC_CAN_IGNORE Scheme_Object *tmpv;
	      global_lookup(RUNSTACK[0] =, lo->value, tmpv);
	    }
	    break;
	  case SCHEME_EVAL_LOCAL:
	    RUNSTACK[0] = RUNSTACK[SCHEME_LOCAL_POS(lo->value)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    RUNSTACK[0] = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(lo->value)]);
	    break;
	  default:
	    UPDATE_THREAD_RSPTR();
	    {
	      GC_CAN_IGNORE Scheme_Object *val;
	      val = _scheme_eval_linked_expr_wp(lo->value, p);
	      RUNSTACK[0] = val;
	    }
	    break;
	  }

	  obj = lo->body;
#undef lo
	  goto eval_top;
	}
      
      case scheme_with_cont_mark_type:
	{
	  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)obj;
	  GC_CAN_IGNORE Scheme_Object *key, *val;
	  
	  UPDATE_THREAD_RSPTR();
	  key = wcm->key;
	  if (SCHEME_TYPE(key) < _scheme_values_types_)
	    key = _scheme_eval_linked_expr_wp(wcm->key, p);
	  val = wcm->val;
	  if (SCHEME_TYPE(val) < _scheme_values_types_)
	    val = _scheme_eval_linked_expr_wp(wcm->val, p);

	  scheme_set_cont_mark(key, val);

	  obj = wcm->body;

	  goto eval_top;
	}
      
      default:
	v = obj;
	goto returnv;
      }
  }

  if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING)) {
    obj = p->ku.apply.tail_rator;
    num_rands = p->ku.apply.tail_num_rands;
    rands = p->ku.apply.tail_rands;
    p->ku.apply.tail_rands = NULL;
    RUNSTACK = old_runstack;
    RUNSTACK_CHANGED();
    goto apply_top;
  }

  if (SAME_OBJ(v, SCHEME_EVAL_WAITING)) {
    RESET_LOCAL_RUNSTACK();
    obj = p->ku.eval.wait_expr;
    p->ku.eval.wait_expr = NULL;
    goto eval_top;
  }

 returnv:

  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES))
    if (get_value > 0) {
      scheme_wrong_return_arity(NULL, 1, p->ku.multiple.count, 
				p->ku.multiple.array,
				NULL);
      return NULL;
    }

  MZ_RUNSTACK = old_runstack;
  MZ_CONT_MARK_STACK = old_cont_mark_stack;
  MZ_CONT_MARK_POS -= 2;

  DEBUG_CHECK_TYPE(v);

  return v;

#ifndef MZ_REAL_THREADS
# ifdef REGISTER_POOR_MACHINE
#  undef p
# endif
#endif
}

/*========================================================================*/
/*                  eval/compile/expand starting points                   */
/*========================================================================*/

Scheme_Object *scheme_eval(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_eval_compiled(scheme_compile(obj, env, 0), env);
}

Scheme_Object *scheme_eval_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_eval_compiled_multi(scheme_compile(obj, env, 0), env);
}

static void *eval_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *v;
  Scheme_Env *env;
  int isexpr, multi, expr_let_depth;

  v = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  multi = p->ku.k.i1;
  isexpr = p->ku.k.i2;
  expr_let_depth = p->ku.k.i3;

  if (isexpr) {
    if (!scheme_check_runstack(expr_let_depth)) {
      p->ku.k.p1 = v;
      p->ku.k.i1 = multi;
      p->ku.k.i2 = 1;
      return (Scheme_Object *)scheme_enlarge_runstack(expr_let_depth, eval_k);
    }

    if (multi)
      v = _scheme_eval_linked_expr_multi_wp(v, p);
    else
      v = _scheme_eval_linked_expr_wp(v, p);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)v;

    if (!scheme_check_runstack(top->max_let_depth)) {
      p->ku.k.p1 = top;
      p->ku.k.p2 = env;
      p->ku.k.i1 = multi;
      p->ku.k.i2 = 0;
      return (Scheme_Object *)scheme_enlarge_runstack(top->max_let_depth, eval_k);
    }

    v = top->code;
    v = scheme_link_expr(v, env);

    if (multi)
      v = _scheme_eval_linked_expr_multi_wp(v, p);
    else
      v = _scheme_eval_linked_expr_wp(v, p);
  } else {
    v = scheme_void;
  }

  return (void *)v;
}

static Scheme_Object *_eval(Scheme_Object *obj, Scheme_Env *env, 
			    int isexpr, int expr_let_depth,
			    int multi, int top)
{
  Scheme_Process *p = scheme_current_process;
  
  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = multi;
  p->ku.k.i2 = isexpr;
  p->ku.k.i3 = expr_let_depth;

  if (top)
    return (Scheme_Object *)scheme_top_level_do(eval_k, 1);
  else
    return (Scheme_Object *)eval_k();
}

Scheme_Object *scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 0, 1);
}

Scheme_Object *scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 1, 1);
}

Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 0, 0);
}

Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 1, 0);
}

Scheme_Object *scheme_eval_compiled_expr(Scheme_Object *obj, Scheme_Env *env, int let_depth)
{
  return _eval(scheme_link_expr(obj, env), NULL, 1, let_depth, 0, 1);
}

Scheme_Object *scheme_eval_linked_expr(Scheme_Object *obj, int let_depth)
{
  return _eval(obj, NULL, 1, let_depth, 0, 1);
}

static void *expand_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *obj;
  Scheme_Comp_Env *env;

  obj = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  if (!SCHEME_STXP(obj))
    obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 1);

  /* Renamings for imports: */
  if (env->genv->rename)
    obj = scheme_add_rename(obj, env->genv->rename);
  if (env->genv->exp_env && env->genv->exp_env->rename)
    obj = scheme_add_rename(obj, env->genv->exp_env->rename);

  return scheme_expand_expr(obj, env, p->ku.k.i1, scheme_false);
}

static Scheme_Object *_expand(Scheme_Object *obj, Scheme_Comp_Env *env, int depth, int eb)
{
  Scheme_Process *p = scheme_current_process;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = depth;

  return (Scheme_Object *)scheme_top_level_do(expand_k, eb);
}

Scheme_Object *scheme_expand(Scheme_Object *obj, Scheme_Env *env)
{
  return _expand(obj, env->init, 1, -1);
}

Scheme_Object *scheme_tail_eval_expr(Scheme_Object *obj)
{
  return scheme_tail_eval(obj);
}

static Scheme_Object *
do_default_eval_handler(Scheme_Env *env, int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = _compile(argv[0], env, 0, 0);

  return _eval(v, env, 0, 0, 1, 0);
}

/* local functions */

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Config *config;
  Scheme_Object *e;
  Scheme_Object *namespace;
  Scheme_Object *old;
} Eval_In_Env;

static void pre_eval_in_env(void *e)
{
  Eval_In_Env *ee = (Eval_In_Env *)e;
  ee->old = scheme_get_param(ee->config, MZCONFIG_ENV);
  scheme_set_param(ee->config, MZCONFIG_ENV, ee->namespace);
}

static void post_eval_in_env(void *e)
{
  Eval_In_Env *ee = (Eval_In_Env *)e;
  scheme_set_param(ee->config, MZCONFIG_ENV, ee->old);
}

static Scheme_Object *do_eval_in_env(void *e)
{
  Eval_In_Env *ee = (Eval_In_Env *)e;
  return _scheme_apply_multi(scheme_get_param(ee->config, MZCONFIG_EVAL_HANDLER),
			     1, &ee->e);
}

static Scheme_Object *
eval(int argc, Scheme_Object *argv[])
{
  if (argc == 1) {
    return _scheme_apply_multi(scheme_get_param(scheme_config, MZCONFIG_EVAL_HANDLER),
			       1, argv);
  } else {
    Eval_In_Env *ee;
    
    if (SCHEME_TYPE(argv[1]) != scheme_namespace_type)
      scheme_wrong_type("eval", "namespace", 1, argc, argv);

    ee = MALLOC_ONE_RT(Eval_In_Env);
#ifdef MZTAG_REQUIRED
    ee->type = scheme_rt_eval_in_env;
#endif
    ee->e = argv[0];
    ee->config = scheme_config;
    ee->namespace = argv[1];

    return scheme_dynamic_wind(pre_eval_in_env, do_eval_in_env, post_eval_in_env,
			       NULL, ee);
  }
}

Scheme_Object *
scheme_default_eval_handler(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return do_default_eval_handler(env, argc, argv);
}

static Scheme_Object *
current_eval(int argc, Scheme_Object **argv)
{
  return scheme_param_config("eval-handler", 
			     scheme_make_integer(MZCONFIG_EVAL_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
compile(int argc, Scheme_Object *argv[])
{
  return _compile(argv[0], scheme_get_env(scheme_config), 1, 0);
}

static Scheme_Object *
compiled_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_compilation_top_type)
	  ? scheme_true
	  : scheme_false);
}


static Scheme_Object *expand(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return _expand(argv[0], env->init, -1, 0);
}

static Scheme_Object *stop_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
				  Scheme_Compile_Info *rec, int drec)
{
  scheme_signal_error("internal error: shouldn't get to stop syntax");
  return NULL;
}

static Scheme_Object *stop_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return form;
}

Scheme_Object *scheme_get_stop_expander(void)
{
  if (!stop_expander) {
    REGISTER_SO(stop_expander);
    stop_expander = scheme_make_compiled_syntax(stop_syntax, 
						stop_expand);
  }

  return stop_expander;
}

static Scheme_Object *
local_expand(int argc, Scheme_Object **argv)
{
  Scheme_Comp_Env *env;
  Scheme_Object *l, *local_mark;

  env = scheme_current_process->current_local_env;

  if (!env)
    scheme_raise_exn(MZEXN_MISC, "local-expand: not currently transforming");

  /* For each given stop-point identifier, shadow any potential syntax
     in the environment with an identity-expanding syntax expander. */

  (void)scheme_get_stop_expander();

  env = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME, env);
  local_mark = scheme_current_process->current_local_mark;
  
  for (l = argv[1]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    Scheme_Object *i;
    
    i = SCHEME_CAR(l);
    if (!SCHEME_STX_SYMBOLP(i)) {
      scheme_wrong_type("local-expand", "list of identifier syntax", 1, argc, argv);
      return NULL;
    }
    
    scheme_add_local_syntax(i, stop_expander, env);
  }
  if (!SCHEME_NULLP(l)) {
    scheme_wrong_type("local-expand", "list of identifier syntax", 1, argc, argv);
    return NULL;
  }

  l = argv[0];

  if (!SCHEME_STXP(l))
    l = scheme_datum_to_syntax(l, scheme_false, scheme_false, 1);

  /* Since we have an expression from local context,
     we need to remove the temporary mark... */
  l = scheme_add_remove_mark(l, local_mark);

  /* Expand the expression. depth = -2 means expand all the way, but
     preserve letrec-syntax. */
  l = _expand(l, env, -2, 0);

  /* Put the temporary mark back: */
  return scheme_add_remove_mark(l, local_mark);
}

static Scheme_Object *
expand_once(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return _expand(argv[0], env->init, 1, 0);
}

Scheme_Object *scheme_eval_string_all(const char *str, Scheme_Env *env, int cont)
{
  Scheme_Object *port, *expr, *result = scheme_void;

  port = scheme_make_string_input_port(str);
  do {
    expr = scheme_read_syntax(port, scheme_false);
    if (SAME_OBJ(expr, scheme_eof))
      cont = 0;
    else if (cont < 0)
      result = scheme_eval(expr, env);
    else
      result = scheme_eval_multi(expr, env);
  } while (cont > 0);

  return result;
}

Scheme_Object *scheme_eval_string(const char *str, Scheme_Env *env)
{
  return scheme_eval_string_all(str, env, -1);
}

Scheme_Object *scheme_eval_string_multi(const char *str, Scheme_Env *env)
{
  return scheme_eval_string_all(str, env, 0);
}

static Scheme_Object *allow_auto_cond_else(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-auto-cond-else", 
			     scheme_make_integer(MZCONFIG_COND_AUTO_ELSE),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-allow-set!-undefined", 
			     scheme_make_integer(MZCONFIG_ALLOW_SET_UNDEFINED),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *
enable_break(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Process *p = scheme_current_process;

  v = scheme_param_config("break-enabled", 
			  scheme_make_integer(MZCONFIG_ENABLE_BREAK),
			  argc, argv,
			  -1, NULL, NULL, 1);

  if (argc == 1) /* might have turned on breaking... */
    if (p->external_break && scheme_can_break(p, p->config))
      scheme_process_block_w_process(0.0, p);

  return v;
}

/*========================================================================*/
/*       [un]marshalling application, branch, sequence, wcm bytecode      */
/*========================================================================*/

#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_application(Scheme_Object *obj)
{
  Scheme_App_Rec *app;
  Scheme_Object *l;
  int i;

  app = (Scheme_App_Rec *)obj;

  l = scheme_null;
  for (i = app->num_args + 1; i--; ) {
    l = cons(scheme_protect_quote(app->args[i]), l);
  }
  
  return l;
}

static Scheme_Object *read_application(Scheme_Object *obj)
{
  /* (void) optimization: */
  if (SCHEME_PAIRP(obj) && SCHEME_NULLP(SCHEME_CDR(obj))
      && SAME_OBJ(SCHEME_CAR(obj), scheme_void_func))
    return scheme_void;

  return make_application(obj, 1);
}

static Scheme_Object *write_sequence(Scheme_Object *obj)
{
  Scheme_Object *l, **a;
  int i;

  i = ((Scheme_Sequence *)obj)->count;
  a = ((Scheme_Sequence *)obj)->array;

  l = scheme_null;
  for (; i--; ) {
    l = cons(scheme_protect_quote(a[i]), l);
  }
  
  return l;
}

static Scheme_Object *read_sequence(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, 1);
}

static Scheme_Object *read_sequence_save_first(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, -1);
}

static Scheme_Object *write_branch(Scheme_Object *obj)
{
  Scheme_Branch_Rec *b;

  b = (Scheme_Branch_Rec *)obj;

  return cons(scheme_protect_quote(b->test),
	      cons(scheme_protect_quote(b->tbranch), 
		   scheme_protect_quote(b->fbranch)));
}

static Scheme_Object *read_branch(Scheme_Object *obj)
{
#if 0
  if (!SCHEME_PAIRP(obj) || !SCHEME_PAIRP(SCHEME_CDR(obj)))
    scheme_signal_error("bad compiled branch");
#endif

  return scheme_make_branch(SCHEME_CAR(obj), 
			    SCHEME_CAR(SCHEME_CDR(obj)),
			    SCHEME_CDR(SCHEME_CDR(obj)));
}

static Scheme_Object *write_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = (Scheme_With_Continuation_Mark *)obj;

  return cons(scheme_protect_quote(wcm->key),
	      cons(scheme_protect_quote(wcm->val),
		   scheme_protect_quote(wcm->body)));
}

static Scheme_Object *read_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->type = scheme_with_cont_mark_type;
  wcm->key = SCHEME_CAR(obj);
  wcm->val = SCHEME_CADR(obj);
  wcm->body = SCHEME_CDR(SCHEME_CDR(obj));

  return (Scheme_Object *)wcm;
}

static Scheme_Object *write_syntax(Scheme_Object *obj)
{
  Scheme_Object *idx, *rest, *l;
  int protect_after, c;

  c = SCHEME_PINT_VAL(obj);
  idx = scheme_make_integer(c);
  protect_after = scheme_syntax_protect_afters[c];

  l = rest = (Scheme_Object *)SCHEME_IPTR_VAL(obj);
  for (c = 0; SCHEME_PAIRP(l) && (c < protect_after); c++) {
    l = SCHEME_CDR(l);
  }
  if (!SCHEME_NULLP(l) && (c == protect_after)) {
    Scheme_Object *new_l;

    new_l = scheme_protect_quote(l);

    if (new_l != l) {
      Scheme_Object *first = NULL, *last = NULL;
      
      while (rest != l) {
	Scheme_Object *p;
	
	p = scheme_make_pair(SCHEME_CAR(rest), scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;

	rest = SCHEME_CDR(rest);
      }
      
      if (last)
	SCHEME_CDR(last) = new_l;
      else
	first = new_l;
      
      rest = first;
    }
  }

  return cons(idx, rest);
}

static Scheme_Object *read_syntax(Scheme_Object *obj)
{
  Scheme_Object *idx;
  Scheme_Object *first = NULL, *last = NULL;

#if 0
  if (!SCHEME_PAIRP(obj) || !SCHEME_SYMBOLP(SCHEME_CAR(obj)))
    scheme_signal_error("bad compiled syntax");
#endif

  idx = SCHEME_CAR(obj);

  /* Copy obj: */
  obj = SCHEME_CDR(obj);
  while (SCHEME_PAIRP(obj)) {
    Scheme_Object *p;
    p = scheme_make_pair(SCHEME_CAR(obj), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
    obj = SCHEME_CDR(obj);
  }
  
  if (last)
    SCHEME_CDR(last) = obj;
  else
    first = obj;

  return scheme_make_syntax_resolved(SCHEME_INT_VAL(idx), first);
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_EVAL_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_compile_info, mark_comp_info);
  GC_REG_TRAV(scheme_rt_saved_stack, mark_saved_stack);
  GC_REG_TRAV(scheme_rt_eval_in_env, mark_eval_in_env);
}

END_XFORM_SKIP;

#endif
