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
   pass). */

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

#define EMBEDDED_DEFINES_START_ANYWHERE 0

/* globals */
Scheme_Object *scheme_eval_waiting;
Scheme_Object *scheme_multiple_values;

volatile int scheme_fuel_counter;

int scheme_stack_grows_up;

static Scheme_Object *app_symbol;
static Scheme_Object *datum_symbol;
static Scheme_Object *top_symbol;

static Scheme_Object *app_expander;
static Scheme_Object *datum_expander;
static Scheme_Object *top_expander;

static Scheme_Object *stop_expander;

static Scheme_Object *quick_stx;
static int quick_stx_in_use;
static int taking_shortcut;

/* locals */
static Scheme_Object *eval(int argc, Scheme_Object *argv[]);
static Scheme_Object *compile(int argc, Scheme_Object *argv[]);
static Scheme_Object *compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand(int argc, Scheme_Object **argv);
static Scheme_Object *expand_once(int argc, Scheme_Object **argv);
static Scheme_Object *enable_break(int, Scheme_Object *[]);
static Scheme_Object *current_eval(int argc, Scheme_Object *[]);

static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv);

static Scheme_Object *app_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *app_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *datum_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *top_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *top_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

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
static Scheme_Object *letrec_syntaxes_symbol, *begin_symbol;
static Scheme_Object *let_symbol;

static Scheme_Object *internal_define_symbol;
static Scheme_Object *module_symbol;
static Scheme_Object *expression_symbol;
static Scheme_Object *top_level_symbol;

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
unsigned long scheme_stack_boundary;
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
  REGISTER_SO(letrec_syntaxes_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(let_symbol);
  
  define_values_symbol = scheme_intern_symbol("define-values");
  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_symbol = scheme_intern_symbol("let");
  lambda_symbol = scheme_intern_symbol("lambda");
  unknown_symbol = scheme_intern_symbol("unknown");
  void_link_symbol = scheme_intern_symbol("-v");
  quote_symbol = scheme_intern_symbol("quote");
  letrec_syntaxes_symbol = scheme_intern_symbol("letrec-syntaxes+values");
  begin_symbol = scheme_intern_symbol("begin");
  
  REGISTER_SO(module_symbol);
  REGISTER_SO(internal_define_symbol);
  REGISTER_SO(expression_symbol);
  REGISTER_SO(top_level_symbol);

  module_symbol = scheme_intern_symbol("module");
  internal_define_symbol = scheme_intern_symbol("internal-define");
  expression_symbol = scheme_intern_symbol("expression");
  top_level_symbol = scheme_intern_symbol("top-level");

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
						      3, 3), 
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

  scheme_add_global_constant("compile-allow-set!-undefined", 
			     scheme_register_parameter(allow_set_undefined, 
						       "compile-allow-set!-undefined",
						       MZCONFIG_ALLOW_SET_UNDEFINED), 
			     env);

  REGISTER_SO(app_symbol);
  REGISTER_SO(datum_symbol);
  REGISTER_SO(top_symbol);

  app_symbol = scheme_intern_symbol("#%app");
  datum_symbol = scheme_intern_symbol("#%datum");
  top_symbol = scheme_intern_symbol("#%top");

  REGISTER_SO(app_expander);
  REGISTER_SO(datum_expander);
  REGISTER_SO(top_expander);

  app_expander = scheme_make_compiled_syntax(app_syntax,
					     app_expand);
  scheme_add_global_keyword("#%app", 
			    app_expander, 
			    env);

  datum_expander = scheme_make_compiled_syntax(datum_syntax,
					       datum_expand);
  scheme_add_global_keyword("#%datum", 
			    datum_expander, 
			    env);

  top_expander = scheme_make_compiled_syntax(top_syntax,
					     top_expand);
  scheme_add_global_keyword("#%top", 
			    top_expander, 
			    env);

  REGISTER_SO(quick_stx);
  quick_stx = scheme_datum_to_syntax(app_symbol, scheme_false, scheme_false, 0, 0);
}

/*========================================================================*/
/*                   C stack and Scheme stack handling                    */
/*========================================================================*/

# define DO_CHECK_FOR_BREAK(p, e) \
	if (DECREMENT_FUEL(scheme_fuel_counter, 1) <= 0) { \
	  e scheme_thread_block(0); \
          (p)->ran_some = 1; \
	}

Scheme_Object *
scheme_handle_stack_overflow(Scheme_Object *(*k)(void))
{
  scheme_overflow_k = k;
  scheme_init_jmpup_buf(&scheme_overflow_cont);
  scheme_zero_unneeded_rands(scheme_current_thread);
  if (scheme_setjmpup(&scheme_overflow_cont, scheme_current_thread,
		      scheme_current_thread->cc_start)) {
    scheme_reset_jmpup_buf(&scheme_overflow_cont);
    if (!scheme_overflow_reply) {
      scheme_longjmp(scheme_error_buf, 1);
    } else {
      Scheme_Object *reply = scheme_overflow_reply;
      scheme_overflow_reply = NULL;
      return reply;
    }
  } else
    scheme_longjmp(scheme_current_thread->overflow_buf, 1);
  return NULL; /* never gets here */
}

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
  scheme_stack_boundary = scheme_get_stack_base();
  if (scheme_stack_grows_up)
    scheme_stack_boundary += (FIXED_STACK_SIZE - STACK_SAFETY_MARGIN);
  else
    scheme_stack_boundary += (STACK_SAFETY_MARGIN - FIXED_STACK_SIZE);
#endif

#ifdef WINDOWS_FIND_STACK_BOUNDS
  scheme_stack_boundary = scheme_get_stack_base();
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
    unsigned long bnd, lim;
    bnd = (unsigned long)scheme_get_stack_base();

    lim = (unsigned long)rl.rlim_cur;
# ifdef UNIX_STACK_MAXIMUM
    if (lim > UNIX_STACK_MAXIMUM)
      lim = UNIX_STACK_MAXIMUM;
# endif

    if (scheme_stack_grows_up)
      bnd += (lim - STACK_SAFETY_MARGIN);
    else
      bnd += (STACK_SAFETY_MARGIN - lim);

    scheme_stack_boundary = bnd;
  }
#endif
}


int scheme_check_runstack(long size)
{
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Thread *p = scheme_current_thread;
#endif

  return ((MZ_RUNSTACK - MZ_RUNSTACK_START) >= (size + TAIL_COPY_THRESHOLD));
}

void *scheme_enlarge_runstack(long size, void *(*k)())
{
  Scheme_Thread *p = scheme_current_thread;
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

int scheme_omittable_expr(Scheme_Object *o, int vals)
{
  Scheme_Type vtype;

 try_again:

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_compiled_values_types_) 
      || (vtype == scheme_local_type)
      || (vtype == scheme_local_unbox_type)
      || (vtype == scheme_unclosed_procedure_type)
      || (vtype == scheme_compiled_unclosed_procedure_type)
      || (vtype == scheme_case_lambda_sequence_type))
    return (vals == 1);

  if ((vtype == scheme_compiled_quote_syntax_type)) {
    return (vals == 1);
  }

  if ((vtype == scheme_branch_type)) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test, 1)
	    && scheme_omittable_expr(b->tbranch, vals)
	    && scheme_omittable_expr(b->fbranch, vals));
  }

#if 0
  /* We can't do this because a set! to a lexical is turned into
     a let_value_type! */
  if ((vtype == scheme_let_value_type)) {
    Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
    return (scheme_omittable_expr(lv->value, lv->count)
	    && scheme_omittable_expr(lv->body, vals));
  }
#endif

  if ((vtype == scheme_let_one_type)) {
    Scheme_Let_One *lo = (Scheme_Let_One *)o;
    return (scheme_omittable_expr(lo->value, 1)
	    && scheme_omittable_expr(lo->body, vals));
  }

  if ((vtype == scheme_let_void_type)) {
    o = ((Scheme_Let_Void *)o)->body;
    goto try_again;
  }

  if ((vtype == scheme_letrec_type)) {
    o = ((Scheme_Letrec *)o)->body;
    goto try_again;
  }

  if ((vtype == scheme_syntax_type)) {
    int expd;
    expd = SCHEME_PINT_VAL(o);
  }

  if ((vtype == scheme_application_type)) {
    /* Look for multiple values */
    Scheme_App_Rec *app = (Scheme_App_Rec *)o;
    if (app->num_args == vals) {
      if (SAME_OBJ(scheme_values_func, app->args[0])) {
	int i;
	for (i = vals; i--; ) {
	  if (!scheme_omittable_expr(app->args[i + 1], 1))
	    return 0;
	}
	return 1;
      }
    }
    return 0;
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
  else if (SAME_TYPE(type, scheme_toplevel_type))
    return SCHEME_EVAL_GLOBAL;
  else
    return SCHEME_EVAL_GENERAL;
}    

static Scheme_Object *try_apply(Scheme_Object *f, Scheme_Object *args)
{
  Scheme_Object * volatile result;
  mz_jmp_buf savebuf;
  scheme_current_thread->error_invoked = 5;
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

  if (scheme_setjmp(scheme_error_buf))
    result = NULL;
  else
    result = _scheme_apply_to_list(f, args);
  
  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
  scheme_current_thread->error_invoked = 0;  

  return result;
}

static Scheme_Object *make_application(Scheme_Object *v)
{
  Scheme_Object *o;
  Scheme_App_Rec *app;
  int i, nv;
  volatile int n;

  o = v;
  n = 0;
  nv = 0;
  while (!SCHEME_NULLP(o)) {
    Scheme_Type type;
    
    n++;
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

  app = scheme_malloc_application(n);

  for (i = 0; i < n; i++, v = SCHEME_CDR(v)) {
    app->args[i] = SCHEME_CAR(v);
  }

  return (Scheme_Object *)app;
}

Scheme_App_Rec *scheme_malloc_application(int n)
{
  Scheme_App_Rec *app;
  int size;

  size = (sizeof(Scheme_App_Rec) 
	  + ((n - 1) * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  app = (Scheme_App_Rec *)scheme_malloc_tagged(size);

  app->type = scheme_application_type;

  app->num_args = n - 1;

  return app;
}

void scheme_finish_application(Scheme_App_Rec *app)
{
  int i, devals, n;

  n = app->num_args + 1;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));

  for (i = 0; i < n; i++) {
    char etype;
    etype = scheme_get_eval_type(app->args[i]);
    ((char *)app + devals)[i] = etype;
  }
}

static Scheme_Object *resolve_application(Scheme_Object *o, Resolve_Info *info)
{
  Scheme_App_Rec *app;
  int i, n, devals;

  app = (Scheme_App_Rec *)o;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));
  
  n = app->num_args + 1;

  info = scheme_resolve_info_extend(info, n - 1, 0, 0, 0);

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
  Scheme_Object *list, *v, *good;
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
	       && scheme_omittable_expr(v, 1)) {
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
    if ((opt < 0) && !scheme_omittable_expr(SCHEME_CAR(seq), 1)) {
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
  
  --total;
  for (i = k = 0; i < count; k++) {
    v = SCHEME_CAR(seq);
    seq = SCHEME_CDR(seq);

    if (((opt > 0) || k) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      int c, j;
      Scheme_Object **a;

      c = ((Scheme_Sequence *)v)->count;
      a = ((Scheme_Sequence *)v)->array; /* <-- mismaligned for precise GC */
      for (j = 0; j < c; j++) {
	o->array[i++] = a[j];
      }
    } else if (opt 
	       && ((opt > 0 && (k < total))
		   || ((opt < 0) && k))
	       && scheme_omittable_expr(v, 1)) {
      /* Value not the result. Do nothing. */
    } else
      o->array[i++] = v;
  }

  if (addconst)
    o->array[i] = scheme_make_integer(0);
  
  return (Scheme_Object *)o;
}

static Scheme_Object *look_for_letv_change(Scheme_Sequence *s)
{
  int i;

  /* Change (begin e1 ... (set!-for-let [x 10] (void)) e2 ...)
     to (begin e1 ... (set!-for-let [x 10] e2 ...)), which 
     avoids an unneeded recursive call in the evaluator */

  for (i = 0; i < s->count - 1; i++) {
    Scheme_Object *v;
    v = s->array[i];
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_let_value_type)) {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)v;
      if (scheme_omittable_expr(lv->body, 1)) {
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
					   int pos,
					   Scheme_Env *env)
{
  Scheme_Object *modname;
  Scheme_Env *menv;

  /* If it's a name id, resolve the name. */
  modname = scheme_module_resolve(modidx);

  menv = scheme_module_access(modname, env);
  
  if (!menv && env->phase) {
    /* The failure might be due a laziness in required-syntax
       execution. Force all laziness at the prior level 
       and try again. */
    scheme_module_force_lazy(env, 1);
    menv = scheme_module_access(modname, env);
  }

  if (!menv) {
    scheme_wrong_syntax("link", NULL, varname,
			"broken compiled code (phase %d, in %V), no declaration for module"
			": %S", 
			env->phase, 
			env->module ? env->module->modname : scheme_false,
			modname);
    return NULL;
  }

  if (!SAME_OBJ(menv, env))
    varname = scheme_check_accessible_in_module(menv, varname, NULL, pos, 0);
      
  return (Scheme_Object *)scheme_global_bucket(varname, menv);
}

Scheme_Object *scheme_link_toplevel(Scheme_Object *expr, Scheme_Env *env)
{
  if (SAME_TYPE(SCHEME_TYPE(expr), scheme_variable_type)) {
    Scheme_Bucket_With_Home *b = (Scheme_Bucket_With_Home *)expr;
    
    if (!env || !b->home->module)
      return (Scheme_Object *)b;
    else
      return link_module_variable(b->home->module->modname,
				  (Scheme_Object *)b->bucket.bucket.key,
				  -1,
				  env);
  } else {
    Module_Variable *mv = (Module_Variable *)expr;
    
    return link_module_variable(mv->modidx,
				mv->sym,
				mv->pos,
				env);
  }
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
  case scheme_compiled_toplevel_type:
    return scheme_resolve_toplevel(info, expr);
  case scheme_compiled_quote_syntax_type:
    {
      int p;

      p = scheme_resolve_quote_syntax(info, SCHEME_LOCAL_POS(expr));

      return scheme_make_local(scheme_local_type, p);
    }
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
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

    return scheme_make_immutable_pair(c1, c2);
  } else
    return scheme_compile_expr(form, env, rec, drec);
}

static Scheme_Object *compile_application(Scheme_Object *form, Scheme_Comp_Env *env,
					  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *result;
  int len;

  len = scheme_stx_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, NULL);
  
  scheme_compile_rec_done_local(rec, drec);
  form = scheme_inner_compile_list(form, scheme_no_defines(env), rec, drec, 1);

  result = make_application(form);

  if (SAME_TYPE(SCHEME_TYPE(result), scheme_application_type))
    rec[drec].max_let_depth += (len - 1);
  
  return result;
}

Scheme_Object *
scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  return scheme_inner_compile_list(form, env, rec, drec, 0);
}

static void *compile_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form;
  int writeable;
  Scheme_Comp_Env *env;
  Scheme_Compile_Info rec;
  Scheme_Object *o;
  Scheme_Compilation_Top *top;
  Resolve_Prefix *rp;

  form = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;
  writeable = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  rec.dont_mark_local_use = 0;
  rec.resolve_module_ids = !writeable && !env->genv->module;
  rec.value_name = NULL;

  if (!SCHEME_STXP(form))
    form = scheme_datum_to_syntax(form, scheme_false, scheme_false, 1, 0);

  /* Renamings for requires: */
  if (env->genv->rename)
    form = scheme_add_rename(form, env->genv->rename);
  if (env->genv->exp_env && env->genv->exp_env->rename)
    form = scheme_add_rename(form, env->genv->exp_env->rename);

  o = scheme_compile_expr(form, env, &rec, 0);

  rp = scheme_resolve_prefix(0, env->prefix, 1);

  o = scheme_resolve_expr(o, scheme_resolve_info_create(rp));

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  top->type = scheme_compilation_top_type;
  top->max_let_depth = rec.max_let_depth;
  top->code = o;
  top->prefix = rp;

  return (void *)top;
}

static Scheme_Object *_compile(Scheme_Object *form, Scheme_Env *env, int writeable, int eb)
{
  Scheme_Comp_Env *cenv;
  Scheme_Thread *p = scheme_current_thread;

  if (SAME_TYPE(SCHEME_TYPE(form), scheme_compilation_top_type))
    return form;

  if (SCHEME_STXP(form)) {
    if (SAME_TYPE(SCHEME_TYPE(SCHEME_STX_VAL(form)), scheme_compilation_top_type))
      return SCHEME_STX_VAL(form);
  }

  cenv = scheme_new_comp_env(env, SCHEME_TOPLEVEL_FRAME);

  p->ku.k.p1 = form;
  p->ku.k.p2 = cenv;
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
					    int internel_def_pos,
					    Scheme_Object **current_val)
{
  Scheme_Object *name, *val, *orig;
  Scheme_Comp_Env *xenv = NULL;

  orig = first;

 check_top:
  *current_val = NULL;

  if (SCHEME_STX_PAIRP(first))
    name = SCHEME_STX_CAR(first);
  else
    name= first;

  if (!SCHEME_STX_SYMBOLP(name))
    return first;

  val = scheme_lookup_binding(name, env, 
			      SCHEME_NULL_FOR_UNBOUND
			      + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			      + ((rec && rec[drec].dont_mark_local_use) 
				 ? SCHEME_DONT_MARK_USE 
				 : 0)
			      + ((rec && rec[drec].resolve_module_ids)
				 ? SCHEME_RESOLVE_MODIDS
				 : 0));

  if (SCHEME_STX_PAIRP(first))
    *current_val = val;

  if (!val) {
    return first;
  } else if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type)) {
    /* Yep, it's a macro; expand once. Also, extend env to indicate
       an internal-define position, if necessary. */
    if (!xenv) {
      if (internel_def_pos)
	xenv = scheme_new_compilation_frame(0, SCHEME_INTDEF_FRAME, env);
      else
	xenv = env;
    }
    first = scheme_expand_expr(first, xenv, 1, boundname);
  } else {
    return first;
  }

  if (SCHEME_STX_PAIRP(first))
    goto check_top;
  
  return first;
}

static Scheme_Object *
compile_expand_macro_app(Scheme_Object *name, Scheme_Object *macro,
			 Scheme_Object *form, Scheme_Comp_Env *env,
			 Scheme_Compile_Info *rec, int drec, 
			 int depth, Scheme_Object *boundname)
{
  Scheme_Object *xformer;

  xformer = (Scheme_Object *)SCHEME_PTR_VAL(macro);

  if (SAME_TYPE(SCHEME_TYPE(xformer), scheme_id_macro_type)) {
    xformer = SCHEME_PTR_VAL(xformer);
  } else if (!scheme_check_proc_arity(NULL, 1, 0, -1, &xformer)) {
    scheme_wrong_syntax(NULL, NULL, form, "illegal use of syntax");
    return NULL;
  }

  if (rec)
    boundname = rec[drec].value_name;
  if (!boundname)
    boundname = scheme_false;

  return scheme_apply_macro(name, xformer, form, env, boundname);

  /* caller expects rec[drec] to be used to compile the result... */
}

static Scheme_Object *compile_expand_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
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
  Scheme_Object *name, *var, *rest, *stx, *normal;
  GC_CAN_IGNORE char *not_allowed;

 top:

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.p3 = (void *)rec;
      p->ku.k.p4 = (void *)boundname;
      p->ku.k.i3 = drec;
      p->ku.k.i1 = depth;
      p->ku.k.i2 = app_position;
      return scheme_handle_stack_overflow(compile_expand_expr_k);
    }
  }
#endif

  DO_CHECK_FOR_BREAK(scheme_current_thread, ;);

#if 1
  if (!SCHEME_STXP(form))
    scheme_signal_error("not syntax");
#endif

  if (rec)
    scheme_default_compile_rec(rec, drec);

  if (SCHEME_STX_NULLP(form)) {
    stx = app_symbol;
    not_allowed = "function application";
    normal = app_expander;
  } else if (!SCHEME_STX_PAIRP(form)) {
    if (SCHEME_STX_SYMBOLP(form)) {
      var = scheme_lookup_binding(form, env, 
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
	/* Top variable */
	stx = top_symbol;
	not_allowed = "reference to top-level identifiers";
	normal = top_expander;
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	  if (var == stop_expander)
	    return form;
	  else {
	    scheme_wrong_syntax(NULL, NULL, form, "bad syntax");
	    return NULL;
	  }
	} else if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	  name = form;
	  goto macro;
	}
	
	if (rec) {
	  scheme_compile_rec_done_local(rec, drec);
	  if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type))
	    return scheme_register_toplevel_in_prefix(var, env, rec, drec);
	  else
	    return var;
	} else
	  return form;
      }
    } else {
      stx = datum_symbol;
      not_allowed = "literal data";
      normal = datum_expander;
    }
  } else {
    name = SCHEME_STX_CAR(form);
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_SYMBOLP(name)) {
      /* Check for macros: */
      var = scheme_lookup_binding(name, env, 
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
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	  goto macro;
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
    not_allowed = "function application";
    normal = app_expander;
  }

  /* Compile/expand as application, datum, or top: */
  if (!quick_stx_in_use && rec) {
    quick_stx_in_use = 1;
    ((Scheme_Stx *)quick_stx)->val = stx;
    ((Scheme_Stx *)quick_stx)->wraps = ((Scheme_Stx *)form)->wraps;
    stx = quick_stx;
  } else
    stx = scheme_datum_to_syntax(stx, scheme_false, form, 0, 0);

  var = scheme_lookup_binding(stx, env,
			      SCHEME_NULL_FOR_UNBOUND
			      + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			      + SCHEME_DONT_MARK_USE);

  if (SAME_OBJ(stx, quick_stx)) {
    quick_stx_in_use = 0;
    if (!SAME_OBJ(var, normal)) {
      /* Need a new stx after all: */
      stx = scheme_datum_to_syntax(SCHEME_STX_VAL(stx), scheme_false, form, 0, 0);
    }
  }

  if (var && (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
	      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type))) {
    if (SAME_OBJ(var, stop_expander)) {
      /* Return original: */
      return form;
    } else if (rec && SAME_OBJ(var, normal)) {
      /* Skip creation of intermediate form */
      Scheme_Syntax *f;
      taking_shortcut = 1;
      f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
      return f(form, env, rec, drec);
    } else {
      form = scheme_datum_to_syntax(scheme_make_immutable_pair(stx, form), form, form, 0, 1);
      
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
	name = stx;
	goto macro;
      }
    }
  } else {
    /* Not allowed this context! */
    scheme_wrong_syntax(scheme_compile_stx_string, NULL, form, 
			"bad syntax; %s is not allowed, "
			"because no %S syntax transformer is bound",
			not_allowed,
			SCHEME_STX_VAL(stx));
    return NULL;
  }

 macro:
  if (!rec && !depth)
    return form; /* We've gone as deep as requested */

  form = compile_expand_macro_app(name, var, form, env, rec, drec, depth, boundname);
  if (rec)
    goto top;
  else {
    if (depth > 0)
      --depth;
    if (depth)
      goto top;
    else
      return form;
  }
}

static Scheme_Object *
compile_expand_app(Scheme_Object *forms, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec, int drec, 
		   int depth, Scheme_Object *boundname)
{
  Scheme_Object *form, *naya;

  if (taking_shortcut) {
    form = forms;
    taking_shortcut = 0;
  } else {
    form = SCHEME_STX_CDR(forms);
    form = scheme_datum_to_syntax(form, forms, forms, 0, 0);
  }
  
  if (SCHEME_STX_NULLP(form)) {
    /* Compile/expand empty application to null list: */
    if (rec)
      return scheme_null;
    else
      return forms;
  } else if (!SCHEME_STX_PAIRP(form) /* will end in error */
	     || SCHEME_STX_SYMBOLP(SCHEME_CAR(form))) {
    if (rec)
      return compile_application(form, env, rec, drec);
    else {
      naya = scheme_expand_list(form, scheme_no_defines(env), depth, scheme_false);
      /* naya will be prefixed and returned... */
    }
  } else if (rec) {
    Scheme_Object *name;
    name = SCHEME_STX_CAR(form);
    /* look for ((lambda (x) ...) ...); */
    /* rator as a macro has to be a parenthesized expr, otherwise the
       parens for application would have been the macro call. */
    if (SCHEME_STX_PAIRP(name) && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(name))) {
      Scheme_Object *gval, *origname = name;

      name = scheme_check_immediate_macro(name, env, rec, drec, depth, boundname, 0, &gval);
      
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
									 0, 1),
						  env, rec, drec, depth, boundname, 0);
	      } else {
#if 0
 		scheme_wrong_syntax(scheme_application_stx_string, NULL, form, 
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
	form = scheme_datum_to_syntax(scheme_make_immutable_pair(name, form), forms, forms, 0, 1);
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
    return scheme_datum_to_syntax(scheme_make_immutable_pair(first, naya),
				  forms,
				  forms, 0, 1);
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

  if (taking_shortcut) {
    c = form;
    taking_shortcut = 0;
  } else {
    c = SCHEME_STX_CDR(form);
    /* Need datum->syntax, in case c is a list: */
    c = scheme_datum_to_syntax(c, form, form, 0, 1);
  }

  return scheme_syntax_to_datum(c, 0, NULL);
}

static Scheme_Object *
datum_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return form;
}

static Scheme_Object *check_top(const char *when, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *c;

  if (taking_shortcut) {
    c = form;
    taking_shortcut = 0;
  } else
    c = SCHEME_STX_CDR(form);

  if (!SCHEME_STX_SYMBOLP(c))
    scheme_wrong_syntax(NULL, NULL, form, NULL);

  if (env->genv->module) {
    Scheme_Object *modidx, *symbol = c;
    int bad;

    modidx = scheme_stx_module_name(&symbol, env->genv->phase, NULL, NULL);
    if (modidx) {
      /* If it's an access path, resolve it: */
      if (env->genv->module
	  && SAME_OBJ(scheme_module_resolve(modidx), env->genv->module->modname))
	bad = 0;
      else
	bad = 1;
    } else
      bad = 1;

    if (bad || !scheme_lookup_in_table(env->genv->toplevel, (const char *)SCHEME_STX_SYM(c)))
      scheme_wrong_syntax(when, NULL, c, 
			  (env->genv->phase
			   ? "unbound variable in module (transformer environment)"
			   : "unbound variable in module"));
  }

  return c;
}

static Scheme_Object *
top_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *c;

  c = check_top(scheme_compile_stx_string, form, env);

  if (env->genv->module && !rec[drec].resolve_module_ids) {
    /* Self-reference in a module; need to remember the modidx.  Don't
       need a pos, because the symbol's gensym-ness (if any) will be
       preserved within the module. */
    c =  scheme_hash_module_variable(env->genv, env->genv->module->self_modidx, c, -1);
  } else
    c = (Scheme_Object *)scheme_global_bucket(SCHEME_STX_SYM(c), env->genv);

  return scheme_register_toplevel_in_prefix(c, env, rec, drec);
}

static Scheme_Object *
top_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *c;

  check_top(scheme_expand_stx_string, form, env);

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
    if (rec) {
      scheme_compile_rec_done_local(rec, drec);
      return scheme_null;
    } else
      return forms;
  }

  if (SCHEME_STX_PAIRP(forms)) {
    Scheme_Object *rest;
    rest = SCHEME_STX_CDR(forms);
  }

 try_again:

  if (!SCHEME_STX_PAIRP(forms)) {
    scheme_wrong_syntax(scheme_begin_stx_string, NULL, forms, "bad syntax");
    return NULL;
  }

  first = SCHEME_STX_CAR(forms);

  {
    Scheme_Object *gval, *result;
    int more = 1;

    result = forms;

    /* Check for macro expansion, which could mask the real
       define-values, define-syntax, etc.: */
    first = scheme_check_immediate_macro(first, env, rec, drec, depth, scheme_false, 1, &gval);

    if (SAME_OBJ(gval, scheme_begin_syntax)) {
      /* Inline content */
      Scheme_Object *orig_forms = forms;
      Scheme_Object *content;

      content = SCHEME_STX_CDR(first);

      if (scheme_stx_proper_list_length(content) < 0)
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, first, 
			    "bad syntax (" IMPROPER_LIST_FORM ")");

      forms = SCHEME_STX_CDR(forms);
      forms = scheme_append(scheme_flatten_syntax_list(content, NULL),
			    forms);

      if (SCHEME_STX_NULLP(forms)) {
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, first, 
			    "bad syntax (empty form)");
      }

      forms = scheme_datum_to_syntax(forms, orig_forms, orig_forms, 0, 0);

      goto try_again;
    } else if (SAME_OBJ(gval, scheme_define_values_syntax)
	       || SAME_OBJ(gval, scheme_define_syntaxes_syntax)) {
      /* Turn defines into a letrec: */
      Scheme_Object *var, *vars, *v, *link;
      Scheme_Object *l = scheme_null, *start = NULL;
      Scheme_Object *stx_l = scheme_null, *stx_start = NULL;
      int is_val;

      while (1) {
	is_val = SAME_OBJ(gval, scheme_define_values_syntax);
	
	v = SCHEME_STX_CDR(first);
	
	if (!SCHEME_STX_PAIRP(v))
	  scheme_wrong_syntax(NULL, NULL, first, 
			      "bad syntax (" IMPROPER_LIST_FORM ")");

	var = NULL;
	vars = SCHEME_STX_CAR(v);
	while (SCHEME_STX_PAIRP(vars)) {
	  var = SCHEME_STX_CAR(vars);
	  if (!SCHEME_STX_SYMBOLP(var))
	    scheme_wrong_syntax(NULL, var, first, 
				"name must be an identifier");
	  vars = SCHEME_STX_CDR(vars);
	}

	link = scheme_make_immutable_pair(v, scheme_null);
	if (is_val) {
	  if (!start)
	    start = link;
	  else
	    SCHEME_CDR(l) = link;
	  l = link;
	} else {
	  if (!stx_start)
	    stx_start = link;
	  else
	    SCHEME_CDR(stx_l) = link;
	  stx_l = link;
	}

	result = SCHEME_STX_CDR(result);
	if (!SCHEME_STX_NULLP(result) && !SCHEME_STX_PAIRP(result))
	  scheme_wrong_syntax(NULL, NULL, first, NULL);

      define_try_again:
	if (!SCHEME_STX_NULLP(result)) {
	  first = SCHEME_STX_CAR(result);
	  first = scheme_datum_to_syntax(first, forms, forms, 0, 0);
	  first = scheme_check_immediate_macro(first, env, rec, drec, depth, scheme_false, 1, &gval);
	  more = 1;
	  if (NOT_SAME_OBJ(gval, scheme_define_values_syntax)
	      && NOT_SAME_OBJ(gval, scheme_define_syntaxes_syntax)) {
	    if (SAME_OBJ(gval, scheme_begin_syntax)) {
	      /* Inline content */
	      Scheme_Object *content;

	      content = SCHEME_STX_CDR(first);
		
	      if (scheme_stx_proper_list_length(content) < 0)
		scheme_wrong_syntax(NULL, NULL, first, 
				    "bad syntax (" IMPROPER_LIST_FORM ")");
		
	      result = scheme_append(scheme_flatten_syntax_list(content, NULL), 
				     SCHEME_CDR(result));
	      goto define_try_again;
	    } else
	      break;
	  }
	} else
	  break;
      }

      if (SCHEME_STX_PAIRP(result)) {
	if (stx_start) {
	  if (!start)
	    start = scheme_null;
	  result = scheme_make_immutable_pair(letrec_syntaxes_symbol,
					      scheme_make_immutable_pair(stx_start,
									 scheme_make_immutable_pair(start, result)));
	} else {
	  result = scheme_make_immutable_pair(letrec_values_symbol, scheme_make_immutable_pair(start, result));
	}
	result = scheme_datum_to_syntax(result, forms, scheme_sys_wraps(env), 0, 1);

	more = 0;
      } else {
	/* Empty body: illegal. */
	scheme_wrong_syntax(scheme_begin_stx_string, NULL, forms, 
			    "no expression after a sequence of internal definitions");
      }
    }

    if (!more) {
      if (rec)
	result = scheme_compile_expr(result, env, rec, drec);
      else {
	if (depth > 0)
	  --depth;
	if (depth)
	  result = scheme_expand_expr(result, env, depth, boundname);
      }
      
      return scheme_make_immutable_pair(result, scheme_null);
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

    rest = scheme_datum_to_syntax(rest, forms, forms, 0, 0);

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
    rest = scheme_datum_to_syntax(rest, forms, forms, 0, 0);
#if EMBEDDED_DEFINES_START_ANYWHERE
    forms = scheme_compile_expand_block(scheme_datum_to_syntax(rest, forms, forms, 0, 0), 
					env, rec, drec, depth, boundname);
#else
    if (scheme_stx_proper_list_length(rest) < 0)
      scheme_wrong_syntax(scheme_begin_stx_string, NULL, rest, "bad syntax");
    forms = scheme_expand_list(rest, env, depth, boundname);
#endif
  }

  return scheme_make_immutable_pair(first, forms);
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

  if (scheme_stx_proper_list_length(form) < 0) {
    /* This is already checked for anything but application */
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, 
			"bad syntax (" IMPROPER_LIST_FORM ")");
  }

  fm = form;
  while (SCHEME_STX_PAIRP(fm)) {
    Scheme_Object *r, *p;

    r = SCHEME_STX_CAR(fm);
    p = SCHEME_STX_CDR(fm);
    r = scheme_expand_expr(r, env, depth, 
			   (SCHEME_STX_NULLP(p)
			    ? boundname
			    : scheme_false));
    p = scheme_make_immutable_pair(r, scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;

    fm = SCHEME_STX_CDR(fm);
  }

  return scheme_datum_to_syntax(first, form, form, 0, 0);
}

/*========================================================================*/
/*                          continuation marks                            */
/*========================================================================*/

void scheme_push_continuation_frame(Scheme_Cont_Frame_Data *d)
{
  d->cont_mark_pos = MZ_CONT_MARK_POS;
  d->cont_mark_stack = MZ_CONT_MARK_STACK;

  MZ_CONT_MARK_POS += 2;
}

void scheme_pop_continuation_frame(Scheme_Cont_Frame_Data *d)
{
  MZ_CONT_MARK_POS = d->cont_mark_pos;
  MZ_CONT_MARK_STACK = d->cont_mark_stack;
}


void scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Thread *p = scheme_current_thread;
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
  MZ_CONT_MARK_POS -= 2;
}

void scheme_temp_inc_mark_depth()
{
  MZ_CONT_MARK_POS += 2;
}

/*========================================================================*/
/*                         eval-apply helpers                             */
/*========================================================================*/

/* called in schapp.h */

static Scheme_Object *do_apply_known_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
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
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_do_eval(obj, 
			p->ku.k.i1, 
			argv,
			p->ku.k.i2);
}

static void unbound_global(Scheme_Object *obj)
{
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Thread *p = scheme_current_thread;
#endif
  Scheme_Object *tmp;

  tmp = MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(obj)];
  tmp = ((Scheme_Object **)tmp)[SCHEME_TOPLEVEL_POS(obj)];
  scheme_unbound_global((Scheme_Bucket *)tmp);
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

Scheme_Object *
scheme_do_eval(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
		int get_value)
{
  Scheme_Type type;
  Scheme_Object *v;
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **old_runstack;
  GC_MAYBE_IGNORE_INTERIOR MZ_MARK_STACK_TYPE old_cont_mark_stack;
#if USE_LOCAL_RUNSTACK
  GC_MAYBE_IGNORE_INTERIOR Scheme_Object **runstack;
#endif
#ifdef REGISTER_POOR_MACHINE
# define p scheme_current_thread
#else
  Scheme_Thread *p = scheme_current_thread;
#endif

#ifdef DO_STACK_CHECK
# define SCHEME_CURRENT_PROCESS p
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
      if (rands == p->tail_buffer)
	MAKE_TAIL_BUFFER_SAFE();
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
	scheme_wrong_count_m(prim->name, prim->mina, prim->maxa,
			     num_rands, rands,
			     prim->flags & SCHEME_PRIM_IS_METHOD);
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
	    scheme_wrong_count_m(scheme_get_proc_name(obj, NULL, 1), 
				 num_params - 1, -1,
				 num_rands, rands,
				 data->flags & CLOS_IS_METHOD);
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
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    scheme_wrong_count_m(scheme_get_proc_name(obj, NULL, 1), 
				 num_params, num_params,
				 num_rands, rands,
				 data->flags & CLOS_IS_METHOD);
	    return NULL; /* Doesn't get here */
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
	scheme_wrong_count_m(prim->name, prim->mina, prim->maxa, 
			     num_rands, rands,
			     prim->flags & SCHEME_PRIM_IS_METHOD);
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
      case scheme_toplevel_type:
	{
#define global_lookup(prefix, _obj, tmp)                                \
          tmp = RUNSTACK[SCHEME_TOPLEVEL_DEPTH(_obj)];                  \
          tmp = ((Scheme_Object **)tmp)[SCHEME_TOPLEVEL_POS(_obj)];     \
	  tmp = (Scheme_Object *)(SCHEME_VAR_BUCKET(tmp))->val;         \
	  if (!tmp) {                                                   \
            UPDATE_THREAD_RSPTR_FOR_ERROR();                            \
            unbound_global(_obj);                                       \
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

#ifdef REGISTER_POOR_MACHINE
# undef p
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
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v, **save_runstack;
  Scheme_Env *env;
  int isexpr, multi;

  v = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  multi = p->ku.k.i1;
  isexpr = p->ku.k.i2;

  if (isexpr) {
    if (multi)
      v = _scheme_eval_linked_expr_multi_wp(v, p);
    else
      v = _scheme_eval_linked_expr_wp(v, p);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)v;
    int depth;

    depth = top->max_let_depth + scheme_prefix_depth(top->prefix);
    if (!scheme_check_runstack(depth)) {
      p->ku.k.p1 = top;
      p->ku.k.p2 = env;
      p->ku.k.i1 = multi;
      p->ku.k.i2 = 0;
      return (Scheme_Object *)scheme_enlarge_runstack(depth, eval_k);
    }

    v = top->code;

    save_runstack = scheme_push_prefix(env, top->prefix, NULL, NULL, 0, env->phase);

    if (multi)
      v = _scheme_eval_linked_expr_multi_wp(v, p);
    else
      v = _scheme_eval_linked_expr_wp(v, p);

    scheme_pop_prefix(save_runstack);
  } else {
    v = scheme_void;
  }

  return (void *)v;
}

static Scheme_Object *_eval(Scheme_Object *obj, Scheme_Env *env, 
			    int isexpr, int multi, int top)
{
  Scheme_Thread *p = scheme_current_thread;
  
  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = multi;
  p->ku.k.i2 = isexpr;

  if (top)
    return (Scheme_Object *)scheme_top_level_do(eval_k, 1);
  else
    return (Scheme_Object *)eval_k();
}

Scheme_Object *scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 1);
}

Scheme_Object *scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 1, 1);
}

Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 0, 0);
}

Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return _eval(obj, env, 0, 1, 0);
}

Scheme_Object *scheme_eval_linked_expr(Scheme_Object *obj)
{
  return _eval(obj, NULL, 1, 0, 1);
}

Scheme_Object *scheme_eval_linked_expr_multi(Scheme_Object *obj)
{
  return _eval(obj, NULL, 1, 1, 1);
}

/* for mzc: */
Scheme_Object *scheme_load_compiled_stx_string(const char *str, long len)
{
  Scheme_Object *port, *expr;

  port = scheme_make_sized_string_input_port(str, -len);

  expr = scheme_internal_read(port, NULL, 1, scheme_config);

  expr = _scheme_eval_compiled(expr, scheme_get_env(scheme_config));

  /* Unwrap syntax once; */
  expr = SCHEME_STX_VAL(expr);

  return expr;
}

/* for mzc: */
Scheme_Object *scheme_compiled_stx_symbol(Scheme_Object *stx)
{
  return SCHEME_STX_VAL(stx);
}

/* for mzc: */
Scheme_Object *scheme_eval_compiled_stx_string(Scheme_Object *expr, Scheme_Env *env,
					       long shift, Scheme_Object *modidx)
{
  /* If modidx, then last element is a module index; shift the rest. */
  if (modidx) {
    int i, len = SCHEME_VEC_SIZE(expr);
    Scheme_Object *orig = SCHEME_VEC_ELS(expr)[len - 1], *s;
    orig = SCHEME_STX_VAL(orig);
    for (i = 0; i < len - 1; i++) {
      s = scheme_stx_phase_shift(SCHEME_VEC_ELS(expr)[i], shift, orig, modidx);
      SCHEME_VEC_ELS(expr)[i] = s;
    }
  }

  return expr;
}

static void *expand_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj;
  Scheme_Comp_Env *env;
  int depth, rename;

  obj = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;
  depth = p->ku.k.i1;
  rename = p->ku.k.i2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  if (!SCHEME_STXP(obj))
    obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 1, 0);

  if (rename) {
    /* Renamings for requires: */
    if (env->genv->rename)
      obj = scheme_add_rename(obj, env->genv->rename);
    if (env->genv->exp_env && env->genv->exp_env->rename)
      obj = scheme_add_rename(obj, env->genv->exp_env->rename);
  }

  obj = scheme_expand_expr(obj, env, depth, scheme_false);

  if (rename) {
    obj = scheme_add_mark_barrier(obj);
    /* scheme_simplify_stx(obj, scheme_new_stx_simplify_cache()); */ /* too expensive */
  }

  return obj;
}

static Scheme_Object *_expand(Scheme_Object *obj, Scheme_Comp_Env *env, 
			      int depth, int rename, int eb)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = depth;
  p->ku.k.i2 = rename;

  return (Scheme_Object *)scheme_top_level_do(expand_k, eb);
}

Scheme_Object *scheme_expand(Scheme_Object *obj, Scheme_Env *env)
{
  return _expand(obj, scheme_new_expand_env(env, SCHEME_TOPLEVEL_FRAME), -1, 1, -1);
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

  return _eval(v, env, 0, 1, 0);
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

  return _expand(argv[0], scheme_new_expand_env(env, SCHEME_TOPLEVEL_FRAME), -1, 1, 0);
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
  int cnt, pos, kind;

  env = scheme_current_thread->current_local_env;

  if (!env)
    scheme_raise_exn(MZEXN_MISC, "local-expand: not currently transforming");

  if (SAME_OBJ(argv[1], internal_define_symbol))
    kind = SCHEME_INTDEF_FRAME;
  else if (SAME_OBJ(argv[1], module_symbol))
    kind = SCHEME_MODULE_BEGIN_FRAME;
  else if (SAME_OBJ(argv[1], top_level_symbol))
    kind = SCHEME_TOPLEVEL_FRAME;
  else if (SAME_OBJ(argv[1], expression_symbol))
    kind = 0;
  else {
    scheme_wrong_type("local-expand", "'expression, 'top-level, 'internal-define, or 'module",
		      1, argc, argv);
    return NULL;
  }

  /* For each given stop-point identifier, shadow any potential syntax
     in the environment with an identity-expanding syntax expander. */

  (void)scheme_get_stop_expander();

  env = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME | kind, env);
  local_mark = scheme_current_thread->current_local_mark;
  
  cnt = scheme_stx_proper_list_length(argv[2]);
  if (cnt > 0)
    scheme_add_local_syntax(cnt, env);
  pos = 0;

  for (l = argv[2]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    Scheme_Object *i;
    
    i = SCHEME_CAR(l);
    if (!SCHEME_STX_SYMBOLP(i)) {
      scheme_wrong_type("local-expand", "list of identifier syntax", 1, argc, argv);
      return NULL;
    }
    
    if (cnt > 0)
      scheme_set_local_syntax(pos++, i, stop_expander, env);
  }
  if (!SCHEME_NULLP(l)) {
    scheme_wrong_type("local-expand", "list of identifier syntax", 1, argc, argv);
    return NULL;
  }

  l = argv[0];

  if (!SCHEME_STXP(l))
    l = scheme_datum_to_syntax(l, scheme_false, scheme_false, 1, 0);

  /* Since we have an expression from local context,
     we need to remove the temporary mark... */
  l = scheme_add_remove_mark(l, local_mark);

  /* Expand the expression. depth = -2 means expand all the way, but
     preserve letrec-syntax. */
  l = _expand(l, env, -2, 0, 0);

  /* Put the temporary mark back: */
  return scheme_add_remove_mark(l, local_mark);
}

static Scheme_Object *
expand_once(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return _expand(argv[0], scheme_new_expand_env(env, SCHEME_TOPLEVEL_FRAME), 1, 1, 0);
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
  Scheme_Thread *p = scheme_current_thread;

  v = scheme_param_config("break-enabled", 
			  scheme_make_integer(MZCONFIG_ENABLE_BREAK),
			  argc, argv,
			  -1, NULL, NULL, 1);

  if (argc == 1) { /* might have turned on breaking... */
    if (p->external_break && scheme_can_break(p, p->config)) {
      scheme_thread_block_w_thread(0.0, p);
      p->ran_some = 1;
    }
  }

  return v;
}

/*========================================================================*/
/*        creating/pushing prefix for top-levels and syntax objects       */
/*========================================================================*/

int scheme_prefix_depth(Resolve_Prefix *rp)
{
  int d = 0;

  if (rp->num_toplevels)
    d++;
  return rp->num_stxes + d;
}

Scheme_Object **scheme_push_prefix(Scheme_Env *genv, Resolve_Prefix *rp, 
				   Scheme_Object *src_modidx, Scheme_Object *now_modidx,
				   int src_phase, int now_phase)
{
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Thread *p = scheme_current_thread;
#endif
  Scheme_Object **rs_save, **rs, *v, **a;
  int i;

  rs_save = rs = MZ_RUNSTACK;

  rs -= rp->num_stxes;
  MZ_RUNSTACK = rs;
  for (i = 0; i < rp->num_stxes; i++) {
    v = scheme_stx_phase_shift(rp->stxes[i], now_phase - src_phase, src_modidx, now_modidx);
    rs[i] = v;
  }

  if (rp->num_toplevels) {
    a = MALLOC_N(Scheme_Object *, rp->num_toplevels);
    --rs;
    MZ_RUNSTACK = rs;
    rs[0] = (Scheme_Object *)a;
   
    for (i = 0; i < rp->num_toplevels; i++) {
      v = rp->toplevels[i];
      if (genv)
	v = scheme_link_toplevel(rp->toplevels[i], genv);
      a[i] = v;
    }
  }

  MZ_RUNSTACK = rs;

  return rs_save;
}

void scheme_pop_prefix(Scheme_Object **rs)
{
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Thread *p = scheme_current_thread;
#endif

  MZ_RUNSTACK = rs;
}

/*========================================================================*/
/*       [un]marshalling application, branch, sequence, wcm bytecode      */
/*========================================================================*/

#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_application(Scheme_Object *obj)
{
  scheme_signal_error("app writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_application(Scheme_Object *obj)
{
  scheme_signal_error("app reader shouldn't be used");
  return NULL;
}

static Scheme_Object *write_sequence(Scheme_Object *obj)
{
  Scheme_Object *l;
  int i;

  i = ((Scheme_Sequence *)obj)->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(scheme_protect_quote(((Scheme_Sequence *)obj)->array[i]), l);
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
  scheme_signal_error("branch writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_branch(Scheme_Object *obj)
{
  scheme_signal_error("branch reader shouldn't be used");
  return NULL;
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
