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

#include "schpriv.h"
#include "schminc.h"

#if defined(UNIX_LIMIT_STACK) || defined(UNIX_LIMIT_FDSET_SIZE)
# include <signal.h>
# include <sys/time.h>
# include <sys/resource.h>
#endif

#ifdef MZ_USE_IRIX_SPROCS
# include "../gc/gc.h"
#endif

#define GLOBAL_TABLE_SIZE 500
#define REFERENCES_TABLE_SIZE GLOBAL_TABLE_SIZE

/* #define TIME_STARTUP_PROCESS */

/* globals */
int scheme_allow_set_undefined;
int scheme_escape_continuations_only = 0; 

int scheme_starting_up;

Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

Scheme_Env *scheme_initial_env;

static Scheme_Object *kernel_symbol;

/* locals */
static Scheme_Env *make_env(Scheme_Env *base, int semi);
static void make_init_env(void);

static Scheme_Object *namespace_variable_binding(int, Scheme_Object *[]);
static Scheme_Object *local_exp_time_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_set_transformer(int argc, Scheme_Object *argv[]);

static Scheme_Object *write_variable(Scheme_Object *obj);
static Scheme_Object *read_variable(Scheme_Object *obj);
static Scheme_Object *write_module_variable(Scheme_Object *obj);
static Scheme_Object *read_module_variable(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static int set_reference_ids = 0;
static int builtin_ref_counter = 0;

#define ARBITRARY_USE 1
#define CONSTRAINED_USE 2
#define WAS_SET_BANGED 4

typedef struct Compile_Data {
  char **stat_dists; /* (pos, depth) => used? */
  int *sd_depths;
  int num_const;
  Scheme_Object **const_names;
  Scheme_Object **const_vals;
  int *use;
} Compile_Data;

typedef struct Scheme_Full_Comp_Env {
  Scheme_Comp_Env base;
  Compile_Data data;
} Scheme_Full_Comp_Env;
static void init_compile_data(Scheme_Comp_Env *env);

/* Precise GC WARNING: this macro produces unaligned pointers: */
#define COMPILE_DATA(e) (&((Scheme_Full_Comp_Env *)e)->data)

#ifdef MZ_REAL_THREADS
void *scheme_global_lock;
int scheme_global_lock_c;
#endif

static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  if ((o == scheme_orig_stdin_port)
      || (o == scheme_orig_stdout_port)
      || (o == scheme_orig_stderr_port))
    return;

  /* f is NULL for threads */
  if (f)
    f(o, data);
}

Scheme_Env *scheme_basic_env()
{
  Scheme_Env *env;

  if (scheme_main_thread) {
    /* Reset everything: */
    scheme_do_close_managed(NULL, skip_certain_things);
    scheme_main_thread = NULL;

    scheme_reset_finalizations();
    scheme_init_stack_check();
#ifndef MZ_PRECISE_GC
    scheme_init_setjumpup();
#endif

    scheme_make_thread();
    scheme_init_error_escape_proc(scheme_current_thread);

    /* FIXME: this isn't right */

    env = scheme_make_empty_env();
    scheme_require_from_original_env(env, 0);
    
    scheme_prepare_exp_env(env);
    scheme_require_from_original_env(env->exp_env, 0);

    scheme_set_param(scheme_config, MZCONFIG_ENV, (Scheme_Object *)env); 
    scheme_init_port_config();
    scheme_init_port_fun_config();
    scheme_init_error_config();
#ifndef NO_SCHEME_EXNS
    scheme_init_exn_config();
#endif

    return env;
  }

#ifdef UNIX_LIMIT_STACK
  {
    struct rlimit rl;
    
    getrlimit(RLIMIT_STACK, &rl);
    if (rl.rlim_cur > UNIX_LIMIT_STACK) {
      rl.rlim_cur = UNIX_LIMIT_STACK;
      setrlimit(RLIMIT_STACK, &rl);
    }
  }
#endif
#ifdef UNIX_LIMIT_FDSET_SIZE
  {
    struct rlimit rl;
    
    getrlimit(RLIMIT_NOFILE, &rl);
    if (rl.rlim_cur > FD_SETSIZE) {
      rl.rlim_cur = FD_SETSIZE;
      setrlimit(RLIMIT_NOFILE, &rl);
    }
  }
#endif

#ifdef MZ_USE_IRIX_SPROCS
  GC_INIT();
#endif

  scheme_starting_up = 1;

#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
#endif

#ifdef TIME_STARTUP_PROCESS
   printf("#if 0\nbasic @ %ld\n", scheme_get_process_milliseconds());
#endif

#ifdef MZ_REAL_THREADS
  REGISTER_SO(scheme_global_lock);
  scheme_global_lock = SCHEME_MAKE_MUTEX();
#else
  scheme_init_stack_check();
#endif

  {
    int i, k;

#ifndef USE_TAGGED_ALLOCATION
    Scheme_Local *all;

    all = (Scheme_Local *)scheme_malloc_eternal(sizeof(Scheme_Local) * 2 * MAX_CONST_LOCAL_POS);
# ifdef MEMORY_COUNTING_ON
    scheme_misc_count += sizeof(Scheme_Local) * 2 * MAX_CONST_LOCAL_POS;
# endif    
#endif

    for (i = 0; i < MAX_CONST_LOCAL_POS; i++) {
      for (k = 0; k < 2; k++) {
	Scheme_Object *v;
	
#ifndef USE_TAGGED_ALLOCATION
	v = (Scheme_Object *)(all++);
#else
	v = (Scheme_Object *)scheme_malloc_eternal_tagged(sizeof(Scheme_Local));
#endif
	v->type = k + scheme_local_type;
	SCHEME_LOCAL_POS(v) = i;
	
	scheme_local[i][k] = v;
      }
    }
  }

  scheme_init_true_false();

#ifdef MZ_PRECISE_GC
  scheme_register_traversers();
  register_traversers();
  scheme_init_hash_key_procs();
#endif

#ifdef TIME_STARTUP_PROCESS
  printf("pre-process @ %ld\n", scheme_get_process_milliseconds());
#endif

  scheme_make_thread();

#ifdef TIME_STARTUP_PROCESS
  printf("process @ %ld\n", scheme_get_process_milliseconds());
#endif

  make_init_env();

  env = scheme_make_empty_env();
  scheme_require_from_original_env(env, 1); /* Need kernel syntax... */

  scheme_set_param(scheme_current_thread->config, MZCONFIG_ENV, 
		   (Scheme_Object *)env); 

  scheme_add_embedded_builtins(env);

  scheme_save_initial_module_set(env);

  scheme_init_error_escape_proc(scheme_current_thread);

  scheme_starting_up = 0;

  scheme_init_getenv();

#ifdef TIME_STARTUP_PROCESS
  printf("done @ %ld\n#endif\n", scheme_get_process_milliseconds());
#endif

  return env;
}

static void make_init_env(void)
{
  Scheme_Env *env;
#ifdef TIME_STARTUP_PROCESS
  long startt;
#endif

  env = make_env(NULL, 0);

  scheme_set_param(scheme_current_thread->config, MZCONFIG_ENV, 
		   (Scheme_Object *)env);

  REGISTER_SO(scheme_initial_env);
  scheme_initial_env = env;

  scheme_defining_primitives = 1;
  set_reference_ids = 1;
  builtin_ref_counter = 0;

#ifdef TIME_STARTUP_PROCESS
   printf("init @ %ld\n", scheme_get_process_milliseconds());
# define MZTIMEIT(n, f) (MARK_START_TIME(), f, DONE_TIME(n))
# define MARK_START_TIME() startt = scheme_get_process_milliseconds()
# define DONE_TIME(n) (printf(#n ": %ld\n", (long)(scheme_get_process_milliseconds() - startt)))
#else
# define MZTIMEIT(n, f) f
# define MARK_START_TIME() /**/
# define DONE_TIME(n) /**/
#endif

  /* The ordering of the first few init calls is important.
	  Add to the end of the list, not the beginning. */
  MZTIMEIT(symbol-table, scheme_init_symbol_table());
  MZTIMEIT(type, scheme_init_type(env));
  MZTIMEIT(symbol-type, scheme_init_symbol_type(env));
  MZTIMEIT(fun, scheme_init_fun(env));
  MZTIMEIT(symbol, scheme_init_symbol(env));
  MZTIMEIT(list, scheme_init_list(env));
  MZTIMEIT(number, scheme_init_number(env));
  MZTIMEIT(numarith, scheme_init_numarith(env));
  MZTIMEIT(numcomp, scheme_init_numcomp(env));
  MZTIMEIT(numstr, scheme_init_numstr(env));
  MZTIMEIT(stx, scheme_init_stx(env));
  MZTIMEIT(module, scheme_init_module(env));
  MZTIMEIT(port, scheme_init_port(env));
  MZTIMEIT(portfun, scheme_init_port_fun(env));
#ifndef NO_TCP_SUPPORT
  MZTIMEIT(network, scheme_init_network(env));
#endif
  MZTIMEIT(string, scheme_init_string(env));
  MZTIMEIT(vector, scheme_init_vector(env));
  MZTIMEIT(char, scheme_init_char(env));
  MZTIMEIT(bool, scheme_init_bool(env));
  MZTIMEIT(syntax, scheme_init_syntax(env));
  MZTIMEIT(eval, scheme_init_eval(env));
  MZTIMEIT(error, scheme_init_error(env));
  MZTIMEIT(struct, scheme_init_struct(env));
#ifndef NO_SCHEME_EXNS
  MZTIMEIT(exn, scheme_init_exn(env));
#endif
  MZTIMEIT(process, scheme_init_thread(env));
#ifndef NO_SCHEME_THREADS
  MZTIMEIT(sema, scheme_init_sema(env));
#endif
  MZTIMEIT(read, scheme_init_read(env));
  MZTIMEIT(print, scheme_init_print(env));
  MZTIMEIT(file, scheme_init_file(env));
  MZTIMEIT(dynamic-extension, scheme_init_dynamic_extension(env));
  MZTIMEIT(image, scheme_init_image(env));
#ifndef NO_REGEXP_UTILS
  MZTIMEIT(regexp, scheme_regexp_initialize(env));
#endif

  MARK_START_TIME();

  scheme_add_global_constant("namespace-variable-binding",
			     scheme_make_prim_w_arity(namespace_variable_binding,
						      "namespace-variable-binding",
						      1, 2),
			     env);

  scheme_add_global_constant("syntax-local-value", 
			     scheme_make_prim_w_arity(local_exp_time_value,
						      "syntax-local-value",
						      1, 2),
			     env);
  scheme_add_global_constant("syntax-local-name", 
			     scheme_make_prim_w_arity(local_exp_time_name,
						      "syntax-local-name",
						      0, 0),
			     env);
  scheme_add_global_constant("syntax-local-context", 
			     scheme_make_prim_w_arity(local_context,
						      "syntax-local-context",
						      0, 0),
			     env);

  scheme_add_global_constant("make-set!-transformer", 
			     scheme_make_prim_w_arity(make_set_transformer,
						      "make-set!-transformer",
						      1, 1),
			     env);

  DONE_TIME(env);

  scheme_install_type_writer(scheme_variable_type, write_variable);
  scheme_install_type_reader(scheme_variable_type, read_variable);
  scheme_install_type_writer(scheme_module_variable_type, write_module_variable);
  scheme_install_type_reader(scheme_module_variable_type, read_module_variable);
  scheme_install_type_writer(scheme_local_type, write_local);
  scheme_install_type_reader(scheme_local_type, read_local);
  scheme_install_type_writer(scheme_local_unbox_type, write_local);
  scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  MARK_START_TIME();

  set_reference_ids = 0;

  scheme_finish_kernel(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != EXPECTED_PRIM_COUNT) {
    printf("Primitive count %d doesn't match expected count %d\n"
	   "Turn off USE_COMPILED_STARTUP in src/schminc.h\n",
	   builtin_ref_counter, EXPECTED_PRIM_COUNT);
    exit(1);
  }
#endif
   
  scheme_defining_primitives = 0;
}

Scheme_Env *scheme_make_empty_env(void)
{
  return make_env(NULL, 0);
}

static Scheme_Env *make_env(Scheme_Env *base, int semi)
{
  Scheme_Hash_Table *toplevel, *syntax;
  Scheme_Hash_Table *module_registry;
  Scheme_Object *modchain;
  Scheme_Env *env;

  toplevel = scheme_hash_table(7, SCHEME_hash_ptr);
  toplevel->with_home = 1;

  if (semi > 0) {
    syntax = NULL;
    modchain = NULL;
    module_registry = NULL;
  } else {
    syntax = scheme_hash_table(7, SCHEME_hash_ptr);
    if (base) {
      modchain = base->modchain;
      module_registry = base->module_registry;
    } else {
      if (semi < 0) {
	module_registry = NULL;
	modchain = NULL;
      } else {
	Scheme_Hash_Table *modules;

	modules = scheme_hash_table(7, SCHEME_hash_ptr);
	modchain = scheme_make_vector(3, scheme_false);
	SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)modules;

	module_registry = scheme_hash_table(7, SCHEME_hash_ptr);
      }
    }
  }

  env = MALLOC_ONE_TAGGED(Scheme_Env);
  env->type = scheme_namespace_type;

  env->toplevel = toplevel;

  if (semi < 1) {
    env->syntax = syntax;
    env->modchain = modchain;
    env->module_registry = module_registry;

    {
      Scheme_Comp_Env *me;
      me = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
      env->init = me;
    }
#ifdef MZTAG_REQUIRED
    env->init->type = scheme_rt_comp_env;
#endif
    env->init->num_bindings = 0;
    env->init->next = NULL;
    env->init->genv = env;
    init_compile_data(env->init);
  }

  return env;
}

Scheme_Env *
scheme_new_module_env(Scheme_Env *env, Scheme_Module *m, int new_exp_module_tree)
{
  Scheme_Env *menv;

  menv = make_env(env, 0);

  menv->module = m;
  if (menv->init)
    menv->init->flags |= SCHEME_MODULE_FRAME;

  if (new_exp_module_tree) {
    Scheme_Object *p;
    Scheme_Hash_Table *modules;

    modules = scheme_hash_table(7, SCHEME_hash_ptr);
    p = scheme_make_vector(3, scheme_false);
    SCHEME_VEC_ELS(p)[0] = (Scheme_Object *)modules;
    menv->modchain = p;
  }

  return menv;
}

void scheme_prepare_exp_env(Scheme_Env *env)
{
  if (!env->exp_env) {
    Scheme_Env *eenv;
    Scheme_Object *modchain;

    eenv = make_env(NULL, -1);
    eenv->phase = env->phase + 1;

    eenv->module = env->module;
    eenv->module_registry = env->module_registry;

    modchain = SCHEME_VEC_ELS(env->modchain)[1];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *next_modules;

      next_modules = scheme_hash_table(7, SCHEME_hash_ptr);
      modchain = scheme_make_vector(3, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)next_modules;
      SCHEME_VEC_ELS(env->modchain)[1] = modchain;
      SCHEME_VEC_ELS(modchain)[2] = env->modchain;
    }
    eenv->modchain = modchain;

    env->exp_env = eenv;
  }
}

Scheme_Env *scheme_clone_module_env(Scheme_Env *menv, Scheme_Env *ns, Scheme_Object *modchain)
{
  /* New env should have the same syntax and globals table, but it lives in
     a different namespaces. */
  Scheme_Env *menv2;

  menv2 = MALLOC_ONE_TAGGED(Scheme_Env);
  menv2->type = scheme_namespace_type;

  menv2->module = menv->module;

  menv2->module_registry = ns->module_registry;

  menv2->rename = menv->rename;
  menv2->et_rename = menv->et_rename;

  menv2->syntax = menv->syntax;

  menv2->phase = menv->phase;
  menv2->link_midx = menv->link_midx;
  menv2->running = menv->running;

  menv2->toplevel = menv->toplevel;
  
  menv2->modchain = modchain;

  {
    Scheme_Comp_Env *me;
    me = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
    menv2->init = me;
  }
#ifdef MZTAG_REQUIRED
  menv2->init->type = scheme_rt_comp_env;
#endif
  menv2->init->num_bindings = 0;
  menv2->init->next = NULL;
  menv2->init->genv = menv2;
  init_compile_data(menv2->init);

  if (!SCHEME_NULLP(menv2->module->et_requires)) {
    /* We'll need the next link in the modchain: */
    modchain = SCHEME_VEC_ELS(modchain)[1];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *next_modules;
      
      next_modules = scheme_hash_table(7, SCHEME_hash_ptr);
      modchain = scheme_make_vector(3, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)next_modules;
      SCHEME_VEC_ELS(menv2->modchain)[1] = modchain;
      SCHEME_VEC_ELS(modchain)[2] = menv2->modchain;
    }
  }

  return menv2;
}

Scheme_Hash_Table *scheme_clone_toplevel(Scheme_Hash_Table *ht, Scheme_Env *home)
{
  Scheme_Hash_Table *r;
  Scheme_Bucket **bs;
  int i;

  r = scheme_hash_table(7, SCHEME_hash_ptr);
  if (home)
    r->with_home = 1;

  bs = ht->buckets;

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val) {
      Scheme_Object *name = (Scheme_Object *)b->key;
      Scheme_Object *val = (Scheme_Object *)b->val;

      b = scheme_bucket_from_table(r, (const char *)name);
      b->val = val;
      if (home)
	((Scheme_Bucket_With_Home *)b)->home = home;
    }
  }

  return r;
}


void
scheme_do_add_global_symbol(Scheme_Env *env, Scheme_Object *sym, 
			    Scheme_Object *obj, 
			    int valvar, int constant)
{
  if (valvar) {
    Scheme_Bucket *b;
    b = scheme_bucket_from_table(env->toplevel, (const char *)sym);
    b->val = obj;
    ((Scheme_Bucket_With_Home *)b)->home = env;
    if (constant && scheme_defining_primitives) {
      ((Scheme_Bucket_With_Flags *)b)->id = builtin_ref_counter++;
      ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_HAS_REF_ID;
    }
  } else
    scheme_add_to_table(env->syntax, (char *)sym, obj, constant);
}

void
scheme_add_global(const char *name, Scheme_Object *obj, Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 1, 0);
}

void
scheme_add_global_symbol(Scheme_Object *sym, Scheme_Object *obj, Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, sym, obj, 1, 0);
}

void
scheme_add_global_constant(const char *name, Scheme_Object *obj, 
			   Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 1, 1);
}

void
scheme_add_global_constant_symbol(Scheme_Object *name, Scheme_Object *obj, 
				  Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, name, obj, 1, 1);
}

void
scheme_add_global_keyword(const char *name, Scheme_Object *obj, 
			  Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 0, 0);
}

void
scheme_remove_global_symbol(Scheme_Object *sym, Scheme_Env *env)
{
  Scheme_Bucket *b;
  Scheme_Hash_Table *toplevel;

  toplevel = env->toplevel;

  b = scheme_bucket_from_table(toplevel, (char *)sym);

  if (!b)
    return;

  b->val = NULL;
}

void
scheme_remove_global(const char *name, Scheme_Env *env)
{
  Scheme_Object *sym;

  sym = scheme_intern_symbol(name);

  scheme_remove_global_symbol(sym, env);
}

Scheme_Object **scheme_make_builtin_references_table(void)
{
  Scheme_Hash_Table *ht;
  Scheme_Object **t;
  Scheme_Bucket **bs;
  long i;

  t = MALLOC_N(Scheme_Object *, (builtin_ref_counter + 1));
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += sizeof(Scheme_Object *) * (builtin_ref_counter + 1);
#endif

  ht = scheme_initial_env->toplevel;

  bs = ht->buckets;

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_HAS_REF_ID))
      t[((Scheme_Bucket_With_Ref_Id *)b)->id] = (Scheme_Object *)b->val;
  }

  return t;
}

Scheme_Hash_Table *scheme_map_constants_to_globals(void)
{
  Scheme_Hash_Table *ht, *result;
  Scheme_Bucket **bs;
  long i;

  ht = scheme_initial_env->toplevel;
  bs = ht->buckets;

  result = scheme_hash_table(10, SCHEME_hash_ptr);

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST))
      scheme_add_to_table(result, (const char *)b->val, b, 0);
  }

  return result;
}

void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where, Scheme_Comp_Env *env,
			     Scheme_Object *form)
{
  if (!where)
    where = "";

  if (!SCHEME_STX_SYMBOLP(id))
    scheme_wrong_syntax(formname, form ? id : NULL, 
			form ? form : id, 
			"not an identifier%s", where);
}

static void init_compile_data(Scheme_Comp_Env *env)
{
  Compile_Data *data;
  int i, c, *use;

  c = env->num_bindings;
  use = MALLOC_N_ATOMIC(int, c);

  data = COMPILE_DATA(env);

  data->stat_dists = NULL;
  data->sd_depths = NULL;
  data->use = use;
  for (i = 0; i < c; i++) {
    use[i] = 0;
  }
}

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags,
					 Scheme_Comp_Env *base)
{
  Scheme_Comp_Env *frame;
  int count;
  
  count = num_bindings;

  frame = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
#ifdef MZTAG_REQUIRED
  frame->type = scheme_rt_comp_env;
#endif

  {
    Scheme_Object **vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->values = vals;
  }

  frame->num_bindings = num_bindings;
  frame->flags = flags | (base->flags & SCHEME_NO_RENAME);
  frame->next = base;
  frame->genv = base->genv;

  init_compile_data(frame);

  return frame;
}

int scheme_used_app_only(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  if (data->use[which] & ARBITRARY_USE)
    return 0;
  else
    return 1;
}

int scheme_used_ever(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!data->use[which];
}

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!(data->use[which] & WAS_SET_BANGED);
}

void
scheme_add_compilation_binding(int index, Scheme_Object *val, Scheme_Comp_Env *frame)
{
  if ((index >= frame->num_bindings) || (index < 0))
    scheme_signal_error("internal error: scheme_add_binding: "
			"index out of range: %d", index);
  
  frame->values[index] = val;
}

void scheme_add_local_syntax(int cnt, Scheme_Comp_Env *env)
{
  Scheme_Object **ns, **vs;
  
  if (cnt) {
    ns = MALLOC_N(Scheme_Object *, cnt);
    vs = MALLOC_N(Scheme_Object *, cnt);

    COMPILE_DATA(env)->num_const = cnt;
    COMPILE_DATA(env)->const_names = ns;
    COMPILE_DATA(env)->const_vals = vs;
  }
}

void scheme_set_local_syntax(int pos,
			     Scheme_Object *name, Scheme_Object *val,
			     Scheme_Comp_Env *env)
{
  COMPILE_DATA(env)->const_names[pos] = name;
  COMPILE_DATA(env)->const_vals[pos] = val;
}

Scheme_Comp_Env *
scheme_add_compilation_frame(Scheme_Object *vals, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int len, i, count;
  
  len = scheme_stx_list_length(vals);
  count = len;

  frame = scheme_new_compilation_frame(count, flags, env);

  for (i = 0; i < len ; i++) {
    if (SCHEME_STX_SYMBOLP(vals))
      frame->values[i] = vals;
    else {
      Scheme_Object *a;
      a = SCHEME_STX_CAR(vals);
      frame->values[i] = a;
      vals = SCHEME_STX_CDR(vals);
    }
  }
  
  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return scheme_new_compilation_frame(0, 0, env);
  else
    return env;
}

Scheme_Comp_Env *scheme_require_renames(Scheme_Comp_Env *env)
{
  if (env->flags & SCHEME_NO_RENAME) {
    env = scheme_new_compilation_frame(0, 0, env);
    env->flags -= SCHEME_NO_RENAME;
  }

  return env;
}

int scheme_is_toplevel(Scheme_Comp_Env *env)
{
  return !env->next || (env->flags & SCHEME_TOPLEVEL_FRAME);
}

int scheme_is_module_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_FRAME);
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, env);
}


static int env_uid_counter;

static Scheme_Object *env_frame_uid(Scheme_Comp_Env *env)
{
  if (env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME))
    return NULL;

  if (!env->uid) {
    char name[20];
    Scheme_Object *sym;
    env_uid_counter++;
    sprintf(name, "env%d", env_uid_counter);
    sym = scheme_make_symbol(name); /* uninterned! */
    env->uid = sym;
  }
  return env->uid;
}

Scheme_Object *scheme_add_env_renames(Scheme_Object *stx, Scheme_Comp_Env *env, 
				      Scheme_Comp_Env *upto)
{
  if (!SCHEME_STXP(stx)) {
    scheme_signal_error("internal error: not syntax");
    return NULL;
  }

  while (env != upto) {
    if (!(env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME))) {
      Scheme_Object *uid;
      int i, count;
      
      uid = env_frame_uid(env);
      
      count = 0;
      count += COMPILE_DATA(env)->num_const;
      for (i = env->num_bindings; i--; ) {
	if (env->values[i])
	  count++;
      }
      
      if (count) {
	if (!env->renames || (env->rename_var_count != count)) {
	  Scheme_Object *rnm;
	  
	  rnm = scheme_make_rename(uid, count);
	  
	  count = 0;
	  for (i = COMPILE_DATA(env)->num_const; i--; ) {
	    scheme_set_rename(rnm, count++, COMPILE_DATA(env)->const_names[i]);
	  }
	  for (i = env->num_bindings; i--; ) {
	    if (env->values[i])
	      scheme_set_rename(rnm, count++, env->values[i]);
	  }
	  
	  env->renames = rnm;
	  env->rename_var_count = count;
	}
	
	stx = scheme_add_rename(stx, env->renames);
      }
    }

    env = env->next;
  }

  return stx;
}

static Scheme_Object *alloc_local(short type, int pos)
{
  Scheme_Object *v;

  v = (Scheme_Object *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  v->type = type;
  SCHEME_LOCAL_POS(v) = pos;

  return (Scheme_Object *)v;
}

Scheme_Object *scheme_make_local(Scheme_Type type, int pos)
{
  int k;

  k = type - scheme_local_type;

  if (pos < MAX_CONST_LOCAL_POS)
    return scheme_local[pos][k];

  return alloc_local(type, pos);
}

static Scheme_Local *get_frame_loc(Scheme_Comp_Env *frame,
				   int i, int j, int p, int flags)
{
  COMPILE_DATA(frame)->use[i] |= (((flags & (SCHEME_APP_POS | SCHEME_SETTING))
				   ? CONSTRAINED_USE
				   : ARBITRARY_USE)
				  | ((flags & (SCHEME_SETTING | SCHEME_LINKING_REF))
				     ? WAS_SET_BANGED
				     : 0));
  
  if (!COMPILE_DATA(frame)->stat_dists) {
    int k, *ia;
    char **ca;
    ca = MALLOC_N(char*, frame->num_bindings);
    COMPILE_DATA(frame)->stat_dists = ca;
    ia = MALLOC_N_ATOMIC(int, frame->num_bindings);
    COMPILE_DATA(frame)->sd_depths = ia;
    for (k = frame->num_bindings; k--; ) {
      COMPILE_DATA(frame)->sd_depths[k] = 0;
    }
  }
  
  if (COMPILE_DATA(frame)->sd_depths[i] <= j) {
    char *naya, *a;
    int k;
    
    naya = MALLOC_N_ATOMIC(char, (j + 1));
    for (k = j + 1; k--; ) {
      naya[k] = 0;
    }
    a = COMPILE_DATA(frame)->stat_dists[i];
    for (k = COMPILE_DATA(frame)->sd_depths[i]; k--; ) {
      naya[k] = a[k];
    }
    
    COMPILE_DATA(frame)->stat_dists[i] = naya;
    COMPILE_DATA(frame)->sd_depths[i] = j + 1;
  }

  COMPILE_DATA(frame)->stat_dists[i][j] = 1;

  return (Scheme_Local *)scheme_make_local(scheme_local_type, p + i);
}

Scheme_Object *
scheme_static_distance(Scheme_Object *symbol, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int j = 0, p = 0;
  Scheme_Bucket *b;
  Scheme_Object *val, *modidx, *modname, *srcsym;
  Scheme_Env *genv;
  long phase;

  phase = env->genv->phase;
  
  frame = env;
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;
    Scheme_Object *uid;
    
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    uid = env_frame_uid(frame);
    
    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	if (SAME_OBJ(SCHEME_STX_VAL(symbol), SCHEME_STX_VAL(frame->values[i]))
	    && scheme_stx_env_bound_eq(symbol, frame->values[i], uid, phase)) {
	  if (flags & SCHEME_DONT_MARK_USE)
	    return scheme_make_local(scheme_local_type, 0);
	  else
	    return (Scheme_Object *)get_frame_loc(frame, i, j, p, flags);
	}
      }
    }

    for (i = COMPILE_DATA(frame)->num_const; i--; ) {
      int issame;
      if (frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)
	issame = scheme_stx_module_eq(symbol, COMPILE_DATA(frame)->const_names[i], phase);
      else
	issame = (SAME_OBJ(SCHEME_STX_VAL(symbol), 
			   SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))
		  && scheme_stx_env_bound_eq(symbol, COMPILE_DATA(frame)->const_names[i], uid, phase));
      
      if (issame) {
	val = COMPILE_DATA(frame)->const_vals[i];
	
	if (!val) {
	  scheme_wrong_syntax("compile", NULL, symbol,
			      "variable used out of context");
	  return NULL;
	}
	if (!(flags & SCHEME_ENV_CONSTANTS_OK)) {
	  if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type))
	    return val;
	  else
	    scheme_wrong_syntax("set!", NULL, symbol,
				"local syntax identifier cannot be mutated");
	  return NULL;
	}

	return val;
      }
    }

    p += frame->num_bindings;
  }

  srcsym = symbol;
  modidx = scheme_stx_module_name(&symbol, phase);
  
  /* Used out of context? */
  if (SAME_OBJ(modidx, scheme_undefined)) {
    if (!(flags & SCHEME_OUT_OF_CONTEXT_OK))
      scheme_wrong_syntax("compile", NULL, symbol,
			  "identifier used out of context");
    return NULL;
  }

  if (modidx) {
    /* If it's an access path, resolve it: */
    modname = scheme_module_resolve(modidx);

    if (env->genv->module && SAME_OBJ(modname, env->genv->module->modname)) {
      modidx = NULL;
      modname = NULL;
      genv = env->genv;
    } else {
      genv = scheme_module_access(modname, env->genv);

      if (!genv) {
	if (env->genv->phase) {
	  /* The failure might be due a laziness in required-syntax
	     execution. Force all laziness at the prior level 
	     and try again. */
	  scheme_module_force_lazy(env->genv, 1);
	  genv = scheme_module_access(modname, env->genv);
	}

	if (!genv) {
	  scheme_wrong_syntax("require", NULL, srcsym, 
			      "broken compiled code (stat-dist, phase %d): cannot find module %S",
			      env->genv->phase, modname /*,  scheme_syntax_to_datum(srcsym, 1, NULL) */);
	  return NULL;
	}
      }
    }
  } else {
    genv = env->genv;
    modname = NULL;
  }

  /* Try syntax table: */
  if (modname)
    val = scheme_module_syntax(modname, env->genv, SCHEME_STX_SYM(symbol));
  else {
    /* Only try syntax table if there's not an explicit (later)
       variable mapping: */
    if (genv->shadowed_syntax 
	&& scheme_lookup_in_table(genv->shadowed_syntax, (char *)SCHEME_STX_SYM(symbol)))
      val = NULL;
    else
      val = scheme_lookup_in_table(genv->syntax, (char *)SCHEME_STX_SYM(symbol));
  }
  
  if (val)
    return val;

  if (modname)
    scheme_check_accessible_in_module(genv, symbol, srcsym);

  if (modname && (flags & SCHEME_SETTING)) {
    if (SAME_OBJ(srcsym, symbol) || SAME_OBJ(SCHEME_STX_SYM(srcsym), symbol))
      symbol = NULL;
    scheme_wrong_syntax("set!", symbol, srcsym, "cannot mutate module-required variable");
  }

  if (!modname  && (flags & SCHEME_SETTING) && genv->module) {
    /* Check for set! of unbound variable: */
    
    if (!scheme_lookup_in_table(genv->toplevel, (const char *)symbol))
      scheme_wrong_syntax("set!", NULL, srcsym, "unbound variable in module");
  }

  if (!modname && (flags & SCHEME_NULL_FOR_UNBOUND))
    return NULL;

  if (modname && !(flags & SCHEME_RESOLVE_MODIDS) && !SAME_OBJ(modidx, modname)) {
    /* Create a module variable reference, so that idx is preserved: */
    val = scheme_alloc_object();
    val->type = scheme_module_variable_type;

    SCHEME_PTR1_VAL(val) = modidx;
    SCHEME_PTR2_VAL(val) = SCHEME_STX_SYM(symbol);

    return val;
  }

  if (!modname && (flags & SCHEME_SETTING) && genv->module) {
    /* Need to return a variable reference in this case, too. */
    val = scheme_alloc_object();
    val->type = scheme_module_variable_type;

    SCHEME_PTR1_VAL(val) = genv->module->self_modidx;
    SCHEME_PTR2_VAL(val) = SCHEME_STX_SYM(symbol);

    return val;
  }

  b = scheme_bucket_from_table(genv->toplevel, (char *)SCHEME_STX_SYM(symbol));

  if ((flags & SCHEME_ELIM_CONST) && b && b->val 
      && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST)
      && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE))
    return (Scheme_Object *)b->val;

  if (!((Scheme_Bucket_With_Home *)b)->home)
    ((Scheme_Bucket_With_Home *)b)->home = genv;
  
  return (Scheme_Object *)b;
}

void scheme_shadow(Scheme_Env *env, Scheme_Object *n, int stxtoo)
{
  if (!env->module) {
    if (env->rename)
      scheme_remove_module_rename(env->rename, n);

    if (stxtoo) {
      if (!env->shadowed_syntax) {
	Scheme_Hash_Table *ht;
	ht = scheme_hash_table(7, SCHEME_hash_ptr);
	env->shadowed_syntax = ht;
      }
	
      scheme_add_to_table(env->shadowed_syntax, (const char *)n, scheme_true, 0);
    } else {
      if (env->shadowed_syntax) {
	Scheme_Bucket *b;

	b = scheme_bucket_or_null_from_table(env->shadowed_syntax, (const char *)n, 0);
	if (b)
	  b->val = NULL;
      }
    }
  }
}

void scheme_env_make_closure_map(Scheme_Comp_Env *env, short *_size, short **_map)
{
  Compile_Data *data;
  Scheme_Comp_Env *frame;
  int i, j, pos = 0, lpos = 0;
  short *map, size;

  /* Count vars used by this closure (skip args): */
  j = 1;
  for (frame = env->next; frame; frame = frame->next) {
    data = COMPILE_DATA(frame);

    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (data->stat_dists) {
      for (i = 0; i < frame->num_bindings; i++) {
	if (data->sd_depths[i] > j) {
	  if (data->stat_dists[i][j]) {
	    pos++;
	  }
	}
      }
    }
  }

  data = NULL; /* Clear unaligned pointer */

  size = pos;
  *_size = size;
  map = MALLOC_N_ATOMIC(short, size);
  *_map = map;

  /* Build map, unmarking locals and marking deeper in parent prame */
  j = 1; pos = 0;
  for (frame = env->next; frame; frame = frame->next) {
    data = COMPILE_DATA(frame);

    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (data->stat_dists) {
      for (i = 0; i < frame->num_bindings; i++) {
	if (data->sd_depths[i] > j) {
	  if (data->stat_dists[i][j]) {
	    map[pos++] = lpos;
	    data->stat_dists[i][j] = 0; /* This closure's done with these vars... */
	    data->stat_dists[i][j - 1] = 1; /* ... but insure previous keeps */
	  }
	}
	lpos++;
      }
    } else
      lpos += frame->num_bindings;
  }
}

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count)
{
  int *v, i;
  
  v = MALLOC_N_ATOMIC(int, count);
  memcpy(v, COMPILE_DATA(frame)->use + start, sizeof(int) * count);

  for (i = count; i--; ) {
    int old;
    old = v[i];
    v[i] = 0;
    if (old & (ARBITRARY_USE | CONSTRAINED_USE))
      v[i] |= SCHEME_WAS_USED;
    if (old & WAS_SET_BANGED)
      v[i] |= SCHEME_WAS_SET_BANGED;
  }

  return v;
}

void scheme_begin_dup_symbol_check(DupCheckRecord *r, Scheme_Comp_Env *env)
{
  r->phase = env->genv->phase;
  r->count = 0;
}

void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form)
{
  int i;
  char *key;

  if (r->count <= 5) {
    for (i = 0; i < r->count; i++) {
      if (scheme_stx_bound_eq(symbol, r->syms[i], r->phase))
	scheme_wrong_syntax(where, symbol, form,
			    "duplicate %s name", what);
    }

    if (r->count < 5) {
      r->syms[r->count++] = symbol;
      return;
    } else {
      Scheme_Hash_Table *ht;
      ht = scheme_hash_table(7, SCHEME_hash_bound_id);
      r->ht = ht;
      for (i = 0; i < r->count; i++) {
	key = (char *)r->syms[i];
	scheme_add_to_table(ht, key, (void *)scheme_true, 0);
      }
    }
  }

  key = (char *)symbol;
  if (scheme_lookup_in_table(r->ht, key)) {
    scheme_wrong_syntax(where, symbol, form,
			"duplicate %s name", what);
  }

  scheme_add_to_table(r->ht, key, (void *)scheme_true, 0);
}

Resolve_Info *scheme_resolve_info_create()
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->count = 0;
  naya->next = NULL;

  return naya;
}

Resolve_Info *scheme_resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapc)
     /* size = number of appended items in run-time frame */
     /* oldisze = number of appended items in original compile-time frame */
     /* mapc = mappings that will be installed */
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->next = info;
  naya->size = size;
  naya->oldsize = oldsize;
  naya->count = mapc;
  naya->pos = 0;
  naya->anchor_offset = 0;
  if (mapc) {
    int i, *ia;
    short *sa;

    sa = MALLOC_N_ATOMIC(short, mapc);
    naya->old_pos = sa;
    sa = MALLOC_N_ATOMIC(short, mapc);
    naya->new_pos = sa;
    ia = MALLOC_N_ATOMIC(int, mapc);
    naya->flags = ia;

    /* necessary? added when changed allocation to atomic */
    for (i = mapc; i--; ) {
      naya->old_pos[i] = 0;
      naya->new_pos[i] = 0;
      naya->flags[i] = 0;
    }
  }

  return naya;
}

void scheme_resolve_info_add_mapping(Resolve_Info *info, int oldp, int newp, int flags)
{
  if (info->pos == info->count) {
    scheme_signal_error("internal error: add_mapping: "
			"too many: %d", info->pos);
  }

  info->old_pos[info->pos] = oldp;
  info->new_pos[info->pos] = newp;
  info->flags[info->pos] = flags;
  
  info->pos++;
}

void scheme_resolve_info_set_anchor_offset(Resolve_Info *info, int offset)
{
  info->anchor_offset = offset;
}

static int resolve_info_lookup(Resolve_Info *info, int pos, int *flags)
{
  int i, offset = 0, orig = pos;
  int get_anchor;

  if (pos < 0) {
    get_anchor = pos;
    pos = -(pos + 1);
  } else
    get_anchor = 0;

  while (info) {
    for (i = info->pos; i--; ) {
      int oldp = info->old_pos[i];
      if (pos == oldp) {
	/* not yet mapped anchor */
	if (flags)
	  *flags = info->flags[i];
	return info->new_pos[i] + offset + (get_anchor ? info->anchor_offset : 0);
      }
      if (get_anchor && (get_anchor == oldp)) {
	/* re-mapped anchor */
	if (flags)
	  *flags = info->flags[i];
	return info->new_pos[i] + offset;
      }
    }

    pos -= info->oldsize;
    offset += info->size;
    info = info->next;
  }

  scheme_signal_error("internal error: scheme_resolve_info_lookup: "
		      "variable %d not found", orig);

  return 0;
}

int scheme_resolve_info_flags(Resolve_Info *info, int pos)
{
  int flags;

  resolve_info_lookup(info, pos, &flags);

  return flags;
}

int scheme_resolve_info_lookup(Resolve_Info *info, int pos, int *flags)
{
  return resolve_info_lookup(info, pos, flags);
}

int scheme_resolve_info_lookup_anchor(Resolve_Info *info, int pos)
{
  return resolve_info_lookup(info, -(pos + 1), NULL);
}


Scheme_Object *
scheme_lookup_global(Scheme_Object *symbol, Scheme_Env *env)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_or_null_from_table(env->toplevel, (char *)symbol, 0);
  if (b) {
    if (!((Scheme_Bucket_With_Home *)b)->home)
      ((Scheme_Bucket_With_Home *)b)->home = env;
    return (Scheme_Object *)b->val;
  }

  return NULL;
}

Scheme_Bucket *
scheme_global_bucket(Scheme_Object *symbol, Scheme_Env *env)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_from_table(env->toplevel, (char *)symbol);
  if (!((Scheme_Bucket_With_Home *)b)->home)
    ((Scheme_Bucket_With_Home *)b)->home = env;
    
  return b;
}

Scheme_Bucket *
scheme_global_keyword_bucket(Scheme_Object *symbol, Scheme_Env *env)
{
  return scheme_bucket_from_table(env->syntax, (char *)symbol);
}

Scheme_Object *scheme_make_envunbox(Scheme_Object *value)
{
  Scheme_Object *obj;

#ifdef MZ_PRECISE_GC
  obj = (Scheme_Object *)scheme_malloc_envunbox(sizeof(Scheme_Small_Object));
  obj->type = scheme_envunbox_type;
#else
  obj = (Scheme_Object *)scheme_malloc_envunbox(sizeof(Scheme_Object*));
#endif
  SCHEME_ENVBOX_VAL(obj) = value;

  return obj;
}

static Scheme_Object *
namespace_variable_binding(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Env *env;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-defined-value", "symbol", 0, argc, argv);

  env = scheme_get_env(scheme_config);

  if (argc > 1) {
    Scheme_Bucket *bucket;

    bucket = scheme_global_bucket(argv[0], env);

    scheme_set_global_bucket("namespace-defined-value", bucket, argv[1], 1);

    return scheme_void;
  } else {
    v = scheme_lookup_global(argv[0], env);
    
    if (!v)
      scheme_raise_exn(MZEXN_VARIABLE, argv[0],
		       "namespace-defined-value: %S is not defined",
		       argv[0]);

    return v;
  }
}

static Scheme_Object *
local_exp_time_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *sym;
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-value: not currently transforming");

  sym = argv[0];

  if (!SCHEME_SYMBOLP(sym)
      && !SCHEME_STX_SYMBOLP(sym))
    scheme_wrong_type("syntax-local-value", "syntax identifier", 0, argc, argv);

  if (argc > 1)
    scheme_check_proc_arity("syntax-local-value", 0, 1, argc, argv);

  if (SCHEME_STXP(sym))
    if (scheme_current_thread->current_local_mark)
      sym = scheme_add_remove_mark(sym, scheme_current_thread->current_local_mark);

  v = scheme_static_distance(sym, env,
			     (SCHEME_NULL_FOR_UNBOUND
			      + SCHEME_RESOLVE_MODIDS
			      + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			      + SCHEME_OUT_OF_CONTEXT_OK + SCHEME_ELIM_CONST));

  /* Deref globals */
  if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
    v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;

  if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
    if (argc > 1)
      return _scheme_tail_apply(argv[1], 0, NULL);
    else
      scheme_wrong_syntax("syntax-local-value",
			  NULL, 
			  argv[0],
			  "not defined as syntax");
  }
  
  return SCHEME_PTR_VAL(v);
}

static Scheme_Object *
local_exp_time_name(int argc, Scheme_Object *argv[])
{
  Scheme_Object *sym;

  sym = scheme_current_thread->current_local_name;
  if (!sym)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-name: not currently transforming");

  return sym;
}

static Scheme_Object *
local_context(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-context: not currently transforming");

  if (scheme_is_module_env(env))
    return scheme_intern_symbol("module");
  else if (scheme_is_toplevel(env))
    return scheme_intern_symbol("top-level");
  else
    return scheme_intern_symbol("expression");
}

static Scheme_Object *
make_set_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  scheme_check_proc_arity("make-set!-transformer", 1, 0, argc, argv);

  v = scheme_alloc_small_object();
  v->type = scheme_id_macro_type;
  SCHEME_PTR_VAL(v) = argv[0];

  return v;
}


/*********************************************************************/

static Scheme_Object *write_variable(Scheme_Object *obj)
{
  Scheme_Env *home;
  Scheme_Object *sym;
  Scheme_Module *m;

  sym = (Scheme_Object *)(SCHEME_VAR_BUCKET(obj))->key;

  home = ((Scheme_Bucket_With_Home *)obj)->home;
  m = home->module;

  /* If we get a writeable variable (instead of a module variable),
     it must be a reference to a module referenced directly by its
     a symbolic name (i.e., no path). */

  if (m)
    sym = scheme_make_pair(m->modname, sym);

  return sym;
}

static Scheme_Object *read_variable(Scheme_Object *obj)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  if (!SCHEME_SYMBOLP(obj)) {
    /* Find variable from module. */
    Scheme_Object *modname, *varname;

    modname = SCHEME_CAR(obj);
    varname = SCHEME_CDR(obj);

    if (SAME_OBJ(modname, kernel_symbol))
      return (Scheme_Object *)scheme_global_bucket(varname, scheme_initial_env);
    else {
      obj = scheme_alloc_object();
      obj->type = scheme_module_variable_type;
      SCHEME_PTR1_VAL(obj) = modname;
      SCHEME_PTR2_VAL(obj) = varname;

      return obj;
    }
  }

  return (Scheme_Object *)scheme_global_bucket(obj, env);
}

static Scheme_Object *write_module_variable(Scheme_Object *obj)
{
  return scheme_make_pair(SCHEME_PTR1_VAL(obj), SCHEME_PTR2_VAL(obj));
}

static Scheme_Object *read_module_variable(Scheme_Object *obj)
{
  Scheme_Object *r;

  r = scheme_alloc_object();
  r->type = scheme_module_variable_type;
  SCHEME_PTR1_VAL(r) = SCHEME_CAR(obj);
  SCHEME_PTR2_VAL(r) = SCHEME_CDR(obj);

  return r;
}

static Scheme_Object *write_local(Scheme_Object *obj)
{
  return scheme_make_integer(SCHEME_LOCAL_POS(obj));
}

static Scheme_Object *read_local(Scheme_Object *obj)
{
  return scheme_make_local(scheme_local_type,
			   SCHEME_INT_VAL(obj));
}

static Scheme_Object *read_local_unbox(Scheme_Object *obj)
{
  return scheme_make_local(scheme_local_unbox_type,
			   SCHEME_INT_VAL(obj));
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_ENV_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_comp_env, mark_comp_env);
  GC_REG_TRAV(scheme_rt_resolve_info, mark_resolve_info);
}

END_XFORM_SKIP;

#endif
