/*
  MzScheme
  Copyright (c) 2004 PLT Scheme, Inc.
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

/* This file implements environments (both compile-time and top-level
   envionments, a.k.a. namespaces), and also implements much of the
   initialization sequence (filling the initial namespace). */

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

/* #define TIME_STARTUP_PROCESS */

/* globals */
int scheme_allow_set_undefined;

int scheme_starting_up;

Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

#define MAX_CONST_TOPLEVEL_DEPTH 16
#define MAX_CONST_TOPLEVEL_POS 16
Scheme_Object *toplevels[MAX_CONST_TOPLEVEL_DEPTH][MAX_CONST_TOPLEVEL_POS];

Scheme_Env *scheme_initial_env;

/* locals */
static Scheme_Env *make_env(Scheme_Env *base, int semi, int toplevel_size);
static void make_init_env(void);

static Scheme_Object *namespace_identifier(int, Scheme_Object *[]);
static Scheme_Object *namespace_variable_value(int, Scheme_Object *[]);
static Scheme_Object *namespace_set_variable_value(int, Scheme_Object *[]);
static Scheme_Object *namespace_undefine_variable(int, Scheme_Object *[]);
static Scheme_Object *namespace_mapped_symbols(int, Scheme_Object *[]);
static Scheme_Object *namespace_module_registry(int, Scheme_Object *[]);
static Scheme_Object *local_exp_time_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_context(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_introduce(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_module_introduce(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_get_shadower(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_introducer(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_set_transformer(int argc, Scheme_Object *argv[]);
static Scheme_Object *set_transformer_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *set_transformer_proc(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_rename_transformer(int argc, Scheme_Object *argv[]);
static Scheme_Object *rename_transformer_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *write_toplevel(Scheme_Object *obj);
static Scheme_Object *read_toplevel(Scheme_Object *obj);
static Scheme_Object *write_variable(Scheme_Object *obj);
static Scheme_Object *read_variable(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);
static Scheme_Object *write_resolve_prefix(Scheme_Object *obj);
static Scheme_Object *read_resolve_prefix(Scheme_Object *obj);

static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

typedef Scheme_Object *(*Lazy_Macro_Fun)(Scheme_Object *, int);

static Scheme_Object *kernel_symbol;

static int tl_id_counter = 0;
static int intdef_counter = 0;

static int builtin_ref_counter = 0;

static int env_uid_counter;

#define ARBITRARY_USE 1
#define CONSTRAINED_USE 2
#define WAS_SET_BANGED 4

typedef struct Compile_Data {
  char **stat_dists; /* (pos, depth) => used? */
  int *sd_depths;
  int used_toplevel;
  int max_stx_used;
  char *stxes_used;
  int num_const;
  Scheme_Object **const_names;
  Scheme_Object **const_vals;
  Scheme_Object **const_uids;
  int *use;
} Compile_Data;

typedef struct Scheme_Full_Comp_Env {
  Scheme_Comp_Env base;
  Compile_Data data;
} Scheme_Full_Comp_Env;
static void init_compile_data(Scheme_Comp_Env *env);

/* Precise GC WARNING: this macro produces unaligned pointers: */
#define COMPILE_DATA(e) (&((Scheme_Full_Comp_Env *)e)->data)

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

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

    env = scheme_make_empty_env();
    scheme_install_initial_module_set(env);

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

  scheme_init_stack_check();
  scheme_init_portable_case();


  {
    int i, k;

#ifndef USE_TAGGED_ALLOCATION
    GC_CAN_IGNORE Scheme_Local *all;

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

  {
    int i, k;

#ifndef USE_TAGGED_ALLOCATION
    GC_CAN_IGNORE Scheme_Toplevel *all;

    all = (Scheme_Toplevel *)scheme_malloc_eternal(sizeof(Scheme_Toplevel) 
						   * MAX_CONST_TOPLEVEL_DEPTH 
						   * MAX_CONST_TOPLEVEL_POS);
# ifdef MEMORY_COUNTING_ON
    scheme_misc_count += sizeof(Scheme_Toplevel) * MAX_CONST_TOPLEVEL_DEPTH * MAX_CONST_TOPLEVEL_POS);
# endif
#endif

    for (i = 0; i < MAX_CONST_TOPLEVEL_DEPTH; i++) {
      for (k = 0; k < MAX_CONST_TOPLEVEL_POS; k++) {
	Scheme_Toplevel *v;
	
#ifndef USE_TAGGED_ALLOCATION
	v = (all++);
#else
	v = (Scheme_Toplevel *)scheme_malloc_eternal_tagged(sizeof(Scheme_Toplevel));
#endif
	v->type = scheme_toplevel_type;
	v->depth = i;
	v->position = k;
	
	toplevels[i][k] = (Scheme_Object *)v;
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

#ifdef WINDOWS_PROCESSES
  /* Must be called before first scheme_make_thread() */
  scheme_init_thread_memory();
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
  scheme_init_memtrace(env);

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

  env = make_env(NULL, 0, GLOBAL_TABLE_SIZE);

  scheme_set_param(scheme_current_thread->config, MZCONFIG_ENV, 
		   (Scheme_Object *)env);

  REGISTER_SO(scheme_initial_env);
  scheme_initial_env = env;

  scheme_defining_primitives = 1;
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

  /* The ordering of the first few init calls is important, so add to
     the end of the list, not the beginning. */
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

  scheme_add_global_constant("namespace-symbol->identifier",
			     scheme_make_prim_w_arity(namespace_identifier,
						      "namespace-symbol->identifier",
						      1, 2),
			     env);

  scheme_add_global_constant("namespace-variable-value",
			     scheme_make_prim_w_arity(namespace_variable_value,
						      "namespace-variable-value",
						      1, 4),
			     env);

  scheme_add_global_constant("namespace-set-variable-value!",
			     scheme_make_prim_w_arity(namespace_set_variable_value,
						      "namespace-set-variable-value!",
						      2, 4),
			     env);

  scheme_add_global_constant("namespace-undefine-variable!",
			     scheme_make_prim_w_arity(namespace_undefine_variable,
						      "namespace-undefine-variable!",
						      1, 2),
			     env);

  scheme_add_global_constant("namespace-mapped-symbols",
			     scheme_make_prim_w_arity(namespace_mapped_symbols,
						      "namespace-mapped-symbols",
						      0, 1),
			     env);

  scheme_add_global_constant("namespace-module-registry",
			     scheme_make_prim_w_arity(namespace_module_registry,
						      "namespace-module-registry",
						      1, 1),
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
  scheme_add_global_constant("syntax-local-get-shadower", 
			     scheme_make_prim_w_arity(local_get_shadower,
						      "syntax-local-get-shadower",
						      1, 1),
			     env);
  scheme_add_global_constant("syntax-local-introduce", 
			     scheme_make_prim_w_arity(local_introduce,
						      "syntax-local-introduce",
						      1, 1),
			     env);
  scheme_add_global_constant("syntax-local-module-introduce", 
			     scheme_make_prim_w_arity(local_module_introduce,
						      "syntax-local-module-introduce",
						      1, 1),
			     env);
  scheme_add_global_constant("make-syntax-introducer", 
			     scheme_make_prim_w_arity(make_introducer,
						      "make-syntax-introducer",
						      0, 1),
			     env);

  scheme_add_global_constant("make-set!-transformer", 
			     scheme_make_prim_w_arity(make_set_transformer,
						      "make-set!-transformer",
						      1, 1),
			     env);

  scheme_add_global_constant("set!-transformer?", 
			     scheme_make_prim_w_arity(set_transformer_p,
						      "set!-transformer?",
						      1, 1),
			     env);

  scheme_add_global_constant("set!-transformer-procedure", 
			     scheme_make_prim_w_arity(set_transformer_proc,
						      "set!-transformer-procedure",
						      1, 1),
			     env);

  scheme_add_global_constant("make-rename-transformer", 
			     scheme_make_prim_w_arity(make_rename_transformer,
						      "make-rename-transformer",
						      1, 1),
			     env);

  scheme_add_global_constant("rename-transformer?", 
			     scheme_make_prim_w_arity(rename_transformer_p,
						      "rename-transformer?",
						      1, 1),
			     env);

  {
    Scheme_Object *sym;
    sym = scheme_intern_symbol("mzscheme");
    scheme_current_thread->name = sym;
  }

  DONE_TIME(env);

  scheme_install_type_writer(scheme_toplevel_type, write_toplevel);
  scheme_install_type_reader(scheme_toplevel_type, read_toplevel);
  scheme_install_type_writer(scheme_variable_type, write_variable);
  scheme_install_type_reader(scheme_variable_type, read_variable);
  scheme_install_type_writer(scheme_local_type, write_local);
  scheme_install_type_reader(scheme_local_type, read_local);
  scheme_install_type_writer(scheme_local_unbox_type, write_local);
  scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);
  scheme_install_type_writer(scheme_resolve_prefix_type, write_resolve_prefix);
  scheme_install_type_reader(scheme_resolve_prefix_type, read_resolve_prefix);

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  MARK_START_TIME();

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

/* Shutdown procedure for resetting a namespace: */
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

/*========================================================================*/
/*                        namespace constructors                          */
/*========================================================================*/

Scheme_Env *scheme_make_empty_env(void)
{
  return make_env(NULL, 0, 7);
}

static Scheme_Env *make_env(Scheme_Env *base, int semi, int toplevel_size)
{
  Scheme_Bucket_Table *toplevel, *syntax;
  Scheme_Hash_Table *module_registry;
  Scheme_Object *modchain;
  Scheme_Env *env;

  toplevel = scheme_make_bucket_table(toplevel_size, SCHEME_hash_ptr);
  toplevel->with_home = 1;

  if (semi > 0) {
    syntax = NULL;
    modchain = NULL;
    module_registry = NULL;
  } else {
    syntax = scheme_make_bucket_table(7, SCHEME_hash_ptr);
    if (base) {
      modchain = base->modchain;
      module_registry = base->module_registry;
    } else {
      if (semi < 0) {
	module_registry = NULL;
	modchain = NULL;
      } else {
	Scheme_Hash_Table *modules;

	modules = scheme_make_hash_table(SCHEME_hash_ptr);
	modchain = scheme_make_vector(3, scheme_false);
	SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)modules;

	module_registry = scheme_make_hash_table(SCHEME_hash_ptr);
	module_registry->type = scheme_module_registry_type;
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
  }

  return env;
}

Scheme_Env *
scheme_new_module_env(Scheme_Env *env, Scheme_Module *m, int new_exp_module_tree)
{
  Scheme_Env *menv;

  menv = make_env(env, 0, 7);

  menv->module = m;

  if (new_exp_module_tree) {
    Scheme_Object *p;
    Scheme_Hash_Table *modules;

    modules = scheme_make_hash_table(SCHEME_hash_ptr);
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

    eenv = make_env(NULL, -1, 7);
    eenv->phase = env->phase + 1;

    eenv->module = env->module;
    eenv->module_registry = env->module_registry;

    modchain = SCHEME_VEC_ELS(env->modchain)[1];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *next_modules;

      next_modules = scheme_make_hash_table(SCHEME_hash_ptr);
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

  menv2->syntax = menv->syntax;

  menv2->phase = menv->phase;
  menv2->link_midx = menv->link_midx;
  menv2->running = menv->running;
  menv2->et_running = menv->et_running;

  menv2->require_names = menv->require_names;
  menv2->et_require_names = menv->et_require_names;

  menv2->toplevel = menv->toplevel;
  
  menv2->modchain = modchain;

  if (!SCHEME_NULLP(menv2->module->et_requires)) {
    /* We'll need the next link in the modchain: */
    modchain = SCHEME_VEC_ELS(modchain)[1];
    if (SCHEME_FALSEP(modchain)) {
      Scheme_Hash_Table *next_modules;
      
      next_modules = scheme_make_hash_table(SCHEME_hash_ptr);
      modchain = scheme_make_vector(3, scheme_false);
      SCHEME_VEC_ELS(modchain)[0] = (Scheme_Object *)next_modules;
      SCHEME_VEC_ELS(menv2->modchain)[1] = modchain;
      SCHEME_VEC_ELS(modchain)[2] = menv2->modchain;
    }
  }

  return menv2;
}

Scheme_Bucket_Table *scheme_clone_toplevel(Scheme_Bucket_Table *ht, Scheme_Env *home)
{
  Scheme_Bucket_Table *r;
  Scheme_Bucket **bs;
  int i;

  r = scheme_make_bucket_table(ht->size, SCHEME_hash_ptr);
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

void scheme_clean_dead_env(Scheme_Env *env)
{
  Scheme_Object *modchain, *next;

  if (env->exp_env) {
    scheme_clean_dead_env(env->exp_env);
    env->exp_env = NULL;
  }

  env->modvars = NULL;
  
  modchain = env->modchain;
  env->modchain = NULL;
  while (modchain && !SCHEME_VECTORP(modchain)) {
    next = SCHEME_VEC_ELS(modchain)[1];
    SCHEME_VEC_ELS(modchain)[1] = scheme_void;
    modchain = next;
  }
}

/*========================================================================*/
/*                           namespace bindings                           */
/*========================================================================*/

/********** Lookup **********/

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
  Scheme_Bucket *b;
    
  b = scheme_bucket_from_table(env->syntax, (char *)symbol);
    
  return b;
}

Scheme_Bucket *
scheme_exptime_global_bucket(Scheme_Object *symbol, Scheme_Env *env)
{
  scheme_prepare_exp_env(env);
  return scheme_global_bucket(symbol, env->exp_env);
}

/********** Set **********/

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
      ((Scheme_Bucket_With_Flags *)b)->flags |= (GLOB_HAS_REF_ID | GLOB_IS_CONST);
    }
  } else
    scheme_add_to_table(env->syntax, (const char *)sym, obj, constant);
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
scheme_add_global_keyword_symbol(Scheme_Object *name, Scheme_Object *obj, 
				 Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, name, obj, 0, 0);
}

void scheme_shadow(Scheme_Env *env, Scheme_Object *n, int stxtoo)
{
  if (env->rename) {
    scheme_remove_module_rename(env->rename, n);
    if (env->module) {
      scheme_extend_module_rename(env->rename,
				  env->module->self_modidx,
				  n, n,
				  env->module->self_modidx,
				  n);
    }
  }

  if (stxtoo) {
    if (!env->module || env->rename) {
      if (!env->shadowed_syntax) {
	Scheme_Hash_Table *ht;
	ht = scheme_make_hash_table(SCHEME_hash_ptr);
	env->shadowed_syntax = ht;
      }
      
      scheme_hash_set(env->shadowed_syntax, n, scheme_true);
    }
  } else {
    if (env->shadowed_syntax)
      scheme_hash_set(env->shadowed_syntax, n, NULL);
  }
}

/********** Auxilliary tables **********/

Scheme_Object **scheme_make_builtin_references_table(void)
{
  Scheme_Bucket_Table *ht;
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
  Scheme_Bucket_Table *ht;
  Scheme_Hash_Table*result;
  Scheme_Bucket **bs;
  long i;

  ht = scheme_initial_env->toplevel;
  bs = ht->buckets;

  result = scheme_make_hash_table(SCHEME_hash_ptr);

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST))
      scheme_hash_set(result, b->val, (Scheme_Object *)b);
  }

  return result;
}

/*========================================================================*/
/*        compile-time env, constructors and simple queries               */
/*========================================================================*/

static void init_compile_data(Scheme_Comp_Env *env)
{
  Compile_Data *data;
  int i, c, *use;

  c = env->num_bindings;
  if (c)
    use = MALLOC_N_ATOMIC(int, c);
  else
    use = NULL;

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
  frame->prefix = base->prefix;

  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, int flags)
{
  Scheme_Comp_Env *e;
  Comp_Prefix *cp;

  e = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Full_Comp_Env);
#ifdef MZTAG_REQUIRED
  e->type = scheme_rt_comp_env;
#endif
  e->num_bindings = 0;
  e->next = NULL;
  e->genv = genv;
  e->flags = flags;
  init_compile_data(e);

  cp = MALLOC_ONE_RT(Comp_Prefix);
#ifdef MZTAG_REQUIRED
  cp->type = scheme_rt_comp_prefix;
#endif

  e->prefix = cp;

  return e;
}

Scheme_Comp_Env *scheme_new_expand_env(Scheme_Env *genv, int flags)
{
  Scheme_Comp_Env *e;

  e = scheme_new_comp_env(genv, flags);
  e->prefix = NULL;

  return e;
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
  return !!(env->flags & (SCHEME_MODULE_FRAME | SCHEME_MODULE_BEGIN_FRAME));
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, env);
}

static Scheme_Object *make_toplevel(mzshort depth, int position, int resolved)
{
  Scheme_Toplevel *tl;

  if (resolved) {
    if ((depth < MAX_CONST_TOPLEVEL_DEPTH)
	&& (position < MAX_CONST_TOPLEVEL_POS))
      return toplevels[depth][position];
  }

  tl = (Scheme_Toplevel *)scheme_malloc_atomic_tagged(sizeof(Scheme_Toplevel));
  tl->type = (resolved ? scheme_toplevel_type : scheme_compiled_toplevel_type);
  tl->depth = depth;
  tl->position = position;
  return (Scheme_Object *)tl;
}

Scheme_Object *scheme_register_toplevel_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env,
						  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Comp_Env *frame;
  Comp_Prefix *cp = env->prefix;
  Scheme_Hash_Table *ht;
  Scheme_Object *o;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    return make_toplevel(0, 0, 0);
  }

  /* Register use at lambda, if any: */
  frame = env;
  while (frame) {
    if (frame->flags & SCHEME_LAMBDA_FRAME) {
      COMPILE_DATA(frame)->used_toplevel = 1;
      break;
    }
    frame = frame->next;
  }

  ht = cp->toplevels;
  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->toplevels = ht;
  }

  o = scheme_hash_get(ht, var);
  if (o)
    return o;

  o = make_toplevel(0, cp->num_toplevels, 0);

  cp->num_toplevels++;
  scheme_hash_set(ht, var, o);

  return o;
}

Scheme_Object *scheme_register_stx_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env, 
					     Scheme_Compile_Info *rec, int drec)
{
  Comp_Prefix *cp = env->prefix;
  Scheme_Local *l;
  Scheme_Object *o;
  int pos;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
    l->type = scheme_compiled_quote_syntax_type;
    l->position = 0;

    return (Scheme_Object *)l;
  }

  if (!cp->stxes) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->stxes = ht;
  }

  pos = cp->num_stxes;

  l = (Scheme_Local *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  l->type = scheme_compiled_quote_syntax_type;
  l->position = pos;

  cp->num_stxes++;
  o = (Scheme_Object *)l;
  
  scheme_hash_set(cp->stxes, var, o);

  /* Register use at lambda, if any: */
  while (env) {
    if (env->flags & SCHEME_LAMBDA_FRAME) {
      if (COMPILE_DATA(env)->max_stx_used <= pos) {
	char *p;
	int max_stx_used = (pos * 2) + 10;
	
	p = MALLOC_N_ATOMIC(char, max_stx_used);
	memset(p, 0, max_stx_used);
	memcpy(p, COMPILE_DATA(env)->stxes_used, COMPILE_DATA(env)->max_stx_used);
	COMPILE_DATA(env)->stxes_used = p;
	COMPILE_DATA(env)->max_stx_used = max_stx_used;
      }
      
      COMPILE_DATA(env)->stxes_used[pos] = 1;
      break;
    }
    env = env->next;
  }

  return o;
}

/*========================================================================*/
/*                     compile-time env, lookup bindings                  */
/*========================================================================*/

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

static Scheme_Object *force_lazy_macro(Scheme_Object *val, long phase)
{
  Lazy_Macro_Fun f = (Lazy_Macro_Fun)SCHEME_PTR1_VAL(val);
  Scheme_Object *data = SCHEME_PTR2_VAL(val);
  return f(data, phase);
}

static Scheme_Local *get_frame_loc(Scheme_Comp_Env *frame,
				   int i, int j, int p, int flags)
/* Generates a Scheme_Local record for a static distance coodinate, and also
   marks the variable as used for closures. */
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

Scheme_Object *scheme_hash_module_variable(Scheme_Env *env, Scheme_Object *modidx, Scheme_Object *stxsym,
					   int pos)
{
  Scheme_Object *val;
  Scheme_Hash_Table *ht;

  if (!env->modvars) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    env->modvars = ht;
  }

  stxsym = SCHEME_STX_SYM(stxsym);

  ht = (Scheme_Hash_Table *)scheme_hash_get(env->modvars, modidx);

  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(env->modvars, modidx, (Scheme_Object *)ht);
  }

  val = scheme_hash_get(ht, stxsym);

  if (!val) {
    Module_Variable *mv;

    mv = MALLOC_ONE_TAGGED(Module_Variable);
    mv->type = scheme_module_variable_type;
    
    mv->modidx = modidx;
    mv->sym = stxsym;
    mv->pos = pos;

    val = (Scheme_Object *)mv;

    scheme_hash_set(ht, stxsym, val);
  }

  return val;
}

Scheme_Object *scheme_tl_id_sym(Scheme_Env *env, Scheme_Object *id, int is_def)
/* The `env' argument can actually be a hash table. */
{
  Scheme_Object *marks = NULL, *sym, *map, *l, *a, *amarks, *m, *best_match;
  int best_match_skipped, ms;
  Scheme_Hash_Table *marked_names;

  sym = SCHEME_STX_SYM(id);

  if (SCHEME_HASHTP((Scheme_Object *)env))
    marked_names = (Scheme_Hash_Table *)env;
  else {
    /* If there's no table and we're not defining, bail out fast */
    if (!is_def && !env->marked_names)
      return sym;
    marked_names = env->marked_names;
  }

  if (is_def) {
    /* If we're defining, see if we need to create a table.  Getting
       marks is relatively expensive, but we only do this once per
       definition. */
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks))
      return sym;
  }

  if (!marked_names) {
    marked_names = scheme_make_hash_table(SCHEME_hash_ptr);
    env->marked_names = marked_names;
  }

  map = scheme_hash_get(marked_names, sym);

  if (!map) {
    /* If we're not defining, we can bail out before extracting marks. */
    if (!is_def)
      return sym;
    else
      map = scheme_null;
  }

  if (!marks) {
    /* We really do need the marks. Get them. */
    marks = scheme_stx_extract_marks(id);
    if (SCHEME_NULLP(marks))
      return sym;
  }

  best_match = NULL;
  best_match_skipped = scheme_list_length(marks);

  /* Find a mapping that matches the longest tail of marks */
  for (l = map; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    amarks = SCHEME_CAR(a);
    if (is_def) {
      if (scheme_equal(amarks, marks)) {
	best_match = SCHEME_CDR(a);
	break;
      }
    } else {
      for (m = marks, ms = 0; 
	   SCHEME_PAIRP(m) && (ms < best_match_skipped);
	   m = SCHEME_CDR(m), ms++) {
  
	if (scheme_equal(amarks, m)) {
	  if (ms < best_match_skipped) {
	    best_match = SCHEME_CDR(a);
	    best_match_skipped = ms;
	    break;
	  }
	}
      }
    }
  }

  if (!best_match) {
    if (!is_def) {
      return sym;
    }

    /* Last chance before making up a new name. If we're processing a
       module body generated by `expand', then we picked a name last
       time around. We can't pick a new name now, otherwise
       "redundant" module renamings wouldn't be redundant. (See
       simpify in stxobj.c.) So check for a context-determined
       existing rename. */
    if (!SCHEME_HASHTP((Scheme_Object *)env) && env->module && (is_def != 2)) {
      Scheme_Object *mod, *nm = id;
      mod = scheme_stx_module_name(&nm, env->phase, NULL, NULL);
      if (mod /* must refer to env->module, otherwise there would
		 have been an error before getting here */
	  && NOT_SAME_OBJ(nm, sym))
	/* It has a rename already! */
	best_match = nm;
    }

    /* Adding a definition. We "gensym" here in a sense; actually, we
       use a symbol table that's in parallel to the normal table, so
       that we get the same parallel-symbol when unmarshalling
       code. We use a counter, which works fine for modules and
       marshalling, because symbols need to be distinct only within
       the module. For the top level, it's possible that symbols from
       different compilations collide. */
    if (!best_match) {
      char buf[50];
      int len;

      tl_id_counter++;
      len = SCHEME_SYM_LEN(sym);
      if (len > 25)
	len = 25;
      memcpy(buf, SCHEME_SYM_VAL(sym), len);
      sprintf(buf + len, "%d", tl_id_counter);

      best_match = scheme_intern_exact_parallel_symbol(buf, strlen(buf));
    }
    a = scheme_make_pair(marks, best_match);
    map = scheme_make_pair(a, map);
    
    scheme_hash_set(marked_names, sym, map);
  }

  return best_match;
}

static Scheme_Object *make_uid()
{
  char name[20];

  sprintf(name, "env%d", env_uid_counter++);
  return scheme_make_symbol(name); /* uninterned! */
}

static Scheme_Object *env_frame_uid(Scheme_Comp_Env *env)
{
  if (env->flags & (SCHEME_NO_RENAME | SCHEME_CAPTURE_WITHOUT_RENAME))
    return NULL;

  if (!env->uid) {
    Scheme_Object *sym;
    sym = make_uid();
    env->uid = sym;
  }
  return env->uid;
}

static void make_env_renames(Scheme_Comp_Env *env, int rcount, int rstart, int rstart_sec, int force_multi)
{
  Scheme_Object *rnm;
  Scheme_Object *uid = NULL;
  int i, pos;

  env_frame_uid(env);

  if (force_multi) {
    if (env->num_bindings && !env->uids) {
      Scheme_Object **uids;
      uids = MALLOC_N(Scheme_Object *, env->num_bindings);
      env->uids = uids;
    }
    if (COMPILE_DATA(env)->num_const && !COMPILE_DATA(env)->const_uids) {
      Scheme_Object **cuids;
      cuids = MALLOC_N(Scheme_Object *, COMPILE_DATA(env)->num_const);
      COMPILE_DATA(env)->const_uids = cuids;
    }
    if (env->uid && !SCHEME_FALSEP(env->uid)) {
      uid = env->uid;
      env->uid = scheme_false;
    }
  }

  if (!uid) {
    if (env->uid && SCHEME_TRUEP(env->uid)) {
      /* single-uid mode (at least for now) */
      uid = env->uid;
    } else {
      /* multi-uid mode */
      if (!rstart_sec)
	uid = COMPILE_DATA(env)->const_uids[rstart];
      else
	uid = env->uids[rstart];
      if (!uid)
	uid = make_uid();
    }
  }
  
  rnm = scheme_make_rename(uid, rcount);
  pos = 0;

  if (!rstart_sec) {
    for (i = rstart; (i < COMPILE_DATA(env)->num_const) && (pos < rcount); i++, pos++) {
      if (COMPILE_DATA(env)->const_uids)
	COMPILE_DATA(env)->const_uids[i] = uid;
      scheme_set_rename(rnm, pos, COMPILE_DATA(env)->const_names[i]);
    }
    rstart = 0;
  }
  for (i = rstart; pos < rcount; i++, pos++) {
    if (env->uids)
      env->uids[i] = uid;
    scheme_set_rename(rnm, pos, env->values[i]);
  }
  
  if (env->renames) {
    if (SCHEME_PAIRP(env->renames) || SCHEME_NULLP(env->renames))
      rnm = scheme_make_pair(rnm, env->renames);
    else
      rnm = scheme_make_pair(rnm, scheme_make_pair(env->renames, scheme_null));
  }
  env->renames = rnm;
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
      int i, count;
      
      /* How many slots filled in the frame so far?  This can change
	 due to the style of let* compilation, which generates a
	 rename record after each binding set. The "const" bindings
	 are always all in place before we generate any renames. */
      count = COMPILE_DATA(env)->num_const;
      for (i = env->num_bindings; i--; ) {
	if (env->values[i])
	  count++;
      }
      
      if (count) {
	Scheme_Object *l;

	if (!env->renames || (env->rename_var_count != count)) {
	  /* Need to create lexical renaming record(s). We create
	     multiple records as necessary to avoid uids that contain
	     more than one variable with the same symbol name.

	     This is complicated, because we don't want to allocate a
	     hash table in the common case of a binding set with a few
	     names. It's also complicated by incremental rename
	     building: if env->rename_var_count is not zero, we've
	     done this before for a subset of `values' (and there are
	     no consts in that case.) In the incremental case, we have
	     a dup_check hash table left from the previous round. */
	  Scheme_Hash_Table *ht;
	  Scheme_Object *name;
	  int rcount = 0, rstart, rstart_sec = 0, vstart;
	  
	  /* rstart is where the to-be-created rename table starts
	     (saved from last time around, or initially zero).
	     vstart is where we start looking for new dups.
	     rstart_sec is TRUE when we need a different uid for
	     each frame. */
	  rstart = env->rename_rstart;
	  if (env->renames) {
	    /* Incremental mode. Drop the most recent (first) rename
               table, because we'll recreate it: */
	    if (SCHEME_PAIRP(env->renames))
	      env->renames = SCHEME_CDR(env->renames);
	    else
	      env->renames = NULL;
	    /* We already know that the first rename_var_count
	       are distinct (from the last iteration) */
	    vstart = env->rename_var_count;
	    rcount = vstart - rstart;
	  } else
	    vstart = 0;

	  /* Create or find the hash table: */
	  if (env->dup_check)
	    ht = env->dup_check;
	  else if (env->num_bindings + COMPILE_DATA(env)->num_const > 10)
	    ht = scheme_make_hash_table(SCHEME_hash_ptr);
	  else
	    ht = NULL;

	  /* Check for dups among the statics, and build a rename for
             each dup-free set. */

	  /* First: constants. */
	  if (COMPILE_DATA(env)->num_const) {
	    /* Start at the beginning, since this can't be incremental. */
	    for (i = 0; i < COMPILE_DATA(env)->num_const; i++) {
	      int found = 0;
	      name = SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[i]);
	      if (ht) {
		if (scheme_hash_get(ht, name))
		  found = 1;
		else
		  scheme_hash_set(ht, name, scheme_true);
	      } else {
		int j;
		for (j = rstart; j < i; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[j]))) {
		    found = 1;
		    break;
		  }
		}
	      }

	      if (found) {
		make_env_renames(env, rcount, rstart, rstart_sec, 1);
		rcount = 1;
		rstart = i;
		if (ht) {
		  /* Flush the table for a new set: */
		  ht = scheme_make_hash_table(SCHEME_hash_ptr);
		  scheme_hash_set(ht, name, scheme_true);
		}
	      } else
		rcount++;
	    }
	  } else 
	    rstart_sec = 1;

	  for (i = vstart; (i < env->num_bindings) && env->values[i]; i++) {
	    int found = 0;
	    name = SCHEME_STX_VAL(env->values[i]);

	    if (ht) {
	      if (scheme_hash_get(ht, name))
		found = 1;
	      else
		scheme_hash_set(ht, name, scheme_true);
	    } else {
	      int j;
	      if (!rstart_sec) {
		/* Look in consts, first: */
		for (j = rstart; j < COMPILE_DATA(env)->num_const; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(COMPILE_DATA(env)->const_names[j]))) {
		    found = 1;
		    break;
		  }
		}

		j = 0;
	      } else
		j = rstart;

	      if (!found) {
		for (; j < i; j++) {
		  if (SAME_OBJ(name, SCHEME_STX_VAL(env->values[j]))) {
		    found = 1;
		    break;
		  }
		}
	      }
	    }

	    if (found) {
	      make_env_renames(env, rcount, rstart, rstart_sec, 1);
	      rcount = 1;
	      rstart = i;
	      rstart_sec = 1;
	      if (ht) {
		/* Flush the table for a new set: */
		ht = scheme_make_hash_table(SCHEME_hash_ptr);
		scheme_hash_set(ht, name, scheme_true);
	      }
	    } else
	      rcount++;
	  }
	  
	  make_env_renames(env, rcount, rstart, rstart_sec, 0);

	  env->rename_var_count = count;
	  env->rename_rstart = rstart;
	  if (count < env->num_bindings) {
	    /* save for next time around: */
	    env->dup_check = ht;
	  } else { 
	    /* drop a saved table if there; we're done with all increments */
	    env->dup_check = NULL;
	  }
	}
	
	for (l = env->renames; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  stx = scheme_add_rename(stx, SCHEME_CAR(l));
	}
	if (!SCHEME_NULLP(l))
	  stx = scheme_add_rename(stx, l);
      }
    }

    env = env->next;
  }

  return stx;
}

/*********************************************************************/
/* 

   scheme_lookup_binding() is the main resolver of lexical, module,
   and top-level bindings. Depending on the value of `flags', it can
   return a value whose type tag is:

     scheme_macro_type (id was bound to syntax),

     scheme_macro_set_type (id was bound to a set!-transformer),

     scheme_macro_id_type (id was bound to a rename-transformer),

     scheme_local_type (id was lexical),

     scheme_variable_type (id is a global or module-bound variable),
     or

     scheme_module_variable_type (id is a module-boundvariable).

*/

Scheme_Object *
scheme_lookup_binding(Scheme_Object *find_id, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int j = 0, p = 0, modpos, skip_stops = 0;
  Scheme_Bucket *b;
  Scheme_Object *val, *modidx, *modname, *src_find_id, *find_global_id;
  Scheme_Env *genv;
  long phase;

  /* Need to know the phase being compiled */
  phase = env->genv->phase;
  
  /* Walk through the compilation frames */
  frame = env;
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;
    Scheme_Object *uid;

    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;      
    
    if (!skip_stops || !(frame->flags & SCHEME_FOR_STOPS)) {
      if (frame->flags & SCHEME_FOR_STOPS)
	skip_stops = 1;

      uid = env_frame_uid(frame);

      for (i = frame->num_bindings; i--; ) {
	if (frame->values[i]) {
	  if (frame->uids) uid = frame->uids[i];
	  if (SAME_OBJ(SCHEME_STX_VAL(find_id), SCHEME_STX_VAL(frame->values[i]))
	      && scheme_stx_env_bound_eq(find_id, frame->values[i], uid, phase)) {
	    /* Found a lambda- or let-bound variable: */
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
	  issame = scheme_stx_module_eq(find_id, COMPILE_DATA(frame)->const_names[i], phase);
	else {
	  if (COMPILE_DATA(frame)->const_uids) uid = COMPILE_DATA(frame)->const_uids[i];
	  issame = (SAME_OBJ(SCHEME_STX_VAL(find_id), 
			     SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))
		    && scheme_stx_env_bound_eq(find_id, COMPILE_DATA(frame)->const_names[i], uid, phase));
	}
      
	if (issame) {
	  val = COMPILE_DATA(frame)->const_vals[i];
	
	  if (!val) {
	    scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
				"variable used out of context");
	    return NULL;
	  }
	  if (!(flags & SCHEME_ENV_CONSTANTS_OK)) {
	    if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type))
	      return val;
	    else if (SAME_TYPE(SCHEME_TYPE(val), scheme_lazy_macro_type))
	      return force_lazy_macro(val, phase);
	    else
	      scheme_wrong_syntax(scheme_set_stx_string, NULL, find_id,
				  "local syntax identifier cannot be mutated");
	    return NULL;
	  }

	  return val;
	}
      }
    }

    p += frame->num_bindings;
  }

  src_find_id = find_id;
  modidx = scheme_stx_module_name(&find_id, phase, NULL, NULL);
  
  /* Used out of context? */
  if (SAME_OBJ(modidx, scheme_undefined)) {
    if (!(flags & SCHEME_OUT_OF_CONTEXT_OK))
      scheme_wrong_syntax(scheme_compile_stx_string, NULL, find_id,
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
	  scheme_wrong_syntax("require", NULL, src_find_id, 
			      "broken compiled code (phase %d): cannot find module %S",
			      env->genv->phase, modname);
	  return NULL;
	}
      }
    }
  } else {
    genv = env->genv;
    modname = NULL;

    if (genv->module && !genv->rename) {
      /* Free variable. Maybe don't continue. */
      if (flags & SCHEME_SETTING) {
	scheme_wrong_syntax(scheme_set_stx_string, NULL, src_find_id, "unbound variable in module");
	return NULL;
      }
      if (flags & SCHEME_NULL_FOR_UNBOUND)
	return NULL;
    }
  }

  if (!modname && SCHEME_STXP(find_id))
    find_global_id = scheme_tl_id_sym(env->genv, find_id, 0);
  else
    find_global_id = find_id;

  /* Try syntax table: */
  if (modname)
    val = scheme_module_syntax(modname, env->genv, find_id);
  else {
    /* Only try syntax table if there's not an explicit (later)
       variable mapping: */
    if (genv->shadowed_syntax 
	&& scheme_hash_get(genv->shadowed_syntax, find_global_id))
      val = NULL;
    else
      val = scheme_lookup_in_table(genv->syntax, (const char *)find_global_id);
  }
  
  if (val) {
    if (SAME_TYPE(SCHEME_TYPE(val), scheme_lazy_macro_type))
      return force_lazy_macro(val, phase);
    return val;
  }

  if (modname) {
    Scheme_Object *pos;
    pos = scheme_check_accessible_in_module(genv, find_id, src_find_id, -1, 1);
    modpos = SCHEME_INT_VAL(pos);
  } else
    modpos = -1;

  if (modname && (flags & SCHEME_SETTING)) {
    if (SAME_OBJ(src_find_id, find_id) || SAME_OBJ(SCHEME_STX_SYM(src_find_id), find_id))
      find_id = NULL;
    scheme_wrong_syntax(scheme_set_stx_string, find_id, src_find_id, "cannot mutate module-required variable");
    return NULL;
  }

  if (!modname && (flags & SCHEME_SETTING) && (genv->module && !genv->rename)) {
    /* Check for set! of unbound variable: */
    
    if (!scheme_lookup_in_table(genv->toplevel, (const char *)find_global_id)) {
      scheme_wrong_syntax(scheme_set_stx_string, NULL, src_find_id, "unbound variable in module");
      return NULL;
    }
  }

  if (!modname && (flags & SCHEME_NULL_FOR_UNBOUND))
    return NULL;

  /* Used to have `&& !SAME_OBJ(modidx, modname)' below, but that was a bad
     idea, because it causes module instances to be preserved. */
  if (modname && !(flags & SCHEME_RESOLVE_MODIDS) && !SAME_OBJ(modidx, kernel_symbol)) {
    /* Create a module variable reference, so that idx is preserved: */
    return scheme_hash_module_variable(env->genv, modidx, find_id, modpos);
  }

  if (!modname && (flags & SCHEME_SETTING) && genv->module) {
    /* Need to return a variable reference in this case, too. */
    return scheme_hash_module_variable(env->genv, genv->module->self_modidx, find_global_id, modpos);
  }

  b = scheme_bucket_from_table(genv->toplevel, (char *)find_global_id);

  if ((flags & SCHEME_ELIM_CONST) && b && b->val 
      && (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST)
      && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE))
    return (Scheme_Object *)b->val;

  if (!((Scheme_Bucket_With_Home *)b)->home)
    ((Scheme_Bucket_With_Home *)b)->home = genv;
  
  return (Scheme_Object *)b;
}

void scheme_env_make_closure_map(Scheme_Comp_Env *env, mzshort *_size, mzshort **_map)
{
  /* A closure map lists the captured variables for a closure; the
     indices are resolved two new indicies in the second phase of
     compilation. */
  Compile_Data *data;
  Scheme_Comp_Env *frame;
  int i, j, pos = 0, lpos = 0;
  mzshort *map, size;

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
  map = MALLOC_N_ATOMIC(mzshort, size);
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
	    data->stat_dists[i][j - 1] = 1; /* ... but ensure previous keeps */
	  }
	}
	lpos++;
      }
    } else
      lpos += frame->num_bindings;
  }
}

void scheme_env_make_stx_closure_map(Scheme_Comp_Env *frame, mzshort *size, mzshort **_map)
{
  char *used;

  used = COMPILE_DATA(frame)->stxes_used;

  if (used) {
    mzshort *map;
    int i, max_stx_used, count = 0;
    
    max_stx_used = COMPILE_DATA(frame)->max_stx_used;
    
    for (i = 0; i < max_stx_used; i++) {
      if (used[i])
	count++;
    }

    *size = count;
    map = MALLOC_N_ATOMIC(mzshort, count);
    *_map = map;

    count = 0;
    for (i = 0; i < max_stx_used; i++) {
      if (used[i])
	map[count++] = i;
    }

    /* Propagate uses to an enclosing lambda, if any: */
    frame = frame->next;
    while (frame) {
      if (frame->flags & SCHEME_LAMBDA_FRAME) {
	if (COMPILE_DATA(frame)->max_stx_used < max_stx_used) {
	  char *p;

	  p = MALLOC_N_ATOMIC(char, max_stx_used);
	  memset(p, 0, max_stx_used);
	  memcpy(p, COMPILE_DATA(frame)->stxes_used, 
		 COMPILE_DATA(frame)->max_stx_used);
	  COMPILE_DATA(frame)->stxes_used = p;
	  COMPILE_DATA(frame)->max_stx_used = max_stx_used;
	}

	for (i = 0; i < max_stx_used; i++) {
	  if (used[i])
	    COMPILE_DATA(frame)->stxes_used[i] = 1;
	}

	break;
      }
      frame = frame->next;
    }
  } else {
    *size = 0;
    *_map = NULL;
  }
}

int scheme_env_uses_toplevel(Scheme_Comp_Env *frame)
{
  int used;

  used = COMPILE_DATA(frame)->used_toplevel;
  
  if (used) {
    /* Propagate use to an enclosing lambda, if any: */
    frame = frame->next;
    while (frame) {
      if (frame->flags & SCHEME_LAMBDA_FRAME) {
	COMPILE_DATA(frame)->used_toplevel = 1;
	break;
      }
      frame = frame->next;
    }
  }

  return used;
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

/*========================================================================*/
/*                          syntax-checking utils                         */
/*========================================================================*/

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
      ht = scheme_make_hash_table(SCHEME_hash_bound_id);
      r->ht = ht;
      for (i = 0; i < r->count; i++) {
	scheme_hash_set(ht, r->syms[i], scheme_true);
      }
      r->count++;
    }
  }

  if (scheme_hash_get(r->ht, symbol)) {
    scheme_wrong_syntax(where, symbol, form,
			"duplicate %s name", what);
  }

  scheme_hash_set(r->ht, symbol, scheme_true);
}

void scheme_check_context(Scheme_Env *env, Scheme_Object *name, Scheme_Object *form, Scheme_Object *ok_modidx)
{
  Scheme_Object *mod, *id = name;
  int bad = 0;

  mod = scheme_stx_source_module(id, 0);

  if (mod && SCHEME_TRUEP(mod) && NOT_SAME_OBJ(ok_modidx, mod)) {
    bad = 1;
  } else {
    mod = scheme_stx_module_name(&id, env->phase, NULL, NULL);
    if (SAME_OBJ(mod, scheme_undefined))
      bad = 2;
  }

  if (bad) {
    scheme_wrong_syntax(NULL, name, form, 
			"identifier for a %s definition already has a %s%s context",
			ok_modidx ? "module-body" : "top-level",
			(ok_modidx && (bad == 1)) ? "different " : "",
			(bad == 2) ? "lexical" : "module");
  }
}

/*========================================================================*/
/*             compile-time env for phase 2 ("resolve")                   */
/*========================================================================*/

/* See eval.c for information about the compilation phases. */

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, int simplify)
{
  Resolve_Prefix *rp;
  Scheme_Object **tls, **stxes, *simplify_cache;
  Scheme_Hash_Table *ht;
  int i;

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->type = scheme_resolve_prefix_type;
  rp->num_toplevels = cp->num_toplevels;
  rp->num_stxes = cp->num_stxes;
  
  if (rp->num_toplevels)
    tls = MALLOC_N(Scheme_Object*, rp->num_toplevels);
  else
    tls = NULL;
  if (rp->num_stxes)
    stxes = MALLOC_N(Scheme_Object*, rp->num_stxes);
  else
    stxes = NULL;

  rp->toplevels = tls;
  rp->stxes = stxes;

  ht = cp->toplevels;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
	tls[SCHEME_TOPLEVEL_POS(ht->vals[i])] = ht->keys[i];
      }
    }
  }

  if (simplify)
    simplify_cache = scheme_new_stx_simplify_cache();
  else
    simplify_cache = NULL;  

  ht = cp->stxes;
  if (ht) {
    for (i = 0; i < ht->size; i++) {
      if (ht->vals[i]) {
	scheme_simplify_stx(ht->keys[i], simplify_cache);
	stxes[SCHEME_LOCAL_POS(ht->vals[i])] = ht->keys[i];
      }
    }
  }

  return rp;
}

Resolve_Info *scheme_resolve_info_create(Resolve_Prefix *rp)
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = rp;
  naya->count = 0;
  naya->next = NULL;
  naya->toplevel_pos = -1;

  return naya;
}

Resolve_Info *scheme_resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapc, int stxc)
     /* size = number of appended items in run-time frame */
     /* oldisze = number of appended items in original compile-time frame */
     /* mapc = mappings that will be installed */
{
  Resolve_Info *naya;

  naya = MALLOC_ONE_RT(Resolve_Info);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_resolve_info;
#endif
  naya->prefix = info->prefix;
  naya->next = info;
  naya->size = size;
  naya->oldsize = oldsize;
  naya->count = mapc;
  naya->pos = 0;
  naya->stx_count = stxc;
  naya->toplevel_pos = -1;

  if (mapc) {
    int i, *ia;
    mzshort *sa;

    sa = MALLOC_N_ATOMIC(mzshort, mapc);
    naya->old_pos = sa;
    sa = MALLOC_N_ATOMIC(mzshort, mapc);
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

  if (stxc) {
    mzshort *sa;

    sa = MALLOC_N_ATOMIC(mzshort, stxc);
    naya->old_stx_pos = sa;
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

void scheme_resolve_info_add_stx_mapping(Resolve_Info *info, int oldp, int newp)
{
  info->old_stx_pos[newp] = oldp;
}

void scheme_resolve_info_set_toplevel_pos(Resolve_Info *info, int pos)
{
  info->toplevel_pos = pos;
}

static int resolve_info_lookup(Resolve_Info *info, int pos, int *flags)
{
  int i, offset = 0, orig = pos;

  while (info) {
    for (i = info->pos; i--; ) {
      int oldp = info->old_pos[i];
      if (pos == oldp) {
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

int scheme_resolve_toplevel_pos(Resolve_Info *info)
{
  int pos = 0;

  while (info && (info->toplevel_pos < 0)) {
    pos += info->size;
    info = info->next;
  }

  if (!info)
    return pos;
  else
    return info->toplevel_pos + pos;
}

Scheme_Object *scheme_resolve_toplevel(Resolve_Info *info, Scheme_Object *expr)
{
  int skip;

  skip = scheme_resolve_toplevel_pos(info);

  return make_toplevel(skip + SCHEME_TOPLEVEL_DEPTH(expr), /* depth is 0 (normal) or 1 (exp-time) */
		       SCHEME_TOPLEVEL_POS(expr),
		       1);
}

int scheme_resolve_quote_syntax(Resolve_Info *info, int oldpos)
{
  Resolve_Info *start = info;
  int skip = 0;

  while (info) {
    if (info->old_stx_pos) {
      int i;
      for (i = 0; i < info->stx_count; i++) {
	if (info->old_stx_pos[i] == oldpos)
	  return (info->count - info->size) + ((info->toplevel_pos >= 0) ? 1 : 0) + i + skip;
      }
      scheme_signal_error("internal error: didn't find an stx pos");
      return 0;
    } else {
      skip += info->size;
      info = info->next;
    }
  }

  if (start->prefix->num_toplevels)
    skip += 1;
  
  return skip + oldpos;
}

/*========================================================================*/
/*                             run-time "stack"                           */
/*========================================================================*/

Scheme_Object *scheme_make_envunbox(Scheme_Object *value)
{
  Scheme_Object *obj;

  obj = (Scheme_Object *)scheme_malloc_envunbox(sizeof(Scheme_Object*));
  SCHEME_ENVBOX_VAL(obj) = value;

  return obj;
}

/*========================================================================*/
/*             run-time and expansion-time Scheme interface               */
/*========================================================================*/

static Scheme_Object *
namespace_identifier(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj;
  Scheme_Env *genv;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-symbol->identifier", "symbol", 0, argc, argv);
  if ((argc > 1) && !SCHEME_NAMESPACEP(argv[1]))
    scheme_wrong_type("namespace-symbol->identifier", "namespace", 1, argc, argv);

  if (argc > 1)
    genv = (Scheme_Env *)argv[1];
  else
    genv = scheme_get_env(scheme_config);

  obj = argv[0];
  obj = scheme_datum_to_syntax(obj, scheme_false, scheme_false, 1, 0);

  /* Renamings: */
  if (genv->rename)
    obj = scheme_add_rename(obj, genv->rename);
  if (genv->exp_env && genv->exp_env->rename)
    obj = scheme_add_rename(obj, genv->exp_env->rename);

  return obj;
}

static Scheme_Object *
namespace_variable_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *id = NULL;
  Scheme_Env *genv;
  int use_map;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-variable-value", "symbol", 0, argc, argv);
  use_map = ((argc > 0) ? SCHEME_TRUEP(argv[1]) : 1);
  if ((argc > 2) && SCHEME_TRUEP(argv[2])
      && !scheme_check_proc_arity(NULL, 0, 2, argc, argv))
    scheme_wrong_type("namespace-variable-value", "procedure (arity 0) or #f", 1, argc, argv);
  if ((argc > 3) && !SCHEME_NAMESPACEP(argv[3]))
    scheme_wrong_type("namespace-variable-value", "namespace", 3, argc, argv);

  if (argc > 3)
    genv = (Scheme_Env *)argv[3];
  else
    genv = scheme_get_env(scheme_config);

  if (!use_map)
    v = scheme_lookup_global(argv[0], genv);
  else {
    Scheme_Full_Comp_Env inlined_e;

    id = scheme_make_renamed_stx(argv[0], genv->rename);

    inlined_e.base.num_bindings = 0;
    inlined_e.base.next = NULL;
    inlined_e.base.genv = genv;
    inlined_e.base.flags = SCHEME_TOPLEVEL_FRAME;
    init_compile_data((Scheme_Comp_Env *)&inlined_e);
    inlined_e.base.prefix = NULL;

    v = scheme_lookup_binding(id, (Scheme_Comp_Env *)&inlined_e, SCHEME_RESOLVE_MODIDS);
    if (v) {
      if (!SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type)) {
	use_map = -1;
	v = NULL;
      } else
	v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    }
  }
  
  if (!v) {
    if ((argc > 2) && SCHEME_TRUEP(argv[2]))
      return _scheme_tail_apply(argv[2], 0, NULL);
    else if (use_map == -1) {
      scheme_wrong_syntax("namespace-variable-value", NULL, id, "bound to syntax");
      return NULL;
    } else {
      scheme_raise_exn(MZEXN_VARIABLE, argv[0],
		       "namespace-variable-value: %S is not defined",
		       argv[0]);
      return NULL;
    }
  }

  return v;
}

static Scheme_Object *
namespace_set_variable_value(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;
  Scheme_Bucket *bucket;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-set-variable-value!", "symbol", 0, argc, argv);
  if ((argc > 3) && !SCHEME_NAMESPACEP(argv[3]))
    scheme_wrong_type("namespace-set-variable-value!", "namespace", 3, argc, argv);

  if (argc > 3)
    env = (Scheme_Env *)argv[3];
  else
    env = scheme_get_env(scheme_config);

  bucket = scheme_global_bucket(argv[0], env);
  
  scheme_set_global_bucket("namespace-set-variable-value!", bucket, argv[1], 1);
  
  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    scheme_shadow(env, argv[0], 1);
  }

  return scheme_void;
}

static Scheme_Object *
namespace_undefine_variable(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env;
  Scheme_Bucket *bucket;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("namespace-undefine-variable!", "symbol", 0, argc, argv);
  if ((argc > 1) && !SCHEME_NAMESPACEP(argv[1]))
    scheme_wrong_type("namespace-undefine-variable!", "namespace", 1, argc, argv);

  if (argc > 1)
    env = (Scheme_Env *)argv[1];
  else
    env = scheme_get_env(scheme_config);

  if (scheme_lookup_global(argv[0], env)) {
    bucket = scheme_global_bucket(argv[0], env);
    bucket->val = NULL;
  } else {
    scheme_raise_exn(MZEXN_VARIABLE, argv[0],
		     "namespace-undefine-variable!: %S is not defined",
		     argv[0]);
  }

  return scheme_void;
}

static Scheme_Object *
namespace_mapped_symbols(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l;
  Scheme_Env *env;
  Scheme_Hash_Table *mapped;
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  int i, j;

  if ((argc > 0) && !SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-mapped-symbols", "namespace", 0, argc, argv);

  if (argc)
    env = (Scheme_Env *)argv[0];
  else
    env = scheme_get_env(scheme_config);
  
  mapped = scheme_make_hash_table(SCHEME_hash_ptr);

  for (j = 0; j < 2; j++) {
    if (j)
      ht = env->syntax;
    else
      ht = env->toplevel;

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val) {
	scheme_hash_set(mapped, (Scheme_Object *)b->key, scheme_true);
      }
    }
  }

  if (env->rename)
    scheme_list_module_rename(env->rename, mapped);

  l = scheme_null;
  for (i = mapped->size; i--; ) {
    if (mapped->vals[i])
      l = scheme_make_pair(mapped->keys[i], l);
  }

  return l;
}

static Scheme_Object *namespace_module_registry(int argc, Scheme_Object **argv)
{
  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-module-registry", "namespace", 0, argc, argv);

  return (Scheme_Object *)((Scheme_Env *)argv[0])->module_registry;
}

static Scheme_Object *
local_exp_time_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *sym;
  Scheme_Comp_Env *env;
  int renamed = 0;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-value: not currently transforming");

  sym = argv[0];

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_type("syntax-local-value", "syntax identifier", 0, argc, argv);

  if (argc > 1)
    scheme_check_proc_arity("syntax-local-value", 0, 1, argc, argv);

  if (scheme_current_thread->current_local_mark)
    sym = scheme_add_remove_mark(sym, scheme_current_thread->current_local_mark);

  while (1) {
    v = scheme_lookup_binding(sym, env,
			      (SCHEME_NULL_FOR_UNBOUND
			       + SCHEME_RESOLVE_MODIDS
			       + SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + SCHEME_OUT_OF_CONTEXT_OK + SCHEME_ELIM_CONST));
    
    /* Deref globals */
    if (v && SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
      v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;
    
    if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
      if ((argc > 1) && SCHEME_TRUEP(argv[1]))
	return _scheme_tail_apply(argv[1], 0, NULL);
      else
	scheme_arg_mismatch("syntax-local-value",
			    (renamed 
			     ? "not defined as syntax (after renaming): "
			     : "not defined as syntax: "),
			    argv[0]);
    }
    
    v = SCHEME_PTR_VAL(v);
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_id_macro_type)) {
      sym = SCHEME_PTR1_VAL(v);
      renamed = 1;
      SCHEME_USE_FUEL(1);
    } else
      return v;
  }
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

  if (env->flags & SCHEME_INTDEF_FRAME) {
    if (!env->intdef_name) {
      Scheme_Object *sym, *pr, *prev = NULL;
      Scheme_Comp_Env *lenv = env;
      char buf[22];
      while (1) {
	sprintf(buf, "internal-define%d", intdef_counter++);
	sym = scheme_make_symbol(buf); /* uninterned! */
	pr = scheme_make_immutable_pair(sym, scheme_null);
	lenv->intdef_name = pr;
	if (prev)
	  SCHEME_CDR(prev) = pr;
	if (lenv->next->flags & SCHEME_INTDEF_FRAME) {
	  if (lenv->next->intdef_name) {
	    SCHEME_CDR(pr) = lenv->next->intdef_name;
	    break;
	  } else {
	    prev = pr;
	    lenv = lenv->next;
	    /* Go again to continue building the list */
	  }
	} else
	  break;
      }
    }
    return env->intdef_name;
  } else if (scheme_is_module_env(env))
    return scheme_intern_symbol("module");
  else if (scheme_is_toplevel(env))
    return scheme_intern_symbol("top-level");
  else
    return scheme_intern_symbol("expression");
}

static Scheme_Object *
local_introduce(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *s;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-introduce: not currently transforming");

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("syntax-local-introduce", "syntax", 0, argc, argv);

  if (scheme_current_thread->current_local_mark)
    s = scheme_add_remove_mark(s, scheme_current_thread->current_local_mark);

  return s;
}

static Scheme_Object *
local_module_introduce(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;
  Scheme_Object *s, *v;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-module-introduce: not currently transforming");

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("syntax-local-module-introduce", "syntax", 0, argc, argv);

  if (env->genv->module) {
    if (env->genv->module->rn_stx && !SAME_OBJ(scheme_true, env->genv->module->rn_stx)) {
      v = scheme_stx_to_rename(env->genv->module->rn_stx);
      s = scheme_add_rename(s, v);
    }
    if (env->genv->module->et_rn_stx && !SAME_OBJ(scheme_true, env->genv->module->et_rn_stx)) {
      v = scheme_stx_to_rename(env->genv->module->et_rn_stx);
      s = scheme_add_rename(s, v);
    }
  }

  return s;
}

static Scheme_Object *
local_get_shadower(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env, *frame;
  Scheme_Object *sym, *esym, *sym_marks = NULL, *orig_sym, *uid = NULL, *env_marks;

  env = scheme_current_thread->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC, 
		     "syntax-local-get-shadower: not currently transforming");

  sym = argv[0];
  orig_sym = sym;

  if (!(SCHEME_STXP(sym) && SCHEME_SYMBOLP(SCHEME_STX_VAL(sym))))
    scheme_wrong_type("syntax-local-get-shadower", "syntax identifier", 0, argc, argv);

  sym_marks = scheme_stx_extract_marks(sym);

  /* Walk backward through the frames, looking for a renaming binding
     with the same marks as the given identifier, sym. When we find
     it, rename the given identifier so that it matches frame */
  for (frame = env; frame->next != NULL; frame = frame->next) {
    int i;

    for (i = frame->num_bindings; i--; ) {
      if (frame->values[i]) {
	if (SAME_OBJ(SCHEME_STX_VAL(sym), SCHEME_STX_VAL(frame->values[i])))  {
	  esym = frame->values[i];
	  env_marks = scheme_stx_extract_marks(esym);
	  if (scheme_equal(env_marks, sym_marks)) {
	    sym = esym;
	    if (frame->uids)
	      uid = frame->uids[i];
	    else
	      uid = frame->uid;
	    break;
	  }
	}
      }
    }
    if (uid)
      break;

    for (i = COMPILE_DATA(frame)->num_const; i--; ) {
      if (!(frame->flags & SCHEME_CAPTURE_WITHOUT_RENAME)) {
	if (SAME_OBJ(SCHEME_STX_VAL(sym), 
		     SCHEME_STX_VAL(COMPILE_DATA(frame)->const_names[i]))) {
	  esym = COMPILE_DATA(frame)->const_names[i];
	  env_marks = scheme_stx_extract_marks(esym);
	  if (scheme_equal(env_marks, sym_marks)) {
	    sym = esym;
	    if (COMPILE_DATA(frame)->const_uids)
	      uid = COMPILE_DATA(frame)->const_uids[i];
	    else
	      uid = frame->uid;
	    break;
	  }
	}
      }
    }
    if (uid)
      break;
  }

  if (!uid) {
    /* No lexical shadower, but strip module context and mark barriers, if any. */
    sym = scheme_stx_strip_module_context(sym);
    /* Add current module context, if any. */
    sym = local_module_introduce(1, &sym);
    return sym;
  }

  {
    Scheme_Object *rn, *result;

    result = scheme_datum_to_syntax(SCHEME_STX_VAL(sym), orig_sym, sym, 0, 0);
    ((Scheme_Stx *)result)->props = ((Scheme_Stx *)orig_sym)->props;
    
    rn = scheme_make_rename(uid, 1);
    scheme_set_rename(rn, 0, result);

    return scheme_add_rename(result, rn);
  }
}

static Scheme_Object *
introducer_proc(void *mark, int argc, Scheme_Object *argv[])
{
  Scheme_Object *s;

  s = argv[0];
  if (!SCHEME_STXP(s))
    scheme_wrong_type("syntax-introducer", "syntax", 0, argc, argv);

  return scheme_add_remove_mark(s, (Scheme_Object *)mark);
}

static Scheme_Object *
make_introducer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *mark;

  mark = scheme_new_mark();

  return scheme_make_closed_prim_w_arity(introducer_proc, mark,
					 "syntax-introducer", 1, 1);
}

static Scheme_Object *
make_set_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  scheme_check_proc_arity("make-set!-transformer", 1, 0, argc, argv);

  v = scheme_alloc_small_object();
  v->type = scheme_set_macro_type;
  SCHEME_PTR_VAL(v) = argv[0];

  return v;
}

static Scheme_Object *
set_transformer_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_set_macro_type))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
set_transformer_proc(int argc, Scheme_Object *argv[])
{
  if (!(SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_set_macro_type)))
    scheme_wrong_type("set!-transformer-procedure", "set!-transformer", 1, argc, argv);

  return SCHEME_PTR_VAL(argv[0]);
}

static Scheme_Object *
make_rename_transformer(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_SYMBOLP(SCHEME_STX_VAL(argv[0])))
    scheme_wrong_type("make-rename-transformer", "syntax identifier", 0, argc, argv);

  v = scheme_alloc_small_object();
  v->type = scheme_id_macro_type;
  SCHEME_PTR_VAL(v) = argv[0];

  return v;
}

static Scheme_Object *
rename_transformer_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_id_macro_type))
	  ? scheme_true
	  : scheme_false);
}


/*========================================================================*/
/*                    [un]marshalling variable reference                  */
/*========================================================================*/

static Scheme_Object *write_toplevel(Scheme_Object *obj)
{
  return scheme_make_pair(scheme_make_integer(SCHEME_TOPLEVEL_DEPTH(obj)),
			  scheme_make_integer(SCHEME_TOPLEVEL_POS(obj)));
}

static Scheme_Object *read_toplevel(Scheme_Object *obj)
{
  if (!SCHEME_PAIRP(obj)) return NULL;

  return make_toplevel(SCHEME_INT_VAL(SCHEME_CAR(obj)),
		       SCHEME_INT_VAL(SCHEME_CDR(obj)),
		       1);
}

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

    if (!SCHEME_PAIRP(obj)) return NULL;

    modname = SCHEME_CAR(obj);
    varname = SCHEME_CDR(obj);

    if (SAME_OBJ(modname, kernel_symbol))
      return (Scheme_Object *)scheme_global_bucket(varname, scheme_initial_env);
    else {
      Module_Variable *mv;
      
      mv = MALLOC_ONE_TAGGED(Module_Variable);
      mv->type = scheme_module_variable_type;
      
      mv->modidx = modname;
      mv->sym = varname;
      mv->pos = -1;

      return (Scheme_Object *)mv;
    }
  }

  return (Scheme_Object *)scheme_global_bucket(obj, env);
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

static Scheme_Object *write_resolve_prefix(Scheme_Object *obj)
{
  Resolve_Prefix *rp = (Resolve_Prefix *)obj;
  Scheme_Object *tv, *sv;
  int i;

  i = rp->num_toplevels;
  tv = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(tv)[i] = rp->toplevels[i];
  }

  i = rp->num_stxes;
  sv = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(sv)[i] = rp->stxes[i];
  }

  return scheme_make_pair(tv, sv);
}

static Scheme_Object *read_resolve_prefix(Scheme_Object *obj)
{
  Resolve_Prefix *rp;
  Scheme_Object *tv, *sv, **a;
  int i;

  if (!SCHEME_PAIRP(obj)) return NULL;

  tv = SCHEME_CAR(obj);
  sv = SCHEME_CDR(obj);

  rp = MALLOC_ONE_TAGGED(Resolve_Prefix);
  rp->type = scheme_resolve_prefix_type;
  rp->num_toplevels = SCHEME_VEC_SIZE(tv);
  rp->num_stxes = SCHEME_VEC_SIZE(sv);

  i = rp->num_toplevels;
  a = MALLOC_N(Scheme_Object *, i);
  while (i--) {
    a[i] = SCHEME_VEC_ELS(tv)[i];
  }
  rp->toplevels = a;
  
  i = rp->num_stxes;
  a = MALLOC_N(Scheme_Object *, i);
  while (i--) {
    a[i] = SCHEME_VEC_ELS(sv)[i];
  }
  rp->stxes = a;

  return (Scheme_Object *)rp;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

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
