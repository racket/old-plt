/*
  MzScheme
  Copyright (c) 2000-2001 Matthew Flatt
 
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

/* This file implements the first-order, top-level module system. An
   initiantiated module is implemented essentially as a namespace. The
   bindings at the top level of a module are namespace top-level
   bindings. */

#include "schpriv.h"
#include "schmach.h"

/* globals */
Scheme_Object *scheme_sys_wraps0;
Scheme_Object *scheme_sys_wraps1;

/* locals */
static Scheme_Object *current_module_name_resolver(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_module_name_prefix(int argc, Scheme_Object *argv[]);
static Scheme_Object *dynamic_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *dynamic_require_syntax(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_trans_require(int argc, Scheme_Object *argv[]);
static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_path_index_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *require_for_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *require_for_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *provide_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *provide_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *module_execute(Scheme_Object *data);
static Scheme_Object *top_level_require_execute(Scheme_Object *data);

static Scheme_Object *module_link(Scheme_Object *data, Link_Info *info);
static Scheme_Object *top_level_require_link(Scheme_Object *data, Link_Info *info);

static Scheme_Object *module_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *top_level_require_resolve(Scheme_Object *data, Resolve_Info *info);

static Scheme_Object *write_module(Scheme_Object *obj);
static Scheme_Object *read_module(Scheme_Object *obj);

static Scheme_Module *module_load(Scheme_Object *modname, Scheme_Env *env, const char *who);

static void eval_defmacro(Scheme_Object *names, int count,
			  Scheme_Object *expr, Scheme_Comp_Env *env,
			  int let_depth, Scheme_Bucket_Table *syntax);

#define cons scheme_make_pair

static Scheme_Object *modbeg_syntax;

static Scheme_Object *kernel_symbol;
static Scheme_Module *kernel;

static Scheme_Object *module_symbol;
static Scheme_Object *module_begin_symbol;

static Scheme_Object *prefix_symbol;
static Scheme_Object *rename_symbol;
static Scheme_Object *all_except_symbol;
static Scheme_Object *all_from_symbol;
static Scheme_Object *all_from_except_symbol;
static Scheme_Object *struct_symbol;

static Scheme_Object *begin_stx;
static Scheme_Object *define_values_stx;
static Scheme_Object *define_syntaxes_stx;
static Scheme_Object *require_stx;
static Scheme_Object *require_for_syntax_stx;
static Scheme_Object *provide_stx;
static Scheme_Object *set_stx;
static Scheme_Object *app_stx;
static Scheme_Object *top_stx;

static int num_initial_modules;
static Scheme_Object **initial_modules;
static Scheme_Object *initial_renames;
static Scheme_Bucket_Table *initial_toplevel;

typedef void (*Check_Func)(Scheme_Object *name, Scheme_Object *nominal_modname, 
			   Scheme_Object *modname, Scheme_Object *srcname, 
			   int isval, void *data, Scheme_Object *e);
static Scheme_Object *parse_requires(Scheme_Object *form, Scheme_Object *l, 
				    Scheme_Object *base_modidx,
				    Scheme_Env *env,
				    Scheme_Object *rn,
				    Check_Func ck, void *data,
				    int start, Scheme_Object *redef_modname,
				    int unpack_kern);
static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx);
static void expstart_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx);
static void finish_expstart_module(Scheme_Env *menv, Scheme_Env *env);

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv);

#define MODCHAIN_TABLE(p) ((Scheme_Hash_Table *)(SCHEME_VEC_ELS(p)[0]))

/**********************************************************************/
/*                           initialization                           */
/**********************************************************************/

void scheme_init_module(Scheme_Env *env)
{
  Scheme_Object *o;

  scheme_register_syntax(MODULE_EXPD, module_resolve, 
			 module_link, module_execute, 1);
  scheme_register_syntax(REQUIRE_EXPD, top_level_require_resolve, 
			 top_level_require_link, top_level_require_execute, 1);

  scheme_add_global_keyword("module", 
			    scheme_make_compiled_syntax(module_syntax, 
							module_expand),
			    env);

  REGISTER_SO(modbeg_syntax);
  modbeg_syntax = scheme_make_compiled_syntax(module_begin_syntax, 
					      module_begin_expand);

  scheme_add_global_keyword("#%module-begin", 
			    modbeg_syntax,
			    env);

  scheme_add_global_keyword("require", 
			    scheme_make_compiled_syntax(require_syntax, 
							require_expand), 
			    env);
  scheme_add_global_keyword("require-for-syntax", 
			    scheme_make_compiled_syntax(require_for_syntax_syntax, 
							require_for_syntax_expand), 
			    env);
  scheme_add_global_keyword("provide", 
			    scheme_make_compiled_syntax(provide_syntax, 
							provide_expand), 
			    env);

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  REGISTER_SO(module_symbol);
  REGISTER_SO(module_begin_symbol);
  module_symbol = scheme_intern_symbol("module");
  module_begin_symbol = scheme_intern_symbol("#%module-begin");

  scheme_install_type_writer(scheme_module_type, write_module);
  scheme_install_type_reader(scheme_module_type, read_module);

  o = scheme_make_prim_w_arity(default_module_resolver,
			       "default-module-name-resolver",
			       3, 3);
  scheme_set_param(scheme_config, MZCONFIG_CURRENT_MODULE_RESOLVER, o);

  scheme_set_param(scheme_config, MZCONFIG_CURRENT_MODULE_PREFIX, scheme_false);

  scheme_add_global_constant("current-module-name-resolver", 
			     scheme_register_parameter(current_module_name_resolver, 
						       "current-module-name-resolver",
						       MZCONFIG_CURRENT_MODULE_RESOLVER), 
			     env);
  scheme_add_global_constant("current-module-name-prefix", 
			     scheme_register_parameter(current_module_name_prefix, 
						       "current-module-name-prefix",
						       MZCONFIG_CURRENT_MODULE_PREFIX), 
			     env);

  scheme_add_global_constant("dynamic-require", 
			     scheme_make_prim_w_arity(dynamic_require,
						      "dynamic-require",
						      2, 2),
			     env);
  scheme_add_global_constant("dynamic-require-syntax", 
			     scheme_make_prim_w_arity(dynamic_require_syntax,
						      "dynamic-require-syntax",
						      2, 2),
			     env);

  scheme_add_global_constant("namespace-require",
			     scheme_make_prim_w_arity(namespace_require,
						      "namespace-require",
						      1, 1),
			     env);
  scheme_add_global_constant("namespace-transformer-require",
			     scheme_make_prim_w_arity(namespace_trans_require,
						      "namespace-transformer-require",
						      1, 1),
			     env);
  scheme_add_global_constant("namespace-attach-module",
			     scheme_make_prim_w_arity(namespace_attach_module,
						      "namespace-attach-module",
						      2, 2),
			     env);

  scheme_add_global_constant("module-path-index?",
			     scheme_make_folding_prim(module_path_index_p,
						      "module-path-index?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("module-path-index-split",
			     scheme_make_prim_w_arity2(module_path_index_split,
						       "module-path-index-split",
						       1, 1,
						       2, 2),
			     env);
  scheme_add_global_constant("module-path-index-join",
			     scheme_make_prim_w_arity(module_path_index_join,
						      "module-path-index-join",
						      2, 2),
			     env);
}

void scheme_finish_kernel(Scheme_Env *env)
{
  /* When this function is called, the initial namespace has all the
     primitive bindings for syntax and procedures. This function fills
     in the module wrapper for #%kernel. */
  Scheme_Bucket_Table *ht;
  int i, j, count, syntax_start = 0;
  Scheme_Bucket **bs;
  Scheme_Object **exs, *w, *rn;

  REGISTER_SO(kernel);

  kernel = MALLOC_ONE_TAGGED(Scheme_Module);
  kernel->type = scheme_module_type;
  
  scheme_initial_env->module = kernel;

  kernel->modname = kernel_symbol;
  kernel->requires = scheme_null;
  kernel->et_requires = scheme_null;
  
  /* Provide all syntax and variables: */
  count = 0;
  for (j = 0; j < 2; j++) {
    if (!j)
      ht = scheme_initial_env->toplevel;
    else {
      ht = scheme_initial_env->syntax;
      syntax_start = count;
    }

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val)
	count++;
    }
  }

  exs = MALLOC_N(Scheme_Object *, count);
  count = 0;
  for (j = 0; j < 2; j++) {
    if (!j)
      ht = scheme_initial_env->toplevel;
    else
      ht = scheme_initial_env->syntax;

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val)
	exs[count++] = (Scheme_Object *)b->key;
    }
  }
 
  kernel->provides = exs;
  kernel->provide_srcs = NULL;
  kernel->provide_src_names = exs;
  kernel->num_provides = count;
  kernel->num_var_provides = syntax_start;

  scheme_initial_env->running = 1;

  rn = scheme_make_module_rename(0, 0);
  for (i = kernel->num_provides; i--; ) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i]);
  }

  scheme_sys_wraps(NULL);

  REGISTER_SO(begin_stx);
  REGISTER_SO(define_values_stx);
  REGISTER_SO(define_syntaxes_stx);
  REGISTER_SO(require_stx);
  REGISTER_SO(require_for_syntax_stx);
  REGISTER_SO(provide_stx);
  REGISTER_SO(set_stx);
  REGISTER_SO(app_stx);
  REGISTER_SO(top_stx);

  w = scheme_sys_wraps0;
  begin_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, w, 0, 0);
  define_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, w, 0, 0);
  define_syntaxes_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-syntaxes"), scheme_false, w, 0, 0);
  require_stx = scheme_datum_to_syntax(scheme_intern_symbol("require"), scheme_false, w, 0, 0);
  require_for_syntax_stx = scheme_datum_to_syntax(scheme_intern_symbol("require-for-syntax"), scheme_false, w, 0, 0);
  provide_stx = scheme_datum_to_syntax(scheme_intern_symbol("provide"), scheme_false, w, 0, 0);
  set_stx = scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, w, 0, 0);
  app_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%app"), scheme_false, w, 0, 0);
  top_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%top"), scheme_false, w, 0, 0);

  REGISTER_SO(prefix_symbol);
  REGISTER_SO(rename_symbol);
  REGISTER_SO(all_except_symbol);
  REGISTER_SO(all_from_symbol);
  REGISTER_SO(all_from_except_symbol);
  REGISTER_SO(struct_symbol);
  prefix_symbol = scheme_intern_symbol("prefix");
  rename_symbol = scheme_intern_symbol("rename");
  all_except_symbol = scheme_intern_symbol("all-except");
  all_from_symbol = scheme_intern_symbol("all-from");
  all_from_except_symbol = scheme_intern_symbol("all-from-except");
  struct_symbol = scheme_intern_symbol("struct");
}

void scheme_require_from_original_env(Scheme_Env *env, int syntax_only)
{
  Scheme_Object *rn, **exs;
  int i, c;

  rn = env->rename;
  if (!rn) {
    rn = scheme_make_module_rename(env->phase, 1);
    env->rename = rn;
  }

  exs = kernel->provides;
  c = kernel->num_provides;
  i = (syntax_only ? kernel->num_var_provides : 0);
  for (; i < c; i++) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i]);
  }
}

Scheme_Object *scheme_sys_wraps(Scheme_Comp_Env *env)
{
  Scheme_Object *rn, *w;
  long phase;

  if (!env)
    phase = 0;
  else
    phase = env->genv->phase;

  if ((phase == 0) && scheme_sys_wraps0)
    return scheme_sys_wraps0;
  if ((phase == 1) && scheme_sys_wraps1)
    return scheme_sys_wraps1;

  rn = scheme_make_module_rename(phase, 0);

  /* Add a module mapping for all kernel provides: */
  scheme_extend_module_rename_with_kernel(rn);
  
  w = scheme_datum_to_syntax(kernel_symbol, scheme_false, scheme_false, 0, 0);
  w = scheme_add_rename(w, rn);
  if (phase == 0) {
    REGISTER_SO(scheme_sys_wraps0);
    scheme_sys_wraps0 = w;
  }
  if (phase == 1) {
    REGISTER_SO(scheme_sys_wraps1);
    scheme_sys_wraps1 = w;
  }

  return w;
}

void scheme_save_initial_module_set(Scheme_Env *env)
/* Can be called multiple times! */
{
  int i, c, count;
  Scheme_Hash_Table *ht;
	
  ht = env->module_registry;
  c = ht->size;

  count = 0;
  for (i = 0; i < c; i++) {
    if (ht->vals[i])
      count++;
  }

  num_initial_modules = count;
  
  if (!initial_modules) {
    REGISTER_SO(initial_modules);
  }
  initial_modules = MALLOC_N(Scheme_Object *, 3 * count);

  count = 0;
  for (i = 0; i < c; i++) {
    if (ht->vals[i]) {
      initial_modules[count++] = ht->keys[i];
      initial_modules[count++] = ht->vals[i];
      initial_modules[count++] = NULL;
    }
  }

  /* Make sure all initial modules are running: */
  for (i = 0; i < num_initial_modules; i++) {
    Scheme_Module *m = (Scheme_Module *)initial_modules[(i * 3) + 1];
    start_module(m, env, 0, m->modname);
    initial_modules[(i * 3) + 2] = scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
  }

  /* Clone renames: */
  if (!initial_renames) {
    REGISTER_SO(initial_renames);
  }
  initial_renames = scheme_make_module_rename(0, 0);
  scheme_append_module_rename(env->rename, initial_renames);

  /* Clone variable bindings: */
  if (!initial_toplevel) {
     REGISTER_SO(initial_toplevel);
  }
  initial_toplevel = scheme_clone_toplevel(env->toplevel, NULL);
}

void scheme_install_initial_module_set(Scheme_Env *env)
{
  int i;

  /* Copy over module declarations and instances: */
  for (i = 0; i < num_initial_modules; i++) {
    scheme_hash_set(env->module_registry, 
		    initial_modules[i * 3],
		    initial_modules[(i * 3) + 1]);
    scheme_hash_set(MODCHAIN_TABLE(env->modchain),
		    initial_modules[i * 3],
		    initial_modules[(i * 3) + 2]);
  }

  /* Copy renamings: */
  if (!env->rename) {
    Scheme_Object *rn;
    rn = scheme_make_module_rename(0, 1);
    env->rename = rn;
  }
  scheme_append_module_rename(initial_renames, env->rename);

  /* Copy toplevel: */
  {
    Scheme_Bucket_Table *tl;
    tl = scheme_clone_toplevel(initial_toplevel, env);
    env->toplevel = tl;
  }
}

/**********************************************************************/
/*                             parameters                             */
/**********************************************************************/

static Scheme_Object *default_module_resolver(int argc, Scheme_Object **argv)
{
  scheme_arg_mismatch("default-module-name-resolver", 
		      "the kernel's resolver always fails; given: ", 
		      argv[0]);
  return NULL;
}

static Scheme_Object *
current_module_name_resolver(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-module-name-resolver",
			     scheme_make_integer(MZCONFIG_CURRENT_MODULE_RESOLVER),
			     argc, argv,
			     3, NULL, NULL, 0);
}

static Scheme_Object *prefix_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_FALSEP(o) || SCHEME_SYMBOLP(o))
    return o;

  return NULL;
}

static Scheme_Object *
current_module_name_prefix(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-module-name-prefix",
			     scheme_make_integer(MZCONFIG_CURRENT_MODULE_PREFIX),
			     argc, argv,
			     -1, prefix_p, "symbol or #f", 1);
}

/**********************************************************************/
/*                            procedures                              */
/**********************************************************************/

static Scheme_Object *_dynamic_require(int argc, Scheme_Object *argv[], 
				       int get_syntax, int get_bucket, 
				       int exp_time, int indirect_ok,
				       int fail_with_error)
{
  Scheme_Object *modname, *modidx;
  Scheme_Object *name, *srcname, *srcmname;
  Scheme_Module *m, *srcm;
  Scheme_Env *env, *menv;
  int i, count;

  modname = argv[0];
  name = argv[1];

  if (SCHEME_TRUEP(name) && !SCHEME_SYMBOLP(name)) {
    scheme_wrong_type((get_syntax? "dynamic-require" : "dynamic-require-syntax"), "symbol or #f", 1, argc, argv);
    return NULL;
  }

  if (SAME_TYPE(SCHEME_TYPE(modname), scheme_module_index_type))
    modidx = modname;
  else
    modidx = scheme_make_modidx(modname, scheme_false, scheme_false);

  modname = scheme_module_resolve(modidx);

  env = scheme_get_env(scheme_config);
  if (exp_time) {
    scheme_prepare_exp_env(env);
    env = env->exp_env;
  }

  m = module_load(modname, env, "dynamic-require");
  srcm = m;

  srcmname = NULL;
  srcname = NULL;

  if (SCHEME_SYMBOLP(name)) {
  try_again:
    
    /* Before starting, check whether the name is provided */
    count = srcm->num_provides;
    for (i = 0; i < count; i++) {
      if (SAME_OBJ(name, srcm->provides[i])) {
	if ((get_syntax && (i >= srcm->num_var_provides))
	    || (!get_syntax && (i < srcm->num_var_provides))) {
	  srcmname = (srcm->provide_srcs ? srcm->provide_srcs[i] : scheme_false);
	  if (SCHEME_FALSEP(srcmname))
	    srcmname = srcm->modname;
	  srcname = srcm->provide_src_names[i];
	  break;
	} else {
	  if (fail_with_error)
	    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, name,
			     "%s: name is provided as %s: %V by module: %V",
			     (get_syntax ? "dynamic-require-syntax" : "dynamic-require"),
			     (get_syntax ? "syntax" : "a variable"),
			     name, srcm->modname);
	  return NULL;
	}
      }
    }
    
    if ((i == count) && srcm->reprovide_kernel) {
      /* Check kernel. */
      srcm = kernel;
      goto try_again;
    }

    if (i == count) {
      if (indirect_ok) {
	/* Try indirect provides: */
	srcm = m;
	count = srcm->num_indirect_provides;
	for (i = 0; i < count; i++) {
	  if (SAME_OBJ(name, srcm->indirect_provides[i])) {
	    srcname = name;
	    srcmname = srcm->modname;
	    break;
	  }
	}
      }

      if (i == count) {
	if (fail_with_error)
	  scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, name,
			   "%s: name is not provided: %V by module: %V",
			   (get_syntax? "dynamic-require-syntax" : "dynamic-require"),
			   name, srcm->modname);
	return NULL;
      }
    }
  }

  if (get_syntax)
    expstart_module(m, env, 0, modidx);
  else
    start_module(m, env, 0, modidx);

  if (SCHEME_SYMBOLP(name)) {
    menv = scheme_module_access(srcmname, env);

    if (get_syntax) {
      Scheme_Object *v;

      v = scheme_module_syntax(srcmname, env, srcname);

      if (NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_macro_type)) {
	if (fail_with_error)
	  scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, name,
			   "dynamic-require-syntax: name is primitive syntax: %V from module: %V",
			   name, srcm->modname);
	return NULL;
      }

      return SCHEME_PTR_VAL(v);
    } else {
      if (get_bucket)
	return (Scheme_Object *)scheme_bucket_from_table(menv->toplevel, (const char *)srcname);
      else
	return (Scheme_Object *)scheme_lookup_in_table(menv->toplevel, (const char *)srcname);
    }
  } else
    return scheme_void;
}

static Scheme_Object *dynamic_require(int argc, Scheme_Object *argv[])
{
  return _dynamic_require(argc, argv, 0, 0, 0, 0, 1);
}

static Scheme_Object *dynamic_require_syntax(int argc, Scheme_Object *argv[])
{
  return _dynamic_require(argc, argv, 1, 0, 0, 0, 1);
}

static Scheme_Object *do_namespace_require(int argc, Scheme_Object *argv[], int for_exp)
{
  Scheme_Object *form, *rn, *brn;
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);
  if (for_exp) {
    scheme_prepare_exp_env(env);
    env = env->exp_env;
  }

  form = scheme_datum_to_syntax(scheme_make_pair(require_stx,
						 scheme_make_pair(argv[0], scheme_null)),
				scheme_false, scheme_false, 1, 0);
  
  rn = scheme_make_module_rename(for_exp, 1);

  (void)parse_requires(form, form, scheme_false, env, rn, 
		       NULL, NULL, 1, NULL, 1);

  brn = env->rename;
  if (!brn) {
    brn = scheme_make_module_rename(for_exp, 1);
    env->rename = brn;
  }

  scheme_append_module_rename(rn, brn);

  return scheme_void;
}

static Scheme_Object *namespace_require(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(argc, argv, 0);
}

static Scheme_Object *namespace_trans_require(int argc, Scheme_Object *argv[])
{
  return do_namespace_require(argc, argv, 1);
}

static void pre_post_force(void *_v)
{
  Scheme_Object *old, *v = (Scheme_Object *)_v;

  old = scheme_get_param(scheme_config, MZCONFIG_ENV);
  scheme_set_param(scheme_config, MZCONFIG_ENV, SCHEME_VEC_ELS(v)[0]);
  SCHEME_VEC_ELS(v)[0] = old;
}
	
static Scheme_Object *now_do_force(void *_v)
{
  Scheme_Object *v = (Scheme_Object *)_v;
  finish_expstart_module((Scheme_Env *)SCHEME_VEC_ELS(v)[1], 
			 (Scheme_Env *)scheme_get_param(scheme_config, MZCONFIG_ENV));
  return scheme_void;
}
	
static Scheme_Object *namespace_attach_module(int argc, Scheme_Object *argv[])
{
  Scheme_Env *from_env, *to_env, *menv, *menv2;
  Scheme_Object *todo, *next_phase_todo, *name, *notifies = scheme_null, *a[3], *resolver;
  Scheme_Object *to_modchain, *from_modchain, *l;
  Scheme_Hash_Table *checked, *next_checked;
  Scheme_Module *m2;

  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_type("namespace-attach-module", "namespace", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type("namespace-attach-module", "symbol", 1, argc, argv);

  from_env = (Scheme_Env *)argv[0];
  to_env = scheme_get_env(scheme_config);

  todo = scheme_make_pair(scheme_module_resolve(argv[1]), scheme_null);
  next_phase_todo = scheme_null;
  from_modchain = from_env->modchain;
  to_modchain = to_env->modchain;

  /* Check whether todo, or anything it needs, is already declared incompatibly: */
  while (!SCHEME_NULLP(todo)) {
    checked = scheme_make_hash_table(SCHEME_hash_ptr);
    next_checked = scheme_make_hash_table(SCHEME_hash_ptr);

    while (!SCHEME_NULLP(todo)) {
      name = SCHEME_CAR(todo);

      todo = SCHEME_CDR(todo);

      if (!SAME_OBJ(name, kernel_symbol)) {
	menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_modchain), name);

	if (!menv) {
	  /* Assert: name == argv[1] */
	  scheme_arg_mismatch("namespace-attach-module",
			      "unknown module (in the source namespace): ",
			      name);
	}

	if (SCHEME_TRUEP(to_modchain)) {
	  menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
	  if (menv2) {
	    if (!SAME_OBJ(menv->toplevel, menv2->toplevel))
	      m2 = menv2->module;
	    else
	      m2 = NULL;
	  } else {
	    m2 = (Scheme_Module *)scheme_hash_get(to_env->module_registry, name);
	  }
	  
	  if (m2)
	    scheme_arg_mismatch("namespace-attach-module",
				"a different module with the same name is already "
				"in the destination namespace, for name: ",
				name);
	} else
	  menv2 = NULL;
      
	if (!menv2) {
	  /* Push requires onto the check list: */
	  l = menv->module->requires;
	  while (!SCHEME_NULLP(l)) {
	    name = scheme_module_resolve(SCHEME_CAR(l));
	    if (!scheme_hash_get(checked, name)) {
	      todo = scheme_make_pair(name, todo);
	      scheme_hash_set(checked, name, scheme_true);
	    }
	    l = SCHEME_CDR(l);
	  }

	  /* Have to force laziness in source to ensure sharing: */
	  if (menv->lazy_syntax) {
	    Scheme_Object *v;

	    v = scheme_make_vector(2, NULL);
	    SCHEME_VEC_ELS(v)[0] = (Scheme_Object *)from_env;
	    SCHEME_VEC_ELS(v)[1] = (Scheme_Object *)menv;

	    scheme_dynamic_wind(pre_post_force, now_do_force,
				pre_post_force, NULL,
				(void *)v);
	  }

	  l = menv->module->et_requires;
	  while (!SCHEME_NULLP(l)) {
	    name = scheme_module_resolve(SCHEME_CAR(l));
	    if (!scheme_hash_get(next_checked, name)) {
	      next_phase_todo = scheme_make_pair(name, next_phase_todo);
	      scheme_hash_set(next_checked, name, scheme_true);
	    }
	    l = SCHEME_CDR(l);
	  }
	}
      }
    }

    todo = next_phase_todo;
    next_phase_todo = scheme_null;
    from_modchain = SCHEME_VEC_ELS(from_modchain)[1];
    if (SCHEME_TRUEP(to_modchain))
      to_modchain = SCHEME_VEC_ELS(to_modchain)[1];
  }

  /* Go again, this time tranferring modules */

  todo = scheme_make_pair(argv[1], scheme_null);
  next_phase_todo = scheme_null;
  from_modchain = from_env->modchain;
  to_modchain = to_env->modchain;

  while (!SCHEME_NULLP(todo)) {
    while (!SCHEME_NULLP(todo)) {
      name = SCHEME_CAR(todo);
      name = scheme_module_resolve(name);

      todo = SCHEME_CDR(todo);

      if (!SAME_OBJ(name, kernel_symbol)) {
	menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(from_modchain), name);
      
	menv2 = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(to_modchain), name);
	if (!menv2) {
	  /* Clone menv for the new namespace: */
	  menv2 = scheme_clone_module_env(menv, to_env, to_modchain);

	  scheme_hash_set(MODCHAIN_TABLE(to_modchain), name, (Scheme_Object *)menv2);
	  scheme_hash_set(to_env->module_registry, name, (Scheme_Object *)menv2->module);

	  /* Push name onto notify list: */
	  notifies = scheme_make_pair(name, notifies);

	  /* Push requires onto the check list: */
	  todo = scheme_append(menv->module->requires, todo);
	  next_phase_todo = scheme_append(menv->module->et_requires, next_phase_todo);
	}
      }
    }

    todo = next_phase_todo;
    next_phase_todo = scheme_null;
    from_modchain = SCHEME_VEC_ELS(from_modchain)[1];
    to_modchain = SCHEME_VEC_ELS(to_modchain)[1];
  }

  /* Notify module name resolver of attached modules: */
  resolver = scheme_get_param(scheme_config, MZCONFIG_CURRENT_MODULE_RESOLVER);
  while (!SCHEME_NULLP(notifies)) {
    a[0] = scheme_false;
    a[1] = SCHEME_CAR(notifies);
    a[2] = scheme_false;
    
    name = scheme_apply(resolver, 3, a);

    notifies = SCHEME_CDR(notifies);
  }

  return scheme_void;
}

static Scheme_Object *module_path_index_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_path_index_split(int argc, Scheme_Object *argv[])
{
  Scheme_Modidx *modidx;
  Scheme_Object *a[2];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_module_index_type))
    scheme_wrong_type("module-path-index-split", "module-path-index", 0, argc, argv);

  modidx = (Scheme_Modidx *)argv[0];
  a[0] = modidx->path;
  a[1] = modidx->base;

  return scheme_values(2, a);
}

static Scheme_Object *module_path_index_join(int argc, Scheme_Object *argv[])
{
  if (SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("module-path-index-join", "non-symbol", 0, argc, argv);

  if (SCHEME_TRUEP(argv[1])
      && !SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_module_index_type))
    scheme_wrong_type("module-path-index-join", "module-path-index or #f", 1, argc, argv);

  return scheme_make_modidx(argv[0], argv[1], scheme_false);
}

/**********************************************************************/
/*                       basic module operations                      */
/**********************************************************************/

Scheme_Object *scheme_make_modidx(Scheme_Object *path, 
				  Scheme_Object *base_modidx,
				  Scheme_Object *resolved)
{
  Scheme_Modidx *modidx;

  if (SCHEME_SYMBOLP(path))
    return path;

  modidx = MALLOC_ONE_TAGGED(Scheme_Modidx);
  modidx->type = scheme_module_index_type;
  modidx->path = path;
  modidx->base = base_modidx;
  modidx->resolved = resolved;
  
  return (Scheme_Object *)modidx;
}

int same_modidx(Scheme_Object *a, Scheme_Object *b)
{
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_module_index_type))
    a = ((Scheme_Modidx *)a)->path;
  if (SAME_TYPE(SCHEME_TYPE(b), scheme_module_index_type))
    b = ((Scheme_Modidx *)b)->path;

  return scheme_equal(a, b);
}

static Scheme_Object *_module_resolve(Scheme_Object *modidx, Scheme_Object *stx)
{
  if (SCHEME_SYMBOLP(modidx))
    return modidx;

  if (SCHEME_FALSEP(((Scheme_Modidx *)modidx)->resolved)) {
    /* Need to resolve access path to a module name: */
    Scheme_Object *a[3];
    Scheme_Object *name, *base;
    
    base = ((Scheme_Modidx *)modidx)->base;
    if (!SCHEME_FALSEP(base)) {
      /* FIXME: this can go arbitrarily deep, in principle. */
      base = _module_resolve(base, NULL);
    }

    a[0] = ((Scheme_Modidx *)modidx)->path;
    a[1] = base;
    a[2] = (stx ? stx : scheme_false);
    
    if (SCHEME_FALSEP(a[0])) {
      scheme_wrong_syntax("require", NULL, NULL, 
			  "broken compiled code: unresolved module index without path");
    }

    name = scheme_apply(scheme_get_param(scheme_config, MZCONFIG_CURRENT_MODULE_RESOLVER), 3, a);
    
    ((Scheme_Modidx *)modidx)->resolved = name;
  }

  return ((Scheme_Modidx *)modidx)->resolved;
}

Scheme_Object *scheme_module_resolve(Scheme_Object *modidx)
{
  return _module_resolve(modidx, NULL);
}

Scheme_Object *scheme_modidx_shift(Scheme_Object *modidx, 
				   Scheme_Object *shift_from_modidx,
				   Scheme_Object *shift_to_modidx)
{
  Scheme_Object *base;

  if (!shift_to_modidx)
    return modidx;

  if (SAME_OBJ(modidx, shift_from_modidx))
    return shift_to_modidx;

  if (!SAME_TYPE(SCHEME_TYPE(modidx), scheme_module_index_type))
    return modidx;
  
  /* Need to shift relative part? */
  base = ((Scheme_Modidx *)modidx)->base;
  if (!SCHEME_FALSEP(base)) {
    /* FIXME: depth */
    Scheme_Object *sbase;
    sbase = scheme_modidx_shift(base, shift_from_modidx, shift_to_modidx);

    if (!SAME_OBJ(base, sbase)) {
      /* There was a shift in the relative part. */
      /* Shift cached? [If base is a symbol, sbase for the cache.] */
      Scheme_Modidx *sbm = (Scheme_Modidx *)(SCHEME_SYMBOLP(sbase) ? base : sbase);
      int i, c = (sbm->shift_cache ? SCHEME_VEC_SIZE(sbm->shift_cache) : 0);
      Scheme_Object *smodidx;

      for (i = 0; i < c; i += 2) {
	if (!SCHEME_VEC_ELS(sbm->shift_cache)[i])
	  break;
	if (SAME_OBJ(modidx, SCHEME_VEC_ELS(sbm->shift_cache)[i]))
	  return SCHEME_VEC_ELS(sbm->shift_cache)[i + 1];
      }
      
      smodidx = scheme_make_modidx(((Scheme_Modidx *)modidx)->path,
				   sbase,
				   ((Scheme_Modidx *)modidx)->resolved);
      
      if (i >= c) {
	/* Grow cache vector */
	Scheme_Object *old = sbm->shift_cache, *naya;
	int j;

	naya = scheme_make_vector(c + 10, NULL);
	for (j = 0; j < c; j++) {
	  SCHEME_VEC_ELS(naya)[j] = SCHEME_VEC_ELS(old)[j];
	}
	sbm->shift_cache = naya;
      }

      SCHEME_VEC_ELS(sbm->shift_cache)[i] = modidx;
      SCHEME_VEC_ELS(sbm->shift_cache)[i+1] = smodidx;

      return smodidx;
    }
  }

  return modidx;
}

static Scheme_Module *module_load(Scheme_Object *name, Scheme_Env *env, const char *who)
{
  if (name == kernel_symbol)
    return kernel;
  else {
    Scheme_Module *m;

    m = (Scheme_Module *)scheme_hash_get(env->module_registry, name);

    if (!m) {
      scheme_wrong_syntax(who ? who : "require", NULL, name, "unknown module");
      return NULL;
    }

    return m;
  }
}

Scheme_Env *scheme_module_access(Scheme_Object *name, Scheme_Env *env)
{
  if (name == kernel_symbol)
    return scheme_initial_env;
  else
    return (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), name);
}

void scheme_check_accessible_in_module(Scheme_Env *env, Scheme_Object *symbol, Scheme_Object *stx)
{
  if (env == scheme_initial_env)
    return;
  if (env->module->primitive)
    return;

  if (scheme_hash_get(env->module->accessible, symbol))
    return;

  if (stx && SAME_OBJ(SCHEME_STX_SYM(stx), symbol)) {
    symbol = stx;
    stx = NULL;
  }

  scheme_wrong_syntax("compile", stx, symbol, 
		      "variable not provided (directly or indirectly) from module: %S",
		      env->module->modname);
}

Scheme_Object *scheme_module_syntax(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *name)
{
  if (modname == kernel_symbol)
    return scheme_lookup_in_table(scheme_initial_env->syntax, (char *)name);
  else {
    Scheme_Env *menv;
    Scheme_Object *val;

    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), modname);
    
    if (!menv)
      return NULL;

    if (menv->lazy_syntax)
      finish_expstart_module(menv, env);

    val = scheme_lookup_in_table(menv->syntax, (char *)name);

    return val;
  }
}

void scheme_module_force_lazy(Scheme_Env *env, int previous)
{
  Scheme_Hash_Table *mht;
  int mi;

  if (previous)
    mht = MODCHAIN_TABLE(SCHEME_VEC_ELS(env->modchain)[2]);
  else
    mht = MODCHAIN_TABLE(env->modchain);
  
  for (mi = mht->size; mi--; ) {
    if (mht->vals[mi]) {
      /* Check this module for lazy syntax. */
      Scheme_Env *menv = (Scheme_Env *)mht->vals[mi];

      if (menv->lazy_syntax)
	finish_expstart_module(menv, env);
    }
  }
}

static void expstart_module(Scheme_Module *m, Scheme_Env *env, int restart, 
			    Scheme_Object *syntax_idx)
{
  Scheme_Env *menv;
  Scheme_Object *l;

  if (SAME_OBJ(m, kernel))
    return;

  if (!restart) {
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
    if (menv)
      return;
  }

  if (m->primitive) {
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
    if (!menv)
      scheme_hash_set(MODCHAIN_TABLE(env->modchain), m->modname, (Scheme_Object *)m->primitive);
    return;
  }

  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
  if (!menv || restart) {
    if (!menv) {
      menv = scheme_new_module_env(env, m, 0);
      scheme_hash_set(MODCHAIN_TABLE(env->modchain), m->modname, (Scheme_Object *)menv);
      
      menv->phase = env->phase;
      menv->link_midx = syntax_idx;
    } else
      menv->module = m;

    if (!m->accessible) {
      Scheme_Hash_Table *ht;
      int i, count;

      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      count = m->num_var_provides;
      for (i = 0; i < count; i++) {
	if (SCHEME_FALSEP(m->provide_srcs[i])) {
	  scheme_hash_set(ht, m->provide_src_names[i], scheme_false);
	}
      }

      count = m->num_indirect_provides;
      for (i = 0; i < count; i++) {
	scheme_hash_set(ht, m->indirect_provides[i], scheme_false);
      }
      m->accessible = ht;
    }

    /* Create provided global variables: */
    {
      Scheme_Object **exss, **exsns;
      int i, count;

      exsns = m->provide_src_names;
      exss = m->provide_srcs;
      count = m->num_var_provides;

      for (i = 0; i < count; i++) {
	if (SCHEME_FALSEP(exss[i]))
	  scheme_add_to_table(menv->toplevel, (const char *)exsns[i], NULL, 0);
      }

      count = m->num_indirect_provides;
      exsns = m->indirect_provides;
      for (i = 0; i < count; i++) {
	scheme_add_to_table(menv->toplevel, (const char *)exsns[i], NULL, 0);
      }
    }
  }
  
  for (l = m->requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    expstart_module(module_load(scheme_module_resolve(SCHEME_CAR(l)), env, NULL), 
		    env, 0, 
		    scheme_modidx_shift(SCHEME_CAR(l), m->self_modidx, syntax_idx));
  }

  if (m->prim_et_body || !SCHEME_NULLP(m->et_body) || !SCHEME_NULLP(m->et_requires)) {
    /* Set lazy-syntax flag. */
    menv->lazy_syntax = 1;
  }
}

static void finish_expstart_module(Scheme_Env *menv, Scheme_Env *env)
{
  Scheme_Object *l, *body, *e, *names;
  Scheme_Env *exp_env;
  Scheme_Bucket_Table *syntax;
  int let_depth;

  /* Continue a delayed expstart: */
  menv->lazy_syntax = 0;

  syntax = menv->syntax;

  scheme_prepare_exp_env(menv);
  exp_env = menv->exp_env;
  menv->exp_env = NULL;

  exp_env->link_midx = menv->link_midx;

  for (l = menv->module->et_requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    start_module(module_load(scheme_module_resolve(SCHEME_CAR(l)), env, NULL), 
		 exp_env, 0,
		 scheme_modidx_shift(SCHEME_CAR(l), menv->module->self_modidx, exp_env->link_midx));
  }
  
  if (menv->module->prim_et_body) {
    Scheme_Invoke_Proc ivk = menv->module->prim_et_body;
    Scheme_Env *cenv;

    /* Top simplify mzc's job, we make up an environment where the
       syntax table is the same as menv, and the exp_env is exp_env */
    cenv = MALLOC_ONE_TAGGED(Scheme_Env);
    cenv->module = menv->module;
    cenv->syntax = menv->syntax;
    cenv->exp_env = exp_env;    

    ivk(cenv, menv->phase, menv->module->self_modidx, menv->module->body);
  } else {
    for (body = menv->module->et_body; !SCHEME_NULLP(body); body = SCHEME_CDR(body)) {
      e = SCHEME_CAR(body);
      
      names = SCHEME_VEC_ELS(e)[0];
      let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
      e = SCHEME_VEC_ELS(e)[1];
      
      eval_defmacro(names, scheme_proper_list_length(names), e, exp_env->init, let_depth, syntax);
    }
  }
}

static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, 
			 Scheme_Object *syntax_idx)
{
  Scheme_Env *menv;
  Scheme_Object *body, *e, *l;

  if (SAME_OBJ(m, kernel))
    return;

  expstart_module(m, env, restart, syntax_idx);

  if (m->primitive)
    return;

  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);

  if (restart)
    menv->running = 0;

  if (menv->running)
    return;
  
  for (l = m->requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    start_module(module_load(scheme_module_resolve(SCHEME_CAR(l)), env, NULL), 
		 env, 0, 
		 scheme_modidx_shift(SCHEME_CAR(l), m->self_modidx, syntax_idx));
  }

  menv->running = 1;

  body = m->body;

  if (menv->module->prim_body) {
    Scheme_Invoke_Proc ivk = menv->module->prim_body;
    ivk(menv, menv->phase, menv->module->self_modidx, body);
  } else {
    for (; !SCHEME_NULLP(body); body = SCHEME_CDR(body)) {
      e = scheme_link_expr(SCHEME_CAR(body), menv);
      _scheme_eval_linked_expr_multi(e);
    }
  }
}

static void prevent_cyclic_requires(Scheme_Module *m, Scheme_Object *modname, Scheme_Env *env)
{
  Scheme_Object *l;

  if (SAME_OBJ(m->modname, modname)) {
    scheme_wrong_syntax("module", NULL, modname, 
			"import cycle detected for re-definition of module");
  }

  /* Check et_requires: */
  for (l = m->et_requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {    
    prevent_cyclic_requires(module_load(scheme_module_resolve(SCHEME_CAR(l)), env, NULL),
			   modname, env);
  }
  
  /* Check requires: */
  for (l = m->requires; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {    
    prevent_cyclic_requires(module_load(scheme_module_resolve(SCHEME_CAR(l)), env, NULL),
			   modname, env);
  }
}

Scheme_Env *scheme_primitive_module(Scheme_Object *name, Scheme_Env *for_env)
{
  Scheme_Module *m;
  Scheme_Env *env;
  Scheme_Object *prefix;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->type = scheme_module_type;
  
  env = scheme_new_module_env(for_env, m, 0);

  prefix = scheme_get_param(scheme_config, MZCONFIG_CURRENT_MODULE_PREFIX);
  if (SCHEME_SYMBOLP(prefix))
    name = scheme_symbol_append(prefix, name);

  m->modname = name;
  m->requires = scheme_null;
  m->et_requires = scheme_null;
  m->primitive = env;

  scheme_hash_set(for_env->module_registry, m->modname, (Scheme_Object *)m);

  return env;
}

void scheme_finish_primitive_module(Scheme_Env *env)
{
  Scheme_Module *m = env->module;
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  Scheme_Object **exs;
  int i, count;

  /* Provide all variables: */
  count = 0;
  ht = env->toplevel;

  bs = ht->buckets;
  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val)
      count++;
  }

  exs = MALLOC_N(Scheme_Object *, count);
  count = 0;
  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val)
      exs[count++] = (Scheme_Object *)b->key;
  }
 
  m->provides = exs;
  m->provide_srcs = NULL;
  m->provide_src_names = exs;
  m->num_provides = count;
  m->num_var_provides = count;

  env->running = 1;
}

Scheme_Bucket *scheme_module_bucket(Scheme_Object *modname, Scheme_Object *var, Scheme_Env *env)
{
  Scheme_Object *a[2];

  a[0] = modname;
  a[1] = var;

  return (Scheme_Bucket *)_dynamic_require(2, a, 0, 1, 0, 1, 1);
}

Scheme_Bucket *scheme_exptime_module_bucket(Scheme_Object *modname, Scheme_Object *var, Scheme_Env *env)
{
  Scheme_Object *a[2];

  a[0] = modname;
  a[1] = var;

  return (Scheme_Bucket *)_dynamic_require(2, a, 0, 1, 1, 1, 1);
}

Scheme_Object *scheme_builtin_value(const char *name)
{
  Scheme_Object *a[2], *v;

  a[1] = scheme_intern_symbol(name);

  /* Try kernel first: */
  a[0] = kernel_symbol;
  v = _dynamic_require(2, a, 0, 0, 0, 0, 0);

  if (v)
    return v;

  /* Maybe in MzScheme? */
  a[0] = scheme_intern_symbol("mzscheme");
  return _dynamic_require(2, a, 0, 0, 0, 0, 0);
}

/**********************************************************************/
/*                          define-syntaxes                           */
/**********************************************************************/

static void eval_defmacro(Scheme_Object *names, int count,
			  Scheme_Object *expr, Scheme_Comp_Env *env,
			  int let_depth, Scheme_Bucket_Table *syntax)
{
  Scheme_Object *macro, *vals, *name;
  int i, g;

  expr = scheme_link_expr(expr, env->genv);
	
  scheme_on_next_top(env, NULL, scheme_false);
  vals = scheme_eval_linked_expr_multi(expr, let_depth);
  
  if (SAME_OBJ(vals, SCHEME_MULTIPLE_VALUES)) {
    g = scheme_current_thread->ku.multiple.count;
    if (count == g) {
      Scheme_Object **values;

      values = scheme_current_thread->ku.multiple.array;
      for (i = 0; i < g; i++, names = SCHEME_CDR(names)) {
	name = SCHEME_CAR(names);

	macro = scheme_alloc_small_object();
	macro->type = scheme_macro_type;
	SCHEME_PTR_VAL(macro) = values[i];
	
	scheme_add_to_table(syntax, (const char *)name, macro, 0);
      }
	
      return;
    }
  } else if (SCHEME_PAIRP(names) && SCHEME_NULLP(SCHEME_CDR(names))) {
    name = SCHEME_CAR(names);

    macro = scheme_alloc_small_object();
    macro->type = scheme_macro_type;
    SCHEME_PTR_VAL(macro) = vals;

    scheme_add_to_table(syntax, (const char *)name, macro, 0);
      
    return;
  } else
    g = 1;
  
  if (count)
    name = SCHEME_CAR(names);
  else
    name = NULL;
  
  {
    const char *symname;
    symname = (name ? scheme_symbol_name(name) : "");
    scheme_wrong_return_arity("define-syntaxes",
			      count, g,
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
			      "%s%s%s",
			      name ? "defining \"" : "0 names",
			      symname,
			      name ? ((count == 1) ? "\"" : "\", ...") : "");
  }  
}

/**********************************************************************/
/*                               module                               */
/**********************************************************************/

static Scheme_Object *
module_execute(Scheme_Object *data)
{
  Scheme_Module *m = (Scheme_Module *)SCHEME_CAR(data);
  Scheme_Env *env = (Scheme_Env *)SCHEME_CDR(data);
  Scheme_Env *menv;
  Scheme_Object *mzscheme_symbol;
  
  mzscheme_symbol = scheme_intern_symbol("mzscheme");
  
  if ((((SCHEME_SYM_VAL(m->modname)[0] == '#')
	&& (SCHEME_SYM_VAL(m->modname)[1] == '%'))
       || SAME_OBJ(m->modname, mzscheme_symbol))
      && scheme_hash_get(env->module_registry, m->modname)) {
    scheme_arg_mismatch("module",
			(SAME_OBJ(mzscheme_symbol, m->modname) 
			 ? "cannot redefine special module name: " 
			 : "cannot redefine a module name starting with `#%': "),
			m->modname);
  }

  scheme_hash_set(env->module_registry, m->modname, (Scheme_Object *)m);

  /* Replaced an already-running or already-syntaxing module? */
  menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);
  if (menv) {
    if (menv->running)
      start_module(m, env, 1, NULL);
    else
      expstart_module(m, env, 1, NULL);
  }

  return scheme_void;
}

static Scheme_Object *
module_link(Scheme_Object *data, Link_Info *link)
{
  /* We don't actually link, leaving that until module run time(s) */
  return scheme_make_syntax_linked(MODULE_EXPD,
				   cons(data, (Scheme_Object *)link));
}

static Scheme_Object *
module_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *b;

  for (b = m->body; !SCHEME_NULLP(b); b = SCHEME_CDR(b)) {
    Scheme_Object *e;
    e = scheme_resolve_expr(SCHEME_CAR(b), rslv);
    SCHEME_CAR(b) = e;
  }

  return scheme_make_syntax_resolved(MODULE_EXPD, data);
}

static Scheme_Object *do_module(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *nm, *ii, *rn, *et_rn, *iidx, *self_modidx;
  Scheme_Module *iim;
  Scheme_Env *menv;
  Scheme_Module *m;
  Scheme_Object *mbval;
  int saw_mb, check_mb = 0;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("module", NULL, form, "illegal use (not at top-level)");

  fm = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax("module", NULL, form, NULL);
  nm = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(nm))
    scheme_wrong_syntax("module", nm, form, "module name is not an identifier");
  fm = SCHEME_STX_CDR(fm);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax("module", NULL, form, NULL);
  ii = SCHEME_STX_CAR(fm);
  fm = SCHEME_STX_CDR(fm);

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->type = scheme_module_type;
  
  {
    Scheme_Object *prefix, *modname;

    modname = SCHEME_STX_VAL(nm);
    prefix = scheme_get_param(scheme_config, MZCONFIG_CURRENT_MODULE_PREFIX);
    
    if (SCHEME_SYMBOLP(prefix))
      modname = scheme_symbol_append(prefix, modname);
      
    m->modname = modname; /* must set before calling new_module_env */
  }

  menv = scheme_new_module_env(env->genv, m, 1);

  self_modidx = scheme_make_modidx(scheme_false, scheme_false, m->modname);
  m->self_modidx = self_modidx;

  iidx = scheme_make_modidx(scheme_syntax_to_datum(ii, 0, NULL), 
			    self_modidx,
			    scheme_false);

  {
    Scheme_Object *ins;
    ins = cons(iidx, scheme_null);
    m->requires = ins;
    m->et_requires = scheme_null;
  }

  /* load the module for the initial require */
  iim = module_load(_module_resolve(iidx, ii), menv, NULL); 
  expstart_module(iim, menv, 0, iidx);

  if (scheme_hash_get(menv->module_registry, m->modname)) {
    /* Redefinition: cycles are possible. */
    prevent_cyclic_requires(iim, m->modname, menv);
  }

  rn = scheme_make_module_rename(0, 0);
  et_rn = scheme_make_module_rename(1, 0);

  menv->rename = rn;
  menv->et_rename = et_rn;

  /* For each (direct) provide in iim, add a module rename to fm */
  if (SAME_OBJ(iim, kernel)) {
    scheme_extend_module_rename_with_kernel(rn);
    saw_mb = 1;
  } else {
    int i;
    Scheme_Object **exs, **exss, **exsns, *midx;

    saw_mb = 0;

    exs = iim->provides;
    exsns = iim->provide_src_names;
    exss = iim->provide_srcs;
    for (i = iim->num_provides; i--; ) {
      if (exss && !SCHEME_FALSEP(exss[i]))
	midx = scheme_modidx_shift(exss[i], iim->self_modidx, iidx);
      else
	midx = iidx;
      scheme_extend_module_rename(rn, midx, exs[i], exsns[i]);
      if (SAME_OBJ(exs[i], module_begin_symbol))
	saw_mb = 1;
    }

    if (iim->reprovide_kernel) {
      scheme_extend_module_rename_with_kernel(rn);
      saw_mb = 1;
    }
  }

  /* If fm isn't a single expression, it certainly needs a
     `#%module-begin': */
  if (SCHEME_PAIRP(fm) && SCHEME_STX_NULLP(SCHEME_STX_CDR(fm))) {
    /* Perhaps expandable... */
    fm = SCHEME_STX_CAR(fm);
  } else {
    fm = scheme_make_pair(module_begin_symbol, fm);
    check_mb = 1;
  }

  fm = scheme_datum_to_syntax(fm, form, form, 0, 1);
  fm = scheme_add_rename(fm, rn);
  fm = scheme_add_rename(fm, et_rn);

  if (!check_mb) {
    fm = scheme_check_immediate_macro(fm, menv->init, rec, drec, depth, scheme_false, 0, &mbval);

    /* If expansion is not the primitive `#%module-begin', add local one: */
    if (!SAME_OBJ(mbval, modbeg_syntax)) {
      Scheme_Object *mb;
      mb = scheme_datum_to_syntax(module_begin_symbol, form, form, 0, 1);
      mb = scheme_add_rename(mb, rn);
      mb = scheme_add_rename(mb, et_rn);
      fm = scheme_make_pair(mb, scheme_make_pair(fm, scheme_null));
      fm = scheme_datum_to_syntax(fm, form, form, 0, 1);
      check_mb = 1;
    }
  }

  if (check_mb && !saw_mb) {
    scheme_wrong_syntax("module", NULL, form, 
			"no #%%module-begin binding in the module's language");
  }
  
  if (rec) {
    scheme_compile_rec_done_local(rec, drec);
    fm = scheme_compile_expr(fm, menv->init, rec, drec);

    /* result should be a module body value: */
    if (!SAME_OBJ(fm, (Scheme_Object *)m)) {
      scheme_wrong_syntax("module", NULL, form, "compiled body was not built with #%%module-begin");
    }

    return scheme_make_syntax_compiled(MODULE_EXPD, (Scheme_Object *)m);
  } else {
    Scheme_Object *hints;

    fm = scheme_expand_expr(fm, menv->init, depth, scheme_false);

    hints = m->hints;
    m->hints = NULL;

    fm = cons(module_symbol,
	      cons(nm,
		   cons(ii, cons(fm, scheme_null))));

    fm = scheme_datum_to_syntax(fm, form, form, 0, 1);
    
    if (hints) {
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-variable-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-syntax-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-indirect-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-kernel-reprovide-hint"),
			       SCHEME_CAR(hints));
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-self-path-index"),
			       self_modidx);
    }

    return fm;
  }
}

static Scheme_Object *
module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module(form, env, rec, drec, 1, scheme_false);
}

static Scheme_Object *
module_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  if (depth > 0)
    depth++;

  return do_module(form, env, NULL, 0, depth, boundname);
}

static Scheme_Object *mk_req(Scheme_Object *path, Scheme_Object *self)
{
  if (SCHEME_SYMBOLP(path))
    return path;
  else
    return scheme_make_modidx(path, self, scheme_false);
}

/* The mzc interface: */
Scheme_Object *scheme_declare_module(Scheme_Object *shape, Scheme_Invoke_Proc ivk, Scheme_Invoke_Proc sivk, 
				     void *data, Scheme_Env *env)
{
  Scheme_Module *m;
  Scheme_Object *name, *prefix, *a, *self_modidx;
  Scheme_Object *requires, *et_requires, *kernel_exclusion;
  Scheme_Object *var_provides, *syntax_provides, *ind_provides, **exs, **exss, **exns;
  int nvar, nsyntax, i;

  name = SCHEME_CAR(shape);
  shape = SCHEME_CDR(shape);
  requires = SCHEME_CAR(shape);
  shape = SCHEME_CDR(shape);
  et_requires = SCHEME_CAR(shape);
  shape = SCHEME_CDR(shape);
  var_provides = SCHEME_CAR(shape);
  shape = SCHEME_CDR(shape);
  syntax_provides = SCHEME_CAR(shape);
  shape = SCHEME_CDR(shape);
  ind_provides = SCHEME_CAR(shape);
  shape = SCHEME_CDR(shape);
  kernel_exclusion = SCHEME_CAR(shape);

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->type = scheme_module_type;

  prefix = scheme_get_param(scheme_config, MZCONFIG_CURRENT_MODULE_PREFIX);
  if (SCHEME_SYMBOLP(prefix))
    name = scheme_symbol_append(prefix, name);
  
  m->modname = name;

  self_modidx = scheme_make_modidx(scheme_false, scheme_false, m->modname);

  requires = scheme_named_map_1(NULL, mk_req, requires, self_modidx);
  et_requires = scheme_named_map_1(NULL, mk_req, et_requires, self_modidx);

  m->requires = requires;
  m->et_requires = et_requires;

  m->prim_body = ivk;
  m->prim_et_body = sivk;

  m->body = data;

  nvar = scheme_list_length(var_provides);
  nsyntax = scheme_list_length(syntax_provides);

  exs = MALLOC_N(Scheme_Object *, nvar + nsyntax);
  exss = MALLOC_N(Scheme_Object *, nvar + nsyntax);
  exns = MALLOC_N(Scheme_Object *, nvar + nsyntax);

  var_provides = scheme_append(var_provides, syntax_provides);

  for (i = 0; i < nvar + nsyntax; i++, var_provides = SCHEME_CDR(var_provides)) {
    a = SCHEME_CAR(var_provides);
    if (SCHEME_SYMBOLP(a)) {
      exs[i] = a;
      exns[i] = a;
      exss[i] = scheme_false; /* means "self" */
    } else if (SCHEME_SYMBOLP(SCHEME_CDR(a))) {
      exs[i] = SCHEME_CAR(a);
      exns[i] = SCHEME_CDR(a);
      exss[i] = scheme_false; /* means "self" */
    } else {
      exss[i] = SCHEME_CAR(a);
      a = SCHEME_CDR(a);
      exs[i] = SCHEME_CAR(a);
      exns[i] = SCHEME_CDR(a);
    }
  }

  m->provides = exs;
  m->provide_srcs = exss;
  m->provide_src_names = exns;
  m->num_provides = nvar + nsyntax;
  m->num_var_provides = nvar;

  m->reprovide_kernel = SCHEME_TRUEP(kernel_exclusion);
  m->kernel_exclusion = kernel_exclusion;
  
  nvar = scheme_list_length(ind_provides);
  if (nvar) {
    exs = MALLOC_N(Scheme_Object *, nvar);
    for (i = 0; i < nvar; i++, ind_provides = SCHEME_CDR(ind_provides)) {
      exs[i] = SCHEME_CAR(ind_provides);
    }
  } else
    exs = NULL;

  m->indirect_provides = exs;
  m->num_indirect_provides = nvar;

  m->self_modidx = self_modidx;

  scheme_hash_set(env->module_registry, m->modname, (Scheme_Object *)m);

  return scheme_void;
}

/**********************************************************************/
/*                          #%module-begin                            */
/**********************************************************************/

static void check_require_name(Scheme_Object *name, Scheme_Object *nominal_modidx,
			      Scheme_Object *modidx, Scheme_Object *exname,
			      int isval, void *tables, Scheme_Object *e)
{
  Scheme_Bucket_Table *toplevel, *syntax;
  Scheme_Hash_Table *required;
  Scheme_Object *vec;

  toplevel = ((Scheme_Bucket_Table **)tables)[0];
  required = ((Scheme_Hash_Table **)tables)[1];
  syntax = ((Scheme_Bucket_Table **)tables)[2];
  e = ((Scheme_Object **)tables)[3];

  /* Check that it's not yet defined: */
  if (toplevel) {
    if (scheme_lookup_in_table(toplevel, (const char *)name)) {
      scheme_wrong_syntax("module", name, e, "imported identifier already defined");
    }
  }
	    
  /* Not required, or required from same module: */
  vec = scheme_hash_get(required, name);
  if (vec) {
    if (same_modidx(SCHEME_VEC_ELS(vec)[1], modidx)
	&& SAME_OBJ(SCHEME_VEC_ELS(vec)[2], exname))
      return; /* already required, same source */
    scheme_wrong_syntax("module", name, e, 
			"identifier already imported (from a different source)");
  }
	    
  /* Not syntax: */
  if (syntax) {
    if (scheme_lookup_in_table(syntax, (const char *)name)) {
      scheme_wrong_syntax("module", name, e, "imported identifier already defined");
    }
  }

  /* Remember require: */
  vec = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(vec)[0] = nominal_modidx;
  SCHEME_VEC_ELS(vec)[1] = modidx;
  SCHEME_VEC_ELS(vec)[2] = exname;
  SCHEME_VEC_ELS(vec)[3] = (isval ? scheme_true : scheme_false);
  scheme_hash_set(required, name, vec);
}

static Scheme_Object *stx_sym(Scheme_Object *l, Scheme_Object *form)
{
  return SCHEME_STX_SYM(l);
}

static Scheme_Object *do_module_begin(Scheme_Object *form, Scheme_Comp_Env *env, 
				      Scheme_Compile_Info *rec, int drec,
				      int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *first, *last, *p, *rn, *exp_body, *et_rn, *self_modidx;
  Scheme_Comp_Env *xenv, *cenv, *eenv = NULL;
  Scheme_Hash_Table *et_required; /* just to avoid duplicates */
  Scheme_Hash_Table *required;    /* name -> (vector nominal-modidx modidx srcname var?) */
  Scheme_Hash_Table *provided;    /* exname -> locname */
  Scheme_Object *reprovided;      /* list of (list modidx syntax except-name ...) */
  void *tables[4], *et_tables[4];
  Scheme_Object **exs, **exsns, **exss, **exis, *exclude_hint = scheme_false;
  int excount, exvcount, exicount;
  int num_to_compile;
  int reprovide_kernel;
  Scheme_Compile_Info *recs;
  Scheme_Object *redef_modname;

  if (!scheme_is_module_env(env))
    scheme_wrong_syntax("#%module-begin", NULL, form, "illegal use (not a module body)");

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax("#%module-begin", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");

  /* Redefining a module? */
  redef_modname = env->genv->module->modname;
  if (!scheme_hash_get(env->genv->module_registry, redef_modname))
    redef_modname = NULL;

  /* Expand each expression in form up to `begin', `define-values', `define-syntax', 
     `require', `provide', and `#%app'. */
  xenv = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME | SCHEME_MODULE_FRAME, env);
  {
    Scheme_Object *stop;
    stop = scheme_get_stop_expander();
    scheme_add_local_syntax(9, xenv);
    scheme_set_local_syntax(0, begin_stx, stop, xenv);
    scheme_set_local_syntax(1, define_values_stx, stop, xenv);
    scheme_set_local_syntax(2, define_syntaxes_stx, stop, xenv);
    scheme_set_local_syntax(3, require_stx, stop, xenv);
    scheme_set_local_syntax(4, require_for_syntax_stx, stop, xenv);
    scheme_set_local_syntax(5, provide_stx, stop, xenv);
    scheme_set_local_syntax(6, set_stx, stop, xenv);
    scheme_set_local_syntax(7, app_stx, stop, xenv);
    scheme_set_local_syntax(8, top_stx, stop, xenv);
  }

  first = scheme_null;
  last = NULL;

  required = scheme_make_hash_table(SCHEME_hash_ptr);
  /* Put initial requires into the table: */
  {
    int i, numvals;
    Scheme_Module *iim;
    Scheme_Object *midx, *nmidx, *vec;

    nmidx = SCHEME_CAR(env->genv->module->requires);
    iim = module_load(scheme_module_resolve(nmidx), env->genv, NULL);
    exs = iim->provides;
    exsns = iim->provide_src_names;
    exss = iim->provide_srcs;
    numvals = iim->num_var_provides;
    for (i = iim->num_provides; i--; ) {
      midx = (exss ? exss[i] : nmidx);
      vec = scheme_make_vector(4, NULL);
      SCHEME_VEC_ELS(vec)[0] = nmidx;
      SCHEME_VEC_ELS(vec)[1] = midx;
      SCHEME_VEC_ELS(vec)[2] = exsns[i];
      SCHEME_VEC_ELS(vec)[3] = ((i < numvals) ? scheme_true : scheme_false);
      scheme_hash_set(required, exs[i], vec);
    }

    if (iim->reprovide_kernel) {
      exs = kernel->provides;
      numvals = kernel->num_var_provides;
      for (i = kernel->num_provides; i--; ) {
	if (!SAME_OBJ(iim->kernel_exclusion, exs[i])) {
	  vec = scheme_make_vector(4, NULL);
	  SCHEME_VEC_ELS(vec)[0] = nmidx;
	  SCHEME_VEC_ELS(vec)[1] = kernel_symbol;
	  SCHEME_VEC_ELS(vec)[2] = exs[i];
	  SCHEME_VEC_ELS(vec)[3] = ((i < numvals) ? scheme_true : scheme_false);
	  scheme_hash_set(required, exs[i], vec);
	}
      } 
    }
  }
  rn = env->genv->rename;
  et_rn = env->genv->et_rename;

  tables[0] = env->genv->toplevel;
  tables[1] = required;
  tables[2] = env->genv->syntax;

  et_required = scheme_make_hash_table(SCHEME_hash_ptr);
  et_tables[0] = NULL;
  et_tables[1] = et_required;
  et_tables[2] = NULL;

  provided = scheme_make_hash_table(SCHEME_hash_ptr);
  reprovided = scheme_null;

  exp_body = scheme_null;

  num_to_compile = 0;

  self_modidx = env->genv->module->self_modidx;
  
  /* Partially expand all expressions, and process definitions, requires,
     and provides. Also, flatten top-level `begin' expressions: */
  for (fm = SCHEME_STX_CDR(form); !SCHEME_STX_NULLP(fm); fm = SCHEME_STX_CDR(fm)) {
    Scheme_Object *e;
    int normal;

    while (1) {
      Scheme_Object *fst;

      e = SCHEME_STX_CAR(fm);

      /* -2 means expand all the way (to stops), but preserve letrec-syntax. */
      e = scheme_expand_expr(e, xenv, -2, scheme_false);

      if (SCHEME_STX_PAIRP(e))
	fst = SCHEME_STX_CAR(e);
      else
	fst = NULL;

      if (fst && SCHEME_STX_SYMBOLP(fst) && scheme_stx_module_eq(begin_stx, fst, 0)) {
	if (scheme_stx_proper_list_length(e) < 0)
	  scheme_wrong_syntax("begin (module body)", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
	fst = SCHEME_STX_CDR(e);
	fm = SCHEME_STX_CDR(fm);
	fm = scheme_append(scheme_flatten_syntax_list(fst, NULL), fm);
      } else
	break;
    }
    
    if (SCHEME_STX_PAIRP(e)) {
      Scheme_Object *fst;

      fst = SCHEME_STX_CAR(e);

      if (SCHEME_STX_SYMBOLP(fst)) {

	Scheme_Object *n;
	n = SCHEME_STX_CAR(e);
	if (scheme_stx_module_eq(define_values_stx, fst, 0)) {
	  /************ define-values *************/
	  Scheme_Object *vars, *val;

	  /* Create top-level vars */
	  scheme_define_parse(e, &vars, &val, 0, env);

	  while (SCHEME_STX_PAIRP(vars)) {
	    Scheme_Object *name;
	    name = SCHEME_STX_CAR(vars);
	    name = SCHEME_STX_SYM(name);

	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Not required: */
	    if (scheme_hash_get(required, name)) {
	      scheme_wrong_syntax("module", name, e, "identifier is already imported");
	      return NULL;
	    }

	    /* Not syntax: */
	    if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Create the bucket, indicating that the name will be defined: */
	    scheme_add_global_symbol(name, scheme_undefined, env->genv);

	    /* Add a renaming: */
	    scheme_extend_module_rename(rn, self_modidx, name, name);

	    vars = SCHEME_STX_CDR(vars);
	  }
	
	  normal = 1;
	} else if (scheme_stx_module_eq(define_syntaxes_stx, fst, 0)) {
	  /************ define-syntaxes *************/
	  /* Define the macro: */
	  Scheme_Compile_Info mrec;
	  Scheme_Object *names, *l, *code, *m, *vec, *boundname;
	  int count = 0;

	  scheme_define_parse(e, &names, &code, 1, env);

	  if (SCHEME_STX_PAIRP(names) && SCHEME_STX_NULLP(SCHEME_STX_CDR(names)))
	    boundname = SCHEME_STX_CAR(names);
	  else
	    boundname = scheme_false;
	  
	  names = scheme_named_map_1(NULL, stx_sym, names, NULL);

	  for (l = names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	    Scheme_Object *name;
	    name = SCHEME_CAR(l);

	    if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	      return NULL;
	    }
	    
	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	      return NULL;
	    }

	    /* Not required: */
	    if (scheme_hash_get(required, name)) {
	      scheme_wrong_syntax("module", name, e, "identifier is already imported");
	      return NULL;
	    }

	    count++;
	  }

	  mrec.dont_mark_local_use = 0;
	  mrec.resolve_module_ids = 0;
	  mrec.value_name = NULL;

	  if (!eenv) {
	    scheme_prepare_exp_env(env->genv);
	    eenv = scheme_no_defines(env->genv->exp_env->init);
	  }

	  if (!rec)
	    code = scheme_expand_expr(code, eenv, -1, boundname);
	  m = scheme_compile_expr(code, eenv, &mrec, 0);
	  m = scheme_resolve_expr(m, scheme_resolve_info_create());

	  /* Add code with names and lexical depth to exp-time body: */
	  vec = scheme_make_vector(3, NULL);
	  SCHEME_VEC_ELS(vec)[0] = names;
	  SCHEME_VEC_ELS(vec)[1] = m;
	  SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(mrec.max_let_depth);
	  exp_body = scheme_make_pair(vec, exp_body);
	
	  eval_defmacro(names, count, m, eenv, mrec.max_let_depth, env->genv->syntax);

	  /* Add a renaming for each name: */
	  for (l= names; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	    scheme_extend_module_rename(rn, self_modidx, SCHEME_CAR(l), SCHEME_CAR(l));
	  }

	  if (rec)
	    e = NULL;
	  else {
	    m = SCHEME_STX_CDR(e);
	    m = SCHEME_STX_CAR(m);
	    m = scheme_make_pair(define_syntaxes_stx,
				 scheme_make_pair(m, scheme_make_pair(code, scheme_null)));
	    e = scheme_datum_to_syntax(m, e, e, 0, 1);
	  }
	  normal = 0;
	} else if (scheme_stx_module_eq(require_stx, fst, 0)) {	
	  /************ require *************/
	  Scheme_Object *imods;

	  /* Add requires to renaming: */
	  tables[3] = e;
	  imods = parse_requires(form, e, self_modidx, env->genv, 
				rn, check_require_name, tables, 0,
				redef_modname, 0);
	
	  /* Add required modules to requires list: */
	  for (; !SCHEME_NULLP(imods); imods = SCHEME_CDR(imods)) {
	    Scheme_Object *il, *ilast = NULL;
	    Scheme_Object *idx = SCHEME_CAR(imods);
	  
	    for (il = env->genv->module->requires; SCHEME_PAIRP(il); il = SCHEME_CDR(il)) {
	      if (same_modidx(idx, SCHEME_CAR(il)))
		break;
	      ilast = il;
	    }
	  
	    if (SCHEME_NULLP(il)) {
	      il = scheme_make_pair(idx, scheme_null);
	      SCHEME_CDR(ilast) = il;
	    }
	  }

	  if (rec)
	    e = NULL;
	  normal = 0;
	} else if (scheme_stx_module_eq(require_for_syntax_stx, fst, 0)) {	
	  /************ require-for-syntax *************/
	  Scheme_Object *imods;

	  scheme_prepare_exp_env(env->genv);

	  /* Add requires to renaming: */
	  et_tables[3] = e;
	  imods = parse_requires(form, e, self_modidx, env->genv->exp_env,
				et_rn, check_require_name, et_tables, 1,
				redef_modname, 0);

	  /* Add required modules to et_requires list: */
	  for (; !SCHEME_NULLP(imods); imods = SCHEME_CDR(imods)) {
	    Scheme_Object *il, *ilast = NULL;
	    Scheme_Object *idx = SCHEME_CAR(imods);
	  
	    for (il = env->genv->module->et_requires; SCHEME_PAIRP(il); il = SCHEME_CDR(il)) {
	      if (same_modidx(idx, SCHEME_CAR(il)))
		break;
	      ilast = il;
	    }
	  
	    if (SCHEME_NULLP(il)) {
	      il = scheme_make_pair(idx, scheme_null);
	      if (ilast)
		SCHEME_CDR(ilast) = il;
	      else
		env->genv->module->et_requires = il;
	    }
	  }

	  if (rec)
	    e = NULL;
	  normal = 0;
	} else if (scheme_stx_module_eq(provide_stx, fst, 0)) {
	  /************ provide *************/
	  /* Add provides to table: */
	  Scheme_Object *l;

	  if (scheme_stx_proper_list_length(e) < 0)
	    scheme_wrong_syntax("provide", e, form, "bad syntax (" IMPROPER_LIST_FORM ")");

	  for (l = SCHEME_STX_CDR(e); !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
	    Scheme_Object *a, *midx;

	    a = SCHEME_STX_CAR(l);

	    if (SCHEME_STX_SYMBOLP(a)) {
	      /* <id> */
	      a = SCHEME_STX_VAL(a);
	      if (scheme_hash_get(provided, a))
		scheme_wrong_syntax("provide", a, form, "identifier already provided");
	      /* Provide a: */
	      scheme_hash_set(provided, a, a);
	    } else if (SCHEME_STX_PAIRP(a)) {
	      Scheme_Object *rest;

	      fst = SCHEME_STX_CAR(a);
	      rest = SCHEME_STX_CDR(a);

	      if (SAME_OBJ(rename_symbol, SCHEME_STX_VAL(fst))) {
		/* (rename <id> <id>) */
		Scheme_Object *inm, *enm;

		if (!SCHEME_STX_PAIRP(rest)
		    || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
		  scheme_wrong_syntax("provide", a, form, "bad syntax");
		inm = SCHEME_STX_CAR(rest);
		rest = SCHEME_STX_CDR(rest);
		enm = SCHEME_STX_CAR(rest);
		if (!SCHEME_STX_SYMBOLP(inm))
		  scheme_wrong_syntax("provide", a, form, "bad syntax (internal name is not an identifier)");
		if (!SCHEME_STX_SYMBOLP(enm))
		  scheme_wrong_syntax("provide", a, form, "bad syntax (external name is not an identifier)");
		rest = SCHEME_CDR(rest);
		if (!SCHEME_STX_NULLP(rest))
		  scheme_wrong_syntax("provide", a, form, "bad syntax (data following external name)");
		
		inm = SCHEME_STX_VAL(inm);
		enm = SCHEME_STX_VAL(enm);
		
		if (scheme_hash_get(provided, enm))
		  scheme_wrong_syntax("provide", enm, a, "identifier already provided");
		/* Provide enm: */
		scheme_hash_set(provided, enm, inm);
	      } else if (SAME_OBJ(all_from_symbol, SCHEME_STX_VAL(fst))) {
		/* (all-from <modname>) */
		if (!SCHEME_STX_PAIRP(rest))
		  scheme_wrong_syntax("provide", a, form, "bad syntax");
		if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
		  scheme_wrong_syntax("provide", a, form, "bad syntax (data following all keyword)");
		
		midx = SCHEME_STX_CAR(rest);
		midx = scheme_make_modidx(scheme_syntax_to_datum(midx, 0, NULL),
					  self_modidx,
					  scheme_false);
		
		reprovided = scheme_make_pair(scheme_make_pair(midx, scheme_make_pair(a, scheme_null)), 
					      reprovided);
	      } else if (SAME_OBJ(all_from_except_symbol, SCHEME_STX_VAL(fst))) {
		/* (all-from-except <modname> <id> ...) */
		Scheme_Object *exns, *el;
		
		if (scheme_stx_proper_list_length(a) < 0)
		  scheme_wrong_syntax("provide", e, form, "bad syntax (" IMPROPER_LIST_FORM ")");
		
		midx = SCHEME_STX_CAR(rest);
		midx = scheme_make_modidx(scheme_syntax_to_datum(midx, 0, NULL),
					  self_modidx,
					  scheme_false);
		exns = SCHEME_STX_CDR(rest);
		
		/* Check all exclusions are identifiers: */
		for (el = exns; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
		  p = SCHEME_STX_CAR(el);
		  if (!SCHEME_STX_SYMBOLP(p)) {
		    scheme_wrong_syntax("provide", p, a,
					"bad syntax (excluded name is not an identifier)");
		  }
		}
		
		reprovided = scheme_make_pair(scheme_make_pair(midx, scheme_make_pair(a, exns)), 
					      reprovided);
	      } else if (SAME_OBJ(struct_symbol, SCHEME_STX_VAL(fst))) {
		/* (struct <id> (<id> ...)) */
		int len, i;
		Scheme_Object *base, *fields, *el, **names;
		
		len = scheme_stx_proper_list_length(rest);
		if (len != 2) {
		  if (len < 0)
		    scheme_wrong_syntax("provide", e, form, "bad syntax (" IMPROPER_LIST_FORM ")");
		  else
		    scheme_wrong_syntax("provide", e, form, "bad syntax "
					"(not a struct identifier followed by "
					"a sequence of field identifiers)");
		}
		
		base = SCHEME_STX_CAR(rest);
		fields = SCHEME_STX_CDR(rest);
		fields = SCHEME_STX_CAR(fields);
		
		if (!SCHEME_STX_SYM(base))
		  scheme_wrong_syntax("provide", base, a,
				      "bad syntax (struct name is not an identifier)");

		/* Check all field names are identifiers: */
		for (el = fields; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
		  p = SCHEME_STX_CAR(el);
		  if (!SCHEME_STX_SYMBOLP(p)) {
		    scheme_wrong_syntax("provide", p, a,
					"bad syntax (field name is not an identifier)");
		  }
		}
		if (!SCHEME_STX_NULLP(el))
		  scheme_wrong_syntax("provide", fields, a,
				      "bad syntax (" IMPROPER_LIST_FORM ")");
		
		base = SCHEME_STX_VAL(base);
		fields = scheme_syntax_to_datum(fields, 0, NULL);

		names = scheme_make_struct_names(base, fields, 0, &len);

		for (i = 0; i < len; i++) {
		  if (scheme_hash_get(provided, names[i]))
		    scheme_wrong_syntax("provide", names[i], form, "identifier already provided");
		  scheme_hash_set(provided, names[i], names[i]);
		}
	      } else {
		scheme_wrong_syntax("provide", a, form, NULL);
	      }
	    } else {
	      scheme_wrong_syntax("provide", a, form, NULL);
	    }
	  }

	  if (rec)
	    e = NULL;
	  normal = 0;
	} else
	  normal = 1;
      } else
	normal = 1;
    } else
      normal = 1;

    if (e) {
      p = scheme_make_pair(scheme_make_pair(e, normal ? scheme_true : scheme_false), scheme_null);
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
    }

    if (normal)
      num_to_compile++;
  }
  /* first =  a list of (cons semi-expanded-expression normal?) */

  eenv = NULL;

  cenv = scheme_extend_as_toplevel(env);

  if (rec) {
    recs = MALLOC_N_RT(Scheme_Compile_Info, num_to_compile);
    scheme_init_compile_recs(rec, drec, recs, num_to_compile);
    num_to_compile = 0;
  } else
    recs = NULL;

  for (p = first; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
    Scheme_Object *e;
    int normal;

    e = SCHEME_CAR(p);
    normal = SCHEME_TRUEP(SCHEME_CDR(e));
    e = SCHEME_CAR(e);
    if (normal) {
      if (rec) {
	recs[num_to_compile].resolve_module_ids = 0;
	e = scheme_compile_expr(e, cenv, recs, num_to_compile++);
      } else
	e = scheme_expand_expr(e, cenv, depth, scheme_false);
    }
    SCHEME_CAR(p) = e;
  }
  /* first =  a list of expanded/compiled expressions */

  if (rec) {
    scheme_merge_compile_recs(rec, drec, recs, num_to_compile);
  }

  scheme_clean_dead_env(env->genv);

  /* If compiling, drop expressions that are constants: */
  if (rec) {
    Scheme_Object *prev = NULL, *next;
    for (p = first; !SCHEME_NULLP(p); p = next) {
      next = SCHEME_CDR(p);
      if (scheme_omittable_expr(SCHEME_CAR(p))) {
	if (prev)
	  SCHEME_CDR(p) = next;
	else
	  first = next;
      } else
	prev = p;
    }
  }

  /* Compute provides for re-provides: */
  {
    int i;
    Scheme_Object *rx;

    reprovide_kernel = 0;

    /* First, check the sanity of the re-provide specifications: */
    for (rx = reprovided; !SCHEME_NULLP(rx); rx = SCHEME_CDR(rx)) {
      Scheme_Object *midx = SCHEME_CAR(SCHEME_CAR(rx)), *l, *exns;
	
      for (l = env->genv->module->requires; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	if (same_modidx(midx, SCHEME_CAR(l)))
	  break;
      }
      if (SCHEME_NULLP(l)) {
	/* Didn't require the named module */
	scheme_wrong_syntax("provide", midx, ((Scheme_Modidx *)midx)->path,
			    "no `require' matching the module name");
      }

      exns = SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(rx)));
      for (l = exns; !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
	/* Make sure excluded name was required: */
	Scheme_Object *a;
	a = SCHEME_STX_VAL(SCHEME_STX_CAR(l));
	if (!scheme_hash_get(required, a)) {
	  /* FIXME: check source of require */
	  a = SCHEME_STX_CAR(l);
	  scheme_wrong_syntax("provide", a, SCHEME_CAR(SCHEME_CAR(rx)),
			      "excluded name was not required");
	}
      }
    }

    /* Walk through requires, check for re-providing: */
    for (i = required->size; i--; ) {
      if (required->vals[i]) {
	Scheme_Object *nominal_modidx, *name, *modidx, *srcname;

	name = required->keys[i];
	nominal_modidx = SCHEME_VEC_ELS(required->vals[i])[0];
	modidx = SCHEME_VEC_ELS(required->vals[i])[1];
	srcname = SCHEME_VEC_ELS(required->vals[i])[2];

	for (rx = reprovided; !SCHEME_NULLP(rx); rx = SCHEME_CDR(rx)) {
	  if (same_modidx(SCHEME_CAR(SCHEME_CAR(rx)), nominal_modidx)) {
	    Scheme_Object *exns, *ree;
	    
	    ree = SCHEME_CDR(SCHEME_CAR(rx));

	    exns = SCHEME_CDR(ree);
	    if (SAME_OBJ(modidx, kernel_symbol))
	      if (!SCHEME_STX_NULLP(exns))
		exclude_hint = exns;
	    
	    for (; !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
	      /* Make sure excluded name was required: */
	      Scheme_Object *a;
	      a = SCHEME_STX_VAL(SCHEME_STX_CAR(exns));
	      if (SAME_OBJ(a, name))
		break;
	    }

	    if (SCHEME_STX_NULLP(exns)) {
	      /* Not excluded, so provide it. */
	      if (scheme_hash_get(provided, name))
		scheme_wrong_syntax("provide", name, SCHEME_CAR(ree), "identifier already provided");
	      
	      scheme_hash_set(provided, name, name);

	      if (SAME_OBJ(modidx, kernel_symbol) && SAME_OBJ(name, srcname))
		reprovide_kernel++;
	    }
	  }
	}
      }
    }
  }

  /* Ad hoc optimization: mzscheme is everything from kernel except
     #%module_begin */
  if ((reprovide_kernel == (kernel->num_provides - 1))
      && SCHEME_FALSEP(exclude_hint)) {
    exclude_hint = scheme_make_pair(module_begin_symbol, scheme_null);
    exclude_hint = scheme_datum_to_syntax(exclude_hint, scheme_false, top_stx, 0, 0);
  }

  /* Re-providing all of the kernel without prefixing? */
  if (reprovide_kernel) {
    if ((reprovide_kernel == (kernel->num_provides - 1))
	&& SCHEME_TRUEP(exclude_hint)) {
      if (SCHEME_STX_PAIRP(exclude_hint) && SCHEME_NULLP(SCHEME_STX_CDR(exclude_hint))) {
	Scheme_Object *n;

	exclude_hint = SCHEME_STX_CAR(exclude_hint);
	exclude_hint = SCHEME_STX_VAL(exclude_hint);
	n = scheme_hash_get(provided, exclude_hint);
	if (n) {
	  /* may be a single shadowed exclusion, now bound to exclude_hint... */
	  n = scheme_hash_get(required, n);
	  if (n && !SAME_OBJ(SCHEME_VEC_ELS(n)[1], kernel_symbol)) {
	    /* there is a single shadowed exclusion. */
	  } else
	    reprovide_kernel = 0;
	} else
	  reprovide_kernel = 0;
      } else
	reprovide_kernel = 0;
    } else if (reprovide_kernel != kernel->num_provides)
      reprovide_kernel = 0;
    else
      exclude_hint = scheme_false;
  }
  /* If reprovide_kernel is non-zero, we re-provide all of it */

  /* Compute provide arrays */
  {
    int i, count;
    
    for (count = 0, i = provided->size; i--; ) {
      if (provided->vals[i])
	count++;
    }
    
    count -= reprovide_kernel;

    exs = MALLOC_N(Scheme_Object *, count);
    exsns = MALLOC_N(Scheme_Object *, count);
    exss = MALLOC_N(Scheme_Object *, count);

    /* Do non-syntax first. */
    for (count = 0, i = provided->size; i--; ) {
      if (provided->vals[i]) {
	Scheme_Object *name, *v;
	  
	name = provided->vals[i];

	if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	  /* Defined locally */
	  exs[count] = provided->keys[i];
	  exsns[count] = name;
	  exss[count] = scheme_false; /* means "self" */
	  count++;
	} else if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	  /* Skip for now. */
	} else if ((v = scheme_hash_get(required, name))) {
	  /* Required */
	  if (SCHEME_TRUEP(SCHEME_VEC_ELS(v)[3])) {
	    /* If this is a kernel re-provide, don't provide after all. */
	    if (reprovide_kernel
		&& SAME_OBJ(SCHEME_VEC_ELS(v)[1], kernel_symbol)
		&& SAME_OBJ(provided->keys[i], SCHEME_VEC_ELS(v)[2])) {
	      /* skip */
	    } else {
	      exs[count] = provided->keys[i];
	      exsns[count] = SCHEME_VEC_ELS(v)[2];
	      exss[count] = SCHEME_VEC_ELS(v)[1];
	      count++;
	    }
	  }
	} else {
	  /* Not defined! */
	  scheme_wrong_syntax("module", name, form, "provided identifier not defined or imported");
	}
      }
    }

    exvcount = count;

    for (i = provided->size; i--; ) {
      if (provided->vals[i]) {
	Scheme_Object *name, *v;
	  
	name = provided->vals[i];

	if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	  /* Defined locally */
	  exs[count] = provided->keys[i];
	  exsns[count] = name;
	  exss[count] = scheme_false; /* means "self" */
	  count++;
	} else if ((v = scheme_hash_get(required, name))) {
	  /* Required */
	  if (SCHEME_FALSEP(SCHEME_VEC_ELS(v)[3])) {
	    /* If this is a kernel re-provide, don't provide after all. */
	    if (reprovide_kernel
		&& SAME_OBJ(SCHEME_VEC_ELS(v)[1], kernel_symbol)
		&& SAME_OBJ(provided->keys[i], SCHEME_VEC_ELS(v)[2])) {
	      /* skip */
	    } else {
	      exs[count] = provided->keys[i];
	      exsns[count] = SCHEME_VEC_ELS(v)[2];
	      exss[count] = SCHEME_VEC_ELS(v)[1];
	      count++;
	    }
	  }
	}
      }
    }

    excount = count;
  }

  /* Compute indirect provides (which is everything at the top-level): */
  {
    int i, count, j;
    Scheme_Bucket **bs, *b;
    
    bs = env->genv->toplevel->buckets;
    for (count = 0, i = env->genv->toplevel->size; i--; ) {
      b = bs[i];
      if (b && b->val)
	count++;
    }

    exis = MALLOC_N(Scheme_Object *, count);

    for (count = 0, i = env->genv->toplevel->size; i--; ) {
      b = bs[i];
      if (b && b->val) {
	Scheme_Object *name;
	  
	name = (Scheme_Object *)b->key;
	
	/* If the name is directly provided, no need for indirect... */
	for (j = 0; j < exvcount; j++) {
	  if (SAME_OBJ(name, exsns[j]))
	    break;
	}
	
	if (j == exvcount)
	  exis[count++] = name;
      }
    }

    exicount = count;
  }

  if (!rec) {
    /* Produce annotations (in the form of properties)
       for module information:
         'module-variable-provides = '(export-var-item ...)
         'module-syntax-provides = '(export-var-item ...)
	 'module-indirect-provides = '(id ...)
         'module-kernel-reprovide-hint = 'kernel-reexport

      item = name
           | (ext-id . def-id)
           | (modidx ext-id . def-id)
     kernel-reexport = #f
                     | #t
                     | exclusion-id
    */
    int j;
    Scheme_Object *e, *a, *result;

    result = scheme_null;

    /* kernel re-export info: */
    if (reprovide_kernel) {
      if (exclude_hint)
	result = scheme_make_pair(exclude_hint, result);
      else
	result = scheme_make_pair(scheme_true, result);
    } else
      result = scheme_make_pair(scheme_false, result);

    /* Indirect provides */ 
    a = scheme_null;
    for (j = 0; j < exicount; j++) {
      a = scheme_make_pair(exis[j], a);
    }
    result = scheme_make_pair(a, result);
    
    /* add syntax and value exports: */
    for (j = 0; j < 2; j++) {
      int top, i;

      e = scheme_null;

      if (reprovide_kernel) {
	if (!j) {
	  i = kernel->num_var_provides;
	  top = kernel->num_provides;
	} else {
	  i = 0;
	  top = kernel->num_var_provides;
	}

	for (; i < top; i++) {
	  if (!SAME_OBJ(kernel->provides[i], exclude_hint)) {
	    a = scheme_make_pair(kernel->provides[i], kernel->provides[i]);
	    a = scheme_make_pair(kernel_symbol, a);
	    e = scheme_make_pair(a, e);
	  }
	}
      }

      if (!j) {
	i = exvcount;
	top = excount;
      } else {
	i = 0;
	top = exvcount;
      }
      
      for (; i < top; i++) {
	if (SCHEME_FALSEP(exss[i])
	    && SAME_OBJ(exs[i], exsns[i]))
	  a = exs[i];
	else {
	  a = scheme_make_pair(exs[i], exsns[i]);
	  if (!SCHEME_FALSEP(exss[i])) {
	    a = scheme_make_pair(exss[i], a);
	  }
	}
	e = scheme_make_pair(a, e);
      }
      result = scheme_make_pair(e, result);
    }

    env->genv->module->hints = result;
  }

  if (rec) {
    Scheme_Object *exp_body_r = scheme_null;
    
    /* Reverse exp_body */
    while (!SCHEME_NULLP(exp_body)) {
      exp_body_r = scheme_make_pair(SCHEME_CAR(exp_body),
				    exp_body_r);
      exp_body = SCHEME_CDR(exp_body);
    }

    env->genv->module->body = first;
    env->genv->module->et_body = exp_body_r;

    /* Install final provides: */
    env->genv->module->num_provides = excount;
    env->genv->module->num_var_provides = exvcount;
    env->genv->module->provides = exs;
    env->genv->module->provide_src_names = exsns;
    env->genv->module->provide_srcs = exss;

    env->genv->module->reprovide_kernel = reprovide_kernel;
    env->genv->module->kernel_exclusion = exclude_hint;

    env->genv->module->indirect_provides = exis;
    env->genv->module->num_indirect_provides = exicount;

    return (Scheme_Object *)env->genv->module;
  } else {
    p = SCHEME_STX_CAR(form);
    return scheme_datum_to_syntax(cons(p, first), form, form, 0, 1);
  }
}

static Scheme_Object *
module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module_begin(form, env, rec, drec, 1, scheme_false);
}

static Scheme_Object *
module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_module_begin(form, env, NULL, 0, depth, boundname);
}

/**********************************************************************/
/*                         top-level require                           */
/**********************************************************************/

Scheme_Object *parse_requires(Scheme_Object *form, Scheme_Object *ll, 
			      Scheme_Object *base_modidx,
			      Scheme_Env *env,
			      Scheme_Object *rn,
			      Check_Func ck, void *data,
			      int start, Scheme_Object *redef_modname,
			      int unpack_kern) 
{
  Scheme_Module *m;
  int j, var_count, is_kern;
  Scheme_Object **exs, **exsns, **exss;
  Scheme_Object *idxstx, *idx, *name, *i, *exns, *prefix, *iname, *ename, *aa;
  Scheme_Object *imods;

  imods = scheme_null;

  if (scheme_stx_proper_list_length(ll) < 0)
    scheme_wrong_syntax("require", SAME_OBJ(form, ll) ? NULL : ll, form, 
			"bad syntax (" IMPROPER_LIST_FORM ")");
  
  
  for (ll = SCHEME_STX_CDR(ll); !SCHEME_STX_NULLP(ll); ll = SCHEME_STX_CDR(ll)) {
    i = SCHEME_STX_CAR(ll);
    iname = ename = NULL;
    if (SCHEME_STX_PAIRP(i))
      aa = SCHEME_STX_CAR(i);
    else
      aa = NULL;

    if (aa && SAME_OBJ(prefix_symbol, SCHEME_STX_VAL(aa))) {
      int len;

      len = scheme_stx_proper_list_length(i);
      if (len != 3) {
	GC_CAN_IGNORE const char *reason;
	
	if (len < 0)
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else if (len < 2)
	  reason = "bad syntax (prefix missing)";
	else if (len < 3)
	  reason = "bad syntax (module name missing)";
	else
	  reason = "bad syntax (extra data after module name)";
	scheme_wrong_syntax("require", i, form, reason);
	return NULL;
      }

      i = SCHEME_STX_CDR(i);
      prefix = SCHEME_STX_CAR(i);
      i = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(i);
      exns = NULL;

      if (!SCHEME_SYMBOLP(SCHEME_STX_VAL(prefix))) {
	scheme_wrong_syntax("require", prefix, form, "bad prefix (not an identifier)");
	return NULL;
      }

      prefix = SCHEME_STX_VAL(prefix);

    } else if (aa && SAME_OBJ(all_except_symbol, SCHEME_STX_VAL(aa))) {
      Scheme_Object *l;
      int len;

      len = scheme_stx_proper_list_length(i);
      if (len < 0)
	scheme_wrong_syntax("require", i, form, "bad syntax (" IMPROPER_LIST_FORM ")");
      else if (len < 2)
	scheme_wrong_syntax("require", i, form, "bad syntax (module name missing)");

      idxstx = SCHEME_STX_CDR(i);      
      idxstx = SCHEME_STX_CAR(idxstx);

      prefix = NULL;
      exns = SCHEME_STX_CDR(i);
      exns = SCHEME_STX_CDR(exns);

      for (l = exns; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(l))) {
	  l = SCHEME_STX_CAR(l);
	  scheme_wrong_syntax("require", l, form,
			      "bad syntax (excluded name is not an identifier)");
	}
      }
    } else if (aa && SAME_OBJ(rename_symbol, SCHEME_STX_VAL(aa))) {
      int len;
      Scheme_Object *rest;

      len = scheme_stx_proper_list_length(i);
      if (len != 4) {
	GC_CAN_IGNORE const char *reason;
	
	if (len < 0)
	  reason = "bad syntax (" IMPROPER_LIST_FORM ")";
	else if (len < 2)
	  reason = "bad syntax (module name missing)";
	else if (len < 3)
	  reason = "bad syntax (internal name missing)";
	else if (len < 4)
	  reason = "bad syntax (external name missing)";
	else
	  reason = "bad syntax (extra data after external name)";
	scheme_wrong_syntax("require", i, form, reason);
	return NULL;
      }

      rest = SCHEME_STX_CDR(i);
      idxstx = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      iname = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      ename = SCHEME_STX_CAR(rest);

      if (!SCHEME_STX_SYMBOLP(iname))
	scheme_wrong_syntax("require", i, form, "bad syntax (internal name is not an identifier)");
      if (!SCHEME_STX_SYMBOLP(ename))
	scheme_wrong_syntax("require", i, form, "bad syntax (external name is not an identifier)");

      iname = SCHEME_STX_VAL(iname);
      ename = SCHEME_STX_VAL(ename);
      
      prefix = NULL;
      exns = NULL;
    } else {
      idxstx = i;
      exns = NULL;
      prefix = NULL;
    }

    idx = scheme_make_modidx(scheme_syntax_to_datum(idxstx, 0, NULL), 
			     base_modidx,
			     scheme_false);

    name = _module_resolve(idx, idxstx);

    m = module_load(name, env, NULL);
    if (redef_modname)
      prevent_cyclic_requires(m, redef_modname, env);

    if (start)
      start_module(m, env, 0, idx);
    else
      expstart_module(m, env, 0, idx);

    is_kern = (SAME_OBJ(idx, kernel_symbol)
	       && !exns
	       && !prefix
	       && !iname
	       && !unpack_kern);

    while (1) { /* loop to handle kernel re-provides... */

      /* Add name to require list, if it's not there: */
      {
	Scheme_Object *l, *last = NULL, *p;
	for (l = imods; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	  if (same_modidx(SCHEME_CAR(l), idx))
	    break;
	  last = l;
	}
	if (SCHEME_NULLP(l)) {
	  p = scheme_make_pair(idx, scheme_null);
	  if (last)
	    SCHEME_CDR(last) = p;
	  else
	    imods = p;
	}
      }
      
      exs = m->provides;
      exsns = m->provide_src_names;
      exss = m->provide_srcs;
      var_count = m->num_var_provides;

      for (j = m->num_provides; j--; ) {
	Scheme_Object *modidx;
	
	if (ename) {
	  if (!SAME_OBJ(ename, exs[j]))
	    continue;  /* we don't want this one. */
	} else if (exns) {
	  if (SCHEME_STX_PAIRP(exns)) {
	    Scheme_Object *l;
	    for (l = exns; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	      if (SAME_OBJ(SCHEME_STX_VAL(SCHEME_STX_CAR(l)), exs[j]))
		break;
	    }
	    if (!SCHEME_STX_NULLP(l))
	      continue; /* we don't want this one. */
	  } else {
	    if (SAME_OBJ(exns, exs[j]))
	      continue; /* we don't want this one. */
	  }
	}
	
	modidx = ((exss && !SCHEME_FALSEP(exss[j])) 
		  ? scheme_modidx_shift(exss[j], m->self_modidx, idx)
		  : idx);
      
	if (!iname)
	  iname = exs[j];

	if (prefix)
	  iname = scheme_symbol_append(prefix, iname);
	
	if (ck)
	  ck(iname, idx, modidx, exsns[j], (j < var_count), data, i);
	
	if (!is_kern) {
	  if (scheme_starting_up && start && (j < var_count) && !env->module && !env->phase) {
	    /* Kindof a hack: during start-up, we remap import of variables to
	       value copy. */
	    Scheme_Env *menv;
	    Scheme_Object *val;
	    menv = scheme_module_access(modidx, env);
	    val = scheme_lookup_in_table(menv->toplevel, (char *)exsns[j]);
	    scheme_add_global_symbol(iname, val, env);
	  } else
	    scheme_extend_module_rename(rn, modidx, iname, exsns[j]);
	}

	iname = NULL;
	
	if (ename) {
	  ename = NULL;
	  break;
	}
      }

      if (ename) {
	scheme_wrong_syntax("require", i, form, "no such provided variable");
	return NULL;
      }
      
      if (is_kern)
	scheme_extend_module_rename_with_kernel(rn);

      if (m->reprovide_kernel) {
	idx = kernel_symbol;
	exns = m->kernel_exclusion;
	m = kernel;
	is_kern = !prefix && !unpack_kern;
	iname = NULL;
	ename = NULL;
      } else
	break;
    }
  }

  return imods;
}

static void check_dup_require(Scheme_Object *name, Scheme_Object *nominal_modidx, 
			     Scheme_Object *modidx, Scheme_Object *srcname, 
			     int isval, void *ht, Scheme_Object *e)
{
  Scheme_Object *i;

  if (ht) {
    i = scheme_hash_get((Scheme_Hash_Table *)ht, name);

    if (i) {
      if (same_modidx(modidx, SCHEME_CAR(i)) && SAME_OBJ(srcname, SCHEME_CDR(i)))
	return; /* same source */
      scheme_wrong_syntax("require", name, e, "duplicate import identifier");
    } else
      scheme_hash_set((Scheme_Hash_Table *)ht, name, scheme_make_pair(modidx, srcname));
  }
}

static Scheme_Object *
top_level_require_execute(Scheme_Object *data)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *rn;
  Scheme_Object *form = SCHEME_CDR(SCHEME_CAR(data)), *rest, *brn;
  int for_exp = (SCHEME_TRUEP(SCHEME_CAR(SCHEME_CAR(data))) ? 1 : 0);
  Scheme_Env *env = (Scheme_Env *)SCHEME_CDR(data);

  if (for_exp) {
    scheme_prepare_exp_env(env);
    env = env->exp_env;
  }

  /* Don't check for dups if we import from less that two sources: */
  rest = SCHEME_STX_CDR(form);
  if (SCHEME_STX_NULLP(rest)) {
    rest = NULL;
  } else if (SCHEME_STX_PAIRP(rest)) {
    rest = SCHEME_STX_CDR(rest);
    if (SCHEME_STX_NULLP(rest)) {
      rest = NULL;
    }
  }

  if (rest)
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
  else
    ht = NULL;

  rn = scheme_make_module_rename(for_exp, 1);

  (void)parse_requires(form, form, scheme_false, env, rn, 
		      check_dup_require, ht, 1, NULL,
		      !env->module);

  brn = env->rename;
  if (!brn) {
    brn = scheme_make_module_rename(for_exp, 1);
    env->rename = brn;
  }

  scheme_append_module_rename(rn, brn);

  return scheme_void;
}

static Scheme_Object *
top_level_require_link(Scheme_Object *data, Link_Info *link)
{
  return scheme_make_syntax_linked(REQUIRE_EXPD, cons(data, (Scheme_Object *)link));
}

static Scheme_Object *
top_level_require_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  return scheme_make_syntax_resolved(REQUIRE_EXPD, data);
}

static Scheme_Object *do_require(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname,
				int for_exp)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *rn;
  char *name;
  Scheme_Env *genv;

  name = for_exp ? "require-for-syntax" : "require";

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(name, NULL, form, "not at top-level or in module body");

  /* If we get here, it must be a top-level require. */

  /* Hash table is for checking duplicate names in require list: */
  ht = scheme_make_hash_table(SCHEME_hash_ptr);

  rn = scheme_make_module_rename(for_exp, 1);

  genv = env->genv;
  if (for_exp) {
    scheme_prepare_exp_env(genv);
    genv = genv->exp_env;
  }

  (void)parse_requires(form, form, scheme_false, genv, rn, 
		      check_dup_require, ht, 0, 
		      NULL, 0);

  if (rec) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_make_syntax_compiled(REQUIRE_EXPD, 
				       cons((for_exp 
					     ? scheme_true 
					     : scheme_false),
					    form));
  } else
    return form;
}

static Scheme_Object *
require_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec, 1, scheme_false, 0);
}

static Scheme_Object *
require_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_require(form, env, NULL, 0, depth, boundname, 0);
}

static Scheme_Object *
require_for_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_require(form, env, rec, drec, 1, scheme_false, 1);
}

static Scheme_Object *
require_for_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_require(form, env, NULL, 0, depth, boundname, 1);
}

/**********************************************************************/
/*                            dummy forms                             */
/**********************************************************************/

static Scheme_Object *
provide_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  scheme_wrong_syntax("provide", NULL, form, "not in module body");
  return NULL;
}

static Scheme_Object *
provide_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  scheme_wrong_syntax("provide", NULL, form, "not in module body");
  return NULL;
}

/**********************************************************************/
/*                        marshal/unmarshal                           */
/**********************************************************************/

static Scheme_Object *write_module(Scheme_Object *obj)
{
  Scheme_Module *m = (Scheme_Module *)obj;
  Scheme_Object *l, *v;
  int i, count;

  l = m->et_requires;
  l = cons(m->requires, l);

  l = cons(m->body, l);
  l = cons(m->et_body, l);

  l = cons(scheme_make_integer(m->num_provides), l);
  l = cons(scheme_make_integer(m->num_var_provides), l);

  count = m->num_provides;

  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->provides[i];
  }
  l = cons(v, l);
  
  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->provide_srcs[i];
  }
  l = cons(v, l);
  
  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->provide_src_names[i];
  }
  l = cons(v, l);
  
  l = cons(scheme_make_integer(m->num_indirect_provides), l);

  count = m->num_indirect_provides;

  v = scheme_make_vector(count, NULL);
  for (i = 0; i < count; i++) {
    SCHEME_VEC_ELS(v)[i] = m->indirect_provides[i];
  }
  l = cons(v, l);

  l = cons(m->reprovide_kernel ? scheme_true : scheme_false, l);
  l = cons(m->kernel_exclusion, l);

  l = cons(m->self_modidx, l);
  l = cons(m->modname, l);

  return l;
}

static Scheme_Object *read_module(Scheme_Object *obj)
{
  Scheme_Module *m;
  Scheme_Object *ie, *nie, *prefix;
  Scheme_Object *esn, *es, *e, *nve, *ne, **v;
  int i, count;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->type = scheme_module_type;
  m->modname = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  prefix = scheme_get_param(scheme_config, MZCONFIG_CURRENT_MODULE_PREFIX);
  if (SCHEME_SYMBOLP(prefix)) {
    prefix = scheme_symbol_append(prefix, m->modname);
    m->modname = prefix;
  }

  m->self_modidx = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  ((Scheme_Modidx *)m->self_modidx)->resolved = m->modname;

  m->kernel_exclusion = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  m->reprovide_kernel = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  ie = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  nie = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  count = SCHEME_INT_VAL(nie);

  v = MALLOC_N(Scheme_Object *, count);
  for (i = 0; i < count; i++) {
    v[i] = SCHEME_VEC_ELS(ie)[i];
  }
  m->indirect_provides = v;
  m->num_indirect_provides = count;

  esn = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  es = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  e = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  nve = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  ne = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  count = SCHEME_INT_VAL(ne);
  m->num_provides = count;
  m->num_var_provides = SCHEME_INT_VAL(nve);

  v = MALLOC_N(Scheme_Object *, count);
  for (i = 0; i < count; i++) {
    v[i] = SCHEME_VEC_ELS(e)[i];
  }
  m->provides = v;

  v = MALLOC_N(Scheme_Object *, count);
  for (i = 0; i < count; i++) {
    v[i] = SCHEME_VEC_ELS(es)[i];
  }
  m->provide_srcs = v;

  v = MALLOC_N(Scheme_Object *, count);
  for (i = 0; i < count; i++) {
    v[i] = SCHEME_VEC_ELS(esn)[i];
  }
  m->provide_src_names = v;

  e = scheme_copy_list(SCHEME_CAR(obj));
  m->et_body = e;
  obj = SCHEME_CDR(obj);
  e = scheme_copy_list(SCHEME_CAR(obj));
  m->body = e;
  obj = SCHEME_CDR(obj);

  e = scheme_copy_list(SCHEME_CAR(obj));
  m->requires = e;
  obj = SCHEME_CDR(obj);
  e = scheme_copy_list(obj);
  m->et_requires = e;

  return (Scheme_Object *)m;
}
