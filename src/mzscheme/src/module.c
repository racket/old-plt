/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt
 
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

#include "schpriv.h"

/* FIXME: stack overflow in ok_for_expansion */

/* globals */
Scheme_Object *scheme_sys_wraps;

/* locals */
static Scheme_Object *module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *import_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *import_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *import_for_expansion_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *import_for_expansion_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *export_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *export_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

#define cons scheme_make_pair

static Scheme_Object *kernel_symbol;
static Scheme_Env *kernel;

static Scheme_Object *module_symbol;
static Scheme_Object *module_begin_symbol;

static Scheme_Object *map_symbol;
static Scheme_Object *rename_symbol;
static Scheme_Object *all_except_symbol;
static Scheme_Object *all_from_symbol;
static Scheme_Object *all_from_except_symbol;

static Scheme_Object *begin_stx;
static Scheme_Object *define_values_stx;
static Scheme_Object *define_syntax_stx;
static Scheme_Object *import_stx;
static Scheme_Object *import_for_expansion_stx;
static Scheme_Object *export_stx;
static Scheme_Object *set_stx;
static Scheme_Object *app_stx;
static Scheme_Object *unbound_stx;

static Scheme_Object *unbound_expander;

typedef void (*Check_Func)(Scheme_Object *name, 
			   Scheme_Object *modname, Scheme_Object *srcname, 
			   int kind, void *data, Scheme_Object *e);
static Scheme_Object *parse_imports(Scheme_Object *form, Scheme_Object *l, 
				    Scheme_Env *env, Scheme_Object *rn,
				    Check_Func ck, void *data,
				    int start);

void scheme_init_module(Scheme_Env *env)
{
  scheme_add_global_keyword("module", 
			    scheme_make_compiled_syntax(module_syntax, 
							module_expand), 
			    env);
  scheme_add_global_keyword("#%module-begin", 
			    scheme_make_compiled_syntax(module_begin_syntax, 
							module_begin_expand), 
			    env);

  scheme_add_global_keyword("import", 
			    scheme_make_compiled_syntax(import_syntax, 
							import_expand), 
			    env);
  scheme_add_global_keyword("import-for-expansion", 
			    scheme_make_compiled_syntax(import_for_expansion_syntax, 
							import_for_expansion_expand), 
			    env);
  scheme_add_global_keyword("export", 
			    scheme_make_compiled_syntax(export_syntax, 
							export_expand), 
			    env);

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol(".kernel");

  REGISTER_SO(module_symbol);
  REGISTER_SO(module_begin_symbol);
  module_symbol = scheme_intern_symbol("module");
  module_begin_symbol = scheme_intern_symbol("#%module-begin");
}

void scheme_finish_kernel(Scheme_Env *env)
{
  Scheme_Hash_Table *ht;
  int i, j, count, syntax_start = 0;
  Scheme_Bucket **bs;
  Scheme_Object **exs, *rn;

  REGISTER_SO(kernel);

  kernel = env;
  env->modname = kernel_symbol;
  env->imports = scheme_null;
  env->et_imports = scheme_null;
  
  /* Export all syntax and variables: */
  count = 0;
  for (j = 0; j < 2; j++) {
    if (!j)
      ht = env->toplevel;
    else
      ht = env->syntax;

    syntax_start = count;

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
      ht = env->toplevel;
    else
      ht = env->syntax;

    bs = ht->buckets;
    for (i = ht->size; i--; ) {
      Scheme_Bucket *b = bs[i];
      if (b && b->val)
	exs[count++] = (Scheme_Object *)b->key;
    }
  }
 
  env->exports = exs;
  env->export_srcs = NULL;
  env->export_src_names = exs;
  env->num_exports = count;
  env->num_var_exports = syntax_start;
  env->running = 1;

  rn = scheme_make_module_rename(0);

  /* Add a module mapping for all kernel syntax exports: */
  for (i = syntax_start; i < count; i++) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i]);
  }
  
  REGISTER_SO(scheme_sys_wraps);
  scheme_sys_wraps = scheme_datum_to_syntax(scheme_intern_symbol("kernel"),
					    scheme_false,
					    scheme_false);
  scheme_sys_wraps = scheme_add_rename(scheme_sys_wraps, rn);

  REGISTER_SO(begin_stx);
  REGISTER_SO(define_values_stx);
  REGISTER_SO(define_syntax_stx);
  REGISTER_SO(import_stx);
  REGISTER_SO(import_for_expansion_stx);
  REGISTER_SO(export_stx);
  REGISTER_SO(set_stx);
  REGISTER_SO(app_stx);
  REGISTER_SO(unbound_stx);

  begin_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, scheme_sys_wraps);
  define_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, scheme_sys_wraps);
  define_syntax_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-syntax"), scheme_false, scheme_sys_wraps);
  import_stx = scheme_datum_to_syntax(scheme_intern_symbol("import"), scheme_false, scheme_sys_wraps);
  import_for_expansion_stx = scheme_datum_to_syntax(scheme_intern_symbol("import-for-expansion"), scheme_false, scheme_sys_wraps);
  export_stx = scheme_datum_to_syntax(scheme_intern_symbol("export"), scheme_false, scheme_sys_wraps);
  set_stx = scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, scheme_sys_wraps);
  app_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%app"), scheme_false, scheme_sys_wraps);
  unbound_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%unbound"), scheme_false, scheme_sys_wraps);

  REGISTER_SO(map_symbol);
  REGISTER_SO(rename_symbol);
  REGISTER_SO(all_except_symbol);
  REGISTER_SO(all_from_symbol);
  REGISTER_SO(all_from_except_symbol);
  map_symbol = scheme_intern_symbol("map");
  rename_symbol = scheme_intern_symbol("rename");
  all_except_symbol = scheme_intern_symbol("all-except");
  all_from_symbol = scheme_intern_symbol("all-from");
  all_from_except_symbol = scheme_intern_symbol("all-from-except");
}

void scheme_import_from_original_env(Scheme_Env *env)
{
  Scheme_Object *rn, **exs;
  int i;

  rn = env->rename;
  if (!rn) {
    rn = scheme_make_module_rename(1);
    env->rename = rn;
  }

  exs = kernel->exports;
  for (i = kernel->num_exports; i--; ) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i]);
  }
}

/**********************************************************************/
/*                       basic module operations                      */
/**********************************************************************/

Scheme_Env *scheme_module_load(Scheme_Object *name, Scheme_Env *env)
{
  if (name == kernel_symbol)
    return kernel;
  else {
    Scheme_Env *m;

    m = scheme_lookup_in_table(env->modules, (const char *)name);

    if (!m) {
      scheme_wrong_syntax("import", NULL, name, "cannot find module");
      return NULL;
    }

    return m;
  }
}

void scheme_start_module(Scheme_Env *env)
{
  Scheme_Object *body;

  if (env->running)
    return;

  for (body = env->et_imports; !SCHEME_NULLP(body); body = SCHEME_CDR(body)) {
    scheme_start_module(scheme_module_load(SCHEME_CAR(body), env));
  }
  for (body = env->imports; !SCHEME_NULLP(body); body = SCHEME_CDR(body)) {
    scheme_start_module(scheme_module_load(SCHEME_CAR(body), env));
  }

  env->running = 1;

  body = env->body;
  env->body = NULL;

  for (; !SCHEME_NULLP(body); body = SCHEME_CDR(body))
    _scheme_eval_compiled_expr_multi(SCHEME_CAR(body));  
}

/**********************************************************************/
/*                               module                               */
/**********************************************************************/

static Scheme_Object *
module_execute(Scheme_Object *data)
{
  Scheme_Object *body = SCHEME_CAR(data);
  Scheme_Env *env = (Scheme_Env *)SCHEME_CDR(data);
  
  env->body = body;

  scheme_add_to_table(env->modules, (const char *)env->modname, env, 0);

  return scheme_void;
}

static Scheme_Object *
module_link(Scheme_Object *data, Link_Info *link)
{
  Scheme_Object *body = SCHEME_CAR(data), *b;
  Scheme_Env *env = (Scheme_Env *)SCHEME_CDR(data);

  for (b = body; !SCHEME_NULLP(b); b = SCHEME_CDR(b)) {
    Scheme_Object *e;
    e = scheme_link_expr(SCHEME_CAR(b), link);
    SCHEME_CAR(b) = e;
  }

  return scheme_make_syntax_link(module_execute, 
				 cons(body, (Scheme_Object *)env));
}

static Scheme_Object *do_module(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *nm, *ii, *mb, *mw, *rn;
  Scheme_Env *iim;
  Scheme_Env *menv;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("module", NULL, form, "illegal use (not at top-level)");

  fm = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax("module", NULL, form, NULL);
  nm = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(nm))
    scheme_wrong_syntax("module", nm, form, "module name is not an identifier");
  fm = SCHEME_STX_CDR(fm);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax("module", NULL, form, NULL);
  ii = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(ii))
    scheme_wrong_syntax("module", ii, form, "initial import module name is not an identifier");
  fm = SCHEME_STX_CDR(fm);
  
  mw = SCHEME_STX_VAL(nm);
  menv = scheme_new_module_env(env->genv, mw);
  
  {
    Scheme_Object *ins;
    ins = cons(SCHEME_STX_VAL(ii), menv->imports);
    menv->imports = ins;
    menv->et_imports = scheme_null;
  }

  iim = scheme_module_load(SCHEME_STX_SYM(ii), menv); /* load the module for the initial import */

  /* Expand the body of the module via `#%module-begin' */
  fm = scheme_make_pair(module_begin_symbol, fm);

  rn = scheme_make_module_rename(0);
  scheme_extend_module_rename(rn, mw, scheme_false, scheme_false);
  menv->rename = rn;

  fm = scheme_datum_to_syntax(fm, form, form);
  fm = scheme_add_rename(fm, rn);

  /* For each (direct) export in iim, add a module rename to fm */
  {
    int i;
    Scheme_Object **exs, **exss, **exsns;

    exs = iim->exports;
    exsns = iim->export_src_names;
    exss = iim->export_srcs;
    for (i = iim->num_exports; i--; ) {
      scheme_extend_module_rename(rn, exss ? exss[i] : iim->modname, exs[i], exsns[i]);
    }
  }
  
  if (rec) {
    scheme_compile_rec_done_local(rec, drec);
    fm = scheme_compile_expr(fm, menv->init, rec, drec);

    /* result should be a module body value: */
    if (!SAME_TYPE(SCHEME_TYPE(fm), scheme_module_begin_type)) {
      scheme_wrong_syntax("module", NULL, form, "body is not built with #%module-begin");
    }

    return scheme_make_syntax_compile(module_link, cons(SCHEME_PTR_VAL(fm), 
							(Scheme_Object *)menv));
  } else {
    fm = scheme_expand_expr(fm, menv->init, depth, scheme_false);

    if (!SCHEME_STX_PAIRP(fm)
	|| !SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(fm))
	|| !SAME_OBJ(module_begin_symbol, SCHEME_STX_VAL(SCHEME_STX_CAR(fm))))
      scheme_wrong_syntax("module", fm, form, "body expansion was not a #%module-begin expression");
    else if (scheme_stx_proper_list_length(fm) < 0)
      scheme_wrong_syntax("module", fm, form, "body expansion was an ill-formed #%module-begin expression");
    
    mb = SCHEME_STX_CAR(fm);
    fm = SCHEME_STX_CDR(fm);
    if (SCHEME_STXP(fm))
      fm = SCHEME_STX_VAL(fm);

    fm = cons(module_symbol,
	      cons(nm,
		   cons(ii,
			scheme_datum_to_syntax(SCHEME_STX_CDR(fm), form, mb))));
    
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
  return do_module(form, env, NULL, 0, depth, boundname);
}

/**********************************************************************/
/*                          #%module-begin                            */
/**********************************************************************/

static void ok_for_expansion(Scheme_Hash_Table *ht, Scheme_Env *env, 
			     Scheme_Object *imods, int all)
{
  Scheme_Env *m;

  while (!SCHEME_NULLP(imods)) {
    if (all) {
      if (scheme_lookup_in_table(ht, (const char *)SCHEME_CAR(imods)))
	continue;
      scheme_add_to_table(ht, (const char *)SCHEME_CAR(imods), scheme_true, 0);
    }

    m = scheme_module_load(SCHEME_CAR(imods), env);
    ok_for_expansion(ht, env, m->imports, all);
    ok_for_expansion(ht, env, m->et_imports, 1);

    imods = SCHEME_CDR(imods);
  }
}

static void do_unbound(Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *c;

  c = SCHEME_STX_CDR(form);

  if (!SCHEME_STX_SYMBOLP(c))
    scheme_wrong_syntax("#%unbound", NULL, form, NULL);

  if (!scheme_lookup_in_table(env->genv->toplevel, (const char *)SCHEME_STX_VAL(c))) {
    scheme_wrong_syntax("module", NULL, c, "unbound variable");
  }
}

static Scheme_Object *unbound_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
				     Scheme_Compile_Info *rec, int drec)
{
  do_unbound(form, env);
  return (Scheme_Object *)scheme_global_bucket(SCHEME_STX_VAL(SCHEME_STX_CDR(form)), env->genv);
}

static Scheme_Object *unbound_expand(Scheme_Object *form, Scheme_Comp_Env *env, 
				     int depth, Scheme_Object *boundname)
{
  do_unbound(form, env);
  return form;
}

static void check_import_name(Scheme_Object *name, 
			      Scheme_Object *modname, Scheme_Object *exname,
			      int kind, void *tables, Scheme_Object *e)
{
  Scheme_Hash_Table *toplevel, *imported, *syntax;

  toplevel = ((Scheme_Hash_Table **)tables)[0];
  imported = ((Scheme_Hash_Table **)tables)[1];
  syntax = ((Scheme_Hash_Table **)tables)[2];
  e = ((Scheme_Object **)tables)[3];

  /* Check that it's not yet defined: */
  if (scheme_lookup_in_table(toplevel, (const char *)name)) {
    scheme_wrong_syntax("module", name, e, "imported identifier already defined");
  }
	    
  /* Not imported: */
  if (scheme_lookup_in_table(imported, (const char *)name)) {
    scheme_wrong_syntax("module", name, e, "identifier already imported");
  }
	    
  /* Not syntax: */
  if (scheme_lookup_in_table(syntax, (const char *)name)) {
    scheme_wrong_syntax("module", name, e, "imported identifier already defined");
  }

  /* Remember import: */
  scheme_add_to_table(imported, (const char *)name, scheme_make_pair(modname, exname), 0);
}

static Scheme_Object *do_module_begin(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *first, *last, *p, *rn;
  Scheme_Comp_Env *xenv, *cenv;
  Scheme_Hash_Table *imported;   /* name -> (cons modname srcname) */
  Scheme_Hash_Table *exported;   /* exname -> locname */
  Scheme_Hash_Table *reexported; /* modname -> (cons syntax (list except-name ...)) */
  void *tables[4];
  Scheme_Object **exs, **exsns, **exss;
  int excount;

  if (!scheme_is_module_env(env))
    scheme_wrong_syntax("#%module-begin", NULL, form, "illegal use (not a module body)");

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax("#%module-begin", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
  /* Expand each expression in form up to `begin', `define-values', `define-syntax', 
     `import', `export', and `#%app'. */
  xenv = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME, env);
  scheme_add_local_syntax(begin_stx, xenv);
  scheme_set_local_syntax(begin_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(define_values_stx, xenv);
  scheme_set_local_syntax(define_values_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(define_syntax_stx, xenv);
  scheme_set_local_syntax(define_syntax_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(import_stx, xenv);
  scheme_set_local_syntax(import_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(import_for_expansion_stx, xenv);
  scheme_set_local_syntax(import_for_expansion_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(export_stx, xenv);
  scheme_set_local_syntax(export_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(set_stx, xenv);
  scheme_set_local_syntax(set_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(app_stx, xenv);
  scheme_set_local_syntax(app_stx, scheme_get_stop_expander(), xenv);
  scheme_add_local_syntax(unbound_stx, xenv);
  scheme_set_local_syntax(unbound_stx, scheme_get_stop_expander(), xenv);

  first = scheme_null;
  last = NULL;

  imported = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);
  /* Put initial imports into the table: */
  {
    int i;
    Scheme_Env *iim;
    Scheme_Object **exs, **exsns, **exss, *mn;

    iim = scheme_module_load(SCHEME_CAR(env->genv->imports), env->genv);
    exs = iim->exports;
    exsns = iim->export_src_names;
    exss = iim->export_srcs;
    for (i = iim->num_exports; i--; ) {
      mn = (exss ? exss[i] : SCHEME_CAR(env->genv->imports));
      scheme_add_to_table(imported, (const char *)exs[i], scheme_make_pair(mn, exsns[i]), 0);
    }
  }
  rn = env->genv->rename;

  tables[0] = env->genv->toplevel;
  tables[1] = imported;
  tables[2] = env->genv->syntax;

  exported = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);
  reexported = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);

  /* For final expansion: Set up an expander for #%unbound that
     signals a syntax error for variables not defined in the
     module. */
  if (!unbound_expander) {
    REGISTER_SO(unbound_expander);
    unbound_expander = scheme_make_compiled_syntax(unbound_syntax, 
						   unbound_expand);
  }
  cenv = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME, env);
  scheme_add_local_syntax(unbound_stx, cenv);
  scheme_set_local_syntax(unbound_stx, unbound_expander, cenv);

  /* Partially expand all expressions, and process definitions, imports,
     and exports. Also, flatten top-level `begin' expressions: */
  for (fm = SCHEME_STX_CDR(form); !SCHEME_STX_NULLP(fm); fm = SCHEME_STX_CDR(fm)) {
    Scheme_Object *e;
    int normal;

    while (1) {
      e = SCHEME_STX_CAR(fm);

      /* -2 means expand all the way (to stops), but preserve letrec-syntax. */
      e = scheme_expand_expr(e, xenv, -2, scheme_false);

      if (SCHEME_STX_PAIRP(e) && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(e))
	  && scheme_stx_module_eq(begin_stx, SCHEME_STX_CAR(e))) {
	if (scheme_stx_proper_list_length(e) < 0)
	  scheme_wrong_syntax("begin (module body)", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
	fm = scheme_append(e, fm);
      } else
	break;
    }
    
    if (SCHEME_STX_PAIRP(e) && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(e))) {
      Scheme_Object *n;
      n = SCHEME_STX_CAR(e);
      if (scheme_stx_module_eq(define_values_stx, SCHEME_STX_CAR(e))) {
	/************ define-values *************/
	Scheme_Object *vars, *val;

	/* Create top-level vars */
	scheme_define_values_parse(e, &vars, &val, env);

	while (SCHEME_STX_PAIRP(vars)) {
	  Scheme_Object *name;
	  name = SCHEME_STX_CAR(vars);
	  name = SCHEME_STX_SYM(name);

	  /* Check that it's not yet defined: */
	  if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	    scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	    return NULL;
	  }

	  /* Not imported: */
	  if (scheme_lookup_in_table(imported, (const char *)name)) {
	    scheme_wrong_syntax("module", name, e, "identifier is imported");
	    return NULL;
	  }

	  /* Not syntax: */
	  if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	    scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	    return NULL;
	  }

	  /* Create the bucket, indicating that the name will be defined: */
	  scheme_add_global_symbol(name, scheme_undefined, env->genv);

	  vars = SCHEME_STX_CDR(vars);
	}
	
	normal = 1;
      } else if (scheme_stx_module_eq(define_syntax_stx, SCHEME_STX_CAR(e))) {
	/************ define-syntax *************/
	/* Define the macro: */
	Scheme_Compile_Info mrec;
	Scheme_Object *name, *code, *m, *macro;
	Scheme_Hash_Table *ok;

	scheme_defmacro_parse(e, &name, &code, env);
	
	name = SCHEME_STX_SYM(name);

	if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	  scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	  return NULL;
	}

	/* Check that it's not yet defined: */
	if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	  scheme_wrong_syntax("module", name, e, "duplicate definition for identifier");
	  return NULL;
	}

	/* Not imported: */
	if (scheme_lookup_in_table(imported, (const char *)name)) {
	  scheme_wrong_syntax("module", name, e, "identifier is imported");
	  return NULL;
	}

	mrec.dont_mark_local_use = 0;
	mrec.value_name = NULL;

	ok = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);
	scheme_add_to_table(ok, (const char *)kernel_symbol, scheme_true, 0);
	ok_for_expansion(ok, env->genv, env->genv->imports, 0);
	ok_for_expansion(ok, env->genv, env->genv->et_imports, 1);
	cenv->ok_modules = ok;

	if (!rec)
	  code = scheme_expand_expr(code, cenv, -1, name);
	m = scheme_compile_expr(code, cenv, &mrec, 0);
	m = scheme_link_expr(m, scheme_link_info_create());
	m = scheme_eval_compiled_expr(m);

	cenv->ok_modules = NULL;

	/* Add macro to environment: */
	macro = scheme_alloc_stubborn_small_object();
	macro->type = scheme_macro_type;
	SCHEME_PTR_VAL(macro) = m;
	scheme_end_stubborn_change((void *)macro);
	
	scheme_add_to_table(env->genv->syntax, (const char *)name, macro, 0);

	if (rec)
	  e = scheme_compiled_void();
	else {
	  m = scheme_make_pair(define_syntax_stx,
			       scheme_make_pair(SCHEME_STX_CADR(e),
						scheme_make_pair(code, scheme_null)));
	  e = scheme_datum_to_syntax(m, e, e);
	}
	normal = 0;
      } else if (scheme_stx_module_eq(import_stx, SCHEME_STX_CAR(e))) {	
	/************ import *************/
	Scheme_Object *imods;

	/* Add imports to renaming: */
	tables[3] = e;
	imods = parse_imports(form, e, env->genv, rn, check_import_name, tables, 0);
	
	/* Add imported modules to imports list: */
	for (; !SCHEME_NULLP(imods); imods = SCHEME_CDR(imods)) {
	  Scheme_Object *il, *ilast = NULL;
	  Scheme_Object *name = SCHEME_CAR(imods);
	  
	  for (il = env->genv->imports; SCHEME_PAIRP(il); il = SCHEME_CDR(il)) {
	    if (SAME_OBJ(name, SCHEME_CAR(il)))
	      break;
	    ilast = il;
	  }
	  
	  if (SCHEME_NULLP(il)) {
	    il = scheme_make_pair(name, scheme_null);
	    SCHEME_CDR(ilast) = il;
	  }
	}

	if (rec)
	  e = scheme_compiled_void();
	normal = 0;
      } else if (scheme_stx_module_eq(import_for_expansion_stx, SCHEME_STX_CAR(e))) {	
	/************ import-for-expansion *************/
	Scheme_Object *imods;

	/* Add imports to renaming: */
	tables[3] = e;
	imods = parse_imports(form, e, env->genv, rn, check_import_name, tables, 1);

	/* Add imported modules to et_imports list: */
	for (; !SCHEME_NULLP(imods); imods = SCHEME_CDR(imods)) {
	  Scheme_Object *il, *ilast = NULL;
	  Scheme_Object *name = SCHEME_CAR(imods);
	  
	  for (il = env->genv->et_imports; SCHEME_PAIRP(il); il = SCHEME_CDR(il)) {
	    if (SAME_OBJ(name, SCHEME_CAR(il)))
	      break;
	    ilast = il;
	  }
	  
	  if (SCHEME_NULLP(il)) {
	    il = scheme_make_pair(name, scheme_null);
	    if (ilast)
	      SCHEME_CDR(ilast) = il;
	    else
	      env->genv->et_imports = il;
	  }
	}

	if (rec)
	  e = scheme_compiled_void();
	normal = 0;
      } else if (scheme_stx_module_eq(export_stx, SCHEME_STX_CAR(e))) {
	/************ export *************/
	/* Add exports to table: */
	Scheme_Object *l;

	if (scheme_stx_proper_list_length(e) < 0)
	  scheme_wrong_syntax("export", e, form, "bad syntax (" IMPROPER_LIST_FORM ")");

	for (l = SCHEME_STX_CDR(e); !SCHEME_NULLP(l); l = SCHEME_STX_CDR(l)) {
	  Scheme_Object *a, *mn;

	  a = SCHEME_CAR(l);

	  if (SCHEME_STX_SYMBOLP(a)) {
	    /* <id> */
	    a = SCHEME_STX_VAL(a);
	    if (scheme_lookup_in_table(exported, (const char *)a))
	      scheme_wrong_syntax("export", a, form, "identifier already exported");
	    /* Export a: */
	    scheme_add_to_table(exported, (const char *)a, a, 0);
	  } else if (SCHEME_STX_PAIRP(a)
		     && SAME_OBJ(rename_symbol, SCHEME_STX_VAL(SCHEME_STX_CAR(a)))) {
	    /* (rename <id> <id>) */
	    Scheme_Object *inm, *enm;
	    if (!SCHEME_STX_PAIRP(SCHEME_STX_CDR(a))
		|| !SCHEME_STX_PAIRP(SCHEME_STX_CDR(SCHEME_STX_CDR(a))))
	      scheme_wrong_syntax("export", a, form, "bad syntax");
	    inm = SCHEME_STX_CADR(a);
	    enm = SCHEME_STX_CADR(SCHEME_STX_CDR(a));
	    if (!SCHEME_STX_SYMBOLP(inm))
	      scheme_wrong_syntax("export", a, form, "bad syntax (internal name is not an identifier)");
	    if (!SCHEME_STX_SYMBOLP(enm))
	      scheme_wrong_syntax("export", a, form, "bad syntax (external name is not an identifier)");
	    if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(SCHEME_STX_CDR(SCHEME_STX_CDR(a)))))
	      scheme_wrong_syntax("export", a, form, "bad syntax (data following external name)");
	    
	    inm = SCHEME_STX_VAL(inm);
	    enm = SCHEME_STX_VAL(enm);

	    if (scheme_lookup_in_table(exported, (const char *)enm))
	      scheme_wrong_syntax("export", enm, a, "identifier already exported");
	    /* Export enm: */
	    scheme_add_to_table(exported, (const char *)enm, inm, 0);
	  } else if (SCHEME_STX_PAIRP(a)
		     && SAME_OBJ(all_from_symbol, SCHEME_STX_VAL(SCHEME_STX_CAR(a)))) {
	    /* (all-from <modname>) */
	    if (!SCHEME_STX_PAIRP(SCHEME_STX_CDR(a)))
	      scheme_wrong_syntax("export", a, form, "bad syntax");
	    if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CADR(a)))
	      scheme_wrong_syntax("export", a, form, "bad syntax (module name is not an identifier)");
	    if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(SCHEME_STX_CDR(a))))
	      scheme_wrong_syntax("export", a, form, "bad syntax (data following all keyword)");
	    
	    mn = SCHEME_STX_VAL(SCHEME_STX_CADR(a));
	    
	    /* Export everything imported from mn: */
	    if (scheme_lookup_in_table(reexported, (const char *)mn))
	      scheme_wrong_syntax("export", a, form, "identifiers imported from the module are already exported");
	    scheme_add_to_table(reexported, (const char *)mn, scheme_make_pair(a, scheme_null), 0);
	  } else if (SCHEME_STX_PAIRP(a)
		     && SAME_OBJ(all_from_except_symbol, SCHEME_STX_VAL(SCHEME_STX_CAR(a)))) {
	    /* (all-from-except <modname> <id> ...) */
	    Scheme_Object *exns, *el;

	    if (scheme_stx_proper_list_length(a) < 0)
	      scheme_wrong_syntax("export", e, form, "bad syntax (" IMPROPER_LIST_FORM ")");

	    if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(a)))
	      scheme_wrong_syntax("export", e, form, "bad syntax (module name is not an identifier)");

	    mn = SCHEME_STX_VAL(SCHEME_STX_CAR(a));
	    exns = SCHEME_STX_CDR(SCHEME_STX_CDR(a));
	    
	    /* Check all excclusions are identifiers: */
	    for (el = exns; SCHEME_STX_PAIRP(el); el = SCHEME_STX_CDR(el)) {
	      if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(el))) {
		scheme_wrong_syntax("export", SCHEME_STX_CAR(el), a, "bad syntax (excluded name is not an identifier)");
	      }
	    }

	    /* Export everything imported from mn, except some names: */
	    if (scheme_lookup_in_table(reexported, (const char *)mn))
	      scheme_wrong_syntax("export", a, form, "identifiers imported from the import are already exported");
	    scheme_add_to_table(reexported, (const char *)mn, scheme_make_pair(a, exns), 0);
	  } else {
	    scheme_wrong_syntax("export", a, form, NULL);
	  }
	}

	if (rec)
	  e = scheme_compiled_void();
	normal = 0;
      } else
	normal = 1;
    } else
      normal = 1;

    p = scheme_make_pair(scheme_make_pair(e, normal ? scheme_true : scheme_false), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }
  /* first =  a list of (cons semi-expanded-expression normal?) */


  cenv = scheme_extend_as_toplevel(cenv);

  for (p = first; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
    Scheme_Object *e;
    int normal;

    e = SCHEME_CAR(p);
    normal = SCHEME_TRUEP(SCHEME_CDR(e));
    e = SCHEME_CAR(e);
    if (normal) {
      if (rec)
	e = scheme_compile_expr(e, cenv, rec, drec); /* FIXME: rec */
      else
	e = scheme_expand_expr(e, cenv, depth, scheme_false);
    }
    SCHEME_CAR(p) = e;
  }
  /* first =  a list of expanded/compiled expressions */

  /* Compute exports for re-exports: */
  {
    int i;
    Scheme_Bucket **bs, *b;

    /* First, check the sanity of the re-export specifications: */
    bs = reexported->buckets;
    for (i = reexported->size; i--; ) {
      b = bs[i];
      if (b && b->val) {
	Scheme_Object *mn = (Scheme_Object *)b->key, *l, *exns;
	
	for (l = env->genv->imports; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	  if (SAME_OBJ(mn, SCHEME_CAR(l)))
	    break;
	}
	if (SCHEME_NULLP(l)) {
	  /* Didn't import the named module */
	  scheme_wrong_syntax("export", mn, SCHEME_CAR((Scheme_Object *)b->val),
			      "module was not imported");
	}

	exns = SCHEME_CDR((Scheme_Object *)b->val); 
	for (l = exns; !SCHEME_STX_NULLP(l); l = SCHEME_STX_CDR(l)) {
	  /* Make sure excluded name was imported: */
	  Scheme_Object *a;
	  a = SCHEME_STX_VAL(SCHEME_STX_CAR(l));
	  if (!scheme_lookup_in_table(imported, (const char *)a)) {
	    scheme_wrong_syntax("export", SCHEME_STX_CAR(l), SCHEME_CAR((Scheme_Object *)b->val),
				"excluded name was not imported");
	  }
	}
      }
    }

    /* Walk through imports, check for re-exporting: */
     bs = imported->buckets;
    for (i = imported->size; i--; ) {
      b = bs[i];
      if (b && b->val) {
	Scheme_Object *modname, *srcname, *name, *ree;

	name = (Scheme_Object *)b->key;
	modname = SCHEME_CAR((Scheme_Object *)b->val);
	srcname = SCHEME_CDR((Scheme_Object *)b->val);

	ree = scheme_lookup_in_table(reexported, (const char *)modname);
	if (ree) {
	  Scheme_Object *exns;

	  for (exns = SCHEME_CDR(ree); !SCHEME_STX_NULLP(exns); exns = SCHEME_STX_CDR(exns)) {
	    /* Make sure excluded name was imported: */
	    Scheme_Object *a;
	    a = SCHEME_STX_VAL(SCHEME_STX_CAR(exns));
	    if (SAME_OBJ(a, name))
	      break;
	  }

	  if (SCHEME_STX_NULLP(exns)) {
	    /* Not excluded, so export it. */
	    if (scheme_lookup_in_table(exported, (const char *)name))
	      scheme_wrong_syntax("export", name, SCHEME_CAR(ree), "identifier already exported");
	    
	    scheme_add_to_table(exported, (const char *)name, name, 0);
	  }
	}
      }
    }
  }

  /* Compute all exports */
  {
    int i, count;
    Scheme_Bucket **bs, *b;
    
    bs = exported->buckets;
    for (count = 0, i = exported->size; i--; ) {
      b = bs[i];
      if (b && b->val)
	count++;
    }
    
    exs = MALLOC_N(Scheme_Object *, count);
    exsns = MALLOC_N(Scheme_Object *, count);
    exss = MALLOC_N(Scheme_Object *, count);
    
    for (count = 0, i = exported->size; i--; ) {
      b = bs[i];
      if (b && b->val) {
	Scheme_Object *name, *v;
	  
	exs[count] = (Scheme_Object *)b->key;
	name = b->val;

	if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)
	    || scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	  /* Defined locally */
	  exsns[count] = name;
	  exss[count] = env->genv->modname;
	} else if ((v = scheme_lookup_in_table(imported, (const char *)name))) {
	  /* Imported */
	  exsns[count] = SCHEME_CDR(v);
	  exss[count] = SCHEME_CAR(v);
	} else {
	  /* Not defined! */
	  scheme_wrong_syntax("module", name, form, "exported identifier not defined or imported");
	}
	  
	count++;
      }
    }

    excount = count;
  }

  if (rec) {
    Scheme_Object *mb;
    
    mb = scheme_alloc_small_object();
    mb->type = scheme_module_begin_type;
    SCHEME_PTR_VAL(mb) = first;

    /* Install final exports: */
    env->genv->num_exports = excount;
    env->genv->exports = exs;
    env->genv->export_src_names = exsns;
    env->genv->export_srcs = exss;

    return mb;
  } else
    return scheme_datum_to_syntax(cons(module_begin_symbol, first),
				  form, scheme_sys_wraps);
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
/*                         top-level import                           */
/**********************************************************************/

Scheme_Object *parse_imports(Scheme_Object *form, Scheme_Object *ll, 
			     Scheme_Env *env, Scheme_Object *rn,
			     Check_Func ck, void *data,
			     int start)
{
  Scheme_Env *m;
  int j, var_count;
  Scheme_Object **exs, **exsns, **exss;
  Scheme_Object *name, *i, *exns, *prefix, *iname, *ename;
  Scheme_Object *imods;

  imods = scheme_null;

  if (scheme_stx_proper_list_length(ll) < 0)
    scheme_wrong_syntax("import", SAME_OBJ(form, ll) ? NULL : ll, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
  
  for (ll = SCHEME_STX_CDR(ll); !SCHEME_STX_NULLP(ll); ll = SCHEME_STX_CDR(ll)) {
    i = SCHEME_STX_CAR(ll);
    iname = ename = NULL;
    if (SCHEME_STX_SYMBOLP(i)) {
      name = i;
      exns = NULL;
      prefix = NULL;
    } else if (SCHEME_STX_PAIRP(i)
	       && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(i))
	       && SCHEME_STX_SYMBOLP(SCHEME_STX_CDR(i))) {
      name = SCHEME_STX_CDR(i);
      prefix = SCHEME_STX_CAR(i);
      exns = NULL;
    } else if (SCHEME_STX_PAIRP(i)
	       && SAME_OBJ(all_except_symbol, SCHEME_STX_VAL(SCHEME_STX_CAR(i)))) {
      Scheme_Object *l;
      int len;

      len = scheme_stx_proper_list_length(i);
      if (len < 0)
	scheme_wrong_syntax("import", i, form, "bad syntax (" IMPROPER_LIST_FORM ")");
      else if (len < 2)
	scheme_wrong_syntax("import", i, form, "bad syntax (module name missing)");

      name = SCHEME_STX_CADR(i);

      if (!SCHEME_STX_SYMBOLP(name))
	scheme_wrong_syntax("import", i, form, "bad syntax (module name is not an identifier)");
      
      prefix = scheme_null;
      exns = SCHEME_STX_CDR(SCHEME_STX_CDR(i));

      for (l = exns; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	if (!SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(l)))
	  scheme_wrong_syntax("import", SCHEME_STX_CAR(l), form, "bad syntax (except name is not an identifier)");
      }
    } else if (SCHEME_STX_PAIRP(i)
	       && SAME_OBJ(map_symbol, SCHEME_STX_VAL(SCHEME_STX_CAR(i)))) {
      int len;

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
	scheme_wrong_syntax("import", i, form, reason);
	return NULL;
      }

      name = SCHEME_STX_CADR(i);
      iname = SCHEME_STX_CADR(SCHEME_STX_CDR(i));
      ename = SCHEME_STX_CADR(SCHEME_STX_CDR(SCHEME_STX_CDR(i)));

      if (!SCHEME_STX_SYMBOLP(name))
	scheme_wrong_syntax("import", i, form, "bad syntax (module name is not an identifier)");
      if (!SCHEME_STX_SYMBOLP(iname))
	scheme_wrong_syntax("import", i, form, "bad syntax (internal name is not an identifier)");
      if (!SCHEME_STX_SYMBOLP(ename))
	scheme_wrong_syntax("import", i, form, "bad syntax (external name is not an identifier)");

      iname = SCHEME_STX_VAL(iname);
      ename = SCHEME_STX_VAL(ename);
      
      prefix = NULL;
      exns = NULL;
    } else {
      scheme_wrong_syntax("import", i, form, NULL);
      return NULL;
    }

    name = SCHEME_STX_VAL(name);

    m = scheme_module_load(name, env);
    if (start)
      scheme_start_module(m);

    /* Add name to import list, if it's not there: */
    {
      Scheme_Object *l, *last = NULL, *p;
      for (l = imods; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	if (SAME_OBJ(SCHEME_CAR(l), name))
	  break;
	last = l;
      }
      if (SCHEME_NULLP(l)) {
	p = scheme_make_pair(name, scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  imods = p;
      }
    }

    exs = m->exports;
    exsns = m->export_src_names;
    exss = m->export_srcs;
    var_count = m->num_var_exports;

    for (j = m->num_exports; j--; ) {
      Scheme_Object *modname;

      if (ename) {
	if (!SAME_OBJ(ename, exs[j]))
	  continue;  /* we don't want this one. */
      } else if (exns) {
	Scheme_Object *l;
	for (l = exns; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
	  if (SAME_OBJ(SCHEME_STX_VAL(SCHEME_STX_CAR(l)), exs[j]))
	    break;
	}
	if (!SCHEME_STX_NULLP(l))
	  continue; /* we don't want this one. */
      }
  
      modname = exss ? exss[j] : name;

      if (!iname)
	iname = exs[j];

      ck(iname, modname, exsns[j], (j >= var_count) ? 2 : 1, data, i);

      scheme_extend_module_rename(rn, modname, iname, exsns[j]);

      iname = NULL;

      if (ename) {
	ename = NULL;
	break;
      }
    }

    if (ename) {
      scheme_wrong_syntax("import", i, form, "no such exported variable");
    }
  }

  return imods;
}

static Scheme_Object *
top_level_import_execute(Scheme_Object *data)
{
  Scheme_Object *rn = SCHEME_CAR(data), *brn;
  Scheme_Env *env = (Scheme_Env *)SCHEME_CDR(data);

  brn = env->rename;
  if (!brn) {
    brn = scheme_make_module_rename(1);
    env->rename = brn;
  }

  scheme_append_module_rename(rn, brn);

  return scheme_void;
}

static Scheme_Object *
top_level_import_link(Scheme_Object *data, Link_Info *link)
{
  return scheme_make_syntax_link(top_level_import_execute, data);
}

static void check_dup_import(Scheme_Object *name, 
			     Scheme_Object *modname, Scheme_Object *srcname, 
			     int kind, void *ht, Scheme_Object *e)
{
  if (scheme_lookup_in_table((Scheme_Hash_Table *)ht, (const char *)name))
    scheme_wrong_syntax("import", name, e, "duplicate import identifier");
  else
    scheme_add_to_table((Scheme_Hash_Table *)ht, (const char *)name, scheme_false, 0);
}

static Scheme_Object *do_import(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *rn;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("open", NULL, form,  "not at top-level or in module body");

  /* If we get here, it must be a top-level import. */

  /* Hash table is for checking duplicate names in import list: */
  ht = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);

  rn = scheme_make_module_rename(0);

  (void)parse_imports(form, form, env->genv, rn, check_dup_import, ht, 1);

  /* Check syntax: */
  if ((scheme_stx_proper_list_length(form) != 2)
      || !SCHEME_STX_SYMBOLP(SCHEME_STX_CADR(form)))
    scheme_wrong_syntax("import", NULL, form, NULL);

  if (rec) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_make_syntax_compile(top_level_import_link, 
				      cons(rn, (Scheme_Object *)env->genv));
  } else
    return form;
}

static Scheme_Object *
import_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_import(form, env, rec, drec, 1, scheme_false);
}

static Scheme_Object *
import_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_import(form, env, NULL, 0, depth, boundname);
}

/**********************************************************************/
/*                            dummy forms                             */
/**********************************************************************/

static Scheme_Object *
import_for_expansion_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  scheme_wrong_syntax("import-for-expansion", NULL, form, "not at top-level or in module body");
  return NULL;
}

static Scheme_Object *
import_for_expansion_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  scheme_wrong_syntax("import-for-expansion", NULL, form, "not in module body");
  return NULL;
}

static Scheme_Object *
export_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  scheme_wrong_syntax("export", NULL, form, "not at top-level or in module body");
  return NULL;
}

static Scheme_Object *
export_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  scheme_wrong_syntax("export", NULL, form, "not in module body");
  return NULL;
}
