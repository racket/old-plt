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

/* globals */
Scheme_Object *scheme_sys_wraps;

/* locals */
static Scheme_Object *module_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *module_begin_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *module_begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *import_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *import_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *export_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *export_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

#define cons scheme_make_pair

static Scheme_Object *kernel_symbol;
static Scheme_Env *kernel;

static Scheme_Object *module_symbol;
static Scheme_Object *module_begin_symbol;

static Scheme_Object *begin_stx;
static Scheme_Object *define_values_stx;
static Scheme_Object *define_syntax_stx;
static Scheme_Object *import_stx;
static Scheme_Object *export_stx;
static Scheme_Object *set_stx;
static Scheme_Object *app_stx;
static Scheme_Object *unbound_stx;

static Scheme_Object *unbound_expander;

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
  env->export_src_names = exs;
  env->num_exports = count;
  env->num_var_exports = syntax_start;
  env->running = 1;

  rn = scheme_make_module_rename();

  /* Add a module mapping for all kernel syntax exports: */
  for (i = syntax_start; i < count; i++) {
    scheme_extend_module_rename(rn, kernel_symbol, exs[i], exs[i]);
  }
  
  REGISTER_SO(scheme_sys_wraps);
  scheme_sys_wraps = scheme_datum_to_syntax(scheme_intern_symbol("kernel"),
					    scheme_false,
					    scheme_false);
  scheme_sys_wraps = add_rename(scheme_sys_wraps, rn);

  REGISTER_SO(begin_stx);
  REGISTER_SO(define_values_stx);
  REGISTER_SO(define_syntax_stx);
  REGISTER_SO(import_stx);
  REGISTER_SO(export_stx);
  REGISTER_SO(set_stx);
  REGISTER_SO(app_stx);
  REGISTER_SO(unbound_stx);

  begin_stx = scheme_datum_to_syntax(scheme_intern_symbol("begin"), scheme_false, w);
  define_values_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-values"), scheme_false, w);
  define_syntax_stx = scheme_datum_to_syntax(scheme_intern_symbol("define-syntax"), scheme_false, w);
  import_stx = scheme_datum_to_syntax(scheme_intern_symbol("import"), scheme_false, w);
  export_stx = scheme_datum_to_syntax(scheme_intern_symbol("export"), scheme_false, w);
  set_stx = scheme_datum_to_syntax(scheme_intern_symbol("set!"), scheme_false, w);
  app_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%app"), scheme_false, w);
  unbound_stx = scheme_datum_to_syntax(scheme_intern_symbol("#%unbound"), scheme_false, w);
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
  }

  iim = scheme_module_load(SCHEME_STX_SYM(ii), menv); /* load the module for the initial import */

  /* Expand the body of the module via `#%module-begin' */
  fm = scheme_make_pair(module_begin_symbol, fm);

  rn = scheme_make_module_rename();
  scheme_extend_module_rename(rn, mw, scheme_false, scheme_false);
  menv->rename = rn;

  fm = scheme_datum_to_syntax(fm, form, form);
  fm = scheme_add_rename(fm, rn);

  /* For each (direct) export in iim, add a module rename to fm */
  {
    int i;
    Scheme_Object **exs, **exsns;

    exs = iim->exports;
    exsns = iim->export_src_names;
    for (i = iim->num_exports; i--; ) {
      scheme_extend_module_rename(rn, iim->modname, exs[i], exsns[i]);
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

static Scheme_Object *do_module_begin(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *first, *last, *p;
  Scheme_Comp_Env *xenv, *cenv;
  Scheme_Hash_Table *imported;

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
    Scheme_Object **exs;

    iim = scheme_module_load(SCHEME_CAR(env->genv->imports), env->genv);
    exs = iim->exports;
    for (i = iim->num_exports; i--; ) {
      scheme_add_to_table(imported, (const char *)exs[i], scheme_false, 0);
    }
  }

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
	  scheme_add_to_table(env->genv->toplevel, (const char *)name, scheme_undefined, 0);

	  vars = SCHEME_STX_CDR(vars);
	}
	
	normal = 1;
      } else if (scheme_stx_module_eq(define_syntax_stx, SCHEME_STX_CAR(e))) {
	/* Define the macro: */
	Scheme_Compile_Info mrec;
	Scheme_Object *name, *code, *m, *macro;
	Scheme_Bucket *b;

	scheme_defmacro_parse(e, &name, &code, env);
	
	b = scheme_global_keyword_bucket(SCHEME_STX_SYM(name), env->genv);
	if (b->val)
	  scheme_wrong_syntax("module", e, form, "duplicate definition for identifier");

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

	code = scheme_expand_expr(e, env->eenv->init, -1, name);
	m = scheme_compile_expr(code, env->eenv->init, &mrec, 0);
	m = scheme_link_expr(m, scheme_link_info_create());
	m = _scheme_eval_compiled_expr(m);

	/* Add macro to environment: */
	macro = scheme_alloc_stubborn_small_object();
	macro->type = scheme_macro_type;
	SCHEME_PTR_VAL(macro) = m;
	scheme_end_stubborn_change((void *)macro);
	
	b->val = macro;

	if (rec)
	  e = scheme_compiled_void();
	normal = 0;
      } else if (scheme_stx_module_eq(import_stx, SCHEME_STX_CAR(e))) {	
#if 0
	/* Add to import list: */
	Scheme_Object *mod, *ie;
	Scheme_Object *prefix, *exns, *val, *name;
	Scheme_Env *im;
	int i, kind;

	ie = SCHEME_STX_CDR(e);
	while (ie = parse_imports(ie, &mod, &prefix, &exns)) {
	  im = scheme_module_load(mod, env->genv);
	  
	  kind = 1;
	  for (i = 0; kind; i++) {
	    kind = get_export(im, i, prefix, exns, &val, &name);

	    /* Check that it's not yet defined: */
	    if (scheme_lookup_in_table(env->genv->toplevel, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "imported identifier already defined");
	      return NULL;
	    }
	    
	    /* Not imported: */
	    if (scheme_lookup_in_table(imported, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "identifier already imported");
	      return NULL;
	    }
	    
	    /* Not syntax: */
	    if (scheme_lookup_in_table(env->genv->syntax, (const char *)name)) {
	      scheme_wrong_syntax("module", name, e, "imported identifier already defined");
	      return NULL;
	    }

	    /* Remember mappings: */
	    scheme_add_to_table(imported, exs[i], scheme_make_integer(kind), 0);
	  }
	}
#endif	
	if (rec)
	  e = scheme_compiled_void();
	normal = 0;
      } else if (scheme_stx_module_eq(export_stx, SCHEME_STX_CAR(e))) {
	/* Add to export list: */
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

  if (!unbound_expander) {
    REGISTER_SO(unbound_expander);
    unbound_expander = scheme_make_compiled_syntax(unbound_syntax, 
						   unbound_expand);
  }

  cenv = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME, env);
  scheme_add_local_syntax(unbound_stx, cenv);
  scheme_set_local_syntax(unbound_stx, unbound_expander, cenv);

  cenv = scheme_extend_as_toplevel(cenv);

  for (p = first; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
    Scheme_Object *e;
    int normal;

    e = SCHEME_CAR(p);
    normal = SCHEME_TRUEP(SCHEME_CDR(p));
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

  if (rec) {
    Scheme_Object *mb;
    
    mb = scheme_alloc_small_object();
    mb->type = scheme_module_begin_type;
    SCHEME_PTR_VAL(mb) = first;

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

static Scheme_Object *
top_level_import_execute(Scheme_Object *data)
{
  Scheme_Object *name = SCHEME_CAR(data);
  Scheme_Env *env = (Scheme_Env *)SCHEME_CDR(data), *m;
  int i, var_count;
  Scheme_Object **exs, **exsns, *v;

  m = scheme_module_load(name, env);
  scheme_start_module(m);

  exs = m->exports;
  exsns = m->exports;
  var_count = m->num_var_exports;

  for (i = m->num_exports; i--; ) {
    const char *var = (const char *)exs[i];
    const char *snvar = (const char *)exsns[i];
  
    if (i >= var_count) {
      v = scheme_lookup_in_table(m->syntax, snvar);
      scheme_add_to_table(env->syntax, var, v, 0);
    } else {
      v = scheme_lookup_in_table(m->toplevel, snvar);
      scheme_add_to_table(env->toplevel, var, v, 0);
    }
  }

  return scheme_void;
}

static Scheme_Object *
top_level_import_link(Scheme_Object *data, Link_Info *link)
{
  return scheme_make_syntax_link(top_level_import_execute, data);
}

static Scheme_Object *do_import(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("open", NULL, form,  "not at top-level or in module body");

  /* If we get here, it must be a top-level import. */

  /* Check syntax: */
  if ((scheme_stx_proper_list_length(form) != 2)
      || !SCHEME_STX_SYMBOLP(SCHEME_STX_CADR(form)))
    scheme_wrong_syntax("import", NULL, form, NULL);

  if (rec) {
    return scheme_make_syntax_compile(top_level_import_link, 
				      cons(SCHEME_STX_VAL(SCHEME_STX_CADR(form)),
					   (Scheme_Object *)env->genv));
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
/*                           dummy export                             */
/**********************************************************************/

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
