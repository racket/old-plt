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
}

static Scheme_Object *
module_execute(Scheme_Object *data)
{
  Scheme_Object *nm = SCHEME_CAR(data);
  Scheme_Object *body = SCHEME_CADR(data);
  Scheme_Env *env = SCHEME_CADR(data);

  body = _scheme_eval_compiled_expr_multi(vals);

  /* result should be a module value: */
  if (!SCHEME_SAME_TYPE(SCHEME_TYPE(body), scheme_module_type)) {
    scheme_signal_error("bad module result");
  }

  scheme_define_module(env, nm);

  return scheme_void;
}

static Scheme_Object *
module_link(Scheme_Object *data, Link_Info *link)
{
  Scheme_Object *nm = SCHEME_CAR(data);
  Scheme_Object *body = SCHEME_CADR(data);
  Scheme_Env *env = SCHEME_CADR(data);

  body = scheme_link_expr(body, link);

  return scheme_make_syntax_link(module_execute, cons(nm, cons(body, env)));
}

static Scheme_Object *do_module(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *nm, *ii, **iim, *mb, *mw;
  Scheme_Comp_Env *menv;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("module", NULL, form, "illegal use (not at top-level)");

  fm = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIR(form))
    scheme_wrong_syntax("module", NULL, form, NULL);
  nm = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(nm))
    scheme_wrong_syntax("module", nm, form, "module name is not an identifier");
  fm = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIR(form))
    scheme_wrong_syntax("module", NULL, form, NULL);
  ii = SCHEME_STX_CAR(ii);
  if (!SCHEME_STX_SYMBOLP(ii))
    scheme_wrong_syntax("module", nm, form, "initial import module name is not an identifier");
  fm = SCHEME_STX_CDR(form);
  
  mw = scheme_stx_new_module(nm);
  menv = scheme_new_module_env(env, mw);

  iim = scheme_load_module(ii); /* load the module for the initial import */
  scheme_run_module_exp_time(iim, menv, NULL); /* NULL means all exports */

  /* Expand the body of the module via `#%module-begin' */
  fm = scheme_make_pair(module_begin_symbol, fm);

  fm = scheme_datum_to_syntax(fm, form, form);
  fm = scheme_stx_modulize(fm, mw);
  
  if (rec) {
    scheme_compile_rec_done_local(rec, drec);
    fm = scheme_compile_expr(fm, menv, rec, drec);

    return scheme_make_syntax_compile(module_link, cons(nm, cons(fm, env->genv)));
  } else {
    fm = scheme_expand_expr(fm, menv, depth, scheme_false);

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

static Scheme_Object *do_module_begin(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  Scheme_Object *fm, *first, *last;

  if (!scheme_is_module_env(env))
    scheme_wrong_syntax("#%module-begin", NULL, form, "illegal use (not a module body)");

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax("#%module-begin", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  
  /* Expand each expression in form up to `begin', `define-values', `define-syntax', 
     `import', `export', and `#%app'. */
  env = scheme_new_compilation_frame(0, SCHEME_CAPTURE_WITHOUT_RENAME, env);
  scheme_add_local_syntax(begin_stx, env);
  scheme_set_local_syntax(begin_stx, scheme_get_stop_expander(), env);
  scheme_add_local_syntax(define_values_stx, env);
  scheme_set_local_syntax(define_values_stx, scheme_get_stop_expander(), env);
  scheme_add_local_syntax(define_syntax_stx, env);
  scheme_set_local_syntax(define_syntax_stx, scheme_get_stop_expander(), env);
  scheme_add_local_syntax(import_stx, env);
  scheme_set_local_syntax(import_stx, scheme_get_stop_expander(), env);
  scheme_add_local_syntax(export_stx, env);
  scheme_set_local_syntax(export_stx, scheme_get_stop_expander(), env);
  scheme_add_local_syntax(app_stx, env);
  scheme_set_local_syntax(app_stx, scheme_get_stop_expander(), env);

  first = scheme_null;
  last = NULL;

  for (fm = SCHEME_STX_CDR(form); !SCHEME_STX_NULLP(fm); fm = SCHEME_STX_CDR(fm)) {
    Scheme_Object *e, *p;

    while (1) {
      e = SCHEME_STX_CAR(fm);

      /* -2 means expand all the way, but preserve letrec-syntax. */
      e = scheme_expand_expr(e, env, -2, scheme_false);

      if (SCHEME_STX_PAIRP(e) && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(e))
	  && scheme_module_env_eq(begin_stx, SCHEME_STX_CAR(e), env)) {
	if (scheme_stx_proper_list_length(e) < 0)
	  scheme_wrong_syntax("begin (module body)", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
	fm = scheme_append(e, fm);
      } else
	break;
    }
    
    if (SCHEME_STX_PAIRP(e) && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(e))) {
      Scheme_Object *n;
      n = SCHEME_STX_CAR(e);
      if (scheme_module_env_eq(define_values_stx, SCHEME_STX_CAR(e), env)) {
	normal = 1;
      } else if (scheme_module_env_eq(define_syntax_stx, SCHEME_STX_CAR(e), env)) {
	/* Define the macro: */
	Scheme_Compile_Info mrec;
	Scheme_Object *name, *code;
	Scheme_Bucket *b;

	scheme_defmacro_parse(e, &name, &code, env);
	
	b = scheme_global_keyword_bucket(SCHEME_STX_SYM(name), env->genv);
	if (b->val)
	  scheme_syntax_error("module", e, form, "duplicate syntax definition");

	mrec.dont_mark_local_use = 0;
	mrec.value_name = NULL;

	code = scheme_expand_expr(e, env->eenv->init, -1, name);
	m = scheme_compile_expr(code, env->eenv->init, &mrec, 0);
	m = scheme_link_expr(m, scheme_make_link_info());
	m = _scheme_eval_compiled_expr(m, env->eenv);

	/* Add macro to environment: */
	b->val = m;

	if (rec)
	  e = scheme_compiled_void();

	normal = 0;
      } else if (scheme_module_env_eq(import_stx, SCHEME_STX_CAR(e), env)) {	
	/* Add to import list: */

      } else if (scheme_module_env_eq(export_stx, SCHEME_STX_CAR(e), env)) {
	/* Add to export list: */
	
      } else
	normal = 1;
    } else
      normal = 1;

    if (normal) {
      if (rec)
	e = scheme_compile_expr(e, env, rec, drec); /* FIXME: rec */
      else
	e = scheme_expand_expr(e, env, depth, scheme_false);
    }
      
    p = scheme_make_pair(e, scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }

  /* Got a list of compiled/expanded expressions */
  if (rec) {
    
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

static Scheme_Object *do_import(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Info *rec, int drec,
				int depth, Scheme_Object *boundname)
{
  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("open", NULL, form,  "not at top-level or in module body");

  if (rec)
    return scheme_void;
  else
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
