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

#include "schpriv.h"

/* globals */
Scheme_Object *scheme_define_values_syntax, *scheme_defmacro_syntax;
Scheme_Object *scheme_begin_syntax;
Scheme_Object *scheme_lambda_syntax;
Scheme_Object *scheme_compiled_void_code;
Scheme_Object scheme_undefined[1];

Scheme_Syntax_Resolver scheme_syntax_resolvers[_COUNT_EXPD_];
Scheme_Syntax_Linker scheme_syntax_linkers[_COUNT_EXPD_];
Scheme_Syntax_Executer scheme_syntax_executers[_COUNT_EXPD_];
int scheme_syntax_protect_afters[_COUNT_EXPD_];

/* locals */
static Scheme_Object *lambda_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *define_values_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *quote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *quote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *if_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *if_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *set_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *set_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *case_lambda_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *case_lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *let_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *let_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_star_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *let_star_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *letrec_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *let_values_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_star_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *let_star_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *letrec_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_expand (Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *begin0_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin0_expand (Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *with_cont_mark_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *lexical_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *lexical_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *defmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *defmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);
static Scheme_Object *letmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *define_values_execute(Scheme_Object *data);
static Scheme_Object *set_execute(Scheme_Object *data);
static Scheme_Object *defmacro_execute(Scheme_Object *expr);
static Scheme_Object *case_lambda_execute(Scheme_Object *expr);
static Scheme_Object *begin0_execute(Scheme_Object *data);

static Scheme_Object *bangboxenv_execute(Scheme_Object *data);
static Scheme_Object *bangboxvalue_execute(Scheme_Object *data);

static Scheme_Object *define_values_link(Scheme_Object *data, Link_Info *info);
static Scheme_Object *set_link(Scheme_Object *data, Link_Info *info);
static Scheme_Object *defmacro_link(Scheme_Object *expr, Link_Info *info);
static Scheme_Object *case_lambda_link(Scheme_Object *expr, Link_Info *info);
static Scheme_Object *begin0_link(Scheme_Object *data, Link_Info *info);
static Scheme_Object *lexical_syntax_link(Scheme_Object *data, Link_Info *info);

static Scheme_Object *bangboxenv_link(Scheme_Object *data, Link_Info *info);
static Scheme_Object *bangboxvalue_link(Scheme_Object *data, Link_Info *info);

static Scheme_Object *define_values_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *set_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *defmacro_resolve(Scheme_Object *expr, Resolve_Info *info);
static Scheme_Object *case_lambda_resolve(Scheme_Object *expr, Resolve_Info *info);
static Scheme_Object *begin0_resolve(Scheme_Object *data, Resolve_Info *info);
static Scheme_Object *lexical_syntax_resolve(Scheme_Object *data, Resolve_Info *info);

static Scheme_Object *named_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
					Scheme_Compile_Info *rec, int drec, int depth, Scheme_Object *boundname);

static Scheme_Object *write_let_value(Scheme_Object *obj);
static Scheme_Object *read_let_value(Scheme_Object *obj);
static Scheme_Object *write_let_void(Scheme_Object *obj);
static Scheme_Object *read_let_void(Scheme_Object *obj);
static Scheme_Object *write_letrec(Scheme_Object *obj);
static Scheme_Object *read_letrec(Scheme_Object *obj);
static Scheme_Object *write_let_one(Scheme_Object *obj);
static Scheme_Object *read_let_one(Scheme_Object *obj);
static Scheme_Object *write_top(Scheme_Object *obj);
static Scheme_Object *read_top(Scheme_Object *obj);
static Scheme_Object *write_case_lambda(Scheme_Object *obj);
static Scheme_Object *read_case_lambda(Scheme_Object *obj);

/* symbols */
static Scheme_Object *define_values_symbol;
static Scheme_Object *lambda_symbol;
static Scheme_Object *letrec_symbol;
static Scheme_Object *let_star_symbol;
static Scheme_Object *let_symbol;
static Scheme_Object *letrec_values_symbol;
static Scheme_Object *let_star_values_symbol;
static Scheme_Object *let_values_symbol;
static Scheme_Object *begin_symbol;
static Scheme_Object *begin0_symbol;
static Scheme_Object *set_symbol;
static Scheme_Object *quote_symbol;
static Scheme_Object *if_symbol;
static Scheme_Object *case_lambda_symbol;
static Scheme_Object *with_continuation_mark_symbol;

static Scheme_Object *lexical_syntax_symbol;
static Scheme_Object *define_macro_symbol;
static Scheme_Object *let_macro_symbol;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons(a,b) scheme_make_pair(a,b)
#define icons(a,b) scheme_make_immutable_pair(a,b)

#define max(a, b) (((a) > (b)) ? (a) : (b))

void 
scheme_init_syntax (Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(scheme_define_values_syntax);
  REGISTER_SO(scheme_defmacro_syntax);
  REGISTER_SO(scheme_lambda_syntax);
  REGISTER_SO(scheme_begin_syntax);
  REGISTER_SO(scheme_compiled_void_code);

  REGISTER_SO(define_values_symbol);
  REGISTER_SO(lambda_symbol);
  REGISTER_SO(letrec_symbol);
  REGISTER_SO(let_star_symbol);
  REGISTER_SO(let_symbol);
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(let_star_values_symbol);
  REGISTER_SO(let_values_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(begin0_symbol);
  REGISTER_SO(set_symbol);
  REGISTER_SO(quote_symbol);
  REGISTER_SO(if_symbol);
  REGISTER_SO(case_lambda_symbol);
  REGISTER_SO(with_continuation_mark_symbol);
    
  REGISTER_SO(lexical_syntax_symbol);
  REGISTER_SO(define_macro_symbol);
  REGISTER_SO(let_macro_symbol);

  scheme_undefined->type = scheme_undefined_type;
  
  define_values_symbol = scheme_intern_symbol("define-values");
  lambda_symbol = scheme_intern_symbol("lambda");

  letrec_symbol = scheme_intern_symbol("letrec");
  let_star_symbol = scheme_intern_symbol("let*");
  let_symbol = scheme_intern_symbol("let");

  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_star_values_symbol = scheme_intern_symbol("let*-values");
  let_values_symbol = scheme_intern_symbol("let-values");

  begin_symbol = scheme_intern_symbol("begin");
  begin0_symbol = scheme_intern_symbol("begin0");
  quote_symbol = scheme_intern_symbol("quote");
  if_symbol = scheme_intern_symbol("if");
  set_symbol = scheme_intern_symbol("set!");
  case_lambda_symbol = scheme_intern_symbol("case-lambda");
  with_continuation_mark_symbol = scheme_intern_symbol("with-continuation-mark");
  
  lexical_syntax_symbol = scheme_intern_symbol("quote-syntax");
  define_macro_symbol = scheme_intern_symbol("define-syntax");
  let_macro_symbol = scheme_intern_symbol("letrec-syntax");

  scheme_register_syntax(DEFINE_VALUES_EXPD, define_values_resolve, 
			 define_values_link, define_values_execute, 1);
  scheme_register_syntax(SET_EXPD, set_resolve,
			 set_link, set_execute, 1);
  scheme_register_syntax(DEFINE_SYNTAX_EXPD, defmacro_resolve,
			 defmacro_link, defmacro_execute, 1);
  scheme_register_syntax(CASE_LAMBDA_EXPD, case_lambda_resolve,
			 case_lambda_link, case_lambda_execute, 1);
  scheme_register_syntax(BEGIN0_EXPD, begin0_resolve,
			 begin0_link, begin0_execute, 1);
  scheme_register_syntax(QUOTE_SYNTAX_EXPD, lexical_syntax_resolve,
			 lexical_syntax_link, NULL, 1);

  scheme_register_syntax(BOXENV_EXPD, NULL,
			 bangboxenv_link, bangboxenv_execute, 1);
  scheme_register_syntax(BOXVAL_EXPD, NULL,
			 bangboxvalue_link, bangboxvalue_execute, 1);

  scheme_install_type_writer(scheme_let_value_type, write_let_value);
  scheme_install_type_reader(scheme_let_value_type, read_let_value);
  scheme_install_type_writer(scheme_let_void_type, write_let_void);
  scheme_install_type_reader(scheme_let_void_type, read_let_void);
  scheme_install_type_writer(scheme_letrec_type, write_letrec);
  scheme_install_type_reader(scheme_letrec_type, read_letrec);
  scheme_install_type_writer(scheme_let_one_type, write_let_one);
  scheme_install_type_reader(scheme_let_one_type, read_let_one);
  scheme_install_type_writer(scheme_case_lambda_sequence_type, write_case_lambda);
  scheme_install_type_reader(scheme_case_lambda_sequence_type, read_case_lambda);

  scheme_install_type_writer(scheme_compilation_top_type, write_top);
  scheme_install_type_reader(scheme_compilation_top_type, read_top);

  scheme_define_values_syntax = scheme_make_compiled_syntax(define_values_syntax, 
							    define_values_expand);
  scheme_defmacro_syntax = scheme_make_compiled_syntax(defmacro_syntax, 
						       defmacro_expand);
  scheme_lambda_syntax = scheme_make_compiled_syntax(lambda_syntax,
						     lambda_expand);
  scheme_begin_syntax = scheme_make_compiled_syntax(begin_syntax, 
						    begin_expand);
  
  scheme_add_global_keyword("lambda", 
			    scheme_lambda_syntax,
			    env);
  scheme_add_global_keyword("define-values", scheme_define_values_syntax, env);
  scheme_add_global_keyword("quote", 
			    scheme_make_compiled_syntax(quote_syntax,
							quote_expand), 
			    env);
  scheme_add_global_keyword("if", 
			    scheme_make_compiled_syntax(if_syntax, 
							if_expand),
			    env);
  scheme_add_global_keyword("set!", 
			    scheme_make_compiled_syntax(set_syntax, 
							set_expand), 
			    env);

  scheme_add_global_keyword("case-lambda", 
			    scheme_make_compiled_syntax(case_lambda_syntax, 
							case_lambda_expand), 
			    env);

  scheme_add_global_keyword("let", 
			    scheme_make_compiled_syntax(let_syntax, 
							let_expand), 
			    env);
  scheme_add_global_keyword("let*", 
			    scheme_make_compiled_syntax(let_star_syntax, 
							let_star_expand), 
			    env);
  scheme_add_global_keyword("letrec", 
			    scheme_make_compiled_syntax(letrec_syntax, 
						        letrec_expand), 
			    env);  
  
  scheme_add_global_keyword("let-values", 
			    scheme_make_compiled_syntax(let_values_syntax, 
							let_values_expand), 
			    env);
  scheme_add_global_keyword("let*-values", 
			    scheme_make_compiled_syntax(let_star_values_syntax, 
							let_star_values_expand), 
			    env);
  scheme_add_global_keyword("letrec-values", 
			    scheme_make_compiled_syntax(letrec_values_syntax, 
						        letrec_values_expand), 
			    env);  
  
  scheme_add_global_keyword("begin", 
			    scheme_begin_syntax, 
			    env);

  scheme_add_global_keyword("begin0", 
			    scheme_make_compiled_syntax(begin0_syntax, 
						        begin0_expand), 
			    env);

  scheme_add_global_keyword("unquote", 
			    scheme_make_compiled_syntax(unquote_syntax, 
							unquote_expand), 
			    env);
  scheme_add_global_keyword("unquote-splicing", 
			    scheme_make_compiled_syntax(unquote_syntax, 
							unquote_expand), 
			    env);

  scheme_add_global_keyword("with-continuation-mark", 
			    scheme_make_compiled_syntax(with_cont_mark_syntax, 
							with_cont_mark_expand), 
			    env);

  scheme_add_global_keyword("quote-syntax", 
			    scheme_make_compiled_syntax(lexical_syntax_syntax, 
							lexical_syntax_expand), 
			    env);
  scheme_add_global_keyword("define-syntax", scheme_defmacro_syntax, env);
  scheme_add_global_keyword("letrec-syntax", 
			    scheme_make_compiled_syntax(letmacro_syntax, 
							letmacro_expand), 
			    env);
}

Scheme_Object *
scheme_make_compiled_syntax(Scheme_Syntax *proc, 
			    Scheme_Syntax_Expander *eproc)
{
  Scheme_Object *syntax;

  if (scheme_starting_up)
    syntax = scheme_alloc_eternal_object();
  else
    syntax = scheme_alloc_stubborn_object();
  syntax->type = scheme_syntax_compiler_type;
  SCHEME_SYNTAX(syntax) = (Scheme_Object *)proc;
  SCHEME_SYNTAX_EXP(syntax) = (Scheme_Object *)eproc;

  if (!scheme_starting_up)
    scheme_end_stubborn_change((void *)syntax);

  return syntax;
}

static int check_form(char *name, Scheme_Object *form)
{
  int i;
  Scheme_Object *all = form;

  for (i = 0; SCHEME_STX_PAIRP(form); i++) {
    form = SCHEME_STX_CDR(form);
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax(name, form, all, "bad syntax (" IMPROPER_LIST_FORM ")");

  return i;
}

static void bad_form(Scheme_Object *form, const char *name, int l)
{ 
  scheme_wrong_syntax(name, NULL, form, 
		      "bad syntax (has %d part%s after keyword)", 
		      l - 1, (l != 2) ? "s" : "");
}

/* builtin syntax */

static void lambda_check(Scheme_Object *form)
{
  if (SCHEME_STX_PAIRP(form)
      && SCHEME_STX_PAIRP(SCHEME_STX_CDR(form))) {
    Scheme_Object *rest;
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
      return;
  }

  scheme_wrong_syntax("lambda", NULL, form, NULL);
}

static void lambda_check_args(char *who, Scheme_Object *args, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *v, *a;
  DupCheckRecord r;

  if (!SCHEME_STX_SYMBOLP(args)) {
    for (v = args; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      a = SCHEME_STX_CAR(v);
      scheme_check_identifier(who, a, NULL, env, form);
    }

    if (!SCHEME_STX_NULLP(v)) {
      if (!SCHEME_STX_SYMBOLP(v)) {
	scheme_check_identifier(who, v, NULL, env, form);
      }
    }

    /* Check for duplicate names: */
    scheme_begin_dup_symbol_check(&r, env);
    for (v = args; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      Scheme_Object *name;

      name = SCHEME_STX_CAR(v);
      scheme_dup_symbol_check(&r, who, name, "lambda", form);
    }
    if (!SCHEME_STX_NULLP(v)) {
      scheme_dup_symbol_check(&r, who, v, "lambda", form);
    }
  }
}

static Scheme_Object *
lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *args;
  
  lambda_check(form);

  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);
  lambda_check_args("lambda", args, form, env);

  return scheme_make_closure_compilation(env, form, rec, drec);
}

static Scheme_Object *
lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *args, *body, *fn;
  Scheme_Comp_Env *newenv;

  lambda_check(form);
  
  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);

  lambda_check_args("lambda", args, form, env);

  newenv = scheme_add_compilation_frame(args, env, 0);

  body = SCHEME_STX_CDR(form);
  body = SCHEME_STX_CDR(body);
  body = scheme_datum_to_syntax(body, form, form, 0, 0);

  body = scheme_add_env_renames(body, newenv, env);

  args = scheme_add_env_renames(args, newenv, env); /* for re-expansion */

  fn = SCHEME_STX_CAR(form);

  return scheme_datum_to_syntax(icons(fn,
				      icons(args,
					    scheme_expand_block(body,
								newenv,
								depth, 
								scheme_false))),
				form, form, 
				0, 1);
}

void scheme_set_global_bucket(char *who, Scheme_Bucket *b, Scheme_Object *val,
			      int set_undef)
{
  if ((((Scheme_Bucket_With_Flags *)b)->flags & (GLOB_IS_CONST | GLOB_IS_KEYWORD))
      && b->val) {
    Scheme_Object *key = (Scheme_Object *)b->key;
    scheme_raise_exn(MZEXN_VARIABLE_KEYWORD, key,
		     "%s: cannot redefine %s: %S", 
		     who,
		     (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_IS_CONST)
		     ? "constant"
		     : "keyword",
		     (Scheme_Object *)key);
  } else if (b->val || set_undef)
    b->val = val;
  else {
    scheme_raise_exn(MZEXN_VARIABLE, b->key,
		     "%s: cannot set undefined identifier: %S",
		     who,
		     (Scheme_Object *)b->key);
    
  }
}

static Scheme_Object *
define_values_execute(Scheme_Object *data)
{
  Scheme_Object *vars, *vals, *l, *name;
  int i, g, show_any;
  Scheme_Bucket *b;

  vars = SCHEME_CAR(data);
  vals = SCHEME_CDR(data);

  vals = _scheme_eval_linked_expr_multi(vals);
  if (SAME_OBJ(vals, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Object *v, **values;

    for (v = vars, i = 0; SCHEME_PAIRP(v); i++, v = SCHEME_CDR(v)) {}
    
    g = scheme_current_thread->ku.multiple.count;
    if (i == g) {
      values = scheme_current_thread->ku.multiple.array;
      for (i = 0; i < g; i++, vars = SCHEME_CDR(vars)) {
	b = (Scheme_Bucket *)SCHEME_CAR(vars);
	scheme_set_global_bucket("define-values", b, values[i], 1);
	scheme_shadow(((Scheme_Bucket_With_Home *)b)->home, (Scheme_Object *)b->key, 1);
      }
	
      return scheme_void;
    }
  } else if (SCHEME_PAIRP(vars) && SCHEME_NULLP(SCHEME_CDR(vars))) {
    b = (Scheme_Bucket *)SCHEME_CAR(vars);
    scheme_set_global_bucket("define-values", b, vals, 1);
    scheme_shadow(((Scheme_Bucket_With_Home *)b)->home, (Scheme_Object *)b->key, 1);

    return scheme_void;
  } else
    g = 1;
  
  l = vars;
  for (i = 0; SCHEME_PAIRP(l); i++, l = SCHEME_CDR(l)) {}

  show_any = i;

  if (show_any)
    name = (Scheme_Object *)((Scheme_Bucket *)SCHEME_CAR(vars))->key;
  else
    name = NULL;
  
  {
    const char *symname;
    symname = (show_any ? scheme_symbol_name(name) : "");
    scheme_wrong_return_arity("define-values",
			      i, g,
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_thread->ku.multiple.array,
			      "%s%s%s",
			      show_any ? "defining \"" : "0 names",
			      symname,
			      show_any ? ((i == 1) ? "\"" : "\", ...") : "");
  }

  return NULL;
}

static Scheme_Object *
define_values_link(Scheme_Object *data, Link_Info *link)
{
  Scheme_Object *vars, *orig_vars = SCHEME_CAR(data);
  Scheme_Object *val, *orig_val = SCHEME_CDR(data);

  vars = scheme_link_list(orig_vars, link);
  val = scheme_link_expr(orig_val, link);

  if (SAME_OBJ(vars, orig_vars) && SAME_OBJ(val, orig_val))
    return data;
  else
    return scheme_make_syntax_linked(DEFINE_VALUES_EXPD, cons(vars, val));
}

static Scheme_Object *
define_values_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *vars = SCHEME_CAR(data);
  Scheme_Object *val = SCHEME_CDR(data);

  vars = scheme_resolve_list(vars, rslv);
  val = scheme_resolve_expr(val, rslv);

  return scheme_make_syntax_resolved(DEFINE_VALUES_EXPD, cons(vars, val));
}

void scheme_define_values_parse(Scheme_Object *form, 
				Scheme_Object **var, Scheme_Object **val,
				Scheme_Comp_Env *env)
{
  Scheme_Object *vars, *rest;
  int len;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("define-values", NULL, form, "illegal use (not at top-level)");

  len = check_form("define-values", form);
  if (len != 3)
    bad_form(form, "define-values", len);

  rest = SCHEME_STX_CDR(form);
  vars = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  *val = SCHEME_STX_CAR(rest);

  *var = vars;

  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *name, *rest;

    name = SCHEME_STX_CAR(vars);
    scheme_check_identifier("define-values", name, NULL, env, form);

    vars = SCHEME_STX_CDR(vars);

    for (rest = vars; SCHEME_STX_PAIRP(rest); rest = SCHEME_STX_CDR(rest)) {
      Scheme_Object *param;
      param = SCHEME_STX_CAR(rest);
      if (scheme_stx_bound_eq(param, name, env->genv->phase))
	scheme_wrong_syntax("define-values", name, form, "duplicate argument name");
    }
  }  

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax("define-values", *var, form, "bad variable list");
}

static Scheme_Object *
define_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *var, *val, *first = scheme_null, *last = NULL, *variables;
  Scheme_Env *globals;

  globals = env->genv;
  
  scheme_define_values_parse(form, &var, &val, env);
  variables = var;
  
  while (SCHEME_STX_PAIRP(var)) {
    Scheme_Object *name, *pr, *bucket;

    name = SCHEME_STX_CAR(var);

    if (rec[drec].resolve_module_ids || !env->genv->module) {
      bucket = (Scheme_Object *)scheme_global_bucket(SCHEME_STX_SYM(name),
						     globals);
    } else {
      /* Create a module variable reference, so that idx is preserved: */
      bucket = scheme_alloc_object();
      bucket->type = scheme_module_variable_type;
      SCHEME_PTR1_VAL(bucket) = env->genv->module->self_modidx;
      SCHEME_PTR2_VAL(bucket) = SCHEME_STX_SYM(name);
    }

    pr = cons(bucket, scheme_null);
    if (last)
      SCHEME_STX_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    var = SCHEME_STX_CDR(var);
  }  

  scheme_compile_rec_done_local(rec, drec);
  if (SCHEME_STX_PAIRP(first) && SCHEME_STX_NULLP(SCHEME_STX_CDR(first))) {
    var = SCHEME_STX_CAR(variables);
    rec[drec].value_name = SCHEME_STX_SYM(var);
  }

  val = scheme_compile_expr(val, env, rec, drec);

  return scheme_make_syntax_compiled(DEFINE_VALUES_EXPD, cons(first, val));
}

static Scheme_Object *
define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *var, *val, *fn;

  scheme_define_values_parse(form, &var, &val, env);

  env = scheme_no_defines(env);

  if (SCHEME_STX_PAIRP(var) && SCHEME_STX_NULLP(SCHEME_STX_CDR(var)))
    boundname = SCHEME_STX_CAR(var);
  else
    boundname = scheme_false;

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(icons(fn,
				      icons(var,
					    icons(scheme_expand_expr(val, env, depth, boundname), 
						  scheme_null))),
				form,
				form,
				0, 1);
}

static Scheme_Object *
quote_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *v, *rest;

  rest = SCHEME_STX_CDR(form);
  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax("quote", NULL, form, "bad syntax (wrong number of parts)");

  scheme_compile_rec_done_local(rec, drec);
  scheme_default_compile_rec(rec, drec);
  
  v = SCHEME_STX_CAR(rest);

  if (SCHEME_STXP(v))
    return scheme_syntax_to_datum(v, 0, NULL);
  else
    return v;
}

static Scheme_Object *
quote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *first, *rest;

  first = SCHEME_STX_CAR(form);
  rest = SCHEME_STX_CDR(form);

  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax("quote", NULL, form, "bad syntax (wrong number of parts)");

  return form;
}

static Scheme_Object *
if_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len, opt;
  Scheme_Object *test, *thenp, *elsep, *name, *rest;
  Scheme_Compile_Info recs[3];

  len = check_form("if", form);
  if (!(((len == 3) || (len == 4))))
    bad_form(form, "if", len);

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  if (len == 4) {
    rest = SCHEME_STX_CDR(rest);
    elsep = SCHEME_STX_CAR(rest);
  } else
    elsep = scheme_compiled_void();

  scheme_init_compile_recs(rec, drec, recs, 3);
  recs[1].value_name = name;
  recs[2].value_name = name;

  env = scheme_no_defines(env);

  test = scheme_compile_expr(test, env, recs, 0);

  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    opt = 1;
    
    if (SCHEME_FALSEP(test)) {
      /* compile other branch only to get syntax checking: */
      recs[2].dont_mark_local_use = 1;
      scheme_compile_expr(thenp, env, recs, 2);

      if (len == 4)
	test = scheme_compile_expr(elsep, env, recs, 1);
      else
	test = elsep;
    } else {
      if (len == 4) {
	/* compile other branch only to get syntax checking: */
	recs[2].dont_mark_local_use = 1;
	scheme_compile_expr(elsep, env, recs, 2);
      }
      
      test = scheme_compile_expr(thenp, env, recs, 1);
    }
  } else {
    opt = 0;
    thenp = scheme_compile_expr(thenp, env, recs, 1);
    if (len == 4)
      elsep = scheme_compile_expr(elsep, env, recs, 2);
  }

  scheme_merge_compile_recs(rec, drec, recs, (opt || (len == 3)) ? 2 : 3);
  
  if (opt)
    return test;
  else
    return scheme_make_branch(test, thenp, elsep);
}

static Scheme_Object *
if_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *test, *rest, *thenp, *elsep, *fn;
  int len;
  len = check_form("if", form);

  if (!(((len == 3) || (len == 4))))
    bad_form(form, "if", len);

  env = scheme_no_defines(env);

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  test = scheme_expand_expr(test, env, depth, scheme_false);

  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  thenp = scheme_expand_expr(thenp, env, depth, boundname);

  rest = SCHEME_STX_CDR(rest);
  if (!SCHEME_STX_NULLP(rest)) {
    elsep = SCHEME_STX_CAR(rest);
    elsep = scheme_expand_expr(elsep, env, depth, boundname);
    rest = icons(elsep, scheme_null);
  } else {
    rest = scheme_null;
  }

  rest = icons(thenp, rest);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(icons(fn, icons(test, rest)),
				form, form, 
				0, 1);
}

static Scheme_Object *
with_cont_mark_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *key, *val, *expr, *name;
  Scheme_Compile_Info recs[3];
  Scheme_With_Continuation_Mark *wcm;
  int len;
  len = check_form("with-continuation-mark", form);

  if (len != 4)
    bad_form(form, "with-continuation-mark", len);

  env = scheme_no_defines(env);

  form = SCHEME_STX_CDR(form);
  key = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  val = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  expr = SCHEME_STX_CAR(form);

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  scheme_init_compile_recs(rec, drec, recs, 3);
  recs[2].value_name = name;

  key = scheme_compile_expr(key, env, recs, 0);
  val = scheme_compile_expr(val, env, recs, 1);
  expr = scheme_compile_expr(expr, env, recs, 2);

  scheme_merge_compile_recs(rec, drec, recs, 3);

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->type = scheme_with_cont_mark_type;
  wcm->key = key;
  wcm->val = val;
  wcm->body = expr;
  
  return (Scheme_Object *)wcm;
}

static Scheme_Object *
with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *key, *val, *expr, *orig_form = form, *fn;
  int len;
  len = check_form("with-continuation-mark", form);

  if (len != 4)
    bad_form(form, "with-continuation-mark", len);

  env = scheme_no_defines(env);

  form = SCHEME_STX_CDR(form);
  key = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  val = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  expr = SCHEME_STX_CAR(form);

  key = scheme_expand_expr(key, env, depth, scheme_false);
  val = scheme_expand_expr(val, env, depth, scheme_false);
  expr = scheme_expand_expr(expr, env, depth, boundname);

  fn = SCHEME_STX_CAR(orig_form);
  return scheme_datum_to_syntax(icons(fn,
				      icons(key,
					    icons(val,
						  icons(expr, scheme_null)))),
				orig_form,
				orig_form, 
				0, 1);
}

static Scheme_Object *
set_execute (Scheme_Object *data)
{
  Scheme_Object *val, *set_undef;
  Scheme_Bucket *var;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  
  val = SCHEME_STX_CDR(data);
  val = _scheme_eval_linked_expr(val);

  var = (Scheme_Bucket *)SCHEME_STX_CAR(data);
  scheme_set_global_bucket("set!", var, val, SCHEME_TRUEP(set_undef));

  return scheme_void;
}

static Scheme_Object *
set_link(Scheme_Object *data, Link_Info *link)
{
  Scheme_Object *orig_var, *var, *orig_val, *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  orig_var = SCHEME_CAR(SCHEME_CDR(data));
  orig_val = SCHEME_CDR(SCHEME_CDR(data));

  var = scheme_link_expr(orig_var, link);
  val = scheme_link_expr(orig_val, link);
  
  if (SAME_OBJ(var, orig_var) && SAME_OBJ(val, orig_val))
    return data;
  else
    return scheme_make_syntax_linked(SET_EXPD, cons(set_undef, cons(var, val)));
}

static Scheme_Object *
set_resolve(Scheme_Object *data, Resolve_Info *rslv)
{
  Scheme_Object *var, *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  var = SCHEME_CAR(data);
  val = SCHEME_CDR(data);
  
  val = scheme_resolve_expr(val, rslv);

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    Scheme_Let_Value *lv;
    Scheme_Object *cv;
    int flags, li;

    cv = scheme_compiled_void();

    lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
    lv->type = scheme_let_value_type;
    lv->body = cv;
    lv->count = 1;
    li = scheme_resolve_info_lookup(rslv, SCHEME_LOCAL_POS(var), &flags);
    lv->position = li;
    lv->autobox = (flags & SCHEME_INFO_BOXED);
    lv->value = val;

    if (!(flags & SCHEME_INFO_BOXED))
      scheme_signal_error("internal error: set!: set!ed local variable is not boxed");

    return (Scheme_Object *)lv;
  }

  var = scheme_resolve_expr(var, rslv);
  
  return scheme_make_syntax_resolved(SET_EXPD, cons(set_undef, cons(var, val)));
}

static Scheme_Object *
set_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *var, *val, *name, *body, *rest;
  int l, set_undef;

  l = check_form("set!", form);
  if (l != 3)
    bad_form(form, "set!", l);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  body = SCHEME_STX_CAR(rest);
  
  scheme_check_identifier("set!", name, NULL, env, form);

  var = scheme_static_distance(name, env, 
			       SCHEME_SETTING 
			       + SCHEME_GLOB_ALWAYS_REFERENCE
			       + (rec[drec].dont_mark_local_use 
				  ? SCHEME_DONT_MARK_USE 
				  : 0)
			       + (rec[drec].resolve_module_ids
				  ? SCHEME_RESOLVE_MODIDS
				  : 0));

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
    /* Redirect to a macro? */
    if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
      form = scheme_apply_macro(name, SCHEME_PTR_VAL(SCHEME_PTR_VAL(var)), form, env, scheme_false);
			      
      return scheme_compile_expr(form, env, rec, drec);
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
    scheme_wrong_syntax("set!", name, form, "cannot mutate syntax identifier");
  }

  scheme_compile_rec_done_local(rec, drec);
  rec[drec].value_name = SCHEME_STX_SYM(name);

  val = scheme_compile_expr(body, scheme_no_defines(env), rec, drec);

  /* check for (set! x x) */
  if (SAME_TYPE(SCHEME_TYPE(var), SCHEME_TYPE(val))) {
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)
	|| SAME_TYPE(SCHEME_TYPE(var), scheme_local_unbox_type)) {
      /* local */
      if (SCHEME_LOCAL_POS(var) == SCHEME_LOCAL_POS(val))
	return scheme_compiled_void();
    } else {
      /* global; can't do anything b/c var might be undefined or constant */
    }
  }
  
  set_undef = SCHEME_TRUEP(scheme_get_param(scheme_config,
					    MZCONFIG_ALLOW_SET_UNDEFINED));
  
  return scheme_make_syntax_compiled(SET_EXPD, 
				     cons(set_undef
					  ? scheme_true
					  : scheme_false,
					  cons(var, val)));
}

static Scheme_Object *
set_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *name, *var, *fn, *rhs;
  int l;
  l = check_form("set!", form);
  if (l != 3)
    bad_form(form, "set!", l);

  env = scheme_no_defines(env);

  name = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(name);

  scheme_check_identifier("set!", name, NULL, env, form);

  /* Make sure it's mutable, and check for redirects: */
  var = scheme_static_distance(name, env, SCHEME_SETTING);

  if ((depth != 0) && SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
    /* Redirect to a macro? */
    if (SAME_TYPE(SCHEME_TYPE(SCHEME_PTR_VAL(var)), scheme_id_macro_type)) {
      form = scheme_apply_macro(name, SCHEME_PTR_VAL(SCHEME_PTR_VAL(var)), form, env, scheme_false);
      
      if (depth > 0)
	depth--;

      return scheme_expand_expr(form, env, depth, name);
    }
  }

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
    scheme_wrong_syntax("set!", name, form, "cannot mutate syntax identifier");
  }


  fn = SCHEME_STX_CAR(form);
  rhs = SCHEME_STX_CDR(form);
  rhs = SCHEME_STX_CDR(rhs);
  rhs = SCHEME_STX_CAR(rhs);

  return scheme_datum_to_syntax(icons(fn,
				      icons(name,
					    icons(scheme_expand_expr(rhs, env, depth, name),
						  scheme_null))),
				form,
				form, 
				0, 1);
}

static Scheme_Object *
case_lambda_execute(Scheme_Object *expr)
{
  Scheme_Case_Lambda *seqin, *seqout;
  int i;
  Scheme_Thread *p = scheme_current_thread;

  seqin = (Scheme_Case_Lambda *)expr;

  seqout = (Scheme_Case_Lambda *)
    scheme_malloc_stubborn_tagged(sizeof(Scheme_Case_Lambda)
				  + (seqin->count - 1) * sizeof(Scheme_Object *));
  seqout->type = scheme_case_closure_type;
  seqout->count = seqin->count;
  seqout->name = seqin->name;

  for (i = 0; i < seqin->count; i++) {
    if (SAME_TYPE(SCHEME_TYPE(seqin->array[i]), scheme_linked_closure_type))
      seqout->array[i] = seqin->array[i];
    else {
      Scheme_Object *lc;
      lc = scheme_make_linked_closure(p, seqin->array[i], 1);
      seqout->array[i] = lc;
    }
  }

  scheme_end_stubborn_change((void *)seqout);

  return (Scheme_Object *)seqout;
}

static Scheme_Object *
case_lambda_link(Scheme_Object *expr, Link_Info *link)
{
  int i;
  Scheme_Case_Lambda *oseq = (Scheme_Case_Lambda *)expr;
  Scheme_Case_Lambda *nseq;

  nseq = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (oseq->count - 1) * sizeof(Scheme_Object *));
  nseq->type = scheme_case_lambda_sequence_type;
  nseq->count = oseq->count;
  nseq->name = oseq->name;

  for (i = 0; i < oseq->count; i++) {
    Scheme_Object *le;
    le = scheme_link_expr(oseq->array[i], link);
    nseq->array[i] = le;
  }

  for (i = 0; i < oseq->count; i++) {
    if (!SAME_OBJ(oseq->array[i], nseq->array[i])) {
      return scheme_make_syntax_linked(CASE_LAMBDA_EXPD, (Scheme_Object *)nseq);
    }
  }

  return expr;
}

static Scheme_Object *
case_lambda_resolve(Scheme_Object *expr, Resolve_Info *rslv)
{
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    Scheme_Object *le;
    le = scheme_resolve_expr(seq->array[i], rslv);
    seq->array[i] = le;
  }

  return scheme_make_syntax_resolved(CASE_LAMBDA_EXPD, expr);
}

static void case_lambda_check_line(Scheme_Object *line, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *body, *args;

  if (!SCHEME_STX_PAIRP(line))
    scheme_wrong_syntax("case-lambda", line, form, NULL);
  
  body = SCHEME_STX_CDR(line);
  args = SCHEME_STX_CAR(line);
  
  lambda_check_args("case-lambda", args, form, env);
  
  if (!SCHEME_STX_PAIRP(body))
    scheme_wrong_syntax("case-lambda", line, form, "bad syntax (%s)",
			SCHEME_STX_NULLP(body) ? "empty body" : IMPROPER_LIST_FORM);
}

static Scheme_Object *
case_lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *list, *last, *c, *orig_form = form, *name;
  Scheme_Case_Lambda *cl;
  int i, count = 0;
  Scheme_Compile_Info *recs;
  
  form = SCHEME_STX_CDR(form);

  if (SCHEME_STX_NULLP(form)) {
    form = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						 - sizeof(Scheme_Object*));

    form->type = scheme_case_lambda_sequence_type;
    ((Scheme_Case_Lambda *)form)->count = 0;
    ((Scheme_Case_Lambda *)form)->name = rec[drec].value_name;

    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);

    return scheme_make_syntax_compiled(CASE_LAMBDA_EXPD, form);
  }

  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax("case-lambda", form, orig_form, NULL);
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form))) {
    c = SCHEME_STX_CAR(form);

    case_lambda_check_line(c, orig_form, env);

    c = icons(scheme_datum_to_syntax(lambda_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
	      c);
    c = scheme_datum_to_syntax(c, orig_form, orig_form, 0, 1);
    
    return lambda_syntax(c, env, rec, drec);
  }

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  list = last = NULL;
  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *clause;
    clause = SCHEME_STX_CAR(form);
    case_lambda_check_line(clause, orig_form, env);

    c = icons(icons(lambda_symbol, clause),
	      scheme_null);

    c = scheme_datum_to_syntax(c, clause, clause, 0, 0);

    if (list)
      SCHEME_STX_CDR(last) = c;
    else
      list = c;

    last = c;
    form = SCHEME_STX_CDR(form);

    count++;
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax("case-lambda", form, orig_form, NULL);

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_stubborn_tagged(sizeof(Scheme_Case_Lambda)
				  + (count - 1) * sizeof(Scheme_Object *));
  cl->type = scheme_case_lambda_sequence_type;
  cl->count = count;
  if (!name)
    name = scheme_source_to_name(orig_form);
  cl->name = name;

  scheme_compile_rec_done_local(rec, drec);
  recs = MALLOC_N_RT(Scheme_Compile_Info, count);
  scheme_init_compile_recs(rec, drec, recs, count);

  for (i = 0; i < count; i++) {
    Scheme_Object *ce;
    ce = SCHEME_STX_CAR(list);
    ce = scheme_compile_expr(ce, env, recs, i);
    cl->array[i] = ce;
    list = SCHEME_STX_CDR(list);
  }

  scheme_end_stubborn_change((void *)cl);

  scheme_merge_compile_recs(rec, drec, recs, count);

  return scheme_make_syntax_compiled(CASE_LAMBDA_EXPD, (Scheme_Object *)cl);
}

static Scheme_Object *
case_lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *first, *last, *args, *body, *c, *orig_form = form;

  first = SCHEME_STX_CAR(form);
  first = icons(first, scheme_null);
  last = first;
  form = SCHEME_STX_CDR(form);

  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *line_form;
    Scheme_Comp_Env *newenv;

    line_form = SCHEME_STX_CAR(form);

    case_lambda_check_line(line_form, orig_form, env);
    
    body = SCHEME_STX_CDR(line_form);
    args = SCHEME_STX_CAR(line_form);

    body = scheme_datum_to_syntax(body, line_form, line_form, 0, 0);
    
    newenv = scheme_add_compilation_frame(args, env, 0);

    body = scheme_add_env_renames(body, newenv, env);
    args = scheme_add_env_renames(args, newenv, env);

    c = icons(icons(args, scheme_expand_block(body, newenv, depth, scheme_false)),
	      scheme_null);

    SCHEME_STX_CDR(last) = c;
    last = c;

    form = SCHEME_STX_CDR(form);
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax("case-lambda", form, orig_form, NULL);
  
  return scheme_datum_to_syntax(first, orig_form, orig_form, 0, 1);
}

Scheme_Object *bangboxenv_execute(Scheme_Object *data)
{
  int pos = SCHEME_INT_VAL(SCHEME_CAR(data));
  Scheme_Object *bb;
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Thread *p = scheme_current_thread;
#endif

  data = SCHEME_CDR(data);
  
  bb = scheme_make_envunbox(MZ_RUNSTACK[pos]);
  MZ_RUNSTACK[pos] = bb;

  return _scheme_tail_eval(data);
}

Scheme_Object *bangboxenv_link(Scheme_Object *data, Link_Info *info)
{
  Scheme_Object *code, *pos = SCHEME_CAR(data);

  code = scheme_link_expr(SCHEME_CDR(data), info);

  if (SAME_OBJ(code, SCHEME_CDR(data)))
    return data;
  else
    return scheme_make_syntax_linked(BOXENV_EXPD, cons(pos, code));
}

static Scheme_Object *
bangboxvalue_execute(Scheme_Object *data)
{
  int pos;
  Scheme_Object *val;

  pos = SCHEME_INT_VAL(SCHEME_STX_CAR(data));

  data = SCHEME_CDR(data);
  
  val = _scheme_eval_linked_expr_multi(data);

  if (SAME_OBJ(val, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Thread *p = scheme_current_thread;
    if (pos < p->ku.multiple.count) {
      Scheme_Object **naya, **a;
      int i;

      naya = MALLOC_N(Scheme_Object *, p->ku.multiple.count);
      a = p->ku.multiple.array;

      for (i = p->ku.multiple.count; i--; ) {
	naya[i] = a[i];
      }
      {
	Scheme_Object *eb;
	eb = scheme_make_envunbox(naya[pos]);
	naya[pos] = eb;
      }

      p->ku.multiple.array = naya;
    }
  } else if (!pos)
    val = scheme_make_envunbox(val);

  return val;
}

static Scheme_Object *
bangboxvalue_link(Scheme_Object *data, Link_Info *link)
{
  Scheme_Object *val, *pos = SCHEME_CAR(data);

  val = scheme_link_expr(SCHEME_CDR(data), link);

  if (SAME_OBJ(val, SCHEME_CDR(data)))
    return data;
  else
    return scheme_make_syntax_linked(BOXVAL_EXPD, cons(pos, val)); 
}

Scheme_Object *
scheme_link_let_value(Scheme_Object *data, Link_Info *link)
{
  Scheme_Let_Value *olv = (Scheme_Let_Value *)data;
  Scheme_Let_Value *nlv;
  Scheme_Object *v, *b;

  v = scheme_link_expr(olv->value, link);
  b = scheme_link_expr(olv->body, link);
  
  if ((v == olv->value) && (b == olv->body))
    return (Scheme_Object *)olv;

  nlv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
  nlv->type = scheme_let_value_type;

  nlv->value = v;
  nlv->body = b;

  nlv->position = olv->position;
  nlv->count = olv->count;
  nlv->autobox = olv->autobox;

  return (Scheme_Object *)nlv;
}

Scheme_Object *
scheme_link_let_one(Scheme_Object *data, Link_Info *link)
{
  Scheme_Let_One *olo = (Scheme_Let_One *)data;
  Scheme_Let_One *nlo;
  Scheme_Object *v, *b;

  v = scheme_link_expr(olo->value, link);
  b = scheme_link_expr(olo->body, link);
  
  if ((v == olo->value) && (b == olo->body))
    return (Scheme_Object *)olo;

  nlo = MALLOC_ONE_TAGGED(Scheme_Let_One);
  nlo->type = scheme_let_one_type;

  nlo->eval_type = olo->eval_type;

  nlo->value = v;
  nlo->body = b;

  return (Scheme_Object *)nlo;
}

Scheme_Object *
scheme_link_let_void(Scheme_Object *data, Link_Info *link)
{
  Scheme_Let_Void *olvd = (Scheme_Let_Void *)data;
  Scheme_Let_Void *nlvd;
  Scheme_Object *e;

  nlvd = MALLOC_ONE_TAGGED(Scheme_Let_Void);
  nlvd->type = scheme_let_void_type;

  e = scheme_link_expr(olvd->body, link);
  if (SAME_OBJ(e, olvd->body))
    return (Scheme_Object *)olvd;

  nlvd->body = e;

  nlvd->count = olvd->count;
  nlvd->autobox = olvd->autobox;

  return (Scheme_Object *)nlvd;
}

Scheme_Object *
scheme_link_letrec(Scheme_Object *data, Link_Info *link)
{
  Scheme_Letrec *olr = (Scheme_Letrec *)data;
  Scheme_Letrec *nlr;
  Scheme_Object *e, **procs;
  int i, count;

  count = olr->count;

  procs = MALLOC_N(Scheme_Object *, count);

  nlr = MALLOC_ONE_TAGGED(Scheme_Letrec);
  nlr->type = scheme_letrec_type;
  nlr->count = count;
  nlr->procs = procs;

  for (i = 0; i < count; i++) {
    e = scheme_link_expr(olr->procs[i], link);
    procs[i] = e;
  }

  e = scheme_link_expr(olr->body, link);
  nlr->body = e;

  if (SAME_OBJ(nlr->body, e)) {
    for (i = 0; i < count; i++) {
      if (!SAME_OBJ(olr->procs[i], procs[i]))
	break;
    }

    if (i == count)
      return (Scheme_Object *)olr;
  }

  return (Scheme_Object *)nlr;
}

Scheme_Object *
scheme_resolve_lets(Scheme_Object *form, Resolve_Info *info)
{
  Resolve_Info *linfo, *val_linfo;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body;
  Scheme_Let_Value *lv, *last = NULL;
  Scheme_Object *first = NULL, *body;
  Scheme_Letrec *letrec;
  int i, pos, opos, rpos, recbox, num_rec_procs = 0, extra_alloc;

  /* Special case: (let ([x E]) x) where E is lambda, case-lambda, etc. */
  if (!head->recursive && (head->count == 1) && (head->num_clauses == 1)) {
    clv = (Scheme_Compiled_Let_Value *)head->body;
    if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_local_type)
	&& (((Scheme_Local *)clv->body)->position == 0)) {
      Scheme_Type lhs;
      lhs = SCHEME_TYPE(clv->value);
      if ((lhs == scheme_compiled_unclosed_procedure_type)
	  || (lhs == scheme_case_lambda_sequence_type)) {
	linfo = scheme_resolve_info_extend(info, 0, 1, 0);
	return scheme_resolve_expr(clv->value, linfo);
      }
    }
  }

  /* Find body: */
  body = head->body;
  pre_body = NULL;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    body = pre_body->body;
  }

  recbox = 0;
  if (head->recursive) {
    /* Do we need to box vars in a letrec? */
    clv = (Scheme_Compiled_Let_Value *)head->body;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      int is_proc;
      is_proc = scheme_is_compiled_procedure(clv->value, 1);
      
      if (!(is_proc || (SCHEME_TYPE(clv->value) > _scheme_compiled_values_types_))) {
	recbox = 1;
	break;
      } else {
	int j;

	for (j = 0; j < clv->count; j++) {
	  if (clv->flags[j] & SCHEME_WAS_SET_BANGED) {
	    recbox = 1;
	    break;
	  }
	}
	if (recbox)
	  break;

	if (scheme_is_compiled_procedure(clv->value, 0))
	  num_rec_procs++;
      }
    }

    if (recbox)
      num_rec_procs = 0;
  } else {
    /* Sequence of single-value lets? */
    clv = (Scheme_Compiled_Let_Value *)head->body;
    for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
      if (clv->count != 1)
	break;
      if (clv->flags[0] & SCHEME_WAS_SET_BANGED)
	break;
    }
    if (i < 0) {
      /* Yes - build chain of Scheme_Let_Ones and we're done: */
      clv = (Scheme_Compiled_Let_Value *)head->body;

      for (i = 0; i < head->num_clauses; i++, clv = (Scheme_Compiled_Let_Value *)clv->body) {
	Scheme_Let_One *lo;
	int j;

	lo = MALLOC_ONE_TAGGED(Scheme_Let_One);
	lo->type = scheme_let_one_type;

	/* First `i+1' now "exist at runtime". */
	/* The mapping is complicated because we now push in the order of 
	   the variables, but it was compiled using the inverse order. */
	linfo = scheme_resolve_info_extend(info, i + 1, head->count, i + 1);
	for (j = 0; j <= i; j++) {
	  scheme_resolve_info_add_mapping(linfo, j, i - j, 0);
	}

	{
	  Scheme_Object *le;
	  le = scheme_resolve_expr(clv->value, linfo);
	  lo->value = le;
	}
	{
	  int et;
	  et = scheme_get_eval_type(lo->value);
	  lo->eval_type = et;
	}

	if (last)
	  ((Scheme_Let_One *)last)->body = (Scheme_Object *)lo;
	else
	  first = (Scheme_Object *)lo;
	last = (Scheme_Let_Value *)lo;
      }

      linfo = scheme_resolve_info_extend(info, head->count, head->count, head->count);
      for (i = head->count; i--; ) {
	scheme_resolve_info_add_mapping(linfo, i, head->count - 1 - i, 0);
      }
      
      body = scheme_resolve_expr(body, linfo);
      if (last)
	((Scheme_Let_One *)last)->body = body;
      else
	first = body;
      
      return first;
    }
  }

  linfo = scheme_resolve_info_extend(info, head->count, head->count, head->count);

  /* Build mapping of compile-time indices to run-time indices, shuffling
     letrecs to fall together: */
  clv = (Scheme_Compiled_Let_Value *)head->body;
  pos = num_rec_procs;
  rpos = 0; opos = 0;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    int j;

    for (j = 0; j < clv->count; j++) {
      int p;

      if (num_rec_procs && scheme_is_compiled_procedure(clv->value, 0))
	p = rpos++;
      else
	p = pos++;
      
      scheme_resolve_info_add_mapping(linfo, opos, p,
				      ((recbox 
					|| (clv->flags[j] & SCHEME_WAS_SET_BANGED))
				       ? SCHEME_INFO_BOXED
				       : 0));

      opos++;
    }
  }

  /* Resolve body: */
  body = scheme_resolve_expr(body, linfo);

  /* Check for collasping let_void: */
  extra_alloc = 0;
  val_linfo = linfo;
  if (!num_rec_procs) {
    if (SAME_TYPE(SCHEME_TYPE(body), scheme_let_void_type)) {
      Scheme_Let_Void *lvd = (Scheme_Let_Void *)body;
      
      if (!!lvd->autobox == !!recbox) {
	/* Do collapse: */
	extra_alloc = lvd->count;
	body = lvd->body;
	val_linfo = scheme_resolve_info_extend(linfo, extra_alloc, 0, 0);
      }
    }
  }

  if (num_rec_procs) {
    Scheme_Object **sa;
    letrec = MALLOC_ONE_TAGGED(Scheme_Letrec);
    letrec->type = scheme_letrec_type;
    letrec->count = num_rec_procs;
    sa = MALLOC_N(Scheme_Object *, num_rec_procs);
    letrec->procs = sa;
  } else
    letrec = NULL;

  /* Resolve values: */
  clv = (Scheme_Compiled_Let_Value *)head->body;
  rpos = 0; opos = 0;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    int isproc;
    Scheme_Object *expr;
    isproc = scheme_is_compiled_procedure(clv->value, 0);
    expr = scheme_resolve_expr(clv->value, val_linfo);
    if (num_rec_procs && isproc) {
      letrec->procs[rpos++] = expr;
    } else {
      int j;

      lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
      if (last)
	last->body = (Scheme_Object *)lv;
      else
	first = (Scheme_Object *)lv;
      last = lv;
      
      lv->type = scheme_let_value_type;
      lv->value = expr;
      if (clv->count) {
	int li;
	li = scheme_resolve_info_lookup(linfo, clv->position, NULL);
	lv->position = li + extra_alloc;
      } else
	lv->position = 0;
      lv->count = clv->count;
      lv->autobox = recbox;

      for (j = lv->count; j--; ) {
	if (!recbox
	    && (scheme_resolve_info_flags(linfo, opos + j) & SCHEME_INFO_BOXED)) {
	  Scheme_Object *sl;
	  sl = scheme_make_syntax_resolved(BOXVAL_EXPD, 
					   cons(scheme_make_integer(j),
						lv->value));
	  lv->value = sl;
	}
      }
    }
    opos += clv->count;
  }
  
  if (letrec) {
    letrec->body = body;
    if (last)
      last->body = (Scheme_Object *)letrec;
    else
      first = (Scheme_Object *)letrec;
  } else
    last->body = body;

  {
    Scheme_Let_Void *lvd;

    lvd = MALLOC_ONE_TAGGED(Scheme_Let_Void);
    lvd->type = scheme_let_void_type;
    lvd->body = first;
    lvd->count = head->count + extra_alloc;
    lvd->autobox = recbox;

    first = (Scheme_Object *)lvd;
  }

  return first;
}

static Scheme_Object *
gen_let_syntax (Scheme_Object *form, Scheme_Comp_Env *origenv, char *formname,
		int star, int recursive, int multi, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *bindings, *l, *binding, *name, **names, *forms, *defname;
  int num_clauses, num_bindings, i, j, k, m, pre_k;
  Scheme_Comp_Env *frame, *env;
  Scheme_Compile_Info *recs;
  Scheme_Object *first = NULL;
  Scheme_Compiled_Let_Value *last = NULL, *lv;
  DupCheckRecord r;

#if 1
  if (!SCHEME_STXP(form))
    scheme_signal_error("not syntax: %V", form );
#endif

  if (scheme_stx_proper_list_length(form) < 3)
    scheme_wrong_syntax(formname, NULL, form, NULL);

  bindings = SCHEME_STX_CDR(form);
  bindings = SCHEME_STX_CAR(bindings);
  num_clauses = scheme_stx_proper_list_length(bindings);

  if (num_clauses < 0)
    scheme_wrong_syntax(formname, bindings, form, NULL);

  forms = SCHEME_STX_CDR(form);
  forms = SCHEME_STX_CDR(forms);
  forms = scheme_datum_to_syntax(forms, form, form, 0, 0);

  if (!num_clauses) {
    env = scheme_no_defines(origenv);
    return scheme_compile_sequence(forms, env, rec, drec);
  }
  
  if (multi) {
    num_bindings = 0;
    l = bindings;
    while (!SCHEME_STX_NULLP(l)) {
      Scheme_Object *clause, *names, *rest;
      int num_names;

      clause = SCHEME_STX_CAR(l);
      
      if (!SCHEME_STX_PAIRP(clause))
	rest = NULL;
      else {
	rest = SCHEME_STX_CDR(clause);
	if (!SCHEME_STX_PAIRP(rest))
	  rest = NULL;
	else {
	  rest = SCHEME_STX_CDR(rest);
	  if (!SCHEME_STX_NULLP(rest))
	    rest = NULL;
	}
      }
      if (!rest)
	scheme_wrong_syntax(formname, clause, form, NULL);
      
      names = SCHEME_STX_CAR(clause);
      
      num_names = scheme_stx_proper_list_length(names);
      if (num_names < 0)
	scheme_wrong_syntax(formname, names, form, NULL);
     
      num_bindings += num_names;
 
      l = SCHEME_STX_CDR(l);
    }
  } else
    num_bindings = num_clauses;


  names = MALLOC_N_STUBBORN(Scheme_Object *, num_bindings);

  frame = scheme_new_compilation_frame(num_bindings, 0, origenv);
  env = frame;

  recs = MALLOC_N_RT(Scheme_Compile_Info, (num_clauses + 1));

  defname = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);
  scheme_init_compile_recs(rec, drec, recs, num_clauses + 1);

  
  if (!star) {
    scheme_begin_dup_symbol_check(&r, env);
  }

  for (i = 0, k = 0; i < num_clauses; i++) {
    if (!SCHEME_STX_PAIRP(bindings))
      scheme_wrong_syntax(formname, bindings, form, NULL);
    binding = SCHEME_STX_CAR(bindings);
    if (!SCHEME_STX_PAIRP(binding) || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(binding)))
      scheme_wrong_syntax(formname, binding, form, NULL);

    {
      Scheme_Object *rest;
      rest = SCHEME_STX_CDR(binding);
      if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
	scheme_wrong_syntax(formname, binding, form, NULL);
    }
    
    pre_k = k;

    name = SCHEME_STX_CAR(binding);
    if (multi) {
      while (!SCHEME_STX_NULLP(name)) {
	Scheme_Object *n;
	n = SCHEME_STX_CAR(name);
	names[k] = n;
	scheme_check_identifier(formname, names[k], NULL, env, form);
	k++;
	name = SCHEME_STX_CDR(name);
      }

      for (j = pre_k; j < k; j++) {
	for (m = j + 1; m < k; m++) {
	  if (scheme_stx_bound_eq(names[m], names[j], env->genv->phase))
	    scheme_wrong_syntax(formname, NULL, form,
				"multiple bindings of `%S' in the same clause", 
				SCHEME_STX_SYM(names[m]));
	}
      }
    } else {
      scheme_check_identifier(formname, name, NULL, env, form);
      names[k++] = name;
    }
    
    if (!star) {
      for (m = pre_k; m < k; m++) {
	scheme_dup_symbol_check(&r, formname, names[m], "binding", form);
      }
    }

    lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
    lv->type = scheme_compiled_let_value_type;
    if (!last)
      first = (Scheme_Object *)lv;
    else
      last->body = (Scheme_Object *)lv;
    last = lv;
    lv->count = (k - pre_k);
    lv->position = pre_k;

    if (lv->count == 1)
      recs[i].value_name = SCHEME_STX_SYM(names[pre_k]);

    if (!recursive) {
      Scheme_Object *ce, *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      rhs = scheme_add_env_renames(rhs, env, origenv);
      ce = scheme_compile_expr(rhs, env, recs, i);
      lv->value = ce;
    } else {
      Scheme_Object *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      lv->value = rhs;
    }
    
    if (star || recursive) {
      for (m = pre_k; m < k; m++) {
	scheme_add_compilation_binding(m, names[m], frame);
      }
    }
    
    bindings = SCHEME_STX_CDR(bindings);
  }
  
  if (!star && !recursive) {
    for (i = 0; i < num_bindings; i++) {
      scheme_add_compilation_binding(i, names[i], frame);
    }
  }

  if (recursive) {
    lv = (Scheme_Compiled_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)lv->body) {
      Scheme_Object *ce, *rhs;
      rhs = lv->value;
      rhs = scheme_add_env_renames(rhs, env, origenv);
      ce = scheme_compile_expr(rhs, env, recs, i);
      lv->value = ce;
    }
  }

  recs[num_clauses].value_name = defname ? SCHEME_STX_SYM(defname) : NULL;
  {
    Scheme_Object *cs;
    forms = scheme_add_env_renames(forms, env, origenv);
    cs = scheme_compile_sequence(forms, env, recs, num_clauses);
    last->body = cs;
  }

  /* Save flags: */
  lv = (Scheme_Compiled_Let_Value *)first;
  for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)lv->body) {
    int *flags;
    flags = scheme_env_get_flags(env, lv->position, lv->count);
    lv->flags = flags;
  }

  {
    Scheme_Let_Header *head;
    
    head = MALLOC_ONE_TAGGED(Scheme_Let_Header);
    head->type = scheme_compiled_let_void_type;
    head->body = first;
    head->count = num_bindings;
    head->num_clauses = num_clauses;
    head->recursive = recursive;

    first = (Scheme_Object *)head;
  }
  
  scheme_merge_compile_recs(rec, drec, recs, num_clauses + 1);

  scheme_end_stubborn_change((void *)names);

  rec[drec].max_let_depth += num_bindings;

  return first;
}

static Scheme_Object *
do_let_expand(Scheme_Object *form, Scheme_Comp_Env *origenv, int depth, Scheme_Object *boundname,
	      const char *formname, int letrec, int multi, int letstar)
{
  int named;
  Scheme_Object *vars, *body, *first, *last, *name, *v, *vs, *vlist;
  Scheme_Comp_Env *use_env, *env;
  DupCheckRecord r;
  
  vars = SCHEME_STX_CDR(form);

  named = (!multi
	   && !letrec
	   && !letstar
	   && SCHEME_STX_PAIRP(vars) 
	   && SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(vars)));
  
  if (named)
    return named_let_syntax(form, origenv, NULL, 0, depth, boundname);

  if (!SCHEME_STX_PAIRP(vars))
    scheme_wrong_syntax(formname, vars, form, NULL);

  body = SCHEME_STX_CDR(vars);
  vars = SCHEME_STX_CAR(vars);

  if (!SCHEME_STX_PAIRP(body))
    scheme_wrong_syntax(formname, body, form, NULL);

  if (letstar) {
    if (!SCHEME_STX_NULLP(vars)) {
      Scheme_Object *a, *vr;

      if (!SCHEME_STX_PAIRP(vars))
	scheme_wrong_syntax(formname, vars, form, NULL);

      first = multi ? let_values_symbol : let_symbol;
      first = scheme_datum_to_syntax(first, form, scheme_sys_wraps(origenv), 0, 0);
      last = multi ? let_star_values_symbol : let_star_symbol;
      last = scheme_datum_to_syntax(last, form, scheme_sys_wraps(origenv), 0, 0);

      a = SCHEME_STX_CAR(vars);
      vr = SCHEME_STX_CDR(vars);
      body = icons(first,
		   icons(icons(a, scheme_null),
			 icons(icons(last, icons(vr, body)),
			       scheme_null)));
    } else {
      first = scheme_datum_to_syntax(let_values_symbol, form, scheme_sys_wraps(origenv), 0, 0);
      body = icons(first, icons(scheme_null, body));
    }

    body = scheme_datum_to_syntax(body, form, form, 0, 0);

    if (depth > 0)
      --depth;
    if (!depth)
      return body;
    else {
      env = scheme_no_defines(origenv);
      return scheme_expand_expr(body, env, depth, boundname);
    }
  }

  if (!letstar) {
    scheme_begin_dup_symbol_check(&r, origenv);
  }

  vlist = scheme_null;
  vs = vars;
  while (SCHEME_STX_PAIRP(vs)) {
    Scheme_Object *v2;
    v = SCHEME_STX_CAR(vs);
    if (SCHEME_STX_PAIRP(v))
      v2 = SCHEME_STX_CDR(v);
    else
      v2 = scheme_false;
    if (!SCHEME_STX_PAIRP(v2) || !SCHEME_STX_NULLP(SCHEME_STX_CDR(v2)))
      scheme_wrong_syntax(formname, v, form, NULL);

    name = SCHEME_STX_CAR(v);

    if (multi) {
      DupCheckRecord r2;
      Scheme_Object *names = name;
      scheme_begin_dup_symbol_check(&r2, origenv);
      while (SCHEME_STX_PAIRP(names)) {
	name = SCHEME_STX_CAR(names);

	scheme_check_identifier(formname, name, NULL, origenv, form);
	vlist = cons(name, vlist);

	scheme_dup_symbol_check(&r2, formname, name, "clause binding", form);
	if (!letstar)
	  scheme_dup_symbol_check(&r, formname, name, "binding", form);
	
	names = SCHEME_STX_CDR(names);
      }
      if (!SCHEME_STX_NULLP(names))
	scheme_wrong_syntax(formname, names, form, NULL);
    } else {
      scheme_check_identifier(formname, name, NULL, origenv, form);
      vlist = cons(name, vlist);
      if (!letstar)
	scheme_dup_symbol_check(&r, formname, name, "binding", form);
    }

    vs = SCHEME_STX_CDR(vs);
  }

  if (!SCHEME_STX_NULLP(vs))
    scheme_wrong_syntax(formname, vs, form, NULL);

  use_env = origenv;
  env = scheme_add_compilation_frame(vlist, origenv, 0);

  if (letrec)
    use_env = env;

  first = last = NULL;
  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *rhs;

    v = SCHEME_STX_CAR(vars);

    /* Make sure names gets their own renames: */
    name = SCHEME_STX_CAR(v);
    if (!multi) {
      name = scheme_add_env_renames(name, env, origenv);
      name = icons(name, scheme_null);
    } else {
      name = scheme_add_env_renames(name, env, origenv);
    }

    rhs = SCHEME_STX_CDR(v);
    rhs = SCHEME_STX_CAR(rhs);
    rhs = scheme_add_env_renames(rhs, use_env, origenv);
    
    v = scheme_expand_expr(rhs, use_env, depth, name);

    v = icons(icons(name, icons(v, scheme_null)), scheme_null);

    if (!first)
      first = v;
    else
      SCHEME_STX_CDR(last) = v;

    last = v;

    vars = SCHEME_STX_CDR(vars);
  }

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax(formname, vars, form, NULL);
  
  if (!first)
    first = scheme_null;

  body = scheme_datum_to_syntax(body, form, form, 0, 0);
  body = scheme_add_env_renames(body, env, origenv);
  body = scheme_expand_block(body, env, depth, boundname);

  if (named) {
    Scheme_Object *fn, *nm;
    fn = SCHEME_STX_CAR(form);
    nm = SCHEME_STX_CDR(form);
    nm = SCHEME_STX_CAR(nm);
    v = icons(fn,
	      icons(nm,
		    icons(first, body)));
  } else {
    if (multi && !letstar)
      v = SCHEME_STX_CAR(form);
    else
      v = scheme_datum_to_syntax((letrec 
				  ? letrec_values_symbol 
				  : let_values_symbol),
				 form, scheme_sys_wraps(origenv), 
				 0, 0);
    v = icons(v, icons(first, body));
  }

  return scheme_datum_to_syntax(v, form, form, 0, 1);
}

static Scheme_Object *
let_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_let_expand(form, env, depth, boundname, "let", 0, 0, 0);
}

static Scheme_Object *
let_star_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_let_expand(form, env, depth, boundname, "let*", 0, 0, 1);
}

static Scheme_Object *
letrec_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_let_expand(form, env, depth, boundname, "letrec", 1, 0, 0);
}

static Scheme_Object *
let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_let_expand(form, env, depth, boundname, "let", 0, 1, 0);
}

static Scheme_Object *
let_star_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_let_expand(form, env, depth, boundname, "let*", 0, 1, 1);
}

static Scheme_Object *
letrec_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_let_expand(form, env, depth, boundname, "letrec", 1, 1, 0);
}

static Scheme_Object *
let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *rest;

  if (!SCHEME_STX_PAIRP(SCHEME_STX_CDR(form)))
    scheme_wrong_syntax("let", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");

  rest = SCHEME_STX_CDR(form);
  if (SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(rest)))
    return named_let_syntax (form, env, rec, drec, 0, scheme_false);
  
  return gen_let_syntax(form, env, "let", 0, 0, 0, rec, drec);
}

static Scheme_Object *
let_star_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		 Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "let*", 1, 0, 0, rec, drec);
}

static Scheme_Object *
letrec_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "letrec", 0, 1, 0, rec, drec);
}

static Scheme_Object *
let_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "let-values", 0, 0, 1, rec, drec);
}

static Scheme_Object *
let_star_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		 Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "let*-values", 1, 0, 1, rec, drec);
}

static Scheme_Object *
letrec_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return gen_let_syntax(form, env, "letrec-values", 0, 1, 1, rec, drec);
}

static Scheme_Object *nl_car(Scheme_Object *l, Scheme_Object *form)
{
  Scheme_Object *s;
  
  if (!SCHEME_STX_PAIRP(l))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (non-symbol-value-pair)");
  s = SCHEME_STX_CAR(l);
  if (!SCHEME_STX_SYMBOLP(s))
    scheme_wrong_syntax("named let", s, form, 
			"bad syntax (name not an identifier)");

  return s;
}

static Scheme_Object *nl_cadr(Scheme_Object *l, Scheme_Object *form)
{
  Scheme_Object *rest;
  
  if (!SCHEME_STX_PAIRP(l) || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(l)))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (non-identifier-value-pair)");
  
  rest = SCHEME_STX_CDR(l);
  if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (extra data in indentifier-value pair)");
  
  return SCHEME_STX_CAR(rest);
}

static Scheme_Object *
named_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		  Scheme_Compile_Info *rec, int drec, int depth, Scheme_Object *boundname)
{
  Scheme_Object *name, *bindings, *vars, *vals, *forms, *rest;
  Scheme_Object *proc, *app, *letrec;

  rest = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(rest))
    rest = NULL;
  else {
    rest = SCHEME_STX_CDR(rest);
    if (!SCHEME_STX_PAIRP(rest))
      rest = NULL;
    else {
      rest = SCHEME_STX_CDR(rest);
      if (!SCHEME_STX_PAIRP(rest))
	rest = NULL;
    }
  }

  if (!rest)
    scheme_wrong_syntax("named let", NULL, form, NULL);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  bindings = SCHEME_STX_CAR(rest);
  if (!SCHEME_STX_PAIRP(bindings) && !SCHEME_STX_NULLP(bindings))
    scheme_wrong_syntax("named let", bindings, form, NULL);

  vars = scheme_named_map_1("named let", nl_car, bindings, form);
  vals = scheme_named_map_1("named let", nl_cadr, bindings, form);
  
  forms = SCHEME_STX_CDR(form);
  forms = SCHEME_STX_CDR(forms);
  forms = SCHEME_STX_CDR(forms);

  proc = icons(lambda_symbol, icons(vars, forms));
  
  letrec = icons(letrec_symbol,
		 icons(icons(icons(name, icons(proc, scheme_null)), scheme_null),
		       icons(name,
			     scheme_null)));
  app = icons(letrec, vals);

  app = scheme_datum_to_syntax(app, form, scheme_sys_wraps(env), 0, 1);

  if (rec)
    return scheme_compile_expr(app, env, rec, drec);
  else {
    if (depth > 0)
      --depth;
    if (!depth)
      return app;
    else
      return scheme_expand_expr(app, env, depth, boundname);
  }
}

Scheme_Object *scheme_compile_sequence(Scheme_Object *forms,
				       Scheme_Comp_Env *env, 
				       Scheme_Compile_Info *rec, int drec)
{
#if 1
  if (!SCHEME_STXP(forms))
    scheme_signal_error("not syntax: %V", forms);
#endif

  if (SCHEME_STX_PAIRP(forms) && SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    /* If it's a begin, we have to check some more... */
    Scheme_Object *first, *val;

    first = SCHEME_STX_CAR(forms);

    if (SCHEME_STX_PAIRP(first)) {
      first = scheme_check_immediate_macro(first, env, rec, drec, -1, scheme_false, &val);
      
      if (SAME_OBJ(val, scheme_begin_syntax)) {
	/* Flatten begin: */
	Scheme_Object *rest;
	rest = SCHEME_STX_CDR(first);
	if (scheme_stx_proper_list_length(rest) > 0) {
	  first = scheme_datum_to_syntax(rest, first, first, 0, 1);
	  return scheme_compile_sequence(first, env, rec, drec);
	}
      }
    }

    return scheme_compile_expr(first, env, rec, drec);
  } else {
    if (scheme_stx_proper_list_length(forms) < 0) {
      scheme_wrong_syntax("begin", NULL, icons(begin_symbol, forms), 
			  "bad syntax (" IMPROPER_LIST_FORM ")");
      return NULL;
    } else {
      Scheme_Object *body;
      body = scheme_compile_block(forms, env, rec, drec);
      return scheme_make_sequence_compilation(body, 1);
    }
  }
}

Scheme_Object *scheme_compiled_void()
{
  return scheme_void;
}

static Scheme_Object *
begin0_execute(Scheme_Object *obj)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v, **mv;
  int i, mc, apos;
  
  i = ((Scheme_Sequence *)obj)->count;

  v = _scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[0], p);
  i--;
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    mv = p->ku.multiple.array;
    mc = p->ku.multiple.count;
  } else {
    mv = NULL;
    mc = 0; /* makes compilers happy */
  }

  apos = 1;
  while (i--) {
    (void)_scheme_eval_linked_expr_multi_wp(((Scheme_Sequence *)obj)->array[apos++], p);
  }

  if (mv) {
    p->ku.multiple.array = mv;
    p->ku.multiple.count = mc;
  }

  return v;
}

static Scheme_Object *
begin0_link(Scheme_Object *obj, Link_Info *info)
{
  Scheme_Sequence *os = (Scheme_Sequence *)obj;
  Scheme_Sequence *ns;
  int i, count, dft;

  count = os->count;

  ns = (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
					       + (count - 1) 
					       * sizeof(Scheme_Object *));
  ns->type = scheme_begin0_sequence_type;
  ns->count = count;

  dft = 0;
  for (i = 0; i < count; i++) {
    Scheme_Object *le;
    le = scheme_link_expr(os->array[i], info);
    ns->array[i] = le;
    if (!SAME_OBJ(le, os->array[i]))
      dft = 1;
  }
  
  if (!dft)
    return obj;
  else
    return scheme_make_syntax_linked(BEGIN0_EXPD, (Scheme_Object *)ns);
}

static Scheme_Object *
begin0_resolve(Scheme_Object *obj, Resolve_Info *info)
{
  int i;
  
  i = ((Scheme_Sequence *)obj)->count;

  while (i--) {
    Scheme_Object *le;
    le = scheme_resolve_expr(((Scheme_Sequence *)obj)->array[i], info);
    ((Scheme_Sequence *)obj)->array[i] = le;
  }

  return scheme_make_syntax_resolved(BEGIN0_EXPD, obj);
}

static Scheme_Object *
do_begin_syntax(char *name,
		Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, 
		int zero)
{
  Scheme_Object *forms, *body;

  forms = SCHEME_STX_CDR(form);
  
  if (SCHEME_STX_NULLP(forms)) {
    if (!zero && scheme_is_toplevel(env))
      return scheme_compiled_void();
    scheme_wrong_syntax(name, NULL, form, "bad syntax (empty form)");
    return NULL;
  }

  check_form(name, forms);

  if (zero)
    env = scheme_no_defines(env);

  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    forms = SCHEME_STX_CAR(forms);
    return scheme_compile_expr(forms, env, rec, drec);
  }

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level */
    if (zero) {
      /* First expression is not part of the block: */
      Scheme_Compile_Info recs[2];
      Scheme_Object *first, *rest, *vname;

      vname = rec[drec].value_name;
      scheme_compile_rec_done_local(rec, drec);

      scheme_init_compile_recs(rec, drec, recs, 2);
      recs[0].value_name = vname;

      first = SCHEME_STX_CAR(forms);
      first = scheme_compile_expr(first, env, recs, 0);
      rest = SCHEME_STX_CDR(forms);
      rest = scheme_compile_list(rest, env, recs, 1);
      
      scheme_merge_compile_recs(rec, drec, recs, 2);

      body = icons(first, rest);
    } else 
      body = scheme_compile_list(forms, env, rec, drec);
  } else
    /* Top level */
    body = scheme_compile_list(forms, env, rec, drec);

  forms = scheme_make_sequence_compilation(body, zero ? -1 : 1);

  if (!zero || (NOT_SAME_TYPE(SCHEME_TYPE(forms), scheme_begin0_sequence_type)))
    return forms;

  return scheme_make_syntax_compiled(BEGIN0_EXPD, forms);
}

static Scheme_Object *
begin_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_begin_syntax("begin", form, env, rec, drec, 0);
}

static Scheme_Object *
begin0_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_begin_syntax("begin0", form, env, rec, drec, 1);
}

static Scheme_Object *
do_begin_expand(char *name, Scheme_Object *form_name,
		Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname,
		int zero)
{
  Scheme_Object *rest;
  Scheme_Object *orig_form = form;

  check_form(name, form);

  rest = SCHEME_STX_CDR(form);

  if (SCHEME_STX_NULLP(rest)) {
    if (!zero && scheme_is_toplevel(env))
      return form;
    scheme_wrong_syntax(name, NULL, form, "bad syntax (empty form)");
    return NULL;
  }

  if (zero)
    env = scheme_no_defines(env);

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level: */
    if (zero) {
      Scheme_Object *fst;
      fst = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);
      form = icons(scheme_expand_expr(fst, env, depth, scheme_false),
		   scheme_expand_list(scheme_datum_to_syntax(rest, 
							     form, 
							     form, 0, 0),
				     env, depth, boundname));
    } else {
      form = scheme_expand_list(scheme_datum_to_syntax(rest, form, form, 0, 0),
				env, depth, boundname);
#if 0
      if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form)))
	return SCHEME_STX_CAR(form);
#endif
    }
  } else {
    /* Top level */
    form =  scheme_expand_list(scheme_datum_to_syntax(rest, form, form, 0, 0),
			       env, depth, boundname);
  }

  return scheme_datum_to_syntax(icons(form_name, form), 
				orig_form, orig_form, 
				0, 1);
}

static Scheme_Object *
begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_begin_expand("begin", begin_symbol, form, env, depth, boundname, 0);
}

static Scheme_Object *
begin0_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_begin_expand("begin0", begin0_symbol, form, env, depth, boundname, 1);
}

static Scheme_Object *
unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len;
  char *who;
  Scheme_Object *fn;

  if (rec)
    scheme_compile_rec_done_local(rec, drec);

  fn = SCHEME_STX_CAR(form);
  who = scheme_symbol_val(SCHEME_STX_SYM(fn));

  len = check_form(who, form);
  if (len != 2)
    bad_form(form, who, len);

  scheme_wrong_syntax(who, NULL, form, "not in quasiquote");
  return NULL;
}

static Scheme_Object *
unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return unquote_syntax(form, env, NULL, 0);
}

static Scheme_Object *
lexical_syntax_link(Scheme_Object *obj, Link_Info *info)
{
  Scheme_Object *stx;
  Scheme_Object *phse;

  phse = SCHEME_CAR(obj);
  stx = SCHEME_CDR(obj);

  if (info && info->phase && info->module)
    return scheme_stx_phase_shift(stx, info->phase - SCHEME_INT_VAL(phse),
				  info->module->self_modidx, info->link_midx);
  else
    return stx;
}

static Scheme_Object *
lexical_syntax_resolve(Scheme_Object *obj, Resolve_Info *info)
{
  return scheme_make_syntax_resolved(QUOTE_SYNTAX_EXPD, obj);
}

static Scheme_Object *
lexical_syntax_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len;
  char *who;
  Scheme_Object *stx;

  if (rec)
    scheme_compile_rec_done_local(rec, drec);

  who = "quote-syntax";

  len = check_form(who, form);
  if (len != 2)
    bad_form(form, who, len);

  stx = SCHEME_STX_CDR(form);
  stx = SCHEME_STX_CAR(stx);
  
  if (rec)
    return scheme_make_syntax_compiled(QUOTE_SYNTAX_EXPD, icons(scheme_make_integer(env->genv->phase),
								stx));
  else {
    Scheme_Object *fn;
    fn = SCHEME_STX_CAR(form);
    return scheme_datum_to_syntax(icons(fn, icons(stx, scheme_null)),
				  form,
				  form, 
				  0, 1);
  }
}

static Scheme_Object *
lexical_syntax_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return lexical_syntax_syntax(form, env, NULL, 0);
}


static Scheme_Object *
defmacro_execute(Scheme_Object *form)
{
  Scheme_Object *name;
  Scheme_Object *val, *macro;
  Scheme_Thread *p = scheme_current_thread;

  name = SCHEME_CAR(SCHEME_CAR(form));
  val = SCHEME_CDR(form);

  scheme_on_next_top(scheme_get_env(p->config)->init, NULL, scheme_false);
  val = scheme_eval_linked_expr(val, 0);

  macro = scheme_alloc_stubborn_small_object();
  macro->type = scheme_macro_type;
  SCHEME_PTR_VAL(macro) = val;
  scheme_end_stubborn_change((void *)macro);
  
  scheme_set_global_bucket("define-syntax", (Scheme_Bucket *)name, macro, 1);
  scheme_shadow((Scheme_Env *)SCHEME_CDR(SCHEME_CAR(form)), 
		(Scheme_Object *)((Scheme_Bucket *)name)->key, 
		0);

  return scheme_void;
}

void scheme_defmacro_parse(Scheme_Object *form, 
			   Scheme_Object **name, Scheme_Object **code,
			   Scheme_Comp_Env *env)
{
  Scheme_Object *rest;

  if (scheme_stx_proper_list_length(form) != 3)
    scheme_wrong_syntax("define-syntax", NULL, form, NULL);

  rest = SCHEME_STX_CDR(form);
  *name = SCHEME_STX_CAR(rest);
    
  scheme_check_identifier("define-syntax", *name, NULL, env, form);
    
  rest = SCHEME_STX_CDR(rest);
  rest = SCHEME_STX_CAR(rest);
  *code = rest;
}

static Scheme_Object *defmacro_link(Scheme_Object *data, Link_Info *info)
{
  Scheme_Object *name = SCHEME_CAR(data);
  Scheme_Object *val = SCHEME_CDR(data);

  scheme_prepare_exp_env(info);
  val = scheme_link_expr(val, info->exp_env);

  name = (Scheme_Object *)scheme_global_keyword_bucket(name, info);
  
  return scheme_make_syntax_linked(DEFINE_SYNTAX_EXPD, 
				   cons(cons(name, (Scheme_Object *)info), val));
}

static Scheme_Object *defmacro_resolve(Scheme_Object *data, Resolve_Info *info)
{
  Scheme_Object *name = SCHEME_CAR(data);
  Scheme_Object *val = SCHEME_CDR(data);

  val = scheme_resolve_expr(val, info);

  return scheme_make_syntax_resolved(DEFINE_SYNTAX_EXPD, cons(name, val));
}

static Scheme_Object *
defmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *name, *code;
  Scheme_Object *val;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("define-syntax", NULL, form, "illegal use (not at top-level)");
  
  scheme_compile_rec_done_local(rec, drec);

  scheme_defmacro_parse(form, &name, &code, env);

  scheme_prepare_exp_env(env->genv);

  val = scheme_compile_expr(code, env->genv->exp_env->init, rec, drec);
  name = SCHEME_STX_SYM(name);

  return scheme_make_syntax_compiled(DEFINE_SYNTAX_EXPD, cons(name, val));
}

static Scheme_Object *
defmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  Scheme_Object *name, *code, *fpart, *fn;

  scheme_prepare_exp_env(env->genv);

  scheme_defmacro_parse(form, &name, &code, env);
  
  fpart = scheme_expand_expr(code, env->genv->exp_env->init, depth, name);
  
  code = icons(fpart, scheme_null);
  code = icons(name, code);

  fn = SCHEME_STX_CAR(form);
  return scheme_datum_to_syntax(icons(fn, code), 
				form, form, 
				0, 1);
}

static Scheme_Object *
do_letmacro(char *where, Scheme_Object *formname,
	    Scheme_Object *forms, Scheme_Comp_Env *origenv, 
	    Scheme_Compile_Info *rec, int drec, 
	    int depth, Scheme_Object *boundname)
{
  Scheme_Object *form, *bindings, *body, *v;
  Scheme_Object *macro;
  Scheme_Comp_Env *env;
  Scheme_Compile_Info mrec;
  int cnt, i;

  form = SCHEME_STX_CDR(forms);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(where, form, forms, NULL);
  bindings = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(where, form, forms, NULL);
  body = scheme_datum_to_syntax(form, forms, forms, 0, 0);

  env = scheme_new_compilation_frame(0, 0, origenv);

  check_form(where, bindings);
  
  cnt = scheme_stx_proper_list_length(bindings);
  i = 0;
  if (cnt > 0)
    scheme_add_local_syntax(cnt, env);

  for (v = bindings; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
    Scheme_Object *a;

    a = SCHEME_STX_CAR(v);
    if (!SCHEME_STX_PAIRP(a)
	|| !SCHEME_STX_SYMBOLP(SCHEME_STX_CAR(a))
	|| !SCHEME_STX_PAIRP(SCHEME_STX_CDR(a)))
      v = NULL;

    if (a) {
      Scheme_Object *rest;
      rest = SCHEME_STX_CDR(a);
      if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
	v = NULL;
    }

    if (!v)
      scheme_wrong_syntax(where, a, forms, "bad syntax (binding clause not an identifier and expression)");

    a = SCHEME_STX_CAR(a);
    scheme_check_identifier(where, a, NULL, env, forms);

    if (cnt > 0)
      scheme_set_local_syntax(i++, a, NULL, env);
  }

  scheme_prepare_exp_env(env->genv);

  i = 0;

  for (v = bindings; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
    Scheme_Object *a, *name;

    a = SCHEME_STX_CAR(v);
    name = SCHEME_STX_CAR(a);
    a = SCHEME_STX_CDR(a);
    a = SCHEME_STX_CAR(a);

    a = scheme_add_env_renames(a, env, origenv);
    
    mrec.dont_mark_local_use = 0;
    mrec.resolve_module_ids = 1;
    mrec.value_name = NULL;
    a = scheme_compile_expr(a, env->genv->exp_env->init, &mrec, 0);
    a = scheme_resolve_expr(a, scheme_resolve_info_create());
    a = scheme_link_expr(a, NULL);

    scheme_on_next_top(env, NULL, scheme_false);
    a = scheme_eval_linked_expr(a, mrec.max_let_depth);

    macro = scheme_alloc_stubborn_small_object();
    macro->type = scheme_macro_type;
    SCHEME_PTR_VAL(macro) = a;
    scheme_end_stubborn_change((void *)macro);

    scheme_set_local_syntax(i++, name, macro, env);
  }

  body = scheme_add_env_renames(body, env, origenv);

  env = scheme_require_renames(env);

  if (rec) {
    v = scheme_compile_block(body, env, rec, drec);
    v = scheme_make_sequence_compilation(v, 1);
  } else {
    v = scheme_expand_block(body, env, depth, boundname);
    if ((depth >= 0) || (depth == -2))
      v = icons(formname, icons(bindings, v));
    else if (SCHEME_STX_NULLP(SCHEME_STX_CDR(v)))
      v = SCHEME_STX_CAR(v);
    else
      v = icons(begin_symbol, v);

    v = scheme_datum_to_syntax(v, forms, scheme_sys_wraps(origenv), 
			       0, 1);
  }

  return v;
}

static Scheme_Object *
letmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		Scheme_Compile_Info *rec, int drec)
{
  return do_letmacro("letrec-syntax", NULL,
		     form, env, rec, drec, 0, scheme_false);
}

static Scheme_Object *
letmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_letmacro("letrec-syntax", let_macro_symbol, 
		     form, env, NULL, 0, depth, boundname);
}

/*****************************************************************/

static Scheme_Object *write_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)obj;

  return cons(scheme_make_integer(lv->count),
	      cons(scheme_make_integer(lv->position),
		   cons(lv->autobox ? scheme_true : scheme_false,
			cons(scheme_protect_quote(lv->value), 
			     scheme_protect_quote(lv->body)))));
}

static Scheme_Object *read_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)scheme_malloc_stubborn_tagged(sizeof(Scheme_Let_Value));
  lv->type = scheme_let_value_type;

  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  lv->position = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  lv->autobox = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  lv->value = SCHEME_CAR(obj);
  lv->body = SCHEME_CDR(obj);

  scheme_end_stubborn_change((void *)lv);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)obj;

  return cons(scheme_make_integer(lv->count), 
	      cons(lv->autobox ? scheme_true : scheme_false,
		   scheme_protect_quote(lv->body)));
}

static Scheme_Object *read_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)scheme_malloc_stubborn_tagged(sizeof(Scheme_Let_Void));
  lv->type = scheme_let_void_type;

  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  lv->autobox = SCHEME_TRUEP(SCHEME_CAR(obj));
  lv->body = SCHEME_CDR(obj);

  scheme_end_stubborn_change((void *)lv);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_one(Scheme_Object *obj)
{
  Scheme_Let_One *lo;
 
  lo = (Scheme_Let_One *)obj;

  return cons(lo->value, scheme_protect_quote(lo->body));
}

static Scheme_Object *read_let_one(Scheme_Object *obj)
{
  Scheme_Let_One *lo;
  int et;
 
  lo = (Scheme_Let_One *)scheme_malloc_stubborn_tagged(sizeof(Scheme_Let_One));
  lo->type = scheme_let_one_type;

  lo->value = SCHEME_CAR(obj);
  lo->body = SCHEME_CDR(obj);
  et = scheme_get_eval_type(lo->value);
  lo->eval_type = et;

  scheme_end_stubborn_change((void *)lo);

  return (Scheme_Object *)lo;
}

static Scheme_Object *write_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)obj;
  Scheme_Object *l = scheme_null;
  int i = lr->count;
  
  while (i--) {
    l = cons(scheme_protect_quote(lr->procs[i]), l);
  }

  return cons(scheme_make_integer(lr->count), 
	      cons(scheme_protect_quote(lr->body), l));
}

static Scheme_Object *read_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr;
  int i, c;
  Scheme_Object **sa;

  lr = MALLOC_ONE_TAGGED(Scheme_Letrec);

  lr->type = scheme_letrec_type;

  c = lr->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  lr->body = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  sa = MALLOC_N(Scheme_Object*, c);
  lr->procs = sa;
  for (i = 0; i < c; i++) {
    lr->procs[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  return (Scheme_Object *)lr;
}

static Scheme_Object *write_top(Scheme_Object *obj)
{
  Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)obj;

  return cons(scheme_make_integer(top->max_let_depth),
	      scheme_protect_quote(top->code));
}

static Scheme_Object *read_top(Scheme_Object *obj)
{
  Scheme_Compilation_Top *top;

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  top->type = scheme_compilation_top_type;
  top->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  top->code = SCHEME_CDR(obj);

  return (Scheme_Object *)top;
}

static Scheme_Object *write_case_lambda(Scheme_Object *obj)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)obj;
  int i;
  Scheme_Object **a, *l;

  i = cl->count;
  a = cl->array;

  l = scheme_null;
  for (; i--; ) {
    l = cons(a[i], l);
  }
  
  return cons((cl->name ? cl->name : scheme_null),
	      l);
}

static Scheme_Object *read_case_lambda(Scheme_Object *obj)
{
  Scheme_Object *s;
  int count, i;
  Scheme_Case_Lambda *cl;

  s = SCHEME_CDR(obj);
  for (count = 0; SCHEME_PAIRP(s); s = SCHEME_CDR(s)) {
    count++;
  }

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_stubborn_tagged(sizeof(Scheme_Case_Lambda)
				  + (count - 1) * sizeof(Scheme_Object *));

  cl->type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = SCHEME_CAR(obj);
  if (SCHEME_NULLP(cl->name))
    cl->name = NULL;

  s = SCHEME_CDR(obj);
  for (i = 0; i < count; i++, s = SCHEME_CDR(s)) {
    cl->array[i] = SCHEME_CAR(s);
  }
  
  return (Scheme_Object *)cl;
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_SYNTAX_C
#include "mzmark.c"

static void register_traversers(void)
{
}

END_XFORM_SKIP;

#endif
