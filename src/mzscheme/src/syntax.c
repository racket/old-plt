/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt

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
Scheme_Object *scheme_def_id_macro_syntax;
Scheme_Object *scheme_def_exp_time_syntax;
Scheme_Object *scheme_begin_syntax;
Scheme_Object *scheme_lambda_syntax;
Scheme_Object *scheme_compiled_void_code;
Scheme_Object scheme_undefined[1];

/* locals */
static Scheme_Object *lambda_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *define_values_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *quote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *quote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *if_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *if_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *set_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *set_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *case_lambda_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *case_lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *let_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *let_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_star_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_values_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_star_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letrec_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_expand (Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *begin0_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin0_expand (Scheme_Object *form, Scheme_Comp_Env *env, int depth);

static Scheme_Object *empty_cond_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *empty_cond_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);

static Scheme_Object *unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);

static Scheme_Object *with_cont_mark_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);

/* non-standard */
static Scheme_Object *defmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *defmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *def_id_macro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *def_id_macro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *def_exp_time_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *def_exp_time_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *letmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *letmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *let_id_macro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_id_macro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *let_exp_time_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_exp_time_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);

static Scheme_Object *define_values_execute(Scheme_Object *data);
static Scheme_Object *set_execute(Scheme_Object *data);
static Scheme_Object *defmacro_execute(Scheme_Object *expr);
static Scheme_Object *def_id_macro_execute(Scheme_Object *expr);
static Scheme_Object *def_exp_time_execute(Scheme_Object *expr);
static Scheme_Object *case_lambda_execute(Scheme_Object *expr);
static Scheme_Object *void_execute(Scheme_Object *expr);
static Scheme_Object *empty_cond_execute(Scheme_Object *data);
static Scheme_Object *begin0_execute(Scheme_Object *data);

static Scheme_Object *bangboxvalue_execute(Scheme_Object *data);


static Scheme_Object *named_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
					Scheme_Compile_Info *rec, int drec, int depth);

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
static Scheme_Object *letrec_symbol_nohp;
static Scheme_Object *let_star_symbol_nohp;
static Scheme_Object *letrec_values_symbol;
static Scheme_Object *let_star_values_symbol;
static Scheme_Object *let_values_symbol;
static Scheme_Object *letrec_values_symbol_nohp;
static Scheme_Object *let_star_values_symbol_nohp;
static Scheme_Object *let_values_symbol_nohp;
static Scheme_Object *begin_symbol;
static Scheme_Object *begin0_symbol;
static Scheme_Object *set_symbol;
static Scheme_Object *quote_symbol;
static Scheme_Object *if_symbol;
static Scheme_Object *case_lambda_symbol;
static Scheme_Object *with_continuation_mark_symbol;

static Scheme_Object *define_macro_symbol;
static Scheme_Object *define_id_macro_symbol;
static Scheme_Object *define_expansion_time_symbol;
static Scheme_Object *let_macro_symbol;
static Scheme_Object *let_id_macro_symbol;
static Scheme_Object *let_expansion_time_symbol;

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *sym;
  Scheme_Syntax_Registered *f;
  int protect_after;
} Linker_Name;

static int num_link_names = 0, link_names_size = 0;
static Linker_Name *linker_names;

static Scheme_Object *nonempty_cond;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons(a,b) scheme_make_pair(a,b)

#define max(a, b) (((a) > (b)) ? (a) : (b))

void 
scheme_init_syntax (Scheme_Env *env)
{
  if (scheme_starting_up) {
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

    REGISTER_SO(scheme_define_values_syntax);
    REGISTER_SO(scheme_defmacro_syntax);
    REGISTER_SO(scheme_def_id_macro_syntax);
    REGISTER_SO(scheme_def_exp_time_syntax);
    REGISTER_SO(scheme_lambda_syntax);
    REGISTER_SO(scheme_begin_syntax);
    REGISTER_SO(linker_names);
    REGISTER_SO(scheme_compiled_void_code);

    REGISTER_SO(define_values_symbol);
    REGISTER_SO(lambda_symbol);
    REGISTER_SO(letrec_symbol);
    REGISTER_SO(let_star_symbol);
    REGISTER_SO(let_symbol);
    REGISTER_SO(letrec_symbol_nohp);
    REGISTER_SO(let_star_symbol_nohp);
    REGISTER_SO(letrec_values_symbol);
    REGISTER_SO(let_star_values_symbol);
    REGISTER_SO(let_values_symbol);
    REGISTER_SO(letrec_values_symbol_nohp);
    REGISTER_SO(let_star_values_symbol_nohp);
    REGISTER_SO(let_values_symbol_nohp);
    REGISTER_SO(begin_symbol);
    REGISTER_SO(begin0_symbol);
    REGISTER_SO(set_symbol);
    REGISTER_SO(quote_symbol);
    REGISTER_SO(if_symbol);
    REGISTER_SO(case_lambda_symbol);
    REGISTER_SO(with_continuation_mark_symbol);
    
    REGISTER_SO(define_macro_symbol);
    REGISTER_SO(define_id_macro_symbol);
    REGISTER_SO(define_expansion_time_symbol);
    REGISTER_SO(let_macro_symbol);
    REGISTER_SO(let_id_macro_symbol);
    REGISTER_SO(let_expansion_time_symbol);

    scheme_undefined->type = scheme_undefined_type;

    define_values_symbol = scheme_intern_symbol("#%define-values");
    lambda_symbol = scheme_intern_symbol("#%lambda");

    letrec_symbol = scheme_intern_symbol("#%letrec");
    let_star_symbol = scheme_intern_symbol("#%let*");
    let_symbol = scheme_intern_symbol("#%let");
    letrec_symbol_nohp = scheme_intern_symbol("letrec");
    let_star_symbol_nohp = scheme_intern_symbol("let*");

    letrec_values_symbol = scheme_intern_symbol("#%letrec-values");
    let_star_values_symbol = scheme_intern_symbol("#%let*-values");
    let_values_symbol = scheme_intern_symbol("#%let-values");
    letrec_values_symbol_nohp = scheme_intern_symbol("letrec-values");
    let_values_symbol_nohp = scheme_intern_symbol("let-values");
    let_star_values_symbol_nohp = scheme_intern_symbol("let*-values");

    begin_symbol = scheme_intern_symbol("#%begin");
    begin0_symbol = scheme_intern_symbol("#%begin0");
    quote_symbol = scheme_intern_symbol("#%quote");
    if_symbol = scheme_intern_symbol("#%if");
    set_symbol = scheme_intern_symbol("#%set!");
    case_lambda_symbol = scheme_intern_symbol("#%case-lambda");
    with_continuation_mark_symbol = scheme_intern_symbol("#%with-continuation-mark");

    define_macro_symbol = scheme_intern_symbol("#%define-macro");
    define_id_macro_symbol = scheme_intern_symbol("#%define-id-macro");
    define_expansion_time_symbol = scheme_intern_symbol("#%define-expansion-time");
    let_macro_symbol = scheme_intern_symbol("#%let-macro");
    let_id_macro_symbol = scheme_intern_symbol("#%let-id-macro");
    let_expansion_time_symbol = scheme_intern_symbol("#%let-expansion-time");

    scheme_register_syntax("d", define_values_execute, 1);
    scheme_register_syntax("!", set_execute, 2);
    scheme_register_syntax("dm", defmacro_execute, 1);
    scheme_register_syntax("di", def_id_macro_execute, 1);
    scheme_register_syntax("dn", def_exp_time_execute, 1);
    scheme_register_syntax("cl", case_lambda_execute, 1);
    scheme_register_syntax("v", void_execute, 1);
    scheme_register_syntax("e", empty_cond_execute, 1);
    scheme_register_syntax("0", begin0_execute, 1);

    scheme_register_syntax("be", scheme_bangboxenv_execute, 1);
    scheme_register_syntax("b", bangboxvalue_execute, 1);

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
    scheme_def_id_macro_syntax = scheme_make_compiled_syntax(def_id_macro_syntax, 
							     def_id_macro_expand);
    scheme_def_exp_time_syntax = scheme_make_compiled_syntax(def_exp_time_syntax, 
							      def_exp_time_expand);
    scheme_lambda_syntax = scheme_make_compiled_syntax(lambda_syntax,
						       lambda_expand);
    scheme_begin_syntax = scheme_make_compiled_syntax(begin_syntax, 
						      begin_expand);

    scheme_compiled_void_code = scheme_make_syntax_link(void_execute, scheme_null);
  }

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
							let_expand), 
			    env);
  scheme_add_global_keyword("letrec", 
			    scheme_make_compiled_syntax(letrec_syntax, 
						        let_expand), 
			    env);  
  
  scheme_add_global_keyword("let-values", 
			    scheme_make_compiled_syntax(let_values_syntax, 
							let_expand), 
			    env);
  scheme_add_global_keyword("let*-values", 
			    scheme_make_compiled_syntax(let_star_values_syntax, 
							let_expand), 
			    env);
  scheme_add_global_keyword("letrec-values", 
			    scheme_make_compiled_syntax(letrec_values_syntax, 
						        let_expand), 
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

  scheme_add_global_keyword("define-macro", scheme_defmacro_syntax, env);
  scheme_add_global_keyword("define-id-macro", scheme_def_id_macro_syntax, env);
  scheme_add_global_keyword("define-expansion-time", scheme_def_exp_time_syntax, env);
  scheme_add_global_keyword("let-macro", 
			    scheme_make_compiled_syntax(letmacro_syntax, 
							 letmacro_expand), 
			    env);
  scheme_add_global_keyword("let-id-macro", 
			    scheme_make_compiled_syntax(let_id_macro_syntax, 
							let_id_macro_expand), 
			    env);
  scheme_add_global_keyword("let-expansion-time", 
			    scheme_make_compiled_syntax(let_exp_time_syntax, 
							let_exp_time_expand), 
			    env);
}

void
scheme_init_empty_cond(Scheme_Env *env)
{
  /* Hack: use the usual `cond' macro, but (cond) - an empty
     cond - is primitive syntax. Why? Because primitive exceptions
     can only be raised by the system (in the right mode), so you
     can't expand a (cond ...) into something that raises an error at
     a final `else'. */
  Scheme_Bucket *b;
  void *cs;

  b = scheme_global_bucket(scheme_intern_symbol("#%cond"), env);
  
  REGISTER_SO(nonempty_cond);

  nonempty_cond = (Scheme_Object *)b->val;
  cs = (void *)scheme_make_compiled_syntax(empty_cond_syntax, 
					   empty_cond_expand);
  b->val = cs;
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

  for (i = 0; SCHEME_PAIRP(form); i++) {
    form = SCHEME_CDR(form);
  }

  if (!SCHEME_NULLP(form))
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
  if (!SCHEME_PAIRP(form)
      || !SCHEME_PAIRP(SCHEME_CDR(form))
      || !SCHEME_PAIRP(SCHEME_CDDR(form)))
    scheme_wrong_syntax("lambda", NULL, form, NULL);
}

static void lambda_check_args(char *who, Scheme_Object *args, Scheme_Object *form)
{
  Scheme_Object *v;

  if (!SCHEME_SYMBOLP(args)) {
    for (v = args; SCHEME_PAIRP(v); v = SCHEME_CDR(v)) {
      if (!SCHEME_SYMBOLP(SCHEME_CAR(v)))
	scheme_wrong_syntax(who, SCHEME_CAR(v), form, "bad identifier");
    }

    if (!SCHEME_NULLP(v))
      if (!SCHEME_SYMBOLP(v))
	scheme_wrong_syntax(who, v, form, "bad identifier");

    /* Check for duplicate names: */
    for (v = args; SCHEME_PAIRP(v); v = SCHEME_CDR(v)) {
      Scheme_Object *name = SCHEME_CAR(v), *rest;
      for (rest = SCHEME_CDR(v); !SCHEME_NULLP(rest); rest = SCHEME_CDR(rest)) {
	Scheme_Object *param;
	if (!SCHEME_PAIRP(rest))
	  param = rest;
	else
	  param = SCHEME_CAR(rest);
	if (SCHEME_SYMBOLP(name) && SAME_OBJ(param, name))
	  scheme_wrong_syntax(who, name, form, "duplicate argument name");
	if (!SCHEME_PAIRP(rest))
	  break;
      }
    }
  }
}

static Scheme_Object *
lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *args;
  
  lambda_check(form);

  args = SCHEME_CAR(SCHEME_CDR(form));
  lambda_check_args("lambda", args, form);

  return scheme_make_closure_compilation(env, form, rec, drec);
}

static Scheme_Object *
lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *args;

  lambda_check(form);
  
  args = SCHEME_CAR(SCHEME_CDR(form));

  lambda_check_args("lambda", args, form);

  env = scheme_add_compilation_frame(args, env, 0);

  return cons(lambda_symbol,
	      cons(args,
		   scheme_expand_block(SCHEME_CDR(SCHEME_CDR(form)),
				       env,
				       depth)));
}

void scheme_set_global_bucket(char *who, Scheme_Bucket *b, Scheme_Object *val,
			      int set_undef)
{
  if ((((Scheme_Bucket_With_Const_Flag *)b)->flags & (GLOB_IS_CONST | GLOB_IS_KEYWORD))
      && b->val) {
    Scheme_Object *key = (Scheme_Object *)b->key;
    scheme_raise_exn(MZEXN_VARIABLE_KEYWORD, key,
		     "%s: cannot redefine %s: %S", 
		     who,
		     (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_CONST)
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

  vars = SCHEME_CAR(data);
  vals = SCHEME_CDR(data);

  vals = _scheme_eval_compiled_expr_multi(vals);
  if (SAME_OBJ(vals, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Object *v, **values;

    for (v = vars, i = 0; SCHEME_PAIRP(v); i++, v = SCHEME_CDR(v)) {}
    
    g = scheme_current_process->ku.multiple.count;
    if (i == g) {
      values = scheme_current_process->ku.multiple.array;
      for (i = 0; i < g; i++, vars = SCHEME_CDR(vars)) {
	scheme_set_global_bucket("define-values", (Scheme_Bucket *)SCHEME_CAR(vars), 
				 values[i], 1);
      }
	
      return scheme_void;
    }
  } else if (SCHEME_PAIRP(vars) && SCHEME_NULLP(SCHEME_CDR(vars))) {
    scheme_set_global_bucket("define-values", (Scheme_Bucket *)SCHEME_CAR(vars), 
			     vals, 1);

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
			      (g == 1) ? (Scheme_Object **)vals : scheme_current_process->ku.multiple.array,
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
  Scheme_Object *var = SCHEME_CAR(data);
  Scheme_Object *val = SCHEME_CDR(data);

  var = scheme_link_list(var, link);
  val = scheme_link_expr(val, link);

  return scheme_make_syntax_link(define_values_execute, cons(var, val));
}

static void define_values_parse(Scheme_Object *form, 
				Scheme_Object **var, Scheme_Object **val,
				Scheme_Comp_Env *env)
{
  Scheme_Object *vars;
  int len;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax("define-values", NULL, form, "illegal use (not at top-level)");

  len = check_form("define-values", form);
  if (len != 3)
    bad_form(form, "define-values", len);

  vars = SCHEME_CAR(SCHEME_CDR(form));
  *val = SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(form)));

  *var = vars;

  while (SCHEME_PAIRP(vars)) {
    Scheme_Object *name, *rest;

    name = SCHEME_CAR(vars);
    scheme_check_identifier("define-values", name, NULL, env, form);

    vars = SCHEME_CDR(vars);

    for (rest = vars; SCHEME_PAIRP(rest); rest = SCHEME_CDR(rest)) {
      Scheme_Object *param;
      param = SCHEME_CAR(rest);
      if (SAME_OBJ(param, name))
	scheme_wrong_syntax("define-values", name, form, "duplicate argument name");
    }
  }  

  if (!SCHEME_NULLP(vars))
    scheme_wrong_syntax("define-values", *var, form, "bad variable list");
}

static Scheme_Object *
define_values_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *var, *val, *first = scheme_null, *last = NULL, *variables;
  Scheme_Env *globals;

  globals = scheme_min_env(env);
  
  define_values_parse(form, &var, &val, env);
  variables = var;
  
  while (SCHEME_PAIRP(var)) {
    Scheme_Object *name, *pr, *bucket;

    name = SCHEME_CAR(var);

    bucket = (Scheme_Object *)scheme_global_bucket(name, globals);
    pr = scheme_make_pair(bucket, scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    var = SCHEME_CDR(var);
  }  

  scheme_compile_rec_done_local(rec, drec);
  if (SCHEME_PAIRP(first) && SCHEME_NULLP(SCHEME_CDR(first)))
    rec->value_name = SCHEME_CAR(variables);

  val = scheme_compile_expr(val, env, rec, drec);

  return scheme_make_syntax_compile(define_values_link, cons(first, val));
}

static Scheme_Object *
define_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *var, *val;

  define_values_parse(form, &var, &val, env);

  env = scheme_no_defines(env);

  return cons(define_values_symbol,
	      cons(var,
		   cons(scheme_expand_expr(val, env, depth), scheme_null)));
}

static Scheme_Object *
quote_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *v;

  if (!(SCHEME_PAIRP(SCHEME_CDR(form))
	&& SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(form)))))
    scheme_wrong_syntax("quote", NULL, form, "bad syntax (wrong number of parts)");

  scheme_compile_rec_done_local(rec, drec);
  scheme_default_compile_rec(rec, drec);
  
  v = SCHEME_CAR(SCHEME_CDR(form));

  return v;
}

static Scheme_Object *
quote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  if (!(SCHEME_PAIRP(SCHEME_CDR(form))
	&& SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(form)))))
    scheme_wrong_syntax("quote", NULL, form, "bad syntax (wrong number of parts)");

  return scheme_make_pair(quote_symbol, SCHEME_CDR(form));
}

static Scheme_Object *
if_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len, opt;
  Scheme_Object *test, *thenp, *elsep, *name;
  Scheme_Compile_Info recs[3];

  len = check_form("if", form);
  if (!(((len == 3) || (len == 4))))
    bad_form(form, "if", len);

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  test = SCHEME_CAR(SCHEME_CDR(form));
  thenp = SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(form)));
  if (len == 4)
    elsep = SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(form))));
  else
    elsep = scheme_compiled_void();

  scheme_init_compile_recs(rec, drec, recs, 3);
  recs[1].value_name = name;
  recs[2].value_name = name;

  env = scheme_no_defines(env);

  test = scheme_compile_expr(test, env, recs, 0);

  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    Scheme_Object *comp, *exp;

    opt = 1;
    
    if (SCHEME_FALSEP(test)) {
      comp = elsep;
      exp = thenp;
    } else {
      comp = thenp;
      exp = elsep;
    }

    /* compile other branch only to get syntax checking: */
    recs[2].dont_mark_local_use = 1;
    scheme_compile_expr(exp, env, recs, 2);

    test = scheme_compile_expr(comp, env, recs, 1);
  } else {
    opt = 0;
    thenp = scheme_compile_expr(thenp, env, recs, 1);
    elsep = scheme_compile_expr(elsep, env, recs, 2);
  }

  scheme_merge_compile_recs(rec, drec, recs, opt ? 2 : 3);

  if (opt)
    return test;
  else
    return scheme_make_branch(test, thenp, elsep);
}

static Scheme_Object *
if_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *test, *rest;
  int len;
  len = check_form("if", form);

  if (!(((len == 3) || (len == 4))))
    bad_form(form, "if", len);

  env = scheme_no_defines(env);

  test = SCHEME_CADR(form);
  test = scheme_expand_expr(test, env, depth);

  rest = SCHEME_CDR(SCHEME_CDR(form));
  rest = scheme_expand_list(rest, env, depth);

  return cons(if_symbol, cons(test, rest));
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

  form = SCHEME_CDR(form);
  key = SCHEME_CAR(form);
  form = SCHEME_CDR(form);
  val = SCHEME_CAR(form);
  expr = SCHEME_CADR(form);

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
with_cont_mark_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *key, *val, *expr;
  int len;
  len = check_form("with-continuation-mark", form);

  if (len != 4)
    bad_form(form, "with-continuation-mark", len);

  env = scheme_no_defines(env);

  form = SCHEME_CDR(form);
  key = SCHEME_CAR(form);
  form = SCHEME_CDR(form);
  val = SCHEME_CAR(form);
  expr = SCHEME_CADR(form);

  key = scheme_expand_expr(key, env, depth);
  val = scheme_expand_expr(val, env, depth);
  expr = scheme_expand_expr(expr, env, depth);

  return cons(with_continuation_mark_symbol,
	      cons(key,
		   cons(val,
			cons(expr, scheme_null))));
}

static Scheme_Object *
set_execute (Scheme_Object *data)
{
  Scheme_Object *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  
  val = _scheme_eval_compiled_expr(SCHEME_CDR(data));

  scheme_set_global_bucket("set!", (Scheme_Bucket *)SCHEME_CAR(data), 
			   val, SCHEME_TRUEP(set_undef));

  return scheme_void;
}

static Scheme_Object *
set_link(Scheme_Object *data, Link_Info *link)
{
  Scheme_Object *var, *val, *set_undef;

  set_undef = SCHEME_CAR(data);
  data = SCHEME_CDR(data);
  var = SCHEME_CAR(data);
  val = SCHEME_CDR(data);
  
  val = scheme_link_expr(val, link);

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    Scheme_Let_Value *lv;
    Scheme_Object *cv;
    int flags, li;

    cv = scheme_compiled_void();

    lv = MALLOC_ONE_TAGGED(Scheme_Let_Value);
    lv->type = scheme_let_value_type;
    lv->body = cv;
    lv->count = 1;
    li = scheme_link_info_lookup(link, SCHEME_LOCAL_POS(var), &flags);
    lv->position = li;
    lv->autobox = (flags & SCHEME_INFO_BOXED);
    lv->value = val;

    if (!(flags & SCHEME_INFO_BOXED))
      scheme_signal_error("internal error: set!: set!ed local variable is not boxed");

    return (Scheme_Object *)lv;
  }

  var = scheme_link_expr(var, link);
  
  return scheme_make_syntax_link(set_execute, cons(set_undef, cons(var, val)));
}

static Scheme_Object *
set_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *var, *val, *name, *body;
  int l, set_undef;

  l = check_form("set!", form);
  if (l != 3)
    bad_form(form, "set!", l);

  name = SCHEME_CAR(SCHEME_CDR(form));
  body = SCHEME_CADR(SCHEME_CDR(form));
  
  scheme_check_identifier("set!", name, NULL, env, form);

  scheme_compile_rec_done_local(rec, drec);
  rec[drec].value_name = name;

  var = scheme_static_distance(name, env, 
			       SCHEME_SETTING 
			       + SCHEME_GLOB_ALWAYS_REFERENCE
			       + SCHEME_ELIM_CONST 
			       + (rec[drec].dont_mark_local_use 
				  ? SCHEME_DONT_MARK_USE 
				  : 0));

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
  
  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
  } else {
    if (ENV_PRIM_GLOBALS_ONLY(env)) {
      scheme_wrong_syntax("set!", NULL, form,
			  "cannot mutate a global from within a unit");
    }
  } 

  set_undef = SCHEME_TRUEP(scheme_get_param(scheme_config,
					    MZCONFIG_ALLOW_SET_UNDEFINED));
  
  return scheme_make_syntax_compile(set_link, 
				    cons(set_undef
					 ? scheme_true
					 : scheme_false,
					 cons(var, val)));
}

static Scheme_Object *
set_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *name, *var;
  int l;
  l = check_form("set!", form);
  if (l != 3)
    bad_form(form, "set!", l);

  env = scheme_no_defines(env);

  name = SCHEME_CADR(form);

  scheme_check_identifier("set!", name, NULL, env, form);

  /* Make sure it's mutable: */
  var = scheme_static_distance(name, env, SCHEME_SETTING);
  if (ENV_PRIM_GLOBALS_ONLY(env) && SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type))
    scheme_wrong_syntax("set!", NULL, form,
			"cannot mutate a global from within a unit");

  return cons(set_symbol,
	      cons(name,
		   cons(scheme_expand_expr(SCHEME_CADR(SCHEME_CDR(form)), env, depth),
			scheme_null)));
}

static Scheme_Object *
case_lambda_execute(Scheme_Object *expr)
{
  Scheme_Case_Lambda *seqin, *seqout;
  int i;
  Scheme_Process *p = scheme_current_process;

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
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;

  for (i = 0; i < seq->count; i++) {
    Scheme_Object *le;
    le = scheme_link_expr(seq->array[i], link);
    seq->array[i] = le;
  }

  return scheme_make_syntax_link(case_lambda_execute, expr);
}

static void case_lambda_check_line(Scheme_Object *line, Scheme_Object *form)
{
  Scheme_Object *body, *args;

  if (!SCHEME_PAIRP(line))
    scheme_wrong_syntax("case-lambda", line, form, NULL);
  
  body = SCHEME_CDR(line);
  args = SCHEME_CAR(line);
  
  lambda_check_args("case-lambda", args, form);
  
  if (!SCHEME_PAIRP(body))
    scheme_wrong_syntax("case-lambda", line, form, "bad syntax (%s)",
			SCHEME_NULLP(body) ? "empty body" : IMPROPER_LIST_FORM);
}

static Scheme_Object *
case_lambda_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *list, *last, *c, *orig_form = form, *name;
  Scheme_Case_Lambda *cl;
  int i, count = 0;
  Scheme_Compile_Info *recs;
  
  form = SCHEME_CDR(form);

  if (SCHEME_NULLP(form)) {
    form = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						 - sizeof(Scheme_Object*));

    form->type = scheme_case_lambda_sequence_type;
    ((Scheme_Case_Lambda *)form)->count = 0;
    ((Scheme_Case_Lambda *)form)->name = rec[drec].value_name;

    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);

    return scheme_make_syntax_compile(case_lambda_link, form);
  }

  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax("case-lambda", form, orig_form, NULL);
  if (SCHEME_NULLP(SCHEME_CDR(form))) {
    case_lambda_check_line(SCHEME_CAR(form), orig_form);
    
    return lambda_syntax(scheme_make_pair(lambda_symbol,
					  SCHEME_CAR(form)), env, rec, drec);
  }

  name = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  list = last = NULL;
  while (SCHEME_PAIRP(form)) {
    case_lambda_check_line(SCHEME_CAR(form), orig_form);

    c = scheme_make_pair(scheme_make_pair(lambda_symbol, SCHEME_CAR(form)),
			 scheme_null);
    if (list)
      SCHEME_CDR(last) = c;
    else
      list = c;

    last = c;
    form = SCHEME_CDR(form);

    count++;
  }

  if (!SCHEME_NULLP(form))
    scheme_wrong_syntax("case-lambda", form, orig_form, NULL);

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_stubborn_tagged(sizeof(Scheme_Case_Lambda)
				  + (count - 1) * sizeof(Scheme_Object *));
  cl->type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = name;

  scheme_compile_rec_done_local(rec, drec);
  recs = MALLOC_N_RT(Scheme_Compile_Info, count);
  scheme_init_compile_recs(rec, drec, recs, count);

  for (i = 0; i < count; i++, list = SCHEME_CDR(list)) {
    Scheme_Object *ce;
    ce = scheme_compile_expr(SCHEME_CAR(list), env, recs, i);
    cl->array[i] = ce;
  }

  scheme_end_stubborn_change((void *)cl);

  scheme_merge_compile_recs(rec, drec, recs, count);

  return scheme_make_syntax_compile(case_lambda_link, (Scheme_Object *)cl);
}

static Scheme_Object *
case_lambda_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *first, *last, *args, *body, *c, *orig_form = form;

  first = cons(case_lambda_symbol, scheme_null);
  last = first;
  form = SCHEME_CDR(form);

  while (SCHEME_PAIRP(form)) {
    args = SCHEME_CAR(form);

    case_lambda_check_line(args, orig_form);
    
    body = SCHEME_CDR(args);
    args = SCHEME_CAR(args);
    
    c = cons(cons(args, 
		  scheme_expand_block(body, 
				      scheme_add_compilation_frame(args, 
								   env, 
								   0), 
				      depth)),
	     scheme_null);

    SCHEME_CDR(last) = c;
    last = c;

    form = SCHEME_CDR(form);
  }

  if (!SCHEME_NULLP(form))
    scheme_wrong_syntax("case-lambda", form, orig_form, NULL);
  
  return first;
}

Scheme_Object *scheme_bangboxenv_execute(Scheme_Object *data)
{
  int pos = SCHEME_INT_VAL(SCHEME_CAR(data));
  Scheme_Object *bb;
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Process *p = scheme_current_process;
#endif

  data = SCHEME_CDR(data);
  
  bb = scheme_make_envunbox(MZ_RUNSTACK[pos]);
  MZ_RUNSTACK[pos] = bb;

  return _scheme_tail_eval(data);
}

static Scheme_Object *
bangboxvalue_execute(Scheme_Object *data)
{
  int pos = SCHEME_INT_VAL(SCHEME_CAR(data));
  Scheme_Object *val;

  data = SCHEME_CDR(data);
  
  val = _scheme_eval_compiled_expr_multi(data);

  if (SAME_OBJ(val, SCHEME_MULTIPLE_VALUES)) {
    Scheme_Process *p = scheme_current_process;
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

Scheme_Object *
scheme_link_lets(Scheme_Object *form, Link_Info *info)
{
  Link_Info *linfo, *val_linfo;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body;
  Scheme_Let_Value *lv, *last = NULL;
  Scheme_Object *first = NULL, *body;
  Scheme_Letrec *letrec;
  int i, pos, opos, rpos, recbox, num_rec_procs = 0, extra_alloc;

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
	linfo = scheme_link_info_extend(info, i + 1, head->count, i + 1);
	for (j = 0; j <= i; j++) {
	  scheme_link_info_add_mapping(linfo, j, i - j, 0);
	}

	{
	  Scheme_Object *le;
	  le = scheme_link_expr(clv->value, linfo);
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

      linfo = scheme_link_info_extend(info, head->count, head->count, head->count);
      for (i = head->count; i--; ) {
	scheme_link_info_add_mapping(linfo, i, head->count - 1 - i, 0);
      }
      
      body = scheme_link_expr(body, linfo);
      if (last)
	((Scheme_Let_One *)last)->body = body;
      else
	first = body;
      
      return first;
    }
  }

  linfo = scheme_link_info_extend(info, head->count, head->count, head->count);

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
      
      scheme_link_info_add_mapping(linfo, opos, p,
				   ((recbox 
				     || (clv->flags[j] & SCHEME_WAS_SET_BANGED))
				    ? SCHEME_INFO_BOXED
				    : 0));

      opos++;
    }
  }

  /* Link body: */
  body = scheme_link_expr(body, linfo);

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
	val_linfo = scheme_link_info_extend(linfo, extra_alloc, 0, 0);
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

  /* Link values: */
  clv = (Scheme_Compiled_Let_Value *)head->body;
  rpos = 0; opos = 0;
  for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
    int isproc;
    Scheme_Object *expr;
    isproc = scheme_is_compiled_procedure(clv->value, 0);
    expr = scheme_link_expr(clv->value, val_linfo);
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
	li = scheme_link_info_lookup(linfo, clv->position, NULL);
	lv->position = li + extra_alloc;
      } else
	lv->position = 0;
      lv->count = clv->count;
      lv->autobox = recbox;

      for (j = lv->count; j--; ) {
	if (!recbox
	    && (scheme_link_info_flags(linfo, opos + j) & SCHEME_INFO_BOXED)) {
	  Scheme_Object *sl;
	  sl = scheme_make_syntax_link(bangboxvalue_execute, 
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
gen_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, char *formname,
		int star, int recursive, int multi, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *bindings, *l, *binding, *name, **names, *forms, *defname;
  int num_clauses, num_bindings, i, j, k, m, pre_k;
  Scheme_Comp_Env *frame;
  Scheme_Compile_Info *recs;
  Scheme_Object *first = NULL;
  Scheme_Compiled_Let_Value *last = NULL, *lv;
  DupCheckRecord r;

  if (scheme_proper_list_length(form) < 3)
    scheme_wrong_syntax(formname, NULL, form, NULL);

  bindings = SCHEME_CAR(SCHEME_CDR(form));
  num_clauses = scheme_proper_list_length(bindings);

  if (num_clauses < 0)
    scheme_wrong_syntax(formname, bindings, form, NULL);

  forms = SCHEME_CDR(SCHEME_CDR(form));

  if (!num_clauses) {
    env = scheme_no_defines(env);
    return scheme_compile_sequence(forms, env, rec, drec);
  }
  
  if (multi) {
    num_bindings = 0;
    l = bindings;
    while (!SCHEME_NULLP(l)) {
      Scheme_Object *clause, *names;
      int num_names;

      clause = SCHEME_CAR(l);
      
      if (!SCHEME_PAIRP(clause)
	  || !SCHEME_PAIRP(SCHEME_CDR(clause))
	  || !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(clause))))
	scheme_wrong_syntax(formname, clause, form, NULL);
      
      names = SCHEME_CAR(clause);
      
      num_names = scheme_proper_list_length(names);
      if (num_names < 0)
	scheme_wrong_syntax(formname, names, form, NULL);
     
      num_bindings += num_names;
 
      l = SCHEME_CDR(l);
    }
  } else
    num_bindings = num_clauses;


  names = MALLOC_N_STUBBORN(Scheme_Object *, num_bindings);

  frame = scheme_new_compilation_frame(num_bindings, SCHEME_LET_FRAME, env);
  env = frame;

  recs = MALLOC_N_RT(Scheme_Compile_Info, (num_clauses + 1));

  defname = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);
  scheme_init_compile_recs(rec, drec, recs, num_clauses + 1);

  
  if (!star) {
    r.scheck_size = 0;
    scheme_begin_dup_symbol_check(&r);
  }

  for (i = 0, k = 0; i < num_clauses; i++) {
    if (!SCHEME_PAIRP(bindings))
      scheme_wrong_syntax(formname, bindings, form, NULL);
    binding = SCHEME_CAR(bindings);
    if (!(SCHEME_PAIRP(binding) && SCHEME_PAIRP(SCHEME_CDR(binding))
	  && SCHEME_NULLP(SCHEME_CDDR(binding))))
      scheme_wrong_syntax(formname, binding, form, NULL);
    
    pre_k = k;

    name = SCHEME_CAR(binding);
    if (multi) {
      while (!SCHEME_NULLP(name)) {
	names[k] = SCHEME_CAR(name);
	scheme_check_identifier(formname, names[k], NULL, env, form);
	k++;
	name = SCHEME_CDR(name);
      }

      for (j = pre_k; j < k; j++) {
	for (m = j + 1; m < k; m++) {
	  if (SAME_OBJ(names[m], names[j]))
	    scheme_wrong_syntax(formname, NULL, form,
				"multiple bindings of `%S' in the same clause", 
				names[m]);
	}
      }
    } else {
      scheme_check_identifier(formname, name, NULL, env, form);
      names[k++] = name;
    }
    
    if (!star) {
      for (m = pre_k; m < k; m++) {
	scheme_dup_symbol_check(&r, formname, names[m], "binding", form, 0);
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
      recs[i].value_name = names[pre_k];

    if (!recursive) {
      Scheme_Object *ce;
      ce = scheme_compile_expr(SCHEME_CADR(binding), env, recs, i);
      lv->value = ce;
    } else
      lv->value = SCHEME_CADR(binding);
    
    if (star || recursive) {
      for (m = pre_k; m < k; m++) {
	scheme_add_compilation_binding(m, names[m], frame);
      }
    }
    
    bindings = SCHEME_CDR (bindings);
  }
  
  if (!star && !recursive) {
    for (i = 0; i < num_bindings; i++) {
      scheme_add_compilation_binding(i, names[i], frame);
    }
  }

  if (recursive) {
    lv = (Scheme_Compiled_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_Compiled_Let_Value *)lv->body) {
      Scheme_Object *ce;
      ce = scheme_compile_expr(lv->value, env, recs, i);
      lv->value = ce;
    }
  }

  recs[num_clauses].value_name = defname;
  {
    Scheme_Object *cs;
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
let_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  int named, letrec = 0, letstar = 0, multi = 0;
  const char *formname;
  Scheme_Object *fmname;
  Scheme_Object *vars, *body, *first, *last, *name, *v, *vs, *vlist;
  Scheme_Comp_Env *use_env;
  DupCheckRecord r;

  fmname = SCHEME_CAR(form);

  if (SAME_OBJ(fmname, letrec_symbol)
      || SAME_OBJ(fmname, letrec_symbol_nohp)) {
    letrec = 1;
    formname = "letrec";
  } else if (SAME_OBJ(fmname, letrec_values_symbol)
      || SAME_OBJ(fmname, letrec_values_symbol_nohp)) {
    letrec = 1;
    multi = 1;
    formname = "letrec-values";
  } else if (SAME_OBJ(fmname, let_star_symbol)
	     || SAME_OBJ(fmname, let_star_symbol_nohp)) {
    letstar = 1;
    formname = "let*";
  } else if (SAME_OBJ(fmname, let_star_values_symbol)
	     || SAME_OBJ(fmname, let_star_values_symbol_nohp)) {
    letstar = 1;
    multi = 1;
    formname = "let*-values";
  } else if (SAME_OBJ(fmname, let_values_symbol)
	     || SAME_OBJ(fmname, let_values_symbol_nohp)) {
    multi = 1;
    formname = "let-values";
  } else {
    formname = "let";
  }
  
  vars = SCHEME_CDR(form);

  named = !multi && !letrec && !letstar && SCHEME_PAIRP(vars) && SCHEME_SYMBOLP(SCHEME_CAR(vars));
  
  if (named)
    return named_let_syntax(form, env, NULL, 0, depth);

  if (!SCHEME_PAIRP(vars))
    scheme_wrong_syntax(formname, vars, form, NULL);

  body = SCHEME_CDR(vars);
  vars = SCHEME_CAR(vars);

  if (!SCHEME_PAIRP(body))
    scheme_wrong_syntax(formname, body, form, NULL);

  if (letstar) {
    if (!SCHEME_NULLP(vars)) {
      if (!SCHEME_PAIRP(vars))
	scheme_wrong_syntax(formname, vars, form, NULL);

      body = cons(multi ? let_values_symbol : let_symbol,
		  cons(cons(SCHEME_CAR(vars), scheme_null),
		       cons(cons(multi ? let_star_values_symbol : let_star_symbol,
				 cons(SCHEME_CDR(vars), body)),
			    scheme_null)));
    } else
      body = cons(let_values_symbol, cons(scheme_null, body));

    --depth;
    if (!depth)
      return body;
    else {
      env = scheme_no_defines(env);
      return scheme_expand_expr(body, env, depth);
    }
  }

  if (!letstar) {
    r.scheck_size = 0;
    scheme_begin_dup_symbol_check(&r);
  }

  vlist = scheme_null;
  vs = vars;
  while (SCHEME_PAIRP(vs)) {
    Scheme_Object *v2;
    v = SCHEME_CAR(vs);
    if (SCHEME_PAIRP(v))
      v2 = SCHEME_CDR(v);
    else
      v2 = scheme_false;
    if (!SCHEME_PAIRP(v2) || !SCHEME_NULLP(SCHEME_CDR(v2)))
      scheme_wrong_syntax(formname, v, form, NULL);

    name = SCHEME_CAR(v);

    if (multi) {
      DupCheckRecord r2;
      Scheme_Object *names = name;
      r2.scheck_size = 0;
      scheme_begin_dup_symbol_check(&r2);
      while (SCHEME_PAIRP(names)) {
	name = SCHEME_CAR(names);

	scheme_check_identifier(formname, name, NULL, env, form);
	vlist = cons(name, vlist);

	scheme_dup_symbol_check(&r2, formname, name, "clause binding", form, 0);
	if (!letstar)
	  scheme_dup_symbol_check(&r, formname, name, "binding", form, 0);
	
	names = SCHEME_CDR(names);
      }
      if (!SCHEME_NULLP(names))
	scheme_wrong_syntax(formname, names, form, NULL);
    } else {
      scheme_check_identifier(formname, name, NULL, env, form);
      vlist = cons(name, vlist);
      if (!letstar)
	scheme_dup_symbol_check(&r, formname, name, "binding", form, 0);
    }

    vs = SCHEME_CDR(vs);
  }

  if (!SCHEME_NULLP(vs))
    scheme_wrong_syntax(formname, vs, form, NULL);

  use_env = env;
  env = scheme_add_compilation_frame(vlist, env, 0);

  if (letrec)
    use_env = env;

  first = last = NULL;
  while (SCHEME_PAIRP(vars)) {
    v = SCHEME_CAR(vars);

    name = SCHEME_CAR(v);
    if (!multi)
      name = cons(name, scheme_null);

    v = scheme_expand_expr(SCHEME_CDR(v), use_env, depth);
    v = cons(cons(name, v), scheme_null);

    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;

    last = v;

    vars = SCHEME_CDR(vars);
  }

  if (!SCHEME_NULLP(vars))
    scheme_wrong_syntax(formname, vars, form, NULL);
  
  if (!first)
    first = scheme_null;

  body = scheme_expand_block(body, env, depth);

  if (named)
    return cons(let_symbol,
		cons(SCHEME_CADR(form),
		     cons(first, body)));
  else
    return cons((letrec ? letrec_values_symbol : let_values_symbol),
		cons(first, body));
}

static Scheme_Object *
let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  if (!SCHEME_PAIRP(SCHEME_CDR(form)))
    scheme_wrong_syntax("let", NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");

  if (SCHEME_SYMBOLP (SCHEME_CAR (SCHEME_CDR (form))))
    return named_let_syntax (form, env, rec, drec, 0);
  
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
  
  if (!SCHEME_PAIRP(l))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (non-symbol-value-pair)");
  s = SCHEME_CAR(l);
  if (!SCHEME_SYMBOLP(s))
    scheme_wrong_syntax("named let", s, form, 
			"bad syntax (name not an identifier)");

  return s;
}

static Scheme_Object *nl_cadr(Scheme_Object *l, Scheme_Object *form)
{
  if (!SCHEME_PAIRP(l) || !SCHEME_PAIRP(SCHEME_CDR(l)))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (non-identifier-value-pair)");
  
  if (!SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(l))))
    scheme_wrong_syntax("named let", l, form, 
			"bad syntax (extra data in indentifier-value pair)");
  
  return SCHEME_CADR(l);
}

static Scheme_Object *
named_let_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		  Scheme_Compile_Info *rec, int drec, int depth)
{
  Scheme_Object *name, *bindings, *vars, *vals, *forms;
  Scheme_Object *proc, *app, *letrec;

  if (!(SCHEME_PAIRP(SCHEME_CDR (form))
	&& SCHEME_PAIRP(SCHEME_CDR(SCHEME_CDR (form)))
	&& SCHEME_PAIRP(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR (form))))))
    scheme_wrong_syntax("named let", NULL, form, NULL);
  name = SCHEME_CAR (SCHEME_CDR (form));
  bindings = SCHEME_CAR (SCHEME_CDR (SCHEME_CDR (form)));
  if (!SCHEME_LISTP(bindings))
    scheme_wrong_syntax("named let", bindings, form, NULL);

  vars = scheme_named_map_1("named let", nl_car, bindings, form);
  vals = scheme_named_map_1("named let", nl_cadr, bindings, form);
  
  forms = SCHEME_CDR (SCHEME_CDR (SCHEME_CDR (form)));

  proc = cons(lambda_symbol, cons(vars, forms));
  
  letrec = cons(letrec_symbol,
		cons(cons(cons(name, cons(proc, scheme_null)), scheme_null),
		     cons(name,
			  scheme_null)));
  app = cons(letrec, vals);

  if (rec)
    return scheme_compile_expr(app, env, rec, drec);
  else {
    --depth;
    if (!depth)
      return app;
    else
      return scheme_expand_expr(app, env, depth);
  }
}

Scheme_Object *scheme_compile_sequence(Scheme_Object *forms,
				       Scheme_Comp_Env *env, 
				       Scheme_Compile_Info *rec, int drec)
{
  if (SCHEME_PAIRP(forms) && SCHEME_NULLP(SCHEME_CDR(forms))) {
    /* If it's a begin, we have to check some more... */
    Scheme_Object *first = SCHEME_CAR(forms), *val;

    if (SCHEME_PAIRP(first)) {
      first = scheme_check_immediate_macro(first, env, rec, drec, -1, &val);
      
      if (SAME_OBJ(val, scheme_begin_syntax)) {
	if (scheme_proper_list_length(SCHEME_CDR(first)) > 0)
	  return scheme_compile_sequence(SCHEME_CDR(first), env, rec, drec);
      }
    }

    return scheme_compile_expr(first, env, rec, drec);
  } else {
    if (scheme_proper_list_length(forms) < 0) {
      scheme_wrong_syntax("begin", NULL, cons(begin_symbol, forms), 
			  "bad syntax (" IMPROPER_LIST_FORM ")");
      return NULL;
    } else {
      Scheme_Object *body;
      body = scheme_compile_block(forms, env, rec, drec);
      return scheme_make_sequence_compilation(body, 1, 1);
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
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *v, **mv;
  int i, mc, apos;
  
  i = ((Scheme_Sequence *)obj)->count;

  v = _scheme_eval_compiled_expr_multi_wp(((Scheme_Sequence *)obj)->array[0], p);
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
    (void)_scheme_eval_compiled_expr_multi_wp(((Scheme_Sequence *)obj)->array[apos++], p);
  }

  if (mv) {
    p->ku.multiple.array = mv;
    p->ku.multiple.count = mc;
  }

  return v;
}

static Scheme_Object *
begin0_link (Scheme_Object *obj, Link_Info *info)
{
  int i;
  
  i = ((Scheme_Sequence *)obj)->count;

  while (i--) {
    Scheme_Object *le;
    le = scheme_link_expr(((Scheme_Sequence *)obj)->array[i], info);
    ((Scheme_Sequence *)obj)->array[i] = le;
  }

  return scheme_make_syntax_link(begin0_execute, obj);
}

static Scheme_Object *
do_begin_syntax (char *name,
		 Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, 
		 int zero)
{
  Scheme_Object *forms, *body;

  forms = SCHEME_CDR (form);
  
  if (SCHEME_NULLP(forms)) {
    if (!zero && scheme_is_toplevel(env))
      return scheme_compiled_void();
    scheme_wrong_syntax(name, NULL, form, "bad syntax (empty form)");
    return NULL;
  }

  check_form(name, forms);

  if (zero)
    env = scheme_no_defines(env);

  if (SCHEME_NULLP(SCHEME_CDR(forms)))
    return scheme_compile_expr(SCHEME_CAR(forms), env, rec, drec);

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

      first = scheme_compile_expr(SCHEME_CAR(forms), env, recs, 0);
      rest = scheme_compile_list(SCHEME_CDR(forms), env, recs, 1);
      
      scheme_merge_compile_recs(rec, drec, recs, 2);

      body = cons(first, rest);
    } else 
      body = scheme_compile_list(forms, env, rec, drec);
  } else
    /* Top level */
    body = scheme_compile_list(forms, env, rec, drec);

  forms = scheme_make_sequence_compilation(body, 1, zero ? -1 : 1);

  if (!zero || (NOT_SAME_TYPE(SCHEME_TYPE(forms), scheme_begin0_sequence_type)))
    return forms;

  return scheme_make_syntax_compile(begin0_link, forms);
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
		Scheme_Object *form, Scheme_Comp_Env *env, int depth,
		int zero)
{
  Scheme_Object *rest;

  check_form(name, form);

  rest = SCHEME_CDR(form);

  if (SCHEME_NULLP(rest)) {
    if (!zero && scheme_is_toplevel(env))
      return cons(form_name, scheme_null);
    scheme_wrong_syntax(name, NULL, form, "bad syntax (empty form)");
    return NULL;
  }

  if (zero)
    env = scheme_no_defines(env);

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level: */
    if (zero) {
      form = cons(scheme_expand_expr(SCHEME_CAR(rest), env, depth),
		  scheme_expand_list(SCHEME_CDR(rest), env, depth));
    } else {
      form = scheme_expand_list(rest, env, depth);
#if 0
      if (SCHEME_NULLP(SCHEME_CDR(form)))
	return SCHEME_CAR(form);
#endif
    }
  } else {
    /* Top level */
    form =  scheme_expand_list(rest, env, depth);
  }

  return cons(form_name, form);
}

static Scheme_Object *
begin_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_begin_expand("begin", begin_symbol, form, env, depth, 0);
}

static Scheme_Object *
begin0_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_begin_expand("begin0", begin0_symbol, form, env, depth, 1);
}

static Scheme_Object *
empty_cond_execute (Scheme_Object *expr)
{
  scheme_raise_exn(MZEXN_ELSE, "cond or case: no matching clause");
  return scheme_void;
}

static Scheme_Object *
empty_cond_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  if (SCHEME_FALSEP(scheme_get_param(scheme_config, MZCONFIG_COND_AUTO_ELSE)))
    if (SCHEME_NULLP(SCHEME_CDR(form))) {
      return scheme_make_syntax_link(empty_cond_execute, scheme_false);
    }

  return scheme_compile_expand_macro_app(nonempty_cond,
					 form, env, rec, drec, 1);
}

static Scheme_Object *
empty_cond_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  if (SCHEME_FALSEP(scheme_get_param(scheme_config, MZCONFIG_COND_AUTO_ELSE)))
    if (SCHEME_NULLP(SCHEME_CDR(form)))
      return form;

  return scheme_compile_expand_macro_app(nonempty_cond,
					 form, env, NULL, 0, depth);
}


static Scheme_Object *
unquote_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len;
  char *who;

  if (rec)
    scheme_compile_rec_done_local(rec, drec);

  who = scheme_symbol_val(SCHEME_CAR(form));

  len = check_form(who, form);
  if (len != 2)
    bad_form(form, who, len);

  scheme_wrong_syntax(who, NULL, form, "not in quasiquote");
  return NULL;
}

static Scheme_Object *
unquote_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return unquote_syntax(form, env, NULL, 0);
}


static Scheme_Object *
do_def_execute(char *who, Scheme_Object *form, Scheme_Type type, int require_proc)
{
  Scheme_Object *name;
  Scheme_Object *val, *macro;
  Scheme_Comp_Env *save_env;
  Scheme_Process *p = scheme_current_process;

  name = SCHEME_CAR(form);
  val = SCHEME_CDR(form);

  save_env = p->current_local_env;
  p->current_local_env = scheme_get_env(p->config)->init;
  val = scheme_eval_compiled_expr(val);
  p->current_local_env = save_env;

  if (require_proc)
    if (!SCHEME_PROCP(val))
      scheme_raise_exn(MZEXN_MISC,
		       "define-macro: not a procedure");

  if (SCHEME_TRUEP(name)) {
    macro = scheme_alloc_stubborn_small_object ();
    macro->type = type;
    SCHEME_PTR_VAL(macro) = val;
    scheme_end_stubborn_change((void *)macro);
      
    scheme_set_global_bucket(who, (Scheme_Bucket *)name, macro, 1);

    return scheme_void;
  } else
    return val;
}

static void do_def_parse(char *where,
			 Scheme_Object *form, 
			 Scheme_Object **name, Scheme_Object **code,
			 Scheme_Comp_Env *env)
{
  if (scheme_proper_list_length(form) != 3)
    scheme_wrong_syntax(where, NULL, form, NULL);

  *name = SCHEME_CAR (SCHEME_CDR (form));
    
  scheme_check_identifier(where, *name, NULL, env, form);
    
  *code = SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(form)));
}

static Scheme_Object *do_def_link(Scheme_Syntax_Executer *exec,
				  Scheme_Object *data,
				  Link_Info *info)
{
  Scheme_Object *name = SCHEME_CAR(data);
  Scheme_Object *val = SCHEME_CDR(data);

  name = scheme_link_expr(name, info);
  val = scheme_link_expr(val, info);

  return scheme_make_syntax_link(exec, scheme_make_pair(name, val));
}

static Scheme_Object *
do_def_syntax(char *where,
	      Scheme_Object *form, Scheme_Comp_Env *env, 
	      Scheme_Compile_Info *rec, int drec,
	      Scheme_Syntax_Linker *link)
{
  Scheme_Object *name, *code;
  Scheme_Object *val;

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(where, NULL, form, "illegal use (not at top-level)");
  
  scheme_compile_rec_done_local(rec, drec);

  do_def_parse(where, form, &name, &code, env);

  val = scheme_compile_expr(code, env, rec, drec);
  name = (Scheme_Object *)scheme_global_bucket(name, scheme_min_env(env));

  return scheme_make_syntax_compile(link, scheme_make_pair(name, val));
}

static Scheme_Object *
do_def_expand(char *where, Scheme_Object *formname, Scheme_Object *form, 
	      Scheme_Comp_Env *env, int depth)
{
  Scheme_Object *name, *code, *fpart;

  do_def_parse(where, form, &name, &code, env);
  
  fpart = scheme_expand_expr(code, env, depth);
  
  code = cons(fpart, scheme_null);
  code = cons(name, code);

  return cons(formname, code);
}


static Scheme_Object *
defmacro_execute (Scheme_Object *form)
{
  return do_def_execute("define-macro", form, scheme_macro_type, 1);
}

static Scheme_Object *
defmacro_link(Scheme_Object *form, Link_Info *info)
{
  return do_def_link(defmacro_execute, form, info);
}

static Scheme_Object *
defmacro_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		 Scheme_Compile_Info *rec, int drec)
{
  return do_def_syntax("define-macro", form, env, rec, drec,
		       defmacro_link);
}

static Scheme_Object *
defmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_def_expand("define-macro", define_macro_symbol, form, env, depth);
}

static Scheme_Object *
def_id_macro_execute (Scheme_Object *form)
{
  return do_def_execute("define-id-macro", form, scheme_id_macro_type, 0);
}

static Scheme_Object *
def_id_macro_link(Scheme_Object *form, Link_Info *info)
{
  return do_def_link(def_id_macro_execute, form, info);
}

static Scheme_Object *
def_id_macro_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		     Scheme_Compile_Info *rec, int drec)
{
  return do_def_syntax("define-id-macro", form, env, rec, drec,
		       def_id_macro_link);
}

static Scheme_Object *
def_id_macro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_def_expand("define-id-macro", 
		       define_id_macro_symbol, 
		       form, env, depth);
}

static Scheme_Object *
def_exp_time_execute (Scheme_Object *form)
{
  return do_def_execute("define-expansion-time", form, scheme_exp_time_type, 0);
}

static Scheme_Object *
def_exp_time_link(Scheme_Object *form, Link_Info *info)
{
  return do_def_link(def_exp_time_execute, form, info);
}

static Scheme_Object *
def_exp_time_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		      Scheme_Compile_Info *rec, int drec)
{
  return do_def_syntax("define-expansion-time", form, env, rec, drec,
		       def_exp_time_link);
}

static Scheme_Object *
def_exp_time_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_def_expand("define-expansion-time", 
		       define_expansion_time_symbol, 
		       form, env, depth);
}

static Scheme_Object *
do_letmacro(char *where, Scheme_Object *formname,
	    Scheme_Object *forms, Scheme_Comp_Env *env, 
	    Scheme_Compile_Info *rec, int drec, int depth,
	    Scheme_Type type, int anything)
{
  Scheme_Object *form, *name, *body, *cl, *clf, *v;
  Scheme_Object *macro;
  Scheme_Comp_Env *save_env;
  Scheme_Process *p = scheme_current_process;

  env = scheme_no_defines(env);

  form = SCHEME_CDR(forms);
  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax(where, form, forms, NULL);
  name = SCHEME_CAR(form);
  form = SCHEME_CDR(form);
  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax(where, form, forms, NULL);
  cl = SCHEME_CAR(form);
  form = SCHEME_CDR(form);
  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax(where, form, forms, NULL);
  body = form;
  
  scheme_check_identifier(where, name, NULL, env, form);

  save_env = p->current_local_env;
  p->current_local_env = env;
  clf = scheme_eval(cl, scheme_min_env(env));
  p->current_local_env = save_env;

  if (!anything && !SCHEME_PROCP(clf))
    scheme_raise_exn(MZEXN_MISC,
		     "let-macro: not a procedure");

  macro = scheme_alloc_stubborn_small_object ();
  macro->type = type;
  SCHEME_PTR_VAL(macro) = clf;
  scheme_end_stubborn_change((void *)macro);

  scheme_push_constant(name, macro, env);

  if (rec) {
    v = scheme_compile_block(body, env, rec, drec);
    v = scheme_make_sequence_compilation(v, 1, 1);
  } else {
    v = scheme_expand_block(body, env, depth);
    if (depth >= 0)
      v = cons(formname,
	       cons(name,
		    cons(cl, v)));
    else if (SCHEME_NULLP(SCHEME_CDR(v)))
      v = SCHEME_CAR(v);
    else
      v = cons(begin_symbol, v);
  }

  scheme_pop_constant(env);

  return v;
}

static Scheme_Object *
letmacro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		Scheme_Compile_Info *rec, int drec)
{
  return do_letmacro("let-macro", NULL,
		     form, env, rec, drec, 0, scheme_macro_type, 0);
}

static Scheme_Object *
letmacro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_letmacro("let-macro", let_macro_symbol, 
		     form, env, NULL, 0, depth, scheme_macro_type, 0);
}

static Scheme_Object *
let_id_macro_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		Scheme_Compile_Info *rec, int drec)
{
  return do_letmacro("let-id-macro", NULL,
		     form, env, rec, drec, 0, scheme_id_macro_type, 1);
}

static Scheme_Object *
let_id_macro_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_letmacro("let-id-macro", let_id_macro_symbol, 
		     form, env, NULL, 0, depth, scheme_id_macro_type, 1);
}

static Scheme_Object *
let_exp_time_syntax(Scheme_Object *form, Scheme_Comp_Env *env, 
		     Scheme_Compile_Info *rec, int drec)
{
  return do_letmacro("let-expansion-time", NULL,
		     form, env, rec, drec, 0, scheme_exp_time_type, 1);
}

static Scheme_Object *
let_exp_time_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_letmacro("let-expansion-time", let_expansion_time_symbol, 
		     form, env, NULL, 0, depth, scheme_exp_time_type, 1);
}

static Scheme_Object *void_execute(Scheme_Object *expr)
{
  return scheme_void;
}

/*********************************************************************/

void scheme_register_syntax(const char *name, 
			    Scheme_Syntax_Registered *f,
			    int protect_after)
{
  Scheme_Object *s;

  if (num_link_names == link_names_size) {
    Linker_Name *old = linker_names;

    link_names_size += 20;
    linker_names = MALLOC_N_RT(Linker_Name, link_names_size);

    memcpy(linker_names, old, num_link_names * sizeof(Linker_Name));

#ifdef MEMORY_COUNTING_ON
    scheme_misc_count += (20 * sizeof(Linker_Name));
#endif
  }
  
#ifdef MZTAG_REQUIRED
  linker_names[num_link_names].type = scheme_rt_linker_name;
#endif
  s = scheme_intern_symbol(name);
  linker_names[num_link_names].sym = s;
  linker_names[num_link_names].f = f;
  linker_names[num_link_names].protect_after = protect_after;
  num_link_names++;
}

Scheme_Object *scheme_find_linker_name(Scheme_Syntax_Registered *f, int *protect_after)
{
  int i;

  for (i = 0; i < num_link_names; i++) {
    if (SAME_PTR(linker_names[i].f, f)) {
      *protect_after = linker_names[i].protect_after;
      return linker_names[i].sym;
    }
  }

  return NULL;
}

Scheme_Syntax_Registered *scheme_find_linker(Scheme_Object *sym)
{
  int i;

  for (i = 0; i < num_link_names; i++) {
    if (SAME_OBJ(linker_names[i].sym, sym))
      return linker_names[i].f;
  }

  return NULL;
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
  GC_REG_TRAV(scheme_rt_linker_name, mark_linker_name);
}

END_XFORM_SKIP;

#endif
