/*
  mzc --cc ++ccf -I/proj/scheme/plt/src/mzscheme/src x.c
  mzc --ld x.so x.o
 */

#include "escheme.h"
#include "schpriv.h"  // you need this from inside plt/src/mzscheme/src

#define mkint     scheme_make_integer
#define mkbool(x) ((x) ? scheme_true : scheme_false)
#define cons      scheme_make_pair
#define symb       scheme_intern_symbol

#define sym(str) scheme_intern_exact_symbol(str, strlen(str))

Scheme_Object* parse_bytecode_wrapper(int argc, Scheme_Object* argv[]);
Scheme_Object* parse_bytecode(Scheme_Object* expr);

Scheme_Object* scheme_module_name() { return sym("parse-compiled"); }

Scheme_Env* ext_env = NULL;
Scheme_Object* parse_bytecode_prim = NULL;
Scheme_Object* undefined = NULL;

Scheme_Object* scheme_initialize(Scheme_Env* enclosing_ns)
{
  Scheme_Env* env = scheme_primitive_module(scheme_module_name(), enclosing_ns); 
  const char* fname = "parse-compiled";
  Scheme_Object* undefined_struct_type =
                   scheme_make_struct_type(symb("undefined"),
                                           NULL, // super
                                           NULL, // inspector
                                           0,    // number of init fields
                                           0,    // number of auto fields
                                           scheme_null,  // value for auto fields
                                           scheme_null, // properties
					   NULL ); // guard
  undefined = scheme_make_struct_instance(undefined_struct_type, // struct type
                                          0, NULL);  // constructor argc, argv
  scheme_add_global( fname,
		     parse_bytecode_prim = scheme_make_prim_w_arity(parse_bytecode_wrapper, fname, 1, 1),
		     env );
  
  scheme_finish_primitive_module(env);
  ext_env = env;
  return scheme_reload(env);
}

Scheme_Object* scheme_reload(Scheme_Env* env)
{
//  const char* fname = "parse-bytecode";
//  ext_env = env;
  /*scheme_add_global
    ( fname,
      parse_bytecode_prim = scheme_make_prim_w_arity(parse_bytecode_wrapper, fname, 1, 1),
      env );*/
  return scheme_void;
}



Scheme_Object* mklist(int count, Scheme_Object* items[])
{
  return count == 0 ?
         scheme_null :
         cons(items[0], mklist(count - 1, items + 1));
}

Scheme_Object* mkintlist(int count, int items[])
{
  return count == 0 ?
         scheme_null :
         cons(mkint(items[0]), mkintlist(count - 1, items + 1));
}

Scheme_Object* mkbclist(int count, Scheme_Object* items[])
{
  return count == 0 ?
         scheme_null :
         cons(parse_bytecode(items[0]), mkbclist(count - 1, items + 1));
}

Scheme_Object* map1( Scheme_Object* fn, Scheme_Object* list )
{
  Scheme_Object* argv[] = {fn, list};
  return scheme_apply(scheme_lookup_global(symb("map"), ext_env), 2, argv);
}

Scheme_Object* parse_bytecode_wrapper(int argc, Scheme_Object* argv[])
{
  return parse_bytecode(argv[0]);
}

Scheme_Object* parse_bytecode(Scheme_Object* expr)
{
  switch (SCHEME_TYPE(expr)) {

  /* Scheme variable binding:
     (variable id value bucket-flags bucket-id home-namespace) */
  case scheme_variable_type: {
    Scheme_Bucket_With_Home* bh = (Scheme_Bucket_With_Home*) expr;
    Scheme_Bucket* b = SCHEME_VAR_BUCKET(expr);
    Scheme_Object* skey = (Scheme_Object*) b->key;
    return cons( symb("variable"),
           // the type of key is char* but in reality it's a Scheme_Object* symbol
           cons( b->key ?
                  (SCHEME_SYMBOLP(skey) || SCHEME_CHAR_STRINGP(skey) || SCHEME_BYTE_STRINGP(skey) || SCHEME_NUMBERP(skey) ?
                    skey :
                    symb(b->key)) :
                  scheme_false,
           cons( b->val ? ((Scheme_Object*) b->val) : undefined,
           cons( mkint(bh->bucket.flags),
           cons( mkint(bh->bucket.id),
           cons( (Scheme_Object*) bh->home,
                 scheme_null ))))));
  }
  /* Module variable binding location:
    (module-variable id module-path-index position) */
  case scheme_module_variable_type:
    {
    Module_Variable* mv = (Module_Variable*) expr;
    return cons( symb("module-variable"),
           cons( mv->sym,
           cons( mv->modidx,
           cons( mkint(mv->pos),
                 scheme_null))));
    }
  /* This is not necessary anymore -- MzScheme exposes indexes already
  case scheme_module_index_type:
    {
    Scheme_Modidx* idx = (Scheme_Modidx*) expr;
    return cons( symb("module-index"),
           cons( idx->path,
           cons( idx->base,
           cons( idx->resolved,
           cons( idx->shift_cache ? idx->shift_cache : scheme_false,
           cons( idx->cache_next ? parse_bytecode(idx->cache_next) : scheme_false,
                 scheme_null))))));
    }*/
  /* Reference to a toplevel variable:
     (toplevel toplevel-depth toplevel-position) */
  case scheme_toplevel_type:
    return cons( symb("toplevel"),
           cons( mkint(SCHEME_TOPLEVEL_DEPTH(expr)),
           cons( mkint(SCHEME_TOPLEVEL_POS(expr)),
                 scheme_null )));
  /* Static depth/index of a local variable reference:
     (local static-depth/index) */
  case scheme_local_type:
    return cons( symb("local"),
           cons( mkint(SCHEME_LOCAL_POS(expr)),
                 scheme_null ));
  /* ???? */
  case scheme_local_unbox_type:
    return cons( symb("local-unbox"),
           cons( mkint(SCHEME_LOCAL_POS(expr)),
                 scheme_null ));
  /* syntax
     =>
     (syntax index data) */
  case scheme_syntax_type:
    return cons( symb("syntax"),
           cons( mkint(SCHEME_PINT_VAL(expr)),
           cons( parse_bytecode(SCHEME_IPTR_VAL(expr)),
                 scheme_null )));
  /* Function application.  Operator is at argv[0], the rest contains the operands */
  case scheme_application_type: {
    Scheme_App_Rec* app = (Scheme_App_Rec *)expr;
    return cons( symb("application"),
           cons( parse_bytecode(app->args[0]), // the function
           cons( mkbclist(app->num_args, app->args + 1),
                 scheme_null )));
  }
  /* Special case: function application with 1 operator + 1 operands */
  case scheme_application2_type: {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
    return cons( symb("application2"),
           cons( parse_bytecode(app->rator),
           cons( parse_bytecode(app->rand),
                   scheme_null )));
  }
  /* Special case: function application with 1 operator + 2 operands */
  case scheme_application3_type: {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
    return cons( symb("application3"),
           cons( parse_bytecode(app->rator),
           cons( parse_bytecode(app->rand1),
           cons( parse_bytecode(app->rand2),
                 scheme_null ))));
  }
  /*  (begin ...) */
  case scheme_sequence_type: {
    Scheme_Sequence *seq = (Scheme_Sequence *)expr;
    return cons( symb("sequence"),
           cons( mkbclist(seq->count, seq->array),
                 scheme_null ));
  }
  /* (if test tbranch fbranch) */
  case scheme_branch_type: {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;
    return cons( symb("branch"),
           cons( parse_bytecode(b->test),
           cons( parse_bytecode(b->tbranch),
           cons( parse_bytecode(b->fbranch),
                 scheme_null ))));
  }
  /* (with-continuation-mark key val body) */
  case scheme_with_cont_mark_type: {
    Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;
    return cons( symb("with-continuation-mark"),
           cons( parse_bytecode(wcm->key),
           cons( parse_bytecode(wcm->val),
           cons( parse_bytecode(wcm->body),
                 scheme_null ))));
  }
  /* (lambda args body)
      =>
     (closure-compilation-data max-static-depth/index
                               (length args)
                               closure-map
                               body) */
  
  /*
   * TODO: FIXME: Scheme_Closure_Compilation_Data doesn't exist in v299
   case scheme_unclosed_procedure_type: {
    Scheme_Closure_Compilation_Data *data =
      (Scheme_Closure_Compilation_Data *)expr;
    return cons( symb("closure-compilation-data"),
           cons( mkint(data->max_let_depth),
           cons( mkint(data->num_params),
           cons( mkintlist(data->closure_size, data->closure_map),
           cons( parse_bytecode(data->code),
                 scheme_null )))));
  } */
				   
  /* (let-values ([(bindings ...) values] ...) body)
     expands into
     (let-value ((bindings ...) values)
       (let-value ((more-bindings ...) more-values) ...
         body)...)
     =>
     (let-value position (length bindings) values body) */
  case scheme_let_value_type: {
    Scheme_Let_Value *lv = (Scheme_Let_Value *)expr;
    Scheme_Object *rhs;
    return cons( symb("let-value"),
           //cons( mkbool(lv->autobox),
           cons( mkint(lv->position),
           cons( mkint(lv->count),
           cons( parse_bytecode(lv->value),
           cons( parse_bytecode(lv->body),
                 scheme_null )))));
  }
  /* (let-values ([(bindings ...) values] ...) body)
     =>
     (let-void (apply + (length bindings) ...)
               (let-value ...)) */
  case scheme_let_void_type: {
    Scheme_Let_Void *lv = (Scheme_Let_Void *)expr;
    return cons( symb("let-void"),
           //cons( mkbool(lv->autobox),
           cons( mkint(lv->count),
           cons( parse_bytecode(lv->body),
                 scheme_null )));
  }
  /* (letrec ([id proc] ...) body)
     =>
     (letrec (length id...) (proc ...) body) */
  case scheme_letrec_type: {
    Scheme_Letrec *l = (Scheme_Letrec *)expr;
    return cons( symb("letrec"),
           cons( mkint(l->count),
           cons( mkbclist(l->count, l->procs),
           cons( parse_bytecode(l->body),
                 scheme_null ))));
  }
  /* (let ([id value]) body)
     =>
     (let-one value body) */
  case scheme_let_one_type: {
    Scheme_Let_One *lo = (Scheme_Let_One *)expr;
    return cons( symb("let-one"),
           cons( parse_bytecode(lo->value),
           cons( parse_bytecode(lo->body),
                 scheme_null )));
  }
  /* I don't think we ever reach this.
     See case scheme_unclosed_procedure_type. */
  case scheme_closure_type:
    return cons( symb("closure"),
           cons( parse_bytecode(SCHEME_COMPILED_CLOS_CODE(expr)),
                 scheme_null ));
  /* (compile sxp)
     =>
     (compilation-top max-let-depth        ; maximum static depth/index
                      resolve-prefix       ; toplevels and syntaxes
                      sxp)                 ; compiled sxp */
  case scheme_compilation_top_type: {
    Scheme_Compilation_Top* top = (Scheme_Compilation_Top*) expr;
    return cons( symb("compilation-top"),
           cons( mkint(top->max_let_depth),
           cons( parse_bytecode(top->prefix),
           cons( parse_bytecode(top->code),
                 scheme_null ))));
  }
  /*  (resolve-prefix ((variable ...) ...) ; top levels
                      ((stx ...) ...))     ; syntaxes  */
  case scheme_resolve_prefix_type:
    {
    Resolve_Prefix* rp = (Resolve_Prefix*) expr;
    return cons( symb("resolve-prefix"),
           cons( mkbclist(rp->num_toplevels, rp->toplevels),
           cons( mklist(rp->num_stxes, rp->stxes),
                 scheme_null)));
    }
  case scheme_module_type:
    {
    /* TODO: finish this case */
    Scheme_Module* m = (Scheme_Module*) expr;
    //printf("module-type: num_provides: %d\n", m->num_provides);
    return cons( symb("module-code"),
	   cons( cons( symb("requires"),
	         cons( m->requires,
		       scheme_null )),
           cons( cons( symb("requires-for-syntax"),
	         cons( m->et_requires,
	               scheme_null )),
           cons( cons( symb("provides"),
		 cons( mklist(m->num_provides, m->provides),
		       scheme_null )),
	         scheme_null ))));
    }
  /* atoms, lists, pairs, and unhandled cases */
  default:
    return
       (SCHEME_CHAR_STRINGP(expr) ||
	SCHEME_BYTE_STRINGP(expr) ||
        SCHEME_SYMBOLP(expr) ||
        SCHEME_NUMBERP(expr) ||
        SCHEME_VECTORP(expr) ||
        SCHEME_PROCP(expr))  ||
        expr == scheme_true  ||
        expr == scheme_false ||
        expr == scheme_void  ||
        expr == scheme_eof   ||
        expr == scheme_undefined ?
           cons( symb("literal"),
           cons( expr,
                 scheme_null ))
     : scheme_proper_list_length(expr) != -1 ? cons( symb("list"),
                                               cons( map1(parse_bytecode_prim, expr),
                                                     scheme_null))
     : SCHEME_PAIRP(expr) ? cons( symb("pair"),
                            cons( parse_bytecode(SCHEME_CAR(expr)),
                            cons( parse_bytecode(SCHEME_CDR(expr)),
                                  scheme_null)))
     : /*else*/ cons( symb("unknown type"),
                cons( expr,
                      scheme_null ));
  }
}
