
#include <stdlib.h>
#include <stdarg.h>


#define GLOBAL_VARREF(x) ((x)->val ? (Scheme_Object *)(x)->val : \
  (scheme_unbound_global((Scheme_Object*)(x)->key), (Scheme_Object *)NULL))

#define DO_FUEL_POLL ((scheme_fuel_counter-- <= 0) ? (scheme_process_block(0), 0) : 0)

#define _scheme_direct_apply_primitive_multi_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_primitive_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_primitive(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_multi_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_closed_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_poll(prim, argc, argv) \
    (DO_FUEL_POLL, _scheme_direct_apply_closed_primitive(prim, argc, argv))

#define _scheme_make_c_proc_closure(cfunc, rec, name, amin, amax) \
  ((Scheme_Object *)_scheme_fill_prim_closure(&rec->prim, cfunc, &rec->data, name, amin, amax))

#define _scheme_make_c_proc_closure_empty(cfunc, rec, name, amin, amax) \
  ((Scheme_Object *)_scheme_fill_prim_closure(&rec->prim, cfunc, NULL, name, amin, amax))

#define _scheme_make_c_case_proc_closure(cfunc, rec, name, ccnt, cses) \
  ((Scheme_Object *)_scheme_fill_prim_case_closure(&rec->prim, cfunc, &rec->data, name, ccnt, cses))

#define _scheme_make_c_case_proc_closure_empty(cfunc, rec, name, ccnt, cses) \
  ((Scheme_Object *)_scheme_fill_prim_case_closure(&rec->prim, cfunc, NULL, name, ccnt, cses))

#define _scheme_make_c_unit_closure_basic(cfunc, rec, name, imc, exc, expts, datav) \
  (rec->unit.type = scheme_unit_type, \
   rec->unit.num_imports = imc, \
   rec->unit.num_exports = exc, \
   rec->unit.exports = expts, \
   rec->unit.init_func = cfunc, \
   rec->unit.data = (Scheme_Object *)datav, \
   (Scheme_Object *)rec)

#define _scheme_make_c_unit_closure(cfunc, rec, name, imc, exc, expts) \
  _scheme_make_c_unit_closure_basic(cfunc, rec, name, imc, exc, expts, &rec->data)

#define _scheme_make_c_unit_closure_empty(cfunc, rec, name, imc, exc, expts) \
  _scheme_make_c_unit_closure_basic(cfunc, rec, name, imc, exc, expts, NULL)

#define _scheme_make_c_class_closure(assembly, rec, super, interfaces) \
   scheme_create_class(assembly, rec, super, interfaces)

#define _scheme_invoke_unit(u, ni, ins, anchs) \
      scheme_invoke_unit(u, ni, ins, anchs, 0, NULL, 0, 0)
#define _scheme_tail_invoke_unit(u, ni, ins, anchs) \
      scheme_invoke_unit(u, ni, ins, anchs, 0, NULL, 1, 0)
#define _scheme_invoke_unit_multi(u, ni, ins, anchs) \
      scheme_invoke_unit(u, ni, ins, anchs, 0, NULL, 0, 1)
#define _scheme_invoke_open_unit(u, ni, ins, anchs, pref) \
      scheme_invoke_unit(u, ni, ins, anchs, 1, pref, 0, 0)
#define _scheme_tail_invoke_open_unit(u, ni, ins, anchs, pref) \
      scheme_invoke_unit(u, ni, ins, anchs, 1, pref, 1, 0)
#define _scheme_invoke_open_unit_multi(u, ni, ins, anchs, pref) \
      scheme_invoke_unit(u, ni, ins, anchs, 1, pref, 0, 1)

#define NO_MULTIPLE_VALUES(res) \
	if (res == SCHEME_MULTIPLE_VALUES) \
	    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
#define CHECK_MULTIPLE_VALUES(res, expected) \
	if (res != SCHEME_MULTIPLE_VALUES || scheme_multiple_count != expected) \
        scheme_wrong_return_arity(NULL, expected, \
                                  (res == SCHEME_MULTIPLE_VALUES ? scheme_multiple_count : 1), \
								  (res == SCHEME_MULTIPLE_VALUES ? scheme_multiple_array : (Scheme_Object**)res), \
                                  NULL);

#define SCHEME_CURRENT_ENV(pr) ((Scheme_Env *)scheme_get_param(pr->config, MZCONFIG_ENV))

typedef struct {
  Scheme_Object * val;
  Scheme_Object ** array;
  int count;
} _Scheme_Begin0_Rec;

/* this function is called with the super object and a (null-terminated) list  */
/* of the field names and returns multiple values -- it is "struct"            */
/* (struct (a b) (c d)) --> c_struct_imp(stuff(b), 2, "a", "c", "d", NULL);    */
static Scheme_Object * c_struct_imp(int multiok, Scheme_Object * super, int n_fields, 
				    char * name, char * field_list, ...)
{
	va_list marker;
	char * field = field_list;
	Scheme_Object ** field_names;
	Scheme_Object * type_name, * field_names_list, * v;
	Scheme_Object ** struct_names, ** struct_values;
	int count;

	if (super && !SAME_TYPE(SCHEME_TYPE(super), scheme_struct_type_type))
	  scheme_raise_exn(MZEXN_STRUCT_STRUCT_TYPE, super,
			   "struct: supertype expression returned "
			   "a value that is not a struct type value");

	field_names = scheme_malloc(n_fields*sizeof(Scheme_Object*));

	va_start( marker, field_list );
	for(count=0; count < n_fields; count++)
	{
		field_names[count] = scheme_intern_symbol(field);
		field = va_arg( marker, char* );
	}
	va_end(marker);
	field_names_list = scheme_build_list(n_fields, field_names);
	type_name = scheme_make_struct_type(scheme_intern_symbol(name), super, n_fields);
	struct_names = scheme_make_struct_names(scheme_intern_symbol(name), field_names_list, 0, &count);
	struct_values = scheme_make_struct_values(type_name, struct_names, count, 0);

	/* return all this garbage as multiple values */
	v = scheme_values(3+2*n_fields, struct_values);

	if (!multiok && (v == SCHEME_MULTIPLE_VALUES))
	  scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);

	return v;
}

#define _scheme_apply_ckp(f, argc, argv) (SCHEME_CLSD_PRIMP(f) ? _scheme_apply_closed_prim(f, argc, argv) : _scheme_apply(f, argc, argv))
#define _scheme_apply_multi_ckp(f, argc, argv) (SCHEME_CLSD_PRIMP(f) ? _scheme_apply_closed_prim_multi(f, argc, argv) : _scheme_apply_multi(f, argc, argv))

#define MZC_EQP(ltp, av, bv) (SAME_OBJ(av, bv))
#define MZC_EQVP(ltp, av, bv) scheme_eqv(av, bv)
#define MZC_EQUALP(ltp, av, bv) scheme_equal(av, bv)
#define MZC_NOTP(p, av) (SCHEME_FALSEP(av))
#define MZC_NULLP(p, av) (SCHEME_NULLP(av))
#define MZC_PAIRP(p, av) (SCHEME_PAIRP(av))
#define MZC_SYMBOLP(p, av) (SCHEME_SYMBOLP(av))
#define MZC_STRINGP(p, av) (SCHEME_STRINGP(av))
#define MZC_VECTORP(p, av) (SCHEME_VECTORP(av))
#define MZC_NUMBERP(p, av) (SCHEME_NUMBERP(av))
#define MZC_PROCEDUREP(p, av) (SCHEME_PROCP(av))
#define MZC_EOFP(p, av) (SCHEME_EOFP(av))
#define MZC_CHARP(p, av) (SCHEME_CHARP(av))

#define MZC_CONS(p, av, bv) scheme_make_pair(av, bv)
#define MZC_LIST1(p, av) scheme_make_pair(av, scheme_null)
#define MZC_LIST2(p, av, bv) scheme_make_pair(av, scheme_make_pair(bv, scheme_null))
#define MZC_APPEND(p, av, bv) scheme_append(av, bv)

#define MZC_CAR(p, av) (SCHEME_PAIRP(av) ? SCHEME_CAR(av) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CDR(p, av) (SCHEME_PAIRP(av) ? SCHEME_CDR(av) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CADR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CDR(av))) ? SCHEME_CAR(SCHEME_CDR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CDDR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CDR(av))) ? SCHEME_CDR(SCHEME_CDR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CDAR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CAR(av))) ? SCHEME_CDR(SCHEME_CAR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_CAAR(p, av) ((SCHEME_PAIRP(av) && SCHEME_PAIRP(SCHEME_CAR(av))) ? SCHEME_CAR(SCHEME_CAR(av)) : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_SET_CAR(p, av, bv) (SCHEME_PAIRP(av) ? (SCHEME_CAR(av)=bv, scheme_void) : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(p, 2, arg)))
#define MZC_SET_CDR(p, av, bv) (SCHEME_PAIRP(av) ? (SCHEME_CDR(av)=bv, scheme_void) : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(p, 2, arg)))

#define MZC_VECTOR_REF(p, v, i) ((SCHEME_INTP(i) && SCHEME_VECTORP(v) && (SCHEME_INT_VAL(i) >= 0) \
                                  && (SCHEME_INT_VAL(i) < SCHEME_VEC_SIZE(v)) \
                                 ? SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] \
				  : (arg[0] = v, arg[1] = i, _scheme_direct_apply_primitive_multi(p, 2, arg))))
#define MZC_VECTOR_SET(p, v, i, x) ((SCHEME_INTP(i) && SCHEME_VECTORP(v) && (SCHEME_INT_VAL(i) >= 0) \
                                    && (SCHEME_INT_VAL(i) < SCHEME_VEC_SIZE(v)) \
                                    ? (SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] = x, scheme_void) \
				    : (arg[0] = v, arg[1] = i, arg[2] = x, _scheme_direct_apply_primitive_multi(p, 3, arg))))

#define MZC_CHAR_TO_INTEGER(p, v) (SCHEME_CHARP(v) ? scheme_make_integer((unsigned char)SCHEME_CHAR_VAL(v)) \
                                   : (arg[0] = v, _scheme_direct_apply_primitive_multi(p, 1, arg)))

#define _MZC_DBLP(obj) SAME_TYPE(_SCHEME_TYPE(obj), scheme_double_type)

#define MZC_ZEROP(zp, av) (SCHEME_INTP(av) \
                                ? (av == scheme_make_integer(0)) \
                                : (_MZC_DBLP(av) \
                                   ? !SCHEME_DBL_VAL(av) \
                                   : (arg[0] = av, SCHEME_TRUEP(_scheme_direct_apply_primitive_multi(zp, 1, arg)))))

#define MZC_ARITH_COMPARE(cp, av, bv, compareop) \
                                     ((SCHEME_INTP(av) && SCHEME_INTP(bv)) \
                                      ? (SCHEME_INT_VAL(av) compareop SCHEME_INT_VAL(bv)) \
                                      : ((SCHEME_DBLP(av) && SCHEME_DBLP(bv)) \
                                         ? (SCHEME_DBL_VAL(av) compareop SCHEME_DBL_VAL(bv)) \
                                         : (arg[0] = av, arg[1] = bv, SCHEME_TRUEP(_scheme_direct_apply_primitive_multi(cp, 1, arg)))))

#define MZC_LTP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, <)
#define MZC_GTP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, >)
#define MZC_LTEP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, <=)
#define MZC_GTEP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, >=)
#define MZC_EQLP(cp, av, bv) MZC_ARITH_COMPARE(cp, av, bv, ==)

#define MZC_ADD1(p, av) ((SCHEME_INTP(av) && (SCHEME_INT_VAL(av) < 0x3FFFFFFF)) \
                         ? scheme_make_integer(SCHEME_INT_VAL(av)+1) \
                         : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))
#define MZC_SUB1(p, av) ((SCHEME_INTP(av) && (SCHEME_INT_VAL(av) > (-0x3FFFFFFF))) \
                         ? scheme_make_integer(SCHEME_INT_VAL(av)-1) \
                         : (arg[0] = av, _scheme_direct_apply_primitive_multi(p, 1, arg)))

#define MZC_ARITH_OP(cp, av, bv, op, revop) \
                      ((SCHEME_INTP(av) && SCHEME_INTP(bv) \
                        && (((SCHEME_INT_VAL(scheme_make_integer(SCHEME_INT_VAL(av) op SCHEME_INT_VAL(bv))) \
                              revop SCHEME_INT_VAL(bv)) \
                             == SCHEME_INT_VAL(av)))) \
                        ? scheme_make_integer(SCHEME_INT_VAL(av) op SCHEME_INT_VAL(bv)) \
                        : ((SCHEME_DBLP(av) && SCHEME_DBLP(bv)) \
                           ? scheme_make_double(SCHEME_DBL_VAL(av) op SCHEME_DBL_VAL(bv)) \
                           : (arg[0] = av, arg[1] = bv, _scheme_direct_apply_primitive_multi(cp, 2, arg))))

#define MZC_PLUS2(cp, av, bv) MZC_ARITH_OP(cp, av, bv, +, -)
#define MZC_MINUS2(cp, av, bv) MZC_ARITH_OP(cp, av, bv, -, +)

