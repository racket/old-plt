#include <escheme.h>
#include <stdlib.h>
#include <stdarg.h>


#define GLOBAL_VARREF(x) ((x)->val ? (x)->val : \
  (scheme_unbound_global((Scheme_Object*)(x)->key), (Scheme_Object *)NULL))


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
