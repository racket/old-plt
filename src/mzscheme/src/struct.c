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

typedef struct {
  Scheme_Type type;
  Scheme_Object *name;
  Scheme_Object *fields;
  Scheme_Object *parent_type_expr;
  int num_fields;
  int count;
  Scheme_Object **memo_names; /* Memoize name generation */
} Struct_Info;

typedef enum {
  SCHEME_CONSTR = 1, 
  SCHEME_PRED, 
  SCHEME_GETTER, 
  SCHEME_SETTER
} Scheme_ProcT;

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Struct_Type *struct_type;
  Scheme_Object *func_name;
  short field;
} Struct_Proc_Info;

/* globals */
Scheme_Object *scheme_arity_at_least, *scheme_date;

/* locals */
static Scheme_Object *make_inspector(int argc, Scheme_Object *argv[]);
static Scheme_Object *inspector_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_inspector(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *struct_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname);

static Scheme_Object *make_named_constructor(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_length(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_ref(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_setter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_getter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_pred_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_constr_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_proc(Scheme_Struct_Type *struct_type, Scheme_Object *func_name, Scheme_ProcT proc_type, int field_num);

static Scheme_Object *make_name(const char *pre, const char *tn, int tnl, const char *post1, const char *fn, int fnl, const char *post2);

static Scheme_Object *struct_execute(Scheme_Object *form);
static Scheme_Object *struct_link(Scheme_Object *form, Link_Info *info);
static Scheme_Object *struct_resolve(Scheme_Object *form, Resolve_Info *info);

static Scheme_Object *write_struct_info(Scheme_Object *obj);
static Scheme_Object *read_struct_info(Scheme_Object *obj);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons scheme_make_pair
#define icons scheme_make_immutable_pair
#define _intern scheme_intern_symbol

#define BUILTIN_STRUCT_FLAGS 0

static Scheme_Object **as_names;
static Scheme_Object **as_values;
static int as_count;
#ifdef TIME_SYNTAX
static Scheme_Object **ts_names;
static Scheme_Object **ts_values;
static int ts_count;
#endif
#ifndef NO_UNIT_SYSTEM
static Scheme_Object *signature;
static Scheme_Object **us_names;
static Scheme_Object **us_values;
static int us_count;
#endif

static Scheme_Object *struct_symbol;

void
scheme_init_struct (Scheme_Env *env)
{
  int i;

  static const char *arity_fields[1] = { "value" };
#ifdef TIME_SYNTAX
  static const char *date_fields[10] = { "second", "minute", "hour",
					 "day", "month", "year",
					 "week-day", "year-day", "dst?", "time-zone-offset" };
#endif
#ifndef NO_UNIT_SYSTEM
  static const char *unit_fields[3] = { "unit", "imports", "exports" };
#endif
  
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
  
  REGISTER_SO(scheme_arity_at_least);
  REGISTER_SO(as_names);
  REGISTER_SO(as_values);
#ifdef TIME_SYNTAX
  REGISTER_SO(scheme_date);
  REGISTER_SO(ts_names);
  REGISTER_SO(ts_values);
#endif
#ifndef NO_UNIT_SYSTEM
  REGISTER_SO(signature);
  REGISTER_SO(us_names);
  REGISTER_SO(us_values);
#endif
  
  REGISTER_SO(struct_symbol);
  
  scheme_register_syntax(STRUCT_EXPD, struct_resolve, struct_link, struct_execute, 1);
  
  struct_symbol = scheme_intern_symbol("struct");
  
  scheme_install_type_writer(scheme_struct_info_type, write_struct_info);
  scheme_install_type_reader(scheme_struct_info_type, read_struct_info);
  
  scheme_arity_at_least = scheme_make_struct_type_from_string("arity-at-least", NULL, 1);
  
  as_names = scheme_make_struct_names_from_array("arity-at-least",
						 1, arity_fields,
						 BUILTIN_STRUCT_FLAGS, 
						 &as_count);
  
#ifdef TIME_SYNTAX
  scheme_date = scheme_make_struct_type_from_string("date", NULL, 10);
  
  ts_names 
    = scheme_make_struct_names_from_array("date",
					  10, date_fields,
					  BUILTIN_STRUCT_FLAGS, &ts_count);
#endif

#ifndef NO_UNIT_SYSTEM
  signature = scheme_make_struct_type_from_string("unit-with-signature", NULL, 3);
  us_names 
    = scheme_make_struct_names_from_array("unit-with-signature",
					  3, unit_fields,
					  BUILTIN_STRUCT_FLAGS, &us_count);
#endif

  as_values = scheme_make_struct_values(scheme_arity_at_least, as_names, as_count, 
					BUILTIN_STRUCT_FLAGS);
#ifdef TIME_SYNTAX
  ts_values = scheme_make_struct_values(scheme_date, ts_names, ts_count, 
					BUILTIN_STRUCT_FLAGS);
#endif
#ifndef NO_UNIT_SYSTEM
  us_values = scheme_make_struct_values(signature, us_names, us_count, 
					BUILTIN_STRUCT_FLAGS);
#endif

  scheme_add_global_keyword("struct", 
			    scheme_make_compiled_syntax(struct_syntax, 
							struct_expand), 
			    env);

  scheme_add_global_constant("struct?",
			    scheme_make_folding_prim(struct_p,
						     "struct?",
						     1, 1, 1),
			    env);
  scheme_add_global_constant("struct-type?",
			    scheme_make_folding_prim(struct_type_p,
						     "struct-type?",
						     1, 1, 1),
			    env);
  scheme_add_global_constant("struct-length",
			    scheme_make_folding_prim(struct_length,
						     "struct-length",
						     1, 1, 1),
			    env);
  scheme_add_global_constant("struct-ref",
			    scheme_make_prim_w_arity(struct_ref,
						     "struct-ref",
						     2, 2),
			    env);
  
  scheme_add_global_constant("make-naming-constructor",
			     scheme_make_prim_w_arity(make_named_constructor,
						      "make-naming-constructor",
						      2, 2),
			     env);

  scheme_add_global_constant("struct->vector",
			    scheme_make_prim_w_arity(struct_to_vector,
						     "struct->vector",
						     1, 1),
			    env);

  scheme_add_global_constant("struct-setter-procedure?",
			     scheme_make_prim_w_arity(struct_setter_p,
						      "struct-setter-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-getter-procedure?",
			     scheme_make_prim_w_arity(struct_getter_p,
						      "struct-getter-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-predicate-procedure?",
			     scheme_make_prim_w_arity(struct_pred_p,
						      "struct-predicate-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-constructor-procedure?",
			     scheme_make_prim_w_arity(struct_constr_p,
						      "struct-constructor-procedure?",
						      1, 1),
			    env);
  
  scheme_add_global_constant("make-inspector",
			     scheme_make_prim_w_arity(make_inspector,
						      "make-inspector",
						      0, 1),
			     env);
  scheme_add_global_constant("inspector?",
			     scheme_make_prim_w_arity(inspector_p,
						      "inspector?",
						      1, 1),
			     env);
  scheme_add_global_constant("current-inspector", 
			     scheme_register_parameter(current_inspector,
						       "current-inspector",
						       MZCONFIG_INSPECTOR),
			     env);


  /* Add arity structure */
  for (i = 0; i < as_count; i++) {
    scheme_add_global_constant(scheme_symbol_val(as_names[i]), as_values[i],
			       env);
  }

#ifdef TIME_SYNTAX
  for (i = 0; i < ts_count; i++) {
    scheme_add_global_constant(scheme_symbol_val(ts_names[i]), ts_values[i], 
			       env);
  }
#endif

#ifndef NO_UNIT_SYSTEM
  for (i = 0; i < us_count; i++) {
    scheme_add_global_constant(scheme_symbol_val(us_names[i]), us_values[i], 
			       env);  
  }
#endif
}

Scheme_Object *scheme_make_initial_inspectors(void)
{
  Scheme_Inspector *superior, *root;

  superior = MALLOC_ONE_TAGGED(Scheme_Inspector);
  superior->type = scheme_inspector_type;
  superior->depth = 0;
  
  root = MALLOC_ONE_TAGGED(Scheme_Inspector);
  root->type = scheme_inspector_type;
  root->depth = 1;
  root->superior = superior;

  return (Scheme_Object *)root;
}

Scheme_Object *make_inspector(int argc, Scheme_Object **argv)
{
  Scheme_Object *superior;
  Scheme_Inspector *naya;

  if (argc) {
    superior = argv[0];
    if (!SAME_TYPE(SCHEME_TYPE(superior), scheme_inspector_type))
      scheme_wrong_type("make-inspector", "inspector", 0, argc, argv);
  } else
    superior = scheme_get_param(scheme_config, MZCONFIG_INSPECTOR);

  naya = MALLOC_ONE_TAGGED(Scheme_Inspector);
  naya->type = scheme_inspector_type;
  naya->depth = ((Scheme_Inspector *)superior)->depth + 1;
  naya->superior = (Scheme_Inspector *)superior;

  return (Scheme_Object *)naya;
}

Scheme_Object *inspector_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_inspector_type)
	  ? scheme_true
	  : scheme_false);
}

int scheme_is_subinspector(Scheme_Object *i, Scheme_Object *sup)
{
  Scheme_Inspector *ins, *superior;

  if (SCHEME_FALSEP(i))
    return 1;

  ins = (Scheme_Inspector *)i;
  superior = (Scheme_Inspector *)sup;

  while (ins->depth >= superior->depth) {
    if (ins == superior)
      return 1;
    ins = ins->superior;
  }
   
  return 0;
}

static Scheme_Object *current_inspector(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-inspector", 
			     scheme_make_integer(MZCONFIG_INSPECTOR),
			     argc, argv,
			     -1, inspector_p, "inspector", 0);
}

/**********************************************************************/

static void wrong_struct_type(Scheme_Object *name, 
			      Scheme_Object *expected,
			      Scheme_Object *received,
			      int which, int argc,
			      Scheme_Object **argv)
{
  if ((SCHEME_SYM_LEN(expected) == SCHEME_SYM_LEN(received))
      && !scheme_strncmp(SCHEME_SYM_VAL(expected), 
			 SCHEME_SYM_VAL(received),
			 SCHEME_SYM_LEN(expected)))
    scheme_raise_exn(MZEXN_APPLICATION_TYPE, argv[which], expected,
			"%s: expects args of type <%s>; "
			"given instance of a different <%s>",
			scheme_symbol_name(name), 
			scheme_symbol_name(expected), 
			scheme_symbol_name(received));
  else
    scheme_wrong_type(scheme_symbol_name(name), 
		      scheme_symbol_name(expected), 
		      which, argc, argv);
}

#define STRUCT_TYPEP(st, v) \
        ((st->name_pos <= v->stype->name_pos) \
	 && (st == v->stype->parent_types[st->name_pos]))

int scheme_is_struct_instance(Scheme_Object *type, Scheme_Object *v)
{
  Scheme_Struct_Type *stype = (Scheme_Struct_Type *)type;
  Scheme_Structure *s = (Scheme_Structure *)v;

  return STRUCT_TYPEP(stype, s);
}

Scheme_Object *scheme_struct_ref(Scheme_Object *sv, int pos)
{
  Scheme_Structure *s = (Scheme_Structure *)sv;
  
  return s->slots[pos];
}

void scheme_struct_set(Scheme_Object *sv, int pos, Scheme_Object *v)
{
  Scheme_Structure *s = (Scheme_Structure *)sv;  
 
  s->slots[pos] = v;
}


Scheme_Object *
scheme_make_struct_instance(Scheme_Object *_stype, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;
  Scheme_Struct_Type *stype;
  int i;

  stype = (Scheme_Struct_Type *)_stype;
  
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((stype->num_slots - 1) * sizeof(Scheme_Object *)));
  
  inst->type = scheme_structure_type;
  inst->stype = stype;

  for (i = 0; i < argc; i++) {
    inst->slots[i] = args[i];
  }
  
  return (Scheme_Object *)inst;
}

static Scheme_Object *struct_pred(Scheme_Struct_Type *stype, int argc, Scheme_Object **args)
{
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type)
      && STRUCT_TYPEP(stype, ((Scheme_Structure *)args[0])))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *struct_getter(Struct_Proc_Info *i, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;

  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type))
    scheme_wrong_type(scheme_symbol_name(i->func_name), 
		      scheme_symbol_name(i->struct_type->type_name), 
		      0, argc, args);
  
  inst = (Scheme_Structure *)args[0];
  if (!STRUCT_TYPEP(i->struct_type, inst))
    wrong_struct_type(i->func_name, 
		      i->struct_type->type_name, 
		      SCHEME_STRUCT_NAME_SYM(inst), 
		      0, argc, args);
  
  return inst->slots[i->field];
}

static Scheme_Object *struct_setter(Struct_Proc_Info *i, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;

  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type))
    scheme_wrong_type(scheme_symbol_name(i->func_name), 
		      scheme_symbol_name(i->struct_type->type_name), 
		      0, argc, args);
	
  inst = (Scheme_Structure *)args[0];
  if (!STRUCT_TYPEP(i->struct_type, inst))
    wrong_struct_type(i->func_name, 
		      i->struct_type->type_name, 
		      SCHEME_STRUCT_NAME_SYM(inst),
		      0, argc, args);
	
  inst->slots[i->field] = args[1];
  
  return scheme_void;
}

int scheme_equal_structs (Scheme_Object *obj1, Scheme_Object *obj2)
{
  Scheme_Structure *s1, *s2;
  int i;

  if (!SAME_TYPE(SCHEME_TYPE(obj1), scheme_structure_type)
      || !SAME_TYPE(SCHEME_TYPE(obj2), scheme_structure_type))
    return 0;

  s1 = (Scheme_Structure *)obj1;
  s2 = (Scheme_Structure *)obj2;

  if (SCHEME_STRUCT_TYPE(s1) != SCHEME_STRUCT_TYPE(s2))
    return 0;

  for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
    if (!scheme_equal(s1->slots[i], s2->slots[i]))
      return 0;
  }

  return 1;
}

static Scheme_Object *
struct_p(int argc, Scheme_Object *argv[])
{
  return SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type)
    ? scheme_true : scheme_false;
}

static Scheme_Object *
struct_type_p(int argc, Scheme_Object *argv[])
{
  return SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_struct_type_type)
    ? scheme_true : scheme_false;
}

static Scheme_Object *
struct_length(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type("struct-length", "struct", 0, argc, argv);

  s = (Scheme_Structure *)argv[0];

  return scheme_make_integer(SCHEME_STRUCT_NUM_SLOTS(s));
}

static Scheme_Object *
struct_ref(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;
  long i, m;
  char *name = "struct-ref";

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type(name, "struct", 0, argc, argv);
  
  if (!scheme_is_subinspector(SCHEME_STRUCT_INSPECTOR(argv[0]), 
			      scheme_get_param(scheme_config, MZCONFIG_INSPECTOR))) {
    scheme_arg_mismatch(name, "current inspector cannot inspect struct: ", argv[0]);
    return NULL;
  }

  s = (Scheme_Structure *)argv[0];
  m = SCHEME_STRUCT_NUM_SLOTS(s);

  i = scheme_extract_index(name, 1, argc, argv, m);

  if (i >= m)
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     argv[1],
		     "%s: index %s out of range [%d, %d]",
		     name, 
		     scheme_make_provided_string(argv[1], 0, NULL), 
		     0, m - 1);

  return s->slots[i];
}

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;
  Scheme_Object *v, **array;
  int i, m;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type("struct->vector", "struct", 0, argc, argv);

  if (!scheme_is_subinspector(SCHEME_STRUCT_INSPECTOR(argv[0]), 
			      scheme_get_param(scheme_config, MZCONFIG_INSPECTOR))) {
    scheme_arg_mismatch("struct->vector", "current inspector cannot inspect struct: ", argv[0]);
    return NULL;
  }

  s = (Scheme_Structure *)argv[0];

  m = SCHEME_STRUCT_NUM_SLOTS(s);

  v = scheme_make_vector(m + 1, scheme_false);
  array = SCHEME_VEC_ELS(v);
  array[0] = SCHEME_STRUCT_NAME_SYM(s);
  array++;
  for (i = m; i--; ) {
    array[i] = s->slots[i];
  }

  return v;
}

#define STRUCT_PROCP(o, t) \
    (SCHEME_STRUCT_PROCP(o) && (((Scheme_Closed_Primitive_Proc *)o)->flags & t))

static Scheme_Object *
struct_setter_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_SETTER)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_getter_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_GETTER)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_pred_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_PRED)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_constr_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_CONSTR)
	  ? scheme_true : scheme_false);
}

#define NUM_BASE_VALUES 3
#define NUM_VALUES_PER_FIELD 2

#define TYPE_NAME(base, blen) make_name("struct:", base, blen, "", NULL, 0, "")
#define CSTR_NAME(base, blen) make_name("make-", base, blen, "", NULL, 0, "")
#define PRED_NAME(base, blen) make_name("", base, blen, "?", NULL, 0, "")
#define GET_NAME(base, blen, field, flen) make_name("", base, blen, "-", field, flen, "")
#define SET_NAME(base, blen, field, flen) make_name("set-", base, blen, "-", field, flen, "!")

Scheme_Object **scheme_make_struct_values(Scheme_Object *type,
					  Scheme_Object **names,
					  int count,
					  int flags)
{
  Scheme_Struct_Type *struct_type;
  Scheme_Object **values;
  int slot_num, pos;

  struct_type = (Scheme_Struct_Type *)type;

  values = MALLOC_N_STUBBORN(Scheme_Object *, count);
 
#ifdef MEMORY_COUNTING_ON
  if (scheme_starting_up) {
    /* We know that these values will be kept (exns, arity-at-least, etc.). */
    scheme_misc_count += count * sizeof(Scheme_Object *);
  }
#endif

  pos = 0;
  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    values[pos++] = (Scheme_Object *)struct_type;
  if (!(flags & SCHEME_STRUCT_NO_CONSTR)) {
    Scheme_Object *vi;
    vi = make_struct_proc(struct_type,
			  names[pos],
			  SCHEME_CONSTR, 
			  struct_type->num_slots);
    values[pos] = vi;
    pos++;
  }
  if (!(flags & SCHEME_STRUCT_NO_PRED)) {
    Scheme_Object *vi;
    vi = make_struct_proc(struct_type,
			  names[pos],
			  SCHEME_PRED,
			  0);
    values[pos] = vi;
    pos++;
  }

  slot_num = (struct_type->name_pos
	      ? struct_type->parent_types[struct_type->name_pos - 1]->num_slots 
	      : 0);
  while (pos < count) {
    if (!(flags & SCHEME_STRUCT_NO_GET)) {
      Scheme_Object *vi;
      vi = make_struct_proc(struct_type,
			    names[pos],
			    SCHEME_GETTER,
			    slot_num);
      values[pos] = vi;
      pos++;
    }
    
    if (!(flags & SCHEME_STRUCT_NO_SET)) {
      Scheme_Object *vi;
      vi = make_struct_proc(struct_type,
			    names[pos],
			    SCHEME_SETTER,
			    slot_num);
      values[pos] = vi;
      pos++;
    }

    slot_num++;
  }
  
  scheme_end_stubborn_change((void *)values);
 
  return values;
}

static Scheme_Object **_make_struct_names(const char *base, int blen,
					  int fcount,
					  Scheme_Object *field_symbols,
					  const char **field_strings,
					  int flags, int *count_out)
{
  Scheme_Object **names;
  const char *field_name;
  int count, fnlen;
  int slot_num, pos;

  count = 0;

  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_CONSTR))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_PRED))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_GET))
    count += fcount;
  if (!(flags & SCHEME_STRUCT_NO_SET))
    count += fcount;

  if (count_out) {
    *count_out = count;
    count_out = NULL; /* Might be an interior pointer. */
  }

  names = MALLOC_N_STUBBORN(Scheme_Object *, count);

#ifdef MEMORY_COUNTING_ON
  if (scheme_starting_up) {
    /* We know that these names will be kept (exns, arity-at-least, etc.). */
    scheme_misc_count += count * sizeof(Scheme_Object *);
  }
#endif

  pos = 0;

  if (!(flags & SCHEME_STRUCT_NO_TYPE)) {
    Scheme_Object *nm;
    nm = TYPE_NAME(base, blen);
    names[pos++] = nm;
  }
  if (!(flags & SCHEME_STRUCT_NO_CONSTR)) {
    Scheme_Object *nm;
    nm = CSTR_NAME(base, blen);
    names[pos++] = nm;
  }
  if (!(flags & SCHEME_STRUCT_NO_PRED)) {
    Scheme_Object *nm;
    nm = PRED_NAME(base, blen);
    names[pos++] = nm;
  }

  if (fcount) {
    for (slot_num = 0; slot_num < fcount; slot_num++) {
      if (field_symbols) {
	Scheme_Object *fn = SCHEME_CAR(field_symbols);
	field_symbols = SCHEME_CDR(field_symbols);

	field_name = scheme_symbol_val(fn);
	fnlen = SCHEME_SYM_LEN(fn);
      } else {
	field_name = field_strings[slot_num];
	fnlen = strlen(field_name);
      }

      if (!(flags & SCHEME_STRUCT_NO_GET)) {
	Scheme_Object *nm;
	nm = GET_NAME(base, blen, field_name, fnlen);
	names[pos++] = nm;
      }
      if (!(flags & SCHEME_STRUCT_NO_SET)) {
	Scheme_Object *nm;
	nm = SET_NAME(base, blen, field_name, fnlen);
	names[pos++] = nm;
      }
    }
  }

  scheme_end_stubborn_change((void *)names);

  return names;
}

Scheme_Object **scheme_make_struct_names(Scheme_Object *base, 
					 Scheme_Object *field_symbols,
					 int flags, int *count_out)
{
  int len;
  len = field_symbols ? scheme_list_length(field_symbols) : 0;

  return _make_struct_names(scheme_symbol_val(base),
			    SCHEME_SYM_LEN(base),
			    len,
			    field_symbols, NULL,
			    flags, count_out);
}

Scheme_Object **scheme_make_struct_names_from_array(const char *base, 
						    int fcount,
						    const char **fields,
						    int flags, int *count_out)
{
  return _make_struct_names(base,
			    strlen(base),
			    fcount,
			    NULL, fields,
			    flags, count_out);
}

static Scheme_Object *_make_struct_type(const char *base, int blen,
					Scheme_Object *parent,
					Scheme_Object *inspector,
					int num_fields)
{
  Scheme_Struct_Type *struct_type, *parent_type;
  int j, depth;
  
  parent_type = (Scheme_Struct_Type *)parent;

  depth = parent_type ? (1 + parent_type->name_pos) : 0;

  struct_type =(Scheme_Struct_Type *)scheme_malloc_tagged(sizeof(Scheme_Struct_Type)
							  + (depth 
							     * sizeof(Scheme_Struct_Type *)));
  
  struct_type->type = scheme_struct_type_type;
  struct_type->name_pos = depth;
  struct_type->parent_types[depth] = struct_type;
  for (j = depth; j--; ) {
    struct_type->parent_types[j] = parent_type->parent_types[j];
  }

  {
    Scheme_Object *tn;
    tn = TYPE_NAME(base, blen);
    struct_type->type_name = tn;
  }
  struct_type->num_slots = num_fields + (parent_type ? parent_type->num_slots : 0);

  if (!inspector) {
    if (parent_type)
      inspector = parent_type->inspector;
    else {
      inspector = scheme_get_param(scheme_config, MZCONFIG_INSPECTOR);
      inspector = (Scheme_Object *)((Scheme_Inspector *)inspector)->superior;
    }
  }
  struct_type->inspector = inspector;

  return (Scheme_Object *)struct_type;
}

Scheme_Object *scheme_make_struct_type(Scheme_Object *base,
				       Scheme_Object *parent,
				       Scheme_Object *inspector,
				       int num_fields)
{
  return _make_struct_type(scheme_symbol_val(base),
			   SCHEME_SYM_LEN(base),
			   parent, inspector, num_fields);
}

Scheme_Object *scheme_make_struct_type_from_string(const char *base,
						   Scheme_Object *parent,
						   int num_fields)
{
  return _make_struct_type(base, strlen(base), parent, scheme_false, num_fields);
}

static Scheme_Object *
struct_execute (Scheme_Object *form)
{
  Struct_Info *info;
  Scheme_Object **values, **names, *parent, *inspector;
  Scheme_Object *type;

  info = (Struct_Info *)form;

  parent = (SCHEME_NULLP(info->parent_type_expr)
	    ? NULL 
	    : _scheme_eval_linked_expr(info->parent_type_expr));

  if (parent && (SCHEME_FALSEP(parent) 
		 || SAME_TYPE(SCHEME_TYPE(parent), scheme_inspector_type))) {
    inspector = parent;
    parent = NULL;
  } else
    inspector = NULL;

  if (parent && !SAME_TYPE(SCHEME_TYPE(parent),
			   scheme_struct_type_type))
    scheme_raise_exn(MZEXN_STRUCT,
		     "struct: supertype expression returned "
		     "a value that is not a struct type value");

  type = scheme_make_struct_type(info->name,
				 parent, 
				 inspector,
				 info->num_fields);

  if (!info->memo_names) {
    Scheme_Object **sa;
    int c;
    sa = scheme_make_struct_names(info->name,
				  info->fields,
				  0, 
				  &c);
    info->count = c;
    info->memo_names = sa;
  }
  names = info->memo_names;

  values = scheme_make_struct_values(type, names, info->count, 0);

  return scheme_values(info->count, values);
}

static Scheme_Object *
struct_link(Scheme_Object *expr, Link_Info *info)
{
  Struct_Info *osinfo = (Struct_Info *)expr;
  Struct_Info *nsinfo;
  Scheme_Object *e;

  e = osinfo->parent_type_expr;
  if (e)
    e = scheme_link_expr(e, info);

  /* If expression needs no linking, reuse old one */
  if (e == osinfo->parent_type_expr)
    return (Scheme_Object *)osinfo;

  nsinfo = MALLOC_ONE_TAGGED(Struct_Info);
  memcpy(nsinfo, osinfo, sizeof(Struct_Info));
  nsinfo->parent_type_expr = e;

  return scheme_make_syntax_linked(STRUCT_EXPD, (Scheme_Object *)nsinfo);
}

static Scheme_Object *
struct_resolve(Scheme_Object *expr, Resolve_Info *info)
{
  Struct_Info *sinfo;

  sinfo = (Struct_Info *)expr;

  if (sinfo->parent_type_expr) {
    Scheme_Object *le;
    le = scheme_resolve_expr(sinfo->parent_type_expr, info);
    sinfo->parent_type_expr = le;
  }

  return scheme_make_syntax_resolved(STRUCT_EXPD, expr);
}

static Scheme_Object *
do_struct_syntax (Scheme_Object *forms, Scheme_Comp_Env *env, 
		  Scheme_Compile_Info *in_rec, int drec, 
		  int depth, Scheme_Object *boundname)
{
  Struct_Info *info;
  Scheme_Object *base_symbol, *field_symbols, *l, *form, *parent_expr;
  int count;

  form = SCHEME_STX_CDR(forms);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax("struct", form, forms, NULL);
  base_symbol = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax("struct", form, forms, NULL);
  field_symbols = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  if (!SCHEME_STX_NULLP(form)) 
    scheme_wrong_syntax("struct", form, forms, NULL);

  if (in_rec)
    scheme_compile_rec_done_local(in_rec, drec);

  if (SCHEME_STX_PAIRP(base_symbol)) {
    parent_expr = SCHEME_STX_CDR(base_symbol);
    base_symbol = SCHEME_STX_CAR(base_symbol);
    if (!SCHEME_STX_PAIRP(parent_expr) || !SCHEME_STX_NULLP(SCHEME_STX_CDR(parent_expr))) {
      scheme_wrong_syntax("struct", parent_expr, forms, "improper name-parent expression");
      return NULL;
    }
    parent_expr = SCHEME_STX_CAR(parent_expr);
    if (in_rec)
      parent_expr = scheme_compile_expr(parent_expr, env, in_rec, drec);
    else
      parent_expr = scheme_expand_expr(parent_expr, env, depth, scheme_false);
  } else {
    parent_expr = NULL;

    if (in_rec)
      in_rec[drec].max_let_depth = 0;
  }

  if (!SCHEME_STX_SYMBOLP(base_symbol))
    scheme_wrong_syntax("struct", base_symbol, form, "struct name must be an identifier");
  
  if (in_rec) {
    Scheme_Object *fields;

    info = MALLOC_ONE_TAGGED(Struct_Info);
    info->type = scheme_struct_info_type;
    
    info->name = SCHEME_STX_SYM(base_symbol);
    fields = scheme_syntax_to_datum(field_symbols, 0, NULL);
    info->fields = fields;
    info->parent_type_expr = parent_expr ? parent_expr : scheme_null;
  } else
    info = NULL;

  count = 0;
  l = field_symbols;
  while (!SCHEME_STX_NULLP(l)) {
    Scheme_Object *a;
    count++;
    if (!SCHEME_STX_PAIRP(l))
      scheme_wrong_syntax("struct", l, form, "badly formed field list");
    a = SCHEME_STX_CAR(l);
    if (!SCHEME_STX_SYMBOLP(a))
      scheme_wrong_syntax("struct", a, form, "field name must be an identifier");
    l = SCHEME_STX_CDR(l);
  }

  if (in_rec) {
    info->num_fields = count;
    info->memo_names = NULL;

    return scheme_make_syntax_compiled(STRUCT_EXPD, (Scheme_Object *)info);
  } else {
    Scheme_Object *base, *rest;
    base = (parent_expr 
	    ? icons(base_symbol,
		    icons(parent_expr, scheme_null))
	    : base_symbol);
    rest = SCHEME_STX_CAR(forms);
    return scheme_datum_to_syntax(icons(rest,
					icons(base,
					      icons(field_symbols, scheme_null))),
				  forms, forms, 0);
  }
}

static Scheme_Object *
struct_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec, int drec)
{
  return do_struct_syntax(form, env, rec, drec, 0, scheme_false);
}

static Scheme_Object *
struct_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth, Scheme_Object *boundname)
{
  return do_struct_syntax(form, env, NULL, 0, depth, boundname);
}

static Scheme_Object *make_named_constructor(int argc, Scheme_Object *argv[])
{
  Scheme_Object *t, **nms, **vs;
  int c, flags;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_struct_type_type))
    scheme_wrong_type("make-naming-constructor", "struct type", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type("make-naming-constructor", "symbol", 1, argc, argv);

  flags = SCHEME_STRUCT_NO_TYPE | SCHEME_STRUCT_NO_PRED;

  t = scheme_make_struct_type(argv[1], argv[0], NULL, 0);
  nms = scheme_make_struct_names(argv[1], scheme_null, flags, &c);
  vs = scheme_make_struct_values(t, nms, c, flags);

  return vs[0];
}

static Scheme_Object *
make_struct_proc(Scheme_Struct_Type *struct_type, 
		 Scheme_Object *func_name, 
		 Scheme_ProcT proc_type, int field_num)
{
  Scheme_Object *p;
  short flags = SCHEME_PRIM_IS_STRUCT_PROC;

  if (proc_type == SCHEME_CONSTR) {
    p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)scheme_make_struct_instance,
					(void *)struct_type,
					scheme_symbol_name(func_name),
					struct_type->num_slots,
					struct_type->num_slots,
					0);
    flags |= SCHEME_PRIM_IS_STRUCT_CONSTR;
  } else if (proc_type == SCHEME_PRED) {
    p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_pred,
					(void *)struct_type,
					scheme_symbol_name(func_name),
					1, 1, 1);
    flags |= SCHEME_PRIM_IS_STRUCT_PRED;
  } else {
    Struct_Proc_Info *i;

    i = MALLOC_ONE_RT(Struct_Proc_Info);
#ifdef MZTAG_REQUIRED
    i->type = scheme_rt_struct_proc_info;
#endif
    i->struct_type = struct_type;
    i->func_name = func_name;
    i->field = field_num;

    if (proc_type == SCHEME_GETTER) {
      p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_getter,
					  (void *)i,
					  scheme_symbol_name(func_name),
					  1, 1, 1);
      flags |= SCHEME_PRIM_IS_STRUCT_GETTER;
    } else {
      p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_setter,
					  (void *)i,
					  scheme_symbol_name(func_name),
					  2, 2, 0);
      flags |= SCHEME_PRIM_IS_STRUCT_SETTER;
    }
  }

  ((Scheme_Closed_Primitive_Proc *)p)->flags |= flags;

  return p;
}

static Scheme_Object *make_name(const char *pre, const char *tn, int ltn,
				const char *post1, const char *fn, int lfn,
				const char *post2)
{
  int total, lp, lp1, lp2;
  char *name, buffer[256];

  total = lp = strlen(pre);
  total += ltn;
  total += (lp1 = strlen(post1));
  total += lfn;
  total += (lp2 = strlen(post2));

  if (total < 256)
    name = buffer;
  else
    name = (char *)scheme_malloc_atomic(sizeof(char)*(total + 1));
  
  memcpy(name, pre, lp);
  total = lp;
  memcpy(name + total, tn, ltn);
  total += ltn;
  memcpy(name + total, post1, lp1);
  total += lp1;
  memcpy(name + total, fn, lfn);
  total += lfn;
  memcpy(name + total, post2, lp2);
  total += lp2;

  name[total] = 0;

  return scheme_intern_exact_symbol(name, total);
}

/************************************************************************/

#define cons scheme_make_pair

static Scheme_Object *write_struct_info(Scheme_Object *obj)
{
  Struct_Info *info;

  info = (Struct_Info *)obj;

  return cons(scheme_make_integer(info->count),
	      cons(scheme_make_integer(info->num_fields),
		   cons(info->name, 
			cons(scheme_protect_quote(info->parent_type_expr),
			     info->fields))));
}

#define X_SCHEME_ASSERT(x, y)

static Scheme_Object *read_struct_info(Scheme_Object *obj)
{
  Scheme_Object *v, *first = scheme_null, *last = NULL;
  Struct_Info *info;

#define BAD_CS "bad compiled structure info"

  info = (Struct_Info *)scheme_malloc_stubborn_tagged(sizeof(Struct_Info));
  info->type = scheme_struct_info_type;

  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  X_SCHEME_ASSERT(SCHEME_INTP(v), BAD_CS);
  info->count = SCHEME_INT_VAL(v);

  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  X_SCHEME_ASSERT(SCHEME_INTP(v), BAD_CS);
  info->num_fields = SCHEME_INT_VAL(v);

  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  info->name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  info->parent_type_expr = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  X_SCHEME_ASSERT(SCHEME_LISTP(obj), BAD_CS);

  /* must copy obj */
  while (SCHEME_PAIRP(obj)) {
    Scheme_Object *pair;
    pair = scheme_make_pair(SCHEME_CAR(obj), scheme_null);

    if (last)
      SCHEME_CDR(last) = pair;
    else
      first = pair;
    last = pair;

    obj = SCHEME_CDR(obj);
  }

  info->fields = first;

  info->memo_names = NULL;
  
  scheme_end_stubborn_change((void *)info);

  return (Scheme_Object *)info;
}

#ifdef MEMORY_COUNTING_ON
void scheme_count_struct_info(Scheme_Object *o, long *s, long *e, 
			      Scheme_Hash_Table *ht)
{
  Struct_Info *info = (Struct_Info *)o;

  *s = sizeof(Struct_Info);
  *e = (ht 
	? (scheme_count_memory(info->name, ht)
	   + scheme_count_memory(info->parent_type_expr, ht)
	   + scheme_count_memory(info->fields, ht))
	: 0);
}
#endif

/**********************************************************************/

#if MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_STRUCT_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_structure_type, mark_struct_val);
  GC_REG_TRAV(scheme_struct_type_type, mark_struct_type_val);
  GC_REG_TRAV(scheme_struct_info_type, mark_struct_info_val);

  GC_REG_TRAV(scheme_rt_struct_proc_info, mark_struct_proc_info);
}

END_XFORM_SKIP;

#endif
