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

#define PROP_USE_HT_COUNT 5

typedef enum {
  SCHEME_CONSTR = 1, 
  SCHEME_PRED, 
  SCHEME_GETTER, 
  SCHEME_SETTER,
  SCHEME_GEN_GETTER, 
  SCHEME_GEN_SETTER
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

static Scheme_Object *make_struct_type_property(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_property_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_type(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_field_accessor(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_struct_field_mutator(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_setter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_getter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_pred_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_constr_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_proc(Scheme_Struct_Type *struct_type, Scheme_Object *func_name, 
				       Scheme_ProcT proc_type, int field_num);

static Scheme_Object *make_name(const char *pre, const char *tn, int tnl, const char *post1, 
				const char *fn, int fnl, const char *post2);

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

Scheme_Object *ellipses_symbol;

#define TYPE_NAME(base, blen) make_name("struct:", base, blen, "", NULL, 0, "")
#define CSTR_NAME(base, blen) make_name("make-", base, blen, "", NULL, 0, "")
#define PRED_NAME(base, blen) make_name("", base, blen, "?", NULL, 0, "")
#define GET_NAME(base, blen, field, flen) make_name("", base, blen, "-", field, flen, "")
#define SET_NAME(base, blen, field, flen) make_name("set-", base, blen, "-", field, flen, "!")
#define GENGET_NAME(base, blen) make_name("", base, blen, "-ref", NULL, 0, "")
#define GENSET_NAME(base, blen) make_name("", base, blen, "-set!", NULL, 0, "")

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
  
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  /* Add arity structure */
  REGISTER_SO(scheme_arity_at_least);
  REGISTER_SO(as_names);
  REGISTER_SO(as_values);
  scheme_arity_at_least = scheme_make_struct_type_from_string("arity-at-least", NULL, 1);  
  as_names = scheme_make_struct_names_from_array("arity-at-least",
						 1, arity_fields,
						 BUILTIN_STRUCT_FLAGS, 
						 &as_count);
  as_values = scheme_make_struct_values(scheme_arity_at_least, as_names, as_count, 
					BUILTIN_STRUCT_FLAGS);
  for (i = 0; i < as_count; i++) {
    scheme_add_global_constant(scheme_symbol_val(as_names[i]), as_values[i],
			       env);
  }

#ifdef TIME_SYNTAX
  /* Add date structure: */
  REGISTER_SO(scheme_date);
  REGISTER_SO(ts_names);
  REGISTER_SO(ts_values);
  scheme_date = scheme_make_struct_type_from_string("date", NULL, 10);
  
  ts_names = scheme_make_struct_names_from_array("date",
						 10, date_fields,
						 BUILTIN_STRUCT_FLAGS, &ts_count);

  ts_values = scheme_make_struct_values(scheme_date, ts_names, ts_count, 
					BUILTIN_STRUCT_FLAGS);
  for (i = 0; i < ts_count; i++) {
    scheme_add_global_constant(scheme_symbol_val(ts_names[i]), ts_values[i], 
			       env);
  }
#endif

  /*** basic interface ****/

  scheme_add_global_constant("make-struct-type", 
			    scheme_make_prim_w_arity2(make_struct_type,
						      "make-struct-type",
						      4, 7,
						      4, 4),
			    env);

  scheme_add_global_constant("make-struct-type-property", 
			    scheme_make_prim_w_arity2(make_struct_type_property,
						      "make-struct-type-property",
						      0, 1,
						      3, 3),
			    env);

  scheme_add_global_constant("make-struct-field-accessor",
			     scheme_make_prim_w_arity(make_struct_field_accessor,
						      "make-struct-field-accessor",
						      2, 3),
			     env);
  scheme_add_global_constant("make-struct-field-mutator",
			     scheme_make_prim_w_arity(make_struct_field_mutator,
						      "make-struct-field-mutator",
						      2, 3),
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
  scheme_add_global_constant("struct-type-property?",
			     scheme_make_folding_prim(struct_type_property_p,
						     "struct-type-property?",
						     1, 1, 1),
			    env);

  /*** Debugging ****/

#if 0
  scheme_add_global_constant("struct-type",
			     scheme_make_prim_w_arity(struct_type,
						      "struct-type",
						      2, 2),
			     env);
  scheme_add_global_constant("struct-type-info",
			     scheme_make_prim_w_arity(struct_type_info,
						      "struct-type-info",
						      5, 5),
			     env);
#endif
  scheme_add_global_constant("struct->vector",
			     scheme_make_prim_w_arity(struct_to_vector,
						      "struct->vector",
						      1, 2),
			     env);

  /*** Predicates ****/

  scheme_add_global_constant("struct-accessor-procedure?",
			     scheme_make_prim_w_arity(struct_setter_p,
						      "struct-accessor-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-mutator-procedure?",
			     scheme_make_prim_w_arity(struct_getter_p,
						      "struct-mutator-procedure?",
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
  
  /*** Inspectors ****/

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

  REGISTER_SO(ellipses_symbol);
  ellipses_symbol = scheme_intern_symbol("...");
}

/*========================================================================*/
/*                             inspectors                                 */
/*========================================================================*/

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

/*========================================================================*/
/*                             properties                                 */
/*========================================================================*/

static Scheme_Object *prop_pred(Scheme_Object *prop, int argc, Scheme_Object **args)
{
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type)) {
    Scheme_Struct_Type *stype = ((Scheme_Structure *)args[0])->stype;

    if (stype->props_ht) {
      if (scheme_lookup_in_table(stype->props_ht, (char *)prop))
	return scheme_true;
    } else {
      int i;
      for (i = stype->num_props; i--; ) {
	if (SAME_OBJ(SCHEME_CAR(stype->props[i]), prop))
	  return scheme_true;
      }
    }
  }
   
  return scheme_false;
}

static Scheme_Object *prop_accessor(Scheme_Object *prop, int argc, Scheme_Object **args)
{
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type)) {
    Scheme_Struct_Type *stype = ((Scheme_Structure *)args[0])->stype;

    if (stype->props_ht) {
      Scheme_Object *v;
      v = (Scheme_Object *)scheme_lookup_in_table(stype->props_ht, (char *)prop);
      if (v)
	return v;
    } else {
      int i;
      for (i = stype->num_props; i--; ) {
	if (SAME_OBJ(SCHEME_CAR(stype->props[i]), prop))
	  return SCHEME_CDR(stype->props[i]);
      }
    }
  }
  
  scheme_wrong_type("property accessor", "...", 0, argc, args);
  return NULL;
}

static Scheme_Object *make_struct_type_property(int argc, Scheme_Object *argv[])
{
  Scheme_Struct_Property *p;
  Scheme_Object *a[3], *v;
  char *name;
  int len;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("make-struct-type-property", "symbol", 0, argc, argv);

  p = MALLOC_ONE_TAGGED(Scheme_Struct_Property);
  p->type = scheme_struct_property_type;
  p->name = argv[0];

  a[0] = (Scheme_Object *)p;

  len = SCHEME_SYM_LEN(argv[0]);
  name = MALLOC_N_ATOMIC(char, len + 2);
  memcpy(name, SCHEME_SYM_VAL(argv[0]), len);
  name[len] = '?';
  name[len+1] = 0;

  v = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)prop_pred,
				      (void *)p,
				      name,
				      1, 1, 0);
  a[1] = v;

  name = MALLOC_N_ATOMIC(char, len + 10);
  memcpy(name, SCHEME_SYM_VAL(argv[0]), len);
  memcpy(name + len, "-accessor", 10);

  v = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)prop_accessor,
				      (void *)p,
				      name,
				      1, 1, 0);
  a[2] = v;

  return scheme_values(3, a);
}

static Scheme_Object *struct_type_property_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_struct_property_type)
	  ? scheme_true : scheme_false);
}

/*========================================================================*/
/*                             struct ops                                 */
/*========================================================================*/

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
  int p, i, j, nis, ns, c;

  stype = (Scheme_Struct_Type *)_stype;

  c = stype->num_slots;
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((c - 1) * sizeof(Scheme_Object *)));
  
  inst->type = scheme_structure_type;
  inst->stype = stype;

  j = c;
  i = argc;
  for (p = stype->name_pos; p >= 0; p--) {
    /* Fill in defaults: */
    if (p) {
      ns = stype->parent_types[p]->num_slots - stype->parent_types[p - 1]->num_slots;
      nis = stype->parent_types[p]->num_islots - stype->parent_types[p - 1]->num_islots;
    } else {
      ns = stype->parent_types[0]->num_slots;
      nis = stype->parent_types[0]->num_islots;
    }

    ns -= nis;

    while (ns--) {
      inst->slots[--j] = scheme_false;
    }

    while (nis--) {
      inst->slots[--j] = args[--i];
    }
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

static int parse_pos(const char *who, Struct_Proc_Info *i, Scheme_Object **args, int argc)
{
  int pos;

  if (!SCHEME_INTP(args[1]) || (SCHEME_INT_VAL(args[1]) < 0)) {
    if (SCHEME_BIGNUMP(args[1]) && SCHEME_BIGPOS(args[1])) {
      pos = 32769; /* greater than max field count */
    } else {
      if (!who)
	who = scheme_symbol_name(i->func_name);
      scheme_wrong_type(who, 
			"non-negative exact integer", 
			1, argc, args);
      return 0;
    }
  } else
    pos = SCHEME_INT_VAL(args[1]);
  
  if ((pos < i->struct_type->num_slots)
      && i->struct_type->name_pos)
    pos += i->struct_type->parent_types[i->struct_type->name_pos - 1]->num_slots;
  
  if (pos >= i->struct_type->num_slots) {
    if (!who)
      who = scheme_symbol_name(i->func_name);

    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, args[1],
		     "%s: slot index for <%S> not in [0, %d]: %V",
		     who,
		     i->struct_type->type_name,
		     (i->struct_type->name_pos
		      ? (i->struct_type->num_slots
			 - i->struct_type->parent_types[i->struct_type->name_pos - 1]->num_slots)
		      : i->struct_type->num_slots) - 1,
		     args[1]);
    return 0;
  }

  return pos;
}

static Scheme_Object *struct_getter(Struct_Proc_Info *i, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;
  int pos;

  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type)) {
    scheme_wrong_type(scheme_symbol_name(i->func_name), 
		      scheme_symbol_name(i->struct_type->type_name), 
		      0, argc, args);
    return NULL;
  }
  
  inst = (Scheme_Structure *)args[0];

  if (!STRUCT_TYPEP(i->struct_type, inst)) {
    wrong_struct_type(i->func_name, 
		      i->struct_type->type_name, 
		      SCHEME_STRUCT_NAME_SYM(inst), 
		      0, argc, args);
    return NULL;
  }
  
  if (argc == 2)
    pos = parse_pos(NULL, i, args, argc);
  else
    pos = i->field;

  return inst->slots[pos];
}

static Scheme_Object *struct_setter(Struct_Proc_Info *i, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;
  int pos;
  Scheme_Object *v;

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
	
  if (argc == 3) {
    pos = parse_pos(NULL, i, args, argc);
    v = args[2];
  } else {
    pos = i->field;
    v = args[1];
  }

  inst->slots[pos] = v;
  
  return scheme_void;
}

int scheme_equal_structs(Scheme_Object *obj1, Scheme_Object *obj2)
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
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type)
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *
struct_type_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_struct_type_type)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;
  Scheme_Struct_Type *stype;
  Scheme_Object *v, **array, *insp, *unknown_val;
  int i, m, p, n, last_is_unknown;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type("struct->vector", "struct", 0, argc, argv);

  if (argc > 1)
    unknown_val = argv[1];
  else
    unknown_val = ellipses_symbol;

  insp = scheme_get_param(scheme_config, MZCONFIG_INSPECTOR);

  s = (Scheme_Structure *)argv[0];

  stype = s->stype;
  p = stype->name_pos + 1;
  m = 0;
  last_is_unknown = 0;
  while (p--) {
    stype = stype->parent_types[p];
    if (!scheme_is_subinspector(stype->inspector, insp)) {
      if (!last_is_unknown)
	m++;
      last_is_unknown = 1;
    } else {
      last_is_unknown = 0;
      if (p)
	m += stype->num_slots - stype->parent_types[p-1]->num_slots;
      else
	m += stype->num_slots;
    }
  }

  stype = s->stype;
  p = stype->name_pos + 1;
  i = stype->num_slots;
  last_is_unknown = 0;
 
  v = scheme_make_vector(m + 1, NULL);
  array = SCHEME_VEC_ELS(v);
  array[0] = SCHEME_STRUCT_NAME_SYM(s);
  array++;
  while (p--) {
    stype = stype->parent_types[p];
    if (p)
      n = stype->num_slots - stype->parent_types[p-1]->num_slots;
    else
      n = stype->num_slots;
      
    if (!scheme_is_subinspector(stype->inspector, insp)) {
      if (!last_is_unknown)
	array[--m] = unknown_val;
      i -= n;
      last_is_unknown = 1;
    } else {
      while (n--) {
	array[--m] = s->slots[--i];
      }
      last_is_unknown = 0;
    }
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

static Scheme_Object *make_struct_field_xxor(const char *who, int getter,
					      int argc, Scheme_Object *argv[])
{
  Struct_Proc_Info *i;
  int pos;  
  Scheme_Object *name;
  const char *fieldstr;
  char digitbuf[20];
  int fieldstrlen;

  if (!STRUCT_PROCP(argv[0], (getter
			      ? SCHEME_PRIM_IS_STRUCT_GETTER
			      : SCHEME_PRIM_IS_STRUCT_SETTER))
      || (((Scheme_Closed_Primitive_Proc *)argv[0])->mina == 1)) {
    scheme_wrong_type(who, (getter 
			    ? "accessor procedure that requires a field index"
			    : "mutator procedure that requires a field index"),
		      0, argc, argv);
    return NULL;
  }

  i = (Struct_Proc_Info *)((Scheme_Closed_Primitive_Proc *)argv[0])->data;

  pos = parse_pos(who, i, argv, argc);
  
  if (argc > 2) {
    if (!SCHEME_SYMBOLP(argv[2])) {
      scheme_wrong_type(who, "symbol", 2, argc, argv);
      return NULL;
    }
    fieldstr = scheme_symbol_val(argv[2]);
    fieldstrlen = SCHEME_SYM_LEN(argv[2]);
  } else {
    sprintf(digitbuf, "field%d", (int)SCHEME_INT_VAL(argv[1]));
    fieldstr = digitbuf;
    fieldstrlen = strlen(fieldstr);
  }

  if (getter) {
    name = GET_NAME(scheme_symbol_val(i->struct_type->name), 
		    SCHEME_SYM_LEN(i->struct_type->name),
		    fieldstr, fieldstrlen);
  } else {
    name = SET_NAME(scheme_symbol_val(i->struct_type->name), 
		    SCHEME_SYM_LEN(i->struct_type->name),
		    fieldstr, fieldstrlen);
  }

  return make_struct_proc(i->struct_type, 
			  name, 
			  (getter ? SCHEME_GETTER : SCHEME_SETTER), pos);
}

static Scheme_Object *make_struct_field_accessor(int argc, Scheme_Object *argv[])
{
  return make_struct_field_xxor("make-struct-field-accessor", 1, argc, argv);
				
}

static Scheme_Object *make_struct_field_mutator(int argc, Scheme_Object *argv[])
{
  return make_struct_field_xxor("make-struct-field-mutator", 0, argc, argv);
}

/*========================================================================*/
/*                          struct op maker                               */
/*========================================================================*/

#define NUM_BASE_VALUES 3
#define NUM_VALUES_PER_FIELD 2

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

  if (flags & SCHEME_STRUCT_GEN_GET)
    --count;
  if (flags & SCHEME_STRUCT_GEN_SET)
    --count;

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

  if (flags & SCHEME_STRUCT_GEN_GET) {
    Scheme_Object *vi;
    vi = make_struct_proc(struct_type,
			  names[pos],
			  SCHEME_GEN_GETTER,
			  slot_num);
    values[pos] = vi;
    pos++;
  }
  if (flags & SCHEME_STRUCT_GEN_SET) {
    Scheme_Object *vi;
    vi = make_struct_proc(struct_type,
			  names[pos],
			  SCHEME_GEN_SETTER,
			  slot_num);
    values[pos] = vi;
    pos++;
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
  if (flags & SCHEME_STRUCT_GEN_GET)
    count++;
  if (flags & SCHEME_STRUCT_GEN_SET)
    count++;

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

  if (flags & SCHEME_STRUCT_GEN_GET) {
    Scheme_Object *nm;
    nm = GENGET_NAME(base, blen);
    names[pos++] = nm;
  }
  if (flags & SCHEME_STRUCT_GEN_SET) {
    Scheme_Object *nm;
    nm = GENSET_NAME(base, blen);
    names[pos++] = nm;
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
					struct_type->num_islots,
					struct_type->num_islots,
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
    int need_pos;

    i = MALLOC_ONE_RT(Struct_Proc_Info);
#ifdef MZTAG_REQUIRED
    i->type = scheme_rt_struct_proc_info;
#endif
    i->struct_type = struct_type;
    i->func_name = func_name;
    i->field = field_num;

    if ((proc_type == SCHEME_GEN_GETTER)
	|| (proc_type == SCHEME_GEN_SETTER))
      need_pos = 1;
    else
      need_pos = 0;

    if ((proc_type == SCHEME_GETTER) || (proc_type == SCHEME_GEN_GETTER)) {
      p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_getter,
					  (void *)i,
					  scheme_symbol_name(func_name),
					  1 + need_pos, 1 + need_pos, 1);
      flags |= SCHEME_PRIM_IS_STRUCT_GETTER;
    } else {
      p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_setter,
					  (void *)i,
					  scheme_symbol_name(func_name),
					  2 + need_pos, 2 + need_pos, 0);
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

/*========================================================================*/
/*                             struct type                                */
/*========================================================================*/

static Scheme_Object *_make_struct_type(Scheme_Object *basesym, const char *base, int blen,
					Scheme_Object *parent,
					Scheme_Object *inspector,
					int num_fields,
					int num_uninit_fields)
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
    if (basesym)
      tn = basesym;
    else
      tn = scheme_intern_exact_symbol(base, blen);
    struct_type->name = tn;
    if (basesym) {
      base = scheme_symbol_val(basesym);
      blen = SCHEME_SYM_LEN(basesym);
    }
    tn = TYPE_NAME(base, blen);
    struct_type->type_name = tn;
  }
  struct_type->num_slots = num_fields + num_uninit_fields + (parent_type ? parent_type->num_slots : 0);
  struct_type->num_islots = num_fields + (parent_type ? parent_type->num_islots : 0);

  /* Check for integer overflow or total more than 32768: */
  if ((num_fields < 0) || (num_uninit_fields < 0)
      || (num_fields > 32768)
      || (num_uninit_fields > 32768)
      || (num_uninit_fields + num_fields > 32768)
      || (parent_type
	  && ((struct_type->num_slots < parent_type->num_slots)
	      || (struct_type->num_islots < parent_type->num_islots)))) {
    /* Too many fields. */
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "too many fields for struct-type; maximum total field count is 32768");
    return NULL;
  }
  
  if (!inspector) {
    if (parent_type)
      inspector = parent_type->inspector;
    else {
      inspector = scheme_get_param(scheme_config, MZCONFIG_INSPECTOR);
      inspector = (Scheme_Object *)((Scheme_Inspector *)inspector)->superior;
    }
  }
  struct_type->inspector = inspector;

  if (parent_type) {
    struct_type->num_props = parent_type->num_props;
    struct_type->props = parent_type->props;
    struct_type->props_ht = parent_type->props_ht;
  }

  return (Scheme_Object *)struct_type;
}

Scheme_Object *scheme_make_struct_type(Scheme_Object *base,
				       Scheme_Object *parent,
				       Scheme_Object *inspector,
				       int num_fields)
{
  return _make_struct_type(base, NULL, 0,
			   parent, inspector, num_fields, 0);
}

Scheme_Object *scheme_make_struct_type_from_string(const char *base,
						   Scheme_Object *parent,
						   int num_fields)
{
  return _make_struct_type(NULL, base, strlen(base), parent, scheme_false, num_fields, 0);
}

static Scheme_Object *make_struct_type(int argc, Scheme_Object **argv)
{
  int initc, uninitc, num_props = 0, opaque = 1, i;
  Scheme_Object *props = scheme_null, *l, *a, **r;
  Scheme_Object *inspector = NULL, **names;
  Scheme_Struct_Type *type;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("make-struct-type", "symbol", 0, argc, argv);
  if (!SCHEME_FALSEP(argv[1])
      && !SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_struct_type_type))
    scheme_wrong_type("make-struct-type", "struct-type or #f", 1, argc, argv);

  if (!SCHEME_INTP(argv[2]) || (SCHEME_INT_VAL(argv[2]) < 0)) {
    if (SCHEME_BIGNUMP(argv[2]) && SCHEME_BIGPOS(argv[2]))
      initc = -1;
    else {
      scheme_wrong_type("make-struct-type", "non-negative exact integer", 2, argc, argv);
      return NULL;
    }
  } else
    initc = SCHEME_INT_VAL(argv[2]);

  if (!SCHEME_INTP(argv[3]) || (SCHEME_INT_VAL(argv[3]) < 0)) {
    if (SCHEME_BIGNUMP(argv[3]) && SCHEME_BIGPOS(argv[3]))
      uninitc = -1;
    else {
      scheme_wrong_type("make-struct-type", "non-negative exact integer", 3, argc, argv);
      return NULL;
    }
  } else
    uninitc = SCHEME_INT_VAL(argv[3]);
  
  if (argc > 4) {
    props = argv[4];
    for (l = props; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      if (!SCHEME_PAIRP(a)
	  || !SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(a)), scheme_struct_property_type))
	break;
      num_props++;
    }
    if (!SCHEME_NULLP(l)) {
      scheme_wrong_type("make-struct-type", "list of struct-type-property--value pairs", 4, argc, argv);
    }

    if (argc > 5) {
      if (!SAME_TYPE(SCHEME_TYPE(argv[5]), scheme_inspector_type))
	scheme_wrong_type("make-struct-type", "inspector", 5, argc, argv);

      inspector = argv[5];
      if (argc > 6)
	opaque = SCHEME_TRUEP(argv[6]);
    }
  }

  if (!inspector)
    inspector = scheme_get_param(scheme_config, MZCONFIG_INSPECTOR);

  if (opaque)
    inspector = (Scheme_Object *)((Scheme_Inspector *)inspector)->superior;

  type = (Scheme_Struct_Type *)_make_struct_type(argv[0], NULL, 0, 
						 SCHEME_FALSEP(argv[1]) ? NULL : argv[1],
						 inspector,
						 initc, uninitc);

  if (num_props) {
    if (type->props_ht || (type->num_props + num_props > PROP_USE_HT_COUNT)) {
      Scheme_Hash_Table *ht;

      ht = scheme_hash_table(type->num_props + num_props, SCHEME_hash_ptr, 0, 0);
    
      if (!type->props_ht) {
	for (i = 0; i < type->num_props; i++) {
	  scheme_add_to_table(ht, (char *)SCHEME_CAR(type->props[i]), SCHEME_CDR(type->props[i]), 0);
	}
	type->props_ht = ht;
	type->num_props = 0;
	type->props = NULL;
      } else {
	/* Duplicate hash table: */
	Scheme_Bucket **bs;
	bs = type->props_ht->buckets;
	for (i =  type->props_ht->count; i--; ) {
	  if (bs[i] && bs[i]->key) {
	    scheme_add_to_table(ht, bs[i]->key, bs[i]->val, 0);
	  }
	}
      }

      /* Add new props: */
      for (l = props; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	a = SCHEME_CAR(l);
	if (scheme_lookup_in_table(ht, (char *)SCHEME_CAR(a))) {
	  /* Property is already in the supertype */
	  break;
	}
	scheme_add_to_table(ht, (char *)SCHEME_CAR(a), SCHEME_CDR(a), 0);
      }
    } else {
      /* Make props array: */
      Scheme_Object **pa;
      int j;

      i = type->num_props;
      
      pa = MALLOC_N(Scheme_Object *, i + num_props);
      memcpy(pa, type->props, sizeof(Scheme_Object *) * i);

      for (l = props; SCHEME_PAIRP(l); l = SCHEME_CDR(l), i++) {
	a = SCHEME_CAR(l);
	
	for (j = 0; j < i; j++) {
	  if (SAME_OBJ(SCHEME_CAR(pa[j]), SCHEME_CAR(a)))
	    break;
	}
	if (j < i)
	  break;

	a = scheme_make_pair(SCHEME_CAR(a), SCHEME_CDR(a));
	pa[i] = a;
      }
      
      type->num_props += num_props;
      type->props = pa;
    }

    if (!SCHEME_NULLP(l)) {
      /* SCHEME_CAR(l) is a duplicate */
      a = SCHEME_CAR(l);
      scheme_arg_mismatch("make-struct-type", "duplicate property binding", a);
    }
  }

  names = scheme_make_struct_names(argv[0],
				   NULL,
				   SCHEME_STRUCT_GEN_GET | SCHEME_STRUCT_GEN_SET, 
				   &i);
  r = scheme_make_struct_values((Scheme_Object *)type, names, i, 
				SCHEME_STRUCT_GEN_GET | SCHEME_STRUCT_GEN_SET);

  return scheme_values(i, r);
}

/**********************************************************************/

#if MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_STRUCT_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_structure_type, mark_struct_val);
  GC_REG_TRAV(scheme_struct_type_type, mark_struct_type_val);
  GC_REG_TRAV(scheme_struct_property_type, mark_struct_prop_val);

  GC_REG_TRAV(scheme_inspector_type, mark_inspector);

  GC_REG_TRAV(scheme_rt_struct_proc_info, mark_struct_proc_info);
}

END_XFORM_SKIP;

#endif
