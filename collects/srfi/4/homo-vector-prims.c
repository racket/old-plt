#include "escheme.h"
#include <math.h>

Scheme_Type homo_<type-name>_vector_type;
#include "homo-<type-name>-vector-prims.h"

char *homo_vector_int32msg = "expected int32, given %V";
char *homo_vector_anytypemsg = "expected <type-name>, given %V";

static int32_t scheme_get_int32(Scheme_Object* o)
{
  long l;
  int result;

  l = 0;

  result = scheme_get_int_val(o, &l);
  if ((result == 0) || (l > 0x7fffffff) || (l < -0x7fffffff))
    scheme_raise_exn(MZEXN_APPLICATION_TYPE,o,scheme_intern_symbol("int32"),
		     homo_vector_int32msg,o);

  return l;
}

/* references to scheme_get(u)int are inserted by the generating function
   that specializes this code by type */

/* to extend this code to handle 64-bit ints, change the return types of 
   these functions: */

static int32_t scheme_get_int(Scheme_Object* o)
{
  long l;
  int result;

  l = 0;

  result = scheme_get_int_val(o, &l);
  if ((result == 0) || (<enable-check> && (l > <type-largest> || l < <type-smallest>)))
    scheme_raise_exn(MZEXN_APPLICATION_TYPE,o,scheme_intern_symbol("<type-name>"),homo_vector_anytypemsg,o);
  
  return l;
}

static uint32_t scheme_get_uint(Scheme_Object* o)
{
  unsigned long l;
  int result;

  l = 0;

  result = scheme_get_unsigned_int_val(o, &l);
  if ((result == 0) || (<enable-check> && (l > <type-largest> || l < <type-smallest>)))
    scheme_raise_exn(MZEXN_APPLICATION_TYPE,o,scheme_intern_symbol("<type-name>"),homo_vector_anytypemsg,o);
  
  return l;
}

/* 1 argument, assumes that input is a scheme number */

static Scheme_Object *construct_homo_<type-name>_vector_uninitialized(int argc, Scheme_Object **argv)
{
  int32_t length;
  homo_<type-name>_vector *new_vec;

  length = scheme_get_int32(argv[0]);

  new_vec = make_homo_<type-name>_vector(length);

  return (Scheme_Object *)new_vec;
}

/* 2 arguments, assumest that inputs are two scheme real numbers */

static Scheme_Object *construct_homo_<type-name>_vector(int argc, Scheme_Object **argv)
{
  int32_t length = scheme_get_int32(argv[0]);
  int32_t i;
  <type> initializer = <sreal-to-type>(argv[1]);
  homo_<type-name>_vector *new_vec;
  
  new_vec = make_homo_<type-name>_vector(length);

  for (i = 0; i < length; i++) {
    new_vec->els[i] = initializer;
  }

  return (Scheme_Object *)new_vec;
}

/* 1 argument, assumes that input is a vector of Scheme real numbers */
static Scheme_Object* vector_to_homo_<type-name>_vector(int argc, Scheme_Object **argv)
{
  int32_t length, i; 
  Scheme_Object** old_vec;
  homo_<type-name>_vector* new_vec;

  length = SCHEME_VEC_SIZE(argv[0]);
  new_vec = make_homo_<type-name>_vector(length);
  old_vec = SCHEME_VEC_ELS(argv[0]);

  for (i = 0; i < length; ++i) {
    new_vec->els[i] = <sreal-to-type>(old_vec[i]);
  }
  
  return (Scheme_Object*)new_vec;
}

/* 1 argument, assumes that input is a homo-<type-name>-vector */
static Scheme_Object* homo_<type-name>_vector_to_vector(int argc, Scheme_Object **argv)
{
  Scheme_Object* new_vec;
  int32_t i, length;
  homo_<type-name>_vector* old_vec; 

  old_vec = (homo_<type-name>_vector*)argv[0];
  length = old_vec->length;

  new_vec = scheme_make_vector(old_vec->length, scheme_void);
  for (i = 0; i < length; ++i)
    SCHEME_VEC_ELS(new_vec)[i] = <type-to-scheme>(old_vec->els[i]);
  return new_vec;
}

/* 1 argument, assumes that input is a homo-<type-name>-vector */
static Scheme_Object* homo_<type-name>_vector_length(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(((homo_<type-name>_vector*)argv[0])->length);
}

/* 2 arguments, assumes first is homo-<type-name>-vector, 2nd is a scheme integer */
static Scheme_Object* homo_<type-name>_vector_ref(int argc, Scheme_Object **argv)
{
  return <type-to-scheme>(((homo_<type-name>_vector*)argv[0])->els[SCHEME_INT_VAL(argv[1])]);
}

/* 3 arguments: assume homo-<type-name>-vector, scheme integer, scheme real number */
static Scheme_Object* homo_<type-name>_vector_set(int argc, Scheme_Object **argv)
{
  ((homo_<type-name>_vector*)argv[0])->els[SCHEME_INT_VAL(argv[1])] = <sreal-to-type>(argv[2]);
  return scheme_void;
}

/* 1 argument, no assumptions */
static Scheme_Object* homo_<type-name>_vectorP(int argc, Scheme_Object **argv)
{
  if (SCHEME_TYPE(argv[0]) == homo_<type-name>_vector_type)
    return scheme_true;
  else
    return scheme_false;
}

/* 2 arguments, assume both homo-<type-name>-vector of the same length */
static Scheme_Object* homo_<type-name>_vector_plus(int argc, Scheme_Object **argv)
{
  homo_<type-name>_vector* vec;
  int32_t length, i;
  homo_<type-name>_vector *v1, *v2;

  v1 = (homo_<type-name>_vector*) argv[0];
  v2 = (homo_<type-name>_vector*) argv[1];

  length = v1->length;
  vec = make_homo_<type-name>_vector(length);
  
  for (i = 0; i < length; ++i)
    vec->els[i] = v1->els[i] + v2->els[i];

  return (Scheme_Object*) vec;  
}

/* 2 arguments, assume both homo-<type-name>-vector of the same length */
static Scheme_Object* homo_<type-name>_vector_minus(int argc, Scheme_Object **argv)
{
  homo_<type-name>_vector* vec;
  int32_t length, i;
  homo_<type-name>_vector *v1, *v2;

  v1 = (homo_<type-name>_vector*) argv[0];
  v2 = (homo_<type-name>_vector*) argv[1];

  length = v1->length;
  vec = make_homo_<type-name>_vector(length);
  
  for (i = 0; i < length; ++i)
    vec->els[i] = v1->els[i] - v2->els[i];

  return (Scheme_Object*) vec;  
}

/* 2 arguments, assume first scheme real number, 2nd homo-<type-name>-vector */
static Scheme_Object* homo_<type-name>_vector_times(int argc, Scheme_Object **argv)
{
  homo_<type-name>_vector* vec;
  int32_t length, i;
  <type> d;
  homo_<type-name>_vector *v0;

  v0 = (homo_<type-name>_vector*) argv[1];

  d = <sreal-to-type>(argv[0]);
  length = v0->length;
  vec = make_homo_<type-name>_vector(length);

  for (i = 0; i < length; ++i)
    vec->els[i] = d * v0->els[i];

  return (Scheme_Object*)vec;
}

/* 1 argument, homo-<type-name>-vector */
static Scheme_Object* homo_<type-name>_vector_norm(int argc, Scheme_Object **argv)
{
  int32_t length, i;
  <type> d;
  homo_<type-name>_vector *v;

  v = (homo_<type-name>_vector*) argv[0];
  length = v->length;
  d = 0.0;
  for (i = 0; i < length; ++i)
    d = d + v->els[i] * v->els[i];
  return <type-to-scheme>(sqrt(d));
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object* funs[13];

  funs[0] = scheme_make_prim_w_arity(vector_to_homo_<type-name>_vector,
				     "vector-><type-name>vector",
				     0, -1);
  funs[1] = scheme_make_prim_w_arity(homo_<type-name>_vector_to_vector,
				     "<type-name>vector->vector",
				     0, -1);
  funs[2] = scheme_make_prim_w_arity(homo_<type-name>_vector_length,
				     "<type-name>vector-length",
				     0, -1);
  funs[3] = scheme_make_prim_w_arity(homo_<type-name>_vector_ref,
				     "<type-name>vector-ref",
				     0, -1);
  funs[4] = scheme_make_prim_w_arity(homo_<type-name>_vector_set,
				     "<type-name>vector-set!",
				     0, -1);
  funs[5] = scheme_make_prim_w_arity(homo_<type-name>_vectorP,
				     "<type-name>vector?",
				     1, 1);
  funs[6] = scheme_make_prim_w_arity(homo_<type-name>_vector_plus,
				     "<type-name>vector-sum",
				     0, -1);
  funs[7] = scheme_make_prim_w_arity(homo_<type-name>_vector_minus,
				     "<type-name>vector-difference",
				     0, -1);
  funs[8] = scheme_make_prim_w_arity(homo_<type-name>_vector_times,
				     "<type-name>vector-scale",
				     0, -1);
  funs[9] = scheme_make_prim_w_arity(homo_<type-name>_vector_norm,
				     "<type-name>vector-norm",
				     0, -1);
  funs[10] = scheme_make_integer(homo_<type-name>_vector_type);
  funs[11] = scheme_make_prim_w_arity(construct_homo_<type-name>_vector_uninitialized,
				      "make-<type-name>vector-uninitialized",
				      1, 1);
  funs[12] = scheme_make_prim_w_arity(construct_homo_<type-name>_vector,
				      "make-<type-name>vector",
				      2, 2);

  return scheme_values(13, funs);
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  homo_<type-name>_vector_type = scheme_make_type("<<type-name>vector>");
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_false;
}



