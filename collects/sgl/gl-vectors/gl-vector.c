#include "escheme.h"
#ifdef XONX
#include <GL/gl.h>
#else
#ifdef OS_X
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#endif
#include <math.h>

/* Primitives for working with gl-<type-name>-vectors.
   All parameters are checked, except that Scheme values 
   that must be converted into <type>s are not.  This
   happens in vector->gl-<type-name>-vector and
   gl-<type-name>-vector-set!.
*/

static Scheme_Type gl_<type-name>_vector_type;
#include "gl-<type-name>-vector.h"

static Scheme_Object* scheme_make_boolean(GLboolean x)
{
  if (x)
    return scheme_true;
  else
    return scheme_false;
}

static GLboolean scheme_get_boolean(Scheme_Object* o)
{
  return o != scheme_false;
}

static GLint scheme_get_int(Scheme_Object* o)
{
  long l;
  l = 0;
  scheme_get_int_val(o, &l);
  return l;
}

static GLuint scheme_get_uint(Scheme_Object* o)
{
  unsigned long l;
  l = 0;
  scheme_get_unsigned_int_val(o, &l);
  return l;
}

static void check_type(const char *name, const Scheme_Object *v,
		       int which, int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(v), gl_<type-name>_vector_type))
    scheme_wrong_type(name, "gl-<type-name>-vector", which, argc, argv);
  return;
}

/* Copied from vector.c in the mzscheme sources and modified */
static Scheme_Object *
bad_index(char *name, Scheme_Object *vec, Scheme_Object *i, int len)
{
  if (len) {
    char *vstr;
    int vlen;
    vstr = scheme_make_provided_string(vec, 2, &vlen);
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     i,
		     "%s: index %s out of range [%d, %d] for gl-<type-name>-vector: %t",
		     name, 
		     scheme_make_provided_string(i, 2, NULL), 
		     0, len-1,
		     vstr, vlen);
  } else
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     i,
		     "%s: bad index %s for empty gl-<type-name>-vector",
		     name,
		     scheme_make_provided_string(i, 0, NULL));
  
  return NULL;
}

/* Copied from fun.c in do_map in the mzscheme sources and modified */
static void length_mismatch(const char *name, int argc, Scheme_Object **argv, int i)
{
  char *argstr;
  long alen;
  
  argstr = scheme_make_args_string("", -1, argc, argv, &alen);
  
  scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, argv[i],
		   "%s: all gl-<type-name>-vectors must have same length%t", 
		   name, argstr, alen);
  return;
}

/* 1 argument, a vector.
   assumes the values in the vector can be converted to <type>
*/
static Scheme_Object* vector_to_gl_<type-name>_vector(int argc, Scheme_Object **argv)
{
  int length, i;
  Scheme_Object **old_vec;
  gl_<type-name>_vector *new_vec;

  if (!SCHEME_VECTORP(argv[0]))
    scheme_wrong_type("vector->gl-<type-name>-vector", "vector", 0, argc, argv);

  length = SCHEME_VEC_SIZE(argv[0]);
  new_vec = make_gl_<type-name>_vector(length);
  old_vec = SCHEME_VEC_ELS(argv[0]);

  for (i = 0; i < length; ++i)
    new_vec->els[i] = <sreal-to-type>(old_vec[i]);
  
  return (Scheme_Object*)new_vec;
}

/* 1 argument, a gl-<type-name>-vector */
static Scheme_Object* gl_<type-name>_vector_to_vector(int argc, Scheme_Object **argv)
{
  int length, i;
  Scheme_Object *new_vec;
  gl_<type-name>_vector *old_vec; 

  check_type("gl-<type-name>-vector", argv[0], 0, argc, argv);

  old_vec = (gl_<type-name>_vector*)argv[0];
  length = old_vec->length;

  new_vec = scheme_make_vector(old_vec->length, scheme_void);
  for (i = 0; i < length; ++i)
    SCHEME_VEC_ELS(new_vec)[i] = <type-to-scheme>(old_vec->els[i]);
  return new_vec;
}

/* 1 argument, a gl-<type-name>-vector */
static Scheme_Object* gl_<type-name>_vector_length(int argc, Scheme_Object **argv)
{
  check_type("gl-<type-name>-vector-length", argv[0], 0, argc, argv);

  return scheme_make_integer(((gl_<type-name>_vector*)argv[0])->length);
}

/* 2 arguments, a gl-<type-name>-vector and a non-negative integer */
static Scheme_Object* gl_<type-name>_vector_ref(int argc, Scheme_Object **argv)
{
  int length;
  long l;

  check_type("gl-<type-name>-vector-ref", argv[0], 0, argc, argv);
  
  length = ((gl_<type-name>_vector*)argv[0])->length;

  if (!SCHEME_EXACT_INTEGERP(argv[1]))
    scheme_wrong_type("gl-<type-name>-vector-ref",
		      "non-negative exact integer",
		      1, argc, argv);

  l = SCHEME_INT_VAL(argv[1]);

  if (SCHEME_BIGNUMP(argv[1]) || l < 0 || l >= length)
    bad_index("gl-<type-name>-vector-ref", argv[0], argv[1], length);
  
  return <type-to-scheme>(((gl_<type-name>_vector*)argv[0])->els[SCHEME_INT_VAL(argv[1])]);
}

/* 3 arguments, a gl-<type-name>-vector, a non-negative integer,
   assumes the 3rd is convertible to <type>
*/
static Scheme_Object* gl_<type-name>_vector_set(int argc, Scheme_Object **argv)
{
  int length;
  long l;

  check_type("gl-<type-name>-vector-ref", argv[0], 0, argc, argv);
  
  length = ((gl_<type-name>_vector*)argv[0])->length;

  if (!SCHEME_EXACT_INTEGERP(argv[1]))
    scheme_wrong_type("gl-<type-name>-vector-set!",
		      "non-negative exact integer",
		      1, argc, argv);

  l = SCHEME_INT_VAL(argv[1]);

  if (SCHEME_BIGNUMP(argv[1]) || l < 0 || l >= length)
    bad_index("gl-<type-name>-vector-set!", argv[0], argv[1], length);

  ((gl_<type-name>_vector*)argv[0])->els[SCHEME_INT_VAL(argv[1])] = <sreal-to-type>(argv[2]);
  return scheme_void;
}

/* 1 argument */
static Scheme_Object* gl_<type-name>_vectorP(int argc, Scheme_Object **argv)
{
  if (SCHEME_TYPE(argv[0]) == gl_<type-name>_vector_type)
    return scheme_true;
  else
    return scheme_false;
}

/* 2 arguments, gl-<type-name>-vectors of equal lengths. */
static Scheme_Object* gl_<type-name>_vector_plus(int argc, Scheme_Object **argv)
{
  gl_<type-name>_vector* vec;
  int length, i;
  gl_<type-name>_vector *v1, *v2;

  check_type("gl-<type-name>-vector+", argv[0], 0, argc, argv);
  check_type("gl-<type-name>-vector+", argv[1], 1, argc, argv);

  v1 = (gl_<type-name>_vector*) argv[0];
  v2 = (gl_<type-name>_vector*) argv[1];

  length = v1->length;

  if (length != v2->length)
    length_mismatch("gl-<type-name>-vector+", argc, argv, 1);

  vec = make_gl_<type-name>_vector(length);
  
  for (i = 0; i < length; ++i)
    vec->els[i] = v1->els[i] + v2->els[i];

  return (Scheme_Object*) vec;  
}

/* 2 arguments, gl-<type-name>-vector with equal lengths. */
static Scheme_Object* gl_<type-name>_vector_minus(int argc, Scheme_Object **argv)
{
  gl_<type-name>_vector* vec;
  int length, i;
  gl_<type-name>_vector *v1, *v2;

  check_type("gl-<type-name>-vector-", argv[0], 0, argc, argv);
  check_type("gl-<type-name>-vector-", argv[1], 1, argc, argv);

  v1 = (gl_<type-name>_vector*) argv[0];
  v2 = (gl_<type-name>_vector*) argv[1];

  length = v1->length;

  if (length != v2->length)
    length_mismatch("gl-<type-name>-vector-", argc, argv, 1);

  vec = make_gl_<type-name>_vector(length);
  
  for (i = 0; i < length; ++i)
    vec->els[i] = v1->els[i] - v2->els[i];

  return (Scheme_Object*) vec;  
}

/* 2 arguments, a scheme real and a gl-<type-name>-vector */
static Scheme_Object* gl_<type-name>_vector_times(int argc, Scheme_Object **argv)
{
  gl_<type-name>_vector* vec;
  int length, i;
  <type> d;
  gl_<type-name>_vector *v0;

  if (!SCHEME_REALP(argv[0]))
    scheme_wrong_type("gl-<type-name>-vector-times", "real number", 0, argc, argv);

  check_type("gl-<type-name>-vector*", argv[1], 1, argc, argv);

  v0 = (gl_<type-name>_vector*) argv[1];

  d = <sreal-to-type>(argv[0]);
  length = v0->length;
  vec = make_gl_<type-name>_vector(length);

  for (i = 0; i < length; ++i)
    vec->els[i] = d * v0->els[i];

  return (Scheme_Object*)vec;
}

/* 1 argument, a gl-<type-name>-vector */
static Scheme_Object* gl_<type-name>_vector_norm(int argc, Scheme_Object **argv)
{
  int length, i;
  <type> d;
  gl_<type-name>_vector *v;

  check_type("gl-<type-name>-vector-norm", argv[0], 0, argc, argv);

  v = (gl_<type-name>_vector*) argv[0];
  length = v->length;
  d = 0.0;
  for (i = 0; i < length; ++i)
    d = d + v->els[i] * v->els[i];
  return <type-to-scheme>(sqrt(d));
}


static void add_func(char *name, Scheme_Env *mod, Scheme_Prim *c_fun, int mina, int maxa)
{
  scheme_add_global(name, scheme_make_prim_w_arity(c_fun, name, mina, maxa), mod);
  return;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *mod_env = scheme_primitive_module(scheme_intern_symbol("gl-<type-name>-vector"), env);
  
  add_func("vector->gl-<type-name>-vector", mod_env, vector_to_gl_<type-name>_vector, 1, 1);
  add_func("gl-<type-name>-vector->vector", mod_env, gl_<type-name>_vector_to_vector, 1, 1);
  add_func("gl-<type-name>-vector-length", mod_env, gl_<type-name>_vector_length, 1, 1);
  add_func("gl-<type-name>-vector-ref", mod_env, gl_<type-name>_vector_ref, 2, 2);
  add_func("gl-<type-name>-vector-set!", mod_env, gl_<type-name>_vector_set, 3, 3);
  add_func("gl-<type-name>-vector?", mod_env, gl_<type-name>_vectorP, 1, 1);
  add_func("gl-<type-name>-vector+", mod_env, gl_<type-name>_vector_plus, 2, 2);
  add_func("gl-<type-name>-vector-", mod_env, gl_<type-name>_vector_minus, 2, 2);
  add_func("gl-<type-name>-vector*", mod_env, gl_<type-name>_vector_times, 2, 2);
  add_func("gl-<type-name>-vector-norm", mod_env, gl_<type-name>_vector_norm, 1, 1);
  scheme_add_global("gl-<type-name>-vector-internal-type", 
		    scheme_make_integer(gl_<type-name>_vector_type),
		    mod_env);

  scheme_finish_primitive_module(mod_env);
  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  gl_<type-name>_vector_type = scheme_make_type("<gl-<type-name>-vector>");
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("gl-<type-name>-vector");
}



