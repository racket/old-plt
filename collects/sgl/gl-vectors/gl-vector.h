#ifndef _GL_<type-name>_VECTOR_PRIMS_H
#define _GL_<type-name>_VECTOR_PRIMS_H

typedef struct 
{
  Scheme_Type type;
  short keyex; /* 1 in low bit indicates immutable */
  int length;
  <type> els[1];
} gl_<type-name>_vector;

static gl_<type-name>_vector* make_gl_<type-name>_vector(int length)
{
  gl_<type-name>_vector* vec;

  vec = (gl_<type-name>_vector*)scheme_malloc_atomic(sizeof(<type>) * length +
						     sizeof(gl_<type-name>_vector) - sizeof(<type>));
  vec->type = gl_<type-name>_vector_type;
  vec->length = length;
  vec->keyex = 0;
  return vec;
}

static <type> *arg_<type-name>_data(const char *name, Scheme_Object *arg, 
				    unsigned long n,
				    int which, int argc, Scheme_Object** argv)
{
  if (gl_<type-name>_vector_type == SCHEME_TYPE(arg) &&
      ((gl_<type-name>_vector *) arg)->length == n)
    return ((gl_<type-name>_vector *) arg)->els;
  else
  {
    char err_str[128];
    sprintf(err_str, "gl-<type-name>-vector of length %d", n);
    scheme_wrong_type(name, err_str, which, argc, argv);
  }
  return NULL;
}

#define arg_<type>v(idx, len) \
          ((<type> *) arg_<type-name>_data((char *)p, v[idx], len, idx, c, v))

#endif
