/* 
 * GLSCM 0.9 -- An OpenGL 1.3 extension of MzScheme
 *
 * Copyright (C) 2002 Robert Kooima <r.l.kooima@larc.nasa.gov>
 *
 * This  library is  free  software; you  can  redistribute it  and/or
 * modify it under the terms  of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
 * MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU
 * Lesser General Public License for more details.
 *
 * Modifications Copyright (C) 2003 by Scott Owens <sowens@cs.utah.edu>
 *
 */

#ifdef _WIN32
#include <windows.h>
#endif

#include <escheme.h>

#ifdef XONX
#include <GL/gl.h>
#else
#ifdef OS_X
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#endif

#ifndef GL_CLIENT_ALL_ATTRIB_BITS
#define GL_CLIENT_ALL_ATTRIB_BITS GL_ALL_CLIENT_ATTRIB_BITS
#endif

#ifndef GL_ALL_CLIENT_ATTRIB_BITS
#define GL_ALL_CLIENT_ATTRIB_BITS GL_CLIENT_ALL_ATTRIB_BITS
#endif

struct scm_prim
{
	char               *name;
	Scheme_Closed_Prim *prim;
	int                 mina;
	int                 maxa;
};

static void scheme_load_prim(Scheme_Env *env, const struct scm_prim *scm_prim,
			     unsigned long s)
{
	Scheme_Object *prim;
	int i, N = s / sizeof (struct scm_prim);

	/* printf("%d procedures\n", N - 45); */

	for (i = 0; i < N; i++)
	{
		prim = scheme_make_closed_prim_w_arity(scm_prim[i].prim,
						       scm_prim[i].name,
						       scm_prim[i].name,
						       scm_prim[i].mina,
						       scm_prim[i].maxa);
		scheme_add_global(scm_prim[i].name, prim, env);
	}
}

static Scheme_Type gl_double_vector_type;
static Scheme_Type gl_float_vector_type;
static Scheme_Type gl_int_vector_type;
static Scheme_Type gl_short_vector_type;
static Scheme_Type gl_byte_vector_type;
static Scheme_Type gl_uint_vector_type;
static Scheme_Type gl_ushort_vector_type;
static Scheme_Type gl_ubyte_vector_type;
static Scheme_Type gl_boolean_vector_type;

static void scheme_set_types()
{
  Scheme_Object *dynamic_require = scheme_builtin_value("dynamic-require");
  Scheme_Object *args[2];
  
  args[0] = scheme_make_string("gl-vectors/gl-double-vector.ss");
  args[1] = scheme_intern_symbol("gl-double-vector-internal-type");
  gl_double_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-float-vector.ss");
  args[1] = scheme_intern_symbol("gl-float-vector-internal-type");
  gl_float_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-uint-vector.ss");
  args[1] = scheme_intern_symbol("gl-uint-vector-internal-type");
  gl_uint_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-ushort-vector.ss");
  args[1] = scheme_intern_symbol("gl-ushort-vector-internal-type");
  gl_ushort_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-ubyte-vector.ss");
  args[1] = scheme_intern_symbol("gl-ubyte-vector-internal-type");
  gl_ubyte_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-int-vector.ss");
  args[1] = scheme_intern_symbol("gl-int-vector-internal-type");
  gl_int_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-short-vector.ss");
  args[1] = scheme_intern_symbol("gl-short-vector-internal-type");
  gl_short_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-byte-vector.ss");
  args[1] = scheme_intern_symbol("gl-byte-vector-internal-type");
  gl_byte_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  args[0] = scheme_make_string("gl-vectors/gl-boolean-vector.ss");
  args[1] = scheme_intern_symbol("gl-boolean-vector-internal-type");
  gl_boolean_vector_type = SCHEME_INT_VAL(scheme_apply(dynamic_require, 2, args));

  return;
}


#include "gl-vectors/gl-double-vector.h"
#include "gl-vectors/gl-float-vector.h"
#include "gl-vectors/gl-int-vector.h"
#include "gl-vectors/gl-short-vector.h"
#include "gl-vectors/gl-byte-vector.h"
#include "gl-vectors/gl-uint-vector.h"
#include "gl-vectors/gl-ushort-vector.h"
#include "gl-vectors/gl-ubyte-vector.h"
#include "gl-vectors/gl-boolean-vector.h"

static int arg_bool(const char *name, Scheme_Object *arg)
{
  return (SCHEME_FALSEP(arg)) ? 0 : 1;
}

static int arg_int(const char *name, Scheme_Object *arg,
		   int which, int argc, Scheme_Object **argv)
{
  if (SCHEME_INTP(arg))
    return SCHEME_INT_VAL(arg);
  else
    scheme_wrong_type(name, "exact integer", which, argc, argv);
  
  return 0;
}

static int arg_pos(const char *name, Scheme_Object *arg,
		   int which, int argc, Scheme_Object **argv)
{
  if (SCHEME_INTP(arg) && SCHEME_INT_VAL(arg) >= 0)
    return SCHEME_INT_VAL(arg);
  else
    scheme_wrong_type(name, "non-negative exact integer", which, argc, argv);
  
  return 0;
}

static double arg_real(const char *name, Scheme_Object *arg,
		       int which, int argc, Scheme_Object **argv)
{
  double x = 0.0;
  
  if (SCHEME_REALP(arg))
    x = scheme_real_to_double(arg);
  else
    scheme_wrong_type(name, "real", which, argc, argv);
  
  return x;
}
static void *arg_void_data(const char* name, GLsizei length,
			   const Scheme_Object *data, GLenum type,
			   int which, int argc, Scheme_Object** argv)
{
  Scheme_Type t;
  t = SCHEME_TYPE(data);
  switch (type)
  {
  case GL_UNSIGNED_BYTE:
  case GL_BITMAP:
#ifdef GL_VERSION_1_2
  case GL_UNSIGNED_BYTE_3_3_2:
  case GL_UNSIGNED_BYTE_2_3_3_REV:
#endif
    if (t != gl_ubyte_vector_type)
      scheme_wrong_type(name, "gl-ubyte-vector", which, argc, argv);
    break;
  case GL_BYTE:
    if (t != gl_byte_vector_type)
      scheme_wrong_type(name, "gl-byte-vector", which, argc, argv);
    break;
  case GL_UNSIGNED_SHORT:
#ifdef GL_VERSION_1_2
  case GL_UNSIGNED_SHORT_5_6_5:
  case GL_UNSIGNED_SHORT_5_6_5_REV:
  case GL_UNSIGNED_SHORT_4_4_4_4:
  case GL_UNSIGNED_SHORT_4_4_4_4_REV:
  case GL_UNSIGNED_SHORT_5_5_5_1:
  case GL_UNSIGNED_SHORT_1_5_5_5_REV:
#endif
      if (t != gl_ushort_vector_type)
      scheme_wrong_type(name, "gl-ushort-vector", which, argc, argv);
    break;
  case GL_SHORT:
    if (t != gl_short_vector_type)
      scheme_wrong_type(name, "gl-short-vector", which, argc, argv);
    break;
  case GL_UNSIGNED_INT:
#ifdef GL_VERSION_1_2
  case GL_UNSIGNED_INT_8_8_8_8:
  case GL_UNSIGNED_INT_8_8_8_8_REV:
  case GL_UNSIGNED_INT_10_10_10_2:
  case GL_UNSIGNED_INT_2_10_10_10_REV:
#endif
      if (t != gl_uint_vector_type)
      scheme_wrong_type(name, "gl-uint-vector", which, argc, argv);
    break;
  case GL_INT:
    if (t != gl_int_vector_type)
      scheme_wrong_type(name, "gl-int-vector", which, argc, argv);
    break;
  default:
    if (t != gl_float_vector_type)
      scheme_wrong_type(name, "gl-float-vector", which, argc, argv);
  }
  if (((gl_byte_vector*)data)->length < length)
  {
    char err_str[128];
    sprintf(err_str, "gl-vector of length at least %d", length);
    scheme_wrong_type(name, err_str, which, argc, argv);
  }
  return (void*)(((gl_byte_vector*)data)->els);
}

#define arg_GLboolean(idx)	\
	  ((GLboolean)   arg_bool ((char *) p, v[idx]))
#define arg_GLbyte(idx)		\
	  ((GLbyte)      arg_int ((char *) p, v[idx], idx, c, v))
#define arg_GLubyte(idx)	\
	  ((GLubyte)     arg_pos ((char *) p, v[idx], idx, c, v))
#define arg_GLshort(idx)	\
	  ((GLshort)     arg_int ((char *) p, v[idx], idx, c, v))
#define arg_GLushort(idx)	\
	  ((GLushort)    arg_pos ((char *) p, v[idx], idx, c, v))
#define arg_GLint(idx)		\
	  ((GLint)       arg_int ((char *) p, v[idx], idx, c, v))
#define arg_GLuint(idx)		\
	  ((GLuint)      arg_pos ((char *) p, v[idx], idx, c, v))
#define arg_GLsizei(idx)	\
	  ((GLenum)      arg_pos ((char *) p, v[idx], idx, c, v))
#define arg_GLenum(idx)		\
	  ((GLenum)      arg_pos ((char *) p, v[idx], idx, c, v))
#define arg_GLbitfield(idx)	\
	  ((GLbitfield)  arg_pos ((char *) p, v[idx], idx, c, v))
#define arg_GLfloat(idx)	\
	  ((GLfloat)     arg_real((char *) p, v[idx], idx, c, v))
#define arg_GLclampf(idx)	\
	  ((GLclampf)    arg_real((char *) p, v[idx], idx, c, v))
#define arg_GLdouble(idx)	\
	  ((GLdouble)    arg_real((char *) p, v[idx], idx, c, v))
#define arg_GLclampd(idx)	\
	  ((GLclampd)    arg_real((char *) p, v[idx], idx, c, v))

#define arg_GLvoidv(idx, len, type)	\
	  (arg_void_data ((char *) p, len, v[idx], type, idx, c, v))
