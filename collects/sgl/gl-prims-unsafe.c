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

#include "gl-prims.h"

#define arg_GLunsafev(idx)		\
	  ((GLvoid *)    arg_unsafev_data((char *) p, v[idx], idx, c, v))

static GLvoid *arg_unsafev_data(const char *name, Scheme_Object *arg,
				int which, int argc, Scheme_Object **argv)
{
  Scheme_Type t = SCHEME_TYPE(arg);
  if (t == gl_double_vector_type ||
      t == gl_float_vector_type ||
      t == gl_uint_vector_type ||
      t == gl_ushort_vector_type ||
      t == gl_ubyte_vector_type ||
      t == gl_int_vector_type ||
      t == gl_short_vector_type ||
      t == gl_byte_vector_type ||
      t == gl_boolean_vector_type)
    return ((gl_byte_vector*)arg)-> els;
  else
    scheme_wrong_type(name, "gl-vector", which, argc, argv);
}

/*---------------------------------------------------------------------------*/
/* 2.8. Vertex Arrays							     */

static Scheme_Object *scm_EdgeFlagPointer(void *p, int c, Scheme_Object **v)
{
	glEdgeFlagPointer(arg_GLsizei(0),
			  arg_GLunsafev(1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoordPointer(void *p, int c, Scheme_Object **v)
{
	glTexCoordPointer(arg_GLsizei(0),
			  arg_GLenum (1),
			  arg_GLsizei(2),
			  arg_GLunsafev(3));
	return scheme_void;
}

static Scheme_Object *scm_ColorPointer(void *p, int c, Scheme_Object **v)
{
	glColorPointer(arg_GLsizei(0),
		       arg_GLenum (1),
		       arg_GLsizei(2),
		       arg_GLunsafev(3));
	return scheme_void;
}

static Scheme_Object *scm_IndexPointer(void *p, int c, Scheme_Object **v)
{
	glIndexPointer(arg_GLenum (0),
		       arg_GLsizei(1),
		       arg_GLunsafev(2));
	return scheme_void;
}

static Scheme_Object *scm_NormalPointer(void *p, int c, Scheme_Object **v)
{
	glNormalPointer(arg_GLenum (0),
			arg_GLsizei(1),
			arg_GLunsafev(2));
	return scheme_void;
}

static Scheme_Object *scm_VertexPointer(void *p, int c, Scheme_Object **v)
{
	glVertexPointer(arg_GLsizei(0),
			arg_GLenum (1),
			arg_GLsizei(2),
			arg_GLunsafev(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_ArrayElement(void *p, int c, Scheme_Object **v)
{
	glArrayElement(arg_GLint(0));
	return scheme_void;
}

static Scheme_Object *scm_DrawArrays(void *p, int c, Scheme_Object **v)
{
	glDrawArrays(arg_GLenum (0),
		     arg_GLint  (1),
		     arg_GLsizei(2));
	return scheme_void;
}

static Scheme_Object *scm_DrawElements(void *p, int c, Scheme_Object **v)
{
  GLsizei count = arg_GLsizei(1);
  GLenum t = arg_GLenum(2);

  glDrawElements(arg_GLenum (0),
		 count,
		 t,
		 arg_GLvoidv(3, count, t));
  return scheme_void;
}

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_DrawRangeElements(void *p, int c, Scheme_Object **v)
{
  GLsizei count = arg_GLsizei(3);
  GLenum t = arg_GLenum(4);

  glDrawRangeElements(arg_GLenum (0),
		      arg_GLuint (1),
		      arg_GLuint (2),
		      count,
		      t,
		      arg_GLvoidv(5, count, t));
  return scheme_void;
}

#endif /* GL_VERSION_1_2 */

static Scheme_Object *scm_InterleavedArrays(void *p, int c, Scheme_Object **v)
{
	glInterleavedArrays(arg_GLenum (0),
			    arg_GLsizei(1),
			    arg_GLunsafev(2));
	return scheme_void;
}

/*...........................................................................*/
/* 3.8.3. Compressed Texture Images					     */

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_CompressedTexImage3D(void *p,
					       int c, Scheme_Object **v)
{
  glCompressedTexImage3D(arg_GLenum (0),
			 arg_GLint  (1),
			 arg_GLenum (2),
			 arg_GLsizei(3),
			 arg_GLsizei(4),
			 arg_GLsizei(5),
			 arg_GLint  (6),
			 arg_GLsizei(7),
			 arg_GLunsafev(8));
  return scheme_void;
}

static Scheme_Object *scm_CompressedTexImage2D(void *p,
					       int c, Scheme_Object **v)
{
  glCompressedTexImage2D(arg_GLenum (0),
			 arg_GLint  (1),
			 arg_GLenum (2),
			 arg_GLsizei(3),
			 arg_GLsizei(4),
			 arg_GLint  (5),
			 arg_GLsizei(6),
			 arg_GLunsafev(7));
  return scheme_void;
}

static Scheme_Object *scm_CompressedTexImage1D(void *p,
					       int c, Scheme_Object **v)
{
  glCompressedTexImage1D(arg_GLenum (0),
			 arg_GLint  (1),
			 arg_GLenum (2),
			 arg_GLsizei(3),
			 arg_GLint  (4),
			 arg_GLsizei(5),
			 arg_GLunsafev(6));
  return scheme_void;
}

static Scheme_Object *scm_CompressedTexSubImage3D(void *p,
						  int c, Scheme_Object **v)
{
  glCompressedTexSubImage3D(arg_GLenum (0),
			    arg_GLint  (1),
			    arg_GLint  (2),
			    arg_GLint  (3),
			    arg_GLint  (4),
			    arg_GLsizei(5),
			    arg_GLsizei(6),
			    arg_GLsizei(7),
			    arg_GLenum (8),
			    arg_GLsizei(9),
			    arg_GLunsafev(10));
  return scheme_void;
}

static Scheme_Object *scm_CompressedTexSubImage2D(void *p,
						  int c, Scheme_Object **v)
{
  glCompressedTexSubImage2D(arg_GLenum (0),
			    arg_GLint  (1),
			    arg_GLint  (2),
			    arg_GLint  (3),
			    arg_GLsizei(4),
			    arg_GLsizei(5),
			    arg_GLenum (6),
			    arg_GLsizei(7),
			    arg_GLunsafev(8));
	return scheme_void;
}

static Scheme_Object *scm_CompressedTexSubImage1D(void *p,
						  int c, Scheme_Object **v)
{
  glCompressedTexSubImage1D(arg_GLenum (0),
			    arg_GLint  (1),
			    arg_GLint  (2),
			    arg_GLsizei(3),
			    arg_GLenum (4),
			    arg_GLsizei(5),
			    arg_GLunsafev(6));
  return scheme_void;
}

#endif /* GL_VERSION_1_3 */

/*---------------------------------------------------------------------------*/
/* 6.1. Querying GL State						     */

/*...........................................................................*/
/* 6.1.1 Simple Queries							     */

static Scheme_Object *scm_GetBooleanv(void *p, int c, Scheme_Object **v)
{
  GLenum value = arg_GLenum(0);
  glGetBooleanv(value, arg_GLunsafev(1));
  return scheme_void;
}

static Scheme_Object *scm_GetIntegerv(void *p, int c, Scheme_Object **v)
{
  GLenum value = arg_GLenum(0);
  glGetIntegerv(value, arg_GLunsafev(1));
  return scheme_void;
}

static Scheme_Object *scm_GetFloatv(void *p, int c, Scheme_Object **v)
{
  GLenum value = arg_GLenum(0);
  glGetFloatv(value, arg_GLunsafev(1));
  return scheme_void;
}

static Scheme_Object *scm_GetDoublev(void *p, int c, Scheme_Object **v)
{
  GLenum value = arg_GLenum(0);
  glGetDoublev(value, arg_GLunsafev(1));
  return scheme_void;
}

/*...........................................................................*/
/* 6.1.3 Enumerated Queries						     */

static Scheme_Object *scm_GetTexEnviv(void *p, int c, Scheme_Object **v)
{
  glGetTexEnviv(arg_GLenum (0),
		arg_GLenum(1),
		arg_GLunsafev(2));
  return scheme_void;
}

static Scheme_Object *scm_GetTexEnvfv(void *p, int c, Scheme_Object **v)
{
  glGetTexEnvfv(arg_GLenum (0),
		arg_GLenum(1),
		arg_GLunsafev(2));
  return scheme_void;
}

static Scheme_Object *scm_GetTexGeniv(void *p, int c, Scheme_Object **v)
{
  GLenum n = arg_GLenum(1);

  glGetTexGeniv(arg_GLenum (0),
		n,
		arg_GLunsafev(2));
  return scheme_void;
}

static Scheme_Object *scm_GetTexGenfv(void *p, int c, Scheme_Object **v)
{
  GLenum n = arg_GLenum(1);
  
  glGetTexGenfv(arg_GLenum (0),
		n,
		arg_GLunsafev(2));
  return scheme_void;
}

static Scheme_Object *scm_GetTexParameteriv(void *p, int c, Scheme_Object **v)
{
  glGetTexParameteriv(arg_GLenum (0),
		      arg_GLenum(1),
		      arg_GLunsafev(2));
  return scheme_void;
}

static Scheme_Object *scm_GetTexParameterfv(void *p, int c, Scheme_Object **v)
{
  glGetTexParameterfv(arg_GLenum (0),
		      arg_GLenum(1),
		      arg_GLunsafev(2));
  return scheme_void;
}

static Scheme_Object *scm_GetTexLevelParameteriv(void *p,
						 int c, Scheme_Object **v)
{
	glGetTexLevelParameteriv(arg_GLenum (0),
				 arg_GLint  (1),
				 arg_GLenum (2),
				 arg_GLunsafev(3));
	return scheme_void;
}

static Scheme_Object *scm_GetTexLevelParameterfv(void *p,
						 int c, Scheme_Object **v)
{
	glGetTexLevelParameterfv(arg_GLenum (0),
				 arg_GLint  (1),
				 arg_GLenum (2),
				 arg_GLunsafev(3));
	return scheme_void;
}

static Scheme_Object *scm_GetPixelMapuiv(void *p, int c, Scheme_Object **v)
{
	glGetPixelMapuiv(arg_GLenum (0),
			 arg_GLunsafev(1));
	return scheme_void;
}

static Scheme_Object *scm_GetPixelMapusv(void *p, int c, Scheme_Object **v)
{
	glGetPixelMapusv(arg_GLenum (0),
			 arg_GLunsafev(1));
	return scheme_void;
}

static Scheme_Object *scm_GetPixelMapfv(void *p, int c, Scheme_Object **v)
{
	glGetPixelMapfv(arg_GLenum (0),
			arg_GLunsafev(1));
	return scheme_void;
}

static Scheme_Object *scm_GetMapiv(void *p, int c, Scheme_Object **v)
{
	glGetMapiv(arg_GLenum (0),
		   arg_GLenum (1),
		   arg_GLunsafev(2));
	return scheme_void;
}

static Scheme_Object *scm_GetMapfv(void *p, int c, Scheme_Object **v)
{
	glGetMapfv(arg_GLenum (0),
		   arg_GLenum (1),
		   arg_GLunsafev(2));
	return scheme_void;
}

static Scheme_Object *scm_GetMapdv(void *p, int c, Scheme_Object **v)
{
	glGetMapdv(arg_GLenum (0),
		   arg_GLenum (1),
		   arg_GLunsafev(2));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.4. Texture Queries						     */

static Scheme_Object *scm_GetTexImage(void *p, int c, Scheme_Object **v)
{
	glGetTexImage(arg_GLenum (0),
		      arg_GLint  (1),
		      arg_GLenum (2),
		      arg_GLenum (3),
		      arg_GLunsafev(4));
	return scheme_void;
}

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_GetCompressedTexImage(void *p,
						int c, Scheme_Object **v)
{
	glGetCompressedTexImage(arg_GLenum (0),
				arg_GLint  (1),
				arg_GLunsafev(2));
	return scheme_void;
}

#endif /* GL_VERSION_1_3 */

/*...........................................................................*/
/* 6.1.7. Color Table Query						     */

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_GetColorTable(void *p, int c, Scheme_Object **v)
{
	glGetColorTable(arg_GLenum (0),
			arg_GLenum (1),
			arg_GLenum (2),
			arg_GLunsafev(3));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.8. Convolution Filter						     */

static Scheme_Object *scm_GetConvolutionFilter(void *p,
					       int c, Scheme_Object **v)
{
	glGetConvolutionFilter(arg_GLenum (0),
			       arg_GLenum (1),
			       arg_GLenum (2),
			       arg_GLunsafev(3));
	return scheme_void;
}

static Scheme_Object *scm_GetSeparableFilter(void *p, int c, Scheme_Object **v)
{
	glGetSeparableFilter(arg_GLenum (0),
			     arg_GLenum (1),
			     arg_GLenum (2),
			     arg_GLunsafev(3),
			     arg_GLunsafev(4),
			     arg_GLunsafev(5));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.9. Histogram Query						     */

static Scheme_Object *scm_GetHistogram(void *p, int c, Scheme_Object **v)
{
	glGetHistogram(arg_GLenum   (0),
		       arg_GLboolean(1),
		       arg_GLenum   (2),
		       arg_GLenum   (3),
		       arg_GLunsafev  (4));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.10. Minmax Query							     */

static Scheme_Object *scm_GetMinmax(void *p, int c, Scheme_Object **v)
{
	glGetMinmax(arg_GLenum   (0),
		    arg_GLboolean(1),
		    arg_GLenum   (2),
		    arg_GLenum   (3),
		    arg_GLunsafev  (4));
	return scheme_void;
}


#endif /* GL_VERSION_1_2 */

/*...........................................................................*/
/* 6.1.11. Pointer and String Queries					     */

static Scheme_Object *scm_GetPointerv(void *p, int c, Scheme_Object **v)
{
	return scheme_void;	/* Run away!! */
}





static const struct scm_prim scm_prim[] = {

	{ "glEdgeFlagPointer",		scm_EdgeFlagPointer,		2, 2 },
	{ "glTexCoordPointer",		scm_TexCoordPointer,		4, 4 },
	{ "glColorPointer",		scm_ColorPointer,		4, 4 },
	{ "glIndexPointer",		scm_IndexPointer,		3, 3 },
	{ "glNormalPointer",		scm_NormalPointer,		3, 3 },
	{ "glVertexPointer",		scm_VertexPointer,		4, 4 },
	{ "glArrayElement",		scm_ArrayElement,		1, 1 },
	{ "glDrawArrays",		scm_DrawArrays,			3, 3 },
	{ "glDrawElements",		scm_DrawElements,		4, 4 },
	{ "glInterleavedArrays",	scm_InterleavedArrays,		3, 3 },
	{ "glGetBooleanv",		scm_GetBooleanv,		2, 2 },
	{ "glGetDoublev",		scm_GetDoublev,			2, 2 },
	{ "glGetFloatv",		scm_GetFloatv,			2, 2 },
	{ "glGetIntegerv",		scm_GetIntegerv,		2, 2 },
	{ "glGetMapdv",			scm_GetMapdv,			3, 3 },
	{ "glGetMapfv",			scm_GetMapfv,			3, 3 },
	{ "glGetMapiv",			scm_GetMapiv,			3, 3 },
	{ "glGetPixelMapfv",		scm_GetPixelMapfv,		2, 2 },
	{ "glGetPixelMapuiv",		scm_GetPixelMapuiv,		2, 2 },
	{ "glGetPixelMapusv",		scm_GetPixelMapusv,		2, 2 },
	{ "glGetPointerv",		scm_GetPointerv,		2, 2 },
	{ "glGetTexEnvfv",		scm_GetTexEnvfv,		3, 3 },
	{ "glGetTexEnviv",		scm_GetTexEnviv,		3, 3 },
	{ "glGetTexGenfv",		scm_GetTexGenfv,		3, 3 },
	{ "glGetTexGeniv",		scm_GetTexGeniv,		3, 3 },
	{ "glGetTexImage",		scm_GetTexImage,		5, 5 },
	{ "glGetTexLevelParameterfv",	scm_GetTexLevelParameterfv,	4, 4 },
	{ "glGetTexLevelParameteriv",	scm_GetTexLevelParameteriv,	4, 4 },
	{ "glGetTexParameterfv",	scm_GetTexParameterfv,		3, 3 },
	{ "glGetTexParameteriv",	scm_GetTexParameteriv,		3, 3 },

#ifdef GL_VERSION_1_2
	{ "glDrawRangeElements",	scm_DrawRangeElements,		6, 6 },
	{ "glGetColorTable",		scm_GetColorTable,		4, 4 },
	{ "glGetConvolutionFilter",	scm_GetConvolutionFilter,	4, 4 },
	{ "glGetHistogram",		scm_GetHistogram,		5, 5 },
	{ "glGetMinmax",		scm_GetMinmax,			5, 5 },
	{ "glGetSeparableFilter",	scm_GetSeparableFilter,		6, 6 },
#endif

#ifdef GL_VERSION_1_3
	{ "glCompressedTexImage1D",	scm_CompressedTexImage1D,	7, 7 },
	{ "glCompressedTexImage2D",	scm_CompressedTexImage2D,	8, 8 },
	{ "glCompressedTexImage3D",	scm_CompressedTexImage3D,	9, 9 },
	{ "glCompressedTexSubImage1D",	scm_CompressedTexSubImage1D,	7, 7 },
	{ "glCompressedTexSubImage2D",	scm_CompressedTexSubImage2D,	9, 9 },
	{ "glCompressedTexSubImage3D",	scm_CompressedTexSubImage3D,	11,11},
	{ "glGetCompressedTexImage",	scm_GetCompressedTexImage,	3, 3 }
#endif
};

/*---------------------------------------------------------------------------*/
/* MzScheme extension interface						     */

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *mod_env;

  mod_env = scheme_primitive_module(scheme_intern_symbol("gl-prims-unsafe"),
				    env);
  scheme_load_prim(mod_env, scm_prim, sizeof(scm_prim));
  scheme_finish_primitive_module(mod_env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  scheme_set_types();
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name(void)
{
  return scheme_intern_symbol("gl-prims-unsafe");
}

/*---------------------------------------------------------------------------*/
