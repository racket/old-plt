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

/*****************************************************************************/
/* 2. OpenGL Operation							     */

/*===========================================================================*/
/* 2.5. GL Errors							     */

static Scheme_Object *scm_GetError(void *data,
				   int c, Scheme_Object **v)
{
	return scheme_make_integer((int) glGetError());
}

/*===========================================================================*/
/* 2.6. Begin / End Paradigm						     */

/*---------------------------------------------------------------------------*/
/* 2.6.1. Begin and End Objects						     */

static Scheme_Object *scm_Begin(void *p,
				int c, Scheme_Object **v)
{
	glBegin(arg_GLenum(0));
	return scheme_void;
}

static Scheme_Object *scm_End(void *p,
			      int c, Scheme_Object **v)
{
	glEnd();
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.6.2. Polygon Edges							     */

static Scheme_Object *scm_EdgeFlag(void *p, int c, Scheme_Object **v)
{
	glEdgeFlag(arg_GLboolean(0));
	return scheme_void;
}

static Scheme_Object *scm_EdgeFlagv(void *p, int c, Scheme_Object **v)
{
	glEdgeFlagv(arg_GLbooleanv(0, 1));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.7. Vertex Specification						     */

static Scheme_Object *scm_Vertex2s(void *p, int c, Scheme_Object **v)
{
	glVertex2s(arg_GLshort(0),
		   arg_GLshort(1));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3s(void *p, int c, Scheme_Object **v)
{
	glVertex3s(arg_GLshort(0),
		   arg_GLshort(1),
		   arg_GLshort(2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4s(void *p, int c, Scheme_Object **v)
{
	glVertex4s(arg_GLshort(0),
		   arg_GLshort(1),
		   arg_GLshort(2),
		   arg_GLshort(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2i(void *p, int c, Scheme_Object **v)
{
	glVertex2i(arg_GLint(0),
		   arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3i(void *p, int c, Scheme_Object **v)
{
	glVertex3i(arg_GLint(0),
		   arg_GLint(1),
		   arg_GLint(2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4i(void *p, int c, Scheme_Object **v)
{
	glVertex4i(arg_GLint(0),
		   arg_GLint(1),
		   arg_GLint(2),
		   arg_GLint(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2f(void *p, int c, Scheme_Object **v)
{
	glVertex2f(arg_GLfloat(0),
		   arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3f(void *p, int c, Scheme_Object **v)
{
	glVertex3f(arg_GLfloat(0),
		   arg_GLfloat(1),
		   arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4f(void *p, int c, Scheme_Object **v)
{
	glVertex4f(arg_GLfloat(0),
		   arg_GLfloat(1),
		   arg_GLfloat(2),
		   arg_GLfloat(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2d(void *p, int c, Scheme_Object **v)
{
	glVertex2d(arg_GLdouble(0),
		   arg_GLdouble(1));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3d(void *p, int c, Scheme_Object **v)
{
	glVertex3d(arg_GLdouble(0),
		   arg_GLdouble(1),
		   arg_GLdouble(2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4d(void *p, int c, Scheme_Object **v)
{
	glVertex4d(arg_GLdouble(0),
		   arg_GLdouble(1),
		   arg_GLdouble(2),
		   arg_GLdouble(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2sv(void *p, int c, Scheme_Object **v)
{
	glVertex2sv(arg_GLshortv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3sv(void *p, int c, Scheme_Object **v)
{
	glVertex3sv(arg_GLshortv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4sv(void *p, int c, Scheme_Object **v)
{
	glVertex4sv(arg_GLshortv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2iv(void *p, int c, Scheme_Object **v)
{
	glVertex2iv(arg_GLintv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3iv(void *p, int c, Scheme_Object **v)
{
	glVertex3iv(arg_GLintv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4iv(void *p, int c, Scheme_Object **v)
{
	glVertex4iv(arg_GLintv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2fv(void *p, int c, Scheme_Object **v)
{
	glVertex2fv(arg_GLfloatv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3fv(void *p, int c, Scheme_Object **v)
{
	glVertex3fv(arg_GLfloatv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4fv(void *p, int c, Scheme_Object **v)
{
	glVertex4fv(arg_GLfloatv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Vertex2dv(void *p, int c, Scheme_Object **v)
{
	glVertex2dv(arg_GLdoublev(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_Vertex3dv(void *p, int c, Scheme_Object **v)
{
	glVertex3dv(arg_GLdoublev(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Vertex4dv(void *p, int c, Scheme_Object **v)
{
	glVertex4dv(arg_GLdoublev(0, 4));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/

static Scheme_Object *scm_TexCoord1s(void *p, int c, Scheme_Object **v)
{
	glTexCoord1s(arg_GLshort(0));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2s(void *p, int c, Scheme_Object **v)
{
	glTexCoord2s(arg_GLshort(0),
		     arg_GLshort(1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3s(void *p, int c, Scheme_Object **v)
{
	glTexCoord3s(arg_GLshort(0),
		     arg_GLshort(1),
		     arg_GLshort(2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4s(void *p, int c, Scheme_Object **v)
{
	glTexCoord4s(arg_GLshort(0),
		     arg_GLshort(1),
		     arg_GLshort(2),
		     arg_GLshort(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1i(void *p, int c, Scheme_Object **v)
{
	glTexCoord1i(arg_GLint(0));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2i(void *p, int c, Scheme_Object **v)
{
	glTexCoord2i(arg_GLint(0),
		     arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3i(void *p, int c, Scheme_Object **v)
{
	glTexCoord3i(arg_GLint(0),
		     arg_GLint(1),
		     arg_GLint(2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4i(void *p, int c, Scheme_Object **v)
{
	glTexCoord4i(arg_GLint(0),
		     arg_GLint(1),
		     arg_GLint(2),
		     arg_GLint(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1f(void *p, int c, Scheme_Object **v)
{
	glTexCoord1f(arg_GLfloat(0));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2f(void *p, int c, Scheme_Object **v)
{
	glTexCoord2f(arg_GLfloat(0),
		     arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3f(void *p, int c, Scheme_Object **v)
{
	glTexCoord3f(arg_GLfloat(0),
		     arg_GLfloat(1),
		     arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4f(void *p, int c, Scheme_Object **v)
{
	glTexCoord4f(arg_GLfloat(0),
		     arg_GLfloat(1),
		     arg_GLfloat(2),
		     arg_GLfloat(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1d(void *p, int c, Scheme_Object **v)
{
	glTexCoord1d(arg_GLdouble(0));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2d(void *p, int c, Scheme_Object **v)
{
	glTexCoord2d(arg_GLdouble(0),
		     arg_GLdouble(1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3d(void *p, int c, Scheme_Object **v)
{
	glTexCoord3d(arg_GLdouble(0),
		     arg_GLdouble(1),
		     arg_GLdouble(2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4d(void *p, int c, Scheme_Object **v)
{
	glTexCoord4d(arg_GLdouble(0),
		     arg_GLdouble(1),
		     arg_GLdouble(2),
		     arg_GLdouble(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1sv(void *p, int c, Scheme_Object **v)
{
	glTexCoord1sv(arg_GLshortv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2sv(void *p, int c, Scheme_Object **v)
{
	glTexCoord2sv(arg_GLshortv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3sv(void *p, int c, Scheme_Object **v)
{
	glTexCoord3sv(arg_GLshortv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4sv(void *p, int c, Scheme_Object **v)
{
	glTexCoord4sv(arg_GLshortv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1iv(void *p, int c, Scheme_Object **v)
{
	glTexCoord1iv(arg_GLintv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2iv(void *p, int c, Scheme_Object **v)
{
	glTexCoord2iv(arg_GLintv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3iv(void *p, int c, Scheme_Object **v)
{
	glTexCoord3iv(arg_GLintv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4iv(void *p, int c, Scheme_Object **v)
{
	glTexCoord4iv(arg_GLintv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1fv(void *p, int c, Scheme_Object **v)
{
	glTexCoord1fv(arg_GLfloatv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2fv(void *p, int c, Scheme_Object **v)
{
	glTexCoord2fv(arg_GLfloatv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3fv(void *p, int c, Scheme_Object **v)
{
	glTexCoord3fv(arg_GLfloatv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4fv(void *p, int c, Scheme_Object **v)
{
	glTexCoord4fv(arg_GLfloatv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexCoord1dv(void *p, int c, Scheme_Object **v)
{
	glTexCoord1dv(arg_GLdoublev(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord2dv(void *p, int c, Scheme_Object **v)
{
	glTexCoord2dv(arg_GLdoublev(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord3dv(void *p, int c, Scheme_Object **v)
{
	glTexCoord3dv(arg_GLdoublev(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_TexCoord4dv(void *p, int c, Scheme_Object **v)
{
	glTexCoord4dv(arg_GLdoublev(0, 4));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_MultiTexCoord1s(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1s(arg_GLenum (0),
			  arg_GLshort(1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2s(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2s(arg_GLenum (0),
			  arg_GLshort(1),
			  arg_GLshort(2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3s(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3s(arg_GLenum (0),
			  arg_GLshort(1),
			  arg_GLshort(2),
			  arg_GLshort(3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4s(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4s(arg_GLenum (0),
			  arg_GLshort(1),
			  arg_GLshort(2),
			  arg_GLshort(3),
			  arg_GLshort(4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1i(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1i(arg_GLenum(0),
			  arg_GLint (1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2i(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2i(arg_GLenum(0),
			  arg_GLint (1),
			  arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3i(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3i(arg_GLenum(0),
			  arg_GLint (1),
			  arg_GLint (2),
			  arg_GLint (3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4i(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4i(arg_GLenum(0),
			  arg_GLint (1),
			  arg_GLint (2),
			  arg_GLint (3),
			  arg_GLint (4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1f(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1f(arg_GLenum (0),
			  arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2f(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2f(arg_GLenum (0),
			  arg_GLfloat(1),
			  arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3f(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3f(arg_GLenum (0),
			  arg_GLfloat(1),
			  arg_GLfloat(2),
			  arg_GLfloat(3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4f(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4f(arg_GLenum (0),
			  arg_GLfloat(1),
			  arg_GLfloat(2),
			  arg_GLfloat(3),
			  arg_GLfloat(4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1d(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1d(arg_GLenum  (0),
			  arg_GLdouble(1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2d(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2d(arg_GLenum  (0),
			  arg_GLdouble(1),
			  arg_GLdouble(2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3d(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3d(arg_GLenum  (0),
			  arg_GLdouble(1),
			  arg_GLdouble(2),
			  arg_GLdouble(3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4d(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4d(arg_GLenum  (0),
			  arg_GLdouble(1),
			  arg_GLdouble(2),
			  arg_GLdouble(3),
			  arg_GLdouble(4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1sv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1sv(arg_GLenum(0),
			   arg_GLshortv(1, 1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2sv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2sv(arg_GLenum(0),
			   arg_GLshortv(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3sv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3sv(arg_GLenum(0),
			   arg_GLshortv(1, 3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4sv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4sv(arg_GLenum(0),
			   arg_GLshortv(1, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1iv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1iv(arg_GLenum(0),
			   arg_GLintv(1, 1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2iv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2iv(arg_GLenum(0),
			   arg_GLintv(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3iv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3iv(arg_GLenum(0),
			   arg_GLintv(1, 3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4iv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4iv(arg_GLenum(0),
			   arg_GLintv(1, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1fv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1fv(arg_GLenum(0),
			   arg_GLfloatv(1, 1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2fv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2fv(arg_GLenum(0),
			   arg_GLfloatv(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3fv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3fv(arg_GLenum(0),
			   arg_GLfloatv(1, 3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4fv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4fv(arg_GLenum(0),
			   arg_GLfloatv(1, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MultiTexCoord1dv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord1dv(arg_GLenum(0),
			   arg_GLdoublev(1, 1));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord2dv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord2dv(arg_GLenum(0),
			   arg_GLdoublev(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord3dv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord3dv(arg_GLenum(0),
			   arg_GLdoublev(1, 3));
	return scheme_void;
}

static Scheme_Object *scm_MultiTexCoord4dv(void *p, int c, Scheme_Object **v)
{
	glMultiTexCoord4dv(arg_GLenum(0),
			   arg_GLdoublev(1, 4));
	return scheme_void;
}

#endif /* GL_VERSION_1_3 */

/*---------------------------------------------------------------------------*/

static Scheme_Object *scm_Normal3b(void *p, int c, Scheme_Object **v)
{
	glNormal3b(arg_GLbyte(0),
		   arg_GLbyte(1),
		   arg_GLbyte(2));
	return scheme_void;
}

static Scheme_Object *scm_Normal3s(void *p, int c, Scheme_Object **v)
{
	glNormal3s(arg_GLshort(0),
		   arg_GLshort(1),
		   arg_GLshort(2));
	return scheme_void;
}

static Scheme_Object *scm_Normal3i(void *p, int c, Scheme_Object **v)
{
	glNormal3i(arg_GLint(0),
		   arg_GLint(1),
		   arg_GLint(2));
	return scheme_void;
}

static Scheme_Object *scm_Normal3f(void *p, int c, Scheme_Object **v)
{
	glNormal3f(arg_GLfloat(0),
		   arg_GLfloat(1),
		   arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Normal3d(void *p, int c, Scheme_Object **v)
{
	glNormal3d(arg_GLdouble(0),
		   arg_GLdouble(1),
		   arg_GLdouble(2));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Normal3bv(void *p, int c, Scheme_Object **v)
{
	glNormal3bv(arg_GLbytev(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Normal3sv(void *p, int c, Scheme_Object **v)
{
	glNormal3sv(arg_GLshortv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Normal3iv(void *p, int c, Scheme_Object **v)
{
	glNormal3iv(arg_GLintv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Normal3fv(void *p, int c, Scheme_Object **v)
{
	glNormal3fv(arg_GLfloatv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Normal3dv(void *p, int c, Scheme_Object **v)
{
	glNormal3dv(arg_GLdoublev(0, 3));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/

static Scheme_Object *scm_Color3ub(void *p, int c, Scheme_Object **v)
{
	glColor3ub(arg_GLubyte(0),
		   arg_GLubyte(1),
		   arg_GLubyte(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3b(void *p, int c, Scheme_Object **v)
{
	glColor3b(arg_GLbyte(0),
		  arg_GLbyte(1),
		  arg_GLbyte(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3us(void *p, int c, Scheme_Object **v)
{
	glColor3us(arg_GLushort(0),
		   arg_GLushort(1),
		   arg_GLushort(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3s(void *p, int c, Scheme_Object **v)
{
	glColor3s(arg_GLshort(0),
		  arg_GLshort(1),
		  arg_GLshort(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3ui(void *p, int c, Scheme_Object **v)
{
	glColor3ui(arg_GLuint(0),
		   arg_GLuint(1),
		   arg_GLuint(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3i(void *p, int c, Scheme_Object **v)
{
	glColor3i(arg_GLint(0),
		  arg_GLint(1),
		  arg_GLint(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3f(void *p, int c, Scheme_Object **v)
{
	glColor3f(arg_GLfloat(0),
		  arg_GLfloat(1),
		  arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Color3d(void *p, int c, Scheme_Object **v)
{
	glColor3d(arg_GLdouble(0),
		  arg_GLdouble(1),
		  arg_GLdouble(2));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Color4ub(void *p, int c, Scheme_Object **v)
{
	glColor4ub(arg_GLubyte(0),
		   arg_GLubyte(1),
		   arg_GLubyte(2),
		   arg_GLubyte(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4b(void *p, int c, Scheme_Object **v)
{
	glColor4b(arg_GLbyte(0),
		  arg_GLbyte(1),
		  arg_GLbyte(2),
		  arg_GLbyte(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4us(void *p, int c, Scheme_Object **v)
{
	glColor4us(arg_GLushort(0),
		   arg_GLushort(1),
		   arg_GLushort(2),
		   arg_GLushort(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4s(void *p, int c, Scheme_Object **v)
{
	glColor4s(arg_GLshort(0),
		  arg_GLshort(1),
		  arg_GLshort(2),
		  arg_GLshort(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4ui(void *p, int c, Scheme_Object **v)
{
	glColor4ui(arg_GLuint(0),
		   arg_GLuint(1),
		   arg_GLuint(2),
		   arg_GLuint(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4i(void *p, int c, Scheme_Object **v)
{
	glColor4i(arg_GLint(0),
		  arg_GLint(1),
		  arg_GLint(2),
		  arg_GLint(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4f(void *p, int c, Scheme_Object **v)
{
	glColor4f(arg_GLfloat(0),
		  arg_GLfloat(1),
		  arg_GLfloat(2),
		  arg_GLfloat(3));
	return scheme_void;
}

static Scheme_Object *scm_Color4d(void *p, int c, Scheme_Object **v)
{
	glColor4d(arg_GLdouble(0),
		  arg_GLdouble(1),
		  arg_GLdouble(2),
		  arg_GLdouble(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Color3ubv(void *p, int c, Scheme_Object **v)
{
	glColor3ubv(arg_GLubytev(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3bv(void *p, int c, Scheme_Object **v)
{
	glColor3bv(arg_GLbytev(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3usv(void *p, int c, Scheme_Object **v)
{
	glColor3usv(arg_GLushortv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3sv(void *p, int c, Scheme_Object **v)
{
	glColor3sv(arg_GLshortv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3uiv(void *p, int c, Scheme_Object **v)
{
	glColor3uiv(arg_GLuintv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3iv(void *p, int c, Scheme_Object **v)
{
	glColor3iv(arg_GLintv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3fv(void *p, int c, Scheme_Object **v)
{
	glColor3fv(arg_GLfloatv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_Color3dv(void *p, int c, Scheme_Object **v)
{
	glColor3dv(arg_GLdoublev(0, 3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Color4ubv(void *p, int c, Scheme_Object **v)
{
	glColor4ubv(arg_GLubytev(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4bv(void *p, int c, Scheme_Object **v)
{
	glColor4bv(arg_GLbytev(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4usv(void *p, int c, Scheme_Object **v)
{
	glColor4usv(arg_GLushortv(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4sv(void *p, int c, Scheme_Object **v)
{
	glColor4sv(arg_GLshortv(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4uiv(void *p, int c, Scheme_Object **v)
{
	glColor4uiv(arg_GLuintv(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4iv(void *p, int c, Scheme_Object **v)
{
	glColor4iv(arg_GLintv(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4fv(void *p, int c, Scheme_Object **v)
{
	glColor4fv(arg_GLfloatv(0, 4));
	return scheme_void;
}

static Scheme_Object *scm_Color4dv(void *p, int c, Scheme_Object **v)
{
	glColor4dv(arg_GLdoublev(0, 4));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/

static Scheme_Object *scm_Indexub(void *p, int c, Scheme_Object **v)
{
	glIndexub(arg_GLubyte(0));
	return scheme_void;
}

static Scheme_Object *scm_Indexs(void *p, int c, Scheme_Object **v)
{
	glIndexs(arg_GLshort(0));
	return scheme_void;
}

static Scheme_Object *scm_Indexi(void *p, int c, Scheme_Object **v)
{
	glIndexi(arg_GLint(0));
	return scheme_void;
}

static Scheme_Object *scm_Indexf(void *p, int c, Scheme_Object **v)
{
	glIndexf(arg_GLfloat(0));
	return scheme_void;
}

static Scheme_Object *scm_Indexd(void *p, int c, Scheme_Object **v)
{
	glIndexd(arg_GLdouble(0));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Indexubv(void *p, int c, Scheme_Object **v)
{
	glIndexubv(arg_GLubytev(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_Indexsv(void *p, int c, Scheme_Object **v)
{
	glIndexsv(arg_GLshortv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_Indexiv(void *p, int c, Scheme_Object **v)
{
	glIndexiv(arg_GLintv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_Indexfv(void *p, int c, Scheme_Object **v)
{
	glIndexfv(arg_GLfloatv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_Indexdv(void *p, int c, Scheme_Object **v)
{
	glIndexdv(arg_GLdoublev(0, 1));
	return scheme_void;
}


/*...........................................................................*/

static Scheme_Object *scm_EnableClientState(void *p, int c, Scheme_Object **v)
{
	glEnableClientState(arg_GLenum(0));
	return scheme_void;
}

static Scheme_Object *scm_DisableClientState(void *p, int c, Scheme_Object **v)
{
	glDisableClientState(arg_GLenum(0));
	return scheme_void;
}

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_ClientActiveTexture(void *p, int c, Scheme_Object **v)
{
	glClientActiveTexture(arg_GLenum(0));
	return scheme_void;
}

#endif /* GL_VERSION_1_3 */



/*---------------------------------------------------------------------------*/
/* 2.9. Rectangles							     */

static Scheme_Object *scm_Rects(void *p, int c, Scheme_Object **v)
{
	glRects(arg_GLshort(0),
		arg_GLshort(1),
		arg_GLshort(2),
		arg_GLshort(3));
	return scheme_void;
}

static Scheme_Object *scm_Recti(void *p, int c, Scheme_Object **v)
{
	glRecti(arg_GLint(0),
		arg_GLint(1),
		arg_GLint(2),
		arg_GLint(3));
	return scheme_void;
}

static Scheme_Object *scm_Rectf(void *p, int c, Scheme_Object **v)
{
	glRectf(arg_GLfloat(0),
		arg_GLfloat(1),
		arg_GLfloat(2),
		arg_GLfloat(3));
	return scheme_void;
}

static Scheme_Object *scm_Rectd(void *p, int c, Scheme_Object **v)
{
	glRectd(arg_GLdouble(0),
		arg_GLdouble(1),
		arg_GLdouble(2),
		arg_GLdouble(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Rectsv(void *p, int c, Scheme_Object **v)
{
	glRectsv(arg_GLshortv(0, 2),
		 arg_GLshortv(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_Rectiv(void *p, int c, Scheme_Object **v)
{
	glRectiv(arg_GLintv(0, 2),
		 arg_GLintv(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_Rectfv(void *p, int c, Scheme_Object **v)
{
	glRectfv(arg_GLfloatv(0, 2),
		 arg_GLfloatv(1, 2));
	return scheme_void;
}

static Scheme_Object *scm_Rectdv(void *p, int c, Scheme_Object **v)
{
	glRectdv(arg_GLdoublev(0, 2),
		 arg_GLdoublev(1, 2));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.10. Coordinate Transformations					     */

static Scheme_Object *scm_DepthRange(void *p, int c, Scheme_Object **v)
{
	glDepthRange(arg_GLclampd(0),
		     arg_GLclampd(1));
	return scheme_void;
}

static Scheme_Object *scm_Viewport(void *p, int c, Scheme_Object **v)
{
	glViewport(arg_GLint  (0),
		   arg_GLint  (1),
		   arg_GLsizei(2),
		   arg_GLsizei(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MatrixMode(void *p, int c, Scheme_Object **v)
{
	glMatrixMode(arg_GLenum(0));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_LoadMatrixf(void *p, int c, Scheme_Object **v)
{
	glLoadMatrixf(arg_GLfloatv(0, 16));
	return scheme_void;
}

static Scheme_Object *scm_LoadMatrixd(void *p, int c, Scheme_Object **v)
{
	glLoadMatrixd(arg_GLdoublev(0, 16));
	return scheme_void;
}

static Scheme_Object *scm_MultMatrixf(void *p, int c, Scheme_Object **v)
{
	glMultMatrixf(arg_GLfloatv(0, 16));
	return scheme_void;
}

static Scheme_Object *scm_MultMatrixd(void *p, int c, Scheme_Object **v)
{
	glMultMatrixd(arg_GLdoublev(0, 16));
	return scheme_void;
}

/*...........................................................................*/

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_LoadTransposeMatrixf(void *p,
					       int c, Scheme_Object **v)
{
	glLoadTransposeMatrixf(arg_GLfloatv(0, 16));
	return scheme_void;
}

static Scheme_Object *scm_LoadTransposeMatrixd(void *p,
					       int c, Scheme_Object **v)
{
	glLoadTransposeMatrixd(arg_GLdoublev(0, 16));
	return scheme_void;
}

static Scheme_Object *scm_MultTransposeMatrixf(void *p,
					       int c, Scheme_Object **v)
{
	glMultTransposeMatrixf(arg_GLfloatv(0, 16));
	return scheme_void;
}

static Scheme_Object *scm_MultTransposeMatrixd(void *p,
					       int c, Scheme_Object **v)
{
	glMultTransposeMatrixd(arg_GLdoublev(0, 16));
	return scheme_void;
}

#endif /* GL_VERSION_1_3 */

/*...........................................................................*/

static Scheme_Object *scm_LoadIdentity(void *p, int c, Scheme_Object **v)
{
	glLoadIdentity();
	return scheme_void;
}

static Scheme_Object *scm_Rotatef(void *p, int c, Scheme_Object **v)
{
	glRotatef(arg_GLfloat(0),
		  arg_GLfloat(1),
		  arg_GLfloat(2),
		  arg_GLfloat(3));
	return scheme_void;
}

static Scheme_Object *scm_Rotated(void *p, int c, Scheme_Object **v)
{
	glRotated(arg_GLdouble(0),
		  arg_GLdouble(1),
		  arg_GLdouble(2),
		  arg_GLdouble(3));
	return scheme_void;
}

static Scheme_Object *scm_Translatef(void *p, int c, Scheme_Object **v)
{
	glTranslatef(arg_GLfloat(0),
		     arg_GLfloat(1),
		     arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Translated(void *p, int c, Scheme_Object **v)
{
	glTranslated(arg_GLdouble(0),
		     arg_GLdouble(1),
		     arg_GLdouble(2));
	return scheme_void;
}

static Scheme_Object *scm_Scalef(void *p, int c, Scheme_Object **v)
{
	glScalef(arg_GLfloat(0),
		 arg_GLfloat(1),
		 arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Scaled(void *p, int c, Scheme_Object **v)
{
	glScaled(arg_GLdouble(0),
		 arg_GLdouble(1),
		 arg_GLdouble(2));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_Frustum(void *p, int c, Scheme_Object **v)
{
	glFrustum(arg_GLdouble(0),
		  arg_GLdouble(1),
		  arg_GLdouble(2),
		  arg_GLdouble(3),
		  arg_GLdouble(4),
		  arg_GLdouble(5));
	return scheme_void;
}

static Scheme_Object *scm_Ortho(void *p, int c, Scheme_Object **v)
{
	glOrtho(arg_GLdouble(0),
		arg_GLdouble(1),
		arg_GLdouble(2),
		arg_GLdouble(3),
		arg_GLdouble(4),
		arg_GLdouble(5));
	return scheme_void;
}

/*...........................................................................*/

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_ActiveTexture(void *p, int c, Scheme_Object **v)
{
	glActiveTexture(arg_GLenum(0));
	return scheme_void;
}

#endif /* GL_VERSION_1_3 */

static Scheme_Object *scm_PushMatrix(void *p, int c, Scheme_Object **v)
{
	glPushMatrix();
	return scheme_void;
}

static Scheme_Object *scm_PopMatrix(void *p, int c, Scheme_Object **v)
{
	glPopMatrix();
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.10.4 Generating Texture Coordinates				     */

static Scheme_Object *scm_TexGeni(void *p, int c, Scheme_Object **v)
{
	glTexGeni(arg_GLenum(0),
		  arg_GLenum(1),
		  arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_TexGenf(void *p, int c, Scheme_Object **v)
{
	glTexGenf(arg_GLenum (0),
		  arg_GLenum (1),
		  arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_TexGend(void *p, int c, Scheme_Object **v)
{
	glTexGend(arg_GLenum  (0),
		  arg_GLenum  (1),
		  arg_GLdouble(2));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_TexGeniv(void *p, int c, Scheme_Object **v)
{
	GLenum n = arg_GLenum(1);

	if (n == GL_TEXTURE_GEN_MODE)
		glTexGeniv(arg_GLenum(0), n, arg_GLintv(2, 1));
	else
		glTexGeniv(arg_GLenum(0), n, arg_GLintv(2, 4));

	return scheme_void;
}

static Scheme_Object *scm_TexGenfv(void *p, int c, Scheme_Object **v)
{
	GLenum n = arg_GLenum(1);

	if (n == GL_TEXTURE_GEN_MODE)
		glTexGenfv(arg_GLenum(0), n, arg_GLfloatv(2, 1));
	else
		glTexGenfv(arg_GLenum(0), n, arg_GLfloatv(2, 4));

	return scheme_void;
}

static Scheme_Object *scm_TexGendv(void *p, int c, Scheme_Object **v)
{
	GLenum n = arg_GLenum(1);

	if (n == GL_TEXTURE_GEN_MODE)
		glTexGendv(arg_GLenum(0), n, arg_GLdoublev(2, 1));
	else
		glTexGendv(arg_GLenum(0), n, arg_GLdoublev(2, 4));

	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.11. Clipping							     */

static Scheme_Object *scm_ClipPlane(void *p, int c, Scheme_Object **v)
{
	glClipPlane(arg_GLenum(0), arg_GLdoublev(1, 4));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.12. Current Raster Position					     */

static Scheme_Object *scm_RasterPos2s(void *p, int c, Scheme_Object **v)
{
	glRasterPos2s(arg_GLshort(0),
		      arg_GLshort(1));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3s(void *p, int c, Scheme_Object **v)
{
	glRasterPos3s(arg_GLshort(0),
		      arg_GLshort(1),
		      arg_GLshort(2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4s(void *p, int c, Scheme_Object **v)
{
	glRasterPos4s(arg_GLshort(0),
		      arg_GLshort(1),
		      arg_GLshort(2),
		      arg_GLshort(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2i(void *p, int c, Scheme_Object **v)
{
	glRasterPos2i(arg_GLint(0),
		      arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3i(void *p, int c, Scheme_Object **v)
{
	glRasterPos3i(arg_GLint(0),
		      arg_GLint(1),
		      arg_GLint(2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4i(void *p, int c, Scheme_Object **v)
{
	glRasterPos4i(arg_GLint(0),
		      arg_GLint(1),
		      arg_GLint(2),
		      arg_GLint(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2f(void *p, int c, Scheme_Object **v)
{
	glRasterPos2f(arg_GLfloat(0),
		      arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3f(void *p, int c, Scheme_Object **v)
{
	glRasterPos3f(arg_GLfloat(0),
		      arg_GLfloat(1),
		      arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4f(void *p, int c, Scheme_Object **v)
{
	glRasterPos4f(arg_GLfloat(0),
		      arg_GLfloat(1),
		      arg_GLfloat(2),
		      arg_GLfloat(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2d(void *p, int c, Scheme_Object **v)
{
	glRasterPos2d(arg_GLdouble(0),
		      arg_GLdouble(1));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3d(void *p, int c, Scheme_Object **v)
{
	glRasterPos3d(arg_GLdouble(0),
		      arg_GLdouble(1),
		      arg_GLdouble(2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4d(void *p, int c, Scheme_Object **v)
{
	glRasterPos4d(arg_GLdouble(0),
		      arg_GLdouble(1),
		      arg_GLdouble(2),
		      arg_GLdouble(3));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2sv(void *p, int c, Scheme_Object **v)
{
	glRasterPos2sv(arg_GLshortv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3sv(void *p, int c, Scheme_Object **v)
{
	glRasterPos3sv(arg_GLshortv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4sv(void *p, int c, Scheme_Object **v)
{
	glRasterPos4sv(arg_GLshortv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2iv(void *p, int c, Scheme_Object **v)
{
	glRasterPos2iv(arg_GLintv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3iv(void *p, int c, Scheme_Object **v)
{
	glRasterPos3iv(arg_GLintv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4iv(void *p, int c, Scheme_Object **v)
{
	glRasterPos4iv(arg_GLintv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2fv(void *p, int c, Scheme_Object **v)
{
	glRasterPos2fv(arg_GLfloatv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3fv(void *p, int c, Scheme_Object **v)
{
	glRasterPos3fv(arg_GLfloatv(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4fv(void *p, int c, Scheme_Object **v)
{
	glRasterPos4fv(arg_GLfloatv(0, 4));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_RasterPos2dv(void *p, int c, Scheme_Object **v)
{
	glRasterPos2dv(arg_GLdoublev(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos3dv(void *p, int c, Scheme_Object **v)
{
	glRasterPos3dv(arg_GLdoublev(0, 3));
	return scheme_void;
}

static Scheme_Object *scm_RasterPos4dv(void *p, int c, Scheme_Object **v)
{
	glRasterPos4dv(arg_GLdoublev(0, 4));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 2.13. Colors and Coloring						     */

/*...........................................................................*/
/* 2.13.1. Lighting							     */

static Scheme_Object *scm_FrontFace(void *p, int c, Scheme_Object **v)
{
	glFrontFace(arg_GLenum(0));
	return scheme_void;
}

/*...........................................................................*/
/* 2.13.2. Lighting Parameter Specification				     */

static int pnum_Material(GLenum pname)
{
	switch (pname)
	{
	case GL_AMBIENT:
	case GL_DIFFUSE:
	case GL_AMBIENT_AND_DIFFUSE:
	case GL_SPECULAR:
	case GL_EMISSION:	return 4;
	case GL_SHININESS:	return 1;
	case GL_COLOR_INDEXES:	return 3;
	}
	return 0;
}

static Scheme_Object *scm_Materiali(void *p, int c, Scheme_Object **v)
{
	glMateriali(arg_GLenum(0),
		    arg_GLenum(1),
		    arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_Materialf(void *p, int c, Scheme_Object **v)
{
	glMaterialf(arg_GLenum (0),
		    arg_GLenum (1),
		    arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Materialiv(void *p, int c, Scheme_Object **v)
{
	GLenum pname = arg_GLenum(1);

	glMaterialiv(arg_GLenum(0), pname,
		     arg_GLintv(2, pnum_Material(pname)));
	return scheme_void;
}

static Scheme_Object *scm_Materialfv(void *p, int c, Scheme_Object **v)
{
	GLenum pname = arg_GLenum(1);

	glMaterialfv(arg_GLenum  (0), pname,
		     arg_GLfloatv(2, pnum_Material(pname)));
	return scheme_void;
}

/*...........................................................................*/

static int pnum_Light(GLenum pname)
{
	switch (pname)
	{
	case GL_SPOT_EXPONENT:
	case GL_SPOT_CUTOFF:
	case GL_CONSTANT_ATTENUATION:
	case GL_LINEAR_ATTENUATION:
	case GL_QUADRATIC_ATTENUATION:	return 1;
	case GL_SPOT_DIRECTION:		return 3;
	case GL_AMBIENT:
	case GL_DIFFUSE:
	case GL_SPECULAR:
	case GL_POSITION:		return 4;
	}
	return 0;
}

static Scheme_Object *scm_Lighti(void *p, int c, Scheme_Object **v)
{
	glLighti(arg_GLenum(0),
		 arg_GLenum(1),
		 arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_Lightf(void *p, int c, Scheme_Object **v)
{
	glLightf(arg_GLenum (0),
		 arg_GLenum (1),
		 arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_Lightiv(void *p, int c, Scheme_Object **v)
{
	GLenum pname = arg_GLenum(1);

	glLightiv(arg_GLenum(0), pname,
		  arg_GLintv(2, pnum_Light(pname)));
	return scheme_void;
}

static Scheme_Object *scm_Lightfv(void *p, int c, Scheme_Object **v)
{
	GLenum pname = arg_GLenum(1);

	glLightfv(arg_GLenum  (0), pname,
		  arg_GLfloatv(2, pnum_Light(pname)));
	return scheme_void;
}

/*...........................................................................*/

static int pnum_LightModel(GLenum pname)
{
	switch (pname)
	{
	case GL_LIGHT_MODEL_AMBIENT:		return 4;

#ifdef GL_VERSION_1_2
	case GL_LIGHT_MODEL_COLOR_CONTROL:
#endif
	case GL_LIGHT_MODEL_LOCAL_VIEWER:
	case GL_LIGHT_MODEL_TWO_SIDE:		return 1;

	}
	return 0;
}

static Scheme_Object *scm_LightModeli(void *p, int c, Scheme_Object **v)
{
	glLightModeli(arg_GLenum(0), arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_LightModelf(void *p, int c, Scheme_Object **v)
{
	glLightModelf(arg_GLenum(0), arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_LightModeliv(void *p, int c, Scheme_Object **v)
{
	GLenum pname = arg_GLenum(0);

	glLightModeliv(pname, arg_GLintv(1, pnum_LightModel(pname)));
	return scheme_void;
}

static Scheme_Object *scm_LightModelfv(void *p, int c, Scheme_Object **v)
{
	GLenum pname = arg_GLenum(0);

	glLightModelfv(pname, arg_GLfloatv(1, pnum_LightModel(pname)));
	return scheme_void;
}

/*...........................................................................*/
/* 2.13.3. Color Material						     */

static Scheme_Object *scm_ColorMaterial(void *p, int c, Scheme_Object **v)
{
	glColorMaterial(arg_GLenum(0), arg_GLenum(1));
	return scheme_void;
}

static Scheme_Object *scm_ShadeModel(void *p, int c, Scheme_Object **v)
{
	glShadeModel(arg_GLenum(0));
	return scheme_void;
}

/*===========================================================================*/
/* 3. Rasterization							     */

/*---------------------------------------------------------------------------*/
/* 3.3. Points								     */

static Scheme_Object *scm_PointSize(void *p, int c, Scheme_Object **v)
{
	glPointSize(arg_GLfloat(0));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 3.4. Line Segments							     */

static Scheme_Object *scm_LineWidth(void *p, int c, Scheme_Object **v)
{
	glLineWidth(arg_GLfloat(0));
	return scheme_void;
}

static Scheme_Object *scm_LineStipple(void *p, int c, Scheme_Object **v)
{
	glLineStipple(arg_GLint(0), arg_GLushort(1));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 3.5. Polygons							     */

static Scheme_Object *scm_CullFace(void *p, int c, Scheme_Object **v)
{
	glCullFace(arg_GLenum(0));
	return scheme_void;
}

static Scheme_Object *scm_PolygonStipple(void *p, int c, Scheme_Object **v)
{
	glPolygonStipple(arg_GLubytev(0, (32 * 32 / 8)));
	return scheme_void;
}

static Scheme_Object *scm_PolygonMode(void *p, int c, Scheme_Object **v)
{
	glPolygonMode(arg_GLenum(0), arg_GLenum(1));
	return scheme_void;
}

static Scheme_Object *scm_PolygonOffset(void *p, int c, Scheme_Object **v)
{
	glPolygonOffset(arg_GLfloat(0), arg_GLfloat(1));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 3.6. Pixel Rectangles						     */

/*...........................................................................*/
/* 3.6.1. Pixel Storage Modes						     */

static Scheme_Object *scm_PixelStorei(void *p, int c, Scheme_Object **v)
{
	glPixelStorei(arg_GLenum(0), arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_PixelStoref(void *p, int c, Scheme_Object **v)
{
	glPixelStoref(arg_GLenum(0), arg_GLfloat(1));
	return scheme_void;
}

/*...........................................................................*/
/* 3.6.3 Pixel Transfer Modes						     */

static Scheme_Object *scm_PixelTransferi(void *p, int c, Scheme_Object **v)
{
	glPixelTransferi(arg_GLenum(0), arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_PixelTransferf(void *p, int c, Scheme_Object **v)
{
	glPixelTransferf(arg_GLenum(0), arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_PixelMapusv(void *p, int c, Scheme_Object **v)
{
	glPixelMapusv(arg_GLenum   (0),
		      arg_GLsizei  (1),
		      arg_GLushortv(2, arg_GLsizei(1)));
	return scheme_void;
}

static Scheme_Object *scm_PixelMapuiv(void *p, int c, Scheme_Object **v)
{
	glPixelMapuiv(arg_GLenum (0),
		      arg_GLsizei(1),
		      arg_GLuintv(2, arg_GLsizei(1)));
	return scheme_void;
}

static Scheme_Object *scm_PixelMapfv(void *p, int c, Scheme_Object **v)
{
	glPixelMapfv(arg_GLenum  (0),
		     arg_GLsizei (1),
		     arg_GLfloatv(2, arg_GLsizei(1)));
	return scheme_void;
}

/* Color Table Specification						     */

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_ColorTable(void *p, int c, Scheme_Object **v)
{
  GLenum t = arg_GLenum(4);
  GLsizei l = arg_GLsizei(2);
  glColorTable(arg_GLenum (0),
	       arg_GLenum (1),
	       l,
	       arg_GLenum (3),
	       t,
	       arg_GLvoidv(5, l, t));
  return scheme_void;
}

static Scheme_Object *scm_ColorTableParameteriv(void *p,
						int c, Scheme_Object **v)
{
	glColorTableParameteriv(arg_GLenum(0),
				arg_GLenum(1),
				arg_GLintv(2, 4));
	return scheme_void;
}

static Scheme_Object *scm_ColorTableParameterfv(void *p,
						int c, Scheme_Object **v)
{
	glColorTableParameterfv(arg_GLenum  (0),
				arg_GLenum  (1),
				arg_GLfloatv(2, 4));
	return scheme_void;
}

/* Alternate Color Table Specification Commands				     */

static Scheme_Object *scm_CopyColorTable(void *p, int c, Scheme_Object **v)
{
	glCopyColorTable(arg_GLenum (0),
			 arg_GLenum (1),
			 arg_GLint  (2),
			 arg_GLint  (3),
			 arg_GLsizei(4));
	return scheme_void;
}

static Scheme_Object *scm_ColorSubTable(void *p, int c, Scheme_Object **v)
{
  GLsizei start = arg_GLsizei(1);
  GLsizei count = arg_GLsizei(2);
  GLenum t = arg_GLenum(4);
  glColorSubTable(arg_GLenum (0),
		  start,
		  count,
		  arg_GLenum (3),
		  t,
		  arg_GLvoidv(5, start + count, t));
  return scheme_void;
}

static Scheme_Object *scm_CopyColorSubTable(void *p, int c, Scheme_Object **v)
{
	glCopyColorSubTable(arg_GLenum (0),
			    arg_GLsizei(1),
			    arg_GLint  (2),
			    arg_GLint  (3),
			    arg_GLsizei(4));
	return scheme_void;
}

/* Convolution Filter Specification					     */

static Scheme_Object *scm_ConvolutionFilter2D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(2);
  GLsizei height = arg_GLsizei(3);
  GLenum t = arg_GLenum(5);
  glConvolutionFilter2D(arg_GLenum (0),
			arg_GLenum (1),
			width,
			height,
			arg_GLenum (4),
			t,
			arg_GLvoidv(6, width * height, t));
  return scheme_void;
}

static Scheme_Object *scm_ConvolutionFilter1D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(2);
  GLenum t = arg_GLenum(4);
  glConvolutionFilter1D(arg_GLenum (0),
			arg_GLenum (1),
			width,
			arg_GLenum (3),
			t,
			arg_GLvoidv(5, width, t));
  return scheme_void;
}

static Scheme_Object *scm_SeparableFilter2D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(2);
  GLsizei height = arg_GLsizei(3);
  GLenum t = arg_GLenum(5);
  glSeparableFilter2D(arg_GLenum (0),
		      arg_GLenum (1),
		      width,
		      height,
		      arg_GLenum (4),
		      t,
		      arg_GLvoidv(6, width, t),
		      arg_GLvoidv(7, height, t));
  return scheme_void;
}

/* Alternate Convolution Filter Specification Commands			     */

static Scheme_Object *scm_CopyConvolutionFilter2D(void *p,
						  int c, Scheme_Object **v)
{
	glCopyConvolutionFilter2D(arg_GLenum (0),
				  arg_GLenum (1),
				  arg_GLint  (2),
				  arg_GLint  (3),
				  arg_GLsizei(4),
				  arg_GLsizei(5));
	return scheme_void;
}

static Scheme_Object *scm_CopyConvolutionFilter1D(void *p,
						  int c, Scheme_Object **v)
{
	glCopyConvolutionFilter1D(arg_GLenum (0),
				  arg_GLenum (1),
				  arg_GLint  (2),
				  arg_GLint  (3),
				  arg_GLsizei(4));
	return scheme_void;
}

/* Histogram Table Specification					     */

static Scheme_Object *scm_Histogram(void *p, int c, Scheme_Object **v)
{
	glHistogram(arg_GLenum   (0),
		    arg_GLsizei  (1),
		    arg_GLenum   (2),
		    arg_GLboolean(3));
	return scheme_void;
}

/* Minmax Table Specification						     */

static Scheme_Object *scm_Minmax(void *p, int c, Scheme_Object **v)
{
	glMinmax(arg_GLenum   (0),
		 arg_GLenum   (1),
		 arg_GLboolean(2));
	return scheme_void;
}

#endif /* GL_VERSION_1_2 */

/*...........................................................................*/
/* 3.6.4 Rasterization of Pixel Rectangles				     */

static Scheme_Object *scm_DrawPixels(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(0);
  GLsizei height = arg_GLsizei(1);
  GLenum t = arg_GLenum(3);
  glDrawPixels(width,
	       height,
	       arg_GLenum(2),
	       t,
	       arg_GLvoidv(4, width * height, t));
  return scheme_void;
}

static Scheme_Object *scm_PixelZoom(void *p, int c, Scheme_Object **v)
{
	glPixelZoom(arg_GLfloat(0), arg_GLfloat(1));
	return scheme_void;
}

/*...........................................................................*/
/* 3.6.5 Pixel Transfer Operations					     */

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_ConvolutionParameteri(void *p,
						int c, Scheme_Object **v)
{
	glConvolutionParameteri(arg_GLenum(0),
				arg_GLenum(1),
				arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_ConvolutionParameterf(void *p,
						int c, Scheme_Object **v)
{
	glConvolutionParameterf(arg_GLenum (0),
				arg_GLenum (1),
				arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_ConvolutionParameteriv(void *p,
						 int c, Scheme_Object **v)
{
	glConvolutionParameteriv(arg_GLenum(0),
				 arg_GLenum(1),
				 arg_GLintv(2, 4));
	return scheme_void;
}

static Scheme_Object *scm_ConvolutionParameterfv(void *p,
						 int c, Scheme_Object **v)
{
	glConvolutionParameterfv(arg_GLenum  (0),
				 arg_GLenum  (1),
				 arg_GLfloatv(2, 4));
	return scheme_void;
}

#endif /* GL_VERSION_1_2 */

/*---------------------------------------------------------------------------*/
/* 3.7. Bitmaps								     */

static Scheme_Object *scm_Bitmap(void *p, int c, Scheme_Object **v)
{
  GLsizei w = arg_GLsizei(0);
  GLsizei h = arg_GLsizei(1);
  glBitmap(w,
	   h,
	   arg_GLfloat(2),
	   arg_GLfloat(3),
	   arg_GLfloat(4),
	   arg_GLfloat(5),
	   arg_GLubytev(6, w * h));
  return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 3.8. Texturing							     */

/*...........................................................................*/
/* 3.8.1 Texture Image Specification					     */

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_TexImage3D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(3);
  GLsizei height = arg_GLsizei(4);
  GLsizei depth = arg_GLsizei(5);
  GLenum t = arg_GLenum(8);

  glTexImage3D(arg_GLenum (0),
	       arg_GLint  (1),
	       arg_GLint  (2),
	       width,
	       height,
	       depth,
	       arg_GLint  (6),
	       arg_GLenum (7),
	       t,
	       arg_GLvoidv(9, width * height * depth, t));
  return scheme_void;
}

#endif /* GL_VERSION_1_2 */

static Scheme_Object *scm_TexImage2D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(3);
  GLsizei height = arg_GLsizei(4);
  GLenum t = arg_GLenum(7);

  glTexImage2D(arg_GLenum (0),
	       arg_GLint  (1),
	       arg_GLint  (2),
	       width,
	       height,
	       arg_GLint  (5),
	       arg_GLenum (6),
	       t,
	       arg_GLvoidv(8, width * height, t));
  return scheme_void;
}

static Scheme_Object *scm_TexImage1D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(3);
  GLenum t = arg_GLenum(6);

  glTexImage1D(arg_GLenum (0),
	       arg_GLint  (1),
	       arg_GLint  (2),
	       width,
	       arg_GLint  (4),
	       arg_GLenum (5),
	       t,
	       arg_GLvoidv(7, width, t));
  return scheme_void;
}

/*...........................................................................*/
/* 3.8.2 Alternate Texture Image Specification Commands			     */

static Scheme_Object *scm_CopyTexImage2D(void *p, int c, Scheme_Object **v)
{
	glCopyTexImage2D(arg_GLenum (0),
			 arg_GLint  (1),
			 arg_GLenum (2),
			 arg_GLint  (3),
			 arg_GLint  (4),
			 arg_GLsizei(5),
			 arg_GLsizei(6),
			 arg_GLint  (7));
	return scheme_void;
}

static Scheme_Object *scm_CopyTexImage1D(void *p, int c, Scheme_Object **v)
{
	glCopyTexImage1D(arg_GLenum (0),
			 arg_GLint  (1),
			 arg_GLenum (2),
			 arg_GLint  (3),
			 arg_GLint  (4),
			 arg_GLsizei(5),
			 arg_GLint  (6));
	return scheme_void;
}

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_TexSubImage3D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(5);
  GLsizei height = arg_GLsizei(6);
  GLsizei depth = arg_GLsizei(7);
  GLenum t = arg_GLenum(9);

  glTexSubImage3D(arg_GLenum (0),
		  arg_GLint  (1),
		  arg_GLint  (2),
		  arg_GLint  (3),
		  arg_GLint  (4),
		  width,
		  height,
		  depth,
		  arg_GLenum (8),
		  t,
		  arg_GLvoidv(10, width * height * depth, t));
  return scheme_void;
}

#endif /* GL_VERSION_1_2 */

static Scheme_Object *scm_TexSubImage2D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(4);
  GLsizei height = arg_GLsizei(5);
  GLenum t = arg_GLenum(7);

  glTexSubImage2D(arg_GLenum (0),
		  arg_GLint  (1),
		  arg_GLint  (2),
		  arg_GLint  (3),
		  width,
		  height,
		  arg_GLenum (6),
		  t,
		  arg_GLvoidv(8, width * height, t));
  return scheme_void;
}

static Scheme_Object *scm_TexSubImage1D(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(3);
  GLenum t = arg_GLenum(5);
  
  glTexSubImage1D(arg_GLenum (0),
		  arg_GLint  (1),
		  arg_GLint  (2),
		  width,
		  arg_GLenum (4),
		  t,
		  arg_GLvoidv(6, width, t));
  
  return scheme_void;
}

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_CopyTexSubImage3D(void *p, int c, Scheme_Object **v)
{
	glCopyTexSubImage3D(arg_GLenum (0),
			    arg_GLint  (1),
			    arg_GLint  (2),
			    arg_GLint  (3),
			    arg_GLint  (4),
			    arg_GLint  (5),
			    arg_GLint  (6),
			    arg_GLsizei(7),
			    arg_GLsizei(8));
	return scheme_void;
}

#endif /* GL_VERSION_1_2 */

static Scheme_Object *scm_CopyTexSubImage2D(void *p, int c, Scheme_Object **v)
{
	glCopyTexSubImage2D(arg_GLenum (0),
			    arg_GLint  (1),
			    arg_GLint  (2),
			    arg_GLint  (3),
			    arg_GLint  (4),
			    arg_GLint  (5),
			    arg_GLsizei(6),
			    arg_GLsizei(7));
	return scheme_void;
}

static Scheme_Object *scm_CopyTexSubImage1D(void *p, int c, Scheme_Object **v)
{
	glCopyTexSubImage1D(arg_GLenum (0),
			    arg_GLint  (1),
			    arg_GLint  (2),
			    arg_GLint  (3),
			    arg_GLint  (4),
			    arg_GLsizei(5));
	return scheme_void;
}


/*...........................................................................*/
/* 3.8.4. Texture Parameters						     */

static int pnum_TexParameter(GLenum pname)
{
	if (pname == GL_TEXTURE_BORDER_COLOR)
		return 4;
	else
		return 1;
}

static Scheme_Object *scm_TexParameteri(void *p, int c, Scheme_Object **v)
{
	glTexParameteri(arg_GLenum(0),
			arg_GLenum(1),
			arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_TexParameterf(void *p, int c, Scheme_Object **v)
{
	glTexParameterf(arg_GLenum (0),
			arg_GLenum (1),
			arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_TexParameteriv(void *p, int c, Scheme_Object **v)
{
	GLsizei pname = arg_GLenum(1);

	glTexParameteriv(arg_GLenum(0), pname,
			 arg_GLintv(2, pnum_TexParameter(pname)));
	return scheme_void;
}

static Scheme_Object *scm_TexParameterfv(void *p, int c, Scheme_Object **v)
{
	GLsizei pname = arg_GLenum(1);

	glTexParameterfv(arg_GLenum  (0), pname,
			 arg_GLfloatv(2, pnum_TexParameter(pname)));
	return scheme_void;
}

/*...........................................................................*/
/* 3.8.11 Texture Objects						     */

static Scheme_Object *scm_BindTexture(void *p, int c, Scheme_Object **v)
{
	glBindTexture(arg_GLenum(0), arg_GLuint(1));
	return scheme_void;
}

static Scheme_Object *scm_DeleteTextures(void *p, int c, Scheme_Object **v)
{
	GLsizei n = arg_GLsizei(0);

	glDeleteTextures(n, arg_GLuintv(1, n));
	return scheme_void;
}

static Scheme_Object *scm_GenTextures(void *p, int c, Scheme_Object **v)
{
	GLsizei n = arg_GLsizei(0);

	glGenTextures(n, arg_GLuintv(1, n));
	return scheme_void;
}

static Scheme_Object *scm_AreTexturesResident(void *p, int c, Scheme_Object **v)
{
	GLsizei n = arg_GLsizei(0);

	if (glAreTexturesResident(n, arg_GLuintv   (1, n),
				     arg_GLbooleanv(2, n)))
		return scheme_true;
	else
		return scheme_false;
}

static Scheme_Object *scm_PrioritizeTextures(void *p, int c, Scheme_Object **v)
{
	GLsizei n = arg_GLsizei(0);

	glPrioritizeTextures(n, arg_GLuintv (1, n),
				arg_GLfloatv(2, n));
	return scheme_void;
}

/*...........................................................................*/
/* 3.8.12 Texture Environments and Texture Functions			     */

static Scheme_Object *scm_TexEnvi(void *p, int c, Scheme_Object **v)
{
	glTexEnvi(arg_GLenum(0),
		  arg_GLenum(1),
		  arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_TexEnvf(void *p, int c, Scheme_Object **v)
{
	glTexEnvf(arg_GLenum (0),
		  arg_GLenum (1),
		  arg_GLfloat(2));
	return scheme_void;
}

static int pnum_TexEnv(GLenum pname)
{
  if (pname == GL_TEXTURE_ENV_COLOR)
    return 4;
  return 1;
}

static Scheme_Object *scm_TexEnviv(void *p, int c, Scheme_Object **v)
{
  GLenum e = arg_GLenum(1);
  
  glTexEnviv(arg_GLenum (0),
	     e,
	     arg_GLintv(2, pnum_TexEnv(e)));
  return scheme_void;
}

static Scheme_Object *scm_TexEnvfv(void *p, int c, Scheme_Object **v)
{
  GLenum e = arg_GLenum(1);
  
  glTexEnvfv(arg_GLenum (0),
	     e,
	     arg_GLfloatv(2, pnum_TexEnv(e)));
  return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 3.10. Fog								     */

static Scheme_Object *scm_Fogi(void *p, int c, Scheme_Object **v)
{
	glFogi(arg_GLenum(0), arg_GLint(1));
	return scheme_void;
}

static Scheme_Object *scm_Fogf(void *p, int c, Scheme_Object **v)
{
	glFogf(arg_GLenum(0), arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_Fogiv(void *p, int c, Scheme_Object **v)
{
	glFogiv(arg_GLenum(0), arg_GLintv(1, 1));
	return scheme_void;
}

static Scheme_Object *scm_Fogfv(void *p, int c, Scheme_Object **v)
{
	glFogfv(arg_GLenum(0), arg_GLfloatv(1, 1));
	return scheme_void;
}

/*===========================================================================*/
/* 4. Per-Fragment Operations and the Framebuffer			     */

/*---------------------------------------------------------------------------*/
/* 4.1. Per-Fragment Operations						     */

/*...........................................................................*/
/* 4.1.2. Scissor Test							     */

static Scheme_Object *scm_Scissor(void *p, int c, Scheme_Object **v)
{
	glScissor(arg_GLint  (0),
		  arg_GLint  (1),
		  arg_GLsizei(2),
		  arg_GLsizei(3));
	return scheme_void;
}

/*...........................................................................*/
/* 4.1.3. Multisample Fragmant Operations				     */

#ifdef GL_VERSION_1_3

static Scheme_Object *scm_SampleCoverage(void *p, int c, Scheme_Object **v)
{
	glSampleCoverage(arg_GLclampf(0), arg_GLboolean(1));
	return scheme_void;
}

#endif /* GL_VERSION_1_3 */

/*...........................................................................*/
/* 4.1.4. Alpha Test							     */

static Scheme_Object *scm_AlphaFunc(void *p, int c, Scheme_Object **v)
{
	glAlphaFunc(arg_GLenum(0), arg_GLclampf(1));
	return scheme_void;
}

/*...........................................................................*/
/* 4.1.5. Stencil Test							     */

static Scheme_Object *scm_StencilFunc(void *p, int c, Scheme_Object **v)
{
	glStencilFunc(arg_GLenum(0),
		      arg_GLint (1),
		      arg_GLuint(2));
	return scheme_void;
}

static Scheme_Object *scm_StencilOp(void *p, int c, Scheme_Object **v)
{
	glStencilOp(arg_GLenum(0),
		    arg_GLenum(1),
		    arg_GLenum(2));
	return scheme_void;
}

/*...........................................................................*/
/* 4.1.6. Depth Test							     */

static Scheme_Object *scm_DepthFunc(void *p, int c, Scheme_Object **v)
{
	glDepthFunc(arg_GLenum(0));
	return scheme_void;
}

/*...........................................................................*/
/* 4.1.7. Blending							     */

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_BlendColor(void *p, int c, Scheme_Object **v)
{
	glBlendColor(arg_GLclampf(0),
		     arg_GLclampf(1),
		     arg_GLclampf(2),
		     arg_GLclampf(3));
	return scheme_void;
}

static Scheme_Object *scm_BlendEquation(void *p, int c, Scheme_Object **v)
{
	glBlendEquation(arg_GLenum(0));
	return scheme_void;
}

#endif /* GL_VERSION_1_2 */

static Scheme_Object *scm_BlendFunc(void *p, int c, Scheme_Object **v)
{
	glBlendFunc(arg_GLenum(0), arg_GLenum(1));
	return scheme_void;
}

/*...........................................................................*/
/* 4.1.9. Logical Operation						     */

static Scheme_Object *scm_LogicOp(void *p, int c, Scheme_Object **v)
{
	glLogicOp(arg_GLenum(0));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 4.2. Whole Framebuffer Operations					     */

/*...........................................................................*/
/* 4.2.1. Selecting a Buffer for Writing				     */

static Scheme_Object *scm_DrawBuffer(void *p, int c, Scheme_Object **v)
{
	glDrawBuffer(arg_GLenum(0));
	return scheme_void;
}

/*...........................................................................*/
/* 4.2.2. Fine Control of Buffer Updates				     */

static Scheme_Object *scm_IndexMask(void *p, int c, Scheme_Object **v)
{
	glIndexMask(arg_GLuint(0));
	return scheme_void;
}

static Scheme_Object *scm_ColorMask(void *p, int c, Scheme_Object **v)
{
	glColorMask(arg_GLboolean(0),
		    arg_GLboolean(1),
		    arg_GLboolean(2),
		    arg_GLboolean(3));
	return scheme_void;
}

static Scheme_Object *scm_DepthMask(void *p, int c, Scheme_Object **v)
{
	glDepthMask(arg_GLboolean(0));
	return scheme_void;
}

static Scheme_Object *scm_StencilMask(void *p, int c, Scheme_Object **v)
{
	glStencilMask(arg_GLuint(0));
	return scheme_void;
}

/*...........................................................................*/
/* 4.2.3. Clearing the Buffers						     */

static Scheme_Object *scm_Clear(void *p, int c, Scheme_Object **v)
{
	glClear(arg_GLbitfield(0));
	return scheme_void;
}

static Scheme_Object *scm_ClearColor(void *p, int c, Scheme_Object **v)
{
	glClearColor(arg_GLclampf(0),
		     arg_GLclampf(1),
		     arg_GLclampf(2),
		     arg_GLclampf(3));
	return scheme_void;
}

static Scheme_Object *scm_ClearIndex(void *p, int c, Scheme_Object **v)
{
	glClearIndex(arg_GLfloat(0));
	return scheme_void;
}

static Scheme_Object *scm_ClearDepth(void *p, int c, Scheme_Object **v)
{
	glClearDepth(arg_GLclampd(0));
	return scheme_void;
}

static Scheme_Object *scm_ClearStencil(void *p, int c, Scheme_Object **v)
{
	glClearStencil(arg_GLint(0));
	return scheme_void;
}

static Scheme_Object *scm_ClearAccum(void *p, int c, Scheme_Object **v)
{
	glClearAccum(arg_GLfloat(0),
		     arg_GLfloat(1),
		     arg_GLfloat(2),
		     arg_GLfloat(3));
	return scheme_void;
}

/*...........................................................................*/
/* 4.2.4. The Accumulation Buffer					     */

static Scheme_Object *scm_Accum(void *p, int c, Scheme_Object **v)
{
	glAccum(arg_GLenum(0), arg_GLfloat(1));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 4.3. Drawing, Reading, and Copying Pixels				     */

/*...........................................................................*/
/* 4.3.2. Reading Pixels						     */

static Scheme_Object *scm_ReadPixels(void *p, int c, Scheme_Object **v)
{
  GLsizei width = arg_GLsizei(2);
  GLsizei height = arg_GLsizei(3);
  GLenum t = arg_GLenum(5);
  glReadPixels(arg_GLint  (0),
	       arg_GLint  (1),
	       width,
	       height,
	       arg_GLenum (4),
	       t,
	       arg_GLvoidv(6, width * height, t));
  return scheme_void;
}

static Scheme_Object *scm_ReadBuffer(void *p, int c, Scheme_Object **v)
{
	glReadBuffer(arg_GLenum(0));
	return scheme_void;
}

/*...........................................................................*/
/* 4.3.3. Copying Pixels						     */

static Scheme_Object *scm_CopyPixels(void *p, int c, Scheme_Object **v)
{
	glCopyPixels(arg_GLint  (0),
		     arg_GLint  (1),
		     arg_GLsizei(2),
		     arg_GLsizei(3),
		     arg_GLenum (4));
	return scheme_void;
}

/*===========================================================================*/
/* 5. Special Functions							     */

/*---------------------------------------------------------------------------*/
/* 5.1. Evaluators							     */

static int pnum_Map1(GLenum target)
{
	switch (target)
	{
	case GL_MAP1_INDEX:
	case GL_MAP1_TEXTURE_COORD_1:	return 1;
	case GL_MAP1_TEXTURE_COORD_2:	return 2;
	case GL_MAP1_VERTEX_3:
	case GL_MAP1_NORMAL:
	case GL_MAP1_TEXTURE_COORD_3:	return 3;
	case GL_MAP1_VERTEX_4:
	case GL_MAP1_COLOR_4:
	case GL_MAP1_TEXTURE_COORD_4:	return 4;
	}
	return 0;
}

static Scheme_Object *scm_Map1f(void *p, int c, Scheme_Object **v)
{
	GLenum target = arg_GLenum(0);

	glMap1f(target, arg_GLfloat (1),
			arg_GLfloat (2),
			arg_GLint   (3),
			arg_GLint   (4),
			arg_GLfloatv(5, pnum_Map1(target)));
	return scheme_void;
}

static Scheme_Object *scm_Map1d(void *p, int c, Scheme_Object **v)
{
	GLenum target = arg_GLenum(0);

	glMap1d(target, arg_GLdouble (1),
			arg_GLdouble (2),
			arg_GLint    (3),
			arg_GLint    (4),
			arg_GLdoublev(5, pnum_Map1(target)));
	return scheme_void;
}

/*...........................................................................*/

static int pnum_Map2(GLenum target)
{
	switch (target)
	{
	case GL_MAP2_INDEX:
	case GL_MAP2_TEXTURE_COORD_1:	return 1;
	case GL_MAP2_TEXTURE_COORD_2:	return 2;
	case GL_MAP2_VERTEX_3:
	case GL_MAP2_NORMAL:
	case GL_MAP2_TEXTURE_COORD_3:	return 3;
	case GL_MAP2_VERTEX_4:
	case GL_MAP2_COLOR_4:
	case GL_MAP2_TEXTURE_COORD_4:	return 4;
	}
	return 0;
}

static Scheme_Object *scm_Map2f(void *p, int c, Scheme_Object **v)
{
	GLenum target = arg_GLenum(0);

	glMap2f(target, arg_GLfloat (1),
			arg_GLfloat (2),
			arg_GLint   (3),
			arg_GLint   (4),
			arg_GLfloat (5),
			arg_GLfloat (6),
			arg_GLint   (7),
			arg_GLint   (8),
			arg_GLfloatv(9, pnum_Map2(target)));
	return scheme_void;
}

static Scheme_Object *scm_Map2d(void *p, int c, Scheme_Object **v)
{
	GLenum target = arg_GLenum(0);

	glMap2d(target, arg_GLdouble (1),
			arg_GLdouble (2),
			arg_GLint    (3),
			arg_GLint    (4),
			arg_GLdouble (5),
			arg_GLdouble (6),
			arg_GLint    (7),
			arg_GLint    (8),
			arg_GLdoublev(9, pnum_Map2(target)));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_EvalCoord1f(void *p, int c, Scheme_Object **v)
{
	glEvalCoord1f(arg_GLfloat(0));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord1d(void *p, int c, Scheme_Object **v)
{
	glEvalCoord1d(arg_GLdouble(0));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord2f(void *p, int c, Scheme_Object **v)
{
	glEvalCoord2f(arg_GLfloat(0), arg_GLfloat(1));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord2d(void *p, int c, Scheme_Object **v)
{
	glEvalCoord2d(arg_GLdouble(0), arg_GLdouble(1));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord1fv(void *p, int c, Scheme_Object **v)
{
	glEvalCoord1fv(arg_GLfloatv(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord1dv(void *p, int c, Scheme_Object **v)
{
	glEvalCoord1dv(arg_GLdoublev(0, 1));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord2fv(void *p, int c, Scheme_Object **v)
{
	glEvalCoord2fv(arg_GLfloatv(0, 2));
	return scheme_void;
}

static Scheme_Object *scm_EvalCoord2dv(void *p, int c, Scheme_Object **v)
{
	glEvalCoord2dv(arg_GLdoublev(0, 2));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_MapGrid1f(void *p, int c, Scheme_Object **v)
{
	glMapGrid1f(arg_GLint  (0),
		    arg_GLfloat(1),
		    arg_GLfloat(2));
	return scheme_void;
}

static Scheme_Object *scm_MapGrid1d(void *p, int c, Scheme_Object **v)
{
	glMapGrid1d(arg_GLint   (0),
		    arg_GLdouble(1),
		    arg_GLdouble(2));
	return scheme_void;
}

static Scheme_Object *scm_MapGrid2f(void *p, int c, Scheme_Object **v)
{
	glMapGrid2f(arg_GLint  (0),
		    arg_GLfloat(1),
		    arg_GLfloat(2),
		    arg_GLint  (3),
		    arg_GLfloat(4),
		    arg_GLfloat(5));
	return scheme_void;
}

static Scheme_Object *scm_MapGrid2d(void *p, int c, Scheme_Object **v)
{
	glMapGrid2d(arg_GLint   (0),
		    arg_GLdouble(1),
		    arg_GLdouble(2),
		    arg_GLint   (3),
		    arg_GLdouble(4),
		    arg_GLdouble(5));
	return scheme_void;
}

/*...........................................................................*/

static Scheme_Object *scm_EvalMesh1(void *p, int c, Scheme_Object **v)
{
	glEvalMesh1(arg_GLenum(0),
		    arg_GLint (1),
		    arg_GLint (2));
	return scheme_void;
}

static Scheme_Object *scm_EvalMesh2(void *p, int c, Scheme_Object **v)
{
	glEvalMesh2(arg_GLenum(0),
		    arg_GLint (1),
		    arg_GLint (2),
		    arg_GLint (3),
		    arg_GLint (4));
	return scheme_void;
}

static Scheme_Object *scm_EvalPoint1(void *p, int c, Scheme_Object **v)
{
	glEvalPoint1(arg_GLint(0));
	return scheme_void;
}

static Scheme_Object *scm_EvalPoint2(void *p, int c, Scheme_Object **v)
{
	glEvalPoint2(arg_GLint(0), arg_GLint(1));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 5.2. Selection							     */

static Scheme_Object *scm_InitNames(void *p, int c, Scheme_Object **v)
{
	glInitNames();
	return scheme_void;
}

static Scheme_Object *scm_PopName(void *p, int c, Scheme_Object **v)
{
	glPopName();
	return scheme_void;
}

static Scheme_Object *scm_PushName(void *p, int c, Scheme_Object **v)
{
	glPushName(arg_GLuint(0));
	return scheme_void;
}

static Scheme_Object *scm_LoadName(void *p, int c, Scheme_Object **v)
{
	glLoadName(arg_GLuint(0));
	return scheme_void;
}

static Scheme_Object *scm_RenderMode(void *p, int c, Scheme_Object **v)
{
	return scheme_make_integer(glRenderMode(arg_GLenum(0)));
}

static Scheme_Object *scm_SelectBuffer(void *p, int c, Scheme_Object **v)
{
	GLsizei n = arg_GLsizei(0);

	glSelectBuffer(n, arg_GLuintv(1, n));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 5.3. Feedback							     */

static Scheme_Object *scm_FeedbackBuffer(void *p, int c, Scheme_Object **v)
{
	GLsizei n = arg_GLsizei(0);

	glFeedbackBuffer(n, arg_GLenum  (1),
			    arg_GLfloatv(2, n));
	return scheme_void;
}

static Scheme_Object *scm_PassThrough(void *p, int c, Scheme_Object **v)
{
	glPassThrough(arg_GLfloat(0));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 5.4. Display Lists							     */

static Scheme_Object *scm_NewList(void *p, int c, Scheme_Object **v)
{
	glNewList(arg_GLuint(0), arg_GLenum(1));
	return scheme_void;
}

static Scheme_Object *scm_EndList(void *p, int c, Scheme_Object **v)
{
	glEndList();
	return scheme_void;
}

static Scheme_Object *scm_CallList(void *p, int c, Scheme_Object **v)
{
	glCallList(arg_GLuint(0));
	return scheme_void;
}

static Scheme_Object *scm_CallLists(void *p, int c, Scheme_Object **v)
{
  GLsizei length = arg_GLsizei(0);
  GLenum t = arg_GLenum(1);
  
  glCallLists(length,
	      t,
	      arg_GLvoidv(2, length, t));
  return scheme_void;
}

static Scheme_Object *scm_ListBase(void *p, int c, Scheme_Object **v)
{
	glListBase(arg_GLuint(0));
	return scheme_void;
}

static Scheme_Object *scm_GenLists(void *p, int c, Scheme_Object **v)
{
	return scheme_make_integer(glGenLists(arg_GLsizei(0)));
}

static Scheme_Object *scm_IsList(void *p, int c, Scheme_Object **v)
{
	if (glIsList(arg_GLuint(0)))
		return scheme_true;
	else
		return scheme_false;
}

static Scheme_Object *scm_DeleteLists(void *p, int c, Scheme_Object **v)
{
	glDeleteLists(arg_GLuint(0), arg_GLsizei(1));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 5.5. Flush and Finish						     */

static Scheme_Object *scm_Flush(void *p, int c, Scheme_Object **v)
{
	glFlush();
	return scheme_void;
}

static Scheme_Object *scm_Finish(void *p, int c, Scheme_Object **v)
{
	glFinish();
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 5.6. Hints								     */

static Scheme_Object *scm_Hint(void *p, int c, Scheme_Object **v)
{
	glHint(arg_GLenum(0), arg_GLenum(1));
	return scheme_void;
}

/*===========================================================================*/
/* 6. State and State Requests						     */

static Scheme_Object *scm_Enable(void *p, int c, Scheme_Object **v)
{
	glEnable(arg_GLenum(0));
	return scheme_void;
}

static Scheme_Object *scm_Disable(void *p, int c, Scheme_Object **v)
{
	glDisable(arg_GLenum(0));
	return scheme_void;
}

/*---------------------------------------------------------------------------*/
/* 6.1. Querying GL State						     */

/*...........................................................................*/
/* 6.1.1 Simple Queries							     */
static Scheme_Object *scm_IsEnabled(void *p, int c, Scheme_Object **v)
{
	if (glIsEnabled(arg_GLenum(0)))
		return scheme_true;
	else
		return scheme_false;
}

/*...........................................................................*/
/* 6.1.3 Enumerated Queries						     */

static Scheme_Object *scm_GetClipPlane(void *p, int c, Scheme_Object **v)
{
	glGetClipPlane(arg_GLenum(0), arg_GLdoublev(1, 4));
	return scheme_void;
}

static Scheme_Object *scm_GetLightiv(void *p, int c, Scheme_Object **v)
{
	GLenum value = arg_GLenum(1);

	glGetLightiv(arg_GLenum(0), value,
		     arg_GLintv(2, pnum_Light(value)));
	return scheme_void;
}

static Scheme_Object *scm_GetLightfv(void *p, int c, Scheme_Object **v)
{
	GLenum value = arg_GLenum(1);

	glGetLightfv(arg_GLenum  (0), value,
		     arg_GLfloatv(2, pnum_Light(value)));
	return scheme_void;
}

static Scheme_Object *scm_GetMaterialiv(void *p, int c, Scheme_Object **v)
{
	GLenum value = arg_GLenum(1);

	glGetMaterialiv(arg_GLenum(0), value,
			arg_GLintv(2, pnum_Material(value)));
	return scheme_void;
}

static Scheme_Object *scm_GetMaterialfv(void *p, int c, Scheme_Object **v)
{
	GLenum value = arg_GLenum(1);

	glGetMaterialfv(arg_GLenum  (0), value,
			arg_GLfloatv(2, pnum_Material(value)));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.4. Texture Queries						     */

static Scheme_Object *scm_IsTexture(void *p, int c, Scheme_Object **v)
{
	if (glIsTexture(arg_GLuint(0)))
		return scheme_true;
	else
		return scheme_false;
}

/*...........................................................................*/
/* 6.1.5. Stipple Query							     */

static Scheme_Object *scm_GetPolygonStipple(void *p, int c, Scheme_Object **v)
{
	glGetPolygonStipple(arg_GLubytev(0, (32 * 32 / 8)));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.7. Color Table Query						     */

#ifdef GL_VERSION_1_2

static Scheme_Object *scm_GetColorTableParameteriv(void *p,
						   int c, Scheme_Object **v)
{
	glGetColorTableParameteriv(arg_GLenum(0),
				   arg_GLenum(1),
				   arg_GLintv(2, 4));
	return scheme_void;
}

static Scheme_Object *scm_GetColorTableParameterfv(void *p,
						   int c, Scheme_Object **v)
{
	glGetColorTableParameterfv(arg_GLenum  (0),
				   arg_GLenum  (1),
				   arg_GLfloatv(2, 4));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.8. Convolution Filter						     */

static Scheme_Object *scm_GetConvolutionParameteriv(void *p,
						    int c, Scheme_Object **v)
{
	glGetConvolutionParameteriv(arg_GLenum(0),
				    arg_GLenum(1),
				    arg_GLintv(2, 4));
	return scheme_void;
}

static Scheme_Object *scm_GetConvolutionParameterfv(void *p,
						    int c, Scheme_Object **v)
{
	glGetConvolutionParameterfv(arg_GLenum  (0),
				    arg_GLenum  (1),
				    arg_GLfloatv(2, 4));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.9. Histogram Query						     */

static Scheme_Object *scm_ResetHistogram(void *p, int c, Scheme_Object **v)
{
	glResetHistogram(arg_GLenum(0));
	return scheme_void;
}

static Scheme_Object *scm_GetHistogramParameteriv(void *p,
						  int c, Scheme_Object **v)
{
	glGetHistogramParameteriv(arg_GLenum(0),
				  arg_GLenum(1),
				  arg_GLintv(2, 1));
	return scheme_void;
}

static Scheme_Object *scm_GetHistogramParameterfv(void *p,
						  int c, Scheme_Object **v)
{
	glGetHistogramParameterfv(arg_GLenum  (0),
				  arg_GLenum  (1),
				  arg_GLfloatv(2, 1));
	return scheme_void;
}

/*...........................................................................*/
/* 6.1.10. Minmax Query							     */

static Scheme_Object *scm_ResetMinmax(void *p, int c, Scheme_Object **v)
{
	glResetMinmax(arg_GLenum(0));
	return scheme_void;
}

static Scheme_Object *scm_GetMinmaxParameteriv(void *p,
					       int c, Scheme_Object **v)
{
	glGetMinmaxParameteriv(arg_GLenum(0),
			       arg_GLenum(1),
			       arg_GLintv(2, 1));
	return scheme_void;
}

static Scheme_Object *scm_GetMinmaxParameterfv(void *p,
					       int c, Scheme_Object **v)
{
	glGetMinmaxParameterfv(arg_GLenum  (0),
			       arg_GLenum  (1),
			       arg_GLfloatv(2, 1));
	return scheme_void;
}

#endif /* GL_VERSION_1_2 */

/*...........................................................................*/
/* 6.1.11. Pointer and String Queries					     */


static Scheme_Object *scm_GetString(void *p, int c, Scheme_Object **v)
{
	return scheme_make_string((char *) glGetString(arg_GLenum(0)));
}

/*...........................................................................*/
/* 6.1.12. Saving and Restoring State					     */

static Scheme_Object *scm_PushAttrib(void *p, int c, Scheme_Object **v)
{
	glPushAttrib(arg_GLbitfield(0));
	return scheme_void;
}

static Scheme_Object *scm_PushClientAttrib(void *p, int c, Scheme_Object **v)
{
	glPushClientAttrib(arg_GLbitfield(0));
	return scheme_void;
}

static Scheme_Object *scm_PopAttrib(void *p, int c, Scheme_Object **v)
{
	glPopAttrib();
	return scheme_void;
}

static Scheme_Object *scm_PopClientAttrib(void *p, int c, Scheme_Object **v)
{
	glPopClientAttrib();
	return scheme_void;
}

/*****************************************************************************/
/* Scheme environment initialization					     */

/*---------------------------------------------------------------------------*/
/* Enumerations								     */

#define MAKE_ENUM(e) { #e, e }

struct scm_enum
{
	const char *name;
	GLenum      val;
};

static const struct scm_enum scm_enum[] = {
	MAKE_ENUM(GL_2D),
	MAKE_ENUM(GL_2_BYTES),
	MAKE_ENUM(GL_3D),
	MAKE_ENUM(GL_3D_COLOR),
	MAKE_ENUM(GL_3D_COLOR_TEXTURE),
	MAKE_ENUM(GL_3_BYTES),
	MAKE_ENUM(GL_4D_COLOR_TEXTURE),
	MAKE_ENUM(GL_4_BYTES),
	MAKE_ENUM(GL_ACCUM),
	MAKE_ENUM(GL_ACCUM_ALPHA_BITS),
	MAKE_ENUM(GL_ACCUM_BLUE_BITS),
	MAKE_ENUM(GL_ACCUM_BUFFER_BIT),
	MAKE_ENUM(GL_ACCUM_CLEAR_VALUE),
	MAKE_ENUM(GL_ACCUM_GREEN_BITS),
	MAKE_ENUM(GL_ACCUM_RED_BITS),
	MAKE_ENUM(GL_ADD),
	MAKE_ENUM(GL_ALL_ATTRIB_BITS),
	MAKE_ENUM(GL_ALPHA),
	MAKE_ENUM(GL_ALPHA12),
	MAKE_ENUM(GL_ALPHA16),
	MAKE_ENUM(GL_ALPHA4),
	MAKE_ENUM(GL_ALPHA8),
	MAKE_ENUM(GL_ALPHA_BIAS),
	MAKE_ENUM(GL_ALPHA_BITS),
	MAKE_ENUM(GL_ALPHA_SCALE),
	MAKE_ENUM(GL_ALPHA_TEST),
	MAKE_ENUM(GL_ALPHA_TEST_FUNC),
	MAKE_ENUM(GL_ALPHA_TEST_REF),
	MAKE_ENUM(GL_ALWAYS),
	MAKE_ENUM(GL_AMBIENT),
	MAKE_ENUM(GL_AMBIENT_AND_DIFFUSE),
	MAKE_ENUM(GL_AND),
	MAKE_ENUM(GL_AND_INVERTED),
	MAKE_ENUM(GL_AND_REVERSE),
	MAKE_ENUM(GL_ATTRIB_STACK_DEPTH),
	MAKE_ENUM(GL_AUTO_NORMAL),
	MAKE_ENUM(GL_AUX0),
	MAKE_ENUM(GL_AUX1),
	MAKE_ENUM(GL_AUX2),
	MAKE_ENUM(GL_AUX3),
	MAKE_ENUM(GL_AUX_BUFFERS),
	MAKE_ENUM(GL_BACK),
	MAKE_ENUM(GL_BACK_LEFT),
	MAKE_ENUM(GL_BACK_RIGHT),
	MAKE_ENUM(GL_BITMAP),
	MAKE_ENUM(GL_BITMAP_TOKEN),
	MAKE_ENUM(GL_BLEND),
	MAKE_ENUM(GL_BLEND_DST),
	MAKE_ENUM(GL_BLEND_SRC),
	MAKE_ENUM(GL_BLUE),
	MAKE_ENUM(GL_BLUE_BIAS),
	MAKE_ENUM(GL_BLUE_BITS),
	MAKE_ENUM(GL_BLUE_SCALE),
	MAKE_ENUM(GL_BYTE),
	MAKE_ENUM(GL_C3F_V3F),
	MAKE_ENUM(GL_C4F_N3F_V3F),
	MAKE_ENUM(GL_C4UB_V2F),
	MAKE_ENUM(GL_C4UB_V3F),
	MAKE_ENUM(GL_CCW),
	MAKE_ENUM(GL_CLAMP),
	MAKE_ENUM(GL_CLEAR),
	MAKE_ENUM(GL_CLIENT_ALL_ATTRIB_BITS),
	MAKE_ENUM(GL_CLIENT_ATTRIB_STACK_DEPTH),
	MAKE_ENUM(GL_CLIENT_PIXEL_STORE_BIT),
	MAKE_ENUM(GL_CLIENT_VERTEX_ARRAY_BIT),
	MAKE_ENUM(GL_CLIP_PLANE0),
	MAKE_ENUM(GL_CLIP_PLANE1),
	MAKE_ENUM(GL_CLIP_PLANE2),
	MAKE_ENUM(GL_CLIP_PLANE3),
	MAKE_ENUM(GL_CLIP_PLANE4),
	MAKE_ENUM(GL_CLIP_PLANE5),
	MAKE_ENUM(GL_COEFF),
	MAKE_ENUM(GL_COLOR),
	MAKE_ENUM(GL_COLOR_ARRAY),
	MAKE_ENUM(GL_COLOR_ARRAY_POINTER),
	MAKE_ENUM(GL_COLOR_ARRAY_SIZE),
	MAKE_ENUM(GL_COLOR_ARRAY_STRIDE),
	MAKE_ENUM(GL_COLOR_ARRAY_TYPE),
	MAKE_ENUM(GL_COLOR_BUFFER_BIT),
	MAKE_ENUM(GL_COLOR_CLEAR_VALUE),
	MAKE_ENUM(GL_COLOR_INDEX),
	MAKE_ENUM(GL_COLOR_INDEXES),
	MAKE_ENUM(GL_COLOR_LOGIC_OP),
	MAKE_ENUM(GL_COLOR_MATERIAL),
	MAKE_ENUM(GL_COLOR_MATERIAL_FACE),
	MAKE_ENUM(GL_COLOR_MATERIAL_PARAMETER),
	MAKE_ENUM(GL_COLOR_WRITEMASK),
	MAKE_ENUM(GL_COMPILE),
	MAKE_ENUM(GL_COMPILE_AND_EXECUTE),
	MAKE_ENUM(GL_COPY),
	MAKE_ENUM(GL_COPY_INVERTED),
	MAKE_ENUM(GL_COPY_PIXEL_TOKEN),
	MAKE_ENUM(GL_CULL_FACE),
	MAKE_ENUM(GL_CULL_FACE_MODE),
	MAKE_ENUM(GL_CURRENT_BIT),
	MAKE_ENUM(GL_CURRENT_COLOR),
	MAKE_ENUM(GL_CURRENT_INDEX),
	MAKE_ENUM(GL_CURRENT_NORMAL),
	MAKE_ENUM(GL_CURRENT_RASTER_COLOR),
	MAKE_ENUM(GL_CURRENT_RASTER_DISTANCE),
	MAKE_ENUM(GL_CURRENT_RASTER_INDEX),
	MAKE_ENUM(GL_CURRENT_RASTER_POSITION),
	MAKE_ENUM(GL_CURRENT_RASTER_POSITION_VALID),
	MAKE_ENUM(GL_CURRENT_RASTER_TEXTURE_COORDS),
	MAKE_ENUM(GL_CURRENT_TEXTURE_COORDS),
	MAKE_ENUM(GL_CW),
	MAKE_ENUM(GL_DECAL),
	MAKE_ENUM(GL_DECR),
	MAKE_ENUM(GL_DEPTH),
	MAKE_ENUM(GL_DEPTH_BIAS),
	MAKE_ENUM(GL_DEPTH_BITS),
	MAKE_ENUM(GL_DEPTH_BUFFER_BIT),
	MAKE_ENUM(GL_DEPTH_CLEAR_VALUE),
	MAKE_ENUM(GL_DEPTH_COMPONENT),
	MAKE_ENUM(GL_DEPTH_FUNC),
	MAKE_ENUM(GL_DEPTH_RANGE),
	MAKE_ENUM(GL_DEPTH_SCALE),
	MAKE_ENUM(GL_DEPTH_TEST),
	MAKE_ENUM(GL_DEPTH_WRITEMASK),
	MAKE_ENUM(GL_DIFFUSE),
	MAKE_ENUM(GL_DITHER),
	MAKE_ENUM(GL_DOMAIN),
	MAKE_ENUM(GL_DONT_CARE),
	MAKE_ENUM(GL_DOUBLE),
	MAKE_ENUM(GL_DOUBLEBUFFER),
	MAKE_ENUM(GL_DRAW_BUFFER),
	MAKE_ENUM(GL_DRAW_PIXEL_TOKEN),
	MAKE_ENUM(GL_DST_ALPHA),
	MAKE_ENUM(GL_DST_COLOR),
	MAKE_ENUM(GL_EDGE_FLAG),
	MAKE_ENUM(GL_EDGE_FLAG_ARRAY),
	MAKE_ENUM(GL_EDGE_FLAG_ARRAY_POINTER),
	MAKE_ENUM(GL_EDGE_FLAG_ARRAY_STRIDE),
	MAKE_ENUM(GL_EMISSION),
	MAKE_ENUM(GL_ENABLE_BIT),
	MAKE_ENUM(GL_EQUAL),
	MAKE_ENUM(GL_EQUIV),
	MAKE_ENUM(GL_EVAL_BIT),
	MAKE_ENUM(GL_EXP),
	MAKE_ENUM(GL_EXP2),
	MAKE_ENUM(GL_EXTENSIONS),
	MAKE_ENUM(GL_EYE_LINEAR),
	MAKE_ENUM(GL_EYE_PLANE),
	MAKE_ENUM(GL_FALSE),
	MAKE_ENUM(GL_FASTEST),
	MAKE_ENUM(GL_FEEDBACK),
	MAKE_ENUM(GL_FEEDBACK_BUFFER_POINTER),
	MAKE_ENUM(GL_FEEDBACK_BUFFER_SIZE),
	MAKE_ENUM(GL_FEEDBACK_BUFFER_TYPE),
	MAKE_ENUM(GL_FILL),
	MAKE_ENUM(GL_FLAT),
	MAKE_ENUM(GL_FLOAT),
	MAKE_ENUM(GL_FOG),
	MAKE_ENUM(GL_FOG_BIT),
	MAKE_ENUM(GL_FOG_COLOR),
	MAKE_ENUM(GL_FOG_DENSITY),
	MAKE_ENUM(GL_FOG_END),
	MAKE_ENUM(GL_FOG_HINT),
	MAKE_ENUM(GL_FOG_INDEX),
	MAKE_ENUM(GL_FOG_MODE),
	MAKE_ENUM(GL_FOG_START),
	MAKE_ENUM(GL_FRONT),
	MAKE_ENUM(GL_FRONT_AND_BACK),
	MAKE_ENUM(GL_FRONT_FACE),
	MAKE_ENUM(GL_FRONT_LEFT),
	MAKE_ENUM(GL_FRONT_RIGHT),
	MAKE_ENUM(GL_GEQUAL),
	MAKE_ENUM(GL_GREATER),
	MAKE_ENUM(GL_GREEN),
	MAKE_ENUM(GL_GREEN_BIAS),
	MAKE_ENUM(GL_GREEN_BITS),
	MAKE_ENUM(GL_GREEN_SCALE),
	MAKE_ENUM(GL_HINT_BIT),
	MAKE_ENUM(GL_INCR),
	MAKE_ENUM(GL_INDEX_ARRAY),
	MAKE_ENUM(GL_INDEX_ARRAY_POINTER),
	MAKE_ENUM(GL_INDEX_ARRAY_STRIDE),
	MAKE_ENUM(GL_INDEX_ARRAY_TYPE),
	MAKE_ENUM(GL_INDEX_BITS),
	MAKE_ENUM(GL_INDEX_CLEAR_VALUE),
	MAKE_ENUM(GL_INDEX_LOGIC_OP),
	MAKE_ENUM(GL_INDEX_MODE),
	MAKE_ENUM(GL_INDEX_OFFSET),
	MAKE_ENUM(GL_INDEX_SHIFT),
	MAKE_ENUM(GL_INDEX_WRITEMASK),
	MAKE_ENUM(GL_INT),
	MAKE_ENUM(GL_INTENSITY),
	MAKE_ENUM(GL_INTENSITY12),
	MAKE_ENUM(GL_INTENSITY16),
	MAKE_ENUM(GL_INTENSITY4),
	MAKE_ENUM(GL_INTENSITY8),
	MAKE_ENUM(GL_INVALID_ENUM),
	MAKE_ENUM(GL_INVALID_OPERATION),
	MAKE_ENUM(GL_INVALID_VALUE),
	MAKE_ENUM(GL_INVERT),
	MAKE_ENUM(GL_KEEP),
	MAKE_ENUM(GL_LEFT),
	MAKE_ENUM(GL_LEQUAL),
	MAKE_ENUM(GL_LESS),
	MAKE_ENUM(GL_LIGHT0),
	MAKE_ENUM(GL_LIGHT1),
	MAKE_ENUM(GL_LIGHT2),
	MAKE_ENUM(GL_LIGHT3),
	MAKE_ENUM(GL_LIGHT4),
	MAKE_ENUM(GL_LIGHT5),
	MAKE_ENUM(GL_LIGHT6),
	MAKE_ENUM(GL_LIGHT7),
	MAKE_ENUM(GL_LIGHTING),
	MAKE_ENUM(GL_LIGHTING_BIT),
	MAKE_ENUM(GL_LIGHT_MODEL_AMBIENT),
	MAKE_ENUM(GL_LIGHT_MODEL_LOCAL_VIEWER),
	MAKE_ENUM(GL_LIGHT_MODEL_TWO_SIDE),
	MAKE_ENUM(GL_LINE),
	MAKE_ENUM(GL_LINEAR),
	MAKE_ENUM(GL_LINEAR_ATTENUATION),
	MAKE_ENUM(GL_LINEAR_MIPMAP_LINEAR),
	MAKE_ENUM(GL_LINEAR_MIPMAP_NEAREST),
	MAKE_ENUM(GL_LINES),
	MAKE_ENUM(GL_LINE_BIT),
	MAKE_ENUM(GL_LINE_LOOP),
	MAKE_ENUM(GL_LINE_RESET_TOKEN),
	MAKE_ENUM(GL_LINE_SMOOTH),
	MAKE_ENUM(GL_LINE_SMOOTH_HINT),
	MAKE_ENUM(GL_LINE_STIPPLE),
	MAKE_ENUM(GL_LINE_STIPPLE_PATTERN),
	MAKE_ENUM(GL_LINE_STIPPLE_REPEAT),
	MAKE_ENUM(GL_LINE_STRIP),
	MAKE_ENUM(GL_LINE_TOKEN),
	MAKE_ENUM(GL_LINE_WIDTH),
	MAKE_ENUM(GL_LINE_WIDTH_GRANULARITY),
	MAKE_ENUM(GL_LINE_WIDTH_RANGE),
	MAKE_ENUM(GL_LIST_BASE),
	MAKE_ENUM(GL_LIST_BIT),
	MAKE_ENUM(GL_LIST_INDEX),
	MAKE_ENUM(GL_LIST_MODE),
	MAKE_ENUM(GL_LOAD),
	MAKE_ENUM(GL_LOGIC_OP),
	MAKE_ENUM(GL_LOGIC_OP_MODE),
	MAKE_ENUM(GL_LUMINANCE),
	MAKE_ENUM(GL_LUMINANCE12),
	MAKE_ENUM(GL_LUMINANCE12_ALPHA12),
	MAKE_ENUM(GL_LUMINANCE12_ALPHA4),
	MAKE_ENUM(GL_LUMINANCE16),
	MAKE_ENUM(GL_LUMINANCE16_ALPHA16),
	MAKE_ENUM(GL_LUMINANCE4),
	MAKE_ENUM(GL_LUMINANCE4_ALPHA4),
	MAKE_ENUM(GL_LUMINANCE6_ALPHA2),
	MAKE_ENUM(GL_LUMINANCE8),
	MAKE_ENUM(GL_LUMINANCE8_ALPHA8),
	MAKE_ENUM(GL_LUMINANCE_ALPHA),
	MAKE_ENUM(GL_MAP1_COLOR_4),
	MAKE_ENUM(GL_MAP1_GRID_DOMAIN),
	MAKE_ENUM(GL_MAP1_GRID_SEGMENTS),
	MAKE_ENUM(GL_MAP1_INDEX),
	MAKE_ENUM(GL_MAP1_NORMAL),
	MAKE_ENUM(GL_MAP1_TEXTURE_COORD_1),
	MAKE_ENUM(GL_MAP1_TEXTURE_COORD_2),
	MAKE_ENUM(GL_MAP1_TEXTURE_COORD_3),
	MAKE_ENUM(GL_MAP1_TEXTURE_COORD_4),
	MAKE_ENUM(GL_MAP1_VERTEX_3),
	MAKE_ENUM(GL_MAP1_VERTEX_4),
	MAKE_ENUM(GL_MAP2_COLOR_4),
	MAKE_ENUM(GL_MAP2_GRID_DOMAIN),
	MAKE_ENUM(GL_MAP2_GRID_SEGMENTS),
	MAKE_ENUM(GL_MAP2_INDEX),
	MAKE_ENUM(GL_MAP2_NORMAL),
	MAKE_ENUM(GL_MAP2_TEXTURE_COORD_1),
	MAKE_ENUM(GL_MAP2_TEXTURE_COORD_2),
	MAKE_ENUM(GL_MAP2_TEXTURE_COORD_3),
	MAKE_ENUM(GL_MAP2_TEXTURE_COORD_4),
	MAKE_ENUM(GL_MAP2_VERTEX_3),
	MAKE_ENUM(GL_MAP2_VERTEX_4),
	MAKE_ENUM(GL_MAP_COLOR),
	MAKE_ENUM(GL_MAP_STENCIL),
	MAKE_ENUM(GL_MATRIX_MODE),
	MAKE_ENUM(GL_MODELVIEW),
	MAKE_ENUM(GL_MODELVIEW_MATRIX),
	MAKE_ENUM(GL_MODELVIEW_STACK_DEPTH),
	MAKE_ENUM(GL_MODULATE),
	MAKE_ENUM(GL_MULT),
	MAKE_ENUM(GL_N3F_V3F),
	MAKE_ENUM(GL_NAME_STACK_DEPTH),
	MAKE_ENUM(GL_NAND),
	MAKE_ENUM(GL_NEAREST),
	MAKE_ENUM(GL_NEAREST_MIPMAP_LINEAR),
	MAKE_ENUM(GL_NEAREST_MIPMAP_NEAREST),
	MAKE_ENUM(GL_NEVER),
	MAKE_ENUM(GL_NICEST),
	MAKE_ENUM(GL_NONE),
	MAKE_ENUM(GL_NOOP),
	MAKE_ENUM(GL_NOR),
	MAKE_ENUM(GL_NORMALIZE),
	MAKE_ENUM(GL_NORMAL_ARRAY),
	MAKE_ENUM(GL_NORMAL_ARRAY_POINTER),
	MAKE_ENUM(GL_NORMAL_ARRAY_STRIDE),
	MAKE_ENUM(GL_NORMAL_ARRAY_TYPE),
	MAKE_ENUM(GL_NOTEQUAL),
	MAKE_ENUM(GL_NO_ERROR),
	MAKE_ENUM(GL_OBJECT_LINEAR),
	MAKE_ENUM(GL_OBJECT_PLANE),
	MAKE_ENUM(GL_ONE),
	MAKE_ENUM(GL_ONE_MINUS_DST_ALPHA),
	MAKE_ENUM(GL_ONE_MINUS_DST_COLOR),
	MAKE_ENUM(GL_ONE_MINUS_SRC_ALPHA),
	MAKE_ENUM(GL_ONE_MINUS_SRC_COLOR),
	MAKE_ENUM(GL_OR),
	MAKE_ENUM(GL_ORDER),
	MAKE_ENUM(GL_OR_INVERTED),
	MAKE_ENUM(GL_OR_REVERSE),
	MAKE_ENUM(GL_OUT_OF_MEMORY),
	MAKE_ENUM(GL_PACK_ALIGNMENT),
	MAKE_ENUM(GL_PACK_LSB_FIRST),
	MAKE_ENUM(GL_PACK_ROW_LENGTH),
	MAKE_ENUM(GL_PACK_SKIP_PIXELS),
	MAKE_ENUM(GL_PACK_SKIP_ROWS),
	MAKE_ENUM(GL_PACK_SWAP_BYTES),
	MAKE_ENUM(GL_PASS_THROUGH_TOKEN),
	MAKE_ENUM(GL_PERSPECTIVE_CORRECTION_HINT),
	MAKE_ENUM(GL_PIXEL_MAP_A_TO_A),
	MAKE_ENUM(GL_PIXEL_MAP_A_TO_A_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_B_TO_B),
	MAKE_ENUM(GL_PIXEL_MAP_B_TO_B_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_G_TO_G),
	MAKE_ENUM(GL_PIXEL_MAP_G_TO_G_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_A),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_A_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_B),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_B_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_G),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_G_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_I),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_I_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_R),
	MAKE_ENUM(GL_PIXEL_MAP_I_TO_R_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_R_TO_R),
	MAKE_ENUM(GL_PIXEL_MAP_R_TO_R_SIZE),
	MAKE_ENUM(GL_PIXEL_MAP_S_TO_S),
	MAKE_ENUM(GL_PIXEL_MAP_S_TO_S_SIZE),
	MAKE_ENUM(GL_PIXEL_MODE_BIT),
	MAKE_ENUM(GL_POINT),
	MAKE_ENUM(GL_POINTS),
	MAKE_ENUM(GL_POINT_BIT),
	MAKE_ENUM(GL_POINT_SIZE),
	MAKE_ENUM(GL_POINT_SIZE_GRANULARITY),
	MAKE_ENUM(GL_POINT_SIZE_RANGE),
	MAKE_ENUM(GL_POINT_SMOOTH),
	MAKE_ENUM(GL_POINT_SMOOTH_HINT),
	MAKE_ENUM(GL_POINT_TOKEN),
	MAKE_ENUM(GL_POLYGON),
	MAKE_ENUM(GL_POLYGON_BIT),
	MAKE_ENUM(GL_POLYGON_MODE),
	MAKE_ENUM(GL_POLYGON_OFFSET_FACTOR),
	MAKE_ENUM(GL_POLYGON_OFFSET_FILL),
	MAKE_ENUM(GL_POLYGON_OFFSET_LINE),
	MAKE_ENUM(GL_POLYGON_OFFSET_POINT),
	MAKE_ENUM(GL_POLYGON_OFFSET_UNITS),
	MAKE_ENUM(GL_POLYGON_SMOOTH),
	MAKE_ENUM(GL_POLYGON_SMOOTH_HINT),
	MAKE_ENUM(GL_POLYGON_STIPPLE),
	MAKE_ENUM(GL_POLYGON_STIPPLE_BIT),
	MAKE_ENUM(GL_POLYGON_TOKEN),
	MAKE_ENUM(GL_POSITION),
	MAKE_ENUM(GL_PROJECTION),
	MAKE_ENUM(GL_PROJECTION_MATRIX),
	MAKE_ENUM(GL_PROJECTION_STACK_DEPTH),
	MAKE_ENUM(GL_PROXY_TEXTURE_1D),
	MAKE_ENUM(GL_PROXY_TEXTURE_2D),
	MAKE_ENUM(GL_Q),
	MAKE_ENUM(GL_QUADRATIC_ATTENUATION),
	MAKE_ENUM(GL_QUADS),
	MAKE_ENUM(GL_QUAD_STRIP),
	MAKE_ENUM(GL_R),
	MAKE_ENUM(GL_R3_G3_B2),
	MAKE_ENUM(GL_READ_BUFFER),
	MAKE_ENUM(GL_RED),
	MAKE_ENUM(GL_RED_BIAS),
	MAKE_ENUM(GL_RED_BITS),
	MAKE_ENUM(GL_RED_SCALE),
	MAKE_ENUM(GL_RENDER),
	MAKE_ENUM(GL_RENDERER),
	MAKE_ENUM(GL_RENDER_MODE),
	MAKE_ENUM(GL_REPEAT),
	MAKE_ENUM(GL_REPLACE),
	MAKE_ENUM(GL_RETURN),
	MAKE_ENUM(GL_RGB),
	MAKE_ENUM(GL_RGB10),
	MAKE_ENUM(GL_RGB10_A2),
	MAKE_ENUM(GL_RGB12),
	MAKE_ENUM(GL_RGB16),
	MAKE_ENUM(GL_RGB4),
	MAKE_ENUM(GL_RGB5),
	MAKE_ENUM(GL_RGB5_A1),
	MAKE_ENUM(GL_RGB8),
	MAKE_ENUM(GL_RGBA),
	MAKE_ENUM(GL_RGBA12),
	MAKE_ENUM(GL_RGBA16),
	MAKE_ENUM(GL_RGBA2),
	MAKE_ENUM(GL_RGBA4),
	MAKE_ENUM(GL_RGBA8),
	MAKE_ENUM(GL_RGBA_MODE),
	MAKE_ENUM(GL_RIGHT),
	MAKE_ENUM(GL_S),
	MAKE_ENUM(GL_SCISSOR_BIT),
	MAKE_ENUM(GL_SCISSOR_BOX),
	MAKE_ENUM(GL_SCISSOR_TEST),
	MAKE_ENUM(GL_SELECT),
	MAKE_ENUM(GL_SELECTION_BUFFER_POINTER),
	MAKE_ENUM(GL_SELECTION_BUFFER_SIZE),
	MAKE_ENUM(GL_SET),
	MAKE_ENUM(GL_SHADE_MODEL),
	MAKE_ENUM(GL_SHININESS),
	MAKE_ENUM(GL_SHORT),
	MAKE_ENUM(GL_SMOOTH),
	MAKE_ENUM(GL_SPECULAR),
	MAKE_ENUM(GL_SPHERE_MAP),
	MAKE_ENUM(GL_SPOT_CUTOFF),
	MAKE_ENUM(GL_SPOT_DIRECTION),
	MAKE_ENUM(GL_SPOT_EXPONENT),
	MAKE_ENUM(GL_SRC_ALPHA),
	MAKE_ENUM(GL_SRC_ALPHA_SATURATE),
	MAKE_ENUM(GL_SRC_COLOR),
	MAKE_ENUM(GL_STACK_OVERFLOW),
	MAKE_ENUM(GL_STACK_UNDERFLOW),
	MAKE_ENUM(GL_STENCIL),
	MAKE_ENUM(GL_STENCIL_BITS),
	MAKE_ENUM(GL_STENCIL_BUFFER_BIT),
	MAKE_ENUM(GL_STENCIL_CLEAR_VALUE),
	MAKE_ENUM(GL_STENCIL_FAIL),
	MAKE_ENUM(GL_STENCIL_FUNC),
	MAKE_ENUM(GL_STENCIL_INDEX),
	MAKE_ENUM(GL_STENCIL_PASS_DEPTH_FAIL),
	MAKE_ENUM(GL_STENCIL_PASS_DEPTH_PASS),
	MAKE_ENUM(GL_STENCIL_REF),
	MAKE_ENUM(GL_STENCIL_TEST),
	MAKE_ENUM(GL_STENCIL_VALUE_MASK),
	MAKE_ENUM(GL_STENCIL_WRITEMASK),
	MAKE_ENUM(GL_STEREO),
	MAKE_ENUM(GL_SUBPIXEL_BITS),
	MAKE_ENUM(GL_T),
	MAKE_ENUM(GL_T2F_C3F_V3F),
	MAKE_ENUM(GL_T2F_C4F_N3F_V3F),
	MAKE_ENUM(GL_T2F_C4UB_V3F),
	MAKE_ENUM(GL_T2F_N3F_V3F),
	MAKE_ENUM(GL_T2F_V3F),
	MAKE_ENUM(GL_T4F_C4F_N3F_V4F),
	MAKE_ENUM(GL_T4F_V4F),
	MAKE_ENUM(GL_TEXTURE),
	MAKE_ENUM(GL_TEXTURE_1D),
	MAKE_ENUM(GL_TEXTURE_2D),
	MAKE_ENUM(GL_TEXTURE_ALPHA_SIZE),
	MAKE_ENUM(GL_TEXTURE_BINDING_1D),
	MAKE_ENUM(GL_TEXTURE_BINDING_2D),
	MAKE_ENUM(GL_TEXTURE_BIT),
	MAKE_ENUM(GL_TEXTURE_BLUE_SIZE),
	MAKE_ENUM(GL_TEXTURE_BORDER),
	MAKE_ENUM(GL_TEXTURE_BORDER_COLOR),
	MAKE_ENUM(GL_TEXTURE_COMPONENTS),
	MAKE_ENUM(GL_TEXTURE_COORD_ARRAY),
	MAKE_ENUM(GL_TEXTURE_COORD_ARRAY_POINTER),
	MAKE_ENUM(GL_TEXTURE_COORD_ARRAY_SIZE),
	MAKE_ENUM(GL_TEXTURE_COORD_ARRAY_STRIDE),
	MAKE_ENUM(GL_TEXTURE_COORD_ARRAY_TYPE),
	MAKE_ENUM(GL_TEXTURE_ENV),
	MAKE_ENUM(GL_TEXTURE_ENV_COLOR),
	MAKE_ENUM(GL_TEXTURE_ENV_MODE),
	MAKE_ENUM(GL_TEXTURE_GEN_MODE),
	MAKE_ENUM(GL_TEXTURE_GEN_Q),
	MAKE_ENUM(GL_TEXTURE_GEN_R),
	MAKE_ENUM(GL_TEXTURE_GEN_S),
	MAKE_ENUM(GL_TEXTURE_GEN_T),
	MAKE_ENUM(GL_TEXTURE_GREEN_SIZE),
	MAKE_ENUM(GL_TEXTURE_HEIGHT),
	MAKE_ENUM(GL_TEXTURE_INTENSITY_SIZE),
	MAKE_ENUM(GL_TEXTURE_INTERNAL_FORMAT),
	MAKE_ENUM(GL_TEXTURE_LUMINANCE_SIZE),
	MAKE_ENUM(GL_TEXTURE_MAG_FILTER),
	MAKE_ENUM(GL_TEXTURE_MATRIX),
	MAKE_ENUM(GL_TEXTURE_MIN_FILTER),
	MAKE_ENUM(GL_TEXTURE_PRIORITY),
	MAKE_ENUM(GL_TEXTURE_RED_SIZE),
	MAKE_ENUM(GL_TEXTURE_RESIDENT),
	MAKE_ENUM(GL_TEXTURE_STACK_DEPTH),
	MAKE_ENUM(GL_TEXTURE_WIDTH),
	MAKE_ENUM(GL_TEXTURE_WRAP_S),
	MAKE_ENUM(GL_TEXTURE_WRAP_T),
	MAKE_ENUM(GL_TRANSFORM_BIT),
	MAKE_ENUM(GL_TRIANGLES),
	MAKE_ENUM(GL_TRIANGLE_FAN),
	MAKE_ENUM(GL_TRIANGLE_STRIP),
	MAKE_ENUM(GL_TRUE),
	MAKE_ENUM(GL_UNPACK_ALIGNMENT),
	MAKE_ENUM(GL_UNPACK_LSB_FIRST),
	MAKE_ENUM(GL_UNPACK_ROW_LENGTH),
	MAKE_ENUM(GL_UNPACK_SKIP_PIXELS),
	MAKE_ENUM(GL_UNPACK_SKIP_ROWS),
	MAKE_ENUM(GL_UNPACK_SWAP_BYTES),
	MAKE_ENUM(GL_UNSIGNED_BYTE),
	MAKE_ENUM(GL_UNSIGNED_INT),
	MAKE_ENUM(GL_UNSIGNED_SHORT),
	MAKE_ENUM(GL_V2F),
	MAKE_ENUM(GL_V3F),
	MAKE_ENUM(GL_VENDOR),
	MAKE_ENUM(GL_VERSION),
	MAKE_ENUM(GL_VERTEX_ARRAY),
	MAKE_ENUM(GL_VERTEX_ARRAY_POINTER),
	MAKE_ENUM(GL_VERTEX_ARRAY_SIZE),
	MAKE_ENUM(GL_VERTEX_ARRAY_STRIDE),
	MAKE_ENUM(GL_VERTEX_ARRAY_TYPE),
	MAKE_ENUM(GL_VIEWPORT),
	MAKE_ENUM(GL_VIEWPORT_BIT),
	MAKE_ENUM(GL_XOR),
	MAKE_ENUM(GL_ZERO),
	MAKE_ENUM(GL_ZOOM_X),
	MAKE_ENUM(GL_ZOOM_Y),

#ifdef GL_VERSION_1_2
	MAKE_ENUM(GL_ALIASED_LINE_WIDTH_RANGE),
	MAKE_ENUM(GL_ALIASED_POINT_SIZE_RANGE),
	MAKE_ENUM(GL_BGR),
	MAKE_ENUM(GL_BGRA),
	MAKE_ENUM(GL_BLEND_COLOR),
	MAKE_ENUM(GL_BLEND_EQUATION),
	MAKE_ENUM(GL_CLAMP_TO_EDGE),
	MAKE_ENUM(GL_COLOR_MATRIX),
	MAKE_ENUM(GL_COLOR_MATRIX_STACK_DEPTH),
	MAKE_ENUM(GL_COLOR_TABLE),
	MAKE_ENUM(GL_COLOR_TABLE_ALPHA_SIZE),
	MAKE_ENUM(GL_COLOR_TABLE_BIAS),
	MAKE_ENUM(GL_COLOR_TABLE_BLUE_SIZE),
	MAKE_ENUM(GL_COLOR_TABLE_FORMAT),
	MAKE_ENUM(GL_COLOR_TABLE_GREEN_SIZE),
	MAKE_ENUM(GL_COLOR_TABLE_INTENSITY_SIZE),
	MAKE_ENUM(GL_COLOR_TABLE_LUMINANCE_SIZE),
	MAKE_ENUM(GL_COLOR_TABLE_RED_SIZE),
	MAKE_ENUM(GL_COLOR_TABLE_SCALE),
	MAKE_ENUM(GL_COLOR_TABLE_WIDTH),
	MAKE_ENUM(GL_CONSTANT_ALPHA),
	MAKE_ENUM(GL_CONSTANT_ATTENUATION),
	MAKE_ENUM(GL_CONSTANT_BORDER),
	MAKE_ENUM(GL_CONSTANT_COLOR),
	MAKE_ENUM(GL_CONVOLUTION_1D),
	MAKE_ENUM(GL_CONVOLUTION_2D),
	MAKE_ENUM(GL_CONVOLUTION_BORDER_COLOR),
	MAKE_ENUM(GL_CONVOLUTION_BORDER_MODE),
	MAKE_ENUM(GL_CONVOLUTION_FILTER_BIAS),
	MAKE_ENUM(GL_CONVOLUTION_FILTER_SCALE),
	MAKE_ENUM(GL_CONVOLUTION_FORMAT),
	MAKE_ENUM(GL_CONVOLUTION_HEIGHT),
	MAKE_ENUM(GL_CONVOLUTION_WIDTH),
	MAKE_ENUM(GL_FUNC_ADD),
	MAKE_ENUM(GL_FUNC_REVERSE_SUBTRACT),
	MAKE_ENUM(GL_FUNC_SUBTRACT),
	MAKE_ENUM(GL_HISTOGRAM),
	MAKE_ENUM(GL_HISTOGRAM_ALPHA_SIZE),
	MAKE_ENUM(GL_HISTOGRAM_BLUE_SIZE),
	MAKE_ENUM(GL_HISTOGRAM_FORMAT),
	MAKE_ENUM(GL_HISTOGRAM_GREEN_SIZE),
	MAKE_ENUM(GL_HISTOGRAM_LUMINANCE_SIZE),
	MAKE_ENUM(GL_HISTOGRAM_RED_SIZE),
	MAKE_ENUM(GL_HISTOGRAM_SINK),
	MAKE_ENUM(GL_HISTOGRAM_WIDTH),
#ifdef GL_IGNORE_BORDER
        MAKE_ENUM(GL_IGNORE_BORDER),
#endif
        MAKE_ENUM(GL_LIGHT_MODEL_COLOR_CONTROL),
	MAKE_ENUM(GL_MAX),
	MAKE_ENUM(GL_MAX_3D_TEXTURE_SIZE),
	MAKE_ENUM(GL_MAX_ATTRIB_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_CLIENT_ATTRIB_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_CLIP_PLANES),
	MAKE_ENUM(GL_MAX_COLOR_MATRIX_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_CONVOLUTION_HEIGHT),
	MAKE_ENUM(GL_MAX_CONVOLUTION_WIDTH),
	MAKE_ENUM(GL_MAX_ELEMENTS_INDICES),
	MAKE_ENUM(GL_MAX_ELEMENTS_VERTICES),
	MAKE_ENUM(GL_MAX_EVAL_ORDER),
	MAKE_ENUM(GL_MAX_LIGHTS),
	MAKE_ENUM(GL_MAX_LIST_NESTING),
	MAKE_ENUM(GL_MAX_MODELVIEW_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_NAME_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_PIXEL_MAP_TABLE),
	MAKE_ENUM(GL_MAX_PROJECTION_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_TEXTURE_SIZE),
	MAKE_ENUM(GL_MAX_TEXTURE_STACK_DEPTH),
	MAKE_ENUM(GL_MAX_VIEWPORT_DIMS),
	MAKE_ENUM(GL_MIN),
	MAKE_ENUM(GL_MINMAX),
	MAKE_ENUM(GL_MINMAX_FORMAT),
	MAKE_ENUM(GL_MINMAX_SINK),
	MAKE_ENUM(GL_ONE_MINUS_CONSTANT_ALPHA),
	MAKE_ENUM(GL_ONE_MINUS_CONSTANT_COLOR),
	MAKE_ENUM(GL_PACK_IMAGE_HEIGHT),
	MAKE_ENUM(GL_PACK_SKIP_IMAGES),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_ALPHA_BIAS),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_ALPHA_SCALE),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_BLUE_BIAS),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_BLUE_SCALE),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_COLOR_TABLE),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_GREEN_BIAS),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_GREEN_SCALE),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_RED_BIAS),
	MAKE_ENUM(GL_POST_COLOR_MATRIX_RED_SCALE),
	MAKE_ENUM(GL_POST_CONVOLUTION_ALPHA_BIAS),
	MAKE_ENUM(GL_POST_CONVOLUTION_ALPHA_SCALE),
	MAKE_ENUM(GL_POST_CONVOLUTION_BLUE_BIAS),
	MAKE_ENUM(GL_POST_CONVOLUTION_BLUE_SCALE),
	MAKE_ENUM(GL_POST_CONVOLUTION_COLOR_TABLE),
	MAKE_ENUM(GL_POST_CONVOLUTION_GREEN_BIAS),
	MAKE_ENUM(GL_POST_CONVOLUTION_GREEN_SCALE),
	MAKE_ENUM(GL_POST_CONVOLUTION_RED_BIAS),
	MAKE_ENUM(GL_POST_CONVOLUTION_RED_SCALE),
	MAKE_ENUM(GL_PROXY_COLOR_TABLE),
	MAKE_ENUM(GL_PROXY_HISTOGRAM),
	MAKE_ENUM(GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE),
	MAKE_ENUM(GL_PROXY_POST_CONVOLUTION_COLOR_TABLE),
	MAKE_ENUM(GL_PROXY_TEXTURE_3D),
	MAKE_ENUM(GL_REDUCE),
	MAKE_ENUM(GL_REPLICATE_BORDER),
	MAKE_ENUM(GL_RESCALE_NORMAL),
	MAKE_ENUM(GL_SEPARABLE_2D),
	MAKE_ENUM(GL_SEPARATE_SPECULAR_COLOR),
	MAKE_ENUM(GL_SINGLE_COLOR),
	MAKE_ENUM(GL_SMOOTH_LINE_WIDTH_GRANULARITY),
	MAKE_ENUM(GL_SMOOTH_LINE_WIDTH_RANGE),
	MAKE_ENUM(GL_SMOOTH_POINT_SIZE_GRANULARITY),
	MAKE_ENUM(GL_SMOOTH_POINT_SIZE_RANGE),
	MAKE_ENUM(GL_TABLE_TOO_LARGE),
	MAKE_ENUM(GL_TEXTURE_3D),
	MAKE_ENUM(GL_TEXTURE_BASE_LEVEL),
#ifdef GL_TEXTURE_BINDING_3D
        MAKE_ENUM(GL_TEXTURE_BINDING_3D),
#endif
        MAKE_ENUM(GL_TEXTURE_DEPTH),
	MAKE_ENUM(GL_TEXTURE_MAX_LEVEL),
	MAKE_ENUM(GL_TEXTURE_MAX_LOD),
	MAKE_ENUM(GL_TEXTURE_MIN_LOD),
	MAKE_ENUM(GL_TEXTURE_WRAP_R),
	MAKE_ENUM(GL_UNSIGNED_BYTE_2_3_3_REV),
	MAKE_ENUM(GL_UNSIGNED_BYTE_3_3_2),
	MAKE_ENUM(GL_UNSIGNED_INT_10_10_10_2),
	MAKE_ENUM(GL_UNSIGNED_INT_2_10_10_10_REV),
	MAKE_ENUM(GL_UNSIGNED_INT_8_8_8_8),
	MAKE_ENUM(GL_UNSIGNED_INT_8_8_8_8_REV),
	MAKE_ENUM(GL_UNSIGNED_SHORT_1_5_5_5_REV),
	MAKE_ENUM(GL_UNSIGNED_SHORT_4_4_4_4),
	MAKE_ENUM(GL_UNSIGNED_SHORT_4_4_4_4_REV),
	MAKE_ENUM(GL_UNSIGNED_SHORT_5_5_5_1),
	MAKE_ENUM(GL_UNSIGNED_SHORT_5_6_5),
	MAKE_ENUM(GL_UNSIGNED_SHORT_5_6_5_REV),

#endif /* GL_VERSION_1_2 */

#ifdef GL_VERSION_1_3

	MAKE_ENUM(GL_ACTIVE_TEXTURE),
	MAKE_ENUM(GL_ADD_SIGNED),
	MAKE_ENUM(GL_CLAMP_TO_BORDER),
	MAKE_ENUM(GL_CLIENT_ACTIVE_TEXTURE),
	MAKE_ENUM(GL_COMBINE),
	MAKE_ENUM(GL_COMBINE_ALPHA),
	MAKE_ENUM(GL_COMBINE_RGB),
	MAKE_ENUM(GL_COMPRESSED_ALPHA),
	MAKE_ENUM(GL_COMPRESSED_INTENSITY),
	MAKE_ENUM(GL_COMPRESSED_LUMINANCE),
	MAKE_ENUM(GL_COMPRESSED_LUMINANCE_ALPHA),
	MAKE_ENUM(GL_COMPRESSED_RGB),
	MAKE_ENUM(GL_COMPRESSED_RGBA),
	MAKE_ENUM(GL_COMPRESSED_TEXTURE_FORMATS),
	MAKE_ENUM(GL_CONSTANT),
	MAKE_ENUM(GL_DOT3_RGB),
	MAKE_ENUM(GL_DOT3_RGBA),
	MAKE_ENUM(GL_INTERPOLATE),
	MAKE_ENUM(GL_MAX_CUBE_MAP_TEXTURE_SIZE),
	MAKE_ENUM(GL_MAX_TEXTURE_UNITS),
	MAKE_ENUM(GL_MULTISAMPLE),
	MAKE_ENUM(GL_MULTISAMPLE_BIT),
	MAKE_ENUM(GL_NORMAL_MAP),
	MAKE_ENUM(GL_NUM_COMPRESSED_TEXTURE_FORMATS),
	MAKE_ENUM(GL_OPERAND0_ALPHA),
	MAKE_ENUM(GL_OPERAND0_RGB),
	MAKE_ENUM(GL_OPERAND1_ALPHA),
	MAKE_ENUM(GL_OPERAND1_RGB),
	MAKE_ENUM(GL_OPERAND2_ALPHA),
	MAKE_ENUM(GL_OPERAND2_RGB),
	MAKE_ENUM(GL_PREVIOUS),
	MAKE_ENUM(GL_PRIMARY_COLOR),
	MAKE_ENUM(GL_PROXY_TEXTURE_CUBE_MAP),
	MAKE_ENUM(GL_REFLECTION_MAP),
	MAKE_ENUM(GL_RGB_SCALE),
	MAKE_ENUM(GL_SAMPLES),
	MAKE_ENUM(GL_SAMPLE_ALPHA_TO_COVERAGE),
	MAKE_ENUM(GL_SAMPLE_ALPHA_TO_ONE),
	MAKE_ENUM(GL_SAMPLE_BUFFERS),
	MAKE_ENUM(GL_SAMPLE_COVERAGE),
	MAKE_ENUM(GL_SAMPLE_COVERAGE_INVERT),
	MAKE_ENUM(GL_SAMPLE_COVERAGE_VALUE),
	MAKE_ENUM(GL_SOURCE0_ALPHA),
	MAKE_ENUM(GL_SOURCE0_RGB),
	MAKE_ENUM(GL_SOURCE1_ALPHA),
	MAKE_ENUM(GL_SOURCE1_RGB),
	MAKE_ENUM(GL_SOURCE2_ALPHA),
	MAKE_ENUM(GL_SOURCE2_RGB),
	MAKE_ENUM(GL_SUBTRACT),
	MAKE_ENUM(GL_TEXTURE0),
	MAKE_ENUM(GL_TEXTURE1),
	MAKE_ENUM(GL_TEXTURE10),
	MAKE_ENUM(GL_TEXTURE11),
	MAKE_ENUM(GL_TEXTURE12),
	MAKE_ENUM(GL_TEXTURE13),
	MAKE_ENUM(GL_TEXTURE14),
	MAKE_ENUM(GL_TEXTURE15),
	MAKE_ENUM(GL_TEXTURE16),
	MAKE_ENUM(GL_TEXTURE17),
	MAKE_ENUM(GL_TEXTURE18),
	MAKE_ENUM(GL_TEXTURE19),
	MAKE_ENUM(GL_TEXTURE2),
	MAKE_ENUM(GL_TEXTURE20),
	MAKE_ENUM(GL_TEXTURE21),
	MAKE_ENUM(GL_TEXTURE22),
	MAKE_ENUM(GL_TEXTURE23),
	MAKE_ENUM(GL_TEXTURE24),
	MAKE_ENUM(GL_TEXTURE25),
	MAKE_ENUM(GL_TEXTURE26),
	MAKE_ENUM(GL_TEXTURE27),
	MAKE_ENUM(GL_TEXTURE28),
	MAKE_ENUM(GL_TEXTURE29),
	MAKE_ENUM(GL_TEXTURE3),
	MAKE_ENUM(GL_TEXTURE30),
	MAKE_ENUM(GL_TEXTURE31),
	MAKE_ENUM(GL_TEXTURE4),
	MAKE_ENUM(GL_TEXTURE5),
	MAKE_ENUM(GL_TEXTURE6),
	MAKE_ENUM(GL_TEXTURE7),
	MAKE_ENUM(GL_TEXTURE8),
	MAKE_ENUM(GL_TEXTURE9),
	MAKE_ENUM(GL_TEXTURE_BINDING_CUBE_MAP),
	MAKE_ENUM(GL_TEXTURE_COMPRESSED),
#ifdef GL_TEXTURE_COMPRESSED_IMAGE_SIZE
        MAKE_ENUM(GL_TEXTURE_COMPRESSED_IMAGE_SIZE),
#endif
        MAKE_ENUM(GL_TEXTURE_COMPRESSION_HINT),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP_NEGATIVE_X),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP_POSITIVE_X),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP_POSITIVE_Y),
	MAKE_ENUM(GL_TEXTURE_CUBE_MAP_POSITIVE_Z),
	MAKE_ENUM(GL_TRANSPOSE_COLOR_MATRIX),
	MAKE_ENUM(GL_TRANSPOSE_MODELVIEW_MATRIX),
	MAKE_ENUM(GL_TRANSPOSE_PROJECTION_MATRIX),
	MAKE_ENUM(GL_TRANSPOSE_TEXTURE_MATRIX),
	MAKE_ENUM(GL_UNPACK_IMAGE_HEIGHT),
	MAKE_ENUM(GL_UNPACK_SKIP_IMAGES),

#endif /* GL_VERSION_1_3 */
};

void scheme_load_enum(Scheme_Env *env)
{
	Scheme_Object *val;
	int i, N = sizeof (scm_enum) / sizeof (struct scm_enum);

	/* printf("%d enumerations\n", N); */

	for (i = 0; i < N; i++)
	{
		val = scheme_make_integer_value_from_unsigned((unsigned int)
							      scm_enum[i].val);
		scheme_add_global(scm_enum[i].name, val, env);
	}
}

/*---------------------------------------------------------------------------*/
/* Primative Procedures							     */


static const struct scm_prim scm_prim[] = {

	{ "glAccum",			scm_Accum,			2, 2 },
	{ "glAlphaFunc",		scm_AlphaFunc,			2, 2 },
	{ "glAreTexturesResident",	scm_AreTexturesResident,	3, 3 },
	{ "glBegin",			scm_Begin,			1, 1 },
	{ "glBindTexture",		scm_BindTexture,		2, 2 },
	{ "glBitmap",			scm_Bitmap,			7, 7 },
	{ "glBlendFunc",		scm_BlendFunc,			2, 2 },
	{ "glCallList",			scm_CallList,			1, 1 },
	{ "glCallLists",		scm_CallLists,			3, 3 },
	{ "glClear",			scm_Clear,			1, 1 },
	{ "glClearAccum",		scm_ClearAccum,			4, 4 },
	{ "glClearColor",		scm_ClearColor,			4, 4 },
	{ "glClearDepth",		scm_ClearDepth,			1, 1 },
	{ "glClearIndex",		scm_ClearIndex,			1, 1 },
	{ "glClearStencil",		scm_ClearStencil,		1, 1 },
	{ "glClipPlane",		scm_ClipPlane,			2, 2 },
	{ "glColor3b",			scm_Color3b,			3, 3 },
	{ "glColor3bv",			scm_Color3bv,			1, 1 },
	{ "glColor3d",			scm_Color3d,			3, 3 },
	{ "glColor3dv",			scm_Color3dv,			1, 1 },
	{ "glColor3f",			scm_Color3f,			3, 3 },
	{ "glColor3fv",			scm_Color3fv,			1, 1 },
	{ "glColor3i",			scm_Color3i,			3, 3 },
	{ "glColor3iv",			scm_Color3iv,			1, 1 },
	{ "glColor3s",			scm_Color3s,			3, 3 },
	{ "glColor3sv",			scm_Color3sv,			1, 1 },
	{ "glColor3ub",			scm_Color3ub,			3, 3 },
	{ "glColor3ubv",		scm_Color3ubv,			1, 1 },
	{ "glColor3ui",			scm_Color3ui,			3, 3 },
	{ "glColor3uiv",		scm_Color3uiv,			1, 1 },
	{ "glColor3us",			scm_Color3us,			3, 3 },
	{ "glColor3usv",		scm_Color3usv,			1, 1 },
	{ "glColor4b",			scm_Color4b,			4, 4 },
	{ "glColor4bv",			scm_Color4bv,			1, 1 },
	{ "glColor4d",			scm_Color4d,			4, 4 },
	{ "glColor4dv",			scm_Color4dv,			1, 1 },
	{ "glColor4f",			scm_Color4f,			4, 4 },
	{ "glColor4fv",			scm_Color4fv,			1, 1 },
	{ "glColor4i",			scm_Color4i,			4, 4 },
	{ "glColor4iv",			scm_Color4iv,			1, 1 },
	{ "glColor4s",			scm_Color4s,			4, 4 },
	{ "glColor4sv",			scm_Color4sv,			1, 1 },
	{ "glColor4ub",			scm_Color4ub,			4, 4 },
	{ "glColor4ubv",		scm_Color4ubv,			1, 1 },
	{ "glColor4ui",			scm_Color4ui,			4, 4 },
	{ "glColor4uiv",		scm_Color4uiv,			1, 1 },
	{ "glColor4us",			scm_Color4us,			4, 4 },
	{ "glColor4usv",		scm_Color4usv,			1, 1 },
	{ "glColorMask",		scm_ColorMask,			4, 4 },
	{ "glColorMaterial",		scm_ColorMaterial,		2, 2 },
	{ "glCopyPixels",		scm_CopyPixels,			5, 5 },
	{ "glCopyTexImage1D",		scm_CopyTexImage1D,		7, 7 },
	{ "glCopyTexImage2D",		scm_CopyTexImage2D,		8, 8 },
	{ "glCopyTexSubImage1D",	scm_CopyTexSubImage1D,		6, 6 },
	{ "glCopyTexSubImage2D",	scm_CopyTexSubImage2D,		8, 8 },
	{ "glCullFace",			scm_CullFace,			1, 1 },
	{ "glDeleteLists",		scm_DeleteLists,		2, 2 },
	{ "glDeleteTextures",		scm_DeleteTextures,		2, 2 },
	{ "glDepthFunc",		scm_DepthFunc,			1, 1 },
	{ "glDepthMask",		scm_DepthMask,			1, 1 },
	{ "glDepthRange",		scm_DepthRange,			2, 2 },
	{ "glDisable",			scm_Disable,			1, 1 },
	{ "glDisableClientState",	scm_DisableClientState,		1, 1 },
	{ "glDrawBuffer",		scm_DrawBuffer,			1, 1 },
	{ "glDrawPixels",		scm_DrawPixels,			5, 5 },
	{ "glEdgeFlag",			scm_EdgeFlag,			1, 1 },
	{ "glEdgeFlagv",		scm_EdgeFlagv,			1, 1 },
	{ "glEnable",			scm_Enable,			1, 1 },
	{ "glEnableClientState",	scm_EnableClientState,		1, 1 },
	{ "glEnd",			scm_End,			0, 0 },
	{ "glEndList",			scm_EndList,			0, 0 },
	{ "glEvalCoord1d",		scm_EvalCoord1d,		1, 1 },
	{ "glEvalCoord1dv",		scm_EvalCoord1dv,		1, 1 },
	{ "glEvalCoord1f",		scm_EvalCoord1f,		1, 1 },
	{ "glEvalCoord1fv",		scm_EvalCoord1fv,		1, 1 },
	{ "glEvalCoord2d",		scm_EvalCoord2d,		2, 2 },
	{ "glEvalCoord2dv",		scm_EvalCoord2dv,		1, 1 },
	{ "glEvalCoord2f",		scm_EvalCoord2f,		2, 2 },
	{ "glEvalCoord2fv",		scm_EvalCoord2fv,		1, 1 },
	{ "glEvalMesh1",		scm_EvalMesh1,			3, 3 },
	{ "glEvalMesh2",		scm_EvalMesh2,			5, 5 },
	{ "glEvalPoint1",		scm_EvalPoint1,			1, 1 },
	{ "glEvalPoint2",		scm_EvalPoint2,			2, 2 },
	{ "glFeedbackBuffer",		scm_FeedbackBuffer,		3, 3 },
	{ "glFinish",			scm_Finish,			0, 0 },
	{ "glFlush",			scm_Flush,			0, 0 },
	{ "glFogf",			scm_Fogf,			2, 2 },
	{ "glFogfv",			scm_Fogfv,			2, 2 },
	{ "glFogi",			scm_Fogi,			2, 2 },
	{ "glFogiv",			scm_Fogiv,			2, 2 },
	{ "glFrontFace",		scm_FrontFace,			1, 1 },
	{ "glFrustum",			scm_Frustum,			6, 6 },
	{ "glGenLists",			scm_GenLists,			1, 1 },
	{ "glGenTextures",		scm_GenTextures,		2, 2 },
	{ "glGetClipPlane",		scm_GetClipPlane,		2, 2 },
	{ "glGetError",			scm_GetError,			0, 0 },
	{ "glGetLightfv",		scm_GetLightfv,			3, 3 },
	{ "glGetLightiv",		scm_GetLightiv,			3, 3 },
	{ "glGetMaterialfv",		scm_GetMaterialfv,		3, 3 },
	{ "glGetMaterialiv",		scm_GetMaterialiv,		3, 3 },
	{ "glGetPolygonStipple",	scm_GetPolygonStipple,		1, 1 },
	{ "glGetString",		scm_GetString,			1, 1 },
	{ "glHint",			scm_Hint,			2, 2 },
	{ "glIndexMask",		scm_IndexMask,			1, 1 },
	{ "glIndexd",			scm_Indexd,			1, 1 },
	{ "glIndexdv",			scm_Indexdv,			1, 1 },
	{ "glIndexf",			scm_Indexf,			1, 1 },
	{ "glIndexfv",			scm_Indexfv,			1, 1 },
	{ "glIndexi",			scm_Indexi,			1, 1 },
	{ "glIndexiv",			scm_Indexiv,			1, 1 },
	{ "glIndexs",			scm_Indexs,			1, 1 },
	{ "glIndexsv",			scm_Indexsv,			1, 1 },
	{ "glIndexub",			scm_Indexub,			1, 1 },
	{ "glIndexubv",			scm_Indexubv,			1, 1 },
	{ "glInitNames",		scm_InitNames,			0, 0 },
	{ "glIsEnabled",		scm_IsEnabled,			1, 1 },
	{ "glIsList",			scm_IsList,			1, 1 },
	{ "glIsTexture",		scm_IsTexture,			1, 1 },
	{ "glLightModelf",		scm_LightModelf,		2, 2 },
	{ "glLightModelfv",		scm_LightModelfv,		2, 2 },
	{ "glLightModeli",		scm_LightModeli,		2, 2 },
	{ "glLightModeliv",		scm_LightModeliv,		2, 2 },
	{ "glLightf",			scm_Lightf,			3, 3 },
	{ "glLightfv",			scm_Lightfv,			3, 3 },
	{ "glLighti",			scm_Lighti,			3, 3 },
	{ "glLightiv",			scm_Lightiv,			3, 3 },
	{ "glLineStipple",		scm_LineStipple,		2, 2 },
	{ "glLineWidth",		scm_LineWidth,			1, 1 },
	{ "glListBase",			scm_ListBase,			1, 1 },
	{ "glLoadIdentity",		scm_LoadIdentity,		0, 0 },
	{ "glLoadMatrixd",		scm_LoadMatrixd,		1, 1 },
	{ "glLoadMatrixf",		scm_LoadMatrixf,		1, 1 },
	{ "glLoadName",			scm_LoadName,			1, 1 },
	{ "glLogicOp",			scm_LogicOp,			1, 1 },
	{ "glMap1d",			scm_Map1d,			6, 6 },
	{ "glMap1f",			scm_Map1f,			6, 6 },
	{ "glMap2d",			scm_Map2d,			10,10},
	{ "glMap2f",			scm_Map2f,			10,10},
	{ "glMapGrid1d",		scm_MapGrid1d,			3, 3 },
	{ "glMapGrid1f",		scm_MapGrid1f,			3, 3 },
	{ "glMapGrid2d",		scm_MapGrid2d,			6, 6 },
	{ "glMapGrid2f",		scm_MapGrid2f,			6, 6 },
	{ "glMaterialf",		scm_Materialf,			3, 3 },
	{ "glMaterialfv",		scm_Materialfv,			3, 3 },
	{ "glMateriali",		scm_Materiali,			3, 3 },
	{ "glMaterialiv",		scm_Materialiv,			3, 3 },
	{ "glMatrixMode",		scm_MatrixMode,			1, 1 },
	{ "glMultMatrixd",		scm_MultMatrixd,		1, 1 },
	{ "glMultMatrixf",		scm_MultMatrixf,		1, 1 },
	{ "glNewList",			scm_NewList,			2, 2 },
	{ "glNormal3b",			scm_Normal3b,			3, 3 },
	{ "glNormal3bv",		scm_Normal3bv,			1, 1 },
	{ "glNormal3d",			scm_Normal3d,			3, 3 },
	{ "glNormal3dv",		scm_Normal3dv,			1, 1 },
	{ "glNormal3f",			scm_Normal3f,			3, 3 },
	{ "glNormal3fv",		scm_Normal3fv,			1, 1 },
	{ "glNormal3i",			scm_Normal3i,			3, 3 },
	{ "glNormal3iv",		scm_Normal3iv,			1, 1 },
	{ "glNormal3s",			scm_Normal3s,			3, 3 },
	{ "glNormal3sv",		scm_Normal3sv,			1, 1 },
	{ "glOrtho",			scm_Ortho,			6, 6 },
	{ "glPassThrough",		scm_PassThrough,		1, 1 },
	{ "glPixelMapfv",		scm_PixelMapfv,			3, 3 },
	{ "glPixelMapuiv",		scm_PixelMapuiv,		3, 3 },
	{ "glPixelMapusv",		scm_PixelMapusv,		3, 3 },
	{ "glPixelStoref",		scm_PixelStoref,		2, 2 },
	{ "glPixelStorei",		scm_PixelStorei,		2, 2 },
	{ "glPixelTransferf",		scm_PixelTransferf,		2, 2 },
	{ "glPixelTransferi",		scm_PixelTransferi,		2, 2 },
	{ "glPixelZoom",		scm_PixelZoom,			2, 2 },
	{ "glPointSize",		scm_PointSize,			1, 1 },
	{ "glPolygonMode",		scm_PolygonMode,		2, 2 },
	{ "glPolygonOffset",		scm_PolygonOffset,		2, 2 },
	{ "glPolygonStipple",		scm_PolygonStipple,		1, 1 },
	{ "glPopAttrib",		scm_PopAttrib,			0, 0 },
	{ "glPopClientAttrib",		scm_PopClientAttrib,		0, 0 },
	{ "glPopMatrix",		scm_PopMatrix,			0, 0 },
	{ "glPopName",			scm_PopName,			0, 0 },
	{ "glPrioritizeTextures",	scm_PrioritizeTextures,		3, 3 },
	{ "glPushAttrib",		scm_PushAttrib,			1, 1 },
	{ "glPushClientAttrib",		scm_PushClientAttrib,		1, 1 },
	{ "glPushMatrix",		scm_PushMatrix,			0, 0 },
	{ "glPushName",			scm_PushName,			1, 1 },
	{ "glRasterPos2d",		scm_RasterPos2d,		2, 2 },
	{ "glRasterPos2dv",		scm_RasterPos2dv,		1, 1 },
	{ "glRasterPos2f",		scm_RasterPos2f,		2, 2 },
	{ "glRasterPos2fv",		scm_RasterPos2fv,		1, 1 },
	{ "glRasterPos2i",		scm_RasterPos2i,		2, 2 },
	{ "glRasterPos2iv",		scm_RasterPos2iv,		1, 1 },
	{ "glRasterPos2s",		scm_RasterPos2s,		2, 2 },
	{ "glRasterPos2sv",		scm_RasterPos2sv,		1, 1 },
	{ "glRasterPos3d",		scm_RasterPos3d,		3, 3 },
	{ "glRasterPos3dv",		scm_RasterPos3dv,		1, 1 },
	{ "glRasterPos3f",		scm_RasterPos3f,		3, 3 },
	{ "glRasterPos3fv",		scm_RasterPos3fv,		1, 1 },
	{ "glRasterPos3i",		scm_RasterPos3i,		3, 3 },
	{ "glRasterPos3iv",		scm_RasterPos3iv,		1, 1 },
	{ "glRasterPos3s",		scm_RasterPos3s,		3, 3 },
	{ "glRasterPos3sv",		scm_RasterPos3sv,		1, 1 },
	{ "glRasterPos4d",		scm_RasterPos4d,		4, 4 },
	{ "glRasterPos4dv",		scm_RasterPos4dv,		1, 1 },
	{ "glRasterPos4f",		scm_RasterPos4f,		4, 4 },
	{ "glRasterPos4fv",		scm_RasterPos4fv,		1, 1 },
	{ "glRasterPos4i",		scm_RasterPos4i,		4, 4 },
	{ "glRasterPos4iv",		scm_RasterPos4iv,		1, 1 },
	{ "glRasterPos4s",		scm_RasterPos4s,		4, 4 },
	{ "glRasterPos4sv",		scm_RasterPos4sv,		1, 1 },
	{ "glReadBuffer",		scm_ReadBuffer,			1, 1 },
	{ "glReadPixels",		scm_ReadPixels,			7, 7 },
	{ "glRectd",			scm_Rectd,			4, 4 },
	{ "glRectdv",			scm_Rectdv,			2, 2 },
	{ "glRectf",			scm_Rectf,			4, 4 },
	{ "glRectfv",			scm_Rectfv,			2, 2 },
	{ "glRecti",			scm_Recti,			4, 4 },
	{ "glRectiv",			scm_Rectiv,			2, 2 },
	{ "glRects",			scm_Rects,			4, 4 },
	{ "glRectsv",			scm_Rectsv,			2, 2 },
	{ "glRenderMode",		scm_RenderMode,			1, 1 },
	{ "glRotated",			scm_Rotated,			4, 4 },
	{ "glRotatef",			scm_Rotatef,			4, 4 },
	{ "glScaled",			scm_Scaled,			3, 3 },
	{ "glScalef",			scm_Scalef,			3, 3 },
	{ "glScissor",			scm_Scissor,			4, 4 },
	{ "glSelectBuffer",		scm_SelectBuffer,		2, 2 },
	{ "glShadeModel",		scm_ShadeModel,			1, 1 },
	{ "glStencilFunc",		scm_StencilFunc,		3, 3 },
	{ "glStencilMask",		scm_StencilMask,		1, 1 },
	{ "glStencilOp",		scm_StencilOp,			3, 3 },
	{ "glTexCoord1d",		scm_TexCoord1d,			1, 1 },
	{ "glTexCoord1dv",		scm_TexCoord1dv,		1, 1 },
	{ "glTexCoord1f",		scm_TexCoord1f,			1, 1 },
	{ "glTexCoord1fv",		scm_TexCoord1fv,		1, 1 },
	{ "glTexCoord1i",		scm_TexCoord1i,			1, 1 },
	{ "glTexCoord1iv",		scm_TexCoord1iv,		1, 1 },
	{ "glTexCoord1s",		scm_TexCoord1s,			1, 1 },
	{ "glTexCoord1sv",		scm_TexCoord1sv,		1, 1 },
	{ "glTexCoord2d",		scm_TexCoord2d,			2, 2 },
	{ "glTexCoord2dv",		scm_TexCoord2dv,		1, 1 },
	{ "glTexCoord2f",		scm_TexCoord2f,			2, 2 },
	{ "glTexCoord2fv",		scm_TexCoord2fv,		1, 1 },
	{ "glTexCoord2i",		scm_TexCoord2i,			2, 2 },
	{ "glTexCoord2iv",		scm_TexCoord2iv,		1, 1 },
	{ "glTexCoord2s",		scm_TexCoord2s,			2, 2 },
	{ "glTexCoord2sv",		scm_TexCoord2sv,		1, 1 },
	{ "glTexCoord3d",		scm_TexCoord3d,			3, 3 },
	{ "glTexCoord3dv",		scm_TexCoord3dv,		1, 1 },
	{ "glTexCoord3f",		scm_TexCoord3f,			3, 3 },
	{ "glTexCoord3fv",		scm_TexCoord3fv,		1, 1 },
	{ "glTexCoord3i",		scm_TexCoord3i,			3, 3 },
	{ "glTexCoord3iv",		scm_TexCoord3iv,		1, 1 },
	{ "glTexCoord3s",		scm_TexCoord3s,			3, 3 },
	{ "glTexCoord3sv",		scm_TexCoord3sv,		1, 1 },
	{ "glTexCoord4d",		scm_TexCoord4d,			4, 4 },
	{ "glTexCoord4dv",		scm_TexCoord4dv,		1, 1 },
	{ "glTexCoord4f",		scm_TexCoord4f,			4, 4 },
	{ "glTexCoord4fv",		scm_TexCoord4fv,		1, 1 },
	{ "glTexCoord4i",		scm_TexCoord4i,			4, 4 },
	{ "glTexCoord4iv",		scm_TexCoord4iv,		1, 1 },
	{ "glTexCoord4s",		scm_TexCoord4s,			4, 4 },
	{ "glTexCoord4sv",		scm_TexCoord4sv,		1, 1 },
	{ "glTexEnvf",			scm_TexEnvf,			3, 3 },
	{ "glTexEnvfv",			scm_TexEnvfv,			3, 3 },
	{ "glTexEnvi",			scm_TexEnvi,			3, 3 },
	{ "glTexEnviv",			scm_TexEnviv,			3, 3 },
	{ "glTexGend",			scm_TexGend,			3, 3 },
	{ "glTexGendv",			scm_TexGendv,			3, 3 },
	{ "glTexGenf",			scm_TexGenf,			3, 3 },
	{ "glTexGenfv",			scm_TexGenfv,			3, 3 },
	{ "glTexGeni",			scm_TexGeni,			3, 3 },
	{ "glTexGeniv",			scm_TexGeniv,			3, 3 },
	{ "glTexImage1D",		scm_TexImage1D,			8, 8 },
	{ "glTexImage2D",		scm_TexImage2D,			9, 9 },
	{ "glTexParameterf",		scm_TexParameterf,		3, 3 },
	{ "glTexParameterfv",		scm_TexParameterfv,		3, 3 },
	{ "glTexParameteri",		scm_TexParameteri,		3, 3 },
	{ "glTexParameteriv",		scm_TexParameteriv,		3, 3 },
	{ "glTexSubImage1D",		scm_TexSubImage1D,		7, 7 },
	{ "glTexSubImage2D",		scm_TexSubImage2D,		9, 9 },
	{ "glTranslated",		scm_Translated,			3, 3 },
	{ "glTranslatef",		scm_Translatef,			3, 3 },
	{ "glVertex2d",			scm_Vertex2d,			2, 2 },
	{ "glVertex2dv",		scm_Vertex2dv,			1, 1 },
	{ "glVertex2f",			scm_Vertex2f,			2, 2 },
	{ "glVertex2fv",		scm_Vertex2fv,			1, 1 },
	{ "glVertex2i",			scm_Vertex2i,			2, 2 },
	{ "glVertex2iv",		scm_Vertex2iv,			1, 1 },
	{ "glVertex2s",			scm_Vertex2s,			2, 2 },
	{ "glVertex2sv",		scm_Vertex2sv,			1, 1 },
	{ "glVertex3d",			scm_Vertex3d,			3, 3 },
	{ "glVertex3dv",		scm_Vertex3dv,			1, 1 },
	{ "glVertex3f",			scm_Vertex3f,			3, 3 },
	{ "glVertex3fv",		scm_Vertex3fv,			1, 1 },
	{ "glVertex3i",			scm_Vertex3i,			3, 3 },
	{ "glVertex3iv",		scm_Vertex3iv,			1, 1 },
	{ "glVertex3s",			scm_Vertex3s,			3, 3 },
	{ "glVertex3sv",		scm_Vertex3sv,			1, 1 },
	{ "glVertex4d",			scm_Vertex4d,			4, 4 },
	{ "glVertex4dv",		scm_Vertex4dv,			1, 1 },
	{ "glVertex4f",			scm_Vertex4f,			4, 4 },
	{ "glVertex4fv",		scm_Vertex4fv,			1, 1 },
	{ "glVertex4i",			scm_Vertex4i,			4, 4 },
	{ "glVertex4iv",		scm_Vertex4iv,			1, 1 },
	{ "glVertex4s",			scm_Vertex4s,			4, 4 },
	{ "glVertex4sv",		scm_Vertex4sv,			1, 1 },
	{ "glViewport",			scm_Viewport,			4, 4 },

#ifdef GL_VERSION_1_2

	{ "glBlendColor",		scm_BlendColor,			4, 4 },
	{ "glBlendEquation",		scm_BlendEquation,		1, 1 },
	{ "glColorSubTable",		scm_ColorSubTable,		6, 6 },
	{ "glColorTable",		scm_ColorTable,			6, 6 },
	{ "glColorTableParameterfv",	scm_ColorTableParameterfv,	3, 3 },
	{ "glColorTableParameteriv",	scm_ColorTableParameteriv,	3, 3 },
	{ "glConvolutionFilter1D",	scm_ConvolutionFilter1D,	6, 6 },
	{ "glConvolutionFilter2D",	scm_ConvolutionFilter2D,	7, 7 },
	{ "glConvolutionParameterf",	scm_ConvolutionParameterf,	3, 3 },
	{ "glConvolutionParameterfv",	scm_ConvolutionParameterfv,	3, 3 },
	{ "glConvolutionParameteri",	scm_ConvolutionParameteri,	3, 3 },
	{ "glConvolutionParameteriv",	scm_ConvolutionParameteriv,	3, 3 },
	{ "glCopyColorSubTable",	scm_CopyColorSubTable,		5, 5 },
	{ "glCopyColorTable",		scm_CopyColorTable,		5, 5 },
	{ "glCopyConvolutionFilter1D",	scm_CopyConvolutionFilter1D,	5, 5 },
	{ "glCopyConvolutionFilter2D",	scm_CopyConvolutionFilter2D,	6, 6 },
	{ "glCopyTexSubImage3D",	scm_CopyTexSubImage3D,		9, 9 },
	{ "glGetColorTableParameterfv",	scm_GetColorTableParameterfv,	3, 3 },
	{ "glGetColorTableParameteriv",	scm_GetColorTableParameteriv,	3, 3 },
	{ "glGetConvolutionParameterfv",scm_GetConvolutionParameterfv,	3, 3 },
	{ "glGetConvolutionParameteriv",scm_GetConvolutionParameteriv,	3, 3 },
	{ "glGetHistogramParameterfv",	scm_GetHistogramParameterfv,	3, 3 },
	{ "glGetHistogramParameteriv",	scm_GetHistogramParameteriv,	3, 3 },
	{ "glGetMinmaxParameterfv",	scm_GetMinmaxParameterfv,	3, 3 },
	{ "glGetMinmaxParameteriv",	scm_GetMinmaxParameteriv,	3, 3 },
	{ "glHistogram",		scm_Histogram,			4, 4 },
	{ "glMinmax",			scm_Minmax,			3, 3 },
	{ "glResetHistogram",		scm_ResetHistogram,		1, 1 },
	{ "glResetMinmax",		scm_ResetMinmax,		1, 1 },
	{ "glSeparableFilter2D",	scm_SeparableFilter2D,		8, 8 },
	{ "glTexImage3D",		scm_TexImage3D,			10,10},
	{ "glTexSubImage3D",		scm_TexSubImage3D,		11,11},

#endif /* GL_VERSION_1_2 */

#ifdef GL_VERSION_1_3
	{ "glActiveTexture",		scm_ActiveTexture,		1, 1 },
	{ "glClientActiveTexture",	scm_ClientActiveTexture,	1, 1 },
	{ "glLoadTransposeMatrixd",	scm_LoadTransposeMatrixd,	1, 1 },
	{ "glLoadTransposeMatrixf",	scm_LoadTransposeMatrixf,	1, 1 },
	{ "glMultTransposeMatrixd",	scm_MultTransposeMatrixd,	1, 1 },
	{ "glMultTransposeMatrixf",	scm_MultTransposeMatrixf,	1, 1 },
	{ "glMultiTexCoord1d",		scm_MultiTexCoord1d,		2, 2 },
	{ "glMultiTexCoord1dv",		scm_MultiTexCoord1dv,		2, 2 },
	{ "glMultiTexCoord1f",		scm_MultiTexCoord1f,		2, 2 },
	{ "glMultiTexCoord1fv",		scm_MultiTexCoord1fv,		2, 2 },
	{ "glMultiTexCoord1i",		scm_MultiTexCoord1i,		2, 2 },
	{ "glMultiTexCoord1iv",		scm_MultiTexCoord1iv,		2, 2 },
	{ "glMultiTexCoord1s",		scm_MultiTexCoord1s,		2, 2 },
	{ "glMultiTexCoord1sv",		scm_MultiTexCoord1sv,		2, 2 },
	{ "glMultiTexCoord2d",		scm_MultiTexCoord2d,		3, 3 },
	{ "glMultiTexCoord2dv",		scm_MultiTexCoord2dv,		2, 2 },
	{ "glMultiTexCoord2f",		scm_MultiTexCoord2f,		3, 3 },
	{ "glMultiTexCoord2fv",		scm_MultiTexCoord2fv,		2, 2 },
	{ "glMultiTexCoord2i",		scm_MultiTexCoord2i,		3, 3 },
	{ "glMultiTexCoord2iv",		scm_MultiTexCoord2iv,		2, 2 },
	{ "glMultiTexCoord2s",		scm_MultiTexCoord2s,		3, 3 },
	{ "glMultiTexCoord2sv",		scm_MultiTexCoord2sv,		2, 2 },
	{ "glMultiTexCoord3d",		scm_MultiTexCoord3d,		4, 4 },
	{ "glMultiTexCoord3dv",		scm_MultiTexCoord3dv,		2, 2 },
	{ "glMultiTexCoord3f",		scm_MultiTexCoord3f,		4, 4 },
	{ "glMultiTexCoord3fv",		scm_MultiTexCoord3fv,		2, 2 },
	{ "glMultiTexCoord3i",		scm_MultiTexCoord3i,		4, 4 },
	{ "glMultiTexCoord3iv",		scm_MultiTexCoord3iv,		2, 2 },
	{ "glMultiTexCoord3s",		scm_MultiTexCoord3s,		4, 4 },
	{ "glMultiTexCoord3sv",		scm_MultiTexCoord3sv,		2, 2 },
	{ "glMultiTexCoord4d",		scm_MultiTexCoord4d,		5, 5 },
	{ "glMultiTexCoord4dv",		scm_MultiTexCoord4dv,		2, 2 },
	{ "glMultiTexCoord4f",		scm_MultiTexCoord4f,		5, 5 },
	{ "glMultiTexCoord4fv",		scm_MultiTexCoord4fv,		2, 2 },
	{ "glMultiTexCoord4i",		scm_MultiTexCoord4i,		5, 5 },
	{ "glMultiTexCoord4iv",		scm_MultiTexCoord4iv,		2, 2 },
	{ "glMultiTexCoord4s",		scm_MultiTexCoord4s,		5, 5 },
	{ "glMultiTexCoord4sv",		scm_MultiTexCoord4sv,		2, 2 },
	{ "glSampleCoverage",		scm_SampleCoverage,		2, 2 },

#endif /* GL_VERSION_1_3 */

};

/*---------------------------------------------------------------------------*/
/* MzScheme extension interface						     */

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *mod_env;
  double ver = 
#ifdef GL_VERSION_1_3
  1.3
#else
#ifdef GL_VERSION_1_2
  1.2
#else
  1.1
#endif
#endif
;
  mod_env = scheme_primitive_module(scheme_intern_symbol("gl-prims"), env);
  scheme_load_enum(mod_env);
  scheme_load_prim(mod_env, scm_prim, sizeof(scm_prim));
  scheme_add_global("gl-version", scheme_make_double(ver), mod_env);
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
  return scheme_intern_symbol("gl-prims");
}

/*---------------------------------------------------------------------------*/
