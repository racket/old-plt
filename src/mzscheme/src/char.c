/*
  MzScheme
  Copyright (c) 2004 PLT Scheme, Inc.
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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include <ctype.h>

/* globals */
#include "schuchar.inc"
Scheme_Object **scheme_char_constants;

/* locals */
static Scheme_Object *char_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_alphabetic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_numeric (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_whitespace (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_symbolic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_graphic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_blank (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_control (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_punctuation (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upper_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lower_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_title_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_to_integer (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_to_char (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_titlecase (int argc, Scheme_Object *argv[]);

void scheme_init_portable_case(void)
{
  init_uchar_table();
}

void scheme_init_char (Scheme_Env *env)
{
  int i;

  REGISTER_SO(scheme_char_constants);

  scheme_char_constants = 
    (Scheme_Object **)scheme_malloc_eternal(256 * sizeof(Scheme_Object*));
    
  for (i = 0; i < 256; i++) {
    Scheme_Object *sc;
    sc = scheme_alloc_eternal_small_object();
    sc->type = scheme_char_type;
    SCHEME_CHAR_VAL(sc) = i;
    
    scheme_char_constants[i] = sc;
  }

  scheme_add_global_constant("char?", 
			     scheme_make_folding_prim(char_p, 
						      "char?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char=?", 
			     scheme_make_folding_prim(char_eq, 
						      "char=?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char<?", 
			     scheme_make_folding_prim(char_lt, 
						      "char<?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char>?", 
			     scheme_make_folding_prim(char_gt, 
						      "char>?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char<=?", 
			     scheme_make_folding_prim(char_lt_eq, 
						      "char<=?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char>=?", 
			     scheme_make_folding_prim(char_gt_eq, 
						      "char>=?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci=?", 
			     scheme_make_folding_prim(char_eq_ci, 
						      "char-ci=?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci<?", 
			     scheme_make_folding_prim(char_lt_ci, 
						      "char-ci<?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci>?", 
			     scheme_make_folding_prim(char_gt_ci, 
						      "char-ci>?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci<=?", 
			     scheme_make_folding_prim(char_lt_eq_ci, 
						      "char-ci<=?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci>=?", 
			     scheme_make_folding_prim(char_gt_eq_ci, 
						      "char-ci>=?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-alphabetic?", 
			     scheme_make_folding_prim(char_alphabetic, 
						      "char-alphabetic?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-numeric?", 
			     scheme_make_folding_prim(char_numeric, 
						      "char-numeric?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-symbolic?", 
			     scheme_make_folding_prim(char_symbolic, 
						      "char-symbolic?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-graphic?", 
			     scheme_make_folding_prim(char_graphic, 
						      "char-graphic?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-whitespace?", 
			     scheme_make_folding_prim(char_whitespace, 
						      "char-whitespace?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-blank?", 
			     scheme_make_folding_prim(char_blank, 
						      "char-blank?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-iso-control?", 
			     scheme_make_folding_prim(char_control, 
						      "char-iso-control?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-punctuation?", 
			     scheme_make_folding_prim(char_punctuation, 
						      "char-punctuation?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-upper-case?", 
			     scheme_make_folding_prim(char_upper_case, 
						      "char-upper-case?", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("char-title-case?", 
			     scheme_make_folding_prim(char_title_case, 
						      "char-title-case?", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("char-lower-case?", 
			     scheme_make_folding_prim(char_lower_case, 
						      "char-lower-case?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-title-case?", 
			     scheme_make_folding_prim(char_title_case, 
						      "char-title-case?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char->integer", 
			     scheme_make_folding_prim(char_to_integer, 
						      "char->integer", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("integer->char",
			     scheme_make_folding_prim(integer_to_char, 
						      "integer->char",
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-upcase", 
			     scheme_make_folding_prim(char_upcase, 
						      "char-upcase", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-downcase", 
			     scheme_make_folding_prim(char_downcase, 
						      "char-downcase", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("char-titlecase", 
			     scheme_make_folding_prim(char_titlecase, 
						      "char-titlecase", 
						      1, 1, 1),
			     env);
}

Scheme_Object *scheme_make_char(mzchar ch)
{
  Scheme_Object *o;

  if (ch < 256)
    return scheme_char_constants[ch];
  
  o = scheme_alloc_small_object();
  o->type = scheme_char_type;
  SCHEME_CHAR_VAL(o) = ch;

  return o;
}

/* locals */

static Scheme_Object *
char_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHARP(argv[0]) ? scheme_true : scheme_false);
}

#define charSTD_DOWNCASE(nl) nl;
#define charNO_DOWNCASE(nl) /* empty */

#define GEN_CHAR_COMP(func_name, scheme_name, comp, DOWNCASE) \
 static Scheme_Object *func_name(int argc, Scheme_Object *argv[])     \
 { int c, prev, i; Scheme_Object *rv = scheme_true; \
   if (!SCHEME_CHARP(argv[0]))      \
     scheme_wrong_type(#scheme_name, "character", 0, argc, argv);     \
   prev = SCHEME_CHAR_VAL(argv[0]);     \
   DOWNCASE(prev = scheme_tolower(prev)) \
   for (i = 1; i < argc; i++) {     \
     if (!SCHEME_CHARP(argv[i]))      \
       scheme_wrong_type(#scheme_name, "character", i, argc, argv);     \
     c = SCHEME_CHAR_VAL(argv[i]);     \
     DOWNCASE(c = scheme_tolower(c)) \
     if (!(prev comp c)) rv = scheme_false;   \
     prev = c;     \
   }     \
   return rv;     \
 }

GEN_CHAR_COMP(char_eq, char=?, ==, charNO_DOWNCASE)
GEN_CHAR_COMP(char_lt, char<?, <, charNO_DOWNCASE)
GEN_CHAR_COMP(char_gt, char>?, >, charNO_DOWNCASE)
GEN_CHAR_COMP(char_lt_eq, char<=?, <=, charNO_DOWNCASE)
GEN_CHAR_COMP(char_gt_eq, char>=?, >=, charNO_DOWNCASE)

GEN_CHAR_COMP(char_eq_ci, char-ci=?, ==, charSTD_DOWNCASE)
GEN_CHAR_COMP(char_lt_ci, char-ci<?, <, charSTD_DOWNCASE)
GEN_CHAR_COMP(char_gt_ci, char-ci>?, >, charSTD_DOWNCASE)
GEN_CHAR_COMP(char_lt_eq_ci, char-ci<=?, <=, charSTD_DOWNCASE)
GEN_CHAR_COMP(char_gt_eq_ci, char-ci>=?, >=, charSTD_DOWNCASE)

#define GEN_CHAR_TEST(func_name, scheme_name, pred) \
static Scheme_Object *func_name (int argc, Scheme_Object *argv[]) \
{ \
  mzchar c;    \
  if (!SCHEME_CHARP(argv[0]))  \
    scheme_wrong_type(scheme_name, "character", 0, argc, argv); \
  c = SCHEME_CHAR_VAL(argv[0]);                    \
  return (pred(c) ? scheme_true : scheme_false);   \
}
     
GEN_CHAR_TEST(char_numeric, "char-numeric?", scheme_isdigit)
GEN_CHAR_TEST(char_alphabetic, "char-alphabetic?", scheme_isalpha)
GEN_CHAR_TEST(char_whitespace, "char-whitespace?", scheme_isspace)
GEN_CHAR_TEST(char_blank, "char-blank?", scheme_isblank)
GEN_CHAR_TEST(char_control, "char-iso-control?", scheme_isspace)
GEN_CHAR_TEST(char_punctuation, "char-punctuation?", scheme_ispunc)
GEN_CHAR_TEST(char_symbolic, "char-symbolic?", scheme_issymbol)
GEN_CHAR_TEST(char_graphic, "char-graphic?", scheme_isgraphic)
GEN_CHAR_TEST(char_upper_case, "char-upper-case?", scheme_isupper)
GEN_CHAR_TEST(char_lower_case, "char-lower-case?", scheme_islower)
GEN_CHAR_TEST(char_title_case, "char-title-case?", scheme_istitle)

static Scheme_Object *
char_to_integer (int argc, Scheme_Object *argv[])
{
  mzchar c;

  if (!SCHEME_CHARP(argv[0]))
    scheme_wrong_type("char->integer", "character", 0, argc, argv);

  c = SCHEME_CHAR_VAL(argv[0]);

  return scheme_make_integer_value(c);
}

static Scheme_Object *
integer_to_char (int argc, Scheme_Object *argv[])
{
  if (SCHEME_INTP(argv[0])) {
    long v;
    v = SCHEME_INT_VAL(argv[0]);
    if ((v >= 0) 
	&& (v <= 0x7FFFFFFF)
	&& (v != 0xFFFE)
	&& (v != 0xFFFF)
	&& ((v < 0xD800) || (v > 0xDFFF)))
      return _scheme_make_char(v);
  } else if (SCHEME_BIGNUMP(argv[0])
	     && SCHEME_BIGPOS(argv[0])) {
    /* On 32-bit machines, there's still a chance... */
    long y;
    if (scheme_get_int_val(argv[0], &y)) {
      if (y <= 0x7FFFFFFF)
	return _scheme_make_char(y);
    }
  }

  scheme_wrong_type("integer->char", 
		    "exact integer in [0,#x7FFFFFFF], not in [#xD800,#xDFFF] or [#xFFFE,#xFFFF]", 
		    0, argc, argv);
  return NULL;
}

#define GEN_RECASE(func_name, scheme_name, cvt) \
static Scheme_Object *func_name (int argc, Scheme_Object *argv[]) \
{ \
  mzchar c;    \
  if (!SCHEME_CHARP(argv[0]))  \
    scheme_wrong_type(scheme_name, "character", 0, argc, argv); \
  c = SCHEME_CHAR_VAL(argv[0]);                    \
  c = cvt(c);                                      \
  return scheme_make_character(c);   \
}

GEN_RECASE(char_upcase, "char-upcase", scheme_toupper)
GEN_RECASE(char_downcase, "char-downcase", scheme_tolower)
GEN_RECASE(char_titlecase, "char-titlecase", scheme_totitle)

