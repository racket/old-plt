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
#ifndef DONT_USE_LOCALE
# include <locale.h>
#endif

/* globals */

/* All characters */
Scheme_Object **scheme_char_constants;

unsigned char scheme_portable_upcase[256];
unsigned char scheme_portable_downcase[256];

/* locals */
static Scheme_Object *char_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_locale (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_locale (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq_locale_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_locale_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_locale_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_alphabetic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_alphabetic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_numeric (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_numeric (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_whitespace (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_whitespace (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upper_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_upper_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lower_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_lower_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_to_integer (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_to_char (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_to_latin1_integer (int argc, Scheme_Object *argv[]);
static Scheme_Object *latin1_integer_to_char (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_locale_downcase (int argc, Scheme_Object *argv[]);

void scheme_init_portable_case(void)
{
  int i;

  for (i = 0; i < 256; i++) {
    scheme_portable_upcase[i] = i;
    scheme_portable_downcase[i] = i;
  }

  for (i = 'a'; i < 'z' + 1; i++) {
    scheme_portable_upcase[i] = i - ('a' - 'A');
    scheme_portable_downcase[i - ('a' - 'A')] = i;
  }
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
  scheme_add_global_constant("char-locale<?", 
			     scheme_make_nonlocale_folding_prim(char_lt_locale, 
								"char-locale<?", 
								2, -1, 1), 
			     env);
  scheme_add_global_constant("char>?", 
			     scheme_make_folding_prim(char_gt, 
						      "char>?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-locale>?", 
			     scheme_make_nonlocale_folding_prim(char_gt_locale, 
								"char-locale>?", 
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
  scheme_add_global_constant("char-locale-ci=?", 
			     scheme_make_nonlocale_folding_prim(char_eq_locale_ci, 
								"char-locale-ci=?", 
								2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci<?", 
			     scheme_make_folding_prim(char_lt_ci, 
						      "char-ci<?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-locale-ci<?", 
			     scheme_make_nonlocale_folding_prim(char_lt_locale_ci, 
								"char-locale-ci<?", 
								2, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci>?", 
			     scheme_make_folding_prim(char_gt_ci, 
						      "char-ci>?", 
						      2, -1, 1), 
			     env);
  scheme_add_global_constant("char-locale-ci>?", 
			     scheme_make_nonlocale_folding_prim(char_gt_locale_ci, 
								"char-locale-ci>?", 
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
  scheme_add_global_constant("char-locale-alphabetic?", 
			     scheme_make_nonlocale_folding_prim(char_locale_alphabetic, 
								"char-locale-alphabetic?", 
								1, 1, 1), 
			     env);
  scheme_add_global_constant("char-numeric?", 
			     scheme_make_folding_prim(char_numeric, 
						      "char-numeric?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-locale-numeric?", 
			     scheme_make_nonlocale_folding_prim(char_locale_numeric, 
								"char-locale-numeric?", 
								1, 1, 1), 
			     env);
  scheme_add_global_constant("char-whitespace?", 
			     scheme_make_nonlocale_folding_prim(char_whitespace, 
								"char-whitespace?", 
								1, 1, 1), 
			     env);
  scheme_add_global_constant("char-locale-whitespace?", 
			     scheme_make_nonlocale_folding_prim(char_locale_whitespace, 
								"char-locale-whitespace?", 
								1, 1, 1), 
			     env);
  scheme_add_global_constant("char-upper-case?", 
			     scheme_make_folding_prim(char_upper_case, 
						      "char-upper-case?", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("char-locale-upper-case?", 
			     scheme_make_nonlocale_folding_prim(char_locale_upper_case, 
								"char-locale-upper-case?", 
								1, 1, 1),
			     env);
  scheme_add_global_constant("char-lower-case?", 
			     scheme_make_folding_prim(char_lower_case, 
						      "char-lower-case?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-locale-lower-case?", 
			     scheme_make_nonlocale_folding_prim(char_locale_lower_case, 
								"char-locale-lower-case?", 
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
  scheme_add_global_constant("char->latin-1-integer", 
			     scheme_make_prim_w_arity(char_to_latin1_integer, 
						      "char->latin-1-integer", 
						      1, 1),
			     env);
  scheme_add_global_constant("latin-1-integer->char",
			     scheme_make_prim_w_arity(latin1_integer_to_char, 
						      "latin-1-integer->char",
						      1, 1), 
			     env);
  scheme_add_global_constant("char-upcase", 
			     scheme_make_folding_prim(char_upcase, 
						      "char-upcase", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-locale-upcase", 
			     scheme_make_nonlocale_folding_prim(char_locale_upcase, 
								"char-locale-upcase", 
								1, 1, 1), 
			     env);
  scheme_add_global_constant("char-downcase", 
			     scheme_make_folding_prim(char_downcase, 
						      "char-downcase", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("char-locale-downcase", 
			     scheme_make_nonlocale_folding_prim(char_locale_downcase, 
								"char-locale-downcase", 
								1, 1, 1),
			     env);
}

Scheme_Object *scheme_make_char(char ch)
{
  return _scheme_make_char(ch);
}

/* locals */

static Scheme_Object *
char_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHARP(argv[0]) ? scheme_true : scheme_false);
}

#define CHAR_UN_CHECK(name) \
 if (!SCHEME_CHARP(argv[0])) \
   char_un_error(name, argc, argv)

static void char_un_error(char *name, int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHARP(argv[0]))
    scheme_wrong_type(name, "character", 0, argc, argv);
}

#define charSTD_UPCASE(t, l, nl) nl;
#define charNO_UPCASE(t, l, nl) /* empty */
#define charSTD_LOCCOMP(t, prev, c, comp, rv) /* empty */

#ifdef DONT_USE_LOCALE
# define charLOC_UPCASE(t, l, nl) nl;
# define charLOCCOMP(t, prev, c, comp, rv) /* empty */
#else
# define charLOC_UPCASE(t, l, nl) if (t) { l; } else { nl; }
# define charLOC_LOCCOMP(t, prev, c, comp, rv) \
     if (t) { \
        char a[2], b[2]; a[1] = 0; b[1] = 0; a[0] = (char)prev; b[0] = (char)c; \
        if (!(strcoll(a, b) comp 0)) rv = scheme_false; \
     } else
#endif

#define GEN_CHAR_COMP(func_name, scheme_name, comp, UPCASE, LOCCOMP) \
 static Scheme_Object *func_name(int argc, Scheme_Object *argv[])     \
 { int c, prev, i; Scheme_Object *rv = scheme_true; \
   if (!SCHEME_CHARP(argv[0]))      \
     scheme_wrong_type(#scheme_name, "character", 0, argc, argv);     \
   prev = ((unsigned char)SCHEME_CHAR_VAL(argv[0]));     \
   UPCASE(scheme_locale_on, prev = toupper(prev), prev = mz_portable_toupper(prev)) \
   for (i = 1; i < argc; i++) {     \
     if (!SCHEME_CHARP(argv[i]))      \
       scheme_wrong_type(#scheme_name, "character", i, argc, argv);     \
     c = ((unsigned char)SCHEME_CHAR_VAL(argv[i]));     \
     UPCASE(scheme_locale_on, c = toupper(c), c = mz_portable_toupper(c)) \
     LOCCOMP(scheme_locale_on, prev, c, comp, rv) \
     if (!(prev comp c)) rv = scheme_false;   \
     prev = c;     \
   }     \
   return rv;     \
 }

GEN_CHAR_COMP(char_eq, char=?, ==, charNO_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_lt, char<?, <, charNO_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_gt, char>?, >, charNO_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_lt_eq, char<=?, <=, charNO_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_gt_eq, char>=?, >=, charNO_UPCASE, charSTD_LOCCOMP)

GEN_CHAR_COMP(char_eq_ci, char-ci=?, ==, charSTD_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_lt_ci, char-ci<?, <, charSTD_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_gt_ci, char-ci>?, >, charSTD_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_lt_eq_ci, char-ci<=?, <=, charSTD_UPCASE, charSTD_LOCCOMP)
GEN_CHAR_COMP(char_gt_eq_ci, char-ci>=?, >=, charSTD_UPCASE, charSTD_LOCCOMP)

GEN_CHAR_COMP(char_lt_locale, char<?, <, charNO_UPCASE, charLOC_LOCCOMP)
GEN_CHAR_COMP(char_gt_locale, char>?, >, charNO_UPCASE, charLOC_LOCCOMP)
GEN_CHAR_COMP(char_eq_locale_ci, char-ci=?, ==, charLOC_UPCASE, charLOC_LOCCOMP)
GEN_CHAR_COMP(char_lt_locale_ci, char-ci<?, <, charLOC_UPCASE, charLOC_LOCCOMP)
GEN_CHAR_COMP(char_gt_locale_ci, char-ci>?, >, charLOC_UPCASE, charLOC_LOCCOMP)

static Scheme_Object *
char_alphabetic (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-alphabetic?");

  c = SCHEME_CHAR_VAL(argv[0]);

  return ((((c >= 'A') && (c <= 'Z'))
	   || ((c >= 'a') && (c <= 'z')))
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *
char_locale_alphabetic (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale-alphabetic?");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return scheme_false;

  return isalpha(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_numeric (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-numeric?");

  c = SCHEME_CHAR_VAL(argv[0]);

  return ((c >= '0') && (c <= '9')) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_locale_numeric (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale_numeric?");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return scheme_false;

  return isdigit(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_whitespace (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-whitespace?");

  c = SCHEME_CHAR_VAL(argv[0]);

  if ((c == ' ')
      || (c == '\t')
      || (c == '\n')
      || (c == '\v')
      || (c == '\f')
      || (c == '\r'))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *
char_locale_whitespace (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale-whitespace?");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return scheme_false;

  return isspace(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_upper_case (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-upper-case?");

  c = SCHEME_CHAR_VAL(argv[0]);

  return ((c >= 'A') && (c <= 'Z')) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_locale_upper_case (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale-upper-case?");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return scheme_false;

  return isupper(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_lower_case (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-lower-case?");

  c = SCHEME_CHAR_VAL(argv[0]);

  return ((c >= 'a') && (c <= 'z')) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_locale_lower_case (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale-lower-case?");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return scheme_false;

  return islower(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_to_integer (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char->integer");

  c = SCHEME_CHAR_VAL(argv[0]);

  return scheme_make_integer(c);
}

static Scheme_Object *
integer_to_char (int argc, Scheme_Object *argv[])
{
  long v;

  if (!SCHEME_INTP(argv[0]))
    scheme_wrong_type("integer->char", "exact in [0, 255]", 0, argc, argv);

  v = SCHEME_INT_VAL(argv[0]);
  if ((v < 0) || (v > 255))
    scheme_wrong_type("integer->char", "exact in [0, 255]", 0, argc, argv);

  return _scheme_make_char(v);
}

#ifdef MACROMAN_CHAR_SET
static unsigned char latin1_to_mac_mapping[256];
static int l2m_mapping_inited = 0;
static unsigned char mac_to_latin1_mapping[256];
static int m2l_mapping_inited = 0;
# include "mac_roman.inc"
#endif

static Scheme_Object *
char_to_latin1_integer (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char->latin-1-integer");

  c = SCHEME_CHAR_VAL(argv[0]);

#if defined(WINLATIN_CHAR_SET)
  if ((0x80 <= c) && (c <= 0x9F))
    return scheme_false;
#endif

#ifdef MACROMAN_CHAR_SET
  if (!m2l_mapping_inited) {
    int i;
    for (i = 0; i < 256; i++) {
      mac_to_latin1_mapping[i] = (unsigned char)i;
    }
    for (i = 0; deviation_table[i]; i += 2) {
      if (deviation_table[i + 1])
	mac_to_latin1_mapping[deviation_table[i + 1]] = deviation_table[i];
    }
    for (i = 0; mac_extras_table[i]; i++) {
      mac_to_latin1_mapping[mac_extras_table[i]] = 0;
    }
      
    m2l_mapping_inited = 1;
  }
  
  if (c) {
    c = mac_to_latin1_mapping[(int)c];
    if (!c)
      return scheme_false;
  }
#endif

  return scheme_make_integer(c);
}

static Scheme_Object *
latin1_integer_to_char (int argc, Scheme_Object *argv[])
{
  long v;

  if (!SCHEME_INTP(argv[0]))
    scheme_wrong_type("latin1-integer->char", "exact in [0, 255]", 0, argc, argv);

  v = SCHEME_INT_VAL(argv[0]);
  if ((v < 0) || (v > 255))
    scheme_wrong_type("latin1-integer->char", "exact in [0, 255]", 0, argc, argv);

#if defined(MACROMAN_CHAR_SET) || defined(WINLATIN_CHAR_SET)
  if ((0x80 <= v) && (v <= 0x9F))
    return scheme_false;
#endif

#ifdef MACROMAN_CHAR_SET
  if (!l2m_mapping_inited) {
    int i;
    for (i = 0; i < 256; i++) {
      latin1_to_mac_mapping[i] = (unsigned char)i;
    }
    for (i = 0; deviation_table[i]; i += 2) {
      latin1_to_mac_mapping[deviation_table[i]] = deviation_table[i + 1];
    }
    l2m_mapping_inited = 1;
  }
  
  if (v) {
    v = latin1_to_mac_mapping[v];
    if (!v)
      return scheme_false;
  }
#endif

  return _scheme_make_char(v);
}

static Scheme_Object *
char_upcase (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-upcase");

  c = SCHEME_CHAR_VAL(argv[0]);

  if ((c >= 'a') && (c <= 'z'))
    return _scheme_make_char(c - ('a' - 'A'));
  else
    return argv[0];
}

static Scheme_Object *
char_locale_upcase (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale-upcase");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return argv[0];

  return _scheme_make_char(toupper(c));
}

static Scheme_Object *
char_downcase (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-downcase");

  c = SCHEME_CHAR_VAL(argv[0]);

  if ((c >= 'A') && (c <= 'Z'))
    return _scheme_make_char(c + ('a' - 'A'));
  else
    return argv[0];
}

static Scheme_Object *
char_locale_downcase (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char-locale-downcase");

  c = SCHEME_CHAR_VAL(argv[0]);

  /* ensure non-locale consistency above 127: */
  if (!scheme_locale_on && (c > 127)) return argv[0];

  return _scheme_make_char(tolower(c));
}

