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
#include <string.h>
#include <ctype.h>
#ifndef DONT_USE_LOCALE
# include <locale.h>
#endif

#ifndef SCHEME_PLATFORM_LIBRARY_SUBPATH
# include "schsys.h"
#endif

/* globals */
int scheme_locale_on;
static const char *current_locale_name = "C";

/* locals */
static Scheme_Object *make_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *version(int argc, Scheme_Object *argv[]);
static Scheme_Object *format(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_printf(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_fprintf(int argc, Scheme_Object *argv[]);
static Scheme_Object *banner(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_type(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[]);
static Scheme_Object *cmdline_args(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_locale(int argc, Scheme_Object *argv[]);

static int mz_strcmp(unsigned char *str1, int l1, unsigned char *str2, int l2, int eq);
static int mz_strcmp_ci(unsigned char *str1, int l1, unsigned char *str2, int l2, int eq);

static Scheme_Object *sys_symbol;
static Scheme_Object *platform_path;
#ifdef MZ_PRECISE_GC
static Scheme_Object *platform_path_no_variant;
#endif
static Scheme_Object *zero_length_string;

static Scheme_Hash_Table *putenv_str_table;

static char *embedding_banner;
static Scheme_Object *vers_str, *banner_str;

void
scheme_init_string (Scheme_Env *env)
{
  REGISTER_SO(sys_symbol);
  sys_symbol = scheme_intern_symbol(SYSTEM_TYPE_NAME);

  REGISTER_SO(zero_length_string);
  zero_length_string = scheme_alloc_string(0, 0); 

  REGISTER_SO(platform_path);
#ifdef MZ_PRECISE_GC
# ifdef UNIX_FILE_SYSTEM
#  define MZ3M_SUBDIR "/3m"
# else
#  ifdef DOS_FILE_SYSTEM
#   define MZ3M_SUBDIR "\\3m"
#  else
#   define MZ3M_SUBDIR ":3m"
#  endif
# endif
  REGISTER_SO(platform_path_no_variant);
  platform_path_no_variant = scheme_make_string(SCHEME_PLATFORM_LIBRARY_SUBPATH);
#else
# define MZ3M_SUBDIR /* empty */
#endif
  platform_path = scheme_make_string(SCHEME_PLATFORM_LIBRARY_SUBPATH MZ3M_SUBDIR);

  REGISTER_SO(putenv_str_table);

  REGISTER_SO(embedding_banner);

  scheme_add_global_constant("string?", 
			     scheme_make_folding_prim(string_p,
						      "string?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("make-string", 
			     scheme_make_prim_w_arity(make_string,
						      "make-string",
						      1, 2),
			     env);
  scheme_add_global_constant("string", 
			     scheme_make_prim_w_arity(string,
						      "string", 
						      0, -1),
			     env);
  scheme_add_global_constant("string-length", 
			     scheme_make_folding_prim(string_length,
						      "string-length",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("string-ref", 
			     scheme_make_prim_w_arity(string_ref,
						      "string-ref", 
						      2, 2),
			     env);
  scheme_add_global_constant("string-set!", 
			     scheme_make_prim_w_arity(string_set,
						      "string-set!", 
						      3, 3),
			     env);
  scheme_add_global_constant("string=?", 
			     scheme_make_prim_w_arity(string_eq,
						      "string=?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-ci=?", 
			     scheme_make_prim_w_arity(string_ci_eq,
						      "string-ci=?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-locale-ci=?", 
			     scheme_make_prim_w_arity(string_locale_ci_eq,
						      "string-locale-ci=?",
						      2, -1),
			     env);
  scheme_add_global_constant("string<?", 
			     scheme_make_prim_w_arity(string_lt,
						      "string<?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-locale<?", 
			     scheme_make_prim_w_arity(string_locale_lt,
						      "string-locale<?",
						      2, -1),
			     env);
  scheme_add_global_constant("string>?", 
			     scheme_make_prim_w_arity(string_gt,
						      "string>?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-locale>?", 
			     scheme_make_prim_w_arity(string_locale_gt,
						      "string-locale>?",
						      2, -1),
			     env);
  scheme_add_global_constant("string<=?", 
			     scheme_make_prim_w_arity(string_lt_eq,
						      "string<=?",
						      2, -1),
			     env);
  scheme_add_global_constant("string>=?", 
			     scheme_make_prim_w_arity(string_gt_eq,
						      "string>=?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-ci<?", 
			     scheme_make_prim_w_arity(string_ci_lt,
						      "string-ci<?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-locale-ci<?", 
			     scheme_make_prim_w_arity(string_locale_ci_lt,
						      "string-locale-ci<?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-ci>?", 
			     scheme_make_prim_w_arity(string_ci_gt,
						      "string-ci>?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-locale-ci>?", 
			     scheme_make_prim_w_arity(string_locale_ci_gt,
						      "string-locale-ci>?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-ci<=?", 
			     scheme_make_prim_w_arity(string_ci_lt_eq,
						      "string-ci<=?",
						      2, -1),
			     env);
  scheme_add_global_constant("string-ci>=?", 
			     scheme_make_prim_w_arity(string_ci_gt_eq,
						      "string-ci>=?",
						      2, -1),
			     env);
  scheme_add_global_constant("substring", 
			     scheme_make_prim_w_arity(substring,
						      "substring", 
						      2, 3),
			     env);
  scheme_add_global_constant("string-append", 
			     scheme_make_prim_w_arity(string_append,
						      "string-append", 
						      0, -1),
			     env);
  scheme_add_global_constant("string->list", 
			     scheme_make_prim_w_arity(string_to_list,
						      "string->list",
						      1, 1),
			     env);
  scheme_add_global_constant("list->string", 
			     scheme_make_prim_w_arity(list_to_string,
						      "list->string",
						      1, 1),
			     env);
  scheme_add_global_constant("string-copy", 
			     scheme_make_prim_w_arity(string_copy,
						      "string-copy",
						      1, 1),
			     env);
  scheme_add_global_constant("string-fill!", 
			     scheme_make_prim_w_arity(string_fill,
						      "string-fill!", 
						      2, 2),
			     env);
  scheme_add_global_constant("string->immutable-string", 
			     scheme_make_prim_w_arity(string_to_immutable,
						      "string->immutable-string",
						      1, 1),
			     env);
  

  scheme_add_global_constant("format", 
			     scheme_make_folding_prim(format,
						      "format", 
						      1, -1, 1),
			     env);
  scheme_add_global_constant("printf", 
			     scheme_make_prim_w_arity(sch_printf,
						      "printf", 
						      1, -1),
			     env);
  scheme_add_global_constant("fprintf", 
			     scheme_make_prim_w_arity(sch_fprintf,
						      "fprintf", 
						      2, -1),
			     env);
  

  /* In principle, `version' could be foldable, but it invites
     more problems than it solves... */

  scheme_add_global_constant("version", 
			     scheme_make_prim_w_arity(version,
						      "version", 
						      0, 0),
			     env);
  scheme_add_global_constant("banner", 
			     scheme_make_prim_w_arity(banner,
						      "banner", 
						      0, 0),
			     env);
  
  scheme_add_global_constant("getenv", 
			     scheme_make_prim_w_arity(sch_getenv,
						      "getenv",
						      1, 1),
			     env);
  scheme_add_global_constant("putenv", 
			     scheme_make_prim_w_arity(sch_putenv,
						      "putenv", 
						      2, 2),
			     env);
  
  /* Don't make these folding, since they're platform-specific: */

  scheme_add_global_constant("system-type", 
			     scheme_make_prim_w_arity(system_type,
						      "system-type", 
						      0, 1),
			     env);
  scheme_add_global_constant("system-library-subpath",
			     scheme_make_prim_w_arity(system_library_subpath,
						      "system-library-subpath",
						      0, 1),
			     env);

  scheme_add_global_constant("current-command-line-arguments", 
			     scheme_register_parameter(cmdline_args, 
						       "current-command-line-arguments",
						       MZCONFIG_CMDLINE_ARGS), 
			     env);

  scheme_add_global_constant("current-locale", 
			     scheme_register_parameter(current_locale, 
						       "current-locale",
						       MZCONFIG_LOCALE), 
			     env);

  scheme_reset_locale();
}

void
scheme_init_getenv(void)
{
#ifndef GETENV_FUNCTION
  FILE *f = fopen("Environment", "r");
  if (f) {
    Scheme_Object *p = scheme_make_file_input_port(f);
    mz_jmp_buf savebuf;
    memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));
    if (!scheme_setjmp(scheme_error_buf)) {
      while (1) {
	Scheme_Object *v = scheme_read(p);
	if (SCHEME_EOFP(v))
	  break;

	if (SCHEME_PAIRP(v) && SCHEME_PAIRP(SCHEME_CDR(v))
	    && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(v)))) {
	  Scheme_Object *key = SCHEME_CAR(v);
	  Scheme_Object *val = SCHEME_CADR(v);
	  if (SCHEME_STRINGP(key) && SCHEME_STRINGP(val)) {
	    Scheme_Object *a[2];
	    a[0] = key;
	    a[1] = val;
	    sch_putenv(2, a);
	    v = NULL;
	  }
	}

	if (v)
	  scheme_signal_error("bad environment specification: %V", v);
      }
    }
    memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
    scheme_close_input_port(p);
  }
#endif
}

Scheme_Object *
scheme_make_sized_offset_string(char *chars, long d, long len, int copy)
{
  Scheme_Object *str;

  if (!chars) chars = "";

  str = scheme_alloc_object();
  str->type = scheme_string_type;

  if (len < 0)
    len = strlen(chars + d);
  if (copy) {
    char *naya;

    naya = (char *)scheme_malloc_fail_ok(scheme_malloc_atomic, len + 1);
    SCHEME_STR_VAL(str) = naya;
    memcpy(naya, chars + d, len);
    naya[len] = 0;
  } else
    SCHEME_STR_VAL(str) = chars + d;
  SCHEME_STRTAG_VAL(str) = len;

  return str;
}

Scheme_Object *
scheme_make_sized_string(char *chars, long len, int copy)
{
  return scheme_make_sized_offset_string(chars, 0, len, copy);
}

Scheme_Object *
scheme_make_immutable_sized_string(char *chars, long len, int copy)
{
  Scheme_Object *s;
  
  s = scheme_make_sized_offset_string(chars, 0, len, copy);
  SCHEME_SET_STRING_IMMUTABLE(s);

  return s;
}

Scheme_Object *
scheme_make_string_without_copying(char *chars)
{
  return scheme_make_sized_offset_string(chars, 0, -1, 0);
}

Scheme_Object *
scheme_make_string(const char *chars)
{
  return scheme_make_sized_offset_string((char *)chars, 0, -1, 1);
}

Scheme_Object *
scheme_alloc_string(int size, char fill)
{
  Scheme_Object *str;
  char *s;
  int i;
  
  if (size < 0) {
    str = scheme_make_integer(size);
    scheme_wrong_type("make-string", "non-negative exact integer",
		      -1, 0, &str);
  }

  str = scheme_alloc_object();
  str->type = scheme_string_type;
  s = (char *)scheme_malloc_fail_ok(scheme_malloc_atomic, sizeof(char)*(size + 1));
  for (i = size; i--; ) {
    s[i] = fill;
  }
  s[size] = '\0';
  SCHEME_STR_VAL(str) = s;
  SCHEME_STRTAG_VAL(str) = size;

  return str;
}

void scheme_out_of_string_range(const char *name, const char *which, 
				Scheme_Object *i, Scheme_Object *s, 
				long start, long len)
{
  if (SCHEME_STRTAG_VAL(s)) {
    char *sstr;
    int slen;
    
    sstr = scheme_make_provided_string(s, 2, &slen);
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     scheme_make_integer(i),
		     "%s: %sindex %s out of range [%d, %d] for string: %t",
		     name, which,
		     scheme_make_provided_string(i, 2, NULL), 
		     start, len,
		     sstr, slen);
  } else {
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     scheme_make_integer(i),
		     "%s: %sindex %s out of range for empty string",
		     name, which,
		     scheme_make_provided_string(i, 0, NULL));
  }
}

long scheme_extract_index(const char *name, int pos, int argc, Scheme_Object **argv, long top, int false_ok)
{
  long i;
  int is_top = 0;

  if (SCHEME_INTP(argv[pos])) {
    i = SCHEME_INT_VAL(argv[pos]);
  } else if (SCHEME_BIGNUMP(argv[pos])) {
    if (SCHEME_BIGPOS(argv[pos])) {
      i = top; /* out-of-bounds */
      is_top = 1;
    } else
      i = -1; /* negative */
  } else
    i = -1;

  if (!is_top && (i < 0))
    scheme_wrong_type(name, 
		      (false_ok ? "non-negative exact integer or #f" : "non-negative exact integer"), 
		      pos, argc, argv);
  
  return i;
}

/* locals */

static Scheme_Object *
string_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_STRINGP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
make_string (int argc, Scheme_Object *argv[])
{
  long len;
  char fill;
  Scheme_Object *str;

  len = scheme_extract_index("make-string", 0, argc, argv, -1, 0);

  if (len == -1) {
    scheme_raise_out_of_memory("make-string", "making string of length %s",
			       scheme_make_provided_string(argv[0], 0, NULL));
  }

  if (argc == 2) {
    if (!SCHEME_CHARP(argv[1]))
      scheme_wrong_type("make-string", "character", 1, argc, argv);
    fill = SCHEME_CHAR_VAL(argv[1]);
  } else
    fill = 0;

  str = scheme_alloc_string(len, fill);
  return (str);
}

static Scheme_Object *
string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *str;
  int i;

  str = scheme_alloc_string(argc, 0);

  for ( i=0 ; i<argc ; ++i ) {
    if (!SCHEME_CHARP (argv[i]))
      scheme_wrong_type("string", "character", i, argc, argv);
    SCHEME_STR_VAL(str)[i] = SCHEME_CHAR_VAL(argv[i]);
  }

  return (str);
}

static Scheme_Object *
string_length (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-length", "string", 0, argc, argv);

  return scheme_make_integer(SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
string_ref (int argc, Scheme_Object *argv[])
{
  long i, len;
  char *str;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-ref", "string", 0, argc, argv);

  str = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  i = scheme_extract_index("string-ref", 1, argc, argv, len, 0);

  if (i >= len) {
    scheme_out_of_string_range("string-ref", "", argv[1], argv[0], 0, len - 1);
    return NULL;
  }

  return scheme_make_char(str[i]);
}

static Scheme_Object *
string_set (int argc, Scheme_Object *argv[])
{
  long i, len;
  char *str;

  if (!SCHEME_MUTABLE_STRINGP(argv[0]))
    scheme_wrong_type("string-set!", "mutable-string", 0, argc, argv);

  str = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  i = scheme_extract_index("string-set!", 1, argc, argv, len, 0);

  if (!SCHEME_CHARP(argv[2]))
    scheme_wrong_type("string-set!", "character", 2, argc, argv);

  if (i >= len) {
    scheme_out_of_string_range("string-set!", "", argv[1], argv[0], 0, len - 1);
    return NULL;
  }

  str[i] = SCHEME_CHAR_VAL(argv[2]);

  return scheme_void;
}

/* comparisons */

#define GEN_STRING_COMP(name, scheme_name, comp, op, ul) \
static Scheme_Object * name (int argc, Scheme_Object *argv[]) \
{  char *s, *prev; int i, sl, pl; int falz = 0;\
   if (!SCHEME_STRINGP(argv[0])) \
    scheme_wrong_type(scheme_name, "string", 0, argc, argv); \
   prev = SCHEME_STR_VAL(argv[0]); pl = SCHEME_STRTAG_VAL(argv[0]); \
   for (i = 1; i < argc; i++) { \
     if (!SCHEME_STRINGP(argv[i])) \
      scheme_wrong_type(scheme_name, "string", i, argc, argv); \
     s = SCHEME_STR_VAL(argv[i]); sl = SCHEME_STRTAG_VAL(argv[i]); \
     if (!falz) if (!(comp((unsigned char *)prev, pl, \
                           (unsigned char *)s, sl, ul) op 0)) falz = 1; \
     prev = s; pl = sl; \
  } \
  return falz ? scheme_false : scheme_true; \
}

GEN_STRING_COMP(string_eq, "string=?", mz_strcmp, ==, 0)
GEN_STRING_COMP(string_lt, "string<?", mz_strcmp, <, 0)
GEN_STRING_COMP(string_gt, "string>?", mz_strcmp, >, 0)
GEN_STRING_COMP(string_lt_eq, "string<=?", mz_strcmp, <=, 0)
GEN_STRING_COMP(string_gt_eq, "string>=?", mz_strcmp, >=, 0)

GEN_STRING_COMP(string_ci_eq, "string-ci=?", mz_strcmp_ci, ==, 0)
GEN_STRING_COMP(string_ci_lt, "string-ci<?", mz_strcmp_ci, <, 0)
GEN_STRING_COMP(string_ci_gt, "string-ci>?", mz_strcmp_ci, >, 0)
GEN_STRING_COMP(string_ci_lt_eq, "string-ci<=?", mz_strcmp_ci, <=, 0)
GEN_STRING_COMP(string_ci_gt_eq, "string-ci>=?", mz_strcmp_ci, >=, 0)

GEN_STRING_COMP(string_locale_lt, "string-locale<?", mz_strcmp, <, 1)
GEN_STRING_COMP(string_locale_gt, "string-locale>?", mz_strcmp, >, 1)
GEN_STRING_COMP(string_locale_ci_eq, "string-locale-ci=?", mz_strcmp_ci, ==, 1)
GEN_STRING_COMP(string_locale_ci_lt, "string-locale-ci<?", mz_strcmp_ci, <, 1)
GEN_STRING_COMP(string_locale_ci_gt, "string-locale-ci>?", mz_strcmp_ci, >, 1)


void scheme_get_substring_indices(const char *name, Scheme_Object *str, 
				  int argc, Scheme_Object **argv, 
				  int spos, int fpos, long *_start, long *_finish)
{
  long len = SCHEME_STRTAG_VAL(str);
  long start, finish;

  if (argc > spos)
    start = scheme_extract_index(name, spos, argc, argv, len + 1, 0);
  else
    start = 0;
  if (argc > fpos)
    finish = scheme_extract_index(name, fpos, argc, argv, len + 1, 0);
  else
    finish = len;

  if (!(start <= len)) {
    scheme_out_of_string_range(name, "starting ", argv[spos], str, 0, len);
  }
  if (!(finish >= start && finish <= len)) {
    scheme_out_of_string_range(name, "ending ", argv[fpos], str, start, len);
  }

  *_start = start;
  *_finish = finish;
}

static Scheme_Object *
substring (int argc, Scheme_Object *argv[])
{
  long start, finish, _start, _finish, i;
  char *chars, *dchars;
  Scheme_Object *str;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("substring", "string", 0, argc, argv);

  chars = SCHEME_STR_VAL(argv[0]);

  scheme_get_substring_indices("substring", argv[0], argc, argv, 1, 2,
			       &_start, &_finish);
  start = _start;
  finish = _finish;

  str = scheme_alloc_string(finish-start, 0);
  dchars = SCHEME_STR_VAL(str);
  for (i = 0; i < finish-start; i++) {
    dchars[i] = chars[i+start];
  }

  return (str);
}

static Scheme_Object *
string_append (int argc, Scheme_Object *argv[])
{
  Scheme_Object *naya, *s;
  char *chars;
  int i;
  long len;

  if (argc == 2) {
    Scheme_Object *s1 = argv[0], *s2 = argv[1];
    if (!SCHEME_STRINGP(s1))
      scheme_wrong_type("string-append", "string", 0, argc, argv);
    if (!SCHEME_STRINGP(s2))
      scheme_wrong_type("string-append", "string", 1, argc, argv);
    return scheme_append_string(s1, s2);
  }

  if (!argc)
    return zero_length_string;
  else if (argc == 1)
    return scheme_append_string(zero_length_string, argv[0]);
  
  len = 0;
  for (i = 0; i < argc; i++) {
    s = argv[i];
    if (!SCHEME_STRINGP(s))
      scheme_wrong_type("string-append", "string", i, argc, argv);
    len += SCHEME_STRLEN_VAL(s);
  }

  naya = scheme_alloc_string(len, 0);
  chars = SCHEME_STR_VAL(naya);

  for (i = 0; i < argc; i++) {
    s = argv[i];
    len = SCHEME_STRLEN_VAL(s);
    memcpy(chars, SCHEME_STR_VAL(s), len);
    chars += len;
  }
  
  return naya;
}

Scheme_Object *
scheme_append_string(Scheme_Object *str1, Scheme_Object *str2)
{
  int len1, len2, i;
  char *chars1, *chars2, *r;
  Scheme_Object *naya;

  if (!SCHEME_STRINGP(str1))
    scheme_wrong_type("string-append", "string", -1, 0, &str1);
  if (!SCHEME_STRINGP(str2))
    scheme_wrong_type("string-append", "string", -1, 0, &str2);

  chars1 = SCHEME_STR_VAL(str1);
  chars2 = SCHEME_STR_VAL(str2);
  len1 = SCHEME_STRTAG_VAL(str1);
  len2 = SCHEME_STRTAG_VAL(str2);
  naya = scheme_alloc_string(len1 + len2, 0);

  r = SCHEME_STR_VAL(naya);
  for (i = 0; i < len1; i++, r++) {
    *r = chars1[i];
  }

  for (i = 0; i < len2; i++, r++) {
    *r = chars2[i];
  }

  *r = '\0';

  return naya;
}


static Scheme_Object *
string_to_list (int argc, Scheme_Object *argv[])
{
  int len, i;
  char *chars;
  Scheme_Object *pair = scheme_null;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->list", "string", 0, argc, argv);

  chars = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  if (len < 0xFFF) {
    for (i = len ; i--; ) {
      pair = scheme_make_pair(scheme_make_char(chars[i]), pair);
    }
  } else {
    for (i = len ; i--; ) {
      if (!(i & 0xFFF))
	SCHEME_USE_FUEL(0xFFF);
      pair = scheme_make_pair(scheme_make_char(chars[i]), pair);
    }
  }

  return pair;
}

static Scheme_Object *
list_to_string (int argc, Scheme_Object *argv[])
{
  int len, i;
  Scheme_Object *list, *str, *ch;

  list = argv[0];
  len = scheme_list_length (list);
  str = scheme_alloc_string (len, 0);
  i = 0;
  while (SCHEME_PAIRP (list))
    {
      ch = SCHEME_CAR (list);

      if (!SCHEME_CHARP(ch))
	scheme_wrong_type("list->string", "proper character list", 0, 
			  argc, argv);

      SCHEME_STR_VAL(str)[i] = SCHEME_CHAR_VAL(ch);
      i++;
      list = SCHEME_CDR (list);
    }  

  if (!SCHEME_NULLP(list))
    scheme_wrong_type("list->string", "proper character list", 0, argc, argv);

  return (str);
}

static Scheme_Object *
string_copy (int argc, Scheme_Object *argv[])
{
  Scheme_Object *naya;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-copy", "string", 0, argc, argv);

  naya = scheme_make_sized_string(SCHEME_STR_VAL(argv[0]), 
				  SCHEME_STRTAG_VAL(argv[0]), 1);
  return naya;
}

static Scheme_Object *
string_fill (int argc, Scheme_Object *argv[])
{
  int len, i;
  char *chars, ch;

  if (!SCHEME_MUTABLE_STRINGP(argv[0]))
    scheme_wrong_type("string-fill!", "mutable-string", 0, argc, argv);
  if (!SCHEME_CHARP(argv[1]))
    scheme_wrong_type("string-fill!", "character", 1, argc, argv);
  
  chars = SCHEME_STR_VAL (argv[0]);
  ch = SCHEME_CHAR_VAL (argv[1]);
  len = SCHEME_STRTAG_VAL (argv[0]);
  for (i = 0; i < len; i++) {
    chars[i] = ch;
  }

  return scheme_void;
}

static Scheme_Object *string_to_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *s = argv[0];

  if (!SCHEME_STRINGP(s))
    scheme_wrong_type("string->immutable-string", "string", 0, argc, argv);

  if (SCHEME_MUTABLE_STRINGP(s)) {
    Scheme_Object *s2;
    s2 = scheme_make_sized_string(SCHEME_STR_VAL(s), SCHEME_STRLEN_VAL(s), 1);
    SCHEME_SET_STRING_IMMUTABLE(s2);
    return s2;
  } else
    return s;
}

static int mz_strcmp(unsigned char *str1, int l1, unsigned char *str2, int l2, int use_locale)
{
  int endres;
  
  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

#ifndef DONT_USE_LOCALE
  if (use_locale && scheme_locale_on) {
    /* Walk back through the strings looking for
       nul characters. If we find one, compare
       the part after the null character to update
       endres, then continue. Unfortunately, we
       do too much work if an earlier part of the
       string (tested later) determines the result,
       but hopefully nul characters are rare. 

       We assume that strcoll() always treats shorter
       as <, (i.e., "abc" is always less than "abcd"). */
    int v;

    while (l1--) {
      if (!(str1[l1]) || !(str2[l1])) {
	if (str1[l1])
	  endres = 1;
	else if (str2[l1])
	  endres = -1;

	v = strcoll((char *)str1 + l1 + 1,(char *) str2 + l1 + 1);
	if (v)
	  endres = v;
      }
    }

    v = strcoll((char *)str1, (char *)str2);
    if (v)
      endres = v;

    return endres;
  }
#endif

  while (l1--) {
    unsigned int a, b;
    
    a = *(str1++);
    b = *(str2++);
    
    a = a - b;
    if (a)
      return a;
  }

  return endres;
}

static int mz_strcmp_ci(unsigned char *str1, int l1, unsigned char *str2, int l2, int use_locale)
{
  int endres;

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

#ifndef DONT_USE_LOCALE
  if (use_locale && scheme_locale_on) {
# define mzCASE_BUF 100
    unsigned char *cstr1, *cstr2;
    unsigned char buf1[mzCASE_BUF], buf2[mzCASE_BUF];
    int i;

    if (l1 < mzCASE_BUF) {
      cstr1 = buf1;
      cstr2 = buf2;
    } else {
      cstr1 = scheme_malloc_atomic(l1 + 1);
      cstr2 = scheme_malloc_atomic(l1 + 1);
    }

    memcpy(cstr1, str1, l1);
    memcpy(cstr2, str2, l1);
    cstr1[l1] = cstr2[l1] = '\0';

    for (i = 0; i < l1; ++i) {
      cstr1[i] = toupper(cstr1[i]);
      cstr2[i] = toupper(cstr2[i]);
    }

    i = mz_strcmp(cstr1, l1, cstr2, l1, 1);
    if (i)
      endres = i;

    return endres;
  }
#endif

  while (l1--) {
    unsigned int a, b;
    
    a = *(str1++);
    b = *(str2++);
    a = mz_portable_toupper(a);
    b = mz_portable_toupper(b);

    a = a - b;
    if (a)
      return a;
  }

  return endres;
}

void scheme_do_format(const char *procname, Scheme_Object *port, 
		      const unsigned char *format, int flen, 
		      int fpos, int offset, int argc, Scheme_Object **argv)
{
  int i, start, end;
  int used = offset;
  int num_err = 0, char_err = 0, end_ok = 0;
  Scheme_Object *a[2];

  if (!format) {
    if (!SCHEME_STRINGP(argv[fpos])) {
      scheme_wrong_type(procname, "format-string", fpos, argc, argv);
      return;
    }
    format = (unsigned char *)SCHEME_STR_VAL(argv[fpos]);
    flen = SCHEME_STRTAG_VAL(argv[fpos]);
  } else if (flen == -1)
    flen = strlen((char *)format);

  /* Check string first: */
  end = flen - 1;
  for (i = 0; i < end; i++) {
    if (format[i] == '~') {
      i++;
      if ((format[i] < 128) && isspace(format[i])) {
	/* skip spaces... */
      } else switch (format[i]) {
      case '~':
	if (i == end)
	  end_ok = 1;
	break;
      case '%':
      case 'n':
      case 'N':
	break;
      case 'a':
      case 'A':
      case 's':
      case 'S':
      case 'v':
      case 'V':
      case 'e':
      case 'E':
	used++;
	break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	if (!num_err && !char_err && (used < argc)) {
	  Scheme_Object *o = argv[used];
	  if (!SCHEME_EXACT_REALP(o)
	      && (!SCHEME_COMPLEXP(o)
		  || !SCHEME_EXACT_REALP(scheme_complex_real_part(o))))
	    num_err = used + 1;
	}
	used++;
	break;
      case 'c':
      case 'C':
	if (!num_err && !char_err && (used < argc)) {
	  if (!SCHEME_CHARP(argv[used]))
	    char_err = used + 1;
	}
	used++;
	break;
      default:
	{
	  char buffer[64];
	  sprintf(buffer, "pattern-string (tag ~%c not allowed)", format[i]);
	  scheme_wrong_type(procname, buffer, fpos, argc, argv);
	  return;
	}
      }
    }
  }
  if ((format[end] == '~') && !end_ok) {
    scheme_wrong_type(procname, "pattern-string (cannot end in ~)", fpos, argc, argv);
    return;
  }
  if (used != argc) {
    char *args;
    long alen;

    args = scheme_make_args_string("", -1, argc, argv, &alen);

    if (used > argc) {
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       argv[fpos],
		       "%s: format string requires %d arguments, given %d%t",
		       procname, used - offset, argc - offset, args, alen);
    } else {
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       argv[fpos],
		       "%s: format string requires %d arguments, given %d%t",
		       procname, used - offset, argc - offset, args, alen);
    }
    return;
  }
  if (num_err || char_err) {
    int pos = (num_err ? num_err : char_err) - 1;
    char *args, *bstr;
    long alen;
    int blen;
    char *type = (num_err ? "exact-number" : "character");
    Scheme_Object *bad = argv[pos];

    args = scheme_make_args_string("other ", pos, argc, argv, &alen);
    bstr = scheme_make_provided_string(bad, 1, &blen);
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     bad,
		     "%s: format string requires argument of type <%s>, given %t%t",
		     procname, type, 
		     bstr, blen,
		     args, alen);
    return;
  }

  for (used = offset, i = start = 0; i < flen; i++) {
    if (format[i] == '~') {
      if (start < i) {
	(void)scheme_put_string(procname, port, (char *)format, start, i - start, 0);
      }
      i++;
      if (isspace(format[i])) {
	/* skip spaces (at most one newline) */
	do {
	  if ((format[i] == '\n') || (format[i] == '\r')) {
	    /* got one */
	    if ((format[i] == '\r') && (format[i + 1] == '\n'))
	      i++; /* Windows-style CR-NL */
	    i++;
	    while (isspace(format[i]) 
		   && !((format[i] == '\n') || (format[i] == '\r'))) {
	      i++;
	    }
	    break;
	  } else
	    i++;
	} while (isspace(format[i]));
	--i; /* back up over something */
      } else switch (format[i]) {
      case '~':
	scheme_write_string("~", 1, port);
	break;
      case '%':
      case 'n':
      case 'N':
	scheme_write_string("\n", 1, port);
	break;
      case 'c':
      case 'C':
      case 'a':
      case 'A':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_display_proc, 2, a);
	break;
      case 's':
      case 'S':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_write_proc, 2, a);
	break;
      case 'v':
      case 'V':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_print_proc, 2, a);
	break;
      case 'e':
      case 'E':
	{
	  int len;
	  char *s;
	  s = scheme_make_provided_string(argv[used++], 0, &len);
	  scheme_write_string(s, len, port);
	}
	break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	{
	  char *s;
	  int radix;

	  switch(format[i]) {
	  case 'x':
	  case 'X':
	    radix = 16;
	    break;
	  case 'o':
	  case 'O':
	    radix = 8;
	    break;
	  default:
	  case 'b':
	  case 'B':
	    radix = 2;
	    break;
	  }
	  s = scheme_number_to_string(radix, argv[used++]);
	  
	  scheme_write_string(s, strlen(s), port);
	}
	break;
      }
      SCHEME_USE_FUEL(1);
      start = i + 1;
    }
  }

  SCHEME_USE_FUEL(flen);

  if (start < i) {
    (void)scheme_put_string(procname, port, (char *)format, start, i - start, 0);
  }
}

char *scheme_format(char *format, int flen, int argc, Scheme_Object **argv, long *rlen)
{
  Scheme_Object *port;
  port = scheme_make_string_output_port();
  scheme_do_format("format", port, (unsigned char *)format, flen, 0, 0, argc, argv);
  return scheme_get_sized_string_output(port, rlen);
}

void scheme_printf(char *format, int flen, int argc, Scheme_Object **argv)
{
  scheme_do_format("printf", scheme_get_param(scheme_config, MZCONFIG_OUTPUT_PORT), 
	    (unsigned char *)format, flen, 0, 0, argc, argv);
}

static Scheme_Object *
format(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  char *s;
  long len;

  port = scheme_make_string_output_port();

  scheme_do_format("format", port, NULL, 0, 0, 1, argc, argv);

  s = scheme_get_sized_string_output(port, &len);
  return scheme_make_sized_string(s, len, 0);
}

static Scheme_Object *
sch_printf(int argc, Scheme_Object *argv[])
{
  scheme_do_format("printf", scheme_get_param(scheme_config, MZCONFIG_OUTPUT_PORT), 
		   NULL, 0, 0, 1, argc, argv);
  return scheme_void;
}

static Scheme_Object *
sch_fprintf(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("fprintf", "output-port", 0, argc, argv);

  scheme_do_format("fprintf", argv[0], NULL, 0, 1, 2, argc, argv);
  return scheme_void;
}

static Scheme_Object *
version(int argc, Scheme_Object *argv[])
{
  if (!vers_str) {
    REGISTER_SO(vers_str);
    vers_str = scheme_make_string(scheme_version());
    SCHEME_SET_STRING_IMMUTABLE(vers_str);
  }

  return vers_str;
}

static Scheme_Object *
banner(int argc, Scheme_Object *argv[])
{
  if (!banner_str) {
    REGISTER_SO(banner_str);
    banner_str = scheme_make_string(scheme_banner());
    SCHEME_SET_STRING_IMMUTABLE(banner_str);
  }

  return banner_str;
}

char *scheme_version(void)
{
  return MZSCHEME_VERSION;
}

#ifdef USE_SENORA_GC
# define VERSION_SUFFIX " (sgc)"
#else
# ifdef USE_LOCALE_STRCMP
#  define VERSION_SUFFIX " (using locale strcmp)"
# else
#  define VERSION_SUFFIX /* empty */
# endif
#endif

char *scheme_banner(void)
{
  if (embedding_banner)
    return embedding_banner;
  else
    return "Welcome to MzScheme" 
#ifdef MZ_PRECISE_GC
      "3m"
#endif      
      " version " MZSCHEME_VERSION VERSION_SUFFIX
      ", Copyright (c) 2004 PLT Scheme, Inc.\n";
}

void scheme_set_banner(char *s)
{
  embedding_banner = s;
}

int scheme_string_has_null(Scheme_Object *o)
{
  const char *s = SCHEME_STR_VAL(o);
  int i = SCHEME_STRTAG_VAL(o);
  while (i--) {
    if (!s[i])
      return 1;
  }
  return 0;
}

static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[])
{
  char *s;

  if (!SCHEME_STRINGP(argv[0]) || scheme_string_has_null(argv[0]))
    scheme_wrong_type("getenv", STRING_W_NO_NULLS, 0, argc, argv);

#ifdef GETENV_FUNCTION
  s = getenv(SCHEME_STR_VAL(argv[0]));
#else
  if (putenv_str_table) {
    s = (char *)scheme_hash_get(putenv_str_table, (Scheme_Object *)SCHEME_STR_VAL(argv[0]));
    /* If found, skip over the `=' in the table: */
    if (s)
      s += SCHEME_STRTAG_VAL(argv[0]) + 1;
  } else
    s = NULL;
#endif

  if (s)
    return scheme_make_string(s);

  return scheme_false;
}

static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[])
{
  char *s, *var, *val;
  long varlen, vallen;

  if (!SCHEME_STRINGP(argv[0]) || scheme_string_has_null(argv[0]))
    scheme_wrong_type("putenv", STRING_W_NO_NULLS, 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]) || scheme_string_has_null(argv[1]))
    scheme_wrong_type("putenv", STRING_W_NO_NULLS, 1, argc, argv);

  var = SCHEME_STR_VAL(argv[0]);
  val = SCHEME_STR_VAL(argv[1]);

  varlen = strlen(var);
  vallen = strlen(val);

  s = (char *)scheme_malloc_atomic(varlen + vallen + 2);
  memcpy(s, var, varlen);
  memcpy(s + varlen + 1, val, vallen + 1);
  s[varlen] = '=';

#ifdef MZ_PRECISE_GC
  {
    /* Can't put moveable string into array. */
    char *ss;
    ss = s;
    s = malloc(varlen + vallen + 2);
    memcpy(s, ss, varlen + vallen + 2);
    
    /* Free old, if in table: */
    if (putenv_str_table) {
      ss = (char *)scheme_hash_get(putenv_str_table, (Scheme_Object *)var);
      if (ss)
	free(ss);
    }
  }
#endif

  if (!putenv_str_table)
    putenv_str_table = scheme_make_hash_table(SCHEME_hash_string);

  scheme_hash_set(putenv_str_table, (Scheme_Object *)var, (Scheme_Object *)s);

#ifdef GETENV_FUNCTION
  return MSC_IZE(putenv)(s) ? scheme_false : scheme_true;
#else
  return scheme_true;
#endif
}

static void machine_details(char *s);

static Scheme_Object *system_type(int argc, Scheme_Object *argv[])
{
  if (!argc || SCHEME_FALSEP(argv[0]))
    return sys_symbol;
  else {
    char buff[1024];

    machine_details(buff);

    return scheme_make_string(buff);
  }
}

static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[])
{
#ifdef MZ_PRECISE_GC
  if ((argc > 0) && SCHEME_FALSEP(argv[0]))
    return platform_path_no_variant;
  else
#endif
    return platform_path;
}

const char *scheme_system_library_subpath()
{
  return SCHEME_PLATFORM_LIBRARY_SUBPATH;
}

/* Our own strncpy - which would be really stupid, except the one for
   the implementation in Solaris 2.6 is broken (it doesn't always stop
   at the null terminator). */
int scheme_strncmp(const char *a, const char *b, int len)
{
  while (len-- && (*a == *b) && *a) {
    a++;
    b++;
  }

  if (len < 0)
    return 0;
  else
    return *a - *b;
}

static Scheme_Object *ok_cmdline(int argc, Scheme_Object **argv)
{
  if (SCHEME_VECTORP(argv[0])) {
    Scheme_Object *vec = argv[0], *vec2, *str;
    int i, size = SCHEME_VEC_SIZE(vec);


    if (!size)
      return vec;

    for (i = 0; i < size; i++) {
      if (!SCHEME_STRINGP(SCHEME_VEC_ELS(vec)[i]))
	return NULL;
    }
    
    /* Make sure vector and strings are immutable: */
    vec2 = scheme_make_vector(size, NULL);
    if (size)
      SCHEME_SET_VECTOR_IMMUTABLE(vec2);
    for (i = 0; i < size; i++) {
      str = SCHEME_VEC_ELS(vec)[i];
      if (!SCHEME_IMMUTABLE_STRINGP(str)) {
	str = scheme_make_sized_string(SCHEME_STR_VAL(str), SCHEME_STRLEN_VAL(str), 0);
	SCHEME_SET_STRING_IMMUTABLE(str);
      }
      SCHEME_VEC_ELS(vec2)[i] = str;
    }

    return vec2;
  }

  return NULL;
}

static Scheme_Object *cmdline_args(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-command-line-arguments", 
			     scheme_make_integer(MZCONFIG_CMDLINE_ARGS),
			     argc, argv,
			     -1, ok_cmdline, "vector of strings", 1);
}

static Scheme_Object *ok_locale(int argc, Scheme_Object **argv)
{
  if (SCHEME_FALSEP(argv[0]))
    return argv[0];
  else if (SCHEME_STRINGP(argv[0])) {
    if (SCHEME_IMMUTABLEP(argv[0]))
      return argv[0];
    else {
      Scheme_Object *str = argv[0];
      str = scheme_make_immutable_sized_string(SCHEME_STR_VAL(str), SCHEME_STRLEN_VAL(str), 0);
      return str;
    }
  }

  return NULL;
}

static Scheme_Object *current_locale(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = scheme_param_config("current-locale", 
			  scheme_make_integer(MZCONFIG_LOCALE), 
			  argc, argv, 
			  -1, ok_locale, "#f or string", 1);

  scheme_reset_locale();

  return v;
}

void scheme_reset_locale(void)
{
  Scheme_Object *v;
  const char *name;

  v = scheme_get_param(scheme_config, MZCONFIG_LOCALE);
  scheme_locale_on = SCHEME_TRUEP(v);

  if (scheme_locale_on) {
    name = SCHEME_STR_VAL(v);

    if ((current_locale_name != name) && strcmp(current_locale_name, name)) {
#ifndef DONT_USE_LOCALE
      /* We only need CTYPE and COLLATE; two calls seem to be much
	 faster than one call with ALL */
      if (!setlocale(LC_CTYPE, name))
	setlocale(LC_CTYPE, "C");
      if (!setlocale(LC_COLLATE, name))
	setlocale(LC_COLLATE, "C");
#endif
      current_locale_name = name;
    }
  }
}

/**********************************************************************/
/*                               unicode                              */
/**********************************************************************/

#if 0
int scheme_utf8_decode(unsigned char *sc, int *us, int start, int end, int permissive)
{
  int i, j;

  if (end < 0)
    end = strlen(sc);

  for (i = start, j = 0; i < end; i++, j++) {
    sc = s[i];
    if (sc < 0x80) {
      us[j] = sc;
    } else {
      if ((sc & 0xE0) == 0xC0) {
	if ((i + 1 < end) && ((s[i + 1] & 0xC0) == 0x80)) {
	  us[j++] = ((sc & 0x1F) << 5) | (s[i+1] & 0x3F);
	  i++;
	  continue;
	}
      } else if ((sc & 0xF0) == 0xE0) {
	if ((i + 2 < end) 
	    && ((s[i + 1] & 0xC0) == 0x80)
	    && ((s[i + 2] & 0xC0) == 0x80)) {
	  us[j] = ((sc & 0xF) << 12) | ((s[i+1] & 0x3F) << 6) | (s[i+2] & 0x3F);
	  i += 2;
	  continue;
	} else
	  break;
      }
      raise_conversion_error("string->unicode-string/utf8", argv[0], start, end);
      return NULL;
    }
  }
}
#endif

/**********************************************************************/
/*                     machine type details                           */
/**********************************************************************/

/**************************** MacOS ***********************************/

#if defined(MACINTOSH_EVENTS) && !defined(OS_X)
# include <Gestalt.h>
extern long scheme_this_ip(void);
static void machine_details(char *s)
{
   OSErr err;
   long lng;
   char sysvers[30];
   char machine_name[256];

   err = Gestalt(gestaltSystemVersion, &lng);
   if (err != noErr) {
     strcpy(sysvers, "<unknown system>");
   } else {
     int i;
     sprintf(sysvers, "%X.%X",
	     (lng >> 8) & 0xff,
	     lng & 0xff);
     /* remove trailing zeros, put dot before something else */
     i = strlen(sysvers);
     if (i > 1) {
       if (sysvers[i-1] != '.') {
	 if (sysvers[i-1] == '0') {
	   sysvers[i-1] = 0;
	   i--;
	 } else {
	   sysvers[i] = sysvers[i-1];
	   sysvers[i-1] = '.';
	   i++;
	   sysvers[i] = 0;
	 }
       }
     }
   }

   err = Gestalt(gestaltMachineType, &lng);
   if (err != noErr) {
     strcpy(machine_name, "<unknown machine>");
   } else {
   	 Str255 machine_name_pascal;
   	 
   	 GetIndString(machine_name_pascal, kMachineNameStrID, lng);
	 CopyPascalStringToC(machine_name_pascal, machine_name);
   }

   lng = scheme_this_ip();

   sprintf(s, "%s %s %d.%d.%d.%d", sysvers, machine_name,
	   ((unsigned char *)&lng)[0],
	   ((unsigned char *)&lng)[1],
	   ((unsigned char *)&lng)[2],
	   ((unsigned char *)&lng)[3]);
}
#endif

/*************************** Windows **********************************/

#ifdef DOS_FILE_SYSTEM
# include <windows.h>
void machine_details(char *buff)
{
  OSVERSIONINFO info;
  BOOL hasInfo;
  char *p;

  info.dwOSVersionInfoSize = sizeof(info);
  
  GetVersionEx(&info);

  hasInfo = FALSE;

  p = info.szCSDVersion;

  while (p < info.szCSDVersion + sizeof(info.szCSDVersion) &&
	 *p) {
    if (*p != ' ') {
      hasInfo = TRUE;
      break;
    }
    p++;
  }

  sprintf(buff,"Windows %s %ld.%ld (Build %ld)%s%s",
	  (info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) ?
	  "9x" :
	  (info.dwPlatformId == VER_PLATFORM_WIN32_NT) ?
	  "NT" : "Unknown platform",
	  info.dwMajorVersion,info.dwMinorVersion,
	  (info.dwPlatformId == VER_PLATFORM_WIN32_NT) ?
	  info.dwBuildNumber :
	  info.dwBuildNumber & 0xFFFF,
	  hasInfo ? " " : "",hasInfo ? info.szCSDVersion : "");
}
#endif

/***************************** OSKit **********************************/

#ifdef USE_OSKIT_CONSOLE
void machine_details(char *buff)
{
  strcpy(buff, "OSKit");
}
#endif

/***************************** Unix ***********************************/

#if (!defined(MACINTOSH_EVENTS) || defined(OS_X)) && !defined(DOS_FILE_SYSTEM) && !defined(USE_OSKIT_CONSOLE)
static char *uname_locations[] = { "/bin/uname",
				   "/usr/bin/uname",
				   /* The above should cover everything, but
				      just in case... */
				   "/sbin/uname",
				   "/usr/sbin/uname",
				   "/usr/local/uname",
				   NULL };

static int try_subproc(Scheme_Object *subprocess_proc, char *prog)
{
  Scheme_Object *a[5];

  if (!scheme_setjmp(scheme_error_buf)) {
    a[0] = scheme_false;
    a[1] = scheme_false;
    a[2] = scheme_false;
    a[3] = scheme_make_string(prog);
    a[4] = scheme_make_string("-a");
    _scheme_apply_multi(subprocess_proc, 5, a);
    return 1;
  } else {
    scheme_clear_escape();
    return 0;
  }
}

void machine_details(char *buff)
{
  Scheme_Object *subprocess_proc;
  int i;
  mz_jmp_buf savebuf;

  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

  subprocess_proc = scheme_builtin_value("subprocess");

  for (i = 0; uname_locations[i]; i++) {
    if (scheme_file_exists(uname_locations[i])) {
      /* Try running it. */
      if (try_subproc(subprocess_proc, uname_locations[i])) {
	Scheme_Object *sout, *sin, *serr;
	long c;

	sout = scheme_current_thread->ku.multiple.array[1];
	sin = scheme_current_thread->ku.multiple.array[2];
	serr = scheme_current_thread->ku.multiple.array[3];

	scheme_close_output_port(sin);
	scheme_close_input_port(serr);

	memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));

	/* Read result: */
	strcpy(buff, "<unknown machine>");
	c = scheme_get_chars(sout, 1023, buff, 0);
	buff[c] = 0;
	
	scheme_close_input_port(sout);

	/* Remove trailing whitespace (especially newlines) */
	while (c && isspace(((unsigned char *)buff)[c - 1])) {
	  buff[--c] = 0;
	}

	return;
      }
    }
  }

  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));

  strcpy(buff, "<unknown machine>");
}
#endif

