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
# ifndef USE_ICONV_DLL
#  include <iconv.h>
#  include <langinfo.h>
# endif
# include <wchar.h>
# include <wctype.h>
# include <errno.h>
# ifdef MACOS_UNICODE_SUPPORT
#  include <CoreFoundation/CFString.h>
# endif
# ifdef WINDOWS_UNICODE_SUPPORT
#  include <windows.h>
# endif
#endif

#ifndef SCHEME_PLATFORM_LIBRARY_SUBPATH
# include "schsys.h"
#endif

#ifdef USE_ICONV_DLL
typedef long iconv_t;
static char *(*nl_langinfo)(int which);
static size_t (*iconv)(iconv_t cd,
		       char **inbuf, size_t *inbytesleft,
		       char **outbuf, size_t *outbytesleft);
static iconv_t (*iconv_open)(const char *tocode, const char *fromcode);
static void (*iconv_close)(iconv_t cd);
# define CODESET 0
#endif

typedef struct Scheme_Converter {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short closed;
  iconv_t cd;
  Scheme_Custodian_Reference *mref;
} Scheme_Converter;

/* globals */
int scheme_locale_on;
static const char *current_locale_name = "C";

static const char * const STRING_IS_NOT_UTF_8 = "string is not a well-formed UTF-8 encoding: ";

/* locals */
static Scheme_Object *make_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_eq (int argc, Scheme_Object *argv[]);
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
static Scheme_Object *string_locale_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_any_to_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_unicode_index (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_unicode_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_unicode_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy_bang (int argc, Scheme_Object *argv[]);
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
static Scheme_Object *string_open_converter(int argc, Scheme_Object *argv[]);
static Scheme_Object *string_close_converter(int argc, Scheme_Object *argv[]);
static Scheme_Object *string_convert(int argc, Scheme_Object *argv[]);
static Scheme_Object *string_convert_end(int argc, Scheme_Object *argv[]);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static int mz_strcmp(const char *who, unsigned char *str1, int l1, unsigned char *str2, int l2, int eq);
static int mz_strcmp_ci(const char *who, unsigned char *str1, int l1, unsigned char *str2, int l2, int eq);

#define portable_isspace(x) (((x) < 128) && isspace(x))

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
  REGISTER_SO(current_locale_name);

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
  scheme_add_global_constant("string-locale=?", 
			     scheme_make_prim_w_arity(string_locale_eq,
						      "string-locale=?",
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
  scheme_add_global_constant("string-copy!", 
			     scheme_make_prim_w_arity(string_copy_bang,
						      "string-copy!",
						      4, 5),
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

  scheme_add_global_constant("string->unicode-vector", 
			     scheme_make_prim_w_arity(string_to_vector,
						      "string->unicode-vector",
						      1, 3),
			     env);
  scheme_add_global_constant("string-any->unicode-vector", 
			     scheme_make_prim_w_arity(string_any_to_vector,
						      "string-any->unicode-vector",
						      2, 4),
			     env);
  scheme_add_global_constant("string-unicode-index", 
			     scheme_make_prim_w_arity(string_unicode_index,
						      "string-unicode-index",
						      2, 3),
			     env);
  scheme_add_global_constant("unicode-vector->string", 
			     scheme_make_prim_w_arity(vector_to_string,
						      "unicode-vector->string",
						      1, 1),
			     env);
  scheme_add_global_constant("string-unicode-length",
			     scheme_make_prim_w_arity(string_unicode_length,
						      "string-unicode-length",
						      1, 1),
			     env);
  scheme_add_global_constant("string-unicode-ref",
			     scheme_make_prim_w_arity(string_unicode_ref,
						      "string-unicode-ref",
						      2, 3),
			     env);
  scheme_add_global_constant("string-locale-upcase", 
			     scheme_make_prim_w_arity(string_locale_upcase,
						      "string-locale-upcase",
						      1, 1),
			     env);
  scheme_add_global_constant("string-locale-downcase", 
			     scheme_make_prim_w_arity(string_locale_downcase,
						      "string-locale-downcase",
						      1, 1),
			     env);

  scheme_add_global_constant("current-locale", 
			     scheme_register_parameter(current_locale, 
						       "current-locale",
						       MZCONFIG_LOCALE), 
			     env);


  scheme_add_global_constant("string-convert",
			     scheme_make_prim_w_arity2(string_convert,
						       "string-convert",
						       1, 7,
						       3, 3),
			     env);
  scheme_add_global_constant("string-convert-end",
			     scheme_make_prim_w_arity2(string_convert_end,
						       "string-convert-end",
						       0, 3,
						       2, 2),
			     env);
  scheme_add_global_constant("string-open-converter", 
			     scheme_make_prim_w_arity(string_open_converter,
						      "string-open-converter",
						      2, 2),
			     env);
  scheme_add_global_constant("string-close-converter", 
			     scheme_make_prim_w_arity(string_close_converter,
						      "string-close-converter",
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

  scheme_reset_locale();

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
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
    len = strlen(chars XFORM_OK_PLUS d);
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
     if (!falz) if (!(comp(scheme_name, \
                           (unsigned char *)prev, pl, \
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

GEN_STRING_COMP(string_locale_eq, "string-locale=?", mz_strcmp, ==, 1)
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
    chars = chars XFORM_OK_PLUS len;
  }
  
  return naya;
}

static Scheme_Object *append_all_strings_backwards(Scheme_Object *l)
{
  int i, len;
  Scheme_Object **a;

  len = scheme_list_length(l);
  a = MALLOC_N(Scheme_Object *, len);
  for (i = len; i--; l = SCHEME_CDR(l)) {
    a[i] = SCHEME_CAR(l);
  }
  
  return string_append(len, a);
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
do_string_to_vector (const char *who, int offset_start, Scheme_Object *perm, int argc, Scheme_Object *argv[])
{
  int i, ulen;
  long istart, ifinish;
  char *chars;
  unsigned int *v;
  Scheme_Object *vec, *val;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(who, "string", 0, argc, argv);
  
  scheme_get_substring_indices(who, argv[0], argc, argv, 
			       offset_start, offset_start + 1,
			       &istart, &ifinish);

  chars = SCHEME_STR_VAL(argv[0]);

  ulen = scheme_utf8_decode((unsigned char *)chars, istart, ifinish, 
			    NULL, 0, -1,
			    NULL, 0, perm ? 0xFFFF : 0);
  if (ulen < 0) {
    scheme_arg_mismatch(who,
			STRING_IS_NOT_UTF_8,
			argv[0]);
  }

  v = (unsigned int *)scheme_malloc_atomic(ulen * sizeof(unsigned int));
  scheme_utf8_decode((unsigned char *)chars, istart, ifinish, 
		     v, 0, -1,
		     NULL, 0, perm ? 0xFFFF : 0);

  vec = scheme_make_vector(ulen, NULL);
  for (i = 0; i < ulen; i++) {
    if (v[i] == 0xFFFF)
      val = perm;
    else
      val = scheme_make_integer_value_from_unsigned(v[i]);
    SCHEME_VEC_ELS(vec)[i] = val;
  }

  return vec;
}

static Scheme_Object *
string_to_vector (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("string->unicode-vector", 1, NULL, argc, argv);
}

static Scheme_Object *
string_any_to_vector (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("string-any->unicode-vector", 2, argv[1], argc, argv);
}

static Scheme_Object *
vector_to_string(int argc, Scheme_Object *argv[])
{
  if (SCHEME_VECTORP(argv[0])) {
    int len, i;
    Scheme_Object *vec;
    unsigned int *v;
    unsigned long val;

    vec = argv[0];
    len = SCHEME_VEC_SIZE(vec);

    v = (unsigned int *)scheme_malloc_atomic(len * sizeof(unsigned int));

    for (i = 0; i < len; i++) {
      if (scheme_get_unsigned_int_val(SCHEME_VEC_ELS(vec)[i], &val)) {
	if ((val >= 0xD800 && (val <= 0xDFFF))
	    || (val == 0xFFFE)
	    || (val == 0xFFFF)) {
	  /* UTF-16 surrogates or other illegal code units */
	  break;
	} else
	  v[i] = val;
      } else
	break;
    }
    
    if (i >= len) {
      char *s;
      int slen;

      slen = scheme_utf8_encode_all(v, len, NULL);
      s = (char *)scheme_malloc_atomic(slen + 1);
      scheme_utf8_encode_all(v, len, (unsigned char *)s);
      s[slen] = 0;

      return scheme_make_sized_string(s, slen, 0);
    }
  }
   
  scheme_wrong_type("unicode-vector->string", "vector of unicode exact integers", 0, argc, argv);

  return NULL;
}

static Scheme_Object *
string_unicode_length (int argc, Scheme_Object *argv[])
{
  int len;
  long istart, ifinish;
  char *chars;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-unicode-length", "string", 0, argc, argv);
  
  chars = SCHEME_STR_VAL(argv[0]);

  scheme_get_substring_indices("string-unicode-length", argv[0], argc, argv, 
			       1, 2,
			       &istart, &ifinish);
  
  len = scheme_utf8_decode((unsigned char *)chars, istart, ifinish,
			   NULL, 0, -1,
			   NULL, 0, 0);

  if (len < 0)
    return scheme_false;
  else
    return scheme_make_integer(len);
}

static Scheme_Object *
string_unicode_index(int argc, Scheme_Object *argv[])
{
  long istart, ifinish, pos = -1, opos, ipos;
  char *chars;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-unicode-index", "string", 0, argc, argv);
  
  chars = SCHEME_STR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1])) {
    pos = SCHEME_INT_VAL(argv[1]);
  } else if (SCHEME_BIGNUMP(argv[1])) {
    if (SCHEME_BIGPOS(argv[1]))
      pos = 0x7FFFFFFF;
  }

  if (pos < 0) {
    scheme_wrong_type("string-unicode-index", "non-negative exact integer", 1, argc, argv);
  }

  scheme_get_substring_indices("string-unicode-index", argv[0], argc, argv, 
			       2, 3,
			       &istart, &ifinish);
  
  opos = scheme_utf8_decode((unsigned char *)chars, istart, ifinish,
			   NULL, 0, pos,
			   &ipos, 0, 0);

  if ((opos < 0) || (ipos == ifinish))
    return scheme_false;
  else
    return scheme_make_integer(ipos);
}

static Scheme_Object *
string_unicode_ref(int argc, Scheme_Object *argv[])
{
  long istart, ifinish, pos = -1, opos, ipos;
  char *chars;
  unsigned int us[1];

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-unicode-ref", "string", 0, argc, argv);
  
  chars = SCHEME_STR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1])) {
    pos = SCHEME_INT_VAL(argv[1]);
  } else if (SCHEME_BIGNUMP(argv[1])) {
    if (SCHEME_BIGPOS(argv[1]))
      pos = 0x7FFFFFFF;
  }

  if (pos < 0) {
    scheme_wrong_type("string-unicode-ref", "non-negative exact integer", 1, argc, argv);
  }

  scheme_get_substring_indices("string-unicode-ref", argv[0], argc, argv, 
			       2, 3,
			       &istart, &ifinish);
  
  if (pos > 0) {
    opos = scheme_utf8_decode((unsigned char *)chars, istart, ifinish,
			      NULL, 0, pos,
			      &ipos, 0, 0);
    if (opos < pos)
      return scheme_false;
    istart = ipos;
  }

  opos = scheme_utf8_decode((unsigned char *)chars, istart, ifinish,
			    us, 0, 1,
			    &ipos, 0, 0);

  if (opos < 1)
    return scheme_false;
  else
    return scheme_make_integer_value_from_unsigned(us[0]);
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
string_copy_bang(int argc, Scheme_Object *argv[])
{
  long istart, ifinish;
  long ostart, ofinish;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-copy!", "string", 0, argc, argv);

  scheme_get_substring_indices("string-copy!", argv[0], 
			       argc, argv, 1, 2, 
			       &istart, &ifinish);

  if (!SCHEME_MUTABLE_STRINGP(argv[3]))
    scheme_wrong_type("string-copy!", "mutable-string", 3, argc, argv);

  scheme_get_substring_indices("string-copy!", argv[3], 
			       argc, argv, 4, 5, 
			       &ostart, &ofinish);

  if ((ofinish - ostart) < (ifinish - istart)) {
    scheme_arg_mismatch("string-copy!",
			"not enough room in output string: ",
			argv[3]);
    return NULL;
  }

  memmove(SCHEME_STR_VAL(argv[3]) + ostart,
	  SCHEME_STR_VAL(argv[0]) + istart,
	  ifinish - istart);
  
  return scheme_void;
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

#ifndef DONT_USE_LOCALE

static char *do_convert(iconv_t cd,
			const char *from_e, const char *to_e, 
			/* in can be NULL to output just a shift; in that case,
			   id should be 0, too */
			char *in, int id, int iilen, 
			char *out, int od, int iolen, 
			/* if grow, then reallocate when out isn't big enough */
			int grow, 
			/* extra specifies the length of a terminator, 
			   not included in iolen or *oolen */
			int extra,
			/* these two report actual read/wrote sizes: */
			long *oilen, long *oolen,
			/* status is set to 
			   0 for complete, 
			   -1 for error (possibly partial input),
			   1 for more avail */
			int *status)
{
  int dip, dop, close_it = 0;
  size_t il, ol, r;
  GC_CAN_IGNORE char *ip, *op;

  /* Defaults: */
  *status = -1;
  if (oilen)
    *oilen = 0;
  *oolen = 0;

  if (cd == (iconv_t)-1) {
    if (iconv_open) {
      if (!from_e)
	from_e = nl_langinfo(CODESET);
      if (!to_e)
	to_e = nl_langinfo(CODESET);
      cd = iconv_open(to_e, from_e);
      close_it = 1;
    }
  }

  if (cd == (iconv_t)-1) {
    if (out) {
      while (extra--) {
	out[extra] = 0;
      }
    }
    return out;
  }

  /* The converter is ready. Allocate out space, if necessary */

  if (!out) {
    iolen = iilen;
    out = (char *)scheme_malloc_atomic(iolen + extra);
    od = 0;
  }
  
  /* il and ol are the number of available chars */
  il = iilen;
  ol = iolen;
  /* dip and dop are the number of characters read so far;
     we use these and NULL out the ip and op pointers
     for the sake of precise GC */
  dip = 0;
  dop = 0;

  while (1) {
    ip = in XFORM_OK_PLUS id + dip;
    op = out XFORM_OK_PLUS od + dop;
    r = iconv(cd, &ip, &il, &op, &ol);
    dip = ip - (in XFORM_OK_PLUS id);
    dop = op - (out XFORM_OK_PLUS od);
    ip = op = NULL;

    /* Record how many chars processed, now */
    if (oilen)
      *oilen = dip;
    *oolen = dop;

    /* Got all the chars? */
    if (r == (size_t)-1) {
      if (errno == E2BIG) {
	if (grow) {
	  /* Double the string size and try again */
	  char *naya;
	  naya = (char *)scheme_malloc_atomic((iolen * 2) + extra);
	  memcpy(naya, out + od, *oolen);
	  ol += iolen;
	  iolen += iolen;
	  out = naya;
	  od = 0;
	} else {
	  *status = 1;
	  if (close_it)
	    iconv_close(cd);
	  while (extra--) {
	    out[od + dop + extra] = 0;
	  }
	  return out;
	}
      } else {
	/* Assume EINVAL */
	if (close_it)
	  iconv_close(cd);
	while (extra--) {
	  out[od + dop + extra] = 0;
	}
	return out;
      }
    } else {
      /* All done. */
      *status = 0;
      if (close_it)
	iconv_close(cd);
      while (extra--) {
	out[od + dop + extra] = 0;
      }
      return out;
    }
  }
}


static char *locale_recase(int to_up,
			   /* in must be null-terminated, iilen doesn't include it */ 
			   char *in, int id, int iilen,
			   /* iolen, in contrast, includes the terminator */
			   char *out, int od, int iolen,
			   long *oolen)
{
  /* To change the case, convert the string to multibyte, re-case the
     multibyte, then convert back. */
# define MZ_WC_BUF_SIZE 32
  GC_CAN_IGNORE mbstate_t state;
  size_t wl, wl2, ml, ml2;
  wchar_t *wc, *ws, wcbuf[MZ_WC_BUF_SIZE], cwc;
  const char *s;
  int j;
  /* The "n" versions are apparently not too standard: */
# define mz_mbsnrtowcs(t, f, fl, tl, s) MSC_IZE(mbsrtowcs)(t, f, tl, s) 
# define mz_wcsnrtombs(t, f, fl, tl, s) MSC_IZE(wcsrtombs)(t, f, tl, s) 
  
  /* ----- to wide char ---- */

  /* Get length */
  memset(&state, 0, sizeof(mbstate_t));
  s = in XFORM_OK_PLUS id;
  wl = mz_mbsnrtowcs(NULL, &s, iilen, 0, &state);
  s = NULL;
  if (wl < 0) return NULL;

  /* Allocate space */
  if (wl < MZ_WC_BUF_SIZE) {
    wc = wcbuf;
  } else {
    wc = (wchar_t *)scheme_malloc_atomic(sizeof(wchar_t) * (wl + 1));
  }

  /* Convert */
  memset(&state, 0, sizeof(mbstate_t));
  s = in XFORM_OK_PLUS id;
  wl2 = mz_mbsnrtowcs(wc, &s, iilen, wl + 1, &state);
  s = NULL;
  if (wl2 < 0) return NULL; /* Very strange! */

  wc[wl] = 0; /* just in case */

  /* ---- re-case ---- */

  if (to_up) {
    for (j = 0; j < wl; j++) {
      cwc = towupper(wc[j]);
      wc[j] = cwc;
    }
  } else {
    for (j = 0; j < wl; j++) {
      cwc = towlower(wc[j]);
      wc[j] = cwc;
    }
  }

  /* ---- back to multibyte ---- */

  /* Measure */
  memset(&state, 0, sizeof(mbstate_t));
  ws = wc;
  ml = mz_wcsnrtombs(NULL, (const wchar_t **)&ws, wl, 0, &state);
  ws = NULL;
  if (ml < 0) return NULL;

  /* Allocate space */
  *oolen = ml;
  if (ml + 1 >= iolen) {
    out = (char *)scheme_malloc_atomic(ml + 1);
    od = 0;
  }

  /* Convert */
  memset(&state, 0, sizeof(mbstate_t));
  ws = wc;
  ml2 = mz_wcsnrtombs(out + od, (const wchar_t **)&ws, wl, ml + 1, &state);
  ws = NULL;
  if (ml2 < 0) return NULL; /* Very strange! */

  out[od + ml] = 0;

  return out;
}

int mz_locale_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UCS-4. */
{
# define MZ_SC_BUF_SIZE 32
# ifdef SCHEME_BIG_ENDIAN
#  define MZ_UCS4_NAME "UCS-4BE"
# else
#  define MZ_UCS4_NAME "UCS-4LE"
# endif
  long clen1, clen2, used1, used2, origl1, origl2;
  char *c1, *c2, buf1[MZ_SC_BUF_SIZE], buf2[MZ_SC_BUF_SIZE];
  char case_buf1[MZ_SC_BUF_SIZE], case_buf2[MZ_SC_BUF_SIZE];
  int status, got_more;

  /* First, convert UCS-4 to locale-specific encoding. If some
     characters don't fit into the encoding, then we'll have leftover
     characters. Count unconvertable charc as greater than anything
     that can be converted */

  origl1 = l1;
  origl2 = l2;
  
  /* Loop to check both convertable and unconvertable parts */
  while (1) {
    if (!origl1 && !origl2)
      return 0;
    if (!origl1)
      return -1;
    if (!origl2)
      return 1;

    /* Loop to get consistent parts of the wto strings, in case
       a conversion fails. */
    got_more = 0;
    l1 = origl1;
    l2 = origl2;
    while (1) {
      c1 = do_convert((iconv_t)-1, MZ_UCS4_NAME, NULL, 
		      s1, d1 * 4, 4 * l1,
		      buf1, 0, MZ_SC_BUF_SIZE - 1,
		      1 /* grow */, 1 /* terminator size */,
		      &used1, &clen1,
		      &status);
      c2 = do_convert((iconv_t)-1, MZ_UCS4_NAME, NULL, 
		      s2, d2 * 4, 4 * l2,
		      buf2, 0, MZ_SC_BUF_SIZE - 1,
		      1 /* grow */, 1 /* terminator size */,
		      &used2, &clen2,
		      &status);

      if ((used1 < 4 * l1) || (used2 < 4 * l2)) {
	if (got_more) {
	  /* Something went wrong. We've already tried to
	     even out the parts that work. Let's give up
	     on the first characters */
	  clen1 = clen2 = 0;
	  break;
	} else if (used1 == used2) {
	  /* Not everything, but both ended at the same point */
	  break;
	} else {
	  /* Pick the smallest */
	  if (used2 < used1) {
	    used1 = used2;
	    got_more = 1;
	  } else
	    got_more = 2;
	  l2 = (used1 >> 2);
	  l1 = (used1 >> 2);

	  if (!l1) {
	    /* Nothing to get this time. */
	    clen1 = clen2 = 0;
	    c1 = c2 = "";
	    used1 = used2 = 0;
	    break;
	  }
	}
      } else
	/* Got all that we wanted */
	break;
    }
    
    if (cvt_case) {
      if (clen1)
	c1 = locale_recase(0, c1, 0, clen1,
			   case_buf1, 0, MZ_SC_BUF_SIZE - 1,
			   &clen1);
      else
	c1 = NULL;
      if (clen2)
	c2 = locale_recase(0, c2, 0, clen2,
			   case_buf2, 0, MZ_SC_BUF_SIZE - 1,
			   &clen2);
      else
	c2 = NULL;
      /* There shouldn't have been conversion errors, but just in
	 case, care of NULL. */
      if (!c1) c1 = "";
      if (!c2) c2 = "";
    }

    /* Collate, finally. */
    status = strcoll(c1, c2);

    /* If one is bigger than the other, we're done. */
    if (status)
      return status;

    /* Otherwise, is there more to check? */
    origl1 -= (used1 >> 2);
    origl2 -= (used2 >> 2);
    d1 += (used1 >> 2);
    d2 += (used2 >> 2);
    if (!origl1 && !origl2)
      return 0;

    /* There's more. It must be that the next character wasn't
       convertable in one of the encodings. */
    if (got_more)
      return ((got_more == 2) ? 1 : -1);

    if (!origl1)
      return -1;

    /* Compare an unconverable character directly. No case conversions
       if it's outside the locale. */
    if (((unsigned int *)s1)[d1] > ((unsigned int *)s2)[d2])
      return 1;
    else if (((unsigned int *)s1)[d1] < ((unsigned int *)s2)[d2])
      return -1;
    else {
      /* We've skipped one unconvertable char, and they still look the
	 same.  Now try again. */
      origl1 -= 1;
      origl2 -= 1;
      d1 += 1;
      d2 += 1;
    }
  }
}

#ifdef MACOS_UNICODE_SUPPORT
int mz_native_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UTF-16. */
{
  CFStringRef str1, str2;
  CFComparisonResult r;

  str1 = CFStringCreateWithBytes(NULL, s1 XFORM_OK_PLUS (d1 * 2), (l1 * 2), kCFStringEncodingUnicode, FALSE);
  str2 = CFStringCreateWithBytes(NULL, s2 XFORM_OK_PLUS (d2 * 2), (l2 * 2), kCFStringEncodingUnicode, FALSE);
  
  r = CFStringCompare(str1, str2, (kCFCompareLocalized 
				   | (cvt_case ? kCFCompareCaseInsensitive : 0)));
  
  CFRelease(str1);
  CFRelease(str2);

  return (int)r;
}
#endif

#ifdef WINDOWS_UNICODE_SUPPORT
int mz_native_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UTF-16. */
{
  int r;

  r = CompareString(LOCALE_USER_DEFAULT,
		    ((cvt_case ? NORM_IGNORECASE : 0)
		     | NORM_IGNOREKANATYPE
		     | NORM_IGNOREWIDTH),
		    d1, l1, d2, l2);

  return r - 2;
}
#endif

typedef int (*strcoll_proc)(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case);

int do_locale_comp(const char *who, unsigned char *str1, int l1, unsigned char *str2, int l2, int cvt_case)
{
  int ul1, ul2, xl1;
  unsigned int *us1, *us2;
  int v, endres, csize = 4, utf16 = 0;
  GC_CAN_IGNORE strcoll_proc mz_strcoll = mz_locale_strcoll;
  
#if defined(MACOS_UNICODE_SUPPORT) || defined(WINDOWS_UNICODE_SUPPORT)
  if (current_locale_name && !*current_locale_name) {
    utf16 = 1;
    csize = 2;
    mz_strcoll = mz_native_strcoll;
  }
#endif

  ul1 = scheme_utf8_decode(str1, 0, l1, NULL, 0, -1, NULL, utf16, 0);
  if (ul1 < 0)
    scheme_arg_mismatch(who, STRING_IS_NOT_UTF_8, scheme_make_sized_string(str1, l1, 1));
  ul2 = scheme_utf8_decode(str2, 0, l2, NULL, 0, -1, NULL, utf16, 0);
  if (ul2 < 0)
    scheme_arg_mismatch(who, STRING_IS_NOT_UTF_8, scheme_make_sized_string(str2, l2, 1));
  
  us1 = (unsigned int *)scheme_malloc_atomic(csize * (ul1 + 1));
  us2 = (unsigned int *)scheme_malloc_atomic(csize * (ul2 + 1));
  
  ul1 = scheme_utf8_decode(str1, 0, l1, (unsigned int *)us1, 0, -1, NULL, utf16, 0);
  ul2 = scheme_utf8_decode(str2, 0, l2, (unsigned int *)us2, 0, -1, NULL, utf16, 0);

  if (utf16) {
    ((short *)us1)[ul1] = 0;
    ((short *)us2)[ul2] = 0;
  } else {
    us1[ul1] = 0;
    us2[ul2] = 0;
  }

  if (ul1 > ul2) {
    ul1 = ul2;
    endres = 1;
  } else {
    if (ul2 > ul1)
      endres = -1;
    else
      endres = 0;
  }

  /* Walk back through the strings looking for nul characters. If we
     find one, compare the part after the null character to update
     endres, then continue. Unfortunately, we do too much work if an
     earlier part of the string (tested later) determines the result,
     but hopefully nul characters are rare. */

  xl1 = 0;
  while (ul1--) {
    if ((utf16 && (!(((short *)us1)[ul1]) || !(((short *)us2)[ul1])))
	|| (!utf16 && (!(us1[ul1]) || !(us2[ul1])))) {
      if (utf16) {
	if (((short *)us1)[ul1])
	  endres = 1;
	else if (((short *)us2)[ul1])
	  endres = -1;
      } else {
	if (us1[ul1])
	  endres = 1;
	else if (us2[ul1])
	  endres = -1;
      }
     
      if (xl1)
	v = mz_strcoll((char *)us1, ul1 + 1, xl1, (char *)us2, ul1 + 1, xl1, cvt_case);
      else
	v = 0;

      if (v)
	endres = v;
      xl1 = 0;
    } else {
      xl1++;
    }
  }
  
  v = mz_strcoll((char *)us1, 0, xl1, (char *)us2, 0, xl1, cvt_case);
  if (v)
    endres = v;
  
  return endres;
}


char *do_locale_recase(int to_up, char *in, int delta, int len, long *olen)
{
  Scheme_Object *parts = scheme_null;
  char *c, buf[MZ_SC_BUF_SIZE], case_buf[MZ_SC_BUF_SIZE];
  long clen, used;
  int status;

  while (len) {
    /* We might have conversion errors... */
    c = do_convert((iconv_t)-1, MZ_UCS4_NAME, NULL, 
		   in, 4 * delta, 4 * len,
		   buf, 0, MZ_SC_BUF_SIZE - 1,
		   1 /* grow */, 1 /* terminator size */,
		   &used, &clen,
		   &status);

    used >>= 2;
    delta += used;
    len -= used;
    
    c = locale_recase(to_up, c, 0, clen,
		      case_buf, 0, MZ_SC_BUF_SIZE - 1,
		      &clen);
    if (!c)
      clen = 0;
    
    c = do_convert((iconv_t)-1, NULL, MZ_UCS4_NAME,
		   c, 0, clen,
		   NULL, 0, 0,
		   1 /* grow */, 0 /* terminator size */,
		   &used, &clen,
		   &status);

    if (!len && SCHEME_NULLP(parts)) {
      *olen = (clen >> 2);
      return c;
    }

    /* We can get here if there was some conversion error at some
       point. We're building up a list of parts. */

    parts = scheme_make_pair(scheme_make_sized_string(c, clen, 0),
			     parts);
    
    if (len) {
      /* Conversion error, so skip one char. */
      parts = scheme_make_pair(scheme_make_sized_offset_string(in, (delta * 4), 4, 1),
			       parts);
      delta += 1;
      len -= 1;
    }
  }
  
  parts = append_all_strings_backwards(parts);
  *olen = (SCHEME_STRTAG_VAL(parts) >> 2);

  return SCHEME_STR_VAL(parts);
}

#ifdef MACOS_UNICODE_SUPPORT
char *do_native_recase(int to_up, char *in, int delta, int len, long *olen)
     /* The in argument is actually UTF-16. */
{
  CFMutableStringRef mstr;
  CFStringRef str;
  char *result;

  str = CFStringCreateWithBytes(NULL, in XFORM_OK_PLUS (delta * 2), (len * 2), kCFStringEncodingUnicode, FALSE);
  mstr = CFStringCreateMutableCopy(NULL, 0, str);
  CFRelease(str);
  
  if (to_up)
    CFStringUppercase(mstr, NULL);
  else
    CFStringLowercase(mstr, NULL);

  len = CFStringGetLength(mstr);
  *olen = len;

  result = (char *)scheme_malloc_atomic(len * 2);
  
  CFStringGetCharacters(mstr, CFRangeMake(0, len), (UniChar *)result);
  CFRelease(mstr);
  
  return result;
}
#endif

#ifdef WINDOWS_UNICODE_SUPPORT
char *do_native_recase(int to_up, char *in, int delta, int len, long *olen)
     /* The in argument is actually UTF-16. */
{
  char *result;

  result = (char *)scheme_malloc_atomic(len * 2);
  memcpy(result, in + (2 * delta), len * 2);
  
  if (to_up)
    CharUpperBuffW(result, len);
  else
    CharLowerBuffW(result, len);

  *olen = len;
  return result;
}
#endif

typedef char *(*recase_proc)(int to_up, char *in, int delta, int len, long *olen);

Scheme_Object *mz_recase(const char *who, int to_up, unsigned char *str1, int l1)
{
  long ulen, ulen1, rl;
  int csize = 4, utf16 = 0, i, delta = 0;
  char *rs;
  unsigned int *us, *us1;
  recase_proc mz_do_recase = do_locale_recase;
  Scheme_Object *s, *parts = scheme_null;

#if defined(MACOS_UNICODE_SUPPORT) || defined(WINDOWS_UNICODE_SUPPORT)
  if (current_locale_name && !*current_locale_name) {
    utf16 = 1;
    csize = 2;
    mz_do_recase = do_native_recase;
  }
#endif

  ulen = scheme_utf8_decode(str1, 0, l1, NULL, 0, -1, NULL, utf16, 0);
  if (ulen < 0) {
    scheme_arg_mismatch(who, STRING_IS_NOT_UTF_8, scheme_make_sized_string(str1, l1, 1));
  }

  us = (unsigned int *)scheme_malloc_atomic(csize * (ulen + 1));  
  ulen = scheme_utf8_decode(str1, 0, l1, (unsigned int *)us, 0, -1, NULL, utf16, 0);
  us[ulen] = 0;

  /* If there are nulls in the string, then we have to make multiple
     calls to mz_do_recase */
  i = 0;
  while (1) {
    for (; i < ulen; i++) {
      if (utf16) {
	if (!((short *)us)[i])
	  break;
      } else if (!us[i])
	break;
    }

    us1 = (unsigned int *)mz_do_recase(to_up, (char *)us, delta, i - delta, &ulen1);

    rl = scheme_utf8_encode(us1, 0, ulen1, NULL, 0, utf16);
    rs = (char *)scheme_malloc_atomic(rl + 1);
    rl = scheme_utf8_encode(us1, 0, ulen1, rs, 0, utf16);
    rs[rl] = 0;
    
    s = scheme_make_sized_string(rs, rl, 0);

    if (SCHEME_NULLP(parts) && (i == ulen))
      return s;

    parts = scheme_make_pair(s, parts);
    
    if (i == ulen) 
      break;
    
    /* upcasing and encoding a nul char is easy: */
    s = scheme_make_sized_string("\0", 1, 0);
    parts = scheme_make_pair(s, parts);
    i++;
    delta = i;

    if (i == ulen) 
      break;
  }

  return append_all_strings_backwards(parts);
}

#endif

static int mz_strcmp(const char *who, unsigned char *str1, int l1, unsigned char *str2, int l2, int use_locale)
{
  int endres;
  
#ifndef DONT_USE_LOCALE
  if (use_locale && scheme_locale_on) {
    return do_locale_comp(who, str1, l1, str2, l2, 0);
  }
#endif

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

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

static int mz_strcmp_ci(const char *who, unsigned char *str1, int l1, unsigned char *str2, int l2, int use_locale)
{
  int endres;

#ifndef DONT_USE_LOCALE
  if (use_locale && scheme_locale_on) {
    return do_locale_comp(who, str1, l1, str2, l2, 1);
  }
#endif

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

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

/********************************************************************/
/*                            format                                */
/********************************************************************/

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
      if ((format[i] < 128) && portable_isspace(format[i])) {
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
      if (portable_isspace(format[i])) {
	/* skip spaces (at most one newline) */
	do {
	  if ((format[i] == '\n') || (format[i] == '\r')) {
	    /* got one */
	    if ((format[i] == '\r') && (format[i + 1] == '\n'))
	      i++; /* Windows-style CR-NL */
	    i++;
	    while (portable_isspace(format[i]) 
		   && !((format[i] == '\n') || (format[i] == '\r'))) {
	      i++;
	    }
	    break;
	  } else
	    i++;
	} while (portable_isspace(format[i]));
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


static void close_converter(Scheme_Object *o, void *data)
{
  Scheme_Converter *c = (Scheme_Converter *)o;

  if (!c->closed) {
    c->closed = 1;
    iconv_close(c->cd);
    c->cd = (iconv_t)-1;
    scheme_remove_managed(c->mref, (Scheme_Object *)c);
  }
}

static Scheme_Object *string_open_converter(int argc, Scheme_Object **argv)
{
  Scheme_Converter *c;
  char *from_e, *to_e;
  iconv_t cd;
  Scheme_Custodian_Reference *mref;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-open-converter", "string", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("string-open-converter", "string", 1, argc, argv);
  
  scheme_custodian_check_available(NULL, "string-open-converter", "converter");

  from_e = SCHEME_STR_VAL(argv[0]);
  to_e = SCHEME_STR_VAL(argv[1]);

  if (!iconv_open)
    return scheme_false;

  if (!*from_e)
    from_e = nl_langinfo(CODESET);
  if (!*to_e)
    to_e = nl_langinfo(CODESET);
  cd = iconv_open(to_e, from_e);

  if (cd == (iconv_t)-1)
    return scheme_false;

  c = MALLOC_ONE_TAGGED(Scheme_Converter);
  c->type = scheme_string_converter_type;
  c->closed = 0;
  c->cd = cd;
  mref = scheme_add_managed(NULL,
			    (Scheme_Object *)c,
			    close_converter,
			    NULL, 1);
  c->mref = mref;

  return (Scheme_Object *)c;
}

static Scheme_Object *convert_one(const char *who, int opos, int argc, Scheme_Object *argv[])
{
  char *r;
  int status;
  long amt_read, amt_wrote;
  long istart, ifinish, ostart, ofinish;
  Scheme_Object *a[3];
  Scheme_Converter *c;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type))
    scheme_wrong_type(who, "converter", 0, argc, argv);  

  if (opos > 1) {
    if (!SCHEME_STRINGP(argv[1]))
      scheme_wrong_type(who, "string", 1, argc, argv);  
    scheme_get_substring_indices("substring", argv[1], argc, argv, 2, 3, &istart, &ifinish);
  } else {
    istart = 0;
    ifinish = 4; /* This is really a guess about how much space we need */
  }
  
  if (argc > opos) {
    if (!SCHEME_MUTABLE_STRINGP(argv[opos]))
      scheme_wrong_type(who, "mutable string", opos, argc, argv);  
    r = SCHEME_STR_VAL(argv[opos]);
    scheme_get_substring_indices("substring", argv[opos], argc, argv, opos + 1, opos + 2, &ostart, &ofinish);
  } else {
    r = NULL;
    ostart = 0;
    ofinish = 0;
  }

  c = (Scheme_Converter *)argv[0];
  if (c->closed)
    scheme_arg_mismatch(who, "converter is closed: ", argv[0]);

  r = do_convert(c->cd, NULL, NULL,
		 ((opos > 1) ? SCHEME_STR_VAL(argv[1]) : NULL), istart, ifinish-istart,
		 r, ostart, ofinish-ostart,
		 !r, /* grow? */
		 (r ? 0 : 1), /* terminator */
		 &amt_read, &amt_wrote,
		 &status);
  
  if (argc <= opos) {
    a[0] = scheme_make_sized_string(r, amt_wrote, 0);
  } else {
    a[0] = scheme_make_integer(amt_wrote);
  }
  if (opos > 1) {
    a[1] = scheme_make_integer(amt_read);
    a[2] = ((status < 0) ? scheme_false : scheme_true);
    return scheme_values(3, a);
  } else {
    a[1] = ((status < 0) ? scheme_false : scheme_true);
    return scheme_values(2, a);
  }
}

static Scheme_Object *string_convert(int argc, Scheme_Object *argv[])
{
  return convert_one("string-convert", 4, argc, argv);
}

static Scheme_Object *string_convert_end(int argc, Scheme_Object *argv[])
{
  return convert_one("string-convert-end", 1, argc, argv);
}

static Scheme_Object *string_close_converter(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type))
    scheme_wrong_type("string-close-converter", "converter", 0, argc, argv);

  close_converter(argv[0], NULL);

  return scheme_void;
}


static Scheme_Object *
unicode_recase(const char *who, int to_up, int argc, Scheme_Object *argv[])
{
  long len;
  char *chars;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(who, "string", 0, argc, argv);
  
  chars = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);
  
  return mz_recase(who, to_up, chars, len);
}

static Scheme_Object *
string_locale_upcase(int argc, Scheme_Object *argv[])
{
  return unicode_recase("string-locale-upcase", 1, argc, argv);
}

static Scheme_Object *
string_locale_downcase(int argc, Scheme_Object *argv[])
{
  return unicode_recase("string-locale-downcase", 0, argc, argv);
}

void scheme_reset_locale(void)
{
  Scheme_Object *v;
  const char *name;

  v = scheme_get_param(scheme_config, MZCONFIG_LOCALE);
  scheme_locale_on = SCHEME_TRUEP(v);

  if (scheme_locale_on) {
    name = SCHEME_STR_VAL(v);
#ifndef DONT_USE_LOCALE
    /* This is probably ok to keep, but */
    if ((current_locale_name != name) && strcmp(current_locale_name, name)) {
      /* We only need CTYPE and COLLATE; two calls seem to be much
	 faster than one call with ALL */
      if (!setlocale(LC_CTYPE, name))
	setlocale(LC_CTYPE, "C");
      if (!setlocale(LC_COLLATE, name))
	setlocale(LC_COLLATE, "C");
    }
#endif
    current_locale_name = name;
  }
}

/**********************************************************************/
/*                               unicode                              */
/**********************************************************************/

int scheme_utf8_decode(const unsigned char *s, int start, int end, 
		       unsigned int *us, int dstart, int dend,
		       long *ipos, char utf16, int permissive)
{
  int i, j;
  unsigned int sc;

  /* In non-permissive mode, a negative result means ill-formed input.
     Permissive mode accepts anything and tries to convert it.  In
     that case, the strategy for illegal sequences is to convert
     anything bad with two bits set to the given "permissive"
     value. That way, computing the length by ignoring
     middle-of-sequence characters produces the same length as this
     permissive conversion (so position counting in ports matches a
     permissive conversion). */

  if (end < 0)
    end = strlen(s);
  if (dend < 0)
    dend = 0x7FFFFFFF;

  if (!us && permissive) {
    /* Fast path */
    for (i = start, j = dstart; ((i < end) && (j < dend)); i++) {
      if (!((s[i] & 0xC0) == 0x80))
	j++;
    }
    return j - dstart;
  }

  for (i = start, j = dstart; (i < end) && (j < dend); i++, j++) {
    sc = s[i];
    if (sc < 0x80) {
      if (us) {
	if (utf16)
	  ((unsigned short *)us)[j] = sc;
	else
	  us[j] = sc;
      }
    } else {
      unsigned int v, checkmin;
      if ((sc & 0xE0) == 0xC0) {
	if ((i + 1 < end) 
	    && ((s[i + 1] & 0xC0) == 0x80)) {
	  v = (((sc & 0x1F) << 6) 
	       | (s[i+1] & 0x3F));
	  checkmin = 0x80;
	  i++;
	} else if (permissive) {
	  v = permissive;
	  checkmin = 0;
	} else
	  break;
      } else if ((sc & 0xF0) == 0xE0) {
	if ((i + 2 < end) 
	    && ((s[i + 1] & 0xC0) == 0x80)
	    && ((s[i + 2] & 0xC0) == 0x80)) {
	  v = (((sc & 0xF) << 12) 
	       | ((s[i+1] & 0x3F) << 6) 
	       | (s[i+2] & 0x3F));
	  checkmin = 0x800;
	  if (((v >= 0xD800) && (v <= 0xDFFF))
	      || (v == 0xFFFE)
	      || (v == 0xFFFF)) {
	    /* UTF-16 surrogates or other illegal code units */
	    if (permissive) {
	      v = permissive;
	      checkmin = 0;
	    } else
	      break;
	  } else
	    i += 2;
	} else if (permissive) {
	  v = permissive;
	  checkmin = 0;
	} else
	  break;
      } else if ((sc & 0xF8) == 0xF0) {
	if ((i + 3 < end) 
	    && ((s[i + 1] & 0xC0) == 0x80)
	    && ((s[i + 2] & 0xC0) == 0x80)
	    && ((s[i + 3] & 0xC0) == 0x80)) {
	  v = (((sc & 0x7) << 18) 
	       | ((s[i+1] & 0x3F) << 12) 
	       | ((s[i+2] & 0x3F) << 6) 
	       | (s[i+3] & 0x3F));
	  checkmin = 0x10000;
	  i += 3;
	} else if (permissive) {
	  v = permissive;
	  checkmin = 0;
	} else
	  break;
      } else if ((sc & 0xFC) == 0xF8) {
	if ((i + 4 < end) 
	    && ((s[i + 1] & 0xC0) == 0x80)
	    && ((s[i + 2] & 0xC0) == 0x80)
	    && ((s[i + 3] & 0xC0) == 0x80)
	    && ((s[i + 4] & 0xC0) == 0x80)) {
	  v = (((sc & 0x7) << 24) 
	       | ((s[i+1] & 0x3F) << 18) 
	       | ((s[i+2] & 0x3F) << 12) 
	       | ((s[i+3] & 0x3F) << 6) 
	       | (s[i+4] & 0x3F));
	  checkmin = 0x200000;
	  i += 4;
	} else if (permissive) {
	  v = permissive;
	  checkmin = 0;
	} else
	  break;
      } else if ((sc & 0xFE) == 0xFC) {
	if ((i + 5 < end) 
	    && ((s[i + 1] & 0xC0) == 0x80)
	    && ((s[i + 2] & 0xC0) == 0x80)
	    && ((s[i + 3] & 0xC0) == 0x80)
	    && ((s[i + 4] & 0xC0) == 0x80)
	    && ((s[i + 5] & 0xC0) == 0x80)) {
	  v = (((sc & 0x1) << 30) 
	       | ((s[i+1] & 0x3F) << 24) 
	       | ((s[i+2] & 0x3F) << 18) 
	       | ((s[i+3] & 0x3F) << 12) 
	       | ((s[i+4] & 0x3F) << 6) 
	       | (s[i+5] & 0x3F));
	  checkmin = 0x4000000;
	  i += 5;
	} else if (permissive) {
	  v = permissive;
	  checkmin = 0;
	} else
	  break;
      } else {
	if (permissive) {
	  if (sc >= 254) {
	    v = permissive;
	    checkmin = 0;
	  } else {
	    /* Corresponds to the middle of a char. We simply drop
	       it. */
	    j--;
	    continue;
	  }
	} else
	  break;
      }

      if (v < checkmin) {
	if (permissive) {
	  v = permissive;
	} else
	  break;
      }

      if (utf16) {
	if (v > 0xFFFF) {
	  if (us) {
	    v -= 0x10000;
#ifdef SCHEME_BIG_ENDIAN
	    ((unsigned short *)us)[j] = 0xD8000000 | ((v >> 10) & 0x3FF);
	    ((unsigned short *)us)[j+1] = 0xDC000000 | (v & 0x3FF);
#else
	    ((unsigned short *)us)[j+1] = 0xD8000000 | ((v >> 10) & 0x3FF);
	    ((unsigned short *)us)[j] = 0xDC000000 | (v & 0x3FF);
#endif
	  }
	  j++;
	} else if (us) {
	  ((unsigned short *)us)[j] = v;
	}	  
      } else if (us) {
	us[j] = v;
      }
    }
  }

  if ((i < end) && (j < dend))
    return -1;

  if (ipos)
    *ipos = i;

  return j;
}

int scheme_utf8_decode_all(const unsigned char *s, int len, unsigned int *us, int permissive)
{
  return scheme_utf8_decode(s, 0, len, us, 0, -1, NULL, 0 /* utf16 */, permissive);
}

int scheme_utf8_encode(const unsigned int *us, int start, int end, 
		       unsigned char *s, int dstart,
		       char utf16)
{
  int i, j;

  if (!s) {
    unsigned int wc;
    j = 0;
    for (i = start; i < end; i++) {
      if (utf16) {
	wc = ((unsigned short *)us)[i];
	if ((wc & 0xD800) == 0xD800) {
	  /* Unparse surrogates. We assume that the surrogates are
	     well formed. */
#ifdef SCHEME_BIG_ENDIAN
	  wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)us)[i]) & 0x3FF);
#else
	  wc = (wc & 0x3FF) + (((((unsigned short *)us)[i]) & 0x3FF) << 10);
#endif
	  wc += 0x100000;
	}
      } else {
	wc = us[i];
      }
      if (wc < 0x80) {
	j += 1;
      } else if (wc < 0x800) {
	j += 2;
      } else if (wc < 0x10000) {
	j += 3;
      } else if (wc < 0x200000) {
	j += 4;
      } else if (wc < 0x4000000) {
	j += 5;
      } else {
	j += 6;
      }
    }
    return j;
  } else {
    unsigned int wc;
    j = dstart;
    for (i = start; i < end; i++) {
      if (utf16) {
	wc = ((unsigned short *)us)[i];
	if ((wc & 0xD800) == 0xD800) {
	  /* Unparse surrogates. We assume that the surrogates are
	     well formed. */
#ifdef SCHEME_BIG_ENDIAN
	  wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)us)[i]) & 0x3FF);
#else
	  wc = (wc & 0x3FF) + (((((unsigned short *)us)[i]) & 0x3FF) << 10);
#endif
	  wc += 0x100000;
	}
      } else {
	wc = us[i];
      }

      if (wc < 0x80) {
	s[j++] = wc;
      } else if (wc < 0x800) {
	s[j++] = 0xC0 | ((wc & 0x7C0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x10000) {
	s[j++] = 0xE0 | ((wc & 0xF000) >> 12);
	s[j++] = 0x80 | ((wc & 0x0FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x200000) {
	s[j++] = 0xF0 | ((wc & 0x1C0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x03F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x4000000) {
	s[j++] = 0xF8 | ((wc & 0x3000000) >> 24);
	s[j++] = 0x80 | ((wc & 0x0FC0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x003F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x0000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else {
	s[j++] = 0xFC | ((wc & 0x40000000) >> 30);
	s[j++] = 0x80 | ((wc & 0x3F000000) >> 24);
	s[j++] = 0x80 | ((wc & 0x00FC0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x0003F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x00000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      }
    }
    return j;
  }
}

int scheme_utf8_encode_all(const unsigned int *us, int len, unsigned char *s)
{
  return scheme_utf8_encode(us, 0, len, s, 0, 0 /* utf16 */);
}

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
	while (c && portable_isspace(((unsigned char *)buff)[c - 1])) {
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


/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_STRING_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_string_converter_type, mark_string_convert);
}

END_XFORM_SKIP;

#endif
