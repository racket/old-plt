/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

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
#include <math.h>
#include <string.h>
#include <ctype.h>

#define TO_DOUBLE scheme_TO_DOUBLE

#define zeroi scheme_make_integer(0)

/* Don't bother reading more than the following number of digits in a
   floating-point mantissa: */
#define MAX_FLOATREAD_PRECISION_DIGITS 50

/* We'd like to use strtod() for the common case, but we don't trust
   it entirely. */
#define MAX_FAST_FLOATREAD_LEN 50

/* Exponent threshold for obvious infinity. Must be at least
   max(MAX_FAST_FLOATREAD_LEN, MAX_FLOATREAD_PRECISION_DIGITS) more
   than the larget possible FP exponent. */
#define CHECK_INF_EXP_THRESHOLD 400

#ifdef USE_EXPLICT_FP_FORM_CHECK

/* Fixes Linux problem of 0e...  => non-number (0 with ptr at e...) */
/* Fixes SunOS problem with numbers like .3e2666666666666 => 0.0 */
/* Fixes HP/UX problem with numbers like .3e2666666666666 => non-number */

# ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
# endif

double STRTOD(const char *orig_c, char **f)
{
  int neg = 0;
  int found_dot = 0, is_infinity = 0, is_zero = 0;
  const char *c = orig_c;

  *f = (char *)c;

  if (*c == '-') {
    c++;
    neg = 1;
  } else if (*c == '+') {
    c++;
  }

  if (!isdigit((unsigned char)*c)) {
    if (*c == '.') {
      if (!isdigit((unsigned char)c[1]))
	return 0; /* no digits - bad! */
    } else
      return 0; /* no digits - bad! */
  }

  for (; *c; c++) {
    int ch = *c;

    if (isdigit(ch)) {
      /* ok */
    } else if ((ch == 'e') || (ch == 'E')) {
      int e = 0, neg_exp = 0;

      c++;
      if (*c == '-') {
	c++;
	neg_exp = 1;
      } else if (*c == '+') {
	c++;
      }
      if (!isdigit((unsigned char)*c))
	return 0; /* no digits - bad! */

      for (; *c; c++) {
	int ch = *c;
	if (!isdigit(ch))
	  return 0; /* not a digit - bad! */
	else {
	  e = (e * 10) + (ch - '0');
	  if (e > CHECK_INF_EXP_THRESHOLD) {
	    if (neg_exp)
	      is_zero  = 1;
	    else
	      is_infinity  = 1;
	  }
	}
      }

      break;
    } else if (ch == '.') {
      if (found_dot)
	return 0; /* two dots - bad! */
      found_dot = 1;
    } else
      return 0; /* unknown non-digit - bad! */
  }
  
  *f = (char *)c;

  if (is_infinity) {
    if (neg)
      return scheme_minus_infinity_val;
    else
      return scheme_infinity_val;
  }

  if (is_zero) {
    if (neg)
      return scheme_floating_point_nzero;
    else
      return scheme_floating_point_zero;
  }

  /* It's OK if c is ok: */
  return strtod(orig_c, NULL);
}
# ifdef MZ_PRECISE_GC
START_XFORM_ARITH;
# endif
#else
#define STRTOD(x, y) strtod(x, y)
#endif

#ifdef MZ_USE_SINGLE_FLOATS
static Scheme_Object *CHECK_SINGLE(Scheme_Object *v, int s)
{
  if (s && SCHEME_DBLP(v))
    return scheme_make_float((float)SCHEME_DBL_VAL(v));
  else
    return v;
}
#else
# define CHECK_SINGLE(v, s) v
#endif

Scheme_Object *scheme_read_number(const char *str, long len,
				  int is_float, 
				  int is_not_float,
				  int decimal_means_float,
				  int radix, int radix_set, 
				  Scheme_Object *complain,
				  int *div_by_zero,
				  int test_only)
{
  int i, has_decimal, must_parse, has_slash;
  int report, delta;
  Scheme_Object *next_complain;
  int has_hash, has_expt, has_i, has_sign, has_at, saw_digit, saw_nonzero_digit;
  Scheme_Object *o;
  const char *orig;
#ifdef MZ_USE_SINGLE_FLOATS
  int single;
#endif

  if (len < 0)
    len = strlen(str);

  orig = str;

  delta = 0;

  while (str[delta] == '#') {
    if (str[delta+1] != 'E' && str[delta+1] != 'e' && str[delta+1] != 'I' && str[delta+1] != 'i') {
      if (radix_set) {
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad radix specification: %t",
			   str, len);
	else
	  return scheme_false;
      }
      radix_set = 1;
    } else {
      if (is_float || is_not_float) {
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad exactness specification: %t", 
			   str, len);
	else
	  return scheme_false;
      }
    }

    switch (str[delta+1]) {
    case 'B':
    case 'b':
      radix = 2;
      break;
    case 'O':
    case 'o':
      radix = 8;
      break;
    case 'D':
    case 'd':
      radix = 10;
      break;
    case 'X':
    case 'x':
      radix = 16;
      break;
    case 'I':
    case 'i':
      is_float = 1;
      break;
    case 'E':
    case 'e':
      is_not_float = 1;
      break;
    default:
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: bad `#' indicator `%c' in: %t",
			 str[delta+1], str, len);
      return scheme_false;
    }
    delta += 2;
  }

  must_parse = (radix_set || is_float || is_not_float);

  report = complain && must_parse;
  next_complain = must_parse ? complain : NULL;

  if (!(len - delta)) {
    if (report)
      scheme_raise_exn(MZEXN_READ, complain, 
		       "read-number: no digits");
    return scheme_false;
  }

  /* look for +inf.0, etc: */
  if (len -delta == 6) {
    Scheme_Object *special;
    special = scheme_read_special_number(str, delta);
    if (special)
      return special;
  }

  /* Look for <special>+...i and ...<special>i */
  if ((len-delta > 7) && str[len-1] == 'i') {
    Scheme_Object *special;
    char *s2;
    
    /* Try <special>+...i */
    special = scheme_read_special_number(str, delta);
    if (special) {
      s2 = scheme_malloc_atomic(len - delta - 6 + 4 + 1);
      memcpy(s2, "+0.0", 4);
      memcpy(s2 + 4, str + delta + 6, len - delta - 5);
    } else {
      /* Try ...<special>i: */
      special = scheme_read_special_number(str, len - 7);
      if (special) {
	s2 = scheme_malloc_atomic(len - delta - 6 + 4 + 1);
	memcpy(s2, str + delta, len - delta - 7);
	memcpy(s2 + len - delta - 7, "+0.0i", 6);
	special = scheme_bin_mult(special, scheme_plus_i);
      } else
	s2 = NULL;
    }

    if (special) {
      Scheme_Object *other;
      int dbz = 0;

      other = scheme_read_number(s2, len - delta - 6 + 4,
				 is_float, is_not_float, 1,
				 radix, 1, 0,
				 &dbz, test_only);

      if (dbz) {
	if (div_by_zero)
	  *div_by_zero = 1;
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: division by zero in %t",
			   str, len);
	return scheme_false;
      }

      if (!SCHEME_FALSEP(other))
	return scheme_bin_plus(special, other);
      
      if (!complain)
	return scheme_false;
    }
  }

  /* Look for <special>@... and ...@<special> */
  if ((len - delta > 7) && ((str[delta+6] == '@') || (str[len - 7] == '@'))) {
    Scheme_Object *special;
    char *s2;
    int spec_mag = 0;

    /* Try <special>@... */
    if (str[delta+6] == '@')
      special = scheme_read_special_number(str, delta);
    else
      special = NULL;
    if (special) {
      s2 = scheme_malloc_atomic(len - delta - 6);
      memcpy(s2, str + delta + 7, len - delta - 6);
      spec_mag = 1;
    } else {
      if (str[len - 7] == '@')
	special = scheme_read_special_number(str, len - 6);
      else
	special = NULL;
      
      if (special) {
	s2 = scheme_malloc_atomic(len - delta - 6);
	memcpy(s2, str + delta, len - delta - 7);
	s2[len - delta - 7] = 0;
      } else
	s2 = NULL;
    }

    if (special) {
      Scheme_Object *other;
      int dbz = 0;

      /* s2 can't contain @: */
      for (i = 0; s2[i]; i++) {
	if (s2[i] == '@')
	  break;
      }

      if (s2[i])
	other = scheme_false;
      else
	other = scheme_read_number(s2, len - delta - 7,
				   is_float, is_not_float, 1,
				   radix, 1, 0,
				   &dbz, test_only);

      if (dbz) {
	if (div_by_zero)
	  *div_by_zero = 1;
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: division by zero in %t", 
			   str, len);
	return scheme_false;
      }

      if (!SCHEME_FALSEP(other)) {
	/* If string is complex, not well-formed: */
	if (!SCHEME_COMPLEXP(other)) {
	  Scheme_Object *a[2];
	  if (spec_mag) {
	    a[0] = special;
	    a[1] = other;
	  } else {
	    a[0] = other;
	    a[1] = special;
	  }

	  return scheme_make_polar(2, a);
	}
      }

      if (!complain)
	return scheme_false;
    }
  }
      
#define isinexactmark(ch) ((ch == 'e') || (ch == 'E') \
			   || (ch == 's') || (ch == 'S') \
			   || (ch == 'f') || (ch == 'F') \
			   || (ch == 'd') || (ch == 'D') \
			   || (ch == 'l') || (ch == 'L'))

#define isbaseNdigit(N, ch) (((ch >= 'a') && (ch <= ('a' + N - 11))) \
                             || ((ch >= 'A') && (ch <= ('A' + N - 11))))

  has_i = 0;
  has_at = 0;
  has_sign = delta-1;
  for (i = delta; i < len; i++) {
    int ch = str[i];
    if (!ch) {
      if (report)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: embedded null character: %t",
			 str, len);
      return scheme_false;
    } else if (isinexactmark(ch) && ((radix <= 10) || !isbaseNdigit(radix, ch))) {
      /* If a sign follows, don't count it */
      if (str[i+1] == '+' || str[i+1] == '-')
	i++;
    } else if ((ch == '+') || (ch == '-')) {
      if ((has_sign > delta) || ((has_sign == delta) && (i == delta+1))) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: too many signs: %t", 
			   str, len);
	return scheme_false;
      }
      has_sign = i;
    } else if (((ch == 'I') || (ch == 'i')) && (has_sign >= delta)) {
      if (has_at) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: cannot mix `@' and `i': %t", 
			   str, len);
	return scheme_false;
      }
      if (i + 1 < len) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: `i' must be at the end: %t", 
			   str, len);
	return scheme_false;
      }
      has_i = i;
    } else if (ch == '@') {
      if (has_at) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: too many `@'s: %t", 
			   str, len);
	return scheme_false;
      }
      if (i == delta) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: `@' cannot be at start: %t", 
			   str, len);
	return scheme_false;
      }
      has_at = i;
      if (has_sign >= delta)
	has_sign = delta-1;
    }
  }

  if (has_i) {
    Scheme_Object *n1, *n2;
    char *first, *second;
    int fdbz = 0, sdbz = 0;

    if (has_sign != delta) {
      first = (char *)scheme_malloc_atomic(has_sign - delta + 1);
      memcpy(first, str + delta, has_sign - delta);
      first[has_sign - delta] = 0;
    } else
      first = NULL;

    if (has_i - has_sign > 1) {
      second = (char *)scheme_malloc_atomic(has_i - has_sign + 1);
      memcpy(second, str + has_sign, has_i - has_sign);
      second[has_i - has_sign] = 0;
    } else
      second = NULL;

    if (first)
      n1 = scheme_read_number(first, has_sign - delta,
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &fdbz, test_only);
    else
      n1 = zeroi;

    if (SAME_OBJ(n1, scheme_false) && !fdbz)
      return scheme_false;
    else if (SCHEME_FLOATP(n1)) {
      double d = SCHEME_FLOAT_VAL(n1);
      if (MZ_IS_NAN(d))
	return scheme_false;
    }
    
    if (second)
      n2 = scheme_read_number(second, has_i - has_sign,
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &sdbz, test_only);
    else if (str[has_sign] == '-')
      n2 = scheme_make_integer(-1);
    else
      n2 = scheme_make_integer(1);
    
    if (SAME_OBJ(n2, scheme_false) && !sdbz)
      return scheme_false;
    else if (SCHEME_FLOATP(n2)) {
      double d = SCHEME_FLOAT_VAL(n2);
      if (MZ_IS_NAN(d))
	return scheme_false;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %t", 
			 str, len);
      return scheme_false;
    }

    if (!is_not_float && ((SCHEME_FLOATP(n1) && (n2 != zeroi)) || is_float))
      n2 = scheme_exact_to_inexact(1, &n2);  /* uses default conversion: float or double */
    else if (is_not_float)
      n2 = scheme_inexact_to_exact(1, &n2);

    if (!is_not_float && ((SCHEME_FLOATP(n2) && (n1 != zeroi)) || is_float))
      n1 = scheme_exact_to_inexact(1, &n1); /* uses default conversion: float or double */
    else if (is_not_float)
      n1 = scheme_inexact_to_exact(1, &n1);

    return scheme_make_complex(n1, n2);
  }

  if (has_at) {
    Scheme_Object *n1, *n2;
    double d1, d2, r1, r2;
    char *first;
    const char *second;
    int fdbz = 0, sdbz = 0;

    first = (char *)scheme_malloc_atomic(has_at - delta + 1);
    memcpy(first, str + delta, has_at - delta);
    first[has_at - delta] = 0;

#ifdef MZ_PRECISE_GC
    {
      /* Can't pass mis-aligned pointer to scheme_read_number. */
      int slen = len - (has_at + 1) + 1;
      second = (char *)scheme_malloc_atomic(slen);
      memcpy((char *)second, str + has_at + 1, slen);
    }
#else
    second = str + has_at + 1;
#endif

    n2 = scheme_read_number(second, len - has_at - 1,
			    is_float, is_not_float, decimal_means_float,
			    radix, 1, next_complain,
			    &fdbz, test_only);

    if (!fdbz) {
      if (SCHEME_FALSEP(n2))
	return scheme_false;

      /* Special case: angle is zero => real number */
      if (n2 == zeroi)
	return scheme_read_number(first, has_at - delta,
				  is_float, is_not_float, decimal_means_float,
				  radix, 1, complain,
				  div_by_zero,
				  test_only);
      
      n2 = TO_DOUBLE(n2);

      d2 = SCHEME_FLOAT_VAL(n2);
      
      if (MZ_IS_NAN(d2))
	return scheme_false;

      n1 = scheme_read_number(first, has_at - delta, 
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &sdbz,
			      test_only);

      /* Special case: magnitude is zero => zero */
      if (n1 == zeroi)
	return zeroi;

      if (!SCHEME_FALSEP(n1))
	n1 = TO_DOUBLE(n1);
    } else {
      n1 = NULL;
      d2 = 0;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %t", 
			 str, len);
      return scheme_false;
    }

    if (SCHEME_FALSEP(n1))
      return scheme_false;

    d1 = SCHEME_FLOAT_VAL(n1);

    if (MZ_IS_NAN(d1))
      return scheme_false;

    r1 = d1 * cos(d2);
    r2 = d1 * sin(d2);

#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(n1) && SCHEME_FLTP(n2))
      return scheme_make_complex(scheme_make_float((float)r1),
				 scheme_make_float((float)r2));
#endif

    return scheme_make_complex(scheme_make_double(r1),
			       scheme_make_double(r2));
  }

  has_decimal = has_slash = has_hash = has_expt = saw_digit = saw_nonzero_digit = 0;
  for (i = delta; i < len; i++) {
    int ch = str[i];
    if (ch == '.') {
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: multiple decimal points: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: decimal points and fractions "
			   "cannot be mixed: %t", 
			   str, len);
	return scheme_false;
      }
      has_decimal = 1;
    } else if (isinexactmark(ch)) {
      if ((radix <= 10) || !isbaseNdigit(radix, ch)) {
	if (i == delta) {
	  if (report)
	    scheme_raise_exn(MZEXN_READ, complain, 
			     "read-number: cannot begin with `%c' in %t", 
			     ch, str, len);
	  return scheme_false;
	}
	has_expt = i;
	break;
      }
    } else if (ch == '/') {
      if (i == delta) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: cannot have slash at start: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: multiple slashes: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: decimal points and fractions "
			   "cannot be mixed: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
	return scheme_false;
      }
      has_slash = i;
    } else if ((ch == '-') || (ch == '+')) {
      if (has_slash || has_decimal || has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced sign: %t", 
			   str, len);
	return scheme_false;
      }
    } else if (ch == '#') {
      if (has_slash || /* has_decimal || (radix > 10) || */ !saw_digit) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
	return scheme_false;
      }
      has_hash = 1;
    } else if (!isdigit(ch) && !((radix > 10) && isbaseNdigit(radix, ch))) {
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad decimal number: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
	return scheme_false;
      }
      break;
    } else {
      saw_digit = 1;
      if (ch != '0')
	saw_nonzero_digit = 1;
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
	return scheme_false;
      }
    }
  }

#ifdef MZ_USE_SINGLE_FLOATS
  if (has_expt && str[has_expt]) {
    single = str[has_expt];
    single = ((single == 'f') || (single == 'F')
	      || (single == 's') || (single == 'S'));
  } else {
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    single = 1;
# else
    single = 0;
# endif
  }
#endif


  /* When possible, use the standard floating-point parser */
  if (!is_not_float && (is_float || decimal_means_float) 
      && !has_slash && !has_hash && (radix == 10) 
      && (has_decimal || has_expt)
      && (len <= MAX_FAST_FLOATREAD_LEN)) {
    double d;
    const char *cpy;
    char *ptr;

    if (has_expt && (str[has_expt] != 'e' && str[has_expt] != 'E')) {
      char *str2;
      str2 = (char *)scheme_malloc_atomic(len + 1);
      memcpy(str2, str, len + 1);
      str2[has_expt] = 'e';
      cpy = str2;
    } else
      cpy = str;
    d = STRTOD(cpy + delta, &ptr);
    if ((ptr - cpy) < len) {
      ptr = NULL; /* because not precise-gc aligned */
      if (report)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: bad decimal number %t",
			 str, len);
      return scheme_false;
    } 
    ptr = NULL; /* because not precise-gc aligned */

    if (!saw_nonzero_digit) {
      /* Assert: d = 0.0 or -0.0 */
      if (str[delta] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (single) return nzerof;
#endif
	return scheme_nzerod;
      }
    }

#ifdef DOUBLE_CHECK_NEG_ZERO_UNDERFLOW
    if (!d) {
      if (str[delta] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (single) return nzerof;
#endif
	return scheme_nzerod;
      }
    }
#endif

#ifdef MZ_USE_SINGLE_FLOATS
    if (single)
      return scheme_make_float((float)d);
#endif
    return scheme_make_double(d);
  }

  if (has_decimal || has_expt || has_hash) {
    Scheme_Object *mantissa, *exponent, *power, *n;
    Scheme_Object *args[2];
    int result_is_float= (is_float || (!is_not_float && decimal_means_float));

    if (has_expt) {
      char *substr;

#ifdef MZ_PRECISE_GC
      {
	/* Can't pass misaligned pointer to scheme_read_bignum: */
	int slen = len - (has_expt + 1) + 1;
	substr = (char *)scheme_malloc_atomic(slen);
	memcpy(substr, str + has_expt + 1, slen);
      }
#else
      substr = (char *)str + has_expt + 1;
#endif

      exponent = scheme_read_bignum(substr, 0, radix);
      if (SCHEME_FALSEP(exponent)) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad exponent: %t", 
			   str, len);
	return scheme_false;
      }
    } else
      exponent = zeroi;

    if (!has_expt)
      has_expt = len;

    if (has_slash) {
      /* Mantissa is a fraction. */
      char *s;
      int dbz;
      
      s = (char *)scheme_malloc_atomic(has_expt - delta + 1);
      memcpy(s, str + delta, has_expt - delta);
      s[has_expt - delta] = 0;
      
      mantissa = scheme_read_number(s, has_expt - delta, 
				    0, 0, 1,
				    radix, 1, next_complain,
				    &dbz,
				    test_only);

      if (SCHEME_FALSEP(mantissa)) {
	if (dbz) {
	  if (div_by_zero)
	    *div_by_zero = 1;
	  if (complain)
	    scheme_raise_exn(MZEXN_READ, complain, 
			     "read-number: division by zero in %t", 
			     str, len);
	}
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad number %t", 
			   str, len);
	return scheme_false;
      }
    } else {
      /* Mantissa is not a fraction. */
      char *digits;
      int extra_power = 0, dcp = 0, num_ok;

      digits = (char *)scheme_malloc_atomic(has_expt - delta + 1);

      i = delta;
      if (str[i] == '+' || str[i] == '-')
	digits[dcp++] = str[i++];

      for (; isdigit((unsigned char)str[i]) || ((radix > 10) && isbaseNdigit(radix, str[i])); i++) {
	digits[dcp++] = str[i];
      }

      if (str[i] == '#') {
	for (; str[i] == '#'; i++) {
	  digits[dcp++] = '0';
	}
	num_ok = 0;
      } else
	num_ok = 1;
	
      if (str[i] == '.') {
	i++;
	if (num_ok)
	  for (; isdigit((unsigned char)str[i]) || ((radix > 10) && isbaseNdigit(radix, str[i])); i++) {
	    digits[dcp++] = str[i];
	    extra_power++;
	  }

	for (; str[i] == '#'; i++) {
	  digits[dcp++] = '0';  
	  extra_power++;
	}
      }

      if ((str[i] && (!has_expt || i != has_expt))
	  || !dcp || (dcp == 1 && !(isdigit((unsigned char)digits[0])
				    || ((radix > 10) && isbaseNdigit(radix, digits[0]))))) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad decimal number %t", 
			   str, len);
	return scheme_false;
      }

      /* Reduce unnecessary mantissa-reading work for inexact results.
         This is also necessary to make the range check on `exponent'
         correct. */
      if (result_is_float && (dcp > MAX_FLOATREAD_PRECISION_DIGITS)) {
	extra_power -= (dcp - MAX_FLOATREAD_PRECISION_DIGITS);
	dcp = MAX_FLOATREAD_PRECISION_DIGITS;
      }

      digits[dcp] = 0;
      mantissa = scheme_read_bignum(digits, 0, radix);
      if (SCHEME_FALSEP(mantissa)) {
	/* can get here with bad radix */
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad number %t", 
			   str, len);
	return scheme_false;
      }

      if (extra_power)
	exponent = scheme_bin_minus(exponent, scheme_make_integer(extra_power));
    
      /* Don't calculate a huge exponential if we're returning a float: */
      if (result_is_float) {
	if (scheme_bin_gt(exponent, scheme_make_integer(CHECK_INF_EXP_THRESHOLD))) {
	  if (SCHEME_TRUEP(scheme_negative_p(1, &mantissa)))
	    return CHECK_SINGLE(scheme_minus_inf_object, single);
	  else
	    return CHECK_SINGLE(scheme_inf_object, single);
	} else if (scheme_bin_lt(exponent, scheme_make_integer(-CHECK_INF_EXP_THRESHOLD))) {
	  if (SCHEME_TRUEP(scheme_negative_p(1, &mantissa)))
	    return CHECK_SINGLE(scheme_nzerod, single);
	  else
	    return CHECK_SINGLE(scheme_zerod, single);
	}
      }
    }

    /* This is the important use of test_only, because it's the one
       place where the read calculation is not linear in the input. */
    if (test_only)
      return scheme_make_integer(1);

    args[0] = scheme_make_integer(radix);
    args[1] = exponent;
    power = scheme_expt(2, args);

    n = scheme_bin_mult(mantissa, power);

    if (result_is_float)
      n = CHECK_SINGLE(TO_DOUBLE(n), single);
    else
      n = CHECK_SINGLE(n, single);

    if (SCHEME_FLOATP(n) && str[0] == '-') {
      if (SCHEME_FLOAT_VAL(n) == 0.0) {
	/* 0.0 => -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (SCHEME_FLTP(n)) {
	  n = scheme_make_float(-SCHEME_FLT_VAL(n));
	} else
#endif
	  n = scheme_make_double(-SCHEME_DBL_VAL(n));
      }
    }

    return n;
  }
  
  if (has_slash) {
    Scheme_Object *n1, *n2;
    char *first;

    first = (char *)scheme_malloc_atomic(has_slash - delta + 1);
    memcpy(first, str + delta, has_slash - delta);
    first[has_slash - delta] = 0;

    n1 = scheme_read_number(first, has_slash - delta,
			    0, 0, 1,
			    radix, 1, next_complain,
			    div_by_zero,
			    test_only);
    if (SAME_OBJ(n1, scheme_false))
      return scheme_false;

    {
      char *substr;

#ifdef MZ_PRECISE_GC
      {
	/* Can't pass misaligned pointer to scheme_read_bignum: */
	int slen = len - (has_slash + 1) + 1;
	substr = (char *)scheme_malloc_atomic(slen);
	memcpy(substr, str + has_slash + 1, slen);
      }
#else
      substr = (char *)str + has_slash + 1;
#endif

      n2 = scheme_read_number(substr, len - has_slash - 1,
			      0, 0, 1,
			      radix, 1, next_complain,
			      div_by_zero,
			      test_only);
    }

    if (SAME_OBJ(n2, scheme_false))
      return scheme_false;

    if (SCHEME_TRUEP(scheme_zero_p(1, &n2))) {
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %t", 
			 str, len);
      if (div_by_zero)
	*div_by_zero = 1;
      return scheme_false;
    }

    if (test_only)
      return scheme_make_integer(1);

    n1 = scheme_bin_div(n1, n2);

    if (is_not_float)
      n1 = scheme_inexact_to_exact(1, &n1);
    else if (is_float)
      n1 = TO_DOUBLE(n1);

    return CHECK_SINGLE(n1, single);
  }

  o = scheme_read_bignum(str, delta, radix);
  if (SAME_OBJ(o, scheme_false)) {
    if (report)
      scheme_raise_exn(MZEXN_READ, complain, 
		       "read-number: bad number %t", 
		       str, len);
  } else if (is_float) {
    /* Special case: "#i-0" => -0. */
    if ((o == zeroi) && str[delta] == '-') {
#ifdef MZ_USE_SINGLE_FLOATS
      if (single) return nzerof;
#endif
      return scheme_nzerod;
    }

    return CHECK_SINGLE(TO_DOUBLE(o), single);
  }

  return o;
}

