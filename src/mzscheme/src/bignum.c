/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt

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


  Some algorithms imitate VSCM:

   (C) m.b (Matthias Blume); May 1992, HUB; Jan 1993 PU/CS
           Humboldt-University of Berlin
           Princeton University, Dept. of Computer Science
*/


/* DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER!

   This code is fragile, due to the Small_Bignum optimization, and
   memory subtleties of bignums.

   When allocating a bignum for a small integer, a Small_Bignum is
   allocated. The Small_Bignum structure has room for one bigdig, and
   sometimes it is allocated with room for two.

   The digit array pointer of a Small_Bignum points into the (middle
   of the) Small_Bignum record itself. This means:

     1) For all collectors, the digit array point must not be copied
        to another Scheme_Bignum record (because it points into the
        middle of the Small_Bignum record, and interior pointers are
        not allowed).

     2) Since SCHEME_BIGDIG() can return an interior pointer, for
        precise GC the code must be careful about putting
        SCHEME_BIGDIG() results into local variables. In some cases, a
        variable has to be zeroed out before calling a sub-procedure;
        in other cases, the zeroing is skipped because we can provide
        that it's never a digit array for a Small_Bignum.

   In addition, the precise GC needs to distinguish Scheme_Bignum from
   Small_Bignum for computing sizes; the allocated_inline flag does
   that. 
*/

#include "schpriv.h"
#include <ctype.h>
#include <math.h>
#include "gmp/gmp.h"

#ifdef SIXTY_FOUR_BIT_INTEGERS
#define FIRST_BIT_MASK 0x8000000000000000
#define SECOND_BIT_MASK 0x4000000000000000
#define MAX_TWO_BIT_MASK 0xC000000000000000
#define BIG_RADIX 0x10000000000000000
#define ALL_ONES 0xFFFFFFFFFFFFFFFF
#define WORD_SIZE 64
#else
#define FIRST_BIT_MASK 0x80000000
#define SECOND_BIT_MASK 0x40000000
#define MAX_TWO_BIT_MASK 0xC0000000
#define BIG_RADIX 0x100000000
#define ALL_ONES 0xFFFFFFFF
#define WORD_SIZE 32
#endif

static Scheme_Object *bignum_one;

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif


#define xor(a, b) (((a) && !(b)) || (!(a) && (b)))

Scheme_Object *scheme_make_small_bignum(long v, Small_Bignum *o)
{
  o->o.type = scheme_bignum_type;
  SCHEME_BIGPOS(&o->o) = ((v >= 0) ? 1 : 0);
  if (v < 0)
    v = -v;

  if (v == 0)
    SCHEME_BIGLEN(&o->o) = 0;
  else
    SCHEME_BIGLEN(&o->o) = 1;

  SCHEME_BIGDIG(&o->o) = o->v;

  o->v[0] = v;

  return (Scheme_Object *)o;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

Scheme_Object *scheme_make_bignum(long v)
{
  Small_Bignum *r;
  r = MALLOC_ONE_TAGGED(Small_Bignum);
#if MZ_PRECISE_GC
  r->o.allocated_inline = 1;
#endif  
  return scheme_make_small_bignum(v, r);
}

Scheme_Object *scheme_make_bignum_from_unsigned(unsigned long v)
{
  Small_Bignum *r;
  r = MALLOC_ONE_TAGGED(Small_Bignum);
#if MZ_PRECISE_GC
  r->o.allocated_inline = 1;
#endif  
  r->o.type = scheme_bignum_type;
  SCHEME_BIGPOS(&r->o) = 1;
  if (v == 0)
    SCHEME_BIGLEN(&r->o) = 0;
  else
    SCHEME_BIGLEN(&r->o) = 1;

  SCHEME_BIGDIG(&r->o) = r->v;
  
  r->v[0] = v;

  return (Scheme_Object*) r;
}

/*
  Should only succeed if the bignum can fit into a signed 32/64 bit word.
  This means that the bignum must have length 0 or 1 and the top bit
  of its bigdig must be zero, unless it is -100...000.
  
*/
int scheme_bignum_get_int_val(const Scheme_Object *o, long *v)
{

  if (SCHEME_BIGLEN(o) > 1)    /* won't fit in a tagged word */
    return 0;
  else if (SCHEME_BIGLEN(o) == 0) {
    *v = 0;
    return 1;
  } else if (SCHEME_BIGDIG(o)[0] == FIRST_BIT_MASK && !SCHEME_BIGPOS(o)) {
    /* Special case for the most negative number representable in a signed word */
    *v = SCHEME_BIGDIG(o)[0];
    return 1;
  } else if ((SCHEME_BIGDIG(o)[0] & FIRST_BIT_MASK) != 0)/*Won't fit into a signed word */
    return 0;
  else if (SCHEME_BIGPOS(o)) {
    *v = SCHEME_BIGDIG(o)[0];
    return 1;
  } else {
    *v = -SCHEME_BIGDIG(o)[0];
    return 1;
  }
}

/* 
   I'm going to guess that we want to return anything that will fit into a 
   full 32/64 bit unsigned word.
*/
int scheme_bignum_get_unsigned_int_val(const Scheme_Object *o, unsigned long *v)
{
  if ((SCHEME_BIGLEN(o) > 1) || !SCHEME_BIGPOS(o))
    /* Won't fit into word, or not positive */
    return 0;
  else if (SCHEME_BIGLEN(o) == 0) {
    *v = 0;
    return 1;
  } else {
    *v = SCHEME_BIGDIG(o)[0];
    return 1;
  }
}

/* If the bignum fits into a scheme integer, return that instead */
Scheme_Object *scheme_bignum_normalize(const Scheme_Object *o)
{
  long v;
  
  if (!SCHEME_BIGNUMP(o))
    return (Scheme_Object *)o;

  if (scheme_bignum_get_int_val(o, &v)) {
    long t;
    
    t = v & MAX_TWO_BIT_MASK;
    if (t == 0 || t == MAX_TWO_BIT_MASK)
      return scheme_make_integer(v);
    else
      return (Scheme_Object*)o;
  } else
    return (Scheme_Object*)o;
}

/* 
   copy the bignum a, and if msd != 0, concat. it as the most significant
   digit
*/
static Scheme_Object *bignum_copy(const Scheme_Object *a, long msd)
{
  Scheme_Object* o;
  int c;
  bigdig* o_digs;

  c = SCHEME_BIGLEN(a);
  o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));

  o->type = scheme_bignum_type;
  SCHEME_BIGLEN(o) = c;
  SCHEME_BIGPOS(o) = SCHEME_BIGPOS(a);
  if (msd)
    o_digs = (bigdig *)scheme_malloc_atomic(sizeof(bigdig) * (c + 1));
  else
    o_digs = (bigdig *)scheme_malloc_atomic(sizeof(bigdig) * c);
  SCHEME_BIGDIG(o) = o_digs;

  memcpy(o_digs, SCHEME_BIGDIG(a), sizeof(bigdig) * c);

  if (msd) {
    o_digs[c] = msd;
    SCHEME_BIGLEN(o) = SCHEME_BIGLEN(o) + 1;
  }
  return o;
}

int scheme_bignum_eq(const Scheme_Object *a, const Scheme_Object *b)
{
  long a_len, b_len;

  a_len = SCHEME_BIGLEN(a);
  b_len = SCHEME_BIGLEN(b);

  if (a_len == 0 && b_len == 0)
    return 1;

  if (a_len == b_len && SCHEME_BIGPOS(a) == SCHEME_BIGPOS(b))
    return mpn_cmp(SCHEME_BIGDIG(a), SCHEME_BIGDIG(b), b_len) == 0;
  else
    return 0;
}

/* - if a < b, 0 if a == b, + if  a > b */
static int bignum_abs_cmp(const Scheme_Object *a, const Scheme_Object *b)
{
  long a_len, b_len;

  a_len = SCHEME_BIGLEN(a);
  b_len = SCHEME_BIGLEN(b);
 
  if (a_len > b_len)
    return 1;
  else if (a_len < b_len)
    return -1;
  else if (a_len == 0)
    return 0;
  else
    return mpn_cmp(SCHEME_BIGDIG(a), SCHEME_BIGDIG(b), b_len);
}

int scheme_bignum_lt(const Scheme_Object *a, const Scheme_Object *b)
{
  long a_pos, b_pos;
  int res;

  a_pos = SCHEME_BIGPOS(a);
  b_pos = SCHEME_BIGPOS(b);
  
  if (!a_pos && b_pos)
    return 1;
  else if (a_pos && !b_pos)
    return 0;
  else 
    res = bignum_abs_cmp(a, b);
  if (!a_pos)
    return (res > 0);
  else
    return (res < 0);
}

int scheme_bignum_gt(const Scheme_Object *a, const Scheme_Object *b)
{
  return scheme_bignum_lt(b, a);
}

int scheme_bignum_le(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_bignum_gt(a, b);
}

int scheme_bignum_ge(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_bignum_lt(a, b);
}

Scheme_Object *scheme_bignum_negate(const Scheme_Object *n)
{
  Scheme_Object *o;
  int len;

  len = SCHEME_BIGLEN(n);

  if (SCHEME_BIGDIG(n) == ((Small_Bignum *)n)->v) {
    /* Can't share bigdig array when n is a Small_Bignum */
    o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Small_Bignum) + ((len - 1) * sizeof(bigdig)));
#if MZ_PRECISE_GC
    ((Scheme_Bignum *)o)->allocated_inline = len;
#endif  
    ((Small_Bignum *)o)->v[0] = SCHEME_BIGDIG(n)[0];
    if (len > 1)
      ((Small_Bignum *)o)->v[1] = SCHEME_BIGDIG(n)[1];
    SCHEME_BIGDIG(o) = ((Small_Bignum *)o)->v;
  } else {
    o = (Scheme_Object *)MALLOC_ONE_TAGGED(Scheme_Bignum);
    SCHEME_BIGDIG(o) = SCHEME_BIGDIG(n);
  }

  o->type = scheme_bignum_type;
  SCHEME_BIGPOS(o) = !SCHEME_BIGPOS(n);
  SCHEME_BIGLEN(o) = len;
 
  return o;
}

static bigdig* allocate_bigdig_array(int length)
{
  int i;
  bigdig* res;
  res = (bigdig *)scheme_malloc_atomic(length * sizeof(bigdig));
  for(i = 0; i < length; ++i) {
    res[i] = 0;
  }
  return res;
}

/* We don't want to count leading digits of 0 in the bignum's length */
static int bigdig_length(bigdig* array, int alloced)
{
  alloced--;
  while (alloced >= 0 && array[alloced] == 0) {
    alloced--;
  }
  return alloced + 1;
}

/* if (sub) a - b else a + b */
Scheme_Object *bignum_add_sub(const Scheme_Object *a, const Scheme_Object *b, int sub)
{
  Scheme_Object *o;
  long a_size, a_pos, b_size, b_pos, max_size;
  bigdig *o_digs, *a_digs, *b_digs;

  a_size = SCHEME_BIGLEN(a);
  b_size = SCHEME_BIGLEN(b);
  a_pos = SCHEME_BIGPOS(a);
  b_pos = xor(SCHEME_BIGPOS(b), sub);
  if (b_size == 0)
    return scheme_bignum_normalize(bignum_copy(a, 0));
  else if (a_size == 0)
  {
    o = bignum_copy(b, 0);
    SCHEME_BIGPOS(o) = b_pos;
    return scheme_bignum_normalize(o);
  }
  
  o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  o->type = scheme_bignum_type;
  
  o_digs = NULL; /* Get rid of erroneous gcc warning */
  
  max_size = (a_size > b_size) ? a_size : b_size;

  if (a_pos == b_pos) /* addition */
  {
    int carry;

    o_digs = allocate_bigdig_array(max_size);
    a_digs = SCHEME_BIGDIG(a);
    b_digs = SCHEME_BIGDIG(b);

    if (a_size > b_size)
      carry = mpn_add(o_digs, a_digs, a_size, b_digs, b_size);
    else
      carry = mpn_add(o_digs, b_digs, b_size, a_digs, a_size);

    a_digs = NULL; /* Precise GC - might be misaligned */
    b_digs = NULL;

    SCHEME_BIGPOS(o) = a_pos;
    SCHEME_BIGLEN(o) = max_size;
    SCHEME_BIGDIG(o) = o_digs;
    if (carry)
      o = bignum_copy(o, 1);
  }
  else /* subtraction */
  {
    int sw;
    if (a_size > b_size)
      sw = 0;
    else if (b_size > a_size)
      sw = 1;
    else
    {
      int cmp;
      cmp = mpn_cmp(SCHEME_BIGDIG(a), SCHEME_BIGDIG(b), a_size);
      if (cmp == 0)
	return scheme_make_integer(0);
      else if (cmp > 0) /* a > b */
	sw = 0;
      else
	sw = 1;
    }
    o_digs = allocate_bigdig_array(max_size);

    a_digs = SCHEME_BIGDIG(a);
    b_digs = SCHEME_BIGDIG(b);

    if (sw)
      mpn_sub(o_digs, b_digs, b_size, a_digs, a_size);
    else
      mpn_sub(o_digs, a_digs, a_size, b_digs, b_size);
    
    a_digs = NULL; /* Precise GC - might be misaligned */
    b_digs = NULL;

    SCHEME_BIGPOS(o) = xor(sw, a_pos);
    SCHEME_BIGLEN(o) = bigdig_length(o_digs, max_size);
    SCHEME_BIGDIG(o) = o_digs;
  }
  return scheme_bignum_normalize(o);
}

Scheme_Object *scheme_bignum_add(const Scheme_Object *a, const Scheme_Object *b)
{
  return bignum_add_sub(a, b, 0);
}

Scheme_Object *scheme_bignum_subtract(const Scheme_Object *a, const Scheme_Object *b)
{
  return bignum_add_sub(a, b, 1);
}

Scheme_Object *scheme_bignum_add1(const Scheme_Object *n)
{
  if (!bignum_one) {
    REGISTER_SO(bignum_one);
    bignum_one = scheme_make_bignum(1);
  }

  return bignum_add_sub(n, bignum_one, 0);
}

Scheme_Object *scheme_bignum_sub1(const Scheme_Object *n)
{
  if (!bignum_one) {
    REGISTER_SO(bignum_one);
    bignum_one = scheme_make_bignum(1);
  }

  return bignum_add_sub(n, bignum_one, 1);
}

/* norm determines if we normalize the result */
static Scheme_Object *bignum_multiply(const Scheme_Object *a, const Scheme_Object *b, int norm)
{
  Scheme_Object *o;
  long a_size, a_pos, b_size, b_pos, res_size;
  bigdig* o_digs, *a_digs, *b_digs;
  
  a_size = SCHEME_BIGLEN(a);
  b_size = SCHEME_BIGLEN(b);

  if (a_size == 0 || b_size == 0)
  {
    if (norm)
      return scheme_make_integer(0);
    else
      return scheme_make_bignum(0);
  }

  a_pos = SCHEME_BIGPOS(a);
  b_pos = SCHEME_BIGPOS(b);
  
  res_size = a_size + b_size;
  
  o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  o->type = scheme_bignum_type;
  
  o_digs = allocate_bigdig_array(res_size);

  a_digs = SCHEME_BIGDIG(a);
  b_digs = SCHEME_BIGDIG(b);

  if (a_size > b_size)
    mpn_mul(o_digs, a_digs, a_size, b_digs, b_size);
  else
    mpn_mul(o_digs, b_digs, b_size, a_digs, a_size);

  a_digs = NULL; /* Precise GC - might be misaligned */
  b_digs = NULL;
    
  SCHEME_BIGLEN(o) = bigdig_length(o_digs, res_size);
  
  SCHEME_BIGDIG(o) = o_digs;

  SCHEME_BIGPOS(o) = !xor(a_pos, b_pos);

  return (norm ? scheme_bignum_normalize(o) : o);
}

Scheme_Object *scheme_bignum_multiply(const Scheme_Object *a, const Scheme_Object *b)
{
  return bignum_multiply(a, b, 1);
}

Scheme_Object *scheme_bignum_power(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Object *a_acc, *b_acc, *result, *two;
  
  a_acc = bignum_copy(a, 0);
  b_acc = bignum_copy(b, 0);
  result = scheme_make_bignum(1);
  two = scheme_make_bignum(2);

  while (SCHEME_BIGLEN(b_acc) > 0)
  {
    if (SCHEME_BIGDIG(b_acc)[0] & 0x1) /* if (odd?) */
      result = bignum_multiply(a_acc, result, 0);
    a_acc = bignum_multiply(a_acc, a_acc, 0);
    scheme_bignum_divide(b_acc, two, &b_acc, NULL, 0);
  }
  return scheme_bignum_normalize(result);  
}

Scheme_Object *scheme_bignum_max(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_bignum_lt(a, b);
  return scheme_bignum_normalize(lt ? b : a);
}

Scheme_Object *scheme_bignum_min(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_bignum_lt(a, b);
  return scheme_bignum_normalize(lt ? a : b);
}

/* op = 0 : &
   op = 1 : |
   op = 2 : ^
assumes len a >= len b */
static Scheme_Object *do_bitop(const Scheme_Object *a, const Scheme_Object *b, int op)
{
  long a_size, b_size, a_pos, b_pos, res_alloc, res_pos, i;
  bigdig* a_digs, *b_digs, *res_digs;
  int carry_out_a, carry_out_b, carry_out_res, carry_in_a, carry_in_b, carry_in_res;
  Scheme_Object* o;

  a_size = SCHEME_BIGLEN(a);
  b_size = SCHEME_BIGLEN(b);

  if (a_size == 0) /* b_size == 0 too */
  {
    return scheme_make_integer(0); /* for all 3 ops */
  }
  else if (b_size == 0)
  {
    if (op == 0)
      return scheme_make_integer(0);
    else
      return scheme_bignum_normalize(bignum_copy(a, 0));
  }

  a_pos = SCHEME_BIGPOS(a);
  b_pos = SCHEME_BIGPOS(b);
 
  if (op == 0)
  {
    res_pos = a_pos || b_pos;
    res_alloc = (b_pos ? b_size : a_size);
  }
  else if (op == 1)
  {
    res_pos = a_pos && b_pos;
    res_alloc = (b_pos ? a_size : b_size);
  }
  else
  {
    res_pos = !xor(a_pos, b_pos);
    res_alloc = a_size;
  }
  
  res_digs = allocate_bigdig_array(res_alloc);
  
  a_digs = SCHEME_BIGDIG(a); /* Price GC - may be mis-aligned! */
  b_digs = SCHEME_BIGDIG(b);

  carry_out_a = carry_out_b = carry_out_res = 1;  
  carry_in_a = carry_in_b = carry_in_res = 0;  
  
  for (i = 0; i < res_alloc; ++i)
  {
    long a_val, b_val, res_val;

    a_val = a_digs[i];
    if (!a_pos)
    {
      /* We have to do te operation on the 2's complement of a */
      carry_in_a = carry_out_a;
      carry_out_a = (carry_in_a == 1 && a_val == 0) ? 1 : 0;
      a_val = ~a_val + carry_in_a;
    }

    if (i < b_size)
    {
      b_val = b_digs[i];
      if (!b_pos)
      {
	carry_in_b = carry_out_b;
	carry_out_b = (carry_in_b == 1 && b_val == 0) ? 1 : 0;
	b_val = ~b_val + carry_in_b;
      }
    }
    else
    {
      if (b_pos)
	b_val = 0;
      else
	b_val = ALL_ONES;
    }

    if (op == 0)
      res_val = a_val & b_val;
    else if (op == 1)
      res_val = a_val | b_val;
    else
      res_val = a_val ^ b_val;

    if (!res_pos)
    {
      carry_in_res = carry_out_res;
      carry_out_res = (carry_in_res == 1 && res_val == 0) ? 1 : 0;
      res_val = ~res_val + carry_in_res;
    }
    
    res_digs[i] = res_val;
  }

  a_digs = NULL; /* Precise GC - might be misaligned */
  b_digs = NULL;

  o = (Scheme_Object*)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  o->type = scheme_bignum_type;
  SCHEME_BIGPOS(o) = res_pos;
  if (!res_pos && carry_out_res == 1) /* Overflow */
  {
    res_digs = allocate_bigdig_array(res_alloc + 1);
    res_digs[res_alloc] = 1;
    SCHEME_BIGLEN(o) = bigdig_length(res_digs, res_alloc + 1);
  }
  else
    SCHEME_BIGLEN(o) = bigdig_length(res_digs, res_alloc);

  SCHEME_BIGDIG(o) = res_digs;

  return scheme_bignum_normalize(o);
}

Scheme_Object *scheme_bignum_and(const Scheme_Object *a, const Scheme_Object *b)
{
  if (SCHEME_BIGLEN(a) > SCHEME_BIGLEN(b))
    return do_bitop(a, b, 0);
  else
    return do_bitop(b, a, 0);    
}

Scheme_Object *scheme_bignum_or(const Scheme_Object *a, const Scheme_Object *b)
{
  if (SCHEME_BIGLEN(a) > SCHEME_BIGLEN(b))
    return do_bitop(a, b, 1);
  else
    return do_bitop(b, a, 1);    
}

Scheme_Object *scheme_bignum_xor(const Scheme_Object *a, const Scheme_Object *b)
{
   if (SCHEME_BIGLEN(a) > SCHEME_BIGLEN(b))
    return do_bitop(a, b, 2);
  else
    return do_bitop(b, a, 2);    
}

Scheme_Object *scheme_bignum_not(const Scheme_Object *a)
{
  Scheme_Object *o;

  o = scheme_bignum_add1(a);

  SCHEME_BIGPOS(o) = !SCHEME_BIGPOS(a);
  return scheme_bignum_normalize(o);
}

Scheme_Object *scheme_bignum_shift(const Scheme_Object *n, long shift)
{
  Scheme_Object* o;
  bigdig* res_digs, *n_digs;
  long res_alloc, shift_words, shift_bits, i, j, n_size, shift_out;

  n_size = SCHEME_BIGLEN(n);
  if (n_size == 0)
    return scheme_make_integer(0);
  if (shift == 0) /* no shift */
    return scheme_bignum_normalize(bignum_copy(n, 0));

  o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  o->type = scheme_bignum_type;

  if (shift < 0) /* right shift */
  {
    int shifted_off_one = 0;
    
    shift = -shift;
    shift_words = shift / WORD_SIZE;
    shift_bits = shift % WORD_SIZE;

    if (shift_words >= n_size) {
      if (SCHEME_BIGPOS(n))
	return scheme_make_integer(0);
      else
	return scheme_make_integer(-1);
    }

    res_alloc = n_size - shift_words;
    if (shift_bits == 0 && !SCHEME_BIGPOS(n))
      res_alloc++;   /* Very unlikely event of a carryout on the later add1 increasing the word size */
    res_digs = allocate_bigdig_array(res_alloc);
    
    n_digs = SCHEME_BIGDIG(n); /* Precise GC - might be mis-aligned */

    if (!SCHEME_BIGPOS(n)) {
      for(i = 0; i < shift_words; ++i) {
	if (n_digs[i] != 0) {
	  shifted_off_one = 1;
	  break;  
	}  
      }
    }

    for(i = 0, j = shift_words; j < n_size; ++i, ++j) {
      res_digs[i] = n_digs[j];
    }

    n_digs = NULL; /* Precise GC - might be mis-aligned */

    shift_out = mpn_rshift(res_digs, res_digs, res_alloc, shift_bits);

    SCHEME_BIGPOS(o) = SCHEME_BIGPOS(n);
    if (!SCHEME_BIGPOS(n) && (shifted_off_one || !shift_out == 0)) {
      mpn_add_1(res_digs, res_digs, res_alloc, 1);
    }
    SCHEME_BIGLEN(o) = bigdig_length(res_digs, res_alloc);
    SCHEME_BIGDIG(o) = res_digs;
  }
  else /* left shift */
  {
    shift_words = shift / WORD_SIZE;
    shift_bits = shift % WORD_SIZE;
    res_alloc = SCHEME_BIGLEN(n) + shift_words;
    if (shift_bits != 0)
      ++res_alloc;
    res_digs = allocate_bigdig_array(res_alloc);
    
    n_digs = SCHEME_BIGDIG(n); /* Precise GC - might be mis-aligned */

    for(i = 0, j = shift_words; i < SCHEME_BIGLEN(n); ++i, ++j) {
      res_digs[j] = n_digs[i];
    }

    n_digs = NULL; /* Precise GC - might be mis-aligned */

    if (shift_bits != 0)
      mpn_lshift(res_digs + shift_words, res_digs + shift_words, res_alloc - shift_words, shift_bits);

  }

  SCHEME_BIGDIG(o) = res_digs;
  SCHEME_BIGLEN(o) = bigdig_length(res_digs, res_alloc);
  SCHEME_BIGPOS(o) = SCHEME_BIGPOS(n);
  return scheme_bignum_normalize(o);
}


char *scheme_bignum_to_string(const Scheme_Object *b, int radix)
{
  
  Scheme_Object *c;
  unsigned char* str, *str2;
  int i, slen, start;

  if (radix != 10 && radix != 2 && radix != 8 && radix != 16)
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, scheme_make_integer(radix),
		     "bad bignum radix (%d)", radix);

  if (SCHEME_BIGLEN(b) == 0)
    return "0";

  c = bignum_copy(b, 1);  /* mpn_get_string may need a word of scratch space */

  if (radix == 2)
    slen = WORD_SIZE * SCHEME_BIGLEN(b) + 2;
  else if (radix == 8)
    slen = ceil(WORD_SIZE * SCHEME_BIGLEN(b) / 3.0) + 2;
  else if (radix == 16)
    slen = WORD_SIZE * SCHEME_BIGLEN(b) / 4 + 2;
  else /* (radix == 10) */
    slen = ceil(WORD_SIZE * SCHEME_BIGLEN(b) * 0.30102999566398115) + 1;

  str = (char*)scheme_malloc_atomic(sizeof(unsigned char) * slen);
  
  slen = mpn_get_str(str, radix, SCHEME_BIGDIG(c), SCHEME_BIGLEN(c) - 1);

  i = 0;
  while (i < slen && str[i] == 0) {
    ++i;
  }
  
  if (i == slen)
    return "0";
  else
    slen = slen - i + 1 + (SCHEME_BIGPOS(b) ? 0 : 1);

  str2 = (char*)scheme_malloc_atomic(sizeof(unsigned char) * slen);

  start = i;

  if (!(SCHEME_BIGPOS(b))) {
    i = 1;
    start--;
    str2[0] = '-';
  } else
    i = 0;
  
  for (; i < slen - 1; ++i) {
    if (str[i + start] < 10)
      str2[i] = str[i + start] + '0';
    else
      str2[i] = str[i + start] + 'a' - 10;
  }
  
  str2[slen - 1] = 0;

  return str2;
}

Scheme_Object *scheme_read_bignum(const char *str, int offset, int radix)
{
  int len, negate, stri, alloc, i, test;
  Scheme_Object* o;
  bigdig* digs;
  char* istring;

  if (radix < 0 || radix > 16) {
    return scheme_false;
  }
  
  negate = 0;
  stri = offset;
  while ((str[stri] == '+') || (str[stri] == '-')) {
    if (str[stri] == '-')
      negate = !negate;
    stri++;
  }
  len = strlen(str + stri);

  if (len == 0)
    return scheme_false;
  
  istring = (char*)scheme_malloc_atomic(sizeof(char) * len);

  i = stri;
  while(str[i] != 0) {
    if (str[i] >= '0' && str[i] <= '9')
      istring[i - stri] = str[i] - '0';
    else if (str[i] >= 'a' && str[i] <= 'z')
      istring[i - stri] = str[i] - 'a' + 10;
    else if (str[i] >= 'A' && str[i] <= 'Z')
      istring[i - stri] = str[i] - 'A' + 10;
    else
      return scheme_false;

    if (istring[i - stri] >= radix || istring[i - stri] < 0)
      return scheme_false;    
    ++i;
  }

  o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  o->type = scheme_bignum_type;

  alloc = ceil(len * log(radix) / (32 * log(2)));

  digs = allocate_bigdig_array(alloc);

  SCHEME_BIGPOS(o) = !negate;

  test = mpn_set_str(digs, istring, len, radix);

  if (test > alloc)
    printf("error\n");

  SCHEME_BIGLEN(o) = bigdig_length(digs, alloc);
  SCHEME_BIGDIG(o) = digs;

  return scheme_bignum_normalize(o);

}

#define USE_FLOAT_BITS 51
#define FP_TYPE double
#define IS_FLOAT_INF is_double_inf
#define SCHEME_BIGNUM_TO_FLOAT_INFO scheme_bignum_to_double_inf_info
#define SCHEME_BIGNUM_TO_FLOAT scheme_bignum_to_double
#define SCHEME_CHECK_FLOAT scheme_check_double
#define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_double
#include "bgnfloat.inc"

#ifdef MZ_USE_SINGLE_FLOATS
# undef USE_FLOAT_BITS
# undef FP_TYPE
# undef IS_FLOAT_INF
# undef SCHEME_BIGNUM_TO_FLOAT_INFO
# undef SCHEME_BIGNUM_TO_FLOAT
# undef SCHEME_CHECK_FLOAT
# undef SCHEME_BIGNUM_FROM_FLOAT

# define USE_FLOAT_BITS 22
# define FP_TYPE float
# define IS_FLOAT_INF is_float_inf
# define SCHEME_BIGNUM_TO_FLOAT_INFO scheme_bignum_to_float_inf_info
# define SCHEME_BIGNUM_TO_FLOAT scheme_bignum_to_float
# define SCHEME_CHECK_FLOAT scheme_check_float
# define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_float
# include "bgnfloat.inc"
#endif


void scheme_bignum_divide(const Scheme_Object *n, const Scheme_Object *d,
			  Scheme_Object **qp, Scheme_Object **rp, int norm)
{
  int cmp;
  
  cmp = bignum_abs_cmp(n, d);

  if (cmp == -1) {
    if (qp)
      *qp = (norm ? scheme_make_integer(0) : scheme_make_bignum(0));
    if (rp)
      *rp = (norm ? scheme_bignum_normalize(bignum_copy(n, 0)) : bignum_copy(n, 0));
    return;
  } else if (cmp == 0) {
    int n_pos, d_pos, res;

    n_pos = SCHEME_BIGPOS(n);
    d_pos = SCHEME_BIGPOS(d);

    res = (xor(n_pos, d_pos) ? -1 : 1);

    if (qp)
      *qp = (norm ? scheme_make_integer(res) : scheme_make_bignum(res));
    if (rp)
      *rp = (norm ? scheme_make_integer(0) : scheme_make_bignum(0));
    return;
  } else {    
    long n_size, d_size, q_alloc, r_alloc, n_pos, d_pos;
    bigdig *q_digs, *r_digs;
    Scheme_Object *q, *r;

    n_size = SCHEME_BIGLEN(n);
    d_size = SCHEME_BIGLEN(d);

    q = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
    r = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
    q->type = scheme_bignum_type;
    r->type = scheme_bignum_type;
    
    q_alloc = n_size - d_size + 1;
    r_alloc = d_size;

    q_digs = allocate_bigdig_array(q_alloc);
    r_digs = allocate_bigdig_array(r_alloc);


    mpn_tdiv_qr(q_digs, r_digs, 0, SCHEME_BIGDIG(n), n_size,
		SCHEME_BIGDIG(d), d_size);
    
    n_pos = SCHEME_BIGPOS(n);
    d_pos = SCHEME_BIGPOS(d);
    
    if (rp) {
      SCHEME_BIGDIG(r) = r_digs;
      SCHEME_BIGLEN(r) = bigdig_length(r_digs, r_alloc);
      SCHEME_BIGPOS(r) = n_pos;
      *rp = (norm ? scheme_bignum_normalize(r) : r);
    } if (qp) {
      SCHEME_BIGDIG(q) = q_digs;
      SCHEME_BIGLEN(q) = bigdig_length(q_digs, q_alloc);
      SCHEME_BIGPOS(q) = !xor(n_pos, d_pos);
      *qp = (norm ? scheme_bignum_normalize(q) : q);
    }
  }
}


Scheme_Object *scheme_integer_sqrt(const Scheme_Object *n)
{
  Scheme_Object *o;
  long n_size, res_alloc;
  bigdig *res_digs, *sqr_digs, tmp[1];
  int res;

  if (SCHEME_INTP(n)) {
    long t = SCHEME_INT_VAL(n);
    if (t == 0)
      return scheme_make_integer(0);
    n_size = 1;
    sqr_digs = tmp;
    sqr_digs[0] = t;
  } else {
    n_size = SCHEME_BIGLEN(n);
    if (n_size == 0)
      return scheme_make_integer(0);
    sqr_digs = NULL; /* set below */
  }

  if (n_size & 0x1)
    res_alloc = (n_size + 1) >> 1;
  else
    res_alloc = n_size >> 1;
  res_digs = allocate_bigdig_array(res_alloc);
  
  if (!sqr_digs)
    sqr_digs = SCHEME_BIGDIG(n); /* Precise GC - might be mis-aligned */

  res = mpn_sqrtrem(res_digs, NULL, sqr_digs, n_size);

  sqr_digs = NULL; /* Precise GC - might be mis-aligned */

  if (!res) {
    /* An integer result */
    o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
    o->type = scheme_bignum_type;
    SCHEME_BIGLEN(o) = bigdig_length(res_digs, res_alloc);
    SCHEME_BIGDIG(o) = res_digs;
    SCHEME_BIGPOS(o) = 1;
    return scheme_bignum_normalize(o);
  } else {
    double v;
    
    if (SCHEME_INTP(n))
      v = (double)SCHEME_INT_VAL(n);
    else {
      v = scheme_bignum_to_float(n);
      
      if (MZ_IS_POS_INFINITY(v))
	return scheme_make_double(v);
    }
    
    return scheme_make_double(sqrt(v));
  }
}

