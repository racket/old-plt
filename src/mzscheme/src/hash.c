/*
  MzScheme
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
#include "schmach.h"
#include <string.h>
#include <ctype.h>

#ifdef MZ_PRECISE_GC
# define PTR_TO_LONG(p) scheme_hash_key(p)
#else
# ifdef DOS_MEMORY
#  include <dos.h>
#  define PTR_TO_LONG(p) ((FP_SEG(p) << 4) + FP_OFF(p))
# else
#  define PTR_TO_LONG(p) ((long)(p))
# endif
#endif

#ifdef SMALL_HASH_TABLES
#define FILL_FACTOR 1.30
#else
#define FILL_FACTOR 2
#endif

#define MIN_HTABLE_SIZE 7

long scheme_hash_primes[] = 
{MIN_HTABLE_SIZE, 31, 127, 257, 521, 1031, 2053, 4099, 8209, 16411, 
   32779, 65543, 131101, 262147, 425329, 1048583, 2097169,
   4194319, 8388617, 16777259, 33554467, 67108879, 134217757,
   268435459, 536870923, 1073741827};

typedef int (*Hash_Compare_Proc)(void*, void*);

typedef long hash_v_t;

/*========================================================================*/
/*                         hashing functions                              */
/*========================================================================*/

static void string_hash_indices(void *_key, long *_h, long *_h2)
{
  const char *key = (char *)_key;
  long i, h, h2;

  h2 = h = i = 0;
  while (key[i]) {
    int c = key[i++];
    if (!scheme_case_sensitive)
      if (isupper(c))
	c = tolower(c);
    h += (h << 5) + h + c;
    h2 += c;
  }

  *_h = h;
  *_h2 = h2;
}

#ifdef PALMOS_STUFF
static int p_strcmp(char *a, char *b)
{
  return strcmp(a, b);
}
#endif

static void id_hash_indices(void *_key, long *_h, long *_h2)
{
  Scheme_Object *key = (Scheme_Object *)_key;
  long lkey;

  if (SCHEME_STXP(key))
    key = SCHEME_STX_VAL(key);
    
  lkey = PTR_TO_LONG((Scheme_Object *)key);
  *_h = (lkey >> 2);
  *_h2 = (lkey >> 3);
}

static int not_stx_bound_eq(char *a, char *b)
{
  return !scheme_stx_bound_eq((Scheme_Object *)a, (Scheme_Object *)b, 0);
}

/*========================================================================*/
/*                         normal hash table                              */
/*========================================================================*/

static Scheme_Object GONE[1];

Scheme_Hash_Table *scheme_make_hash_table(int type)
{
  Scheme_Hash_Table *table;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);

  table->step = 0;
  table->size = 0;
    
  table->type = scheme_hash_table_type;

  if (type == SCHEME_hash_string) {
    table->make_hash_indices = string_hash_indices;
#ifdef PALMOS_STUFF
    table->compare = (Hash_Compare_Proc)p_strcmp;
#else
    table->compare = (Hash_Compare_Proc)strcmp;
#endif
  }
  if (type == SCHEME_hash_bound_id) {
    table->make_hash_indices = id_hash_indices;
    table->compare = (Hash_Compare_Proc)not_stx_bound_eq;
  }

  return table;
}

static Scheme_Object *do_hash(Scheme_Hash_Table *table, Scheme_Object *key, int set, Scheme_Object *val)
{
  Scheme_Object *tkey, **keys;
  hash_v_t h, h2, useme = 0;
  long size = table->size;

 rehash_key:

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, &h, &h2);
    h = h % size;
    h2 = h2 % size;
  } else {
    long lkey;
    lkey = PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) % size;
    h2 = (lkey >> 3) % size;
  }

  if (h < 0) h = -h;
  if (h2 < 0) h2 = -h2;
  
  if (!h2)
    h2 = 2;
  else if (h2 & 0x1)
    h2++; /* note: table size is never even, so no % needed */

  keys = table->keys;
  
  if (table->compare) {
    while ((tkey = keys[h])) {
      if (SAME_PTR(tkey, GONE)) {
	if (set > 1) {
	  useme = h;
	  set = 1;
	}
      } else if (!table->compare(tkey, (char *)key)) {
	if (set) {
	  table->vals[h] = val;
	  if (!val)
	    keys[h] = GONE;
	  return val;
	} else
	  return table->vals[h];
      }
      h = (h + h2) % size;
    }
  } else {
    while ((tkey = keys[h])) {
      if (SAME_PTR(tkey, key)) {
	if (set) {
	  table->vals[h] = val;
	  if (!val)
	    keys[h] = GONE;
	  return val;
	} else
	  return table->vals[h];
      } else if (SAME_PTR(tkey, GONE)) {
	if (set && (useme < 0)) {
	  useme = h;
	}
      } 
      h = (h + h2) % size;
    }
  }

  if (!set || !val)
    return NULL;

  if (set == 1) {
    h = useme;
    --table->count; /* counter increment below */
  } else if (table->count * FILL_FACTOR >= size) {
    /* Rehash */
    int i, oldsize = table->size;
    Scheme_Object **oldkeys = table->keys;
    Scheme_Object **oldvals = table->vals;

    table->size = scheme_hash_primes[++table->step];
    size = table->size;
    
    {
      Scheme_Object **ba;
      ba = MALLOC_N(Scheme_Object *, size);
      table->vals = ba;
      ba = MALLOC_N(Scheme_Object *, size);
      table->keys = ba;
    }

    table->count = 0;
    for (i = 0; i < oldsize; i++) {
      if (oldkeys[i] && !SAME_PTR(oldkeys[i], GONE))
	do_hash(table, oldkeys[i], 2, oldvals[i]);
    }

    goto rehash_key;
  }

  table->keys[h] = key;
  table->vals[h] = val;

  table->count++;

  return val;
}

void scheme_hash_set(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
{
  if (!table->vals) {
    Scheme_Object **ba;

    table->size = scheme_hash_primes[0];

    ba = MALLOC_N(Scheme_Object *, table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->keys = ba;
  }

  do_hash(table, key, 2, val);
}

Scheme_Object *scheme_hash_get(Scheme_Hash_Table *table, Scheme_Object *key)
{
  Scheme_Object *val;

  if (!table->vals)
    val = NULL;
  else
    val = do_hash(table, key, 0, NULL);

  return val;
}

/*========================================================================*/
/*                  old-style hash table, with buckets                    */
/*========================================================================*/

Scheme_Bucket_Table *
scheme_make_bucket_table (int size, int type)
{
  Scheme_Bucket_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Bucket_Table);

  table->step = 0;
  while (scheme_hash_primes[table->step] < size) {
    table->step++;
  }
  table->size = scheme_hash_primes[table->step];

  table->count = 0;

  table->type = scheme_bucket_table_type;

  asize = (size_t)table->size * sizeof(Scheme_Bucket *);
  {
    Scheme_Bucket **ba;
    ba = (Scheme_Bucket **)scheme_malloc(asize);
    table->buckets = ba;
  }

  table->weak = (type == SCHEME_hash_weak_ptr);
  
  return table;
}

static Scheme_Bucket *
get_bucket (Scheme_Bucket_Table *table, const char *key, int add, Scheme_Bucket *b)
{
  hash_v_t h, h2;
  Scheme_Bucket *bucket;
  Compare_Proc compare = table->compare;


 rehash_key:

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, &h, &h2);
    h = h % table->size;
    h2 = h2 % table->size;
  } else {
    long lkey;
    lkey = PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) % table->size;
    h2 = (lkey >> 3) % table->size;
  }

  if (h < 0) h = -h;
  if (h2 < 0) h2 = -h2;
  
  if (!h2)
    h2 = 2;
  else if (h2 & 0x1)
    h2++;

  if (table->weak) {
    while ((bucket = table->buckets[h])) {
      if (bucket->key) {
	void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
	if (!hk) {
	  if (add) {
	    /* Re-use a bucket slot whose key is collected: */
	    /* DON'T increment counter overall... */
	    --table->count;
	    break;
	  }
	} else if (SAME_PTR(hk, key))
	  return bucket;
	else if (compare && !compare((void *)hk, (void *)key))
	  return bucket;
      } else if (add)
	break;
      h = (h + h2) % table->size;
    }
  } else {
    while ((bucket = table->buckets[h])) {
      if (SAME_PTR(bucket->key, key))
	return bucket;
      else if (compare && !compare((void *)bucket->key, (void *)key))
	return bucket;
      h = (h + h2) % table->size;
    }
  }

  if (!add)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size;
    size_t asize;
    Scheme_Bucket **old = table->buckets;

    if (table->weak && (table->size > 4096)) {
      int actual = 0;

      /* Forced GC: so that the new table is as small as possible. */
      scheme_collect_garbage();

      /* Check actual count: */
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key)) {
	  actual++;
	}
      }

      if (actual * FILL_FACTOR < table->count) {
	/* Decrement step so that the table won't actually grow. */
	--table->step;
      }
    }

    table->size = scheme_hash_primes[++table->step];
    
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
    {
      Scheme_Bucket **ba;
      ba = (Scheme_Bucket **)scheme_malloc(asize);
      table->buckets = ba;
    }

    table->count = 0;
    if (table->weak) {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key))
	  get_bucket(table, (char *)HT_EXTRACT_WEAK(old[i]->key), 1, old[i]);
      }
    } else {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key)
	  get_bucket(table, old[i]->key, 1, old[i]);
      }
    }

    goto rehash_key;
  }

  if (b) {
    bucket = b;
  } else {
    size_t bsize;
    Scheme_Type type;

    if (table->with_home) {
      bsize = sizeof(Scheme_Bucket_With_Home);
      type = scheme_variable_type;
    } else  {
      bsize = sizeof(Scheme_Bucket);
      type = scheme_bucket_type;
    }

    bucket = (Scheme_Bucket *)scheme_malloc_tagged(bsize);

    bucket->type = type;

    if (type == scheme_variable_type)
      ((Scheme_Bucket_With_Flags *)bucket)->flags = GLOB_HAS_HOME_PTR;

    if (table->weak) {
#ifdef MZ_PRECISE_GC
      void *kb;
      kb = GC_malloc_weak_box((void *)key, (void **)bucket, (void **)&bucket->val - (void **)bucket);
      bucket->key = (char *)kb;
#else
      char *kb;
      kb = (char *)MALLOC_ONE_WEAK(void *);
      bucket->key = kb;
      *(void **)bucket->key = (void *)key;
      scheme_weak_reference_indirect((void **)bucket->key, (void *)key);
      scheme_weak_reference_indirect((void **)&bucket->val, (void *)key);
#endif
    } else
      bucket->key = (char *)key;
    bucket->val = NULL;
  }

  table->buckets[h] = bucket;

  table->count++;

  return bucket;
}

Scheme_Bucket *
scheme_bucket_or_null_from_table (Scheme_Bucket_Table *table, const char *key, int add)
{
  Scheme_Bucket *b;

  b = get_bucket(table, key, add, NULL);

  return b;
}

Scheme_Bucket *
scheme_bucket_from_table (Scheme_Bucket_Table *table, const char *key)
{
  return scheme_bucket_or_null_from_table(table, key, 1);
}

void 
scheme_add_to_table (Scheme_Bucket_Table *table, const char *key, void *val, 
		     int constant)
{
  Scheme_Bucket *b;

  b = get_bucket(table, key, 1, NULL);

  if (val)
    b->val = val;
  if (constant && table->with_home)
    ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_CONST;
}

void scheme_add_bucket_to_table(Scheme_Bucket_Table *table, Scheme_Bucket *b)
{
  get_bucket(table, table->weak ? (char *)HT_EXTRACT_WEAK(b->key) : b->key, 1, b);
}

void *
scheme_lookup_in_table (Scheme_Bucket_Table *table, const char *key)
{
  Scheme_Bucket *bucket;

  bucket = get_bucket(table, key, 0, NULL);

  if (bucket)
    return bucket->val;
  else
    return NULL;
}

void
scheme_change_in_table (Scheme_Bucket_Table *table, const char *key, void *naya)
{
  Scheme_Bucket *bucket;

  bucket = get_bucket(table, key, 0, NULL);

  if (bucket)
    bucket->val = naya;
}

/*========================================================================*/
/*                         precise GC hashing                             */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

typedef long (*Hash_Key_Proc)(Scheme_Object *o);
Hash_Key_Proc hash_key_procs[_scheme_last_normal_type_];
static short keygen;

static long hash_addr(Scheme_Object *o)
{
  return (long)o;
}

static long hash_general(Scheme_Object *o)
{
  if (!(((short *)o)[1] & 0xFFFC)) {
    if (!keygen)
      keygen += 4;
    ((short *)o)[1] |= keygen;
    keygen += 4;
  }

  return *(long *)o;
}

static long hash_prim(Scheme_Object *o)
{
  return (long)((Scheme_Primitive_Proc *)o)->prim_val;
}

static long hash_case(Scheme_Object *o)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)o;

  if (cl->count)
    return scheme_hash_key(cl->array[0]);
  else
    return scheme_case_closure_type << 2;
}

static long hash_bignum(Scheme_Object *o)
{
  int i = SCHEME_BIGLEN(o);
  bigdig *d = SCHEME_BIGDIG(o);
  long k = 0;
  
  while (i--) {
    k += d[i];
  }
  
  return k;
}

void scheme_init_hash_key_procs(void)
{
#define PROC(t,f) hash_key_procs[t] = f
  PROC(scheme_prim_type, hash_prim);
  PROC(scheme_closed_prim_type, hash_prim);
  PROC(scheme_linked_closure_type, hash_general);
  PROC(scheme_case_closure_type, hash_case);
  PROC(scheme_cont_type, hash_general);
  PROC(scheme_escaping_cont_type, hash_general);
  PROC(scheme_char_type, hash_addr);
  PROC(scheme_bignum_type, hash_bignum);
  PROC(scheme_rational_type, hash_general);
  PROC(scheme_float_type, hash_general);
  PROC(scheme_double_type, hash_general);
  PROC(scheme_complex_izi_type, hash_general);
  PROC(scheme_complex_type, hash_general);
  PROC(scheme_string_type, hash_general);
  PROC(scheme_symbol_type, hash_general);
  PROC(scheme_null_type, hash_addr);
  PROC(scheme_pair_type, hash_general);
  PROC(scheme_vector_type, hash_general);
  PROC(scheme_input_port_type, hash_general);
  PROC(scheme_output_port_type, hash_general);
  PROC(scheme_eof_type, hash_addr);
  PROC(scheme_true_type, hash_addr);
  PROC(scheme_false_type, hash_addr);
  PROC(scheme_void_type, hash_addr);
  PROC(scheme_syntax_compiler_type, hash_general);
  PROC(scheme_macro_type, hash_general);
  PROC(scheme_box_type, hash_general);
  PROC(scheme_thread_type, hash_general);
  PROC(scheme_structure_type, hash_general);
  PROC(scheme_cont_mark_set_type, hash_general);
  PROC(scheme_sema_type, hash_general);
  PROC(scheme_hash_table_type, hash_general);
  PROC(scheme_bucket_table_type, hash_general);
  PROC(scheme_weak_box_type, hash_general);
  PROC(scheme_struct_type_type, hash_general);
  PROC(scheme_id_macro_type, hash_general);
  PROC(scheme_listener_type, hash_general);
  PROC(scheme_namespace_type, hash_general);
  PROC(scheme_config_type, hash_general);
  PROC(scheme_will_executor_type, hash_general);
  PROC(scheme_stx_type, hash_general);
  PROC(scheme_module_index_type, hash_general);
  PROC(scheme_custodian_type, hash_general);
  PROC(scheme_random_state_type, hash_general);
  PROC(scheme_regexp_type, hash_general);
  PROC(scheme_compilation_top_type, hash_general);
  PROC(scheme_placeholder_type, hash_general);
  PROC(scheme_inspector_type, hash_general);
  PROC(scheme_struct_property_type, hash_general);
  PROC(scheme_rename_table_type, hash_general);
  PROC(scheme_module_index_type, hash_general);
  PROC(scheme_module_variable_type, hash_general);
#undef PROC
}

long scheme_hash_key(Scheme_Object *o)
{
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return (long)o;

  t = SCHEME_TYPE(o);

  if (t >= _scheme_last_normal_type_) {
    return hash_general(o);
  } else {
#if 0
    if (!hash_key_procs[t]) {
      printf("Can't hash %d\n", t);
      abort();
    }
#endif
    
    return hash_key_procs[t](o);
  }
}

END_XFORM_SKIP;

#endif

/*========================================================================*/
/*                           equal? hashing                               */
/*========================================================================*/

static Scheme_Object *hash_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;
  
  return (Scheme_Object *)scheme_equal_hash_key(v);
}

/* Number of lists/vectors/structs/boxes to hash before
   paying for a stack check. */
#define HASH_COUNT_START 20

long scheme_equal_hash_key(Scheme_Object *o)
{
  Scheme_Type t;
  long k = 0;
  static int hash_counter = HASH_COUNT_START;

 top:
  t = SCHEME_TYPE(o);
  k += t;

  if (t == scheme_integer_type) {
    return k + SCHEME_INT_VAL(o);
#ifdef MZ_USE_SINGLE_FLOATS
  } else if (t == scheme_float_type) {
    float f;
    f = SCHEME_FLOAT_VAL(o);
    return k + *(long *)&f;
#endif
  } else if (t == scheme_double_type) {
    double d;
    d = SCHEME_DBL_VAL(o);
    return k + ((long *)&d)[0] + ((long *)&d)[1];
  } else if (t == scheme_bignum_type) {
    int i = SCHEME_BIGLEN(o);
    bigdig *d = SCHEME_BIGDIG(o);
    
    while (i--) {
      k += d[i];
    }
    
    return k;
  } else if (t == scheme_rational_type) {
    k += scheme_equal_hash_key(scheme_rational_numerator(o));
    o = scheme_rational_denominator(o);
  } else if ((t == scheme_complex_type) || (t == scheme_complex_izi_type)) {
    Scheme_Complex *c = (Scheme_Complex *)o;
    k += scheme_equal_hash_key(c->r);
    o = c->i;
  } else if (t == scheme_pair_type) {
#   include "mzhashchk.inc"
    k += scheme_equal_hash_key(SCHEME_CAR(o));
    o = SCHEME_CDR(o);
  } else if (t == scheme_vector_type) {
    int len = SCHEME_VEC_SIZE(o), i;
#   include "mzhashchk.inc"

    if (!len)
      return k + 1;
    
    --len;
    for (i = 0; i < len; i++) {
      SCHEME_USE_FUEL(1);
      k += scheme_equal_hash_key(SCHEME_VEC_ELS(o)[i]);
      k = (k << 1) + k;
    }
    
    o = SCHEME_VEC_ELS(o)[len];
  } else if (t == scheme_string_type) {
    int i = SCHEME_STRLEN_VAL(o);
    char *s = SCHEME_STR_VAL(o);
    
    while (i--) {
      k += s[i];
    }
    
    return k;
  } else  if (t == scheme_structure_type) {
    Scheme_Structure *s = (Scheme_Structure *)o;
    int len, i;
#   include "mzhashchk.inc"

    len = SCHEME_STRUCT_NUM_SLOTS(s);
    k += PTR_TO_LONG((Scheme_Object *)s->stype);

    if (!len)
      return k;

    --len;
    for (i = 0; i < len; i++) {
      SCHEME_USE_FUEL(1);
      k += scheme_equal_hash_key(s->slots[i]);
      k = (k << 1) + k;
    }
    
    o = s->slots[len];
  } else if (SCHEME_BOXP(o)) {
    SCHEME_USE_FUEL(1);
    k += 1;
    o = SCHEME_BOX_VAL(o);
  } else
    return k + (PTR_TO_LONG(o) >> 4);

  k = (k << 1) + k;
  goto top;
}

long scheme_equal_hash_key2(Scheme_Object *o)
{
  Scheme_Type t;

 top:
  t = SCHEME_TYPE(o);

  if (t == scheme_integer_type) {
    return t;
#ifdef MZ_USE_SINGLE_FLOATS
  } else if (t == scheme_float_type) {
    return t;
#endif
  } else if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
    return ((long *)&d)[1];
  } else if (t == scheme_bignum_type) {
    return SCHEME_BIGDIG(o)[0];
  } else if (t == scheme_rational_type) {
    return scheme_equal_hash_key(scheme_rational_numerator(o));
  } else if ((t == scheme_complex_type) || (t == scheme_complex_izi_type)) {
    Scheme_Complex *c = (Scheme_Complex *)o;
    return (scheme_equal_hash_key(c->r)
	    + scheme_equal_hash_key(c->i));
  } else if (t == scheme_pair_type) {
    return (scheme_equal_hash_key(SCHEME_CAR(o))
	    + scheme_equal_hash_key(SCHEME_CDR(o)));
  } else if (t == scheme_vector_type) {
    int len = SCHEME_VEC_SIZE(o), i;
    long k = 0;

    for (i = 0; i < len; i++) {
      SCHEME_USE_FUEL(1);
      k += scheme_equal_hash_key(SCHEME_VEC_ELS(o)[i]);
    }
    
    return k;
  } else if (t == scheme_string_type) {
    return SCHEME_STRLEN_VAL(o);
  } else  if (t == scheme_structure_type) {
    Scheme_Structure *s = (Scheme_Structure *)o;
    int len, i;
    long k = 0;

    len = SCHEME_STRUCT_NUM_SLOTS(s);

    for (i = 0; i < len; i++) {
      SCHEME_USE_FUEL(1);
      k += scheme_equal_hash_key(s->slots[i]);
    }
    
    return k;
  } else if (SCHEME_BOXP(o)) {
    o = SCHEME_BOX_VAL(o);
    goto top;
  } else
    return t;
}
