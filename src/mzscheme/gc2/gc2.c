/*
  Precise GC for MzScheme
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

#include <stdlib.h>
#include "gc2.h"
#include "../src/stypes.h"

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_custom_finalize)(void);
void (*GC_out_of_memory)(void);

void **GC_variable_stack;
int GC_variable_count;

Traverse_Proc tag_table[_scheme_last_type_];

void *alloc_space;
long alloc_size;
void **tagged_high, **atomic_low;

void GC_add_roots(void *start, void *end)
{
  
}

static unsigned long stack_base;

void GC_set_stack_base(void *base)
{
  stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base(void)
{
  return stack_base;
}

void GC_dump(void)
{
}

long GC_get_memory_use()
{
  return 0;
}

void GC_end_stubborn_change(void *s)
{
}

void GC_gcollect()
{
}

static void *identity(void *p)
{
  return p;
}

void gcollect(int needsize)
{
  /* Check old: */
  long *p, *top;

  p = (long *)atomic_low;
  top = (long *)(alloc_space + alloc_size);
  while (p < top) {
    p += *p + 1;
  }
  if (p == top) {
    printf("Atomic ok\n");
  }

  p = (long *)alloc_space;
  while (p < tagged_high) {
    if (!*p) {
      p++;
    } else if (*p & 0xC0000000) {
      int count = *p & 0x3FFFFFFF;
      p++;
      p += count;
    } else if (*p & 0x80000000) {
      size_t count = *p & 0x3FFFFFFF;
      Scheme_Type tag = *(Scheme_Type *)(p + 1);
      p++;
      p += tag_table[tag].Traverse(p, identity);
    } else {
      p += tag_table[tag].Traverse(p, identity);
    }
  }
  if (p == tagged_high) {
    printf("Tagged ok\n");
  }

  alloc_size = 32000;
  if (alloc_size < 2 * needsize)
    alloc_size = 2 * needsize;

  alloc_space = malloc(alloc_size);
  tagged_high = (void **)(alloc_space + 4);
  atomic_low = (void **)(alloc_space + alloc_size);

  memset(alloc_space, 0, alloc_size);
}

/* Array of pointers: */
static void *do_malloc(size_t size_in_bytes, int array)
{
  void **m, **naya;

  if (!size_in_bytes)
    return alloc_space;

  size_in_bytes = ((size_in_bytes + 3) & 0xFFFFFFFC);
  if ((array != 1) && !(size_in_bytes & 0x4)) {
    /* Make sure memory is 8-(mis)aligned */
    if ((array && !((long)tagged_high & 0x4))
	|| (!array && ((long)tagged_high & 0x4))) {
      if (tagged_high == atomic_low) {
	gcollect(size_in_bytes);
	return do_malloc(size_in_bytes, array);
      }
      ((long *)tagged_high)[0] = 0;
      tagged_high += 1;
    }
  }

  m = tagged_high;
  naya = tagged_high + (size_in_bytes >> 2) + 1;
  if (naya > atomic_low) {
    gcollect(size_in_bytes);
    return do_malloc(size_in_bytes, array);
  }
  tagged_high = naya;

  if (array) {
    ((long *)m)[0] = (size_in_bytes >> 2) & (array > 1 ? 0xC0000000 : 0x80000000);
    return m + 1;
  } else
    return m;
}

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes)
{
  return do_malloc(size_in_bytes, 1);
}

/* Tagged item: */
void *GC_malloc_one_tagged(size_t size_in_bytes)
{
  return do_malloc(size_in_bytes, 0);
}

void *GC_malloc_array_tagged(size_t size_in_bytes)
{
  return do_malloc(size_in_bytes, 2);
}

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes)
{
  void **naya;

  if (!size_in_bytes)
    return alloc_space;

  size_in_bytes = ((size_in_bytes + 3) & 0xFFFFFFFC);
  if (!(size_in_bytes & 0x4)) {
    /* Make sure memory is 8-misaligned */
    if (!((long)atomic_low & 0x4)) {
      if (atomic_low == tagged_high) {
	gcollect(size_in_bytes);
	return GC_malloc_atomic(size_in_bytes);
      }
      atomic_low -= 1;
      ((long *)atomic_low)[0] = 0;
    }
  }

  naya = atomic_low - ((size_in_bytes >> 2) + 1);
  if (naya < tagged_high) {
    gcollect(size_in_bytes);
    return GC_malloc_atomic(size_in_bytes);
  }
  atomic_low = naya;

  ((long *)naya)[0] = (size_in_bytes >> 2);
  return naya + 1;
}

/* Plain malloc: */
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes)
{
  return malloc(size_in_bytes);
}

void GC_free(void *s) /* noop */
{
}

void GC_general_register_disappearing_link(void **p, void *a)
{
}

void GC_register_late_disappearing_link(void **p, void *a)
{
}

void GC_general_unregister_disappearing_link(void **p, void *a)
{
}

void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
			   void *data, void (**oldf)(void *p, void *data), 
			   void **olddata)
{
  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;
}

void GC_register_finalizer_ignore_self(void *p, void (*f)(void *p, void *data), 
				       void *data, void (**oldf)(void *p, void *data), 
				       void **olddata)
{
  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;
}

void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
				 void *data, void (**oldf)(void *p, void *data), 
				 void **olddata)
{
  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;
}

void GC_unregister_disappearing_link(void **p)
{
}

void GC_register_traverser(Scheme_Type tag, Traverse_Proc proc)
{
  tag_table[tag] = proc;
}
