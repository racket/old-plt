/*
  Precise GC for MzScheme
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

#include <stdlib.h>
#include <stdio.h>

typedef short Scheme_Type;
#define MZTAG_REQUIRED

#include "gc2.h"
#include "../src/stypes.h"

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_custom_finalize)(void);
void (*GC_out_of_memory)(void);

void **GC_variable_stack;
int GC_variable_count;

Traverse_Proc tag_table[_scheme_last_type_];

#define STARTING_PLACE 0x400000

void *alloc_space = STARTING_PLACE;
long alloc_size;
void **tagged_high = STARTING_PLACE, **untagged_low = STARTING_PLACE;

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

#define SKIP 0x70000000

static Scheme_Type prev_tag, prev_prev_tag;

void gcollect(int needsize)
{
  /* Check old: */
  long *p, *top;

  p = (long *)untagged_low;
  top = (long *)(alloc_space + alloc_size);
  while (p < top) {
    p += (*p & 0x3FFFFFFF) + 1;
    /* Should also check tags... */
  }
  if (p == top) {
    /* printf("Untagged ok\n"); */
  }

  p = ((long *)alloc_space) + 1;
  while (p < (long *)tagged_high) {
    if (*p == SKIP) {
      p++;
    } else {
      size_t size;
      Scheme_Type tag = *(Scheme_Type *)p;
      /* printf("tag: %d %lx\n", tag, (long)p); */
      if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	*(int *)0x0 = 1;
      }
      size = tag_table[tag](p, identity);
      p += size;
      prev_prev_tag = prev_tag;
      prev_tag = tag;
    }
  }
  if (p == (long *)tagged_high) {
    /* printf("Tagged ok\n"); */
  }

  alloc_size = 32000;
  if (alloc_size < 2 * needsize)
    alloc_size = 2 * needsize;

  alloc_space = malloc(alloc_size);
  tagged_high = (void **)(alloc_space + 4);
  untagged_low = (void **)(alloc_space + alloc_size);

  memset(alloc_space, 0, alloc_size);
}

static void *malloc_tagged(size_t size_in_bytes)
{
  void **m, **naya;

  size_in_bytes = ((size_in_bytes + 3) & 0xFFFFFFFC);
  if (!(size_in_bytes & 0x4)) {
    /* Make sure memory is 8-aligned */
    if (((long)tagged_high & 0x4)) {
      if (tagged_high == untagged_low) {
	gcollect(size_in_bytes);
	return malloc_tagged(size_in_bytes);
      }
      ((long *)tagged_high)[0] = SKIP;
      tagged_high += 1;
    }
  }

  m = tagged_high;
  naya = tagged_high + (size_in_bytes >> 2);
  if (naya > untagged_low) {
    gcollect(size_in_bytes);
    return malloc_tagged(size_in_bytes);
  }
  tagged_high = naya;

  return m;
}

static void *malloc_untagged(size_t size_in_bytes, unsigned long nonatomic)
{
  void **naya;

  if (!size_in_bytes)
    return alloc_space;

  size_in_bytes = ((size_in_bytes + 3) & 0xFFFFFFFC);
  if (!(size_in_bytes & 0x4)) {
    /* Make sure memory is 8-aligned */
    if ((long)untagged_low & 0x4) {
      if (untagged_low == tagged_high) {
	gcollect(size_in_bytes);
	return malloc_untagged(size_in_bytes, nonatomic);
      }
      untagged_low -= 1;
      ((long *)untagged_low)[0] = 0;
    }
  }

  naya = untagged_low - ((size_in_bytes >> 2) + 1);
  if (naya < tagged_high) {
    gcollect(size_in_bytes);
    return malloc_untagged(size_in_bytes, nonatomic);
  }
  untagged_low = naya;

  ((long *)naya)[0] = (size_in_bytes >> 2) | nonatomic;
  
  return naya + 1;
}

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x80000000);
}

/* Tagged item: */
void *GC_malloc_one_tagged(size_t size_in_bytes)
{
  return malloc_tagged(size_in_bytes);
}

void *GC_malloc_array_tagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x40000000);
}

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, 0x00000000);
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
