/*
  Precise GC for MzScheme
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

#include <stdlib.h>
#include "gc2.h"

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_custom_finalize)(void);
void (*GC_out_of_memory)(void);

void **GC_variable_stack;
int GC_variable_count;

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

void GC_gcollect(void)
{
}

#if 0
void *prev;

int check_all()
{
  void *m = prev;
  int c = 0;

  while (m) {
    long size = ((long *)m)[0];
    if (size < 0)
      printf("bad size: %lx %d", m, size);
    if (((long *)m)[2] != 0xd5d5d5d5)
      printf("bad start marker: %lx %d", m, ((long *)m)[2]);
    if (*((long *)(m + size + 12)) != 0xd5d5d5d5)
      printf("bad end marker: %lx %d", m, *((long *)(m + size + 12)));
    
    c++;

    m = ((void **)m)[1];
  }

  return c;
}
#endif

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes)
{
#if 0
  void *m = malloc(12 + size_in_bytes + 4);

  memset(m + 12, 0, size_in_bytes);
  ((long *)m)[0] = size_in_bytes;
  ((void **)m)[1] = prev;
  ((long *)m)[2] = 0xd5d5d5d5;
  *((long *)(m + size_in_bytes + 12)) = 0xd5d5d5d5;
  
  prev = m;

  return m + 12;
#else
  void *m = malloc(size_in_bytes);
  memset(m, 0, size_in_bytes);
  return m;
#endif
}

/* Tagged item: */
void *GC_malloc_one_tagged(size_t s)
{
  return GC_malloc(s);
}

void *GC_malloc_array_tagged(size_t s)
{
  return GC_malloc(s);
}

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes)
{
  return malloc(size_in_bytes);
}

/* Plain malloc: */
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes)
{
  return GC_malloc_atomic(size_in_bytes);
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

void **GC_prepare_stack_frame(int size)
{
  return (void **)malloc(sizeof(void *) * size);
}

void GC_set_stack_frame(void **v)
{
}

