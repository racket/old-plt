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

#define STARTING_PLACE ((void *)0x400000)

void *alloc_space = STARTING_PLACE;
long alloc_size, heap_size = 32000;
void **tagged_high = STARTING_PLACE, **untagged_low = STARTING_PLACE;
void **new_tagged_high, **new_untagged_low;

char *alloc_bitmap;

char zero_sized[4];

/******************************************************************************/

#define PTR_ALIGNMENT 4
#define PTR_TO_INT(x) ((unsigned long)x)

static long roots_count;
static long roots_size;
static unsigned long *roots;

static int compare_roots(const void *a, const void *b)
{
  if (*(unsigned long *)a < *(unsigned long *)b)
    return -1;
  else
    return 1;
}

static void sort_and_merge_roots()
{
  static int counter = 0;
  int i, offset, top;

  if (roots_count < 4)
    return;

  /* Only try this every 5 collections or so: */
  if (counter--)
    return;
  counter = 5;

  qsort(roots, roots_count >> 1, 2 * sizeof(unsigned long), compare_roots);
  offset = 0;
  top = roots_count;
  for (i = 2; i < top; i += 2) {
    if ((roots[i - 2 - offset] <= roots[i])
	&& ((roots[i - 1 - offset] + (PTR_ALIGNMENT - 1)) >= roots[i])) {
      /* merge: */
      if (roots[i + 1] > roots[i - 1 - offset])
	roots[i - 1 - offset] = roots[i + 1];
      offset += 2;
      roots_count -= 2;
    } else if (offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }
}

void GC_add_roots(void *start, void *end)
{
  if (roots_count >= roots_size) {
    unsigned long *naya;

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (unsigned long *)malloc(sizeof(unsigned long) * (roots_size + 1));

    memcpy((void *)naya, (void *)roots, 
	   sizeof(unsigned long) * roots_count);

    if (roots)
      free(roots);

    roots = naya;
  }

  roots[roots_count++] = PTR_TO_INT(start);
  roots[roots_count++] = PTR_TO_INT(end) - PTR_ALIGNMENT;
}

/******************************************************************************/

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

#define SKIP ((Scheme_Type)0x7000)
#define MOVED ((Scheme_Type)0x3000)

static Scheme_Type prev_tag, prev_prev_tag;

void *search_for;

void stop()
{
  printf("stopped\n");
}

static void *mark(void *p)
{
  if ((p >= alloc_space)
      && (p < (alloc_space + alloc_size))) {

#if 1
      long diff = ((char *)p - (char *)alloc_space);
      if (!(alloc_bitmap[diff >> 5] & (1 << ((diff >> 2) & 0x7)))) {
	printf("Failed sanity check\n");
	*(int *)0x0 = 1;
      }
#endif

    if (p < (void *)tagged_high) {
      Scheme_Type tag = *(Scheme_Type *)p;
      size_t size;
      void *naya;

      if (tag == MOVED)
	return ((void **)p)[1];

      if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	*(int *)0x0 = 1;
      }

      size = tag_table[tag](p, NULL);
      if (!(size & 0x4)) {
	if ((long)new_tagged_high & 0x4) {
	  ((Scheme_Type *)new_tagged_high)[0] = SKIP;
	  new_tagged_high += 1;
	}
      }

      memcpy(new_tagged_high, p, size << 2);

      naya = new_tagged_high;
      ((Scheme_Type *)p)[0] = MOVED;
      ((void **)p)[1] = naya;

      new_tagged_high += size;
      if (naya == search_for) {
	stop();
      }
      return naya;
    } else {
      long size;
      
      p -= 4;
      size = ((*(long *)p) & 0x3FFFFFFF);
      
      if (!size)
	return ((void **)p)[1];

      if (!(size & 1)) {
	if ((long)new_untagged_low & 0x40) {
	  new_untagged_low--;
	  *(long *)new_untagged_low = 0;
	}
      }
      size++;

      new_untagged_low -= size;
      memcpy(new_untagged_low, p, size << 2);
      ((void **)p)[1] = new_untagged_low + 1;
      ((long *)p)[0] = 0;

      if ((new_untagged_low + 1) == search_for) {
	stop();
      }

      return new_untagged_low + 1;
    }
  } else
    return p;
}

static void *cautious_mark(void *p)
{
  if ((p >= alloc_space)
      && (p < (alloc_space + alloc_size))) {
    
    if ((p < (void *)tagged_high) || (p >= (void *)untagged_low)) {
      long diff = ((char *)p - (char *)alloc_space);
      long diff1 = diff;
      
      diff &= 0xFFFFFFFC;

      while (!(alloc_bitmap[diff >> 5] & (1 << ((diff >> 2) & 0x7)))) {
	diff -= 4;
      }
      
      return (void *)((char *)mark(diff + (char *)alloc_space) + (diff1 - diff));
    } else
      return p;
  } else
    return p;
}

void gcollect(int needsize)
{
  /* Check old: */
  long *p, *top;
  void *new_space;
  long new_size = alloc_size;
  void **tagged_mark, **untagged_mark;
  void **var_stack;
  char *bitmap;
  int i, var_count;

  printf("gc\n");

  sort_and_merge_roots();

  if (new_size < (heap_size * 2)) {
    if (heap_size < needsize)
      new_size = needsize * 2;
    else
      new_size = heap_size * 2;
  }

  new_space = malloc(new_size);

  /******************** Make bitmap image: ****************************/

  alloc_bitmap = bitmap = (char *)malloc((alloc_size >> 5) + 1);
  memset(bitmap, 0, (alloc_size >> 5) + 1);
  p = (long *)untagged_low;
  top = (long *)(alloc_space + alloc_size);
  while (p < top) {
    long diff = ((char *)p - (char *)alloc_space) + 4;
    bitmap[diff >> 5] |= (1 << ((diff >> 2) & 0x7));

    p += (*p & 0x3FFFFFFF) + 1;
    /* Should also check tags... */
  }
  if (p == top) {
    /* printf("Untagged ok\n"); */
  }

  p = ((long *)alloc_space);
  while (p < (long *)tagged_high) {
    Scheme_Type tag = *(Scheme_Type *)p;
    if (tag == SKIP) {
      p++;
    } else {
      size_t size;

      long diff = ((char *)p - (char *)alloc_space);
      bitmap[diff >> 5] |= (1 << ((diff >> 2) & 0x7));

      if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	*(int *)0x0 = 1;
      }
      size = tag_table[tag](p, NULL);
      p += size;
      prev_prev_tag = prev_tag;
      prev_tag = tag;
    }
  }
  if (p == (long *)tagged_high) {
    /* printf("Tagged ok\n"); */
  }

  /******************** Mark/Copy ****************************/

  tagged_mark = new_tagged_high = (void **)new_space;
  untagged_mark = new_untagged_low = (void **)(new_space + new_size);

  var_stack = GC_variable_stack;
  var_count = GC_variable_count;
  while (var_stack) {
    int size = var_count;
    void ***p = (void ***)(var_stack + 2);
    
    while (size--) {
      **p = cautious_mark(**p);
      p++;
    }

    var_count = ((long *)var_stack)[1]; 
    var_stack = *var_stack;
  }

  for (i = 0; i < roots_count; i += 2) {
    void **s = (void **)roots[i];
    void **e = (void **)roots[i + 1];
    
    while (s < e) {
      *s = cautious_mark(*s);
      s++;
    }
  }

  while ((tagged_mark < new_tagged_high)
	 || (untagged_mark > new_untagged_low)) {

    while (tagged_mark < new_tagged_high) {
      Scheme_Type tag = *(Scheme_Type *)tagged_mark;

      if (tag == SKIP)
	tagged_mark++;
      else {
	size_t size;

	/* printf("%d\n", tag); */

	if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	  *(int *)0x0 = 1;
	}
	size = tag_table[tag](tagged_mark, cautious_mark);
	tagged_mark += size;
      }
    }

    while (untagged_mark > new_untagged_low) {
      void **mp, **started;

      mp = started = new_untagged_low;
      while (mp < untagged_mark) {
	long v = *(long *)mp;
	size_t size = (v & 0x3FFFFFFF);

	if (v & 0xC0000000) {
	  mp++;	    
	  if (v & 0x80000000) {
	    /* Array of pointers */
	    int i;
	    printf("parray: %d %lx\n", size, (long)mp);
	    for (i = size; i--; mp++)
	      *mp = cautious_mark(*mp);
	  } else {
	    /* Array of tagged */
	    int i, elem_size;
	    Scheme_Type tag = *(Scheme_Type *)mp;
	    
	    printf("tarray: %d %d\n", size, tag);

	    elem_size = tag_table[tag](mp, cautious_mark);
	    mp += elem_size;
	    for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	      tag_table[tag](mp, cautious_mark);
	  }
	} else
	  mp += v + 1;
      }
      untagged_mark = started;
    }
  }

  /******************************************************/

  if (alloc_size) {
    free(alloc_space);
    free(alloc_bitmap);
  }

  alloc_size = new_size;
  alloc_space = new_space;
  tagged_high = new_tagged_high;
  untagged_low = new_untagged_low;

  heap_size = new_size - ((untagged_low - tagged_high) << 2);
  
  memset(tagged_high, 0, ((untagged_low - tagged_high) << 2));
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
      ((Scheme_Type *)tagged_high)[0] = SKIP;
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

  if (m == search_for) {
    stop();
  }

  return m;
}

static void *malloc_untagged(size_t size_in_bytes, unsigned long nonatomic)
{
  void **naya;

  if (!size_in_bytes)
    return zero_sized;

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
  
  if ((naya + 1) == search_for) {
    stop();
  }

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
