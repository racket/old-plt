/*
  Precise GC for MzScheme
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

#include <stdlib.h>
#include <stdio.h>

#define USE_MMAP 0

#if USE_MMAP
/* For mmap: */
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>
#endif

typedef short Scheme_Type;
#define MZTAG_REQUIRED

#include "gc2.h"
#include "../src/stypes.h"

#define TIME 0
#define SEARCH 1
#define SAFETY 1
#define MAX_ERRORS 1

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_custom_finalize)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);

void **GC_variable_stack;
int GC_variable_count;

Traverse_Proc tag_table[_scheme_last_type_];

#define STARTING_PLACE ((void *)0x400000)

void *GC_alloc_space = STARTING_PLACE, *GC_alloc_top;
static long alloc_size, heap_size = 32000;
static void **tagged_high = STARTING_PLACE, **untagged_low = STARTING_PLACE;
static void **new_tagged_high, **new_untagged_low;

static void *old_space;
static long old_size;

static char *alloc_bitmap;

static char zero_sized[4];

static GC_Weak_Box *weak_boxes;

/******************************************************************************/

#if USE_MMAP

int fd, fd_created;

#define PAGE_SIZE 4096

void *malloc_pages(size_t len)
{
  void *r;

  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }

  if (len & (PAGE_SIZE - 1)) {
    len += PAGE_SIZE - (len & (PAGE_SIZE - 1));
  }

  r = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

  if (r  == (void *)-1) {
    printf("mmap failed: %d\n", errno);
    exit(-1);
  }

  return r;
}

void free_pages(void *p, size_t len)
{
  munmap(p, len);
}

#endif

/******************************************************************************/

#if !USE_MMAP

void *malloc_pages(size_t len)
{
  return malloc(len);
}

void free_pages(void *p, size_t len)
{
  free(p);
}

#endif

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

#define SKIP ((Scheme_Type)0x7000)
#define MOVED ((Scheme_Type)0x3000)

#if SEARCH
void *search_for;
#endif

void stop()
{
  printf("stopped\n");
}

static int mark_weak_box(void *p, Mark_Proc mark)
{
  if (mark) {
    GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
    gcMARK(wb->secondary_erase);
    if (wb->val) {
      wb->next = weak_boxes;
      weak_boxes = wb;
    }
    
    /* For now: */
    gcMARK(wb->val);
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static void *mark(void *p)
{
  long diff = ((char *)p - (char *)GC_alloc_space) >> 2;
  if (!(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
    long diff1 = ((char *)p - (char *)GC_alloc_space);
      
    while (!(alloc_bitmap[diff >> 3] & (1 << (diff & 0x7)))) {
      diff--;
    }
      
    diff <<= 2;

    return (void *)((char *)mark(diff + (char *)GC_alloc_space) + (diff1 - diff));
  } else {
    if (p < (void *)tagged_high) {
      Scheme_Type tag = *(Scheme_Type *)p;
      long size;
      void *naya;
	
      if (tag == MOVED)
	return ((void **)p)[1];

#if SAFETY	
      if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	*(int *)0x0 = 1;
      }
#endif
	
      size = tag_table[tag](p, NULL);
      if (!(size & 0x4)) {
	if ((long)new_tagged_high & 0x4) {
	  ((Scheme_Type *)new_tagged_high)[0] = SKIP;
	  new_tagged_high += 1;
	}
      }
	
      {
	int i;
	long *a, *b;
	a = (long *)new_tagged_high;
	b = (void *)p;
	for (i = size; i--; )
	  *(a++) = *(b++);
      }
	
      naya = new_tagged_high;
      ((Scheme_Type *)p)[0] = MOVED;
      ((void **)p)[1] = naya;
	
      new_tagged_high += size;
#if SEARCH
      if (naya == search_for) {
	stop();
      }
#endif
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
      {
	int i;
	long *a, *b;
	a = (long *)new_untagged_low;
	b = (void *)p;
	for (i = size; i--; )
	  *(a++) = *(b++);
      }
      ((void **)p)[1] = new_untagged_low + 1;
      ((long *)p)[0] = 0;
	
#if SEARCH
      if ((new_untagged_low + 1) == search_for) {
	stop();
      }
#endif
	
      return new_untagged_low + 1;
    }
  }
}

void **o_var_stack;
int o_var_count;

void GC_mark_variable_stack(void **var_stack,
			    int var_count,
			    long delta,
			    void *limit)
{
  int stack_depth;

  stack_depth = 0;
  while (var_stack) {
    int size = var_count;
    void ***p;

    var_stack = (void **)((char *)var_stack + delta);
    if (var_stack == limit)
      return;

    o_var_stack = var_stack;
    o_var_count = var_count;
    /* printf("%lx (%lx) %d\n", (long)var_stack, (long)((char *)var_stack - delta), var_count); */

    p = (void ***)(var_stack + 2);
    
    while (size--) {
      if (!*p) {
	/* Array */
	long count = ((long *)p)[2];
	void **a = ((void ***)p)[1];
	p += 2;
	size -= 2;
	a = (void **)((char *)a + delta);
	while (count--) {
	  gcMARK(*a);
	  a++;
	}
      } else {
	void **a = *p;
	a = (void **)((char *)a + delta);
	gcMARK(*a);
      }
      p++;
    }

#if 0
    if (*var_stack && ((unsigned long)*var_stack < (unsigned long)var_stack)) {
      printf("bad %d\n", stack_depth);
      *(int *)0x0 = 1;
    }
#endif

    var_count = ((long *)var_stack)[1]; 
    var_stack = *var_stack;
    stack_depth++;
  }
}

#if 0
# define GETTIME() ((long)scheme_get_milliseconds())
#else
# define GETTIME() ((long)scheme_get_process_milliseconds())
#endif

#if TIME
# define PRINTTIME(x) fprintf x
# define STDERR stderr
static long started, rightnow, old;
# define INITTIME() (started = GETTIME())
# define GETTIMEREL() (rightnow = GETTIME(), old = started, started = rightnow, rightnow - old)
#else
# define INITTIME() /* empty */
# define PRINTTIME(x) /* empty */
#endif

void gcollect(int needsize)
{
  /* Check old: */
  long *p, *top;
  void *new_space;
  long new_size = alloc_size;
  void **tagged_mark, **untagged_mark;
  char *bitmap;
  int i;
  long diff, iterations;

  INITTIME();
  PRINTTIME((STDERR, "gc: start: %ld\n", GETTIMEREL()));

  tag_table[scheme_weak_box_type] = mark_weak_box;
  weak_boxes = NULL;

  if (GC_collect_start_callback)
    GC_collect_start_callback();

  sort_and_merge_roots();

  if (new_size < (heap_size * 2)) {
    if (heap_size < needsize)
      new_size = needsize * 2;
    else
      new_size = heap_size * 2;
  }

  if (old_size >= new_size) {
    new_size = old_size;
    new_space = old_space;
  } else {
    if (old_size) {
      free_pages(old_space, old_size);
      old_size = 0;
    }

    new_space = malloc_pages(new_size + 4);
  }

  /******************** Make bitmap image: ****************************/

  {
    alloc_bitmap = bitmap = (char *)malloc((alloc_size >> 5) + 1);
    memset(bitmap, 0, (alloc_size >> 5) + 1);
  }

  p = (long *)untagged_low;
  diff = (((char *)p - (char *)GC_alloc_space) + 4) >> 2;
  top = (long *)GC_alloc_top;
  while (p < top) {
    long size = (*p & 0x3FFFFFFF) + 1;

    bitmap[diff >> 3] |= (1 << (diff & 0x7));

    p += size;
    diff += size;
  }

  p = ((long *)GC_alloc_space);
  diff = ((char *)p - (char *)GC_alloc_space) >> 2;
  while (p < (long *)tagged_high) {
    Scheme_Type tag = *(Scheme_Type *)p;
    if (tag == SKIP) {
      p++;
      diff++;
    } else {
      long size;

      bitmap[diff >> 3] |= (1 << (diff & 0x7));

      /* printf("tag %d\n", tag); */

#if SAFETY
      if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	*(int *)0x0 = 1;
      }
#endif
      size = tag_table[tag](p, NULL);
      p += size;
      diff += size;
    }
  }

  PRINTTIME((STDERR, "gc: bitmap: %ld\n", GETTIMEREL()));

  /******************** Mark/Copy ****************************/

  tagged_mark = new_tagged_high = (void **)new_space;
  untagged_mark = new_untagged_low = (void **)(new_space + new_size);

  GC_mark_variable_stack(GC_variable_stack,
			 GC_variable_count,
			 0,
			 (void *)(GC_get_thread_stack_base
				  ? GC_get_thread_stack_base()
				  : stack_base));

  PRINTTIME((STDERR, "gc: stack: %ld\n", GETTIMEREL()));

  for (i = 0; i < roots_count; i += 2) {
    void **s = (void **)roots[i];
    void **e = (void **)roots[i + 1];
    
    while (s < e) {
      gcMARK(*s);
      s++;
    }
  }

  PRINTTIME((STDERR, "gc: roots: %ld\n", GETTIMEREL()));

  iterations = 0;

  while ((tagged_mark < new_tagged_high)
	 || (untagged_mark > new_untagged_low)) {

    iterations++;

    while (tagged_mark < new_tagged_high) {
      Scheme_Type tag = *(Scheme_Type *)tagged_mark;

      if (tag == SKIP)
	tagged_mark++;
      else {
	long size;

#if SAFETY
	if ((tag < 0) || (tag >= _scheme_last_type_) || !tag_table[tag]) {
	  *(int *)0x0 = 1;
	}
#endif

	size = tag_table[tag](tagged_mark, mark);

#if SAFETY
	if (size <= 1) {
	  *(int *)0x0 = 1;
	}
#endif

	tagged_mark += size;

#if SAFETY
	if (tagged_mark < new_space) {
	  *(int *)0x0 = 1;
	}
#endif
      }
    }

    while (untagged_mark > new_untagged_low) {
      void **mp, **started;

      mp = started = new_untagged_low;
      while (mp < untagged_mark) {
	long v = *(long *)mp;
	long size = (v & 0x3FFFFFFF);

	if (v & 0xC0000000) {
	  mp++;	    
	  if (v & 0x80000000) {
	    /* Array of pointers */
	    int i;
	    /* printf("parray: %d %lx\n", size, (long)mp); */
	    for (i = size; i--; mp++) {
	      gcMARK(*mp);
	    }
	  } else {
	    /* Array of tagged */
	    int i, elem_size;
	    Scheme_Type tag = *(Scheme_Type *)mp;
	    
	    elem_size = tag_table[tag](mp, mark);
	    mp += elem_size;
	    for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	      tag_table[tag](mp, mark);
	  }
	} else
	  mp += v + 1;
      }
      untagged_mark = started;
    }
  }

  PRINTTIME((STDERR, "gc: mark/copy (%d): %ld\n", iterations, GETTIMEREL()));

  /******************************************************/

  
#if MAX_ERRORS
  if (alloc_size)
    free_pages(GC_alloc_space, alloc_size + 4);
#else
  old_space = GC_alloc_space;
  old_size = alloc_size;
#endif

  free(alloc_bitmap);

  PRINTTIME((STDERR, "gc: free: %ld\n", GETTIMEREL()));

  alloc_size = new_size;
  GC_alloc_space = new_space;
  GC_alloc_top = GC_alloc_space + alloc_size;
  tagged_high = new_tagged_high;
  untagged_low = new_untagged_low;

  heap_size = new_size - ((untagged_low - tagged_high) << 2);
  
  {
    long *p = (long *)untagged_low;
    while (p-- > (long *)tagged_high)
      *p = 0;
  }

  PRINTTIME((STDERR, "gc: done (t=%d, u=%d): %ld\n", 
	     (long)((void *)tagged_high - GC_alloc_space),
	     (long)(GC_alloc_top - (void *)untagged_low),
	     GETTIMEREL()));

  if (GC_collect_start_callback)
    GC_collect_end_callback();
}

void *GC_resolve(void *p)
{
  if ((p >= GC_alloc_space) && (p <= GC_alloc_top)) {  
    if (p < (void *)tagged_high) {
      Scheme_Type tag = *(Scheme_Type *)p;

      if (tag == MOVED)
	return ((void **)p)[1];
      else
	return p;
    } else {
      long size;
      
      p -= 4;
      size = ((*(long *)p) & 0x3FFFFFFF);
      
      if (!size)
	return ((void **)p)[1];
      else
	return p;
    }
  } else
    return p;
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

#if SEARCH
  if (m == search_for) {
    stop();
  }
#endif

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
  
#if SEARCH
  if ((naya + 1) == search_for) {
    stop();
  }
#endif

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

void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
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

void GC_register_traverser(Scheme_Type tag, Traverse_Proc proc)
{
  tag_table[tag] = proc;
}

void GC_gcollect()
{
  gcollect(0);
}
