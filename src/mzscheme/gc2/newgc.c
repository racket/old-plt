/* A new accouting precise GC for MzScheme
   Copyright (C) 2001, 2002 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for known improvement points 
*/

#define MZ_PRECISE_GC 1 /* required for mz includes to work right */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc2.h"
#include "../src/schpriv.h"

#ifdef _WIN32
# include <windows.h>
# define bzero(m, s) memset(m, 0, s)
# define inline _inline
#endif


#if defined(sparc) || defined(__sparc) || defined(__sparc__)
# define ALIGN_DOUBLES
#endif

#include "msgprint.c"

/*****************************************************************************/
/* Collector selection. Change the definitions of these to set or unset the  */
/* particular collector you want.                                            */
/*****************************************************************************/

/* This turns on blame-the-child automatic memory accounting */
/* #define NEWGC_BTC_ACCOUNT */

/* This turns on memory tracing */
/* #define NEWGC_MEMORY_TRACE */

/* This turns on support for heap debugging (FIXME: NOT IMPLEMENTED YET) */
/* #define NEWGC_HEAP_DEBUGGING */

/* This turns on some internal debugging logs. Don't turn this on unless you
   don't care about performance and you're hacking the collector */
/* #define NEWGC_INTERNAL_DEBUGGING */

/* The initial size of generation 0. This will grow and shrink a bit as time
   goes on */
#define INIT_GEN0_SIZE (1 * 1024 * 1024)
#define GEN0_GROW_FACTOR 2
#define GEN0_GROW_ADDITION (1 * 1024 * 1024)
#define MAX_GEN0_SIZE (32 * 1024 * 1024)
#define MAX_GEN0_GROW_SHRINK (16 * 1024 * 1024)

#define FULL_COLLECT_EVERY 10
#define GENERATIONS 2

/* This is the log base 2 of the size of one word, given in bytes */
#define LOG_WORD_SIZE 2

/* This is the log base 2 of the standard memory page size. 14 means 2^14,
   which is 16k. This seems to be a good size. */
#define LOG_PAGE_SIZE 14

/* the number of tags to use for tagged objects */
#define NUMBER_OF_TAGS 260

#if defined(__APPLE__)&&defined(__ppc__)&&defined(__MACH__) && !defined(OS_X)
# define OS_X
#endif

/* the size of the blocks we use to build the nursery is dependent on the
   OS we're using. Different OSes react to being repeatedly asked to 
   allocated and deallocate blocks in different ways. These number have been
   tuned to offer the right performance */
#if defined(__FreeBSD__)
# define GEN0_PAGE_SIZE (1024 * 1024)
#endif

#if defined(OS_X)
# define GEN0_PAGE_SIZE (128 * 1024)
#endif

#if defined(sparc) || defined(__sparc) || defined(__sparc__)
# define GEN0_PAGE_SIZE (64 * 1024)
#endif

#ifndef GEN0_PAGE_SIZE
# define GEN0_PAGE_SIZE (512 * 1024)
#endif

/* These are computed from the previous settings. You shouldn't mess with 
   them */
#define PTR(x) ((void*)(x))
#define PPTR(x) ((void**)(x))
#define NUM(x) ((unsigned long)(x))
#define USEFUL_ADDR_BITS ((8 << LOG_WORD_SIZE) - LOG_PAGE_SIZE)
#define ADDR_BITS(x) (NUM(x) >> LOG_PAGE_SIZE)
#define WORD_SIZE (1 << LOG_WORD_SIZE)
#define WORD_BITS (8 * WORD_SIZE)
#define PAGE_SIZE (1 << LOG_PAGE_SIZE)

/* the externals */
void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

#include "my_qsort.c"

/*****************************************************************************/
/* OS-Level Memory Management Routines                                       */
/*****************************************************************************/
static unsigned long pages_in_heap = 0;
static unsigned long max_heap_size = 0;
static unsigned long max_used_pages = 0;
static unsigned long used_pages = 0;
static unsigned long in_unsafe_allocation_mode = 0;
static void (*unsafe_allocation_abort)();
static void garbage_collect(int);

inline static void check_used_against_max(size_t len) 
{
  used_pages += (len / PAGE_SIZE) + (((len % PAGE_SIZE) == 0) ? 0 : 1);

  if(in_unsafe_allocation_mode) {
    if(used_pages > pages_in_heap)
      unsafe_allocation_abort();
  } else {
    if(used_pages > max_used_pages) {
      garbage_collect(0); /* hopefully this will free enough space */
      if(used_pages > max_used_pages) {
	garbage_collect(1); /* hopefully *this* will free enough space */
	if(used_pages > max_used_pages) {
	  /* nope, no go. there's simply too much memory allocated. Inform
	     the thunk and then die semi-gracefully */
	  if(GC_out_of_memory)
	    GC_out_of_memory();
	  GCPRINT(GCOUTF, "The system has run out of memory!\n"); abort();
	}
      }
    }
  }
}

inline static void free_used_pages(size_t len) 
{
  used_pages -= (len / PAGE_SIZE) + (((len % PAGE_SIZE) == 0) ? 0 : 1);
}

#define CHECK_USED_AGAINST_MAX(len) check_used_against_max(len)
#define LOGICALLY_ALLOCATING_PAGES(len) /* empty */
#define ACTUALLY_ALLOCATING_PAGES(len) /* empty */
#define LOGICALLY_FREEING_PAGES(len) free_used_pages(len)
#define ACTUALLY_FREEING_PAGES(len) /* empty */

#if _WIN32
# include "vm_win.c"
# define MALLOCATOR_DEFINED
#endif

#ifdef OS_X
# define TEST 0
void designate_modified(void *p);
# include "vm_osx.c"
# define MALLOCATOR_DEFINED
#endif

#if OSKIT
# include "vm_osk.c"
# define MALLOCATOR_DEFINED
#endif

#ifndef MALLOCATOR_DEFINED
# include "vm_mmap.c"
#endif

/*****************************************************************************/
/* Memory Tracing, Part 1                                                    */
/*****************************************************************************/
#define MTRACE_FIELD_SIZE (WORD_BITS - (1 + 3 + 1 + 1 + 1 + LOG_PAGE_SIZE))

#ifdef NEWGC_MEMORY_TRACE
# error "memory tracing not implemented in this particular revision \
         please revert to early versions of this collector, and then nag \
         Adam (awick@cs.utah.edu) to put this stuff back in"
#endif

int GC_mtrace_new_id(void *f)
{
  return 0;
}

int GC_mtrace_union_current_with(int newval)
{
  return 0;
}

/*****************************************************************************/
/* Allocation                                                                */
/*****************************************************************************/

struct objhead {
  unsigned int tracefun : MTRACE_FIELD_SIZE;
  unsigned int clean_heap : 1;
  unsigned int type : 3;
  unsigned int debug_mark : 1;
  unsigned int btc_mark : 1;
  unsigned int mark : 1;
  unsigned int size : LOG_PAGE_SIZE;
};

/* this structure must be an odd number of words. The code that makes
   alignment work on Sparcs assumes this invariant, and messing with it
   will cause Sparc ports to crash almost immediately */
struct mpage {
  unsigned int size, previous_size; /* 2 */
  unsigned char generation;         /* + .25 */
  unsigned char back_pointers;      /* + .25 */
  unsigned char big_page;           /* + .25 */
  unsigned char page_type;          /* + .25 */
  struct mpage *next, *prev;        /* + 2 */
};                                  /* = 5 */
      
/* these give the size of a *page* header in words and bytes, respsectively */
#define HEADER_SIZEW gcBYTES_TO_WORDS(sizeof(struct mpage))
#define HEADER_SIZEB gcWORDS_TO_BYTES(HEADER_SIZEW)

/* this is the maximum size of an object that will fit on a page, in words.
   the "- 3" is basically used as a fudge/safety factor, and has no real, 
   important meaning. */
#define MAX_OBJECT_SIZEW (gcBYTES_TO_WORDS(PAGE_SIZE) - HEADER_SIZEW - 3)

/* the page type constants */
#define PAGE_TAGGED 0
#define PAGE_ATOMIC 1
#define PAGE_ARRAY 2
#define PAGE_TARRAY 3
#define PAGE_XTAGGED 4
#define PAGE_BIG 5

/* the number of page types. */
#define PAGE_TYPES 6

/* the page map makes a nice mapping from addresses to pages, allowing
   fairly fast lookup. this is useful. */
static struct mpage *page_map[1 << USEFUL_ADDR_BITS];

/* Generation 0. Generation 0 is a set of very large pages in a list, plus
   a set of smaller bigpages in a separate list. The former is purely an
   optimization, saving us from constantly deallocating and allocating the
   entire nursery on every GC. The latter is useful because it simplifies
   the allocation process (which is also a speed hack, come to think of it) 

   gen0_pages is the list of very large nursery pages. gen0_alloc_page is
   the member of this list we are currently allocating on. The size count
   helps us trigger collection quickly when we're running out of space; see
   the test in allocate_big. 
*/
static struct mpage *gen0_pages = NULL;
static struct mpage *gen0_alloc_page = NULL;
static struct mpage *gen0_big_pages = NULL;
static unsigned long gen0_current_size = 0;
static unsigned long gen0_max_size = 0;

/* We keep three lists of pages, which MUST BE DISTINCT. Pages is the list
   we keep around between collections, and also contains pages during 
   minor collections which are not being used by the collector. From_pages
   and to_pages have their usual copying collector meanings. To_pages is
   further expanded to include pages containing objects which are not 
   being collected, but either (a) have objects on them which refer to an
   earlier generaiont or (b) have sufficiently much unused space on them
   that it seems profitable to copy things to them (thus decreasing 
   fragmentation between major collections). 

   None of these structures is separated by generation, because anything
   in any of these is either (a) necessarily in generation 1 or (b) not
   going to survive the current collection.
*/
static struct mpage *pages[PAGE_TYPES];
static struct mpage *from_pages[PAGE_TYPES];
static struct mpage *to_pages[PAGE_TYPES];

/* miscellaneous variables */
static char *zero_sized[4]; /* all 0-sized allocs get this */
static int gc_full = 0; /* a flag saying if this is a full/major collection */
static Mark_Proc mark_table[NUMBER_OF_TAGS]; /* the table of mark procs */
static Fixup_Proc fixup_table[NUMBER_OF_TAGS]; /* the talbe of repair procs */
static unsigned long memory_in_use = 0; /* the amount of memory in use */

/* These procedures modify or use the page map. The page map provides us very
   fast mappings from pointers to the page the reside on, if any. The page 
   map itself serves two important purposes:

   Between collections, it maps pointers to write-protected pages, so that 
     the write-barrier can identify what page a write has happened to and
     mark it as potentially containing pointers from gen 1 to gen 0. 

   During collections, it maps pointers to "from" pages. */
#define modify_page_map(page, val) {                                  \
    long size_left = page->big_page ? page->size : PAGE_SIZE;         \
    void *p = page;                                                   \
                                                                      \
    while(size_left > 0) {                                            \
      page_map[ADDR_BITS(p)] = val;                                   \
      size_left -= PAGE_SIZE;                                         \
      p = (char *)p + PAGE_SIZE;                                      \
    }                                                                 \
  }

inline static void pagemap_add(struct mpage *page)
{
  modify_page_map(page, page);
}

inline static void pagemap_remove(struct mpage *page)
{
  modify_page_map(page, NULL);
}

inline static struct mpage *find_page(void *p)
{
  return page_map[ADDR_BITS(p)];
}


/* the core allocation functions */
static void *allocate_big(size_t sizeb, int type)
{
  unsigned long sizew;
  struct mpage *bpage;

  /* the actual size of this is the size, ceilinged to the next largest word,
     plus the size of the page header, plus one word for the object header.
     This last serves many purposes, including making sure the object is 
     aligned for Sparcs. */
  sizew = gcBYTES_TO_WORDS(sizeb) + HEADER_SIZEW + 1;
  sizeb = gcWORDS_TO_BYTES(sizew);

  if((gen0_current_size + sizeb) >= gen0_max_size) {
    garbage_collect(0);
  }
  gen0_current_size += sizeb;

  bpage = malloc_pages(sizeb, PAGE_SIZE);
  bpage->size = sizeb;
  bpage->big_page = 1;
  bpage->page_type = type;
  bpage->next = gen0_big_pages;
  if(bpage->next) bpage->next->prev = bpage;
  gen0_big_pages = bpage;

  return (void*)(NUM(bpage) + HEADER_SIZEB + WORD_SIZE);
}

#ifdef ALIGN_DOUBLES
# define ALIGN_SIZE(sizew) ((sizew & 0x1) ? (sizew + 1) : sizew)
#else
# define ALIGN_SIZE(sizew) (sizew)
#endif

inline static void *allocate(size_t sizeb, int type)
{
  if(sizeb) {
    size_t sizew = gcBYTES_TO_WORDS(sizeb) + 1;

    sizew = ALIGN_SIZE(sizew);
    if(sizew < MAX_OBJECT_SIZEW) {
      struct objhead *info;
      unsigned int newsize;

      sizeb = gcWORDS_TO_BYTES(sizew);
    alloc_retry:
      newsize = gen0_alloc_page->size + sizeb;

      if(newsize > GEN0_PAGE_SIZE) {
	if(gen0_alloc_page->next) gen0_alloc_page = gen0_alloc_page->next; else
	  garbage_collect(0);
	goto alloc_retry;
      } else {
	void *retval = PTR(NUM(gen0_alloc_page) + gen0_alloc_page->size);

	info = (struct objhead *)retval;
	info->type = type;
	info->size = sizew;
	gen0_alloc_page->size = newsize;
	gen0_current_size += sizeb;

	return PTR(NUM(retval) + WORD_SIZE);
      }
    } else return allocate_big(sizeb, type);
  } else return zero_sized;
}

/* the allocation mechanism we present to the outside world */
void *GC_malloc(size_t s) { return allocate(s, PAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t s) { return allocate(s, PAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t s) { return allocate(s, PAGE_XTAGGED); }
void *GC_malloc_array_tagged(size_t s) { return allocate(s, PAGE_TARRAY); }
void *GC_malloc_atomic(size_t s) { return allocate(s, PAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t s) { return malloc(s); }
void *GC_malloc_allow_interior(size_t s) {return allocate_big(s, PAGE_ARRAY);}
void GC_free(void *p) {}

/* this function resizes generation 0 to the closest it can get (erring high)
   to the size we've computed as ideal */
inline static void resize_gen0(unsigned long new_size)
{
  struct mpage *work = gen0_pages, *prev = NULL;
  unsigned long alloced_size = 0;
  
  /* fist make sure the big pages pointer is clean */
  gen0_big_pages = NULL; 

  /* then, clear out any parts of gen0 we're keeping, and deallocated any
     parts we're throwing out */
  while(work) {
    if(alloced_size > new_size) {
      /* there should really probably be an ASSERT here. If prev is NULL,
	 that's a BIG, BIG PROBLEM. After allocating it at startup, the
	 first page in gen0_pages should *never* be deallocated, so we
         should never arrive here without a valid prev */
      prev->next = NULL;
      
      /* remove the excess pages */
      while(work) {
	struct mpage *next = work->next;
	work->big_page = 1;
	work->size = GEN0_PAGE_SIZE;
	pagemap_remove(work);
	free_pages(work, GEN0_PAGE_SIZE);
	work = next;
      }
      
      break;
    } else {
      void **start = PPTR(work) + HEADER_SIZEW;
      void **end = PPTR(NUM(work) + work->size);

      alloced_size += GEN0_PAGE_SIZE;
      while(start < end) *start++ = NULL;
      work->size = HEADER_SIZEB;
      prev = work;
      work = work->next;
    }
  }

  /* if we're short, add more */
  while(alloced_size < new_size) {
    work = malloc_pages(GEN0_PAGE_SIZE, PAGE_SIZE);
    work->size = GEN0_PAGE_SIZE;
    work->big_page = 1;
    if(prev)
      prev->next = work;
    else gen0_pages = work;
    prev = work;
    pagemap_add(prev);
    work->size = HEADER_SIZEB;
    work->big_page = 0;
    alloced_size += GEN0_PAGE_SIZE;
  }

  /* we're going to allocate onto the first page now */
  gen0_alloc_page = gen0_pages;

  /* set the two size variables */
  gen0_max_size = alloced_size;
  gen0_current_size = 0;
}

#define difference(x, y) ((x > y) ? (x - y) : (y - x))

inline static void reset_nursery(void)
{
  unsigned long new_gen0_size;

  new_gen0_size = (GEN0_GROW_FACTOR * memory_in_use) + GEN0_GROW_ADDITION;
  if(new_gen0_size > MAX_GEN0_SIZE)
    new_gen0_size = MAX_GEN0_SIZE;

  if(difference(new_gen0_size, gen0_max_size) > MAX_GEN0_GROW_SHRINK) {
    if(gen0_max_size > new_gen0_size)
      new_gen0_size = gen0_max_size - MAX_GEN0_GROW_SHRINK;
    else
      new_gen0_size = gen0_max_size + MAX_GEN0_GROW_SHRINK;
  }
  
  resize_gen0(new_gen0_size);
  flush_freed_pages();
}

/* This procedure fundamentally returns true if a pointer is marked, and
   false if it isn't. This function assumes that you're talking, at this
   point, purely about the mark field of the object. It ignores things like
   the object not being one of our GC heap objects, being in a higher gen
   than we're collectiong, not being a pointer at all, etc. */
inline static int marked(void *p)
{
  if(!p) return 0;
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->mark;
}

/*****************************************************************************/
/* Internal Debugging Routines                                               */
/*****************************************************************************/
#ifdef NEWGC_INTERNAL_DEBUGGING
static FILE *dump;
static int collections = 0;

static void init_debug_file(void) 
{
  char *filename = malloc(8 * sizeof(char));
  
  filename[0] = 'g'; filename[1] = 'c'; filename[2] = 'l';
  filename[3] = 'o'; filename[4] = 'g'; filename[7] = 0;
  filename[5] = '0' + (collections / 10);
  filename[6] = '0' + (collections % 10);
  dump = fopen(filename, "a");
  collections += 1;
}

static void close_debug_file(void)
{
  fclose(dump);
}

static void dump_region(void **start, void **end)
{
  while(start < end) {
    fprintf(dump, "%.8lx: %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx\n", 
	    NUM(start), NUM(*start), NUM(*(start + 1)), NUM(*(start + 2)),
	    NUM(*(start + 3)), NUM(*(start + 4)), NUM(*(start + 5)), 
	    NUM(*(start + 6)), NUM(*(start + 7)));
    start += 8;
  }
  fprintf(dump, "\n\n");
}

static void dump_heap(void)
{
  struct mpage *page;
  short i;
  
  if(collections >= 0) {
    for(page = gen0_pages; page; page = page->next) {
      fprintf(dump, "Generation 0 Page (%p - %p, size %i):\n", 
	      page, PTR(NUM(page) + GEN0_PAGE_SIZE), page->size);
      dump_region(PPTR(page), PPTR(NUM(page) + page->size));
    }
    for(page = gen0_big_pages; page; page = page->next) {
      fprintf(dump, "Page %p (gen %i, type %i, big %i, back %i, size %i)\n",
	      page, page->generation, page->page_type, page->big_page,
	      page->back_pointers, page->size);
      dump_region(PPTR(page), PPTR(NUM(page) + page->size));
    }
    for(i = 0; i < PAGE_TYPES; i++)
      for(page = pages[i]; page; page = page->next) {
	fprintf(dump, "Page %p (gen %i, type %i, big %i, back %i, size %i)\n",
		page, page->generation, page->page_type, page->big_page,
		page->back_pointers, page->size);
	dump_region(PPTR(page), PPTR(NUM(page) + page->size));
      }
    fflush(dump);
  }
}
#endif

#ifdef NEWGC_INTERNAL_DEBUGGING
# define INIT_DEBUG_FILE() init_debug_file()
# define CLOSE_DEBUG_FILE() close_debug_file()
# define DUMP_HEAP() dump_heap()
# define DEBUGOUTF dump
# define GCDEBUG(args) { GCPRINT args; GCFLUSHOUT(); }
#else
# define INIT_DEBUG_FILE() /* */
# define CLOSE_DEBUG_FILE() /* */
# define DUMP_HEAP() /* */
# define GCDEBUG(args) /* */
#endif

#define GCWARN(args) { GCPRINT args; GCFLUSHOUT(); }
#define GCERR(args) { GCPRINT args; GCFLUSHOUT(); abort(); }

/*****************************************************************************/
/* Routines dealing with various runtime execution stacks                    */
/*                                                                           */
/* With the exception of the "traverse" macro and resultant simplification,  */
/* this code is entirely lifted from compact.c                               */
/*****************************************************************************/
void **GC_variable_stack;
static unsigned long stack_base;
static void *park[2];

void GC_set_stack_base(void *base) 
{
  stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base() 
{
  return stack_base;
}

#define traverse_stack(var_stack, delta, limit, operation) {          \
    long size, count;                                                 \
    void ***p, **a;                                                   \
                                                                      \
    if(park[0]) operation(park[0]); if(park[1]) operation(park[1]);   \
    while(var_stack) {                                                \
      var_stack = (void **)((char *)var_stack + delta);               \
      if(var_stack == limit) return;                                  \
                                                                      \
      size = *(long*)(var_stack + 1); p = (void***)(var_stack + 2);   \
      while(size--) {                                                 \
        a = *p;                                                       \
        if(!a) {                                                      \
          count = ((long *)p)[2]; a = ((void***)p)[1];                \
          p += 2; size -= 2; a = (void**)((char *)a + delta);         \
          while(count--) { operation(*a); a++; }                      \
        } else {                                                      \
          a = (void**)((char *)a + delta); operation(*a);             \
        }                                                             \
        p++;                                                          \
      }                                                               \
      var_stack = *var_stack;                                         \
    }                                                                 \
  }


#define gc_stack_base ((void*)(GC_get_thread_stack_base               \
                               ? GC_get_thread_stack_base()           \
                               : (unsigned long)stack_base)) 

/* these names are fixed by the gc2 spec; which is a bit unfortunate */
void GC_mark_variable_stack(void **var_stack, long delta, void *limit) 
{
  traverse_stack(var_stack, delta, limit, gcMARK);
}

void GC_fixup_variable_stack(void **var_stack, long delta, void *limit) 
{
  traverse_stack(var_stack, delta, limit, gcFIXUP);
} 

/*****************************************************************************/
/* Routines for root sets                                                    */
/*                                                                           */
/* This code is lifted in its entirety from compact.c                        */
/*****************************************************************************/

static unsigned long roots_count = 0;
static unsigned long roots_size = 0;
static unsigned long *roots = NULL;;
static int nothing_new = 0;

static int compare_roots(const void *a, const void *b)
{
  if(*(unsigned long*)a < *(unsigned long*)b)
    return -1;
  else 
    return 1;
}

static void sort_and_merge_roots()
{
  int i, offset, top;

  if(nothing_new || (roots_count < 4))
    return;

  my_qsort(roots, roots_count >> 1, 2 * sizeof(unsigned long), compare_roots);
  offset = 0; top = roots_count;
  for(i = 2; i < top; i += 2) {
    if((roots[i - 2 - offset] <= roots[i])
       && ((roots[i - 1 - offset] + (WORD_SIZE - 1)) >= roots[i])) {
      /* merge: */
      if(roots[i + 1] > roots[i - 1 - offset])
	roots[i - 1 - offset] = roots[i + 1];
      offset += 2; roots_count -= 2;
    } else if(roots[i] == roots[i + 1]) {
      /* Remove empty range: */
      offset += 2;
      roots_count -= 2;
    } else if(offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }
  nothing_new = 1;
}

void GC_add_roots(void *start, void *end)
{
  if(roots_count >= roots_size) {
    unsigned long *naya;

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (unsigned long*)malloc(sizeof(unsigned long) * (roots_size + 1));
    if(!naya) GCERR((GCOUTF, "Couldn't allocate space for root pointers!\n"));
    memcpy((void*)naya, (void*)roots, sizeof(unsigned long) * roots_count);
    
    if(roots) free(roots);
    roots = naya;
  }

  roots[roots_count++] = (unsigned long)start;
  roots[roots_count++] = (unsigned long)end - WORD_SIZE;
  nothing_new = 0;
}

inline static void mark_repair_roots() 
{
  unsigned long j;

  GCDEBUG((DEBUGOUTF, "MARKING ROOTS.\n"));
  if(roots) {
    sort_and_merge_roots();
    for(j = 0; j < roots_count; j+= 2) {
      void **start = (void**)roots[j];
      void **end = (void**)roots[j + 1];

      while(start < end)
	gcFIXUP(*start++);
    }
  }
}

/*****************************************************************************/
/* immobile boxes                                                            */
/*****************************************************************************/
struct immobile_box {
  void *p; /* this must be first or mred dies */
  struct immobile_box *next, *prev;
};

static struct immobile_box *immobile_boxes = NULL;

void **GC_malloc_immobile_box(void *p)
{
  struct immobile_box *ib = malloc(sizeof(struct immobile_box));
  if(!ib) GCERR((GCOUTF, "Couldn't allocate space for immobile box!\n"));
  ib->p = p; ib->next = immobile_boxes; ib->prev = NULL;
  if(ib->next) ib->next->prev = ib;
  immobile_boxes = ib;
  return (void**)ib;
}

void GC_free_immobile_box(void **b) 
{
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    if(PPTR(ib) == b) {
      if(ib->prev) ib->prev->next = ib->next;
      if(!ib->prev) immobile_boxes = ib->next;
      if(ib->next) ib->next->prev = ib->prev;
      free(ib);
      return;
    }
  GCWARN((GCOUTF, "Attempted free of non-existent immobile box %p\n", b));
}

inline static void mark_repair_immobiles(void)
{
  struct immobile_box *ib;

  GCDEBUG((DEBUGOUTF, "MARKING IMMOBILES.\n"));
  for(ib = immobile_boxes; ib; ib = ib->next)
    gcFIXUP(ib->p);
}

/*****************************************************************************/
/* finalizers                                                                */
/*****************************************************************************/
struct finalizer {
  char eager_level;
  char tagged;
  void *p;
  GC_finalization_proc f;
  void *data;
  struct finalizer *next;
};

static struct finalizer *finalizers = NULL;
static struct finalizer *run_queue = NULL, *last_in_queue = NULL;

void GC_set_finalizer(void *p, int tagged, int level, 
		      GC_finalization_proc f, void *data,
		      GC_finalization_proc *oldf, void **olddata)
{
  struct mpage *page = find_page(p);
  struct finalizer *fnl, *prev;

  if(!page) {
    if(oldf) *oldf = NULL;
    if(olddata) *olddata = NULL;
    return;
  }

  for(fnl = finalizers, prev = NULL; fnl; prev = fnl, fnl = fnl->next)
    if(fnl->p == p) {
      if(oldf) *oldf = fnl->f;
      if(olddata) *olddata = fnl->data;
      
      if(f) {
	fnl->f = f;
	fnl->data = data;
	fnl->eager_level = level;
      } else {
	if(prev) prev->next = fnl->next;
	if(!prev) finalizers = fnl->next;
      }
      return;
    }

  if(oldf) *oldf = NULL;
  if(olddata) *olddata = NULL;
  if(!f) return;

  park[0] = p; park[1] = data;
  fnl = GC_malloc_atomic(sizeof(struct finalizer));
  p = park[0]; data = park[1]; park[0] = park[1] = NULL;
  fnl->p = p; fnl->f = f; fnl->data = data; fnl->eager_level = level;
  fnl->tagged = tagged; fnl->next = finalizers;
  finalizers = fnl;
}

inline static void mark_repair_finalizer_structs(void)
{
  struct finalizer *fnl;

  GCDEBUG((DEBUGOUTF, "MARKING FINALIZERS.\n"));
  /* first repair the finalizers in the finalizers list */
  gcFIXUP(finalizers);
  for(fnl = finalizers; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->next);
  }
  /* then repair the finalizers in the run_queue */
  gcFIXUP(run_queue);
  for(fnl = run_queue; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->next);
    gcFIXUP(fnl->p);
  }
}

inline static void check_finalizers(int level)
{
  struct finalizer *work = finalizers, *prev = NULL;

  while(work) {
    if((work->eager_level == level) && find_page(work->p) && !marked(work->p)) {
      struct finalizer *next = work->next;

      GCDEBUG((DEBUGOUTF, 
	       "CFNL: Level %i finalizer %p on %p queued for finalization.\n",
	       work->eager_level, work, work->p));
      gcFIXUP(work->p);
      if(prev) prev->next = next;
      if(!prev) finalizers = next;
      if(last_in_queue) last_in_queue = last_in_queue->next = work;
      if(!last_in_queue) run_queue = last_in_queue = work;
      work->next = NULL;

      work = next;
    } else { 
      /* if this is level 3 (the last level), then repair the pointers
	 for that've been marked */
      if(level == 3) gcFIXUP(work->p);
      prev = work; 
      work = work->next; 
    }
  }
}

inline static void do_ordered_level3(void)
{
  struct finalizer *temp;

  for(temp = finalizers; temp; temp = temp->next)
    if(find_page(temp->p) && !marked(temp->p)) {
      GCDEBUG((DEBUGOUTF,
	       "LVL3: %p is not marked. Marking payload (%p)\n", 
	       temp, temp->p));
      if(temp->tagged) fixup_table[*(unsigned short*)temp->p](temp->p);
      if(!temp->tagged) GC_fixup_xtagged(temp->p);
    }
}

struct weak_finalizer {
  void *p;
  int offset;
  void *saved;
  struct weak_finalizer *next;
};

static struct weak_finalizer *weak_finalizers = NULL;

void GC_finalization_weak_ptr(void **p, int offset)
{
  struct weak_finalizer *wfnl;

  park[0] = p; wfnl = GC_malloc_atomic(sizeof(struct weak_finalizer));
  p = park[0]; park[0] = NULL;

  wfnl->p = p; wfnl->offset = offset * sizeof(void*);
  wfnl->next = weak_finalizers; weak_finalizers = wfnl;
}

inline static void mark_repair_weak_finalizers(void)
{
  struct weak_finalizer *work;

  GCDEBUG((DEBUGOUTF, "MARKING WEAK FINALIZERS.\n"));
  gcFIXUP(weak_finalizers);
  for(work = weak_finalizers; work; work = work->next)
    gcFIXUP(work->next);
}

inline static void clean_up_weak_finalizers(void)
{
  struct weak_finalizer *work = weak_finalizers, *prev = NULL;
  
  while(work) {
    if(find_page(work->p) && !marked(work->p)) {
      if(prev) prev->next = work->next;
      if(!prev) weak_finalizers = work->next;
      work = work->next;
    } else { work->p = GC_resolve(work->p); prev = work; work = work->next; }
  }
}

inline static void zero_weak_finalizers(void)
{
  struct weak_finalizer *wfnl;

  for(wfnl = weak_finalizers; wfnl; wfnl = wfnl->next) {
    wfnl->saved = *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset);
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = NULL;
  }
}

inline static void reset_weak_finalizers(void)
{
  struct weak_finalizer *wfnl;

  for(wfnl = weak_finalizers; wfnl; wfnl = wfnl->next) {
    if(marked(wfnl->p)) gcFIXUP(wfnl->saved); 
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = wfnl->saved;
    wfnl->saved = NULL;
  }
}

/*****************************************************************************/
/* weak boxes and arrays                                                     */
/*****************************************************************************/
struct weak_box {
  unsigned short type;
  short keyex;
  void *val;
  void **secondary_erase;
  int soffset;
  struct weak_box *next;
};

static unsigned short weak_box_tag;
static struct weak_box *weak_boxes = NULL;

void *GC_malloc_weak_box(void *p, void **secondary, int soffset)
{
  struct weak_box *wb;

  park[0] = p; park[1] = secondary;
  wb = GC_malloc_one_tagged(sizeof(struct weak_box));
  p = park[0]; secondary = park[1]; park[0] = park[1] = NULL;

  wb->type = weak_box_tag; wb->val = p; 
  wb->secondary_erase = secondary; wb->soffset = soffset;
  return wb;
}

inline static int size_weak_box(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

static int mark_weak_box(void *p)
{
  struct weak_box *wb = (struct weak_box *)p;

  gcFIXUP(wb->secondary_erase);
  return size_weak_box(p);
}

static int repair_weak_box(void *p)
{
  struct weak_box *wb = (struct weak_box *)p;

  gcFIXUP(wb->secondary_erase);
  wb->next = weak_boxes;
  weak_boxes = wb;

  return size_weak_box(p);
}

inline static void clean_up_weak_boxes(void)
{
  struct weak_box *work;

  for(work = weak_boxes; work; work = work->next) {
    if(find_page(work->val) && !marked(work->val)) {
      work->val = NULL;
      if(work->secondary_erase)
	*(work->secondary_erase + work->soffset) = NULL;
      work->secondary_erase = NULL;
    } else { 
      work->val = GC_resolve(work->val); 
    }
  }
  weak_boxes = NULL;
}

struct weak_array {
  unsigned short type;
  short keyex;
  long count;
  void *replace_val;
  struct weak_array *next;
  void *data[1];
};

static unsigned short weak_array_tag = 256;
static struct weak_array *weak_arrays = NULL;

void *GC_malloc_weak_array(size_t size, void *replace_val)
{
  struct weak_array *wa;

  park[0] = replace_val; 
  wa = GC_malloc_one_tagged(size + sizeof(struct weak_array) - sizeof(void*));
  replace_val = park[0]; park[0] = NULL;

  wa->type = weak_array_tag;
  wa->replace_val = replace_val;
  wa->count = gcBYTES_TO_WORDS(size);
  return wa;
}

inline static int size_weak_array(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(struct weak_array)
			  + ((((struct weak_array *)p)->count - 1) 
			     * sizeof(void*)));
}

static int mark_weak_array(void *p)
{
  struct weak_array *wa = (struct weak_array *)p;

  gcMARK(wa->replace_val);
  return size_weak_array(p);
}

static int repair_weak_array(void *p)
{
  struct weak_array *wa = (struct weak_array *)p;

  gcFIXUP(wa->replace_val);
  wa->next = weak_arrays;
  weak_arrays = wa;
  return size_weak_array(p);
}

inline static void clean_up_weak_arrays()
{
  struct weak_array *work;
  
  for(work = weak_arrays; work; work = work->next) {
    void **data;
    int i;
    
    for(data = work->data, i = work->count; i--; ) {
      void *p = data[i];
      
      if(p) {
	if(find_page(p) && !marked(p)) {
	  data[i] = work->replace_val;
	} else {
	  data[i] = GC_resolve(p);
	}
      }
    }
  }
  weak_arrays = NULL;
}

/*****************************************************************************/
/* thread list                                                               */
/*****************************************************************************/
#ifdef NEWGC_BTC_ACCOUNT
inline static int current_owner(Scheme_Custodian *c);

struct thread {
  void *thread;
  int owner;
  struct thread *next;
};

static struct thread *threads = NULL;

inline static void register_thread(void *t, void *c)
{
  struct thread *work;

  for(work = threads; work; work = work->next)
    if(work->thread == t) {
      work->owner = current_owner((Scheme_Custodian *)c);
      return;
    }
  work = (struct thread *)malloc(sizeof(struct thread));
  work->owner = current_owner((Scheme_Custodian *)c);
  work->thread = t;
  work->next = threads;
  threads = work;
}

inline static void mark_threads(int owner)
{
  struct thread *work;

  for(work = threads; work; work = work->next)
    if(work->owner == owner) {
      mark_table[scheme_thread_type](work->thread);
    }
}

inline static void clean_up_thread_list(void)
{
  struct thread *work = threads, *prev = NULL;

  while(work) {
    if(!find_page(work->thread) || marked(work->thread)) {
      work->thread = GC_resolve(work->thread);
      prev = work;
      work = work->next;
    } else {
      struct thread *next = work->next;

      if(prev) prev->next = next;
      if(!prev) threads = next;
      free(work);
      work = next;
    }
  }
}

inline static int thread_get_owner(void *p)
{
  struct thread *work; 

  for(work = threads; work; work = work->next)
    if(work->thread == p)
      return work->owner;
  GCERR((GCOUTF, "Bad thread value for thread_get_owner!\n"));
}
#endif

#ifndef NEWGC_BTC_ACCOUNT
# define register_thread(t,c) /* */
# define clean_up_thread_list() /* */
#endif

void GC_register_thread(void *t, void *c)
{
  register_thread(t, c);
}

/*****************************************************************************/
/* Internal Stack Routines                                                   */
/*****************************************************************************/
#if defined(NEWGC_BTC_ACCOUNT) || defined(NEWGC_HEAP_DEBUGGING)
#define STACKLET_SIZE GEN0_PAGE_SIZE

struct stacklet {
  struct stacklet *prev;
  void **cur;
  void **start;
  void **end;
};

static struct stacklet *istack = NULL;

inline static void push_2ptr(void *ptr1, void *ptr2)
{
  if(!istack || (istack->cur == istack->end)) {
    struct stacklet *temp = (struct stacklet *)malloc_pages(STACKLET_SIZE, 
							    PAGE_SIZE);
    if(temp) {
      temp->prev = istack;
      temp->cur = (void**)((unsigned long)temp + sizeof(struct stacklet));
      temp->start = temp->cur;
      temp->end = (void**)((unsigned long)temp + STACKLET_SIZE);
      istack = temp;
    } else {
      GCPRINT(GCOUTF, "No room left on stack.\n");
      GCPRINT(GCOUTF, "Accounting/Debugging information will be incomplete.\n");
    }
  }
  *(istack->cur++) = ptr1;
  *(istack->cur++) = ptr2;
}

inline static int pop_2ptr(void **ptr1, void **ptr2)
{
  if(istack && (istack->cur == istack->start)) {
    void *temp = istack;
    istack = istack->prev;
    free_pages(temp, STACKLET_SIZE);
  }
  if(istack) {
    *ptr2 = *(--istack->cur);
    *ptr1 = *(--istack->cur);
    return 1;
  } else return 0;
}

inline static void clear_2ptr_stack()
{
  while(istack) {
    struct stacklet *prev = istack->prev;
    free_pages(istack, STACKLET_SIZE);
    istack = prev;
  }
  flush_freed_pages();
}
  
#endif

/*****************************************************************************/
/* blame-the-child accounting                                                */
/*****************************************************************************/
#ifdef NEWGC_BTC_ACCOUNT

#define OWNER_TABLE_GROW_AMT 10

struct ot_entry {
  Scheme_Custodian *originator;
  Scheme_Custodian **members;
  unsigned int memory_use;
};

static struct ot_entry **owner_table = NULL;
static unsigned int owner_table_top = 0;

inline static int create_blank_owner_set(void)
{
  int i;

  for(i = 1; i < owner_table_top; i++)
    if(!owner_table[i]) {
      owner_table[i] = malloc(sizeof(struct ot_entry));
      bzero(owner_table[i], sizeof(struct ot_entry));
      return i;
    }

  owner_table_top += OWNER_TABLE_GROW_AMT;
  owner_table = realloc(owner_table, owner_table_top*sizeof(struct ot_entry*));
  bzero((char*)owner_table + (sizeof(struct ot_entry*) * 
			      (owner_table_top - OWNER_TABLE_GROW_AMT)),
	OWNER_TABLE_GROW_AMT * sizeof(struct ot_entry*));
  
  return create_blank_owner_set();
}

inline static int custodian_to_owner_set(Scheme_Custodian *cust)
{
  int i;

  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i] && (owner_table[i]->originator == cust))
      return i;
  i = create_blank_owner_set();
  owner_table[i]->originator = cust;
  return i;
}

inline static int current_owner(Scheme_Custodian *c)
{
  static int has_gotten_root_custodian = 0;

  if(!owner_table) {
    owner_table = malloc(10 * sizeof(struct ot_entry*));
    bzero(owner_table, 10 * sizeof(struct ot_entry*));
    if(create_blank_owner_set() != 1) {
      GCPRINT(GCOUTF, "Something extremely weird (and bad) has happened.\n");
      abort();
    }
  }

  if(!scheme_current_thread || !scheme_current_thread->config)
    return 1;

  if(!c)
    c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
  
  if(!has_gotten_root_custodian) {
    has_gotten_root_custodian = 1;
    owner_table[1]->originator = c;
    return 1;
  }

  return custodian_to_owner_set(c);
}

inline static int custodian_member_owner_set(void *cust, int set)
{
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *work = owner_table[set]->originator;

  while(work) {
    if(work == cust) return 1;
    box = work->parent;
    work = box ? box->u.two_ptr_val.ptr1 : NULL;
  }
  return 0;
}

inline static void account_memory(int set, int amount)
{
  owner_table[set]->memory_use += amount;
}

inline static void free_owner_set(int set)
{
  if(owner_table[set]) {
    free(owner_table[set]);
  }
  owner_table[set] = NULL;
}

inline static void clean_up_owner_table(void)
{
  int i;

  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i]) {
      /* repair or delete the originator */
      if(find_page(owner_table[i]->originator) &&
	 !marked(owner_table[i]->originator)) {
	owner_table[i]->originator = NULL;
      } else 
	owner_table[i]->originator = GC_resolve(owner_table[i]->originator);

      /* potential delete */
      if(i != 1) 
	if((owner_table[i]->memory_use == 0) && !owner_table[i]->originator)
	  free_owner_set(i);
    }
}

inline static unsigned long custodian_usage(void *custodian)
{
  unsigned long retval = 0;
  int i;

  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i] && custodian_member_owner_set(custodian, i)) 
      retval += owner_table[i]->memory_use;
  return gcWORDS_TO_BYTES(retval);
}

/* this is the actual code that runs the pass. it's a bit, er, hefty */
static int current_mark_owner = 0;

inline static void clear_old_owner_data(void)
{
  struct mpage *page;
  int j;

  for(j = 0; j < PAGE_TYPES; j++)
    for(page = pages[j]; page; page = page->next) {
      protect_pages(page, page->size, 1);
      if(page->big_page) {
	((struct objhead *)((char*)page + HEADER_SIZEB))->btc_mark = 0;
      } else {
	void **start = (void**)((char*)page + HEADER_SIZEB);
	void **end = (void**)((char*)page + page->size);
	
	while(start < end) {
	  struct objhead *info = (struct objhead *)start;
	  
	  info->btc_mark = 0;
	  start += info->size;
	}
      }
    }
}

inline static void memory_account_mark(struct mpage *page, void *ptr)
{
  GCDEBUG((DEBUGOUTF, "memory_account_mark: %p/%p\n", page, ptr));
  if(page->big_page) {
    struct objhead *info = (struct objhead *)((char*)page + HEADER_SIZEB);

    if(!info->btc_mark) {
      info->btc_mark = 1;
      account_memory(current_mark_owner, gcBYTES_TO_WORDS(page->size));
      push_2ptr(page, ptr);
    }
  } else {
    struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
    
    if(!info->btc_mark) {
      info->btc_mark = 1;
      account_memory(current_mark_owner, info->size);
      push_2ptr(page, ptr);
    }
  }
}

inline static void mark_normal_obj(struct mpage *page, void *ptr)
{
  struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);

  switch(page->page_type) {
    case PAGE_TAGGED: 
      if(*(unsigned short*)ptr == scheme_thread_type) {
	if(thread_get_owner(ptr) == current_mark_owner)
	  mark_table[scheme_thread_type](ptr);
      } else if(*(unsigned short*)ptr == scheme_custodian_type) {
	if(custodian_to_owner_set(ptr) == current_mark_owner)
	  mark_table[scheme_custodian_type](ptr);
      } else {
	mark_table[*(unsigned short*)ptr](ptr);
      }
      break;
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: { 
      void **temp = ptr, **end = temp + (info->size - 1);
      
      while(temp < end) gcMARK(*(temp++));
      break;
    };
    case PAGE_TARRAY: {
      unsigned short tag = *(unsigned short*)ptr;
      void **temp = ptr, **end = temp + (info->size - 1);
      
      while(temp < end) temp += mark_table[tag](temp);
      break;
    }
    case PAGE_XTAGGED: GC_mark_xtagged(ptr); break;
  }
}

inline static void mark_acc_big_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB + WORD_SIZE);
  void **end = PPTR(NUM(page) + page->size);

  switch(page->page_type) {
    case PAGE_TAGGED: mark_table[*(unsigned short*)start](start); break;
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: while(start < end) gcMARK(*(start++)); break;
    case PAGE_XTAGGED: GC_mark_xtagged(start); break;
    case PAGE_TARRAY: {
      unsigned short tag = *(unsigned short *)start;
      while(start < end) start += mark_table[tag](start);
      break;
    }
  }
}

int kill_propagation_loop = 0;

static void btc_overmem_abort()
{
  kill_propagation_loop = 1;
  GCWARN((GCOUTF, "WARNING: Ran out of memory accounting. "
	          "Info will be wrong.\n"));
}

static void propagate_accounting_marks(void)
{
  struct mpage *page;
  void *p;

  while(pop_2ptr((void**)&page, &p) && !kill_propagation_loop) {
    GCDEBUG((DEBUGOUTF, "btc_account: popped off page %p, ptr %p\n", page, p));
    if(page->big_page)
      mark_acc_big_page(page);
    else
      mark_normal_obj(page, p);
  }
  if(kill_propagation_loop)
    clear_2ptr_stack();
}

static int doing_memory_accounting = 0;

static void do_btc_accounting(void)
{
  Scheme_Custodian *cur = owner_table[current_owner(NULL)]->originator;
  Scheme_Custodian_Reference *box = cur->global_next;
  int i;

  GCDEBUG((DEBUGOUTF, "\nBEGINNING MEMORY ACCOUNTING\n"));
  doing_memory_accounting = 1;
  in_unsafe_allocation_mode = 1;
  unsafe_allocation_abort = btc_overmem_abort;
  
  /* clear the memory use numbers out */
  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i])
      owner_table[i]->memory_use = 0;
  clear_old_owner_data();
  
  /* the end of the custodian list is where we want to start */
  while(box->u.two_ptr_val.ptr1) {
    cur = (Scheme_Custodian*)box->u.two_ptr_val.ptr1;
    box = cur->global_next;
  }
  
  /* walk backwards for the order we want */
  while(cur) {
    int owner = custodian_to_owner_set(cur);
    
    current_mark_owner = owner;
    GCDEBUG((DEBUGOUTF, "MARKING THREADS OF OWNER %i (CUST %p)\n", owner, cur));
    kill_propagation_loop = 0;
    mark_threads(owner);
    GCDEBUG((DEBUGOUTF, "Propagating accounting marks\n"));
    propagate_accounting_marks();
    
    box = cur->global_prev; cur = box ? box->u.two_ptr_val.ptr1 : NULL;
  }
  
  in_unsafe_allocation_mode = 0;
  doing_memory_accounting = 0;
}

struct account_hook {
  int type;
  void *c1, *c2;
  unsigned long amount;
  struct account_hook *next;
};

static struct account_hook *hooks = NULL;

inline static void add_account_hook(int type,void *c1,void *c2,unsigned long b)
{
  struct account_hook *work;

  for(work = hooks; work; work = work->next) {
    if((work->type == type) && (work->c2 == c2)) {
      if(type == MZACCT_REQUIRE) {
	if(b > work->amount) work->amount = b;
      } else { /* (type == MZACCT_LIMIT) */
	if(b < work->amount) work->amount = b;
      }
    } 
  }

  if(!work) {
    work = malloc(sizeof(struct account_hook));
    work->type = type; work->c1 = c1; work->c2 = c2; work->amount = b;
    work->next = hooks; hooks = work;
  }
}

inline static void clean_up_account_hooks()
{
  struct account_hook *work = hooks, *prev = NULL;

  while(work) {
    if((!find_page(work->c1) || marked(work->c1)) &&
       (!find_page(work->c2) || marked(work->c2))) {
      work->c1 = GC_resolve(work->c1);
      work->c2 = GC_resolve(work->c2);
      prev = work; work = work->next;
    } else {
      struct account_hook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) hooks = next;
      free(work);
      work = next;
    }
  }
}

inline static void run_account_hooks()
{
  struct account_hook *work = hooks, *prev = NULL;

  while(work) {
    if( ((work->type == MZACCT_REQUIRE) && 
	 (((max_used_pages - used_pages) * PAGE_SIZE) < work->amount))
	||
	((work->type == MZACCT_LIMIT) &&
	 (GC_get_memory_use(work->c1) > work->amount))) {
      struct account_hook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) hooks = next;
      scheme_schedule_custodian_close(work->c2);
      free(work);
      work = next;
    } else {
      prev = work; work = work->next;
    }
  }
}

# define set_account_hook(a,b,c,d) { add_account_hook(a,b,c,d); return 1; }
#endif

#ifndef NEWGC_BTC_ACCOUNT
# define clean_up_owner_table() /* */
# define do_btc_accounting() /* */
# define doing_memory_accounting 0
# define memory_account_mark(p,o) /* */
# define set_account_hook(a,b,c,d) return 0
# define clean_up_account_hooks() /* */
# define run_account_hooks() /* */
# define custodian_usage(cust) 0
#endif

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2)
{
  set_account_hook(type, c1, c2, b);
}

/*****************************************************************************/
/* administration / initialization                                           */
/*****************************************************************************/

static int generations_available = 1;

void designate_modified(void *p)
{
  struct mpage *page = find_page(p);

  if(page) {
    protect_pages(page, page->size, 1);
    page->back_pointers = 1;
  } else GCERR((GCOUTF, "Seg fault (internal error) at %p\n", p));
}

#include "sighand.c"

void GC_init_type_tags(int count, int weakbox)
{
  static int initialized = 0;

  weak_box_tag = weakbox;
  if(!initialized) {
    initialized = 1;
    max_heap_size = determine_max_heap_size();
    pages_in_heap = max_heap_size / PAGE_SIZE;
    max_used_pages = pages_in_heap / 2;
    
    resize_gen0(INIT_GEN0_SIZE);
    
    GC_register_traversers(weakbox, size_weak_box, mark_weak_box,
			   repair_weak_box, 0, 0);
    GC_register_traversers(weak_array_tag, size_weak_array, mark_weak_array,
			   repair_weak_array, 0, 0);
    initialize_signal_handler();
  }
}

void GC_gcollect(void)
{
  garbage_collect(1);
}

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark,
			    Fixup_Proc fixup, int constant_Size, int atomic)
{
  mark_table[tag] = atomic ? (Mark_Proc)PAGE_ATOMIC : mark;
  fixup_table[tag] = fixup;
}

long GC_get_memory_use(void *o) 
{
  Scheme_Object *arg = (Scheme_Object*)o;
  unsigned long retval = 0;

  if(arg) {
    if(SCHEME_PROCP(arg)) {
      retval = 0;
    } else if(SAME_TYPE(SCHEME_TYPE(arg), scheme_custodian_type)) {
      retval = custodian_usage(arg);
    }
  } else {
    retval = gen0_current_size + memory_in_use;
  }

  return retval;
}

/*****************************************************************************/
/* Undocumented Extensions                                                   */
/*****************************************************************************/

void GC_count_lambdas_and_structs(Scheme_Object *stype,
				  unsigned long *lambdas,
				  unsigned long *structs)
{
  Scheme_Structure *s;
  struct mpage *work;

  /* set up the counters */
  *lambdas = *structs = 0;
  /* skim through the normal pages */
  for(work = pages[PAGE_TAGGED]; work; work = work->next) {
    void **start = PPTR(NUM(work) + HEADER_SIZEB);
    void **end = PPTR(NUM(work) + work->size);

    while(start < end) {
      int size = ((struct objhead *)start)->size;
      unsigned short tag = *(unsigned short *)(NUM(start) + WORD_SIZE);

      switch(tag) {
        case scheme_closure_type:
        case scheme_case_closure_type:
	  *lambdas += 1;
	  break;
        case scheme_structure_type:
        case scheme_proc_struct_type:
	  s = (Scheme_Structure*)(NUM(start) + WORD_SIZE);
	  if(s->stype == (Scheme_Struct_Type*)stype)
	    *structs += 1;
      }
      start += size;
    }
  }
  /* now look through generation 0 */
  for(work = gen0_pages; work; work = work->next) {
    void **start = PPTR(NUM(work) + HEADER_SIZEB);
    void **end = PPTR(NUM(work) + work->size);
    
    while(start < end) {
      int size = ((struct objhead *)start)->size;
      if(((struct objhead*)start)->type == PAGE_TAGGED) {
	unsigned short tag = *(unsigned short *)(NUM(start) + WORD_SIZE);
	
	switch(tag) {
          case scheme_closure_type:
          case scheme_case_closure_type:
	    *lambdas += 1;
	    break;
          case scheme_structure_type:
          case scheme_proc_struct_type:
	    s = (Scheme_Structure*)(NUM(start) + WORD_SIZE);
	    if(s->stype == (Scheme_Struct_Type*)stype)
	      *structs += 1;
	}
      }
      start += size;
    }
  }
}

static unsigned long tag_counts[NUMBER_OF_TAGS];
static int running_undocumented_reach_printer = 0;
static int kill_reach_prop = 0;
extern char *scheme_get_type_name(short t);
static unsigned long total_reach_use = 0;

static void undoc_reach_abort() {
  kill_reach_prop = 1;
  GCWARN((GCOUTF, "Ran out of memory tracing links.\n"));
}

static inline void reach_printer_mark(struct mpage *page, void *p)
{
  struct objhead *info;

  /* we do things this way ... which is slow ... because we're in 
     between collections, and thus the invariants we rely on later
     aren't necessarily held here */
  if((NUM(p) % WORD_SIZE) != 0)
    return;

  if(page->big_page && !(page->generation == 0)) {
    info = (struct objhead *)(NUM(page) + HEADER_SIZEB); 
    if(!info->debug_mark) {
      total_reach_use += page->size;
      info->debug_mark = 1;
      if(info->type == PAGE_TAGGED) 
	tag_counts[*(unsigned short*)p]++;
      push_2ptr(page, PTR(NUM(page) + HEADER_SIZEB));
    }
  } else {
    void **start = PPTR(NUM(page) + HEADER_SIZEB);
    void **end = PPTR(NUM(page) + page->size);

    if((PPTR(p) < start) || (PPTR(p) > end))
      return;

    while(start < PPTR(NUM(p) - WORD_SIZE)) {
      struct objhead *info = (struct objhead *)start;
      start += info->size;
    }

    if(start == PPTR((NUM(p) - WORD_SIZE))) {
      struct objhead *info = (struct objhead *)start;
      
      if(!info->debug_mark) {
	total_reach_use += gcWORDS_TO_BYTES(info->size);
	info->debug_mark = 1;
	if(info->type == PAGE_TAGGED)
	  tag_counts[*(unsigned short*)p]++;
	push_2ptr(page, p);
      }
    } 
  }
}

static inline void clear_page_debug_marks(struct mpage *page)
{
  if(page->big_page) {
    ((struct objhead *)(NUM(page) + HEADER_SIZEB))->debug_mark = 0;
  } else {
    void **start = PPTR(NUM(page) + HEADER_SIZEB);
    void **end = PPTR(NUM(page) + page->size);
    
    while(start < end) {
      struct objhead *info = (struct objhead *)start;
      info->debug_mark = 0;
      start += info->size;
    }
  }
}

static inline void clear_debug_marks()
{
  struct mpage *work;
  int i;
  
  /* unprotect everthing in generation 1 */
  for(i = 0; i < PAGE_TYPES; i++)
    for(work = pages[i]; work; work = work->next)
      protect_pages(work, work->size, 1);
  /* clear them in generation 0 */
  for(work = gen0_pages; work; work = work->next)
    clear_page_debug_marks(work);
  /* clear them in generation 1 */
  for(i = 0; i < PAGE_TYPES; i++)
    for(work = pages[i]; work; work = work->next)
      clear_page_debug_marks(work);
}


int print_object_reaches(Scheme_Object *obj)
{
  if(!find_page(obj) || (NUM(obj) & 0x1) || (NUM(obj) == 0)) {
    return 0;
  } else {
    struct mpage *page;
    void *ptr;
    int i;

    /* clear out the counts */
    for(i = 0; i < NUMBER_OF_TAGS; i++)
      tag_counts[i] = 0;

    in_unsafe_allocation_mode = 1;
    running_undocumented_reach_printer = 1;
    unsafe_allocation_abort = undoc_reach_abort;
    total_reach_use = 0;
    
    gcMARK(obj);
    while(pop_2ptr((void**)&page, &ptr) && !kill_reach_prop) {
      if(page->big_page && !(page->generation == 0)) {
	void **start = PPTR(NUM(page) + HEADER_SIZEB + WORD_SIZE);
	void **end = PPTR(NUM(page) + page->size);

	switch(page->page_type) {
	  case PAGE_TAGGED: 
	    if(NUM(mark_table[*(unsigned short*)start]) > PAGE_TYPES)
	      mark_table[*(unsigned short*)start](start); 
	    break;
	  case PAGE_ATOMIC: break;
	  case PAGE_ARRAY: while(start < end) gcMARK(*(start++)); break;
	  case PAGE_XTAGGED: GC_mark_xtagged(start); break;
	  case PAGE_TARRAY: {
	    unsigned short tag = *(unsigned short*)start;
	    while(start < end) start += mark_table[tag](start);
	    break;
	  }
	}
      } else {
	struct objhead *info = (struct objhead *)(NUM(ptr) - WORD_SIZE);

	switch(info->type) {
	  case PAGE_TAGGED: 
	    if(NUM(mark_table[*(unsigned short*)ptr]) > PAGE_TYPES)
	      mark_table[*(unsigned short*)ptr](ptr); 
	    break;
	  case PAGE_ATOMIC: break;
	  case PAGE_ARRAY: {
	    void **temp = ptr, **end = temp + info->size;
	    while(temp < end) gcMARK(*(temp++));
	    break;
	  };
	}
      }
    }

    if(kill_reach_prop)
      clear_2ptr_stack();

    running_undocumented_reach_printer = 0;
    in_unsafe_allocation_mode = 0;
    /* reprotect everything in generation 1 */
    for(i = 0; i < PAGE_TYPES; i++)
      for(page = pages[i]; page; page = page->next)
	if(page->page_type != PAGE_ATOMIC)
	  if(!page->back_pointers)
	    protect_pages(page, page->size, 0);

    GCWARN((GCOUTF, "Object reaches:\n"));
    for(i = 0; i < NUMBER_OF_TAGS; i++)
      if(tag_counts[i] > 0) 
	GCWARN((GCOUTF, "   %li %s objects\n", tag_counts[i],
		scheme_get_type_name(i)));
    GCWARN((GCOUTF, "... for a total of %li bytes\n", total_reach_use));

    return 1;
  }
}

/*****************************************************************************/
/* resolution / repair                                                       */
/*****************************************************************************/

void *GC_resolve(void *p)
{
  struct mpage *page = find_page(p);

  if(!page || page->big_page)
    return p;
  if(marked(p))
    return *(void**)p;
  return p;
}

void GC_fixup(void *pp)
{
  void *p = *(void**)pp;
  
  if(doing_memory_accounting) {
/*  GCWARN((GCOUTF, "Hit GC_fixup with pp = %p and p = %p during accounting\n",  
           pp, p));  */
    return;
  }

  if(p && !(NUM(p) & 0x1)) {
    struct mpage *page = find_page(p);

    if(page) {

      /* if this is not on a page, we don't want to mark it. if it is on
	 a page, but that's a new page, then this has already been marked
	 and repaired. Otherwise, we at least need to repair it, and 
	 possibly mark it. */
      if(page->big_page) {
	if(page->big_page == 1) {
	  /* we need to both mark and repair this */
	  page->big_page = 2;
	  page->previous_size = HEADER_SIZEB;
	  page->generation = 1;

	  if(page->prev) page->prev->next = page->next; else
	    from_pages[PAGE_BIG] = page->next;
	  if(page->next) page->next->prev = page->prev;

	  page->next = to_pages[PAGE_BIG]; 
	  page->prev = NULL;
	  if(page->next) page->next->prev = page;
	  to_pages[PAGE_BIG] = page;
	}  
	/* if the page has been marked, we don't need to remark it, and 
	   since it doesn't move, we don't have to do any repairs */
      } else {
	if(!marked(p)) {
	  /* we need to both mark and repair this */
	  struct objhead *ohead = (struct objhead *)(NUM(p) - WORD_SIZE);
	  unsigned short type = ohead->type;
	  struct mpage *work;
	  size_t size, sizeb;
	  void **dest, **src;
	  void *newplace;

	  if(!page->generation)
	    if(type == PAGE_TAGGED)
	      if((int)mark_table[*(unsigned short*)p] < PAGE_TYPES) 
		type = ohead->type = (int)mark_table[*(unsigned short*)p];
	  work = to_pages[type]; size = ohead->size; 
	  sizeb = gcWORDS_TO_BYTES(size);
	  
	  while(work && ((work->size + sizeb) >= PAGE_SIZE))
	    work = work->next;
	  
	  if(work && ((work->size + sizeb) < PAGE_SIZE)) {
	    newplace = PTR(NUM(work) + work->size); 
	    /* if we've found space on this, move it to the front */
/*  	    if(work->prev) {  */
/*  	      work->prev->next = work->next; */
/*  	      if(work->next) work->next->prev = work->prev; */
/*  	      work->next = to_pages[type];  */
/*  	      work->prev = NULL;  */
/*  	      to_pages[type]->prev = work; */
/*  	      to_pages[type] = work; */
/*  	    } */
	  } else {
	    work = (struct mpage *)malloc_pages(PAGE_SIZE, PAGE_SIZE);
	    work->generation = 1; work->page_type = type; 
	    work->size = work->previous_size = HEADER_SIZEB;
	    work->next = to_pages[type]; work->prev = NULL;
	    if(work->next) work->next->prev = work;
	    to_pages[type] = work;
	    /* DO NOT ADD THIS PAGE TO THE PAGE MAP HERE. We wait until
	       GC is completed before putting things in the page map */
	    newplace = PTR(NUM(work) + HEADER_SIZEB);
	  }
	  work->size += sizeb;
	  
	  dest = PPTR(newplace); src = PPTR(ohead);
	  while(size--) *(dest++) = *(src++);
	  *(void**)p = PTR(NUM(newplace) + WORD_SIZE);
	  ohead->mark = 1;
	  GCDEBUG((DEBUGOUTF, "Marked/copied %i bytes from obj %p to %p\n", 
		   ohead->size, p, PTR(NUM(newplace) + WORD_SIZE)));
	}
	/* we always need to repair this */
	*(void**)pp = *(void**)p;
      }
    } else GCDEBUG((DEBUGOUTF, "Did not mark %p from %p (no page)\n", p, pp)); 
  } else GCDEBUG((DEBUGOUTF, "Did not mark %p from %p (no ptr)\n", p, pp));
}

/*****************************************************************************/
/* marks                                                                     */
/*****************************************************************************/

void GC_mark(const void *p)
{
  if(p && !(NUM(p) & 0x1)) {
    struct mpage *page = find_page(PTR(p));

    if(page) {
      if(doing_memory_accounting) {
	memory_account_mark(page, PTR(p));
      } else if(running_undocumented_reach_printer) {
	reach_printer_mark(page, PTR(p));
      }
    }
  }
}

inline static void mark_big_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB + WORD_SIZE);
  void **end = PPTR(NUM(page) + page->size);

  switch(page->page_type) {
    case PAGE_TAGGED: fixup_table[*(unsigned short*)start](start); break;
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: while(start < end) gcFIXUP(*(start++)); break;
    case PAGE_XTAGGED: GC_fixup_xtagged(start); break;
    case PAGE_TARRAY: {
      unsigned short tag = *(unsigned short *)start;
      while(start < end) start += fixup_table[tag](start);
      break;
    }
  }
}

inline static void mark_tagged_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);

  while(start < PPTR(NUM(page) + page->size)) {
    fixup_table[*(unsigned short*)(start + 1)](start + 1);
    start += ((struct objhead *)start)->size;
  }
}

inline static void mark_array_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);
  
  while(start < PPTR(NUM(page) + page->size)) {
    struct objhead *info = (struct objhead *)start++;
    unsigned long size = info->size;
    while(--size) gcFIXUP(*(start++));
  }
}

inline static void mark_tarray_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);

  while(start < PPTR(NUM(page) + page->size)) {
    struct objhead *info = (struct objhead *)start;
    void **tempend = (start + info->size) - 1;
    unsigned short tag = *(unsigned short*)(++start);

    while(start < tempend)
      start += fixup_table[tag](start);
  }
}

inline static void mark_xtagged_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);
  
  while(start < PPTR(NUM(page) + page->size)) {
    GC_fixup_xtagged(start + 1);
    start += ((struct objhead *)start)->size;
  }
}

#define mark_repair_page(page) {                                      \
    if(page->big_page) mark_big_page(page); else                      \
      switch(page->page_type) {                                       \
        case PAGE_TAGGED: mark_tagged_page(page); break;              \
        case PAGE_ATOMIC: break;                                      \
        case PAGE_ARRAY: mark_array_page(page); break;                \
        case PAGE_TARRAY: mark_tarray_page(page); break;              \
        case PAGE_XTAGGED: mark_xtagged_page(page); break;            \
      }                                                               \
  }

/*****************************************************************************/
/* garbage collection                                                        */
/*****************************************************************************/

/* When we have incompletely filled pages in generation 1 during minor 
   collections, we copy some of the reachable gen0 objects onto them, 
   in an attempt to minimize fragmentation. This constant declares the
   cutoff for these pages. Pages that use more than this amount will not
   be considered for this, and won't have objects copied onto them. */
#define DEFRAG_CUTOFF (PAGE_SIZE >> 1)

/* These collect information about memory usage, for use in GC_dump. */
static unsigned long peak_memory_use = 0;
static unsigned long num_minor_collects = 0;
static unsigned long num_major_collects = 0;

static char *type_name[PAGE_TYPES] = { "tagged", "atomic", "array",
				       "tagged array", "xtagged", "big" };
void GC_dump(void)
{
  struct mpage *page;
  int i;

  GCWARN((GCOUTF, "Generation 0: %li of %li bytes used\n",
	  gen0_current_size, gen0_max_size));
  
  for(i = 0; i < PAGE_TYPES; i++) {
    unsigned long total_use = 0;
    
    for(page = pages[i]; page; page = page->next)
      total_use += page->size;
    GCWARN((GCOUTF, "Generation 1 [%s]: %li bytes used\n", 
	    type_name[i], total_use));
  }

  GCWARN((GCOUTF, "\n"));
  GCWARN((GCOUTF, "Current memory use: %li\n", GC_get_memory_use(NULL)));
  GCWARN((GCOUTF, "Peak memory use after a collection: %li\n", peak_memory_use));
  GCWARN((GCOUTF, "# of major collections: %li\n", num_major_collects));
  GCWARN((GCOUTF, "# of minor collections: %li\n", num_minor_collects));
}

static void prepare_pages_for_collection(void)
{
  struct mpage *work;
  int i, j = 0;

  GCDEBUG((DEBUGOUTF, "PREPPING PAGES.\n"));

  /* if this is a full collection, move the pages in gen 1 into from_pages */
  if(gc_full) {
    for(j = 0; j < PAGE_TYPES; j++) {
      from_pages[j] = pages[j];
      pages[j] = NULL;
    }
  }
  
  /* first move the bigpages in gen0 to from_pages */
  if(from_pages[PAGE_BIG]) {
    for(work = from_pages[PAGE_BIG]; work->next; work = work->next) {}
    work->next = gen0_big_pages;
    if(gen0_big_pages) gen0_big_pages->prev = work;
  } else from_pages[PAGE_BIG] = gen0_big_pages;

  /* we reset the memory_in_use counter here. it will be set to an accurate
     number later on */
  memory_in_use = 0;

  /* clear out the to_pages, so we don't get junk pointers anywhere */
  for(i = 0; i < PAGE_TYPES; i++)
    to_pages[i] = NULL;

  /* if this is a minor collection, we may have backpointers and/or spave
     in pages outside the scope of this collection with a good deal of
     free space left on them. We move these over to to_pages for convenience */
  if(!gc_full) {
    for(j = 0; j < PAGE_TYPES; j++) 
      for(work = pages[j]; work;) 
	if(work->back_pointers 
	   || ((j != PAGE_BIG) && (work->size < DEFRAG_CUTOFF))) {
	  struct mpage *next = work->next;
	  
	  if(generations_available) 
	    protect_pages(work, work->size, 1);

	  /* remove this page from the pages[] set */
	  if(work->next) work->next->prev = work->prev;
	  if(work->prev) work->prev->next = work->next; else
	    pages[j] = work->next;
	  
	  /* add this page to the to_pages[] set */
	  work->prev = NULL;
	  work->next = to_pages[j];
	  if(work->next) work->next->prev = work;
	  to_pages[j] = work;
	  
	  work = next;
	} else work = work->next;
  }

  /* now we remove all the entries in to_pages and pages from the page map.
     this is VERY IMPORTANT. The only thing we want in the pagemap are things
     in from_pages and the nursery. */
  for(j = 0; j < PAGE_TYPES; j++) {
    for(work = pages[j]; work; work = work->next)
      pagemap_remove(work);
    for(work = to_pages[j]; work; work = work->next)
      pagemap_remove(work);
  }

  /* now we make absolutely sure that everything in from_pages *is* in the
     page map. This is also VERY IMPORTANT. We know for sure that bigpages
     in gen0 will not be in the map and THEY REALLY MUST BE */
  for(j = 0; j < PAGE_TYPES; j++)
    for(work = from_pages[j]; work; work = work->next)
      pagemap_add(work);
  
  /* we have to do the mark of the older pointers as a separate traversal, 
     because we need all the pages we're going to move to to_pages to have
     their NEW_BITs set before we do any marking. This is VERY IMPORTANT
     ADAM */
  for(j = 0; j < PAGE_TYPES; j++) 
    for(work = to_pages[j]; work; work = work->next)
      if(work->back_pointers) {
	work->previous_size = HEADER_SIZEB;
	mark_repair_page(work);
	work->previous_size = work->size;
      }
}

static void mark_repair_full_heap(void)
{
  struct mpage *page;
  int j, changed;

  do {
    changed = 0;

    for(j = 0; j < PAGE_TYPES; j++)
      for(page = to_pages[j]; 
	  page; /*  && (page->previous_size != page->size);  */
	  page = page->next) {
	if(page->previous_size != page->size) {
	  mark_repair_page(page);
	  page->previous_size = page->size;
	  changed = 1;
	}
      }
  } while(changed);
}

static void clean_up_heap(void)
{
  struct mpage *work;
  int i;

  /* zeroth (heh) step; make sure the first page in to_pages is OK
     to write to. */
  for(i = 0; i < PAGE_TYPES; i++)
    if(pages[i])
      protect_pages(pages[i], pages[i]->size, 1);

  /* first, move everything in to_pages over to pages */
  for(i = 0; i < PAGE_TYPES; i++) {
    work = to_pages[i];
    
    while(work) {
      struct mpage *next = work->next;
      
      if(work->big_page) work->big_page = 1;
      work->next = pages[i]; work->prev = NULL;
      if(work->next) work->next->prev = work;
      pages[i] = work;
      work->back_pointers = 0;
      work = next;
    }
  }

  /* then delete everything in the from_pages, and remove them from the
     page map */
  for(i = 0; i < PAGE_TYPES; i++) {
    work = from_pages[i];

    while(work) {
      struct mpage *temp = work;
      work = work->next;
      pagemap_remove(temp);
      protect_pages(temp, temp->size, 1);
      free_pages(temp, temp->big_page ? temp->size : PAGE_SIZE);
    }
    from_pages[i] = NULL;
  }

  /* now add everything to the page map. This needs to be done now or 
     the memory accounting pass won't see it (which is bad) */
  for(i = 0; i < PAGE_TYPES; i++) 
    for(work = pages[i]; work; work = work->next)
      pagemap_add(work);

  /* we need to do memory accounting right here. Any earlier and the page
     map won't help us. Any later and our writes will cause memory 
     violations. So do it. */
  do_btc_accounting();

  /* then go through and protect all the pages, add them to the page map,
     and get the memory_in_use number accurate */
  for(i = 0; i < PAGE_TYPES; i++) 
    for(work = pages[i]; work; work = work->next) {
      memory_in_use += work->size;
      if(work->page_type != PAGE_ATOMIC) {
	protect_pages(work, work->size, 0);
      }
    }

  /* finally, null out the to_pages and from_pages sets */
  for(i = 0; i < PAGE_TYPES; i++) 
    to_pages[i] = from_pages[i] = NULL;
}

static void gc_overmem_abort()
{
  GCERR((GCOUTF, "ERROR: out of memory during collection!\n"));
}

static void garbage_collect(int force_full)
{
  static unsigned long number = 0;
  static unsigned int since_last_full = 0;
  static unsigned int running_finalizers = 0;

  /* determine if this should be a full collection or not */
  gc_full = force_full || !generations_available || 
            (since_last_full == FULL_COLLECT_EVERY);
  number++; if(gc_full) since_last_full = 0; else since_last_full++;
  INIT_DEBUG_FILE(); DUMP_HEAP();

  /* we don't want the low-level allocator freaking because we've gone past
     half the available memory */
  in_unsafe_allocation_mode = 1;
  unsafe_allocation_abort = gc_overmem_abort;
  
  /* inform the system (if it wants us to) that we're starting collection */
  if(GC_collect_start_callback)
    GC_collect_start_callback();

  /* prep the pages for collection. this sets up several important invariants
     we rely on deeply during collection. perhaps most importantly, the 
     segregation into from/to_pages and the set up of the page map */
  prepare_pages_for_collection();

  /* mark and repair the roots for collection */
  mark_repair_finalizer_structs();
  mark_repair_weak_finalizers();
  mark_repair_roots();
  mark_repair_immobiles();
  GC_fixup_variable_stack(GC_variable_stack, 0, gc_stack_base);

  /* now propagate/repair the marks we got from these roots, and do the
     finalizer passes */
  mark_repair_full_heap();

  check_finalizers(1); mark_repair_full_heap();
  check_finalizers(2); mark_repair_full_heap();
  if(gc_full) zero_weak_finalizers();
  do_ordered_level3(); mark_repair_full_heap();
  check_finalizers(3); mark_repair_full_heap();
  if(gc_full) {
    reset_weak_finalizers(); 
    mark_repair_full_heap();
  }

  /* do some cleanup structures that either change state based on the
     heap state after collection or that become useless based on changes
     in state after collection */
  clean_up_weak_finalizers();
  clean_up_weak_boxes(); 
  clean_up_weak_arrays();
  clean_up_thread_list();
  clean_up_owner_table();
  clean_up_account_hooks();

  /* now cleanup all the junk that we no longer need, since we're now 
     down with collection proper */
  clean_up_heap();
  reset_nursery();

  /* new we do want the allocator freaking if we go over half */
  in_unsafe_allocation_mode = 0;

  /* update some statistics */
  if(gc_full) num_major_collects++; else num_minor_collects++;
  if(peak_memory_use < memory_in_use) peak_memory_use = memory_in_use;

  /* inform the system (if it wants us to) that we're done with collection */
  if(GC_collect_start_callback)
    GC_collect_end_callback();

  /* run any queued finalizers, EXCEPT in the case where this collection was
     triggered by the execution of a finalizer. The outside world needs this
     invariant in some corner case I don't have a reference for. In any case,
     if we run a finalizer after collection, and it triggers a collection,
     we should not run the next finalizer in the queue until the "current"
     finalizer completes its execution */
  if(!running_finalizers) {
    running_finalizers = 1;
    while(run_queue) {
      struct finalizer *f;
      void **gcs;

      f = run_queue; run_queue = run_queue->next;
      if(!run_queue) last_in_queue = NULL;

      GCDEBUG((DEBUGOUTF, "Running finalizers %p for pointer %p (lvl %i)\n",
	       f, f->p, f->eager_level));
      gcs = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = gcs;
    }
    run_account_hooks();
    running_finalizers = 0;
  }

  DUMP_HEAP(); CLOSE_DEBUG_FILE();
}



