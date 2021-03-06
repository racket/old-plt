/* A new accouting precise GC for MzScheme
   Copyright (C) 2001, 2002 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for known improvement points 

   IF YOU'RE NOT ADAM (AND PROBABLY IF YOU ARE) READ THIS FIRST:

   This is now a hybrid copying/mark-compact collector. The nursery
   (generation 0) is copied into the old generation (generation 1),
   but the old generation compacts. This yields a nice combination
   of performance, scalability and memory efficiency.

   The following page map invariants are required:

   Outside of collection, only pages in the older generation should
   be in the global poitner -> page map.

   During the mark phase of collection, only pages which contain
   objects which may be marked should be in the page map. This means
   that during minor collections, only pages in the nursery should
   be in the map.

   During the rest of collection, only pages which contain the past
   locations of moved data should be in the page map. This means only
   the nursery and pages being compacted.
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
#define GEN0_SIZE_FACTOR 0.5
#define GEN0_SIZE_ADDITION (512 * 1024)
#define MAX_GEN0_SIZE (32 * 1024 * 1024)
#define MAX_GEN0_GROW_SHRINK (16 * 1024 * 1024)
#define GEN0_PAGE_SIZE (1 * 1024 * 1024)

/* This is the log base 2 of the size of one word, given in bytes */
#define LOG_WORD_SIZE 2

/* This is the log base 2 of the standard memory page size. 14 means 2^14,
   which is 16k. This seems to be a good size. */
#define LOG_APAGE_SIZE 14

/* the number of tags to use for tagged objects */
#define NUMBER_OF_TAGS 260

/* the size of a page we use for the internal mark stack */
#define STACK_PART_SIZE (1 * 1024 * 1024)

/* the maximum number of pages to compact at any one time */
#define MAX_PAGES_TO_COMPACT 25

#if defined(__APPLE__)&&defined(__ppc__)&&defined(__MACH__) && !defined(OS_X)
# define OS_X
#endif

/* These are computed from the previous settings. You shouldn't mess with 
   them */
#define PTR(x) ((void*)(x))
#define PPTR(x) ((void**)(x))
#define NUM(x) ((unsigned long)(x))
#define USEFUL_ADDR_BITS ((8 << LOG_WORD_SIZE) - LOG_APAGE_SIZE)
#define ADDR_BITS(x) (NUM(x) >> LOG_APAGE_SIZE)
#define WORD_SIZE (1 << LOG_WORD_SIZE)
#define WORD_BITS (8 * WORD_SIZE)
#define APAGE_SIZE (1 << LOG_APAGE_SIZE)
#define GENERATIONS 2

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
  used_pages += (len / APAGE_SIZE) + (((len % APAGE_SIZE) == 0) ? 0 : 1);

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
  used_pages -= (len / APAGE_SIZE) + (((len % APAGE_SIZE) == 0) ? 0 : 1);
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

#define malloc_dirty_pages(size,align) malloc_pages(size,align)

/*****************************************************************************/
/* Memory Tracing, Part 1                                                    */
/*****************************************************************************/
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
  unsigned int reserved : ((8*WORD_SIZE) - (4+3+LOG_APAGE_SIZE));
  /* the type and size of the object */
  unsigned int type : 3;
  /* these are the various mark bits we use */
  unsigned int mark : 1;
  unsigned int btc_mark : 1;
  /* these are used for compaction et al*/
  unsigned int moved : 1;
  unsigned int dead : 1;
  unsigned int size : LOG_APAGE_SIZE;
};

/* For sparcs, this structure must have an odd number of 4 byte words, or
   our alignment stuff is going to get screwy */
struct mpage {                      /* BYTES: */
  struct mpage *next, *prev;        /*    8 */
  unsigned int previous_size;       /* +  4 */
  unsigned int size;                /* +  4 */
  unsigned char generation;         /* +  1 */
  unsigned char back_pointers;      /* +  1 */
  unsigned char big_page;           /* +  1 */
  unsigned char page_type;          /* +  1 */
  unsigned char marked_on;          /* +  1 */
  unsigned char has_new;            /* +  1 */
  unsigned short live_size;         /* +  2 */
  struct mpage *mirror;             /* +  4 */
                                    /* = 28 bytes */
                                    /* = 28 / 4 = 7 words */
};

      
/* these give the size of a *page* header in words and bytes, respsectively */
#define HEADER_SIZEW gcBYTES_TO_WORDS(sizeof(struct mpage))
#define HEADER_SIZEB gcWORDS_TO_BYTES(HEADER_SIZEW)

/* this is the maximum size of an object that will fit on a page, in words.
   the "- 3" is basically used as a fudge/safety factor, and has no real, 
   important meaning. */
#define MAX_OBJECT_SIZEW (gcBYTES_TO_WORDS(APAGE_SIZE) - HEADER_SIZEW - 3)

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

/* All non-gen0 pages are held in the following structure. */
static struct mpage *pages[PAGE_TYPES];

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
    long size_left = page->big_page ? page->size : APAGE_SIZE;         \
    void *p = page;                                                   \
                                                                      \
    while(size_left > 0) {                                            \
      page_map[ADDR_BITS(p)] = val;                                   \
      size_left -= APAGE_SIZE;                                         \
      p = (char *)p + APAGE_SIZE;                                      \
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

  bpage = malloc_pages(sizeb, APAGE_SIZE);
  bpage->size = sizeb;
  bpage->big_page = 1;
  bpage->page_type = type;
  bpage->next = gen0_big_pages;
  if(bpage->next) bpage->next->prev = bpage;
  gen0_big_pages = bpage;
  pagemap_add(bpage);

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
    work = malloc_pages(GEN0_PAGE_SIZE, APAGE_SIZE);
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

  new_gen0_size = NUM((GEN0_SIZE_FACTOR * (float)memory_in_use) 
		      + GEN0_SIZE_ADDITION);
  if(new_gen0_size > MAX_GEN0_SIZE)
    new_gen0_size = MAX_GEN0_SIZE;

/*   if(difference(new_gen0_size, gen0_max_size) > MAX_GEN0_GROW_SHRINK) { */
/*     if(gen0_max_size > new_gen0_size) */
/*       new_gen0_size = gen0_max_size - MAX_GEN0_GROW_SHRINK; */
/*     else */
/*       new_gen0_size = gen0_max_size + MAX_GEN0_GROW_SHRINK; */
/*   } */
  
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
  struct mpage *page;

  if(!p) return 0;
  if(!(page = find_page(p))) return 1;
  if((NUM(page) + page->previous_size) > NUM(p)) return 1;
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->mark;
}

/*****************************************************************************/
/* Internal Debugging Routines                                               */
/*****************************************************************************/
#ifdef NEWGC_INTERNAL_DEBUGGING
static FILE *dump;
static int collections = 0;
static unsigned long stack_base;

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
    fprintf(dump, "STACK:\n");
    dump_region((void*)(NUM(&i) & 0xfffffff0),
		(void*)((GC_get_thread_stack_base   
			 ? GC_get_thread_stack_base()       
			 : (unsigned long)stack_base) & 0xfffffff0)); 
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
  long size, count;
  void ***p, **a;

  if(park[0]) gcMARK(park[0]);
  if(park[1]) gcMARK(park[1]);
  while (var_stack) {
    var_stack = (void **)((char *)var_stack + delta);
    if (var_stack == limit)
      return;

    size = *(long *)(var_stack + 1);
    p = (void ***)(var_stack + 2);
    
    while (size--) {
      a = *p;
      if (!a) {
	/* Array */
	count = ((long *)p)[2];
	a = ((void ***)p)[1];
	p += 2;
	size -= 2;
	a = (void **)((char *)a + delta);
	while (count--) {
	  gcMARK(*a);
	  a++;
	}
      } else {
	a = (void **)((char *)a + delta);
	gcMARK(*a);
      }
      p++;
    }

    var_stack = *var_stack;
  }
/*   traverse_stack(var_stack, delta, limit, gcMARK); */
}

void GC_fixup_variable_stack(void **var_stack, long delta, void *limit) 
{
  long size, count;
  void ***p, **a;

  if(park[0]) gcFIXUP(park[0]);
  if(park[1]) gcFIXUP(park[1]);
  while (var_stack) {
    var_stack = (void **)((char *)var_stack + delta);
    if (var_stack == limit)
      return;

    size = *(long *)(var_stack + 1);

    p = (void ***)(var_stack + 2);
    
    while (size--) {
      a = *p;
      if (!a) {
	/* Array */
	count = ((long *)p)[2];
	a = ((void ***)p)[1];
	p += 2;
	size -= 2;
	a = (void **)((char *)a + delta);
	while (count--) {
	  gcFIXUP(*a);
	  a++;
	}
      } else {
	a = (void **)((char *)a + delta);
	gcFIXUP(*a);
      }
      p++;
    }

    var_stack = *var_stack;
  }
/*   traverse_stack(var_stack, delta, limit, gcFIXUP); */
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

#define traverse_roots(gcMUCK) {                                            \
    unsigned long j;                                                        \
    if(roots) {                                                             \
      sort_and_merge_roots();                                               \
      for(j = 0; j < roots_count; j += 2) {                                 \
        void **start = (void**)roots[j];                                    \
        void **end = (void**)roots[j+1];                                    \
        while(start < end) gcMUCK(*start++);                                \
      }                                                                     \
    }                                                                       \
  }

inline static void mark_roots() 
{
  traverse_roots(gcMARK);
}

inline static void repair_roots()
{
  traverse_roots(gcFIXUP);
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

#define traverse_immobiles(gcMUCK) {                                        \
    struct immobile_box *ib;                                                \
    for(ib = immobile_boxes; ib; ib = ib->next)                             \
      gcMUCK(ib->p);                                                        \
  }

inline static void mark_immobiles(void)
{
  traverse_immobiles(gcMARK);
}

inline static void repair_immobiles(void)
{
  traverse_immobiles(gcFIXUP);
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

inline static void mark_finalizer_structs(void)
{
  struct finalizer *fnl;

  for(fnl = GC_resolve(finalizers); fnl; fnl = GC_resolve(fnl->next)) { 
    gcMARK(fnl->data); 
    gcMARK(fnl);
  }
  for(fnl = run_queue; fnl; fnl = fnl->next) {
    gcMARK(fnl->data);
    gcMARK(fnl->p);
    gcMARK(fnl);
  }
}  

inline static void repair_finalizer_structs(void)
{
  struct finalizer *fnl;

  /* repair the base parts of the list */
  gcFIXUP(finalizers); gcFIXUP(run_queue);
  /* then repair the stuff inside them */
  for(fnl = finalizers; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->p);
    gcFIXUP(fnl->next);
  }
  for(fnl = run_queue; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->p);
    gcFIXUP(fnl->next);
  }
}

inline static void check_finalizers(int level)
{
  struct finalizer *work = GC_resolve(finalizers), *prev = NULL;

  GCDEBUG((DEBUGOUTF, "CFNL: Checking level %i finalizers\n", level));
  while(work) {
    if((work->eager_level == level) && !marked(work->p)) {
      struct finalizer *next = GC_resolve(work->next);

      GCDEBUG((DEBUGOUTF, 
	       "CFNL: Level %i finalizer %p on %p queued for finalization.\n",
	       work->eager_level, work, work->p));
      gcMARK(work->p);
      if(prev) prev->next = next;
      if(!prev) finalizers = next;
      if(last_in_queue) last_in_queue = last_in_queue->next = work;
      if(!last_in_queue) run_queue = last_in_queue = work;
      work->next = NULL;

      work = next;
    } else { 
      GCDEBUG((DEBUGOUTF, "CFNL: Not finalizing %p (level %i on %p): %p / %i\n",
	       work, work->eager_level, work->p, find_page(work->p),
	       marked(work->p)));
      prev = work; 
      work = GC_resolve(work->next); 
    }
  }
}

inline static void do_ordered_level3(void)
{
  struct finalizer *temp;

  for(temp = GC_resolve(finalizers); temp; temp = GC_resolve(temp->next))
    if(!marked(temp->p)) {
      GCDEBUG((DEBUGOUTF,
	       "LVL3: %p is not marked. Marking payload (%p)\n", 
	       temp, temp->p));
      if(temp->tagged) mark_table[*(unsigned short*)temp->p](temp->p);
      if(!temp->tagged) GC_mark_xtagged(temp->p);
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

inline static void mark_weak_finalizer_structs(void)
{
  struct weak_finalizer *work;

  GCDEBUG((DEBUGOUTF, "MARKING WEAK FINALIZERS.\n"));
  for(work = weak_finalizers; work; work = work->next)
    gcMARK(work);
}

inline static void repair_weak_finalizer_structs(void)
{
  struct weak_finalizer *work, *prev;

  gcFIXUP(weak_finalizers);
  work = weak_finalizers; prev = NULL;
  while(work) {
    gcFIXUP(work->next);
    if(!marked(work->p)) {
      if(prev) prev->next = work->next;
      if(!prev) weak_finalizers = work->next;
      work = GC_resolve(work->next);
    } else {
      gcFIXUP(work->p);
      prev = work;
      work = work->next;
    }
  }
}

inline static void zero_weak_finalizers(void)
{
  struct weak_finalizer *wfnl;

  for(wfnl = GC_resolve(weak_finalizers); wfnl; wfnl = GC_resolve(wfnl->next)) {
    wfnl->saved = *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset);
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = NULL;
  }
}

inline static void reset_weak_finalizers(void)
{
  struct weak_finalizer *wfnl;

  for(wfnl = GC_resolve(weak_finalizers); wfnl; wfnl = GC_resolve(wfnl->next)) {
    if(marked(wfnl->p)) gcMARK(wfnl->saved); 
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

  gcMARK(wb->secondary_erase);
  wb->next = weak_boxes;
  weak_boxes = wb;
  if(!gc_full) gcMARK(wb->val);

  return size_weak_box(p);
}

static int repair_weak_box(void *p)
{
  struct weak_box *wb = (struct weak_box *)p;

  gcFIXUP(wb->secondary_erase);

  return size_weak_box(p);
}

inline static void clean_up_weak_boxes(void)
{
  struct weak_box *work;

  gcFIXUP(weak_boxes);
  for(work = weak_boxes; work; work = work->next) {
    if(!marked(work->val)) {
      work->val = NULL;
      if(work->secondary_erase)
	*(work->secondary_erase + work->soffset) = NULL;
      work->secondary_erase = NULL;
    } else { 
      work->val = GC_resolve(work->val); 
    }
    gcFIXUP(work->next);
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
  wa->next = weak_arrays;
  weak_arrays = wa;
  return size_weak_array(p);
}

static int repair_weak_array(void *p)
{
  struct weak_array *wa = (struct weak_array *)p;

  gcFIXUP(wa->replace_val);
  return size_weak_array(p);
}

inline static void clean_up_weak_arrays()
{
  struct weak_array *work;

  gcFIXUP(weak_arrays);
  for(work = weak_arrays; work; work = work->next) {
    void **data;
    int i;
    
    for(data = work->data, i = work->count; i--; ) {
      void *p = data[i];
      
      if(p) {
	if(!marked(p)) {
	  GCDEBUG((DEBUGOUTF, "Replacing %p in %p because marked is %i (%p/%i)\n",
		   p, work, marked(p), find_page(p), find_page(p)->previous_size));
	  data[i] = work->replace_val;
	} else {
	  data[i] = GC_resolve(p);
	}
      }
    }
    gcFIXUP(work->next);
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

static Mark_Proc normal_thread_mark = NULL, normal_custodian_mark = NULL;
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
    if(work->owner == owner) 
      normal_thread_mark(work->thread);
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

/* This is the code we use to implement the mark stack. We can't, sadly, use
   the standard C stack because we'll blow it; propagation makes for a *very*
   deep stack. So we use this instead. */
struct stacklet {
  struct stacklet *prev, *next;
  void **top;
  void **end;
  void **stop_here; /* this is only used for its address */
};

static struct stacklet *int_top = NULL;

inline static void push_ptr(void *ptr)
{
  /* This happens at the very beginning */
  if(!int_top) {
    int_top = (struct stacklet*)malloc(STACK_PART_SIZE);
    int_top->prev = int_top->next = NULL;
    int_top->top = PPTR(int_top) + 4;
    int_top->end = PPTR(NUM(int_top) + STACK_PART_SIZE);
  }

  /* This happens during propoagation if we go past the end of this stacklet */
  if(int_top->top == int_top->end) {
    /* test to see if we already have another stack page ready */
    if(int_top->next) {
      /* we do, so just use it */
      int_top = int_top->next;
      int_top->top = PPTR(int_top) + 4;
    } else {
      /* we don't, so we need to allocate one */
      int_top->next = (struct stacklet*)malloc(STACK_PART_SIZE);
      int_top->next->prev = int_top;
      int_top = int_top->next;
      int_top->next = NULL;
      int_top->top = PPTR(int_top) + 4;
      int_top->end = PPTR(NUM(int_top) + STACK_PART_SIZE);
    }
  }

  /* at this point, we're guaranteed to be good to push a  pointers */
  *(int_top->top++) = ptr;
}

inline static int pop_ptr(void **ptr)
{
  if(int_top->top == PPTR(&int_top->stop_here)) {
    if(int_top->prev) {
      /* if there is a previous page, go to it */
      int_top = int_top->prev;
    } else {
      /* if there isn't a previous page, then we've hit the bottom of the
	 stack */
      return 0;
    }
  }

  /* if we get here, we're guaranteed to have data */
  *ptr = *(--int_top->top);
  return 1;
}

inline static void clear_stack_pages(void)
{
  if(int_top) {
    struct stacklet *temp, *base;
    int keep = 2;
    
    /* go to the head of the list */
    for(; int_top->prev; int_top = int_top->prev) {}
    /* then go through and clear them out */
    base = int_top;
    for(; int_top; int_top = temp) {
      temp = int_top->next;
      if(keep) keep--; else free(int_top);
    }
    int_top = base;
    int_top->top = PPTR(int_top) + 4;
  }
}

inline static void reset_pointer_stack(void)
{
  struct stacklet *temp;

  /* go to the head of the list */
  for(temp = int_top; int_top->prev; int_top = int_top->prev) {}
  /* reset the stack */
  int_top->top = PPTR(int_top) + 4;
}

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
static int doing_memory_accounting = 0;
static int really_doing_accounting = 0;
static int current_mark_owner = 0;
static int old_btc_mark = 0;
static int new_btc_mark = 1;

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

  if(!has_gotten_root_custodian && c) {
    has_gotten_root_custodian = 1;
    owner_table[1]->originator = c;
    return 1;
  }

  if(!scheme_current_thread)
    return 1;
  else if (!c)
    return thread_get_owner(scheme_current_thread);
  else
    return custodian_to_owner_set(c);
}

inline static int custodian_member_owner_set(void *cust, int set)
{
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *work = owner_table[set]->originator;

  while(work) {
    if(work == cust) return 1;
    box = work->parent;
    work = box ? SCHEME_PTR1_VAL(box) : NULL;
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
      if(!marked(owner_table[i]->originator)) {
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
  
  if(!really_doing_accounting) {
    park[0] = custodian;
    really_doing_accounting = 1;
    garbage_collect(0);
    custodian = park[0]; 
    park[0] = NULL;
  }
  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i] && custodian_member_owner_set(custodian, i)) 
      retval += owner_table[i]->memory_use;
  return gcWORDS_TO_BYTES(retval);
}


inline static void memory_account_mark(struct mpage *page, void *ptr)
{
  GCDEBUG((DEBUGOUTF, "memory_account_mark: %p/%p\n", page, ptr));
  if(page->big_page) {
    struct objhead *info = (struct objhead *)((char*)page + HEADER_SIZEB);

    if(info->btc_mark == old_btc_mark) {
      info->btc_mark = new_btc_mark;
      account_memory(current_mark_owner, gcBYTES_TO_WORDS(page->size));
      push_ptr(ptr);
    }
  } else {
    struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
    
    if(info->btc_mark == old_btc_mark) {
      info->btc_mark = new_btc_mark;
      account_memory(current_mark_owner, info->size);
      push_ptr(ptr);
    }
  }
}

int BTC_thread_mark(void *p)
{
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

int BTC_custodian_mark(void *p)
{
  if(custodian_to_owner_set(p) == current_mark_owner)
    return normal_custodian_mark(p);
  else
    return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}


inline static void mark_normal_obj(struct mpage *page, void *ptr)
{
  switch(page->page_type) {
    case PAGE_TAGGED: {
      /* we do not want to mark the pointers in a thread or custodian 
	 unless the object's owner is the current owner. In the case
	 of threads, we already used it for roots, so we can just
	 ignore them outright. In the case of custodians, we do need
	 to do the check */
      mark_table[*(unsigned short*)ptr](ptr);
/*       unsigned short tag = *(unsigned short*)ptr; */
/*       if(tag != scheme_thread_type) { */
/* 	if(tag == scheme_custodian_type) { */
/* 	  if(custodian_to_owner_set(ptr) == current_mark_owner) */
/* 	    mark_table[scheme_custodian_type](ptr); */
/* 	} else mark_table[tag](ptr); */
/*       } */
      break;
    }
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: { 
      struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
      void **temp = ptr, **end = temp + (info->size - 1);
      
      while(temp < end) gcMARK(*(temp++));
      break;
    };
    case PAGE_TARRAY: {
      struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
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

  while(pop_ptr(&p) && !kill_propagation_loop) {
    page = find_page(p);
    GCDEBUG((DEBUGOUTF, "btc_account: popped off page %p, ptr %p\n", page, p));
    if(page->big_page)
      mark_acc_big_page(page);
    else
      mark_normal_obj(page, p);
  }
  if(kill_propagation_loop)
    reset_pointer_stack();
}

static void do_btc_accounting(void)
{
  if(really_doing_accounting) {
    Scheme_Custodian *cur = owner_table[current_owner(NULL)]->originator;
    Scheme_Custodian_Reference *box = cur->global_next;
    int i;

    GCDEBUG((DEBUGOUTF, "\nBEGINNING MEMORY ACCOUNTING\n"));
    doing_memory_accounting = 1;
    in_unsafe_allocation_mode = 1;
    unsafe_allocation_abort = btc_overmem_abort;
    
    if(!normal_thread_mark) {
      normal_thread_mark = mark_table[scheme_thread_type];
      normal_custodian_mark = mark_table[scheme_custodian_type];
    }
    mark_table[scheme_thread_type] = &BTC_thread_mark;
    mark_table[scheme_custodian_type] = &BTC_custodian_mark;
    
    /* clear the memory use numbers out */
    for(i = 1; i < owner_table_top; i++)
      if(owner_table[i])
	owner_table[i]->memory_use = 0;
    
    /* the end of the custodian list is where we want to start */
    while(SCHEME_PTR1_VAL(box)) {
      cur = (Scheme_Custodian*)SCHEME_PTR1_VAL(box);
      box = cur->global_next;
    }
    
    /* walk backwards for the order we want */
    while(cur) {
      int owner = custodian_to_owner_set(cur);
      
      current_mark_owner = owner;
      GCDEBUG((DEBUGOUTF,"MARKING THREADS OF OWNER %i (CUST %p)\n",
	       owner, cur));
      kill_propagation_loop = 0;
      mark_threads(owner);
      GCDEBUG((DEBUGOUTF, "Propagating accounting marks\n"));
      propagate_accounting_marks();
      
      box = cur->global_prev; cur = box ? SCHEME_PTR1_VAL(box) : NULL;
    }
  
    mark_table[scheme_thread_type] = normal_thread_mark;
    mark_table[scheme_custodian_type] = normal_custodian_mark;
    in_unsafe_allocation_mode = 0;
    doing_memory_accounting = 0;
    old_btc_mark = new_btc_mark;
    new_btc_mark = !new_btc_mark;
    clear_stack_pages();
  }
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

  if(!really_doing_accounting) {
    park[0] = c1; park[1] = c2;
    really_doing_accounting = 1;
    garbage_collect(0);
    c1 = park[0]; c2 = park[1];
    park[0] = park[1] = NULL;
  }
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
    if(marked(work->c1) && marked(work->c2)) {
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
	 (((max_used_pages - used_pages) * APAGE_SIZE) < work->amount))
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
# define set_btc_mark(x) (((struct objhead *)(x))->btc_mark = old_btc_mark)
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
# define set_btc_mark(x) /* */
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
    pages_in_heap = max_heap_size / APAGE_SIZE;
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
/* Garbage collection proper ... and all the mess therein                    */
/*****************************************************************************/

/* We use two mark routines to handle propagation. Why two? The first is the
   one that we export out, and it does a metric crapload of work. The second
   we use internally, and it doesn't do nearly as much. */

/* This is the first mark routine. It's a bit complicated. */
void GC_mark(const void *const_p)
{
  struct mpage *page;
  void *p = (void*)const_p;

  if(!p || (NUM(p) & 0x1)) {
    GCDEBUG((DEBUGOUTF, "Not marking %p (bad ptr)\n", p));
    return;
  }

  if((page = find_page(p))) {
    /* toss this over to the BTC mark routine if we're doing accounting */
    if(doing_memory_accounting) { memory_account_mark(page,p); return; }

    if(page->big_page) {
      /* This is a bigpage. The first thing we do is see if its been marked
	 previously */
      if(page->big_page == 1) {
	/* in this case, it has not. So we want to mark it, first off. */
	page->big_page = 2;

	/* if this is in the nursery, we want to move it out of the nursery */
	if(!page->generation) {
	  page->generation = 1;
	  if(page->prev) page->prev->next = page->next; else
	    gen0_big_pages = page->next;
	  if(page->next) page->next->prev = page->prev;

	  page->next = pages[PAGE_BIG]; 
	  page->prev = NULL;
	  if(page->next) page->next->prev = page;
	  pages[PAGE_BIG] = page;
	  /* if we're doing memory accounting, then we need to make sure the
	     btc_mark is right */
	  set_btc_mark(NUM(page) + HEADER_SIZEB);
	}
	
	page->marked_on = 1;
	GCDEBUG((DEBUGOUTF, "Marking %p on big page %p\n", p, page));
	/* Finally, we want to add this to our mark queue, so we can 
	   propagate its pointers */
	push_ptr(p);
      } else GCDEBUG((DEBUGOUTF, "Not marking %p on big %p (already marked)\n",
		      p, page));
    } else {
      struct objhead *ohead = (struct objhead *)(NUM(p) - WORD_SIZE);

      if(!ohead->mark) {
	/* what we do next depends on whether this is a gen0 or gen1 
	   object */
	if(page->generation) {
	  /* this is a generation 1 object. This means we are not going
	     to move it, we don't have to check to see if it's an atomic
	     object masquerading as a tagged object, etc. So all we do
	     is add the pointer to the mark queue and note on the page
	     that we marked something on it*/
	  if((NUM(page) + page->previous_size) <= NUM(p)) {
	    GCDEBUG((DEBUGOUTF, "Marking %p (leaving alone)\n", p));
	    ohead->mark = 1;
	    page->marked_on = 1;
	    page->previous_size = HEADER_SIZEB;
	    page->live_size += ohead->size;
	    push_ptr(p);
	  } else GCDEBUG((DEBUGOUTF, "Not marking %p (it's old; %p / %i)\n",
			  p, page, page->previous_size));
	} else {
	  /* this is a generation 0 object. This means that we do have
	     to do all of the above. Fun, fun, fun. */
	  unsigned short type = ohead->type;
	  struct mpage *work;
	  size_t size;
	  void *newplace;

	  /* first check to see if this is an atomic object masquerading
	     as a tagged object; if it is, then convert it */
	  if(type == PAGE_TAGGED)
	    if((int)mark_table[*(unsigned short*)p] < PAGE_TYPES)
	      type = ohead->type = (int)mark_table[*(unsigned short*)p];

	  /* now set us up for the search for where to put this thing */
	  work = pages[type];
	  size = gcWORDS_TO_BYTES(ohead->size);

	  /* search for a page with the space to spare */
	  while(work && ((work->size + size) >= APAGE_SIZE))
	    work = work->next;

	  /* now either fetch where we're going to put this object or make
	     a new page if we couldn't find a page with space to spare */
	  if(work) {
	    pagemap_add(work);
	    work->marked_on = 1;
	    newplace = PTR(NUM(work) + work->size);
	  } else {
	    /* Allocate and prep the page */
	    work = (struct mpage *)malloc_dirty_pages(APAGE_SIZE, APAGE_SIZE);
	    work->generation = 1;
	    work->page_type = type;
	    work->size = work->previous_size = HEADER_SIZEB;
	    work->marked_on = 1;
	    work->next = pages[type];
	    work->prev = NULL;
	    if(work->next) {
	      work->next->prev = work;
	    }
	    pagemap_add(work);
	    pages[type] = work;
	    newplace = PTR(NUM(work) + HEADER_SIZEB);
	  }

	  /* update the size */
	  work->size += size;
	  work->has_new = 1;

	  /* transfer the object */
	  memcpy(newplace, (const void *)ohead, size);
	  /* mark the old location as marked and moved, and the new location
	     as marked */
	  ohead->mark = ohead->moved = 1;
	  ((struct objhead *)newplace)->mark = 1;
	  /* if we're doing memory accounting, then we need the btc_mark
	     to be set properly */
	  set_btc_mark(ohead);
	  /* drop the new location of the object into the forwarding space
	     and into the mark queue */
	  newplace = PTR(NUM(newplace) + WORD_SIZE);
	  GCDEBUG((DEBUGOUTF,"Marking %p (moved to %p on page %p)\n", 
		   p, newplace, work));
	  *(void**)p = newplace;
	  push_ptr(newplace);
	}
      } else GCDEBUG((DEBUGOUTF,"Not marking %p (already marked)\n", p));
    }
  } else GCDEBUG((DEBUGOUTF,"Not marking %p (no page)\n",p));
}

/* this is the second mark routine. It's not quite as complicated. */
inline static void internal_mark(void *p)
{
  struct mpage *page = find_page(p);

  /* we can assume a lot here -- like it's a valid pointer with a page --
     because we vet bad cases out in GC_mark, above */
  if(page->big_page) {
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
  } else {
    struct objhead *info = (struct objhead *)(NUM(p) - WORD_SIZE);
    
    switch(info->type) {
      case PAGE_TAGGED: mark_table[*(unsigned short*)p](p); break;
      case PAGE_ATOMIC: break;
      case PAGE_ARRAY: {
	void **start = p;
	void **end = PPTR(info) + info->size;
	while(start < end) gcMARK(*start++);
	break;
      }
      case PAGE_TARRAY: {
	void **start = p;
	void **end = PPTR(info) + info->size;
	unsigned short tag = *(unsigned short *)start;
	while(start < end) start += mark_table[tag](start);
	break;
      }
      case PAGE_XTAGGED: GC_mark_xtagged(p); break;
    }
  }
}

/* this is what actually does mark propagation */
static void propagate_marks(void) 
{
  void *p;

  while(pop_ptr(&p)) {
    GCDEBUG((DEBUGOUTF, "Popped pointer %p\n", p));
    internal_mark(p);
  }
}

void *GC_resolve(void *p)
{
  struct mpage *page = find_page(p);
  struct objhead *info;

  if(!page || page->big_page)
    return p;
  
  info = (struct objhead *)(NUM(p) - WORD_SIZE);
  if(info->mark && info->moved) 
    return *(void**)p;
  else 
    return p;
}

void *GC_fixup_self(void *p)
{
  return p;
}

void GC_fixup(void *pp)
{
  struct mpage *page;
  void *p = *(void**)pp;

  if(!p || (NUM(p) & 0x1))
    return;

  if((page = find_page(p))) {
    struct objhead *info;

    if(page->big_page) return;
    info = (struct objhead *)(NUM(p) - WORD_SIZE);
    if(info->mark && info->moved) 
      *(void**)pp = *(void**)p;
    else GCDEBUG((DEBUGOUTF, "Not repairing %p from %p (not moved)\n",p,pp));
  } else GCDEBUG((DEBUGOUTF, "Not repairing %p from %p (no page)\n", p, pp));
}

/*****************************************************************************/
/* garbage collection                                                        */
/*****************************************************************************/

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

  GCWARN((GCOUTF,"\n"));
  GCWARN((GCOUTF,"Current memory use: %li\n", GC_get_memory_use(NULL)));
  GCWARN((GCOUTF,"Peak memory use after a collection: %li\n",peak_memory_use));
  GCWARN((GCOUTF,"# of major collections: %li\n", num_major_collects));
  GCWARN((GCOUTF,"# of minor collections: %li\n", num_minor_collects));
}

static void prepare_pages_for_collection(void)
{
  struct mpage *work;
  int i;
  
  GCDEBUG((DEBUGOUTF, "PREPPING PAGES.\n"));
  if(gc_full) {
    /* we need to make sure that previous_size for every page is reset, so
       we don't accidentally screw up the mark routine */
    for(i = 0; i < PAGE_TYPES; i++)
      for(work = pages[i]; work; work = work->next) {
	protect_pages(work, work->big_page ? work->size : APAGE_SIZE, 1);
	work->live_size = 0;
	work->previous_size = HEADER_SIZEB;
      }
  } else {
    /* if we're not doing a major collection, then we need to remove all the
       pages in pages[] from the page map */
    for(i = 0; i < PAGE_TYPES; i++)
      for(work = pages[i]; work; work = work->next) {
	protect_pages(work, work->big_page ? work->size : APAGE_SIZE, 1);
	pagemap_remove(work);
      }
  }

  /* we do this here because, well, why not? */
  weak_boxes = NULL;
  weak_arrays = NULL;
}

static void mark_backpointers(void)
{
  if(!gc_full) {
    struct mpage *work;
    int i;

    /* if this is not a full collection, then we need to mark any pointers
       which point backwards into generation 0, since they're roots. */
    for(i = 0; i < PAGE_TYPES; i++) {
      for(work = pages[i]; work; work = work->next) {
	if(work->back_pointers) {
	  /* these pages are guaranteed not to be write protected, because
	     if they were, they wouldn't have this bit set */
	  work->marked_on = 1;
	  work->previous_size = HEADER_SIZEB;
	  pagemap_add(work);
	  if(work->big_page) {
	    work->big_page = 2;
	    push_ptr(PPTR(NUM(work) + HEADER_SIZEB));
	  } else {
	    if(work->page_type != PAGE_ATOMIC) {
	      void **start = PPTR(NUM(work) + HEADER_SIZEB);
	      void **end = PPTR(NUM(work) + work->size);
	    
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		if(!info->dead) {
		  info->mark = 1;
		  /* This must be a push_ptr, and not a direct call to
		     internal_mark. This is because we need every object
		     in the older heap to be marked out of and noted as
		     marked before we do anything else */
		  push_ptr(start + 1);
		}
		start += info->size;
	      }
	    }
	  }
	  work->previous_size = HEADER_SIZEB;
	} else {
	  protect_pages(work, work->big_page ? work->size : APAGE_SIZE, 1);
	  GCDEBUG((DEBUGOUTF,"Setting previous_size on %p to %i\n", work,
		   work->size));
	  work->previous_size = work->size;
	}
      }
    }
  }
}

#define should_compact_page(lsize,tsize) ((lsize << 2) < tsize)

inline static void do_heap_compact(void)
{
  int compactable_pages_left = MAX_PAGES_TO_COMPACT; 
  int i;

  for(i = 0; i < PAGE_BIG; i++) {
    struct mpage *work = pages[i], *prev = NULL;

    while(work) {
      if(work->marked_on && !work->has_new) {
	/* then determine if we actually want to do compaction */
	if(should_compact_page(gcWORDS_TO_BYTES(work->live_size),work->size)) {
	  void **start = PPTR(NUM(work) + HEADER_SIZEB);
	  void **end = PPTR(NUM(work) + work->size);
	  struct mpage *npage = malloc_dirty_pages(APAGE_SIZE, APAGE_SIZE);
	  void **newplace;

	  GCDEBUG((DEBUGOUTF, "Compacting page %p: new version at %p\n", 
		   work, npage));
	  /* Set up the basic page parameters */
	  /* FIXME: ANY OTHER MAINTANENCE */
	  npage->previous_size = npage->size = HEADER_SIZEB;
	  npage->generation = 1;
	  npage->back_pointers = 0;
	  npage->big_page = 0;
	  npage->page_type = work->page_type;
	  npage->marked_on = 1;
	  npage->mirror = work;
	  /* Link in this new replacement page */
	  if(prev) prev->next = work->next; else pages[i] = work->next;
	  if(work->next) work->next->prev = prev;
	  npage->prev = NULL;
	  npage->next = pages[i];
	  if(npage->next) npage->next->prev = npage;
	  pages[i] = npage;
	  if(!prev) prev = npage;
	  /* set up the traversal pointers */
	  newplace = PPTR(NUM(npage) + HEADER_SIZEB);
	  start = PPTR(NUM(work) + HEADER_SIZEB);
	  end = PPTR(NUM(work) + work->size);

	  while(start < end) {
	    struct objhead *info = (struct objhead *)start;
	    
	    if(info->mark) {
	      GCDEBUG((DEBUGOUTF,"Moving size %i object from %p to %p\n",
		       gcWORDS_TO_BYTES(info->size), start+1, newplace+1));
	      memcpy(newplace, start, gcWORDS_TO_BYTES(info->size));
	      info->moved = 1;
	      *(PPTR(NUM(start) + 4)) = PTR(NUM(newplace) + 4);
	      newplace += info->size;
	    }
	    start += info->size;
	  }

	  /* add the old page to the page map so fixups can find forwards */
	  pagemap_add(npage->mirror);
	  /* set the size */
	  npage->size = NUM(newplace) - NUM(npage);
	  compactable_pages_left--;
	  if(!compactable_pages_left)
	    return;
	  work = work->next;
	} else { 
	  prev = work;
	  work = work->next;
	}
      } else {
	/* Much as I'd like to free pages here, so that we can just have the
	   relevant pages cached, that causes problems. Specifically, if a
	   weak whatever has a pointer into this pace, we *cannot* reuse it
	   yet, as we need that information to fix those bits later. */
	prev = work;
	work = work->next;
      }
    }
  }
}

static void repair_heap(void)
{
  struct mpage *page;
  int i;

  for(i = 0; i < PAGE_TYPES; i++) {
    for(page = pages[i]; page; page = page->next) {
      if(page->marked_on) {
	page->has_new = 0;
	/* these are guaranteed not to be protected */
	if(page->big_page) {
	  void **start = PPTR(NUM(page) + HEADER_SIZEB + WORD_SIZE);
	  void **end = PPTR(NUM(page) + page->size);

	  GCDEBUG((DEBUGOUTF, "Cleaning objs on page %p, starting with %p\n",
		   page, start));
	  page->big_page = 1; /* remove the mark */
	  switch(page->page_type) {
	    case PAGE_TAGGED: 
	      fixup_table[*(unsigned short*)start](start); 
	      break;
	    case PAGE_ATOMIC: break;
	    case PAGE_ARRAY: 
	      while(start < end) gcFIXUP(*(start++)); 
	      break;
	    case PAGE_XTAGGED: 
	      GC_fixup_xtagged(start); 
	      break;
	    case PAGE_TARRAY: {
	      unsigned short tag = *(unsigned short *)start;
	      while(start < end) start += fixup_table[tag](start);
	      break;
	    }
	  }
	} else {
 	  void **start = PPTR(NUM(page) + page->previous_size);
	  void **end = PPTR(NUM(page) + page->size);

	  GCDEBUG((DEBUGOUTF, "Cleaning objs on page %p, starting with %p\n",
		   page, start));
	  switch(page->page_type) {
	    case PAGE_TAGGED: 
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		
		if(info->mark) {
		  info->mark = 0;
		  fixup_table[*(unsigned short*)(start+1)](start+1);
		} else {
		  info->dead = 1;
		}
		start += info->size;
	      }
	      break;
	    case PAGE_ATOMIC:
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		if(info->mark) {
		  info->mark = 0;
		} else info->dead = 1;
		start += info->size;
	      }
	      break;
	    case PAGE_ARRAY: 
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		size_t size = info->size;
		if(info->mark) {
		  void **tempend = start++ + size;
		  while(start < tempend) gcFIXUP(*start++);
		  info->mark = 0;
		} else { 
		  info->dead = 1;
		  start += info->size;
		}
	      }
	      break;
	    case PAGE_TARRAY:
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		size_t size = info->size;
		if(info->mark) {
		  void **tempend = start++ + size;
		  unsigned short tag = *(unsigned short*)start;
		  while(start < tempend)
		    start += fixup_table[tag](start);
		  info->mark = 0;
		  start = PPTR(info) + info->size;
		} else {
		  info->dead = 1;
		  start += info->size;
		}
	      }
	      break;
	    case PAGE_XTAGGED:
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		if(info->mark) {
		  GC_fixup_xtagged(start + 1);
		  info->mark = 0;
		} else info->dead = 1;
		start += info->size;
	      }
	  }
	}
      } else GCDEBUG((DEBUGOUTF,"Not Cleaning page %p\n", page));
    }
  }
}

static void clean_up_heap(void)
{
  struct mpage *work, *prev;
  int i;

  memory_in_use = 0;
  
  /* For the purposes of this little loop, s/prev/next/ */
  for(work = gen0_big_pages; work; work = prev) {
    prev = work->next;
    pagemap_remove(work);
    free_pages(work, work->size);
  }

  for(i = 0; i < PAGE_TYPES; i++) {
    struct mpage *prev = NULL;

    if(gc_full) {
      work = pages[i];
      while(work) {
	if(!work->marked_on) {
	  struct mpage *next = work->next;
	  
	  if(prev) prev->next = next; else pages[i] = next;
	  if(work->next) work->next->prev = prev;
	  pagemap_remove(work);
	  free_pages(work, work->big_page ? work->size : APAGE_SIZE);
	  work = next;
	} else {
	  pagemap_add(work);
	  if(work->mirror) {
	    pagemap_remove(work->mirror);
	    free_pages(work->mirror, APAGE_SIZE);
	    work->mirror = NULL;
	  }
	  work->back_pointers = work->marked_on = 0;
	  prev = work; 
	  work = work->next;
	}
      }
    } else {
      for(work = pages[i]; work; work = work->next) {
	pagemap_add(work);
	work->back_pointers = work->marked_on = 0;
      }
    }
    
    /* since we're here anyways, compute the total memory use */
    for(work = pages[i]; work; work = work->next)
      memory_in_use += work->size;
  }
}

static void protect_old_pages(void)
{
  struct mpage *page;
  int i;

  for(i = 0; i < PAGE_TYPES; i++) 
    if(i != PAGE_ATOMIC)
      for(page = pages[i]; page; page = page->next)
	if(page->page_type != PAGE_ATOMIC) 
	  protect_pages(page, page->size, 0);
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
  static unsigned long last_full_mem_use = (20 * 1024 * 1024);
  unsigned long old_mem_use = memory_in_use;

  /* determine if this should be a full collection or not */
  gc_full = force_full || !generations_available 
    || (since_last_full > 100) || (memory_in_use > (2 * last_full_mem_use));
/*   printf("Collection #li (full = %i): %i / %i / %i / %i\n", number, */
/* 	 gc_full, force_full, !generations_available, */
/* 	 (since_last_full > 100), (memory_in_use > (2 * last_full_mem_use))); */
  

  number++; 
  INIT_DEBUG_FILE(); DUMP_HEAP();

  /* we don't want the low-level allocator freaking because we've gone past
     half the available memory */
  in_unsafe_allocation_mode = 1;
  unsafe_allocation_abort = gc_overmem_abort;
  
  /* inform the system (if it wants us to) that we're starting collection */
  if(GC_collect_start_callback)
    GC_collect_start_callback();

  prepare_pages_for_collection();
  /* at this point, the page map should only include pages that contain
     collectable objects */

  /* mark and repair the roots for collection */
  mark_backpointers();
  mark_finalizer_structs();
  mark_weak_finalizer_structs();
  mark_roots();
  mark_immobiles();
  GC_mark_variable_stack(GC_variable_stack, 0, gc_stack_base);

  /* now propagate/repair the marks we got from these roots, and do the
     finalizer passes */
  propagate_marks();
  check_finalizers(1); propagate_marks();
  check_finalizers(2); propagate_marks();
  if(gc_full) zero_weak_finalizers();
  do_ordered_level3(); propagate_marks();
  check_finalizers(3); propagate_marks();
  if(gc_full) {
    reset_weak_finalizers(); 
    propagate_marks();
  }
#ifndef NEWGC_BTC_ACCOUNT
  /* we need to clear out the stack pages. If we're doing memory accounting,
     though, we might as well leave them up for now and let the accounting
     system clear them later. Better then freeing them, at least. If we're
     not doing accounting, though, there is no "later" where they'll get
     removed */
  clear_stack_pages();  
#endif

  if(gc_full) do_heap_compact();

  /* do some cleanup structures that either change state based on the
     heap state after collection or that become useless based on changes
     in state after collection */
  clean_up_weak_boxes(); 
  clean_up_weak_arrays();
  clean_up_thread_list();
  clean_up_owner_table();
  clean_up_account_hooks();
  repair_finalizer_structs();
  repair_weak_finalizer_structs();
  repair_roots();
  repair_immobiles();
  GC_fixup_variable_stack(GC_variable_stack, 0, gc_stack_base);
  repair_heap();
  clean_up_heap();
  reset_nursery();
  do_btc_accounting();
  if (generations_available)
    protect_old_pages();
  flush_freed_pages();

  /* new we do want the allocator freaking if we go over half */
  in_unsafe_allocation_mode = 0;

  /* update some statistics */
  if(gc_full) num_major_collects++; else num_minor_collects++;
  if(peak_memory_use < memory_in_use) peak_memory_use = memory_in_use;
  if(gc_full)
    since_last_full = 0;
  else if((float)(memory_in_use - old_mem_use) < (0.1 * (float)old_mem_use))
    since_last_full += 1;
  else if((float)(memory_in_use - old_mem_use) < (0.4 * (float)old_mem_use))
    since_last_full += 5;
  else 
    since_last_full += 10;
  if(gc_full)
    last_full_mem_use = memory_in_use;

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



