/* A new accouting precise GC for MzScheme
   Copyright (C) 2001, 2002 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for known improvement points */

#define MZ_PRECISE_GC 1 /* required for mz includes to work right */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc2.h"
#include "../src/schpriv.h"

#define inline /* */

/*****************************************************************************/
/* Collector selection. Change the definitions of these to set or unset the  */
/* particular collector you want.                                            */
/*****************************************************************************/

/* This turns on blame-the-child automatic memory accounting */
/* #define NEWGC_BTC_ACCOUNT */

/* This turns on memory tracing */
/* #define NEWGC_MEMORY_TRACE */

/* This turns on a split generation 0, which might improve performance but
   may not (FIXME: NOT IMPLEMENTED YET) */
/* #define NEWGC_SPLIT_GEN0 */

/* This turns on support for heap debugging (FIXME: NOT IMPLEMENTED YET) */
/* #define NEWGC_HEAP_DEBUGGING */

/* This turns on some internal debugging logs. Don't turn this on unless you
   don't care about performance and you're hacking the collector */
/* #define NEWGC_INTERNAL_DEBUGGING */

/* These set the maximum and minimum size of generation 0. The size is fixed
   upon startup, and set to the closest power of 2 to 1/16th the available
   heap size without going over. Higher values tend to perform better, but
   they do force a fixed, potentially large, memory consumption. */
#define MIN_GEN0_SIZE (8 * 1024 * 1024)
#define MAX_GEN0_SIZE (32 * 1024 * 1024)

/* This computes the top generation collected given a collection number (which
   is zero-based). The collector has 3 generations numbered 0, 1 and 2. */
#define COMPUTE_GCTOP(x) ((x % 100)==0) ? 2 : (((x % 15)==0) ? 1 : 0)

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

/* These are computed from the previous settings. You shouldn't mess with 
   them */
#define PTR(x) ((void*)(x))
#define PPTR(x) ((void**)(x))
#define NUM(x) ((unsigned long)(x))
#define USEFUL_ADDR_BITS ((8 << LOG_WORD_SIZE) - LOG_PAGE_SIZE)
#define ADDR_BITS(x) (NUM(x) >> LOG_PAGE_SIZE)
#define WORD_SIZE (1 << LOG_WORD_SIZE)
#define PAGE_SIZE (1 << LOG_PAGE_SIZE)
#define INCGEN(g) ((g == 2) ? 2 : (g + 1))

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
	  fprintf(stderr, "The system has run out of memory!\n"); abort();
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
/* Allocation                                                                */
/*****************************************************************************/

struct objhead {
  unsigned int clean_heap : 1;
  unsigned int type : 3;
  unsigned int debug_mark : 1;
  unsigned int btc_mark : 1;
  unsigned int mark : 1;
  unsigned int size : LOG_PAGE_SIZE;
};

struct mpage {
  unsigned int size, previous_size;
  unsigned char generation, back_pointers, big_page, page_type;
  struct mpage *next, *prev;
};

#define HEADER_SIZEW gcBYTES_TO_WORDS(sizeof(struct mpage))
#define HEADER_SIZEB gcWORDS_TO_BYTES(HEADER_SIZEW)
#define MAX_OBJECT_SIZEW (gcBYTES_TO_WORDS(PAGE_SIZE) - HEADER_SIZEW - 3)
#define PAGE_TAGGED 0
#define PAGE_ATOMIC 1
#define PAGE_ARRAY 2
#define PAGE_TARRAY 3
#define PAGE_XTAGGED 4
#define PAGE_BIG 5
#define PAGE_TYPES 6
#define GENERATIONS 3

static struct mpage *page_map[1 << USEFUL_ADDR_BITS];
static void **gen0_alloc_region = NULL;
static void **gen0_alloc_current = NULL;
static void **gen0_alloc_end = NULL;
static unsigned long gen0_bigpages_size = 0;
static unsigned long gen0_size = 0;
static struct mpage *pages[GENERATIONS][PAGE_TYPES];
static struct mpage *from_pages[GENERATIONS][PAGE_TYPES];
static struct mpage *to_pages[GENERATIONS][PAGE_TYPES];
static char *zero_sized[4]; /* all 0-sized allocs get this */
static int gc_top = 0;
static Mark_Proc mark_table[NUMBER_OF_TAGS];
static Fixup_Proc fixup_table[NUMBER_OF_TAGS];

#define modify_page_map(page, val) {                                  \
    long size_left = page->big_page ? page->size : PAGE_SIZE;         \
    void *p = page;                                                   \
                                                                      \
    while(size_left > 0) {                                            \
      page_map[ADDR_BITS(p)] = val;                                   \
      size_left -= PAGE_SIZE;                                         \
      p += PAGE_SIZE;                                                 \
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

static void *allocate_big(size_t sizeb, int type)
{
  unsigned long sizew;
  struct mpage *bpage;

  sizew = gcBYTES_TO_WORDS(sizeb) + HEADER_SIZEW + 1;
  sizeb = gcWORDS_TO_BYTES(sizew);
  gen0_bigpages_size += sizeb;
  if(gen0_bigpages_size > gen0_size) {
    garbage_collect(0);
    gen0_bigpages_size = sizeb;
  }

  bpage = malloc_pages(sizeb, PAGE_SIZE);
  bpage->size = sizeb;
  bpage->big_page = 1;
  bpage->page_type = type;
  bpage->next = pages[0][PAGE_BIG];
  if(bpage->next) bpage->next->prev = bpage;
  pages[0][PAGE_BIG] = bpage;

  pagemap_add(bpage);
  return (void*)(NUM(bpage) + HEADER_SIZEB + WORD_SIZE);
}

inline static void *allocate(size_t sizeb, int type)
{
  if(sizeb) {
    size_t sizew = gcBYTES_TO_WORDS(sizeb) + 1;

    if(sizew < MAX_OBJECT_SIZEW) {
      struct objhead *info;
      void *retval = PTR(gen0_alloc_current);

      gen0_alloc_current += sizew;
      if(gen0_alloc_current >= gen0_alloc_end) {
	gen0_alloc_current -= sizew;
	garbage_collect(0);
	retval = gen0_alloc_current;
	gen0_alloc_current += sizew;
      }
      info = (struct objhead *)retval;
      info->type = type;
      info->size = sizew;
      return PTR(NUM(retval) + WORD_SIZE);
    } else return allocate_big(sizeb, type);
  } else return zero_sized;
}

void *GC_malloc(size_t s) { return allocate(s, PAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t s) { return allocate(s, PAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t s) { return allocate(s, PAGE_XTAGGED); }
void *GC_malloc_array_tagged(size_t s) { return allocate(s, PAGE_TARRAY); }
void *GC_malloc_atomic(size_t s) { return allocate(s, PAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t s) { return malloc(s); }
void *GC_malloc_allow_interior(size_t s) {return allocate_big(s, PAGE_ARRAY);}
void GC_free(void *p) {}

inline static void reset_nursery(void)
{
  gen0_bigpages_size = 0;
  bzero(gen0_alloc_region, NUM(gen0_alloc_current) - NUM(gen0_alloc_region));
  gen0_alloc_current = gen0_alloc_region + HEADER_SIZEW;
}

inline static int marked(void *p)
{
  struct mpage *page = find_page(p);
  if(!p || !page) return 0;
  if(page->generation > gc_top) return 1;
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->mark;
}

/*****************************************************************************/
/* Internal Debugging Routines                                               */
/*****************************************************************************/
#ifdef NEWGC_INTERNAL_DEBUGGING
static FILE *dump;

static void init_debug_file(void) 
{
  static int collections = 0;
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
  short i, j;
  
  fprintf(dump, "Generation 0: (%p - %p)\n", gen0_alloc_region, gen0_alloc_end);
  dump_region(gen0_alloc_region, gen0_alloc_current);
  for(i = 0; i < GENERATIONS; i++)
    for(j = 0; j < PAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next) {
	fprintf(dump, "Page %p (gen %i, type %i, big %i, back %i, size %i/%i)\n",
		page, page->generation, page->page_type, page->big_page,
		page->back_pointers, page->size, page->previous_size);
	dump_region(PPTR(page), PPTR(NUM(page) + page->size));
      }
  fflush(dump);
}
#endif

#ifdef NEWGC_INTERNAL_DEBUGGING
# define INIT_DEBUG_FILE() init_debug_file()
# define CLOSE_DEBUG_FILE() close_debug_file()
# define DUMP_HEAP() dump_heap()
# define GC_DEBUG(args...) { fprintf(dump, args); fflush(dump); }
#else
# define INIT_DEBUG_FILE() /* */
# define CLOSE_DEBUG_FILE() /* */
# define DUMP_HEAP() /* */
# define GC_DEBUG(args...) /* */
#endif

#define GCWARN(args...) { fprintf(stderr, args); fflush(stderr); }
#define GCERR(args...) { fprintf(stderr, args); fflush(stderr); abort(); }

/*****************************************************************************/
/* Routines dealing with various runtime execution stacks                    */
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
    if(!naya) GCERR("Couldn't allocate space for root pointers!\n");
    memcpy((void*)naya, (void*)roots, sizeof(unsigned long) * roots_count);
    
    if(roots) free(roots);
    roots = naya;
  }

  roots[roots_count++] = (unsigned long)start;
  roots[roots_count++] = (unsigned long)end - WORD_SIZE;
  nothing_new = 0;
}

#define traverse_roots(muck) {                                             \
    unsigned long j;                                                       \
                                                                           \
    if(roots) {                                                            \
      sort_and_merge_roots();                                              \
      for(j = 0; j < roots_count; j += 2) {                                \
        void **s = (void**)roots[j];                                       \
        void **e = (void**)roots[j + 1];                                   \
                                                                           \
        while(s < e) { muck(*s); s++; }                                    \
      }                                                                    \
    }                                                                      \
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
  if(!ib) GCERR("Couldn't allocate space for immobile box!\n");
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
  GCWARN("Attempted free of non-existent immobile box %p\n", b);
}

#define traverse_immobiles(muck) {       		              \
    struct immobile_box *ib;                                          \
                                                                      \
    for(ib = immobile_boxes; ib; ib = ib->next)                       \
      muck(ib->p);                                                    \
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

#define traverse_finalizers(fnls, muck) {	                      \
    struct finalizer *work;                                           \
                                                                      \
    for(work = fnls; work; work = work->next)                         \
      muck(work);                                                     \
  }

#define mark_all(fnl) { gcMARK(fnl->data); gcMARK(fnl->p); gcMARK(fnl); }
#define mark_data(fnl) { gcMARK(fnl->data); gcMARK(fnl); }
#define repair_next(fnl) { gcFIXUP(fnl->next); }
#define repair_data(fnl) { gcFIXUP(fnl->data); gcFIXUP(fnl->p); }

inline static void mark_finalizers(void)
{
  traverse_finalizers(finalizers, mark_data);
  traverse_finalizers(run_queue, mark_all);
  gcFIXUP(finalizers); gcFIXUP(run_queue);
  traverse_finalizers(finalizers, repair_next);
  traverse_finalizers(run_queue, repair_next);
}

inline static void repair_finalizers(void)
{
  traverse_finalizers(finalizers, repair_data);
  traverse_finalizers(run_queue, repair_data);
}

inline static void check_finalizers(int level)
{
  struct finalizer *work = finalizers, *prev = NULL;

  while(work) {
    if((work->eager_level == level) && !marked(work->p)) {
      struct finalizer *next = work->next;

      GC_DEBUG("CFNL: Level %i finalizer %p on %p queued for finalization.\n",
	       work->eager_level, work, work->p);
      gcMARK(work->p);
      if(prev) prev->next = next;
      if(!prev) finalizers = next;
      if(last_in_queue) last_in_queue = last_in_queue->next = work;
      if(!last_in_queue) run_queue = last_in_queue = work;
      work->next = NULL;

      work = next;
    } else { prev = work; work = work->next; }
  }
}

inline static void do_ordered_level3(void)
{
  struct finalizer *temp;

  for(temp = finalizers; temp; temp = temp->next)
    if(!marked(temp->p)) {
      GC_DEBUG("LVL3: %p is not marked. Marking payload (%p)\n", temp, temp->p);
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

inline static void mark_weak_finalizers(void)
{
  struct weak_finalizer *work;

  for(work = weak_finalizers; work; work = work->next)
    gcMARK(work);
  gcFIXUP(weak_finalizers);
  for(work = weak_finalizers; work; work = work->next)
    gcFIXUP(work->next);
}

inline static void repair_weak_finalizers(void)
{
  struct weak_finalizer *work = weak_finalizers, *prev = NULL;
  
  while(work) {
    if(!marked(work->p)) {
      if(prev) prev->next = work->next;
      if(!prev) weak_finalizers = work->next;
      work = work->next;
    } else { gcFIXUP(work->p); prev = work; work = work->next; }
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
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = wfnl->saved;
    gcMARK(wfnl->saved); wfnl->saved = NULL;
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
static int doing_memory_accounting_pass = 0;

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
  if(!doing_memory_accounting_pass) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }
  return size_weak_box(p);
}

static int repair_weak_box(void *p)
{
  struct weak_box *wb = (struct weak_box *)p;
  gcFIXUP(wb->val);
  gcFIXUP(wb->secondary_erase);
  return size_weak_box(p);
}

inline static void reset_weak_boxes(void)
{
  struct weak_box *work;

  for(work = weak_boxes; work; work = work->next) {
    if(!marked(work->val)) {
      work->val = NULL;
      if(work->secondary_erase)
	*(work->secondary_erase + work->soffset) = NULL;
      work->secondary_erase = NULL;
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
  if(!doing_memory_accounting_pass) {
    wa->next = weak_arrays;
    weak_arrays = wa;
  }
  return size_weak_array(p);
}

static int repair_weak_array(void *p)
{
  struct weak_array *wa = (struct weak_array *)p;
  int i;
  void **data;

  gcFIXUP(wa->replace_val);
  for(data = wa->data, i = wa->count; i--; ) 
    if(data[i]) gcFIXUP(data[i]);
  return size_weak_array(p);
}

inline static void reset_weak_arrays()
{
  struct weak_array *work;
  
  for(work = weak_arrays; work; work = work->next) {
    void **data;
    int i;
    
    for(data = work->data, i = work->count; i--; ) {
      void *p = data[i];
      
      if(p && !marked(p)) {
	data[i] = work->replace_val;
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

inline static void do_thread_register(void *t, void *c)
{
  struct thread *work;

  for(work = threads; work; work = work->next)
    if(work->thread == t) {
      work->owner = current_owner(NULL);
      return;
    }
  work = (struct thread *)malloc(sizeof(struct thread));
  work->owner = current_owner(NULL);
  work->thread = t;
  work->next = threads;
  threads = work;
}

inline static void mark_threads(int owner)
{
  struct thread *work;

  for(work = threads; work; work = work->next)
    if(work->owner == owner)
      gcMARK(work->thread);
}

inline static void repair_thread_list(void)
{
  struct thread *work = threads, *prev = NULL;

  while(work) {
    if(marked(work->thread)) {
      gcFIXUP(work->thread);
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
#endif

#ifdef NEWGC_BTC_ACCOUNT
# define REGISTER_THREAD(t,c) do_thread_register(t,c)
# define REPAIR_THREAD_LIST() repair_thread_list()
#else
# define REGISTER_THREAD(t,c) /* */
# define REPAIR_THREAD_LIST() /* */
#endif

void GC_register_thread(void *t, void *c)
{
  REGISTER_THREAD(t, c);
}

/*****************************************************************************/
/* Internal Stack Routines                                                   */
/*****************************************************************************/
#if defined(NEWGC_BTC_ACCOUNT) || defined(NEWGC_MEMORY_TRACE)
#define STACKLET_SIZE (32 * 1024)
#define NULL 0

struct stacklet {
  struct stacklet *prev;
  void **cur;
  void **start;
  void **end;
};

static struct stacklet *istack = NULL;

static void push_2ptr(void *ptr1, void *ptr2)
{
  if(!istack || (istack->cur == istack->end)) {
    struct stacklet *temp = (struct stacklet *)malloc(STACKLET_SIZE);
    if(temp) {
      temp->prev = istack;
      temp->cur = (void**)((unsigned long)temp + sizeof(struct stacklet));
      temp->start = temp->cur;
      temp->end = (void**)((unsigned long)temp + STACKLET_SIZE);
      istack = temp;
    } else {
      fprintf(stderr, "No room left on stack.\n");
      fprintf(stderr, "Accounting/Debugging information will be incomplete.\n");
    }
  }
  *(istack->cur++) = ptr1;
  *(istack->cur++) = ptr2;
}

static int pop_2ptr(void **ptr1, void **ptr2)
{
  if(istack && (istack->cur == istack->start)) {
    void *temp = istack;
    istack = istack->prev;
    free(temp);
  }
  if(istack) {
    *ptr2 = *(--istack->cur);
    *ptr1 = *(--istack->cur);
    return 1;
  } else return 0;
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
      printf("Something extremely weird (and bad) has happened.\n");
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

inline static void repair_owner_table(void)
{
  int i;

  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i]) {
      /* repair or delete the originator */
      if(!marked(owner_table[i]->originator)) 
	owner_table[i]->originator = NULL;
      else 
	gcFIXUP(owner_table[i]->originator);
      /* potential delete */
      if(i != 1) 
	if((owner_table[i]->memory_use == 0) && !owner_table[i]->originator)
	  free_owner_set(i);
    }
}

inline static unsigned long custodian_get_memory(void *custodian)
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
  int i, j;

  for(i = 0; i < GENERATIONS; i++)
    for(j = 0; j < PAGE_TYPES; j++)
      if(j == PAGE_BIG) {
	for(page = pages[i][j]; page; page = page->next)
	  ((struct objhead *)((char*)page + HEADER_SIZEB))->btc_mark = 0;
      } else {
	for(page = pages[i][j]; page; page = page->next) {
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

inline static void mark_for_accounting(struct mpage *page, void *ptr)
{
  GC_DEBUG("btc_account: %p/%p\n", page, ptr);
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
    case PAGE_TAGGED: if((*(unsigned short*)ptr != scheme_thread_type) &&
                         (*(unsigned short*)ptr != scheme_custodian_type))
                        mark_table[*(unsigned short*)ptr](ptr); 
                      break;
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: { 
      void **temp = ptr, **end = temp + info->size;
      
      while(temp < end) gcMARK(*(temp++));
      break;
    };
    case PAGE_TARRAY: {
      unsigned short tag = *(unsigned short*)ptr;
      void **temp = ptr, **end = temp + info->size;
      
      while(temp < end) temp += mark_table[tag](temp);
      break;
    }
    case PAGE_XTAGGED: GC_mark_xtagged(ptr); break;
  }
}

inline static void mark_big_page(struct mpage *page);

static void propagate_accounting_marks(void)
{
  struct mpage *page;
  void *p;

  while(pop_2ptr((void**)&page, &p)) {
    if(page->big_page)
      mark_big_page(page);
    else
      mark_normal_obj(page, p);
  }
}

static void btc_account(void)
{
  Scheme_Custodian *cur = owner_table[current_owner(NULL)]->originator;
  Scheme_Custodian_Reference *box = cur->global_next;
  int i;
  
  doing_memory_accounting_pass = 1;
  
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
    mark_threads(owner);
    propagate_accounting_marks();
    
    box = cur->global_prev; cur = box ? box->u.two_ptr_val.ptr1 : NULL;
  }
  
  doing_memory_accounting_pass = 0;
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
  struct account_hook *work = malloc(sizeof(struct account_hook));
  work->type = type; work->c1 = c1; work->c2 = c2; work->amount = b;
  work->next = hooks; hooks = work;
}

inline static void repair_account_hooks()
{
  struct account_hook *work = hooks, *prev = NULL;

  while(work) {
    if(!marked(work->c1) || !marked(work->c2)) {
      struct account_hook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) hooks = next;
      free(work);
      work = next;
    } else {
      gcFIXUP(work->c1); gcFIXUP(work->c2);
      prev = work; work = work->next;
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

#endif

long GC_get_memory_use(void *custodian) 
{
  unsigned long retval = 0;

  if(custodian ) {
#ifdef NEWGC_BTC_ACCOUNT
    retval = custodian_get_memory(custodian);
#endif
  } else {
    struct mpage *page;
    unsigned short i, j;

    retval += ((unsigned long)gen0_alloc_current) - 
              ((unsigned long)gen0_alloc_region);
    for(i = 0; i < GENERATIONS; i++)
      for(j = 0; j < PAGE_TYPES; j++)
	for(page = pages[i][j]; page; page = page->next)
	  retval += page->size;
  }

  return retval;
}

#ifdef NEWGC_BTC_ACCOUNT
# define REPAIR_OWNER_TABLE() repair_owner_table()
# define DO_BTC_ACCOUNTING() btc_account()
# define DOING_MEMORY_ACCOUNTING doing_memory_accounting_pass
# define MEMORY_ACCOUNT_MARK(p,o) mark_for_accounting(p, (void*)(o))
# define ADD_ACCOUNT_HOOK(t,c1,c2,b) { add_account_hook(t, c1, c2, b); return 1; }
# define REPAIR_ACCOUNT_HOOKS() repair_account_hooks()
# define RUN_ACCOUNT_HOOKS() run_account_hooks()
#else
# define REPAIR_OWNER_TABLE() /* */
# define DO_BTC_ACCOUNTING() /* */
# define DOING_MEMORY_ACCOUNTING 0
# define MEMORY_ACCOUNT_MARK(p,o) /* */
# define ADD_ACCOUNT_HOOK(a,b,c,d) return 0
# define REPAIR_ACCOUNT_HOOKS() /* */
# define RUN_ACCOUNT_HOOKS() /* */
#endif */

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2)
{
  ADD_ACCOUNT_HOOK(type, c1, c2, b);
}

/*****************************************************************************/
/* administration / initialization                                           */
/*****************************************************************************/

void designate_modified(void *p)
{
  struct mpage *page = find_page(p);

  if(page) {
    protect_pages(page, page->size, 1);
    page->back_pointers = 1;
  } else GCERR("Seg fault (internal error) at %p\n", p);
}

#include "sighand.c"

void GC_init_type_tags(int count, int weakbox)
{
  static int initialized = 0;

  weak_box_tag = weakbox;
  if(!initialized) {
    size_t wanted_gen0_size;

    initialized = 1;
    max_heap_size = determine_max_heap_size();
    pages_in_heap = max_heap_size / PAGE_SIZE;
    max_used_pages = pages_in_heap / 2;
    
    gen0_size = MIN_GEN0_SIZE; wanted_gen0_size = max_heap_size >> 4;
    while((gen0_size < wanted_gen0_size) && (gen0_size < MAX_GEN0_SIZE))
      gen0_size *= 2;
    
    gen0_alloc_region = PPTR(malloc_pages(gen0_size, PAGE_SIZE));
    gen0_alloc_end = PPTR(NUM(gen0_alloc_region) + gen0_size);
    ((struct mpage *)gen0_alloc_region)->size = gen0_size;
    ((struct mpage *)gen0_alloc_region)->big_page = 1;
    pagemap_add((struct mpage *)gen0_alloc_region);
    ((struct mpage *)gen0_alloc_region)->big_page = 0;
    bzero(gen0_alloc_region, gen0_size);
    gen0_alloc_current = PPTR(NUM(gen0_alloc_region) + HEADER_SIZEB);
    
    GC_register_traversers(weakbox, size_weak_box, mark_weak_box,
			   repair_weak_box, 0, 0);
    GC_register_traversers(weak_array_tag, size_weak_array, mark_weak_array,
			   repair_weak_array, 0, 0);
    initialize_signal_handler();
  }
}

static char *type_name[PAGE_TYPES] = { "tagged", "atomic", "array",
				       "tagged array", "xtagged", "big" };
void GC_dump(void)
{
  unsigned long gen0_bigs = 0;
  struct mpage *page;
  int i;

  for(page = pages[0][PAGE_BIG]; page; page = page->next)
    gen0_bigs++;
  GCWARN("Generation 0: %li of %li bytes used (%li in %li bigpages)\n",
	 gen0_bigpages_size + (NUM(gen0_alloc_current)-NUM(gen0_alloc_region)),
	 gen0_bigpages_size + gen0_size, gen0_bigpages_size, gen0_bigs);

  for(i = 1; i < GENERATIONS; i++) {
    unsigned long num_pages = 0, size_pages = 0;
    unsigned long tnum_pages[PAGE_TYPES], tsize_pages[PAGE_TYPES];
    int j;

    bzero(tnum_pages, sizeof(tnum_pages)); 
    bzero(tsize_pages, sizeof(tsize_pages));
    for(j = 0; j < PAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next) {
	num_pages += 1; tnum_pages[j] += 1;
	size_pages += page->size; tsize_pages[j] += page->size;
      }
    GCWARN("Generation %i: %li bytes used in %li pages\n", 
	   i, size_pages, num_pages);
    if(size_pages) 
      for(j = 0; j < PAGE_TYPES; j++) 
	GCWARN("    ... %li %s pages (%li, %2.2f%%)\n",
	       tnum_pages[j], type_name[j], tsize_pages[j],
	       100.0 * ((float)tsize_pages[j] / (float)size_pages));
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

/*****************************************************************************/
/* resolution / repair                                                       */
/*****************************************************************************/

void *GC_resolve(void *p)
{
  struct mpage *page = find_page(p);

  if(!page || page->big_page)
    return p;
  if(((struct objhead *)(NUM(p) - WORD_SIZE))->mark)
    return *(void**)p;
  return p;
}

void GC_fixup(void *pp)
{
  void *p = *(void**)pp;
  
  if(!p || ((long)p & 0x1)) return; else {
    struct mpage *page = find_page(p);

    if(page) {
      if(page->big_page) {
	struct mpage *from_page = find_page(pp);

	if(from_page && !from_page->back_pointers)
	  if(from_page->generation > page->generation)
	    from_page->back_pointers = 1;
      } else if( ((struct objhead *)(NUM(p) - WORD_SIZE))->mark ) {
	struct mpage *from_page = find_page(pp);
	
	p = *(void**)p; *(void**)pp = p; page = find_page(p);
	if(from_page && !from_page->back_pointers)
	  if(from_page->generation > page->generation)
	    from_page->back_pointers = 1;
      } 
    } 
  }
}

inline static void repair_big_page(struct mpage *page) 
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

inline static void repair_tagged_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB);
  void **end = PPTR(NUM(page) + page->size);

  while(start < end) {
    start++; start += fixup_table[*(unsigned short*)start](start);
  }
}

inline static void repair_array_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB);
  void **end = PPTR(NUM(page) + page->size);
  
  while(start < end) {
    struct objhead *ohead = (struct objhead *)start++;
    unsigned long size = ohead->size;
    while(--size) gcFIXUP(*(start++));
  }
}

inline static void repair_tarray_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB);
  void **end = PPTR(NUM(page) + page->size);
  
  while(start < end) {
    struct objhead *ohead = (struct objhead *)start;
    void **tempend = start + ohead->size;
    unsigned short tag = *(unsigned short*)(++start);
    
    while(start < tempend)
      start += fixup_table[tag](start);
  }
}

inline static void repair_xtagged_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB);
  void **end = PPTR(NUM(page) + page->size);

  while(start < end) {
    GC_fixup_xtagged(start + 1);
    start += ((struct objhead *)start)->size;
  }
}

static void repair_page(struct mpage *page)
{
  if(page->big_page) repair_big_page(page); else 
    switch(page->page_type) {
      case PAGE_TAGGED: repair_tagged_page(page); break;
      case PAGE_ATOMIC: break;
      case PAGE_ARRAY: repair_array_page(page); break;
      case PAGE_TARRAY: repair_tarray_page(page); break;
      case PAGE_XTAGGED: repair_xtagged_page(page); break;
    }
}

/*****************************************************************************/
/* marks                                                                     */
/*****************************************************************************/

inline static void mark_normal_object(struct mpage *page, void *p)
{
  struct objhead *ohead = (struct objhead *)(NUM(p) - WORD_SIZE);

  if(!ohead->mark) {
    unsigned long tgen = page ? INCGEN(page->generation) : 1;
    unsigned short type = ohead->type;
    struct mpage *work;
    size_t size, sizeb;
    void **dest, **src;
    void *newplace;

    if(tgen == 1)
      if(type == PAGE_TAGGED)
	if((int)mark_table[*(unsigned short*)p] < PAGE_TYPES)
	  type = ohead->type = (int)mark_table[*(unsigned short*)p];
    work = to_pages[tgen][type]; size = ohead->size; 
    sizeb = gcWORDS_TO_BYTES(size);

    while(work && ((work->size + sizeb) >= PAGE_SIZE))
      work = work->next;

    if(work) newplace = PTR(NUM(work) + work->size); else {
      work = (struct mpage *)malloc_pages(PAGE_SIZE, PAGE_SIZE);
      work->generation = tgen; work->page_type = type; 
      work->size = work->previous_size = HEADER_SIZEB;
      work->next = to_pages[tgen][type]; work->prev = NULL;
      if(work->next) work->next->prev = work;
      to_pages[tgen][type] = work;
      pagemap_add(work);
      newplace = PTR(NUM(work) + HEADER_SIZEB);
    }
    work->size += sizeb;
      
    dest = PPTR(newplace); src = PPTR(ohead);
    while(size--) *(dest++) = *(src++);
   *(void**)p = PTR(NUM(newplace) + WORD_SIZE);
    ohead->mark = 1;
    GC_DEBUG("Marked/copid %i bytes from obj %p to %p\n", ohead->size,
	     p, PTR(NUM(newplace) + WORD_SIZE));
  }
}

inline static void mark_big_object(struct mpage *page)
{
  if(page->big_page == 1) {
    int oldgen = page->generation;
    int newgen = INCGEN(oldgen);

    page->big_page = 2;
    page->previous_size = HEADER_SIZEB;
    page->generation = newgen;

    if(page->prev) page->prev->next = page->next;
    if(!page->prev) from_pages[oldgen][PAGE_BIG] = page->next;
    if(page->next) page->next->prev = page->prev;

    page->next = to_pages[newgen][PAGE_BIG]; page->prev = NULL;
    if(page->next) page->next->prev = page;
    to_pages[newgen][PAGE_BIG] = page;
  }
}

void GC_mark(const void *p)
{
  if(p && !(NUM(p) & 0x1)) {
    struct mpage *page = find_page(PTR(p));

    if(page) {
      if(DOING_MEMORY_ACCOUNTING) MEMORY_ACCOUNT_MARK(page, PTR(p)); else {
	if(page->generation <= gc_top) {
	  if(page->big_page)
	    mark_big_object(page);
	  else
	    mark_normal_object(page, PTR(p));
	}
      }
    }
  }
}

inline static void mark_big_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + HEADER_SIZEB + WORD_SIZE);
  void **end = PPTR(NUM(page) + page->size);

  switch(page->page_type) {
    case PAGE_TAGGED: mark_table[*(unsigned short*)start](start); break;
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: while(start < end) GC_mark(*(start++)); break;
    case PAGE_XTAGGED: GC_mark_xtagged(start); break;
    case PAGE_TARRAY: {
      unsigned short tag = *(unsigned short *)start;
      while(start < end) start += mark_table[tag](start);
      break;
    }
  }
}

inline static void mark_tagged_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);

  while(start < PPTR(NUM(page) + page->size)) {
    start++; start += mark_table[*(unsigned short*)start](start);
  }
}

inline static void mark_array_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);
  
  while(start < PPTR(NUM(page) + page->size)) {
    struct objhead *info = (struct objhead *)start++;
    unsigned long size = info->size;
    while(--size) GC_mark(*(start++));
  }
}

inline static void mark_tarray_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);

  while(start < PPTR(NUM(page) + page->size)) {
    struct objhead *info = (struct objhead *)start;
    void **tempend = start + info->size;
    unsigned short tag = *(unsigned short*)(++start);

    while(start < tempend)
      start += mark_table[tag](start);
  }
}

inline static void mark_xtagged_page(struct mpage *page)
{
  void **start = PPTR(NUM(page) + page->previous_size);
  
  while(start < PPTR(NUM(page) + page->size)) {
    GC_mark_xtagged(start + 1);
    start += ((struct objhead *)start)->size;
  }
}

static void mark_page(struct mpage *page)
{
  if(page->big_page) mark_big_page(page); else 
    switch(page->page_type) {
      case PAGE_TAGGED: mark_tagged_page(page); break;
      case PAGE_ATOMIC: break;
      case PAGE_ARRAY: mark_array_page(page); break;
      case PAGE_TARRAY: mark_tarray_page(page); break;
      case PAGE_XTAGGED: mark_xtagged_page(page); break;
    }
}

/*****************************************************************************/
/* garbage collection                                                        */
/*****************************************************************************/

#define CUTOFF ((PAGE_SIZE >> 1) + (PAGE_SIZE >> 2))

static void prepare_pages_for_collection(void)
{
  struct mpage *work;
  int i, j = 0;

  /* first move pages in generations we're collecting to from_pages */
  for(i = 0; i < GENERATIONS; i++)
    if(i <= gc_top) {
      for(j = 0; j < PAGE_TYPES; j++) {
	from_pages[i][j] = pages[i][j];
	pages[i][j] = NULL;

	for(work = from_pages[i][j]; work; work = work->next)
	  protect_pages(work, work->size, 1);
      }
    } else from_pages[i][j] = NULL;

  /* then move anything from an older generation that either has back pointers
     or has sufficient free space to to_pages */
  for(i = 0; i < GENERATIONS; i++)
    for(j = 0; j < PAGE_TYPES; j++) {
      work = pages[i][j]; to_pages[i][j] = NULL;

      while(work) {
	if(work->back_pointers || ((j != PAGE_BIG) && (work->size < CUTOFF))) {
	  struct mpage *next = work->next;

	  protect_pages(work, work->size, 1);

	  if(work->prev) work->prev->next = work->next;
	  if(!work->prev) pages[i][j] = work->next;
	  if(work->next) work->next->prev = work->prev;

	  work->prev = NULL; work->next = to_pages[i][j];
	  if(work->next) work->next->prev = work;
	  to_pages[i][j] = work;

	  work = next;
	} else work = work->next;
      }
    }

  for(i = 0; i < GENERATIONS; i++)
    for(j = 0; j < PAGE_TYPES; j++) 
      for(work = to_pages[i][j]; work; work = work->next) 
	if(work->back_pointers) {
	  work->previous_size = HEADER_SIZEB;
	  mark_page(work);
	  work->previous_size = work->size;
	}
}

static void propagate_marks(void)
{
  struct mpage *page;
  int i, j, changed;

  do {
    changed = 0;

    for(i = 1; i < GENERATIONS; i++) 
      for(j = 0; j < PAGE_TYPES; j++)
	for(page = to_pages[i][j]; page; page = page->next) 
	  if(page->previous_size != page->size) {
	    mark_page(page);
	    page->previous_size = page->size;
	    changed = 1;
	  }
  } while(changed);
}

static void repair_heap(void)
{
  struct mpage *work;
  int i, j;

  for(i = 1; i < GENERATIONS; i++) 
    for(j = 0; j < PAGE_TYPES; j++) {
      work = to_pages[i][j];

      while(work) {
	struct mpage *next = work->next;

	repair_page(work);
	if(work->big_page) work->big_page = 1;
	work->next = pages[i][j]; work->prev = NULL;
	if(work->next) work->next->prev = work;
	pages[i][j] = work;

	work = next;
      }
    }

  for(i = 0; i < GENERATIONS; i++)
    for(j = 0; j < PAGE_TYPES; j++) {
      work = from_pages[i][j];

      while(work) {
	struct mpage *temp = work;
	work = work->next;
	pagemap_remove(temp);
	free_pages(temp, temp->big_page ? temp->size : PAGE_SIZE);
      }
      from_pages[i][j] = NULL;
    }
  flush_freed_pages();
}

static void protect_older_pages(void)
{
  struct mpage *work;
  int i, j;

  for(i = 1; i < GENERATIONS; i++)
    for(j = 0; j < PAGE_TYPES; j++)
      if(j != PAGE_ATOMIC)
	for(work = pages[i][j]; work; work = work->next)
	  if(work->page_type != PAGE_ATOMIC)
	    protect_pages(work, work->size, 0);
}

static void garbage_collect(int force_full)
{
  static unsigned long number = 0;
  static unsigned int running_finalizers = 0;

  if(force_full)
    gc_top = (GENERATIONS - 1);
  else
    gc_top = COMPUTE_GCTOP(number);
  number++;
  INIT_DEBUG_FILE(); DUMP_HEAP();
  
  if(GC_collect_start_callback)
    GC_collect_start_callback();

  prepare_pages_for_collection();
  mark_finalizers();
  mark_weak_finalizers();
  mark_roots();
  mark_immobiles();
  GC_mark_variable_stack(GC_variable_stack, 0, gc_stack_base);

  propagate_marks();
  check_finalizers(1);
  propagate_marks();
  check_finalizers(2);
  propagate_marks();
  if(gc_top == (GENERATIONS-1)) zero_weak_finalizers();
  do_ordered_level3();
  propagate_marks();
  check_finalizers(3);
  if(gc_top == (GENERATIONS-1)) reset_weak_finalizers();
  propagate_marks();

  repair_finalizers();
  repair_weak_finalizers();
  repair_roots();
  repair_immobiles();
  GC_fixup_variable_stack(GC_variable_stack, 0, gc_stack_base);
  REPAIR_OWNER_TABLE(); REPAIR_THREAD_LIST(); REPAIR_ACCOUNT_HOOKS();
  reset_weak_boxes(); reset_weak_arrays();
  repair_heap();

  reset_nursery();
  DO_BTC_ACCOUNTING();
  protect_older_pages();

  if(GC_collect_start_callback)
    GC_collect_end_callback();

  if(!running_finalizers) {
    running_finalizers = 1;
    while(run_queue) {
      struct finalizer *f;
      void **gcs;

      f = run_queue; run_queue = run_queue->next;
      if(!run_queue) last_in_queue = NULL;

      GC_DEBUG("Running finalizers %p for pointer %p (lvl %i)\n",
	       f, f->p, f->eager_level);
      gcs = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = gcs;
    }
    RUN_ACCOUNT_HOOKS();
    running_finalizers = 0;
  }

  DUMP_HEAP(); CLOSE_DEBUG_FILE();
}



