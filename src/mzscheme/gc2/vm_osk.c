/* 
   Provides:
      allocator
      determine_max_heap_size()
   Requires:
   Optional:
      CHECK_USED_AGAINST_MAX(len)
      PAGE_ALLOC_STATS(expr)
      GCPRINT
      GCOUTF
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <oskit/c/malloc.h>

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif
#ifndef PAGE_ALLOC_STATS
# define PAGE_ALLOC_STATS(x) /* empty */
#endif

inline static void *malloc_pages(size_t len, size_t alignment)
{
  void *p;

  check_used_against_max(len);
  p = smemalign(alignment, len);
  memset(p, 0, len);

  PAGE_ALLOC_STATS(page_allocations += len);
  PAGE_ALLOC_STATS(page_reservations += len);

  return p;
}

static void free_pages(void *p, size_t len)
{
  free_used_pages(len);
  sfree(p, len);

  PAGE_ALLOC_STATS(page_allocations -= len);
  PAGE_ALLOC_STATS(page_reservations -= len);
}

static void flush_freed_pages(void)
{
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
static unsigned long determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, 
	  "Don't know how to get heap size for OSKit: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
