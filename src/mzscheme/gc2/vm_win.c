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

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif
#ifndef PAGE_ALLOC_STATS
# define PAGE_ALLOC_STATS(x) /* empty */
#endif

static void *malloc_pages(size_t len, size_t alignment)
{
  PAGE_ALLOC_STATS(page_allocations += len);
  PAGE_ALLOC_STATS(page_reservations += len);

  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

static void free_pages(void *p, size_t len)
{
  VirtualFree(p, 0, MEM_RELEASE);

  PAGE_ALLOC_STATS(page_allocations -= len);
  PAGE_ALLOC_STATS(page_reservations -= len);
}

static void flush_freed_pages(void)
{
}

static void protect_pages(void *p, size_t len, int writeable)
{
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

#ifndef DONT_NEED_MAX_HEAP_SIZE
static unsigned long determine_max_heap_size(void)
{
  GCPRINT(GCOUTF, 
	  "Don't know how to get heap size for Windows: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}
#endif
