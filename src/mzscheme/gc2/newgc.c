/*
   A new Precise GC for MzScheme
   Copyright (C) 2001 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for possible improvement points
*/
#include <stdlib.h> /* for malloc and friends*/
#include <stdio.h> /* for printf, fprintf, stdout and friends*/
#include <string.h> /* for memmove*/
#include "gc2.h" /* need this for the headers*/
#include "../src/schpriv.h" /* needed for Scheme_Custodian*/

/* 
   There are four possible schemes for memory accounting, and it's
   necessary to choose which one to use at compile time. The base
   is the minimal version, ACCOUNT_INPLACE.

   ACCNT_OFF: Do no memory accounting.
   ACCNT_INPLACE: Do memory accounting inside the objects; this
                    adds little in the way of space requirements
                    but doesn't do a very good job of accounting
   ACCNT_EXTERNAL: The linear time algorithm. This adds an extra
                     word to each tagged object, which can have bad
                     effects on memory use
   ACCNT_PRECISE: A superlinear algorithm that computes exact 
                    accounting information, but is slow and uses (as
                    above) an extra word per tagged object.
*/
#if !defined(ACCNT_OFF) && !defined(ACCNT_EXTERNAL) && !defined(ACCNT_PRECISE)
# define ACCNT_INPLACE
#endif

/*
  The collector can work in non-generational and generational mode.
  It is highly suggested that generations be used, not only for 
  efficiency but also because it's better tested.
  
  If you don't want generations, uncomment the #define below. It is
  highly suggested that you not modify the number, however.
*/
#define GENERATIONS		16

/*
  These are somewhat tuned values which inform us about when the
  collector should run. You probably shouldn't mess with these 
  unless you know what you're doing
*/
#define INIT_HEAP_LIMIT		2000000
#define HEAP_GROW_FACTOR	1.5
#define HEAP_GROW_ADD		500000

/*
  Information about the size of certain system stats. 
*/
#define LOG_WORD_SIZE		2
#define LOG_MPAGE_SIZE		14
#define MAX_HEAP_SIZE		(1024*1024*1024) /* FIXME: Should be syscall*/

/*
  Some typedefs
*/
typedef unsigned long UWORD;
typedef unsigned short Type_Tag;
typedef struct MPage {
  struct MPage *next; 
#ifdef ACCNT_INPLACE
  unsigned long owner;
#endif
  struct {
    unsigned char type : 3;
    unsigned char gen : 4; 
    unsigned char bigpage : 1;
    unsigned char bpointers : 1;
    unsigned char mark : 2;
  } flags;
  unsigned int size;
  unsigned int former_size;
} MPage;

/* 
   Some simple macros 
*/
#define INCGEN(x)		((x==(GENERATIONS-1)) ? x : (x + 1))            
#define PAGE_HEADER_SIZE	gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(sizeof(MPage)))


/*
  Some unavoidably needed forward declarations
*/
static void init(void);
static void garbage_collect(int force_full);
static int is_marked(void *p);

/*
  Tags for the objects defined in here
*/
#define _num_tags			259
#define gc_weak_array_tag		256
#define gc_finalization_tag		257
#define gc_finalization_weak_link_tag	258


/*
  Callbacks to do various things with tagged objects
*/
Size_Proc size_table[_num_tags];
Mark_Proc mark_table[_num_tags];
Fixup_Proc fixup_table[_num_tags];

/*
  Some necessary top-level variables
*/
void **GC_variable_stack;
void *stack_base;
int gc_deny = 0;
#ifdef GENERATIONS
int mark_gen = 0;
#endif
#ifndef ACCNT_OFF
int mark_owner = 0;
#endif


/*
  OS Memory routines, wrapped in all sorts of ifdefs so that we get what
  we need in a somewhat standard manner.
*/

#if _WIN32 /* Windows */

void *malloc_pages(size_t len, size_t alignment) {
  return (void *)VirtualAlloc(NULL, len, MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}
 
void free_pages(void *p, size_t len) {
  VirtualFree(p, 0, MEM_RELEASE);
}

void flush_freed_pages(void) {}

void protect_pages(void *p, size_t len, int writeable) {
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

# define MALLOCATOR_DEFINED
#endif /* Windows */

#if OSKIT /* OSKit */
# include <oskit/c/malloc.h>

void *malloc_pages(size_t len, size_t alignment) {
  void *p;
  p = smemalign(alignment, len);
  memset(p, 0, len);
  return p;
}

void free_pages(void *p, size_t len) {
  sfree(p, len);
}

void flush_freed_pages(void) {}

# define MALLOCATOR_DEFINED
#endif /* OSKit */

#ifndef MALLOCATOR_DEFINED /* Default: mmap */
# include <unistd.h>
# include <fcntl.h>
# include <sys/types.h>
# include <sys/mman.h>
# include <errno.h>

#ifndef MAP_ANON
int fd, fd_created;
#endif

static int page_size; /* OS page size */

void *malloc_pages(size_t len, size_t alignment)
{
  void *r;
  size_t extra = 0;

  if (!page_size)
    page_size = getpagesize();

#ifndef MAP_ANON
  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }
#endif

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  /* May need to try twice to get a desired alignment: */
 try_again:

#ifdef MAP_ANON
  r = mmap(NULL, len + extra, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#else
  r = mmap(NULL, len + extra, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
#endif

  if (r  == (void *)-1)
    return NULL;

  if (extra) {
    /* We allocated too large so we can choose the alignment. */
    void *real_r;
    long pre_extra;

    real_r = (void *)(((unsigned long)r+(alignment-1))&(~(alignment-1)));
    
    pre_extra = real_r - r;
    if (pre_extra)
      if (munmap(r, pre_extra))
	fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", 
		(long)r, pre_extra, errno);
    if (pre_extra < extra)
      if (munmap(real_r + len, extra - pre_extra))
	fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", 
		(long)r, pre_extra, errno);
    r = real_r;
  }

  if (alignment && ((unsigned long)r & (alignment - 1))) {
    /* Need better alignment. Try harder. */
    munmap(r, len);
    extra = alignment;
    goto try_again;
  }

  return r;
}

#define BLOCKFREE_CACHE_SIZE 10
static void *blockfree_ptrs[BLOCKFREE_CACHE_SIZE];
static long blockfree_sizes[BLOCKFREE_CACHE_SIZE];

void free_pages(void *p, size_t len)
{
  int i;

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  /* Try to free pages in larger blocks, since the OS may be slow. */

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (p == blockfree_ptrs[i] + blockfree_sizes[i]) {
      blockfree_sizes[i] += len;
      return;
    }
    if (p + len == blockfree_ptrs[i]) {
      blockfree_ptrs[i] = p;
      blockfree_sizes[i] += len;
      return;
    }
  }

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (!blockfree_ptrs[i]) {
      blockfree_ptrs[i] = p;
      blockfree_sizes[i] = len;
      return;
    }
  }

  if (munmap(p, len)) {
    fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", (long)p, (long)len, errno);
  }
}

void flush_freed_pages(void)
{
  int i;

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree_ptrs[i]) {
      if (munmap(blockfree_ptrs[i], blockfree_sizes[i])) {
	fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", 
		(long)blockfree_ptrs[i], blockfree_sizes[i],
		errno);
      }
      blockfree_ptrs[i] = NULL;
      blockfree_sizes[i] = 0;
    }
  }
}

void protect_pages(void *p, size_t len, int writeable)
{
  if (len & (page_size - 1)) {
    len += page_size - (len & (page_size - 1));
  }

  mprotect(p, len, (writeable ? (PROT_READ | PROT_WRITE) : PROT_READ));
}

#endif

/*
  The mid-level allocator. A couple routines which add a layer between the
  OS stuff we just did and the allocators used by the system below. It adds
  a little complication but makes it easier to keep track of some information
*/
#define MPAGE_SIZE		(1 << LOG_MPAGE_SIZE)
#define PAGES_IN_HEAP		(MAX_HEAP_SIZE / MPAGE_SIZE)
#define MAX_USED_PAGES		(PAGES_IN_HEAP / 2)

static MPage *dead_pages = NULL;
static UWORD used_pages = 0;

static void *malloc_mempages(UWORD size) {
  UWORD numpages = (size / MPAGE_SIZE) + (((size % MPAGE_SIZE) == 0) ? 0 : 1);
  
  if((used_pages + numpages) > MAX_USED_PAGES) {
    garbage_collect(0);
    if((used_pages + numpages) > MAX_USED_PAGES) {
      garbage_collect(1);
      if((used_pages + numpages) > MAX_USED_PAGES) {
	if (GC_out_of_memory)
	  GC_out_of_memory();
	
	printf("Out of memory\n");
	abort();
      }
    }
  }

  used_pages += numpages;
  if((size == MPAGE_SIZE) && dead_pages) {
    MPage *retval = dead_pages;
    dead_pages = retval->next;
    bzero(retval, MPAGE_SIZE);
    return retval;
  } else {
    void *m;
    int i = 5;

    while (i--) {
      m = malloc_pages(size, MPAGE_SIZE);
      if (m) {
	bzero(m, size);
	return m;
      } else garbage_collect(1);
    }

    if (GC_out_of_memory)
      GC_out_of_memory();
    
    printf("Out of memory\n");
    abort();
  }
  printf("Extremely bad state in malloc_mempages!\n");
  abort();
  return NULL;
}

static void free_mempages(MPage *page) {
  if(page->flags.bigpage) {
    UWORD numpages = (page->size/MPAGE_SIZE)+(((page->size%MPAGE_SIZE)==0)?0:1);
    free_pages(page, page->size);
    used_pages -= numpages;
  } else {
    page->next = dead_pages;
    dead_pages = page;
    used_pages -= 1;
  }
}

static UWORD midlevel_room_left(void) {
  return (MAX_USED_PAGES - used_pages) * MPAGE_SIZE;
}

/*
  The MPage routines, including the two main allocators
*/
#define MPAGE_SIZE	        (1 << LOG_MPAGE_SIZE)

/* 
   The types for the pages
*/
#define MPAGE_TAGGED			0
#define MPAGE_ATOMIC			1
#define MPAGE_ARRAY			2
#define MPAGE_TARRAY			3
#define MPAGE_XTAGGED			4


#define OFFSET_BITS		(LOG_MPAGE_SIZE - LOG_WORD_SIZE)
#define OWNER_BITS		((1 << (LOG_WORD_SIZE+3)) - (OFFSET_BITS + 1))

#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
/*
  In either of the external owner set schemes, each object has some
  information about it tacked onto the front which contains information
  about the objects size, whether it's marked and its owner. 
*/
typedef struct ObjHeader {
  unsigned int owner : OWNER_BITS;
  unsigned int mark : 1;
  unsigned int size : OFFSET_BITS;
} ObjHeader;
#else
/*
  In the non-external owner set schemes, we still need a word up front
  for untagged objects but don't need it for tagged objects; this also
  somewhat complicates marking, for which the following macros are used
*/
#define TAGGED_MARKED_P(x)	((x) & (1 << 15))
#define TAGGED_MARK(x)		((x) | (1 << 15))
#define TAGGED_UNMARK(x)	((x) & ~(1 << 15))
#define UNTAGGED_MARKED_P(x)	((x) & (1 << 31))
#define UNTAGGED_MARK(x)	((x) | (1 << 31))
#define UNTAGGED_UNMARK(x)	((x) & ~(1 << 31))
#endif

/*
  This is a little table of the generations, which allows for easy, quick
  access.
*/
#ifdef GENERATIONS
MPage *gen[GENERATIONS][6];
#else
MPage *gen[1][6];
#endif

/*
  This is the list of pages we're collecting
*/
static MPage *collect_pages = NULL;

/* 
   For those strange places where someone wants an object of size zero,
   here's what we return:
*/
static char zero_sized[4];

/* 
   this is a constant which defines the maximum size for an object which
   is going to be allocated to a normal page
*/
#define MAX_NORMAL_SIZE		(MPAGE_SIZE - PAGE_HEADER_SIZE)

/*
  Information about our current heap size, all in bytes
*/
UWORD heap_size = 0;
UWORD size_to_collect_at = INIT_HEAP_LIMIT;

/*
  Often we need to look up what page a given pointer is on. Since we
  do this a lot, the added efficiency of the following makes up for
  the somewhat huge space inefficiencies we're adding here
*/
#define WORD_SIZE_IN_BITS	(1 << (LOG_WORD_SIZE + 3))
#define DEAD_BITS		LOG_MPAGE_SIZE
#define USEFUL_BITS		(WORD_SIZE_IN_BITS - DEAD_BITS)
#define FLINDEX_SIZE		(USEFUL_BITS >> 1)
#define FLINDEX_SHIFT		(WORD_SIZE_IN_BITS - FLINDEX_SIZE)
#define SLINDEX_SIZE		(USEFUL_BITS - FLINDEX_SIZE)
#define SLINDEX_SHIFT		DEAD_BITS
#define SLINDEX_MASK		((1 << SLINDEX_SIZE) - 1)
#define FLINDEX(x)		((UWORD)x >> FLINDEX_SHIFT)
#define SLINDEX(x)		(((UWORD)x >> SLINDEX_SHIFT) & SLINDEX_MASK)

MPage *mpage_map[1 << FLINDEX_SIZE][1 << SLINDEX_SIZE];

static void add_to_page_map(MPage *work) {
  MPage *cur = work;
  long size_left = work->flags.bigpage ? work->size : MPAGE_SIZE;
  
  while(size_left > 0) {
    mpage_map[FLINDEX(cur)][SLINDEX(cur)] = work;
    size_left -= MPAGE_SIZE;
    cur = (MPage*)((UWORD)cur + MPAGE_SIZE);
  }
}

#define find_page(p)		(mpage_map[FLINDEX(p)][SLINDEX(p)])

static void rem_from_page_map(MPage *work) {
  MPage *cur = work;
  long size_left = work->flags.bigpage ? work->size : MPAGE_SIZE;

  while(size_left > 0) {
    mpage_map[FLINDEX(cur)][SLINDEX(cur)] = NULL;
    size_left -= MPAGE_SIZE;
    cur = (MPage*)((UWORD)cur + MPAGE_SIZE);
  }
}

/* 
   When we start up, the mpage_map variable is not guaranteed by the
   C standard to be zero-filled. Therefor, we have to make sure what
   we have is clean. Unfortunately, since running init() is not 
   necessarily triggered by the first allocation, we may have to go
   back and readd some pages
*/
static void initialize_mpage_subsystem(void) {
  MPage *work;
  int i;

  bzero(mpage_map, (1<<FLINDEX_SIZE)*(1<<SLINDEX_SIZE)*sizeof(MPage*));
  /* now go back and fix things up*/
  for(i = 0; i < 6; i++) {
    for(work = gen[0][i]; work; work = work->next) {
      add_to_page_map(work);
    }
  }
}

/* 
   This is the "main" allocator, in that it is used by all the hooks into
   the external system. It's declared 'inline', which may blow up space 
   usage but hopefully will cut down on some time.
*/
inline void *alloc_new_bits(size_t size_in_bytes, int type, int force_big) {
  MPage *page;
  void *retval;
  long size_in_words = gcBYTES_TO_WORDS(size_in_bytes);

  if(!size_in_bytes) {
    return zero_sized;
  }

#if defined(ACCNT_EXTERNAL) || defined(ACCNT_EXACT)
  /* 
     If we're keeping external owner information, we always want to
     add a word for this.
  */
  size_in_words += 1;
#else
  /* 
     Otherwise, we only want to add one if this is an untagged object
  */
  if(type) size_in_words += 1;
#endif

  size_in_bytes = gcWORDS_TO_BYTES(size_in_words);

  /* see if we should stop and do a collection */
  heap_size += size_in_bytes;
  if(heap_size > size_to_collect_at)
    garbage_collect(0);

  /* see if we should be allocating this as a big page or a normal page */
  if((size_in_bytes > MAX_NORMAL_SIZE) || force_big) {
    page = (MPage*)malloc_mempages(size_in_bytes+PAGE_HEADER_SIZE);
    page->next = gen[0][5];
/*  page->owner = 0 */
/*  page->flags.gen = 0; */
    page->flags.bigpage = 1;
    page->flags.type = type;
/*  page->flags.mark = 0; */
    page->size = size_in_bytes + PAGE_HEADER_SIZE;
/*  page->former_size = 0; */
    add_to_page_map(page);
    gen[0][5] = page;
    retval = (void*)((UWORD)page + PAGE_HEADER_SIZE);
  } else {
    page = gen[0][type];

    /* FIXME: Choose which one to use! */
    while(page && ((page->size + size_in_bytes) >= MPAGE_SIZE))
      page = page->next;
/*     if(page && ((page->size + size_in_bytes) >= MPAGE_SIZE)) */
/*       page = NULL; */

    if(page) {
      /* add this to a pre-existing page */
      retval = (void*)((UWORD)page + page->size);
      page->size += size_in_bytes;
    } else {
      /* we need to add a new page */
      page = (MPage*)malloc_mempages(MPAGE_SIZE);
      page->next = gen[0][type];
/*    page->owner = 0; */
/*    page->flags.bigpage = 0; */
      page->flags.type = type;
/*    page->flags.mark = 0; */
      page->size = PAGE_HEADER_SIZE + size_in_bytes;
/*    page->former_size = 0; */
      add_to_page_map(page);
      gen[0][type] = page;
      retval = (void*)((UWORD)page + PAGE_HEADER_SIZE);
    }
  }
  
/*
  If we need header information, add it and bump the pointer
*/
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
 {
   ObjHeader *info = (ObjHeader*)retval;
   info->owner = 0; /* FIXME: Use a real owner? */
   info->mark = 0;
   info->size = size_in_words;
   retval = (void*)((UWORD)retval + 4);
 }
#else
  if(type) {
    *(UWORD*)retval = size_in_words;
    retval = (void*)((UWORD)retval + 4);
  }
#endif

  return retval;
}

/* this is a performance hack, and a serious one at that.*/
typedef struct MPageList {
  MPage *page;
  struct MPageList *next;
} MPageList;

static MPageList *mucked_pages = NULL;

void add_to_mucked_pages(MPage *page) {
  MPageList *work = mucked_pages, *prev = NULL;

  if(work) {
    while(work) {
      if(work->page == page) {
	if(prev) {
	  prev->next = work->next;
	  work->next = mucked_pages;
	  mucked_pages = work;
	} 
	return;
      } else {
	prev = work;
	work = work->next;
      }
    }
    work = (MPageList*)malloc(sizeof(MPageList));
    work->page = page;
    work->next = mucked_pages;
    mucked_pages = work;
  } else {
    mucked_pages = (MPageList*)malloc(sizeof(MPageList));
    mucked_pages->page = page;
    mucked_pages->next = NULL;
  }
}

/*
  This is the "allocator" used by the mark phase of the collector to
  create new space in the to-space. It tries a little harder to not
  fragment than the previous one, you'll notice
*/
/* FIXME: inline? */
void *copy_bits(void *oldptr, UWORD size_in_words, int type, int tgen
#ifdef ACCNT_INPLACE
		, UWORD owner 
#endif
		) 
{
  UWORD size_in_bytes = gcWORDS_TO_BYTES(size_in_words);
  MPage *work = gen[tgen][type];
  void *retval;

#ifdef ACCNT_INPLACE
  while(work && !(((work->size + size_in_bytes) < MPAGE_SIZE)
		  && (work->owner == owner)))
    work = work->next;
#else
  while(work && ((work->size + size_in_bytes) >= MPAGE_SIZE)) 
    work = work->next;
#endif

  if(work) {
    /* We're putting this on a pre-existing page. Good */
    retval = (void*)((UWORD)work + work->size);
    memcpy(retval, oldptr, size_in_bytes);
    work->size += size_in_bytes;
    add_to_mucked_pages(work);
    return retval;
  } else {
    /* We need to add a new page */
    work = (MPage*)malloc_mempages(MPAGE_SIZE);
    work->next = gen[tgen][type];
#ifdef ACCNT_INPLACE
    work->owner = owner;
#endif
    work->flags.gen = tgen;
/*  work->flags.bigpage = 0; */
    work->flags.type = type;
/*  work->flags.mark = 0; */
    work->size = PAGE_HEADER_SIZE + size_in_bytes;
    work->former_size = PAGE_HEADER_SIZE;
    add_to_page_map(work);
    gen[tgen][type] = work;
    retval = (void*)((UWORD)work + PAGE_HEADER_SIZE);
    memcpy(retval, oldptr, size_in_bytes);
    add_to_mucked_pages(work);
    return retval;
  }
}

static char *flags_to_strtype(MPage *page) {
  if(page->flags.bigpage) {
    switch(page->flags.type) {
    case MPAGE_TAGGED: return "BTGD";
    case MPAGE_ATOMIC: return "BATM";
    case MPAGE_ARRAY: return "BARR";
    case MPAGE_TARRAY: return "BTAR";
    case MPAGE_XTAGGED: return "BXTG";
    }
  } else {
    switch(page->flags.type) {
    case MPAGE_TAGGED: return "TAGD";
    case MPAGE_ATOMIC: return "ATOM";
    case MPAGE_ARRAY: return "ARRY";
    case MPAGE_TARRAY: return "TARR";
    case MPAGE_XTAGGED: return "XTGD";
    }
  }
  return "XXXX";
}

static void dump_mpage_information(FILE *file) {
  MPage *work;
  int i = 0, j;
  char *format = "%10p                  %i      %5i    %3s (%i)\n";
  UWORD pages = 0, possible = 0;

  fprintf(file, "Memory page information:\n");
  fprintf(file, "Page:                       Gen:   Size:    Type:\n");
  fprintf(file, "-------------------------------------------------\n");
#ifdef GENERATIONS  
  for(i = 0; i < GENERATIONS; i++) {
#endif
    for(j = 0; j < 6; j++) {
      for(work = gen[i][j]; work; work = work->next) {
	fprintf(file, format, work, work->flags.gen, work->size, 
		flags_to_strtype(work), work->flags.bpointers);
	pages++;
	if(work->flags.bigpage) {
	  int blocks = (work->size/MPAGE_SIZE)+(((work->size%MPAGE_SIZE)>0)?1:0);
	  possible += (blocks * MPAGE_SIZE);
	} else possible += MPAGE_SIZE;
      }
    }
#ifdef GENERATIONS
  }
#endif
  fprintf(file, "\n");
  fprintf(file, "%li pages using %li of %li bytes (%2.1f%% wasted)\n",
	  pages, heap_size, 
	  possible, 100 - (((float)heap_size / (float)possible) * 100));
}

/*
  The routines and data structures to implement owner sets. 
*/

#ifndef ACCNT_OFF

typedef struct CustodianList {
  Scheme_Custodian *cust;
  struct CustodianList *next;
} CustodianList;

typedef struct UnionList {
  unsigned int with_owner;
  unsigned int result;
  struct UnionList *next;
} UnionList;

typedef struct OwnerTableEntry {
  Scheme_Custodian *creator;
  CustodianList *custs;
  UnionList *unions;
  UWORD mem_use[GENERATIONS];
} OwnerTableEntry;

OwnerTableEntry *owner_table[(1 << OWNER_BITS)];
int owner_table_top = 1;

void dump_ownerset_information(FILE *file) {
  int i;

  fprintf(file, "\nOwner Set Information:\n");
  fprintf(file, "Owner table entry:         Type:         Memory usage:\n");
  fprintf(file, "--------------------------------------------------------\n");
  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      int j;
      long memsize = 0;
      for(j = 0; j < GENERATIONS; j++) {
	memsize += owner_table[i]->mem_use[j];
      }
      fprintf(file, "%010i                 %10s    %li\n", i,
	      owner_table[i]->creator ? "Singular  " : "Unioned   ",
	      memsize);
    }
  }
}

UWORD custodian_to_ownerset(Scheme_Custodian *cust) {
  UWORD i, j;

  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i] && (owner_table[i]->creator == cust)) {
      return i;
    }
  }

  for(i = 0; i < (1 << OWNER_BITS); i++) {
    if(!owner_table[i]) {
      CustodianList *custs = NULL;
      Scheme_Custodian *cur = cust;

      while(cur) {
	CustodianList *temp = (CustodianList*)malloc(sizeof(CustodianList));
	Scheme_Object *box;
	temp->cust = cur;
	temp->next = custs;
	custs = temp;
	box = (Scheme_Object*)cur->parent;
	cur = box ? (Scheme_Custodian*)box->u.two_ptr_val.ptr1 : NULL;
      }

      owner_table[i] = (OwnerTableEntry*)malloc(sizeof(OwnerTableEntry));
      owner_table[i]->creator = cust;
      owner_table[i]->custs = custs;
      owner_table[i]->unions = NULL;
      for(j = 0; j < GENERATIONS; j++) {
	owner_table[i]->mem_use[j] = 0;
      }
      if(i >= owner_table_top) {
	owner_table_top = i + 1;
      }
      return i;
    }
  }

  fprintf(stderr, "No more room for custodians. Accounting is dying.\n");
  return 0;
}

#define ownerset_account_memory(o, g, s) owner_table[o]->mem_use[g] += s

int ownerset_member_p(Scheme_Custodian *cust, UWORD set) {
  CustodianList *work;

  for(work = owner_table[set]->custs; work; work = work->next) {
    if(work->cust == cust) {
      return 1;
    }
  }
  return 0;
}

int ownerset_subset_p(UWORD set1, UWORD set2) {
  CustodianList *work;

  for(work = owner_table[set1]->custs; work; work = work->next) {
    if(!ownerset_member_p(work->cust, set2)) {
      return 0;
    }
  }
  return 1;
}

int ownerset_union(UWORD set1, UWORD set2) {
  UnionList *work;
  UWORD res;
  
  work = owner_table[set1]->unions;
  while(work) {
    if(work->with_owner == set2) {
      return work->result;
    } else work = work->next;
  }

  /* damn, this isn't memoized. See if one is a subset of the other */
  if(ownerset_subset_p(set1, set2)) {
    /* yup. 1 is a subset of 2 */
    res = set2;
  } else if(ownerset_subset_p(set2, set1)) {
    /* yup. 2 is a subset of 1 */
    res = set1;
  } else {
    /* nope. */
    CustodianList *work = NULL;
    CustodianList *combo = NULL;
    UWORD i;

    /* add every element in set2 */
    for(work = owner_table[set2]->custs; work; work = work->next) {
      CustodianList *temp = (CustodianList*)malloc(sizeof(CustodianList));
      temp->cust = work->cust;
      temp->next = combo;
      combo = temp;
    }
    /* add every element in set1 that's not in set2 */
    for(work = owner_table[set1]->custs; work; work = work->next) {
      if(!ownerset_member_p(work->cust, set2)) {
	CustodianList *temp = (CustodianList*)malloc(sizeof(CustodianList));
	temp->cust = work->cust;
	temp->next = combo;
	combo = temp;
      }
    }

    res = 0;
    while(res < (1 << OWNER_BITS)) {
      if(!owner_table[res]) {
	break;
      }
      if(++res == (1 << OWNER_BITS)) {
	fprintf(stderr, "Out of owner slots. Accounting going bad.\n");
	return 0;
      }
    }
    owner_table[res] = (OwnerTableEntry*)malloc(sizeof(OwnerTableEntry));
    owner_table[res]->creator = NULL;
    owner_table[res]->custs = combo;
    owner_table[res]->unions = NULL;
    for(i = 0; i < GENERATIONS; i++) {
      owner_table[res]->mem_use[i] = 0;
    }
    if(res >= owner_table_top) {
      owner_table_top = res + 1;
    }
  }

  /* res is now the correct resultant value. Tack on some things 
     so that we can just pull this as memoized next time */
  work = (UnionList*)malloc(sizeof(UnionList));
  work->with_owner = set2;
  work->result = res;
  work->next = owner_table[set1]->unions;
  owner_table[set1]->unions = work;
  work = (UnionList*)malloc(sizeof(UnionList));
  work->with_owner = set1;
  work->result = res;
  work->next = owner_table[set2]->unions;
  owner_table[set2]->unions = work;
  
  /* now that's all taken care of, so return */

  return res;
}

#define ownerset_eq(own1,own2)		(own1 == own2)

UWORD current_owner() { 
  static int initialized = 0;
  
  if(initialized) {
    Scheme_Custodian *c;
    c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
    return custodian_to_ownerset(c);
  } else if(scheme_current_thread && scheme_current_thread->config) {
    OwnerTableEntry *oldent = owner_table[0];
    UWORD res;
    
    initialized = 1;
    owner_table[0] = NULL;
    res = current_owner();
    owner_table[0]->mem_use[0] = oldent->mem_use[0];
    free(oldent);
    init();
    return res;
  } else {
    if(!owner_table[0]) {
      UWORD i;
      owner_table[0] = (OwnerTableEntry*)malloc(sizeof(OwnerTableEntry));
      owner_table[0]->creator = NULL;
      owner_table[0]->custs = NULL;
      owner_table[0]->unions = NULL;
      for(i = 0; i < GENERATIONS; i++) {
	owner_table[0]->mem_use[i] = 0;
      }
    }
    return 0;
  }
}

void prep_ownerset_information(int mark_gen) {
  UWORD i, j;

  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      for(j = 0; j <= mark_gen; j++) {
	owner_table[i]->mem_use[j] = 0;
      }
    }
  }
}


void ownerset_delete(UWORD i) {
  CustodianList *ctemp = owner_table[i]->custs;
  UnionList *utemp = owner_table[i]->unions;

  while(ctemp) {
    CustodianList *temp = ctemp;
    ctemp = ctemp->next;
    free(temp);
  }
  while(utemp) {
    UnionList *temp = utemp;
    utemp = utemp->next;
    free(temp);
  }
  free(owner_table[i]);
  owner_table[i] = NULL;
}

void fixup_ownersets() {
  UWORD i;

  /* First, clear out any entries whose creators are custodians which
     no longer exist and fixup the ones that do */
  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i] && owner_table[i]->creator) {
      if(is_marked(owner_table[i]->creator)) {
	gcFIXUP(owner_table[i]->creator);
      } else {
	ownerset_delete(i);
      }
    }
  }

  /* Then clear out all the entries that contain unmarked custodians
     in their list and/or fixup the good custodians */
  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      CustodianList *temp;
      for(temp = owner_table[i]->custs; temp; temp = temp->next) {
	if(is_marked(temp->cust)) {
	  gcFIXUP(temp->cust);
	} else {
	  ownerset_delete(i);
	  temp = NULL;
	}
      }
    }
  }

  /* Finally, clear out any unions that involve a set we've nulled out*/
  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      UnionList *temp = owner_table[i]->unions;
      UnionList *prev = NULL;
      
      while(temp) {
	if(!owner_table[temp->with_owner] || !owner_table[temp->result]) {
	  if(prev) {
	    UnionList *cur = temp;
	    prev->next = temp = temp->next;
	    free(cur);
	  } else {
	    UnionList *cur = temp;
	    owner_table[i]->unions = temp = temp->next;
	    free(cur);
	  }
	} else temp = temp->next;
      }
    }
  }
}

UWORD get_custodian_mem_usage(Scheme_Custodian *cust) {
  UWORD i;
  long res = 0;

  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      if(ownerset_member_p(cust, i)) {
	UWORD j;
	for(j = 0; j < GENERATIONS; j++) {
	  res += owner_table[i]->mem_use[j];
	}
      }
    }
  }
  return res;
}

#endif /* (#ifndef ACCNT_OFF) */

/* 
   The hooks into the accounting system
*/

typedef struct AccountHook {
  unsigned short type; 
  int owner;
  Scheme_Custodian *cust;
  unsigned long bytes;
  Scheme_Object *f;
  struct AccountHook *next;
} AccountHook;

AccountHook *account_hooks = NULL;
UWORD total_requires = 0;

void mark_account_hooks(void) {
#ifndef ACCNT_OFF
  AccountHook *cur = account_hooks;
  
  while(cur) {
    mark_owner = cur->owner;
    gcMARK(cur->f);
    cur = cur->next;
  }
#endif
}

void fixup_account_hooks(void) {
#ifndef ACCNT_OFF
  AccountHook *cur, *prev;

  cur = account_hooks;
  prev = NULL;
  while(cur) {
    if((!is_marked(cur->cust) && (cur->type == MZACCT_LIMIT))
       || !owner_table[cur->owner]) {
      if(cur->type == MZACCT_REQUIRE) {
	total_requires -= cur->bytes;
      }
      if(prev) {
	prev->next = cur->next;
	free(cur);
	cur = prev->next;
      } else {
	account_hooks = cur->next;
	free(cur);
	cur = account_hooks;
      }
    } else {
      gcFIXUP(cur->cust);
      gcFIXUP(cur->f);
      prev = cur;
      cur = cur->next;
    }
  }
#endif
}

void run_applicable_account_hooks(void) {
#ifndef ACCNT_OFF
  AccountHook *cur;

  for(cur = account_hooks; cur; cur = cur->next) {
    if(cur->type == MZACCT_REQUIRE) {
      if(midlevel_room_left() <= total_requires) {
	_scheme_apply(cur->f, 0, NULL);
      }
    } else if(cur->type == MZACCT_LIMIT) {
      if(get_custodian_mem_usage(cur->cust) >= cur->bytes) {
	_scheme_apply(cur->f, 0, NULL);
      }
    } else {
      fprintf(stderr, "Unknown type on account hook (%i)\n", cur->type);
      abort();
    }
  }
#endif
}

/*
  These are the routines which do marking. This is a bit complicated due
  to the fact that we're allowing for different levels of accounting.
*/
#define MARK_WHITE	0
#define MARK_GRAY	1
#define MARK_BLACK	2

int is_marked(void *p) {
  MPage *page = find_page(p);

  if(!page) return 0;
#ifdef GENERATIONS
  if(page->flags.gen > mark_gen) return 1;
#endif
  if(page->flags.bigpage) return page->flags.mark;

#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
  {
    ObjHeader *info = (ObjHeader*)((UWORD)p - 4);
    return info->mark;
  }
#else
  if(page->flags.type) {
    UWORD size = *(UWORD*)((UWORD)p - 4);
    return UNTAGGED_MARKED_P(size);
  } else {
    Type_Tag tag = *(Type_Tag*)p;
    return TAGGED_MARKED_P(tag);
  }
#endif
}

void GC_mark(const void *p) {
  MPage *page;

  if(!p || ((UWORD)p & 0x1))
    return;

  /* 
     if we can't find the page, either this is a pointer that's not into
     our heap or into an older generation. In either case, we don't need
     to do anything.
  */
  page = find_page((void*)p);
  if(!page) return;
#ifdef GENERATIONS
  if(page->flags.gen > mark_gen) return;
#endif

  if(page->flags.bigpage) {
    /*
      We're on a bigpage. We need to mark this, figure out whatever needs
      to be done for the owner information, and then move this page from
      the collect_pages list and onto the proper, saved list
     */
    switch(page->flags.mark) {
      case MARK_WHITE: {
	MPage *temp = collect_pages;

	fflush(stdout);
#ifndef ACCNT_OFF
# ifdef ACCNT_INPLACE
	temp->owner = mark_owner;
# else
	((ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE))->owner = mark_owner;
# endif
#endif	
	page->flags.mark = MARK_GRAY;
	page->flags.gen = INCGEN(page->flags.gen);
#ifndef ACCNT_OFF
	ownerset_account_memory(mark_owner, page->flags.gen, page->size);
#endif
	while(temp && (temp->next != page)) 
	  temp = temp->next;
	if(temp) {
	  temp->next = page->next;
	  page->next = gen[page->flags.gen][5];
	  gen[page->flags.gen][5] = page;
	} else if(collect_pages == page) {
	  collect_pages = page->next;
	  page->next = gen[page->flags.gen][5];
	  gen[page->flags.gen][5] = page;
	} else {
	  fprintf(stderr, "Unexpected case in mark of big page (%p)!\n", page);
	  abort();
	}
	add_to_mucked_pages(page);
	break;
      }

      case MARK_GRAY: {
#ifndef ACCNT_OFF
	UWORD owner;
# ifdef ACCNT_INPLACE
	owner = page->owner;
# else
	owner = ((ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE))->owner;
# endif	
	if(!ownerset_eq(owner, mark_owner)) {
	  ownerset_account_memory(owner, page->flags.gen, -page->size);
	  owner = ownerset_union(owner, mark_owner);
# ifdef ACCNT_INPLACE
	  page->owner = owner;
# else
	  ((ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE))->owner = owner;
# endif	
	  ownerset_account_memory(owner, page->flags.gen, page->size);
	}
#endif
      }
	
      case MARK_BLACK: {
#ifndef ACCNT_OFF
	UWORD owner;
# ifdef ACCNT_INPLACE
	owner = page->owner;
# else
	owner = ((ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE))->owner;
# endif	
	if(!ownerset_eq(owner, mark_owner)) {
	  ownerset_account_memory(owner, page->flags.gen, -page->size);
	  owner = ownerset_union(owner, mark_owner);
# ifdef ACCNT_INPLACE
	  page->owner = owner;
# else
	  ((ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE))->owner = owner;
# endif	
	  ownerset_account_memory(owner, page->flags.gen, page->size);
# ifdef ACCNT_PRECISE
	  retrace_owner_computation(p);
# endif
	}
#endif
      }
    }
  } else {
    int marked;
#if defined(ACCNT_PRECISE) || defined(ACCNT_EXTERNAL)
    marked = ((ObjHeader*)((UWORD)p - 4))->mark;
#else
    if(page->flags.type) {
      marked = UNTAGGED_MARKED_P(*(UWORD*)((UWORD)p - 4));
    } else {
      marked = TAGGED_MARKED_P(*(Type_Tag*)p);
    }
#endif    
    if(marked) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
      /* we've already marked this. However, don't think this lets us
	 get off free; we still have some work to do. Pull up the
	 forward */
      void *newplace = *(void**)p;
      ObjHeader *newinfo = (ObjHeader*)((UWORD)newplace - 4);

      /* see if the owners are the same. If they are, we don't need to
	 worry about anything. If they aren't, we've got some issues */
      if(!ownerset_eq(newinfo->owner, mark_owner)) {
	/* crap. well, regardless we're going to use the right owner */
	ownerset_account_memory(newinfo->owner, INCGEN(mark_gen),
				-newinfo->size);
	newinfo->owner = ownerset_union(newinfo->owner, mark_owner);
	ownerset_account_memory(newinfo->owner, INCGEN(mark_gen),
				newinfo->size);
	/* now the question is whether or not we're going to fix the 
	   owners in anything downstream. Leave that up to whoever
	   built the system*/
# ifdef ACCNT_PRECISE
	retrace_owner_computation(newplace);
# endif
      }
#endif
    } else {
      /* we've never marked this. this is the "simple" case */
      void *startobj, *newplace;
      UWORD size;
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
      size = ((ObjHeader*)((UWORD)p - 4))->size;
      startobj = (void*)((UWORD)p - 4);
#else
      if(page->flags.type) {
	size = *(UWORD*)((UWORD)p - 4);
	startobj = (void*)((UWORD)p - 4);
      } else {
	size = size_table[*(Type_Tag*)p]((void*)p);
	startobj = (void*)p;
      }
#endif
      newplace = copy_bits(startobj, size, page->flags.type, INCGEN(page->flags.gen)			   
#ifdef ACCNT_INPLACE
			   , mark_owner
#endif
			   );
#ifndef ACCNT_OFF
      ownerset_account_memory(mark_owner, INCGEN(page->flags.gen), 
			      gcWORDS_TO_BYTES(size));
#endif

#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
      {
	ObjHeader *info = (ObjHeader*)startobj;
	/* fix the header*/
	info->mark = 1;
	info->owner = mark_owner;
	ownerset_account_memory(mark_owner, INCGEN(mark_gen), info->size);
	/* set in the forward*/
	*(void**)p = (void*)((UWORD)newplace + 4);
      }
#else 
      if(page->flags.type) {
	*(UWORD*)startobj = UNTAGGED_MARK(size);
	*(void**)p = (void*)((UWORD)newplace + 4);
      } else {
	*(Type_Tag*)p = TAGGED_MARK(*(Type_Tag*)p);
	*(void**)((UWORD)p + 4) = newplace;
      }
#endif
    }
  }
}

static void propogate_bpage_marks(MPage *page) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE + 4);
#else
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE + (page->flags.type?4:0));
#endif
  void **end = (void**)((UWORD)page + page->size);

#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
  mark_owner = ((ObjHeader*)((UWORD)start - 4))->owner;
#else
# ifdef ACCNT_INPLACE  
  mark_owner = page->owner;
# endif
#endif

  switch(page->flags.type) {
    case MPAGE_TAGGED: mark_table[*(Type_Tag*)start](start); break;
    case MPAGE_ATOMIC: break;
    case MPAGE_ARRAY: while(start < end) GC_mark(*(start++)); break;
    case MPAGE_TARRAY: {
      Type_Tag tag = *(Type_Tag*)start;
      while(start < end) {
	start += mark_table[tag](start);
      }
      break;
    }
    case MPAGE_XTAGGED: GC_mark_xtagged(start); break;
  }
}

void propogate_tagged_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);
  
#ifdef ACCNT_INPLACE
  mark_owner = page->owner;
#endif
  while(start < (void**)((UWORD)page + page->size)) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start++;
    mark_owner = info->owner;
#endif
    start += mark_table[*(Type_Tag*)start](start);
  }
  page->former_size = page->size;
}

static void propogate_array_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);

#ifdef ACCNT_INPLACE
  mark_owner = page->owner;
#endif
  while(start < (void**)((UWORD)page + page->size)) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start++;
    UWORD size = info->size;
    mark_owner = info->owner;
#else
    UWORD size = *(UWORD*)start++;
#endif
    while(--size) {
      GC_mark(*(start++));
    }
  }
  page->former_size = page->size;
}

static void propogate_tarray_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);

#ifdef ACCNT_INPLACE
  mark_owner = page->owner;
#endif  
  while(start < (void**)((UWORD)page + page->size)) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start;
    void **tempend = start + info->size;
    Type_Tag tag = *(Type_Tag*)(++start);
    mark_owner = info->owner;
#else
    UWORD size = *(UWORD*)start;
    void **tempend = start + size;
    Type_Tag tag = *(Type_Tag*)(++start);
#endif
    while(start < tempend) {
      start += mark_table[tag](start);
    }
  }
  page->former_size = page->size;
}

static void propogate_xtagged_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);

#ifdef ACCNT_INPLACE
  mark_owner = page->owner;
#endif			
  while(start < (void**)((UWORD)page + page->size)) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start;
    UWORD size = info->size;
    mark_owner = info->owner;
#else
    UWORD size = *(UWORD*)start;
#endif
    GC_mark_xtagged(start + 1);
    start += size;
  }
  page->former_size = page->size;
}

inline void propogate_mpage_marks(MPage *page) {
  if(page->flags.bigpage) {
    abort();
  }
  switch(page->flags.type) {
  case MPAGE_TAGGED: propogate_tagged_marks(page); break;
  case MPAGE_ATOMIC: page->former_size = page->size; break;
  case MPAGE_ARRAY: propogate_array_marks(page); break;
  case MPAGE_TARRAY: propogate_tarray_marks(page); break;
  case MPAGE_XTAGGED: propogate_xtagged_marks(page); break;
  default:
    printf("INTERNAL ERROR: really bad case in propogate_mpage_marks (%p)",
	   page);
    abort();
  }
}

static void propogate_all_mpages(void) {
  MPageList *mucked, *temp;
  MPage *work;

  while(mucked_pages) {
    mucked = mucked_pages;
    mucked_pages = NULL;
    while(mucked) {
      work = mucked->page;
      /* we have to do slightly different things dependent on whether this*/
      /* is a bigpage or a normal page.*/
      if(work->flags.bigpage) {
	/* if we're a bigpage, we only propogate those pages we've marked*/
	/* as gray. */
	if(work->flags.mark == MARK_GRAY) {
	  /* Luckily, we can use the the propogation routine as*/
	  /* long as we make sure 'former_size' is PAGE_HEADER_SIZE*/
	  work->flags.mark = MARK_BLACK;
	  /*work->former_size = PAGE_HEADER_SIZE;*/
	  propogate_bpage_marks(work);
	}
      } else {
	/* if we're a normal page, we only need to propogate those pages*/
	/* that have been added on to. this means only the pages where*/
	/* former_size < size.*/
	if(work->former_size < work->size) {
	  propogate_mpage_marks(work);
	}
      } /* if*/
      temp = mucked;
      mucked = mucked->next;
      free(temp);
    }
  } /* while*/
}

/*
  Immobile boxes: their structure in implementation
*/

typedef struct ImmobileBox {
  void *p;
  int owner; /* this needs to be second or Window.cc dies*/
  struct ImmobileBox *next;
} ImmobileBox;

static ImmobileBox *immobile_boxes = NULL;

static void mark_immobile_boxes(void) {
  ImmobileBox *ibox = immobile_boxes;
  while(ibox) {
#ifndef ACCNT_OFF    
    mark_owner = ibox->owner;
#endif
    GC_mark(ibox->p);
    ibox = ibox->next;
  }
}

static void fixup_immobile_boxes(void) {
  ImmobileBox *ibox = immobile_boxes;

  while(ibox) {
    gcFIXUP(ibox->p);
    ibox = ibox->next;
  }
}

static void initialize_immobile_subsystem(void) {
  ImmobileBox *ibox = immobile_boxes;
  while(ibox) {
#ifndef ACCNT_OFF
    ibox->owner = current_owner();
#endif
    ibox = ibox->next;
  }
}

/*
  Roots. Roots are a pain in the rear, because we'd like to merge them
  in order to save a little energy.
*/

typedef struct Root {
  void **start; /* the start of this set of root pointers*/
  void **end; /* the end of this set of root pointers*/
  struct Root *next; /* the next set of root pointers*/
} Root;

typedef struct RootSet {
  UWORD owner; /* the owner set associated with this set of roots*/
  Root *roots; /* the actual roots*/
  struct RootSet *next; /* the next RootSet*/
} RootSet;

static RootSet *roots = NULL;

static void add_root(void *start, void *end, UWORD owner) {
  RootSet *work = roots;
  int done = 0;

  /* search to see if we already have a root set for this owner*/
  while(work && !done) {
    done = (work->owner == owner);
    if(!done) work = work->next;
  }

  if(done) {
    /* A root set for this owner already exists. This is a rather 
       complicated case, since we have to insert the root into the
       array while watching for overlaps.*/
    Root *cur = work->roots, *prev = NULL;

    /* First off, search for roughly the appropriate place to put this
       new root */
    while(cur && ((UWORD)cur->start < (UWORD)start)) {
      prev = cur;
      cur = cur->next;
    }

    /* there are several possible case for us at this point. We need
       to treat each one separately. */
    /* FIXME: Some of these cases could probably be collapsed */
    if(cur && prev) {
      /* we're somewhere in the middle of the list, so we need to
	 check for overlap on both sides. This is the most complicated
	 case as we actually have four more possible subcases.*/
      if(((UWORD)prev->end >= (UWORD)start) 
	 && ((UWORD)cur->start <= (UWORD)end)) {
	/* this root joins these two adjacent roots, which is nice because
	   it frees up a little memory. We use prev for the modified root
	   for no particular reason. */
	prev->end = cur->end;
	prev->next = cur->next;
	free(cur);
      } else if((UWORD)prev->end >= (UWORD)start) {
	/* this root and the root at 'prev' overlap, so we only need to
	   modify the previous one. Note that technically there is the
	   possibility that this item is completely subsumed by the extant
	   root. */
	prev->end = ((UWORD)prev->end < (UWORD)end) ? (void**)end : prev->end;
      } else if((UWORD)cur->start <= (UWORD)end) {
	/* this root and the root at 'cur' overlap, so we only need to
	   modify the current one. Again we check for subsumption. */
	cur->start = ((UWORD)cur->start>(UWORD)start)?(void**)start:cur->start;
      } else {
	/* this root is completely new.*/
	Root *newroot = (Root*)malloc(sizeof(Root));
	newroot->start = (void**)start;
	newroot->end = (void**)end;
	newroot->next = cur;
	prev->next = newroot;
      }
    } else if(cur) {
      /* we're at the very front of the list. Thus we only need to check
	 for overlap to the "right" of us (further down the list) */
      if((UWORD)end >= (UWORD)cur->start) {
	/* these do overlap, so we munge the start pointer of the pre-extant
	   root and we're done. */
	cur->start = (void**)start;
      } else {
	/* these do not overlap, so we have to add in this new root to
	   the set.*/
	prev = (Root*)malloc(sizeof(Root));
	prev->start = (void**)start;
	prev->end = (void**)end;
	prev->next = cur;
	work->roots = prev;
      }
    } else if(prev) {
      /* we're at the very end of the list. Thus we only need to check
	 for overlap to the "left" of us (further up the list)*/
      if((UWORD)prev->end >= (UWORD)start) {
	/* these do overlap, so we munge the end pointer of the pre-extant*/
	/* root and we're done.*/
	prev->end = (void**)end;
      } else {
	/* these do not overlap, so we're "safe" and should just add this
	   to the end.*/
	cur = (Root*)malloc(sizeof(Root));
	cur->start = (void**)start;
	cur->end = (void**)end;
	cur->next = NULL;
	prev->next = cur;
      }
    } else {
      /* there was no list. This should never happen, so scream and
	 die.*/
      printf("SERIOUS INTERNAL ERROR: Attempt to add root to null root set!\n");
      abort();
    }
  } else {
    /* There is no root set for this owner, so we add one. This is the
       easy case for this routine. */
    work = (RootSet*)malloc(sizeof(RootSet));
    work->owner = owner;
    work->roots = (Root*)malloc(sizeof(Root));
    work->roots->start = (void**)start;
    work->roots->end = (void**)end;
    work->roots->next = NULL;
    work->next = roots;
    roots = work;
  }
}

/* this routine marks all the roots for us. this is called by the collector
   to start things off */
static void mark_roots(void) {
  RootSet *rtsets;
  Root *rts;

  for(rtsets = roots; rtsets; rtsets = rtsets->next) {
#ifndef ACCNT_OFF
    int owner = rtsets->owner;
#endif
    for(rts = rtsets->roots; rts; rts = rts->next) {
      void **start = rts->start;
      void **end = rts->end;
      while(start < end) {
#ifndef ACCNT_OFF
	mark_owner = owner;
#endif
	GC_mark(*start);
	start++;
      }
    }
  }
}

/* this routine fixes up all the roots for us in the last stages of 
   collection */
static void fixup_roots(void) {
  RootSet *rtsets = roots;
  Root *rts;

  
  for(rtsets = roots; rtsets; rtsets = rtsets->next) {
#ifndef ACCNT_OFF
    if(!owner_table[rtsets->owner]) {
      rtsets->owner = 0;
    }
#endif
    for(rts = rtsets->roots; rts; rts = rts->next) {
      void **start = rts->start;
      void **end = rts->end;
      while(start < end) {
	gcFIXUP(*start);
	start++;
      }
    }
  }
}

/* this routine dumps information about the roots to the given file
   handle. this is used in the main dump routine plus in debugging. */
static void dump_root_information(FILE *file) {
  RootSet *rtsets;
  Root *rts;

  fprintf(file, "Root information:\n");
  for(rtsets = roots; rtsets; rtsets = rtsets->next) {
    for(rts = rtsets->roots; rts; rts = rts->next) {
      fprintf(file, "[%10p - %10p] Owner = %li\n", 
	      rts->start, rts->end, rtsets->owner);
    }
  }
  fprintf(file, "\n");
}

/* since there's an unfortunate period in the very beginning where 
   we don't know who owns what, this little function is called to
   fix a little munging we've done*/
static void initialize_roots_subsystem(void) {
  RootSet *rtsets = roots;
  
  if(rtsets) {
#ifndef ACCNT_OFF
    rtsets->owner = current_owner();
#endif
    if(rtsets->next) {
      printf("SERIOUS INTERNAL ERROR: More than one set before init_roots!\n");
      abort();
    }
  }
}

/*
  Weak boxes and arrays: Their structure and implementation
*/

typedef struct WeakArray {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct WeakArray *next;
  void *data[1];
} WeakArray;

static WeakArray *weak_arrays;

static int size_weak_array(void *p) {
  WeakArray *a = (WeakArray *)p;
  return gcBYTES_TO_WORDS(sizeof(WeakArray)+((a->count - 1) * sizeof(void *)));
}

static int mark_weak_array(void *p) {
  WeakArray *a = (WeakArray *)p;
  gcMARK(a->replace_val);
  a->next = weak_arrays;
  weak_arrays = a;
  return gcBYTES_TO_WORDS(sizeof(WeakArray)+((a->count - 1) * sizeof(void *)));
}

static int fixup_weak_array(void *p) {
  WeakArray *a = (WeakArray *)p;
  int i;
  void **data;
  gcFIXUP(a->replace_val);
  data = a->data;
  for (i = a->count; i--; ) {
    if (data[i])
      gcFIXUP(data[i]);
  }
  return gcBYTES_TO_WORDS(sizeof(WeakArray)+((a->count - 1) * sizeof(void *)));
}

static void clean_up_weak_arrays(void) {
  WeakArray *wa;
  for(wa = weak_arrays; wa; wa = wa->next) {
    void **data;
    int i;
    data = wa->data;
    for(i = wa->count; i--; ) {
      void *p = data[i];
      if(p && !is_marked(p)) {
	data[i] = wa->replace_val;
      }
    }
  }
}

typedef struct WeakBox {
  /* The first three fields are mandated by the GC spec: */
  Type_Tag type;
  short keyex;
  void *val;
  /* The rest is up to us: */
  void **secondary_erase;
  int soffset;
  struct WeakBox *next;
} WeakBox;

Type_Tag weak_box_tag = 42; /* set by MzScheme*/
WeakBox *weak_boxes;

static int size_weak_box(void *p) {
  return gcBYTES_TO_WORDS(sizeof(WeakBox));
}

static int mark_weak_box(void *p) {
  WeakBox *wb = (WeakBox *)p;
    
  gcMARK(wb->secondary_erase);

  if (wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }

  return gcBYTES_TO_WORDS(sizeof(WeakBox));
}

static int fixup_weak_box(void *p) {
  WeakBox *wb = (WeakBox *)p;
    
  gcFIXUP(wb->secondary_erase);
  gcFIXUP(wb->val);

  return gcBYTES_TO_WORDS(sizeof(WeakBox));
}


/* after collection we want to kill out the pointers in the weak box*/
/* list that have died.*/
static void clean_up_weak_boxes(void) {
  WeakBox *wb;
  for(wb = weak_boxes; wb; wb = wb->next) {
    if(!is_marked(wb->val)) {
      wb->val = NULL;
      if(wb->secondary_erase) {
	if(is_marked(wb->secondary_erase)) {
	  *((void**)GC_resolve(wb->secondary_erase) + wb->soffset) = NULL;
	} else *(wb->secondary_erase + wb->soffset) = NULL;
	wb->secondary_erase = NULL;
      }
    }
  }
}

static void initialize_weak_subsystem() {
  GC_register_traversers(weak_box_tag, size_weak_box, mark_weak_box, 
			 fixup_weak_box);
  GC_register_traversers(gc_weak_array_tag, size_weak_array, mark_weak_array, 
			 fixup_weak_array);
}

/*
  Finalizers: their structure and implementation
 */

typedef struct Fnl {
  Type_Tag type;
  char eager_level;
  char tagged;
  void *p;
  void (*f)(void *p, void *data);
  void *data;
  struct Fnl *next;
} Fnl;

static Fnl *fnls, *run_queue, *last_in_queue;

static int size_finalizer(void *p) {
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int mark_finalizer(void *p) {
  Fnl *fnl = (Fnl *)p;
  gcMARK(fnl->next);
  gcMARK(fnl->data);
  /* !eager_level => queued for run: */
  if (!fnl->eager_level) {
    gcMARK(fnl->p);
  }
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int fixup_finalizer(void *p) {
  Fnl *fnl = (Fnl *)p;
  gcFIXUP(fnl->next);
  gcFIXUP(fnl->data);
  gcFIXUP(fnl->p);
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

typedef struct Fnl_Weak_Link {
  Type_Tag type;
  void *p;
  int offset;
  void *saved;
  struct Fnl_Weak_Link *next;
} Fnl_Weak_Link;

static Fnl_Weak_Link *fnl_weaks;

static int size_finalizer_weak_link(void *p) {
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int mark_finalizer_weak_link(void *p) {
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
  gcMARK(wl->next);
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int fixup_finalizer_weak_link(void *p) {
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
  gcFIXUP(wl->next);
  gcFIXUP(wl->p);
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

/* after collection we need to run through our list of finals and */
/* clear out weak links that have died*/
static void clean_up_weak_finals(void) {
  Fnl_Weak_Link *wl, *prev, *next;

  prev = NULL;
  for(wl = fnl_weaks; wl; wl = next) {
    next = wl->next;
    if(!is_marked(wl->p)) {
      /* will be collected, so remove the link*/
      wl->p = NULL;
      if(prev) prev->next = next;
      else fnl_weaks = next;
    } else prev = wl;
  }
}

static void initialize_final_subsystem() {
  GC_register_traversers(gc_finalization_tag, size_finalizer, mark_finalizer, 
			 fixup_finalizer);
  GC_register_traversers(gc_finalization_weak_link_tag, 
			 size_finalizer_weak_link, 
			 mark_finalizer_weak_link, fixup_finalizer_weak_link);
  GC_add_roots(&fnls, (char *)&fnls + sizeof(fnls) + 1);
  GC_add_roots(&fnl_weaks, (char *)&fnl_weaks + sizeof(fnl_weaks) + 1);
  GC_add_roots(&run_queue, (char *)&run_queue + sizeof(run_queue) + 1);
  GC_add_roots(&last_in_queue, 
	       (char *)&last_in_queue + sizeof(last_in_queue) + 1);
}

/*
  Repairs. After we've marked everything, we're going to have a lot of pointers
  int tospace pointin to fromspace. These routines fix all that stuff for us
*/

static void repair_bpage_ptrs(MPage *page) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE + 4);
#else
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE + (page->flags.type?4:0));
#endif
  void **end = (void**)((UWORD)page + page->size);

  switch(page->flags.type) {
    case MPAGE_TAGGED: fixup_table[*(Type_Tag*)start](start); break;
    case MPAGE_ATOMIC: break;
    case MPAGE_ARRAY: while(start < end) gcFIXUP(*(start++)); break;
    case MPAGE_TARRAY: {
      Type_Tag tag = *(Type_Tag*)start;
      while(start < end) {
	start += fixup_table[tag](start);
      }
      break;
    }
    case MPAGE_XTAGGED: GC_fixup_xtagged(start); break;
  }
}

static void repair_tagged_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);

  while(start < end) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    start++;
#endif
    start += fixup_table[*(Type_Tag*)start](start);
  }
}

static void repair_array_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);
  
  while(start < end) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start++;
    UWORD size = info->size;
    info->mark = 0;
#else
    UWORD size = *(UWORD*)start++;
#endif
    while(--size)
      gcFIXUP(*(start++));
  }
}

static void repair_tarray_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);
  
  while(start < end) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start;
    UWORD size = info->size;
    info->mark = 0;
#else
    UWORD size = *(UWORD*)start;
#endif
    {
      void **tempend = start + size;
      Type_Tag tag = *(Type_Tag*)(++start);
      while(start < tempend)
	start += fixup_table[tag](start);
    }
  }
}

static void repair_xtagged_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);

  while(start < end) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)start;
    UWORD size = info->size;
    info->mark = 0;
#else
    UWORD size = *(UWORD*)start;
#endif
    GC_fixup_xtagged(start+1);
    start += size;
  }
}

inline void repair_mpage_ptrs(MPage *page) {
  page->flags.mark = MARK_WHITE;
  if(page->flags.bigpage) {
    repair_bpage_ptrs(page);
    return;
  }
  switch(page->flags.type) {
  case MPAGE_TAGGED: repair_tagged_ptrs(page); break;
  case MPAGE_ATOMIC: break;
  case MPAGE_ARRAY: repair_array_ptrs(page); break;
  case MPAGE_TARRAY: repair_tarray_ptrs(page); break;
  case MPAGE_XTAGGED: repair_xtagged_ptrs(page); break;
  default:
    printf("INTERNAL ERROR: really bad case in repair_mpage_ptrs (%p)\n",
	   page);
    abort();
  }
}

/* 
   this is the routine called by garbage_collect to repair all the pointers.
   note that this not only has to repair all the pointers in the new pages,
   but also all the pointers in the older generations which were modified
*/
static void repair_mpages(void) {
  MPage *work;
  int i, j;
  int new_top_gen = INCGEN(mark_gen);
  int old_start_gen = new_top_gen + 1;

  /* repair all the new pages*/
  for(i = 1; i <= new_top_gen; i++) {
    for(j = 0; j < 6; j++) {
      for(work = gen[i][j]; work; work = work->next) {
	/* note that this routine works just as well for big and normal pages*/
	repair_mpage_ptrs(work);
      }
    }
  }
  /* repair the old, modded pages*/
  for(i = old_start_gen; i < GENERATIONS; i++) {
    for(j = 0; j < 6; j++) {
      for(work = gen[i][j]; work; work = work->next) {
	/* note that this routine works just as well for big and normal pages*/
	repair_mpage_ptrs(work);
      }
    }
  }
}

/* 
   Older generation modifications. This is the stuff that deals with
   modifications to older generations, if we're using a generational
   collector
*/

#ifdef GENERATIONS
static void designate_modified(void *p) {
  MPage *page;
  page = find_page(p);
  if(page) {
    protect_pages(page, page->size, 1);
    page->flags.bpointers = 1;
  } else {
    fprintf(stderr, "Seg fault (internal error) at %p\n", p);
    abort();
  }  
}
#endif

static void protect_older_pages(void) {
#ifdef GENERATIONS
  MPage *work;
  int i, j;

  for(i = 1; i < GENERATIONS; i++) {
    for(j = 0; j < 6; j++) {
      if(j != MPAGE_ATOMIC) { 
	for(work = gen[i][j]; work; work = work->next) {
	  if(work->flags.type != MPAGE_ATOMIC) {
	    protect_pages(work, work->size, 0);
	  }
	}
      }
    }
  }
#endif
}


/* when we start collection, we want to run the marks that are included in*/
/* modified pages. for the purposes of owner tests, we assume that the */
/* info on the page is correct. */
static void run_older_marks(int top_gen) {
#ifdef GENERATIONS
  MPage *work;
  int i, j;
  
  for(i = top_gen + 1; i < GENERATIONS; i++) {
    for(j = 0; j < 6; j++) {
      if(j != MPAGE_ATOMIC) {
	for(work = gen[i][j]; work; work = work->next) {
	  if(work->flags.bpointers) {
	    work->former_size = PAGE_HEADER_SIZE;
	    if(work->flags.bigpage) {
	      propogate_bpage_marks(work);
	    } else propogate_mpage_marks(work);
 	  } 
	}
      }
    }
  }
#endif
}

#ifdef GENERATIONS
#if defined(linux)
# include <signal.h>
void fault_handler(int sn, struct sigcontext sc)
{
  designate_modified((void *)sc.cr2);
  signal(SIGSEGV, (void (*)(int))fault_handler);
}
# define NEED_SIGSEGV
#endif

/* FreeBSD signal handler: */
#if defined(__FreeBSD__)
# include <signal.h>
void fault_handler(int sn, int code, struct sigcontext *sc, char *addr)
{
  designate_modified(addr);
}
# define NEED_SIGBUS
#endif

/* Solaris signal handler: */
#if defined(sun)
# include <signal.h>
void fault_handler(int sn, struct siginfo *si, void *ctx)
{
  designate_modified(si->si_addr);
}
# define NEED_SIGACTION
#endif

/* Windows signal handler: */
#if defined(_WIN32)
LONG WINAPI fault_handler(LPEXCEPTION_POINTERS e) 
{
  if ((e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
      && (e->ExceptionRecord->ExceptionInformation[0] == 1)) {
    designate_modified((void *)e->ExceptionRecord->ExceptionInformation[1]);

    return EXCEPTION_CONTINUE_EXECUTION;
  } else
    return EXCEPTION_CONTINUE_SEARCH;
}
# define NEED_SIGWIN
#endif
#endif

void initialize_memory_protection_subsystem(void) {
#ifdef GENERATIONS
# ifdef NEED_SIGSEGV
    signal(SIGSEGV, (void (*)(int))fault_handler);
# endif
# ifdef NEED_SIGBUS
    signal(SIGBUS, (void (*)(int))fault_handler);
# endif
# ifdef NEED_SIGACTION
    {
      struct sigaction act, oact;
      act.sa_sigaction = fault_handler;
      sigemptyset(&act.sa_mask);
      act.sa_flags = SA_SIGINFO;
      sigaction(SIGSEGV, &act, &oact);
    }
# endif
# ifdef NEED_SIGWIN
    SetUnhandledExceptionFilter(fault_handler);
# endif
#endif
}

/* 
   These are a few helper routines, and then the collector proper
*/

static void shunt_off_collecting_pages(int top_gen) {
  MPage *work;
  int i, j;

  collect_pages = NULL;
  for(i = 0; i <= top_gen; i++) {
    for(j = 0; j < 6; j++) {
      work = gen[i][j];
      while(work && work->next) {
	work = work->next;
      }
      if(work) {
	work->next = collect_pages;
	collect_pages = gen[i][j];
	gen[i][j] = NULL;
      }
    }
  }
  for(work = collect_pages; work; work = work->next) 
    work->flags.mark = MARK_WHITE;
}

static void rebuild_broken_final_list(void) {
  Fnl *f = fnls, *prev = NULL;
  Fnl_Weak_Link *wl = fnl_weaks, *prevwl = NULL;
  
  while(f) {
    if(is_marked(f)) {
      if(prev) {
	f = GC_resolve(f);
	prev->next = f;
	prev = f;
	f = f->next;
      } else {
	fnls = GC_resolve(f);
	prev = fnls;
	f = fnls->next;
      }
    } else {
      prev = f;
      f = f->next;
    }
  }
  while(wl) {
    if(is_marked(wl)) {
      if(prevwl) {
	wl = GC_resolve(wl);
	prevwl->next = wl;
	prevwl = wl;
	wl = wl->next;
      } else {
	fnl_weaks = GC_resolve(wl);
	prevwl = fnl_weaks;
	wl = fnl_weaks->next;
      }
    } else {
      prevwl = wl;
      wl = wl->next;
    }
  }
}

static void free_dead_pages(void) {
  MPage *work, *next;

  for(work = collect_pages; work; work = next) {
    next = work->next;
    rem_from_page_map(work);
    protect_pages(work, work->flags.bigpage ? work->size : MPAGE_SIZE, 1);
    free_mempages(work);
  }
  flush_freed_pages();
  collect_pages = NULL;
  mucked_pages = NULL;
}

static void recalculate_heap_size(void) {
  MPage *work;
  int i, j;

  heap_size = 0;
  for(i = 1; i < GENERATIONS; i++) {
    for(j = 0; j < 6; j++) {
      for(work = gen[i][j]; work; work = work->next) {
	heap_size += work->size;
      }
    }
  }
}

/* #define DEBUG_START_AT		1000 */
/* static void debug_dump_heap(UWORD collection, int mark_gen, int before) { */
/*   FILE *file; */
/*   int i, j; */

/*   if(before) { */
/*     fprintf(stdout, "Garbage collection #%li (mark_gen = %i)\n",  */
/* 	    collection, mark_gen); */
/*   } */
/*   if(collection < DEBUG_START_AT) { */
/*     return; */
/*   } */
/*   file = fopen("gcdump", ((before && (collection == DEBUG_START_AT)) ? "w" : "a")); */

/*   fprintf(file, "\nGarbage collection #%li (mark_gen = %i) [%s]\n",  */
/* 	  collection, mark_gen, before ? "BEFORE" : "AFTER"); */
/*   dump_mpage_information(file); */
/*   for(i = 0; i < GENERATIONS; i++) { */
/*     for(j = 0; j < 6; j++) { */
/*       MPage *work; */
/*       for(work = gen[i][j]; work; work = work->next) { */
/* 	UWORD p = (UWORD)work; */
/* 	fprintf(file, "%p:\n", work); */
/* 	while(p < ((UWORD)work + work->size)) { */
/* 	  int k; */
/* 	  fprintf(file, "0x%8lx:\t", p); */
/* 	  for(k = 0; k < 5; k++) { */
/* 	    fprintf(file, "%08lx ", *(UWORD*)p); p += 4; */
/* 	  } */
/* 	  fprintf(file, "\n"); */
/* 	} */
/*       } */
/*     } */
/*   } */


/*   fflush(file); */
/*   fclose(file); */
/* } */

static void garbage_collect(int force_full) {
  static UWORD collection = 0;
  Fnl *workrq = run_queue;
  int did_fnls;

  if(!gc_deny) {

#ifdef ACCNT_OFF
    if(collection == 0) {
      init();
    }
#endif
#ifdef GENERATIONS
    if(force_full) mark_gen = GENERATIONS - 1;
    else if(!(collection & 0xF)) mark_gen = GENERATIONS - 1;
    else if(!(collection & 0x7)) mark_gen = (GENERATIONS / 2);
    else if(!(collection & 0x3)) mark_gen = (GENERATIONS / 5);
    else mark_gen = 0;
#endif    
/*     debug_dump_heap(collection, mark_gen, 1); */
    collection++;

    /* notify MzScheme that we're about to collect*/
    if(GC_collect_start_callback)
      GC_collect_start_callback();

    /* we don't want collections happening while we're collecting!*/
    gc_deny = 1;
    
    /* initialize the weak and final stuff*/
    weak_boxes = NULL;
    weak_arrays = NULL;
    did_fnls = 0;
    
    /* Now we move all the pages we're collecting to a more centralized*/
    /* location. This helps much later down the line. */
    shunt_off_collecting_pages(mark_gen);
#ifndef ACCNT_OFF
    prep_ownerset_information(mark_gen);
#endif

    /* Now to the actual collection. First we need to mark off our base*/
    /* pointers, which amounts to the immobile boxes, the roots and */
    /* the current stack*/
    run_older_marks(mark_gen);
    mark_immobile_boxes();
    mark_roots();
    mark_account_hooks();
#ifndef ACCNT_OFF
    mark_owner = current_owner();
#endif
    GC_mark_variable_stack(GC_variable_stack, 0, 
			   (void*)(GC_get_thread_stack_base
				   ? GC_get_thread_stack_base() 
				   : (UWORD)stack_base));

    while(workrq) {
      GC_mark(workrq);
      workrq = workrq->next;
    }
    
    /* once we're done with this, it's on to propogation. Unfortunately*/
    /* this isn't as easy as one might think because of finalizers. To*/
    /* be honest I (Adam) don't fully understand how this works. Ask*/
    /* Matthew.*/
    while(1) {
      /* first propogate out of the marks we already have*/
      propogate_all_mpages();

      /* this is a copying collector, so at this point the linked lists*/
      /* we normally use to trace through finalizers is trashed. So we */
      /* have to fix them before we do anything else*/
      rebuild_broken_final_list();

      if((did_fnls >= 3) || !fnls) {
	if(did_fnls == 3) {
	  /* Finish up ordered finalization*/
	  Fnl *f, *next, *prev;
	  Fnl_Weak_Link *wl;
	  
	  /* Enqueue and mark level 3 finalizers that still haven't been */
	  /* marked. */
	  prev = NULL;
	  for(f = fnls; f; f = next) {
	    next = f->next;
	    if(f->eager_level == 3) {
	      if(!is_marked(f->p)) {
#ifndef ACCNT_OFF
		mark_owner = current_owner();
#endif
		gcMARK(f->p);

		if(prev) prev->next = next;
		else fnls = next;
		
		f->eager_level = 0; /* indicates this has been queued*/
		f->next = NULL;
		if(last_in_queue) {
		  last_in_queue->next = f;
		  last_in_queue = f;
		} else {
		  run_queue = last_in_queue = f;
		}
	      } else {
		prev = f;
	      }
	    }
	  }
	  
	  /* restores zeroed out weak links, marking as we go*/
	  for(wl = fnl_weaks; wl; wl = wl->next) {
	    void *wp = (void*)wl->p;

	    if(is_marked(wp)) {
	      /* for the purposes of this, we account the saved pointer*/
	      /* to the holder of the weak link*/
#if defined(MARK_EXTERNAL) || defined(MARK_PRECISE)
	      mark_owner = ((ObjHeader*)((UWORD)wp - 4))->owner;
#endif
	      gcMARK(wl->saved);
	    }
	    /* this is a little gotcha. if we've moved wp, the change in*/
	    /* pointer hasn't shown here yet and we need to have this set*/
	    /* run in both places (?). FIXME: not sure about this both*/
	    /* business*/
	    if(is_marked(wp)) {
	      *(void**)((char*)GC_resolve(wp) + wl->offset) = wl->saved;
	    }
	    *(void**)((char*)wp + wl->offset) = wl->saved;
	  }

	  /* we have to mark one more time, because restoring a weak link*/
	  /* may have made something reachable*/
	  did_fnls++;
	} else break;
      } else {
	int eager_level = did_fnls+1;

	if(eager_level == 3) {
	  /* Ordered finalization*/
	  Fnl *f;
	  Fnl_Weak_Link *wl;

	  /* Zero out weak links for ordered finalization*/
	  for(wl = fnl_weaks; wl; wl = wl->next) {
	    void *wp = (void*)wl->p;
	    wl->saved = *(void**)((char*)wp + wl->offset);
	    *(void**)((char*)wp + wl->offset) = NULL;
	  }

	  /* Mark content of not-yet-marked finalized objects, but don't*/
	  /* mark the finalized objects themselves*/
	  for(f = fnls; f; f = f->next) {
	    if(f->eager_level == 3) {
	      if(!is_marked(f->p)) {
		/* not yet marked. mark the content*/
#if defined(MARK_EXTERNAL) || defined(MARK_PRECISE)
		mark_owner = current_owner();
#endif
		if(f->tagged) {
		  Type_Tag tag = *(Type_Tag*)f->p;
		  mark_table[tag](f->p);
		} else {
		  GC_mark_xtagged(f->p);
		}
	      }
	    }
	  }
	} else {
	  /* unordered finalization*/
	  Fnl *f, *prev, *queue;

	  f = fnls;
	  prev = NULL;
	  queue = NULL;

	  while(f) {
	    if(f->eager_level == eager_level) {
	      if(!is_marked(f->p)) {
		/* not yet marked. move finalization to run queue*/
		Fnl *next = f->next;
		
		if(prev) prev->next = next;
		else fnls = next;
		
		f->eager_level = 0; /* indicating queuednessness (=))*/

		f->next = NULL;
		if(last_in_queue) {
		  last_in_queue->next = f;
		  last_in_queue = f;
		} else {
		  run_queue = last_in_queue = f;
		}
		if(!queue) queue = f;
		f = next;
	      } else {
		prev = f;
		f = f->next;
	      }
	    } else {
	      prev = f;
	      f = f->next;
	    }
	  }

	  /* Mark items added to run queue*/
	  f = queue;
	  while(f) {
	    /* FIXME: this is probably not the thing to do*/
#if defined(MARK_EXTERNAL) || defined(MARK_PRECISE)
	    mark_owner = current_owner();
#endif
	    gcMARK(f->p);
	    f = f->next;
	  }
	}

	did_fnls++;
      }
    }
    
    /* we're done. amazing. so now we have to go back and fix all the things*/
    /* we've broken.*/
#ifndef ACCNT_OFF
    fixup_ownersets();
#endif
    clean_up_weak_boxes();
    clean_up_weak_arrays();
    clean_up_weak_finals();
    repair_mpages();
    fixup_immobile_boxes();
    fixup_account_hooks();
    fixup_roots();
    GC_fixup_variable_stack(GC_variable_stack, 0, 
			    (void*)(GC_get_thread_stack_base
				    ? GC_get_thread_stack_base()
				    : (UWORD)stack_base));
    free_dead_pages();
    protect_older_pages();
    recalculate_heap_size();
    /* recalculate how much space we're using*/
    size_to_collect_at = (UWORD)((heap_size * HEAP_GROW_FACTOR) + HEAP_GROW_ADD);

    /* we can now run collections again*/
    gc_deny = 0;

    /* notify MzScheme that we're done with collection now*/
    if(GC_collect_end_callback) 
      GC_collect_end_callback();

/*     debug_dump_heap(collection, mark_gen, 0); */

    /* and finally run the finalization routines. */
    while(run_queue) {
      Fnl *f;
      void **gcs;
      
      f = run_queue;
      run_queue = run_queue->next;
      if(!run_queue) last_in_queue = NULL;
      gcs = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = gcs;
    }
    
    run_applicable_account_hooks();
  }
}

/*
  The initialization routine
*/

static void init(void) {
  initialize_roots_subsystem();
  initialize_mpage_subsystem();
  initialize_immobile_subsystem();
  initialize_memory_protection_subsystem();
  initialize_final_subsystem();
  initialize_weak_subsystem();
}

/* 
   API Conversions: the implementations of the APIs given the above stuff
*/

void GC_set_stack_base(void *base) {
  stack_base = base;
}
unsigned long GC_get_stack_base(void) {
  return (unsigned long)stack_base;
}

void GC_add_roots(void *start, void *end) {
#ifdef ACCNT_OFF
  add_root(start, end, 0);
#else
  add_root(start, end, current_owner());
#endif
}

void GC_init_type_tags(int count, int weakbox) {
  weak_box_tag = weakbox;
}

void GC_dump(void) {
  dump_root_information(stderr);
  dump_mpage_information(stderr);
#ifndef ACCNT_OFF
  dump_ownerset_information(stderr);
#endif
}

long GC_get_memory_use(void *c) {
#ifndef ACCNT_OFF
  Scheme_Custodian *cust = (Scheme_Custodian*)c; 

  if(cust) 
    return get_custodian_mem_usage(cust);
  else 
    return heap_size;
#else
  return heap_size;
#endif
}

void GC_gcollect(void) {
  garbage_collect(1);
}

void *GC_malloc(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_ARRAY, 0);
}

void *GC_malloc_one_tagged(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_TAGGED, 0);
}

void *GC_malloc_one_xtagged(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_XTAGGED, 0);
}

void *GC_malloc_array_tagged(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_TARRAY, 0);
}

void *GC_malloc_atomic(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_ATOMIC, 0);
}

void *GC_malloc_atomic_uncollectable(size_t size_in_bytes) {
  return malloc(size_in_bytes);
}

void *GC_malloc_allow_interior(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_ARRAY, 1);
}

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val) {
  WeakArray *w;
  gc_deny = 1;
  w = (WeakArray*)GC_malloc_one_tagged(size_in_bytes + sizeof(WeakArray)
				       - sizeof(void*));
  gc_deny = 0;
  w->type = gc_weak_array_tag;
  w->replace_val = replace_val;
  w->count = (size_in_bytes >> 2);
  return w;
}

void GC_free(void *p) {}

void *GC_malloc_weak_box(void *p, void **secondary, int soffset) {
  WeakBox *w;
  gc_deny = 1;
  w = (WeakBox*)GC_malloc_one_tagged(sizeof(WeakBox));
  gc_deny = 0;
  w->type = weak_box_tag;
  w->val = p;
  w->secondary_erase = secondary;
  w->soffset = soffset;
  return w;
}

void **GC_malloc_immobile_box(void *p) {
  ImmobileBox *ibox = (ImmobileBox*)malloc(sizeof(ImmobileBox));
#ifndef ACCNT_OFF
  ibox->owner = current_owner();
#endif
  ibox->p = p;
  ibox->next = immobile_boxes;
  immobile_boxes = ibox;
  return (void**)ibox;
}

void GC_free_immobile_box(void **b) {
  ImmobileBox *ibox = (ImmobileBox*)b;
  ImmobileBox *cur, *prev;

  if(!ibox)
    return;

  cur = immobile_boxes;
  prev = NULL;
  while(cur) {
    if(cur == ibox) {
      if(prev) {
	prev->next = cur->next;
      } else {
	immobile_boxes = cur->next;
      }
      free(ibox);
      return;
    } else {
      prev = cur;
      cur = cur->next;
    }
  }
}

int GC_set_account_hook(int type, void *cust, unsigned long b, void *f) {
  AccountHook *work = (AccountHook*)malloc(sizeof(AccountHook));
  work->type = type;
#ifndef ACCNT_OFF
  work->owner = current_owner();
#endif
  work->cust = cust;
  work->bytes = b;
  work->f = f;
  work->next = account_hooks;
  account_hooks = work;
  if(type == MZACCT_REQUIRE)
    total_requires += b;
  return 1;
}

void GC_set_finalizer(void *p, int tagged, int level, 
		      void (*f)(void *p, void *data), 
		      void *data, void (**oldf)(void *p, void *data), 
		      void **olddata)
{
  Fnl *fnl, *prev;

  {
    MPage *page;
    page = find_page(p);
    if (!page) {
      /* Never collected. Don't finalize it. */
      if (oldf) *oldf = NULL;
      if (olddata) *olddata = NULL;
      return;
    }
  }

  fnl = fnls;
  prev = NULL;
  while (fnl) {
    if (fnl->p == p) {
      if (oldf) *oldf = fnl->f;
      if (olddata) *olddata = fnl->data;
      if (f) {
	fnl->f = f;
	fnl->data = data;
	fnl->eager_level = level;
      } else {
	if (prev)
	  prev->next = fnl->next;
	else
	  fnls = fnl->next;
	return;
      }
      return;
    } else {
      prev = fnl;
      fnl = fnl->next;
    }
  }
  
  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;

  if (!f)
    return;

  gc_deny = 1;
  fnl = GC_malloc_one_tagged(sizeof(Fnl));
  gc_deny = 0;
  fnl->type = gc_finalization_tag;
  fnl->next = fnls;
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;
  fnl->tagged = tagged;
  fnls = fnl;
}

void GC_finalization_weak_ptr(void **p, int offset)
{
  Fnl_Weak_Link *wl;
  gc_deny = 1;
  wl = (Fnl_Weak_Link *)GC_malloc_one_tagged(sizeof(Fnl_Weak_Link));
  gc_deny = 0;
  wl->type = gc_finalization_weak_link_tag;
  wl->p = p;
  wl->next = fnl_weaks;
  wl->offset = offset * sizeof(void*);
  fnl_weaks = wl;
}

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, 
			    Fixup_Proc fixup) {
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
}

void *GC_resolve(void *p) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
  ObjHeader *info = (ObjHeader*)((UWORD)p - 4);

  if(info->mark)
    return *(void**)p;
  else
    return p;
#else
  MPage *page = find_page(p);

  if(!page) return p;
  if(page->flags.type) {
    if(UNTAGGED_MARKED_P(*(UWORD*)((UWORD)p - 4))) {
      return *(void**)p;
    } else return p;
  } else {
    if(TAGGED_MARKED_P(*(Type_Tag*)p)) {
      return *(void**)((UWORD)p + 4);
    } else return p;
  }
#endif
}

void GC_fixup(void *pp) {
  void *p = *(void**)pp;
  MPage *page;

  if(!p)
    return;
  if((long)p & 0x1)
    return;

  page = find_page(p);

  if(page && !page->flags.bigpage) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
    ObjHeader *info = (ObjHeader*)((UWORD)p - 4);
    int marked = info->mark;
#else
    int marked = page->flags.type ? UNTAGGED_MARKED_P(*(UWORD*)((UWORD)(p -4)))
                            : TAGGED_MARKED_P(*(Type_Tag*)p);
#endif

#ifdef GENERATIONS
    if(page->flags.gen > INCGEN(mark_gen)) return;
#endif

    if(marked) {
#if defined(ACCNT_EXTERNAL) || defined(ACCNT_PRECISE)
      void *newloc = *(void**)p;
#else
      void *newloc = *(void**)((UWORD)p + (page->flags.type ? 0 : 4));
#endif
      MPage *mypage = find_page(pp);
      
      *(void**)pp = newloc;
#ifdef GENERATIONS
      if(mypage) {
	MPage *newlocpage = find_page(newloc);
	if(mypage->flags.gen > newlocpage->flags.gen) {
	  mypage->flags.bpointers = 1;
	}
      }
#endif
    } 
  }
}

/* Called to mark the current stack. See README for more details*/
void GC_mark_variable_stack(void **var_stack, long delta, void *limit) {
  long size, count;
  void ***p, **a;

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
}

/* Called to fix up the current stack. See README for more details*/
void GC_fixup_variable_stack(void **var_stack, long delta, void *limit) {
  long size, count;
  void ***p, **a;

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
}

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

