//------------------------------------------------------------------------------
// A new Precise GC for MzScheme
// Copyright (C) 2001 Matthew Flatt and Adam Wick
// All rights reserved.
//
// Please see full copyright in the documentation
// Search for "FIXME" for possible improvement points
//------------------------------------------------------------------------------

// Some standard includes we use:
#include <stdlib.h> // for malloc and friends
#include <stdio.h> // for printf, fprintf, stdout and friends
#include <string.h> // for memmove
#include "gc2.h" // need this for the headers
#include "../src/schpriv.h" // needed for Scheme_Custodian

// Some overall defines. Modify these defines *only*. The other defines
// will fall into place, and modifying anything else could severely
// damage the functionality of the system
#define NUM_GENERATIONS		3 // the # of generations
#define INITIAL_GENERATION_SIZE 2000000 // size of initial generation
#define SIZE_GROW_FACTOR	1.5 // two variables used to determine when
#define SIZE_GROW_ADDITION      500000 // the next collect happens
#define LOG_WORD_SIZE		2 // the log_2 of the word size. must be >= 2
#define LOG_MPAGE_SIZE		14 // log_2 of the size of a GC page

// when we're marking things, we need to increment their generation. This
// is, unfortunately, not quite an increment since we need to saturate
// at NUM_GENERATIONS - 1. This little macro deals with the problem.
// (just a note: ++ is bad here, since we don't necessarily want to mutate
// the variable we get in)
#define INCREMENT_GENERATION(x) ((x==(NUM_GENERATIONS-1))?x:(x+1))

// we do a lot of casting back and forth from pointers to unsigned longs,
// mostly for comparison and math purposes. This typedef shortens typing
// time. (UWORD == "Unsigned Word")
typedef unsigned long UWORD;

// I tried to write this using as few forward declarations as possible, but
// a few become necessary for various reasons:
static void init(void);
static void garbage_collect(int force_full);
static int is_marked(void *p);

// Note: This is not the "real" definition of a type tag. However, in order
// to avoid messy alignment issues, we define it as the following:
typedef unsigned short Type_Tag;

// Tags for a few objects defined in this system
#define _num_tags			259
#define gc_weak_array_tag		256
#define gc_finalization_tag		257
#define gc_finalization_weak_link_tag	258

// For tagged items, we use these little callbacks to make the system a
// little more general. These tables hold the callbacks for us
Size_Proc size_table[_num_tags];
Mark_Proc mark_table[_num_tags];
Fixup_Proc fixup_table[_num_tags];

// The stacks MzScheme uses to talk to us
void **GC_variable_stack;
void *stack_base;

// there are times when we allocate something and really don't want to
// have a collection run behind our backs. This boolean will stifle any
// collections that might occur.
static int gc_deny = 0;

// the top generation we're collecting. we declare this here because we
// need it here, although that's probably not a particularly good reason
static int mark_gen = 0;
static int mark_owner = 0;


//------------------------------------------------------------------------------
// OS Memory routines: ifdefs give us the right allocators givin whatever OS
// we have available to us
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// OWNER SETS: The point of rewriting this monstrosity was to allow accounting
// across custodians. This way we can watch as sub-custodians grow and shrink
// in size, and kill custodians that are starting to use too much memory. This
// is not a trivial task from any angle; even keeping track of who owns what
// is tricky.
//
// In order to denote an 'owner', we first realize that an owner may be (and 
// usually is) more than one custodian. Therefor instead of talking about single
// owners, we talk about "owner sets". Below are the routines for manipulating
// these.
//
// Note: According to the semantics of MzScheme, the memory which is declared
// owned by Custodian C is also owned by all the parents of C.
//
// Note: It is believed (by me), that the most common thing done between two
// owner sets is an equality test. Thus we've implemented this in a way that
// equality is very quick.
//------------------------------------------------------------------------------
#define OFFSET_BITS		(LOG_MPAGE_SIZE - LOG_WORD_SIZE)
#define OWNER_BITS		((1 << (LOG_WORD_SIZE+3)) - (OFFSET_BITS + 1))

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
  UWORD mem_use[NUM_GENERATIONS];
} OwnerTableEntry;

static OwnerTableEntry *owner_table[(1 << OWNER_BITS)];
static int owner_table_top = 1;

static void dump_ownerset_information(FILE *file) {
  int i;

  fprintf(file, "\nOwner Set Information:\n");
  fprintf(file, "Owner table entry:         Type:         Memory usage:\n");
  fprintf(file, "--------------------------------------------------------\n");
  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      int j;
      long memsize = 0;
      for(j = 0; j < NUM_GENERATIONS; j++) {
	memsize += owner_table[i]->mem_use[j];
      }
      fprintf(file, "%010i                 %10s    %li\n", i,
	      owner_table[i]->creator ? "Singular  " : "Unioned   ",
	      memsize);
    }
  }
}

static int custodian_to_ownerset(Scheme_Custodian *cust) {
  int i, j;

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
      for(j = 0; j < NUM_GENERATIONS; j++) {
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

static int ownerset_member_p(Scheme_Custodian *cust, int set) {
  CustodianList *work;

  for(work = owner_table[set]->custs; work; work = work->next) {
    if(work->cust == cust) {
      return 1;
    }
  }
  return 0;
}

static int ownerset_subset_p(int set1, int set2) {
  CustodianList *work;

  for(work = owner_table[set1]->custs; work; work = work->next) {
    if(!ownerset_member_p(work->cust, set2)) {
      return 0;
    }
  }
  return 1;
}

static int ownerset_union(int set1, int set2) {
  UnionList *work;
  int res;
  
  work = owner_table[set1]->unions;
  while(work) {
    if(work->with_owner == set2) {
      return work->result;
    } else work = work->next;
  }

  // damn, this isn't memoized. See if one is a subset of the other
  if(ownerset_subset_p(set1, set2)) {
    // yup. 1 is a subset of 2
    res = set2;
  } else if(ownerset_subset_p(set2, set1)) {
    // yup. 2 is a subset of 1
    res = set1;
  } else {
    // nope. 
    CustodianList *work = NULL;
    CustodianList *combo = NULL;
    int i;

    // add every element in set2
    for(work = owner_table[set2]->custs; work; work = work->next) {
      CustodianList *temp = (CustodianList*)malloc(sizeof(CustodianList));
      temp->cust = work->cust;
      temp->next = combo;
      combo = temp;
    }
    // add every element in set1 that's not in set2
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
    for(i = 0; i < NUM_GENERATIONS; i++) {
      owner_table[res]->mem_use[i] = 0;
    }
    if(res >= owner_table_top) {
      owner_table_top = res + 1;
    }
  }

  // res is now the correct resultant value. Tack on some things
  // so that we can just pull this as memoized next time
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
  
  // now that's all taken care of, so return

  return res;
}

// finally, this macro does equivalence. I do this as a macro because it's
// entirely possible I'll junk this whole section and replace it, and I'd
// like to do that as non-destructively as possible
#define ownerset_eq(own1,own2)		(own1 == own2)

// finally, this is a little routine which returns the owner set associated
// with the currently running thread/custodian. Note that there's some 
// aggravation here becuase there's a brief point in time where we're
// running without a custodian. Therefor, this routine has three basic states;
// before custodian, initializing and after custodian
int current_owner() { 
  static int initialized = 0;
  
  if(initialized) {
    Scheme_Custodian *c;
    c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
    return custodian_to_ownerset(c);
  } else if(scheme_current_thread && scheme_current_thread->config) {
    OwnerTableEntry *oldent = owner_table[0];
    int res;
    
    initialized = 1;
    owner_table[0] = NULL;
    res = current_owner();
    owner_table[0]->mem_use[0] = oldent->mem_use[0];
    free(oldent);
    init();
    return res;
  } else {
    if(!owner_table[0]) {
      int i;
      owner_table[0] = (OwnerTableEntry*)malloc(sizeof(OwnerTableEntry));
      owner_table[0]->creator = NULL;
      owner_table[0]->custs = NULL;
      owner_table[0]->unions = NULL;
      for(i = 0; i < NUM_GENERATIONS; i++) {
	owner_table[0]->mem_use[i] = 0;
      }
    }
    return 0;
  }
}

static void prep_ownerset_information(int mark_gen) {
  int i, j;

  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      for(j = 0; j <= mark_gen; j++) {
	owner_table[i]->mem_use[j] = 0;
      }
    }
  }
}


inline void ownerset_delete(int i) {
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

static void fixup_ownersets() {
  int i;

  // First, clear out any entries whose creators are custodians which
  // no longer exist and fixup the ones that do
  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i] && owner_table[i]->creator) {
      if(is_marked(owner_table[i]->creator)) {
	gcFIXUP(owner_table[i]->creator);
      } else {
	ownerset_delete(i);
      }
    }
  }

  // Then clear out all the entries that contain unmarked custodians
  // in their list and/or fixup the good custodians
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

  // Finally, clear out any unions that involve a set we've nulled out
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

static long get_custodian_mem_usage(Scheme_Custodian *cust) {
  int i;
  long res = 0;

  for(i = 0; i < owner_table_top; i++) {
    if(owner_table[i]) {
      if(ownerset_member_p(cust, i)) {
	int j;
	for(j = 0; j < NUM_GENERATIONS; j++) {
	  res += owner_table[i]->mem_use[j];
	}
      }
    }
  }
  return res;
}

//------------------------------------------------------------------------------
// MPAGE ROUTINES: These are the routines the memory system uses to interact
// with pages. This includes allocation, copying and page lookups.
//
// Note that this is not a "strict" copying collector in that memory is
// not double allocated; the collector allocates the space for the new items
// as needed by the copy routine. This saves us a little space and allows
// transparant compaction across memory pages. In the worst case (all memory
// at GC time is reachable) this will use as much memory as a normal copying
// collector, but this is rarely the case in practice (I believe).
//------------------------------------------------------------------------------

// there are a number of very small bits of information that we'd like
// to know on a page-by-page basis. the 'flags' field of this structure
// stores this information for us. 
typedef struct MPageFlags {
  unsigned int type      : 3;
  unsigned int mark      : 2;
  unsigned int bigpage   : 1;
  unsigned int bpointers : 1;
  unsigned int gen       : 9;
} MPageFlags;

typedef struct MPage {
  struct MPage *next;
  MPageFlags flags; // the flags for this page. See below
  unsigned int size; // the current size of this page
  unsigned int former_size; // used in mark propogation
} MPage;
#define PAGE_HEADER_SIZE       gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(sizeof(MPage)))
#define MPAGE_SIZE	        (1 << LOG_MPAGE_SIZE)

static void *malloc_mempages(UWORD size);

// every page allocated in the system has a kind attached to it; this is
// something like a BeBop'd system although we don't cut things very fine
// at all. The values for these types are below:
#define MPAGE_TAGGED			0
#define MPAGE_ATOMIC			1
#define MPAGE_ARRAY			2
#define MPAGE_TARRAY			3
#define MPAGE_XTAGGED			4

// each allocated item in the system has a header associated with it.
// the header keeps a bunch of information we generally find useful at
// different points in the system. Because of the way we store them 
// internally, the format of the header is different for tagged and 
// nontagged items. 
typedef struct ObjHeader {
  unsigned int owner : OWNER_BITS;
  unsigned int mark  : 1;
  unsigned int size : OFFSET_BITS;
} ObjHeader;

// for various reasons (read: speed), we seperate out generation 0 into
// a bunch of different pointers we can directly access. However, since 
// doing this makes other portions of the code tedious, we *also* include
// a little array to access them. Fun, eh?
static MPage *gen0[6] = { NULL, NULL, NULL, NULL, NULL, NULL };

// the following is the list of pages we're currently collecting out of.
// during non-collection points this should always be null, since anything
// else will end up being a dangling pointer.
static MPage *collect_pages = NULL;

// the following array holds information about the older generations. Note
// that the zero index is unused, but the wasted word of memory is probably
// nothing compared from not having to do some subtractions on every access
static MPage *gen[NUM_GENERATIONS];

// this is just a standard place where we're going to point any calls to
// allocation routines which want a zero sized object. This makes life
// elsewhere much easier, since we don't have to watch for such oddities
static char zero_sized[4];

// this is a constant which defines the maximum size for an object which
// is going to be allocated to a normal page
#define MAX_NORMAL_SIZE		(MPAGE_SIZE - PAGE_HEADER_SIZE)

// the current size of our heap in bytes
static UWORD heap_size = 0; 
// the size at which we should run a collection
static UWORD size_to_collect_at = INITIAL_GENERATION_SIZE;

// during a considerable number of activities we'd like to find the
// page record for a pointer we have around. since we do things like
// this quite a bit, we waste a bunch of memory to make the lookup all
// the much faster. Hence the defines and table below:
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

// the table:
MPage *mpage_map[1 << FLINDEX_SIZE][1 << SLINDEX_SIZE];

// add a newly created mpage to the set.
static void add_to_page_map(MPage *work) {
  MPage *cur = work;
  long size_left = work->flags.bigpage ? work->size : MPAGE_SIZE;
  
  while(size_left > 0) {
    mpage_map[FLINDEX(cur)][SLINDEX(cur)] = work;
    size_left -= MPAGE_SIZE;
    cur = (MPage*)((UWORD)cur + MPAGE_SIZE);
  }
}

// find a page in the map given a pointer.
#define find_page(p)		(mpage_map[FLINDEX(p)][SLINDEX(p)])

// remove a mpage from the set once we're done with it
static void rem_from_page_map(MPage *work) {
  MPage *cur = work;
  long size_left = work->flags.bigpage ? work->size : MPAGE_SIZE;

  while(size_left > 0) {
    mpage_map[FLINDEX(cur)][SLINDEX(cur)] = NULL;
    size_left -= MPAGE_SIZE;
    cur = (MPage*)((UWORD)cur + MPAGE_SIZE);
  }
}

// initialize the memory map to it's initial values. this is actually
// probably not necessary if the c compiler gives us zero initialized
// arrays, but just to be on the safe side we don't assume this. Note
// that unfortunately several pages may have been allocated before this
// routine runs, so we need to re-add those back in. Luckily it is 
// guaranteed that this will run before any collections, so we only
// need to add back the things in gen0.
static void initialize_mpage_subsystem(void) {
  MPage *work;
  int i;

  bzero(mpage_map, (1<<FLINDEX_SIZE)*(1<<SLINDEX_SIZE)*sizeof(MPage*));
  // now go back and fix things up
  for(i = 0; i < 6; i++) {
    for(work = gen0[i]; work; work = work->next) {
      add_to_page_map(work);
    }
  }
}

// the following routine is used by all the allocation "front ends" to 
// allocate new memory for MzScheme routines. "force_big" tells us that
// we should consider this new thing a bigpage as it will have pointers
// to its interiors.
inline void *alloc_new_bits(size_t size_in_bytes, int type, int force_big) {
  MPage *page;
  void *retval;
  long size_in_words;
  ObjHeader *info;

  // zero sized items all point to the same thing
  if(!size_in_bytes) {
    return zero_sized;
  }

  // since we're going to drop a header on the front of this object, we
  // convert the number of bytes the caller wants to the number of words
  size_in_words = gcBYTES_TO_WORDS(size_in_bytes);
  // We need an extra word for the header here
  size_in_words++;
  // and then convert it back
  size_in_bytes = gcWORDS_TO_BYTES(size_in_words);

  // see if we should stop and do a collection
  heap_size += size_in_bytes;
  if(heap_size > size_to_collect_at)
    garbage_collect(0);

  // see if we should be allocating this as a big page or a normal page
  if((size_in_bytes > MAX_NORMAL_SIZE) || force_big) {
    // allocate this as a bigpage. bigpages go on the end of the mpage
    // list so as not to interfere with the following routine. The *only*
    // reason for this is speed tuning.
    page = (MPage*)malloc_mempages(size_in_bytes+PAGE_HEADER_SIZE);
    page->flags.type = type;
    page->flags.mark = 0;
    page->flags.bigpage = 1;
    page->flags.bpointers = 0;
    page->flags.gen = 0;
    page->size = PAGE_HEADER_SIZE + size_in_bytes;
    page->former_size = 0;
    page->next = gen0[5];

    gen0[5] = page;
    add_to_page_map(page);
    retval = (void*)((UWORD)page + PAGE_HEADER_SIZE);
  } else {
    // we have two cases. first we see if we can add this to a pre-existing
    // page or not. Note that this is tuned for speed and doesn't try real
    // hard to put this on an existing page. 
    if(gen0[type] && ((gen0[type]->size + size_in_bytes) < MPAGE_SIZE)) {
      // we can put this on the page. yay!
      page = gen0[type];
      retval = (void*)((UWORD)page + page->size);
      page->size = page->size + size_in_bytes;
    } else {
      // we have to make a new page. oh well.
      page = (MPage*)malloc_mempages(MPAGE_SIZE);
      page->flags.type = type;
      page->flags.mark = 0;
      page->flags.bigpage = 0;
      page->flags.bpointers = 0;
      page->flags.gen = 0;
      page->size = PAGE_HEADER_SIZE + size_in_bytes;
      page->former_size = 0;
      page->next = gen0[type];

      gen0[type] = page;
      add_to_page_map(page);
      retval = (void*)((UWORD)page + PAGE_HEADER_SIZE);
    }
  }

  // drop in the information we need in the header
  info = (ObjHeader*)retval;
  info->owner = current_owner();
  info->mark = 0;
  info->size = size_in_words;

  // account for this stuff. 
  ownerset_account_memory(info->owner, 0, size_in_bytes);

  // advance the pointer
  retval = (void*)((UWORD)retval + 4);

  return retval;
}

// this is a performance hack, and a serious one at that.
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

// the following routine is used in the marking system to copy a reachable
// data item from the old pages to the newpages. This routine is slightly
// less speed-tuned than the last, which results in slightly increased 
// runtimes and (ideally) slightly decreased memory fragmentation.
static void *copy_bits(void *old_ptr, UWORD size_in_words, int type, int tgen) {
  UWORD size_in_bytes = gcWORDS_TO_BYTES(size_in_words);
  MPage *work = gen[tgen];
  void *retval;

  // search through the list of pages for this generation and see if we
  // can find an extant page with enough space for this item
  while(work && !((work->flags.type == type) && !work->flags.bigpage && 
		  ((work->size + size_in_bytes) < MPAGE_SIZE)))
    work = work->next;
	
  if(work) {
    // Good, there already exists a place for us to put this. This is rather
    // nice, actually.
    retval = (void*)((UWORD)work + work->size);
    // so copy the new information
    memcpy(retval, old_ptr, size_in_bytes);
    // bump up the size counter on the page
    work->size += size_in_bytes;
    // and return the new pointer. note that unlike alloc_new_bits, we don't
    // add 4 here. someone will do that later.
    add_to_mucked_pages(work);
    return retval;
  } else {
    // There isn't already a page for us, so we have to add one. This is
    // a bit annoying but necessary.
    work = (MPage*)malloc_mempages(MPAGE_SIZE);
    work->flags.type = type;
/*     work->flags.mark = 0; */
/*     work->flags.bigpage = 0; */
/*     work->flags.bpointers = 0; */
    work->flags.gen = tgen;
    work->size = work->former_size = PAGE_HEADER_SIZE;
    work->size += size_in_bytes;
    work->next = gen[tgen];

    gen[tgen] = work;
    add_to_page_map(work);
    retval = (void*)((UWORD)work + PAGE_HEADER_SIZE);
    memcpy(retval, old_ptr, size_in_bytes);
    add_to_mucked_pages(work);
    return retval;
  }
}

// this is a silly little helper routine that helps us make nice output.
static char *flags_to_strtype(MPageFlags flags) {
  if(flags.bigpage) {
    switch(flags.type) {
    case MPAGE_TAGGED: return "BTGD";
    case MPAGE_ATOMIC: return "BATM";
    case MPAGE_ARRAY: return "BARR";
    case MPAGE_TARRAY: return "BTAR";
    case MPAGE_XTAGGED: return "BXTG";
    }
  } else {
    switch(flags.type) {
    case MPAGE_TAGGED: return "TAGD";
    case MPAGE_ATOMIC: return "ATOM";
    case MPAGE_ARRAY: return "ARRY";
    case MPAGE_TARRAY: return "TARR";
    case MPAGE_XTAGGED: return "XTGD";
    }
  }
  return "XXXX";
}

// this routine dumps information about the mpage allocations to the
// given file. this is used in the outer dump routine and in debugging
static void dump_mpage_information(FILE *file) {
  MPage *work;
  int i;
  char *format = "%10p                  %i      %5i    %3s\n";
  UWORD pages = 0, possible = 0;

  fprintf(file, "Memory page information:\n");
  fprintf(file, "Page:                       Gen:   Size:    Type:\n");
  fprintf(file, "-------------------------------------------------\n");
  for(i = 0; i < 6; i++) {
    for(work = gen0[i]; work; work = work->next) {
      fprintf(file, format, work, work->flags.gen, work->size, 
	      flags_to_strtype(work->flags));
      pages++;
      if(work->flags.bigpage) {
	int blocks = (work->size/MPAGE_SIZE)+(((work->size%MPAGE_SIZE)>0)?1:0);
	possible += (blocks * MPAGE_SIZE);
      } else possible += MPAGE_SIZE;
    }
  }
  for(i = 1; i < NUM_GENERATIONS; i++) {
    for(work = gen[i]; work; work = work->next) {
      fprintf(file, format, work, work->flags.gen, work->size, 
	      flags_to_strtype(work->flags));
      pages++;
      if(work->flags.bigpage) {
	int blocks = (work->size/MPAGE_SIZE)+(((work->size%MPAGE_SIZE)>0)?1:0);
	possible += (blocks * MPAGE_SIZE);
      } else possible += MPAGE_SIZE;
    }
  }
  fprintf(file, "\n");
  fprintf(file, "%li pages using %li of %li bytes (%2.1f%% wasted)\n",
	  pages, heap_size, 
	  possible, 100 - (((float)heap_size / (float)possible) * 100));
}

//------------------------------------------------------------------------------
// MID-LEVEL ALLOCATOR: This allocator gives us pages we can use in the mpage
// routines below. Since this is a copying collector (first) and for owner
// routines we want to know the biggest heap size (second), this mid-level 
// allocator is the "main" one. 
//
// Note that we're killing startup time with this allocator at the moment.
//------------------------------------------------------------------------------
#define MAX_HEAP_SIZE		(1024 * 1024 * 1024) // FIXME: Should be syscall
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

//------------------------------------------------------------------------------
// ACCOUNTING HOOKS: These procedures implement the interface which allows
// users to set hooks into the accounting system
//------------------------------------------------------------------------------
typedef struct AccountHook {
  unsigned short type; 
  int owner;
  Scheme_Custodian *cust;
  unsigned long bytes;
  Scheme_Object *f;
  struct AccountHook *next;
} AccountHook;

static AccountHook *account_hooks = NULL;
static UWORD total_requires = 0;

static void mark_account_hooks(void) {
  AccountHook *cur = account_hooks;
  
  while(cur) {
    mark_owner = cur->owner;
    gcMARK(cur->f);
    cur = cur->next;
  }
}

static void fixup_account_hooks(void) {
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
}

static void run_applicable_account_hooks(void) {
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
}

//------------------------------------------------------------------------------
// MARKING: Of course in a collector we have to have a marking system so that
// we know which variables are reachable. In our system, we mark things by
// copying to a new location, setting the mark field in the header and dropping
// in a forward.
//
// Of course, things are never *quite* that simple. Since we're constructing
// information on who owns what as we go, there is a bit of extra headache
// involved in setting that information straight. More on that in GC_mark
//------------------------------------------------------------------------------
 
// these are the values of the three marks we use. their meaning is the
// standard one (white == unmarked, gray == marked not propped, black
// == marked and propped)
#define MARK_WHITE	0
#define MARK_GRAY	1
#define MARK_BLACK	2

// we've got a couple cases for the is_marked primitive. Since we're a 
// generational collector, we don't mark things in generations above the ones
// we're collecting. However, it's important that we treated such items as
// marked since they will continue to exist after collection finishes
static int is_marked(void *p) {
  MPage *page = find_page(p);
  
  if(!page) 
    return 0;

  if(page->flags.gen > mark_gen)
    return 1;

  if(page->flags.bigpage) {
    return page->flags.mark;
  } else {
    ObjHeader *info = (ObjHeader*)((UWORD)p - 4);
    return info->mark;
  }
}

void GC_mark(const void *p) {
  MPage *page;
  int owner;

  if(!p || ((UWORD)p & 0x1))
    return;

  // if we can't find the page, either this is a pointer that's not into
  // our heap or into an older generation. In either case, we don't need
  // to do anything.
  page = find_page((void*)p);
  if(!page || (page->flags.gen > mark_gen)) {
    return;
  }

  owner = mark_owner;
  // check to see what kind of page we're marking on
  if(page->flags.bigpage) {
    // We're on a bigpage. The first thing we do is determine who the owner
    // of this page should be. If the mark is white, then the owner is the
    // owner passed to GC_mark. Otherwise, the owner is the union of
    // the two owner sets. Then we need to update the mark on the page. 
    // If the owner changes and the mark is black, we might set it back to
    // gray if the compiler flags want us to try harder.
    switch(page->flags.mark) {
      case MARK_WHITE: {
	MPage *temp = collect_pages;
	ObjHeader *info = (ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE);
	
	info->owner = owner;
	page->flags.mark = MARK_GRAY;
 	page->flags.gen = INCREMENT_GENERATION(page->flags.gen); 
	ownerset_account_memory(owner, page->flags.gen, page->size);
	// we also need to move this out of collect_pages and into
	// the older generation so we remember to propogate its marks
	while(temp && (temp->next != page)) 
	  temp = temp->next;
	if(temp) {
	  temp->next = page->next;
	  page->next = gen[page->flags.gen];
	  gen[page->flags.gen] = page;
	} else if(collect_pages == page) {
	  collect_pages = page->next;
	  page->next = gen[page->flags.gen];
	  gen[page->flags.gen] = page;
	} else {
	  fprintf(stderr, "Unexpected case in mark of big page (%p)!\n", page);
	  abort();
	}
	add_to_mucked_pages(page);
	break;
      }

      case MARK_GRAY: {
	ObjHeader *info = (ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE);
	if(!ownerset_eq(info->owner, owner)) {
	  ownerset_account_memory(info->owner, page->flags.gen, -page->size);
	  info->owner = ownerset_union(info->owner, owner);
	  ownerset_account_memory(info->owner, page->flags.gen, page->size);
	}
	break;
      }
	
      case MARK_BLACK: {
	ObjHeader *info = (ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE);
	if(!ownerset_eq(info->owner, owner)) {
	  ownerset_account_memory(info->owner, page->flags.gen, -page->size);
	  info->owner = ownerset_union(info->owner, owner);
	  ownerset_account_memory(info->owner, page->flags.gen, page->size);
#ifdef GC_ACCOUNT_TRY_HARDER
	  propogate_changed_owner(p);
#endif       
	}
      }
    }
  } else {
    ObjHeader *info = (ObjHeader*)((UWORD)p - 4);
    
    if(info->mark) {
      // we've already marked this. However, don't think this lets us
      // get off free; we still have some work to do. Pull up the
      // forward
      void *newplace = *(void**)p;
      ObjHeader *newinfo = (ObjHeader*)((UWORD)newplace - 4);

      // see if the owners are the same. If they are, we don't need to
      // worry about anything. If they aren't, we've got some issues
      if(!ownerset_eq(newinfo->owner, owner)) {
	// crap. well, regardless we're going to use the right owner
	ownerset_account_memory(newinfo->owner, 
				INCREMENT_GENERATION(mark_gen),
				-newinfo->size);
	newinfo->owner = ownerset_union(newinfo->owner, owner);
	ownerset_account_memory(newinfo->owner, 
				INCREMENT_GENERATION(mark_gen),
				newinfo->size);
	// now the question is whether or not we're going to fix the
	// owners in anything downstream. Leave that up to whoever
	// built the system
#ifdef GC_ACCOUNT_TRY_HARDER
	propogate_changed_owner(newplace);
#endif
      }
    } else {
      // we've never marked this. this is the "simple" case
      UWORD size = info->size;
      void *newplace = copy_bits((void*)p-4, size, page->flags.type,
				 INCREMENT_GENERATION(mark_gen));
      ObjHeader *newinfo = (ObjHeader*)newplace;
      // fix the header
      info->mark = 1;
      newinfo->owner = owner;
      ownerset_account_memory(owner, INCREMENT_GENERATION(mark_gen),
			      newinfo->size);
      // set in the forward
      *(void**)p = (void*)((UWORD)newplace + 4);
    }
  }
}

//------------------------------------------------------------------------------
// IMMOBILE BOXES: Immobile boxes are used to store pointer for program
// points which require an unmoving location. Note that the pointer in the
// immobile box may change locations, but the box itself will not.
//------------------------------------------------------------------------------
typedef struct ImmobileBox {
  void *p;
  int owner; // this needs to be second or Window.cc dies
  struct ImmobileBox *next;
} ImmobileBox;

// this keeps the list of immobile boxes for the system
static ImmobileBox *immobile_boxes = NULL;

// This routine is used to mark all the pointers saved in immobile
// boxes. These are used as roots of the collection system.
static void mark_immobile_boxes(void) {
  ImmobileBox *ibox = immobile_boxes;
  while(ibox) {
    mark_owner = ibox->owner;
    GC_mark(ibox->p);
    ibox = ibox->next;
  }
}

// this routine is used to fix up all the pointers saved in immobile
// boxes, since they will have moved during collection.
static void fixup_immobile_boxes(void) {
  ImmobileBox *ibox = immobile_boxes;

  while(ibox) {
    gcFIXUP(ibox->p);
    ibox = ibox->next;
  }
}

// it is possible (in fact, mred does do this) for immobile boxes to get
// allocated before we have an owner to blame things on. In this case,
// we need to run through and fix up the boxes we've allocated.
static void initialize_immobile_subsystem(void) {
  ImmobileBox *ibox = immobile_boxes;
  while(ibox) {
    ibox->owner = current_owner();
    ibox = ibox->next;
  }
}

//------------------------------------------------------------------------------
// ROOTS: Roots are used as (obviously) roots of the collection system. Roots
// are sets of pointers which point into the dynamic heap to start of the
// system. This version is a bit more complicated than the original (compact.c)
// version due to the added complication of OwnerSets.
//
// Rather than the flat malloc'd array that compact.c uses, this version uses
// a linked list of "root sets" to track roots. Each owner set has its own
// root set, one per owner. The root sets, in turn, are a linked list of 
// start and end pointers. These are kept sorted and overlapping sets are
// combined.
//
// FIXME: Would the original implementation speed up or slow down these
// routines if they were used in each root set?
//------------------------------------------------------------------------------

// this is the structure for the roots within the root set
typedef struct Root {
  void **start; // the start of this set of root pointers
  void **end; // the end of this set of root pointers
  struct Root *next; // the next set of root pointers
} Root;

// this is the structure for root sets
typedef struct RootSet {
  int owner; // the owner set associated with this set of roots
  Root *roots; // the actual roots
  struct RootSet *next; // the next RootSet
} RootSet;

// This is holds all the roots of the system
static RootSet *roots = NULL;

// Add a new set of roots (from start to end) to the applicable root
// set.
static void add_root(void *start, void *end, int owner) {
  RootSet *work = roots;
  int done = 0;

  // search to see if we already have a root set for this owner
  while(work && !done) {
    done = (work->owner == owner);
    if(!done) work = work->next;
  }

  if(done) {
    // A root set for this owner already exists. This is a rather 
    // complicated case, since we have to insert the root into the
    // array while watching for overlaps.
    Root *cur = work->roots, *prev = NULL;

    // First off, search for roughly the appropriate place to put this
    // new root
    while(cur && ((UWORD)cur->start < (UWORD)start)) {
      prev = cur;
      cur = cur->next;
    }

    // there are several possible case for us at this point. We need
    // to treat each one separately.
    // FIXME: Some of these cases could probably be collapsed
    if(cur && prev) {
      // we're somewhere in the middle of the list, so we need to
      // check for overlap on both sides. This is the most complicated
      // case as we actually have four more possible subcases.
      if(((UWORD)prev->end >= (UWORD)start) 
	 && ((UWORD)cur->start <= (UWORD)end)) {
	// this root joins these two adjacent roots, which is nice because
	// it frees up a little memory. We use prev for the modified root
	// for no particular reason.
	// prev->start = prev->start;
	prev->end = cur->end;
	prev->next = cur->next;
	free(cur);
      } else if((UWORD)prev->end >= (UWORD)start) {
	// this root and the root at 'prev' overlap, so we only need to
	// modify the previous one. Note that technically there is the
	// possibility that this item is completely subsumed by the extant
	// root.
	prev->end = ((UWORD)prev->end < (UWORD)end) ? (void**)end : prev->end;
      } else if((UWORD)cur->start <= (UWORD)end) {
	// this root and the root at 'cur' overlap, so we only need to
	// modify the current one. Again we check for subsumption.
	cur->start = ((UWORD)cur->start>(UWORD)start)?(void**)start:cur->start;
      } else {
	// this root is completely new.
	Root *newroot = (Root*)malloc(sizeof(Root));
	newroot->start = (void**)start;
	newroot->end = (void**)end;
	newroot->next = cur;
	prev->next = newroot;
      }
    } else if(cur) {
      // we're at the very front of the list. Thus we only need to check
      // for overlap to the "right" of us (further down the list)
      if((UWORD)end >= (UWORD)cur->start) {
	// these do overlap, so we munge the start pointer of the pre-extant
	// root and we're done. 
	cur->start = (void**)start;
      } else {
	// these do not overlap, so we have to add in this new root to
	// the set.
	prev = (Root*)malloc(sizeof(Root));
	prev->start = (void**)start;
	prev->end = (void**)end;
	prev->next = cur;
	work->roots = prev;
      }
    } else if(prev) {
      // we're at the very end of the list. Thus we only need to check
      // for overlap to the "left" of us (further up the list)
      if((UWORD)prev->end >= (UWORD)start) {
	// these do overlap, so we munge the end pointer of the pre-extant
	// root and we're done.
	prev->end = (void**)end;
      } else {
	// these do not overlap, so we're "safe" and should just add this
	// to the end.
	cur = (Root*)malloc(sizeof(Root));
	cur->start = (void**)start;
	cur->end = (void**)end;
	cur->next = NULL;
	prev->next = cur;
      }
    } else {
      // there was no list. This should never happen, so scream and
      // die.
      printf("SERIOUS INTERNAL ERROR: Attempt to add root to null root set!\n");
      abort();
    }
  } else {
    // There is no root set for this owner, so we add one. This is the
    // easy case for this routine.
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

// this routine marks all the roots for us. this is called by the collector
// to start things off
static void mark_roots(void) {
  RootSet *rtsets;
  Root *rts;

  for(rtsets = roots; rtsets; rtsets = rtsets->next) {
    int owner = rtsets->owner;
    for(rts = rtsets->roots; rts; rts = rts->next) {
      void **start = rts->start;
      void **end = rts->end;
      while(start < end) {
	mark_owner = owner;
	GC_mark(*start);
	start++;
      }
    }
  }
}

// this routine fixes up all the roots for us in the last stages of 
// collection
static void fixup_roots(void) {
  RootSet *rtsets = roots;
  Root *rts;

  
  for(rtsets = roots; rtsets; rtsets = rtsets->next) {
    if(!owner_table[rtsets->owner]) {
      rtsets->owner = 0;
    }
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

// this routine dumps information about the roots to the given file
// handle. this is used in the main dump routine plus in debugging.
static void dump_root_information(FILE *file) {
  RootSet *rtsets;
  Root *rts;

  fprintf(file, "Root information:\n");
  for(rtsets = roots; rtsets; rtsets = rtsets->next) {
    for(rts = rtsets->roots; rts; rts = rts->next) {
      fprintf(file, "[%10p - %10p] Owner = %i\n", 
	      rts->start, rts->end, rtsets->owner);
    }
  }
  fprintf(file, "\n");
}

// since there's an unfortunate period in the very beginning where 
// we don't know who owns what, this little function is called to
// fix a little munging we've done
static void initialize_roots_subsystem(void) {
  RootSet *rtsets = roots;
  
  if(!rtsets) {
    printf("SERIOUS INTERNAL ERROR: No roots allocd before init_roots!\n");
    abort();
  }
  if(rtsets->next) {
    printf("SERIOUS INTERNAL ERROR: More than one set before init_roots!\n");
    abort();
  }

  rtsets->owner = current_owner();
}

//----------------------------------------------------------------------------
// WEAK BOXES AND ARRAYS: These are those bizarre items where we want to 
// point to an object but don't want to force it to be reachable. For a more
// thorough explanation, please see the README.
//------------------------------------------------------------------------------

typedef struct WeakArray {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct WeakArray *next;
  void *data[1];
} WeakArray;

static WeakArray *weak_arrays;

static int size_weak_array(void *p)
{
  WeakArray *a = (WeakArray *)p;

  return gcBYTES_TO_WORDS(sizeof(WeakArray)+((a->count - 1) * sizeof(void *)));
}

static int mark_weak_array(void *p)
{
  WeakArray *a = (WeakArray *)p;

  gcMARK(a->replace_val);

  a->next = weak_arrays;
  weak_arrays = a;

  return gcBYTES_TO_WORDS(sizeof(WeakArray)+((a->count - 1) * sizeof(void *)));
}

static int fixup_weak_array(void *p)
{
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

// after collection we want to kill out the pointers in the weak array
// list that have died.
static void clean_up_weak_arrays(void) {
  WeakArray *wa;
  for(wa = weak_arrays; wa; wa = wa->next) {
    void **data;
    int i;

    data = wa->data;
    for(i = wa->count; i--; ) {
      void *p = data[i];
      if(p && !is_marked(p))
	data[i] = wa->replace_val;
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

static Type_Tag weak_box_tag = 42; // set by MzScheme
static WeakBox *weak_boxes;

static int size_weak_box(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(WeakBox));
}

static int mark_weak_box(void *p)
{
  WeakBox *wb = (WeakBox *)p;
    
  gcMARK(wb->secondary_erase);

  if (wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }

  return gcBYTES_TO_WORDS(sizeof(WeakBox));
}

static int fixup_weak_box(void *p)
{
  WeakBox *wb = (WeakBox *)p;
    
  gcFIXUP(wb->secondary_erase);
  gcFIXUP(wb->val);

  return gcBYTES_TO_WORDS(sizeof(WeakBox));
}


// after collection we want to kill out the pointers in the weak box
// list that have died.
static void clean_up_weak_boxes(void) {
  WeakBox *wb;
  for(wb = weak_boxes; wb; wb = wb->next) {
    if(!is_marked(wb->val)) {
      wb->val = NULL;
      if(wb->secondary_erase) {
	*(wb->secondary_erase + wb->soffset) = NULL;
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

//------------------------------------------------------------------------------
// FINALIZERS: Finalizers are used to invoke code after a given object would
// be freed. The finalizer system is a bit hairy. See the README for more
// detials.
//------------------------------------------------------------------------------

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

static int size_finalizer(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int mark_finalizer(void *p)
{
  Fnl *fnl = (Fnl *)p;
    
  gcMARK(fnl->next);
  gcMARK(fnl->data);
  /* !eager_level => queued for run: */
  if (!fnl->eager_level) {
    gcMARK(fnl->p);
  }

  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int fixup_finalizer(void *p)
{
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

static int size_finalizer_weak_link(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int mark_finalizer_weak_link(void *p)
{
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
  
  gcMARK(wl->next);

  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int fixup_finalizer_weak_link(void *p)
{
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
    
  gcFIXUP(wl->next);
  gcFIXUP(wl->p);

  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

// after collection we need to run through our list of finals and 
// clear out weak links that have died
static void clean_up_weak_finals(void) {
  Fnl_Weak_Link *wl, *prev, *next;

  prev = NULL;
  for(wl = fnl_weaks; wl; wl = next) {
    next = wl->next;
    if(!is_marked(wl->p)) {
      // will be collected, so remove the link
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

//------------------------------------------------------------------------------
// PROPOGATION: Once we mark the bases, we need to propogate those marks out
// to the rest of the world. This would be trivial if we could allow ourselves
// the luxury of a possibly infinite call stack, but that would be silly. Thus
// we do this with a helper function and a fixpoint iteration.
//------------------------------------------------------------------------------

// big pages have to propogated specially, but rather than be exhautive
// and write all 10 possibilities separately, we condense the bigpage
// handler here. We have to do this because the size field of an ObjHeader
// is invalid in a bigpage and we rely on that elsewhere
static void propogate_bpage_marks(MPage *page) {
  ObjHeader *info = (ObjHeader*)((UWORD)page + PAGE_HEADER_SIZE);
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE + 4);
  void **end = (void**)((UWORD)page + page->size);

  mark_owner = info->owner;
  switch(page->flags.type) {
  case MPAGE_TAGGED: 
    mark_table[*(Type_Tag*)start](start); 
    break;
  case MPAGE_ATOMIC: break;
  case MPAGE_ARRAY: 
    while(start < end) {
      GC_mark(*(start++)); 
    }
    break;
  case MPAGE_TARRAY: {
    Type_Tag tag = *(Type_Tag*)start;
    while(start < end) {
      start += mark_table[tag](start);
    }
    break;
  }
  case MPAGE_XTAGGED: 
    GC_mark_xtagged(start); break;
    break;
  }
}

// these routines take a mpage and propogate any marks that haven't already
// been propogated. this is where the former_size field of mpages come in; 
// they will only mark the pointers from former_size to size rather than 
// redoing the entire page
static void propogate_tagged_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);

  while(start < (void**)((UWORD)page + page->size)) {
    ObjHeader *info = (ObjHeader*)start++;
    mark_owner = info->owner;
    start += mark_table[*(Type_Tag*)start](start);
  }
  page->former_size = page->size;
}

static void propogate_array_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);

  while(start < (void**)((UWORD)page + page->size)) {
    ObjHeader *info = (ObjHeader*)start++;
    UWORD size = info->size;

    while(--size) {
      mark_owner = info->owner;
      GC_mark(*(start++));
    }
  }
  page->former_size = page->size;
}

static void propogate_tarray_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);
  
  while(start < (void**)((UWORD)page + page->size)) {
    ObjHeader *info = (ObjHeader*)start;
    void **tempend = start + info->size;
    Type_Tag tag = *(Type_Tag*)(++start);

    mark_owner = info->owner;
    while(start < tempend) {
      start += mark_table[tag](start);
    }
  }
  page->former_size = page->size;
}

static void propogate_xtagged_marks(MPage *page) {
  void **start = (void**)((UWORD)page + page->former_size);
			
  while(start < (void**)((UWORD)page + page->size)) {
    ObjHeader *info = (ObjHeader*)start;
    mark_owner = info->owner;
    GC_mark_xtagged(start + 1);
    start += info->size;
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


// this is the routine that propogates all the marks. it's your basic 
// fixpoint system in that you keep looping until nothing has been 
// changed.
static void propogate_all_mpages(void) {
  MPageList *mucked, *temp;
  MPage *work;

  while(mucked_pages) {
    mucked = mucked_pages;
    mucked_pages = NULL;
    while(mucked) {
      work = mucked->page;
      // we have to do slightly different things dependent on whether this
      // is a bigpage or a normal page.
      if(work->flags.bigpage) {
	// if we're a bigpage, we only propogate those pages we've marked
	// as gray. 
	if(work->flags.mark == MARK_GRAY) {
	  // Luckily, we can use the the propogation routine as
	  // long as we make sure 'former_size' is PAGE_HEADER_SIZE
	  work->flags.mark = MARK_BLACK;
	  //work->former_size = PAGE_HEADER_SIZE;
	  propogate_bpage_marks(work);
	}
      } else {
	// if we're a normal page, we only need to propogate those pages
	// that have been added on to. this means only the pages where
	// former_size < size.
	if(work->former_size < work->size) {
	  propogate_mpage_marks(work);
	}
      } // if
      temp = mucked;
      mucked = mucked->next;
      free(temp);
    }
  } // while
}

//------------------------------------------------------------------------------
// REPAIRS: After everything has been marked, our to space is going to have
// a lot of pointers pointing back to the old pages. The following routines
// are responsible for fixing these so that they point back into the new 
// space.
//------------------------------------------------------------------------------

// this routine takes a page and repairs all the ptrs in it. It looks 
// a lot like the propogation routine, basically because it does the
// same page walk.
static void repair_bpage_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE + 4);
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

  while(start++ < end) {
    start += fixup_table[*(Type_Tag*)start](start);
  }
}

static void repair_array_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);
  
  while(start < end) {
    ObjHeader *info = (ObjHeader*)start++;
    UWORD size = info->size;

    info->mark = 0;
    while(--size)
      gcFIXUP(*(start++));
  }
}

static void repair_tarray_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);
  
  while(start < end) {
    ObjHeader *info = (ObjHeader*)start;
    void **tempend = start + info->size;
    Type_Tag tag = *(Type_Tag*)(++start);

    info->mark = 0;
    while(start < tempend)
      start += fixup_table[tag](start);
  }
}

static void repair_xtagged_ptrs(MPage *page) {
  void **start = (void**)((UWORD)page + PAGE_HEADER_SIZE);
  void **end = (void**)((UWORD)page + page->size);

  while(start < end) {
    ObjHeader *info = (ObjHeader*)start;

    info->mark = 0;
    GC_fixup_xtagged(start+1);
    start += info->size;
  }
}

inline void repair_mpage_ptrs(MPage *page) {
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

// this is the routine called by garbage_collect to repair all the pointers.
// note that this not only has to repair all the pointers in the new pages,
// but also all the pointers in the older generations which were modified
// hence the dual for loops.
static void repair_mpages(void) {
  MPage *work;
  int i;
  int new_top_gen = INCREMENT_GENERATION(mark_gen);
  int old_start_gen = new_top_gen + 1;

  // repair all the new pages
  for(i = 1; i <= new_top_gen; i++) {
    for(work = gen[i]; work; work = work->next) {
      // note that this routine works just as well for big and normal pages
      repair_mpage_ptrs(work);
      // but if we are at a big page, we need to clear the marks for the
      // next generation
      if(work->flags.bigpage)
	work->flags.mark = MARK_WHITE;
    }
  }

  // repair the old, modded pages
  for(i = old_start_gen; i < NUM_GENERATIONS; i++) {
    for(work = gen[i]; work; work = work->next) {
      if(work->flags.bpointers) {
	repair_mpage_ptrs(work);
      }
    }
  }
}

//------------------------------------------------------------------------------
// OLDER GENERATION MODIFICATIONS: One of the unfortunate points of generational
// collection is that mutations which set pointers from older generations to
// newer generations have to be tracked or we start losing information. Ideally
// these situations won't happen very often, but just in case we need to watch
// things.
//------------------------------------------------------------------------------

// called by the signal handlers when we get an access to a protected page.
// this sets the modded bit in the flags part of the appropriate page
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

// after we've collected, we need to protect the older generations so that
// we can track any modifications that happen to them. 
static void protect_older_pages(void) {
  MPage *work;
  int i;

  for(i = 1; i < NUM_GENERATIONS; i++) {
    for(work = gen[i]; work; work = work->next) {
      if((work->flags.type != MPAGE_ATOMIC) && !work->flags.bpointers)
	protect_pages(work, work->size, 0);
    }
  }
}


// when we start collection, we want to run the marks that are included in
// modified pages. for the purposes of owner tests, we assume that the 
// info on the page is correct. 
static void run_older_marks(int top_gen) {
  MPage *work;
  int i;
  
  for(i = top_gen + 1; i < NUM_GENERATIONS; i++) {
    for(work = gen[i]; work; work = work->next) {
      if(work->flags.bpointers) { 
	work->former_size = PAGE_HEADER_SIZE;
	if(work->flags.bigpage) 
	  propogate_bpage_marks(work);
	else
	  propogate_mpage_marks(work);
      }
    }
  }
}

//------------------------------------------------------------------------------
// SIGNAL HANDLERS: Routines which set up designate_modified to be called at
// the right times.
//------------------------------------------------------------------------------
/* Linux signal handler: */
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

void initialize_memory_protection_subsystem(void) {
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
}

//------------------------------------------------------------------------------
// COLLECTION: This is the main loop which does the collection routine. Note
// that it's unfortunately rather complicated. This has everything to do with
// the rather messy way finalizers are done in MzScheme, unfortunately, and is
// difficult to get out.
//------------------------------------------------------------------------------

// This is a little routine which moves stuff from gen0 and any other
// generations we're collecting out of and moves them to collect_pages.
// this makes a couple things easier and isn't too costly time-wise.
static void shunt_off_collecting_pages(top_gen) {
  MPage *work;
  int i;

  // We're always going to collect the first generation, so initially
  // collect_pages is just that.
  collect_pages = gen0[0];
  // while we're here, null out gen0
  gen0[0] = NULL;

  for(i = 1; i < 6; i++) {
    work = gen0[i];
    while(work && work->next)
      work = work->next;
    if(work) {
      work->next = collect_pages;
      collect_pages = gen0[i];
      gen0[i] = NULL;
    }
  }

  // We also need to add in the pages from any higher generations that
  // are being collected this round
  for(i = 1; i <= top_gen; i++) {
    work = gen[i];
    while(work && work->next)
      work = work->next;
    if(work) {
      work->next = collect_pages;
      collect_pages = gen[i];
      gen[i] = NULL;
    }
  }
}

// This is a little routine which copes with the fact that our finalizer
// list has been trashed by the propogation routines and rebuilds them
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

// at the end of collection, we'll have a whole bunch of pages hanging
// off collect_pages which are now effectively dead. So what we need to
// do is kill them.
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
  int i;

  heap_size = 0;
  for(i = 1; i < NUM_GENERATIONS; i++) {
    for(work = gen[i]; work; work = work->next) {
      heap_size += work->size;
    }
  }
}

static void garbage_collect(int force_full) {
  static short cycle_count = 0;
  static UWORD collection = 0;
  int did_fnls;

  if(!gc_deny) {
    if(force_full) mark_gen = NUM_GENERATIONS - 1;
    else {
      int i, mask = 1 << (NUM_GENERATIONS - 1);
      for(i = NUM_GENERATIONS - 1; i; i--) {
	if(mask & cycle_count)
	  break;
	mask = mask >> 1;
      }
      mark_gen = i;
      cycle_count = (cycle_count + 1) % ((NUM_GENERATIONS * 2) - 1);
    }

    collection++;
    // notify MzScheme that we're about to collect
    if(GC_collect_start_callback)
      GC_collect_start_callback();

    // we don't want collections happening while we're collecting!
    gc_deny = 1;
    
    // initialize the weak and final stuff
    weak_boxes = NULL;
    weak_arrays = NULL;
    did_fnls = 0;
    
    // Now we move all the pages we're collecting to a more centralized
    // location. This helps much later down the line. 
    shunt_off_collecting_pages(mark_gen);
    prep_ownerset_information(mark_gen);

    // Now to the actual collection. First we need to mark off our base
    // pointers, which amounts to the immobile boxes, the roots and 
    // the current stack
    mark_immobile_boxes();
    mark_roots();
    mark_account_hooks();
    mark_owner = current_owner();
    GC_mark_variable_stack(GC_variable_stack, 0, 
			   (void*)(GC_get_thread_stack_base
				   ? GC_get_thread_stack_base() 
				   : (UWORD)stack_base));
    run_older_marks(mark_gen);
    
    // once we're done with this, it's on to propogation. Unfortunately
    // this isn't as easy as one might think because of finalizers. To
    // be honest I (Adam) don't fully understand how this works. Ask
    // Matthew.
    while(1) {
      // first propogate out of the marks we already have
      propogate_all_mpages();

      // this is a copying collector, so at this point the linked lists
      // we normally use to trace through finalizers is trashed. So we 
      // have to fix them before we do anything else
      rebuild_broken_final_list();

      if((did_fnls >= 3) || !fnls) {
	if(did_fnls == 3) {
	  // Finish up ordered finalization
	  Fnl *f, *next, *prev;
	  Fnl_Weak_Link *wl;
	  
	  // Enqueue and mark level 3 finalizers that still haven't been 
	  // marked. 
	  prev = NULL;
	  for(f = fnls; f; f = next) {
	    next = f->next;
	    if(f->eager_level == 3) {
	      if(!is_marked(f->p)) {
		mark_owner = current_owner();
		gcMARK(f->p);

		if(prev) prev->next = next;
		else fnls = next;
		
		f->eager_level = 0; // indicates this has been queued
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
	  
	  // restores zeroed out weak links, marking as we go
	  for(wl = fnl_weaks; wl; wl = wl->next) {
	    void *wp = (void*)wl->p;

	    if(is_marked(wp)) {
	      // for the purposes of this, we account the saved pointer
	      // to the holder of the weak link
	      mark_owner = ((ObjHeader*)((UWORD)wp - 4))->owner;
	      gcMARK(wl->saved);
	    }
	    // this is a little gotcha. if we've moved wp, the change in
	    // pointer hasn't shown here yet and we need to have this set
	    // run in both places (?). FIXME: not sure about this both
	    // business
	    if( ((ObjHeader*)((UWORD)wp - 4))->mark ) {
	      void *newplace = *(void**)wp;
	      *(void**)((char*)newplace + wl->offset) = wl->saved;
	    }
	    *(void**)((char*)wp + wl->offset) = wl->saved;
	  }

	  // we have to mark one more time, because restoring a weak link
	  // may have made something reachable
	  did_fnls++;
	} else break;
      } else {
	int eager_level = did_fnls+1;

	if(eager_level == 3) {
	  // Ordered finalization
	  Fnl *f;
	  Fnl_Weak_Link *wl;

	  // Zero out weak links for ordered finalization
	  for(wl = fnl_weaks; wl; wl = wl->next) {
	    void *wp = (void*)wl->p;
	    wl->saved = *(void**)((char*)wp + wl->offset);
	    *(void**)((char*)wp + wl->offset) = NULL;
	  }

	  // Mark content of not-yet-marked finalized objects, but don't
	  // mark the finalized objects themselves
	  for(f = fnls; f; f = f->next) {
	    if(f->eager_level == 3) {
	      if(!is_marked(f->p)) {
		// not yet marked. mark the content
		mark_owner = current_owner();
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
	  // unordered finalization
	  Fnl *f, *prev, *queue;

	  f = fnls;
	  prev = NULL;
	  queue = NULL;

	  while(f) {
	    if(f->eager_level == eager_level) {
	      if(!is_marked(f->p)) {
		// not yet marked. move finalization to run queue
		Fnl *next = f->next;
		
		if(prev) prev->next = next;
		else fnls = next;
		
		f->eager_level = 0; // indicating queuednessness (=))

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

	  // Mark items added to run queue
	  f = queue;
	  while(f) {
	    // FIXME: this is probably not the thing to do
	    mark_owner = current_owner();
	    gcMARK(f->p);
	    f = f->next;
	  }
	}

	did_fnls++;
      }
    }
    
    // we're done. amazing. so now we have to go back and fix all the things
    // we've broken.
    fixup_ownersets();
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
    
    // notify MzScheme that we're done with collection now
    if(GC_collect_end_callback) 
      GC_collect_end_callback();

    // and finally run the finalization routines. 
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
    
    // we can now run collections again
    gc_deny = 0;
    
    recalculate_heap_size();
    // recalculate how much space we're using
    size_to_collect_at = (UWORD)((heap_size * SIZE_GROW_FACTOR) 
				 + SIZE_GROW_ADDITION);
    run_applicable_account_hooks();
  }
}

//------------------------------------------------------------------------------
// INITIALIZATION: Several structures and system items have to be initialized
// before a collection occurs. This routine is called through current_owner,
// which will trigger this shortly after the base custodian exists.
//------------------------------------------------------------------------------

static void init(void) {
  initialize_roots_subsystem();
  initialize_mpage_subsystem();
  initialize_immobile_subsystem();
  initialize_memory_protection_subsystem();
  initialize_final_subsystem();
  initialize_weak_subsystem();
}

//------------------------------------------------------------------------------
// API CONVERSIONS: These are the routines the outset world see. I've seperated
// them out because most of them simply call into internal routines rather then
// do anything very interesting. 
//------------------------------------------------------------------------------

// these routines get and set the stack for the outside world. They're
// pretty innocous.

void GC_set_stack_base(void *base) {
  stack_base = base;
}
unsigned long GC_get_stack_base(void) {
  return (unsigned long)stack_base;
}

// Called by MzScheme to install roots. The memory between
// `start' (inclusive) and `end' (exclusive) contains pointers.
void GC_add_roots(void *start, void *end) {
  add_root(start, end, current_owner());
}

// Called by MzScheme to indicate the number of different type tags
// it uses, starting from 0. `count' is always less than 256. The weakbox
// argument is the value to be used for tagging weak box. (The GC has
// some freedom in the layout of a weak box, so it performs weak box
// traversals itself, but MzScheme gets to choose the tag.)
void GC_init_type_tags(int count, int weakbox) {
  weak_box_tag = weakbox;
}

// Called by the user to dump memory info to stderr
void GC_dump(void) {
  dump_root_information(stderr);
  dump_mpage_information(stderr);
  dump_ownerset_information(stderr);
}

// Returns the number of currently-allocated bytes
long GC_get_memory_use(void *c) {
  Scheme_Custodian *cust = (Scheme_Custodian*)c; 

  // there's two ways we can be called; with or without a custodian. So
  // check that first:
  if(cust) 
    return get_custodian_mem_usage(cust);
  else 
    return heap_size;
}

// performs an immediate (full) collection
void GC_gcollect(void) {
  garbage_collect(1);
}

// Allocate an array of pointers, initially zeroed
void *GC_malloc(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_ARRAY, 0);
}

// Allocate an item, initially zero. MzScheme will set the tag before
// we garbage collect.
void *GC_malloc_one_tagged(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_TAGGED, 0);
}

// Allocate an item, initially zero. XTagged objects are generally 
// used in the windowing routines to cope with C++ objects. The
// callbacks GC_{mark|fixup}_xtagged handle the marking and fixing
// up of these objects.
void *GC_malloc_one_xtagged(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_XTAGGED, 0);
}

// Allocate an array of tagged items. MzScheme will set the tag on
// the first item, but may not set them for all items.
void *GC_malloc_array_tagged(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_TARRAY, 0);
}

// Allocate an object which contains no pointers
void *GC_malloc_atomic(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_ATOMIC, 0);
}

// Allocates an object which shouldn't be collected and doesn't
// contain any pointers. We just forward this off to malloc, since
// we don't really care about this object.
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes) {
  return malloc(size_in_bytes);
}

// Alloc an array of pointers (typically large) and allow pointers
// to point into the middle of the array or just past the end of
// the array.
void *GC_malloc_allow_interior(size_t size_in_bytes) {
  return alloc_new_bits(size_in_bytes, MPAGE_ARRAY, 1);
}

// Allocate a weak array, initially zeroed. See gc2.h and the README
// for information on weak arrays/
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

// Called by MzScheme to free some space. Ignored.
void GC_free(void *p) {}

// Allocate a weak box. See README for details.
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

// Allocate a box that we shouldn't allow to be moved. These allocated
// in a seperate linked list.
void **GC_malloc_immobile_box(void *p) {
  ImmobileBox *ibox = (ImmobileBox*)malloc(sizeof(ImmobileBox));
  ibox->owner = current_owner();
  ibox->p = p;
  ibox->next = immobile_boxes;
  immobile_boxes = ibox;
  return (void**)ibox;
}

// Free up an immobile box.
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

// add an account hook
void GC_set_account_hook(int type, void *cust, unsigned long b, void *f) {
  AccountHook *work = (AccountHook*)malloc(sizeof(AccountHook));
  work->type = type;
  work->owner = current_owner();
  work->cust = cust;
  work->bytes = b;
  work->f = f;
  work->next = account_hooks;
  account_hooks = work;
  if(type == MZACCT_REQUIRE)
    total_requires += b;
}

// Add a finalizer to the system. See README for details

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

// Add a weak finalizer to the system. See README for details
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

// Add a new traverser to the system.
void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, 
			    Fixup_Proc fixup) {
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
}

// Called by fixup procedures in MzScheme to resolve the values of
// pointers which survive collection.
void *GC_resolve(void *p) {
  ObjHeader *info = (ObjHeader*)((UWORD)p - 4);

  if(info->mark)
    return *(void**)p;
  else
    return p;
}

// Called by MzScheme (rather, the macro gcFIXUP) to fix up a ptr during
// the last stages of collection
void GC_fixup(void *pp) {
  void *p = *(void**)pp;
  MPage *page;

  if(!p)
    return;
  if((long)p & 0x1)
    return;

  page = find_page(p);
  if(page && (page->flags.gen <= INCREMENT_GENERATION(mark_gen)) && 
     !page->flags.bigpage) {
    ObjHeader *info = (ObjHeader*)((UWORD)p - 4);
    
    if(info->mark) {
      void *newloc = *(void**)p;
      MPage *mypage = find_page(pp);
      
      *(void**)pp = newloc;
      if(mypage) {
	MPage *newlocpage = find_page(newloc);
	if(mypage->flags.gen > newlocpage->flags.gen) {
	  mypage->flags.bpointers = 1;
	}
      }
    } 
  }
}

// Called to mark the current stack. See README for more details
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

// Called to fix up the current stack. See README for more details
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

