/*
   A new Precise GC for MzScheme
   Copyright (C) 2001 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for possible improvement points
*/
#define MZ_PRECISE_GC 1
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc2.h"
#include "../src/schpriv.h"

#warning "This collector is incompatible with the GL extension."
#warning "If you haven't already, rerun configure with '--disable-gl'"

/* To do accounting, turn this on: */
/* #define NEWGC_ACCNT */

/* To actually use the space we put in the object header, turn on this.
   You *must* have defined NEWGC_ACCNT for this to work. */
/* #define NEWGC_USE_HEADER */

/* To do precise accounting, turn on this. You *must* have defined both
   NEWGC_ACCNT and NEWGC_USE_HEADER for this to work. */
/* #define NEWGC_PRECISE */

/* To make things very slow and create very large files, turn on this: */
/* #define NEWGC_DEBUG */

#ifdef NEWGC_DEBUG
#include <stdarg.h>
FILE *debug;
inline void GCDBG(char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vfprintf(debug, format, ap);
  va_end(ap);
  fflush(debug);
}
#endif

/* #defines you can change without definately breaking the collector */
#define GEN0_SIZE		(8 * 1024 * 1024)
#define OT_INIT_SIZE		5
#define OT_ADD_SIZE		10

/* #defines you can change which will most likely break the collector */
#define LOG_WORD_SIZE		2
#define LOG_MPAGE_SIZE		14
#define GENERATIONS		3
#define MPAGE_TAGGED		0
#define MPAGE_ATOMIC		1
#define MPAGE_ARRAY		2
#define MPAGE_TARRAY		3
#define MPAGE_XTAGGED		4
#define MPAGE_BIG		5
#define MPAGE_TYPES		6

typedef unsigned char bool;
typedef unsigned short Type_Tag;
#define true			1
#define false			0

inline void *malloc_pages(size_t len, size_t align);
static void free_pages(void *p, size_t len);
static void flush_freed_pages(void);
static void protect_pages(void *p, size_t len, int rw);
inline void *bpage_malloc(size_t sizeb, int type, int tgen);
static void gc_collect(int full);
inline void pmap_add(void *page);
inline void pmap_remove(void *page);
static void rq_add(int pri, short type, void *obj);
inline bool mark_p(void *p);
static void old_init(void);
#ifdef NEWGC_ACCNT
static bool ot_subset_p(short owner1, short owner2);
#endif

static unsigned long gc_numcollects = 0; /* total # of collections done */
static unsigned int gc_cycle = 0; /* used to determine top gen of collection */
static unsigned int gc_topgen = 0; /* top gen of current collection */
#ifdef NEWGC_ACCNT
static unsigned short gc_owner = 0; /* owner we're marking with */
# ifdef NEWGC_PRECISE
static bool gc_markprecise = false; /* flag for GC_mark when remarking */
# endif
#endif
static bool gc_deny = 0; /* don't allow collections to happen */

#if defined(sparc) || defined(__sparc) || defined(__sparc__)
/* Sun's qsort() is broken. */
# include "my_qsort.c"
#else
# define my_qsort qsort
#endif

/*****************************************************************************
 * Owner information structures and primitives
 *****************************************************************************/
#ifdef NEWGC_ACCNT
/* the structure of a list of owners */
struct owner_list {
  short owner;
  struct owner_list *next;
};

/* the structure of a list of custodians */
struct cust_list {
  Scheme_Custodian *cust;
  struct cust_list *next;
};

/* the structure of a list of unions with other owners and their results */
struct union_list {
  short owner;
  short result;
  struct union_list *next;
};

/* the structure of an entry in the owner table */
struct otentry {
  Scheme_Custodian *creator;
  long memuse[GENERATIONS]; /* memory use for each generation */
  struct cust_list *custs;
  struct union_list *unions;
};

static struct otentry **ot_table = NULL;
static unsigned short ot_top = 0;

/* return true iff the given owner is a member of the given list */
static bool ol_member_p(struct owner_list *ol, short owner) {
  while(ol) 
    if(ol->owner == owner) return true;
    else ol = ol->next;
  return false;
}

/* return a new list with owner added to ol */
static struct owner_list *ol_add(struct owner_list *ol, short owner) {
  struct owner_list *retval = malloc(sizeof(struct owner_list));
  retval->owner = owner;
  retval->next = ol;
  return retval;
}

/* compare function for use with qsort; compares custodians according
   to a partial order of their 'generation'. A parent will always be
   after its children. */
static int ol_sort_cmp(const void *a, const void *b) {
  struct owner_list *ol1 = *(struct owner_list **)a;
  struct owner_list *ol2 = *(struct owner_list **)b;

  if(ot_subset_p(ol1->owner, ol2->owner)) 
    return 1;
  else
    return -1;
}

/* Sort a list of owners according to the function above and return the
   list. This function destroys the list given to it. */
static struct owner_list *ol_sort(struct owner_list *ol) {
  if(!ol || !ol->next) return ol;
  else {
    struct owner_list **table;
    struct owner_list *temp;
    short lslen = 0;
    short i;
    
    for(temp = ol; temp; temp = temp->next)
      lslen += 1;
    table = malloc(lslen * sizeof(struct owner_list *));
    for(temp = ol, i = 0; temp; temp = temp->next, i++)
      table[i] = temp;
    my_qsort(table, lslen, sizeof(struct owner_list *), ol_sort_cmp);
    lslen -= 1; /* save a few cycles at the cost of confusion. YAY! */
    for(i = 0; i < lslen; i++) 
      table[i]->next = table[i+1];
    table[lslen]->next = NULL;
    temp = table[0];
    free(table);
    return temp;
  }
}

/* free every item in an owner set list */
static void ol_free(struct owner_list *ol) {
  while(ol) {
    struct owner_list *next = ol->next;
    free(ol);
    ol = next;
  }
}

/* return true iff the given custodian is a member of the given list */
static bool cl_member_p(struct cust_list *cl, Scheme_Custodian *c) {
  struct cust_list *temp;
  for(temp = cl; temp; temp = temp->next)
    if(temp->cust == c) 
      return true;
  return false;
}

/* return a new list with the given custodian added to the given list */
static struct cust_list *cl_add(struct cust_list *cl, Scheme_Custodian *c) {
  struct cust_list *retval = malloc(sizeof(struct cust_list));
  retval->cust = c;
  retval->next = cl;
  return retval;
}

/* free every item in a custodian list */
static void cl_free(struct cust_list *cl) {
  while(cl) {
    struct cust_list *next = cl->next;
    free(cl);
    cl = next;
  }
}

/* free every item in a union list */
static void ul_free(struct union_list *ul) {
  while(ul) {
    struct union_list *next = ul->next;
    free(ul);
    ul = next;
  }
}

/* return true if the custodians which make up owner1 are a subset of the
   custodians which make up owner2 */
static bool ot_subset_p(short owner1, short owner2) {
  struct cust_list *clist;

  for(clist = ot_table[owner1]->custs; clist; clist = clist->next)
    if(!cl_member_p(ot_table[owner2]->custs, clist->cust))
      return 0;
  return 1;
}

/* add an entry to the owner table and return its entry number */
static short ot_addentry(Scheme_Custodian *cust) {
  short i;

  for(i = 0; i < ot_top; i++) 
    if(!ot_table[i]) {
      ot_table[i] = malloc(sizeof(struct otentry));
      ot_table[i]->creator = cust;
      ot_table[i]->custs = NULL;
      ot_table[i]->unions = NULL;
      while(cust) {
	Scheme_Object *box = (Scheme_Object*)cust->parent;
	ot_table[i]->custs = cl_add(ot_table[i]->custs, cust);
	cust = box ? (Scheme_Custodian*)box->u.two_ptr_val.ptr1 : NULL;
      }
      return i;
    }

  ot_top += OT_ADD_SIZE;
  ot_table = realloc(ot_table, ot_top * sizeof(struct otentry *));
  bzero( (void*)((unsigned long)ot_table + ((ot_top - OT_ADD_SIZE)
					    * sizeof(struct otentry*))),
	 OT_ADD_SIZE * sizeof(struct otentry *));
  return ot_addentry(cust);
}

/* Convert the given custodian to an owner set */
static short ot_convert(Scheme_Custodian *cust) {
  short i;

  for(i = 0; i < ot_top; i++)
    if(ot_table[i] && (ot_table[i]->creator == cust))
      return i;
  return ot_addentry(cust);
}

/* return the current owner set */
short ot_current() {
  Scheme_Custodian *c;

  if(!scheme_current_thread) {
    return 0;
  } else if(!(scheme_current_thread->config)) {
    return 0;
  } else {
    c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
    return ot_convert(c);
  }
}

/* return an owner set entry which is the union of the two owner sets */
static short ot_union(short own1, short own2) {
  struct union_list *ulist;
  short res;

  if(own1 > own2) {
    short temp = own2;
    own2 = own1;
    own1 = temp;
  }
/*   if(own1 == own2) { printf("Unioning the same sets?!\n"); abort(); } */
/*   if(!ot_table[own1]) { printf("Unioning bad own1!\n"); abort(); } */
/*   if(!ot_table[own2]) { printf("Unioning bad own2!\n"); abort(); } */
  
  for(ulist = ot_table[own1]->unions; ulist; ulist = ulist->next)
    if(ulist->owner == own2) return ulist->result;

  /* we've never unioned these before */
  if(ot_subset_p(own1, own2)) {
    res = own2;
  } else if(ot_subset_p(own2, own1)) {
    res = own1;
  } else {
    bool done = false;
    while(!done) {
      for(res = 0; res < ot_top; res++)
	if(!ot_table[res]) {
	  struct cust_list *custs;
	  int i;

	  done = true;
	  ot_table[res] = malloc(sizeof(struct otentry));
	  ot_table[res]->creator = NULL;
	  ot_table[res]->custs = NULL;
	  ot_table[res]->unions = NULL;
	  for(i = 0; i < GENERATIONS; i++)
	    ot_table[res]->memuse[i] = 0;
	  for(custs = ot_table[own1]->custs; custs; custs = custs->next)
	    if(!cl_member_p(ot_table[res]->custs, custs->cust))
	      ot_table[res]->custs = cl_add(ot_table[res]->custs, custs->cust);
	  for(custs = ot_table[own2]->custs; custs; custs = custs->next)
	    if(!cl_member_p(ot_table[res]->custs, custs->cust))
	      ot_table[res]->custs = cl_add(ot_table[res]->custs, custs->cust);
	  break;
	}
      if(!done) {
	ot_top += OT_ADD_SIZE;
	ot_table = realloc(ot_table, ot_top * sizeof(struct otentry *));
	bzero( (void*)((unsigned long)ot_table + ((ot_top - OT_ADD_SIZE)
						  * sizeof(struct otentry*))),
	       OT_ADD_SIZE * sizeof(struct otentry *));
      }
    }
  }
  
  ulist = malloc(sizeof(struct union_list));
  ulist->owner = own2;
  ulist->result = res;
  ulist->next = ot_table[own1]->unions;
  ot_table[own1]->unions = ulist;
  return res;
}

/* return the amount of memory a given custodian is using */
static unsigned long ot_memuse(Scheme_Custodian *c) {
  unsigned long retval = 0;
  short i, j;

  for(i = 0; i < ot_top; i++) 
    if(ot_table[i] && cl_member_p(ot_table[i]->custs, c))
      for(j = 0; j < GENERATIONS; j++)
	retval += ot_table[i]->memuse[j];
  return retval;
}

/* Fix up the pointers into the scheme heap from the owner table, and
   remove any dead owner sets */
static void ot_fixup(struct owner_list *root_owners) {
  short i;

  /* only do this cleaning when we're doing a full collection */
  if(gc_topgen == (GENERATIONS - 1)) {
    /* First look for owner sets which are using no memory and are not
       referenced through roots. Those that are so are dead, and can be
       removed */
    for(i = 0; i < ot_top; i++) {
      if(ot_table[i] && !ol_member_p(root_owners, i)) {
	unsigned long mem = 0;
	short gen = 0;

	for(gen = 0; gen < GENERATIONS; gen++) 
	  mem = mem + ot_table[i]->memuse[gen];
	if(mem == 0) {
	  cl_free(ot_table[i]->custs);
	  ul_free(ot_table[i]->unions);
	  free(ot_table[i]);
	  ot_table[i] = NULL;
	}
      } 
    }
    /* Then look through union sets for unions with things that are now
       dead and kill them */
    for(i = 0; i < ot_top; i++) {
      if(ot_table[i]) {
	struct union_list *ul = ot_table[i]->unions, *next;
	ot_table[i]->unions = NULL;
	for(; ul; ul = next) {
	  next = ul->next;
	  if(ot_table[ul->owner] && ot_table[ul->result]) {
	    ul->next = ot_table[i]->unions;
	    ot_table[i]->unions = ul;
	  } else free(ul);
	}
      }
    }
  }

  /* Finally, clean out custodians that are dead and fix those that are
     alive */
  for(i = 0; i < ot_top; i++) 
    if(ot_table[i]) {
      struct cust_list *cl = ot_table[i]->custs, *next;
      
      if(ot_table[i]->creator) {
	if(mark_p(ot_table[i]->creator))
	  gcFIXUP(ot_table[i]->creator);
	else ot_table[i]->creator = NULL;
      }
      ot_table[i]->custs = NULL;
      for(; cl; cl = next) {
	next = cl->next;
	if(mark_p(cl->cust)) {
	  gcFIXUP(cl->cust);
	  cl->next = ot_table[i]->custs;
	  ot_table[i]->custs = cl;
	} else free(cl);
      }
    }
}

/* Prepare the owner set table for a collection which is about to happen */
static void ot_collectprep() {
  unsigned short i, j;

  for(i = 0; i < ot_top; i++)
    if(ot_table[i])
      for(j = 0; j <= gc_topgen; j++)
	ot_table[i]->memuse[j] = 0;
}

/* Account size bytes from generation gen to the owner owner */
static void ot_accnt(unsigned short owner, short gen, long size) {
  ot_table[owner]->memuse[gen] += size; 
}
#endif
/*****************************************************************************
 * Root structures and primitives
 *****************************************************************************/

/* The structure of a root */
struct root {
#ifdef NEWGC_ACCNT
  short owner;
#endif
  long count;
  long size;
  unsigned long *roots;
  bool nothing_new;
  struct root *next;
};

/* Compare two roots; this is just a basic < comparison for qsort */
static int roots_cmp(const void *a, const void *b) {
  return (*(unsigned long *)a < *(unsigned long *)b) ? -1 : 1;
}

/* Sort the given root and merge any overlapping entries */
static void roots_sort_merge(struct root *root) {
  int i, offset, top;

  if(root->nothing_new) return;
  if(root->count < 4) return;
  my_qsort(root->roots, root->count >> 1, 2*sizeof(unsigned long), roots_cmp);
  offset = 0; top = root->count;
  for(i = 2; i < top; i += 2) {
    if((root->roots[i - 2 - offset] <= root->roots[i])
       && ((root->roots[i - 1 - offset] +  3) >= root->roots[i])) {
      /* merge */
      if(root->roots[i+1] > root->roots[i - 1 - offset])
	root->roots[i - 1 - offset] = root->roots[i + 1];
      offset += 2;
      root->count -= 2;
    } else if(root->roots[i] == root->roots[i + 1]) {
      /* remove empty range */
      offset += 2;
      root->count -= 2;
    } else if(offset) {
      /* compact */
      root->roots[i - offset] = root->roots[i];
      root->roots[i + 1 - offset] = root->roots[i + 1];
    }
  }
  root->nothing_new = 1;
}

static struct root *roots = NULL;

/* add a root to the system */
void GC_add_roots(void *start, void *end) {
#ifdef NEWGC_ACCNT
  unsigned short owner = ot_current();
#endif
  struct root *work = roots;

#ifdef NEWGC_ACCNT
  /* see if an entry for this owner already exists or make it */
  while(1) {
    if(!work) {
      work = (struct root *)malloc(sizeof(struct root));
      work->owner = owner;
      work->count = 0;
      work->size = 0;
      work->roots = NULL;
      work->nothing_new = false;
      work->next = roots;
      roots = work;
      break;
    } else if(work->owner == owner) {
      break;
    } else {
      work = work->next;
    }
  }
#else
  if(!work) {
      work = (struct root *)malloc(sizeof(struct root));
      work->count = 0;
      work->size = 0;
      work->roots = NULL;
      work->nothing_new = false;
      work->next = roots;
      roots = work;
  }
#endif
  if(work->count >= work->size) {
    work->size = work->size ? 2 * work->size : 500;
    work->roots = realloc(work->roots, sizeof(unsigned long)*(work->size + 1));
  }
  work->roots[work->count++] = (unsigned long)start;
  work->roots[work->count++] = ((unsigned long)end - 4);
  work->nothing_new = 0;
}

#ifdef NEWGC_ACCNT
/* return a list of the owners referenced from the roots set */
static struct owner_list *roots_get_owners(struct owner_list *ol) {
  struct root *work;
  
  for(work = roots; work; work = work->next) {
    roots_sort_merge(work);
    if(!ol_member_p(ol, work->owner)) {
      ol = ol_add(ol, work->owner);
    }
  }
  return ol;
}
#endif

/* mark the roots. if accountint is being done, only mark the roots 
   for the given owner. otherwise, mark them all regardless of what's
   passed in. */
static void roots_mark(short owner) {
  struct root *work;
/*   if(owner != 1) return; */
  for(work = roots; work; work = work->next) {
#ifdef NEWGC_ACCNT
    if(work->owner == owner) {
#endif
      unsigned long i;
      for(i = 0; i < work->count; i += 2) {
	void **s = (void**)work->roots[i];
	void **e = (void**)work->roots[i + 1];
	while(s < e) gcMARK(*(s++));
      }
#ifdef NEWGC_ACCNT
    }
#endif
  }
}

/* fix up the pointers in the roots set */
static void roots_fixup(void) {
  struct root *work;

  for(work = roots; work; work = work->next) {
    unsigned long i;
    for(i = 0; i < work->count; i += 2) {
      void **s = (void**)work->roots[i];
      void **e = (void**)work->roots[i + 1];
      while(s < e) gcFIXUP(*(s++));
    }
  }
}

/*****************************************************************************
 * Memory allocation (midlevel)
 *****************************************************************************/

/* FIXME: this still needs work */
#if defined(__FreeBSD__)
#include <machine/vmparam.h>
unsigned long max_heap_size = MAXDSIZ;
#else
#warning "Couldn't figure out how to get max heap size for your platform"
#warning "Assuming that this is ~1GB"
unsigned long max_heap_size = (1024 * 1024 * 1024);
#endif

#define MPAGE_SIZE		(1 << LOG_MPAGE_SIZE)

/* the number of pages in the heap */
unsigned long pages_in_heap;
/* the maximum number of pages we should use */
unsigned long max_used_pages;
/* the number of pages currently in active use */
static unsigned long mla_usedpages = 0;

/* allocate a memory page of the given size */
inline void *mla_malloc(size_t size) {
  unsigned long numpages = (size/MPAGE_SIZE)+(((size%MPAGE_SIZE)==0) ? 0 : 1);

  mla_usedpages += numpages;
  if(mla_usedpages > max_used_pages) {
    gc_collect(0);
    if(mla_usedpages > max_used_pages) {
      gc_collect(1);
      if(mla_usedpages > max_used_pages) {
	if(GC_out_of_memory) GC_out_of_memory();
	fprintf(stderr, "Out of memory\n");
	abort();
      }
    }
  }
  return malloc_pages(size, MPAGE_SIZE);
}

/* free a previously allocated page. this may cause and actual deallocation,
   or it may simply cache the page for later reuse */
inline void mla_free(void *page, size_t size, int bigpage) {
  unsigned long numpages = (size/MPAGE_SIZE)+(((size%MPAGE_SIZE)==0)?0:1);
  mla_usedpages -= numpages;
  free_pages(page, size);
}

#ifdef NEWGC_ACCNT
/* return the amount of memory that is not currently in use */
inline unsigned long mla_memfree(void) {
  return (max_used_pages - mla_usedpages) * MPAGE_SIZE;
}
#endif

/*****************************************************************************
 * Memory allocation (toplevel)
 *****************************************************************************/
#define BITS_PER_WORD			((1 << LOG_WORD_SIZE) * 8)
#define OBJ_LOG_MAX_SIZE		(LOG_MPAGE_SIZE - 2)
#define OBJ_MAX_SIZE			(1 << OBJ_LOG_MAX_SIZE)
#define OBJ_MAX_WORD_SIZE		gcBYTES_TO_WORDS(OBJ_MAX_SIZE)

static void **tla_heap = NULL;
static void **tla_regions = NULL;
static void **tla_regione = NULL;

/* this is the structure of the header at the front of every object */
struct objhead {
  unsigned int owner : (BITS_PER_WORD - (5 + OBJ_LOG_MAX_SIZE));
  unsigned int type : 3;
  unsigned int mark : 1;
  unsigned int markf : 1;
  unsigned int size : OBJ_LOG_MAX_SIZE;
};

char zero_sized[4]; /* what's returned for zero sized allocation requests */

/* the allocator */
#define allocate(t) { \
  if(size_in_bytes) { \
    size_t size_in_words = gcBYTES_TO_WORDS(size_in_bytes) + 1; \
    if(size_in_words < OBJ_MAX_WORD_SIZE) { \
      void *retval = tla_regions; \
      tla_regions += size_in_words; \
      if(tla_regions >= tla_regione) { \
        tla_regions -= size_in_words; \
        gc_collect(0); \
        retval = tla_regions; \
        tla_regions += size_in_words; \
      } \
      ((struct objhead *)retval)->type = t; \
      ((struct objhead *)retval)->size = size_in_words; \
      return (void*)((unsigned long)retval + 4); \
    } else return bpage_malloc(size_in_bytes, t, 0); \
  } else return zero_sized; \
}
  
/* public instances of the allocator */
void *GC_malloc(size_t size_in_bytes) { allocate(MPAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t size_in_bytes) { allocate(MPAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t size_in_bytes) { allocate(MPAGE_XTAGGED); }
void *GC_malloc_array_tagged(size_t size_in_bytes) { allocate(MPAGE_TARRAY); }
void *GC_malloc_atomic(size_t size_in_bytes) { allocate(MPAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t size) { return malloc(size); }

/* return true if the pointer is a member of the top level heap */
#define tla_member_p(p) (((unsigned long)p >= (unsigned long)tla_heap) && ((unsigned long)p < (unsigned long)tla_regions))

/*****************************************************************************
 * Memory allocation (gclevel)
 *****************************************************************************/

/* the structure of a memory page */
struct mpage {
  unsigned int size;
  unsigned int psize;
  unsigned char gen;
  unsigned char bpointers;
  unsigned char bigpage;
  unsigned char type;
  struct mpage *next;
};

#define PAGE_HEADER_SIZEW	gcBYTES_TO_WORDS(sizeof(struct mpage))
#define PAGE_HEADER_SIZE	gcWORDS_TO_BYTES(PAGE_HEADER_SIZEW)
#define INCGEN(x)		(((x + 1) == GENERATIONS) ? x : (x + 1))

static struct mpage *gen[GENERATIONS][MPAGE_TYPES] = {{NULL, NULL, NULL,
						       NULL, NULL, NULL},
						      {NULL, NULL, NULL,
						       NULL, NULL, NULL},
						      {NULL, NULL, NULL,
						       NULL, NULL, NULL}};

/* create a pointer into a page of the given type and generation (and possibly
   owner), allocate sizew words for it and copy the object located in oldptr
   to that location. return the new pointer. */
static void *copy_bits(void *oldptr, unsigned long sizew, int type, int tgen) {
  unsigned long sizeb = gcWORDS_TO_BYTES(sizew);
  struct mpage *work = gen[tgen][type];
  void *retval;

  /* try to find a previously extant place to put this */
  while(work && ((work->size + sizeb) >= MPAGE_SIZE))  
    work = work->next;  
  if(work) {
    void **startfrom = oldptr, **startto, **endfrom; 
    startto =  retval = (void*)((unsigned long)work + work->size);
    endfrom = startfrom + sizew; 
    while(startfrom < endfrom) *(startto++) = *(startfrom++); 
    work->size += sizeb;
    return retval;
  } else {
    void **startfrom = oldptr, **startto, **endfrom; 
    work = (struct mpage *)mla_malloc(MPAGE_SIZE);
    work->next = gen[tgen][type];
    work->gen = tgen;
    work->type = type;
    work->size = PAGE_HEADER_SIZE + sizeb;
    work->psize = PAGE_HEADER_SIZE;
    pmap_add(work);
    gen[tgen][type] = work;
    startto = retval = (void*)((unsigned long)work + PAGE_HEADER_SIZE);
    endfrom = startfrom + sizew; 
    while(startfrom < endfrom) *(startto++) = *(startfrom++); 
    return retval;
  }
}

/*****************************************************************************
 * Page mapping structures and routines
 *****************************************************************************/
#define DEAD_BITS	LOG_MPAGE_SIZE
#define USEFUL_BITS	(BITS_PER_WORD - DEAD_BITS)
#define FLINDEX_SIZE	(USEFUL_BITS >> 1)
#define FLINDEX_SHIFT	(BITS_PER_WORD - FLINDEX_SIZE)
#define SLINDEX_SIZE	(USEFUL_BITS - FLINDEX_SIZE)
#define SLINDEX_SHIFT	DEAD_BITS
#define SLINDEX_MASK	((1 << SLINDEX_SIZE) - 1)
#define FLINDEX(x)	((unsigned long)x >> FLINDEX_SHIFT)
#define SLINDEX(x)	(((unsigned long)x >> SLINDEX_SHIFT) & SLINDEX_MASK)

static struct mpage *pmap[1 << FLINDEX_SIZE][1 << SLINDEX_SIZE];

/* add a page to our page map */
inline void pmap_add(void *page) {
  struct mpage *mpage = (struct mpage *)page;
  long size_left = mpage->bigpage ? mpage->size : MPAGE_SIZE;

  while(size_left > 0) {
    pmap[FLINDEX(page)][SLINDEX(page)] = mpage;
    size_left -= MPAGE_SIZE;
    page += MPAGE_SIZE;
  }
}

/* remove a page from our page map */
inline void pmap_remove(void *page) {
  struct mpage *mpage = (struct mpage *)page;
  long size_left = mpage->bigpage ? mpage->size : MPAGE_SIZE;
  
  while(size_left > 0) {
    pmap[FLINDEX(page)][SLINDEX(page)] = NULL;
    size_left -= MPAGE_SIZE;
    page += MPAGE_SIZE;
  }
}

/* return the page on which the given pointer is stored */
#define pmap_find(p)		(pmap[FLINDEX(p)][SLINDEX(p)])

/*****************************************************************************
 * Big page routines 
 *****************************************************************************/

static unsigned long bpage_g0size = 0; 

/* allocate an object onto a bigpage of the given size, type and gen */
inline void *bpage_malloc(size_t sizeb, int type, int tgen) {
  unsigned long sizew = gcBYTES_TO_WORDS(sizeb) + 1;
  struct mpage *bpage;

  /* without this there is a slight flaw in which if someone only
     allocates bigpages, they can completely avoid collection. bad! */
  if( (bpage_g0size + ((unsigned long)tla_regions - (unsigned long)tla_heap))
      > GEN0_SIZE) {
    gc_collect(0);
    bpage_g0size = 0;
  }
    
  sizeb = gcWORDS_TO_BYTES(sizew);
  bpage_g0size += sizeb;
  bpage = mla_malloc(PAGE_HEADER_SIZE + sizeb);
  bpage->size = PAGE_HEADER_SIZE + sizeb;
  bpage->psize = PAGE_HEADER_SIZE;
  bpage->gen = tgen;
  bpage->bigpage = 1;
  bpage->type = type;
  pmap_add(bpage);
  bpage->next = gen[tgen][MPAGE_BIG];
  gen[tgen][MPAGE_BIG] = bpage;
  return (void*)((unsigned long)bpage + PAGE_HEADER_SIZE + 4);
}

/* a public interface to the above */
void *GC_malloc_allow_interior(size_t size) {
  return bpage_malloc(size, MPAGE_ARRAY, 0);
}

/*****************************************************************************
 * Memory accounting 
 *****************************************************************************/
#ifdef NEWGC_ACCNT
#define RQ_TYPE_ACCNT		2

/* the stucture of an account hook */
struct accnthook {
  short type;
  short owner;
  unsigned long bytes;
  Scheme_Custodian *cust;
  Scheme_Custodian *cust_to_kill;
  struct accnthook *next;
};

/* the actual account hooks */
static struct accnthook *accnthooks = NULL;
static unsigned long accnt_requires = 0;

/* return the current memory use. if c is null, return the total memory
   use of the system. if c is not null, then c is a custodian and return
   the memory use for that custodian */
long GC_get_memory_use(void *c) {
  if(!c) {
    unsigned long retval = 
      ((unsigned long)tla_regions - (unsigned long)tla_heap);
    struct mpage *page;
    int g, type;
    
    for(g = 0; g < GENERATIONS; g++) 
      for(type = 0; type < MPAGE_TYPES; type++)
	for(page = gen[g][type]; page; page = page->next)
	  retval += page->size;
    return retval;
  } else {
    unsigned long retval = 0;
    unsigned short i, j;

    for(i = 0; i < ot_top; i++) 
      if(ot_table[i] && cl_member_p(ot_table[i]->custs, c))
	for(j = 0; j < GENERATIONS; j++) 
	  retval += ot_table[i]->memuse[j];
    return retval;
  }
}

/* public interface to set up an account hook. always returns one for 
   this collector (one meaning that the function is supported and a hook
   was added) */
int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2) {
  struct accnthook *work = (struct accnthook*)malloc(sizeof(struct accnthook));
  work->type = type;
  work->owner = ot_current();
  work->bytes = b;
  work->cust = c1;
  work->cust_to_kill = c2;
  work->next = accnthooks;
  accnthooks = work;
  if(type == MZACCT_REQUIRE) accnt_requires += b;
  return 1;
}

/* get the owners referenced by the account hook */
static struct owner_list *accnt_get_owners(struct owner_list *ol) {
  struct accnthook *work;

  for(work = accnthooks; work; work = work->next)
    if(!ol_member_p(ol, work->owner))
      ol = ol_add(ol, work->owner);
  return ol;
}

/* mark the pointers into the scheme heap held by the account hooks. If
   accounting is enabled, only mark those that are owned by the given 
   owner. Otherwise mark them all. */
static void accnt_mark(short owner) {
  struct accnthook *work;
  
/*   if(owner != 1) return; */
  for(work = accnthooks; work; work = work->next)
    if(work->owner == owner) 
      gcMARK(work->cust_to_kill);
}

/* fix up the pointers into the scheme heap held by the account hooks. If
   any of them have been triggered, add them to the queue of things to be
   run. If any of them are no longer able to be triggered, just remove 
   them */
static void accnt_fixup(void) {
  struct accnthook *cur = accnthooks, *prev = NULL;

  while(cur) {
    if((!mark_p(cur->cust) && (cur->type == MZACCT_LIMIT))
       || !ot_table[cur->owner]) {
      if(cur->type == MZACCT_REQUIRE) accnt_requires -= cur->bytes;
      if(prev) { prev->next = cur->next; free(cur); cur = prev->next; }
      else { accnthooks = cur->next; free(cur); cur = accnthooks; }
    } else {
      gcFIXUP(cur->cust); gcFIXUP(cur->cust_to_kill);
      if(gc_topgen == (GENERATIONS - 1)) {
	if((cur->type == MZACCT_REQUIRE) && (mla_memfree() < accnt_requires)) {
	  if(prev) prev->next = cur->next;
	  else accnthooks = cur->next;
	  rq_add(0, RQ_TYPE_ACCNT, cur);
	  cur = cur->next;
	} else if((cur->type == MZACCT_LIMIT) && 
		  (ot_memuse(cur->cust) >= cur->bytes)) {
	  if(prev) prev->next = cur->next;
	  else accnthooks = cur->next;
	  rq_add(0, RQ_TYPE_ACCNT, cur);
	  cur = cur->next;
	} else {
	  prev = cur; 
	  cur = cur->next;
	}
      } else { prev = cur; cur = cur->next; }
    }
  }
}
#else
long GC_get_memory_use(void *c) {
  unsigned long retval = GEN0_SIZE;
  struct mpage *page;
  int g, type;
  
  for(g = 0; g < GENERATIONS; g++) 
    for(type = 0; type < MPAGE_TYPES; type++)
      for(page = gen[g][type]; page; page = page->next)
	retval += page->size;
  return retval;
}

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2) {
  return 0;
}
#endif

/*****************************************************************************
 * Weak box structure and functions
 *****************************************************************************/
/* the structure of a weak box */
struct weakbox {
  Type_Tag tag;
  short keyex;
  void *val;
  void **secondary_erase;
  int soffset;
  struct weakbox *next;
}; 

Type_Tag weak_box_tag = 42;
static struct weakbox *weak_boxes = NULL;

/* return the size of a weak box */
int size_weak_box(void *p) { 
  return gcBYTES_TO_WORDS(sizeof(struct weakbox)); 
}

/* mark a weak box */
int mark_weak_box(void *p) {
  struct weakbox *wb = (struct weakbox *)p;
  gcMARK(wb->secondary_erase);
  if(wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }
  return gcBYTES_TO_WORDS(sizeof(struct weakbox));
}

/* fixup a weak box; this also does the appropriate zeroing if its target
   is no longer marked */
int fixup_weak_box(void *p) {
  struct weakbox *wb = (struct weakbox *)p;
  gcFIXUP(wb->secondary_erase);
  if(!mark_p(wb->val)) {
    wb->val = NULL;
    if(wb->secondary_erase)
      *(wb->secondary_erase + wb->soffset) = NULL;
    wb->secondary_erase = NULL;
  } else gcFIXUP(wb->val);
  return gcBYTES_TO_WORDS(sizeof(struct weakbox));
}

/* public interface to allocate a weak box */
void *GC_malloc_weak_box(void *p, void **secondary, int soffset) {
  struct weakbox *w = 
    (struct weakbox *)GC_malloc_one_tagged(sizeof(struct weakbox));
  w->tag = weak_box_tag;
  w->val = p;
  w->secondary_erase = secondary;
  w->soffset = soffset;
  return w;
}

/*****************************************************************************
 * Weak array structure and functions
 *****************************************************************************/

#define gc_weak_array_tag		256

/* the structure of a weak array */
struct weakarray {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct weakarray *next;
  void *data[1];
};

static struct weakarray *weak_arrays = NULL;

/* return the size of a given weak array */
int size_weak_array(void *p) {
  struct weakarray *wa = (struct weakarray *)p;
  return gcBYTES_TO_WORDS(sizeof(struct weakarray) +
			  ((wa->count - 1) * sizeof(void *)));
}

/* mark a given weak array */
int mark_weak_array(void *p) {
  struct weakarray *wa = (struct weakarray *)p;
  gcMARK(wa->replace_val);
  wa->next = weak_arrays;
  weak_arrays = wa;
  return gcBYTES_TO_WORDS(sizeof(struct weakarray) +
			  ((wa->count - 1) * sizeof(void *)));
}

/* fixup a weak array. also does any appropriate zeroing if some pointers
   have died */
int fixup_weak_array(void *p) {
  struct weakarray *wa = (struct weakarray *)p;
  int i;
  void **data;
  gcFIXUP(wa->replace_val);
  data = wa->data;
  for(i = wa->count; i--; ) {
    if(data[i] && !mark_p(data[i])) {
      data[i] = wa->replace_val; 
    } else if(data[i]) gcFIXUP(data[i]);
  }
  return gcBYTES_TO_WORDS(sizeof(struct weakarray) +
			  ((wa->count - 1) * sizeof(void *)));
}

/* public interface to allocated a weak array */
void *GC_malloc_weak_array(size_t sizeb, void *replace_val) {
  struct weakarray *wa =
    (struct weakarray *)GC_malloc_one_tagged(sizeb + sizeof(struct weakarray)
					     - sizeof (void*));
  wa->type = gc_weak_array_tag;
  wa->replace_val = replace_val;
  wa->count = (sizeb >> LOG_WORD_SIZE);
  return wa;
}

/*****************************************************************************
 * Immobile box structures and routines
 *****************************************************************************/
/* the structure of an immobile box */
struct immobile {
  void *p; /* this must be the first item in the structure */
#ifdef NEWGC_ACCNT
  short owner;
#endif
  struct immobile *next;
};

static struct immobile *immobiles = NULL;

#ifdef NEWGC_ACCNT
/* return the list of owners holding immobile boxes */
static struct owner_list *imm_get_owners(struct owner_list *ol) {
  struct immobile *work;

  for(work = immobiles; work; work = work->next) 
    if(!ol_member_p(ol, work->owner))
      ol = ol_add(ol, work->owner);
  return ol;
}
#endif

/* mark the pointers in the immobile boxes. if accounting is on, only mark
   the boxes owned by the gived owner. otherwise, mark them all. */
static void imm_mark(short owner) {
  struct immobile *work;
/*   if(owner != 1) return; */
  for(work = immobiles; work; work = work->next)
#ifdef NEWGC_ACCNT
    if(work->owner == owner) 
#endif
      gcMARK(work->p);
}

/* fixup the pointers in the immobile boxes */
static void imm_fixup(void) {
  struct immobile *work;
  for(work = immobiles; work; work = work->next)
    gcFIXUP(work->p);
}

/* public interface to allocate an immobile box */
void **GC_malloc_immobile_box(void *p) {
  struct immobile *ib = malloc(sizeof(struct immobile));
  ib->p = p;
#ifdef NEWGC_ACCNT
  ib->owner = ot_current();
#endif
  ib->next = immobiles;
  immobiles = ib;
  return (void**)ib;
}

/* free an allocated immobile box */
void GC_free_immobile_box(void **b) {
  struct immobile *cur, *prev;

  for(prev = NULL, cur = immobiles; cur; prev = cur, cur = cur->next) {
    if(cur == (struct immobile *)b) {
      if(prev) prev->next = cur->next;
      else immobiles = cur->next;
      return;
    }
  }
}

/*****************************************************************************
 * Stack stuff
 *****************************************************************************/

void **GC_variable_stack;
void *stack_base;

/* set the base of the stack */
void GC_set_stack_base(void *base) {
  stack_base = base;
}

/* get the base of the stack */
unsigned long GC_get_stack_base(void) {
  return (unsigned long)stack_base;
}

/* mark the pointers in a stack */
void GC_mark_variable_stack(void **var_stack,
			    long delta,
			    void *limit)
{
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

/* fixup the pointers in a stack */
void GC_fixup_variable_stack(void **var_stack,
			     long delta,
			     void *limit)
{
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

/*****************************************************************************
 * Finalization
 *****************************************************************************/
#define RQ_TYPE_FINAL		1		

/* the structure of a finalizer */
struct fnl {
  char eager_level;
  char tagged;
  void *p;
  void (*f)(void *p, void *data);
  void *data;
#ifdef NEWGC_ACCNT
  short owner;
#endif
  struct fnl *next;
};

/* the structure of a weak finalizer */
struct weakfnl {
  void *p;
  int offset;
  void *saved;
  struct weakfnl *next;
};

static struct fnl *fnls = NULL;
static struct weakfnl *weakfnls = NULL;

/* public interface to create/modify a finalizer for a given pointer */
void GC_set_finalizer(void *p, int tagged, int level,
		      void (*f)(void *p, void *data),
		      void *data, void (**oldf)(void *p, void *data),
		      void **olddata)
{
  struct mpage *page = pmap_find(p);
  struct fnl *fnl, *prev;

  if(!page && !tla_member_p(p)) {
    /* this item is never collected */
    if(oldf) *oldf = NULL;
    if(olddata) *olddata = NULL;
    return;
  }

  fnl = fnls; prev = NULL;
  while(fnl) {
    if(fnl->p == p) {
      if(oldf) *oldf = fnl->f;
      if(olddata) *olddata = fnl->data;
#ifdef NEWGC_ACCNT
      fnl->owner = ot_current();
#endif
      if(f) {
	fnl->f = f;
	fnl->data = data;
	fnl->eager_level = level;
      } else {
	if(prev) prev->next = fnl->next;
	else fnls = fnl->next;
	return;
      }
      return;
    } else { prev = fnl; fnl = fnl->next; }
  }

  /* this is new */
  if(oldf) *oldf = NULL;
  if(olddata) *olddata = NULL;
  if(!f) return;
  fnl = GC_malloc_atomic(sizeof(struct fnl));
  fnl->next = fnls;
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;
  fnl->tagged = tagged;
#ifdef NEWGC_ACCNT
  fnl->owner = ot_current();
#endif
  fnls = fnl;
}

/* public interface to create a weak finalizer */
void GC_finalization_weak_ptr(void **p, int offset) {
  struct weakfnl *wl;
  
  wl = GC_malloc_atomic(sizeof(struct weakfnl));
  wl->p = p;
  wl->next = weakfnls;
  wl->offset = offset * sizeof(void*);
  weakfnls = wl;
}

#ifdef NEWGC_ACCNT
/* get the owners which are holding finalizers */
static struct owner_list *fnl_get_owners(struct owner_list *ol) {
  struct fnl *fnl;

  for(fnl = fnls; fnl; fnl = fnl->next) 
    if(!ol_member_p(ol, fnl->owner)) 
      ol = ol_add(ol, fnl->owner);
  return ol;
}

/* get the owners which are holding weak finalizers */
static struct owner_list *wfnl_get_owners(struct owner_list *ol) {
  return ol;
}
#endif

/* mark the finalizers. if accounting is on, only mark those owned by
   the given owner. otherwise mark them all */
static void fnl_mark(short owner) {
  struct fnl *fnl;

/*   if(owner != 1) return; */
  for(fnl = fnls; fnl; fnl = fnl->next) {
#ifdef NEWGC_ACCNT
    if(fnl->owner == owner) {
#endif
      gcMARK(fnl);
      gcMARK(fnl->data);
      if(!mark_p(fnl->p)) {
	gcMARK(fnl->p);
	if( ((struct objhead *)((unsigned long)fnl->p - 4))->mark ) {
	  ((struct objhead *)((unsigned long)fnl->p - 4))->markf = 1;
	  ((struct objhead *)((unsigned long)fnl->p - 4))->mark = 0;
	}
      }
#ifdef NEWGC_ACCNT
    } /* else printf("Not marking fnl %p because %i != %i\n", */
/* 		  fnl, fnl->owner, owner); */
#endif
  }
}

/* mark the weak finalizers */
static void wfnl_mark(short owner) {
  struct weakfnl *wfnl; 
  int i = 0;
  for(wfnl = weakfnls; wfnl; wfnl = wfnl->next) {
    gcMARK(wfnl); i++;
  }
}

/* fixup the finalizers. if its pointer died, add the finalizer to the run
   queue */
static void fnl_fixup(void) {
  struct fnl *fnl, *prev;

  gcFIXUP(fnls);
  fnl = fnls; prev = NULL;
  while(fnl) {
    struct objhead *phead = (struct objhead *)((unsigned long)fnl->p - 4);
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->p);
    gcFIXUP(fnl->next);
    if(phead->markf && !phead->mark) {
      rq_add(fnl->eager_level, RQ_TYPE_FINAL, fnl);
      if(prev) prev->next = fnl->next;
      else fnls = fnl->next;
      fnl = fnl->next;
    } else { prev = fnl; fnl = fnl->next; }
  }
}

/* fixup the weak finalizers */
static void wfnl_fixup(void) {
  struct weakfnl *cur, *prev;
  
  gcFIXUP(weakfnls);
  cur = weakfnls; prev = NULL;
  while(cur) {
    gcFIXUP(cur->next);
    if(!mark_p(cur->p)) {
      cur->p = NULL;
      if(prev) { prev->next = cur->next; cur = prev->next; }
      else { weakfnls = cur->next; cur = weakfnls; }
      /* FIXME: free cur here? */
    } else {
      gcFIXUP(cur->p);
      prev = cur;
      cur = cur->next;
    }
  }
}

/* weak finals are evil, bad and wrong. they are the scourge of this
   particular collector writer. I hate them. */
static void wfnl_clear_weaks() {
  struct weakfnl *cur;

  for(cur = weakfnls; cur; cur = cur->next) {
    cur->saved = *(void**)((unsigned long)cur->p + cur->offset);
    if(mark_p(cur->p)) 
      *(void**)((unsigned long)GC_resolve(cur->p) + cur->offset) = NULL;
    else
      *(void**)((unsigned long)cur->p + cur->offset) = NULL;
  }
}

/* reset the appropriate pointers after collection is done. see above */
static void wfnl_reset_weaks() {
  struct weakfnl *cur;

  for(cur = weakfnls; cur; cur = cur->next) {
    cur = *(void**)cur;
    gcMARK(cur->saved);
    if(mark_p(cur->p))
      *(void**)((unsigned long)GC_resolve(cur->p) + cur->offset) = cur->saved;
    else
      *(void**)((unsigned long)cur->p + cur->offset) = cur->saved;
  }
}

/*****************************************************************************
 * Run queue structures and functions
 *****************************************************************************/
struct rqentry {
  short type;
  void *obj;
  struct rqentry *next;
};

static struct rqentry *run_queue[4];

static void rq_mark() {
  struct rqentry *ent;
  int i;
  
  for(i = 0; i < 4; i++)
    for(ent = run_queue[i]; ent; ent = ent->next)
      if(ent->type == RQ_TYPE_FINAL) {
	struct fnl *fnl = (struct fnl *)ent->obj;
	gcMARK(fnl);
	gcMARK(fnl->f);
	gcMARK(fnl->p);
	gcMARK(fnl->data);
      } else {
#ifdef NEWGC_ACCNT
	struct accnthook *hook = (struct accnthook *)ent->obj;
	gcMARK(hook->cust);
	gcMARK(hook->cust_to_kill);
#endif
      }
}

static void rq_fixup() {
  struct rqentry *ent;
  int i;
  
  for(i = 0; i < 4; i++) {
    for(ent = run_queue[i]; ent; ent = ent->next) {
      gcFIXUP(ent->obj);
      if(ent->type == RQ_TYPE_FINAL) {
	struct fnl *fnl = (struct fnl *)ent->obj;
	gcFIXUP(fnl->f);
	gcFIXUP(fnl->p);
	gcFIXUP(fnl->data);
      } else {
#ifdef NEWGC_ACCNT
	struct accnthook *hook = (struct accnthook *)ent->obj;
	gcFIXUP(hook->cust);
	gcFIXUP(hook->cust_to_kill);
#endif
      }
    }
  }
}

/* add an item to the run queue */
static void rq_add(int pri, short type, void *obj) {
  struct rqentry *newrqe = malloc(sizeof(struct rqentry));
  struct rqentry *temp = run_queue[pri]; 

  newrqe->type = type;
  newrqe->obj = obj;
  newrqe->next = NULL;
  while(temp && temp->next) temp = temp->next;
  if(temp) temp->next = newrqe;
  else run_queue[pri] = newrqe;
}

/* run any queued items */
static void rq_run() {
  struct rqentry *work;
  int i;

  for(i = 0; i < 4; i++) 
    for(work = run_queue[i]; work; work = run_queue[i]) {
      void **gcs = GC_variable_stack;
      run_queue[i] = work->next;
      if(work->type == RQ_TYPE_FINAL) {
	struct fnl *f = (struct fnl *)work->obj;
 	if(i != 3) f->f(f->p, f->data);
/* 	f->f(f->p, f->data); */
	free(work);
      } else {
#ifdef NEWGC_ACCNT
	struct accnthook *ah = (struct accnthook *)work->obj;
/* 	printf("Scheduling custodian for death\n"); */
	scheme_schedule_custodian_close(ah->cust_to_kill);
	free(ah);
	free(work);
#endif
      }
      GC_variable_stack = gcs;
    }
}


/*****************************************************************************
 * Initialization
 *****************************************************************************/
#define _num_tags			257

static Size_Proc size_table[_num_tags];
static Mark_Proc mark_table[_num_tags];
static Fixup_Proc fixup_table[_num_tags];
static Mark_Proc thread_marker = NULL;
static bool atomic_table[_num_tags];


void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, 
			    Fixup_Proc fixup, int is_constant_size, 
			    int is_atomic) {
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
  atomic_table[tag] = is_atomic;
#ifdef NEWGC_ACCNT
  thread_marker = mark;
  mark_table[tag] = (Mark_Proc)size;
  atomic_table[tag] = 1;
#endif
}

void GC_init_type_tags(int count, int weakbox) {
  static int initialized = 0;

  if(!initialized) {
    pages_in_heap = max_heap_size / MPAGE_SIZE;
    max_used_pages = pages_in_heap / 2;
#ifdef NEWGC_ACCNT
    ot_table = malloc(OT_INIT_SIZE * sizeof(struct otentry *));
#endif
    tla_heap = tla_regions = malloc(GEN0_SIZE);
    tla_regione = (void**)((unsigned long)tla_regions + GEN0_SIZE);
    GC_register_traversers(weakbox, size_weak_box, mark_weak_box,
			   fixup_weak_box, 0, 0);
    GC_register_traversers(gc_weak_array_tag, size_weak_array, mark_weak_array,
			   fixup_weak_array, 0, 0);
    old_init();
#ifdef NEWGC_DEBUG
    debug = fopen("log", "w");
#endif
    initialized = 1;
  }
  weak_box_tag = weakbox;
}

/*****************************************************************************
 * Thread list routines
 *****************************************************************************/
#ifdef NEWGC_ACCNT
struct thread_list {
  void *thread;
  short owner;
  void *next;
};

static struct thread_list *thread_list = NULL;

void GC_register_thread(void *t) {
  struct thread_list *tlist = malloc(sizeof(struct thread_list));
  tlist->thread = t;
  tlist->owner = ot_current();
  tlist->next = thread_list;
  thread_list = tlist;
}

/* return a list of the owners referenced from the roots set */
static struct owner_list *threads_get_owners(struct owner_list *ol) {
  struct thread_list *tlist;
  for(tlist = thread_list; tlist; tlist = tlist->next) 
    if(!ol_member_p(ol, tlist->owner))
      ol = ol_add(ol, tlist->owner);
  return ol;
}
    
static void threads_mark(unsigned short owner) {
  struct thread_list *tlist;
  for(tlist = thread_list; tlist; tlist = tlist->next) 
    if(tlist->owner == owner) 
      thread_marker(tlist->thread);
}

static void threads_fixup() {
  struct thread_list *tlist = thread_list, *next;

  thread_list = NULL;
  while(tlist) {
    next = tlist->next;
    if(!mark_p(tlist->thread)) {
      free(tlist);
    } else {
      gcFIXUP(tlist->thread);
      fixup_table[scheme_thread_type](tlist->thread);
      tlist->next = thread_list;
      thread_list = tlist;
    }
    tlist = next;
  }
}
#else
void GC_register_thread(void *t) {}
#endif

/*****************************************************************************
 * Marking routines
 *****************************************************************************/
inline bool mark_p(void *p) {
  if(!p) return false;
  if(tla_member_p(p)) {
    return ((struct objhead *)((unsigned long)p - 4))->mark 
      ||((struct objhead *)((unsigned long)p - 4))->markf;
  } else {
    struct mpage *page = pmap_find(p);
    if(!page) return false;
    if(page->gen > gc_topgen) return true;
    return ((struct objhead *)((unsigned long)p - 4))->mark 
      ||((struct objhead *)((unsigned long)p - 4))->markf;
  }
}

static void mark_propbig(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE + 4);
  void **end = (void**)((unsigned long)page + page->size);

#ifdef NEWGC_ACCNT  
  gc_owner = ((struct objhead *)((unsigned long)page + PAGE_HEADER_SIZE))->owner;
#endif
  switch(page->type) {
    case MPAGE_TAGGED: mark_table[*(Type_Tag*)start](start); break;
    case MPAGE_ATOMIC: break;
    case MPAGE_ARRAY: while(start < end) GC_mark(*(start++)); break;
    case MPAGE_TARRAY: {
      Type_Tag tag = *(Type_Tag*)start;
      while(start < end) start += mark_table[tag](start);
      break;
    }
    case MPAGE_XTAGGED: GC_mark_xtagged(start); break;
  }
}

#ifdef NEWGC_PRECISE
static void precise_reprop(void *p, struct mpage *page, short owner) {
  if(page->bigpage) {
    mark_propbig(page);
  } else {
    page->psize = PAGE_HEADER_SIZE;
  }
}

static void precise_mark(const void *p) {
  /* mark_p() isn't acceptable here because it returns true for objects
     which are in a generation higher than we're dealing with, and we
     don't want to chase pointers in high generations */
  if(!tla_member_p(p) && !pmap_find(p)) return;
  if(((struct objhead *)((unsigned long)p - 4))->mark 
     || ((struct objhead *)((unsigned long)p - 4))->markf) {
    struct mpage *ppage = pmap_find(p);
    if(ppage && ppage->bigpage) {
      struct objhead *info = 
	(struct objhead *)((unsigned long)ppage + PAGE_HEADER_SIZE);
      if(info->owner != gc_owner) {
	unsigned short oldowner = info->owner;
	info->owner = ot_union(info->owner, gc_owner);
	if(oldowner != info->owner)
	  precise_reprop(ppage, ppage, info->owner);
      }
    } else {
      void *newp = *(void**)p;
      struct mpage *page = pmap_find(newp);
      struct objhead *info = (struct objhead *)((unsigned long)newp - 4);
      if(info->owner != gc_owner) {
	unsigned short oldowner = info->owner;
	info->owner = ot_union(info->owner, gc_owner);
	if(oldowner != info->owner)
	  precise_reprop(newp, page, info->owner);
      }
    }
  }
}
#endif

#if defined(NEWGC_ACCNT) && defined(NEWGC_USE_HEADER)
inline void mark_owner_reprocess(const void *p) {
  void *newp = *(void**)p;
  struct objhead *info = (struct objhead *)((unsigned long)newp - 4);
  if(info->owner != gc_owner) {
    struct mpage *page = pmap_find(newp);
# ifdef NEWGC_PRECISE
    unsigned short oldowner = info->owner;
# endif
    ot_accnt(info->owner, page->gen, -gcWORDS_TO_BYTES(info->size));
    info->owner = ot_union(info->owner, gc_owner);
    ot_accnt(info->owner, page->gen, gcWORDS_TO_BYTES(info->size));
# ifdef NEWGC_PRECISE
    if(newp < (void*)((unsigned long)page + page->psize))
      if(oldowner != info->owner)
	precise_reprop(newp, page, info->owner);
# endif
  }
}
#else
#define mark_owner_reprocess(p) { }
#endif

inline void mark_normal(struct mpage *page, const void *p) {
  struct objhead *info = (struct objhead *)((unsigned long)p - 4);
  if(info->mark) {
    mark_owner_reprocess(p);
    return;
  } else if(info->markf) {
    ((struct objhead *)((unsigned long)p - 4))->mark = 1;
    mark_owner_reprocess(p);
  } else {
    unsigned short type = info->type;
    unsigned short gen = page ? INCGEN(page->gen) : 1;
    void *newplace;

    if(type == MPAGE_TAGGED)
      if(atomic_table[*(Type_Tag*)p]) {
	info->type = MPAGE_ATOMIC;
	type = MPAGE_ATOMIC;
      }
    newplace = copy_bits((void*)info, info->size, type, gen);
/*     GCDBG("Moved %p (size %i) to %p\n", info, info->size, newplace); */
    info->mark = 1;
    *(void**)p = (void*)((unsigned long)newplace + 4);
#ifdef NEWGC_ACCNT
    ot_accnt(gc_owner, gen, gcWORDS_TO_BYTES(info->size));
    ((struct objhead *)newplace)->owner = gc_owner;
#endif    
  }
}

inline void mark_bigpage(struct mpage *page, const void *p) {
  if(page->bigpage == 1) {
#ifdef NEWGC_ACCNT
    ot_accnt(gc_owner, INCGEN(page->gen), page->size);
    ((struct objhead *)((unsigned long)page + PAGE_HEADER_SIZE))->owner
      = gc_owner;
#endif
    page->bigpage++;
    mark_propbig(page);
  } else {
#ifdef NEWGC_USE_HEADER
    struct objhead *info = 
      (struct objhead *)((unsigned long)page + PAGE_HEADER_SIZE);
    if(info->owner != gc_owner) {
#ifdef NEWGC_PRECISE
      unsigned short old_owner = info->owner;
#endif
      ot_accnt(info->owner, INCGEN(page->gen), -page->size);
      info->owner = ot_union(info->owner, gc_owner);
      ot_accnt(info->owner, INCGEN(page->gen), page->size);
# ifdef NEWGC_PRECISE
      if(old_owner != info->owner)
	precise_reprop(page, page, info->owner);
# endif
    }
#endif
  }
}

void GC_mark(const void *p) {
  if(!p || ((unsigned long)p & 0x1)) return;

#ifdef NEWGC_PRECISE
  if(gc_markprecise) { precise_mark(p); return; }
#endif
  if(tla_member_p(p)) mark_normal(NULL, p); else {
    struct mpage *page = pmap_find(p);
    if(!page || (page->gen > gc_topgen)) return;
    if(page->bigpage) mark_bigpage(page, p); else mark_normal(page, p);   
  }
}
  
static void mark_proptagged(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->psize);
  while(start < (void**)((unsigned long)page + page->size)) {
#ifdef NEWGC_ACCNT
    struct objhead *info = (struct objhead *)start++;
    gc_owner = info->owner;
#else
    start++;
#endif
    start += mark_table[*(Type_Tag*)start](start);
  }
}

static void mark_proparray(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->psize);
  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start++;
    unsigned long size = info->size;
#ifdef NEWGC_USE_ACCNT
    gc_owner = info->owner;
#endif
    while(--size) GC_mark(*(start++));
  }
}

static void mark_proptarray(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->psize);
  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start;
    void **tempend = start + info->size;
    Type_Tag tag = *(Type_Tag*)(++start);
#ifdef NEWGC_ACCNT
    gc_owner = info->owner;
#endif
    while(start < tempend) start += mark_table[tag](start);
  }
}

static void mark_propxtagged(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->psize);
  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start;
    unsigned long size = info->size;
#ifdef NEWGC_ACCNT
    gc_owner = info->owner;
#endif
    GC_mark_xtagged(start + 1);
    start += size;
  }
}

static void mark_propogate(void) {
  struct mpage *page;
  short i, j;
  short topgenp1 = INCGEN(gc_topgen);
  bool changed = true;

  while(changed) {
    changed = false;
    for(i = 1; i <= topgenp1; i++) {
      for(j = 0; j < MPAGE_TYPES; j++) {
	for(page = gen[i][j]; page; page = page->next) {
	  if(page->psize != page->size) {
	    if(j == MPAGE_BIG) {
	      mark_propbig(page);
	    } else {
	      switch(page->type) {
	      case MPAGE_TAGGED: mark_proptagged(page); break;
	      case MPAGE_ATOMIC: break;
	      case MPAGE_ARRAY: mark_proparray(page); break;
	      case MPAGE_TARRAY: mark_proptarray(page); break;
	      case MPAGE_XTAGGED: mark_propxtagged(page); break;
	      }
	    }
	    page->psize = page->size;
	    changed = true;
	  }
	}
      }
    }
  }
}

/*****************************************************************************
 * Fixup routines
 *****************************************************************************/

void *GC_resolve(void *p) {
  if(tla_member_p(p)) {
    if(((struct objhead *)((unsigned long)p - 4))->mark) return *(void**)p;
    else return p;
  } else {
    struct mpage *page = pmap_find(p);
    if(!page) return p;
    if(((struct objhead *)((unsigned long)p - 4))->mark) return *(void**)p;
    else return p;
  }
}

void GC_fixup(void *pp) {
  void *p = *(void**)pp;
  if(!p || ((long)p & 0x1)) return;
  else {
    if(tla_member_p(p)) {
      struct mpage *ppage;
      *(void**)pp = *(void**)p;
      ppage = pmap_find(pp);
      if(ppage && !ppage->bpointers) {
	if(ppage->gen > 1) ppage->bpointers = 1;
      }
    } else {
      struct mpage *page = pmap_find(p);
      if(page) {
	if(page->bigpage) {
	  struct mpage *ppage = pmap_find(pp);
	  if(ppage && !ppage->bpointers)
	    if(ppage->gen > page->gen)
	      ppage->bpointers = 1;
	} else {
	  struct objhead *info = (struct objhead *)((unsigned long)p - 4);
	  if(info->mark || info->markf) {
	    void **newloc = *(void**)p;
	    struct mpage *ppage = pmap_find(pp);
	    struct mpage *npage = pmap_find(newloc);
	    *(void**)pp = newloc;
	    if(ppage && npage && !ppage->bpointers)
	      if(ppage->gen > npage->gen)
		ppage->bpointers = 1;
	  }
	}
      }
    }
  }
}

static void fixup_bpage(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE + 4);
  void **end = (void**)((unsigned long)page + page->size);

  switch(page->type) {
    case MPAGE_TAGGED: fixup_table[*(Type_Tag*)start](start); break;
    case MPAGE_ATOMIC: break;
    case MPAGE_ARRAY: while(start < end) gcFIXUP(*(start++)); break;
    case MPAGE_TARRAY: {
      Type_Tag tag = *(Type_Tag*)start;
      while(start < end) start += fixup_table[tag](start);
      break;
    }
    case MPAGE_XTAGGED: GC_fixup_xtagged(start); break;
  }
}

static void fixup_tagged(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
  void **end = (void**)((unsigned long)page + page->size);
  
  while(start < end) {
    if( ((struct objhead *)start)->markf
	|| ((struct objhead *)start)->mark )
      printf("Mark from other place was retained!\n");
    start++;
    start += fixup_table[*(Type_Tag*)start](start);
  }
}

static void fixup_array(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
  void **end = (void**)((unsigned long)page + page->size);
  
  while(start < end) {
    struct objhead *ohead = (struct objhead *)start++;
    unsigned long size = ohead->size;
    while(--size) gcFIXUP(*(start++));
  }
}

static void fixup_tarray(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start;
    unsigned long size = ohead->size;
    void **tempend = start + size;
    Type_Tag tag = *(Type_Tag*)(++start);
    while(start < tempend) start += fixup_table[tag](start);
  }
}

static void fixup_xtagged(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start;
    unsigned long size = ohead->size;
    GC_fixup_xtagged(start + 1);
    start += size;
  }
}

static void fixup_heap(void) {
  struct mpage *page;
  short i, j;
  short topgenp1 = INCGEN(gc_topgen);

  for(i = 0; i <= topgenp1; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = gen[i][j]; page; page = page->next) {
	switch(j) {
	  case MPAGE_TAGGED: fixup_tagged(page); break;
	  case MPAGE_ATOMIC: break;
	  case MPAGE_ARRAY: fixup_array(page); break;
	  case MPAGE_TARRAY: fixup_tarray(page); break;
	  case MPAGE_XTAGGED: fixup_xtagged(page); break;
	  case MPAGE_BIG: fixup_bpage(page); break;
	}
	page->psize = page->size;
      }
  for(i = topgenp1 + 1; i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = gen[i][j]; page; page = page->next) {
	if(page->bpointers) {
	  switch(j) {
 	    case MPAGE_TAGGED: fixup_tagged(page); break;
	    case MPAGE_ATOMIC: break;
	    case MPAGE_ARRAY: fixup_array(page); break;
	    case MPAGE_TARRAY: fixup_tarray(page); break;
	    case MPAGE_XTAGGED: fixup_xtagged(page); break;
	    case MPAGE_BIG: fixup_bpage(page); break;
	  }
	  page->psize = page->size;
	}
      }
}

/*****************************************************************************
 * Routines having to do with older generations
 *****************************************************************************/
void designate_modified(void *p) {
  struct mpage *page = pmap_find(p);
  if(page != NULL) {
    protect_pages(page, page->size, 1);
    page->bpointers = 1;
  } else {
    fprintf(stderr, "Seg fault (internal error) at %p\n", p);
    abort();
  }
}

static void old_protect(void) {
  struct mpage *work;
  int i, j;

  for(i = 1; i < GENERATIONS; i++)
    for(j = 0; j < MPAGE_TYPES; j++)
      for(work = gen[i][j]; work; work = work->next)
	if(work->type != MPAGE_ATOMIC)
	  protect_pages(work, work->size, 0);
}

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

static void old_init(void) {
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

static void old_mark(void) {
  struct mpage *page;
  short i, j;

  for(i = gc_topgen + 1; i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = gen[i][j]; page; page = page->next)
 	if(page->bpointers) {
	  switch(j) {
  	    case MPAGE_TAGGED: {
	      void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
	      void **end = (void**)((unsigned long)page + page->psize);
	      while(start < end) {
#ifdef NEWGC_ACCNT
		struct objhead *info = (struct objhead *)start++;
		gc_owner = info->owner;
#else
		start++; 
#endif
		start += mark_table[*(Type_Tag*)start](start);
	      }
	      break;
	    }
	    case MPAGE_ATOMIC: break;
	    case MPAGE_ARRAY: {
	      void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
	      void **end = (void**)((unsigned long)page + page->psize);
	      while(start < end) {
		struct objhead *info = (struct objhead *)start++;
		unsigned long size = info->size;
#ifdef NEWGC_ACCNT
		gc_owner = info->owner;
#endif
		while(--size) GC_mark(*(start++));
	      }
	      break;
	    }
	    case MPAGE_TARRAY: {
	      void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
	      void **end = (void**)((unsigned long)page + page->psize);
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		void **tempend = start + info->size;
		Type_Tag tag = *(Type_Tag*)(++start);
#ifdef NEWGC_ACCNT
		gc_owner = info->owner;
#endif
		while(start < tempend) start += mark_table[tag](start);
	      }
	      break;
	    }
  	    case MPAGE_XTAGGED: {
	      void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
	      void **end = (void**)((unsigned long)page + page->psize);
	      while(start < end) {
		struct objhead *info = (struct objhead *)start;
		unsigned long size = info->size;
#ifdef NEWGC_ACCNT
		gc_owner = info->owner;
#endif
		GC_mark_xtagged(start + 1);
		start += size;
	      }
	      break;
	    }
	    case MPAGE_BIG: mark_propbig(page); break;
	  }
 	} 
}

/*****************************************************************************
 * Collection routines
 *****************************************************************************/

static struct mpage *gc_pages = NULL;

static void gc_runcycle(int force_full) {
  gc_numcollects += 1;
  gc_cycle += 1;
  if(force_full) {
    gc_topgen = 2; 
    gc_cycle = 0;
  } else if(gc_cycle < 12) {
    gc_topgen = 0;
  } else if(gc_cycle < 15) {
    gc_topgen = 1;
  } else {
    gc_topgen = 2;
    gc_cycle = 0;
  }
}

static void gc_list_len(void) {
  unsigned long len = 0;
  struct mpage *page = gc_pages;
  while(page) { len++; page = page->next; }
}

static void gc_prepare(void) {
  short i, j;

  gc_pages = NULL;
  for(i = 0; i <= gc_topgen; i++) 
    for(j = 0; j < MPAGE_TYPES; j++) {
      struct mpage *work = gen[i][j];
      while(work) {
	struct mpage *next = work->next;
	protect_pages(work, work->size, 1);
	work->next = gc_pages;
	gc_pages = work;
	work = next;
      }
      gen[i][j] = NULL;
    }
  gc_list_len();
#ifdef NEWGC_ACCNT
  ot_collectprep();
#endif
}

static void gc_cleanup(void) {
  struct mpage *work = gc_pages, *prev = NULL;
    
  gc_list_len();
  /* first, we need to see if we need to move any bpages out and
     fix them */
  while(work) {
    if(work->bigpage > 1) {
      if(prev) prev->next = work->next;
      else gc_pages = work->next;

      work->gen = INCGEN(work->gen);
      work->next = gen[work->gen][MPAGE_BIG];
      gen[work->gen][MPAGE_BIG] = work;
      fixup_bpage(work);
      work->bigpage = 1;

      if(prev) work = prev->next;
      else work = gc_pages;
    } else {
      prev = work;
      work = work->next;
    }
  }

  /* now we need to delete everything we didn't save */
  work = gc_pages;
  while(work) {
    struct mpage *temp = work;

    work = work->next;
    if(temp->bigpage) {
      pmap_remove(temp);
      protect_pages(temp, temp->size, 1);
      mla_free(temp, temp->size, 1);
    } else {
      pmap_remove(temp);
      protect_pages(temp, MPAGE_SIZE, 1);
      mla_free(temp, MPAGE_SIZE, 0);
    }
  }
  {
    void **work = tla_heap;
    while(work <= tla_regions) *(work++) = NULL;
  }
  bpage_g0size = 0;
  tla_regions = tla_heap;
  old_protect();
  flush_freed_pages();
}

#ifdef NEWGC_DEBUG
static void debug_dump_heap(void) {
  struct mpage *page;
  void **start = tla_heap;
  void **end = tla_regions;
  short i, j;

  GCDBG("Gen0: %p - %p\n", tla_heap, tla_regions);
  while(start < end) {
    GCDBG("%8p: %08lx %08lx %08lx %08lx %08lx %08lx %08lx %08lx\n",
	  start, (unsigned long)*start, (unsigned long)*(start + 1),
	  (unsigned long)*(start + 2), (unsigned long)*(start + 3),
	  (unsigned long)*(start + 4), (unsigned long)*(start + 5),
	  (unsigned long)*(start + 6), (unsigned long)*(start + 7));
    start += 8;
  }
  for(i = 0; i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++) 
      for(page = gen[i][j]; page; page = page->next) {
	GCDBG("Page %p (type %i, bigpage %i, gen %i)\n",
	      page, page->type, page->bigpage, page->gen);
	start = (void**)page;
	end = (void**)((unsigned long)page + page->size);
	while(start < end) {
	  GCDBG("%8p: %08lx %08lx %08lx %08lx %08lx %08lx %08lx %08lx\n",
		start, (unsigned long)*start, (unsigned long)*(start + 1),
		(unsigned long)*(start + 2), (unsigned long)*(start + 3),
		(unsigned long)*(start + 4), (unsigned long)*(start + 5),
		(unsigned long)*(start + 6), (unsigned long)*(start + 7));
	  start += 8;
	}
      }  
}
#endif

static void gc_collect(int force_full) {
  if(!gc_deny) {
#ifdef NEWGC_ACCNT
    short curown = ot_current();
    struct owner_list *ol = ol_add(NULL, curown), *temp;
#endif

    gc_runcycle(force_full);
    if(GC_collect_start_callback) GC_collect_start_callback();

    gc_prepare();
/*     printf("Collection #%li (topgen %i, tla_heap %p, tla_regions %p)\n",  */
/* 	   gc_numcollects, gc_topgen, tla_heap, tla_regions); fflush(stdout); */
/*     debug_dump_heap(); */
    rq_mark();
#ifdef NEWGC_ACCNT
    /* compute the owner list */
    ol = threads_get_owners(ol);
    ol = roots_get_owners(ol);
    ol = accnt_get_owners(ol);
    ol = imm_get_owners(ol);
    ol = fnl_get_owners(ol);
    ol = wfnl_get_owners(ol);
    /* sort the owner list */
    ol = ol_sort(ol);

    if(gc_topgen == (GENERATIONS - 1)) wfnl_clear_weaks();
    /* mark the roots from the owner list */
    old_mark();
    for(temp = ol; temp; temp = temp->next) {
      gc_owner = temp->owner; 
      threads_mark(temp->owner);
      roots_mark(temp->owner); 
      accnt_mark(temp->owner); 
      imm_mark(temp->owner); 
      fnl_mark(temp->owner); 
      wfnl_mark(temp->owner);
      if(temp->owner == curown)
	GC_mark_variable_stack(GC_variable_stack, 0, 
			       (void*)(GC_get_thread_stack_base
				       ? GC_get_thread_stack_base()
				       : (unsigned long)stack_base));
      mark_propogate();
    }
#else
    if(roots) roots_sort_merge(roots);
    if(gc_topgen == (GENERATIONS - 1)) wfnl_clear_weaks();
    roots_mark(0);
    imm_mark(0);
    fnl_mark(0);
    wfnl_mark(0);
    GC_mark_variable_stack(GC_variable_stack, 0,
			   (void*)(GC_get_thread_stack_base
				   ? GC_get_thread_stack_base()
				   : (unsigned long)stack_base));
    old_mark();
    mark_propogate();
#endif

    /* propogate stuff from finals*/
    if(gc_topgen == (GENERATIONS - 1)) {
      wfnl_reset_weaks();
      mark_propogate();
    }

    /* repair */
#ifdef NEWGC_ACCNT
    ot_fixup(ol); 
    ol_free(ol); 
    threads_fixup();
#endif
    rq_fixup(); 
    roots_fixup(); 
#ifdef NEWGC_ACCNT
    /* accnt_fixup must be after ot_fixup */
    accnt_fixup(); 
#endif
    imm_fixup(); 
    fnl_fixup(); 
    wfnl_fixup();
    GC_fixup_variable_stack(GC_variable_stack, 0,
			    (void*)(GC_get_thread_stack_base
				    ? GC_get_thread_stack_base()
				    : (unsigned long)stack_base));
    fixup_heap();
    gc_cleanup();

    if(GC_collect_start_callback) GC_collect_end_callback();
    rq_run();
/*     printf("done\n"); fflush(stdout); */
  }
}

void GC_gcollect() { 
  gc_collect(1); 
}

/*****************************************************************************
 * Memory allocation (system level)
 *****************************************************************************/
/* Windows */

#if _WIN32

void *malloc_pages(size_t len, size_t alignment)
{
  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

void free_pages(void *p, size_t len)
{
  VirtualFree(p, 0, MEM_RELEASE);
}

void flush_freed_pages(void)
{
}

void protect_pages(void *p, size_t len, int writeable)
{
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/

/* OSKit */

#if OSKIT
# include <oskit/c/malloc.h>

void *malloc_pages(size_t len, size_t alignment)
{
  void *p;
  p = smemalign(alignment, len);
  memset(p, 0, len);
  return p;
}

void free_pages(void *p, size_t len)
{
  sfree(p, len);
}

void flush_freed_pages(void)
{
}

# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/

/* Default: mmap */

#ifndef MALLOCATOR_DEFINED

# include <unistd.h>
# include <fcntl.h>
# include <sys/types.h>
# include <sys/mman.h>
# include <errno.h>

#ifndef MAP_ANON
int fd, fd_created;
#endif

/* Instead of immediaately freeing pages with munmap---only to mmap
   them again---we cache BLOCKFREE_CACHE_SIZE freed pages. A page is
   cached unused for at most BLOCKFREE_UNMAP_AGE cycles of the
   collector. (A max age of 1 seems useful, anything more seems
   dangerous.) 

   The cache is small enough that we don't need an elaborate search
   mechanism, but we do a bit of work to collapse adjacent pages in
   the cache. */

typedef struct {
  void *start;
  long len;
  int age;
} Free_Block;

#define BLOCKFREE_UNMAP_AGE 1
#define BLOCKFREE_CACHE_SIZE 96
static Free_Block blockfree[BLOCKFREE_CACHE_SIZE];

static int compare_free_block(const void *a, const void *b)
{
  if ((unsigned long)((Free_Block *)a)->start < (unsigned long)((Free_Block *)b)->start)
    return -1;
  else
    return 1;
}

void collapse_adjacent_pages(void)
{
  int i, j;

  /* collapse adjacent: */
  my_qsort(blockfree, BLOCKFREE_CACHE_SIZE, sizeof(Free_Block), compare_free_block);
  j = 0;
  for (i = 1; i < BLOCKFREE_CACHE_SIZE; i++) {
    if ((blockfree[j].start + blockfree[j].len) ==blockfree[i].start) {
      blockfree[j].len += blockfree[i].len;
      blockfree[i].start = NULL;
      blockfree[i].len = 0;
    } else
      j = i;
  }
}


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

  /* Something from the cache, perhaps? */
  {
    int i;

    /* Try an exact fit: */
    for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
      if (blockfree[i].len == len) {
	r = blockfree[i].start;
	if (!alignment || !((unsigned long)r & (alignment - 1))) {
	  blockfree[i].start = NULL;
	  blockfree[i].len = 0;
	  memset(r, 0, len);
	  return r;
	}
      }
    }

    /* Try a first fit: */
    for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
      if (blockfree[i].len > len) {
	/* Align at start? */
	r = blockfree[i].start;
	if (!alignment || !((unsigned long)r & (alignment - 1))) {
	  blockfree[i].start += len;
	  blockfree[i].len -= len;
	  memset(r, 0, len);
	  return r;
	}

	/* Align at end? */
	r = blockfree[i].start + (blockfree[i].len - len);
	if (!((unsigned long)r & (alignment - 1))) {
	  blockfree[i].len -= len;
	  memset(r, 0, len);
	  return r;
	}

	/* We don't try a middle alignment, because that would
	   split the block into three. */
      }
    }

    /* Nothing useable in the cache... */
  }

  extra = alignment;

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

    real_r = (void *)(((unsigned long)r + (alignment - 1)) & (~(alignment - 1)));
    
    pre_extra = real_r - r;
    if (pre_extra)
      if (munmap(r, pre_extra))
	fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", (long)r, pre_extra, errno);
    if (pre_extra < extra)
      if (munmap(real_r + len, extra - pre_extra))
	fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", (long)r, pre_extra, errno);
    r = real_r;
  }

  page_allocations += len;

  return r;
}

void free_pages(void *p, size_t len)
{
  int i;

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  page_allocations -= len;

  /* Try to free pages in larger blocks, since the OS may be slow. */

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (p == blockfree[i].start + blockfree[i].len) {
      blockfree[i].len += len;
      return;
    }
    if (p + len == blockfree[i].start) {
      blockfree[i].start = p;
      blockfree[i].len += len;
      return;
    }
  }

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (!blockfree[i].start) {
      blockfree[i].start = p;
      blockfree[i].len = len;
      blockfree[i].age = 0;
      return;
    }
  }

  /* Might help next time around: */
  collapse_adjacent_pages();

  if (munmap(p, len)) {
    fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", (long)p, (long)len, errno);
  }
}

void flush_freed_pages(void)
{
  int i;

  collapse_adjacent_pages();

  for (i = 0; i < BLOCKFREE_CACHE_SIZE; i++) {
    if (blockfree[i].start) {
      if (blockfree[i].age == BLOCKFREE_UNMAP_AGE) {
	if (munmap(blockfree[i].start, blockfree[i].len)) {
	  fprintf(stderr, "Unmap warning: %lx, %ld, %d\n", 
		  (long)blockfree[i].start, blockfree[i].len,
		  errno);
	}
	blockfree[i].start = NULL;
	blockfree[i].len = 0;
      } else
	blockfree[i].age++;
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
/*****************************************************************************
 * Miscellaneous routines
 *****************************************************************************/

void GC_free(void *p) {}

static char *type_name[MPAGE_TYPES] = { "tagged",
					"atomic",
					"array",
					"tagged array",
					"xtagged",
					"big" };
void GC_dump() {
  unsigned long gen0_bigs = 0, gen0_bigs_size = 0, i;
#ifdef NEWGC_ACCNT
  unsigned long own_memuse = 0;
  unsigned short num_owners = 0;
  struct root *root;
  struct thread_list *thread;
  struct accnthook *hook;
  struct immobile *box;
  struct fnl *fnl;
#endif
  struct mpage *page;

  for(page = gen[0][MPAGE_BIG]; page; page = page->next)
    { gen0_bigs += 1; gen0_bigs_size += page->size; }
  fprintf(stderr, 
	  "Generation 0: %li of %li bytes used (+ %li in %li bigpages)\n",
	  ((unsigned long)tla_regions - (unsigned long)tla_heap), 
	  (unsigned long)GEN0_SIZE, gen0_bigs_size, gen0_bigs);
  for(i = 1; i < GENERATIONS; i++) {
    unsigned long pages = 0, size = 0, j;
    unsigned long tpages[MPAGE_TYPES] = { 0, 0, 0, 0, 0, 0 };
    unsigned long tsize[MPAGE_TYPES] = { 0, 0, 0, 0, 0, 0 };

    for(j = 0; j < MPAGE_TYPES; j++) 
      for(page = gen[i][j]; page; page = page->next) {
	pages += 1; size += page->size;
	tpages[j] += 1; tsize[j] += page->size;
      }
    fprintf(stderr, "Generation %li: %li bytes used in %li pages\n",
	    i, size, pages);
    if(size) {
      for(j = 0; j < MPAGE_TYPES; j++) {
	fprintf(stderr, "   ... %li %s pages (%li bytes, %2.2f%%)\n",
		tpages[j], type_name[j], tsize[j],
		100.0 * ((float)tsize[j] / (float)size));
      }
    }
  }

#ifdef NEWGC_ACCNT
  for(i = 0; i < ot_top; i++)
    if(ot_table[i]) {
      struct cust_list *cl;
      struct union_list *ul;

      num_owners++;
      own_memuse += sizeof(struct otentry);
      for(cl = ot_table[i]->custs; cl; cl = cl->next)
	own_memuse += sizeof(struct cust_list);
      for(ul = ot_table[i]->unions; ul; ul = ul->next)
	own_memuse += sizeof(struct union_list);
    }
  for(root = roots; root; root = root->next)
    own_memuse += sizeof(short);
  for(thread = thread_list; thread; thread = thread->next)
    own_memuse += sizeof(struct thread_list);
  for(hook = accnthooks; hook; hook = hook->next)
    own_memuse += sizeof(struct accnthook);
  for(box = immobiles; box; box = box->next)
    own_memuse += sizeof(short);
  for(fnl = fnls; fnl; fnl = fnl->next)
    own_memuse += sizeof(short);
  own_memuse += ot_top * sizeof(struct otentry *);
  fprintf(stderr, "Tracking %i owners\n", num_owners);
  fprintf(stderr, "   ... owner table top = %i\n", ot_top);
  fprintf(stderr, "   ... using %li bytes in owner system\n", own_memuse);
  for(i = 0; i < ot_top; i++) 
    if(ot_table[i]) {
      struct cust_list *cl;
      unsigned long use = 0, j = 0;
      
      for(j = 0; j < GENERATIONS; j++)
	use += ot_table[i]->memuse[j];
      fprintf(stderr, "OTENTRY #%li (%li): creator(%p)", i, use, 
	      ot_table[i]->creator);
      for(cl = ot_table[i]->custs; cl; cl = cl->next)
	fprintf(stderr, ", %p", cl->cust);
      fprintf(stderr, "\n");
    }
#endif
}

/*****************************************************************************
 * Callbacks
 *****************************************************************************/

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);
