#define MZ_PRECISE_GC 1
/*
   A new Precise GC for MzScheme
   Copyright (C) 2001 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for possible improvement points
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc2.h"
#include "../src/schpriv.h"

#warning "This collector is incompatible with the GL extension."
#warning "If you haven't already, rerun configure with '--disable-gl'"

/* To do accounting, turn on this: */
#define NEWGC_ACCNT

/* To do precise accounting, turn on this: */
/* #define NEWGC_PRECISE */

/* To make things very slow and create very large files, uncomment this: */
/* #define NEWGC_DEBUG */

#ifdef NEWGC_DEBUG
#include <stdarg.h>
FILE *debug;
void GCDBG(char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vfprintf(debug, format, ap);
  va_end(ap);
  fflush(debug);
}
#else
#define GCDBG(x, ...) {}
#endif

/* #defines you can change without definately breaking the collector */
#define GEN0_SIZE		(8 * 1024 * 1024)
#define OT_INIT_SIZE		5
#define OT_ADD_SIZE		10

/* #defines you can change which will break the collector */
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

static void *malloc_pages(size_t len, size_t align);
static void free_pages(void *p, size_t len);
static void flush_freed_pages(void);
static void protect_pages(void *p, size_t len, int rw);
static void *bpage_malloc(size_t sizeb, int type, int tgen);
static void gc_collect(int full);
static void pmap_add(void *page);
static void pmap_remove(void *page);
static void rq_add(int pri, short type, void *obj);
static bool mark_p(void *p);
static bool ot_subset_p(short owner1, short owner2);
static void old_init(void);

static unsigned long gc_numcollects = 0;
static unsigned int gc_cycle = 0;
static unsigned int gc_topgen = 0;
#ifdef NEWGC_ACCNT
static unsigned short gc_owner = 0;
# ifdef NEWGC_PRECISE
static bool gc_markprecise = false;
# endif
#endif
static bool gc_deny = 0;

/*****************************************************************************
 * Owner information structures and primitives
 *****************************************************************************/
struct owner_list {
  short owner;
  struct owner_list *next;
};

struct cust_list {
  Scheme_Custodian *cust;
  struct cust_list *next;
};

struct union_list {
  short owner;
  short result;
  struct union_list *next;
};

struct otentry {
  Scheme_Custodian *creator;
  unsigned long memuse[GENERATIONS];
  struct cust_list *custs;
  struct union_list *unions;
};

static struct otentry **ot_table = NULL;
static unsigned short ot_top = 0;

static bool ol_member_p(struct owner_list *ol, short owner) {
  while(ol) 
    if(ol->owner == owner) return true;
    else ol = ol->next;
  return false;
}

static struct owner_list *ol_add(struct owner_list *ol, short owner) {
  struct owner_list *retval = malloc(sizeof(struct owner_list));
  retval->owner = owner;
  retval->next = ol;
  return retval;
}

static struct owner_list *ol_sort(struct owner_list *ol) {
  if(!ol || !ol->next) return ol;
  else {
    struct owner_list **table;
    struct owner_list *temp;
    short lslen = 0;
    short i;
    bool changed = 1;
    
    for(temp = ol; temp; temp = temp->next)
      lslen += 1;
    table = malloc(lslen * sizeof(struct owner_list *));
    for(temp = ol, i = 0; temp; temp = temp->next, i++)
      table[i] = temp;
    lslen -= 1; /* save a few cycles at the cost of confusion. YAY! */
    while(changed) {
      changed = 0;
      for(i = 0; i < lslen; i++) {
	if(ot_subset_p(table[i]->owner, table[i+1]->owner)) {
	  temp = table[i];
	  table[i] = table[i+1];
	  table[i+1] = temp;
	  changed = 1;
	}
      }
    }
    for(i = 0; i < lslen; i++) 
      table[i]->next = table[i+1];
    table[lslen]->next = NULL;
    temp = table[0];
    free(table);
    return temp;
  }
}

static bool cl_member_p(struct cust_list *cl, Scheme_Custodian *c) {
  while(cl)
    if(cl->cust == c) return true;
    else cl = cl->next;
  return false;
}

static struct cust_list *cl_add(struct cust_list *cl, Scheme_Custodian *c) {
  struct cust_list *retval = malloc(sizeof(struct cust_list));
  retval->cust = c;
  retval->next = cl;
  return retval;
}

static bool ot_subset_p(short owner1, short owner2) {
  struct cust_list *clist;

  for(clist = ot_table[owner1]->custs; clist; clist = clist->next)
    if(!cl_member_p(ot_table[owner2]->custs, clist->cust))
      return 0;
  return 1;
}

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

static short ot_convert(Scheme_Custodian *cust) {
  short i;

  for(i = 0; i < ot_top; i++)
    if(ot_table[i] && (ot_table[i]->creator == cust))
      return i;
  return ot_addentry(cust);
}

short ot_current() {
  Scheme_Custodian *c;

  if(!scheme_current_thread) {
    return 0;
  } else if(!(scheme_current_thread->config)) {
    return NULL;
  } else {
    c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
    return ot_convert(c);
  }
}

static short ot_union(short own1, short own2) {
  struct union_list *ulist;
  short res;

  if(own1 > own2) return ot_union(own2, own1); /* this just simplifies life */
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

	  done = true;
	  ot_table[res] = malloc(sizeof(struct otentry));
	  ot_table[res]->creator = NULL;
	  ot_table[res]->custs = NULL;
	  ot_table[res]->unions = NULL;
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

static unsigned long ot_memuse(Scheme_Custodian *c) {
  unsigned long retval = 0;
  short i, j;

  for(i = 0; i < ot_top; i++) 
    if(ot_table[i] && cl_member_p(ot_table[i]->custs, c))
      for(j = 0; j < GENERATIONS; j++)
	retval += ot_table[i]->memuse[j];
  return retval;
}

static void ot_fixup(void) {
  struct cust_list *temp;
  short i;

  for(i = 0; i < ot_top; i++)
    if(ot_table[i]) {
      if(!mark_p(ot_table[i]->creator)) ot_table[i]->creator = NULL;
      else GC_fixup(&(ot_table[i]->creator));
      for(temp = ot_table[i]->custs; temp; temp = temp->next)
	if(!mark_p(temp->cust)) temp->cust = NULL;
        else gcFIXUP(temp->cust);
    }
}

static void ot_collectprep() {
  unsigned short i, j;

  for(i = 0; i < ot_top; i++)
    if(ot_table[i])
      for(j = 0; j <= gc_topgen; j++)
	ot_table[i]->memuse[j] = 0;
}

static void ot_accnt(unsigned short owner, short gen, unsigned long size) {
  ot_table[owner]->memuse[gen] += size;
}

/*****************************************************************************
 * Root structures and primitives
 *****************************************************************************/

struct root {
  short owner;
  void **start;
  void **end;
  bool nothing_new;
  struct root *next;
};

struct root *roots = NULL;

void GC_add_roots(void *start, void *end) {
  struct root *newroot = malloc(sizeof(struct root));
  newroot->owner = 23945;
  newroot->owner = ot_current();
  newroot->start = (void**)start;
  newroot->end = (void**)end;
  newroot->next = roots;
  roots = newroot;
}

static struct owner_list *roots_get_owners(struct owner_list *ol) {
  struct root *work;

  for(work = roots; work; work = work->next)
    if(!ol_member_p(ol, work->owner))
      ol = ol_add(ol, work->owner);
  return ol;
}

static void roots_mark(short owner) {
  struct root *work;

  for(work = roots; work; work = work->next)
    if(work->owner == owner) {
      void **start = work->start;
      while(start < work->end) {
	gcMARK(*(start++));
      }
    }
}

static void roots_fixup(void) {
  struct root *work;

  for(work = roots; work; work = work->next) {
    void **start = work->start;
    while(start < work->end) gcFIXUP(*(start++));
  }
}

/*****************************************************************************
 * Memory allocation (midlevel)
 *****************************************************************************/

#if defined(__FreeBSD__)
#include <machine/vmparam.h>
#define MAX_HEAP_SIZE		MAXDSIZ
#elif defined(linux)
unsigned long MAX_HEAP_SIZE;
#error "This won't work"
#else
#define MAX_HEAP_SIZE		(1024*1024*1024) /* FIXME: Should be syscall*/
#endif


#define MPAGE_SIZE		(1 << LOG_MPAGE_SIZE)
#define PAGES_IN_HEAP		(MAX_HEAP_SIZE / MPAGE_SIZE)
#define MAX_USED_PAGES		(PAGES_IN_HEAP / 2)

struct mpage_list {
  void *mpage;
  struct mpage_list *next;
};

static unsigned long mla_usedpages = 0;
static struct mpage_list *mla_freedpages = NULL;

static void *mla_malloc(size_t size) {
  unsigned long numpages = (size/MPAGE_SIZE)+(((size%MPAGE_SIZE)==0) ? 0 : 1);

  if((numpages == 1) && mla_freedpages) {
    struct mpage_list *temp = mla_freedpages;
    void *retval = temp->mpage;
    void **start = (void**)retval; 
    void **end = (void**)((unsigned long)retval + size); 
    mla_freedpages = temp->next;
    free(temp);
    while(start < end) *(start++) = NULL; 
    /* Timing benefits:

       bzero call:
18.114u 1.740s 0:23.91 83.0%    906+12178k 0+11io 93pf+0w (quiet.ss)
18.047u 1.740s 0:23.68 83.5%    906+12183k 0+11io 0pf+0w (quiet.ss)
17.927u 1.832s 0:23.70 83.3%    909+12221k 0+11io 0pf+0w (quiet.ss)
18.029av (quiet.ss)
       Custom loop:
17.945u 1.899s 0:23.98 82.6%    905+12167k 0+11io 93pf+0w (quiet.ss)
18.125u 1.684s 0:23.74 83.4%    905+12166k 0+11io 0pf+0w (quiet.ss)
17.999u 1.808s 0:23.78 83.2%    909+12216k 0+11io 0pf+0w (quiet.ss)
18.023av (quiet.ss)
    */
/*     bzero(retval, MPAGE_SIZE); */
    return retval;
  }

  mla_usedpages += numpages;
  if(mla_usedpages > MAX_USED_PAGES) {
    gc_collect(0);
    if(mla_usedpages > MAX_USED_PAGES) {
      gc_collect(1);
      if(mla_usedpages > MAX_USED_PAGES) {
	if(GC_out_of_memory) GC_out_of_memory();
	fprintf(stderr, "Out of memory\n");
	abort();
      }
    }
  }

  return malloc_pages(size, MPAGE_SIZE);
}

static void mla_free(void *page, size_t size, int bigpage) {
  if(bigpage) {
    unsigned long numpages = (size/MPAGE_SIZE)+(((size%MPAGE_SIZE)==0)?0:1);
    mla_usedpages -= numpages;
    free_pages(page, size);
  } else {
    struct mpage_list *temp = malloc(sizeof(struct mpage_list));
    temp->mpage = page;
    temp->next = mla_freedpages;
    mla_freedpages = temp;
  }
}

static unsigned long mla_memfree(void) {
  return (MAX_USED_PAGES - mla_usedpages) * MPAGE_SIZE;
}

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

struct objhead {
  unsigned int owner : (BITS_PER_WORD - (5 + OBJ_LOG_MAX_SIZE));
  unsigned int type : 3;
  unsigned int mark : 1;
  unsigned int markf : 1;
  unsigned int size : OBJ_LOG_MAX_SIZE;
};

char zero_sized[4];

#define allocate(t) { \
  size_t size_in_words = gcBYTES_TO_WORDS(size_in_bytes) + 1;\
  void *retval = tla_regions; \
  if(size_in_words == 1) return zero_sized; \
  if(size_in_words >= OBJ_MAX_WORD_SIZE) \
    return bpage_malloc(gcWORDS_TO_BYTES(size_in_words), t, 0); \
  tla_regions += size_in_words; \
  if(tla_regions >= tla_regione) { \
    tla_regions -= size_in_words; \
    gc_collect(0); \
    retval = tla_regions;\
    tla_regions += size_in_words; \
  }\
  ((struct objhead *)retval)->type = t; \
  ((struct objhead *)retval)->size = size_in_words; \
  return (void*)((unsigned long)retval + 4); }

void *GC_malloc(size_t size_in_bytes) { allocate(MPAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t size_in_bytes) { allocate(MPAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t size_in_bytes) { allocate(MPAGE_XTAGGED); }
void *GC_malloc_array_tagged(size_t size_in_bytes) { allocate(MPAGE_TARRAY); }
void *GC_malloc_atomic(size_t size_in_bytes) { allocate(MPAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t size) { return malloc(size); }

#define tla_member_p(p) (((unsigned long)p >= (unsigned long)tla_heap) && ((unsigned long)p < (unsigned long)tla_regions))

/*****************************************************************************
 * Memory allocation (gclevel)
 *****************************************************************************/

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

static void *copy_bits(void *oldptr, unsigned long sizew, int type, int tgen) {
  unsigned long sizeb = gcWORDS_TO_BYTES(sizew);
  struct mpage *work = gen[tgen][type];
  void *retval;

  /* Timing benefits:
     This loop searches the entire list to find if there's a place to
     put the object:
12.884u 1.590s 0:18.56 77.9%    908+12225k 0+11io 96pf+0w
13.013u 1.456s 0:18.63 77.6%    910+12260k 0+11io 0pf+0w
13.143u 1.466s 0:18.77 77.7%    912+12286k 0+11io 0pf+0w
     This loop simply looks at the first page.
12.887u 1.637s 0:18.59 78.0%    912+12285k 0+11io 96pf+0w
12.678u 1.832s 0:18.76 77.2%    919+12373k 0+11io 0pf+0w
12.835u 1.633s 0:18.67 77.4%    913+12298k 0+11io 0pf+0w
  */

/*   while(work && ((work->size + sizeb) >= MPAGE_SIZE))  */
/*     work = work->next;  */

  if(work && ((work->size + sizeb) < MPAGE_SIZE)) {
/*   if(work) {  */
/* Timing benefits:
   Custom copy:
13.082u 1.509s 0:18.54 78.6%    910+12251k 0+11io 0pf+0w
12.966u 1.523s 0:18.44 78.5%    906+12196k 0+11io 0pf+0w
12.860u 1.557s 0:18.44 78.1%    906+12196k 0+11io 0pf+0w
   LibC MemCpy:
13.094u 1.384s 0:18.51 78.1%    910+12247k 0+11io 96pf+0w
13.225u 1.309s 0:18.56 78.2%    909+12237k 0+11io 0pf+0w
13.050u 1.428s 0:18.43 78.5%    906+12199k 0+11io 0pf+0w
*/
    void **startfrom = oldptr, **startto, **endfrom; 
    startto =  retval = (void*)((unsigned long)work + work->size);
    endfrom = startfrom + sizew; 
    while(startfrom < endfrom) *(startto++) = *(startfrom++); 
/*     memcpy(retval, oldptr, sizeb); */
    work->size += sizeb;
    return retval;
  } else {
    void **startfrom = oldptr, **startto, **endfrom; 
    work = (struct mpage *)mla_malloc(MPAGE_SIZE);
    GCDBG("Allocated pages %p\n", work);
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
/*     memcpy(retval, oldptr, sizeb); */
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

static void pmap_add(void *page) {
  struct mpage *mpage = (struct mpage *)page;
  long size_left = mpage->bigpage ? mpage->size : MPAGE_SIZE;

  while(size_left > 0) {
    pmap[FLINDEX(page)][SLINDEX(page)] = mpage;
    size_left -= MPAGE_SIZE;
    page += MPAGE_SIZE;
  }
}

static void pmap_remove(void *page) {
  struct mpage *mpage = (struct mpage *)page;
  long size_left = mpage->bigpage ? mpage->size : MPAGE_SIZE;
  
  GCDBG("Removing page %p (bpage %i, size %i)\n", 
	mpage, mpage->bigpage, mpage->size);
  while(size_left > 0) {
    pmap[FLINDEX(page)][SLINDEX(page)] = NULL;
    size_left -= MPAGE_SIZE;
    page += MPAGE_SIZE;
  }
}

#define pmap_find(p)		(pmap[FLINDEX(p)][SLINDEX(p)])

/*****************************************************************************
 * Big page routines 
 *****************************************************************************/

static unsigned long bpage_g0size = 0; 

static void *bpage_malloc(size_t sizeb, int type, int tgen) {
  unsigned long sizew = gcBYTES_TO_WORDS(sizeb) + 1;
  struct mpage *bpage;

  /* without this there is a slight flaw in which if someone only
     allocates bigpages, they can completely avoid collection. bad! */
  if( (bpage_g0size + ((unsigned long)tla_regions - (unsigned long)tla_heap))
      > GEN0_SIZE)
    gc_collect(0);
    
  sizeb = gcWORDS_TO_BYTES(sizew);
  bpage_g0size += sizeb;
  bpage = mla_malloc(PAGE_HEADER_SIZE + sizeb);
  GCDBG("Allocating bigpage %p of size %li, type %i, gen %i\n",
	bpage, sizeb, type, tgen);
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

void *GC_malloc_allow_interior(size_t size) {
  return bpage_malloc(size, MPAGE_ARRAY, 0);
}

/*****************************************************************************
 * Memory accounting 
 *****************************************************************************/
#define RQ_TYPE_ACCNT		2

struct accnthook {
  short type;
  short owner;
  unsigned long bytes;
  Scheme_Custodian *cust;
  Scheme_Custodian *cust_to_kill;
  struct accnthook *next;
};

static struct accnthook *accnthooks = NULL;
static unsigned long accnt_requires = 0;

long GC_get_memory_use(void *c) {
  if(!c) {
    unsigned long retval = GEN0_SIZE;
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

static struct owner_list *accnt_get_owners(struct owner_list *ol) {
  struct accnthook *work;

  for(work = accnthooks; work; work = work->next)
    if(!ol_member_p(ol, work->owner))
      ol = ol_add(ol, work->owner);
  return ol;
}

static void accnt_mark(short owner) {
  struct accnthook *work;
  
  for(work = accnthooks; work; work = work->next)
    if(work->owner == owner) gcMARK(work->cust_to_kill);
}

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
    }
  }
}

/*****************************************************************************
 * Weak box structure and functions
 *****************************************************************************/
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

int size_weak_box(void *p) { 
  return gcBYTES_TO_WORDS(sizeof(struct weakbox)); 
}

int mark_weak_box(void *p) {
  struct weakbox *wb = (struct weakbox *)p;
  gcMARK(wb->secondary_erase);
  if(wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }
  return gcBYTES_TO_WORDS(sizeof(struct weakbox));
}

int fixup_weak_box(void *p) {
  struct weakbox *wb = (struct weakbox *)p;
  GCDBG("Fixing up weak box\n");
  gcFIXUP(wb->secondary_erase);
  if(!mark_p(wb->val)) {
    wb->val = NULL;
    if(wb->secondary_erase)
      *(wb->secondary_erase + wb->soffset) = NULL;
    wb->secondary_erase = NULL;
  } else gcFIXUP(wb->val);
  return gcBYTES_TO_WORDS(sizeof(struct weakbox));
}

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

struct weakarray {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct weakarray *next;
  void *data[1];
};

static struct weakarray *weak_arrays = NULL;

int size_weak_array(void *p) {
  struct weakarray *wa = (struct weakarray *)p;
  return gcBYTES_TO_WORDS(sizeof(struct weakarray) +
			  ((wa->count - 1) * sizeof(void *)));
}

int mark_weak_array(void *p) {
  struct weakarray *wa = (struct weakarray *)p;
  gcMARK(wa->replace_val);
  wa->next = weak_arrays;
  weak_arrays = wa;
  return gcBYTES_TO_WORDS(sizeof(struct weakarray) +
			  ((wa->count - 1) * sizeof(void *)));
}

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
struct immobile {
  void *p;
  short owner;
  struct immobile *next;
};

static struct immobile *immobiles = NULL;

static struct owner_list *imm_get_owners(struct owner_list *ol) {
  struct immobile *work;

  for(work = immobiles; work; work = work->next) 
    if(!ol_member_p(ol, work->owner))
      ol = ol_add(ol, work->owner);
  return ol;
}

static void imm_mark(short owner) {
  struct immobile *work;
  for(work = immobiles; work; work = work->next)
    if(work->owner == owner) gcMARK(work->p);
}

static void imm_fixup(void) {
  struct immobile *work;
  for(work = immobiles; work; work = work->next)
    gcFIXUP(work->p);
}

void **GC_malloc_immobile_box(void *p) {
  struct immobile *ib = malloc(sizeof(struct immobile));
  ib->p = p;
  ib->owner = ot_current();
  ib->next = immobiles;
  immobiles = ib;
  return (void**)ib;
}

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
 * Finalization
 *****************************************************************************/
#define RQ_TYPE_FINAL		1		

struct fnl {
  char eager_level;
  char tagged;
  void *p;
  void (*f)(void *p, void *data);
  void *data;
  short owner;
  struct fnl *next;
};

struct weakfnl {
  void *p;
  int offset;
  void *saved;
  struct weakfnl *next;
};

static struct fnl *fnls = NULL;
static struct weakfnl *weakfnls = NULL;

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
      fnl->owner = ot_current();
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
  fnl = malloc(sizeof(struct fnl));
  fnl->next = fnls;
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;
  fnl->tagged = tagged;
  fnl->owner = ot_current();
  fnls = fnl;
}

void GC_finalization_weak_ptr(void **p, int offset) {
  struct weakfnl *wl;
  
  wl = malloc(sizeof(struct weakfnl));
  wl->p = p;
  wl->next = weakfnls;
  wl->offset = offset * sizeof(void*);
  weakfnls = wl;
}

static struct owner_list *fnl_get_owners(struct owner_list *ol) {
  struct fnl *fnl;

  for(fnl = fnls; fnl; fnl = fnl->next) 
    if(!ol_member_p(ol, fnl->owner)) 
      ol = ol_add(ol, fnl->owner);
  return ol;
}

static struct owner_list *wfnl_get_owners(struct owner_list *ol) {
  return ol;
}

static void fnl_mark(short owner) {
  struct fnl *fnl;

  for(fnl = fnls; fnl; fnl = fnl->next)
    if(fnl->owner == owner) {
      gcMARK(fnl->data);
      if(!mark_p(fnl)) {
	gcMARK(fnl->p);
	if( ((struct objhead *)((unsigned long)fnl->p - 4))->mark ) {
	  ((struct objhead *)((unsigned long)fnl->p - 4))->markf = 1;
	  ((struct objhead *)((unsigned long)fnl->p - 4))->mark = 0;
	}
      }
    }
}

static void wfnl_mark(short owner) {
  return;
}

static void fnl_fixup(void) {
  struct fnl *fnl, *prev;

  fnl = fnls; prev = NULL;
  while(fnl) {
    struct objhead *phead = (struct objhead *)((unsigned long)fnl->p - 4);
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->p);
    if(phead->markf && !phead->mark) {
      rq_add(fnl->eager_level, RQ_TYPE_FINAL, fnl);
      if(prev) prev->next = fnl->next;
      else fnls = fnl->next;
      fnl = fnl->next;
    } else { prev = fnl; fnl = fnl->next; }
  }
}

static void wfnl_fixup(void) {
  struct weakfnl *cur = weakfnls, *prev = NULL;
  
  while(cur) {
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

static void wfnl_reset_weaks() {
  struct weakfnl *cur;

  for(cur = weakfnls; cur; cur = cur->next) {
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
	free(f);
	free(work);
      } else {
	struct accnthook *ah = (struct accnthook *)work->obj;
/* 	printf("Scheduling custodian for death\n"); */
	scheme_schedule_custodian_close(ah->cust_to_kill);
	free(ah);
	free(work);
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
static bool atomic_table[_num_tags];

void **GC_variable_stack;
void *stack_base;

void GC_set_stack_base(void *base) {
  stack_base = base;
}

unsigned long GC_get_stack_base(void) {
  return (unsigned long)stack_base;
}

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, 
			    Fixup_Proc fixup, int is_constant_size, 
			    int is_atomic) {
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
  atomic_table[tag] = is_atomic;
}

void GC_init_type_tags(int count, int weakbox) {
  static int initialized = 0;

  if(!initialized) {
    ot_table = malloc(OT_INIT_SIZE * sizeof(struct otentry *));
    tla_heap = tla_regions = malloc(GEN0_SIZE);
    tla_regione = (void**)((unsigned long)tla_regions + GEN0_SIZE);
    GC_register_traversers(weakbox, size_weak_box, mark_weak_box,
			   fixup_weak_box, 0, 0);
    GC_register_traversers(gc_weak_array_tag, size_weak_array, mark_weak_array,
			   fixup_weak_array, 0, 0);
    old_init();
    GC_add_roots(&weakfnls, (char*)&weakfnls + sizeof(weakfnls) + 1);
#ifdef NEWGC_DEBUG
    debug = fopen("log", "w");
#endif
    initialized = 1;
  }
  weak_box_tag = weakbox;
}

/*****************************************************************************
 * Marking routines
 *****************************************************************************/
static bool mark_p(void *p) {
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

/* static bool mark_pure_p(void *p) { */
/*   if(!p) return false; */
/*   if(tla_member_p(p)) { */
/*     return ((struct objhead *)((unsigned long)p - 4))->mark; */
/*   } else { */
/*     struct mpage *page = pmap_find(p); */
/*     if(page->gen > gc_topgen) return true; */
/*     return ((struct objhead *)((unsigned long)p - 4))->mark; */
/*   } */
/* } */

static bool mark_final_p(void *p) {
  if(!p) return false;
  if(tla_member_p(p)) {
    return ((struct objhead *)((unsigned long)p - 4))->markf;
  } else {
    struct mpage *page = pmap_find(p);
    if(page->gen > gc_topgen) return true;
    return ((struct objhead *)((unsigned long)p - 4))->markf;
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
  bool old_gc_markp = gc_markprecise;
  /*   printf("precise_reprop(%p, %p, %i)\n", p, page, owner); fflush(stdout); */
  gc_markprecise = true;
  gc_owner = owner;
  if(page->bigpage) {
    ((struct objhead *)((unsigned long)page+PAGE_HEADER_SIZE))->owner = owner;
    mark_propbig(page);
  } else {
    ((struct objhead *)((unsigned long)p - 4))->owner = owner;
    switch(page->type) {
      case MPAGE_TAGGED: mark_table[*(Type_Tag*)p](p); break;
      case MPAGE_ATOMIC: break;
      case MPAGE_ARRAY: {
	unsigned long size = ((struct objhead *)((unsigned long)p - 4))->size;
	void **start = (void**)p;
	while(--size) GC_mark(*(start++));
	break;
      }
      case MPAGE_TARRAY: {
	Type_Tag tag = *(Type_Tag*)p;
	void **start = (void**)p;
	void **end = start + ((struct objhead *)((unsigned long)p - 4))->size;
	while(start < end) start += mark_table[tag](start);
	break;
      }
      case MPAGE_XTAGGED: GC_mark_xtagged(p);
    }
  }
  gc_markprecise = old_gc_markp;
}

static void precise_mark(const void *p) {
  /* mark_p() isn't acceptable here because it returns true for objects
     which are in a generation higher than we're dealing with, and we
     don't want to chase pointers in high generations */
  if(!tla_member_p(p) && !pmap_find(p)) return;
  if(((struct objhead *)((unsigned long)p - 4))->mark 
     ||((struct objhead *)((unsigned long)p - 4))->markf) {
    struct mpage *ppage = pmap_find(p);
    if(ppage && ppage->bigpage) {
      struct objhead *info = 
	(struct objhead *)((unsigned long)ppage + PAGE_HEADER_SIZE);
      if(info->owner != gc_owner) {
	info->owner = ot_union(info->owner, gc_owner);
	precise_reprop(ppage, ppage, info->owner);
      }
    } else {
      void *newp = *(void**)p;
      struct mpage *page = pmap_find(newp);
      struct objhead *info = (struct objhead *)((unsigned long)newp - 4);
      if(info->owner != gc_owner) {
	info->owner = ot_union(info->owner, gc_owner);
	precise_reprop(newp, page, info->owner);
      }
    }
  }
}
#endif

void GC_mark(const void *p) {
  if(!p || ((unsigned long)p & 0x1)) return;
#ifdef NEWGC_PRECISE
  if(gc_markprecise) { precise_mark(p); return; }
#endif
  if(tla_member_p(p)) {
    if(!mark_p((void*)p)) {
      void *startobj = (void*)((unsigned long)p - 4);
      struct objhead *ohead = (struct objhead *)startobj;
      unsigned long size = ohead->size;
      unsigned short type = ohead->type;
      void *newplace;
      
      if(type == MPAGE_TAGGED) {
	if(atomic_table[*(Type_Tag*)p]) {
	  type = MPAGE_ATOMIC;
	}
      }

      newplace = copy_bits(startobj, size, type, 1);
      GCDBG("MARK: moved %p to %p, size %li, type %i, gen 1\n",
	    p, newplace, size, type);
      ohead->mark = 1;
      *(void**)p = (void*)((unsigned long)newplace + 4);
#ifdef NEWGC_ACCNT
      ohead = (struct objhead *)newplace;
      ohead->owner = gc_owner;
      ot_accnt(gc_owner, 1, gcWORDS_TO_BYTES(size));
#endif
    } else if(mark_final_p((void*)p)) {
      ((struct objhead *)((unsigned long)p - 4))->mark = 1;
#ifdef NEWGC_ACCNT
      {
	void *newp = *(void**)p;
	struct objhead *newinfo = (struct objhead *)((unsigned long)newp - 4);
	if(newinfo->owner != gc_owner) {
	  newinfo->owner = ot_union(newinfo->owner, gc_owner);
# ifdef NEWGC_PRECISE
	  {
	    struct mpage *page = pmap_find(newp);
	    if(newp < (void*)((unsigned long)page + page->psize)) {
	      precise_reprop(newp, page, newinfo->owner);
	    }
	  }
# endif	  
	}
      }
#endif
    }
#ifdef NEWGC_ACCNT
    else {
      void *newp = *(void**)p;
      struct objhead *newinfo = (struct objhead *)((unsigned long)newp - 4);
      if(newinfo->owner != gc_owner) {
	newinfo->owner = ot_union(newinfo->owner, gc_owner);
# ifdef NEWGC_PRECISE
	{
	  struct mpage *page = pmap_find(newp);
	  if(newp < (void*)((unsigned long)page + page->psize)) {
	    precise_reprop(newp, page, newinfo->owner);
	  }
	}
# endif
      }
    }
#endif
  } else {
    struct mpage *page = pmap_find(p);
    if(!page) return;
    if(page->gen > gc_topgen) return;
    if(page->bigpage) {
      GCDBG("Marking big page %p (page->bigpage = %i)\n", page, page->bigpage);
      if(page->bigpage == 1) {
#ifdef NEWGC_ACCNT
	((struct objhead *)((unsigned long)page + PAGE_HEADER_SIZE))->owner
	  = gc_owner;
	ot_accnt(gc_owner, INCGEN(page->gen), page->size);
#endif
	page->bigpage++;
	mark_propbig(page);
      }
#ifdef NEWGC_ACCNT
      else {
	struct objhead *info = 
	  (struct objhead *)((unsigned long)page + PAGE_HEADER_SIZE);
	if(info->owner != gc_owner) {
	  info->owner = ot_union(info->owner, gc_owner);
# ifdef NEWGC_PRECISE
	  precise_reprop(page, page, info->owner);
# endif
	}
      }
#endif
    } else {
      if(!mark_p((void*)p)) {
	void *startobj = (void*)((unsigned long)p - 4);
	struct objhead *ohead = (struct objhead *)startobj;
	unsigned long size = ohead->size;
	unsigned short type = ohead->type;
	void *newplace = copy_bits(startobj, size, type, INCGEN(page->gen));
	GCDBG("MARK: moved %p to %p, size %li, type %i, gen %i\n",
	      p, newplace, size, type, INCGEN(page->gen));
	ohead->mark = 1;
	*(void**)p = (void*)((unsigned long)newplace + 4);
#ifdef NEWGC_ACCNT
	ohead = (struct objhead *)newplace;
	ohead->owner = gc_owner;
	ot_accnt(gc_owner, INCGEN(page->gen), gcWORDS_TO_BYTES(size));
#endif
      } else if(mark_final_p((void*)p)) {
	((struct objhead *)((unsigned long)p - 4))->mark = 1;
#ifdef NEWGC_ACCNT
	{
	  void *newp = *(void**)p;
	  struct objhead *newinfo = (struct objhead *)((unsigned long)newp-4);
	  if(newinfo->owner != gc_owner) {
	    newinfo->owner = ot_union(newinfo->owner, gc_owner);
# ifdef NEWGC_PRECISE
	    {
	      struct mpage *page = pmap_find(newp);
	      if(newp < (void*)((unsigned long)page + page->psize))
		precise_reprop(newp, page, newinfo->owner);
	    }
# endif
	  }
	}
#endif
      }
#ifdef NEWGC_ACCNT
      else {
	void *newp = *(void**)p;
	struct objhead *newinfo = (struct objhead *)((unsigned long)newp - 4);
	if(newinfo->owner != gc_owner) {
	  newinfo->owner = ot_union(newinfo->owner, gc_owner);
# ifdef NEWGC_PRECISE
	  {
	    struct mpage *page = pmap_find(newp);
	    if(newp < (void*)((unsigned long)page + page->psize))
	      precise_reprop(newp, page, newinfo->owner);
	  }
# endif
	}
      }
#endif
    }
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
#ifdef NEWGC_ACCNT
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
    GCDBG("Starting propogation loop\n");
    for(i = 1; i <= topgenp1; i++) {
      GCDBG("Propogating from generation %i\n", i);
      for(j = 0; j < MPAGE_TYPES; j++) {
	GCDBG("Propogating from type %i\n", j);
	for(page = gen[i][j]; page && (page->size != page->psize); page = page->next) {
	  GCDBG("Deciding whether to propogate %p\n", page);
	  if(page->psize != page->size) {
	    GCDBG("Propogating page %p\n", page);
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

void GC_mark_variable_stack(void **passed_stack,
			    long delta,
			    void *limit)
{
  void **var_stack = passed_stack;
  long size, count;
  void ***p, **a;

  GCDBG("Marking variable stack %p\n", var_stack);
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
	  GCDBG("Marking stack item %p\n", *a);
	  gcMARK(*a);
	  a++;
	}
      } else {
	a = (void **)((char *)a + delta);
	GCDBG("Marking stack item2 %p\n", *a);
	gcMARK(*a);
      }
      p++;
    }

    var_stack = *var_stack;
  }
  GCDBG("Done marking stack\n");
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
  struct mpage *ppage;
  short gen = 1;

  if(!p || ((long)p & 0x1)) return;
  if(tla_member_p(p)) {
/*     struct objhead *info = (struct objhead *)((unsigned long)p - 4); */
/*     if(info->mark || info->markf) { */
      *(void**)pp = *(void**)p;
/*     } else { printf("fixup of unmarked tla!\n"); abort(); } */
  } else {
    struct mpage *page = pmap_find(p);
    if(page) {
      if(page->bigpage) gen = page->gen; else {
	struct objhead *info = (struct objhead *)((unsigned long)p - 4);
	if(info->mark || info->markf) {
	  void **newloc = *(void**)p;
	  struct mpage *newpage = pmap_find(newloc);
	  *(void**)pp = newloc;
	  if(!newpage) return; /* happens from finalizers */
	  gen = newpage->gen;
	}
      }
    } else return;
  }
  
  ppage = pmap_find(pp);
  if(ppage && (ppage->gen > gen)) ppage->bpointers = 1;
}

static void fixup_bpage(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE + 4);
  void **end = (void**)((unsigned long)page + page->size);

  GCDBG("Fixing up bigpage %p, type %i. start = %p, end = %p\n",
	page, page->type, start, end);
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
    start++;
    GCDBG("Fixing tagged (%i) item %p\n", *(Type_Tag*)start, start);
    start += fixup_table[*(Type_Tag*)start](start);
  }
}

static void fixup_array(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_HEADER_SIZE);
  void **end = (void**)((unsigned long)page + page->size);
  
  while(start < end) {
    struct objhead *ohead = (struct objhead *)start++;
    unsigned long size = ohead->size;
    GCDBG("Fixing up array item %p\n", start);
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
    GCDBG("Fixing up tagged (%i) array %p\n", tag, start);
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
  GCDBG("gc_page list length = %li\n", len);
}

static void gc_prepare(void) {
  short i, j;

  gc_pages = NULL;
  for(i = 0; i <= gc_topgen; i++) 
    for(j = 0; j < MPAGE_TYPES; j++) {
      struct mpage *work = gen[i][j];
      while(work) {
	struct mpage *next = work->next;
	if(work->bigpage)
	  GCDBG("Adding bpage %p to list\n", work);
	protect_pages(work, work->size, 1);
	work->next = gc_pages;
	gc_pages = work;
	work = next;
      }
      gen[i][j] = NULL;
    }
  gc_list_len();
  ot_collectprep();
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
/*   bzero(tla_heap, (unsigned long)tla_regions - (unsigned long)tla_heap); */
  bpage_g0size = 0;
  tla_regions = tla_heap;
  old_protect();
  flush_freed_pages();
}

#ifdef NEWGC_DEBUG
static void debug_dump_heap(void) {
  struct mpage *page;
  struct root *root;
  void **start = tla_heap;
  void **end = tla_regions;
  short i, j;

  GCDBG("Dumped heap for collection %li, cycle %i, topgen %i\n",
	gc_numcollects, gc_cycle, gc_topgen);

  for(root = roots; root; root = root->next)
    GCDBG("%p - %p (owner %i)\n", root->start, root->end, root->owner);
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
    short curown = ot_current();
    struct owner_list *ol = ol_add(NULL, curown);

    gc_runcycle(force_full);
/*     printf("Running collection %li (topgen = %i)\n", gc_numcollects,  */
/* 	   gc_topgen); */
    if(GC_collect_start_callback) GC_collect_start_callback();

#ifdef NEWGC_DEBUG
    debug_dump_heap();
#endif
    gc_prepare();
    /* compute the owner list */
    ol = roots_get_owners(ol);
    ol = accnt_get_owners(ol);
    ol = imm_get_owners(ol);
    ol = fnl_get_owners(ol);
    ol = wfnl_get_owners(ol);
    /* sort the owner list */
    ol = ol_sort(ol);

    if(gc_topgen == 2) wfnl_clear_weaks();
    /* mark the roots from the owner list */
    while(ol) {
      struct owner_list *next = ol->next;
      GCDBG("Owner %i\n", ol->owner);

#ifdef NEWGC_ACCNT
      gc_owner = ol->owner;
#endif
      GCDBG("Marking roots\n"); roots_mark(ol->owner);
      GCDBG("Marking accnt\n"); accnt_mark(ol->owner);
      GCDBG("Marking immobiles\n"); imm_mark(ol->owner);
      GCDBG("Marking finals\n"); fnl_mark(ol->owner);
      GCDBG("Marking weak finals\n"); wfnl_mark(ol->owner);
      GCDBG("Marking stack\n");
      if(ol->owner == curown)
	GC_mark_variable_stack(GC_variable_stack, 0,
			       (void*)(GC_get_thread_stack_base
				       ? GC_get_thread_stack_base()

				       : (unsigned long)stack_base));
      free(ol);
      ol = next;
    }
    GCDBG("Marking old pointers\n"); old_mark();

    /* propogate */
    GCDBG("Propogating marks\n"); mark_propogate();
    if(gc_topgen == 2) {
      wfnl_reset_weaks();
      mark_propogate();
    }

    /* repair */
    GCDBG("Fixing up owners\n"); ot_fixup();
    GCDBG("Fixing up roots\n"); roots_fixup();
    /* accnt_fixup must be after ot_fixup */
    GCDBG("Fixing up accnt stuff\n"); accnt_fixup();
    GCDBG("Fixing up immobiles\n"); imm_fixup();
    GCDBG("Fixing up finals\n"); fnl_fixup();
    GCDBG("Fixing up weak finals\n"); wfnl_fixup();
    GCDBG("Fixing up stack\n");
    GC_fixup_variable_stack(GC_variable_stack, 0,
			    (void*)(GC_get_thread_stack_base
				    ? GC_get_thread_stack_base()
				    : (unsigned long)stack_base));
    GCDBG("Fixing up heap\n"); fixup_heap();
    GCDBG("Cleaning up\n");gc_cleanup();

    if(GC_collect_start_callback) GC_collect_end_callback();
    rq_run();
  }
}

void GC_gcollect() { gc_collect(1); }

/*****************************************************************************
 * Memory allocation (system level)
 *****************************************************************************/
#if _WIN32
static void *malloc_pages(size_t len, size_t align) {
  return VirtualAlloc(NULL, len, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
}

static void *free_pages(void *p, size_t len) {
  VirtualFree(p, 0, MEM_RELEASE);
}

static void flush_freed_pages(void) {}

static void protect_pages(void *p, size_t len, int rw) {
  DWORD old;
  VirtualProtect(p, len, rw ? PAGE_READWRITE : PAGE_READONLY, &old);
}
#else
# include <unistd.h>
# include <fcntl.h>
# include <sys/types.h>
# include <sys/mman.h>
# include <errno.h>

#ifndef MAP_ANON
int fd, fd_created;
#endif

int page_size; /* OS page size */

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
void *blockfree_ptrs[BLOCKFREE_CACHE_SIZE];
long blockfree_sizes[BLOCKFREE_CACHE_SIZE];

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
  if(!writeable)
    GCDBG("Protecting %p through %p\n", p, p + len);
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
  unsigned long own_memuse = 0;
  unsigned short num_owners = 0;
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
  own_memuse = ot_top * sizeof(struct otentry *);
  fprintf(stderr, "Tracking %i owners\n", num_owners);
  fprintf(stderr, "   ... owner table top = %i\n", ot_top);
  fprintf(stderr, "   ... using %li bytes in owner system\n", own_memuse);
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
