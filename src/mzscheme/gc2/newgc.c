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

/*****************************************************************************/
/* Constant declarations. These can maybe be changed without destroying the  */
/* system, but I wouldn't bet on it. So change AYOR.                         */
/*****************************************************************************/
#define GENERATIONS 		3
#define GEN0_SIZE		(16 * 1024 * 1024)
#define OWNER_TABLE_INIT_SIZE   5
#define OWNER_TABLE_GROW_AMT    10
#define SET_OF_ROOTS_GROW_AMT   5
#define LOG_WORD_SIZE           2
#define LOG_MPAGE_SIZE		14

#ifdef USE_ACCT_3M_GC
#define NEWGC_ACCNT
#endif

#ifdef USE_PRECISE_ACCT_3M_GC
#define NEWGC_ACCNT
#define NEWGC_PRECISE
#endif

/*****************************************************************************/
/* Global structure definitions                                              */
/*****************************************************************************/
#ifdef NEWGC_ACCNT
#define OWNER_FIELD unsigned short owner;
#else
#define OWNER_FIELD /* */
#endif


/* this is the structure of the header for every object we allocate */
struct objhead {
  unsigned int		owner : ((8 << LOG_WORD_SIZE) - (5 + LOG_MPAGE_SIZE));
  unsigned int 		type : 3;
  unsigned int          mark : 1;
  unsigned int          mark_fnl : 1;
  unsigned int          size : LOG_MPAGE_SIZE;
};

#ifdef NEWGC_ACCNT
struct cust_list {
  Scheme_Custodian     *custodian;
  struct cust_list     *next;
};

struct union_list {
  unsigned short 	other_owner;
  unsigned short	result;
  struct union_list    *next;
};

struct owner_set {
  struct cust_list     *members;
  struct union_list    *unions;
  long 			memory_use;
};

struct owner_list {
  unsigned short	owner;
  struct owner_list    *next;
};
#endif

struct root {
  unsigned long 	count;
  unsigned long 	size;
  unsigned long        *root;
  unsigned int 		nothing_new;
};

#ifdef NEWGC_ACCNT
struct thread_list {
  unsigned short	owner;
  Scheme_Thread	       *thread;
  struct thread_list   *next;
};

struct account_hook {
  short			type;
  unsigned long		bytes;
  Scheme_Custodian     *cust;
  Scheme_Custodian     *cust_to_kill;
  struct account_hook  *next;
};
#endif

struct weak_box {
  unsigned short 	type;
  short			keyex;
  void		       *val;
  void                **secondary_erase;
  int			soffset;
};

struct weak_array {
  unsigned short	type;
  short			keyex;
  long			count;
  void		       *replace_val;
  void		       *reserved; /* this is necessary */
  void 		       *data[1];
};

struct immobile_box {
  void 		       *p; /* this must be first or window.cc dies */
  struct immobile_box  *next;
  struct immobile_box  *prev;
  OWNER_FIELD
};

struct mpage {
  unsigned int		size;
  unsigned int          previous_size;
  unsigned char 	generation;
  unsigned char         back_pointers_p;
  unsigned char         big_page_p;
  unsigned char         page_type;
  struct mpage         *next;
  struct mpage         *prev;
};

struct finalizer {
  char			eager_level;
  char 			tagged;
  void		       *p;
  GC_finalization_proc  f;
  void		       *data;
  OWNER_FIELD
  struct finalizer     *next;
};

struct weak_finalizer {
  void		       *p;
  int			offset;
  void		       *saved;
  struct weak_finalizer *next;
};

/*****************************************************************************/
/* Derived constant defintions. Don't mess with these.                       */
/*****************************************************************************/
#define USEFUL_ADDR_BITS		((8 << LOG_WORD_SIZE) - LOG_MPAGE_SIZE)
#define ADDR_BITS(x)			(((unsigned long)x) >> LOG_MPAGE_SIZE)
#define MPAGE_SIZE			(1 << LOG_MPAGE_SIZE)
#define MAX_OBJECT_BYTE_SIZE		(MPAGE_SIZE - sizeof(struct mpage) - 4)
#define MAX_OBJECT_WORD_SIZE		gcBYTES_TO_WORDS(MAX_OBJECT_BYTE_SIZE)
#define PAGE_WORD_OVERHEAD		gcBYTES_TO_WORDS(sizeof(struct mpage))
#define PAGE_BYTE_OVERHEAD		gcWORDS_TO_BYTES(PAGE_WORD_OVERHEAD)
#define INCGEN(g)			((g == (GENERATIONS-1)) ? g : (g + 1))
#define MPAGE_TAGGED			0
#define MPAGE_ATOMIC			1
#define MPAGE_ARRAY			2
#define MPAGE_TARRAY			3
#define MPAGE_XTAGGED			4
#define MPAGE_BIG			5
#define MPAGE_TYPES			6
#define NUMBER_OF_TAGS			257

/*****************************************************************************/
/* Global variables. Probably shouldn't mess with these, either.             */
/*****************************************************************************/
static struct owner_set		      **owner_table = NULL;
static unsigned long			owner_table_top  = 0;
/*  */ void			      **GC_variable_stack;
static unsigned long			stack_base;
static struct root		      **roots = NULL;
static unsigned long			roots_top = 0;
static struct mpage		       *page_map[1 << USEFUL_ADDR_BITS];
static unsigned long			pages_in_heap = 0;
static unsigned long 			max_heap_size = 0;
static unsigned long			max_used_pages = 0;
static unsigned long		        used_pages = 0;
static char			       *zero_sized[4];
static void			      **gen0_heap = NULL;
static void			      **gen0_region_p = NULL;
static void 			      **gen0_region_end = NULL;
static unsigned long			gen0_bigpages_size = 0;
static unsigned short			weak_box_tag;
static unsigned short			weak_array_tag = 256;
static struct immobile_box	       *immobile_boxes = NULL;
static struct mpage		       *pages[GENERATIONS][MPAGE_TYPES];
static struct finalizer		       *finalizers = NULL;
static struct finalizer		       *run_queue = NULL;
static struct weak_finalizer           *weak_finalizers;
static Size_Proc			size_table[NUMBER_OF_TAGS];
static Mark_Proc			mark_table[NUMBER_OF_TAGS];
static Fixup_Proc			fixup_table[NUMBER_OF_TAGS];
static unsigned long			collection_number = 0;
static unsigned int			collection_cycle = 0;
static unsigned int			collection_top = 0;
static unsigned int			collection_full_p = 0;
static struct mpage 		       *collection_from_pages = NULL;
static void 			       *park[2];
#ifdef NEWGC_ACCNT
static unsigned short			collection_mark_owner = 0;
static struct thread_list	       *thread_list = NULL;
static struct account_hook	       *account_hooks = NULL;
static unsigned long			total_requires = 0;
static Mark_Proc			thread_marker;
#endif

/*****************************************************************************/
/* Local routines defined here. I got tired of trying to sort things so that */
/* I only had to include a minimal set here, so instead I've just given      */
/* forwards for every function we use.                                       */
/*****************************************************************************/

/* owner set functions */
#ifdef NEWGC_ACCNT
inline struct cust_list *cust_list_add(struct cust_list *, Scheme_Custodian *);
inline short cust_list_member_p(struct cust_list *, Scheme_Custodian *);
inline short cust_list_subset_p(struct cust_list *, struct cust_list *);
static unsigned short add_owner_set(Scheme_Custodian *);
#ifdef NEWGC_PRECISE
static unsigned short owner_set_union(unsigned short, unsigned short);
#endif
inline void owner_table_account(unsigned short, unsigned long);
inline unsigned short custodian_to_owner_set(Scheme_Custodian *);
inline void fixup_owner_table(void);
inline unsigned short current_owner(void);
inline struct owner_list *owner_list_add(struct owner_list *, unsigned short);
inline short owner_list_member_p(struct owner_list *, unsigned short);
static int owner_compare(const void *, const void *);
inline struct owner_list *sort_owner_list(struct owner_list *);
#else
#define current_owner() 0 
#endif

/* root functions */
static int compare_roots(const void *, const void *);
static void sort_and_merge_roots(struct root *);
inline struct owner_list *get_root_owners(struct owner_list *);
inline void mark_some_roots(unsigned short);
inline void mark_all_roots(void);
inline void fixup_all_roots(void);

/* thread list functions */
#ifdef NEWGC_ACCNT
inline void mark_some_threads(unsigned short);
inline void mark_all_threads(void);
inline struct owner_list *get_thread_owners(struct owner_list *);
inline void fixup_all_threads(void);
#endif

/* finalizer functions */
inline void mark_some_finalizers(unsigned short);
inline void mark_all_finalizers(void);
inline void reset_weak_finalizers(void);
inline struct owner_list *get_finalizer_owners(struct owner_list *);
inline void fixup_all_finalizers(void);

/* weak item functions */
static int size_weak_box(void *);
static int mark_weak_box(void *);
static int fixup_weak_box(void *);
static int size_weak_array(void *);
static int mark_weak_array(void *);
static int fixup_weak_array(void *);

/* immobile box functions */
inline void mark_some_immobiles(unsigned short);
inline void mark_all_immobiles(void);
inline struct owner_list *get_immobile_box_owners(struct owner_list *);
inline void fixup_all_immobiles(void);

/* account hook functions */
#ifdef NEWGC_ACCNT
inline void fixup_account_hooks(void);
inline void run_account_hooks(void);
#endif

/* allocation functions */
inline void *allocate(size_t, int);
static void *allocate_big(size_t, int);
inline void *copy_bits(void *, unsigned long, short, short);

/* page map functions */
inline void add_page_to_page_map(struct mpage *);
inline void remove_page_from_page_map(struct mpage *);
inline struct mpage *find_page(void *);

/* marking functions */
inline int marked_p(void *);
inline void mark_normal(struct mpage *, void *);
inline void mark_big_page(struct mpage *);
inline void mark_tagged_page(struct mpage *);
inline void mark_array_page(struct mpage *);
inline void mark_tarray_page(struct mpage *);
inline void mark_xtagged_page(struct mpage *);
inline void mark_older_pointers(void);
inline void propogate_all_marks(void);

/* fixup functions */
inline void fixup_big_page(struct mpage *);
inline void fixup_tagged_page(struct mpage *);
inline void fixup_array_page(struct mpage *);
inline void fixup_tarray_page(struct mpage *);
inline void fixup_xtagged_page(struct mpage *);
inline void fixup_heap(void);

/* others */
static void gc_collect(int);
inline void *malloc_pages(size_t, size_t);
static void free_pages(void *, size_t);
static void flush_freed_pages(void);
static void protect_pages(void *, size_t, int);
static unsigned long determine_max_heap_size(void);

/*****************************************************************************/
/* External routines we use and/or define here                               */
/*****************************************************************************/

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

/* Sun's qsort() is broken, so we may have to use our own */
#if defined(sparc) || defined(__sparc) || defined(__sparc__)
# include "my_qsort.c"
#else
# define my_qsort qsort
#endif

/*****************************************************************************/
/* Routines dealing with various runtime execution stacks                    */
/*****************************************************************************/

void GC_set_stack_base(void *base) {
  stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base() {
  return stack_base;
}

void GC_mark_variable_stack(void **var_stack, long delta, void *limit) {
  long size, count;
  void ***p, **a;


  if(park[0]) gcMARK(park[0]);
  if(park[1]) gcMARK(park[1]);
  while(var_stack) {
    var_stack = (void **)((char *)var_stack + delta);
    if(var_stack == limit) 
      return;

    size = *(long*)(var_stack + 1);
    p = (void***)(var_stack + 2);
    while(size--) {
      a = *p;
      if(!a) {
        count = ((long *)p)[2];
        a = ((void***)p)[1];
        p += 2; size -= 2; a = (void**)((char *)a + delta);
        while(count--) {
           gcMARK(*a);
           a++;
        }
      } else {
        a = (void**)((char *)a + delta);
        gcMARK(*a);
      }
      p++;
    }
    var_stack = *var_stack;
  }
}

void GC_fixup_variable_stack(void **var_stack, long delta, void *limit) {
  long size, count;
  void ***p, **a;

  if(park[0]) gcFIXUP(park[0]);
  if(park[1]) gcFIXUP(park[1]);
  while(var_stack) {
    var_stack = (void**)((char *)var_stack + delta);
    if(var_stack == limit)
      return;
    
    size = *(long*)(var_stack + 1);
    p = (void***)(var_stack + 2);
    while(size--) {
      a = *p;
      if(!a) {
        count = ((long *)p)[2]; a = ((void***)p)[1];
        p += 2; size -= 2; a = (void**)((char *)a + delta);
        while(count--) {
          gcFIXUP(*a);
          a++;
        }
      } else {
        a = (void**)((char *)a + delta);
        gcFIXUP(*a);
      }
      p++;
    }
    var_stack = *var_stack;
  }
} 

/*****************************************************************************/
/* Routines dealing with the owner table; none of these are defined if we're */
/* not doing accounting, of course                                           */
/*****************************************************************************/
#ifdef NEWGC_ACCNT

inline struct cust_list *cust_list_add(struct cust_list *clist,
				       Scheme_Custodian *cust) {
  struct cust_list *retval = malloc(sizeof(struct cust_list));
  retval->custodian = cust;
  retval->next = clist;
  return retval;
}

inline short cust_list_member_p(struct cust_list *clist, 
				Scheme_Custodian *cust) {
  for(; clist; clist = clist->next) 
    if(clist->custodian == cust)
      return 1;
  return 0;
}

inline short cust_list_subset_p(struct cust_list *list1, 
				struct cust_list *list2) {
  for(; list1; list1 = list1->next)
    if(!cust_list_member_p(list2, list1->custodian))
      return 0;
  return 1;
}

inline void owner_table_account(unsigned short owner, unsigned long size) {
  if(collection_full_p) {
    owner_table[owner]->memory_use += size;
  }
}

static unsigned short add_owner_set(Scheme_Custodian *cust) {
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *temp = cust;
  unsigned short i;

  for(i = 0; i < owner_table_top; i++)
    if(!owner_table[i]) {
      owner_table[i] = malloc(sizeof(struct owner_set));
      owner_table[i]->unions = NULL;
      owner_table[i]->memory_use = 0;
      owner_table[i]->members = NULL;

      do {
        box = temp->parent;
        temp = box ? box->u.two_ptr_val.ptr1 : NULL;
        if(temp) owner_table[i]->members = 
                   cust_list_add(owner_table[i]->members, temp);
      } while(temp);
      owner_table[i]->members = cust_list_add(owner_table[i]->members, cust);
      return i;
    }

  if(owner_table_top == (2 << ((8 << LOG_WORD_SIZE) - (6 + LOG_MPAGE_SIZE)))) {
    fprintf(stderr, "Out of entries in the owner table\n");
    fprintf(stderr, "Accounting is going to be invalid for awhile\n");
    return 0;
  }

  owner_table_top += OWNER_TABLE_GROW_AMT;
  if(owner_table_top > (2 << ((8 << LOG_WORD_SIZE) - (6 + LOG_MPAGE_SIZE))))
    owner_table_top = (2 << ((8 << LOG_WORD_SIZE) - (6 + LOG_MPAGE_SIZE)));
  owner_table = realloc(owner_table, owner_table_top * 
                                     sizeof(struct owner_set *));
  bzero( (void*)((unsigned long)owner_table
                 + ((owner_table_top - OWNER_TABLE_GROW_AMT)
                    * sizeof(struct owner_set *))),
         OWNER_TABLE_GROW_AMT * sizeof(struct owner_set *));
  return add_owner_set(cust);
}

#ifdef NEWGC_PRECISE
static unsigned short owner_set_union(unsigned short set1, 
				      unsigned short set2) {
  struct union_list *ulist;
  short res;

  /* put the two in a standard (though arbitrary) order */
  if(set1 > set2) {
    short temp = set2;
    set2 = set1;
    set1 = temp;
  }

  /* see if we've already done this one */
  for(ulist = owner_table[set1]->unions; ulist; ulist = ulist->next)
    if(ulist->other_owner == set2)
      return ulist->result;

  /* we've never seen these before */
  if(cust_list_subset_p(owner_table[set1]->members, 
			owner_table[set2]->members)) {
    res = set2;
  } else if(cust_list_subset_p(owner_table[set2]->members,
			       owner_table[set1]->members)) {
    res = set1;
  } else {
    short done = 0;

    do {
      for(res = 0; res < owner_table_top; res++) {
	if(!owner_table[res]) {
	  struct cust_list *custs;

	  done = 1;
	  owner_table[res] = malloc(sizeof(struct owner_set));
	  owner_table[res]->members = NULL;
	  owner_table[res]->unions = NULL;
	  owner_table[res]->memory_use = 0;
	  for(custs = owner_table[set1]->members; custs; custs = custs->next)
	    if(!cust_list_member_p(owner_table[res]->members,
				   custs->custodian))
	      owner_table[res]->members = 
		cust_list_add(owner_table[res]->members,
			      custs->custodian);
	  for(custs = owner_table[set2]->members; custs; custs = custs->next)
	    if(!cust_list_member_p(owner_table[res]->members,
				   custs->custodian))
	      owner_table[res]->members = 
		cust_list_add(owner_table[res]->members,
			      custs->custodian);
	  break;
	}
      }
      
      if(!done) {
	owner_table_top += OWNER_TABLE_GROW_AMT;
	owner_table = realloc(owner_table, owner_table_top *
			                   sizeof(struct owner_set *));
	bzero( (void*)((unsigned long)owner_table +
		       ((owner_table_top - OWNER_TABLE_GROW_AMT)
			* sizeof(struct owner_set *))),
	       OWNER_TABLE_GROW_AMT * sizeof(struct owner_set *));
      }
    } while(!done);
  }

  ulist = malloc(sizeof(struct union_list));
  ulist->other_owner = set2;
  ulist->result = res;
  ulist->next = owner_table[set1]->unions;
  owner_table[set1]->unions = ulist;
  return res;
}
#endif

inline unsigned short custodian_to_owner_set(Scheme_Custodian *c) {
  unsigned short i;
  
  for(i = 0; i < owner_table_top; i++)
    if(owner_table[i] && (owner_table[i]->members->custodian == c))
      return i;
  return add_owner_set(c);
}

inline unsigned short current_owner() {
  Scheme_Custodian *c;

  if(!scheme_current_thread || !scheme_current_thread->config)
    return 0;

  c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
  return custodian_to_owner_set(c);
}

inline void fixup_owner_table(void) {
  unsigned short i;

  for(i = 0; i < owner_table_top; i++)
    if(owner_table[i]) {
      struct cust_list *temp;

      for(temp = owner_table[i]->members; temp; temp = temp->next)
        gcFIXUP(temp->custodian);
    }
}

inline struct owner_list *owner_list_add(struct owner_list *ol, 
					 unsigned short owner) {
  struct owner_list *retval = malloc(sizeof(struct owner_list));
  retval->owner = owner;
  retval->next = ol;
  return retval;
}

inline short owner_list_member_p(struct owner_list *ol, 
				 unsigned short owner) {
  for(; ol; ol = ol->next) 
    if(ol->owner == owner)
      return 1;
  return 0;
}

static int owner_compare(const void *a, const void *b) {
  struct owner_list *ol1 = *(struct owner_list **)a;
  struct owner_list *ol2 = *(struct owner_list **)b;
  
  if(cust_list_subset_p(owner_table[ol1->owner]->members,
			owner_table[ol2->owner]->members))
    return 1;
  else
    return 2;
}

inline struct owner_list *sort_owner_list(struct owner_list *ol) {
  struct owner_list **table;
  struct owner_list *temp;
  short list_length = 0;
  short i;

  if(!ol || !ol->next)
    return ol;

  for(temp = ol; temp; temp = temp->next)
    list_length += 1;

  table = malloc(list_length * sizeof(struct owner_list *));
  
  for(temp = ol, i = 0; temp; temp = temp->next, i++)
    table[i] = temp;
  for(i = 0; i < list_length; i++) 
    table[i]->next = NULL;
  my_qsort(table, list_length, sizeof(struct owner_list *), owner_compare);

  list_length -= 1;
  for(i = 0; i < list_length; i++)
    table[i]->next = table[i+1];
  temp = table[0];
  free(table);

  return temp;
}

#endif 
/*****************************************************************************/
/* Routines for root pointers                                                */
/*****************************************************************************/

void GC_add_roots(void *start, void *end) {
  unsigned short owner = current_owner();

  if(owner >= roots_top) {
    unsigned long old_roots_top = roots_top;

    roots_top = owner + SET_OF_ROOTS_GROW_AMT;
    roots = (struct root **)realloc(roots, roots_top * sizeof(struct root *));
    bzero( (void*)((unsigned long)roots + (old_roots_top
					   * sizeof(struct root *))),
           (roots_top - old_roots_top) * sizeof(struct root *) );
  }

  if(roots[owner] == NULL) {
    struct root *newroot = (struct root *)malloc(sizeof(struct root));
    roots[owner] = newroot;
    roots[owner]->count = 0;
    roots[owner]->size = 0;
    roots[owner]->root = NULL;
    roots[owner]->nothing_new = 0;
  }

  if(roots[owner]->count >= roots[owner]->size) {
    unsigned long oldsize = roots[owner]->size;
    roots[owner]->size = roots[owner]->size ? 2 * roots[owner]->size : 500;
    roots[owner]->root = realloc(roots[owner]->root, sizeof(unsigned long) *
                                                     (roots[owner]->size + 1));
    bzero((void*)((char*)roots[owner]->root + (oldsize*sizeof(unsigned long))),
	  (roots[owner]->size - oldsize) * sizeof(unsigned long));
  }

  roots[owner]->root[roots[owner]->count++] = (unsigned long)start;
  roots[owner]->root[roots[owner]->count++] = ((unsigned long)end - 4);
}

static int compare_roots(const void *a, const void *b) {
  return (*(unsigned long *)a < *(unsigned long *)b) ? -1 : 1;
}

static void sort_and_merge_roots(struct root *root) {
  int i, offset, top;

  if(root->nothing_new || (root->count < 4))
    return;

  my_qsort(root->root, root->count >> 1, 2 * sizeof(unsigned long), 
           compare_roots);
  offset = 0; top = root->count;
  for(i = 2; i < top; i += 2) { 
    if((root->root[i - 2 - offset] <= root->root[i])
       && ((root->root[i - 1 - offset] + 3) >= root->root[i])) {
       /* merge */
       if(root->root[i+1] > root->root[i - 1 - offset])
         root->root[i - 1 - offset] = root->root[i + 1];
       offset += 2;
       root->count -= 2;
    } else if(root->root[i] == root->root[i + 1]) {
       /* remove empty range */
       offset += 2;
       root->count -= 2;
    } else if(offset) {
       /* compact */
       root->root[i - offset] = root->root[i];
       root->root[i + 1 - offset] = root->root[i + 1];
    }
  }
  root->nothing_new = 1;
}

inline struct owner_list *get_root_owners(struct owner_list *ol) {
  unsigned short i;

  for(i = 0; i < roots_top; i++)
    if(roots[i]) 
      if(!owner_list_member_p(ol, i))
	ol = owner_list_add(ol, i);
  return ol;
}

inline void mark_some_roots(unsigned short owner) {
  unsigned long j;

  if(roots[owner] && (owner < roots_top)) {
    sort_and_merge_roots(roots[owner]);
#ifdef NEWGC_ACCNT
    if(collection_full_p) collection_mark_owner = owner;
#endif
    for(j = 0; j < roots[owner]->count; j += 2) {
      void **s = (void**)roots[owner]->root[j];
      void **e = (void**)roots[owner]->root[j + 1];
      
      while(s < e) 
	gcMARK(*(s++));
    }
  }
}

inline void mark_all_roots(void) {
  unsigned short i;

  for(i = 0; i < roots_top; i++)
    if(roots[i]) mark_some_roots(i);
}

inline void fixup_all_roots(void) {
  unsigned short i;

  for(i = 0; i < roots_top; i++)
    if(roots[i]) {
      unsigned long j;

      for(j = 0; j < roots[i]->count; j += 2) {
        void **s = (void**)roots[i]->root[j];
        void **e = (void**)roots[i]->root[j + 1];
        while(s < e) gcFIXUP(*(s++));
      }
    }
}

/*****************************************************************************/
/* Administration, Initialization and Write Barrier Routines                 */
/*****************************************************************************/

static void designate_modified(void *p) {
  struct mpage *page = find_page(p);
 
  if(page != NULL) {
    protect_pages(page, page->size, 1);
    page->back_pointers_p = 1;
  } else {
    fprintf(stderr, "Seg fault (internal error) at %p\n", p);
    abort();
  }
}

#if defined(linux)
# include <signal.h>
void fault_handler(int sn, struct sigcontext sc) {
  designate_modified((void *)sc.cr2);
  signal(SIGSEGV, (void(*)(int))fault_handler);
}
# define NEED_SIGSEGV
#endif

#if defined(__FreeBSD__)
# include <signal.h>
void fault_handler(int sn, int code, struct sigcontext *sc, char *addr) {
  designate_modified(addr);
}
# define NEED_SIGBUS
#endif

#if defined(sun)
# include <signal.h>
void fault_handler(int sn, struct siginfo *si, void *ctx) {
  designate_modified(si->su_addr);
}
# define NEED_SIGACTION
#endif

#if defined(_WIN32)
LONG WINAPI fault_handler(LPEXCEPTION_POINTERS e) {
  if((e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
     && (e->ExceptionRecord->ExceptionInformation[0] == 1)) {
    designate_modifed((void *)e->ExceptionRecord->ExceptionInformation[1]);
    return EXCEPTION_CONTINUE_EXECUTION;
  } else return EXCEPTION_CONTINUE_SEARCH;
}
# define NEED_SIGWIN
#endif

void GC_init_type_tags(int count, int weakbox) {
  static int initialized = 0;

  if(!initialized) {
    initialized = 1;

    max_heap_size = determine_max_heap_size();
    pages_in_heap = max_heap_size / MPAGE_SIZE;
    max_used_pages = pages_in_heap / 2;
    gen0_heap = (void**)malloc(GEN0_SIZE);
    gen0_region_p = gen0_heap;
    gen0_region_end = (void**)((unsigned long)gen0_heap + GEN0_SIZE);
    GC_register_traversers(weakbox, size_weak_box, mark_weak_box, 
                           fixup_weak_box, 0, 0);
    GC_register_traversers(weak_array_tag, size_weak_array, mark_weak_array,
                           fixup_weak_array, 0, 0);
    roots = malloc(sizeof(struct root *));
    roots_top = 1;
#ifdef NEWGC_ACCNT
    owner_table = (struct owner_set **)malloc(OWNER_TABLE_INIT_SIZE
                                              * sizeof(struct owner_set *));
    owner_table_top = OWNER_TABLE_INIT_SIZE;
#endif
#ifdef NEED_SIGSEGV
    signal(SIGSEGV, (void (*)(int))fault_handler);
#endif
#ifdef NEED_SIGBUS
    signal(SIGBUS, (void (*)(int))fault_handler);     
#endif
#ifdef NEED_SIGACTION
    {
      struct sigaction act, oact;
      act.sa_sigaction = fault_handler;
      sigemptyset(&act.sa_mask);
      act.sa_flags = SA_SIGINFO;
      sigaction(SIGSEGV, &act, &oact);
    }
#endif
#ifdef NEED_SIGWIN
    SetUnhandledExceptionFilter(fault_handler);
#endif
  }
  weak_box_tag = weakbox;
}

static char *type_name[MPAGE_TYPES] = { "tagged",
					"atomic",
					"array",
					"tagged array",
					"xtagged",
					"big" };

void GC_dump(void) {
  unsigned long gen0_bigs = 0;
  unsigned short i;
  struct mpage *page;
  
  for(page = pages[0][MPAGE_BIG]; page; page = page->next) { gen0_bigs++; }
  fprintf(stderr, "Gen 0: %li of %li bytes used (+ %li in %li bigpages)\n",
	  ((unsigned long)gen0_region_p - (unsigned long)gen0_heap), 
	  (unsigned long)GEN0_SIZE, gen0_bigpages_size, gen0_bigs);
  
  for(i = 1; i < GENERATIONS; i++) {
    unsigned long num_pages = 0, size_pages = 0;
    unsigned long tnum_pages[MPAGE_TYPES] = { 0, 0, 0, 0, 0, 0 };
    unsigned long tsize_pages[MPAGE_TYPES] = { 0, 0, 0, 0, 0, 0 };
    unsigned short j;

    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next) {
	num_pages += 1;	tnum_pages[j] += 1; 
	size_pages += page->size; tsize_pages[j] += page->size;
      }
    fprintf(stderr, "Gen %i: %li bytes used in %li pages\n", i, 
	    size_pages, num_pages);
    if(size_pages) {
      for(j = 0; j < MPAGE_TYPES; j++) {
	fprintf(stderr, "   ... %li %s pages (%li bytes, %2.2f%%)\n",
		tnum_pages[j], type_name[j], tsize_pages[j],
		100.0 * ((float)tsize_pages[j] / (float)size_pages));
      }
    }
  }

#ifdef NEWGC_ACCNT
  {
    unsigned long num_owners = 0, owner_memuse = 0, k;
    struct thread_list *tlist;
    struct account_hook *ahook;
    struct immobile_box *ibox;
    struct finalizer *fnl;

    for(i = 0; i < owner_table_top; i++) {
      if(owner_table[i]) {
	struct cust_list *ctemp;
	struct union_list *utemp;

	num_owners += 1;
	owner_memuse += sizeof(struct owner_set);
	for(ctemp = owner_table[i]->members; ctemp; ctemp = ctemp->next)
	  owner_memuse += sizeof(struct cust_list);
	for(utemp = owner_table[i]->unions; utemp; utemp = utemp->next)
	  owner_memuse += sizeof(struct union_list);
      }
    }

    for(k = 0; k < roots_top; k++)
      if(roots[k]) owner_memuse += sizeof(short);
    for(tlist = thread_list; tlist; tlist = tlist->next)
      owner_memuse += sizeof(struct thread_list);
    for(ahook = account_hooks; ahook; ahook = ahook->next)
      owner_memuse += sizeof(struct account_hook);
    for(ibox = immobile_boxes; ibox; ibox = ibox->next)
      owner_memuse += sizeof(short);
    for(fnl = finalizers; fnl; fnl = fnl->next)
      owner_memuse += sizeof(short);
    owner_memuse += owner_table_top * sizeof(struct otentry *);

    fprintf(stderr, "Tracking %li owner(s)\n", num_owners);
    fprintf(stderr, "   ... owner table top = %li\n", owner_table_top);
    fprintf(stderr, "   ... using %li bytes in owner system\n", owner_memuse);
  }
#endif
}

long GC_get_memory_use(void *custodian) {
  unsigned long retval = 0;

  if(custodian) {
    unsigned short i;

    for(i = 0; i < owner_table_top; i++) 
      if(owner_table[i])
	if(cust_list_member_p(owner_table[i]->members, custodian))
	  retval += owner_table[i]->memory_use;
  } else {
    struct mpage *page;
    unsigned short i, j;

    retval += ((unsigned long)gen0_region_p) - ((unsigned long)gen0_heap);
    for(i = 0; i < GENERATIONS; i++)
      for(j = 0; j < MPAGE_TYPES; j++)
	for(page = pages[i][j]; page; page = page->next)
	  retval += page->size;
  }

  return retval;
}

void GC_gcollect(void) {
  gc_collect(1);
}

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark,
                            Fixup_Proc fixup, int cs, int atomic) {
  size_table[tag] = size;
  mark_table[tag] = atomic ? NULL : mark;
  fixup_table[tag] = fixup;

#ifdef NEWGC_ACCNT
  if(tag == scheme_thread_type) {
    thread_marker = mark_table[tag];
    mark_table[tag] = NULL;
  }
#endif
}

/*****************************************************************************/
/* Thread list routines; these are not defined (for the most part) unless    */
/* we're doing accounting                                                    */
/*****************************************************************************/

void GC_register_thread(void *t) {
#ifdef NEWGC_ACCNT
  struct thread_list *newt = malloc(sizeof(struct thread_list));

  newt->owner = current_owner();
  newt->thread = (Scheme_Thread*)t;
  newt->next = thread_list;
  thread_list = newt;
#endif
}

#ifdef NEWGC_ACCNT
inline struct owner_list *get_thread_owners(struct owner_list *ol) {
  struct thread_list *thread;
  
  for(thread = thread_list; thread; thread = thread->next) 
    if(!owner_list_member_p(ol, thread->owner))
      ol = owner_list_add(ol, thread->owner);
  return ol;
}

inline void mark_some_threads(unsigned short owner) {
  struct thread_list *thread;

  collection_mark_owner = owner;
  for(thread = thread_list; thread; thread = thread->next)
    if(thread->owner == owner) {
      thread_marker(thread->thread);
      gcMARK(thread->thread);
    }
}

inline void mark_all_threads(void) {
  struct thread_list *thread;

  for(thread = thread_list; thread; thread = thread->next) {
    thread_marker(thread->thread);
    gcMARK(thread->thread);
  }
}

inline void fixup_all_threads(void) {
  struct thread_list *thread;

  for(thread = thread_list; thread; thread = thread->next) {
    if(marked_p(thread->thread)) {
      gcFIXUP(thread->thread);
      fixup_table[scheme_thread_type](thread->thread);
    } else {
      thread->thread = NULL;
    }
  }
}
#endif
 
/*****************************************************************************/
/* Routines dealing with the page map; these make page lookups AFAP          */
/*****************************************************************************/

inline void add_page_to_page_map(struct mpage *page) {
  long size_left = page->big_page_p ? page->size : MPAGE_SIZE;
  void *p = page;

  while(size_left > 0) {
    page_map[ADDR_BITS(p)] = page;
    size_left -= MPAGE_SIZE;
    p += MPAGE_SIZE;
  }
}

inline void remove_page_from_page_map(struct mpage *page) {
  long size_left = page->big_page_p ? page->size : MPAGE_SIZE;
  void *p = page;

  while(size_left > 0) {
    page_map[ADDR_BITS(p)] = NULL;
    size_left -= MPAGE_SIZE;
    p += MPAGE_SIZE;
  }
}

inline struct mpage *find_page(void *p) {
  return page_map[ADDR_BITS(p)];
}

/*****************************************************************************/
/* Allocation routines                                                       */
/*****************************************************************************/

/* inline int gen0_member_p(void *p) { */
/*   return (((unsigned long)p >= (unsigned long)gen0_heap) */
/*           && ((unsigned long)p < (unsigned long)gen0_region_p)); */
/* } */

#define gen0_member_p(p) (((unsigned long)p >= (unsigned long)gen0_heap) && ((unsigned long)p < (unsigned long)gen0_region_p))


inline void *allocate(size_t size_in_bytes, int type) {
  if(size_in_bytes) {
    size_t size_in_words = gcBYTES_TO_WORDS(size_in_bytes) + 1;

    if(size_in_words < MAX_OBJECT_WORD_SIZE) {
      struct objhead *info;
      void *retval = (void*)gen0_region_p;

      gen0_region_p += size_in_words;
      if(gen0_region_p >= gen0_region_end) {
        gen0_region_p -= size_in_words;
        gc_collect(0);
        retval = gen0_region_p;
        gen0_region_p += size_in_words;
      }
      info = (struct objhead *)retval;
      info->type = type;
      info->size = size_in_words;
      return (void*)((unsigned long)retval + 4);
    } else return allocate_big(size_in_words, type);
  } else return zero_sized;
}

static void *allocate_big(size_t size_in_words, int type) {
  unsigned long size_in_bytes;
  struct mpage *bpage;

  size_in_words += PAGE_WORD_OVERHEAD;
  size_in_bytes = gcWORDS_TO_BYTES(size_in_words);
  gen0_bigpages_size += size_in_bytes;
  if((gen0_bigpages_size + ((unsigned long)gen0_region_p - 
                            (unsigned long)gen0_heap)) > GEN0_SIZE) {
    gc_collect(0);
    gen0_bigpages_size = size_in_bytes;
  }

  bpage = malloc_pages(size_in_bytes, MPAGE_SIZE);
  bpage->size = size_in_bytes;
  bpage->big_page_p = 1;
  bpage->page_type = type;
  bpage->next = pages[0][MPAGE_BIG];

  add_page_to_page_map(bpage);
  if(pages[0][MPAGE_BIG])
    pages[0][MPAGE_BIG]->prev = bpage;
  pages[0][MPAGE_BIG] = bpage;
  return (void*)((unsigned long)bpage + PAGE_BYTE_OVERHEAD + 4);
}

inline void *copy_bits(void *from, unsigned long size, short type, short gen) {
  unsigned long size_in_bytes = gcWORDS_TO_BYTES(size);
  struct mpage *work = pages[gen][type];
  void *retval;

  while(work && ((work->size + size_in_bytes) >= MPAGE_SIZE))
    work = work->next;

  if(work) {
    retval = (void*)((unsigned long)work + work->size);
  } else {
    work = (struct mpage *)malloc_pages(MPAGE_SIZE, MPAGE_SIZE);
    work->next = pages[gen][type];
    work->generation = gen;
    work->page_type = type;
    work->size = work->previous_size = PAGE_BYTE_OVERHEAD;
    add_page_to_page_map(work);

    if(pages[gen][type])
      pages[gen][type]->prev = work;
    pages[gen][type] = work;
    retval = (void*)((unsigned long)work + PAGE_BYTE_OVERHEAD);
  }

  work->size += size_in_bytes;
  {
    void **dest = (void**)retval;
    void **src = (void**)from;

    while(size--)
      *(dest++) = *(src++);
  }
  //memcpy(retval, from, size_in_bytes);
  return retval;
}

void *GC_malloc(size_t size) {
  return allocate(size, MPAGE_ARRAY);
}

void *GC_malloc_one_tagged(size_t size) {
  return allocate(size, MPAGE_TAGGED);
}

void *GC_malloc_one_xtagged(size_t size) {
  return allocate(size, MPAGE_XTAGGED);
}

void *GC_malloc_array_tagged(size_t size) {
  return allocate(size, MPAGE_TARRAY);
}

void *GC_malloc_atomic(size_t size) {
  return allocate(size, MPAGE_ATOMIC);
}

void *GC_malloc_atomic_uncollectable(size_t size) {
  return malloc(size);
}

void *GC_malloc_weak_array(size_t size, void *replace_val) {
  struct weak_array *wa;


  park[0] = replace_val;
  wa = GC_malloc_one_tagged(size + sizeof(struct weak_array) - sizeof(void*));
  replace_val = park[0]; park[0] = NULL;

  wa->type = weak_array_tag;
  wa->replace_val = replace_val;
  wa->count = gcBYTES_TO_WORDS(size);
  return wa;
}

void *GC_malloc_weak_box(void *p, void **secondary, int soffset) {
  struct weak_box *wb;

  park[0] = p;
  park[1] = secondary;
  wb = GC_malloc_one_tagged(sizeof(struct weak_box));
  p = park[0]; park[0] = NULL;
  secondary = (void**)park[1]; park[1] = NULL;

  wb->type = weak_box_tag;
  wb->val = p;
  wb->secondary_erase = secondary;
  wb->soffset = soffset;
  return wb;
}

void **GC_malloc_immobile_box(void *p) {
  struct immobile_box *ib = malloc(sizeof(struct immobile_box));
  ib->p = p;
#ifdef NEWGC_ACCNT
  ib->owner = current_owner();
#endif
  ib->next = immobile_boxes;
  ib->prev = NULL;
  if(immobile_boxes) immobile_boxes->prev = ib;
  immobile_boxes = ib;
  return (void**)ib;
}

void GC_free_immobile_box(void **b) {
  struct immobile_box *ib = (struct immobile_box *)b;

  if(b) {
    if(ib->prev)
      ib->prev->next = ib->next;
    else
      immobile_boxes = ib->next;
    if(ib->next)
      ib->next->prev = ib->prev;
    free(ib); 
  }
}

void *GC_malloc_allow_interior(size_t size_in_bytes) {
  return allocate_big(gcBYTES_TO_WORDS(size_in_bytes) + 1, MPAGE_ARRAY);
}

void GC_free(void *p) {
  struct mpage *page = find_page(p);

  if(page && page->big_page_p) {
    if(page->prev)
      page->prev->next = page->next;
    else
      pages[page->generation][MPAGE_BIG] = page->next;
    if(page->next)
      page->next->prev = page->prev;
    remove_page_from_page_map(page);
    free_pages(page, page->size); 
  }
}

/*****************************************************************************/
/* Routines dealing with finalizers and finalization                         */
/*****************************************************************************/

void GC_set_finalizer(void *p, int tagged, int level,
                      GC_finalization_proc f, void *data,
                      GC_finalization_proc *oldf, void **olddata) {
  struct mpage *page = find_page(p);
  struct finalizer *fnl, *prev;

  if(!page && !gen0_member_p(p)) {
    /* never collected */
    if(oldf) *oldf = NULL;
    if(olddata) *olddata = NULL;
    return;
  }

  for(fnl = finalizers, prev = NULL; fnl; prev = fnl, fnl = fnl->next) {
    if(fnl->p == p) {
      if(oldf) *oldf = fnl->f;
      if(olddata) *olddata = fnl->data;
      if(f) {
        fnl->f = f;
        fnl->data = data;
        fnl->eager_level = level;
#ifdef NEWGC_ACCNT
        fnl->owner = current_owner();
#endif
      } else {
        if(prev)
          prev->next = fnl->next;
        else 
          finalizers = fnl->next;
      }
      return;
    }
  }

  if(oldf) *oldf = NULL;
  if(olddata) *olddata = NULL;
  if(!f) return;

  park[0] = p;
  park[1] = data;
  fnl = GC_malloc_atomic(sizeof(struct finalizer));
  p = park[0]; park[0] = NULL;
  data = park[1]; park[1] = NULL;

  fnl->next = finalizers;
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;
  fnl->tagged = tagged;
  finalizers = fnl;
}

void GC_finalization_weak_ptr(void **p, int offset) {
  struct weak_finalizer *wfnl;

  park[0] = p;
  wfnl = GC_malloc_atomic(sizeof(struct weak_finalizer));
  p = park[0]; park[0] = NULL;

  wfnl->p = p;
  wfnl->next = weak_finalizers;
  wfnl->offset = offset * sizeof(void*);
  weak_finalizers = wfnl;
}

inline struct owner_list *get_finalizer_owners(struct owner_list *ol) {
  struct finalizer *fnl;

  for(fnl = run_queue; fnl; fnl = fnl->next)
    if(!owner_list_member_p(ol, fnl->owner))
      ol = owner_list_add(ol, fnl->owner);
  for(fnl = finalizers; fnl; fnl = fnl->next)
    if(!owner_list_member_p(ol, fnl->owner))
      ol = owner_list_add(ol, fnl->owner);
  return ol;
}

inline void mark_some_finalizers(unsigned short owner) {
  struct finalizer *fnl;
  
  for(fnl = run_queue; fnl; fnl = fnl->next) 
    if(fnl->owner == owner) {
      gcMARK(fnl);
      gcMARK(fnl->data);
      gcMARK(fnl->p);
    }
  for(fnl = finalizers; fnl; fnl = fnl->next) 
    if(fnl->owner == owner) {
      gcMARK(fnl);
      gcMARK(fnl->data);
      if(!marked_p(fnl->p)) {
	gcMARK(fnl->p);
	if(((struct objhead *)((unsigned long)fnl->p - 4))->mark) {
	  ((struct objhead *)((unsigned long)fnl->p - 4))->mark_fnl = 1;
	  ((struct objhead *)((unsigned long)fnl->p - 4))->mark = 0;
	}
      }
    }
  
  if(owner == current_owner()) {
    struct weak_finalizer *wfnl;

    for(wfnl = weak_finalizers; wfnl; wfnl = wfnl->next) {
      wfnl->saved = *(void**)((unsigned long)wfnl->p + wfnl->offset);
      *(void**)((unsigned long)wfnl->p + wfnl->offset) = NULL;
      gcMARK(wfnl);
    }
  }
}

inline void mark_all_finalizers(void) {
  struct finalizer *fnl;
  struct weak_finalizer *wfnl;

  for(wfnl = weak_finalizers; wfnl; wfnl = wfnl->next) {
    gcMARK(wfnl);
    if(collection_full_p) {
      wfnl = *(void**)wfnl;
      wfnl->saved = *(void**)((unsigned long)wfnl->p + wfnl->offset);
      if(marked_p(wfnl->p))
	*(void**)((unsigned long)GC_resolve(wfnl->p) + wfnl->offset) = NULL;
      else
	*(void**)((unsigned long)wfnl->p + wfnl->offset) = NULL;
    }
  }

  for(fnl = run_queue; fnl; fnl = fnl->next) {
    gcMARK(fnl);
    gcMARK(fnl->data);
    gcMARK(fnl->p);
  }

  for(fnl = finalizers; fnl; fnl = fnl->next) {
    gcMARK(fnl);
    gcMARK(fnl->data);
    if(!marked_p(fnl->p)) {
      gcMARK(fnl->p);
      if(((struct objhead *)((unsigned long)fnl->p - 4))->mark) {
        ((struct objhead *)((unsigned long)fnl->p - 4))->mark_fnl = 1;
        ((struct objhead *)((unsigned long)fnl->p - 4))->mark = 0;
      }
    }
  }
}

inline void reset_weak_finalizers(void) {
  struct weak_finalizer *wfnl;

  for(wfnl = weak_finalizers; wfnl; wfnl = wfnl->next) {
    wfnl = *(void**)wfnl;
    if(marked_p(wfnl->p)) {
      gcMARK(wfnl->saved);
      *(void**)((unsigned long)GC_resolve(wfnl->p) + wfnl->offset) =
        wfnl->saved;
    } else {
      *(void**)((unsigned long)wfnl->p + wfnl->offset) = wfnl->saved;
    }
  }
}

inline void fixup_all_finalizers(void) {
  struct finalizer *fnl, *prevf = NULL;
  struct weak_finalizer *wfnl, *prevw = NULL;

  gcFIXUP(run_queue); 
  for(fnl = run_queue; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data); gcFIXUP(fnl->p); gcFIXUP(fnl->next);
  }

  gcFIXUP(finalizers); fnl = finalizers; prevf = NULL;
  while(fnl) {
    struct objhead *phead = (struct objhead *)((unsigned long)fnl->p - 4);
    
    gcFIXUP(fnl->data); gcFIXUP(fnl->p); gcFIXUP(fnl->next);
    if(phead->mark_fnl && !phead->mark) {
      struct finalizer *temp = fnl;

      fnl = fnl->next;
      temp->next = run_queue;
      run_queue = temp;
      if(prevf)
        prevf->next = fnl;
      else
        finalizers = fnl;
    } else {
      prevf = fnl;
      fnl = fnl->next;
    }
  }

  gcFIXUP(weak_finalizers); wfnl = weak_finalizers; prevw = NULL;
  while(wfnl) {
    gcFIXUP(wfnl->next);

    if(!marked_p(wfnl->p)) {
      wfnl->p = NULL;
      if(prevw) 
        prevw->next = wfnl->next; 
      else 
        weak_finalizers = wfnl->next;
      wfnl = wfnl->next;
    } else { 
      gcFIXUP(wfnl->p);
      prevw = wfnl; 
      wfnl = wfnl->next;
    }
  }
}

/*****************************************************************************/
/* Routines dealing with weak items (arrays and boxes)                       */
/*****************************************************************************/

static int size_weak_box(void *p) {
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

static int mark_weak_box(void *p) {
  struct weak_box *wb = (struct weak_box *)p;
  gcMARK(wb->secondary_erase);
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

static int fixup_weak_box(void *p) {
  struct weak_box *wb = (struct weak_box *)p;
  gcFIXUP(wb->secondary_erase);
  if(!marked_p(wb->val)) {
    wb->val = NULL;
    if(wb->secondary_erase) 
      *(wb->secondary_erase + wb->soffset) = NULL;
    wb->secondary_erase = NULL;
  } else gcFIXUP(wb->val);
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

static int size_weak_array(void *p) {
  struct weak_array *wa = (struct weak_array *)p;
  return gcBYTES_TO_WORDS(sizeof(struct weak_array)
                          + ((wa->count - 1) * sizeof(void)));
}

static int mark_weak_array(void *p) {
  struct weak_array *wa = (struct weak_array *)p;
  gcMARK(wa->replace_val);
  return gcBYTES_TO_WORDS(sizeof(struct weak_array)
                          + ((wa->count - 1) * sizeof(void)));
}

static int fixup_weak_array(void *p) {
  struct weak_array *wa = (struct weak_array *)p;
  void **data = wa->data;
  int i;

  gcFIXUP(wa->replace_val);
  for(i = wa->count; i--; ) { 
    if(data[i] && !marked_p(data[i])) {
      data[i] = wa->replace_val;
    } else if(data[i]) gcFIXUP(data[i]);
  }
  return gcBYTES_TO_WORDS(sizeof(struct weak_array)
                          + ((wa->count - 1) * sizeof(void)));
}

/*****************************************************************************/
/* Immobile box routines                                                     */
/*****************************************************************************/

inline struct owner_list *get_immobile_box_owners(struct owner_list *ol) {
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    if(!owner_list_member_p(ol, ib->owner))
      ol = owner_list_add(ol, ib->owner);
  return ol;
}

inline void mark_some_immobiles(unsigned short owner) {
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    if(ib->owner == owner)
      gcMARK(ib->p);
}

inline void mark_all_immobiles(void) {
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    gcMARK(ib->p);
}

inline void fixup_all_immobiles(void) {
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    gcFIXUP(ib->p);
}

/*****************************************************************************/
/* Account hook routines                                                     */
/*****************************************************************************/

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2) {
#ifdef NEWGC_ACCNT
  struct account_hook *work = malloc(sizeof(struct account_hook));

  work->type = type;
  work->bytes = b;
  work->cust = c1;
  work->cust_to_kill = c2;
  work->next = account_hooks;
  account_hooks = work;

  if(type == MZACCT_REQUIRE)
    total_requires += b;
  return 1;
#else
  return 0;
#endif
}

#ifdef NEWGC_ACCNT
inline void fixup_account_hooks(void) {
  struct account_hook *work;

  for(work = account_hooks; work; work = work->next) 
    if(marked_p(work->cust) && marked_p(work->cust_to_kill)) {
      gcFIXUP(work->cust);
      gcFIXUP(work->cust_to_kill);
    } else {
      work->cust = NULL;
      work->cust_to_kill = NULL;
    }
}

inline void run_account_hooks(void) {
  struct account_hook *work = account_hooks, *prev = NULL;

  while(work) {
    int keep = 1;

    /* many cases: */
    if(!work->cust && !work->cust_to_kill) {
      /* this should be "collected", since at least one of the pointers
         was dead */
      keep = 0;
    } else if(collection_full_p) {
      /* this is a full collection, so we need to check any limits and
	 requires */

      if(work->type == MZACCT_LIMIT) {
	/* check a LIMIT */
	if(GC_get_memory_use(work->cust) > work->bytes) {
	  scheme_schedule_custodian_close(work->cust_to_kill);
	  keep = 0;
	} 
      } else {
	/* check a REQUIRE */
	if( ((max_used_pages - used_pages) * MPAGE_SIZE) < total_requires ) {
	  scheme_schedule_custodian_close(work->cust_to_kill);
	  keep = 0;
	} 
      }
    } 

    if(keep) {
      prev = work;
      work = work->next;
    } else {
      struct account_hook *temp = work;

      work = work->next;
      if(prev)
	prev->next = work;
      else
	account_hooks = work;

      if(temp->type == MZACCT_REQUIRE)
	total_requires -= temp->bytes;

      free(temp);
    }
  }
}
#endif

/*****************************************************************************/
/* Resolution and Fixup Routines                                             */
/*****************************************************************************/

void *GC_resolve(void *p) {
  if(gen0_member_p(p)) {
    if(((struct objhead *)((unsigned long)p - 4))->mark
       || ((struct objhead *)((unsigned long)p - 4))->mark_fnl)
      return *(void**)p;
    else return p;
  } else {
    struct mpage *page = find_page(p);

    if(!page || page->big_page_p)
      return p;
    if(((struct objhead *)((unsigned long)p - 4))->mark
       || ((struct objhead *)((unsigned long)p - 4))->mark_fnl)
      return *(void**)p;
    else return p;
  }
}

void GC_fixup(void *pp) {
  void *p = *(void**)pp;

  if(!p || ((long)p & 0x1))
    return;

  if(gen0_member_p(p)) {
    struct mpage *from_page = find_page(pp);

    if(from_page && !from_page->back_pointers_p)
      if(from_page->generation > 1)
        from_page->back_pointers_p = 1;
    *(void**)pp = *(void**)p;
  } else {
    struct mpage *page = find_page(p);
    struct objhead *info = (struct objhead *)((unsigned long)p - 4);

    if(page) {
      if(page->big_page_p) {
        struct mpage *from_page = find_page(pp);

        if(from_page && !from_page->back_pointers_p) {
          if(from_page->generation > page->generation) {
            from_page->back_pointers_p = 1;
          }
        }
        return;
      }

      if(info->mark || info->mark_fnl) {
        struct mpage *from_page = find_page(pp);
  
        if(from_page && !from_page->back_pointers_p)
          if(from_page->generation > INCGEN(page->generation))
            from_page->back_pointers_p = 1;
        *(void**)pp = *(void**)p;
      }
    }
  }
}

inline void fixup_big_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD + 4);
  void **end = (void**)((unsigned long)page + page->size);

  switch(page->page_type) {
    case MPAGE_TAGGED:
      fixup_table[*(unsigned short*)start](start);
      break;
    case MPAGE_ATOMIC:
      break;
    case MPAGE_ARRAY:
      while(start < end)
        gcFIXUP(*(start++));
      break;
    case MPAGE_TARRAY: {
      unsigned short tag = *(unsigned short*)start;
      while(start < end)
        start += fixup_table[tag](start);
      break;
    }
    case MPAGE_XTAGGED:
      GC_fixup_xtagged(start);
      break;
  }
}

inline void fixup_tagged_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    start++;
    start += fixup_table[*(unsigned short*)start](start);
  }
}

inline void fixup_array_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start++;
    unsigned long size = ohead->size;
    while(--size)
      gcFIXUP(*(start++));
  }
}

inline void fixup_tarray_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start;
    unsigned long size = ohead->size;
    void **tempend = start + size;
    unsigned short tag = *(unsigned short*)(++start);
    while(start < tempend)
      start += fixup_table[tag](start);
  }
}

inline void fixup_xtagged_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start;
    GC_fixup_xtagged(start + 1);
    start += ohead->size;
  }
} 

inline void fixup_heap(void) {
  struct mpage *page;
  unsigned short i, j;
  unsigned short top_gen_plus_one = INCGEN(collection_top);

  for(i = 1; i <= top_gen_plus_one; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next) {
        switch(j) {
          case MPAGE_TAGGED: fixup_tagged_page(page); break;
          case MPAGE_ATOMIC: break;
          case MPAGE_ARRAY: fixup_array_page(page); break;
          case MPAGE_TARRAY: fixup_tarray_page(page); break;
          case MPAGE_XTAGGED: fixup_xtagged_page(page); break;
          case MPAGE_BIG: fixup_big_page(page); break;
        }
        page->previous_size = page->size;
      }

  if(collection_top == 0) 
    for(j = 0; j < MPAGE_TYPES; j++) 
      for(page = pages[2][j]; page; page = page->next) 
        if(page->back_pointers_p)
          switch(j) {
            case MPAGE_TAGGED: fixup_tagged_page(page); break;
            case MPAGE_ATOMIC: break;
            case MPAGE_ARRAY: fixup_array_page(page); break;
            case MPAGE_TARRAY: fixup_tarray_page(page); break;
            case MPAGE_XTAGGED: fixup_xtagged_page(page); break;
            case MPAGE_BIG: fixup_big_page(page); break;
          }
}

/*****************************************************************************/
/* Routines dealing with marks and marking                                   */
/*****************************************************************************/

inline int marked_p(void *p) {
  if(!p) return 0;

  if(gen0_member_p(p)) {
    struct objhead *ohead = (struct objhead *)((unsigned long)p - 4);
    return (ohead->mark || ohead->mark_fnl);
  } else {
    struct mpage *page = find_page(p);
    struct objhead *ohead;
  
    if(!page) return 0;
    if(page->generation > collection_top)
      return 1;

    ohead = (struct objhead *)((unsigned long)p - 4);
    return (ohead->mark || ohead->mark_fnl);
  }
}

#ifdef NEWGC_PRECISE
inline void mark_owner_reprocess(const void *p) {
  if(collection_full_p) {
    void *newp = *(void**)p;
    struct objhead *info = (struct objhead *)((unsigned long)newp - 4);
    if(info->owner != collection_mark_owner) {
      unsigned short oldowner = info->owner;
      struct mpage *page = find_page(newp);

      info->owner = owner_set_union(info->owner, collection_mark_owner);
      if(info->owner != oldowner) {
	owner_table_account(oldowner, -gcWORDS_TO_BYTES(info->size));
	owner_table_account(info->owner, gcWORDS_TO_BYTES(info->size));
	if(newp < (void*)((unsigned long)page + page->previous_size)) {
	  page->previous_size = PAGE_BYTE_OVERHEAD;
	}
      }
    }
  }
}
#else
#define mark_owner_reprocess(p) { }
#endif

inline void mark_normal(struct mpage *page, void *p) {
  struct objhead *ohead = (struct objhead *)((unsigned long)p - 4);

  if(ohead->mark) {
    mark_owner_reprocess(p);
  } else if(ohead->mark_fnl) {
    ohead->mark = 1;
    mark_owner_reprocess(p);
  } else {
    unsigned long tgen = page ? INCGEN(page->generation) : 1;
    unsigned short type = ohead->type;
    void *newplace;

    if(type == MPAGE_TAGGED)
      if(mark_table[*(unsigned short*)p] == NULL) {
        ohead->type = MPAGE_ATOMIC;
        type = MPAGE_ATOMIC;
      }

    newplace = copy_bits((void*)ohead, ohead->size, type, tgen);
    *(void**)p = (void*)((unsigned long)newplace + 4);
    ohead->mark = 1;
#ifdef NEWGC_ACCNT
    owner_table_account(collection_mark_owner, gcWORDS_TO_BYTES(ohead->size));
    ((struct objhead *)newplace)->owner = collection_mark_owner;
#endif
  }
}

void GC_mark(const void *p) {
  if(!p || ((unsigned long)p & 0x1))
    return;

  if(gen0_member_p((void*)p)) mark_normal(NULL, (void*)p); else {
    struct mpage *page = find_page((void*)p);
    
    if(!page || (page->generation > collection_top))
      return;
    if(page->big_page_p) {
      if(page->big_page_p == 1) {
#ifdef NEWGC_ACCNT
        struct objhead *ohead = 
          (struct objhead *)((unsigned long)page + PAGE_BYTE_OVERHEAD);
        owner_table_account(collection_mark_owner, page->size);
        ohead->owner = collection_mark_owner;
#endif
        page->big_page_p++;
        page->previous_size = PAGE_BYTE_OVERHEAD;

        if(page->prev) 
          page->prev->next = page->next;
        else {
          if(page->next) page->next->prev = NULL;
          collection_from_pages = page->next;
        }

        if(page->next)
          page->next->prev = page->prev;

        page->generation = INCGEN(page->generation);
        if(pages[page->generation][MPAGE_BIG])
          pages[page->generation][MPAGE_BIG]->prev = page;
        page->next = pages[page->generation][MPAGE_BIG];
        pages[page->generation][MPAGE_BIG] = page;
      } else {
#ifdef NEWGC_PRECISE
	if(collection_full_p) {
	  struct objhead *info =
	    (struct objhead *)((unsigned long)page + PAGE_BYTE_OVERHEAD);
	  
	  if(info->owner != collection_mark_owner) {
	    unsigned short old_owner = info->owner;

	    info->owner = owner_set_union(old_owner, collection_mark_owner);
	    if(old_owner != info->owner) {
	      owner_table_account(old_owner, -page->size);
	      owner_table_account(info->owner, page->size);
	      page->previous_size = PAGE_BYTE_OVERHEAD;
	    }
	  }
	}
#endif	
      }
    } else mark_normal(page, (void*)p);
  }
}

inline void mark_big_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD + 4);
  void **end = (void**)((unsigned long)page + page->size);

  switch(page->page_type) {
    case MPAGE_TAGGED: mark_table[*(unsigned short*)start](start); break;
    case MPAGE_ATOMIC: break;
    case MPAGE_ARRAY: while(start < end) GC_mark(*(start++)); break;
    case MPAGE_TARRAY: {
      unsigned short tag = *(unsigned short*)start;
 
      while(start < end)
        start += mark_table[tag](start);
      break;
    }
    case MPAGE_XTAGGED: GC_mark_xtagged(start); break;
  }
}

inline void mark_tagged_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
#ifdef NEWGC_ACCNT
    struct objhead *info = (struct objhead *)start;
    collection_mark_owner = info->owner;
#endif
    start++;
    start += mark_table[*(unsigned short*)start](start);
  }
}

inline void mark_array_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start++;
    unsigned long size = info->size;

#ifdef NEWGC_ACCNT
    collection_mark_owner = info->owner;
#endif
    while(--size)
      GC_mark(*(start++));
  }
}

inline void mark_tarray_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start;
    void **tempend = start + info->size;
    unsigned short tag = *(unsigned short*)(++start);

#ifdef NEWGC_ACCNT
    collection_mark_owner = info->owner;
#endif
    while(start < tempend) 
      start += mark_table[tag](start);
  }
}

inline void mark_xtagged_page(struct mpage *page) {
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start;

#ifdef NEWGC_ACCNT
    collection_mark_owner = info->owner;
#endif
    GC_mark_xtagged(start + 1);
    start += info->size;
  }
}

inline void mark_older_pointers(void) {
  unsigned short i, j;
  struct mpage *page;

  for(i = INCGEN(collection_top); i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next)
        if(page->back_pointers_p) {
          page->previous_size = PAGE_BYTE_OVERHEAD;
          switch(j) {
            case MPAGE_TAGGED: mark_tagged_page(page); break;
            case MPAGE_ATOMIC: break;
            case MPAGE_ARRAY: mark_array_page(page); break;
            case MPAGE_TARRAY: mark_tarray_page(page); break;
            case MPAGE_XTAGGED: mark_xtagged_page(page); break;
            case MPAGE_BIG: mark_big_page(page); break;
          }
        }
}

inline void propogate_all_marks(void) {
  struct mpage *page;
  unsigned short i, j;
  unsigned short top_gen_plus_one = INCGEN(collection_top);
  unsigned short changes;

  do {
    changes = 0;

    for(i = 1; i <= top_gen_plus_one; i++) {
      for(j = 0; j < MPAGE_TYPES; j++) {
        for(page = pages[i][j]; page; page = page->next) {
          if(page->previous_size != page->size) {
             switch(j) {
               case MPAGE_TAGGED: mark_tagged_page(page); break;
               case MPAGE_ATOMIC: break;
               case MPAGE_ARRAY: mark_array_page(page); break;
               case MPAGE_TARRAY: mark_tarray_page(page); break;
               case MPAGE_XTAGGED: mark_xtagged_page(page); break; 
               case MPAGE_BIG: mark_big_page(page); break;
             }
             page->previous_size = page->size;
             changes = 1;
          }
        }
      }
    }
  } while(changes);
}

/*****************************************************************************/
/* Core garbage collection routines                                          */
/*****************************************************************************/

static void gc_collect(int force_full) {
  struct mpage *work;
  unsigned short i, j;

  if(GC_collect_start_callback)
    GC_collect_start_callback();

  if(force_full) 
    collection_cycle = 11;
  collection_number++;
  switch(collection_cycle) {
    case 3: case 7: collection_top = 1; collection_cycle++; break;
    case 11: collection_top = 2; collection_cycle = 0; break;
    default: collection_top = 0; collection_cycle++; break;
  }
  collection_full_p = (collection_top == (GENERATIONS - 1));

  /* rip away the pages we might collect */
  for(i = 0; i <= collection_top; i++)
    for(j = 0; j < MPAGE_TYPES; j++) {
      work = pages[i][j];
      while(work) {
        struct mpage *next = work->next;

        protect_pages(work, work->size, 1);
        if(work->big_page_p)
          work->big_page_p = 1;

        if(collection_from_pages)
          collection_from_pages->prev = work;
        work->prev = NULL;
        work->next = collection_from_pages;
        collection_from_pages = work;

        work = next;
      }
      pages[i][j] = NULL;
    }


#ifdef NEWGC_ACCNT
  if(collection_full_p) {
    struct owner_list *ol = NULL, *temp;
    unsigned short curowner = current_owner();

    ol = get_root_owners(ol);
    ol = get_thread_owners(ol);
    ol = get_finalizer_owners(ol);
    ol = get_immobile_box_owners(ol);
    ol = sort_owner_list(ol);

    for(temp = ol; temp; temp = temp->next) {
      mark_some_threads(temp->owner);
      mark_some_roots(temp->owner);
      mark_some_finalizers(temp->owner);
      mark_some_immobiles(temp->owner);
      if(temp->owner == curowner)
	GC_mark_variable_stack(GC_variable_stack, 0,
			       (void*)(GC_get_thread_stack_base
				       ? GC_get_thread_stack_base()
				       : (unsigned long)stack_base));
      propogate_all_marks();
    }
  } else {
#endif
    if(!collection_full_p)
      mark_older_pointers();
    mark_all_finalizers();
    mark_all_roots();
#ifdef NEWGC_ACCNT
    mark_all_threads();
#endif
    mark_all_immobiles();
    GC_mark_variable_stack(GC_variable_stack, 0,
			   (void*)(GC_get_thread_stack_base
				   ? GC_get_thread_stack_base()
				   : (unsigned long)stack_base));
    propogate_all_marks();
#ifdef NEWGC_ACCNT
  }
#endif

  if(collection_full_p) {
    reset_weak_finalizers();
    propogate_all_marks();
  }

#ifdef NEWGC_ACCNT
  fixup_owner_table();
  fixup_all_threads();
  fixup_account_hooks();
#endif
  fixup_all_finalizers();
  fixup_all_roots();
  fixup_all_immobiles();
  GC_fixup_variable_stack(GC_variable_stack, 0, 
                          (void*)(GC_get_thread_stack_base
                                  ? GC_get_thread_stack_base()
                                  : (unsigned long)stack_base));
  fixup_heap();

  /* delete all the old, dead pages */
  work = collection_from_pages;
  while(work) {
    struct mpage *temp = work;
  
    work = work->next;
    remove_page_from_page_map(temp);
    free_pages(temp, temp->big_page_p ? temp->size : MPAGE_SIZE);
  }
  collection_from_pages = NULL;

  /* protect the new pages */
  for(i = 1; i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      if(j != MPAGE_ATOMIC)
        for(work = pages[i][j]; work; work = work->next)
          protect_pages(work, work->size, 0);
  gen0_bigpages_size = 0;
  bzero(gen0_heap, ((unsigned long)gen0_region_p - 
                    (unsigned long)gen0_heap));
  gen0_region_p = gen0_heap;
  flush_freed_pages();

  if(GC_collect_start_callback)
    GC_collect_end_callback();

  /* run any queued finalizers */
  run_account_hooks();
  for(i = 0; run_queue; ) {
    struct finalizer *work = run_queue, *prev = NULL;

    while(work && (work->eager_level != i)) {
      prev = work;
      work = work->next;
    }
    
    if(work) {
      void **gcs = GC_variable_stack;

      if(prev)
        prev->next = work->next;
      else
        run_queue = work->next;
 
      work->f(work->p, work->data);
      GC_variable_stack = gcs;
    } else i++;
  }
} 

/*****************************************************************************/
/* OS-Specific allocation routines                                           */
/*****************************************************************************/

/* Windows */

#if _WIN32

inline void *malloc_pages(size_t len, size_t alignment)
{
  {
    used_pages += (len / MPAGE_SIZE);
    used_pages += ((len % MPAGE_SIZE) == 0) ? 0 : 1;
    if(used_pages > max_used_pages) {
      gc_collect(0);
      if(used_pages > max_used_pages) {
	gc_collect(1);
	if(used_pages > max_used_pages) {
	  if(GC_out_of_memory) GC_out_of_memory();
	  printf("The System is out of memory!\n");
	  abort();
	}
      }
    }
  }

  return (void *)VirtualAlloc(NULL, len, 
			      MEM_COMMIT | MEM_RESERVE, 
			      PAGE_READWRITE);
}

static void free_pages(void *p, size_t len)
{
  used_pages -= (len / MPAGE_SIZE);
  used_pages -= ((len % MPAGE_SIZE) == 0) ? 0 : 1;
  VirtualFree(p, 0, MEM_RELEASE);
}

static void flush_freed_pages(void)
{
}

static void protect_pages(void *p, size_t len, int writeable)
{
  DWORD old;
  VirtualProtect(p, len, (writeable ? PAGE_READWRITE : PAGE_READONLY), &old);
}

static unsigned long determine_max_heap_size(void) 
{
  fprintf(stderr, 
	  "Don't know how to get heap size for Windows: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
}

# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/

/* OSKit */

#if OSKIT
# include <oskit/c/malloc.h>

inline void *malloc_pages(size_t len, size_t alignment)
{
  void *p;

  {
    used_pages += (len / MPAGE_SIZE);
    used_pages += ((len % MPAGE_SIZE) == 0) ? 0 : 1;
    if(used_pages > max_used_pages) {
      gc_collect(0);
      if(used_pages > max_used_pages) {
	gc_collect(1);
	if(used_pages > max_used_pages) {
	  if(GC_out_of_memory) GC_out_of_memory();
	  printf("The System is out of memory!\n");
	  abort();
	}
      }
    }
  }

  p = smemalign(alignment, len);
  memset(p, 0, len);
  return p;
}

static void free_pages(void *p, size_t len)
{
  used_pages -= (len / MPAGE_SIZE);
  used_pages -= ((len % MPAGE_SIZE) == 0) ? 0 : 1;
  sfree(p, len);
}

static void flush_freed_pages(void)
{
}

static unsigned long determine_max_heap_size(void)
{
  fprintf(stderr, 
	  "Don't know how to get heap size for OSKit: assuming 1GB\n");
  return (1 * 1024 * 1024 * 1024);
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

static int page_size;

inline void *malloc_pages(size_t len, size_t alignment)
{
  void *r;
  size_t extra = 0;

  if (!page_size)
    page_size = getpagesize();

  {
    used_pages += (len / MPAGE_SIZE);
    used_pages += ((len % MPAGE_SIZE) == 0) ? 0 : 1;
    if(used_pages > max_used_pages) {
      gc_collect(0);
      if(used_pages > max_used_pages) {
	gc_collect(1);
	if(used_pages > max_used_pages) {
	  if(GC_out_of_memory) GC_out_of_memory();
	  printf("The System is out of memory!\n");
	  abort();
	}
      }
    }
  }

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

  return r;
}

static void free_pages(void *p, size_t len)
{
  int i;

  used_pages -= (len / MPAGE_SIZE);
  used_pages -= ((len % MPAGE_SIZE) == 0) ? 0 : 1;
  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

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

static void flush_freed_pages(void)
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

static void protect_pages(void *p, size_t len, int writeable)
{
  if (len & (page_size - 1)) {
    len += page_size - (len & (page_size - 1));
  }

  mprotect(p, len, (writeable ? (PROT_READ | PROT_WRITE) : PROT_READ));
}


#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

static unsigned long determine_max_heap_size(void) {
  struct rlimit *rlim = malloc(sizeof(struct rlimit));
  unsigned long retval = 0;

  getrlimit(RLIMIT_DATA, rlim);
  retval = rlim->rlim_cur;
  return (retval == RLIM_INFINITY) ? (1024 * 1024 * 1024) : retval;
}

#endif

