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
/* Collector selection. Change the definitions of these to set or unset the  */
/* particular collector you want.                                            */
/*****************************************************************************/

/* 
   These are the various collector options you can choose:

   Automatic Accounting (these two are mutually exclusive)
     NEWGC_BTC_ACCOUNT
     NEWGC_PRECISE_ACCOUNT

   Manual accounting / tracing (can be used with neither or either of the above)
     NEWGC_MANUAL_ACCOUNT

   A Split Generation 0 (theoretically improves performance)
     NEWGC_SPLIT_GEN0 (FIXME: NOT IMPLEMENTED YET)

   Heap Debugging Support
     NEWGC_HEAP_DEBUGGING (FIXME: NOT IMPLEMENTED YET)
*/

/*****************************************************************************/
/* Constant declarations. These can maybe be changed without destroying the  */
/* system, but I wouldn't bet on it. So change AYOR.                         */
/*****************************************************************************/
#define GENERATIONS 		  3
#define COMPUTE_COLLECTION_TOP(x) ((x % 100)==0) ? 2 : (((x % 15)==0) ? 1 : 0)
#define GEN0_SIZE		  (16 * 1024 * 1024)
#define OWNER_TABLE_GROW_AMT      10
#define UNION_CACHE_SIZE	  10
#define LOG_WORD_SIZE             2
#define LOG_MPAGE_SIZE		  14
#define PREEMPT_COLLECTION_AROUND (1024 * 1024)

#if defined(__APPLE__)&&defined(__ppc__)&&defined(__MACH__) && !defined(OS_X)
# define OS_X
#endif

/*****************************************************************************/
/* Global structure definitions                                              */
/*****************************************************************************/

struct objhead {
  unsigned int		owner : ((8 << LOG_WORD_SIZE) - (4 + LOG_MPAGE_SIZE));
  unsigned int 		type : 3;
  unsigned int          mark : 1;
  unsigned int          size : LOG_MPAGE_SIZE;
};

struct mpage {
  unsigned int		size;
  unsigned int          previous_size;
  unsigned char 	generation;
  unsigned char         back_pointers;
  unsigned char         big_page;
  unsigned char         page_type;
#ifdef NEWGC_MANUAL_ACCOUNT
  int			man_owner;
#endif
  struct mpage         *next;
  struct mpage         *prev;
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
/* External routines we use and/or define here                               */
/*****************************************************************************/

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

#include "my_qsort.c"

/*****************************************************************************/
/* Global variables. Probably shouldn't mess with these, either.             */
/*****************************************************************************/
static struct mpage		       *page_map[1 << USEFUL_ADDR_BITS];
static void			      **gen0_alloc_region = NULL;
static void			      **gen0_alloc_current = NULL;
static void 			      **gen0_alloc_end = NULL;
static unsigned long			gen0_bigpages_size = 0;
static struct mpage		       *pages[GENERATIONS][MPAGE_TYPES];
static Size_Proc			size_table[NUMBER_OF_TAGS];
static Mark_Proc			mark_table[NUMBER_OF_TAGS];
static Fixup_Proc			fixup_table[NUMBER_OF_TAGS];
static unsigned long			collection_number = 0;
static unsigned int			collection_top = 0;
static unsigned int			collection_full = 0;
static struct mpage 		       *collection_from_pages = NULL;
static int				running_finalizers = 0;
static void 			       *park[2];

/*****************************************************************************/
/* Internal debugging stuff                                                  */
/*****************************************************************************/

#if 0
static FILE *dump;

#define GC_DEBUG(args...) { fprintf(dump, args); fflush(dump); }

static void INIT_DEBUG_FILE() {
  char *filename = malloc(8 * sizeof(char));
  static int collections = 0;

  filename[0] = 'g'; filename[1] = 'c'; filename[2] = 'l';
  filename[3] = 'o'; filename[4] = 'g'; filename[7] = 0;
  filename[5] = '0' + (collections / 10);
  filename[6] = '0' + (collections % 10);
  dump = fopen(filename, "a");
  collections += 1;
}

#define CLOSE_DEBUG_FILE() fclose(dump)

static void dump_region(void **start, void **end)
{
  while(start < end) {
    GC_DEBUG("%.8lx: %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx\n", 
             start, *start, *(start + 1), *(start + 2), *(start + 3), 
             *(start + 4), *(start + 5), *(start + 6), *(start + 7));
    start += 8;
  }
  GC_DEBUG("\n\n");
}

static void DUMP_HEAP()
{
  struct mpage *page;
  short i, j;

  GC_DEBUG("Generation 0: (%p - %p)\n", gen0_alloc_region,
           gen0_alloc_end);
  dump_region((void**)gen0_alloc_region, gen0_alloc_current);
  for(i = 0; i < GENERATIONS; i++)
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next) {
        GC_DEBUG("Page %p (generation %i, type %i, bigpage %i)\n", 
                 page, page->generation, page->page_type, page->big_page);
        dump_region((void**)page, 
                    (void**)((char*)page + (page->big_page ? page->size
                                            : MPAGE_SIZE)));
      }
        
}

#else
#define GC_DEBUG(args...) /* */
#define INIT_DEBUG_FILE() /* */
#define CLOSE_DEBUG_FILE() /* */
#define DUMP_HEAP() /* */
#endif

/*****************************************************************************/
/* OS-Specific allocation routines                                           */
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
  used_pages += (len / MPAGE_SIZE) + (((len % MPAGE_SIZE) == 0) ? 0 : 1);

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
  used_pages -= (len / MPAGE_SIZE) + (((len % MPAGE_SIZE) == 0) ? 0 : 1);
}

#define CHECK_USED_AGAINST_MAX(len) check_used_against_max(len)
#define LOGICALLY_ALLOCATING_PAGES(len) /* empty */
#define ACTUALLY_ALLOCATING_PAGES(len) /* empty */
#define LOGICALLY_FREEING_PAGES(len) free_used_pages(len)
#define ACTUALLY_FREEING_PAGES(len) /* empty */

/* Windows */

#if _WIN32
# include "vm_win.c"
# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/
/* OS/X */

#ifdef OS_X
# define TEST 0
static void designate_modified(void *p);
# include "vm_osx.c"
# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/

/* OSKit */

#if OSKIT
# include "vm_osk.c"
# define MALLOCATOR_DEFINED
#endif

/******************************************************************************/

/* Default: mmap */

#ifndef MALLOCATOR_DEFINED
# include "vm_mmap.c"
#endif

/*****************************************************************************/
/* Routines dealing with various runtime execution stacks                    */
/*****************************************************************************/
void **GC_variable_stack;
static unsigned long stack_base;

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


void GC_mark_variable_stack(void **var_stack, long delta, void *limit) 
{
  traverse_stack(var_stack, delta, limit, gcMARK);
}

void GC_fixup_variable_stack(void **var_stack, long delta, void *limit) 
{
  traverse_stack(var_stack, delta, limit, gcFIXUP);
} 

/*****************************************************************************/
/* Routines used in automatic memory accounting                              */
/*****************************************************************************/
#if defined(NEWGC_BTC_ACCOUNT) || defined(NEWGC_PRECISE_ACCOUNT)

struct ot_entry {
  Scheme_Custodian     *originator;
  Scheme_Custodian    **members;
#if defined(NEWGC_PRECISE_ACCOUNT)
  int			union_cache_with[UNION_CACHE_SIZE];
  int			union_cache_result[UNION_CACHE_SIZE];
#endif
  unsigned int		num_members, members_size;
  unsigned int		memory_use[GENERATIONS]; /* given in *WORDS* */
};

static struct ot_entry **owner_table = NULL;
static unsigned int owner_table_top = 0;
static unsigned int current_mark_owner = 0;

inline static int marked(void*); 
inline static void modify_immobiles_owner(int, int);
inline static void modify_thread_owner(int, int);
inline static void modify_finalizer_owner(int, int); 

inline static void account_memory(int set, int amount)
{
  owner_table[set]->memory_use[collection_top] += amount;
}

inline static int create_blank_owner_set(void)
{
  int i;

  /* see if there's a blank spot to put this */
  for(i = 1; i < owner_table_top; i++)
    if(!owner_table[i]) {
      owner_table[i] = malloc(sizeof(struct ot_entry));
      bzero(owner_table[i], sizeof(struct ot_entry));
      return i;
    }

  /* there wasn't */
  owner_table_top += OWNER_TABLE_GROW_AMT;
  owner_table = realloc(owner_table, 
			(owner_table_top * sizeof(struct ot_entry *)));
  bzero((char*)owner_table + (sizeof(struct ot_entry*)
			      * (owner_table_top - OWNER_TABLE_GROW_AMT)),
	OWNER_TABLE_GROW_AMT * sizeof(struct ot_entry *));
  return create_blank_owner_set();
}

inline static void add_owner_set_member(int owner, Scheme_Custodian *member)
{
  /* make absolutely sure we have enough space available to us */
  if(owner_table[owner]->num_members == owner_table[owner]->members_size) {
    owner_table[owner]->members_size += 5;
    owner_table[owner]->members = realloc(owner_table[owner]->members,
					  (owner_table[owner]->members_size *
					   sizeof(Scheme_Custodian*)));
  }
  owner_table[owner]->members[owner_table[owner]->num_members++] = member;
}

inline static int custodian_member_owner_set(void *cust, int set)
{
  int i;

  for(i = 0; i < owner_table[set]->num_members; i++)
    if(owner_table[set]->members[i] == cust) 
      return 1;
  return 0;
}

inline static int custodian_to_owner_set(Scheme_Custodian *cust)
{
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *temp = cust;
  int i;

  /* see if we've already done this one */
  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i] && (owner_table[i]->originator == cust))
      return i;
  /* we haven't, so we need to make a new one */
  i = create_blank_owner_set();
  /* initialize the fields */
  owner_table[i]->originator = cust;
  while(temp) {
    add_owner_set_member(i, temp);
    box = temp->parent; temp = box ? box->u.two_ptr_val.ptr1 : NULL;
  }
  return i;
}

inline static unsigned long custodian_get_memory_use(void *custodian, 
						     int all_gens)
{
  unsigned long retval = 0;
  int i;
  
/*   printf("Determining memory use for custodian %p\n", custodian); */
  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i] && custodian_member_owner_set(custodian, i)) {
/*       printf("  ... owner set %i\n", i); */
      if(all_gens) {
	int j;
	for(j = 0; j < GENERATIONS; j++)
	  retval += owner_table[i]->memory_use[j];
      } else retval += owner_table[i]->memory_use[(GENERATIONS-1)];
    }
  return gcWORDS_TO_BYTES(retval);
}

inline static int current_owner()
{
  static int has_gotten_root_custodian = 0;
  Scheme_Custodian *c;

  if(!owner_table) {
    owner_table = malloc(OWNER_TABLE_GROW_AMT * sizeof(struct ot_entry*));
    bzero(owner_table, OWNER_TABLE_GROW_AMT * sizeof(struct ot_entry*));
    if(create_blank_owner_set() != 1) {
      printf("Something extremely weird (and bad) has happened.\n"); abort();
    }
  }

  if(!scheme_current_thread || !scheme_current_thread->config)
    return 1;

  c = (Scheme_Custodian*)scheme_get_param(scheme_config, MZCONFIG_CUSTODIAN);
  if(!has_gotten_root_custodian) {
    has_gotten_root_custodian = 1;
    owner_table[1]->originator = c;
    add_owner_set_member(1, c);
    return 1;
  }
  return custodian_to_owner_set(c);
}

inline static void free_owner_set(int set)
{
  int parent = -1;

  /* see if this owner set has an obvious parent */
  if(owner_table[set]->originator) {
    if(owner_table[set]->originator->parent->u.two_ptr_val.ptr1) {
      Scheme_Custodian *p = 
	owner_table[set]->originator->parent->u.two_ptr_val.ptr1;
      int i;
      
      for(i = 1; i < owner_table_top; i++) 
	if(owner_table[i] && (owner_table[i]->originator == p))
	  parent = i;
    }
  }
  /* move the roots to the appropriate parent */
  modify_immobiles_owner(set, parent);
  modify_finalizer_owner(set, parent);
  modify_thread_owner(set, parent);
  /* now free anything that's left */
  free(owner_table[set]->members);
  free(owner_table[set]);
  owner_table[set] = NULL;
}

inline static void repair_owner_table(void)
{
  int i;

/*   printf("Repairing owner table\n"); */
  for(i = 1; i < owner_table_top; i++)
    if(owner_table[i]) {
      unsigned long j, memuse = 0;
      int cur, nextfree;

      if(!marked(owner_table[i]->originator)) {
	owner_table[i]->originator = NULL;
/* 	printf("  ... Killing originator at set %i\n", i); */
      } else {
	gcFIXUP(owner_table[i]->originator);
/* 	printf("  ... repaired originator at set %i\n", i); */
      }

      for(cur = nextfree = 0; cur < owner_table[i]->num_members; cur++) {
	if(marked(owner_table[i]->members[cur])) {
	  owner_table[i]->members[nextfree++]
	    = GC_resolve(owner_table[i]->members[cur]);
/* 	  printf("  ... saving constituent %p\n",  */
/* 		 owner_table[i]->members[nextfree-1]); */
	}
      }

      for(j = 0; j < GENERATIONS; j++)
	memuse += owner_table[i]->memory_use[j];
      if((memuse == 0) && !owner_table[i]->originator) {
/* 	printf("  ... killing owner set at %i\n", i); */
	free_owner_set(i);
	owner_table[i] = NULL;
      } 
      if(owner_table[i])
	for(; nextfree < owner_table[i]->members_size; nextfree++)
	  owner_table[i]->members[nextfree] = NULL;
    }
/*   printf("  ... finished\n"); */
}

#ifdef NEWGC_PRECISE_ACCOUNT
inline static void cache_union_result(int set1, int set2, int result)
{
  int i;

  for(i = 0; i < UNION_CACHE_SIZE; i++)
    if(owner_table[set1]->union_cache_with[i] == -1) {
      owner_table[set1]->union_cache_with[i] = set2;
      owner_table[set1]->union_cache_result[i] = result;
      return;
    }
  owner_table[set1]->union_cache_with[UNION_CACHE_SIZE-1] = set2;
  owner_table[set1]->union_cache_result[UNION_CACHE_SIZE-1] = result;
}

inline static int lookup_cache_result(int set1, int set2)
{
  int i;
  
  for(i = 0; i < UNION_CACHE_SIZE; i++)
    if(owner_table[set1]->union_cache_with[i] == set2) {
      if(i != 0) {
	struct ot_entry *ote = owner_table[set1];
	int temp1, temp2;
	/* heuristically move this forward */
	temp1 = ote->union_cache_with[i-1]; temp2 =ote->union_cache_result[i-1];
	ote->union_cache_with[i-1] = ote->union_cache_with[i];
	ote->union_cache_result[i-1] = ote->union_cache_result[i];
	ote->union_cache_with[i] = temp1; ote->union_cache_result[i] = temp2;
	i--;
      }
      return owner_table[set1]->union_cache_result[i];
    }
  return -1;
}

inline static int owner_set_union(int set1, int set2)
{
  int result, i;

  /* force these sets to be in a standard order */
  if(set1 > set2) { int temp = set1; set1 = set2; set2 = temp; }
  /* see if this result has been cached */
  if((result = lookup_cache_result(set1, set2)) != -1) return result;
  
  /* we have to do this the hard way */
  result = create_blank_owner_set();
  for(i = 0; i < owner_table[set1]->num_members; i++)
    if(owner_table[set1]->members[i])
      add_owner_set_member(result, owner_table[set1]->members[i]);
  for(i = 0; i < owner_table[set2]->num_members; i++)
    if(owner_table[set2]->members[i])
      if(!custodian_member_owner_set(owner_table[set2]->members[i], result))
	add_owner_set_member(result, owner_table[set2]->members[i]);
  /* now that this all is set up, make sure we haven't created a 
     dumplicate entry */
  for(i = 0; i < owner_table_top; i++)
    if(owner_table[i])
      if(owner_table[i]->num_members == owner_table[result]->num_members) {
	int j = 0, match = 1;
	
	for(j = 0; j < owner_table[i]->num_members; j++)
	  if(!custodian_member_owner_set(owner_table[i]->members[j], result))
	    { match = 0; break; }
	if(match) {
	  /* this is already there, so remove our new one */
	  free(owner_table[result]->members);
	  free(owner_table[result]);
	  owner_table[result] = NULL;
	  cache_union_result(set1, set2, i);
	  return i;
	}
      }
  cache_union_result(set1, set2, result);
  return result;
}

inline static void REPAIR_UNION_CACHES(void) 
{
  int i, j;

  for(i = 0; i < owner_table_top; i++)
    if(owner_table[i]) 
      for(j = 0; j < UNION_CACHE_SIZE; j++)
	if(!owner_table[owner_table[i]->union_cache_result[j]] ||
	   !owner_table[owner_table[i]->union_cache_with[j]]) {
	  owner_table[i]->union_cache_with[j] = -1;
	  owner_table[i]->union_cache_result[j] = -1;
	}

}
#else
# define REPAIR_UNION_CACHES() /* */
#endif

# define OWNER_FIELD int owner;
# define INITIALIZE_OWNER_FIELD(o) o->owner = current_owner(NULL)
# define REPAIR_OWNER_TABLE() { repair_owner_table(); REPAIR_UNION_CACHES(); }
#else
# define OWNER_FIELD /* */
# define INITIALIZE_OWNER_FIELD(o) /* */
# define REPAIR_OWNER_TABLE() /* */
#endif

/*****************************************************************************/
/* Routines used in manual memory accounting				     */
/*****************************************************************************/
#ifdef NEWGC_MANUAL_ACCOUNT

static Scheme_Object *man_account_key = NULL;
static int track_manual_owner_info = 0;
static Scheme_Object **manual_alias_table = NULL;
static int manual_alias_table_top = 0;
inline static int marked(void*); 

void scheme_init_manual_memory(void *key)
{
  man_account_key = key;
  track_manual_owner_info = 1;
}

void *scheme_new_tracking_val(void)
{
  int i, old_size;

  for(i = 1; i < manual_alias_table_top; i++)
    if(!manual_alias_table[i]) {
      manual_alias_table[i] = (Scheme_Object*)1;
      return scheme_make_integer_value_from_unsigned(i);
    }

  /* we have to grow the table */
  old_size = manual_alias_table_top;
  manual_alias_table_top += OWNER_TABLE_GROW_AMT;
  manual_alias_table = realloc(manual_alias_table,
			       (manual_alias_table_top*sizeof(Scheme_Object*)));
  bzero((char*)manual_alias_table + (sizeof(Scheme_Object*) * old_size),
	OWNER_TABLE_GROW_AMT * sizeof(Scheme_Object*));
  return scheme_new_tracking_val();
}

int scheme_alias_tracking_val(void *val1, void *val2)
{
  int index = SCHEME_INT_VAL(val1);

  if((index >= manual_alias_table_top) || !manual_alias_table[index])
    return 0;
  if((int)manual_alias_table[index] != 1)
    return -1;
  manual_alias_table[index] = val2;
  return 1;
}

inline static void repair_tracking_vals(void)
{
  int i;

  for(i = 1; i < manual_alias_table_top; i++) {
    if(manual_alias_table[i] && ((unsigned long)manual_alias_table[i] != 1)) {
      if(marked(manual_alias_table[i])) {
	gcFIXUP(manual_alias_table[i]);
      } else manual_alias_table[i] = (void*)1;
    }
  }
  gcFIXUP(man_account_key);
}

unsigned long scheme_get_tracking_val_memory(void *val)
{
  int i, j, man_owner = -1;
  unsigned long total_memuse = 0;
  void **start, **end;
  struct mpage *page;

  if(SCHEME_INTP(val)) man_owner = SCHEME_INT_VAL(val); else {
    for(i = 0; i < manual_alias_table_top; i++) {
      if(manual_alias_table[i] == val) {
	man_owner = i;
	break;
      }
    }
  }

  if(man_owner == -1) 
    return 1;

  for(i = 1; i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++) 
      for(page = pages[i][j]; page; page = page->next) {
	if(page->man_owner == man_owner)
	  total_memuse += (page->size - PAGE_BYTE_OVERHEAD);
      }
  for(page = pages[0][MPAGE_BIG]; page; page = page->next)
    if(page->man_owner == man_owner)
      total_memuse += (page->size - PAGE_BYTE_OVERHEAD);
  start = gen0_alloc_region + PAGE_WORD_OVERHEAD;  end = gen0_alloc_current;
  while(start < end) {
    struct objhead *info = (struct objhead *)start;
    if(info->owner == man_owner) 
      total_memuse += gcWORDS_TO_BYTES(info->size);
    start += info->size;
  }
  return total_memuse;
}

inline static int get_current_manual_owner(void)
{
  if(track_manual_owner_info) {
    /* this stuff is pulled from fun.c, around line 2310 (continuation_mark) */
    Scheme_Thread *p = scheme_current_thread;
    Scheme_Cont *cont = NULL;
    long findpos = (long)MZ_CONT_MARK_STACK;

    while(findpos--) {
      Scheme_Cont_Mark *find;
      long pos;

      if(cont) {
	find = cont->cont_mark_stack_copied;
	pos = findpos;
      } else {
	Scheme_Cont_Mark *seg;
	seg = 
	  p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
	pos = findpos & SCHEME_MARK_SEGMENT_MASK;
	find = seg;
      }

      if(find[pos].cached_chain) break; else {
	if(find[pos].key == man_account_key)
	  return SCHEME_INT_VAL(find[pos].val);
      }
    }
    return 0;
  } else return 0;
}

# define REPAIR_MANUAL_ALIAS_TABLE() repair_tracking_vals()
#else 
# define REPAIR_MANUAL_ALIAS_TABLE() /* */
#endif

/*****************************************************************************/
/* Routines for root pointers                                                */
/*****************************************************************************/

/* the structure of a root */
struct root {
  unsigned long 	count;
  unsigned long 	size;
  unsigned long        *root;
  unsigned int 		nothing_new;
};

/* the root set itself */
static struct root *roots = NULL;

void GC_add_roots(void *start, void *end) 
{
  if(roots == NULL) {
    struct root *newroot = (struct root *)malloc(sizeof(struct root));
    roots = newroot;
    roots->count = 0;
    roots->size = 0;
    roots->root = NULL;
    roots->nothing_new = 0;
  }

  if(roots->count >= roots->size) {
    unsigned long oldsize = roots->size;
    roots->size = roots->size ? 2 * roots->size : 500;
    roots->root = realloc(roots->root, sizeof(unsigned long) *
			       (roots->size + 1));
    bzero((void*)((char*)roots->root + (oldsize*sizeof(unsigned long))),
	  (roots->size - oldsize) * sizeof(unsigned long));
  }

  roots->root[roots->count++] = (unsigned long)start;
  roots->root[roots->count++] = ((unsigned long)end - 4);
}

static int compare_roots(const void *a, const void *b) 
{
  return (*(unsigned long *)a < *(unsigned long *)b) ? -1 : 1;
}

static void sort_and_merge_roots(struct root *root) 
{
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

inline static void mark_roots(struct root *roots) 
{
  unsigned long j;

  if(roots) {
    sort_and_merge_roots(roots);
    for(j = 0; j < roots->count; j += 2) {
      void **s = (void**)roots->root[j];
      void **e = (void**)roots->root[j + 1];
      
      while(s < e) 
	gcMARK(*(s++));
    }
  }
}

inline static void repair_roots(struct root *roots) 
{
  if(roots) {
    unsigned long j;
    
    for(j = 0; j < roots->count; j += 2) {
      void **s = (void**)roots->root[j];
      void **e = (void**)roots->root[j + 1];
      while(s < e) gcFIXUP(*(s++));
    }
  }
}

/*****************************************************************************/
/* Routines dealing with weak items (arrays and boxes)                       */
/*****************************************************************************/
struct weak_box {
  unsigned short 	type;
  short			keyex;
  void		       *val;
  void                **secondary_erase;
  int			soffset;
  struct weak_box      *next;
};

struct weak_array {
  unsigned short	type;
  short			keyex;
  long			count;
  void		       *replace_val;
  struct weak_array    *next;
  void 		       *data[1];
};

static unsigned short weak_box_tag;
static unsigned short weak_array_tag = 256;
inline static int marked(void*); 
inline static struct mpage *find_page(void *p);
static struct weak_box *weak_boxes = NULL;
static struct weak_array *weak_arrays = NULL;
static int doing_memory_accounting_pass = 0;

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

void *GC_malloc_weak_box(void *p, void **secondary, int soffset) 
{
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

static int size_weak_box(void *p) 
{
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

static int mark_weak_box(void *p) 
{
  struct weak_box *wb = (struct weak_box *)p;
  gcMARK(wb->secondary_erase);
  if(wb->val && !doing_memory_accounting_pass) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

static int repair_weak_box(void *p) 
{
  struct weak_box *wb = (struct weak_box *)p;
  gcFIXUP(wb->val);
  gcFIXUP(wb->secondary_erase);
/*   if(!marked(wb->val)) { */
/*     wb->val = NULL; */
/*     if(wb->secondary_erase)  */
/*       *(wb->secondary_erase + wb->soffset) = NULL; */
/*     wb->secondary_erase = NULL; */
/*   } else {  */
/*     gcFIXUP(wb->val); */
/*     GC_DEBUG("Keeping weak box %p (reference %p)\n", wb, wb->val); */
/*   } */
  return gcBYTES_TO_WORDS(sizeof(struct weak_box));
}

inline static void reset_appropriate_weak_boxes(void)
{
  struct weak_box *work;

  for(work = weak_boxes; work; work = work->next) {
    struct mpage *page = find_page(work->val);

    GC_DEBUG("Checking box %p", work);
    if(!page || (page->generation == 0)) {
      work->val = NULL;
      if(work->secondary_erase)
	*(work->secondary_erase + work->soffset) = NULL;
      work->secondary_erase = NULL;
      GC_DEBUG(" (KILL)\n");
    } else GC_DEBUG(" (SAVE)\n");
  }
  weak_boxes = NULL;
}

static int size_weak_array(void *p) 
{
  struct weak_array *wa = (struct weak_array *)p;
  return gcBYTES_TO_WORDS(sizeof(struct weak_array)
                          + ((wa->count - 1) * sizeof(void*)));
}

static int mark_weak_array(void *p) 
{
  struct weak_array *wa = (struct weak_array *)p;
  gcMARK(wa->replace_val);
  if(!doing_memory_accounting_pass) {
    wa->next = weak_arrays;
    weak_arrays = wa;
  }
  return gcBYTES_TO_WORDS(sizeof(struct weak_array)
                          + ((wa->count - 1) * sizeof(void*)));
}

static int repair_weak_array(void *p) 
{
  struct weak_array *a = (struct weak_array *)p;
  int i;
  void **data;

  gcFIXUP(a->replace_val);

  data = a->data;
  for (i = a->count; i--; ) {
    if (data[i])
      gcFIXUP(data[i]);
  }

  return gcBYTES_TO_WORDS(sizeof(struct weak_array) 
			  + ((a->count - 1) * sizeof(void *)));
/*   struct weak_array *wa = (struct weak_array *)p; */
/*   void **data = wa->data; */
/*   int i; */

/*   gcFIXUP(wa->replace_val); */
/*   for(i = wa->count; i--; ) {  */
/*     if(data[i] && !marked(data[i])) { */
/*       printf("NEWGC: Replacing old weak array entry with %p\n", wa->replace_val); */
/*       data[i] = wa->replace_val; */
/*     } else if(data[i]) gcFIXUP(data[i]); */
/*   } */
/*   return gcBYTES_TO_WORDS(sizeof(struct weak_array) */
/*                           + ((wa->count - 1) * sizeof(void*))); */
}

inline static void reset_appropriate_weak_arrays()
{
  struct weak_array *work;

  for(work = weak_arrays; work; work = work->next) {
    void **data;
    int i;
    
    data = work->data;
    for (i = work->count; i--; ) {
      void *p = data[i];
      struct mpage *page = find_page(p);

      if (p && (!page || (page->generation == 0))) {
	data[i] = work->replace_val;
      } 
    }
  }
  weak_arrays = NULL;
}

/*****************************************************************************/
/* Routines dealing with the page map; these make page lookups AFAP          */
/*****************************************************************************/

inline static void add_page_to_page_map(struct mpage *page) 
{
  long size_left = page->big_page ? page->size : MPAGE_SIZE;
  void *p = page;

  while(size_left > 0) {
    page_map[ADDR_BITS(p)] = page;
    size_left -= MPAGE_SIZE;
    p += MPAGE_SIZE;
  }
}

inline static void remove_page_from_page_map(struct mpage *page) 
{
  long size_left = page->big_page ? page->size : MPAGE_SIZE;
  void *p = page;

  while(size_left > 0) {
    page_map[ADDR_BITS(p)] = NULL;
    size_left -= MPAGE_SIZE;
    p += MPAGE_SIZE;
  }
}

inline static struct mpage *find_page(void *p) 
{
  return page_map[ADDR_BITS(p)];
}

/*****************************************************************************/
/* Thread list routines; these are not defined (for the most part) unless    */
/* we're doing accounting                                                    */
/*****************************************************************************/
#if defined (NEWGC_PRECISE_ACCOUNT) || defined(NEWGC_BTC_ACCOUNT)
struct thread {
  OWNER_FIELD
  void		       *thread;
  struct thread *next, *prev;
};

static struct thread *threads = NULL;

inline static void repair_thread_list(void)
{
  struct thread *work = threads;

  while(work) {
    if(!marked(work->thread)) {
      struct thread *next = work->next;

      /* this thread has been collected, so drop this information */
      if(work->prev) work->prev->next = next; else threads = next;
      if(work->next) work->next->prev = work->prev;
      free(work);
      work = next;
    } else { gcFIXUP(work->thread); work = work->next; }
  }  
}

inline static void mark_threads_for_owner(int owner)
{
  struct thread *work;

  for(work = threads; work; work = work->next)
    if(!owner || (work->owner == owner)) {
      gcMARK(work->thread);
    }
}

inline static void modify_thread_owner(int set_from, int set_to)
{
  struct thread *work;

  for(work = threads; work; work = work->next)
    if(work->owner == set_from)
      work->owner = set_to;
}

# define REPAIR_THREAD_LIST() repair_thread_list()
#else 
# define REPAIR_THREAD_LIST() /* */
#endif

void GC_register_thread(void *t, void *c) 
{
#if defined(NEWGC_PRECISE_ACCOUNT) || defined(NEWGC_BTC_ACCOUNT)
  struct thread *work;

  /* see if this is an owner reset */
  for(work = threads; work; work = work->next)
    if(work->thread == t) {
      INITIALIZE_OWNER_FIELD(work);
      return;
    }
  /* it isn't, so add the new item to the list */
  work = (struct thread *)malloc(sizeof(struct thread));
  INITIALIZE_OWNER_FIELD(work);
  work->thread = t;
  work->next = threads;
  work->prev = NULL;
  if(threads) threads->prev = work;
  threads = work;
#endif
}

/*****************************************************************************/
/* Allocation routines                                                       */
/*****************************************************************************/
static char *zero_sized[4];

#ifdef NEWGC_MANUAL_ACCOUNT
# define MANUAL_SET_OWNER(info) info->owner = get_current_manual_owner()
# define MANUAL_SET_BIGPAGE_OWNER(p) p->man_owner = get_current_manual_owner()
# define MANUAL_SET_COPYPAGE_OWNER(p,o) p->man_owner = o
# define OWNER_OK(p,i) (p->man_owner == ((struct objhead *)i)->owner)
#else
# define MANUAL_SET_OWNER(i) /* */
# define MANUAL_SET_BIGPAGE_OWNER(p) /* */
# define MANUAL_SET_COPYPAGE_OWNER(p,o) /* */
# define OWNER_OK(p,i) 1
#endif

static void *allocate_big(size_t size_in_words, int type) 
{
  unsigned long size_in_bytes;
  struct mpage *bpage;

  size_in_words += PAGE_WORD_OVERHEAD;
  size_in_bytes = gcWORDS_TO_BYTES(size_in_words);
  gen0_bigpages_size += size_in_bytes;
  if((gen0_bigpages_size + ((unsigned long)gen0_alloc_current - 
                            (unsigned long)gen0_alloc_region)) > GEN0_SIZE) {
    garbage_collect(0);
    gen0_bigpages_size = size_in_bytes;
  }

  bpage = malloc_pages(size_in_bytes, MPAGE_SIZE);
  bpage->size = size_in_bytes;
  bpage->big_page = 1;
  bpage->page_type = type;
  bpage->next = pages[0][MPAGE_BIG];

  add_page_to_page_map(bpage);
  if(pages[0][MPAGE_BIG])
    pages[0][MPAGE_BIG]->prev = bpage;
  pages[0][MPAGE_BIG] = bpage;
  MANUAL_SET_BIGPAGE_OWNER(bpage);
  return (void*)((unsigned long)bpage + PAGE_BYTE_OVERHEAD + 4);
}

inline static void *allocate(size_t size_in_bytes, int type) 
{
  if(size_in_bytes) {
    size_t size_in_words = gcBYTES_TO_WORDS(size_in_bytes) + 1;

    if(size_in_words < MAX_OBJECT_WORD_SIZE) {
      struct objhead *info;
      void *retval = (void*)gen0_alloc_current;

      gen0_alloc_current += size_in_words;
      if(gen0_alloc_current >= gen0_alloc_end) {
        gen0_alloc_current -= size_in_words;
        garbage_collect(0);
        retval = gen0_alloc_current;
        gen0_alloc_current += size_in_words;
      }
      info = (struct objhead *)retval;
      info->type = type;
      info->size = size_in_words;
      MANUAL_SET_OWNER(info);
      return (void*)((unsigned long)retval + 4);
    } else return allocate_big(size_in_words, type);
  } else return zero_sized;
}

#define SPACE_AVAIL(p,s) ((p->size + s) < MPAGE_SIZE)

#if defined(NEWGC_BTC_ACCOUNT)
# define CLEAR_OWNER_FIELD(p) ((struct objhead *)(p))->owner = 0
#else
# define CLEAR_OWNER_FIELD(p) /* */
#endif

inline static void *copy_bits(void *from, unsigned long size, 
			      short type, short gen) 
{
  unsigned long size_in_bytes = gcWORDS_TO_BYTES(size);
  struct mpage *work = pages[gen][type];
  void *retval;

  while(work && !(SPACE_AVAIL(work, size_in_bytes) && OWNER_OK(work, from)))
    work = work->next;

  if(work) {
    retval = (void*)((unsigned long)work + work->size);
  } else {
    work = (struct mpage *)malloc_pages(MPAGE_SIZE, MPAGE_SIZE);
    work->next = pages[gen][type];
    work->generation = gen;
    work->page_type = type;
    work->size = work->previous_size = PAGE_BYTE_OVERHEAD;
    MANUAL_SET_COPYPAGE_OWNER(work, ((struct objhead *)from)->owner);
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
  CLEAR_OWNER_FIELD(retval);
  return retval;
}

void *GC_malloc(size_t size)             { return allocate(size, MPAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t size) { return allocate(size, MPAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t size) {return allocate(size, MPAGE_XTAGGED);}
void *GC_malloc_array_tagged(size_t size) {return allocate(size, MPAGE_TARRAY);}
void *GC_malloc_atomic(size_t size)     { return allocate(size, MPAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t size)       { return malloc(size); }

void *GC_malloc_allow_interior(size_t size_in_bytes) 
{
  return allocate_big(gcBYTES_TO_WORDS(size_in_bytes) + 1, MPAGE_ARRAY);
}

void GC_free(void *p) 
{
  struct mpage *page = find_page(p);

  if(page && page->big_page) {
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

inline static void reset_nursery(void)
{
  gen0_bigpages_size = 0;
  bzero(gen0_alloc_region, ((unsigned long)gen0_alloc_current - 
			    (unsigned long)gen0_alloc_region));
  gen0_alloc_current = gen0_alloc_region + PAGE_WORD_OVERHEAD;
}

/*****************************************************************************/
/* Routines dealing with finalizers and finalization                         */
/*****************************************************************************/
struct finalizer {
  char			eager_level;
  char 			tagged;
  void		       *p;
  GC_finalization_proc  f;
  void		       *data;
  struct finalizer     *next;
  OWNER_FIELD
};

struct weak_finalizer {
  void		       *p;
  int			offset;
  void		       *saved;
  struct weak_finalizer *next;
};

static struct finalizer	*finalizers = NULL;
static struct finalizer	*run_queue = NULL, *last_in_queue = NULL;;
static struct weak_finalizer *weak_finalizers = NULL;
inline static void propogate_all_marks(void); 

void GC_set_finalizer(void *p, int tagged, int level,
                      GC_finalization_proc f, void *data,
                      GC_finalization_proc *oldf, void **olddata) 
{
  struct mpage *page = find_page(p);
  struct finalizer *fnl, *prev;

  if(!page) {
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
	INITIALIZE_OWNER_FIELD(fnl);
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
  INITIALIZE_OWNER_FIELD(fnl);
  finalizers = fnl;
}

void GC_finalization_weak_ptr(void **p, int offset) 
{
  struct weak_finalizer *wfnl;

  park[0] = p;
  wfnl = GC_malloc_atomic(sizeof(struct weak_finalizer));
  p = park[0]; park[0] = NULL;

  wfnl->p = p;
  wfnl->next = weak_finalizers;
  wfnl->offset = offset * sizeof(void*);
  weak_finalizers = wfnl;
}

inline static struct finalizer *mark_finalizers(struct finalizer *fnl, 
						int mark_ptr) 
{
  struct finalizer *work;

  for(work = fnl; work; work = work->next)
    { gcMARK(work->data); if(mark_ptr) gcMARK(work->p); gcMARK(work); }
  for(work = GC_resolve(fnl); work; work = work->next)
    gcFIXUP(work->next);

  return GC_resolve(fnl);
}

#if defined(NEWGC_BTC_ACCOUNT) || defined(NEWGC_PRECISE_ACCOUNT)
inline static void mark_finalizers_for_owner(int owner) 
{
  struct finalizer *work;

  for(work = finalizers; work; work = work->next)
    if(!owner || (owner == work->owner)) {
      gcMARK(work->data);
      gcMARK(work->p);
    }
  for(work = run_queue; work; work = work->next)
    if(!owner || (owner == work->owner)) {
      gcMARK(work->data);
      gcMARK(work->p);
    }
}

inline static void modify_finalizer_owner(int set_from, int set_to)
{
  struct finalizer *work;

  for(work = finalizers; work; work = work->next)
    if(work->owner == set_from)
      work->owner = set_to;
  for(work = run_queue; work; work = work->next)
    if(work->owner == set_from)
      work->owner = set_to;
}
#endif

inline static void repair_finalizers(struct finalizer *fnl)
{
  for(; fnl; fnl = fnl->next)
    { gcFIXUP(fnl->data); gcFIXUP(fnl->p); }
}

inline static struct finalizer *check_finalizers(struct finalizer *fnl, 
						 int level)
{
  struct finalizer *work = fnl, *prev = NULL;

  /* this is a little odd. When the system passes us level '3', we do the 
     pre-level 3 marking out of the pointers, to order finalization. Level
     '4' is where we actually do the finalization moves for level 3 items */
  if((level < 3) || (level == 4)) {
    if(level == 4) level = 3;
    while(work) {
      if((work->eager_level == level) && !marked(work->p)) {
	struct finalizer *next = work->next;
	/* mark the pointer; we'll need it to run */
	GC_DEBUG("CFNL: Level %i finalizer %p on %p not marked, so marking\n",
		 work->eager_level, work, work->p);
	gcMARK(work->p);
	/* remove it from the current list of finalizers */
	if(prev) prev->next = work->next; else fnl = work->next;
	/* and add it to the run_queue */
	if(last_in_queue) last_in_queue = last_in_queue->next = work; else 
	  last_in_queue = run_queue = work;
	work->next = NULL;
	/* and loop */
	work = next;
      } else { prev = work; work = work->next; }
    }
  } else {
    struct finalizer *temp;
    int changed = 0;

    /* basically, for any object that's got a level three finalizer on it
       that hasn't been marked, we mark out of that object (though not the
       object itself). This makes sure that if two objects O1 and O2 both
       have finalizers, both have not yet been marked and O1 reaches O2,
       only O1 is finalized in this session */
    for(temp = fnl; temp; temp = temp->next)
      if(!marked(temp->p)) {
	GC_DEBUG("LVL3: %p is not marked, so we're marking out of it\n", 
		 temp->p);
	if(temp->tagged) mark_table[*(unsigned short*)temp->p](temp->p); else
	  GC_mark_xtagged(temp->p);
	changed = 1;
      }
    if(changed) propogate_all_marks();
  }

  return fnl;
}

inline static struct weak_finalizer *mark_weak_finalizers(struct weak_finalizer 
							  *wfnl)
{
  struct weak_finalizer *work;

  for(work = wfnl; work; work = work->next)
    gcMARK(work);
  for(work = GC_resolve(wfnl); work; work = work->next)
    gcFIXUP(work->next);
  return GC_resolve(wfnl);
}

inline static struct weak_finalizer *repair_weak_finalizers(struct 
							    weak_finalizer 
							    *wfnl)
{
  struct weak_finalizer *work = wfnl, *prev = NULL;

  while(work) {
    if(!marked(work->p)) {
      /* remove it from the current list of finalizers */
      if(prev) prev->next = work->next; else wfnl = work->next;
      /* just advance. this will be collected naturally */
      work = work->next;
    } else { gcFIXUP(work->p); prev = work; work = work->next; }
  }
  return wfnl;
}

inline static void zero_weak_finalizers(struct weak_finalizer *wfnl)
{
  for(; wfnl; wfnl = wfnl->next) {
    wfnl->saved = *(void**)((char*)GC_resolve(wfnl->p) + wfnl->offset);
    *(void**)((char*)GC_resolve(wfnl->p) + wfnl->offset) = NULL;
  }
}

inline static void reset_weak_finalizers(struct weak_finalizer *wfnl)
{
  for(; wfnl; wfnl = wfnl->next) {
    *(void**)((char*)GC_resolve(wfnl->p) + wfnl->offset) = wfnl->saved;
    gcMARK(wfnl->saved); wfnl->saved = NULL;
  }
}

/*****************************************************************************/
/* Immobile box routines                                                     */
/*****************************************************************************/
struct immobile_box {
  void 		       *p; /* this must be first or window.cc dies */
  struct immobile_box  *next;
  struct immobile_box  *prev;
  OWNER_FIELD
};

/* the actual list of immobile boxes */
static struct immobile_box *immobile_boxes = NULL;

void **GC_malloc_immobile_box(void *p) 
{
  struct immobile_box *ib = malloc(sizeof(struct immobile_box));
  ib->p = p;
  ib->next = immobile_boxes;
  ib->prev = NULL;
  INITIALIZE_OWNER_FIELD(ib);
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

inline static void mark_immobiles(struct immobile_box *ib) 
{
  for(; ib; ib = ib->next)
    gcMARK(ib->p);
}

#if defined(NEWGC_BTC_ACCOUNT) || defined(NEWGC_PRECISE_ACCOUNT)
inline static void mark_immobiles_for_owner(int owner)
{
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    if(!owner || (owner == ib->owner))
      gcMARK(ib->p);
}

inline static void modify_immobiles_owner(int set_from, int set_to)
{
  struct immobile_box *ib;

  for(ib = immobile_boxes; ib; ib = ib->next)
    if(ib->owner == set_from)
      ib->owner = set_to;
}
#endif

inline static void repair_immobiles(struct immobile_box *ib) 
{
  for(; ib; ib = ib->next)
    gcFIXUP(ib->p);
}

/*****************************************************************************/
/* Automatic memory accounting routines                                      */
/*****************************************************************************/
#if defined(NEWGC_BTC_ACCOUNT) || defined(NEWGC_PRECISE_ACCOUNT)
struct stacklet {
  struct stacklet *prev;
  void **max;
  void **cur;
};

static int abort_accounting = 0;
static struct stacklet *account_stack = NULL;

static void abort_memory_accounting() 
{
  fprintf(stderr, "WARNING: Aborting accounting due to lack of memory\n");
  fprintf(stderr, "WARNING: Gathered accounting information will be wrong\n");
  abort_accounting = 1;
}

inline static void enlarge_account_stack()
{
  struct stacklet *temp;

  temp = (struct stacklet *)malloc_pages(MPAGE_SIZE, MPAGE_SIZE);
  temp->prev = account_stack;
  temp->max = (void**)(((unsigned long)temp) + MPAGE_SIZE);
  temp->cur = (void*)&(temp->cur); temp->cur++;
  account_stack = temp;
}

inline static void push_ptr(void *p)
{
  if(!account_stack || (account_stack->cur == account_stack->max)) {
/*     if(account_stack) printf("Enlarging stack\n"); */
    enlarge_account_stack();
  }
  if(!abort_accounting) *(account_stack->cur++) = p;
}

inline static void *pop_ptr()
{
  void **retval = *(--account_stack->cur);
  
  if((account_stack->cur - 1) == (void**)&account_stack->cur) {
    struct stacklet *temp = account_stack;

    account_stack = account_stack->prev;
    free_pages(temp, MPAGE_SIZE);
  }
  return retval;
}

#ifdef NEWGC_PRECISE_ACCOUNT
inline static void reprocess_big_object(struct mpage *page, 
					void *ptr, 
					struct objhead *info)
{
  if(current_mark_owner != info->owner) {
    account_memory(info->owner, -gcBYTES_TO_WORDS(page->size));
    info->owner = owner_set_union(info->owner, current_mark_owner);
    account_memory(info->owner, gcBYTES_TO_WORDS(page->size));
    push_ptr(ptr);
  }
}

inline static void reprocess_normal_object(struct mpage *page,
					   void *ptr,
					   struct objhead *info)
{
  if(current_mark_owner != info->owner) {
    account_memory(info->owner, -info->size);
    info->owner = owner_set_union(info->owner, current_mark_owner);
    account_memory(info->owner, info->size);
    push_ptr(ptr);
  }
}
#endif

#ifdef NEWGC_PRECISE_ACCOUNT
# define REPROCESS_BIG_MARKED_OBJECT(x,y,z) reprocess_big_object(x, y, z)
# define REPROCESS_MARKED_OBJECT(x,y,z) reprocess_normal_object(x, y, z)
#else
# define REPROCESS_BIG_MARKED_OBJECT(x,y,z) {}
# define REPROCESS_MARKED_OBJECT(x,y,z) {}
#endif

inline static void mark_for_accounting(struct mpage *page, void *ptr)
{
  if(page->big_page) {
    struct objhead *info = (struct objhead *)((char*)page + PAGE_BYTE_OVERHEAD);
    if(!info->owner) { 
      info->owner = current_mark_owner; 
      account_memory(info->owner, gcBYTES_TO_WORDS(page->size));
      push_ptr(ptr); 
    } else REPROCESS_BIG_MARKED_OBJECT(page, ptr, info);
  } else {
    struct objhead *info = (struct objhead *)((char*)ptr - 4);
    if(!info->owner) {
      info->owner = current_mark_owner;
      account_memory(info->owner, info->size);
      push_ptr(ptr);
    } else REPROCESS_MARKED_OBJECT(page, ptr, info);
  }
}

inline static void mark_normal_object(void *ptr)
{
  struct objhead *info = (struct objhead *)((char*)ptr - 4);
  
  current_mark_owner = info->owner;
  switch(info->type) {
    case MPAGE_TAGGED: mark_table[*(unsigned short*)ptr](ptr); break;
    case MPAGE_ATOMIC: break;
    case MPAGE_ARRAY: { 
      void **temp = ptr, **end = temp + info->size;
      
      while(temp < end) gcMARK(*(temp++));
      break;
    };
    case MPAGE_TARRAY: {
      unsigned short tag = *(unsigned short*)ptr;
      void **temp = ptr, **end = temp + info->size;
      
      while(temp < end) temp += mark_table[tag](temp);
      break;
    }
    case MPAGE_XTAGGED: GC_mark_xtagged(ptr); break;
  }
}

inline static void mark_big_page(struct mpage *page);

inline static void propogate_accounting_marks()
{
  while(account_stack && !abort_accounting) {
    void *ptr = pop_ptr();

    if(find_page(ptr)->big_page)  
      mark_big_page(find_page(ptr)); 
    else 
      mark_normal_object(ptr); 
  }

  if(account_stack) {
    fprintf(stderr, "Tearing down stack structure\n");
    while(account_stack) {
      void **temp = (void**)account_stack->prev;
      free_pages(account_stack, MPAGE_SIZE);
      account_stack = (struct stacklet *)temp;
    }
  }
}

inline static void do_memory_accounting()
{
  Scheme_Custodian *cur = owner_table[current_owner()]->originator;
  Scheme_Custodian_Reference *box = cur->global_next;
  int i, j;

  doing_memory_accounting_pass = in_unsafe_allocation_mode = 1;
  unsafe_allocation_abort = abort_memory_accounting;
  
  /* first clear out all the memory_use values for this top */
  for(i = 1; i < owner_table_top; i++) 
    if(owner_table[i]) 
      for(j = 0; j <= collection_top; j++)
	owner_table[i]->memory_use[j] = 0;
  
  /* push through to the end of the custodian list */
  while(box->u.two_ptr_val.ptr1) {
    cur = (Scheme_Custodian*)box->u.two_ptr_val.ptr1;
    box = cur->global_next;
  }

  /* now, walk backwards through the list, marking as we go */
  while(cur) {
    int owner = custodian_to_owner_set(cur);

/*     printf("Marking out of owner %i\n", owner); */
    current_mark_owner = owner;
    mark_immobiles_for_owner(owner);
    mark_finalizers_for_owner(owner);
    mark_threads_for_owner(owner);
    propogate_accounting_marks();
    
    box = cur->global_prev; cur = box ? box->u.two_ptr_val.ptr1 : NULL;
  }

  /* everything should be up to date now, so stop */
  doing_memory_accounting_pass = in_unsafe_allocation_mode = 0;
}

# define DO_MEMORY_ACCOUNTING() do_memory_accounting()
# define MEMORY_ACCOUNTING() doing_memory_accounting_pass
# define MEMORY_ACCOUNT_MARK(p, o) mark_for_accounting(p, (void*)(o))
#else 
# define DO_MEMORY_ACCOUNTING() /* */
# define MEMORY_ACCOUNTING() 0
# define MEMORY_ACCOUNT_MARK(p, o) /* */
#endif

/*****************************************************************************/
/* Account hook routines                                                     */
/*****************************************************************************/

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2) 
{
  return 0;
}


/*****************************************************************************/
/* Resolution and Fixup Routines                                             */
/*****************************************************************************/

void *GC_resolve(void *p) 
{
  struct mpage *page = find_page(p);

  if(!page || page->big_page)
    return p;
  if(((struct objhead *)((unsigned long)p - 4))->mark)
    return *(void**)p;
  else return p;
}

void GC_fixup(void *pp) 
{
  void *p = *(void**)pp;
  struct mpage *page = find_page(p);
  struct objhead *info = (struct objhead *)((unsigned long)p - 4);

  if(!p || ((long)p & 0x1))
    return;

  if(page) {
    if(page->big_page) {
      struct mpage *from_page = find_page(pp);
      
      if(from_page && !from_page->back_pointers) {
	if(from_page->generation > page->generation) {
	  from_page->back_pointers = 1;
	}
      }
      return;
    }
    
    if(info->mark) {
      struct mpage *from_page = find_page(pp);
      
      if(from_page && !from_page->back_pointers)
	if(from_page->generation > INCGEN(page->generation))
	  from_page->back_pointers = 1;
      *(void**)pp = *(void**)p;
    }
  }
}

inline static void repair_big_page(struct mpage *page) 
{
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

inline static void repair_tagged_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    start++;
    start += fixup_table[*(unsigned short*)start](start);
  }
  GC_DEBUG("Repaired tagged page from %p to %p\n", 
	   (char*)page + PAGE_BYTE_OVERHEAD, start);
}

inline static void repair_array_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start++;
    unsigned long size = ohead->size;
    while(--size)
      gcFIXUP(*(start++));
  }
  GC_DEBUG("Repaired array page from %p to %p\n", 
	   (char*)page + PAGE_BYTE_OVERHEAD, start);
}

inline static void repair_tarray_page(struct mpage *page) 
{
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
  GC_DEBUG("Repaired tagged array page from %p to %p\n", 
	   (char*)page + PAGE_BYTE_OVERHEAD, start);
}

inline static void repair_xtagged_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + PAGE_BYTE_OVERHEAD);
  void **end = (void**)((unsigned long)page + page->size);

  while(start < end) {
    struct objhead *ohead = (struct objhead *)start;
    GC_fixup_xtagged(start + 1);
    start += ohead->size;
  }
  GC_DEBUG("Repaired xtagged page from %p to %p\n", 
	   (char*)page + PAGE_BYTE_OVERHEAD, start);
} 

inline static void repair_heap(void) 
{
  struct mpage *page;
  unsigned short i, j;
  unsigned short top_gen_plus_one = INCGEN(collection_top);

  for(i = 1; i <= top_gen_plus_one; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next) {
        switch(j) {
          case MPAGE_TAGGED: repair_tagged_page(page); break;
          case MPAGE_ATOMIC: break;
          case MPAGE_ARRAY: repair_array_page(page); break;
          case MPAGE_TARRAY: repair_tarray_page(page); break;
          case MPAGE_XTAGGED: repair_xtagged_page(page); break;
          case MPAGE_BIG: repair_big_page(page); break;
        }
        page->previous_size = page->size;
      }

  if(collection_top == 0) 
    for(j = 0; j < MPAGE_TYPES; j++) 
      for(page = pages[2][j]; page; page = page->next) 
	if(page->back_pointers) 
          switch(j) {
            case MPAGE_TAGGED: repair_tagged_page(page); break;
            case MPAGE_ATOMIC: break;
            case MPAGE_ARRAY: repair_array_page(page); break;
            case MPAGE_TARRAY: repair_tarray_page(page); break;
            case MPAGE_XTAGGED: repair_xtagged_page(page); break;
            case MPAGE_BIG: repair_big_page(page); break;
          }
}

/*****************************************************************************/
/* Routines dealing with marks and marking                                   */
/*****************************************************************************/

inline static int marked(void *p) 
{
  struct mpage *page = find_page(p);
  
  if(!p || !page) return 0;
  if(page->generation > collection_top) return 1;
  return ((struct objhead *)((char*)p - 4))->mark;
}

#if defined(NEWGC_MANUAL_ACCOUNT)
# define MANUAL_SET_FAKE_OWNER_INFORMATION(page, ohead) \
    if(page->generation > 0) ohead->owner = page->man_owner
#else
# define MANUAL_SET_FAKE_OWNER_INFORMATION(p,o) /* */
#endif

inline static void mark_normal(struct mpage *page, void *p) 
{
  struct objhead *ohead = (struct objhead *)((unsigned long)p - 4);

  if(ohead->mark) {
  } else {
    unsigned long tgen = page ? INCGEN(page->generation) : 1;
    unsigned short type = ohead->type;
    void *newplace;

    if(type == MPAGE_TAGGED)
      if((int)mark_table[*(unsigned short*)p] < MPAGE_TYPES)
	type = ohead->type = (int)mark_table[*(unsigned short*)p];

    MANUAL_SET_FAKE_OWNER_INFORMATION(page, ohead);
    newplace = copy_bits((void*)ohead, ohead->size, type, tgen);
    *(void**)p = (void*)((unsigned long)newplace + 4);
    ohead->mark = 1;
    GC_DEBUG("Marked/copied %i bytes of data from obj %p to %p (FW: %p)\n",
	     ohead->size, p, (void*)((char*)newplace + 4),
	     *(void**)((char*)newplace + 4));;
  }
}

void GC_mark(const void *p) 
{
  struct mpage *page = find_page((void*)p);

  if(!p || ((unsigned long)p & 0x1) || !page)
    return;

  if(MEMORY_ACCOUNTING()) MEMORY_ACCOUNT_MARK(page, p); else {
    if(page->generation > collection_top)
      return;

    if(page->big_page) {
      if(page->big_page == 1) {
	page->big_page++;
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
 	CLEAR_OWNER_FIELD((char*)page + PAGE_BYTE_OVERHEAD);
      }
    } else mark_normal(page, (void*)p);
  }
}

inline static void mark_big_page(struct mpage *page) 
{
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

inline static void mark_tagged_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    start++;
    start += mark_table[*(unsigned short*)start](start);
  }
}

inline static void mark_array_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start++;
    unsigned long size = info->size;
    
    while(--size)
      GC_mark(*(start++));
  }
}

inline static void mark_tarray_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start;
    void **tempend = start + info->size;
    unsigned short tag = *(unsigned short*)(++start);

    while(start < tempend) 
      start += mark_table[tag](start);
  }
}

inline static void mark_xtagged_page(struct mpage *page) 
{
  void **start = (void**)((unsigned long)page + page->previous_size);

  while(start < (void**)((unsigned long)page + page->size)) {
    struct objhead *info = (struct objhead *)start;

    GC_mark_xtagged(start + 1);
    start += info->size;
  }
}

inline static void mark_older_pointers(void) 
{
  unsigned short i, j;
  struct mpage *page;

  for(i = INCGEN(collection_top); i < GENERATIONS; i++) 
    for(j = 0; j < MPAGE_TYPES; j++)
      for(page = pages[i][j]; page; page = page->next)
	if(page->back_pointers) {
          page->previous_size = PAGE_BYTE_OVERHEAD;
          switch(j) {
            case MPAGE_TAGGED: mark_tagged_page(page); break;
            case MPAGE_ATOMIC: break;
            case MPAGE_ARRAY: mark_array_page(page); break;
            case MPAGE_TARRAY: mark_tarray_page(page); break;
            case MPAGE_XTAGGED: mark_xtagged_page(page); break;
            case MPAGE_BIG: mark_big_page(page); break;
          }
	  page->previous_size = page->size;
        }
}

inline static void propogate_all_marks(void) 
{
  struct mpage *page;
  unsigned short i, j;
  unsigned short top_gen_plus_one = INCGEN(collection_top);
  unsigned short changes;

  do {
    changes = 0;

    for(i = 1; i <= top_gen_plus_one; i++) {
      for(j = 0; j < MPAGE_TYPES; j++) {
        for(page = pages[i][j]; page; page = page->next) {
/* 	  printf("Propogating page %p (type %i, bipage %i)\n", page, */
/* 		 page->page_type, page->big_page); */
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

inline static void prepare_pages_for_collection(void)
{
  struct mpage *work;
  int i, j;

  for(i = 0; i <= collection_top; i++)
    for(j = 0; j < MPAGE_TYPES; j++) {
      work = pages[i][j];
      while(work) {
        struct mpage *next = work->next;

        protect_pages(work, work->size, 1);
        if(work->big_page)
          work->big_page = 1;

        if(collection_from_pages)
          collection_from_pages->prev = work;
        work->prev = NULL;
        work->next = collection_from_pages;
        collection_from_pages = work;

        work = next;
      }
      pages[i][j] = NULL;
    }
}

inline static void mark_all_roots(void)
{
  if(!collection_full) mark_older_pointers();
  GC_DEBUG("MARKING UNQUEUED FINALIZERS\n");
  finalizers = mark_finalizers(finalizers, 0);
  GC_DEBUG("MARKING RUN QUEUE\n");
  run_queue = mark_finalizers(run_queue, 1);
  GC_DEBUG("MARKING WEAK FINALIZERS\n");
  weak_finalizers = mark_weak_finalizers(weak_finalizers);
  GC_DEBUG("MARKING ROOTS\n");
  mark_roots(roots);
  GC_DEBUG("MARKING IMMOBILE BOXES\n");
  mark_immobiles(immobile_boxes);
  GC_DEBUG("MARKING VARIABLE STACK\n");
  GC_mark_variable_stack(GC_variable_stack, 0,
			 (void*)(GC_get_thread_stack_base
				 ? GC_get_thread_stack_base()
				 : (unsigned long)stack_base));
}

# define CHECK_FINALIZERS(i) finalizers = check_finalizers(finalizers, i)

inline static void repair_all_roots(void)
{
  repair_finalizers(finalizers); 
  repair_finalizers(run_queue);
  weak_finalizers = repair_weak_finalizers(weak_finalizers);
  repair_roots(roots);
  repair_immobiles(immobile_boxes);
  GC_fixup_variable_stack(GC_variable_stack, 0, 
                          (void*)(GC_get_thread_stack_base
                                  ? GC_get_thread_stack_base()
                                  : (unsigned long)stack_base));
  REPAIR_MANUAL_ALIAS_TABLE();
  REPAIR_THREAD_LIST();
  REPAIR_OWNER_TABLE();
}

# define SUGGEST_FORCE_FULL() 0
# define REPAIR_ACCOUNT_HOOKS() /* */
# define RUN_ACCOUNT_HOOKS() /* */

inline static void free_and_protect_pages(void)
{
  struct mpage *work = collection_from_pages;
  int i, j;

  while(work) {
    struct mpage *temp = work;
  
    work = work->next;
    remove_page_from_page_map(temp);
    free_pages(temp, temp->big_page ? temp->size : MPAGE_SIZE);
  }
  collection_from_pages = NULL;
  reset_appropriate_weak_boxes();
  reset_appropriate_weak_arrays();

  /* protect the new pages */
  for(i = 1; i < GENERATIONS; i++)  
    for(j = 0; j < MPAGE_TYPES; j++) 
      if(j != MPAGE_ATOMIC) 
	for(work = pages[i][j]; work; work = work->next) 
	  protect_pages(work, work->size, 0);  
}

static void garbage_collect(int force_full) 
{
  unsigned short i;

  force_full = force_full | SUGGEST_FORCE_FULL();
  if(GC_collect_start_callback) GC_collect_start_callback();
  if(force_full) collection_top = (GENERATIONS - 1); else 
    collection_top = COMPUTE_COLLECTION_TOP(collection_number);
  collection_number += 1; collection_full = (collection_top == (GENERATIONS-1));

  INIT_DEBUG_FILE();
  GC_DEBUG("Before collection (top = %i)\n", collection_top);
  DUMP_HEAP();

  prepare_pages_for_collection();
  mark_all_roots();
  propogate_all_marks();
  for(i = 0; i < 5; i++) {
    if((i == 3) && collection_full) zero_weak_finalizers(weak_finalizers);
    CHECK_FINALIZERS(i);
    propogate_all_marks();
    if((i == 3) && collection_full) reset_weak_finalizers(weak_finalizers);
  }
  propogate_all_marks();

  repair_all_roots();
  repair_heap();
  REPAIR_ACCOUNT_HOOKS();

  /* delete all the old, dead pages */
  free_and_protect_pages(); reset_nursery();
  flush_freed_pages();

  if(GC_collect_start_callback)
    GC_collect_end_callback();

  DO_MEMORY_ACCOUNTING();
  /* run any queued finalizers */
  if(!running_finalizers) {
    running_finalizers = 1;
    while(run_queue) {
      struct finalizer *f;
      void **gcs;

      f = run_queue; run_queue = run_queue->next;
      if(!run_queue) last_in_queue = NULL;

      GC_DEBUG("Running finalizer %p for pointer %p, level %i\n", 
	       f, f->p, f->eager_level);
      gcs = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = gcs;
    }
    RUN_ACCOUNT_HOOKS();
    running_finalizers = 0;
  }
  GC_DEBUG("After collection (top = %i)\n", collection_top);
  DUMP_HEAP();
  CLOSE_DEBUG_FILE();
} 

/*****************************************************************************/
/* Administration, Initialization and Write Barrier Routines                 */
/*****************************************************************************/

static void designate_modified(void *p) 
{
  struct mpage *page = find_page(p);
 
  if(page != NULL) {
    protect_pages(page, page->size, 1);
    page->back_pointers = 1;
  } else {
    fprintf(stderr, "Seg fault (internal error) at %p\n", p);
    abort();
  }
}

#include "sighand.c"

void GC_init_type_tags(int count, int weakbox) 
{
  static int initialized = 0;

  if(!initialized) {
    initialized = 1;

    max_heap_size = determine_max_heap_size();
    pages_in_heap = max_heap_size / MPAGE_SIZE;
    max_used_pages = pages_in_heap / 2;
    gen0_alloc_region = (void**)malloc_pages(GEN0_SIZE, MPAGE_SIZE);
    gen0_alloc_current = gen0_alloc_region + PAGE_WORD_OVERHEAD;
    gen0_alloc_end = (void**)((unsigned long)gen0_alloc_region + GEN0_SIZE);
    ((struct mpage *)gen0_alloc_region)->size = GEN0_SIZE;
    ((struct mpage *)gen0_alloc_region)->big_page = 1;
    add_page_to_page_map((struct mpage *)gen0_alloc_region);
    ((struct mpage *)gen0_alloc_region)->big_page = 0;
    GC_register_traversers(weakbox, size_weak_box, mark_weak_box, 
                           repair_weak_box, 0, 0);
    GC_register_traversers(weak_array_tag, size_weak_array, mark_weak_array,
                           repair_weak_array, 0, 0);

    initialize_signal_handler();
  }
  weak_box_tag = weakbox;
}

static char *type_name[MPAGE_TYPES] = { "tagged",
					"atomic",
					"array",
					"tagged array",
					"xtagged",
					"big" 
                                       };

void GC_dump(void) 
{
  unsigned long gen0_bigs = 0;
  unsigned short i;
  struct mpage *page;
  
  for(page = pages[0][MPAGE_BIG]; page; page = page->next) { gen0_bigs++; }
  fprintf(stderr, "Gen 0: %li of %li bytes used (+ %li in %li bigpages)\n",
	  ((unsigned long)gen0_alloc_current-(unsigned long)gen0_alloc_region), 
	  (unsigned long)GEN0_SIZE, gen0_bigpages_size, gen0_bigs);
  
  for(i = 1; i < GENERATIONS; i++) {
    unsigned long num_pages = 0, size_pages = 0;
    unsigned long tnum_pages[MPAGE_TYPES];
    unsigned long tsize_pages[MPAGE_TYPES];
    unsigned short j;

    bzero(tnum_pages, sizeof(tnum_pages)); 
    bzero(tsize_pages, sizeof(tsize_pages));
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
}

long GC_get_memory_use(void *custodian) 
{
  unsigned long retval = 0;

  if(custodian ) {
#if defined(NEWGC_PRECISE_ACCOUNT) || defined(NEWGC_BTC_ACCOUNT)
    retval = custodian_get_memory_use(custodian, 0);
#endif
  } else {
    struct mpage *page;
    unsigned short i, j;

    retval += ((unsigned long)gen0_alloc_current) - 
              ((unsigned long)gen0_alloc_region);
    for(i = 0; i < GENERATIONS; i++)
      for(j = 0; j < MPAGE_TYPES; j++)
	for(page = pages[i][j]; page; page = page->next)
	  retval += page->size;
  }

  return retval;
}

void GC_gcollect(void) 
{
  garbage_collect(1);
}

# define ACCOUNT_SAVE_AND_NULL_THREAD_MARKER(tag,mark) /* */

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark,
                            Fixup_Proc fixup, int cs, int atomic) 
{
  size_table[tag] = size;
  mark_table[tag] = atomic ? (Mark_Proc)MPAGE_ATOMIC : mark;
  fixup_table[tag] = fixup;
  ACCOUNT_SAVE_AND_NULL_THREAD_MARKER(tag, mark);
}


