/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */
/* Boehm, October 9, 1995 1:06 pm PDT */
# include <stdio.h>
# include "gc_priv.h"

/* MAX_ROOT_SETS is the maximum number of ranges that can be 	*/
/* registered as static roots. 					*/
# ifdef LARGE_CONFIG
#   define MAX_ROOT_SETS 4096
# else
#   ifdef PCR
#     define MAX_ROOT_SETS 1024
#   else
#     ifdef MSWIN32
#	define MAX_ROOT_SETS 512
	    /* Under NT, we add only written pages, which can result 	*/
	    /* in many small root sets.					*/
#     else
#       define MAX_ROOT_SETS 1024 /* MATTHEW: was 64, but extensions add a lot */
#     endif
#   endif
# endif

# define MAX_EXCLUSIONS (MAX_ROOT_SETS/4)
/* Maximum number of segments that can be excluded from root sets.	*/

/* Data structure for list of root sets.				*/
/* We keep a hash table, so that we can filter out duplicate additions.	*/
/* Under Win32, we need to do a better job of filtering overlaps, so	*/
/* we resort to sequential search, and pay the price.			*/
struct roots {
	ptr_t r_start;
	ptr_t r_end;
#	ifndef MSWIN32
	  struct roots * r_next;
#	endif
	bool r_tmp;
	  	/* Delete before registering new dynamic libraries */
};

static struct roots static_roots[MAX_ROOT_SETS];

static int n_root_sets = 0;

	/* static_roots[0..n_root_sets) contains the valid root sets. */

# if !defined(NO_DEBUGGING)
/* For debugging:	*/
void GC_print_static_roots()
{
    register int i;
    size_t total = 0;
    
    for (i = 0; i < n_root_sets; i++) {
        GC_printf2("From 0x%lx to 0x%lx ",
        	   (unsigned long) static_roots[i].r_start,
        	   (unsigned long) static_roots[i].r_end);
        if (static_roots[i].r_tmp) {
            GC_printf0(" (temporary)\n");
        } else {
            GC_printf0("\n");
        }
        total += static_roots[i].r_end - static_roots[i].r_start;
    }
    GC_printf1("Total size: %ld\n", (unsigned long) total);
    if (GC_root_size != total) {
     	GC_printf1("GC_root_size incorrect: %ld!!\n",
     		   (unsigned long) GC_root_size);
    }
}
# endif /* NO_DEBUGGING */

/* Primarily for debugging support:	*/
/* Is the address p in one of the registered static			*/
/* root sections?							*/
bool GC_is_static_root(p)
ptr_t p;
{
    static int last_root_set = 0;
    register int i;
    
    
    if (p >= static_roots[last_root_set].r_start
        && p < static_roots[last_root_set].r_end) return(TRUE);
    for (i = 0; i < n_root_sets; i++) {
    	if (p >= static_roots[i].r_start
            && p < static_roots[i].r_end) {
            last_root_set = i;
            return(TRUE);
        }
    }
    return(FALSE);
}

#ifndef MSWIN32
#   define LOG_RT_SIZE 6
#   define RT_SIZE (1 << LOG_RT_SIZE)  /* Power of 2, may be != MAX_ROOT_SETS */

    static struct roots * root_index[RT_SIZE];
	/* Hash table header.  Used only to check whether a range is 	*/
	/* already present.						*/

static int rt_hash(addr)
char * addr;
{
    word result = (word) addr;
#   if CPP_WORDSZ > 8*LOG_RT_SIZE
	result ^= result >> 8*LOG_RT_SIZE;
#   endif
#   if CPP_WORDSZ > 4*LOG_RT_SIZE
	result ^= result >> 4*LOG_RT_SIZE;
#   endif
    result ^= result >> 2*LOG_RT_SIZE;
    result ^= result >> LOG_RT_SIZE;
    result &= (RT_SIZE-1);
    return(result);
}

/* Is a range starting at b already in the table? If so return a	*/
/* pointer to it, else NIL.						*/
struct roots * GC_roots_present(b)
char *b;
{
    register int h = rt_hash(b);
    register struct roots *p = root_index[h];
    
    while (p != 0) {
        if (p -> r_start == (ptr_t)b) return(p);
        p = p -> r_next;
    }
    return(FALSE);
}

/* Add the given root structure to the index. */
static void add_roots_to_index(p)
struct roots *p;
{
    register int h = rt_hash(p -> r_start);
    
    p -> r_next = root_index[h];
    root_index[h] = p;
}

# else /* MSWIN32 */

#   define add_roots_to_index(p)

# endif




word GC_root_size = 0;

void GC_add_roots(b, e)
char * b; char * e;
{
    DCL_LOCK_STATE;
    
    DISABLE_SIGNALS();
    LOCK();
    GC_add_roots_inner(b, e, FALSE);
    UNLOCK();
    ENABLE_SIGNALS();
}


/* Add [b,e) to the root set.  Adding the same interval a second time	*/
/* is a moderately fast noop, and hence benign.  We do not handle	*/
/* different but overlapping intervals efficiently.  (We do handle	*/
/* them correctly.)							*/
/* Tmp specifies that the interval may be deleted before 		*/
/* reregistering dynamic libraries.					*/ 
void GC_add_roots_inner(b, e, tmp)
char * b; char * e;
bool tmp;
{
    struct roots * old;
    
/* MATTHEW: always merge overlapping: */
#   if 1 || defined(MSWIN32)
      /* Spend the time to ensure that there are no overlapping	*/
      /* or adjacent intervals.					*/
      /* This could be done faster with e.g. a			*/
      /* balanced tree.  But the execution time here is		*/
      /* virtually guaranteed to be dominated by the time it	*/
      /* takes to scan the roots.				*/
      {
        register int i;
        
        for (i = 0; i < n_root_sets; i++) {
            old = static_roots + i;
            if ((ptr_t)b <= old -> r_end && (ptr_t)e >= old -> r_start
		&& tmp == old -> r_tmp) { /* MATTHEW: merge only same kinds */
                if ((ptr_t)b < old -> r_start) {
                    old -> r_start = (ptr_t)b;
                    GC_root_size += (old -> r_start - (ptr_t)b);
                }
                if ((ptr_t)e > old -> r_end) {
                    old -> r_end = (ptr_t)e;
                    GC_root_size += ((ptr_t)e - old -> r_end);
                }
                old -> r_tmp &= tmp;
                break;
            }
        }
        if (i < n_root_sets) {
          /* merge other overlapping intervals */
            struct roots *other;
            
            for (i++; i < n_root_sets; i++) {
              other = static_roots + i;
              b = (char *)(other -> r_start);
              e = (char *)(other -> r_end);
	      tmp = other -> r_tmp; /* MATTHEW */
              if ((ptr_t)b <= old -> r_end && (ptr_t)e >= old -> r_start
		  && tmp == old -> r_tmp) {  /* MATTHEW: merge only same kinds */
                if ((ptr_t)b < old -> r_start) {
                    old -> r_start = (ptr_t)b;
                    GC_root_size += (old -> r_start - (ptr_t)b);
                }
                if ((ptr_t)e > old -> r_end) {
                    old -> r_end = (ptr_t)e;
                    GC_root_size += ((ptr_t)e - old -> r_end);
                }
                old -> r_tmp &= other -> r_tmp;
                /* Delete this entry. */
                  GC_root_size -= (other -> r_end - other -> r_start);
                  other -> r_start = static_roots[n_root_sets-1].r_start;
                  other -> r_end = static_roots[n_root_sets-1].r_end;
                                  n_root_sets--;
              }
            }
          return;
        }
      }
#   else
      old = GC_roots_present(b);
      if (old != 0) {
        if ((ptr_t)e <= old -> r_end) /* already there */ return;
        /* else extend */
        GC_root_size += (ptr_t)e - old -> r_end;
        old -> r_end = (ptr_t)e;
        return;
      }
#   endif
    if (n_root_sets == MAX_ROOT_SETS) {
        ABORT("Too many root sets\n");
    }
    static_roots[n_root_sets].r_start = (ptr_t)b;
    static_roots[n_root_sets].r_end = (ptr_t)e;
    static_roots[n_root_sets].r_tmp = tmp;
#   ifndef MSWIN32
      static_roots[n_root_sets].r_next = 0;
#   endif
    add_roots_to_index(static_roots + n_root_sets);
    GC_root_size += (ptr_t)e - (ptr_t)b;
    n_root_sets++;
}

void GC_clear_roots GC_PROTO((void))
{
    DCL_LOCK_STATE;
    
    DISABLE_SIGNALS();
    LOCK();
    n_root_sets = 0;
    GC_root_size = 0;
#   ifndef MSWIN32
    {
    	register int i;
    	
    	for (i = 0; i < RT_SIZE; i++) root_index[i] = 0;
    }
#   endif
    UNLOCK();
    ENABLE_SIGNALS();
}

/* Internal use only; lock held.	*/
void GC_remove_tmp_roots()
{
    register int i;
    
    for (i = 0; i < n_root_sets; ) {
    	if (static_roots[i].r_tmp) {
    	    GC_root_size -= (static_roots[i].r_end - static_roots[i].r_start);
    	    static_roots[i].r_start = static_roots[n_root_sets-1].r_start;
    	    static_roots[i].r_end = static_roots[n_root_sets-1].r_end;
    	    static_roots[i].r_tmp = static_roots[n_root_sets-1].r_tmp;
    	    n_root_sets--;
    	} else {
    	    i++;
    	}
    }
#   ifndef MSWIN32
    {
    	register int i;
    	
    	for (i = 0; i < RT_SIZE; i++) root_index[i] = 0;
    	for (i = 0; i < n_root_sets; i++) add_roots_to_index(static_roots + i);
    }
#   endif
    
}

ptr_t GC_approx_sp()
{
    word dummy;
    
    return((ptr_t)(&dummy));
}

/*
 * Data structure for excluded static roots.
 */
struct exclusion {
    ptr_t e_start;
    ptr_t e_end;
};

struct exclusion excl_table[MAX_EXCLUSIONS];
					/* Array of exclusions, ascending */
					/* address order.		  */
size_t excl_table_entries = 0;		/* Number of entries in use.	  */

/* Return the first exclusion range that includes an address >= start_addr */
/* Assumes the exclusion table contains at least one entry (namely the	   */
/* GC data structures).							   */
struct exclusion * GC_next_exclusion(start_addr)
ptr_t start_addr;
{
    size_t low = 0;
    size_t high = excl_table_entries - 1;
    size_t mid;

    while (high > low) {
	mid = (low + high) >> 1;
	/* low <= mid < high	*/
	if ((word) excl_table[mid].e_end <= (word) start_addr) {
	    low = mid + 1;
	} else {
	    high = mid;
	}
    }
    if ((word) excl_table[low].e_end <= (word) start_addr) return 0;
    return excl_table + low;
}

void GC_exclude_static_roots(start, finish)
GC_PTR start;
GC_PTR finish;
{
    struct exclusion * next;
    size_t next_index, i;

    if (0 == excl_table_entries) {
	next = 0;
    } else {
	next = GC_next_exclusion(start);
    }
    if (0 != next) {
      if ((word)(next -> e_start) < (word) finish) {
	/* incomplete error check. */
	ABORT("exclusion ranges overlap");
      }  
      if ((word)(next -> e_start) == (word) finish) {
        /* extend old range backwards	*/
          next -> e_start = (ptr_t)start;
	  return;
      }
      next_index = next - excl_table;
      for (i = excl_table_entries - 1; i >= next_index; --i) {
	excl_table[i+1] = excl_table[i];
      }
    } else {
      next_index = excl_table_entries;
    }
    if (excl_table_entries == MAX_EXCLUSIONS) ABORT("Too many exclusions");
    excl_table[next_index].e_start = (ptr_t)start;
    excl_table[next_index].e_end = (ptr_t)finish;
    ++excl_table_entries;
}

/* Invoke push_conditional on ranges that are not excluded. */
void GC_push_conditional_with_exclusions(bottom, top, all)
ptr_t bottom;
ptr_t top;
int all;
{
    struct exclusion * next;
    ptr_t excl_start;

    while (bottom < top) {
        next = GC_next_exclusion(bottom);
	if (0 == next || (excl_start = next -> e_start) >= top) {
	    GC_push_conditional(bottom, top, all);
	    return;
	}
	if (excl_start > bottom) GC_push_conditional(bottom, excl_start, all);
	bottom = next -> e_end;
    }
}

/*
 * Call the mark routines (GC_tl_push for a single pointer, GC_push_conditional
 * on groups of pointers) on every top level accessible pointer.
 * If all is FALSE, arrange to push only possibly altered values.
 */

void GC_push_roots(all)
bool all;
{
    register int i;

    /*
     * push registers - i.e., call GC_push_one(r) for each
     * register contents r.
     */
        GC_push_regs(); /* usually defined in machine_dep.c */
        
    /*
     * Next push static data.  This must happen early on, since it's
     * not robust against mark stack overflow.
     */
     /* Reregister dynamic libraries, in case one got added.	*/
#      if (defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(PCR)) \
           && !defined(SRC_M3)
         GC_remove_tmp_roots();
         GC_register_dynamic_libraries();
#      endif
     /* Mark everything in static data areas                             */
       for (i = 0; i < n_root_sets; i++) {
         GC_push_conditional_with_exclusions(
			     static_roots[i].r_start,
			     static_roots[i].r_end, all);
       }

    /*
     * Now traverse stacks.
     */
#   ifndef THREADS
        /* Mark everything on the stack.           */
#   	  ifdef STACK_GROWS_DOWN
	    GC_push_all_stack( GC_approx_sp(), GC_stackbottom );
#	  else
	    GC_push_all_stack( GC_stackbottom, GC_approx_sp() );
#	  endif
#   endif
    if (GC_push_other_roots != 0) (*GC_push_other_roots)();
    	/* In the threads case, this also pushes thread stacks.	*/
}

