/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1997 by Silicon Graphics.  All rights reserved.
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
/* Boehm, October 9, 1995 1:16 pm PDT */
# include "gc_priv.h"

void GC_default_print_heap_obj_proc();
GC_API void GC_register_finalizer_no_order
    	GC_PROTO((GC_PTR obj, GC_finalization_proc fn, GC_PTR cd,
		  GC_finalization_proc *ofn, GC_PTR *ocd));

/* Do we want to and know how to save the call stack at the time of	*/
/* an allocation?  How much space do we want to use in each object?	*/

# define START_FLAG ((word)0xfedcedcb)
# define END_FLAG ((word)0xbcdecdef)
	/* Stored both one past the end of user object, and one before	*/
	/* the end of the object as seen by the allocator.		*/


/* Object header */
typedef struct {
    char * oh_string;		/* object descriptor string	*/
    word oh_int;		/* object descriptor integers	*/
#   ifdef NEED_CALLINFO
      struct callinfo oh_ci[NFRAMES];
#   endif
    word oh_sz;			/* Original malloc arg.		*/
    word oh_sf;			/* start flag */
} oh;
/* The size of the above structure is assumed not to dealign things,	*/
/* and to be a multiple of the word length.				*/

#define DEBUG_BYTES (sizeof (oh) + sizeof (word))
#undef ROUNDED_UP_WORDS
#define ROUNDED_UP_WORDS(n) BYTES_TO_WORDS((n) + WORDS_TO_BYTES(1) - 1)


#ifdef SAVE_CALL_CHAIN
#   define ADD_CALL_CHAIN(base, ra) GC_save_callers(((oh *)(base)) -> oh_ci)
#   define PRINT_CALL_CHAIN(base) GC_print_callers(((oh *)(base)) -> oh_ci)
#else
# ifdef GC_ADD_CALLER
#   define ADD_CALL_CHAIN(base, ra) ((oh *)(base)) -> oh_ci[0].ci_pc = (ra)
#   define PRINT_CALL_CHAIN(base) GC_print_callers(((oh *)(base)) -> oh_ci)
# else
#   define ADD_CALL_CHAIN(base, ra)
#   define PRINT_CALL_CHAIN(base)
# endif
#endif

/* Check whether object with base pointer p has debugging info	*/ 
/* p is assumed to point to a legitimate object in our part	*/
/* of the heap.							*/
GC_bool GC_has_debug_info(p)
ptr_t p;
{
    register oh * ohdr = (oh *)p;
    register ptr_t body = (ptr_t)(ohdr + 1);
    register word sz = GC_size((ptr_t) ohdr);
    
    if (HBLKPTR((ptr_t)ohdr) != HBLKPTR((ptr_t)body)
        || sz < sizeof (oh)) {
        return(FALSE);
    }
    if (ohdr -> oh_sz == sz) {
    	/* Object may have had debug info, but has been deallocated	*/
    	return(FALSE);
    }
    if (ohdr -> oh_sf == (START_FLAG ^ (word)body)) return(TRUE);
    if (((word *)ohdr)[BYTES_TO_WORDS(sz)-1] == (END_FLAG ^ (word)body)) {
        return(TRUE);
    }
    return(FALSE);
}

/* Store debugging info into p.  Return displaced pointer. */
/* Assumes we don't hold allocation lock.		   */
ptr_t GC_store_debug_info(p, sz, string, integer)
register ptr_t p;	/* base pointer */
word sz; 	/* bytes */
char * string;
word integer;
{
    register word * result = (word *)((oh *)p + 1);
    DCL_LOCK_STATE;
    
    /* There is some argument that we should dissble signals here.	*/
    /* But that's expensive.  And this way things should only appear	*/
    /* inconsistent while we're in the handler.				*/
    LOCK();
    ((oh *)p) -> oh_string = string;
    ((oh *)p) -> oh_int = integer;
    ((oh *)p) -> oh_sz = sz;
    ((oh *)p) -> oh_sf = START_FLAG ^ (word)result;
    ((word *)p)[BYTES_TO_WORDS(GC_size(p))-1] =
         result[ROUNDED_UP_WORDS(sz)] = END_FLAG ^ (word)result;
    UNLOCK();
    return((ptr_t)result);
}

/* Check the object with debugging info at p 		*/
/* return NIL if it's OK.  Else return clobbered	*/
/* address.						*/
ptr_t GC_check_annotated_obj(ohdr)
register oh * ohdr;
{
    register ptr_t body = (ptr_t)(ohdr + 1);
    register word gc_sz = GC_size((ptr_t)ohdr);
    if (ohdr -> oh_sz + DEBUG_BYTES > gc_sz) {
        return((ptr_t)(&(ohdr -> oh_sz)));
    }
    if (ohdr -> oh_sf != (START_FLAG ^ (word)body)) {
        return((ptr_t)(&(ohdr -> oh_sf)));
    }
    if (((word *)ohdr)[BYTES_TO_WORDS(gc_sz)-1] != (END_FLAG ^ (word)body)) {
        return((ptr_t)((word *)ohdr + BYTES_TO_WORDS(gc_sz)-1));
    }
    if (((word *)body)[ROUNDED_UP_WORDS(ohdr -> oh_sz)]
        != (END_FLAG ^ (word)body)) {
        return((ptr_t)((word *)body + ROUNDED_UP_WORDS(ohdr -> oh_sz)));
    }
    return(0);
}

void GC_print_obj(p)
ptr_t p;
{
    register oh * ohdr = (oh *)GC_base(p);
    
    GC_err_printf1("0x%lx (", ((unsigned long)ohdr + sizeof(oh)));
    GC_err_puts(ohdr -> oh_string);
    GC_err_printf2(":%ld, sz=%ld)\n", (unsigned long)(ohdr -> oh_int),
        			      (unsigned long)(ohdr -> oh_sz));
    PRINT_CALL_CHAIN(ohdr);
}

void GC_debug_print_heap_obj_proc(p)
ptr_t p;
{
    if (GC_has_debug_info(p)) {
	GC_print_obj(p);
    } else {
	GC_default_print_heap_obj_proc(p);
    }
}

void GC_print_smashed_obj(p, clobbered_addr)
ptr_t p, clobbered_addr;
{
    register oh * ohdr = (oh *)GC_base(p);
    
    GC_err_printf2("0x%lx in object at 0x%lx(", (unsigned long)clobbered_addr,
    					        (unsigned long)p);
    if (clobbered_addr <= (ptr_t)(&(ohdr -> oh_sz))
        || ohdr -> oh_string == 0) {
        GC_err_printf1("<smashed>, appr. sz = %ld)\n",
        	       (GC_size((ptr_t)ohdr) - DEBUG_BYTES));
    } else {
        if (ohdr -> oh_string[0] == '\0') {
            GC_err_puts("EMPTY(smashed?)");
        } else {
            GC_err_puts(ohdr -> oh_string);
        }
        GC_err_printf2(":%ld, sz=%ld)\n", (unsigned long)(ohdr -> oh_int),
        			          (unsigned long)(ohdr -> oh_sz));
        PRINT_CALL_CHAIN(ohdr);
    }
}

void GC_check_heap_proc();

void GC_start_debugging()
{
    GC_check_heap = GC_check_heap_proc;
    GC_print_heap_obj = GC_debug_print_heap_obj_proc;
    GC_debugging_started = TRUE;
    GC_register_displacement((word)sizeof(oh));
}

# if defined(__STDC__) || defined(__cplusplus)
    void GC_debug_register_displacement(GC_word offset)
# else
    void GC_debug_register_displacement(offset) 
    GC_word offset;
# endif
{
    GC_register_displacement(offset);
    GC_register_displacement((word)sizeof(oh) + offset);
}

# ifdef GC_ADD_CALLER
#   define EXTRA_ARGS word ra, char * s, int i
#   define OPT_RA ra,
# else
#   define EXTRA_ARGS char * s, int i
#   define OPT_RA
# endif

# ifdef __STDC__
    GC_PTR GC_debug_malloc(size_t lb, EXTRA_ARGS)
# else
    GC_PTR GC_debug_malloc(lb, s, i)
    size_t lb;
    char * s;
    int i;
#   ifdef GC_ADD_CALLER
	--> GC_ADD_CALLER not implemented for K&R C
#   endif
# endif
{
    GC_PTR result = GC_malloc(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc(%ld) returning NIL (",
        	       (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

#ifdef STUBBORN_ALLOC
# ifdef __STDC__
    GC_PTR GC_debug_malloc_stubborn(size_t lb, EXTRA_ARGS)
# else
    GC_PTR GC_debug_malloc_stubborn(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_stubborn(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc(%ld) returning NIL (",
        	       (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

void GC_debug_change_stubborn(p)
GC_PTR p;
{
    register GC_PTR q = GC_base(p);
    register hdr * hhdr;
    
    if (q == 0) {
        GC_err_printf1("Bad argument: 0x%lx to GC_debug_change_stubborn\n",
        	       (unsigned long) p);
        ABORT("GC_debug_change_stubborn: bad arg");
    }
    hhdr = HDR(q);
    if (hhdr -> hb_obj_kind != STUBBORN) {
        GC_err_printf1("GC_debug_change_stubborn arg not stubborn: 0x%lx\n",
        	       (unsigned long) p);
        ABORT("GC_debug_change_stubborn: arg not stubborn");
    }
    GC_change_stubborn(q);
}

void GC_debug_end_stubborn_change(p)
GC_PTR p;
{
    register GC_PTR q = GC_base(p);
    register hdr * hhdr;
    
    if (q == 0) {
        GC_err_printf1("Bad argument: 0x%lx to GC_debug_end_stubborn_change\n",
        	       (unsigned long) p);
        ABORT("GC_debug_end_stubborn_change: bad arg");
    }
    hhdr = HDR(q);
    if (hhdr -> hb_obj_kind != STUBBORN) {
        GC_err_printf1("debug_end_stubborn_change arg not stubborn: 0x%lx\n",
        	       (unsigned long) p);
        ABORT("GC_debug_end_stubborn_change: arg not stubborn");
    }
    GC_end_stubborn_change(q);
}

#endif /* STUBBORN_ALLOC */

# ifdef __STDC__
    GC_PTR GC_debug_malloc_atomic(size_t lb, EXTRA_ARGS)
# else
    GC_PTR GC_debug_malloc_atomic(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_atomic(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc_atomic(%ld) returning NIL (",
        	      (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

# ifdef __STDC__
    GC_PTR GC_debug_malloc_uncollectable(size_t lb, EXTRA_ARGS)
# else
    GC_PTR GC_debug_malloc_uncollectable(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_uncollectable(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc_uncollectable(%ld) returning NIL (",
        	      (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

#ifdef ATOMIC_UNCOLLECTABLE
# ifdef __STDC__
    GC_PTR GC_debug_malloc_atomic_uncollectable(size_t lb, EXTRA_ARGS)
# else
    GC_PTR GC_debug_malloc_atomic_uncollectable(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_atomic_uncollectable(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1(
		"GC_debug_malloc_atomic_uncollectable(%ld) returning NIL (",
                (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}
#endif /* ATOMIC_UNCOLLECTABLE */

# ifdef __STDC__
    void GC_debug_free(GC_PTR p)
# else
    void GC_debug_free(p)
    GC_PTR p;
# endif
{
    register GC_PTR base = GC_base(p);
    register ptr_t clobbered;
    
    if (base == 0) {
        GC_err_printf1("Attempt to free invalid pointer %lx\n",
        	       (unsigned long)p);
        if (p != 0) ABORT("free(invalid pointer)");
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
        GC_err_printf1(
        	  "GC_debug_free called on pointer %lx wo debugging info\n",
        	  (unsigned long)p);
    } else {
      clobbered = GC_check_annotated_obj((oh *)base);
      if (clobbered != 0) {
        if (((oh *)base) -> oh_sz == GC_size(base)) {
            GC_err_printf0(
                  "GC_debug_free: found previously deallocated (?) object at ");
        } else {
            GC_err_printf0("GC_debug_free: found smashed object at ");
        }
        GC_print_smashed_obj(p, clobbered);
      }
      /* Invalidate size */
      ((oh *)base) -> oh_sz = GC_size(base);
    }
#   ifdef FIND_LEAK
        GC_free(base);
#   else
	{
	    register hdr * hhdr = HDR(p);
	    GC_bool uncollectable = FALSE;

	    if (hhdr ->  hb_obj_kind == UNCOLLECTABLE) {
		uncollectable = TRUE;
	    }
#	    ifdef ATOMIC_UNCOLLECTABLE
		if (hhdr ->  hb_obj_kind == AUNCOLLECTABLE) {
		    uncollectable = TRUE;
		}
#	    endif
	    if (uncollectable) GC_free(base);
	}
#   endif
}

# ifdef __STDC__
    GC_PTR GC_debug_realloc(GC_PTR p, size_t lb, EXTRA_ARGS)
# else
    GC_PTR GC_debug_realloc(p, lb, s, i)
    GC_PTR p;
    size_t lb;
    char *s;
    int i;
# endif
{
    register GC_PTR base = GC_base(p);
    register ptr_t clobbered;
    register GC_PTR result;
    register size_t copy_sz = lb;
    register size_t old_sz;
    register hdr * hhdr;
    
    if (p == 0) return(GC_debug_malloc(lb, OPT_RA s, i));
    if (base == 0) {
        GC_err_printf1(
              "Attempt to reallocate invalid pointer %lx\n", (unsigned long)p);
        ABORT("realloc(invalid pointer)");
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
        GC_err_printf1(
        	"GC_debug_realloc called on pointer %lx wo debugging info\n",
        	(unsigned long)p);
        return(GC_realloc(p, lb));
    }
    hhdr = HDR(base);
    switch (hhdr -> hb_obj_kind) {
#    ifdef STUBBORN_ALLOC
      case STUBBORN:
        result = GC_debug_malloc_stubborn(lb, OPT_RA s, i);
        break;
#    endif
      case NORMAL:
        result = GC_debug_malloc(lb, OPT_RA s, i);
        break;
      case PTRFREE:
        result = GC_debug_malloc_atomic(lb, OPT_RA s, i);
        break;
      case UNCOLLECTABLE:
	result = GC_debug_malloc_uncollectable(lb, OPT_RA s, i);
 	break;
#    ifdef ATOMIC_UNCOLLECTABLE
      case AUNCOLLECTABLE:
	result = GC_debug_malloc_atomic_uncollectable(lb, OPT_RA s, i);
	break;
#    endif
      default:
        GC_err_printf0("GC_debug_realloc: encountered bad kind\n");
        ABORT("bad kind");
    }
    clobbered = GC_check_annotated_obj((oh *)base);
    if (clobbered != 0) {
        GC_err_printf0("GC_debug_realloc: found smashed object at ");
        GC_print_smashed_obj(p, clobbered);
    }
    old_sz = ((oh *)base) -> oh_sz;
    if (old_sz < copy_sz) copy_sz = old_sz;
    if (result == 0) return(0);
    BCOPY(p, result,  copy_sz);
    GC_debug_free(p);
    return(result);
}

/* Check all marked objects in the given block for validity */
/*ARGSUSED*/
void GC_check_heap_block(hbp, dummy)
register struct hblk *hbp;	/* ptr to current heap block		*/
word dummy;
{
    register struct hblkhdr * hhdr = HDR(hbp);
    register word sz = hhdr -> hb_sz;
    register int word_no;
    register word *p, *plim;
    
    p = (word *)(hbp->hb_body);
    word_no = HDR_WORDS;
    if (sz > MAXOBJSZ) {
	plim = p;
    } else {
    	plim = (word *)((((word)hbp) + HBLKSIZE) - WORDS_TO_BYTES(sz));
    }
    /* go through all words in block */
	while( p <= plim ) {
	    if( mark_bit_from_hdr(hhdr, word_no)
	        && GC_has_debug_info((ptr_t)p)) {
	        ptr_t clobbered = GC_check_annotated_obj((oh *)p);
	        
	        if (clobbered != 0) {
	            GC_err_printf0(
	                "GC_check_heap_block: found smashed object at ");
        	    GC_print_smashed_obj((ptr_t)p, clobbered);
	        }
	    }
	    word_no += sz;
	    p += sz;
	}
}


/* This assumes that all accessible objects are marked, and that	*/
/* I hold the allocation lock.	Normally called by collector.		*/
void GC_check_heap_proc()
{
#   ifndef SMALL_CONFIG
	if (sizeof(oh) & (2 * sizeof(word) - 1) != 0) {
	    ABORT("Alignment problem: object header has inappropriate size\n");
	}
#   endif
    GC_apply_to_all_blocks(GC_check_heap_block, (word)0);
}

struct closure {
    GC_finalization_proc cl_fn;
    GC_PTR cl_data;
};

# ifdef __STDC__
    void * GC_make_closure(GC_finalization_proc fn, void * data)
# else
    GC_PTR GC_make_closure(fn, data)
    GC_finalization_proc fn;
    GC_PTR data;
# endif
{
    struct closure * result =
    		(struct closure *) GC_malloc(sizeof (struct closure));
    
    result -> cl_fn = fn;
    result -> cl_data = data;
    return((GC_PTR)result);
}

# ifdef __STDC__
    void GC_debug_invoke_finalizer(void * obj, void * data)
# else
    void GC_debug_invoke_finalizer(obj, data)
    char * obj;
    char * data;
# endif
{
    register struct closure * cl = (struct closure *) data;
    
    (*(cl -> cl_fn))((GC_PTR)((char *)obj + sizeof(oh)), cl -> cl_data);
} 


# ifdef __STDC__
    void GC_debug_register_finalizer(GC_PTR obj, GC_finalization_proc fn,
    				     GC_PTR cd, GC_finalization_proc *ofn,
				     GC_PTR *ocd)
# else
    void GC_debug_register_finalizer(obj, fn, cd, ofn, ocd)
    GC_PTR obj;
    GC_finalization_proc fn;
    GC_PTR cd;
    GC_finalization_proc *ofn;
    GC_PTR *ocd;
# endif
{
    ptr_t base = GC_base(obj);
    if (0 == base || (ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf1(
	    "GC_register_finalizer called with non-base-pointer 0x%lx\n",
	    obj);
    }
    GC_register_finalizer(base, GC_debug_invoke_finalizer,
    			  GC_make_closure(fn,cd), ofn, ocd);
}

# ifdef __STDC__
    void GC_debug_register_finalizer_no_order
    				    (GC_PTR obj, GC_finalization_proc fn,
    				     GC_PTR cd, GC_finalization_proc *ofn,
				     GC_PTR *ocd)
# else
    void GC_debug_register_finalizer_no_order
    				    (obj, fn, cd, ofn, ocd)
    GC_PTR obj;
    GC_finalization_proc fn;
    GC_PTR cd;
    GC_finalization_proc *ofn;
    GC_PTR *ocd;
# endif
{
    ptr_t base = GC_base(obj);
    if (0 == base || (ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf1(
	  "GC_register_finalizer_no_order called with non-base-pointer 0x%lx\n",
	  obj);
    }
    GC_register_finalizer_no_order(base, GC_debug_invoke_finalizer,
     			  	      GC_make_closure(fn,cd), ofn, ocd);
 }

# ifdef __STDC__
    void GC_debug_register_finalizer_ignore_self
    				    (GC_PTR obj, GC_finalization_proc fn,
    				     GC_PTR cd, GC_finalization_proc *ofn,
				     GC_PTR *ocd)
# else
    void GC_debug_register_finalizer_ignore_self
    				    (obj, fn, cd, ofn, ocd)
    GC_PTR obj;
    GC_finalization_proc fn;
    GC_PTR cd;
    GC_finalization_proc *ofn;
    GC_PTR *ocd;
# endif
{
    ptr_t base = GC_base(obj);
    if (0 == base || (ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf1(
	    "GC_register_finalizer_ignore_self called with non-base-pointer 0x%lx\n",
	    obj);
    }
    GC_register_finalizer_ignore_self(base, GC_debug_invoke_finalizer,
    			  	      GC_make_closure(fn,cd), ofn, ocd);
}
