
#ifndef __mzscheme_gc_2__
#define __mzscheme_gc_2__

#ifndef GC2_JUST_MACROS

#include <stddef.h>

# ifdef __cplusplus
extern "C" {
# endif

/*
   General architecture:

   Each allocation is for either tagged or untagged memory. Untagged
   memory must be either atomic or an array of pointers. Heterogenous
   memory is always tagged. After alocating a tagged object, MzScheme
   is responsible for setting the tag before a collection occurs. The
   tag is always a `short' value at the beginning of the

   MzScheme supplies three traversal procedures for every tag value:
   one for obtaining the value's size in words (not bytes!), one for
   marking pointers within the value, and one for fixing up pointers
   in the value. The mark and fixup procedures should use gcMARK and
   gcFIXUP. The fixup procedure must also return the size, like the
   size procedure.

   The value of a pointer can refer to a GCable object, to the middle
   of a GCable object, or to some other point in memory. It might also
   be a fixnum value, which has 1 in the least-significant bit,
   whereas actual pointers are always 2-byte-aligned. Thus, when
   moving GCable objects in a copying collector, the collector must
   modify pointers to objects or to the middle of objects, but leave
   other pointers and fixnums alone.

   At any point when the allocator/collector is invoked, MzScheme will
   have set the GC_variable_stack variable to indicate a chain of
   local pointer variables on the stack (i.e., both the chain of
   record and the pointer variables themselves are on the stack). The
   GC_variable_stack global points to a value of the form

     struct {
        void *next_frame;
        long frame_size;
        void **pointers[...];
     }

   where the size of `pointers' is indicated by `frame_size'. Each
   element of `pointers' is the address of a pointer of the stack.
   The `next_frame' field in the structure gives the address of the
   next frame on the stack, and so on. The garbage collector should
   follow the chain of frames, adjusting pointers for copying
   collection, until it reaches a frame that is deeper than the value
   returned by GC_get_thread_stack_base() (which is supplied by
   MzScheme).

   More generally, GC_mark_variable_stack() can be used to GC a stack
   that has been copied into the heap. See below for more details.  */

/***************************************************************************/
/* Administration                                                          */
/***************************************************************************/

extern unsigned long (*GC_get_thread_stack_base)(void);
/* 
   Called by GC to get the base for stack traversal in the current
   thread. The returned address must not be in the middle of
   a variable-stack record. */

void GC_set_stack_base(void *base);
unsigned long GC_get_stack_base(void);
/*
   Called by MzScheme to set/get value used for stack base when
   GC_get_thread_stack_base is null. This is mainly useful for getting
   MzScheme started, before it has multiple threads. */

void GC_add_roots(void *start, void *end);
/*
   Called by MzScheme to install roots. The memory between
   `start' (inclusive) and `end' (exclusive) contains pointers. */

void GC_init_type_tags(int count, int weakbox);
/*
   Called by MzScheme to indicate the number of different type tags it
   uses, starting from 0. `count' is always less than 256. The weakbox
   argument is the value to be used for tagging weak box. (The GC has
   some freedom in the layout of a weak box, so it performs weak box
   traversals itself, but MzScheme gets to choose the tag.) */

extern void (*GC_collect_start_callback)(void);
extern void (*GC_collect_end_callback)(void);
/*
   Called by GC before/after performing a collection. Used by MzScheme
   to zero out some data and record collection times. */

extern void (*GC_out_of_memory)(void);
/*
   Called by GC when it can't satify a memory request. GC_out_of_memory
   might perform a longjmp. */

void GC_dump(void);
/*
   Dumps memory state info to stderr. */

long GC_get_memory_use();
/*
   Returns the number of currently-allocated bytes. */

void GC_gcollect(void);
/*
   Performs an immediate collection. */

/***************************************************************************/
/* Allocation                                                              */
/***************************************************************************/

/* Note: All alocated memory must be longword-aligned. For architectures
   where `double' values must be 8-byte aligned, the GC must provide
   8-byte aligned memory in response to an allocation request for a
   memory size divisible by 8. */

void *GC_malloc(size_t size_in_bytes);
/*
   Alloc an array of pointers, initially zeroed. */

void *GC_malloc_one_tagged(size_t);
/* 
   Alloc a tagged item, initially zeroed.  MzScheme sets the tag
   before a collection. */

void *GC_malloc_array_tagged(size_t);
/* 
   Alloc an array of tagged items. MzScheme sets the tag in the first
   item before a collection, by maybe not all items. When traversing,
   use the first one for size, and traverse only those with the tag
   set. */

void *GC_malloc_atomic(size_t size_in_bytes);
/*
   Alloc pointerless memory (not necessarily zeroed). */

#define GC_malloc_atomic_tagged GC_malloc_one_tagged
/*
   Alloc pointer-free tagged memory (not necessarily zeroed).
   MzScheme sets the tag before a collection. */

void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
/*
   Like plain malloc: pointer-free, never collected. */

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val);
/* 
   Alloc an array of weak pointers, initially zeroed.  When a value in
   the array is collected, it's replaced by `replace-val'. The
   precense of a pointer in the array doesn't keep the referenced
   memory from being collected. */

void GC_free(void *);
/* 
   Lets the collector optionally reverse an allocation immediately.
   [Generally a noop.] */

void *GC_malloc_weak_box(void *p, void **secondary);
/* 
   Allocate a weak box. A weak box must have the following initial
   structure:

     struct {
       short tag;
       short filler_used_for_hashing;
       void *val;
     }

   but the remainder of the structure is up to the GC. The GC recives
   from MzScheme a tag value to be used for weak boxes (see the
   GC_init_type_tags() function above), but the GC is responsible for
   traversing weak boxes.

   MzScheme can change `val' at any time; when a collection happens,
   if the object in `val' is collectable and is collected, then `val'
   is zeroed.  The `val' pointer must be updated by the GC if the
   object it refers to is moved by the GC, but it does not otherwise
   keep an object from being collected. However, if `val' can be
   moved, it must point to the beginning of the allocated object.

   The `secondary' argument points to an auxilliary address (probably
   in the middle of a collectable object) that should be zeroed
   whenever `val' is zeroed. The memory referenced by `secondary' is
   kept live as long as it isn't zeroed by its registration in the
   weak box, but when the content of `secondary' is zeroed, the
   `secondary' pointer itself should be dropped. */

void *GC_weak_box_val(void *wb);
void GC_set_weak_box_val(void *wb, void *v);
/*
   Gets/sets the `val' field in a structure matching the required
   header of a weak box (see GC_malloc_weak_box). */


void *GC_malloc_immobile_box(void *p);
void GC_free_immobile_box(void *b);
/* 
   Allocate an non-GCed box containing a pointer to a GCed value.
   The pointer is stored as the first longword of the box. */

/***************************************************************************/
/* Finalization                                                            */
/***************************************************************************/

typedef void (*GC_finalization_proc)(void *p, void *data);
/*
   Type of a finalization procedure. */

void GC_register_eager_finalizer(void *p, int level, 
				 GC_finalization_proc f, void *data, 
				 GC_finalization_proc *oldf, void **olddata);
/*
   Installs a finalizer to be queued for invocation when `p' would
   otherwise be collected. `p' isn't actually collected when a
   finalizer is queued, since the finalizer will receive `p' as an
   argument. (Hence, weak references aren't zeroed, either.) `p' must
   point to the beginning of a tagged, allocated object.

   `level' refers to an ordering of finalizers. It can be 1, 2, or
   3. During a collection, level 1 finalizers are queued first, then
   all objects queued for finalization are marked as live and
   traversed. Then, level 2 finalizers are queued in the same
   way. Thus, if a level 1 object refers to a level 2 object, the
   level 1 object will be queued for finalization, and only sometime
   after the finalizer is run and the object is again no longer
   refermced can the level 2 object be finalized.

   Level 3 finalizers are even later. Not only are they after level 1
   and 2, but a level 3 finalizer is only enqueued if no other level-3
   finalizer refers to the object. Note that cycles among level-3
   finalizers can prevent finalization and collection. (But it's also
   possible that other finalizers will break a finalization cycle
   among a set of level 3 finalizers.)

   The `f' and `data' arguments are the finalizer clsoure to be
   called. If a finalizer is already installed for `p', it is
   replaced, and `oldf' and `olddata' are filled with the old
   closure. If `f' is NULL, any existing finalizer is removed and no
   new one is installed. */

void GC_register_finalizer(void *p, 
			   GC_finalization_proc f, void *data, 
			   GC_finalization_proc *oldf, void **olddata);
/* 
   Installs a level-3 finalizer. */

void GC_finalization_weak_ptr(void **p);
/*
   Registers a "weak" pointer for level-3 finalization. `p' must be a
   portion of a finalized object. When checking for references among
   level-3 finalized objects, *p is set to NULL. The mark procedure
   for the object containing `p' will see the NULL value, preventing
   it from markign whatever `p' normally references. After level-3
   finalizers are enqueued, `p' is reset to its original value (and
   marked if the object containing `p' is already marked).

   When the object containing `p' is collected, the weak pointer
   registration is removed automatically. */

/***************************************************************************/
/* Cooperative GC                                                          */
/***************************************************************************/

extern void **GC_variable_stack;
/*
   See the general overview at the top of the file: */

typedef int (*Size_Proc)(void *obj);
typedef int (*Mark_Proc)(void *obj);
typedef int (*Fixup_Proc)(void *obj);
/* 
   Types of the traversal procs (supplied by MzScheme); see overview above
   for information about traversals. The return value is the size of
   the object in words. */

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup);
/*
   Registers a traversal procedure for a tag. Obviously, a traversal
   procedure must be installed for each tag before a collection
   happens where an instance of the tag as been allocated. */

/* #define gcMARK(x) ... see below ... */
/* #define gcFIXUP(x) ... see below ... */
/* #define gcFIXUP_TYPED(t, x) ... see below ... */
/* Macros that, given an l-value and optional type, marks the
   referenced memory as live and updates the pointer as necessary
   (i.e., if it's GCable memory that is moving). The `x' argument can
   appear in the macro's output multiple times, and the output can be
   a statement rather than a expression. */

/* #define gcBYTES_TO_WORDS(x) ((x + 3) >> 2) */
/*
   Helpful macro for computing the return value in a traversal proc,
   which must be in words. */

void *GC_resolve(void *p);
/*
   Can be called by a traversal proc to get the current address of a
   object that might have been moved already. This is necessary, for
   example, if the size or structure of an object depends on the
   content of an object it references. For example, the size of a
   class instance usually depends on a field count that is stored in
   the class. */

/* INTERNAL: */
void GC_mark(void *p);
void GC_fixup(void *p);
/*
   Used in the expansion of gcMARK and gcFIXUP. */

void GC_mark_variable_stack(void **var_stack,
			    long delta,
			    void *limit);
void GC_fixup_variable_stack(void **var_stack,
			     long delta,
			     void *limit);
/*
   Can be called by a mark or fixup traversal proc to traverse and
   update a chunk of (atomically-allocated) memory containing an image
   of the stack.

   The `var_stack' argument corresponds to the value of GC_var_stack
   for the copied stack (see the overview at the top of this
   file). The `var_stack' pointer refers to the address of the chain
   in the original stack, not in the heap copy. The `delta' argument
   specifies the difference heap_copy_address - stack_address (where
   stack_address is the numerically lower bound for the copied stack
   region, regardless of which direction the stack grows). The `limit'
   argument corresponds to the value that would have been returned by
   GC_get_thread_stack_base() at the time the stack was copied. */

extern void *GC_alloc_space, *GC_alloc_top;
/*
   Used by macro the implementations above; not to be considered part
   of the spec. */

# ifdef __cplusplus
};
# endif

#endif

/* Macros: */
#define gcMARK(x) GC_mark((void *)x)
#define gcMARK_TYPED(t, x) gcMARK(x)
#define gcFIXUP_TYPED(t, x) GC_fixup(&(x))
#define gcFIXUP(x) gcFIXUP_TYPED(void*, x)
#define gcBYTES_TO_WORDS(x) ((x + 3) >> 2)
#define gcWORDS_TO_BYTES(x) (x << 2)

#endif /* __mzscheme_gc_2__ */
