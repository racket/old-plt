
#ifndef __mzscheme_gc_2__
#define __mzscheme_gc_2__

# ifdef __cplusplus
extern "C" {
# endif

#define GC_PTR void*

void GC_add_roots(void *start, void *end);

extern void (*GC_collect_start_callback)(void);
extern void (*GC_collect_end_callback)(void);
extern void (*GC_out_of_memory)(void);
extern unsigned long (*GC_get_thread_stack_base)(void);

/* Needed for stack-overflow checks: */
void GC_set_stack_base(void *base);
unsigned long GC_get_stack_base(void);

void GC_dump(void);

long GC_get_memory_use();

void GC_end_stubborn_change(void *);

void GC_gcollect(void);

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes);
#define GC_malloc_stubborn GC_malloc

/* Tagged item: */
void *GC_malloc_one_tagged(size_t);
#define GC_malloc_one_stubborn_tagged GC_malloc_one_tagged
void *GC_malloc_array_tagged(size_t);

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes);

#define GC_malloc_atomic_tagged GC_malloc_one_tagged

/* Plain malloc: */
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
#define GC_malloc_eternal_tagged GC_malloc_atomic_uncollectable

/* Array of weak pointers: */
void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val);

void GC_free(void *); /* noop */

void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
				 void *data, void (**oldf)(void *p, void *data), 
				 void **olddata);

/* Only used to clear finalizer: */
void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
			   void *data, void (**oldf)(void *p, void *data), 
			   void **olddata);

typedef struct GC_Weak_Box {
  Scheme_Type type;
  short keyex;
  struct Scheme_Object *val;
  struct Scheme_Object *secondary_erase;
  struct GC_Weak_Box *next;
} GC_Weak_Box;

extern void **GC_variable_stack;
extern int GC_variable_count;

typedef void *(*Mark_Proc)(void *);
typedef int (*Traverse_Proc)(void *, Mark_Proc);
void GC_register_traverser(Scheme_Type tag, Traverse_Proc proc);

void *GC_resolve(void *p);
void GC_mark_variable_stack(void **var_stack,
			    int var_count,
			    long delta,
			    void *limit);

extern void *GC_alloc_space, *GC_alloc_top;

#define gcMARK(x) if (!((long)(x) & 0x1) && ((void *)(x) >= GC_alloc_space) && ((void *)(x) <= GC_alloc_top)) x = mark(x)
#define gcBYTES_TO_WORDS(x) ((x + 3) >> 2)

# ifdef __cplusplus
};
# endif

#endif /* __mzscheme_gc_2__ */
