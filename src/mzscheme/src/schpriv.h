/*
  MzScheme
  Copyright (c) 1995-2001 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* 
   MzScheme prototypes and declarations for internal consumption.
*/

#ifndef __mzscheme_private__
#define __mzscheme_private__

#include "scheme.h"

/*========================================================================*/
/*                         allocation and GC                              */
/*========================================================================*/

#define MAKE_CLOSED_PRIM(f,v,n,mi,ma) \
  scheme_make_closed_prim_w_arity((Scheme_Closed_Prim *)f, (void *)v, n, mi, ma)

#define _MALLOC_N(x, n, malloc) ((x*)malloc(sizeof(x)*(n)))
#define MALLOC_ONE(x) _MALLOC_N(x, 1, scheme_malloc)
#define MALLOC_ONE_TAGGED(x) _MALLOC_N(x, 1, scheme_malloc_tagged)
#define MALLOC_N_TAGGED(x, n) _MALLOC_N(x, n, scheme_malloc_array_tagged)
#ifdef MZTAG_REQUIRED
# define scheme_malloc_rt(x) scheme_malloc_tagged(x)
# define MALLOC_ONE_RT(x) MALLOC_ONE_TAGGED(x)
# define MALLOC_N_RT(x,c) MALLOC_N_TAGGED(x,c)
# define MALLOC_ONE_WEAK(x) _MALLOC_N(x, 1, scheme_malloc)
# define MALLOC_N_WEAK(x,c) _MALLOC_N(x, c, scheme_malloc)
# define MALLOC_ONE_TAGGED_WEAK(x) _MALLOC_N(x, 1, scheme_malloc_tagged)
# define MALLOC_ONE_WEAK_RT(x) MALLOC_ONE_TAGGED_WEAK(x)
#else
# define scheme_malloc_rt(x) scheme_malloc(x)
# define MALLOC_ONE_RT(x) MALLOC_ONE(x)
# define MALLOC_N_RT(x,c) MALLOC_N(x,c)
# define MALLOC_ONE_WEAK(x) MALLOC_ONE_ATOMIC(x)
# define MALLOC_N_WEAK(x,c) MALLOC_N_ATOMIC(x,c)
# define MALLOC_ONE_WEAK_RT(x) MALLOC_ONE_WEAK(x)
# define MALLOC_ONE_TAGGED_WEAK(x) MALLOC_ONE_WEAK(x)
#endif
#define MALLOC_N(x, n) _MALLOC_N(x, n, scheme_malloc)
#define MALLOC_ONE_ATOMIC(x) _MALLOC_N(x, 1, scheme_malloc_atomic)
#define MALLOC_N_ATOMIC(x, n) _MALLOC_N(x, n, scheme_malloc_atomic)
#define MALLOC_SO_BOX() _MALLOC_ONE(Scheme_Object*, scheme_malloc)
#define MALLOC_N_STUBBORN(x, n) _MALLOC_N(x, n, scheme_malloc_stubborn)

#ifdef MZ_PRECISE_GC
# define WEAKIFY(x) scheme_make_weak_box(x)
# define WEAKIFIED(x) SCHEME_WEAK_BOX_VAL(x)
# define HT_EXTRACT_WEAK(x) SCHEME_WEAK_BOX_VAL(x)
#else
# define WEAKIFY(x) x
# define WEAKIFIED(x) x
# define HT_EXTRACT_WEAK(x) (*(char **)(x))
#endif

#ifndef MZ_PRECISE_GC
# define START_XFORM_SKIP /**/
# define END_XFORM_SKIP /**/
# define GC_CAN_IGNORE /**/
# define GC_MAYBE_IGNORE_INTERIOR /**/
#else
# ifdef GC_INTERIORABLES_NEVER_MOVE
#  define GC_MAYBE_IGNORE_INTERIOR GC_CAN_IGNORE
# else
#  define GC_MAYBE_IGNORE_INTERIOR /**/
# endif
#endif

#ifdef MZ_PRECISE_GC
long scheme_hash_key(Scheme_Object *o);
#else
# define scheme_hash_key(o) ((long)(o))
#endif
typedef int (*Compare_Proc)(void *v1, void *v2);

Scheme_Object *scheme_dump_gc_stats(int c, Scheme_Object *p[]);

#define REGISTER_SO(x) MZ_REGISTER_STATIC(x)

extern long scheme_total_gc_time;

int scheme_num_types(void);

#ifdef MZTAG_REQUIRED
# define MZTAG_IF_REQUIRED  Scheme_Type type;
#else
# define MZTAG_IF_REQUIRED /* empty */
#endif

void scheme_reset_finalizations(void);

extern unsigned long scheme_get_stack_base();

#ifndef MZ_PRECISE_GC
# define HIDE_FROM_XFORM(x) x
#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

extern int scheme_starting_up;

void scheme_init_portable_case(void);
void scheme_init_stack_check(void);
#ifdef MZ_PRECISE_GC
void scheme_register_traversers(void);
void scheme_init_hash_key_procs(void);
#endif
Scheme_Thread *scheme_make_thread(void);
void scheme_init_true_false(void);
void scheme_init_symbol_table(void);
void scheme_init_symbol_type(Scheme_Env *env);
void scheme_init_type(Scheme_Env *env);
void scheme_init_list(Scheme_Env *env);
void scheme_init_stx(Scheme_Env *env);
void scheme_init_module(Scheme_Env *env);
void scheme_init_port(Scheme_Env *env);
void scheme_init_port_fun(Scheme_Env *env);
void scheme_init_network(Scheme_Env *env);
void scheme_init_file(Scheme_Env *env);
void scheme_init_proc(Scheme_Env *env);
void scheme_init_vector(Scheme_Env *env);
void scheme_init_string(Scheme_Env *env);
void scheme_init_number(Scheme_Env *env);
void scheme_init_numarith(Scheme_Env *env);
void scheme_init_numcomp(Scheme_Env *env);
void scheme_init_numstr(Scheme_Env *env);
void scheme_init_eval(Scheme_Env *env);
void scheme_init_promise(Scheme_Env *env);
void scheme_init_struct(Scheme_Env *env);
void scheme_init_fun(Scheme_Env *env);
void scheme_init_symbol(Scheme_Env *env);
void scheme_init_char(Scheme_Env *env);
void scheme_init_bool(Scheme_Env *env);
void scheme_init_syntax(Scheme_Env *env);
void scheme_init_error(Scheme_Env *env);
#ifndef NO_SCHEME_EXNS
void scheme_init_exn(Scheme_Env *env);
#endif
void scheme_init_debug(Scheme_Env *env);
void scheme_init_thread(Scheme_Env *env);
void scheme_init_read(Scheme_Env *env);
void scheme_init_print(Scheme_Env *env);
void scheme_init_image(Scheme_Env *env);
#ifndef NO_SCHEME_THREADS
void scheme_init_sema(Scheme_Env *env);
#endif
void scheme_init_dynamic_extension(Scheme_Env *env);
#ifndef NO_REGEXP_UTILS
extern void scheme_regexp_initialize(Scheme_Env *env);
#endif
void scheme_init_getenv(void);

/* Type readers & writers for compiled code data */
typedef Scheme_Object *(*Scheme_Type_Reader)(Scheme_Object *list);
typedef Scheme_Object *(*Scheme_Type_Writer)(Scheme_Object *obj);

extern Scheme_Type_Reader *scheme_type_readers;
extern Scheme_Type_Writer *scheme_type_writers;

void scheme_init_port_config(void);
void scheme_init_port_fun_config(void);
void scheme_init_error_escape_proc(Scheme_Thread *p);
void scheme_init_error_config(void);
#ifndef NO_SCHEME_EXNS
void scheme_init_exn_config(void);
#endif
#ifdef WINDOWS_PROCESSES
void scheme_init_thread_memory(void);
#endif
    
void scheme_finish_kernel(Scheme_Env *env);

Scheme_Object *scheme_make_initial_inspectors(void);

extern int scheme_builtin_ref_counter;

Scheme_Object **scheme_make_builtin_references_table(void);
Scheme_Object *scheme_make_local(Scheme_Type type, int pos);

void scheme_add_embedded_builtins(Scheme_Env *env);
void scheme_do_add_global_symbol(Scheme_Env *env, Scheme_Object *sym, 
				 Scheme_Object *obj, int constant,
				 int primitive);

/*========================================================================*/
/*                                constants                               */
/*========================================================================*/

extern Scheme_Object *scheme_values_func;

extern Scheme_Object *scheme_not_prim;
extern Scheme_Object *scheme_define_values_syntax, *scheme_define_syntaxes_syntax;
extern Scheme_Object *scheme_lambda_syntax;
extern Scheme_Object *scheme_begin_syntax;

extern Scheme_Object *scheme_def_exit_proc;

extern Scheme_Object *scheme_orig_stdout_port;
extern Scheme_Object *scheme_orig_stdin_port;
extern Scheme_Object *scheme_orig_stderr_port;

extern Scheme_Object *scheme_arity_at_least;

extern Scheme_Object *scheme_write_proc, *scheme_display_proc, *scheme_print_proc;

#ifdef TIME_SYNTAX
extern Scheme_Object *scheme_date;
#endif

/*========================================================================*/
/*                    thread state and maintenance                        */
/*========================================================================*/

#define RUNSTACK_IS_GLOBAL

#ifdef RUNSTACK_IS_GLOBAL
extern Scheme_Object **scheme_current_runstack;
extern Scheme_Object **scheme_current_runstack_start;
extern MZ_MARK_POS_TYPE scheme_current_cont_mark_stack;
extern MZ_MARK_STACK_TYPE scheme_current_cont_mark_pos;
# define MZ_RUNSTACK scheme_current_runstack
# define MZ_RUNSTACK_START scheme_current_runstack_start
# define MZ_CONT_MARK_STACK scheme_current_cont_mark_stack
# define MZ_CONT_MARK_POS scheme_current_cont_mark_pos
#else
# define MZ_RUNSTACK (p->runstack)
# define MZ_RUNSTACK_START (p->runstack_start)
# define MZ_CONT_MARK_STACK (p->cont_mark_stack)
# define MZ_CONT_MARK_POS (p->cont_mark_pos)
#endif

extern volatile int scheme_fuel_counter;

extern Scheme_Thread *scheme_main_thread;

/* Flags for Scheme_Thread's `running' field: */
#define MZTHREAD_RUNNING 0x1
#define MZTHREAD_SUSPENDED 0x2
#define MZTHREAD_KILLED 0x4
#define MZTHREAD_NEED_KILL_CLEANUP 0x8
#define MZTHREAD_STILL_RUNNING(running) ((running) && !((running) & MZTHREAD_KILLED))

#ifdef WINDOWS_PROCESSES
MZ_EXTERN struct Scheme_Thread_Memory *scheme_remember_thread(void *, int);
void scheme_remember_subthread(struct Scheme_Thread_Memory *, void *);
MZ_EXTERN void scheme_forget_thread(struct Scheme_Thread_Memory *);
void scheme_forget_subthread(struct Scheme_Thread_Memory *);
void scheme_suspend_remembered_threads(void);
void scheme_resume_remembered_threads(void);
#endif
#ifdef USE_WIN32_THREAD_TIMER
void scheme_start_itimer_thread(long usec);
#endif

#ifdef UNIX_PROCESSES
void scheme_block_child_signals(int block);
#endif

void scheme_alloc_list_stack(Scheme_Thread *p);
void scheme_clean_list_stack(Scheme_Thread *p);

#ifdef WIN32_THREADS
void *scheme_win32_get_break_semaphore(void *th);
#endif

void scheme_zero_unneeded_rands(Scheme_Thread *p);

int scheme_can_break(Scheme_Thread *p, Scheme_Config *config);

#define MZTHREADELEM(p, x) scheme_ ## x

struct Scheme_Custodian {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int count, alloc;
  Scheme_Object ***boxes;
  Scheme_Custodian_Reference **mrefs;
  Scheme_Close_Custodian_Client **closers;
  void **data;

  /* weak indirections: */
  Scheme_Custodian_Reference *parent;
  Scheme_Custodian_Reference *sibling;
  Scheme_Custodian_Reference *children;
};

Scheme_Thread *scheme_do_close_managed(Scheme_Custodian *m, Scheme_Exit_Closer_Func f);

typedef struct Scheme_Security_Guard {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  struct Scheme_Security_Guard *parent;
  Scheme_Object *file_proc;    /* who-symbol path-string mode-symbol -> void */
  Scheme_Object *network_proc; /* who-symbol host-string-or-'listen port-k -> void */
} Scheme_Security_Guard;

/*========================================================================*/
/*                       hash tables and globals                          */
/*========================================================================*/

#define GLOB_IS_CONST 1
#define GLOB_IS_PRIMITIVE 4
#define GLOB_IS_PERMANENT 8
#define GLOB_HAS_REF_ID 16
#define GLOB_HAS_HOME_PTR 32

typedef struct {
  Scheme_Bucket bucket;
  short flags, id;
} Scheme_Bucket_With_Flags;

typedef Scheme_Bucket_With_Flags Scheme_Bucket_With_Ref_Id;

typedef struct {
  Scheme_Bucket_With_Ref_Id bucket;
  Scheme_Env *home;
} Scheme_Bucket_With_Home;

Scheme_Object *
scheme_get_primitive_global(Scheme_Object *var, Scheme_Env *env, 
			    int bucket_ok, int can_opt, int signal);

void scheme_add_bucket_to_table(Scheme_Bucket_Table *table, Scheme_Bucket *b);
Scheme_Bucket *scheme_bucket_or_null_from_table(Scheme_Bucket_Table *table, const char *key, int add);

void scheme_require_from_original_env(Scheme_Env *env, int syntax_only);

/*========================================================================*/
/*                              structs                                   */
/*========================================================================*/

typedef struct Scheme_Inspector {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int depth;
  struct Scheme_Inspector *superior;
} Scheme_Inspector;

typedef struct Scheme_Struct_Property {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *name; /* a symbol */
} Scheme_Struct_Property;

int scheme_is_subinspector(Scheme_Object *i, Scheme_Object *sup);
int scheme_inspector_sees_part(Scheme_Object *s, Scheme_Object *insp, int pos);

typedef struct Scheme_Struct_Type {
  Scheme_Type type; /* scheme_structure_type or scheme_proc_struct_type */
  MZ_HASH_KEY_EX
  short num_slots, num_islots;
  short name_pos;

  Scheme_Object *name;

  Scheme_Object *inspector;
  Scheme_Object *accessor, *mutator;

  Scheme_Object *uninit_val;

  Scheme_Object **props; /* normally an array of pair of (property, value) pairs */
  int num_props; /* < 0 => props is really a hash table */

  Scheme_Object *proc_attr; /* int (position) or proc, only for proc_struct */

  struct Scheme_Struct_Type *parent_types[1];
} Scheme_Struct_Type;

typedef struct Scheme_Structure
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Struct_Type *stype;
  Scheme_Object *slots[1];
} Scheme_Structure;

#define SCHEME_STRUCT_TYPE(o) (((Scheme_Structure *)o)->stype)

#define SCHEME_STRUCT_NUM_SLOTS(o) (SCHEME_STRUCT_TYPE(o)->num_slots)
#define SCHEME_STRUCT_NAME_SYM(o) (SCHEME_STRUCT_TYPE(o)->name)

Scheme_Object **scheme_make_struct_names_from_array(const char *base, 
						    int fcount,
						    const char **field_names,
						    int flags, int *count_out);
Scheme_Object *scheme_make_struct_type_from_string(const char *base, 
						   Scheme_Object *parent, 
						   int num_fields);

Scheme_Object *scheme_struct_to_vector(Scheme_Object *_s, Scheme_Object *unknown_val, Scheme_Object *insp);

Scheme_Object *scheme_extract_struct_procedure(Scheme_Object *obj, int num_rands, Scheme_Object **rands, int *is_method);

Scheme_Object *scheme_proc_struct_name_source(Scheme_Object *a);

#define SCHEME_STRUCT_INSPECTOR(obj) (((Scheme_Structure *)obj)->stype->inspector)

/*========================================================================*/
/*                         syntax objects                                 */
/*========================================================================*/

typedef struct Scheme_Stx_Srcloc {
  MZTAG_IF_REQUIRED
  long line, col, pos, span;
  Scheme_Object *src;
} Scheme_Stx_Srcloc;

#define STX_GRAPH_FLAG 0x1
#define STX_SUBSTX_FLAG 0x2

typedef struct Scheme_Stx {
  Scheme_Type type;
  short hash_code; /* Precise GC; 0x1 and 0x2 used */
  Scheme_Object *val;
  Scheme_Stx_Srcloc *srcloc;
  Scheme_Object *wraps;
  union {
    long lazy_prefix; /* # of initial items in wraps to propagate */
    Scheme_Object *modinfo_cache;
  } u;
  Scheme_Object *props;
} Scheme_Stx;

typedef struct Scheme_Stx_Offset {
  Scheme_Type type;
  long line, col, pos;
  Scheme_Object *src;
} Scheme_Stx_Offset;

Scheme_Object *scheme_make_stx(Scheme_Object *val, 
			       Scheme_Stx_Srcloc *srcloc,
			       Scheme_Object *props);
Scheme_Object *scheme_make_stx_w_offset(Scheme_Object *val, 
					long line, long col, long pos, long span,
					Scheme_Object *src,
					Scheme_Object *props);
Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx,
				     long line, long col, long pos);

Scheme_Object *scheme_new_stx_simplify_cache();
void scheme_simplify_stx(Scheme_Object *stx, Scheme_Object *simplify_cache);

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, Scheme_Object *stx_src, 
				      Scheme_Object *stx_wraps, 
				      int cangraph, int copyprops);
Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_marks,
				      Scheme_Hash_Table *renames);

int scheme_syntax_is_graph(Scheme_Object *stx);

Scheme_Object *scheme_stx_track(Scheme_Object *naya, 
				Scheme_Object *old, 
				Scheme_Object *origin);

Scheme_Object *scheme_new_mark();
Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m);

Scheme_Object *scheme_make_rename(Scheme_Object *newname, int c);
void scheme_set_rename(Scheme_Object *rnm, int pos, Scheme_Object *oldname);

Scheme_Object *scheme_add_rename(Scheme_Object *o, Scheme_Object *rename);
Scheme_Object *scheme_add_mark_barrier(Scheme_Object *o);

Scheme_Object *scheme_make_module_rename(long phase, int nonmodule);
void scheme_extend_module_rename(Scheme_Object *rn, Scheme_Object *modname, 
				 Scheme_Object *locname, Scheme_Object *exname, 
				 Scheme_Object *nominal_src, Scheme_Object *nominal_ex);
void scheme_extend_module_rename_with_kernel(Scheme_Object *rn, Scheme_Object *nominal_src);
void scheme_remove_module_rename(Scheme_Object *mrn,
				 Scheme_Object *localname);
void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest);
void scheme_list_module_rename(Scheme_Object *src, Scheme_Hash_Table *ht);

Scheme_Object *scheme_stx_content(Scheme_Object *o);
Scheme_Object *scheme_flatten_syntax_list(Scheme_Object *lst, int *islist);

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b, long phase);
int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, long phase);
Scheme_Object *scheme_stx_module_name(Scheme_Object **name, long phase, 
				      Scheme_Object **nominal_modidx,
				      Scheme_Object **nominal_name);

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, long phase);
int scheme_stx_env_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *uid, long phase);

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve);

Scheme_Object *scheme_stx_property(Scheme_Object *_stx,
				   Scheme_Object *key,
				   Scheme_Object *val);

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, long shift,
				      Scheme_Object *old_midx, Scheme_Object *new_midx);

int scheme_stx_list_length(Scheme_Object *list);
int scheme_stx_proper_list_length(Scheme_Object *list);

Scheme_Object *scheme_resolve_placeholders(Scheme_Object *obj, int mkstx);
Scheme_Hash_Table *scheme_setup_datum_graph(Scheme_Object *o, int for_print);

#define SCHEME_STX_VAL(s) ((Scheme_Stx *)s)->val

#define SCHEME_STX_PAIRP(o) (SCHEME_PAIRP(o) || (SCHEME_STXP(o) && SCHEME_PAIRP(SCHEME_STX_VAL(o))))
#define SCHEME_STX_SYMBOLP(o) (SCHEME_SYMBOLP(o) || (SCHEME_STXP(o) && SCHEME_SYMBOLP(SCHEME_STX_VAL(o))))
#define SCHEME_STX_NULLP(o) (SCHEME_NULLP(o) || (SCHEME_STXP(o) && SCHEME_NULLP(SCHEME_STX_VAL(o))))

#define SCHEME_STX_CAR(o) (SCHEME_PAIRP(o) ? SCHEME_CAR(o) : SCHEME_CAR(scheme_stx_content(o)))
#define SCHEME_STX_CDR(o) (SCHEME_PAIRP(o) ? SCHEME_CDR(o) : SCHEME_CDR(scheme_stx_content(o)))
#define SCHEME_STX_SYM(o) (SCHEME_STXP(o) ? SCHEME_STX_VAL(o) : o)

Scheme_Object *scheme_source_to_name(Scheme_Object *code);

#define STX_SRCTAG scheme_false

/*========================================================================*/
/*                   syntax run-time structures                           */
/*========================================================================*/

typedef struct {
  Scheme_Type type;
  short num_args;
  Scheme_Object *args[1];
  /* After array of f & args, array of chars for eval type */
} Scheme_App_Rec;

typedef struct {
  Scheme_Type type;
  Scheme_Object *test;
  Scheme_Object *tbranch;
  Scheme_Object *fbranch;
} Scheme_Branch_Rec;

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short max_let_depth;
  Scheme_Object *code;
  struct Resolve_Prefix *prefix;
} Scheme_Compilation_Top;

typedef struct Scheme_Compiled_Let_Value {
  Scheme_Type type;
  short count;
  short position;
  int *flags;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Compiled_Let_Value;

typedef struct Scheme_Let_Header {
  Scheme_Type type;
  short count;
  short recursive;
  short num_clauses;
  Scheme_Object *body;
} Scheme_Let_Header;

typedef struct {
  Scheme_Type type;
  Scheme_Object *key;
  Scheme_Object *val;
  Scheme_Object *body;
} Scheme_With_Continuation_Mark;

typedef struct Scheme_Local {
  Scheme_Type type;
  short position;
#ifdef MZ_PRECISE_GC
  /* Everything has to be at least 2 words in size. */
  int x;
#endif
} Scheme_Local;

#define SCHEME_LOCAL_POS(obj)    (((Scheme_Local *)(obj))->position)

typedef struct Scheme_Toplevel {
  Scheme_Type type;
  short depth;
  int position;
} Scheme_Toplevel;

#define SCHEME_TOPLEVEL_DEPTH(obj)    (((Scheme_Toplevel *)(obj))->depth)
#define SCHEME_TOPLEVEL_POS(obj)    (((Scheme_Toplevel *)(obj))->position)

typedef struct Scheme_Let_Value {
  Scheme_Type type;
  short count;
  short position;
  short autobox;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Let_Value;

typedef struct Scheme_Let_One {
  Scheme_Type type;
  short eval_type;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Let_One;

typedef struct Scheme_Let_Void {
  Scheme_Type type;
  short count;
  short autobox;
  Scheme_Object *body;
} Scheme_Let_Void;

typedef struct Scheme_Letrec {
  Scheme_Type type;
  short count;
  Scheme_Object **procs;
  Scheme_Object *body;
} Scheme_Letrec;

typedef struct {
  Scheme_Type type;
  short num_bindings;
  Scheme_Object *body;
} Scheme_Let_Frame_Data;

typedef struct {
  Scheme_Type type;
  short count;
  Scheme_Object *array[1];
} Scheme_Sequence;

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short count;
  Scheme_Object *name; /* see note below */
  Scheme_Object *array[1];
} Scheme_Case_Lambda;
/* If count is not 0, then check array[0] for CLOS_IS_METHOD.
   Otherwise, name is a boxed symbol (or #f) to indicate a method. */

Scheme_Object *
scheme_make_prim_w_everything(Scheme_Prim *fun, int eternal,
			      const char *name,
			      short mina, short maxa,
			      short folding,
			      short minr, short maxr);
Scheme_Object *
scheme_make_closed_prim_w_everything(Scheme_Closed_Prim *fun, 
				     void *data,
				     const char *name, 
				     short mina, short maxa,
				     short folding,
				     short minr, short maxr);

#define scheme_make_prim_w_arity2(f, n, mina, maxa, minr, maxr) \
  scheme_make_prim_w_everything(f, 0, n, mina, maxa, 0, minr, maxr)

/*========================================================================*/
/*                              control flow                              */
/*========================================================================*/

Scheme_Object *scheme_handle_stack_overflow(Scheme_Object *(*k)(void));

void scheme_ensure_stack_start(Scheme_Thread *p, void *d);
void scheme_jmpup_free(Scheme_Jumpup_Buf *);
void *scheme_enlarge_runstack(long size, void *(*k)());
int scheme_check_runstack(long size);
void scheme_init_setjumpup(void);

void *scheme_top_level_do(void *(*k)(void), int eb);
#define scheme_top_level_do_w_thread(k, eb, p) scheme_top_level_do(k, eb)

void scheme_on_next_top(struct Scheme_Comp_Env *env, Scheme_Object *mark, Scheme_Object *name);

Scheme_Object *scheme_call_ec(int argc, Scheme_Object *argv[]);

unsigned long scheme_get_deeper_address(void);

#ifdef DO_STACK_CHECK
void scheme_init_stack_limit (void);
#endif


typedef struct Scheme_Saved_Stack {
  MZTAG_IF_REQUIRED
  Scheme_Object **runstack_start;
  Scheme_Object **runstack;
  long runstack_size;
  struct Scheme_Saved_Stack *prev;
} Scheme_Saved_Stack;

typedef struct Scheme_Cont_Mark {
  /* Precise GC: We leave out the tag and make sure everything
     is a pointer, then allocate with GC_malloc_allow_interior */
  Scheme_Object *key;
  Scheme_Object *val;
  struct Scheme_Cont_Mark_Chain *cached_chain;
  MZ_MARK_POS_TYPE pos; /* Odd numbers - so they look like non-pointers */
} Scheme_Cont_Mark;

typedef struct Scheme_Cont_Mark_Chain {
  MZTAG_IF_REQUIRED
  Scheme_Object *key;
  Scheme_Object *val;
  MZ_MARK_POS_TYPE pos;
  struct Scheme_Cont_Mark_Chain *next;
} Scheme_Cont_Mark_Chain;

typedef struct Scheme_Cont_Mark_Set {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  struct Scheme_Cont_Mark_Chain *chain;
  long cmpos;
} Scheme_Cont_Mark_Set;

#define SCHEME_LOG_MARK_SEGMENT_SIZE 8
#define SCHEME_MARK_SEGMENT_SIZE (1 << SCHEME_LOG_MARK_SEGMENT_SIZE)
#define SCHEME_MARK_SEGMENT_MASK (SCHEME_MARK_SEGMENT_SIZE - 1)

typedef struct Scheme_Stack_State {
  Scheme_Object **runstack;
  Scheme_Object **runstack_start;
  long runstack_size;
  Scheme_Saved_Stack *runstack_saved;
  MZ_MARK_POS_TYPE cont_mark_pos;
  MZ_MARK_STACK_TYPE cont_mark_stack;
} Scheme_Stack_State;

typedef struct Scheme_Dynamic_Wind {
  MZTAG_IF_REQUIRED
  void *data;
  void (*pre)(void *);
  void (*post)(void *);
  mz_jmp_buf saveerr;
  struct Scheme_Comp_Env *current_local_env;
  struct Scheme_Stack_State envss;
  struct Scheme_Cont *cont;
  struct Scheme_Dynamic_Wind *prev;
} Scheme_Dynamic_Wind;

typedef struct Scheme_Cont {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *value;
  Scheme_Jumpup_Buf buf;
  long *ok;
  Scheme_Dynamic_Wind *dw, *common;
  Scheme_Thread *home;
  Scheme_Continuation_Jump_State cjs;
  mz_jmp_buf save_overflow_buf;
  int suspend_break;
  Scheme_Stack_State ss;
  Scheme_Saved_Stack *runstack_copied;
  Scheme_Cont_Mark *cont_mark_stack_copied;
  struct Scheme_Overflow *save_overflow;
  struct Scheme_Comp_Env *current_local_env;
  mz_jmp_buf savebuf; /* save old error buffer here */
} Scheme_Cont;

typedef struct Scheme_Escaping_Cont {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Continuation_Jump_State cjs;
  Scheme_Thread *home;
  long *ok;  
  Scheme_Object *f;
  int suspend_break;
  MZ_MARK_STACK_TYPE cont_mark_stack; /* for `continuation-marks' */
  MZ_MARK_POS_TYPE cont_mark_pos; /* for `continuation-marks' */
} Scheme_Escaping_Cont;

#define SCHEME_CONT_HOME(obj)  (((Scheme_Escaping_Cont *)(obj))->home)
#define SCHEME_CONT_OK(obj)  (((Scheme_Escaping_Cont *)(obj))->ok)
#define SCHEME_CONT_F(obj) (((Scheme_Escaping_Cont *)(obj))->f)

#define scheme_save_env_stack_w_thread(ss, p) \
    (ss.runstack = MZ_RUNSTACK, ss.runstack_start = MZ_RUNSTACK_START, \
     ss.cont_mark_stack = MZ_CONT_MARK_STACK, ss.cont_mark_pos = MZ_CONT_MARK_POS, \
     ss.runstack_size = p->runstack_size, ss.runstack_saved = p->runstack_saved)
#define scheme_restore_env_stack_w_thread(ss, p) \
    (MZ_RUNSTACK = ss.runstack, MZ_RUNSTACK_START = ss.runstack_start, \
     MZ_CONT_MARK_STACK = ss.cont_mark_stack, MZ_CONT_MARK_POS = ss.cont_mark_pos, \
     p->runstack_size = ss.runstack_size, p->runstack_saved = ss.runstack_saved)
#define scheme_save_env_stack(ss) \
    scheme_save_env_stack_w_thread(ss, scheme_current_thread)
#define scheme_restore_env_stack(ss) \
    scheme_restore_env_stack_w_thread(ss, scheme_current_thread)

typedef struct Scheme_Overflow {
  MZTAG_IF_REQUIRED
  Scheme_Jumpup_Buf cont; /* continuation after value obtained in overflowed */
  struct Scheme_Overflow *prev; /* old overflow info */
  mz_jmp_buf savebuf; /* save old error buffer here */
  int captured; /* set to 1 if possibly captured in a continuation */
} Scheme_Overflow;

typedef void (*Scheme_Kill_Action_Func)(void *);
void scheme_push_kill_action(Scheme_Kill_Action_Func f, void *d);
void scheme_pop_kill_action();

# define BEGIN_ESCAPEABLE(func, data) \
    { mz_jmp_buf savebuf; \
      scheme_push_kill_action((Scheme_Kill_Action_Func)func, (void *)data); \
      memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf)); \
      if (scheme_setjmp(scheme_error_buf)) { \
        func(data); \
        scheme_longjmp(savebuf, 1); \
      } else {
# define END_ESCAPEABLE() \
      scheme_pop_kill_action(); \
      memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf)); } }

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) \
    || defined(MACOS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE) \
    || defined(BEOS_FIND_STACK_BOUNDS) || defined(OSKIT_FIXED_STACK_BOUNDS) \
    || defined(PALM_FIND_STACK_BOUNDS)
extern unsigned long scheme_stack_boundary;
#endif

/* Compiler helper: */
#define ESCAPED_BEFORE_HERE  return NULL

/*========================================================================*/
/*                         semaphores and locks                           */
/*========================================================================*/

#define SEMAPHORE_WAITING_IS_COLLECTABLE 1

#if SEMAPHORE_WAITING_IS_COLLECTABLE
typedef struct Scheme_Sema_Waiter {
  MZTAG_IF_REQUIRED
  Scheme_Thread *p;
  int in_line;
  struct Scheme_Sema_Waiter *prev, *next;
} Scheme_Sema_Waiter;
#endif

typedef struct Scheme_Sema {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  long value;  
#if SEMAPHORE_WAITING_IS_COLLECTABLE
  Scheme_Sema_Waiter *first, *last;
#endif
} Scheme_Sema;

#define NOT_BLOCKED 0
#define PIPE_BLOCKED 1
#define PORT_BLOCKED 2
#define SEMA_BLOCKED 3
#define EVENTLOOP_BLOCKED 4
#define SLEEP_BLOCKED 10

/*========================================================================*/
/*                                 numbers                                */
/*========================================================================*/

#ifdef MPW_C
/* Optimizer bug! */
# define scheme_exact_zero ((Scheme_Object *)0x1)
# define scheme_exact_one ((Scheme_Object *)0x3)
#else
# define scheme_exact_zero scheme_make_integer(0)
# define scheme_exact_one scheme_make_integer(1)
#endif

/****** Bignums *******/

#ifdef USE_LONG_LONG_FOR_BIGDIG
typedef unsigned long long bigdig;
#else
typedef unsigned long bigdig;
#endif

typedef struct {
  Scheme_Type type;
#if MZ_PRECISE_GC
  char pos;
  char allocated_inline;
#else
  short pos;
#endif
  int len;
  bigdig *digits;
} Scheme_Bignum;

#define SCHEME_BIGPOS(b) (((Scheme_Bignum *)b)->pos)
#define SCHEME_BIGLEN(b) (((Scheme_Bignum *)b)->len)
#define SCHEME_BIGDIG(b) (((Scheme_Bignum *)b)->digits)

typedef struct {
  Scheme_Bignum o;
  bigdig v[1];
} Small_Bignum;
  
Scheme_Object *scheme_make_small_bignum(long v, Small_Bignum *s);
char *scheme_number_to_string(int radix, Scheme_Object *obj);

int scheme_bignum_get_int_val(const Scheme_Object *o, long *v);
int scheme_bignum_get_unsigned_int_val(const Scheme_Object *o, unsigned long *v);

int scheme_bignum_eq(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_lt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_gt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_le(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_ge(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_negate(const Scheme_Object *n);
Scheme_Object *scheme_bignum_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_add1(const Scheme_Object *n);
Scheme_Object *scheme_bignum_sub1(const Scheme_Object *n);
Scheme_Object *scheme_bignum_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_max(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_min(const Scheme_Object *a, const Scheme_Object *b);
void scheme_bignum_divide(const Scheme_Object *n, const Scheme_Object *d,
			  Scheme_Object **qp, Scheme_Object **rp, int norm);
Scheme_Object *scheme_bignum_power(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_integer_sqrt(const Scheme_Object *n);
Scheme_Object *scheme_bignum_and(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_or(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_xor(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_not(const Scheme_Object *a);
Scheme_Object *scheme_bignum_shift(const Scheme_Object *a, long shift);

double scheme_bignum_to_double_inf_info(const Scheme_Object *n, int just_use, int *only_need);
#ifdef MZ_USE_SINGLE_FLOATS
float scheme_bignum_to_float_inf_info(const Scheme_Object *n, int just_use, int *only_need);
#else
# define scheme_bignum_to_float_inf_info scheme_bignum_to_double_inf_info
#endif

/****** Rational numbers *******/

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *num;
  Scheme_Object *denom;
} Scheme_Rational;

typedef Scheme_Rational Small_Rational;

Scheme_Object *scheme_make_small_rational(long n, Small_Rational *space);
Scheme_Object *scheme_integer_to_rational(const Scheme_Object *n);
Scheme_Object *scheme_make_fixnum_rational(long n, long d);
int scheme_rational_eq(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_lt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_gt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_le(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_ge(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_negate(const Scheme_Object *n);
Scheme_Object *scheme_rational_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_add1(const Scheme_Object *n);
Scheme_Object *scheme_rational_sub1(const Scheme_Object *n);
Scheme_Object *scheme_rational_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_max(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_min(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_divide(const Scheme_Object *n, const Scheme_Object *d);
Scheme_Object *scheme_rational_power(const Scheme_Object *a, const Scheme_Object *b);
int scheme_is_rational_positive(const Scheme_Object *o);
Scheme_Object *scheme_rational_floor(const Scheme_Object *a);
Scheme_Object *scheme_rational_truncate(const Scheme_Object *a);
Scheme_Object *scheme_rational_ceiling(const Scheme_Object *a);
Scheme_Object *scheme_rational_round(const Scheme_Object *a);
Scheme_Object *scheme_rational_sqrt(const Scheme_Object *n);

/****** Complex numbers *******/

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *r;
  Scheme_Object *i;
} Scheme_Complex;

typedef Scheme_Complex Small_Complex;

#define _scheme_complex_real_part(n) (((Scheme_Complex *)(n))->r)
#define _scheme_complex_imaginary_part(n) (((Scheme_Complex *)(n))->i)

Scheme_Object *scheme_make_small_complex(const Scheme_Object *n, Small_Complex *space);
Scheme_Object *scheme_real_to_complex(const Scheme_Object *n);
int scheme_complex_eq(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_negate(const Scheme_Object *n);
Scheme_Object *scheme_complex_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_add1(const Scheme_Object *n);
Scheme_Object *scheme_complex_sub1(const Scheme_Object *n);
Scheme_Object *scheme_complex_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_divide(const Scheme_Object *n, const Scheme_Object *d);
Scheme_Object *scheme_complex_power(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_sqrt(const Scheme_Object *a);
int scheme_is_complex_exact(const Scheme_Object *o);

/****** Inexacts ******/

#define REAL_NUMBER_STR "real number"

int scheme_check_double(const char *where, double v, const char *dest);
#ifdef MZ_USE_SINGLE_FLOATS
int scheme_check_float(const char *where, float v, const char *dest);
#else
# define scheme_check_float scheme_check_double
#endif

double scheme_get_val_as_double(const Scheme_Object *n);
int scheme_minus_zero_p(double d);

#ifdef MZ_USE_SINGLE_FLOATS
float scheme_get_val_as_float(const Scheme_Object *n);
#endif

#if !defined(USE_IEEE_FP_PREDS) && !defined(USE_SCO_IEEE_PREDS) \
    && !defined(USE_OSF_FP_PREDS) && !defined(USE_PALM_INF_TESTS)
# define MZ_IS_POS_INFINITY(d) ((d) == scheme_infinity_val)
# define MZ_IS_NEG_INFINITY(d) ((d) == scheme_minus_infinity_val)
# ifdef NAN_EQUALS_ANYTHING
#  define MZ_IS_NAN(d) (((d) == 1.0) && ((d) == 2.0))
# else
#  ifdef DEFEAT_FP_COMP_OPTIMIZATION
extern int scheme_both_nan(double a, double b);
#   define MZ_IS_NAN(d) (scheme_both_nan(d, d))
#  else
#   define MZ_IS_NAN(d) (!((d) == (d)))
#  endif
# endif
#else
# ifdef USE_SCO_IEEE_PREDS
#  include <ieeefp.h>
#  define MZ_IS_POS_INFINITY(d) (fpclass(d) == FP_PINF)
#  define MZ_IS_NEG_INFINITY(d) (fpclass(d) == FP_NINF)
#  define MZ_IS_NAN(d) isnan(d)
# else
#  ifdef USE_PALM_INF_TESTS
#   define MZ_IS_POS_INFINITY(d) scheme_is_pos_inf(d)
#   define MZ_IS_NEG_INFINITY(d) scheme_is_neg_inf(d)
#   define MZ_IS_NAN(d) scheme_is_nan(d)
extern int scheme_is_pos_inf(double); 
extern int scheme_is_neg_inf(double); 
extern int scheme_is_nan(double); 
#  else
#   ifdef USE_OSF_FP_PREDS
#    include <math.h>
#    include <fp_class.h>
#    define MZ_IS_POS_INFINITY(d) (fp_class(d) == FP_POS_INF)
#    define MZ_IS_NEG_INFINITY(d) (fp_class(d) == FP_NEG_INF)
#    define MZ_IS_NAN(d) isnan(d)
#   else
     /* USE_IEEE_FP_PREDS */
#    define MZ_IS_POS_INFINITY(d) (isinf(d) && (d > 0))
#    define MZ_IS_NEG_INFINITY(d) (isinf(d) && (d < 0))
#    define MZ_IS_NAN(d) isnan(d)
#   endif
#  endif
# endif
#endif

#define IZI_REAL_PART(n) (((Scheme_Complex *)(n))->r)

extern double scheme_infinity_val, scheme_minus_infinity_val;
extern double scheme_floating_point_zero;
extern double scheme_floating_point_nzero;
extern Scheme_Object *scheme_zerod, *scheme_nzerod, *scheme_pi, *scheme_half_pi, *scheme_plus_i, *scheme_minus_i;
extern Scheme_Object *scheme_inf_object, *scheme_minus_inf_object, *scheme_nan_object;
#ifdef MZ_USE_SINGLE_FLOATS
extern Scheme_Object *scheme_zerof, *scheme_nzerof, *scheme_single_scheme_pi;
Scheme_Object *scheme_single_inf_object, *scheme_single_minus_inf_object, *scheme_single_nan_object;
#endif

/****** General numeric ******/

Scheme_Object *scheme_read_number(const char *str, long len,
				  int is_float, 
				  int is_not_float,
				  int decimal_means_float,
				  int radix, int radix_set, 
				  Scheme_Object *port,
				  int *div_by_zero,
				  int test_only,
				  Scheme_Object *stxsrc, long line, long col, long pos, long span);

Scheme_Object *scheme_bin_gcd(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_quotient(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_mult(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_div(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_plus(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_minus(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_eq(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_lt(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_gt(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_gt_eq(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_lt_eq(const Scheme_Object *n1, const Scheme_Object *n2);

Scheme_Object *scheme_sub1(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_add1(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_odd_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_expt(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_modulo(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_sqrt(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_abs(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_inexact_to_exact(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_exact_to_inexact(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_inexact_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_TO_DOUBLE(const Scheme_Object *n);
Scheme_Object *scheme_to_bignum(const Scheme_Object *o);
int scheme_is_integer(const Scheme_Object *o);
Scheme_Object *scheme_zero_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_negative_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_positive_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_make_polar(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_bitwise_shift(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_bitwise_and(int argc, Scheme_Object *argv[]);

int scheme_nonneg_exact_p(Scheme_Object *n);

Scheme_Object *scheme_generic_integer_power(const Scheme_Object *o, const Scheme_Object *p);

#ifdef TIME_TYPE_IS_UNSIGNED
#define scheme_make_integer_value_from_time(t) scheme_make_integer_value_from_unsigned((unsigned long)t)
#else
#define scheme_make_integer_value_from_time(t) scheme_make_integer_value((long)t)
#endif

/***** Random number generator *****/

#define	MZ_RANDOM_STATE_DEG 31
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short fpos, rpos;
  long state[MZ_RANDOM_STATE_DEG];
} Scheme_Random_State;

Scheme_Object *scheme_make_random_state(long seed);

/*========================================================================*/
/*                     read, eval, print                                  */
/*========================================================================*/

#define _scheme_do_eval(obj, env, v) \
  ((SCHEME_INTP(obj) || !SCHEME_STRTAG_VAL(_SCHEME_TYPE(obj))) \
   ? obj : scheme_do_eval(obj, -1, env, v))
#define q_scheme_eval_linked(obj) _scheme_do_eval(obj, 1)
#define q_scheme_tail_eval(obj) scheme_tail_eval(obj)

Scheme_Object *scheme_eval_linked_expr(Scheme_Object *expr);
Scheme_Object *scheme_eval_linked_expr_multi(Scheme_Object *expr);

Scheme_Object *_scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands);
Scheme_Object *_scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands);

Scheme_Object *scheme_internal_read(Scheme_Object *port, Scheme_Object *stxsrc, int crc, int cantfail);
void scheme_internal_display(Scheme_Object *obj, Scheme_Object *port, Scheme_Config *);
void scheme_internal_write(Scheme_Object *obj, Scheme_Object *port, Scheme_Config *);

#define _scheme_eval_linked_expr(obj) scheme_do_eval(obj,-1,NULL,1)
#define _scheme_eval_linked_expr_multi(obj) scheme_do_eval(obj,-1,NULL,-1)
#define _scheme_eval_linked_expr_wp(obj, p) scheme_do_eval_w_thread(obj,-1,NULL,1,p)
#define _scheme_eval_linked_expr_multi_wp(obj, p) scheme_do_eval_w_thread(obj,-1,NULL,-1,p)

Scheme_Object *scheme_named_map_1(char *, 
				  Scheme_Object *(*fun)(Scheme_Object*, Scheme_Object *form), 
				  Scheme_Object *lst, Scheme_Object *form);

int scheme_strncmp(const char *a, const char *b, int len);

#define _scheme_make_char(ch) (scheme_char_constants[(unsigned char)(ch)])

Scheme_Object *scheme_default_eval_handler(int, Scheme_Object *[]);
Scheme_Object *scheme_default_print_handler(int, Scheme_Object *[]);
Scheme_Object *scheme_default_prompt_read_handler(int, Scheme_Object *[]);

extern Scheme_Object *scheme_default_global_print_handler;

/* Type readers & writers for compiled code data */
void scheme_install_type_reader(Scheme_Type type, Scheme_Type_Reader f);
void scheme_install_type_writer(Scheme_Type type, Scheme_Type_Writer f);

/*========================================================================*/
/*                          compile and link                              */
/*========================================================================*/

typedef struct Comp_Prefix
{
  MZTAG_IF_REQUIRED
  int num_toplevels, num_stxes;
  Scheme_Hash_Table *toplevels; /* buckets for toplevel/module variables */
  Scheme_Hash_Table *stxes;     /* syntax objects */
} Comp_Prefix;

typedef struct Scheme_Comp_Env
{
  MZTAG_IF_REQUIRED
  short num_bindings;   /* number of `values' slots */
  short flags;          /* used for expanding/compiling */
  Scheme_Env *genv;     /* top-level environment */
  Comp_Prefix *prefix;  /* stack base info: globals and stxes */

  struct Scheme_Object **values; /* names bound in this frame */

  Scheme_Object *uid;            /* renaming symbol for syntax, if all the same */
  struct Scheme_Object **uids;   /* renaming symbol for syntax when multiple are needed */

  struct Scheme_Object *renames; /* an stx lexical rename or an improper list of them */

  short rename_var_count;        /* number of non-NULL `values' when `renames' was computed */
  short rename_rstart;           /* leftover rstart from previous round; see env.c */
  Scheme_Hash_Table *dup_check;  /* table for finding colliding symbols in `values' */

  struct Scheme_Comp_Env *next;
} Scheme_Comp_Env;

#define CLOS_HAS_REST 1
#define CLOS_MUST_ALLOC 2
#define CLOS_ONLY_LOCALS 4
#define CLOS_FOLDABLE 8
#define CLOS_IS_METHOD 16

typedef struct Scheme_Compile_Info
{
  MZTAG_IF_REQUIRED
  int max_let_depth;
  char dont_mark_local_use;
  char resolve_module_ids;
  Scheme_Object *value_name;
} Scheme_Compile_Info;

typedef struct Resolve_Prefix
{
  Scheme_Type type;
  int num_toplevels, num_stxes;
  Scheme_Object **toplevels;
  Scheme_Object **stxes; /* simplified */
} Resolve_Prefix;

typedef struct Resolve_Info
{
  MZTAG_IF_REQUIRED
  int size, oldsize, count, pos;
  Resolve_Prefix *prefix;
  short toplevel_pos; /* -1 mean consult next */
  short *old_pos;
  short *new_pos;
  int stx_count;
  short *old_stx_pos; /* NULL => consult next; new pos is index in array */
  int *flags;
  struct Resolve_Info *next;
} Resolve_Info;

typedef struct Scheme_Object *
(Scheme_Syntax)(struct Scheme_Object *form, struct Scheme_Comp_Env *env,
		struct Scheme_Compile_Info *rec, int drec);

typedef struct Scheme_Object *
(Scheme_Syntax_Expander)(struct Scheme_Object *form, struct Scheme_Comp_Env *env,
			 int depth, Scheme_Object *boundname);

typedef struct Scheme_Object *(*Scheme_Syntax_Resolver)(Scheme_Object *data, Resolve_Info *info);

typedef struct CPort Mz_CPort;

typedef void (*Scheme_Syntax_Validater)(Scheme_Object *data, Mz_CPort *port, 
					char *stack, int depth, int delta, int num_toplevels);

typedef struct Scheme_Object *(*Scheme_Syntax_Executer)(struct Scheme_Object *data);

typedef struct Scheme_Closure_Compilation_Data
{
  Scheme_Type type;
  /* Scheme_Object *src; */
  short flags;
  short num_params; /* includes collecting arg if has_rest */
  short max_let_depth;
  short closure_size;
  short *closure_map; /* Actually a Closure_Info* until resolved! */
  Scheme_Object *code;
  Scheme_Object *name;
} Scheme_Closure_Compilation_Data;

int scheme_has_method_property(Scheme_Object *code);

typedef struct {
  Scheme_Type type;
#ifdef MZ_PRECISE_GC
  MZ_HASH_KEY_EX
  int closure_size;
#else
  short zero_sized;
#endif
  Scheme_Object *code;
  Scheme_Object *vals[1];
} Scheme_Closed_Compiled_Procedure;

#define SCHEME_COMPILED_CLOS_CODE(c) ((Scheme_Closed_Compiled_Procedure *)c)->code
#define SCHEME_COMPILED_CLOS_ENV(c) ((Scheme_Closed_Compiled_Procedure *)c)->vals

#define MAX_CONST_LOCAL_POS 20
extern Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

#define scheme_new_frame(n) scheme_new_special_frame(n, 0)
#define scheme_extend_env(f, e) (f->basic.next = e, f)
#define scheme_next_frame(e) ((e)->basic.next)
#define scheme_settable_frame(f, s) ((f)->basic.has_set_bang = (s))
#define scheme_get_frame_settable(f) ((f)->basic.has_set_bang)
#define scheme_get_binding(f, n) ((f)->values[n])

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, int flags);
Scheme_Comp_Env *scheme_new_expand_env(Scheme_Env *genv, int flags);

void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where,
			     Scheme_Comp_Env *env,
			     Scheme_Object *form);

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first, 
					    Scheme_Comp_Env *env, 
					    Scheme_Compile_Info *rec,
					    int drec,
					    int depth, Scheme_Object *boundname,
					    int int_def_pos,
					    Scheme_Object **current_val);

Scheme_Object *scheme_apply_macro(Scheme_Object *name,
				  Scheme_Object *f, Scheme_Object *code,
				  Scheme_Comp_Env *env, Scheme_Object *boundname);

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags, Scheme_Comp_Env *env);
void scheme_add_compilation_binding(int index, Scheme_Object *val, 
				    Scheme_Comp_Env *frame);
Scheme_Comp_Env *scheme_add_compilation_frame(Scheme_Object *vals, 
					 Scheme_Comp_Env *env, int flags);
Scheme_Comp_Env *scheme_require_renames(Scheme_Comp_Env *env);

Scheme_Object *scheme_lookup_binding(Scheme_Object *symbol, Scheme_Comp_Env *env,
				     int flags);

Scheme_Object *scheme_add_env_renames(Scheme_Object *stx, Scheme_Comp_Env *env, 
				      Scheme_Comp_Env *upto);

void scheme_add_local_syntax(int cnt, Scheme_Comp_Env *env);
void scheme_set_local_syntax(int pos, Scheme_Object *name, Scheme_Object *val,
			     Scheme_Comp_Env *env);

void scheme_env_make_closure_map(Scheme_Comp_Env *frame, short *size, short **map);
void scheme_env_make_stx_closure_map(Scheme_Comp_Env *frame, short *size, short **map);
int scheme_env_uses_toplevel(Scheme_Comp_Env *frame);

Scheme_Object *scheme_make_closure(Scheme_Thread *p, 
				   Scheme_Object *compiled_code,
				   int close);

#define scheme_add_good_binding(i,v,f) (f->values[i] = v)

Scheme_Object *scheme_compiled_void();

Scheme_Object *scheme_register_toplevel_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env, 
						  Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_register_stx_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env, 
					     Scheme_Compile_Info *rec, int drec);

/* Resolving & linking */
#define DEFINE_VALUES_EXPD 0
#define DEFINE_SYNTAX_EXPD 1
#define SET_EXPD           2
#define CASE_LAMBDA_EXPD   3
#define BEGIN0_EXPD        4
#define BOXENV_EXPD        5
#define BOXVAL_EXPD        6
#define MODULE_EXPD        7
#define REQUIRE_EXPD       8
#define _COUNT_EXPD_       9

#define scheme_register_syntax(i, fr, fv, fe, pa) \
     (scheme_syntax_resolvers[i] = fr, \
      scheme_syntax_executers[i] = fe, \
      scheme_syntax_validaters[i] = fv, \
      scheme_syntax_protect_afters[i] = pa)
extern Scheme_Syntax_Resolver scheme_syntax_resolvers[_COUNT_EXPD_];
extern Scheme_Syntax_Validater scheme_syntax_validaters[_COUNT_EXPD_];
extern Scheme_Syntax_Executer scheme_syntax_executers[_COUNT_EXPD_];
extern int scheme_syntax_protect_afters[_COUNT_EXPD_];

Scheme_Object *scheme_protect_quote(Scheme_Object *expr);

Scheme_Object *scheme_make_syntax_resolved(int idx, Scheme_Object *data);
Scheme_Object *scheme_make_syntax_compiled(int idx, Scheme_Object *data);

Scheme_Object *scheme_resolve_expr(Scheme_Object *, Resolve_Info *);
Scheme_Object *scheme_resolve_list(Scheme_Object *, Resolve_Info *);

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed);

Scheme_Object *scheme_resolve_lets(Scheme_Object *form, Resolve_Info *info);

Resolve_Prefix *scheme_resolve_prefix(int phase, Comp_Prefix *cp, int simplify);

Resolve_Info *scheme_resolve_info_create(Resolve_Prefix *rp);
Resolve_Info *scheme_resolve_info_extend(Resolve_Info *info, int size, int oldsize, int mapcount, int stxcount);
void scheme_resolve_info_add_mapping(Resolve_Info *info, int oldp, int newp, int flags);
int scheme_resolve_info_flags(Resolve_Info *info, int pos);
int scheme_resolve_info_lookup(Resolve_Info *resolve, int pos, int *flags);
void scheme_resolve_info_add_stx_mapping(Resolve_Info *info, int oldp, int newp);
void scheme_resolve_info_set_toplevel_pos(Resolve_Info *info, int pos);

int scheme_resolve_toplevel_pos(Resolve_Info *info);
Scheme_Object *scheme_resolve_toplevel(Resolve_Info *info, Scheme_Object *expr);
int scheme_resolve_quote_syntax(Resolve_Info *info, int oldpos);

Scheme_Object *scheme_make_compiled_syntax(Scheme_Syntax *syntax,
					   Scheme_Syntax_Expander *exp);

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_compile_sequence(Scheme_Object *forms, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_compile_block(Scheme_Object *forms, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec, int drec);

void scheme_default_compile_rec(Scheme_Compile_Info *src, int drec);
void scheme_compile_rec_done_local(Scheme_Compile_Info *src, int drec);
void scheme_init_compile_recs(Scheme_Compile_Info *src, int drec,
			      Scheme_Compile_Info *dest, int n);
void scheme_merge_compile_recs(Scheme_Compile_Info *src, int drec, 
			       Scheme_Compile_Info *dest, int n);
void scheme_init_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec);
void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec);


Scheme_Object *scheme_make_closure_compilation(Scheme_Comp_Env *env,
					       Scheme_Object *uncompiled_code,
					       Scheme_Compile_Info *rec, int drec);
Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *compiled_list, 
						int strip_values);


Scheme_Object *scheme_resolve_closure_compilation(Scheme_Object *_data, Resolve_Info *info);

Scheme_App_Rec *scheme_malloc_application(int n);
void scheme_finish_application(Scheme_App_Rec *app);

#define SCHEME_SYNTAX(obj)   ((obj)->u.two_ptr_val.ptr1)
#define SCHEME_SYNTAX_EXP(obj)   ((obj)->u.two_ptr_val.ptr2)

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count);

/* flags reported by scheme_env_get_fags */
#define SCHEME_WAS_USED 1
#define SCHEME_WAS_SET_BANGED 2

/* flags reported by scheme_resolve_info_flags */
#define SCHEME_INFO_BOXED 1

/* flags used with scheme_new_frame */
#define SCHEME_TOPLEVEL_FRAME 1
#define SCHEME_MODULE_FRAME 2
#define SCHEME_MODULE_BEGIN_FRAME 4
#define SCHEME_LAMBDA_FRAME 8
#define SCHEME_INTDEF_FRAME 16
#define SCHEME_NO_RENAME 32
#define SCHEME_CAPTURE_WITHOUT_RENAME 64
#define SCHEME_FOR_STOPS 128

/* Flags used with scheme_static_distance */
#define SCHEME_ELIM_CONST 1
#define SCHEME_APP_POS 2
#define SCHEME_SETTING 4
#define SCHEME_ENV_CONSTANTS_OK 8
#define SCHEME_GLOB_ALWAYS_REFERENCE 16
#define SCHEME_MUST_INDRECT 32
#define SCHEME_LINKING_REF 64
#define SCHEME_DONT_MARK_USE 128
#define SCHEME_OUT_OF_CONTEXT_OK 256
#define SCHEME_NULL_FOR_UNBOUND 512
#define SCHEME_RESOLVE_MODIDS 1024

Scheme_Hash_Table *scheme_map_constants_to_globals(void);

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env,
				  int depth, Scheme_Object *boundname);
Scheme_Object *scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env,
				  int depth, Scheme_Object *boundname);
Scheme_Object *scheme_expand_block(Scheme_Object *form, Scheme_Comp_Env *env,
				   int depth, Scheme_Object *boundname);

Scheme_Object *scheme_make_svector(short v, short *a);

#define SCHEME_SVEC_LEN(obj) ((obj)->u.svector_val.len)
#define SCHEME_SVEC_VEC(obj) ((obj)->u.svector_val.vec)

Scheme_Object *scheme_hash_percent_name(const char *name, int len);

Scheme_Object *scheme_make_branch(Scheme_Object *test,
				  Scheme_Object *tbranch,
				  Scheme_Object *fbranch);

int scheme_is_toplevel(Scheme_Comp_Env *env);
Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env);

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env);

Scheme_Env *scheme_make_empty_env(void);
void scheme_prepare_exp_env(Scheme_Env *env);

int scheme_used_app_only(Scheme_Comp_Env *env, int which);
int scheme_used_ever(Scheme_Comp_Env *env, int which);

int scheme_omittable_expr(Scheme_Object *o, int vals);

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which);

int scheme_get_eval_type(Scheme_Object *obj);

Scheme_Object *scheme_get_stop_expander(void);

void scheme_define_parse(Scheme_Object *form, 
			 Scheme_Object **vars, Scheme_Object **val,
			 int defmacro,
			 Scheme_Comp_Env *env);

void scheme_shadow(Scheme_Env *env, Scheme_Object *n, int stxtoo);

int scheme_prefix_depth(Resolve_Prefix *rp);
Scheme_Object **scheme_push_prefix(Scheme_Env *genv, Resolve_Prefix *rp,
				   Scheme_Object *src_modix, Scheme_Object *now_modix,
				   int src_phase, int now_phase);
void scheme_pop_prefix(Scheme_Object **rs);

Scheme_Object *scheme_make_environment_dummy(Scheme_Comp_Env *env);
Scheme_Env *scheme_environment_from_dummy(Scheme_Object *dummy);

void scheme_validate_code(Mz_CPort *port, Scheme_Object *code, int depth,
			  int num_toplevels, int num_stxes);
void scheme_validate_expr(Mz_CPort *port, Scheme_Object *expr, 
			  char *stack, int depth, int delta, int num_toplevels);
void scheme_validate_toplevel(Scheme_Object *expr, Mz_CPort *port,
			      char *stack, int depth, int delta, int num_toplevels);
void scheme_validate_boxenv(int pos, Mz_CPort *port,
			    char *stack, int depth, int delta);

#define TRACK_ILL_FORMED_CATCH_LINES 0
#if TRACK_ILL_FORMED_CATCH_LINES
void scheme_ill_formed(Mz_CPort *port, const char *file, int line);
# define scheme_ill_formed_code(port) scheme_ill_formed(port, __FILE__, __LINE__)
#else
void scheme_ill_formed(Mz_CPort *port);
# define scheme_ill_formed_code(port) scheme_ill_formed(port) 
#endif

/*========================================================================*/
/*                         namespaces and modules                         */
/*========================================================================*/

struct Scheme_Env {
  Scheme_Type type; /* scheme_namespace_type */
  MZ_HASH_KEY_EX

  struct Scheme_Module *module; /* NULL => top-level */

  Scheme_Hash_Table *module_registry; /* symbol -> module ; loaded modules, 
					 shared with moucles in same space */

  /* For compilation, per-declaration: */
  /* First two are passed from module to module-begin: */
  Scheme_Object *rename;    /* module rename record */
  Scheme_Object *et_rename; /* exp-time rename record */

  Scheme_Bucket_Table *syntax;
  struct Scheme_Env *exp_env;

  Scheme_Hash_Table *shadowed_syntax; /* top level only */

  /* Per-instance: */
  long phase;
  Scheme_Object *link_midx;
  Scheme_Object *require_names, *et_require_names; /* for namespace-attach */
  char running, et_running, lazy_syntax, attached;

  Scheme_Bucket_Table *toplevel;
  Scheme_Object *modchain; /* Vector of:
			       1. symbol -> env ; running modules, 
			           shared with instances in same phase
			       2. modchain for next phase (or #f)
                               3. modchain for previous phase (or #f) */

  Scheme_Hash_Table *modvars; /* for scheme_module_variable_type hashing */
};

/* A module access path (or "idx") is a pair: sexp * symbol-or-#f
   The symbol is the resolved module name, or #f if it's not
   yet resolved. */

typedef struct Scheme_Module
{
  Scheme_Type type; /* scheme_module_type */
  MZ_HASH_KEY_EX

  Scheme_Object *modname;

  Scheme_Object *et_requires; /* list of module access paths */
  Scheme_Object *requires;    /* list of module access paths */

  Scheme_Invoke_Proc prim_body;
  Scheme_Invoke_Proc prim_et_body;

  Scheme_Object *body;        /* or data, if prim_body */
  Scheme_Object *et_body;     /* list of (vector list-of-names expr depth-int resolve-prefix) */

  int functional;
  int et_functional;

  Scheme_Object **provides;          /* symbols (extenal names) */
  Scheme_Object **provide_srcs;      /* module access paths, #f for self */
  Scheme_Object **provide_src_names; /* symbols (original internal names) */
  int num_provides;
  int num_var_provides;              /* non-syntax listed first in provides */

  int reprovide_kernel;              /* if true, extend provides with kernel's */
  Scheme_Object *kernel_exclusion;  /* we allow one exn, but it must be shadowed */

  Scheme_Object **indirect_provides; /* symbols (internal names) */
  int num_indirect_provides;

  Scheme_Object *src_modidx;  /* the one used in marshalled syntax */
  Scheme_Object *self_modidx;

  Scheme_Hash_Table *accessible;

  Scheme_Object *hints; /* set by expansion; moved to properties */
  Comp_Prefix *comp_prefix; /* set by body compile, temporary */

  int max_let_depth;
  Resolve_Prefix *prefix;

  Scheme_Object *dummy; /* for accessing the environment */

  Scheme_Env *primitive;
} Scheme_Module;

typedef struct Scheme_Modidx {
  Scheme_Type type; /* scheme_module_index_type */
  MZ_HASH_KEY_EX

  Scheme_Object *path;
  Scheme_Object *base;
  Scheme_Object *resolved;
  Scheme_Object *shift_cache; /* vector */
  struct Scheme_Modidx *cache_next;
} Scheme_Modidx;

typedef struct Module_Variable {
  Scheme_Type type; /* scheme_module_variable_type */
  MZ_HASH_KEY_EX
  Scheme_Object *modidx;
  Scheme_Object *sym;
  int pos;
} Module_Variable;

void scheme_add_global_keyword(const char *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_keyword_symbol(Scheme_Object *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_constant(const char *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_constant_symbol(Scheme_Object *name, Scheme_Object *v, Scheme_Env *env);

Scheme_Object *scheme_sys_wraps(Scheme_Comp_Env *env);

Scheme_Env *scheme_new_module_env(Scheme_Env *env, Scheme_Module *m, int new_exp_module_tree);
int scheme_is_module_env(Scheme_Comp_Env *env);

Scheme_Object *scheme_module_resolve(Scheme_Object *modidx);
Scheme_Env *scheme_module_access(Scheme_Object *modname, Scheme_Env *env);
void scheme_module_force_lazy(Scheme_Env *env, int previous);

int scheme_module_export_position(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *varname);

Scheme_Object *scheme_check_accessible_in_module(Scheme_Env *env, Scheme_Object *symbol, Scheme_Object *stx, 
						 int position, int want_pos);
Scheme_Object *scheme_module_syntax(Scheme_Object *modname, Scheme_Env *env, Scheme_Object *name);

Scheme_Object *scheme_modidx_shift(Scheme_Object *modidx, 
				   Scheme_Object *shift_from_modidx,
				   Scheme_Object *shift_to_modidx);

Scheme_Object *scheme_hash_module_variable(Scheme_Env *env, Scheme_Object *modidx, Scheme_Object *stxsym, int pos);

extern Scheme_Env *scheme_initial_env;

void scheme_install_initial_module_set(Scheme_Env *env);
Scheme_Bucket_Table *scheme_clone_toplevel(Scheme_Bucket_Table *ht, Scheme_Env *home);

Scheme_Env *scheme_clone_module_env(Scheme_Env *menv, Scheme_Env *ns, Scheme_Object *modchain);

void scheme_clean_dead_env(Scheme_Env *env);

Scheme_Module *scheme_extract_compiled_module(Scheme_Object *o);

void scheme_clear_modidx_cache(void);

/*========================================================================*/
/*                         errors and exceptions                          */
/*========================================================================*/

void scheme_read_err(Scheme_Object *port, 
		     Scheme_Object *stxsrc,
		     long line, long column, long pos, long span,
		     int is_eof, const char *detail, ...);

void scheme_wrong_syntax(const char *where,
			 Scheme_Object *local_form, 
			 Scheme_Object *form, 
			 const char *detail, ...);
extern const char *scheme_compile_stx_string;
extern const char *scheme_expand_stx_string;
extern const char *scheme_application_stx_string;
extern const char *scheme_set_stx_string;
extern const char *scheme_begin_stx_string;

void scheme_wrong_rator(Scheme_Object *rator, int argc, Scheme_Object **argv);

void scheme_raise_out_of_memory(const char *where, const char *msg, ...);

extern long scheme_max_found_symbol_name;

char *scheme_make_arity_expect_string(Scheme_Object *proc,
				      int argc, Scheme_Object **argv,
				      long *len);

long scheme_extract_index(const char *name, int pos, int argc, 
			  Scheme_Object **argv, long top);

void scheme_get_substring_indices(const char *name, Scheme_Object *str, 
				  int argc, Scheme_Object **argv, 
				  int spos, int fpos, long *_start, long *_finish);

void scheme_out_of_string_range(const char *name, const char *which, 
				Scheme_Object *i, Scheme_Object *s, 
				long start, long len);

const char *scheme_number_suffix(int);

void scheme_reset_prepared_error_buffer(void);

char *scheme_make_args_string(char *s, int which, int argc, Scheme_Object **argv, long *olen);

#define IMPROPER_LIST_FORM "illegal use of `.'"

int scheme_string_has_null(Scheme_Object *o);
#define STRING_W_NO_NULLS "string (with no null characters)"

Scheme_Object *scheme_do_exit(int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_make_arity(short minc, short maxc);
Scheme_Object *scheme_arity(Scheme_Object *p);

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *syms[5];
  int count;
  long phase;
  Scheme_Hash_Table *ht;
} DupCheckRecord;

void scheme_begin_dup_symbol_check(DupCheckRecord *r, Scheme_Comp_Env *e);
void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form);

extern int scheme_exiting_result;

Scheme_Object *scheme_special_comment_width(Scheme_Object *o);

/*========================================================================*/
/*                         filesystem utilities                           */
/*========================================================================*/

int scheme_is_relative_path(const char *s, long len);
int scheme_is_complete_path(const char *s, long len);

Scheme_Object *scheme_get_file_directory(const char *filename);

char *scheme_normal_path_case(char *s, int *_len);

int scheme_is_regular_file(char *filename);

#ifdef MAC_FILE_SYSTEM
void scheme_file_create_hook(char *filename);
#endif

void scheme_do_format(const char *procname, Scheme_Object *port, 
		      const unsigned char *format, int flen, 
		      int fpos, int offset, int argc, Scheme_Object **argv);

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[], char *who, int handler_param);

#ifdef MAC_CLASSIC_PROCESS_CONTROL
int scheme_mac_start_app(char *name, int find_path, Scheme_Object *s);
#endif
#ifdef MACINTOSH_EVENTS
int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, Scheme_Object **result, int *err, char **stage);
#endif

Scheme_Object *scheme_default_load_extension(int argc, Scheme_Object **argv);

Scheme_Object *scheme_remove_current_directory_prefix(Scheme_Object *fn);

#ifdef DOS_FILE_SYSTEM
int scheme_is_special_filename(const char *_f, int not_nul);
# define NUM_SPECIAL_FILE_KINDS 30
#endif

char *scheme_get_exec_path(void);

/*========================================================================*/
/*                               ports                                    */
/*========================================================================*/

#ifdef NO_TCP_SUPPORT
# undef USE_UNIX_SOCKETS_TCP
# undef USE_WINSOCK_TCP
# undef USE_MAC_TCP
#endif
#if defined(USE_UNIX_SOCKETS_TCP) || defined(USE_WINSOCK_TCP) || defined(USE_MAC_TCP)
# define USE_TCP
#endif

#if defined(USE_UNIX_SOCKETS_TCP) || defined(USE_WINSOCK_TCP)
# define USE_SOCKETS_TCP
#endif

extern int scheme_active_but_sleeping;
extern int scheme_internal_checking_char;
extern int scheme_file_open_count;

typedef struct Scheme_Indexed_String {
  MZTAG_IF_REQUIRED
  char *string;
  int size;
  int index;
  union { 
    int hot; /* output port */
    int pos; /* input port */
  } u;
} Scheme_Indexed_String;

typedef struct Scheme_Pipe {
  MZTAG_IF_REQUIRED
  unsigned char *buf;
  long buflen, bufmax;
  long bufstart, bufend;
  int eof;
  Scheme_Object *wakeup_on_read;
  Scheme_Object *wakeup_on_write;
} Scheme_Pipe;

#ifdef USE_TCP
typedef struct Scheme_Tcp_Buf {
  MZTAG_IF_REQUIRED
  short refcount;
  char *buffer;
  short bufpos, bufmax;
  short hiteof;
} Scheme_Tcp_Buf;
#endif

extern Scheme_Object *scheme_string_input_port_type;
extern Scheme_Object *scheme_string_output_port_type;
extern Scheme_Object *scheme_user_input_port_type;
extern Scheme_Object *scheme_user_output_port_type;
extern Scheme_Object *scheme_pipe_read_port_type;
extern Scheme_Object *scheme_pipe_write_port_type;
#ifdef USE_TCP
extern Scheme_Object *scheme_tcp_input_port_type;
extern Scheme_Object *scheme_tcp_output_port_type;
#endif

extern int scheme_force_port_closed;

void scheme_flush_orig_outputs(void);
Scheme_Object *scheme_file_stream_port_p(int, Scheme_Object *[]);
Scheme_Object *scheme_do_open_input_file(char *name, int offset, int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_do_open_output_file(char *name, int offset, int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_file_position(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_file_buffer(int argc, Scheme_Object *argv[]);

#ifdef USE_TCP
int scheme_tcp_write_nb_string(char *s, long len, long offset, int rarely_block, Scheme_Output_Port *port);
#endif

Scheme_Object *scheme_call_enable_break(Scheme_Prim *prim, int argc, Scheme_Object *argv[]);

Scheme_Object *scheme_get_special(Scheme_Object *inport, Scheme_Object *stxsrc, long line, long col, long pos, 
				  Scheme_Object **exn);
void scheme_bad_time_for_special(const char *name, Scheme_Object *port);
extern int scheme_special_ok;

Scheme_Input_Port *_scheme_make_input_port(Scheme_Object *subtype,
					   void *data,
					   Scheme_Get_String_Fun get_string_fun,
					   Scheme_Peek_String_Fun peek_string_fun,
					   Scheme_In_Ready_Fun char_ready_fun,
					   Scheme_Close_Input_Fun close_fun,
					   Scheme_Need_Wakeup_Input_Fun need_wakeup_fun,
					   int must_close);

#define CURRENT_INPUT_PORT(config) scheme_get_param(config, MZCONFIG_INPUT_PORT)
#define CURRENT_OUTPUT_PORT(config) scheme_get_param(config, MZCONFIG_OUTPUT_PORT)
#define CHECK_PORT_CLOSED(who, kind, port, closed) if (closed) scheme_raise_exn(MZEXN_I_O_PORT_CLOSED, port, "%s: " kind " port is closed", who);

#ifdef USE_FCNTL_O_NONBLOCK
# define MZ_NONBLOCKING O_NONBLOCK
#else
# define MZ_NONBLOCKING FNDELAY
#endif

/*========================================================================*/
/*                         memory debugging                               */
/*========================================================================*/

#ifdef MEMORY_COUNTING_ON
extern Scheme_Bucket_Table *scheme_symbol_table;
extern long scheme_type_table_count;
extern long scheme_misc_count;

Scheme_Object *scheme_dump_memory_count(int c, Scheme_Object *a[]);

long scheme_count_closure(Scheme_Object **o, short len, Scheme_Hash_Table *ht);

long scheme_count_envbox(Scheme_Object *root, Scheme_Hash_Table *ht);
long scheme_count_memory(Scheme_Object *root, Scheme_Hash_Table *ht);
void scheme_count_input_port(Scheme_Object *port, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_output_port(Scheme_Object *port, long *s, long *e, Scheme_Hash_Table *ht);

void scheme_count_struct_info(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);

#ifndef NO_OBJECT_SYSTEM
void scheme_count_object(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_class(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_class_data(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_generic(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
#endif
#endif

/*========================================================================*/
/*                           miscellaneous                                */
/*========================================================================*/

Scheme_Object *scheme_symbol_append(Scheme_Object *s1, Scheme_Object *s2);
Scheme_Object *scheme_copy_list(Scheme_Object *l);

Scheme_Object *scheme_regexp_source(Scheme_Object *re);

extern int scheme_locale_on;
void scheme_reset_locale(void);
/* "nonlocale" folding used to be folding, until we started using locales. */
#define scheme_make_nonlocale_folding_prim(prim, name, mina, maxa, functional) \
  scheme_make_folding_prim(prim, name, mina, maxa, 0)

#define SCHEME_SYM_UNINTERNED(o) (((Scheme_Symbol *)o)->keyex & 0x1)

extern unsigned char scheme_portable_upcase[256];
extern unsigned char scheme_portable_downcase[256];
#define mz_portable_toupper(c) scheme_portable_upcase[c]
#define mz_portable_tolower(c) scheme_portable_downcase[c]

#endif /* __mzscheme_private__ */
