/*
  MzScheme
  Copyright (c) 1995-2001 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  Originally based on:
  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#ifndef SCHEME_H
#define SCHEME_H

/* The next line is used and set during installation: */
#define INCLUDE_WITHOUT_PATHS

/*========================================================================*/
/*                           configuration                                */
/*========================================================================*/

/* The configuration is not intended to be adjusted here. Instead,
   modify sconfig.h. The code below simply draws a few more
   configuration conclusions and a few extra macros based on those
   settings. */

#ifdef INCLUDE_WITHOUT_PATHS
# include "sconfig.h"
#else
# include "../sconfig.h"
#endif

#if defined(__MWERKS__)
# ifdef MZSCHEME_USES_NEAR_GLOBALS
#  pragma far_data off
# endif
#endif

#if SGC_STD_DEBUGGING
# ifndef USE_SENORA_GC
#  define USE_SENORA_GC
# endif
# define USE_MEMORY_TRACING 
#endif

#ifdef MZ_PRECISE_GC
# define MUST_REGISTER_GLOBALS
# define MZTAG_REQUIRED
# undef UNIX_IMAGE_DUMPS
#endif

#ifdef USE_SENORA_GC
# define MUST_REGISTER_GLOBALS
# undef UNIX_IMAGE_DUMPS
#endif

#ifdef USE_SINGLE_FLOATS
# define MZ_USE_SINGLE_FLOATS
#endif

#ifdef DONT_ITIMER
# undef USE_ITIMER
#endif

#ifdef MZ_REAL_THREADS
# undef USE_ITIMER
# undef USE_WIN32_THREAD_TIMER
#endif

#if defined(USE_ITIMER) || defined(USE_WIN32_THREAD_TIMER)
# define FUEL_AUTODECEREMENTS
#endif

#ifdef MZ_PRECISE_GC
# define MZ_HASH_KEY_EX  short keyex;
#else
# define MZ_HASH_KEY_EX /**/
#endif

#ifdef PALMOS_STUFF
# include <PalmOS.h>
typedef long FILE;
# define _LINUX_TYPES_H  /* Blocks types.h */
#endif

#ifndef SCHEME_DIRECT_EMBEDDED
# define SCHEME_DIRECT_EMBEDDED 1
#endif

#ifndef MSC_IZE
# define MSC_IZE(x) x
#endif

#ifdef SIGSET_IS_SIGNAL
# define MZ_SIGSET(s, f) signal(s, f)
#else
# define MZ_SIGSET(s, f) sigset(s, f)
#endif

#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#ifdef PALMOS_STUFF
typedef jmpbuf jmp_buf[1];
#endif

#define GC_MIGHT_USE_REGISTERED_STATICS

#ifdef __cplusplus
extern "C" 
{
#endif

/*========================================================================*/
/*                        basic Scheme values                             */
/*========================================================================*/

typedef short Scheme_Type;

/* MzScheme values have the type `Scheme_Object *'.
   The actual Scheme_Object structure only defines a few variants.
   The important thing is that all `Scheme_Object *'s start with
   a Scheme_Type field.

   The structures are defined here, instead of in a private header, so
   that macros can provide quick access. Of course, don't access the
   fields of these structures directly; use the macros instead. */

typedef struct Scheme_Object
{
  Scheme_Type type; /* Anything that starts with a type field
		       can be a Scheme_Object */

  /* For precise GC, the keyex field is used for all object types to
     store a hash key extension. The low bit is not used for this
     purpose, though. For string and pair values in all variants of
     MzScheme, the low bit is set to 1 to indicate that the string is
     immutable. */
  short keyex;

  union
    {
      struct { char *string_val; int tag_val; } str_val;
      struct { void *ptr1, *ptr2; } two_ptr_val;
      struct { int int1; int int2; } two_int_val;
      struct { void *ptr; int pint; } ptr_int_val;
      struct { void *ptr; long pint; } ptr_long_val;
      struct { struct Scheme_Object *car, *cdr; } pair_val;
      struct { struct Scheme_Env *env; struct Scheme_Object *code; } closure_val;
      struct { short len; short *vec; } svector_val;
    } u;
} Scheme_Object;

/* Scheme_Small_Object is used for several types of MzScheme values: */
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  union {
    char char_val;
    Scheme_Object *ptr_value;
    long int_val;
    Scheme_Object *ptr_val;
  } u;
} Scheme_Small_Object;  

/* A floating-point number: */
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  double double_val;
} Scheme_Double;

#ifdef MZ_USE_SINGLE_FLOATS
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  float float_val;
} Scheme_Float;
#endif

typedef struct Scheme_Symbol {
  Scheme_Type type;
  short keyex; /* See `keyex' in Scheme_Object; flag here is for non-hygenic ids */
  int len;
  char s[4]; /* Really, a number of chars to match `len' */
} Scheme_Symbol;

typedef struct Scheme_Vector {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int size;
  Scheme_Object *els[1];
} Scheme_Vector;


/* This file defines all the built-in types */
#ifdef INCLUDE_WITHOUT_PATHS
# include "stypes.h"
#else
# include "../src/stypes.h"
#endif


#define SAME_PTR(a, b) ((a) == (b))
#define NOT_SAME_PTR(a, b) ((a) != (b))

#define SAME_OBJ(a, b) SAME_PTR(a, b)
#define NOT_SAME_OBJ(a, b) NOT_SAME_PTR(a, b)

#define SAME_TYPE(a, b) ((Scheme_Type)(a) == (Scheme_Type)(b))
#define NOT_SAME_TYPE(a, b) ((Scheme_Type)(a) != (Scheme_Type)(b))

# define SCHEME_TYPE(obj)     (SCHEME_INTP(obj)?(Scheme_Type)scheme_integer_type:(obj)->type)
# define _SCHEME_TYPE(obj) ((obj)->type) /* unsafe version */

/*========================================================================*/
/*                        basic Scheme predicates                         */
/*========================================================================*/

#define SCHEME_CHARP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_char_type)

#define SCHEME_INTP(obj)     (((long)obj) & 0x1)
#define SCHEME_DBLP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_double_type)
#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLTP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_float_type)
# define SCHEME_FLOATP(obj)     (SCHEME_FLTP(obj) || SCHEME_DBLP(obj))
#else
# define SCHEME_FLTP SCHEME_DBLP
# define SCHEME_FLOATP SCHEME_DBLP
#endif
#define SCHEME_BIGNUMP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_bignum_type)
#define SCHEME_RATIONALP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_rational_type)
#define SCHEME_COMPLEXP(obj)     (!SCHEME_INTP(obj) && ((_SCHEME_TYPE(obj) >= scheme_complex_izi_type) && (_SCHEME_TYPE(obj) <= scheme_complex_type)))
#define SCHEME_COMPLEX_IZIP(obj)     (SCHEME_TYPE(obj) == scheme_complex_izi_type)
#define SCHEME_EXACT_INTEGERP(obj)  (SCHEME_INTP(obj) || (_SCHEME_TYPE(obj) == scheme_bignum_type))
#define SCHEME_EXACT_REALP(obj)  (SCHEME_INTP(obj) || (_SCHEME_TYPE(obj) == scheme_bignum_type) || (_SCHEME_TYPE(obj) == scheme_rational_type))
#define SCHEME_REALP(obj)  (SCHEME_INTP(obj) || ((_SCHEME_TYPE(obj) >= scheme_bignum_type) && (_SCHEME_TYPE(obj) <= scheme_complex_izi_type)))
#define SCHEME_NUMBERP(obj)  (SCHEME_INTP(obj) || ((_SCHEME_TYPE(obj) >= scheme_bignum_type) && (_SCHEME_TYPE(obj) <= scheme_complex_type)))

#define SCHEME_STRINGP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_string_type)
#define SCHEME_MUTABLE_STRINGP(obj)  (SCHEME_STRINGP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_STRINGP(obj)  (SCHEME_STRINGP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_SYMBOLP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_symbol_type)

#define SCHEME_BOOLP(obj)    (SAME_OBJ(obj, scheme_true) || SAME_OBJ(obj, scheme_false))
#define SCHEME_FALSEP(obj)     SAME_OBJ((obj), scheme_false)
#define SCHEME_TRUEP(obj)     (!SCHEME_FALSEP(obj))
#define SCHEME_EOFP(obj)     SAME_OBJ((obj), scheme_eof)
#define SCHEME_VOIDP(obj)     SAME_OBJ((obj), scheme_void)

#define SCHEME_NULLP(obj)    SAME_OBJ(obj, scheme_null)
#define SCHEME_PAIRP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_pair_type)
#define SCHEME_MUTABLE_PAIRP(obj)    (SCHEME_PAIRP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_PAIRP(obj)    (SCHEME_PAIRP(obj) && SCHEME_IMMUTABLEP(obj))
#define SCHEME_LISTP(obj)    (SCHEME_NULLP(obj) || SCHEME_PAIRP(obj))

#define SCHEME_BOXP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_box_type)
#define SCHEME_MUTABLE_BOXP(obj)  (SCHEME_BOXP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_BOXP(obj)  (SCHEME_BOXP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_HASHTP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_hash_table_type)

#define SCHEME_VECTORP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_vector_type)
#define SCHEME_MUTABLE_VECTORP(obj)  (SCHEME_VECTORP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_VECTORP(obj)  (SCHEME_VECTORP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_STRUCTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type)
#define SCHEME_STRUCT_TYPEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_type_type)

#define SCHEME_INPORTP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_input_port_type)
#define SCHEME_OUTPORTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_output_port_type)

#define SCHEME_PROMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_promise_type)

#define SCHEME_THREADP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_thread_type)
#define SCHEME_CUSTODIANP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_custodian_type)
#define SCHEME_SEMAP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_sema_type)


#define SCHEME_CONFIGP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_config_type)
#define SCHEME_NAMESPACEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_namespace_type)
#define SCHEME_WEAKP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_weak_box_type)

#define SCHEME_STXP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_stx_type)

#define SCHEME_MUTABLEP(obj) (!((obj)->keyex & 0x1))
#define SCHEME_IMMUTABLEP(obj) ((obj)->keyex & 0x1)

/*========================================================================*/
/*                        basic Scheme accessors                          */
/*========================================================================*/

#define SCHEME_CHAR_VAL(obj) (((Scheme_Small_Object *)(obj))->u.char_val)
#define SCHEME_INT_VAL(obj)  (((long)(obj))>>1)
#define SCHEME_DBL_VAL(obj)  (((Scheme_Double *)(obj))->double_val)
#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLT_VAL(obj)  (((Scheme_Float *)(obj))->float_val)
# define SCHEME_FLOAT_VAL(obj) (SCHEME_DBLP(obj) ? SCHEME_DBL_VAL(obj) : SCHEME_FLT_VAL(obj))
#else
# define SCHEME_FLT_VAL SCHEME_DBL_VAL
# define SCHEME_FLOAT_VAL SCHEME_DBL_VAL
#endif

#define SCHEME_STR_VAL(obj)  ((obj)->u.str_val.string_val)
#define SCHEME_STRTAG_VAL(obj)  ((obj)->u.str_val.tag_val)
#define SCHEME_STRLEN_VAL(obj)  ((obj)->u.str_val.tag_val)
#define SCHEME_SYM_VAL(obj)  (((Scheme_Symbol *)(obj))->s)
#define SCHEME_SYM_LEN(obj)  (((Scheme_Symbol *)(obj))->len)

#define SCHEME_SYMSTR_OFFSET(obj) ((unsigned long)SCHEME_SYM_VAL(obj)-(unsigned long)(obj))

#define SCHEME_BOX_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.ptr_val)

#define SCHEME_CAR(obj)      ((obj)->u.pair_val.car)
#define SCHEME_CDR(obj)      ((obj)->u.pair_val.cdr)

#define SCHEME_CADR(obj)     (SCHEME_CAR (SCHEME_CDR (obj)))
#define SCHEME_CAAR(obj)     (SCHEME_CAR (SCHEME_CAR (obj)))
#define SCHEME_CDDR(obj)     (SCHEME_CDR (SCHEME_CDR (obj)))

#define SCHEME_VEC_SIZE(obj) (((Scheme_Vector *)(obj))->size)
#define SCHEME_VEC_ELS(obj)  (((Scheme_Vector *)(obj))->els)
#define SCHEME_VEC_BASE(obj) SCHEME_VEC_ELS(obj)

#ifdef MZ_PRECISE_GC
# define SCHEME_ENVBOX_VAL(obj)  SCHEME_PTR_VAL(obj)
#else
# define SCHEME_ENVBOX_VAL(obj)  (*((Scheme_Object **)(obj)))
#endif
#define SCHEME_WEAK_BOX_VAL(obj) SCHEME_BOX_VAL(obj) 

#define SCHEME_PTR_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.ptr_val)
#define SCHEME_PTR1_VAL(obj) ((obj)->u.two_ptr_val.ptr1)
#define SCHEME_PTR2_VAL(obj) ((obj)->u.two_ptr_val.ptr2)
#define SCHEME_IPTR_VAL(obj) ((obj)->u.ptr_int_val.ptr)
#define SCHEME_LPTR_VAL(obj) ((obj)->u.ptr_long_val.ptr)
#define SCHEME_INT1_VAL(obj) ((obj)->u.two_int_val.int1)
#define SCHEME_INT2_VAL(obj) ((obj)->u.two_int_val.int2)
#define SCHEME_PINT_VAL(obj) ((obj)->u.ptr_int_val.pint)
#define SCHEME_PLONG_VAL(obj) ((obj)->u.ptr_long_val.pint)

#define SCHEME_SET_IMMUTABLE(obj)  (((obj)->keyex |= 0x1))
#define SCHEME_SET_STRING_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)
#define SCHEME_SET_PAIR_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)
#define SCHEME_SET_VECTOR_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)
#define SCHEME_SET_BOX_IMMUTABLE(obj) SCHEME_SET_IMMUTABLE(obj)

/*========================================================================*/
/*               fast basic Scheme constructor macros                     */
/*========================================================================*/

#define scheme_make_integer(i) ((Scheme_Object *)((((long)i) << 1) | 0x1))
#define scheme_make_character(ch) (scheme_char_constants[(unsigned char)(ch)])

/*========================================================================*/
/*                          procedure values                              */
/*========================================================================*/

/* Constants for flags in Scheme_Primitive_[Closed]_Proc.
   Do not use them directly. */
#define SCHEME_PRIM_IS_FOLDING 1
#define SCHEME_PRIM_IS_PRIMITIVE 2
#define SCHEME_PRIM_IS_STRUCT_PROC 4
#define SCHEME_PRIM_IS_STRUCT_SETTER 8
#define SCHEME_PRIM_IS_PARAMETER 16
#define SCHEME_PRIM_IS_STRUCT_GETTER 32
#define SCHEME_PRIM_IS_STRUCT_PRED 64
#define SCHEME_PRIM_IS_STRUCT_CONSTR 128
#define SCHEME_PRIM_IS_MULTI_RESULT 256
#define SCHEME_PRIM_IS_GENERIC 512
#define SCHEME_PRIM_IS_USER_PARAMETER 1024

typedef struct Scheme_Object *
(Scheme_Prim)(int argc, struct Scheme_Object *argv[]);

typedef struct Scheme_Object *
(Scheme_Closed_Prim)(void *d, int argc, struct Scheme_Object *argv[]);

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short flags; /* keep flags at same place as in closed */
  Scheme_Prim *prim_val;
  const char *name;
  short mina, maxa;
} Scheme_Primitive_Proc;

typedef struct {
  Scheme_Primitive_Proc p;
  short minr, maxr;
} Scheme_Prim_W_Result_Arity;

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short flags; /* keep flags at same place as in unclosed */
  Scheme_Closed_Prim *prim_val;
  void *data;
  const char *name;
  short mina, maxa; /* mina == -2 => maxa is negated case count and
		       record is a Scheme_Closed_Case_Primitive_Proc */
} Scheme_Closed_Primitive_Proc;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  short minr, maxr;
} Scheme_Closed_Prim_W_Result_Arity;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  short *cases;
} Scheme_Closed_Case_Primitive_Proc;

#define _scheme_fill_prim_closure(rec, cfunc, dt, nm, amin, amax) \
  ((rec)->type = scheme_closed_prim_type, \
   (rec)->prim_val = cfunc, \
   (rec)->data = (void *)(dt), \
   (rec)->name = nm, \
   (rec)->mina = amin, \
   (rec)->maxa = amax, \
   rec)
   
#define _scheme_fill_prim_case_closure(rec, cfunc, dt, nm, ccount, cses) \
  ((rec)->p.type = scheme_closed_prim_type, \
   (rec)->p.prim_val = cfunc, \
   (rec)->p.data = (void *)(dt), \
   (rec)->p.name = nm, \
   (rec)->p.mina = -2, \
   (rec)->p.maxa = -(ccount), \
   (rec)->cases = cses, \
   rec)

#define SCHEME_PROCP(obj)    (SCHEME_PRIMP(obj) || SCHEME_CLSD_PRIMP(obj) || SCHEME_CLOSUREP(obj) || SCHEME_CONTP(obj) || SCHEME_ECONTP(obj))
#define SCHEME_SYNTAXP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_syntax_compiler_type)
#define SCHEME_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_prim_type)
#define SCHEME_CLSD_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_closed_prim_type)
#define SCHEME_CONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_cont_type)
#define SCHEME_ECONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_escaping_cont_type)
#define SCHEME_STRUCT_PROCP(obj) (SCHEME_CLSD_PRIMP(obj) && (((Scheme_Closed_Primitive_Proc *)obj)->flags & SCHEME_PRIM_IS_STRUCT_PROC))
#define SCHEME_GENERICP(obj) (SCHEME_CLSD_PRIMP(obj) && (((Scheme_Closed_Primitive_Proc *)obj)->flags & SCHEME_PRIM_IS_GENERIC))
#define SCHEME_CLOSUREP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_linked_closure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_case_closure_type))

#define SCHEME_PRIM(obj)     (((Scheme_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM_DATA(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->data)
#define SCHEME_CLOS_ENV(obj) ((obj)->u.closure_val.env)
#define SCHEME_CLOS_CODE(obj) ((obj)->u.closure_val.code)

/* Type readers & writers for compiled code data */
typedef Scheme_Object *(*Scheme_Type_Reader)(Scheme_Object *list);
typedef Scheme_Object *(*Scheme_Type_Writer)(Scheme_Object *obj);

/*========================================================================*/
/*                      hash tables and environments                      */
/*========================================================================*/

typedef struct Scheme_Bucket
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  void *val;
  char *key;
} Scheme_Bucket;

typedef struct Scheme_Hash_Table
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int size, count, step;
  Scheme_Bucket **buckets;
  char weak, with_home;
  void (*make_hash_indices)(void *v, long *h1, long *h2);
  int (*compare)(void *v1, void *v2);
#ifdef MZ_REAL_THREADS
  void *mutex;
#endif
} Scheme_Hash_Table;

/* Hash tablekey types, used with scheme_hash_table */
enum {
  SCHEME_hash_string,
  SCHEME_hash_ptr,
  SCHEME_hash_bound_id,
  SCHEME_hash_weak_ptr
};

typedef struct Scheme_Env Scheme_Env;

#define SCHEME_VAR_BUCKET(obj) ((Scheme_Bucket *)(obj))

/*========================================================================*/
/*                    setjmpup (continuation) support                     */
/*========================================================================*/

#ifdef USE_MZ_SETJMP
typedef long mz_jmp_buf[8];
#else
# define mz_jmp_buf jmp_buf
#endif

/* Like setjmp & longjmp, but you can jmp to a deeper stack position */
/* Intialize a Scheme_Jumpup_Buf record before using it */
typedef struct Scheme_Jumpup_Buf {
  void *stack_from, *stack_copy;
  long stack_size, stack_max_size;
  struct Scheme_Jumpup_Buf *cont;
  mz_jmp_buf buf;
#ifdef MZ_PRECISE_GC
  void *gc_var_stack;
  void *external_stack;
#endif
} Scheme_Jumpup_Buf;

typedef struct Scheme_Continuation_Jump_State {
  struct Scheme_Escaping_Cont *jumping_to_continuation;
  union {
    Scheme_Object **vals;
    Scheme_Object *val;
  } u;
  short num_vals;
  short is_kill;
} Scheme_Continuation_Jump_State;

/* Although it's really an integer, it seems beneficial to declare the
   mark position counter as a poiner, perhaps due to locality effects. */
#define MZ_MARK_POS_TYPE char*
#define MZ_MARK_STACK_TYPE char*

typedef struct Scheme_Cont_Frame_Data {
  MZ_MARK_POS_TYPE cont_mark_pos;
  MZ_MARK_STACK_TYPE cont_mark_stack;
} Scheme_Cont_Frame_Data;

/*========================================================================*/
/*                              threads                                   */
/*========================================================================*/

typedef void Scheme_Close_Custodian_Client(Scheme_Object *o, void *data);
typedef void (*Scheme_Exit_Closer_Func)(Scheme_Object *, Scheme_Close_Custodian_Client *, void *);

#ifdef MZ_PRECISE_GC
typedef struct Scheme_Object Scheme_Custodian_Reference;
#else
typedef struct Scheme_Custodian *Scheme_Custodian_Reference;
#endif

typedef struct Scheme_Custodian Scheme_Custodian;

/* The Scheme_Thread structure represents a MzScheme thread. */

typedef struct Scheme_Thread {
  Scheme_Type type;
  MZ_HASH_KEY_EX

  struct Scheme_Thread *next;
  struct Scheme_Thread *prev;

  mz_jmp_buf error_buf;
  Scheme_Continuation_Jump_State cjs;

  struct Scheme_Config *config;

  Scheme_Object **runstack;
  Scheme_Object **runstack_start;
  long runstack_size;
  struct Scheme_Saved_Stack *runstack_saved;
  Scheme_Object **runstack_tmp_keep;

  MZ_MARK_POS_TYPE cont_mark_pos;     /* depth of the continuation chain */
  MZ_MARK_STACK_TYPE cont_mark_stack; /* current mark stack position */
  struct Scheme_Cont_Mark **cont_mark_stack_segments;
  int cont_mark_seg_count;

  long engine_weight;

  void *stack_start, *stack_end;
  Scheme_Jumpup_Buf jmpup_buf;
#ifdef MZ_REAL_THREADS
  void *thread;
  int break_received;
  struct timeval *select_tv;
# ifdef MZ_USE_LINUX_PTHREADS
  int jump_on_signal;
  mz_jmp_buf signal_buf;
# endif
#endif 

  void *cc_start;
  long *cc_ok;
  long *ec_ok;
  struct Scheme_Dynamic_Wind *dw;

  int running;

  struct Scheme_Thread *nester, *nestee;

  float sleep_time; /* blocker has starting sleep time */
  int block_descriptor;
  Scheme_Object *blocker; /* semaphore or port */
  int (*block_check)(Scheme_Object *blocker);
  void (*block_needs_wakeup)(Scheme_Object *blocker, void *fds);
  short ran_some;

  short overflow_set;
  struct Scheme_Overflow *overflow;
  mz_jmp_buf overflow_buf;

  struct Scheme_Comp_Env *current_local_env;
  Scheme_Object *current_local_mark;
  Scheme_Object *current_local_name;

  /* These are used to lock in values during `read' and `print': */
  char quick_can_read_compiled;
  char quick_can_read_pipe_quote;
  char quick_can_read_box;
  char quick_can_read_graph;
  char quick_can_read_dot;
  char quick_case_sens;
  char quick_square_brackets_are_parens;
  char quick_curly_braces_are_parens;
  char quick_read_decimal_inexact;
  Scheme_Object *quick_inspector;

  /* Used during `display' and `write': */
  char *print_buffer;
  long print_position;
  long print_allocated;
  long print_maxlen;
  Scheme_Object *print_port;
  mz_jmp_buf print_escape;

  char exn_raised;
  char error_invoked;
  char err_val_str_invoked;

  Scheme_Object *(*overflow_k)(void);
  Scheme_Object *overflow_reply;
  Scheme_Jumpup_Buf overflow_cont;

  Scheme_Object **tail_buffer;
  int tail_buffer_size;

  union {
    struct {
      Scheme_Object *wait_expr;
    } eval;
    struct {
      Scheme_Object *tail_rator;
      Scheme_Object **tail_rands;
      int tail_num_rands;
    } apply;
    struct {
      Scheme_Object **array;
      int count;
    } multiple;
    struct {
      void *p1, *p2, *p3, *p4;
      long i1, i2, i3;
    } k;
  } ku;

  short suspend_break;
  short external_break;

#ifdef MZ_REAL_THREADS
  Scheme_Object *done_sema;
  volatile long fuel_counter;
# define scheme_fuel_counter (scheme_current_thread->fuel_counter)
# define scheme_stack_boundary ((unsigned long)scheme_current_thread->stack_end)
#endif

  Scheme_Object *list_stack;
  int list_stack_pos;

  Scheme_Hash_Table *rn_memory;

  long block_start_sleep;

  int eof_on_error; /* For port operations */

  /* MzScheme client can use: */
  void (*on_kill)(struct Scheme_Thread *p);
  void *kill_data;

  /* MzScheme use only: */
  void (*private_on_kill)(void *);
  void *private_kill_data;
  void **private_kill_next; /* array of three pointers */

  void **user_tls;
  int user_tls_size;

  struct Scheme_Thread_Custodian_Hop *mr_hop;
  Scheme_Custodian_Reference *mref;
} Scheme_Thread;

#if !SCHEME_DIRECT_EMBEDDED
# ifdef MZ_REAL_THREADS
#  define scheme_current_thread (scheme_get_current_thread())
# else
#  ifdef LINK_EXTENSIONS_BY_TABLE
#   define scheme_current_thread (*scheme_current_thread_ptr)
#  endif
# endif
#endif

/*========================================================================*/
/*                             parameters                                 */
/*========================================================================*/

enum {
  MZCONFIG_ENV,
  MZCONFIG_INPUT_PORT,
  MZCONFIG_OUTPUT_PORT,
  MZCONFIG_ERROR_PORT,

  MZCONFIG_ENABLE_BREAK,

  MZCONFIG_ERROR_DISPLAY_HANDLER,
  MZCONFIG_ERROR_PRINT_VALUE_HANDLER,

  MZCONFIG_EXIT_HANDLER,

  MZCONFIG_EXN_HANDLER,
  MZCONFIG_INIT_EXN_HANDLER,

  MZCONFIG_EVAL_HANDLER,
  MZCONFIG_LOAD_HANDLER,

  MZCONFIG_PRINT_HANDLER,
  MZCONFIG_PROMPT_READ_HANDLER,

  MZCONFIG_CAN_READ_GRAPH,
  MZCONFIG_CAN_READ_COMPILED,
  MZCONFIG_CAN_READ_BOX,
  MZCONFIG_CAN_READ_PIPE_QUOTE,
  MZCONFIG_CAN_READ_DOT,
  MZCONFIG_READ_DECIMAL_INEXACT,

  MZCONFIG_PRINT_GRAPH,
  MZCONFIG_PRINT_STRUCT,
  MZCONFIG_PRINT_BOX,
  MZCONFIG_PRINT_VEC_SHORTHAND,

  MZCONFIG_CASE_SENS,
  MZCONFIG_SQUARE_BRACKETS_ARE_PARENS,
  MZCONFIG_CURLY_BRACES_ARE_PARENS,

  MZCONFIG_ERROR_PRINT_WIDTH,

  MZCONFIG_ERROR_ESCAPE_HANDLER,

  MZCONFIG_ALLOW_SET_UNDEFINED,

  MZCONFIG_CUSTODIAN,
  MZCONFIG_INSPECTOR,

  MZCONFIG_USE_COMPILED_KIND,

  MZCONFIG_LOAD_DIRECTORY,

  MZCONFIG_COLLECTION_PATHS,

  MZCONFIG_PORT_PRINT_HANDLER,

  MZCONFIG_LOAD_EXTENSION_HANDLER,

  MZCONFIG_CURRENT_DIRECTORY,

  MZCONFIG_RANDOM_STATE,

  MZCONFIG_CURRENT_MODULE_RESOLVER,
  MZCONFIG_CURRENT_MODULE_PREFIX,

  __MZCONFIG_BUILTIN_COUNT__
};


typedef struct Scheme_Config {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Hash_Table *extensions;
  Scheme_Object *configs[1];
} Scheme_Config;

#define scheme_set_param(c, pos, o) ((c)->configs[pos] = (o))
#define scheme_get_param(c, pos) ((c)->configs[pos])

/*========================================================================*/
/*                                  ports                                 */
/*========================================================================*/

typedef struct Scheme_Input_Port
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short closed;
  Scheme_Object *sub_type;
  Scheme_Custodian_Reference *mref;
  void *port_data;
  int (*getc_fun) (struct Scheme_Input_Port *port);
  int (*peekc_fun) (struct Scheme_Input_Port *port);
  int (*char_ready_fun) (struct Scheme_Input_Port *port);
  void (*close_fun) (struct Scheme_Input_Port *port);
  void (*need_wakeup_fun)(struct Scheme_Input_Port *, void *);
  Scheme_Object *(*get_special_fun)(struct Scheme_Input_Port *);
  Scheme_Object *read_handler;
  char *name;
  unsigned char *ungotten;
  int ungotten_count, ungotten_allocated;
  long position, lineNumber, charsSinceNewline;
  long column, oldColumn; /* column tracking with one tab/newline ungetc */
  int count_lines;
#ifdef MZ_REAL_THREADS
  Scheme_Object *sema;
#endif
} Scheme_Input_Port;

typedef struct Scheme_Output_Port
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short closed;
  Scheme_Object *sub_type;
  Scheme_Custodian_Reference *mref;
  void *port_data;
  void (*write_string_fun)(char *str, long d, long len, struct Scheme_Output_Port *);
  void (*close_fun) (struct Scheme_Output_Port *);
  long pos;
  Scheme_Object *display_handler;
  Scheme_Object *write_handler;
  Scheme_Object *print_handler;
#ifdef MZ_REAL_THREADS
  Scheme_Object *sema;
#endif
} Scheme_Output_Port;

#define SCHEME_INPORT_VAL(obj) (((Scheme_Input_Port *)(obj))->port_data)
#define SCHEME_OUTPORT_VAL(obj) (((Scheme_Output_Port *)(obj))->port_data)
#define SCHEME_IPORT_NAME(obj) (((Scheme_Input_Port *)obj)->name)

/*========================================================================*/
/*                              exceptions                                */
/*========================================================================*/

/* This file includes the MZEXN constants */
#ifdef INCLUDE_WITHOUT_PATHS
# include "schexn.h"
#else
# include "../src/schexn.h"
#endif

/*========================================================================*/
/*                               evaluation                               */
/*========================================================================*/

/* Exploit the fact that these should never be dereferenced: */
#ifndef FIRST_TWO_BYTES_ARE_LEGAL_ADDRESSES
# define MZ_EVAL_WAITING_CONSTANT ((Scheme_Object *)0x2)
# define MZ_APPLY_WAITING_CONSTANT ((Scheme_Object *)0x4)
# define MZ_MULTIPLE_VALUES_CONSTANT ((Scheme_Object *)0x6)
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
# define SCHEME_EVAL_WAITING MZ_EVAL_WAITING_CONSTANT
# define SCHEME_TAIL_CALL_WAITING MZ_APPLY_WAITING_CONSTANT
# define SCHEME_MULTIPLE_VALUES MZ_MULTIPLE_VALUES_CONSTANT
#else
# define SCHEME_TAIL_CALL_WAITING scheme_tail_call_waiting
# define SCHEME_EVAL_WAITING scheme_eval_waiting
# define SCHEME_MULTIPLE_VALUES scheme_multiple_values
#endif

#define SCHEME_ASSERT(expr,msg) ((expr) ? 1 : (scheme_signal_error(msg), 0))

#define scheme_eval_wait_expr (scheme_current_thread->ku.eval.wait_expr)
#define scheme_tail_rator (scheme_current_thread->ku.apply.tail_rator)
#define scheme_tail_num_rands (scheme_current_thread->ku.apply.tail_num_rands)
#define scheme_tail_rands (scheme_current_thread->ku.apply.tail_rands)
#define scheme_overflow_k (scheme_current_thread->overflow_k)
#define scheme_overflow_reply (scheme_current_thread->overflow_reply)
#define scheme_overflow_cont (scheme_current_thread->overflow_cont)

#define scheme_error_buf (scheme_current_thread->error_buf)
#define scheme_jumping_to_continuation (scheme_current_thread->cjs.jumping_to_continuation)
#define scheme_config (scheme_current_thread->config)

#define scheme_multiple_count (scheme_current_thread->ku.multiple.count)
#define scheme_multiple_array (scheme_current_thread->ku.multiple.array)

#define scheme_setjmpup(b, base, s) scheme_setjmpup_relative(b, base, s, NULL)

#ifdef MZ_REAL_THREADS
#define scheme_do_eval(r,n,e,f) scheme_do_eval_w_thread(r,n,e,f,scheme_current_thread)
#else
#define scheme_do_eval_w_thread(r,n,e,f,p) scheme_do_eval(r,n,e,f)
#endif
#ifdef MZ_REAL_THREADS
#define scheme_apply(r,n,a) scheme_apply_wp(r,n,a,scheme_current_thread)
#define scheme_apply_multi(r,n,a) scheme_apply_multi_wp(r,n,a,scheme_current_thread)
#define scheme_apply_eb(r,n,a) scheme_apply_eb_wp(r,n,a,scheme_current_thread)
#define scheme_apply_multi_eb(r,n,a) scheme_apply_multi_eb_wp(r,n,a,scheme_current_thread)
#else
#define scheme_apply_wp(r,n,a,p) scheme_apply(r,n,a)
#define scheme_apply_multi_wp(r,n,a,p) scheme_apply_multi(r,n,a)
#define scheme_apply_eb_wp(r,n,a,p) scheme_apply_eb(r,n,a)
#define scheme_apply_multi_eb_wp(r,n,a,p) scheme_apply_multi_eb(r,n,a)
#endif

#define _scheme_apply(r,n,rs) scheme_do_eval(r,n,rs,1)
#define _scheme_apply_multi(r,n,rs) scheme_do_eval(r,n,rs,-1)
#define _scheme_apply_wp(r,n,rs,p) scheme_do_eval_w_thread(r,n,rs,1,p)
#define _scheme_apply_multi_wp(r,n,rs,p) scheme_do_eval_w_thread(r,n,rs,-1,p)
#define _scheme_tail_apply scheme_tail_apply
#define _scheme_tail_apply_wp scheme_tail_apply_wp

#define _scheme_tail_eval scheme_tail_eval
#define _scheme_tail_eval_wp scheme_tail_eval_wp

#define _scheme_direct_apply_primitive_multi(prim, argc, argv) \
    (((Scheme_Primitive_Proc *)prim)->prim_val(argc, argv))
#define _scheme_direct_apply_primitive(prim, argc, argv) \
    scheme_check_one_value(_scheme_direct_apply_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_multi(prim, argc, argv) \
    (((Scheme_Closed_Primitive_Proc *)prim)->prim_val(((Scheme_Closed_Primitive_Proc *)prim)->data, argc, argv))
#define _scheme_direct_apply_closed_primitive(prim, argc, argv) \
    scheme_check_one_value(_scheme_direct_apply_closed_primitive_multi(prim, argc, argv))

#define _scheme_force_value(v) ((v == SCHEME_TAIL_CALL_WAITING) ? scheme_force_value(v) : v)

#define scheme_tail_apply_buffer_wp(n, p) ((p)->tail_buffer)
#define scheme_tail_apply_buffer(n) scheme_tail_apply_buffer_wp(n, scheme_current_thread)

#define _scheme_tail_apply_no_copy_wp_tcw(f, n, args, p, tcw) (p->ku.apply.tail_rator = f, p->ku.apply.tail_rands = args, p->ku.apply.tail_num_rands = n, tcw)
#define _scheme_tail_apply_no_copy_wp(f, n, args, p) _scheme_tail_apply_no_copy_wp_tcw(f, n, args, p, SCHEME_TAIL_CALL_WAITING)
#define _scheme_tail_apply_no_copy(f, n, args) _scheme_tail_apply_no_copy_wp(f, n, args, scheme_current_thread)

#ifndef MZ_REAL_THREADS
# define scheme_thread_block_w_thread(t,p) scheme_thread_block(t)
#else
# define scheme_thread_block(t) scheme_thread_block_w_thread(t,scheme_current_thread)
#endif

#ifndef MZ_REAL_THREADS
# if !SCHEME_DIRECT_EMBEDDED
#  ifdef LINK_EXTENSIONS_BY_TABLE
#   define scheme_fuel_counter (*scheme_fuel_counter_ptr)
#  endif
# else
extern volatile int scheme_fuel_counter;
# endif
#endif

#ifdef FUEL_AUTODECEREMENTS
# define DECREMENT_FUEL(f, p) (f)
#else
# define DECREMENT_FUEL(f, p) (f -= (p))
#endif

#ifdef MZ_REAL_THREADS
# define _scheme_check_for_break_wp(penalty, p) \
   { if (DECREMENT_FUEL((p)->fuel_counter, penalty) <= 0) scheme_thread_block_w_thread(0, p); }
#else
# define _scheme_check_for_break_wp(penalty, p) \
   { if (DECREMENT_FUEL(scheme_fuel_counter, penalty) <= 0) scheme_thread_block_w_thread(0, p); }
#endif
#define _scheme_check_for_break(penalty) _scheme_check_for_break_wp(penalty, scheme_current_thread)

#if SCHEME_DIRECT_EMBEDDED
extern Scheme_Object *scheme_eval_waiting;
#define scheme_tail_eval(obj) \
 (scheme_eval_wait_expr = obj, SCHEME_EVAL_WAITING)
#endif

#define scheme_break_waiting(p) (p->external_break)

#ifndef USE_MZ_SETJMP
# ifdef JMP_BUF_IS_JMPBUF
#  define scheme_longjmp(b, v) longjmp(&b, v)
#  define scheme_setjmp(b) setjmp(&b)
# else
#  define scheme_longjmp(b, v) longjmp(b, v)
#  define scheme_setjmp(b) setjmp(b)
# endif
#endif

/*========================================================================*/
/*                      memory management macros                          */
/*========================================================================*/

/* Allocation */
#define scheme_alloc_object() \
   ((Scheme_Object *) scheme_malloc_tagged(sizeof(Scheme_Object)))
#define scheme_alloc_small_object() \
   ((Scheme_Object *) scheme_malloc_tagged(sizeof(Scheme_Small_Object)))
#define scheme_alloc_stubborn_object() \
   ((Scheme_Object *) scheme_malloc_stubborn_tagged(sizeof(Scheme_Object)))
#define scheme_alloc_stubborn_small_object() \
   ((Scheme_Object *) scheme_malloc_stubborn_tagged(sizeof(Scheme_Small_Object)))
#define scheme_alloc_eternal_object() \
   ((Scheme_Object *) scheme_malloc_eternal_tagged(sizeof(Scheme_Object)))
#define scheme_alloc_eternal_small_object() \
   ((Scheme_Object *) scheme_malloc_eternal_tagged(sizeof(Scheme_Small_Object)))

#ifdef SCHEME_NO_GC
void *scheme_malloc(size_t size);
# define scheme_malloc_atomic scheme_malloc
# define scheme_malloc_stubborn scheme_malloc
# define scheme_malloc_uncollectable scheme_malloc
#else
# define scheme_malloc GC_malloc
# define scheme_malloc_atomic GC_malloc_atomic
# ifdef MZ_PRECISE_GC
#  define scheme_malloc_stubborn scheme_malloc
# else
#  define scheme_malloc_stubborn GC_malloc_stubborn
# endif
# define scheme_malloc_uncollectable GC_malloc_uncollectable
#endif

#ifdef USE_MEMORY_TRACING
# define USE_TAGGED_ALLOCATION
# define MEMORY_COUNTING_ON
#endif

#ifdef MZ_PRECISE_GC
# ifdef INCLUDE_WITHOUT_PATHS
#  include "gc2.h"
# else
#  include "../gc2/gc2.h"
# endif
# define scheme_malloc_tagged GC_malloc_one_tagged
# define scheme_malloc_array_tagged GC_malloc_array_tagged
# define scheme_malloc_atomic_tagged GC_malloc_atomic_tagged
# define scheme_malloc_stubborn_tagged GC_malloc_one_tagged
# define scheme_malloc_eternal_tagged GC_malloc_atomic_uncollectable
# define scheme_malloc_uncollectable_tagged >> error <<
# define scheme_malloc_envunbox GC_malloc_one_tagged
# define scheme_malloc_weak GC_malloc_weak
# define scheme_malloc_weak_tagged GC_malloc_one_weak_tagged
# define scheme_malloc_allow_interior GC_malloc_allow_interior
#else
# ifdef USE_TAGGED_ALLOCATION
extern void *scheme_malloc_tagged(size_t);
#  define scheme_malloc_array_tagged scheme_malloc
extern void *scheme_malloc_atomic_tagged(size_t);
extern void *scheme_malloc_stubborn_tagged(size_t);
extern void *scheme_malloc_eternal_tagged(size_t);
extern void *scheme_malloc_uncollectable_tagged(size_t);
extern void *scheme_malloc_envunbox(size_t);
# else
#  define scheme_malloc_tagged scheme_malloc
#  define scheme_malloc_array_tagged scheme_malloc
#  define scheme_malloc_atomic_tagged scheme_malloc_atomic
#  define scheme_malloc_stubborn_tagged scheme_malloc_stubborn
#  define scheme_malloc_eternal_tagged scheme_malloc_eternal
#  define scheme_malloc_uncollectable_tagged scheme_malloc_uncollectable
#  define scheme_malloc_envunbox scheme_malloc
# endif
# define scheme_malloc_allow_interior scheme_malloc
#endif


#ifdef MZ_PRECISE_GC
# define MZ_CWVR(x) (GC_variable_stack = __gc_var_stack__, x)
# define MZ_DECL_VAR_REG(size) void *__gc_var_stack__[size+2]; \
                               __gc_var_stack__[0] = GC_variable_stack; \
                               __gc_var_stack__[1] = (void *)size;
# define MZ_VAR_REG(x, v) (__gc_var_stack__[x+2] = (void *)&(v))
# define MZ_ARRAY_VAR_REG(x, v, l) (__gc_var_stack__[x+2] = (void *)0, \
                                    __gc_var_stack__[x+3] = (void *)&(v), \
                                    __gc_var_stack__[x+4] = (void *)l)
#else
# define MZ_CWVR(x)                x
# define MZ_DECL_VAR_REG(size)     /* empty */
# define MZ_VAR_REG(x, v)          /* empty */
# define MZ_ARRAY_VAR_REG(x, v, l) /* empty */
#endif

/*========================================================================*/
/*                   embedding configuration and hooks                    */
/*========================================================================*/

#if SCHEME_DIRECT_EMBEDDED

#if defined(_IBMR2)
extern long scheme_stackbottom;
#endif

extern int scheme_defining_primitives;

/* These flags must be set before MzScheme is started: */
extern int scheme_case_sensitive; /* Defaults to 0 */
extern int scheme_no_keywords; /* Defaults to 0 */
extern int scheme_allow_set_undefined; /* Defaults to 0 */
extern int scheme_escape_continuations_only; /* Defaults to 0 */
extern int scheme_square_brackets_are_parens; /* Defaults to 1 */
extern int scheme_curly_braces_are_parens; /* Defaults to 1 */
extern int scheme_hash_percent_syntax_only; /* Defaults to 0 */
extern int scheme_hash_percent_globals_only; /* Defaults to 0 */
extern int scheme_binary_mode_stdio; /* Windows-MacOS-specific. Defaults to 0 */

#ifdef MZ_REAL_THREADS
Scheme_Thread *scheme_get_current_thread();
# define scheme_current_thread (SCHEME_GET_CURRENT_THREAD())
#else
extern Scheme_Thread *scheme_current_thread;
#endif
extern Scheme_Thread *scheme_first_thread;

/* Set these global hooks (optionally): */
extern void (*scheme_exit)(int v);
extern void (*scheme_console_printf)(char *str, ...);
extern void (*scheme_console_output)(char *str, long len);
extern void (*scheme_sleep)(float seconds, void *fds);
extern void (*scheme_notify_multithread)(int on);
extern void (*scheme_wakeup_on_input)(void *fds);
extern int (*scheme_check_for_break)(void);
#ifdef MZ_PRECISE_GC
extern void *(*scheme_get_external_stack_val)(void);
extern void (*scheme_set_external_stack_val)(void *);
#endif
#ifdef USE_WIN32_THREADS
extern void (*scheme_suspend_main_thread)(void);
int scheme_set_in_main_thread(void);
void scheme_restore_nonmain_thread(void);
#endif
#ifdef MAC_FILE_SYSTEM
extern long scheme_creator_id;
#endif
#ifdef MACINTOSH_EVENTS
extern void (*scheme_handle_aewait_event)(EventRecord *e);
#endif

extern Scheme_Object *(*scheme_make_stdin)(void);
extern Scheme_Object *(*scheme_make_stdout)(void);
extern Scheme_Object *(*scheme_make_stderr)(void);

void scheme_set_banner(char *s);
Scheme_Object *scheme_set_exec_cmd(char *s);

/* Initialization */
Scheme_Env *scheme_basic_env(void);

#ifdef USE_MSVC_MD_LIBRARY
void GC_pre_init(void);
#endif

void scheme_check_threads(void);
void scheme_wake_up(void);
int scheme_get_external_event_fd(void);

/* image dump enabling startup: */
int scheme_image_main(int argc, char **argv);
extern int (*scheme_actual_main)(int argc, char **argv);

/* GC registration: */
#ifdef GC_MIGHT_USE_REGISTERED_STATICS
void scheme_set_stack_base();
#endif

void scheme_register_static(void *ptr, long size);
#if defined(MUST_REGISTER_GLOBALS) || defined(GC_MIGHT_USE_REGISTERED_STATICS)
# define MZ_REGISTER_STATIC(x)  scheme_register_static((void *)&x, sizeof(x))
#else
# define MZ_REGISTER_STATIC(x) /* empty */
#endif

void scheme_setup_forced_exit(void);

void scheme_start_atomic(void);
void scheme_end_atomic(void);

#endif /* SCHEME_DIRECT_EMBEDDED */

/*========================================================================*/
/*                              FFI functions                             */
/*========================================================================*/

/* If MzScheme is being empbedded, then we just include the
   prototypes. Otherwise, we may include a function-table definition
   instead, plus macros that map the usual name to table lookups. */

#if SCHEME_DIRECT_EMBEDDED

/* All functions & global constants prototyped here */
#ifdef INCLUDE_WITHOUT_PATHS
# include "schemef.h"
#else
# include "../src/schemef.h"
#endif

#else

#ifdef LINK_EXTENSIONS_BY_TABLE
/* Constants and function prototypes as function pointers in a struct: */
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schemex.h"
# else
#  include "../src/schemex.h"
# endif

extern Scheme_Extension_Table *scheme_extension_table;

/* Macro mapping names to record access */
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schemexm.h"
# else
#  include "../src/schemexm.h"
# endif

#else

/* Not LINK_EXTENSIONS_BY_TABLE */
# ifdef INCLUDE_WITHOUT_PATHS
#  include "schemef.h"
# else
#  include "../src/schemef.h"
# endif

#endif

#endif

/*========================================================================*/
/*                              misc flags                                */
/*========================================================================*/

/* For use with scheme_symbol_name_and_size: */
#define SNF_FOR_TS 0x1
#define SNF_PIPE_QUOTE 0x2
#define SNF_NO_PIPE_QUOTE 0x4

/* For use with scheme_make_struct_values et al.: */
#define SCHEME_STRUCT_NO_TYPE 0x01
#define SCHEME_STRUCT_NO_CONSTR 0x02
#define SCHEME_STRUCT_NO_PRED 0x04
#define SCHEME_STRUCT_NO_GET 0x08
#define SCHEME_STRUCT_NO_SET 0x10
#define SCHEME_STRUCT_GEN_GET 0x20
#define SCHEME_STRUCT_GEN_SET 0x40

/*========================================================================*/
/*                           file descriptors                             */
/*========================================================================*/

#if defined(DETECT_WIN32_CONSOLE_STDIN) || defined(WINDOWS_PROCESSES)
# ifndef NO_STDIO_THREADS
#  define USE_FAR_MZ_FDCALLS
# endif
#endif
#ifdef USE_DYNAMIC_FDSET_SIZE
# define USE_FAR_MZ_FDCALLS
#endif
#ifdef USE_BEOS_PORT_THREADS
# define USE_FAR_MZ_FDCALLS
#endif

#ifdef USE_FAR_MZ_FDCALLS
# define MZ_GET_FDSET(p, n) scheme_get_fdset(p, n)
# define MZ_FD_ZERO(p) scheme_fdzero(p)
# define MZ_FD_SET(n, p) scheme_fdset(p, n)
# define MZ_FD_CLR(n, p) scheme_fdclr(p, n)
# define MZ_FD_ISSET(n, p) scheme_fdisset(p, n)
#else
# define MZ_GET_FDSET(p, n) ((void *)(((fd_set *)p) + n))
# define MZ_FD_ZERO(p) FD_ZERO(p)
# define MZ_FD_SET(n, p) FD_SET(n, p)
# define MZ_FD_CLR(n, p) FD_CLR(n, p)
# define MZ_FD_ISSET(n, p) FD_ISSET(n, p)
#endif

#ifdef __cplusplus
}
#endif

#if defined(__MWERKS__)
# ifdef MZSCHEME_USES_NEAR_GLOBALS
#  pragma far_data reset
# endif
#endif

#endif /* ! SCHEME_H */

