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
/*III*/

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

#ifdef INCLUDE_WITHOUT_PATHS
# include "schvers.h"
#else
# include "../src/schvers.h"
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
/* In case SGC is used to build PRECISE_GC: */
# undef USE_SENORA_GC
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
#ifndef MSCBOR_IZE
# define MSCBOR_IZE(x) MSC_IZE(x)
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

#ifdef MACINTOSH_EVENTS
/* We avoid #including the Carbon headers because we only
   need a few abstract struct types: */
typedef struct FSSpec mzFSSpec;
#endif

/* Set up MZ_EXTERN for DLL build */
#if SCHEME_DIRECT_EMBEDDED && defined(WINDOWS_DYNAMIC_LOAD) \
    && (defined(_MSC_VER) || defined(__BORLANDC__)) \
    && !defined(SCHEME_EMBEDDED_NO_DLL)
# define MZ_DLLIMPORT __declspec(dllimport)
# ifdef __mzscheme_private__
#  define MZ_DLLSPEC __declspec(dllexport)
# else
#  define MZ_DLLSPEC __declspec(dllimport)
# endif
#else
# define MZ_DLLSPEC
# define MZ_DLLIMPORT
#endif

#define MZ_EXTERN extern MZ_DLLSPEC

/* PPC Linux plays a slimy trick: it defines strcpy() as a macro that
   uses __extension__. This breaks the 3m xform. */
#if defined(MZ_PRECISE_GC) && defined(strcpy)
START_XFORM_SKIP;
static inline void _mzstrcpy(char *a, const char *b)
{
  strcpy(a, b);
}
END_XFORM_SKIP;
# undef strcpy
# define strcpy _mzstrcpy
#endif

#ifdef __cplusplus
extern "C" 
{
#endif

/*========================================================================*/
/*                        basic Scheme values                             */
/*========================================================================*/

typedef short Scheme_Type;

/* Used to use `short' for app arg counts, etc., but adding limit
   checks is difficult, and seems arbitrary. We can switch back
   to short if the expense turns out to be noticable; in that case
   also define MZSHORT_IS_SHORT. */
typedef int mzshort;

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
     purpose, though. For string, pair, vector, and box values in all
     variants of MzScheme, the low bit is set to 1 to indicate that
     the object is immutable. */
  short keyex;

  union
    {
      struct { char *string_val; int tag_val; } str_val;
      struct { void *ptr1, *ptr2; } two_ptr_val;
      struct { int int1; int int2; } two_int_val;
      struct { void *ptr; int pint; } ptr_int_val;
      struct { void *ptr; long pint; } ptr_long_val;
      struct { struct Scheme_Object *car, *cdr; } pair_val;
      struct { mzshort len; mzshort *vec; } svector_val;
    } u;
} Scheme_Object;

typedef struct Scheme_Object *(*Scheme_Closure_Func)(struct Scheme_Object *);

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
  short keyex; /* 1 in low bit indicates uninterned */
  int len;
  char s[4]; /* Really, a number of chars to match `len' */
} Scheme_Symbol;

typedef struct Scheme_Vector {
  Scheme_Type type;
  short keyex; /* 1 in low bit indicates immutable */
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

#define SCHEME_BUCKTP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_bucket_table_type)
#define SCHEME_HASHTP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_hash_table_type)

#define SCHEME_VECTORP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_vector_type)
#define SCHEME_MUTABLE_VECTORP(obj)  (SCHEME_VECTORP(obj) && SCHEME_MUTABLEP(obj))
#define SCHEME_IMMUTABLE_VECTORP(obj)  (SCHEME_VECTORP(obj) && SCHEME_IMMUTABLEP(obj))

#define SCHEME_STRUCTP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_proc_struct_type))
#define SCHEME_STRUCT_TYPEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_type_type)

#define SCHEME_INPORTP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_input_port_type)
#define SCHEME_OUTPORTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_output_port_type)

#define SCHEME_PROMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_promise_type)

#define SCHEME_THREADP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_thread_type)
#define SCHEME_CUSTODIANP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_custodian_type)
#define SCHEME_SEMAP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_sema_type)
#define SCHEME_CHANNELP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_channel_type)
#define SCHEME_CHANNEL_PUTP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_channel_put_type)

#define SCHEME_CONFIGP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_config_type)
#define SCHEME_NAMESPACEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_namespace_type)
#define SCHEME_WEAKP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_weak_box_type)

#define SCHEME_STXP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_stx_type)

#define SCHEME_UDPP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_udp_type)
#define SCHEME_UDP_WAITP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_udp_waitable_type)

#define SCHEME_CPTRP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_c_pointer_type)

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
# define SCHEME_FLT_VAL(x) ((float)(SCHEME_DBL_VAL(x)))
# define SCHEME_FLOAT_VAL SCHEME_DBL_VAL
# define scheme_make_float(x) scheme_make_double((double)x)
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

#define SCHEME_ENVBOX_VAL(obj)  (*((Scheme_Object **)(obj)))
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

#define SCHEME_CPTR_VAL(obj) SCHEME_PTR1_VAL(obj)
#define SCHEME_CPTR_TYPE(obj) ((char *)SCHEME_PTR2_VAL(obj))

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
#define SCHEME_PRIM_IS_METHOD 2048

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
  mzshort mina, maxa;
} Scheme_Primitive_Proc;

typedef struct {
  Scheme_Primitive_Proc p;
  mzshort minr, maxr;
} Scheme_Prim_W_Result_Arity;

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short flags; /* keep flags at same place as in unclosed */
  Scheme_Closed_Prim *prim_val;
  void *data;
  const char *name;
  mzshort mina, maxa; /* mina == -2 => maxa is negated case count and
		       record is a Scheme_Closed_Case_Primitive_Proc */
} Scheme_Closed_Primitive_Proc;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  mzshort minr, maxr;
} Scheme_Closed_Prim_W_Result_Arity;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  mzshort *cases;
} Scheme_Closed_Case_Primitive_Proc;

#define _scheme_fill_prim_closure(rec, cfunc, dt, nm, amin, amax, flgs) \
  ((rec)->type = scheme_closed_prim_type, \
   (rec)->prim_val = cfunc, \
   (rec)->data = (void *)(dt), \
   (rec)->name = nm, \
   (rec)->mina = amin, \
   (rec)->maxa = amax, \
   (rec)->flags = flgs, \
   rec)
 
#define _scheme_fill_prim_case_closure(rec, cfunc, dt, nm, ccount, cses, flgs) \
  ((rec)->p.type = scheme_closed_prim_type, \
   (rec)->p.prim_val = cfunc, \
   (rec)->p.data = (void *)(dt), \
   (rec)->p.name = nm, \
   (rec)->p.mina = -2, \
   (rec)->p.maxa = -(ccount), \
   (rec)->p.flags = flgs, \
   (rec)->cases = cses, \
   rec)

#define SCHEME_PROCP(obj)  (!SCHEME_INTP(obj) && ((_SCHEME_TYPE(obj) >= scheme_prim_type) && (_SCHEME_TYPE(obj) <= scheme_proc_struct_type)))
#define SCHEME_SYNTAXP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_syntax_compiler_type)
#define SCHEME_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_prim_type)
#define SCHEME_CLSD_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_closed_prim_type)
#define SCHEME_CONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_cont_type)
#define SCHEME_ECONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_escaping_cont_type)
#define SCHEME_PROC_STRUCTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_proc_struct_type)
#define SCHEME_STRUCT_PROCP(obj) (SCHEME_CLSD_PRIMP(obj) && (((Scheme_Closed_Primitive_Proc *)obj)->flags & SCHEME_PRIM_IS_STRUCT_PROC))
#define SCHEME_GENERICP(obj) (SCHEME_CLSD_PRIMP(obj) && (((Scheme_Closed_Primitive_Proc *)obj)->flags & SCHEME_PRIM_IS_GENERIC))
#define SCHEME_CLOSUREP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_closure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_case_closure_type))

#define SCHEME_PRIM(obj)     (((Scheme_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM_DATA(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->data)
#define SCHEME_CLOS_FUNC(obj) ((Scheme_Closure_Func)SCHEME_CAR(obj))
#define SCHEME_CLOS_DATA(obj) SCHEME_CDR(obj)

/*========================================================================*/
/*                      hash tables and environments                      */
/*========================================================================*/

typedef struct Scheme_Hash_Table
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int size, count, step;
  Scheme_Object **keys;
  Scheme_Object **vals;
  void (*make_hash_indices)(void *v, long *h1, long *h2);
  int (*compare)(void *v1, void *v2);
  Scheme_Object *mutex;
} Scheme_Hash_Table;


typedef struct Scheme_Bucket
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  void *val;
  char *key;
} Scheme_Bucket;

typedef struct Scheme_Bucket_Table
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int size, count, step;
  Scheme_Bucket **buckets;
  char weak, with_home;
  void (*make_hash_indices)(void *v, long *h1, long *h2);
  int (*compare)(void *v1, void *v2);
  Scheme_Object *mutex;
} Scheme_Bucket_Table;

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
typedef long mz_pre_jmp_buf[8];
#else
# define mz_pre_jmp_buf jmp_buf
#endif

#ifdef MZ_PRECISE_GC
typedef struct {
  mz_pre_jmp_buf jb;
  long gcvs; /* declared as `long' so it isn't pushed when on the stack! */
  long gcvs_cnt;
} mz_jmp_buf;
#else
# define mz_jmp_buf mz_pre_jmp_buf
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

typedef struct Scheme_Jumpup_Buf_Holder {
  Scheme_Type type; /* for precise GC only */
  Scheme_Jumpup_Buf buf;
} Scheme_Jumpup_Buf_Holder;

typedef struct Scheme_Continuation_Jump_State {
  struct Scheme_Escaping_Cont *jumping_to_continuation;
  union {
    Scheme_Object **vals;
    Scheme_Object *val;
  } u;
  mzshort num_vals;
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

typedef int (*Scheme_Ready_Fun)(Scheme_Object *o);
typedef void (*Scheme_Needs_Wakeup_Fun)(Scheme_Object *, void *);
typedef Scheme_Object *(*Scheme_Wait_Sema_Fun)(Scheme_Object *, int *repost);
typedef int (*Scheme_Wait_Filter_Fun)(Scheme_Object *);

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

  void *cc_start;
  long *cc_ok;
  long *ec_ok;
  struct Scheme_Dynamic_Wind *dw;

  int running;
  Scheme_Object *suspended_box; /* contains pointer to thread when it's suspended */
  Scheme_Object *resumed_box;   /* contains pointer to thread when it's resumed */
  Scheme_Object *dead_box;      /* contains non-zero when the thread is dead */
  Scheme_Object *running_box;   /* contains pointer to thread when it's running */

  struct Scheme_Thread *nester, *nestee;

  float sleep_time; /* blocker has starting sleep time */
  int block_descriptor;
  Scheme_Object *blocker; /* semaphore or port */
  Scheme_Ready_Fun block_check;
  Scheme_Needs_Wakeup_Fun block_needs_wakeup;
  char ran_some;
  char suspend_to_kill;

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
  char quick_can_read_quasi;
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

  Scheme_Object **tail_buffer;
  int tail_buffer_size;

  /* values_buffer is used to avoid allocating for `values'
     calls. When ku.multiple.array is not the same as
     values_buffer, then it can be zeroed at GC points. */
  Scheme_Object **values_buffer;
  int values_buffer_size;

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

  Scheme_Object *list_stack;
  int list_stack_pos;

  Scheme_Hash_Table *rn_memory;

  long block_start_sleep;

  /* MzScheme client can use: */
  void (*on_kill)(struct Scheme_Thread *p);
  void *kill_data;

  /* MzScheme use only: */
  void (*private_on_kill)(void *);
  void *private_kill_data;
  void **private_kill_next; /* array of three pointers */

  void **user_tls;
  int user_tls_size;

  /* save thread-specific GMP state: */
  long gmp_tls[6];

  struct Scheme_Thread_Custodian_Hop *mr_hop;
  Scheme_Custodian_Reference *mref;
  Scheme_Object *transitive_resumes; /* A hash table of running-boxes */
} Scheme_Thread;

#if !SCHEME_DIRECT_EMBEDDED
# ifdef LINK_EXTENSIONS_BY_TABLE
#  define scheme_current_thread (*scheme_current_thread_ptr)
# endif
#endif

typedef void (*Scheme_Kill_Action_Func)(void *);

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
  MZCONFIG_CAN_READ_QUASI,
  MZCONFIG_READ_DECIMAL_INEXACT,

  MZCONFIG_PRINT_GRAPH,
  MZCONFIG_PRINT_STRUCT,
  MZCONFIG_PRINT_BOX,
  MZCONFIG_PRINT_VEC_SHORTHAND,
  MZCONFIG_PRINT_HASH_TABLE,

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

  MZCONFIG_ERROR_PRINT_SRCLOC,

  MZCONFIG_CMDLINE_ARGS,

  MZCONFIG_LOCALE,

  MZCONFIG_SECURITY_GUARD,

  MZCONFIG_PORT_COUNT_LINES,

  MZCONFIG_SCHEDULER_RANDOM_STATE,

  __MZCONFIG_BUILTIN_COUNT__
};


typedef struct Scheme_Config {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  int *use_count; /* non-zero => copy-on-write of extensions table */
  Scheme_Bucket_Table *extensions;
  Scheme_Object *configs[1];
} Scheme_Config;

#define scheme_set_param(c, pos, o) ((c)->configs[pos] = (o))
#define scheme_get_param(c, pos) ((c)->configs[pos])

/*========================================================================*/
/*                                  ports                                 */
/*========================================================================*/

typedef struct Scheme_Input_Port Scheme_Input_Port;
typedef struct Scheme_Output_Port Scheme_Output_Port;

typedef long (*Scheme_Get_String_Fun)(Scheme_Input_Port *port, 
				      char *buffer, long offset, long size,
				      int nonblock);
typedef long (*Scheme_Peek_String_Fun)(Scheme_Input_Port *port, 
				       char *buffer, long offset, long size,
				       Scheme_Object *skip,
				       int nonblock);
typedef int (*Scheme_In_Ready_Fun)(Scheme_Input_Port *port);
typedef void (*Scheme_Close_Input_Fun)(Scheme_Input_Port *port);
typedef void (*Scheme_Need_Wakeup_Input_Fun)(Scheme_Input_Port *, void *);

typedef long (*Scheme_Write_String_Fun)(Scheme_Output_Port *,
					const char *str, long offset, long size,
					int rarely_block);
typedef int (*Scheme_Out_Ready_Fun)(Scheme_Output_Port *port);
typedef void (*Scheme_Close_Output_Fun)(Scheme_Output_Port *port);
typedef void (*Scheme_Need_Wakeup_Output_Fun)(Scheme_Output_Port *, void *);

struct Scheme_Input_Port
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  char closed, pending_eof;
  Scheme_Object *sub_type;
  Scheme_Custodian_Reference *mref;
  void *port_data;
  Scheme_Get_String_Fun get_string_fun;
  Scheme_Peek_String_Fun peek_string_fun;
  Scheme_In_Ready_Fun char_ready_fun;
  Scheme_Close_Input_Fun close_fun;
  Scheme_Need_Wakeup_Input_Fun need_wakeup_fun;
  Scheme_Object *read_handler;
  char *name;
  Scheme_Object *peeked_read, *peeked_write;
  unsigned char ungotten[4];
  int ungotten_count;
  Scheme_Object *special, *ungotten_special;
  long position, readpos, lineNumber, charsSinceNewline;
  long column, oldColumn; /* column tracking with one tab/newline ungetc */
  int count_lines, was_cr;
  struct Scheme_Output_Port *output_half;
};

struct Scheme_Output_Port
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short closed;
  Scheme_Object *sub_type;
  Scheme_Custodian_Reference *mref;
  void *port_data;
  Scheme_Write_String_Fun write_string_fun;
  Scheme_Close_Output_Fun close_fun;
  Scheme_Out_Ready_Fun ready_fun;
  Scheme_Need_Wakeup_Output_Fun need_wakeup_fun;
  long pos;
  Scheme_Object *display_handler;
  Scheme_Object *write_handler;
  Scheme_Object *print_handler;
  struct Scheme_Input_Port *input_half;
};

#define SCHEME_INPORT_VAL(obj) (((Scheme_Input_Port *)(obj))->port_data)
#define SCHEME_OUTPORT_VAL(obj) (((Scheme_Output_Port *)(obj))->port_data)
#define SCHEME_IPORT_NAME(obj) (((Scheme_Input_Port *)obj)->name)

#define SCHEME_SPECIAL (-2)

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
/*                               security                                 */
/*========================================================================*/

#define SCHEME_GUARD_FILE_READ    0x1
#define SCHEME_GUARD_FILE_WRITE   0x2
#define SCHEME_GUARD_FILE_EXECUTE 0x4
#define SCHEME_GUARD_FILE_DELETE  0x8
#define SCHEME_GUARD_FILE_EXISTS  0x10

/*========================================================================*/
/*                               modules                                  */
/*========================================================================*/

typedef void (*Scheme_Invoke_Proc)(Scheme_Env *env, long phase_shift, 
				   Scheme_Object *self_modidx, void *data);

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

#define scheme_error_buf (scheme_current_thread->error_buf)
#define scheme_jumping_to_continuation (scheme_current_thread->cjs.jumping_to_continuation)
#define scheme_config (scheme_current_thread->config)

#define scheme_multiple_count (scheme_current_thread->ku.multiple.count)
#define scheme_multiple_array (scheme_current_thread->ku.multiple.array)

#define scheme_setjmpup(b, base, s) scheme_setjmpup_relative(b, base, s, NULL)

#define scheme_do_eval_w_thread(r,n,e,f,p) scheme_do_eval(r,n,e,f)
#define scheme_apply_wp(r,n,a,p) scheme_apply(r,n,a)
#define scheme_apply_multi_wp(r,n,a,p) scheme_apply_multi(r,n,a)
#define scheme_apply_eb_wp(r,n,a,p) scheme_apply_eb(r,n,a)
#define scheme_apply_multi_eb_wp(r,n,a,p) scheme_apply_multi_eb(r,n,a)

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

#define scheme_thread_block_w_thread(t,p) scheme_thread_block(t)

#if !SCHEME_DIRECT_EMBEDDED
# ifdef LINK_EXTENSIONS_BY_TABLE
#  define scheme_fuel_counter (*scheme_fuel_counter_ptr)
# endif
#else
MZ_EXTERN volatile int scheme_fuel_counter;
#endif

#ifdef FUEL_AUTODECEREMENTS
# define DECREMENT_FUEL(f, p) (f)
#else
# define DECREMENT_FUEL(f, p) (f -= (p))
#endif

#define SCHEME_USE_FUEL(n) \
  { if (DECREMENT_FUEL(scheme_fuel_counter, n) <= 0) { scheme_out_of_fuel(); }}

#if SCHEME_DIRECT_EMBEDDED
MZ_EXTERN Scheme_Object *scheme_eval_waiting;
#define scheme_tail_eval(obj) \
 (scheme_eval_wait_expr = obj, SCHEME_EVAL_WAITING)
#endif

#define scheme_break_waiting(p) (p->external_break)

#ifndef USE_MZ_SETJMP
# ifdef USE_UNDERSCORE_SETJMP
#  define scheme_mz_longjmp(b, v) _longjmp(b, v)
#  define scheme_mz_setjmp(b) _setjmp(b)
# else
#  define scheme_mz_longjmp(b, v) longjmp(b, v)
#  define scheme_mz_setjmp(b) setjmp(b)
# endif
#endif

#ifdef MZ_PRECISE_GC
/* Need to make sure that a __gc_var_stack__ is always available where
   setjmp & longjmp are used. */
# define scheme_longjmp(b, v) (((long *)((b).gcvs))[1] = (b).gcvs_cnt, \
                               GC_variable_stack = (void **)(b).gcvs, \
                               scheme_mz_longjmp((b).jb, v))
# define scheme_setjmp(b)     ((b).gcvs = (long)__gc_var_stack__, \
                               (b).gcvs_cnt = (long)(__gc_var_stack__[1]), \
                               scheme_mz_setjmp((b).jb))
#else
# define scheme_longjmp(b, v) scheme_mz_longjmp(b, v)
# define scheme_setjmp(b) scheme_mz_setjmp(b)
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
# ifndef GC2_EXTERN
#  define GC2_EXTERN MZ_EXTERN
# endif
# ifdef INCLUDE_WITHOUT_PATHS
#  if SCHEME_DIRECT_EMBEDDED
#   include "gc2.h"
#  else
#   define GC2_JUST_MACROS_AND_TYPEDEFS
#   include "schemegc2.h"
#  endif
# else
#  include "../gc2/gc2.h"
# endif
# define scheme_malloc_tagged GC_malloc_one_tagged
# define scheme_malloc_array_tagged GC_malloc_array_tagged
# define scheme_malloc_atomic_tagged GC_malloc_atomic_tagged
# define scheme_malloc_stubborn_tagged GC_malloc_one_tagged
# define scheme_malloc_eternal_tagged GC_malloc_atomic_uncollectable
# define scheme_malloc_uncollectable_tagged >> error <<
# define scheme_malloc_envunbox GC_malloc
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
# define MZ_GC_DECL_REG(size) void *__gc_var_stack__[size+2] = { 0, size };
# define MZ_GC_VAR_IN_REG(x, v) (__gc_var_stack__[x+2] = (void *)&(v))
# define MZ_GC_ARRAY_VAR_IN_REG(x, v, l) (__gc_var_stack__[x+2] = (void *)0, \
                                          __gc_var_stack__[x+3] = (void *)&(v), \
                                          __gc_var_stack__[x+4] = (void *)l)
# define MZ_GC_REG()  (__gc_var_stack__[0] = GC_variable_stack, \
                       GC_variable_stack = __gc_var_stack__)
# define MZ_GC_UNREG() (GC_variable_stack = __gc_var_stack__[0])
#else
# define MZ_GC_DECL_REG(size)            /* empty */
# define MZ_GC_VAR_IN_REG(x, v)          /* empty */
# define MZ_GC_ARRAY_VAR_IN_REG(x, v, l) /* empty */
# define MZ_GC_REG()                     /* empty */
# define MZ_GC_UNREG()                   /* empty */
#endif

/*========================================================================*/
/*                   embedding configuration and hooks                    */
/*========================================================================*/

#if SCHEME_DIRECT_EMBEDDED

#if defined(_IBMR2)
MZ_EXTERN long scheme_stackbottom;
#endif

MZ_EXTERN int scheme_defining_primitives;

/* These flags must be set before MzScheme is started: */
MZ_EXTERN int scheme_case_sensitive; /* Defaults to 0 */
MZ_EXTERN int scheme_no_keywords; /* Defaults to 0 */
MZ_EXTERN int scheme_allow_set_undefined; /* Defaults to 0 */
MZ_EXTERN int scheme_square_brackets_are_parens; /* Defaults to 1 */
MZ_EXTERN int scheme_curly_braces_are_parens; /* Defaults to 1 */
MZ_EXTERN int scheme_hash_percent_syntax_only; /* Defaults to 0 */
MZ_EXTERN int scheme_hash_percent_globals_only; /* Defaults to 0 */
MZ_EXTERN int scheme_binary_mode_stdio; /* Windows-MacOS-specific. Defaults to 0 */

MZ_EXTERN Scheme_Thread *scheme_current_thread;
MZ_EXTERN Scheme_Thread *scheme_first_thread;

/* Set these global hooks (optionally): */
MZ_EXTERN void (*scheme_exit)(int v);
MZ_EXTERN void (*scheme_console_printf)(char *str, ...);
MZ_EXTERN void (*scheme_console_output)(char *str, long len);
MZ_EXTERN void (*scheme_sleep)(float seconds, void *fds);
MZ_EXTERN void (*scheme_notify_multithread)(int on);
MZ_EXTERN void (*scheme_wakeup_on_input)(void *fds);
MZ_EXTERN int (*scheme_check_for_break)(void);
MZ_EXTERN Scheme_Object *(*scheme_module_demand_hook)(int c, Scheme_Object **a);
#ifdef MZ_PRECISE_GC
MZ_EXTERN void *(*scheme_get_external_stack_val)(void);
MZ_EXTERN void (*scheme_set_external_stack_val)(void *);
#endif
#ifdef USE_WIN32_THREADS
MZ_EXTERN void (*scheme_suspend_main_thread)(void);
int scheme_set_in_main_thread(void);
void scheme_restore_nonmain_thread(void);
#endif
#ifdef MAC_FILE_SYSTEM
extern long scheme_creator_id;
#endif

MZ_EXTERN Scheme_Object *(*scheme_make_stdin)(void);
MZ_EXTERN Scheme_Object *(*scheme_make_stdout)(void);
MZ_EXTERN Scheme_Object *(*scheme_make_stderr)(void);

MZ_EXTERN void scheme_set_banner(char *s);
MZ_EXTERN Scheme_Object *scheme_set_exec_cmd(char *s);

/* Initialization */
MZ_EXTERN Scheme_Env *scheme_basic_env(void);

#ifdef USE_MSVC_MD_LIBRARY
MZ_EXTERN void GC_pre_init(void);
#endif

MZ_EXTERN void scheme_check_threads(void);
MZ_EXTERN void scheme_wake_up(void);
MZ_EXTERN int scheme_get_external_event_fd(void);

/* image dump enabling startup: */
MZ_EXTERN int scheme_image_main(int argc, char **argv);
MZ_EXTERN int (*scheme_actual_main)(int argc, char **argv);

/* GC registration: */
#ifdef GC_MIGHT_USE_REGISTERED_STATICS
MZ_EXTERN void scheme_set_stack_base(void *base, int no_auto_statics);
#endif

MZ_EXTERN void scheme_register_static(void *ptr, long size);
#if defined(MUST_REGISTER_GLOBALS) || defined(GC_MIGHT_USE_REGISTERED_STATICS)
# define MZ_REGISTER_STATIC(x)  scheme_register_static((void *)&x, sizeof(x))
#else
# define MZ_REGISTER_STATIC(x) /* empty */
#endif

MZ_EXTERN void scheme_start_atomic(void);
MZ_EXTERN void scheme_end_atomic(void);
MZ_EXTERN void scheme_end_atomic_no_swap(void);
MZ_EXTERN void (*scheme_on_atomic_timeout)(void);

MZ_EXTERN void scheme_immediate_exit(int status);

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
#define SCHEME_SNF_FOR_TS 0x1
#define SCHEME_SNF_PIPE_QUOTE 0x2
#define SCHEME_SNF_NO_PIPE_QUOTE 0x4
#define SCHEME_SNF_NEED_CASE 0x8

/* For use with scheme_make_struct_values et al.: */
#define SCHEME_STRUCT_NO_TYPE 0x01
#define SCHEME_STRUCT_NO_CONSTR 0x02
#define SCHEME_STRUCT_NO_PRED 0x04
#define SCHEME_STRUCT_NO_GET 0x08
#define SCHEME_STRUCT_NO_SET 0x10
#define SCHEME_STRUCT_GEN_GET 0x20
#define SCHEME_STRUCT_GEN_SET 0x40
#define SCHEME_STRUCT_EXPTIME 0x80

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

