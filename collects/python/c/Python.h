#ifndef _PYTHON_H_
#define _PYTHON_H_

// Here is the fake python file
//#include <windows.h>
#include <assert.h>
#include <limits.h>


#include <stdarg.h>
#define HAVE_STDARG_PROTOTYPES


#include "escheme.h"

#ifdef Py_USING_UNICODE
#undef Py_USING_UNICODE
#endif

#define SPY_GLOBALS_SCHEME_STRUCT() _spy_g_scheme_struct
#define SPY_GLOBALS_PYTHON_NODE() _spy_g_python_node
// cache SCHEME_STRUCT and PYTHON-NODE (for spy_init_obj, etc)
extern Scheme_Type _spy_g_scheme_struct;
extern Scheme_Object* _spy_g_python_node;

//struct PyTypeObject;
struct _typeobject;
#define PYTYPEOBJECT struct _typeobject

  // these two should probably be something like SCHEME_HEAD
  #define PyObject_HEAD Scheme_Type scheme_type; \
	MZ_HASH_KEY_EX \
	void* stype; \
        PYTYPEOBJECT * ob_type; \
        Scheme_Object* is_mutable; \
        int ob_refcnt/*Scheme_Object* the_py_dict*/;

        //Scheme_Object* slots[/*3*/2];

#define SPY_SCHEME_TYPE(s_o) (s_o)->scheme_type
#define SCHEME_SET_TYPE(s_o, t) ((s_o)->scheme_type = (t))

/*
#define PyObject_HEAD   Scheme_Type type; \
  short keyex; \
  int u; */
/*  union \
    { \
      struct { char *string_val; int tag_val; } str_val; \
      struct { void *ptr1, *ptr2; } two_ptr_val; \
      struct { int int1; int int2; } two_int_val; \
      struct { void *ptr; int pint; } ptr_int_val; \
      struct { void *ptr; long pint; } ptr_long_val; \
      struct { struct Scheme_Object *car, *cdr; } pair_val; \
      struct { mzshort len; mzshort *vec; } svector_val;\
    } u;
*/

#ifdef MZ_PRECISE_GC
  #define MZ_HASH_KEY_EX_INIT 0,
#else
  #define MZ_HASH_KEY_EX_INIT
#endif

#define PyObject_VAR_HEAD \
		PyObject_HEAD \
		int ob_size;

//#define PyObject_HEAD_INIT(type) 0, MZ_HASH_KEY_EX_INIT 0, {0, 0, 0},
#define PyObject_HEAD_INIT(type) 0, MZ_HASH_KEY_EX_INIT 0, type, scheme_true, 0,

typedef struct dScheme_Structure
{
  PyObject_HEAD
} dScheme_Structure;

typedef struct PyVarObject
{
  PyObject_VAR_HEAD
} PyVarObject;

//typedef Scheme_Object PyObject;
typedef dScheme_Structure PyObject;
//typedef Scheme_Object PyVarObject;
//typedef Scheme_Object PyTypeObject;

//typedef Scheme_Object PyIntObject;
//typedef Scheme_Object PySliceObject;
//typedef PyObject PyIntObject;
//typedef PyObject PySliceObject;

//typedef Scheme_Object PyStringObject;




// Kludge

static int _____spy___zero___ = 0x0;

#define KABOOM() (fprintf(stderr, "undefined fn in file %s at line %d\n", __FILE__, __LINE__), exit(1), 0)
#define KABOOM_SEGFAULT() do{fprintf(stderr, "Spy: Error in file %s at line %d\n", __FILE__, __LINE__); ((PyObject*)_____spy___zero___)->ob_type = ((PyObject*)_____spy___zero___)->ob_refcnt; exit(1);}while(0)
//#define KABOOM_SEGFAULT() fprintf(stderr, "Spy: Error in file %s at line %d\n", __FILE__, __LINE__)

// FIXME!!

// uhh... fake this for now
//#define PyCodec_Decode(str, encoding, errors) (str)
//#define PyCodec_Encode(str, encoding, errors) (str)


#define PY_GET_TYPE(obj) ((PyObject*) obj)->ob_type
	//(sapply1("python-node-type", (PyObject*) obj))
#define PY_SET_TYPE(obj, t) (((PyObject*)obj)->ob_type = (t))
//(sapply2("set-python-node-type!", (PyObject*) obj, t))

#define SPY_SET_MUTABLE(obj, flag) (((PyObject*)obj)->is_mutable = (flag))

//#define SPY_SET_DICT(obj, d) (((PyObject*) obj)->the_py_dict = (d))
#define SPY_SET_DICT(obj, d) (((PyObject*) obj)->ob_refcnt = (int) (d))


//#define PY_TYPE_STR_FN(type) generic_repr

//#define PY_NUMBER_OCT_FN(n) py_number_to_octal_py_string
//#define PY_TYPE_AS_NUMBER(t) (t)
//#define PY_TYPE_AS_MAPPING(t) (t)
//#define PY_NUMBER_HEX_FN(n) py_number_to_hex_py_string

#define PY_TYPE_METHODS(t) ((t)->tp_methods)

//PyObject* py_number_to_octal_py_string(PyObject* num);
//PyObject* py_number_to_hex_py_string(PyObject* num);

#define SPY_INIT_SCHEME_HEADER(obj) \
  ( SCHEME_SET_TYPE((PyObject*) (obj), SPY_GLOBALS_SCHEME_STRUCT()), /* STRUCT */ \
    (((PyObject*) obj)->stype = SPY_GLOBALS_PYTHON_NODE()), /* PYTHON-NODE */ \
    (obj) )

static __inline__ PyObject* SPY_INIT_OBJ(PyObject* obj, PYTYPEOBJECT* type) {
  SPY_INIT_SCHEME_HEADER(obj);
  PY_SET_TYPE(obj, type);
  SPY_SET_MUTABLE(obj, scheme_true);
  return obj;
}

static __inline__ PyObject* SPY_INIT_GLOBAL_impl(PyObject* obj, int size) {
  SPY_INIT_SCHEME_HEADER(obj);
  scheme_register_extension_global((void*) obj, size);
  return obj;
}

#define SPY_INIT_GLOBAL(global) SPY_INIT_GLOBAL_impl(&global, sizeof(global))

#if 0  // what if obj is some expression??
#define SPY_INIT_OBJ(obj, type) \
  ( SPY_INIT_SCHEME_HEADER(obj),
    PY_SET_TYPE(obj, type), \
    SPY_SET_MUTABLE(obj, scheme_true), \
    /*SPY_SET_DICT(obj, seval("(make-hash-table)")),*/ \
    (obj) )
#endif


/* Convert a possibly signed character to a nonnegative int */
/* XXX This assumes characters are 8 bits wide */
#ifdef __CHAR_UNSIGNED__
#define Py_CHARMASK(c)          (c)
#else
#define Py_CHARMASK(c)          ((c) & 0xff)
#endif


// scheme_free?
//#define PY_FREE(obj) 0

#define PY_TYPE_GET_NAME(type_obj) ((PyTypeObject*) type_obj)->tp_name
  //(sapply1("python-get-name", (PyObject*) type_obj))
#define PY_TYPE_GET_ALLOC_FN(type_obj) ((PyTypeObject*) type_obj)->tp_alloc
   //alloc_py_type

PyObject* alloc_py_type(PYTYPEOBJECT * type, int size);
PyObject* generic_repr(PyObject* obj);

//#define PyObject_TypeCheck(obj, type) ((obj) && sapply2("py-is-a?", obj, type))


#define SPY_CODE_GET_LAMBDA(code_obj) (((PyCodeObject*)(code_obj))->co_code)
#define SPY_CODE_SET_LAMBDA(code_obj, lam) (((PyCodeObject*)(code_obj))->co_code = (lam))


#define Py_GCC_ATTRIBUTE(stuff)
#define Py_INCREF(thing) ((PyObject*) thing)->ob_refcnt++
#define Py_DECREF(thing) ((PyObject*) thing)->ob_refcnt++
//#define Py_XDECREF(thing) ((PyObject*) thing)->ob_refcnt--
#define Py_XINCREF(op) if ((op) == NULL) ; else Py_INCREF(op)
#define Py_XDECREF(op) if ((op) == NULL) ; else Py_DECREF(op)


// fake selector
#define PY_REFCNT(obj) 1
#define PY_SET_REFCNT(obj, cnt) 1

#define DL_IMPORT(type) type

//#define Py_None (slookup("py-none"))

#define PyAPI_FUNC(RTYPE) RTYPE
#define PyAPI_DATA(RTYPE) extern RTYPE

//#			if defined(__cplusplus)
//#				define PyMODINIT_FUNC extern "C" __declspec(dllexport) void
//#			else /* __cplusplus */
//#				define PyMODINIT_FUNC __declspec(dllexport) void
//#			endif /* __cplusplus */
#define PyMODINIT_FUNC void

/* from pydebug
void Py_FatalError (const char * message);
*/

/* from pyport
*/
#define Py_IS_INFINITY(X) ((X) && (X)*0.5 == (X))
#define Py_ARITHMETIC_RIGHT_SHIFT(TYPE, I, J) ((I) >> (J))
#ifndef Py_HUGE_VAL
#define Py_HUGE_VAL 99999
#endif
#define Py_OVERFLOWED(X) ((X) != 0.0 && (errno == ERANGE ||    \
                                         (X) == Py_HUGE_VAL || \
                                         (X) == -Py_HUGE_VAL))
#define Py_ADJUST_ERANGE1(X)                                            \
        do {                                                            \
                if (errno == 0) {                                       \
                        if ((X) == Py_HUGE_VAL || (X) == -Py_HUGE_VAL)  \
                                errno = ERANGE;                         \
                }                                                       \
                else if (errno == ERANGE && (X) == 0.0)                 \
                        errno = 0;                                      \
        } while(0)

#define Py_ADJUST_ERANGE2(X, Y)                                         \
        do {                                                            \
                if ((X) == Py_HUGE_VAL || (X) == -Py_HUGE_VAL ||        \
                    (Y) == Py_HUGE_VAL || (Y) == -Py_HUGE_VAL) {        \
                                if (errno == 0)                         \
                                        errno = ERANGE;                 \
                }                                                       \
                else if (errno == ERANGE)                               \
                        errno = 0;                                      \
        } while(0)




//PyObject * PyList_New (int size);

/*  Now declared in their respective CPython headers
#define DECLARE_CHECKER(name) int Py##name##_Check (PyObject * thing)

DECLARE_CHECKER (Float);
DECLARE_CHECKER (Int);
DECLARE_CHECKER (Long);
DECLARE_CHECKER (List);
DECLARE_CHECKER (Tuple);
DECLARE_CHECKER (Sequence);
DECLARE_CHECKER (String);

long PyInt_AsLong (PyObject * pyint);
//long PyInt_AsUnsignedLongMask (PyObject * pyint);
unsigned long PyLong_AsUnsignedLongMask (PyObject * pyint);
double PyFloat_AsDouble (PyObject * pyfloat);
PyObject* PyInt_FromInt(int value);

PyObject* PyTuple_New(int size);
int PyTuple_GET_SIZE (PyObject * tuple);
PyObject* PyTuple_GET_ITEM (PyObject * tuple, int index);
//PyObject* PyTuple_GetItem(PyObject * tuple, int index);
//PyObject* PyTuple_SetItem (PyObject * tuple, int index, PyObject * new_item);

int PySequence_Size (PyObject * sequence);
PyObject * PySequence_GetItem (PyObject * sequence, int index);
*/

PyObject* PyErr_NewException(char *name, PyObject *base, PyObject *dict);

/*
int PyString_Size (PyObject * string);
int PyString_GET_SIZE (PyObject * string);
// use this only when py strings are structs with a PyObject_VAR_HEAD
#define PyString_SET_SIZE(s, size) ((s)->ob_size = size)
//void PyString_SET_SIZE (PyObject * string, int);
char * PyString_AS_STRING (PyObject * string);
*/

#define PyOS_snprintf snprintf

// special pscm.c functions
//void SPY_SET_ATTR(PyObject* obj, const char* attr, PyObject* value);
//PyObject* SPY_GET_ATTR(PyObject* obj, const char* attr);



// from the original Python.h
/* Define macros for inline documentation. */
#define PyDoc_VAR(name) static char name[]
#define PyDoc_STRVAR(name,str) PyDoc_VAR(name) = PyDoc_STR(str)

#ifdef WITH_DOC_STRINGS
#define PyDoc_STR(str) str
#else
#define PyDoc_STR(str) ""
#endif


// this should be in some form of stringobject.h
/*
#define PY_STRING_SET_S_HASH(s, hash) ((s)->ob_shash = (hash))
#define PY_STRING_GET_S_HASH(s) ((s)->ob_shash)
#define PY_STRING_SET_S_STATE(s, state) ((s)->ob_sstate = (state))
*/
//#define PY_STRING_SET_S_HASH(s, hash) SPY_SET_ATTR(s, "shash", hash)
//#define PY_STRING_GET_S_HASH(s) SPY_GET_ATTR(s, "shash")
//#define PY_STRING_SET_S_STATE(s, state) SPY_SET_ATTR(s, "state", state)


// this should be in object.h
//#define Py_PRINT_RAW 1
//PyAPI_DATA(PyObject) _Py_NotImplementedStruct; /* Don't use this directly */
//#define Py_NotImplemented (&_Py_NotImplementedStruct)
/* Rich comparison opcodes */
/*
#define Py_LT 0
#define Py_LE 1
#define Py_EQ 2
#define Py_NE 3
#define Py_GT 4
#define Py_GE 5
*/

//#define PyObject_Repr generic_repr

/*
typedef PyObject * (*unaryfunc)(PyObject *);
typedef PyObject * (*binaryfunc)(PyObject *, PyObject *);
typedef PyObject * (*ternaryfunc)(PyObject *, PyObject *, PyObject *);
typedef int (*inquiry)(PyObject *);
typedef int (*coercion)(PyObject **, PyObject **);
typedef PyObject *(*intargfunc)(PyObject *, int);
typedef PyObject *(*intintargfunc)(PyObject *, int, int);
typedef int(*intobjargproc)(PyObject *, int, PyObject *);
typedef int(*intintobjargproc)(PyObject *, int, int, PyObject *);
typedef int(*objobjargproc)(PyObject *, PyObject *, PyObject *);
typedef int (*getreadbufferproc)(PyObject *, int, void **);
typedef int (*getwritebufferproc)(PyObject *, int, void **);
typedef int (*getsegcountproc)(PyObject *, int *);
typedef int (*getcharbufferproc)(PyObject *, int, const char **);
typedef int (*objobjproc)(PyObject *, PyObject *);
typedef int (*visitproc)(PyObject *, void *);
typedef int (*traverseproc)(PyObject *, visitproc, void *);
*/
/*
typedef struct { */
	/* For numbers without flag bit Py_TPFLAGS_CHECKTYPES set, all
	   arguments are guaranteed to be of the object's type (modulo
	   coercion hacks -- i.e. if the type's coercion function
	   returns other types, then these are allowed as well).  Numbers that
	   have the Py_TPFLAGS_CHECKTYPES flag bit set should check *both*
	   arguments for proper type and implement the necessary conversions
	   in the slot functions themselves. */
/*	binaryfunc nb_add;
	binaryfunc nb_subtract;
	binaryfunc nb_multiply;
	binaryfunc nb_divide;
	binaryfunc nb_remainder;
	binaryfunc nb_divmod;
	ternaryfunc nb_power;
	unaryfunc nb_negative;
	unaryfunc nb_positive;
	unaryfunc nb_absolute;
	inquiry nb_nonzero;
	unaryfunc nb_invert;
	binaryfunc nb_lshift;
	binaryfunc nb_rshift;
	binaryfunc nb_and;
	binaryfunc nb_xor;
	binaryfunc nb_or;
	coercion nb_coerce;
	unaryfunc nb_int;
	unaryfunc nb_long;
	unaryfunc nb_float;
	unaryfunc nb_oct;
	unaryfunc nb_hex;
*/	/* Added in release 2.0 */
/*	binaryfunc nb_inplace_add;
	binaryfunc nb_inplace_subtract;
	binaryfunc nb_inplace_multiply;
	binaryfunc nb_inplace_divide;
	binaryfunc nb_inplace_remainder;
	ternaryfunc nb_inplace_power;
	binaryfunc nb_inplace_lshift;
	binaryfunc nb_inplace_rshift;
	binaryfunc nb_inplace_and;
	binaryfunc nb_inplace_xor;
	binaryfunc nb_inplace_or;
*/	/* Added in release 2.2 */
	/* The following require the Py_TPFLAGS_HAVE_CLASS flag */
/*	binaryfunc nb_floor_divide;
	binaryfunc nb_true_divide;
	binaryfunc nb_inplace_floor_divide;
	binaryfunc nb_inplace_true_divide;
} PyNumberMethods;
typedef struct {
	inquiry sq_length;
	binaryfunc sq_concat;
	intargfunc sq_repeat;
	intargfunc sq_item;
	intintargfunc sq_slice;
	intobjargproc sq_ass_item;
	intintobjargproc sq_ass_slice;
	objobjproc sq_contains;
*/	/* Added in release 2.0 */
/*	binaryfunc sq_inplace_concat;
	intargfunc sq_inplace_repeat;
} PySequenceMethods;
typedef struct {
	inquiry mp_length;
	binaryfunc mp_subscript;
	objobjargproc mp_ass_subscript;
} PyMappingMethods;
typedef struct {
	getreadbufferproc bf_getreadbuffer;
	getwritebufferproc bf_getwritebuffer;
	getsegcountproc bf_getsegcount;
	getcharbufferproc bf_getcharbuffer;
} PyBufferProcs;
*/

// no unicode
#define PyUnicode_Check(obj) 0

// this should be in boolobject.h
/* Don't use these directly */
//PyAPI_DATA(PyIntObject) _Py_ZeroStruct, _Py_TrueStruct;
/* Use these macros */
/*
#define Py_False ((PyObject *) &_Py_ZeroStruct)
#define Py_True ((PyObject *) &_Py_TrueStruct)
*/


// this should be in longobject.h
//#define PyLong_Check(item) (sapply2("py-is-a?", item, slookup("py-number%")) != scheme_false)
//#define PyLong_AsLong PyInt_AsLong
#define PyInt_FromInt(x) PyInt_FromLong((long)x)
#define PyInt_AsInt PyInt_AsLong

// maybe functionobject.h ?
//#define PyFunction_Check(item) (sapply2("py-is-a?", item, slookup("py-function%")) != scheme_false)


// this should be in sliceobject.h
//#define PySlice_Check(obj) sapply2("py-is-a?", item, slookup("py-slice%"))
//#define PySlice_GetIndicesEx(a, b, c, d, e, f) 0

// SCHEME HELPER FUNCTIONS
#define TWO_ARGS(var, arg1, arg2) Scheme_Object* var[2]; \
  var[0] = (arg1); \
  var[1] = (arg2);

#define THREE_ARGS(var, arg1, arg2, arg3) Scheme_Object* var[3]; \
  var[0] = (arg1); \
  var[1] = (arg2); \
  var[2] = (arg3);

#define FOUR_ARGS(var, arg1, arg2, arg3, arg4) Scheme_Object* var[4]; \
  var[0] = (arg1); \
  var[1] = (arg2); \
  var[2] = (arg3); \
  var[3] = (arg4);


extern Scheme_Env* spy_global_env;

#define sapply scheme_apply
#define cons scheme_make_pair
#define seval(str) scheme_eval_string((str), spy_global_env)
#define sym(str) scheme_intern_exact_symbol(str, strlen(str))
//#define slookup(str) scheme_lookup_global(scheme_intern_symbol(str), scheme_get_env(scheme_config))
#define slookup(str) scheme_eval(sym(str), spy_global_env)
//#define slookup(str) seval(str)

PyObject* sapply1(const char* func_name, PyObject* arg);

#define ASSERT_PN(obj) \
  if ( !obj || SCHEME_FALSEP(sapply1("python-node?", obj)) ) \
    { \
    fprintf(stderr, "%s:%d: not a python node\n", __FILE__, __LINE__); \
	exit(1); \
	}



typedef unsigned int    Py_uintptr_t;
typedef int             Py_intptr_t;

#define WITHOUT_COMPLEX

//#define SIZEOF_VOID_P sizeof(void*)
#define SIZEOF_VOID_P 4
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define LONG_BIT (8 * SIZEOF_LONG)


//#define PyMem_Malloc(count) (scheme_malloc_eternal(count))
static __inline__ void* PyMem_Malloc(int count) {
  PyObject* ptr = (PyObject*) scheme_malloc_eternal(count);
  assert(ptr != NULL);
  if (count >= sizeof(PyObject))
     {
     return SPY_INIT_SCHEME_HEADER(ptr);
     }
  else
     {
     return ptr;
     }
}
//#define PyMem_Malloc(count) (scheme_malloc(count))
#define PyMem_MALLOC(count) PyMem_Malloc(count)
#define PyMem_NEW(type, count) ((type *) PyMem_MALLOC(count * sizeof(type)))
// TODO: OPTIMIZE: make this a void macro for speed.
//void PyMem_Free(void* obj);
#define PyMem_REALLOC(p, n)   PyMem_Realloc(p,n)
//  realloc((p), (n) ? (n) : 1)
#define PyObject_MALLOC PyMem_MALLOC



//#include <pyport.h>

// std C stuff
#include <errno.h>
#include <stdlib.h>

/// you will never be FREE!
#define free(ptr) 0

/// stop trying to get around it
#define malloc(size) PyMem_MALLOC(size)

#include <object.h>
PyObject* spy_ext_new_instance(PyTypeObject* type);
// called by PyObject_INIT
PyObject* spy_init_obj(PyObject* obj, PyTypeObject* py_type);
#include <abstract.h>
#include <objimpl.h>
#include <dictobject.h>
#include <listobject.h>
#include <tupleobject.h>
#include <methodobject.h>
#include <moduleobject.h>
#include <pyerrors.h>
#include <modsupport.h>
#include <stringobject.h>
#include <pystate.h>
#include <pythonrun.h>
#include <ceval.h>
// some helpful cpython macros
#include <pyfpe.h>

#include <longobject.h>
#include <floatobject.h>
#include <intobject.h>
#include <boolobject.h>

#include <sliceobject.h>

#include <cobject.h>

#include <classobject.h>

#include <structmember.h>
#include <descrobject.h>

#include <pydebug.h>
#include <weakrefobject.h>

#include <iterobject.h>

#include <cellobject.h>

#include <compile.h> // PyCodeObject
#include <funcobject.h>

#include <fileobject.h>

#define _PyObject_GC_Malloc PyMem_Malloc
#define PyOS_strtol strtol
#define PyOS_strtoul strtoul

#define PyMem_FREE(obj) 0
#define PyObject_Free PyMem_Free
#define PyObject_Del PyMem_Free

#undef PyMem_Free
__inline__ static void PyMem_Free(PyObject* obj) {}

#undef assert
#define assert(b) if (!(b)) {KABOOM_SEGFAULT();}

//#define DEBUG_SPY

#ifdef DEBUG_SPY
 #define PRINTF(fmt, args...) printf(fmt, ##args)
#else
 #define PRINTF(fmt, args...)
#endif

#undef NULL
#define NULL 0

#endif
