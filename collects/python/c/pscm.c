#include "Python.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#define SPY_LIST
#define SPY_STRING
#define SPY_COBJ

#define SPY_NODEP(x) sapply1("python-node?", x)


__inline__ PyObject* smethod0(PyObject* obj, const char* method_name);


#define Scheme_Struct_Type void

typedef struct Scheme_Structure
{
  Scheme_Type scheme_type;
  MZ_HASH_KEY_EX
  Scheme_Struct_Type *stype;
  Scheme_Object *slots[3];
} Scheme_Structure;

#define SCHEME_STRUCT_TYPE(o) (((Scheme_Structure *)o)->stype)

// cache SCHEME_STRUCT and PYTHON-NODE (for spy_init_obj, etc)
Scheme_Type _spy_g_scheme_struct;
Scheme_Object* _spy_g_python_node;

// stuff from pydebug
int Py_VerboseFlag = 0;

//// what are these for??
int _PyTrash_delete_nesting = 0;
PyObject* _PyTrash_delete_later = NULL;
volatile int _Py_Ticker = 0;
int _Py_CheckInterval = 0;
const char * Py_FileSystemDefaultEncoding = NULL;

// these all used to be static in their .c files
extern PyTypeObject PyMethodDescr_Type;
extern PyTypeObject PyClassMethodDescr_Type;
extern PyTypeObject PyMemberDescr_Type;
extern PyTypeObject PyGetSetDescr_Type;
extern PyTypeObject proxytype;
extern PyTypeObject wrappertype;
extern PyTypeObject PyNone_Type;
extern PyTypeObject PyNotImplemented_Type;
extern PyTypeObject PyEllipsis_Type;

// these were not static but not in headers
extern PyTypeObject PyTupleIter_Type;
extern PyTypeObject _PyWeakref_RefType;
extern PyTypeObject PyDictIter_Type;
extern PyTypeObject PyListIter_Type;

typedef struct {
    char *name;
    PyObject **exc;
    PyObject **base;			     /* NULL == PyExc_StandardError */
    char *docstr;
    PyMethodDef *methods;
    int (*classinit)(PyObject *);
} ExceptionTable;

extern ExceptionTable exctable[];


// singletons
PyIntObject _Py_TrueStruct;
PyIntObject _Py_ZeroStruct;

PyTypeObject PyBool_Type;
PyTypeObject PyModule_Type;

PyObject* sapply3(const char* func_name, PyObject* arg1, PyObject* arg2, PyObject* arg3);
PyObject* sapply2(const char* func_name, PyObject* arg1, PyObject* arg2);
PyObject* sapply1(const char* func_name, PyObject* arg);

// Scheme <-> Python
#define DECL_GETSET(name) \
  PyObject* make_py_##name (int argc, Scheme_Object* argv[]); \
  Scheme_Object* get_py_##name (int argc, Scheme_Object* argv[]);

DECL_GETSET(string);
DECL_GETSET(number);
DECL_GETSET(list);
DECL_GETSET(tuple);
DECL_GETSET(dict);
DECL_GETSET(code);
DECL_GETSET(file);

  PyObject* make_py_function (int argc, Scheme_Object* argv[]);
  PyObject* make_py_symbol (int argc, Scheme_Object* argv[]);

PyObject* scheme_argv_to_python_tuple( int argc, Scheme_Object* argv[] );
Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] );
PyObject* spy_ext_call_fn(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_unary(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_binary(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_ternary(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_inquiry(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_coercion(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_intarg(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_intintarg(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_intobjarg(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_intintobjarg(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_objobj(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn_objobjarg(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_varargs(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_noargs(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_onearg(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_kwargs(int argc, Scheme_Object* argv[]);
Scheme_Object* init_spy_ext_method_table(int argc, Scheme_Object* argv[]);
Scheme_Object* spy_ext_object_to_string(int argc, Scheme_Object* argv[]);

//extern PyMethodDef string_methods[];


static const PyMethodDef empty_method_table[] = {
    {NULL}  /* Sentinel */
};

inline void spy_copy_dictionary_from(PyObject* dest, Scheme_Object* src)
{
  PRINTF ("spy_copy_dictionary_from: entry\n");
    sapply2("set-python-node-dict!", dest, sapply1("copy-hash-table", sapply1("python-node-dict", (PyObject*) src)));
}


PyObject* spy_ext_new_class(const char* name)
{
  PRINTF ("spy_ext_new_class: entry\n");
  sapply2("py-call", (PyObject*) seval("py-type%"),
          cons( sapply1("symbol->py-string%", (PyObject*) sym(name)),  /* name */
                cons( sapply1("list->py-tuple%", (PyObject*) cons( seval("py-object%"), scheme_null )), /* parents */
                      cons( (Scheme_Object*) scheme_null, (Scheme_Object*) scheme_null ) ) ) ); /* fields/methods */
}

extern Scheme_Env* spy_global_env;
Scheme_Env* spy_global_env;


PyObject* EMPTY_TUPLE;
PyObject* EMPTY_DICT;


PyObject* spy_ext_new_instance(PyTypeObject* type)
{
  PRINTF ("spy_ext_new_instance: entry\n");
  //return _spy_new_cpython_instance(type, SPY_INIT_OBJ(PyTuple_New(0), &PyTuple_Type));//EMPTY_TUPLE);
  PyObject* obj = PyMem_MALLOC(sizeof(PyObject) + type->tp_basicsize);
  spy_init_obj(obj, type);
  return obj;
//  sapply1("python-new-object", (PyObject*) type);
}


PyObject* _spy_new_cpython_instance(PyTypeObject* type, PyObject* argtup)
{
  PRINTF("_spy_new_cpython_instance: %s\n", type->tp_name);
  if ( type->tp_new )
    {
    PRINTF("_spy_new_cpython_instance: found tp_new\n");
    PyObject* obj = type->tp_new(type, argtup, NULL);
    if ( obj )
      {
      SPY_INIT_OBJ(obj, type);
      }
    return obj;
    }
  else
    {
    PyObject* bases = type->tp_bases;
    int nbases = PyTuple_GET_SIZE(bases);
    int i;
    PRINTF("_spy_new_cpython_instance: did not find tp_new, trying bases\n");
    for ( i = 0; i < nbases; i++ )
      {
      PyObject* obj = _spy_new_cpython_instance((PyTypeObject*) PyTuple_GET_ITEM(bases, i), argtup);
      if ( obj )
        return obj;
      }
    return NULL;
    }
}

PyObject* spy_cpython_instantiate(int argc, Scheme_Object* argv[])
{
  int i;
  PyObject* argtup;
  PyTypeObject* type;
  PRINTF("spy_new_cpython_instance: entry\n");
  type = (PyTypeObject*) argv[0];
  argtup = PyTuple_New(argc - 1);
  for ( i = 1; i < argc; i++ )
    PyTuple_SET_ITEM(argtup, i - 1, argv[i]);
  PyObject* obj = _spy_new_cpython_instance(type, argtup);
  PRINTF("spy_new_cpython_instance: allocated\n");
  return obj;
}

// PyObject -> PyString
PyObject* spy_cpython_pyobj_to_pystr(int argc, Scheme_Object* argv[])
{
  assert(argv[0] != NULL);
  assert(SCHEME_STRUCTP((Scheme_Object*)argv[0]));
  return PyObject_Str(argv[0]);
}

void spy_init_cpython_type(PyTypeObject* tobj)
{
  PRINTF ("spy_init_cpython_type: entry\n");
  SPY_INIT_OBJ((PyObject*) tobj, &PyType_Type);
}


// PyObject PyTypeObject -> bool
Scheme_Object* spy_cpython_instanceof(int argc, Scheme_Object* argv[])
{
  assert(SCHEME_STRUCTP(argv[0]));
  assert(SCHEME_STRUCTP(argv[1]));
  assert(SPY_NODEP(argv[0]));
  assert(SPY_NODEP(argv[1]));
  return PyObject_TypeCheck((PyObject*) argv[0], (PyTypeObject*) argv[1]) ? scheme_true : scheme_false;
}

// PyObject -> bool 
#define DEF_CHECKER(stype, ptype) \
  Scheme_Object* spy_cpython_is##stype (int argc, Scheme_Object* argv[]) \
  { \
    assert(SPY_NODEP(argv[0])); \
    return Py##ptype##_Check((PyObject*) argv[0]) ? scheme_true : scheme_false; \
  }

DEF_CHECKER(number, Number)
DEF_CHECKER(string, String)
DEF_CHECKER(list, List)
DEF_CHECKER(tuple, Tuple)

// PyTypeObject -> string
PyObject* spy_cpython_get_type_name(int argc, Scheme_Object* argv[])
{
  assert(SPY_NODEP(argv[0]));
  assert(PyType_Check(argv[0]));
  return scheme_make_string(PY_TYPE_GET_NAME(argv[0]));
}

// PyObject PyObject -> PyObject
PyObject* spy_cpython_getattr_obj(int argc, Scheme_Object* argv[])
{
  PyObject* attr;
  assert(argv[0] != NULL);
  assert(argv[1] != NULL);
  assert(SPY_NODEP(argv[0]));
  assert(SPY_NODEP(argv[1]));
  assert(PyString_Check(argv[1]));
  attr = PyObject_GetAttr(argv[0], argv[1]);
  assert(attr != NULL);
  assert(SPY_NODEP(attr));
  PRINTF("spy_cpython_getattr_obj: complete\n");
//  assert(0);
  return attr;
}

// PyObject symbol -> PyObject
PyObject* spy_cpython_getattr_sym(int argc, Scheme_Object* argv[])
{
  assert(SPY_NODEP(argv[0]));
  assert(SCHEME_SYMBOLP(argv[1]));
  return PyObject_GetAttrString(argv[0], SCHEME_SYM_VAL(argv[1]));
}

// PyObject string -> PyObject
PyObject* spy_cpython_getattr_str(int argc, Scheme_Object* argv[])
{
  assert(SPY_NODEP(argv[0]));
  assert(SCHEME_STRINGP(argv[1]));
  return PyObject_GetAttrString(argv[0], SCHEME_STR_VAL(argv[1]));
}

// PyObject PyObject PyObject -> PyObject
PyObject* spy_cpython_setattr_obj(int argc, Scheme_Object* argv[])
{
  assert(argv[0] != NULL);
  assert(argv[1] != NULL);
  assert(SPY_NODEP(argv[0]));
  assert(SPY_NODEP(argv[1]));
  assert(SPY_NODEP(argv[2]));
  assert(PyString_Check(argv[1]));
  if ( PyObject_SetAttr(argv[0], argv[1], argv[2]) )
    scheme_signal_error("spy_cpython_setattr_obj failed.");
  return Py_None;
}

// PyObject symbol PyObject -> PyObject
PyObject* spy_cpython_setattr_sym(int argc, Scheme_Object* argv[])
{
  assert(SPY_NODEP(argv[0]));
  assert(SPY_NODEP(argv[2]));
  assert(SCHEME_SYMBOLP(argv[1]));
  if ( PyObject_SetAttrString(argv[0], SCHEME_SYM_VAL(argv[1]), argv[2]) )
    scheme_signal_error("spy_cpython_setattr_sym failed.");
  return Py_None;
}

// PyObject string PyObject -> PyObject
PyObject* spy_cpython_setattr_str(int argc, Scheme_Object* argv[])
{
  assert(SPY_NODEP(argv[0]));
  assert(SPY_NODEP(argv[2]));
  assert(SCHEME_STRINGP(argv[1]));
  if ( PyObject_SetAttrString(argv[0], SCHEME_STR_VAL(argv[1]), argv[2]) )
    scheme_signal_error("spy_cpython_setattr_str failed.");
  return Py_None;
}

void spy_cpython_register_globals()
{
  SPY_INIT_GLOBAL(PyCell_Type);
  SPY_INIT_GLOBAL(PyClass_Type);
  SPY_INIT_GLOBAL(PyInstance_Type);
  SPY_INIT_GLOBAL(PyMethod_Type);
  SPY_INIT_GLOBAL(PyCObject_Type);
  SPY_INIT_GLOBAL(PyMethodDescr_Type);
  SPY_INIT_GLOBAL(PyClassMethodDescr_Type);
  SPY_INIT_GLOBAL(PyMemberDescr_Type);
  SPY_INIT_GLOBAL(PyGetSetDescr_Type);
  SPY_INIT_GLOBAL(PyWrapperDescr_Type);
  SPY_INIT_GLOBAL(proxytype);
  SPY_INIT_GLOBAL(wrappertype);
  SPY_INIT_GLOBAL(PyProperty_Type);
  SPY_INIT_GLOBAL(PyDict_Type);
  SPY_INIT_GLOBAL(PyDictIter_Type);
  SPY_INIT_GLOBAL(PyFile_Type);
  SPY_INIT_GLOBAL(PyFloat_Type);
  SPY_INIT_GLOBAL(PyFunction_Type);
  SPY_INIT_GLOBAL(PyClassMethod_Type);
  SPY_INIT_GLOBAL(PyStaticMethod_Type);
  SPY_INIT_GLOBAL(PyInt_Type);
  SPY_INIT_GLOBAL(PySeqIter_Type);
  SPY_INIT_GLOBAL(PyCallIter_Type);
  SPY_INIT_GLOBAL(PyList_Type);
  SPY_INIT_GLOBAL(PyListIter_Type);
  SPY_INIT_GLOBAL(PyLong_Type);
  SPY_INIT_GLOBAL(PyCFunction_Type);
  SPY_INIT_GLOBAL(PyBool_Type);
  SPY_INIT_GLOBAL(PyModule_Type);
  SPY_INIT_GLOBAL(PyEllipsis_Type);
  SPY_INIT_GLOBAL(PySlice_Type);
  SPY_INIT_GLOBAL(PyCode_Type);
  SPY_INIT_GLOBAL(PyBaseString_Type);
  SPY_INIT_GLOBAL(PyString_Type);
  SPY_INIT_GLOBAL(PyTuple_Type);
  SPY_INIT_GLOBAL(PyTupleIter_Type);
  SPY_INIT_GLOBAL(PyType_Type);
  SPY_INIT_GLOBAL(PyBaseObject_Type);
  SPY_INIT_GLOBAL(PySuper_Type);
}

#define INITTYPE(tobj/*,name*/) do {/*PRINTF("INITTYPE: %s\n", sname );*/ spy_init_cpython_type(&tobj); SPY_INIT_GLOBAL_impl(&tobj, sizeof(PyTypeObject));} while(0)


spy_cpython_init_types()
{
  INITTYPE(PyCell_Type);
  INITTYPE(PyClass_Type);
  INITTYPE(PyInstance_Type);
  INITTYPE(PyMethod_Type);
  INITTYPE(PyCObject_Type);
  INITTYPE(PyMethodDescr_Type);
  INITTYPE(PyClassMethodDescr_Type);
  INITTYPE(PyMemberDescr_Type);
  INITTYPE(PyGetSetDescr_Type);
  INITTYPE(PyWrapperDescr_Type);
  INITTYPE(proxytype);
  INITTYPE(wrappertype);
  INITTYPE(PyProperty_Type);
  INITTYPE(PyDict_Type);
  INITTYPE(PyDictIter_Type);
  INITTYPE(PyFile_Type);
  INITTYPE(PyFloat_Type);
  INITTYPE(PyFunction_Type);
  INITTYPE(PyClassMethod_Type);
  INITTYPE(PyStaticMethod_Type);
  INITTYPE(PyInt_Type);
  INITTYPE(PySeqIter_Type);
  INITTYPE(PyCallIter_Type);
  INITTYPE(PyList_Type);
  INITTYPE(PyListIter_Type);
  INITTYPE(PyLong_Type);
  INITTYPE(PyCFunction_Type);
  INITTYPE(PyBool_Type);
  INITTYPE(PyModule_Type);
  INITTYPE(PyEllipsis_Type);
  INITTYPE(PySlice_Type);
  INITTYPE(PyCode_Type);
  INITTYPE(PyBaseString_Type);
  INITTYPE(PyString_Type);
  INITTYPE(PyTuple_Type);
  INITTYPE(PyTupleIter_Type);
  INITTYPE(PyType_Type);
  INITTYPE(PyBaseObject_Type);
  INITTYPE(PySuper_Type);
  INITTYPE(PyNone_Type);
  INITTYPE(PyNotImplemented_Type);
  INITTYPE(_PyWeakref_RefType);
}

// PyCallable [PyObject ...] -> PyObject
PyObject* spy_cpython_apply(int argc, Scheme_Object* argv[])
{
  PyObject* callable = argv[0];
  PyTypeObject* type;
  assert(SCHEME_STRUCTP((Scheme_Object*)callable));
  type = PY_GET_TYPE(callable);
  argv++; argc--;
  if ( type->tp_call )
    {
    PyObject* ret = type->tp_call(callable, scheme_argv_to_python_tuple(argc, argv), NULL);
    if ( ret == NULL )
      PRINTF("spy_cpython_apply: NULL?!?\n");
    return ret;
    }
  PRINTF("spy_cpython_apply got a non-callable argument!\n");
  return NULL;
}

#define WRAP_BIN_FN(myname, cpy_name) \
  PyObject* spy_cpython_##myname (int argc, Scheme_Object* argv[]) \
  {\
  assert(argc == 2);\
  assert(argv[0]); \
  assert(argv[1]); \
  assert(PyNumber_Check(argv[0]));\
  assert(PyNumber_Check(argv[1]));\
  return cpy_name(argv[0], argv[1]);\
  }

// PyNumber PyNumber -> PyNumber
#define WRAP_BIN_NUM_FN(myname, cpy_num_name) WRAP_BIN_FN(myname, PyNumber_##cpy_num_name)


WRAP_BIN_NUM_FN(add, Add)
WRAP_BIN_NUM_FN(sub, Subtract)
WRAP_BIN_NUM_FN(mul, Multiply)
WRAP_BIN_NUM_FN(div, Divide)


Scheme_Object* spy_cpython_len_scm(int argc, Scheme_Object* argv[])
{
  assert(argv[0]); 
  assert(SPY_NODEP(argv[0]));
  return scheme_make_integer(PyObject_Size(argv[0]));
}

PyObject* spy_cpython_len_py(int argc, Scheme_Object* argv[])
{
  assert(argv[0]); 
  assert(SPY_NODEP(argv[0]));
  return PyInt_FromInt(PyObject_Size(argv[0]));
}


Scheme_Object* scheme_initialize(Scheme_Env* env)
{
  PyObject* m;
  PyObject* dummy;
  int i;

  spy_global_env = env;

  PRINTF("scheme_initialize: entry\n");

  // set up globals
  dummy = sapply2("make-python-node", scheme_false, scheme_false);
  _spy_g_scheme_struct = SPY_SCHEME_TYPE(dummy); // scheme object type: STRUCT
  _spy_g_python_node = SCHEME_STRUCT_TYPE(dummy);  // struct type: PYTHON-NODE

  PRINTF("scheme_initialize: set SPY_GLOBALS_SCHEME_STRUCT and SPY_GLOBALS_PYTHON_NODE\n");

  spy_cpython_register_globals();
  spy_cpython_init_types();

  SPY_INIT_GLOBAL(PyNone_Type);
  SPY_INIT_GLOBAL(*Py_None);



spy_init_PyString();
_Py_ReadyTypes();
  if ( !_PyInt_Init() )
    PRINTF("WARNING: Spy could not initialize the PyInt module.\n");
  else
    {PRINTF("scheme_initialize: initialized the PyInt module\n");}
spy_init_ClassObject();
_PyExc_Init();

  EMPTY_TUPLE = SPY_INIT_OBJ(PyTuple_New(0), &PyTuple_Type);
  EMPTY_DICT = SPY_INIT_OBJ(PyDict_New(), &PyDict_Type);
  scheme_register_extension_global((void*)EMPTY_TUPLE, sizeof(*EMPTY_TUPLE));
  scheme_register_extension_global((void*)EMPTY_DICT, sizeof(*EMPTY_DICT));
  PRINTF("scheme_initialize: empty-tuple is at 0x%x and empty-dict is at 0x%x\n", EMPTY_TUPLE, EMPTY_DICT);
  assert(PyTuple_Check(EMPTY_TUPLE));
  assert(PyDict_Check(EMPTY_DICT));


  /*  register global Spy variables that contain pointers to Scheme values */
//  scheme_register_extension_global((void*) spy_global_env, sizeof(*spy_global_env));
  scheme_register_extension_global((void*) _spy_g_python_node, sizeof(*_spy_g_python_node));


//  pns = (Scheme_Env*) seval("(python-ns)");
  //pns = env;

  return scheme_reload(env);
}

void spy_cpython_add_type_from_stack(PyTypeObject* tobj)
{
  scheme_register_extension_global((void*) tobj, sizeof(PyTypeObject));
  assert(SCHEME_STRUCTP((Scheme_Object*)tobj));
  sapply2("spy-add-cpython-type", tobj->tp_name ? sym(tobj->tp_name) : sym("(null)"), tobj);
}

Scheme_Object* scheme_reload(Scheme_Env* env)
{
  Scheme_Env* pns = env; // python namespace

  #define ADD_TYPE_FROM_STACK(type_val, name) spy_cpython_add_type_from_stack(&type_val)

  ADD_TYPE_FROM_STACK(PyBaseObject_Type, "pobj");
  ADD_TYPE_FROM_STACK(PyType_Type, "ptype");
  ADD_TYPE_FROM_STACK(PyInt_Type, "pint");
  ADD_TYPE_FROM_STACK(PyLong_Type, "plong");
  ADD_TYPE_FROM_STACK(PyFloat_Type, "pfloat");
  ADD_TYPE_FROM_STACK(PyDict_Type, "pdict");
  ADD_TYPE_FROM_STACK(PyString_Type, "pstr");
  ADD_TYPE_FROM_STACK(PyList_Type, "plist");
  ADD_TYPE_FROM_STACK(PyCObject_Type, "pcobj");
  ADD_TYPE_FROM_STACK(PyTuple_Type, "ptuple");
  ADD_TYPE_FROM_STACK(PySlice_Type, "pslice");
  ADD_TYPE_FROM_STACK(PyFunction_Type, "pfunc");
  ADD_TYPE_FROM_STACK(PyCode_Type, "pcode");
  ADD_TYPE_FROM_STACK(PyFile_Type, "pfile");
  ADD_TYPE_FROM_STACK(PyClassMethod_Type, "pclassmethod");
  ADD_TYPE_FROM_STACK(PyStaticMethod_Type, "pstaticmethod");
  ADD_TYPE_FROM_STACK(PySeqIter_Type, "pseqiter");
  ADD_TYPE_FROM_STACK(PyCallIter_Type, "pcalliter");
  ADD_TYPE_FROM_STACK(PyCFunction_Type, "pcfunction");
  ADD_TYPE_FROM_STACK(PyModule_Type, "pmodule");
  ADD_TYPE_FROM_STACK(PySuper_Type, "psuper");
  ADD_TYPE_FROM_STACK(PyProperty_Type, "pproperty");
  ADD_TYPE_FROM_STACK(PyWrapperDescr_Type, "pwrapperdescr");
  ADD_TYPE_FROM_STACK(PyCell_Type, "pcell");
  ADD_TYPE_FROM_STACK(PyBool_Type, "pbool");
  ADD_TYPE_FROM_STACK(PyClass_Type, "pclass");
  ADD_TYPE_FROM_STACK(PyInstance_Type, "pinstance");
  ADD_TYPE_FROM_STACK(PyMethod_Type, "pmethod");
  ADD_TYPE_FROM_STACK(PyMethodDescr_Type, "pmethod_descriptor");
  ADD_TYPE_FROM_STACK(PyClassMethodDescr_Type, "pclassmethod_descriptor");
  ADD_TYPE_FROM_STACK(PyMemberDescr_Type, "pmember_descriptor");
  ADD_TYPE_FROM_STACK(PyGetSetDescr_Type, "pgetset_descriptor");
  ADD_TYPE_FROM_STACK(proxytype, "pdictproxy");
  ADD_TYPE_FROM_STACK(wrappertype, "pmethod-wrapper");
  ADD_TYPE_FROM_STACK(PyNone_Type, "pNoneType");
  ADD_TYPE_FROM_STACK(PyNotImplemented_Type, "pNotImplementedType");
  ADD_TYPE_FROM_STACK(PyEllipsis_Type, "pEllipsisType");
  ADD_TYPE_FROM_STACK(PyTupleIter_Type, "ptupleiterator");
  ADD_TYPE_FROM_STACK(_PyWeakref_RefType, "pweakref");

  
  #define ADDFN_NAMES(cname, sname, min_args, max_args) \
      scheme_add_global( sname , \
                     scheme_make_prim_w_arity( cname , sname, min_args, max_args), \
                     pns );

  #define ADDFN(name, minargs, maxargs) ADDFN_NAMES( spy_cpython_##name , "spy-cpython-" #name "", minargs, maxargs)

  ADDFN(instantiate, 1, 99);
  ADDFN(apply, 1, 99);
  ADDFN(instanceof, 2, 2);
  ADDFN(add, 2, 2);
  ADDFN(sub, 2, 2);
  ADDFN(mul, 2, 2);
  ADDFN(div, 2, 2);

  ADDFN_NAMES(spy_cpython_pyobj_to_pystr, "py-object->py-string", 1, 1);
  ADDFN_NAMES(spy_cpython_get_type_name, "spy-cpython-get-type-name", 1, 1);
  ADDFN_NAMES(spy_cpython_len_scm, "spy-cpython-len/scm", 1, 1);
  ADDFN_NAMES(spy_cpython_len_py, "spy-cpython-len/py", 1, 1);

#define ADD_CHECKER(typename) ADDFN_NAMES(spy_cpython_is##typename , "spy-cpython-"#typename "?", 1, 1)
ADD_CHECKER(number);
ADD_CHECKER(string);
ADD_CHECKER(list);
ADD_CHECKER(tuple);

  scheme_add_global( "scheme-python-dispatch-method",
                     scheme_make_prim_w_arity(scheme_python_dispatch_method, "scheme-python-dispatch-method", 1, 99),
                     pns );

#define ADD_GET(name) \
  scheme_add_global( "get-py-"#name, \
                     scheme_make_prim_w_arity(get_py_##name, "get-py-"#name, 1, 1), \
                     pns );

#define ADD_SET(name) \
  scheme_add_global( "make-py-"#name, \
                     scheme_make_prim_w_arity(make_py_##name, "make-py-"#name, 1, 1), \
                     pns );

#define ADD_GETSET(name) do { ADD_GET(name); ADD_SET(name); } while (0)

ADD_GETSET(string);
ADD_GETSET(number);
ADD_GETSET(list);
ADD_GETSET(tuple);
ADD_GETSET(dict);

  scheme_add_global( "make-py-code",
                     scheme_make_prim_w_arity(make_py_code, "make-py-code", 4, 4),
                     pns );
ADD_GET(code);

ADDFN_NAMES(make_py_file, "make-py-file", 0, 0);
ADD_GET(file);

ADD_SET(symbol);

ADDFN_NAMES(make_py_function, "make-py-function", 1, 2);

ADDFN_NAMES(spy_cpython_getattr_obj, "spy-cpython-getattr/obj", 2, 2);
ADDFN_NAMES(spy_cpython_getattr_sym, "spy-cpython-getattr/sym", 2, 2);
ADDFN_NAMES(spy_cpython_getattr_str, "spy-cpython-getattr/str", 2, 2);
ADDFN_NAMES(spy_cpython_setattr_obj, "spy-cpython-setattr/obj", 3, 3);
ADDFN_NAMES(spy_cpython_setattr_sym, "spy-cpython-setattr/sym", 3, 3);
ADDFN_NAMES(spy_cpython_setattr_str, "spy-cpython-setattr/str", 3, 3);

  scheme_add_global( "spy-ext-call-fn",
                     scheme_make_prim_w_arity(spy_ext_call_fn, "spy-ext-call-fn", 1, -1),
                     pns );

  scheme_add_global( "spy-ext-call-fn-unary",
                     scheme_make_prim_w_arity(spy_ext_call_fn_unary, "spy-ext-call-fn-unary", 2, 2),
                     pns );

  scheme_add_global( "spy-ext-call-fn-binary",
                     scheme_make_prim_w_arity(spy_ext_call_fn_binary, "spy-ext-call-fn-binary", 3, 3),
                     pns );

  scheme_add_global( "spy-ext-call-fn-ternary",
                     scheme_make_prim_w_arity(spy_ext_call_fn_ternary, "spy-ext-call-fn-ternary", 4, 4),
                     pns );

  scheme_add_global( "spy-ext-call-fn-inquiry",
                     scheme_make_prim_w_arity(spy_ext_call_fn_inquiry, "spy-ext-call-fn-inquiry", 2, 2),
                     pns );

  scheme_add_global( "spy-ext-call-fn-coercion",
                     scheme_make_prim_w_arity(spy_ext_call_fn_inquiry, "spy-ext-call-fn-coercion", 3, 3),
                     pns );

#define ADD_CALL_GLOBAL(arity_name, arity_number) \
  scheme_add_global( "spy-ext-call-fn-" #arity_name, \
                     scheme_make_prim_w_arity(spy_ext_call_fn_##arity_name , "spy-ext-call-fn-" #arity_name, arity_number, arity_number), \
                     pns );

ADD_CALL_GLOBAL(intarg, 3);
ADD_CALL_GLOBAL(intintarg, 4);
ADD_CALL_GLOBAL(intobjarg, 4);
ADD_CALL_GLOBAL(intintobjarg, 5);
ADD_CALL_GLOBAL(objobj, 3);
ADD_CALL_GLOBAL(objobjarg, 4);

  scheme_add_global( "spy-ext-call-fn-coercion",
                     scheme_make_prim_w_arity(spy_ext_call_fn_inquiry, "spy-ext-call-fn-coercion", 3, 3),
                     pns );

  scheme_add_global( "spy-ext-call-fn-coercion",
                     scheme_make_prim_w_arity(spy_ext_call_fn_inquiry, "spy-ext-call-fn-coercion", 3, 3),
                     pns );

  scheme_add_global( "spy-ext-call-fn-coercion",
                     scheme_make_prim_w_arity(spy_ext_call_fn_inquiry, "spy-ext-call-fn-coercion", 3, 3),
                     pns );


  scheme_add_global( "spy-ext-call-bound-varargs",
                     scheme_make_prim_w_arity(spy_ext_call_bound_varargs, "spy-ext-call-bound-varargs", 2, -1),
                     pns );

  scheme_add_global( "spy-ext-call-bound-noargs",
                     scheme_make_prim_w_arity(spy_ext_call_bound_noargs, "spy-ext-call-bound-noargs", 2, 2),
                     pns );

  scheme_add_global( "spy-ext-call-bound-onearg",
                     scheme_make_prim_w_arity(spy_ext_call_bound_onearg, "spy-ext-call-bound-noargs", 3, 3),
                     pns );

  scheme_add_global( "spy-ext-call-bound-kwargs",
                     scheme_make_prim_w_arity(spy_ext_call_bound_kwargs, "spy-ext-call-bound-kwargs", 3, -1),
                     pns );

  scheme_add_global( "init-spy-ext-method-table",
                     scheme_make_prim_w_arity(init_spy_ext_method_table, "init-spy-ext-method-table", 3, 3),
                     pns );

  scheme_add_global( "spy-ext-object->string",
                     scheme_make_prim_w_arity(spy_ext_object_to_string, "spy-ext-object->string", 1, 1),
                     pns );

#define ADDOBJ(obj, name) scheme_add_global(name, obj, pns)
ADDOBJ(Py_None, "cpy-none");

  //initspam();
//  PRINTF("SPY now initializing the CPY module\n");
//  m = Py_InitModule("cpy", empty_method_table);

  return scheme_void;
}

Scheme_Object* scheme_module_name() { return scheme_false; }


PyObject* Py_InitModule(const char* name, PyMethodDef methods[])
{
  PyObject* m = sapply1("py-ext-init-module", sym(name));
  int i;
  for ( i = 0; methods[i].ml_meth; i++ )
    {
    PRINTF( "about to add extension method %s\n", methods[i].ml_name );
    sapply2( "python-add-extension-method", m, scheme_make_string(methods[i].ml_name) );
    }
  return m;
}

int PyModule_AddObject(PyObject* module, char* name, PyObject* obj)
{
  sapply3("py-ext-module-add-object", module, sym(name), obj);
  return 0;
}

int spy_is_subtype(PyTypeObject* sub, PyTypeObject* type);

int PyObject_TypeCheck(PyObject* obj, PyTypeObject* type)
{
  PyTypeObject* obtype;
//  PRINTF("PyObject_TypeCheck: entry\n");
  assert(obj != NULL);
  assert(type != NULL);
  assert(obj->ob_type != NULL);
//  assert(PyType_Check(type));
  //PRINTF("PyObject_TypeCheck: checking obj at %x, type at %x, typename at %x\n", obj, type, type->tp_name);
  //PRINTF("PyObject_TypeCheck: looking for %s, got a %s\n", type->tp_name, obj->ob_type->tp_name);
  assert(SCHEME_STRUCTP((Scheme_Object*)obj));
  assert(SCHEME_STRUCTP((Scheme_Object*)type));
  obtype = PY_GET_TYPE(obj);
  //PRINTF("PyObject_TypeCheck: obtype is %s\n", obtype->tp_name);
  if ( obtype == type )
    {
    //PRINTF("PyObject_TypeCheck: success\n");
    return 1;
    }
  else
    {
    return spy_is_subtype(obtype, type);
    }
}

int spy_is_subtype(PyTypeObject* sub, PyTypeObject* type)
{
    PyObject* bases;
    int nbases;
    int i;
    if ( sub == type )
      {
      //PRINTF("PyObject_TypeCheck: success\n");
      return 1;
      }
    bases = sub->tp_bases;
    if ( bases )
      {
      //PRINTF("PyObject_TypeCheck: did not match ob_type, trying ob_type's bases\n");
      assert(PyTuple_Check(bases));
      nbases = PyTuple_GET_SIZE(bases);
      for ( i = 0; i < nbases; i++ )
        {
        PyTypeObject* base = (PyTypeObject*) PyTuple_GET_ITEM(bases, i);
        if ( base == type || spy_is_subtype(base, type) )
          {
          //PRINTF("PyObject_TypeCheck: success\n");
          return 1;
          }
        }
      }
    //PRINTF("PyObject_TypeCheck: failure\n");
    return 0;
}


PyMethodDef *method_def_array = NULL;

// Scheme_Object*[] arr -> PyTupleObject, when arr.length != 1
// Scheme_Object*[] arr -> PyObject,  when arr.length = 1 (NOT ANYMORE, JUST CASE1 FOR NOW)
PyObject* scheme_argv_to_python_tuple( int argc, Scheme_Object* argv[] )
{
  PyObject* tuple;
  int i;
/*
  if ( argc == 1 )
    {
    PRINTF("scheme_argv_to_python_tuple: argc == 1, returning argv[0]\n");
    return argv[0];
    }
  */
  tuple = PyTuple_New(argc);

  PRINTF( "scheme_argv_to_python_tuple: argc: %d\n", argc );
  for ( i = 0; i < argc; i++ )
    PyTuple_SetItem(tuple, i, argv[i]);
  PRINTF( "scheme_argv_to_python_tuple: finished setting up tuple\n" );
  return tuple;
}

// not sure if this is still relevant, will have to see how we load extension methods
Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] )
{
  int i;
  char* method_name = SCHEME_STR_VAL(argv[0]);
  PyCFunction method;

  PRINTF( "scheme_python_dispatch_method: looking for %s\n", method_name );
  PRINTF( "scheme_python_dispatch_method: second argument is %s\n", PyString_AsString(argv[1]));

  for ( i = 0; method_def_array[i].ml_meth; i++ )
    if ( !strcmp( method_def_array[i].ml_name, method_name ) )
      {
      PRINTF( "scheme_python_dispatch_method: found method\n" );
      method = method_def_array[i].ml_meth;
      break;
      }
  PRINTF( "scheme_python_dispatch_method: finished searching\n" );
  argv++; argc--;
  return (*method)( NULL, scheme_argv_to_python_tuple(argc, argv) );
}


PyObject* make_py_number_last_resort(Scheme_Object* n)
{
  PyObject* pynum;
  PRINTF("make_py_number_last_resort: entry\n");
  sapply1("display", scheme_make_string("make_py_number_last_resort: "));
  sapply1("display", n);
  seval("(newline)");
  pynum = PyLong_FromString(SCHEME_STR_VAL((Scheme_Object*)sapply1("number->string", n)), NULL, 10);
  assert(PyNumber_Check(pynum));
  return pynum;
}

#ifdef DEBUG
 PyObject* chk_mpn(PyObject* n)
 {
   PRINTF("chk_mpn: entry\n");
   PRINTF("chk_mpn: n is at %x\n", n);
   return n;
 }
#else
 #define chk_mpn(n) n
#endif

// make-py-number: number -> (U PyInt PyLong PyFloat)
PyObject* make_py_number(int argc, Scheme_Object* argv[])
{
  Scheme_Object* n = argv[0];
  PyObject* pynum;
  assert(SCHEME_NUMBERP(n));
//  PRINTF("make_py_number: entry\n");
//  PRINTF("make_py_number: n is %san int\n", (SCHEME_INTP(n) ? "" : "not"));
  /*return*/
  pynum =
      SCHEME_INTP(n) ? chk_mpn(PyInt_FromLong(SCHEME_INT_VAL(n)))
    : SCHEME_DBLP(n) ? PyFloat_FromDouble(SCHEME_DBL_VAL(n))
    : make_py_number_last_resort(n);
  assert(PyNumber_Check(pynum));
  return pynum;
}

// get-py-number: (U PyInt PyLong PyFloat) -> number
Scheme_Object* get_py_number(int argc, Scheme_Object* argv[])
{
  PyObject* n = argv[0];
  /*PRINTF("get_py_number: entry\n");
  PRINTF("get_py_number: n address: %x\n", n);
  PRINTF("get_py_number: n->ob_type: %x\n", n->ob_type);
  PRINTF("get_py_number: type of n is %s\n", n->ob_type->tp_name);*/
  assert(PyNumber_Check(n));
  return
      PyInt_Check(n) ? scheme_make_integer(PyInt_AsLong(n))
    : PyLong_Check(n) ? scheme_make_integer(PyLong_AsLong(n))
    : PyFloat_Check(n) ? scheme_make_double(PyFloat_AsDouble(n))
    : sapply1("string->number", scheme_make_string(PyString_AsString(PyObject_Str(n))));
}

PyObject* proper_list_to_py_list(Scheme_Object* slist)
{
  int len = scheme_proper_list_length(slist);
  PyObject* plist = PyList_New(len);
  int i = 0;
  while ( slist != scheme_null )
    {
    PyList_SET_ITEM(plist, i, SCHEME_CAR(slist));
    i++;
    slist = SCHEME_CDR(slist);
    }
  return plist;
}

// make-py-list: list -> PyList
PyObject* make_py_list(int argc, Scheme_Object* argv[])
{
  Scheme_Object* slist = argv[0];
  assert(sapply1("list?", slist) != scheme_false);
  return proper_list_to_py_list(slist);
}

Scheme_Object* py_list_to_scheme_list_unsafe(PyObject* plist, int pos, int len);

// get-py-list: PyList -> list
Scheme_Object* get_py_list(int argc, Scheme_Object* argv[])
{
  PyObject* plist = argv[0];
  assert(PyList_Check(plist));
  return py_list_to_scheme_list_unsafe(plist, 0, PyList_GET_SIZE(plist));
}

Scheme_Object* py_list_to_scheme_list_unsafe(PyObject* plist, int pos, int len)
{
  return pos == len ?
          scheme_null :
          scheme_make_pair(PyList_GET_ITEM(plist, pos),
                           py_list_to_scheme_list_unsafe(plist, pos + 1, len));
}

// make-py-tuple: list -> PyTuple
PyObject* make_py_tuple(int argc, Scheme_Object* argv[])
{
  assert(sapply1("list?", argv[0]) != scheme_false);
  return PyList_AsTuple(make_py_list(argc, argv));
}

Scheme_Object* py_tuple_to_scheme_list_unsafe(PyObject* ptup, int pos, int len)
{
  return pos == len ?
          scheme_null :
          scheme_make_pair(PyTuple_GET_ITEM(ptup, pos),
                           py_tuple_to_scheme_list_unsafe(ptup, pos + 1, len));
}

// get-py-tuple: PyTuple -> list
Scheme_Object* get_py_tuple(int argc, Scheme_Object* argv[])
{
  PyObject* ptuple = argv[0];
  PRINTF("get_py_tuple: entry\n");
  assert(PyTuple_Check(ptuple));
  return py_tuple_to_scheme_list_unsafe(ptuple, 0, PyTuple_GET_SIZE(ptuple));
}

// make-py-dict: assoc-list -> PyDict
PyObject* make_py_dict(int argc, Scheme_Object* argv[])
{
  Scheme_Object* al = argv[0];
  int len;
  int i = 0;
  PyObject* dict;
  assert(sapply1("list?", al) != scheme_false);
  len = scheme_proper_list_length(al);
  dict = PyDict_New();
  while ( al != scheme_null )
    {
    Scheme_Object* pair = SCHEME_CAR(al);
    PyObject* key = SCHEME_CAR(pair);
    PyObject* value = SCHEME_CDR(pair);
    //PRINTF("make_py_dict: key is %s\n", PyString_AsString(key));
    //PRINTF("make_py_dict: value is %d\n", PyInt_AsLong(value));
    PyDict_SetItem(dict, key, value);
    i++;
    al = SCHEME_CDR(al);
    }
  return dict;
}

Scheme_Object* py_dict_to_scheme_assoc_list_unsafe_entry(PyObject* dict, PyObject* keys, int pos)
{
  PyObject* key = PyList_GET_ITEM(keys, pos);
  //PRINTF("py_dict_to_scheme_assoc_list_unsafe_entry: pos = %d\n", pos);
  //PRINTF("py_dict_to_scheme_assoc_list_unsafe_entry: key = %s\n", PyString_AsString(key));
  return scheme_make_pair(key, PyDict_GetItem(dict, key));
}

Scheme_Object* py_dict_to_scheme_assoc_list_unsafe(PyObject* dict, PyObject* keys, int pos, int len)
{
  return pos == len ?
          scheme_null :
          scheme_make_pair(py_dict_to_scheme_assoc_list_unsafe_entry(dict, keys, pos),
                           py_dict_to_scheme_assoc_list_unsafe(dict, keys, pos + 1, len));
}

// get-py-dict: PyDict -> assoc-list
Scheme_Object* get_py_dict(int argc, Scheme_Object* argv[])
{
  PyObject* dict = argv[0];
  PyObject* keys;
  //Scheme_Object* lst;
  assert(PyDict_Check(dict));
  keys = PyDict_Keys(dict);
  //PRINTF("get_py_dict: got keys (at address %x)\n", keys);
  assert(PyList_Check(keys));
  //lst = sapply1("get-py-list", keys);
  //PRINTF("get_py_dict: got keys as list\n");
  //sapply1("display", scheme_make_string("get_py_dict: keys = "));
  //sapply1("display", sapply1("get-py-list", keys));
  //seval("(newline)");
  return py_dict_to_scheme_assoc_list_unsafe(dict, keys, 0, PyList_GET_SIZE(keys));
}


// make-py-file: port -> PyFile
PyObject* make_py_file(int argc, Scheme_Object* argv[])
{
 //TODO: implement me
  Scheme_Object* slist = argv[0];
  assert(sapply1("list?", slist) != scheme_false);
  return NULL;
}

// get-py-file: PyFile -> port
Scheme_Object* get_py_file(int argc, Scheme_Object* argv[])
{
// TODO: implement me
  PyObject* pfile = argv[0];
  assert(PyFile_Check(pfile));
  return NULL;
}

// make-py-symbol: symbol -> PyString[interned]
PyObject* make_py_symbol(int argc, Scheme_Object* argv[])
{
  PyObject* str;
  assert(SCHEME_SYMBOLP(argv[0]));
  str = PyString_InternFromString(SCHEME_SYM_VAL(argv[0]));
  assert(str != NULL);
  assert(SPY_NODEP(str));
  assert(PyString_Check(str));
  return str;
}

// make-py-string: string -> PyString
PyObject* make_py_string(int argc, Scheme_Object* argv[])
{
  return PyString_FromString(SCHEME_STR_VAL(argv[0]));
}

// get-py-string: PyString -> string
Scheme_Object* get_py_string(int argc, Scheme_Object* argv[])
{
    return scheme_make_string( ((PyStringObject*) argv[0])->ob_sval );
}

// a CodeFlag is one of:
//   'new-locals
//   'var-args
//   'var-keywords
//   'nested
//   'generator

// make-py-code: procedure number (listof CodeFlag) -> PyCode
PyObject* make_py_code(int argc, Scheme_Object* argv[])
{
  Scheme_Object* name = argv[0];
  Scheme_Object* proc = argv[1];
  Scheme_Object* nargs = argv[2];
  Scheme_Object* flags = argv[3];
  PyCodeObject* code;
  assert(SCHEME_SYMBOLP(name));
  assert(SCHEME_PROCP(proc));
  assert(SCHEME_INTP(nargs));
  assert(sapply1("list?", flags) != scheme_false);
  code = PyObject_NEW(PyCodeObject, &PyCode_Type);
  code->co_argcount = SCHEME_INT_VAL(nargs);
  code->co_nlocals = 0;
  code->co_stacksize = 0;
  code->co_flags = 0;
  while ( flags != scheme_null )
    {
    Scheme_Object* flag = SCHEME_CAR(flags);
    if ( scheme_eq(flag, sym("new-locals")) ) code->co_flags |= CO_NEWLOCALS;
    if ( scheme_eq(flag, sym("var-args")) ) code->co_flags |= CO_VARARGS;
    if ( scheme_eq(flag, sym("var-keywords")) ) code->co_flags |= CO_VARKEYWORDS;
    if ( scheme_eq(flag, sym("nested")) ) code->co_flags |= CO_NESTED;
    if ( scheme_eq(flag, sym("generator")) ) code->co_flags |= CO_GENERATOR;
    flags = SCHEME_CDR(flags);
    }
  code->co_code = NULL;
  code->co_consts = PyTuple_New(0);//EMPTY_TUPLE;
  code->co_names = NULL;
  code->co_varnames = NULL;
  code->co_freevars = NULL;
  code->co_cellvars = NULL;
  code->co_filename = NULL;
  code->co_name = PyString_FromString(SCHEME_SYM_VAL(name));
  code->co_firstlineno = -1;
  code->co_lnotab = NULL;
  PRINTF("make_py_code: checking co_consts\n");
  assert(PyTuple_Check(code->co_consts));
  PRINTF("make_py_code: ok.\n");
  SPY_CODE_SET_LAMBDA(code, proc); // TADA! we'll hide the Scheme lambda here
  return code;
}

// get-py-code: PyCode -> procedure
Scheme_Object* get_py_code(int argc, Scheme_Object* argv[])
{
  PyObject* code = argv[0];
  assert(PyCode_Check(code));
  return SPY_CODE_GET_LAMBDA(code);
}

// make-py-function: PyCode [PyTuple] -> PyFunction
// take a code object and possibly a list of default arg values, wrap them in a PyFunction
PyObject* make_py_function (int argc, Scheme_Object* argv[])
{
  PyObject* code = argv[0];
  PyObject* fn;
  PRINTF("make_py_function: entry\n");
  assert(PyCode_Check(code));
  assert(((PyCodeObject *)code)->co_consts != NULL);
  assert(PyTuple_Check(((PyCodeObject *)code)->co_consts));

  fn = PyFunction_New(code, PyDict_New());//EMPTY_DICT);
  if ( argc == 2 )
    {
    assert(sapply1("list?", argv[1]) != scheme_false);
    PyFunction_SetDefaults(fn, PyList_AsTuple(proper_list_to_py_list(argv[1])));
    }
  return fn;
}

// make-py-type: PyString PyTuple PyDict -> PyType
/*  nevermind, let's just call  type() in Python -- maybe use this later for speed
PyObject* make_py_type(int argc, Scheme_Object* argv[])
{
  PyObject* name = argv[0];
  PyObject* bases = argv[1];
  PyObject* dict = argv[2];
  
  return PyType_Type.tp_new(&PyType_Type, args, NULL);
}
*/


PyObject* spy_init_obj(PyObject* obj, PyTypeObject* py_type)
{
  PRINTF ("spy_init_obj: entry\n");
  assert(obj != NULL);
  assert(py_type != NULL);
  SCHEME_SET_TYPE(obj, SPY_GLOBALS_SCHEME_STRUCT()); // sobj->type; /* STRUCT */
  obj->stype = SPY_GLOBALS_PYTHON_NODE(); //SCHEME_STRUCT_TYPE(sobj); /* PYTHON-NODE */
  PY_SET_TYPE(obj, py_type);
  SPY_SET_MUTABLE(obj, scheme_true);
  SPY_SET_DICT(obj, seval("(make-hash-table)"));
  assert(obj != NULL);
  assert(PyObject_TypeCheck(obj, py_type));
  PRINTF("spy_init_obj: finished\n");
  return obj;
}

///////////////////////// UNFINISHED BUSINESS ///////////////////

PyObject *
PyEval_EvalCodeEx(PyCodeObject *co, PyObject *globals, PyObject *locals,
           PyObject **args, int argcount, PyObject **kws, int kwcount,
           PyObject **defs, int defcount, PyObject *closure)
{

  Scheme_Object** scm_argv = NULL;
  int scm_argc;
#define SETLOCAL(idx, val) (scm_argv[idx] = val)
#define GETLOCAL(idx) scm_argv[idx]
#define INIT_SCM_ARGV(len) \
         if (!scm_argv) \
           { \
           int pltpltpltpltplt; /*erm... daniel.gensym() ? :P */ \
           int mylen = len; \
           scm_argv = PyMem_Malloc(mylen * sizeof(Scheme_Object*)); \
           scm_argc = mylen; \
           for ( pltpltpltpltplt = 0; pltpltpltpltplt < mylen; pltpltpltpltplt++) \
             scm_argv[pltpltpltpltplt] = NULL; \
           }

#if 0
        register PyFrameObject *f;
        register PyObject *retval = NULL;
        register PyObject **fastlocals, **freevars;
        PyThreadState *tstate = PyThreadState_GET();
#endif
        PyObject *x, *u;

#if 0 // mzscheme takes care of it, baybeh
        if (globals == NULL) {
                PyErr_SetString(PyExc_SystemError,
                                "PyEval_EvalCodeEx: NULL globals");
                return NULL;
        }

        assert(globals != NULL);
        f = PyFrame_New(tstate, co, globals, locals);
        if (f == NULL)
                return NULL;

        fastlocals = f->f_localsplus;
        freevars = f->f_localsplus + f->f_nlocals;
#endif
INIT_SCM_ARGV(   (co->co_flags & CO_VARARGS) && (co->co_flags & CO_VARKEYWORDS) ? (co->co_argcount + 2)
               : (co->co_flags & CO_VARARGS) || (co->co_flags & CO_VARKEYWORDS) ? (co->co_argcount + 1)
               : co->co_argcount );

        if (co->co_argcount > 0 ||
            co->co_flags & (CO_VARARGS | CO_VARKEYWORDS)) {
                int i;
                int n = argcount;
                PyObject *kwdict = NULL;
                if (co->co_flags & CO_VARKEYWORDS) {
                        kwdict = PyDict_New();
                        if (kwdict == NULL)
                                goto fail;
                        i = co->co_argcount;
                        if (co->co_flags & CO_VARARGS)
                                i++;
//                       INIT_SCM_ARGV(i + 1); // daniel-code spliced in!
                        SETLOCAL(i, kwdict);
                }
                if (argcount > co->co_argcount) {
                        if (!(co->co_flags & CO_VARARGS)) {
                                PyErr_Format(PyExc_TypeError,
                                    "%.200s() takes %s %d "
                                    "%sargument%s (%d given)",
                                    PyString_AsString(co->co_name),
                                    defcount ? "at most" : "exactly",
                                    co->co_argcount,
                                    kwcount ? "non-keyword " : "",
                                    co->co_argcount == 1 ? "" : "s",
                                    argcount);
                                goto fail;
                        }
                        n = co->co_argcount;
                }
  //            INIT_SCM_ARGV(MAX(co->co_argcount, n)); // daniel-code spliced in!
                for (i = 0; i < n; i++) {
                        x = args[i];
                        Py_INCREF(x);
                        SETLOCAL(i, x);
                }
                if (co->co_flags & CO_VARARGS) {
                        u = PyTuple_New(argcount - n);
                        if (u == NULL)
                                goto fail;
                        SETLOCAL(co->co_argcount, u);
                        for (i = n; i < argcount; i++) {
                                x = args[i];
                                Py_INCREF(x);
                                PyTuple_SET_ITEM(u, i-n, x);
                        }
                }
                for (i = 0; i < kwcount; i++) {
                        PyObject *keyword = kws[2*i];
                        PyObject *value = kws[2*i + 1];
                        int j;
                        if (keyword == NULL || !PyString_Check(keyword)) {
                                PyErr_Format(PyExc_TypeError,
                                    "%.200s() keywords must be strings",
                                    PyString_AsString(co->co_name));
                                goto fail;
                        }
                        /* XXX slow -- speed up using dictionary? */
                        for (j = 0; j < co->co_argcount; j++) {
                                PyObject *nm = PyTuple_GET_ITEM(
                                        co->co_varnames, j);
                                int cmp = PyObject_RichCompareBool(
                                        keyword, nm, Py_EQ);
                                if (cmp > 0)
                                        break;
                                else if (cmp < 0)
                                        goto fail;
                        }
                        /* Check errors from Compare */
                        if (PyErr_Occurred())
                                goto fail;
                        if (j >= co->co_argcount) {
                                if (kwdict == NULL) {
                                        PyErr_Format(PyExc_TypeError,
                                            "%.200s() got an unexpected "
                                            "keyword argument '%.400s'",
                                            PyString_AsString(co->co_name),
                                            PyString_AsString(keyword));
                                        goto fail;
                                }
                                PyDict_SetItem(kwdict, keyword, value);
                        }
                        else {
                                if (GETLOCAL(j) != NULL) {
                                        PyErr_Format(PyExc_TypeError,
                                             "%.200s() got multiple "
                                             "values for keyword "
                                             "argument '%.400s'",
                                             PyString_AsString(co->co_name),
                                             PyString_AsString(keyword));
                                        goto fail;
                                }
                                Py_INCREF(value);
                                SETLOCAL(j, value);
                        }
                }
                if (argcount < co->co_argcount) {
                        int m = co->co_argcount - defcount;
                        for (i = argcount; i < m; i++) {
                                if (GETLOCAL(i) == NULL) {
                                        PyErr_Format(PyExc_TypeError,
                                            "%.200s() takes %s %d "
                                            "%sargument%s (%d given)",
                                            PyString_AsString(co->co_name),
                                            ((co->co_flags & CO_VARARGS) ||
                                             defcount) ? "at least"
                                                       : "exactly",
                                            m, kwcount ? "non-keyword " : "",
                                            m == 1 ? "" : "s", i);
                                        goto fail;
                                }
                        }
                        if (n > m)
                                i = n - m;
                        else
                                i = 0;
                        for (; i < defcount; i++) {
                                if (GETLOCAL(m+i) == NULL) {
                                        PyObject *def = defs[i];
                                        Py_INCREF(def);
                                        SETLOCAL(m+i, def);
                                }
                        }
                }
        }
        else {
                if (argcount > 0 || kwcount > 0) {
                        PyErr_Format(PyExc_TypeError,
                                     "%.200s() takes no arguments (%d given)",
                                     PyString_AsString(co->co_name),
                                     argcount + kwcount);
                        goto fail;
                }
        }

#if 0 // not sure what this does right now -- d
        /* Allocate and initialize storage for cell vars, and copy free
           vars into frame.  This isn't too efficient right now. */
        if (f->f_ncells) {
                int i = 0, j = 0, nargs, found;
                char *cellname, *argname;
                PyObject *c;

                nargs = co->co_argcount;
                if (co->co_flags & CO_VARARGS)
                        nargs++;
                if (co->co_flags & CO_VARKEYWORDS)
                        nargs++;

                /* Check for cells that shadow args */
                for (i = 0; i < f->f_ncells && j < nargs; ++i) {
                        cellname = PyString_AS_STRING(
                                PyTuple_GET_ITEM(co->co_cellvars, i));
                        found = 0;
                        while (j < nargs) {
                                argname = PyString_AS_STRING(
                                        PyTuple_GET_ITEM(co->co_varnames, j));
                                if (strcmp(cellname, argname) == 0) {
                                        c = PyCell_New(GETLOCAL(j));
                                        if (c == NULL)
                                                goto fail;
                                        GETLOCAL(f->f_nlocals + i) = c;
                                        found = 1;
                                        break;
                                }
                                j++;
                        }
                        if (found == 0) {
                                c = PyCell_New(NULL);
                                if (c == NULL)
                                        goto fail;
                                SETLOCAL(f->f_nlocals + i, c);
                        }
                }
                /* Initialize any that are left */
                while (i < f->f_ncells) {
                        c = PyCell_New(NULL);
                        if (c == NULL)
                                goto fail;
                        SETLOCAL(f->f_nlocals + i, c);
                        i++;
                }
        }
#endif
#if 0 // mzscheme should handle this
        if (f->f_nfreevars) {
                int i;
                for (i = 0; i < f->f_nfreevars; ++i) {
                        PyObject *o = PyTuple_GET_ITEM(closure, i);
                        Py_INCREF(o);
                        freevars[f->f_ncells + i] = o;
                }
        }
#endif

#if 0 // yield is evil.
        if (co->co_flags & CO_GENERATOR) {
                /* Don't need to keep the reference to f_back, it will be set
                 * when the generator is resumed. */
                Py_XDECREF(f->f_back);
                f->f_back = NULL;

                PCALL(PCALL_GENERATOR);

                /* Create a new generator that owns the ready to run frame
                 * and return that as the value. */
                return gen_new(f);
        }
#endif

#if 0 // No.
        retval = eval_frame(f);

  fail: /* Jump here from prelude on failure */

        /* decref'ing the frame can cause __del__ methods to get invoked,
           which can call back into Python.  While we're done with the
           current Python frame (f), the associated C stack is still in use,
           so recursion_depth must be boosted for the duration.
        */
        assert(tstate != NULL);
        ++tstate->recursion_depth;
        Py_DECREF(f);
        --tstate->recursion_depth;
        return retval;
#endif

  goto succeed;
  fail:
    return NULL; // kaboom?
  succeed:
    return scheme_apply(SPY_CODE_GET_LAMBDA(co), scm_argc, scm_argv);
}

void PyObject_GC_Track(void *ptr)
{
  // that's great
}


/*taken from cpython*/

/* _Py_Mangle is defined in compile.c */
int _Py_Mangle(char *p, char *name, char *buffer, size_t maxlen)
{
        /* Name mangling: __private becomes _classname__private.
           This is independent from how the name is used. */
        size_t nlen, plen;
        if (p == NULL || name == NULL || name[0] != '_' || name[1] != '_')
                return 0;
        nlen = strlen(name);
        if (nlen+2 >= maxlen)
                return 0; /* Don't mangle __extremely_long_names */
        if (name[nlen-1] == '_' && name[nlen-2] == '_')
                return 0; /* Don't mangle __whatever__ */
        /* Strip leading underscores from class name */
        while (*p == '_')
                p++;
        if (*p == '\0')
                return 0; /* Don't mangle if class is just underscores */
        plen = strlen(p);
        if (plen + nlen >= maxlen)
                plen = maxlen-nlen-2; /* Truncate class name if too long */
        /* buffer = "_" + p[:plen] + name # i.e. 1+plen+nlen bytes */
        buffer[0] = '_';
        strncpy(buffer+1, p, plen);
        strcpy(buffer+1+plen, name);
        return 1;
}



PyObject * PyErr_SetFromErrnoWithFilename(PyObject *o, char *str)
{
  // TODO: implement me
  return NULL;
}


PyObject* PyErr_NewException(char *name, PyObject *base, PyObject *dict)
{
//  THREE_ARGS(args, scheme_make_string(name), base, dict);
//  return sapply( slookup("python-new-exception"), 3, args );
        char *dot;
        PyObject *modulename = NULL;
        PyObject *classname = NULL;
        PyObject *mydict = NULL;
        PyObject *bases = NULL;
        PyObject *result = NULL;
        dot = strrchr(name, '.');
        if (dot == NULL) {
                PyErr_SetString(PyExc_SystemError,
                        "PyErr_NewException: name must be module.class");
                return NULL;
        }
        if (base == NULL)
                base = PyExc_Exception;
        if (!PyClass_Check(base)) {
                /* Must be using string-based standard exceptions (-X) */
                return PyString_FromString(name);
        }
        if (dict == NULL) {
                dict = mydict = PyDict_New();
                if (dict == NULL)
                        goto failure;
        }
        if (PyDict_GetItemString(dict, "__module__") == NULL) {
                modulename = PyString_FromStringAndSize(name, (int)(dot-name));
                if (modulename == NULL)
                        goto failure;
                if (PyDict_SetItemString(dict, "__module__", modulename) != 0)
                        goto failure;
        }
        classname = PyString_FromString(dot+1);
        if (classname == NULL)
                goto failure;
        bases = Py_BuildValue("(O)", base);
        if (bases == NULL)
                goto failure;
        result = PyClass_New(bases, dict, classname);
  failure:
        Py_XDECREF(bases);
        Py_XDECREF(mydict);
        Py_XDECREF(classname);
        Py_XDECREF(modulename);
        return result;

}

PyObject* PyErr_SetFromErrno(PyObject* err)
{
  // TODO: fixme
  return err;
}

void PyErr_WriteUnraisable(PyObject *err)
{
  // TODO: implement me
}


void
Py_FatalError (const char * message)
{
  scheme_signal_error (message);
}

void PyErr_Restore(PyObject *type, PyObject *value, PyObject *traceback)
{
// TODO: implement me
}

void PyErr_Fetch(PyObject **ptype, PyObject **pvalue, PyObject **ptraceback)
{
 // TODO: implement me
}


PyObject * _PyObject_New(PyTypeObject *type)
{
  PRINTF ("_PyObject_New: %s\n", type->tp_name);
//  return sapply3("make-python-node", type, seval("(make-hash-table)"), scheme_true);
  PyObject* op = (PyObject*) _PyObject_GC_Malloc(_PyObject_SIZE(type) + sizeof(PyObject));
  PyObject_INIT(op, type);
  return op;
//  return sapply1("python-new-object", type);
}

PyVarObject *
_PyObject_NewVar(PyTypeObject *tp, int nitems)
{
        const size_t size = _PyObject_VAR_SIZE(tp, nitems);
        PyVarObject *op = (PyVarObject *) _PyObject_GC_Malloc(size + sizeof(PyObject));
        PRINTF("_PyObject_NewVar: size = %d, new var is at 0x%x\n", size, op);
        /*if (op != NULL)
                op =*/ PyObject_INIT_VAR(op, tp, nitems);
        return op;

//  return PyObject_InitVar(_PyObject_New(tp), tp, nitems);
}

PyVarObject *
_PyObject_GC_NewVar(PyTypeObject *tp, int nitems)
{
  // gc is assumed
  return _PyObject_NewVar(tp, nitems);
}

PyVarObject * _PyObject_GC_Resize(PyVarObject *obj, int size)
{
  PyTypeObject* type = PY_GET_TYPE(obj);
  return (PyVarObject*) SPY_INIT_OBJ( (PyObject*) PyMem_Realloc(obj, size), type);
}

PyObject * PyImport_Import(PyObject *name)
{
  // TODO: implement me
  return NULL;
}

PyObject * PyImport_ImportModuleEx(char *name, PyObject *globals, PyObject *locals, PyObject *fromlist)
{
  // TODO: implement me
  return NULL;
}

PyObject * PyImport_ImportModule(char *name)
{
  // TODO: implement me
  return NULL;
//  return sapply1("python-import-path", scheme_make_string(module_name));
}

PyObject * PyEval_CallMethod(PyObject *obj, char *methodname, char *format, ...)
{
  // TODO: implement me
  return NULL;
}


PyObject *
PyErr_Format(PyObject *exception, const char *format, ...)
{
        va_list vargs;
        PyObject* string;

#ifdef HAVE_STDARG_PROTOTYPES
        va_start(vargs, format);
#else
        va_start(vargs);
#endif

        string = PyString_FromFormatV(format, vargs);
        PyErr_SetObject(exception, string);
        Py_XDECREF(string);
        va_end(vargs);
        return NULL;
}


void PyErr_SetNone(PyObject *o)
{
  // TODO: implement me
}

int PyErr_BadArgument(void)
{
  // TODO: fixme
  PyErr_SetString(NULL, "bad argument");
return 0;
}

PyObject * PyErr_NoMemory(void)
{
  // TODO: fixme
 return NULL;
}

void PyErr_SetString(PyObject * o, const char * c)
{

  const char* name = (o && o->ob_type && o->ob_type->tp_name) ?
                      o->ob_type->tp_name :
                      "(null)";
  char* errmsg = c ?
                  c :
                  "(null)";
//  PRINTF("PyErr_SetString: %s\n", errmsg);
//  scheme_signal_error("%s\n", errmsg);
  PRINTF("PyErr_SetString: %s: %s\n", name, errmsg);
  scheme_signal_error("%s: %s\n", name, errmsg);
}

void PyErr_SetObject(PyObject *cls, PyObject *obj)
{
  PyErr_SetString(cls, PyString_AsString(PyObject_Str(obj)));
}


/* External interface to call any callable object.
   The arg must be a tuple or NULL. */

#undef PyEval_CallObject
/* for backward compatibility: export this interface */

PyObject *
PyEval_CallObject(PyObject *func, PyObject *arg)
{
        return PyEval_CallObjectWithKeywords(func, arg, (PyObject *)NULL);
}
#define PyEval_CallObject(func,arg) \
        PyEval_CallObjectWithKeywords(func, arg, (PyObject *)NULL)

PyObject *
PyEval_CallObjectWithKeywords(PyObject *func, PyObject *arg, PyObject *kw)
{
        PyObject *result;

        if (arg == NULL)
                arg = PyTuple_New(0);
        else if (!PyTuple_Check(arg)) {
                PyErr_SetString(PyExc_TypeError,
                                "argument list must be a tuple");
                return NULL;
        }
        else
                Py_INCREF(arg);

        if (kw != NULL && !PyDict_Check(kw)) {
                PyErr_SetString(PyExc_TypeError,
                                "keyword list must be a dictionary");
                Py_DECREF(arg);
                return NULL;
        }

        result = PyObject_Call(func, arg, kw);
        Py_DECREF(arg);
        return result;
}

int PyEval_GetRestricted(void)
{
  return 0;  // ?????
}

PyObject* _PyObject_GC_New(PyTypeObject* type)
{
  return spy_ext_new_instance(type);
  //return sapply1("python-new-object", type);
}


void PyObject_GC_UnTrack(void *obj)
{
}



void _PyErr_BadInternalCall(char *filename, int lineno)
{
   // TODO: fixme
    fprintf(stderr, "Spy: bad internal call in file %s at line %d\n", filename, lineno);
    exit(1);
}


// did an error occur?
PyObject * PyErr_Occurred(void)
{
  // TODO: implement me
//  fprintf(stderr, "Spy: a py-err has occurred.\n");
//  exit(1);
  return 0;
}



PyObject * PyObject_Init(PyObject *obj, PyTypeObject *type)
{
  return spy_init_obj(obj, type);
}

PyVarObject *
PyObject_InitVar(PyVarObject *op, PyTypeObject *tp, int size)
{
  op->ob_size = size;
  return spy_init_obj(op, tp);
}

/*
int PyTuple_GetSize(PyObject *o)
{
 return PySequence_Size(o);
}
*/


// fixme...
int PyErr_ExceptionMatches(PyObject *obj)
{
  // TODO: implement me
 return 0;
}

// fixme...
int _PyEval_SliceIndex(PyObject *o, int *i)
{
  // TODO: implement me
 return 0;
}


void PyErr_Clear(void)
{
  // TODO: implement me
}


PyObject* generic_repr(PyObject* obj)
{
 return smethod0(obj, "__repr__");
}

PyObject* py_number_to_octal_py_string(PyObject* num)
{
 return KABOOM();
}

PyObject* py_number_to_hex_py_string(PyObject* num)
{
 return KABOOM();
}

int PyErr_Warn(PyObject *o, char *str)
{
  // TODO: implement me
 return 1;
}

void _PyTrash_destroy_chain(void)
{
}

void _PyTrash_deposit_object(PyObject* obj)
{
}

// TODO: implement me
PyObject * PyEval_GetLocals(void)
{
  return NULL;
}

// TODO: implement me
PyObject * PyEval_GetGlobals(void)
{
  return NULL;
}



int Py_GetRecursionLimit(void)
{
  return 99999; // uhh yeah
}

char * PyEval_GetFuncDesc(PyObject *fn)
{
  return "foo! there is no func desc because I don't support it.";
}

char * PyEval_GetFuncName(PyObject *fn)
{
  return "foo! there is no func name because I don't support it.";
}

void PyObject_GC_Del(void *obj) {}


int PyErr_CheckSignals() 
{
  // TODO: implement me
  return 0;
}

#ifndef MIN
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#endif

void* PyMem_Realloc(void* o, size_t new_size)
{
  int amt_to_copy;
  void *new_buffer;
  PRINTF("PyMem_Realloc\n");
  if ( new_size == 0 )
    {
    //PRINTF("PyMem_Realloc: size = 0, returning free()\n");
    //return NULL; /* no free() in a GC environment */
    PRINTF("PyMem_Realloc: size = 0, changing it to 1\n");
    new_size = 1;
    }
  if ( !o )
   {
   PRINTF("PyMem_Realloc: orig is null, returning malloc(%d)\n", new_size);
   return PyMem_MALLOC(new_size);
   }
  amt_to_copy = new_size; // how can I get the old size? should be min(new_size, old_size)
  PRINTF("PyMem_Realloc: allocating %d bytes\n", new_size);
//  new_buffer = PyMem_Malloc(new_size ? new_size : sizeof(PyVarObject));
  new_buffer = PyMem_Malloc(new_size);
  PRINTF("PyMem_Realloc: allocated new object\n");
  PRINTF("PyMem_Realloc: copying %d bytes\n", amt_to_copy);
  if ( amt_to_copy )
    memcpy(new_buffer, o, amt_to_copy);
  return new_buffer;
}

void PySys_WriteStderr(char* format, ...)
{
  // TODO: implement me
  fprintf(stderr, "PYSYS_WRITESTDERR: IMPLEMENT ME!\n");
}


PyObject * PyEval_GetBuiltins()
{
  // TODO: implement me
  return PyDict_New();
}




/////////////////// SHORTCUTS TO SCHEME_APPLY ////////////////////


__inline__ PyObject* smethod0(PyObject* obj, const char* method_name)
{
  TWO_ARGS(args, obj, sym(method_name));
  return sapply( seval("python-method-call"), 2, args );
}

__inline__ PyObject* smethod1(PyObject* obj, const char* method_name, PyObject* arg)
{
  THREE_ARGS(args, obj, sym(method_name), scheme_make_pair(arg, scheme_null));
  return sapply( slookup("python-method-call"), 3, args );
}

__inline__ PyObject* smethod2(PyObject* obj, const char* method_name, PyObject* arg1, PyObject* arg2)
{
  THREE_ARGS(args, obj, sym(method_name), cons(arg1, cons(arg2, scheme_null)));
  return sapply( slookup("python-method-call"), 3, args );
}

PyObject* sapply3(const char* func_name, PyObject* arg1, PyObject* arg2, PyObject* arg3)
{
  Scheme_Object* args[3] = { arg1, arg2, arg3 };
  return sapply( slookup(func_name), 3, args );
}


PyObject* sapply2(const char* func_name, PyObject* arg1, PyObject* arg2)
{
  Scheme_Object* args[2] = { arg1, arg2 };
  return sapply( slookup(func_name), 2, args );
}

PyObject* sapply1(const char* func_name, PyObject* arg)
{
  Scheme_Object* args[1] = { arg };
  Scheme_Object* func = slookup(func_name);
//  PRINTF( "about to apply %s with one argument\n", func_name );
  return sapply( func, 1, args );
}




//////////////////////////////////// EXTENSION FUNCTION CALL WRAPPERS AND DISPATCHERS ////////////
//////////// NOT SURE IF WE STILL NEED THESE ///////////////

PyObject* spy_ext_call_fn_unary(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PRINTF("spy_ext_call_fn_unary: calling\n");
  return (*((unaryfunc) SCHEME_CPTR_VAL(fn)))(argv[1]);
}

PyObject* spy_ext_call_fn_binary(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PRINTF("spy_ext_call_fn_binary: calling\n");
  return (*((binaryfunc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2]);
}

PyObject* spy_ext_call_fn_ternary(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PRINTF("spy_ext_call_fn_ternary: calling\n");
  return (*((ternaryfunc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2], argv[3]);
}

PyObject* spy_ext_call_fn_coercion(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* rhs = argv[1];
  PyObject* lhs = argv[2];
  int ret;
  PRINTF("spy_ext_call_fn_coercion: calling\n");
  ret = (*((coercion) SCHEME_CPTR_VAL(fn)))(&rhs, &lhs);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_inquiry(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  PRINTF("spy_ext_call_fn_inquiry: calling\n");
  ret = (*((inquiry) SCHEME_CPTR_VAL(fn)))(argv[1]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_intarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PRINTF("spy_ext_call_fn_intarg: calling\n");
  return (*((intargfunc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]));
}

PyObject* spy_ext_call_fn_intintarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PRINTF("spy_ext_call_fn_intintarg: calling\n");
  return (*((intintargfunc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]), PyInt_AsInt(argv[3]));
}

PyObject* spy_ext_call_fn_intobjarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  PRINTF("spy_ext_call_fn_intobjarg: calling\n");
  ret = (*((intobjargproc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]), argv[3]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_intintobjarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  PRINTF("spy_ext_call_fn_intintobjarg: calling\n");
  ret = (*((intintobjargproc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]), PyInt_AsInt(argv[3]), argv[4]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_objobj(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  PRINTF("spy_ext_call_fn_objobj: calling\n");
  ret = (*((objobjproc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_objobjarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  PRINTF("spy_ext_call_fn_objobjarg: calling\n");
  ret = (*((objobjargproc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2], argv[3]);
  return PyInt_FromInt(ret);
}


PyObject* spy_ext_call_fn(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  argv++;
  PRINTF("spy_ext_call_fn: calling Spy extension function\n");
  return (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(NULL, scheme_argv_to_python_tuple(argc - 1, argv));
}

PyObject* spy_ext_call_bound_varargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  argv++;
  argv++;
  ASSERT_PN(self);
  PRINTF("spy_ext_call_bound_varargs\n");
  PyObject* ret = (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(self, scheme_argv_to_python_tuple(argc - 2, argv));
  ASSERT_PN(ret);
  PRINTF("spy_ext_call_bound_varargs: returning\n");
  return ret;
}

PyObject* spy_ext_call_bound_noargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  ASSERT_PN(self);
  PRINTF("spy_ext_call_bound_noargs\n");
  PyObject* ret = (*((PyNoArgsFunction) SCHEME_CPTR_VAL(fn)))(self);
  ASSERT_PN(ret);
  PRINTF("spy_ext_call_bound_noargs: returning\n");
  return ret;
}

PyObject* spy_ext_call_bound_onearg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  PyObject* arg = argv[2];
  ASSERT_PN(self);
  ASSERT_PN(arg);
  PRINTF("spy_ext_call_bound_onearg\n");
  PyObject* ret = (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(self, arg);
  ASSERT_PN(ret);
  PRINTF("spy_ext_call_bound_onearg: returning\n");
  return ret;
}

PyObject* spy_ext_call_bound_kwargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  PyObject* kwargs = argv[2];
  PyObject* tuple;
  argv++;
  argv++;
  argv++;
  ASSERT_PN(self);
  PRINTF("spy_ext_call_bound_kwargs\n");
  tuple = scheme_argv_to_python_tuple(argc - 3, argv);
  PRINTF("spy_ext_call_bound_kwargs: converted scheme argv to pytuple\n");
  // TODO: implement keyword arguments here
  PyObject* ret = (*((PyCFunctionWithKeywords) SCHEME_CPTR_VAL(fn)))(self, tuple, kwargs);
  ASSERT_PN(ret);
  PRINTF("spy_ext_call_bound_kwargs: returning\n");
  return ret;
}


PyObject* wrap_ext_function(PyCFunction f, const char* name)
{
  return sapply2("python-wrap-ext-function", scheme_make_cptr((void*) f, "python-ext-function"), sym(name));
}

static PyObject* wrap_ext_method_varargs(PyCFunction m, const char* name)
{
  PRINTF("wrap_ext_method_varargs: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-varargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_method_varargs: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_noargs(PyNoArgsFunction m, const char* name)
{
  PRINTF("wrap_ext_method_noargs: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-noargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_method_noargs: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_onearg(PyCFunction m, const char* name)
{
  PRINTF("wrap_ext_method_onearg: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-onearg", scheme_make_cptr((void*) m, "python-ext-method-onearg"), sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_method_onearg done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_kwargs(PyCFunctionWithKeywords m, const char* name)
{
  PRINTF("wrap_ext_method_kwargs: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-kwargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  //ASSERT_PN(wrapped);
  PRINTF("wrap_ext_method_kwargs: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_unary(unaryfunc f, const char* name)
{
  PRINTF("wrap_ext_function_unary: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-unary",
                              scheme_make_cptr((void*) f, "python-ext-function-unary"),
                              sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_function_unary: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_binary(binaryfunc f, const char* name)
{
  PRINTF("wrap_ext_function_binary: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-binary",
                              scheme_make_cptr((void*) f, "python-ext-function-binary"),
                              sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_function_binary: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_ternary(ternaryfunc f, const char* name)
{
  PRINTF("wrap_ext_function_ternary: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-ternary",
                              scheme_make_cptr((void*) f, "python-ext-function-ternary"),
                              sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_function_ternary: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_inquiry(inquiry f, const char* name)
{
  PRINTF("wrap_ext_function_inquiry: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-inquiry",
                              scheme_make_cptr((void*) f, "python-ext-function-inquiry"),
                              sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_function_inquiry: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_coercion(coercion f, const char* name)
{
  PRINTF("wrap_ext_function_coercion: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-coercion",
                              scheme_make_cptr((void*) f, "python-ext-function-coercion"),
                              sym(name));
  ASSERT_PN(wrapped);
  PRINTF("wrap_ext_function_coercion: done with: %s\n", name);
  return wrapped;
}


static void addmethod_wrapped(PyTypeObject* type, char* meth_name, PyObject* wrapped)
{
  PRINTF("addmethod_wrapped: setting attribute: %s\n", meth_name);
  sapply3("python-set-attribute!", type, sym(meth_name), wrapped);
  ASSERT_PN(type);
  //ASSERT_PN(scheme_hash_get((Scheme_Hash_Table*) sapply1("python-node-dict", type), sym(meth_name)));
  PRINTF("addmethod_wrapped: set attribute: %s\n", meth_name);
}

static void addmethod(PyTypeObject* type, char* meth_name, PyCFunction method, int flags, char* doc)
{
  PyObject* wrapped;
  if ( flags & METH_NOARGS )
    wrapped = wrap_ext_method_noargs((PyNoArgsFunction)method, meth_name);
  else if ( flags & METH_KEYWORDS )
    wrapped = wrap_ext_method_kwargs((PyCFunctionWithKeywords)method, meth_name);
  else if ( flags & METH_O )
    wrapped = wrap_ext_method_onearg(method, meth_name);
  else
    wrapped = wrap_ext_method_varargs(method, meth_name);
  addmethod_wrapped(type, meth_name, wrapped);
}

typedef struct name_inquiry_fn_pair
  {
  const char* name;
  inquiry f;
  } name_inquiry_fn_pair;

static void addmethod_inquiry_many(PyTypeObject* type, const name_inquiry_fn_pair pairs[])
{
  int i;
  for ( i = 0; pairs[i].name; i++ )
    addmethod_wrapped(type, pairs[i].name, wrap_ext_function_inquiry(pairs[i].f, pairs[i].name));
}

typedef struct name_unary_fn_pair
  {
  const char* name;
  unaryfunc f;
  } name_unary_fn_pair;

static void addmethod_unary_many(PyTypeObject* type, const name_unary_fn_pair pairs[])
{
  int i;
  for ( i = 0; pairs[i].name; i++ )
    addmethod_wrapped(type, pairs[i].name, wrap_ext_function_unary(pairs[i].f, pairs[i].name));
}


typedef struct name_binary_fn_pair
  {
  const char* name;
  binaryfunc f;
  } name_binary_fn_pair;

static void addmethod_binary_many(PyTypeObject* type, const name_binary_fn_pair pairs[])
{
  int i;
  for ( i = 0; pairs[i].name; i++ )
    addmethod_wrapped(type, pairs[i].name, wrap_ext_function_binary(pairs[i].f, pairs[i].name));
}

typedef struct name_ternary_fn_pair
  {
  const char* name;
  ternaryfunc f;
  } name_ternary_fn_pair;

static void addmethod_ternary_many(PyTypeObject* type, const name_ternary_fn_pair pairs[])
{
  int i;
  for ( i = 0; pairs[i].name; i++ )
    addmethod_wrapped(type, pairs[i].name, wrap_ext_function_ternary(pairs[i].f, pairs[i].name));
}

typedef struct name_coercion_fn_pair
  {
  const char* name;
  coercion f;
  } name_coercion_fn_pair;

static void addmethod_coercion_many(PyTypeObject* type, const name_coercion_fn_pair pairs[])
{
  int i;
  for ( i = 0; pairs[i].name; i++ )
    addmethod_wrapped(type, pairs[i].name, wrap_ext_function_coercion(pairs[i].f, pairs[i].name));
}

#define DEF_ADDMETHOD(arity_name, fn_type) \
  static PyObject* wrap_ext_function_##arity_name (coercion f, const char* name) \
  { \
    PRINTF("wrap_ext_function_" #arity_name ": name: %s\n", name); \
    PyObject* wrapped = sapply2("python-wrap-ext-function-" #arity_name, \
                                scheme_make_cptr((void*) f, "python-ext-function-" #arity_name), \
                                sym(name)); \
    ASSERT_PN(wrapped); \
    PRINTF("wrap_ext_function_" #arity_name ": done with: %s\n", name); \
    return wrapped; \
  } \
  typedef struct name_##arity_name##_fn_pair \
    { \
    const char* name; \
    fn_type f; \
    } name_##arity_name##_fn_pair; \
  static void addmethod_##arity_name##_many(PyTypeObject* type, const name_##arity_name##_fn_pair pairs[]) \
  { \
    int i; \
    for ( i = 0; pairs[i].name; i++ ) \
      addmethod_wrapped(type, pairs[i].name, wrap_ext_function_##arity_name (pairs[i].f, pairs[i].name)); \
  }

DEF_ADDMETHOD(intarg, intargfunc)
DEF_ADDMETHOD(intintarg, intintargfunc)
DEF_ADDMETHOD(intobjarg, intobjargproc)
DEF_ADDMETHOD(intintobjarg, intintobjargproc)
DEF_ADDMETHOD(objobj, objobjproc)
DEF_ADDMETHOD(objobjarg, objobjargproc)

// init-spy-ext-method-table: py-module% symbol py-type% -> void
Scheme_Object* init_spy_ext_method_table(int argc, Scheme_Object* argv[])
{
 PyObject* module = argv[0];
  Scheme_Object* type_name = argv[1];
  PyTypeObject* type = (PyTypeObject*) argv[2];
  struct PyMethodDef *methods = PY_TYPE_METHODS(type);
  int i;

  ASSERT_PN(module);
  ASSERT_PN(type);


  PRINTF("INIT SPY EXT METHOD TABLE\n");

  PRINTF("adding methods for type %s\n", type->tp_name);

  if ( type->tp_new )
    addmethod(type, "__new__", type->tp_new, METH_KEYWORDS, "");

  if ( type->tp_str )
    addmethod(type, "__str__", type->tp_str, METH_NOARGS, "");

  if ( type->tp_repr )
    addmethod(type, "__repr__", type->tp_repr, METH_NOARGS, "");

  if ( !methods )
    PRINTF("Type method table is null.\n");
  else
    for ( i = 0; methods[i].ml_name; i++ )
      {
      PRINTF("adding method: %s\n", methods[i].ml_name);
      addmethod( type, methods[i].ml_name, methods[i].ml_meth, methods[i].ml_flags, methods[i].ml_doc );
      PRINTF("added method: %s\n", methods[i].ml_name);
      }

  if ( type->tp_as_number )
    {
    PyNumberMethods* num_meths = type->tp_as_number;
    const name_unary_fn_pair upairs[] = {{"__neg__", num_meths->nb_negative},
                                         {"__pos__", num_meths->nb_positive},
                                         {"__abs__", num_meths->nb_absolute},
                                         {"__invert__", num_meths->nb_invert},
                                         {"__int__", num_meths->nb_int},
                                         {"__long__", num_meths->nb_long},
                                         {"__float__", num_meths->nb_float},
                                         {"__oct__", num_meths->nb_oct},
                                         {"__hex__", num_meths->nb_hex},
                                         {0,0}};
    const name_binary_fn_pair bpairs[] = {{"__add__", num_meths->nb_add},
                                          {"__sub__", num_meths->nb_subtract},
                                          {"__mul__", num_meths->nb_multiply},
                                          {"__div__", num_meths->nb_divide},
                                          {"__mod__", num_meths->nb_remainder},
                                          {"__floordiv__", num_meths->nb_floor_divide},
                                          {"__truediv__", num_meths->nb_true_divide},
                                          {"__divmod__", num_meths->nb_divmod},
                                          {"__lshift__", num_meths->nb_lshift},
                                          {"__rshift__", num_meths->nb_rshift},
                                          {"__and__", num_meths->nb_and},
                                          {"__xor__", num_meths->nb_xor},
                                          {"__or__", num_meths->nb_or},
                                          {"__iadd__", num_meths->nb_inplace_add},
                                          {"__isub__", num_meths->nb_inplace_subtract},
                                          {"__imul__", num_meths->nb_inplace_multiply},
                                          {"__idiv__", num_meths->nb_inplace_divide},
                                          {"__imod__", num_meths->nb_inplace_remainder},
                                          {"__ilshift__", num_meths->nb_inplace_lshift},
                                          {"__irshift__", num_meths->nb_inplace_rshift},
                                          {"__iand__", num_meths->nb_inplace_and},
                                          {"__ixor__", num_meths->nb_inplace_xor},
                                          {"__ior__", num_meths->nb_inplace_or},
                                          {"__ifloordiv__", num_meths->nb_inplace_floor_divide},
                                          {"__itruediv__", num_meths->nb_inplace_true_divide},
                                          {0,0}};
    const name_ternary_fn_pair tpairs[] = {{"__pow__", num_meths->nb_power},
                                           {"__ipow__", num_meths->nb_inplace_power},
                                           {0,0}};
    const name_coercion_fn_pair cpairs[] = {{"__coerce__", num_meths->nb_coerce},
                                            {0,0}};
    const name_inquiry_fn_pair ipairs[] = {{"__nonzero__", num_meths->nb_nonzero},
                                           {0,0}};
    addmethod_unary_many(type, upairs);
    addmethod_binary_many(type, bpairs);
    addmethod_ternary_many(type, tpairs);
    addmethod_coercion_many(type, cpairs);
    addmethod_inquiry_many(type, ipairs);
    }

  if ( type->tp_as_sequence )
    {
    PySequenceMethods* sm = type->tp_as_sequence;
    const name_binary_fn_pair bpairs[] = {{"__add__", sm->sq_concat},
                                          {"__iadd__", sm->sq_inplace_concat},
                                          {0,0}};
    const name_inquiry_fn_pair ipairs[] = {{"__len__", sm->sq_length},
                                           {0,0}};
    const name_intarg_fn_pair iapairs[] = {{"__mul__", sm->sq_repeat},
                                           {"__getitem__", sm->sq_item},
                                           {"__imul__", sm->sq_inplace_repeat},
                                           {0,0}};
    const name_intintarg_fn_pair iiapairs[] = {{"__getslice__", sm->sq_slice},
                                           {0,0}};
    const name_intobjarg_fn_pair ioapairs[] = {{"__setitem__", sm->sq_ass_item},
                                           {0,0}};
    const name_intintobjarg_fn_pair iioapairs[] = {{"__setslice__", sm->sq_ass_slice},
                                               {0,0}};
    const name_objobj_fn_pair oopairs[] = {{"__contains__", sm->sq_contains},
                                           {0,0}};
    //addmethod_unary_many(type, upairs);
    addmethod_binary_many(type, bpairs);
    //addmethod_ternary_many(type, tpairs);
    //addmethod_coersion_many(type, cpairs);
    addmethod_inquiry_many(type, ipairs);
    addmethod_intarg_many(type, iapairs);
    addmethod_intintarg_many(type, iiapairs);
    addmethod_intobjarg_many(type, ioapairs);
    addmethod_intintobjarg_many(type, iioapairs);
    addmethod_objobj_many(type, oopairs);
    }

  if ( type->tp_as_mapping )
    {
    PyMappingMethods* mm = type->tp_as_mapping;
    const name_inquiry_fn_pair ipairs[] = {{"__len__", mm->mp_length},
                                           {0,0}};
    const name_binary_fn_pair bpairs[] = {{"__subscript__", mm->mp_subscript}, {0,0}};
    const name_objobjarg_fn_pair ooapairs[] = {{"__isubscript__", mm->mp_ass_subscript}, {0,0}};
    addmethod_inquiry_many(type, ipairs);
    addmethod_binary_many(type, bpairs);
    addmethod_objobjarg_many(type, ooapairs);
    }
/*
  if ( type->tp_as_buffer )
    {
    PyBufferMethods* bm = type->tp_as_buffer;
    // yeah, that's great.
    }
*/

  PRINTF("INIT-SPY-EXT-METHOD-TABLE: finished adding methods\n");
  return NULL;
}

/////////////////////////////// REFERENCE IMPLEMENTATION OF __str__ ///////////////////

//#define DEBUG_SEOTS

#ifdef DEBUG_SEOTS // SpyExtObjToString :P
 #define SEOTSF(fmt, args...) PRINTF(fmt, ##args)
#else
 #define SEOTSF(fmt, args...)
#endif

Scheme_Object* spy_ext_object_to_string(int argc, Scheme_Object* argv[])
{
  PyObject* ext_obj = argv[0];
  char *str;
  int len = -3;
  Scheme_Object* ret;

  SEOTSF("spy_ext_object_to_string ---------\n");

  if ( PyString_CheckExact(ext_obj) )
    {
	SEOTSF("spy_ext_object_to_string: obj is a PyString\n");
    if ( PyString_AsStringAndSize((PyStringObject*)ext_obj, &str, &len) )
	  {
	  fprintf(stderr, "spy_ext_object_to_string: string extraction failed!\n");
	  exit(1);
	  }
	//PRINTF("spy_ext_object_to_string: raw access: %s\n", ((PyStringObject*) ext_obj)->ob_sval);
	//PRINTF("spy_ext_object_to_string: selected: %s (len: %d)\n", str, len);
	}
  else if ( PyList_CheckExact(ext_obj) )
   {
   int size, i;
   SEOTSF("spy_ext_object_to_string: this is a LIST object (in C)\n");
#ifdef DEBUG_SEOTSF
   PyList_Type.tp_print(ext_obj, stdout, 0);
#endif
   SEOTSF("\n");
   size = PyList_Size(ext_obj);
   SEOTSF("spy_ext_object_to_string: the list's size is %d\n", size);
   str = (char*) scheme_malloc_atomic(2048);
   strcpy(str, "[");
   for ( i = 0; i < size; i++ )
     {
     PyObject* item;
     Scheme_Object* ss;
     item = PyList_GetItem(ext_obj, i);
     SEOTSF("spy_ext_object_to_string: got item #%d from the list\n", i);
     SEOTSF("spy_ext_object_to_string: item type: %s\n",
            SCHEME_STR_VAL((Scheme_Object*)sapply1("py-object%->string",PY_GET_TYPE(item))));
     ss = sapply1("py-object%->string", item);
     SEOTSF("spy_ext_object_to_string: converted another list item to string\n");
     strcat(str, SCHEME_STR_VAL(ss));
     strcat(str, ", ");
     }
   strcat(str, "]");
   len = strlen(str);
   SEOTSF("spy_ext_object_to_string: phew... the length of the string representation is %d\n", len);
   }
  else { str = ""; len = 0; }
//    PyString_AsStringAndSize((PyStringObject*)PyObject_Str(ext_obj), &str, &len);
  ret = scheme_make_sized_string(str, len, 1);
  SEOTSF("spy_ext_object_to_string: made the scheme string, returning\n");
//  free(str);
  //PRINTF("spy_ext_object_to_string: deleted the C string.  returning from spy_ext_object_to_string...\n");
  return ret;
}






