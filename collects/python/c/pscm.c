#include "Python.h"

#define SPY_LIST
#define SPY_STRING

#define SPY_GLOBALS_SCHEME_STRUCT() _spy_g_scheme_struct
#define SPY_GLOBALS_PYTHON_NODE() _spy_g_python_node

// #define DEBUG_SPY

#ifdef DEBUG_SPY
 #define PRINTF(fmt, args...) printf(fmt, ##args)
#else
 #define PRINTF(fmt, args...)
#endif

#define Scheme_Struct_Type void

typedef struct Scheme_Structure
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Struct_Type *stype;
  Scheme_Object *slots[3];
} Scheme_Structure;

#define SCHEME_STRUCT_TYPE(o) (((Scheme_Structure *)o)->stype)

// cache SCHEME_STRUCT and PYTHON-NODE (for spy_init_obj, etc)
Scheme_Type _spy_g_scheme_struct;
Scheme_Object* _spy_g_python_node;

//// what are these for??
int _PyTrash_delete_nesting = 0;
PyObject* _PyTrash_delete_later = NULL;

PyObject * PyExc_SystemError = 0;
PyObject * PyExc_IndexError = 0;
PyObject * PyExc_RuntimeError = 0;
PyObject * PyExc_FutureWarning = 0;
PyObject * PyExc_DeprecationWarning = 0;
PyObject _Py_NotImplementedStruct; /* Don't use this directly */
PyObject _Py_TrueStruct;
PyObject _Py_ZeroStruct;
PyTypeObject PyInt_Type;
#ifndef SPY_LIST
PyTypeObject PyList_Type;
#endif
#ifndef SPY_STRING
//PyTypeObject PyString_Type;
#endif
PyTypeObject PyDict_Type;
PyTypeObject PyBaseObject_Type;
PyTypeObject PySlice_Type;

PyObject* sapply3(const char* func_name, PyObject* arg1, PyObject* arg2, PyObject* arg3);
PyObject* sapply2(const char* func_name, PyObject* arg1, PyObject* arg2);
PyObject* sapply1(const char* func_name, PyObject* arg);


Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] );
PyObject* make_py_string(int argc, Scheme_Object* argv[]);
Scheme_Object* get_py_string(int argc, Scheme_Object* argv[]);
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
    sapply2("set-python-node-dict!", dest, sapply1("copy-hash-table", sapply1("python-node-dict", (PyObject*) src)));
}


PyObject* spy_ext_new_class(const char* name)
{
  sapply2("py-call", (PyObject*) seval("py-type%"),
          cons( sapply1("symbol->py-string%", (PyObject*) sym(name)),  /* name */
                cons( sapply1("list->py-tuple%", (PyObject*) cons( seval("py-object%"), scheme_null )), /* parents */
                      cons( (Scheme_Object*) scheme_null, (Scheme_Object*) scheme_null ) ) ) ); /* fields/methods */
}

PyObject* spy_ext_new_instance(PyTypeObject* type)
{
  PyObject* obj = PyMem_MALLOC(type->tp_basicsize);
  spy_init_obj(obj, type);
  return obj;
//  sapply1("python-new-object", (PyObject*) type);
}


Scheme_Env* spy_global_env;
extern Scheme_Env* spy_global_env;

Scheme_Object* scheme_initialize(Scheme_Env* env)
{
  spy_global_env = env;
  PyObject* m;
  PyObject* pystr_type = (PyObject*) &PyString_Type;
  Scheme_Object* pstring_class = spy_ext_new_class("pstring");
  PyObject* pylist_type = (PyObject*) &PyList_Type;
  Scheme_Object* plist_class = spy_ext_new_class("plist");
  Scheme_Env* pns; // python namespace

  PyExc_IndexError = slookup("py-index-error%");
  PyExc_SystemError = slookup("py-system-error%");
  PyExc_FutureWarning = slookup("py-future-warning%");
  PyExc_RuntimeError = slookup("py-runtime-error%");
  PyExc_DeprecationWarning = slookup("py-deprecation-warning%");

  // set up globals
  _spy_g_scheme_struct = PyExc_IndexError->type; // scheme object type: STRUCT
  _spy_g_python_node = SCHEME_STRUCT_TYPE(PyExc_IndexError);  // struct type: PYTHON-NODE

  // BLAH!
  PyString_Type.type = pstring_class->type;
  PyString_Type.stype = SCHEME_STRUCT_TYPE(pstring_class);
  sapply2("set-python-node-type!", (PyObject*) &PyString_Type,
          sapply1("python-node-type", pstring_class));
  sapply2("set-python-node-mutable?!", (PyObject*) &PyString_Type, scheme_false);
  spy_copy_dictionary_from((PyObject*) &PyString_Type, pstring_class);

  PyList_Type.type = plist_class->type;
  PyList_Type.stype = SCHEME_STRUCT_TYPE(plist_class);
  sapply2("set-python-node-type!", (PyObject*) &PyList_Type,
          sapply1("python-node-type", plist_class));
  sapply2("set-python-node-mutable?!", (PyObject*) &PyList_Type, scheme_false);
  spy_copy_dictionary_from((PyObject*) &PyList_Type, plist_class);


  /*  register global Spy variables that contain pointers to Scheme values */
  scheme_register_extension_global((void*) &PyString_Type, sizeof(PyString_Type));
  scheme_register_extension_global((void*) &PyList_Type, sizeof(PyList_Type));
  scheme_register_extension_global((void*) spy_global_env, sizeof(spy_global_env));
  scheme_register_extension_global((void*) _spy_g_python_node, sizeof(_spy_g_python_node));

  pns = (Scheme_Env*) seval("(python-ns)");
  //pns = env;

  scheme_add_global( "scheme-python-dispatch-method",
                     scheme_make_prim_w_arity(scheme_python_dispatch_method, "scheme-python-dispatch-method", 1, 99),
                     pns );

  scheme_add_global( "make-py-string",
                     scheme_make_prim_w_arity(make_py_string, "make-py-string", 1, 1),
                     pns );

  scheme_add_global( "get-py-string",
                     scheme_make_prim_w_arity(get_py_string, "get-py-string", 1, 1),
                     pns );

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

  //initspam();
  printf("SPY now initializing the CPY module\n");
  m = Py_InitModule("cpy", empty_method_table);
#ifdef SPY_STRING
  printf("SPY now adding the String type\n");
  if ( PyString_Type.tp_methods )
    printf("methods are fine.\n");
  else
    printf("methods are null.\n");
  PyModule_AddObject(m, "pstr", pystr_type);
#endif
#ifdef SPY_LIST
  printf("SPY now adding the List type\n");
  PyModule_AddObject(m, "list", pylist_type);
#endif
  return scheme_void;
}

int PyModule_AddObject(PyObject* module, char* name, PyObject* obj)
{
  sapply3("py-ext-module-add-object", module, sym(name), obj);
  return 0;
}

Scheme_Object* scheme_reload(Scheme_Env* env){ return scheme_initialize(env); }

Scheme_Object* scheme_module_name() { return scheme_false; }

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
    printf("scheme_argv_to_python_tuple: argc == 1, returning argv[0]\n");
    return argv[0];
    }
  */
  tuple = PyTuple_New(argc);

  printf( "scheme_argv_to_python_tuple: argc: %d\n", argc );
  for ( i = 0; i < argc; i++ )
    PyTuple_SetItem(tuple, i, argv[i]);
  printf( "scheme_argv_to_python_tuple: finished setting up tuple\n" );
  return tuple;
}

Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] )
{
  int i;
  char* method_name = SCHEME_STR_VAL(argv[0]);
  PyCFunction method;

  printf( "scheme_python_dispatch_method: looking for %s\n", method_name );
  printf( "scheme_python_dispatch_method: second argument is %s\n", PyString_AsString(argv[1]));

  for ( i = 0; method_def_array[i].ml_meth; i++ )
    if ( !strcmp( method_def_array[i].ml_name, method_name ) )
      {
      printf( "scheme_python_dispatch_method: found method\n" );
      method = method_def_array[i].ml_meth;
      break;
      }
  printf( "scheme_python_dispatch_method: finished searching\n" );
  argv++; argc--;
  return (*method)( NULL, scheme_argv_to_python_tuple(argc, argv) );
}

Scheme_Object *
scheme_make_struct_instance_plus(Scheme_Object *_stype, int argc, Scheme_Object **args, int extra_mem);



// make-py-string: string -> PyString
PyObject* make_py_string(int argc, Scheme_Object* argv[])
{
    Scheme_Object *type, *dict, *mutable;
	PyStringObject *str;
	Scheme_Object* fields[3];
    Scheme_Object* sobj = spy_ext_new_instance(&PyString_Type);
	int len = strlen(SCHEME_STR_VAL(argv[0]));

    type = sapply1("python-node-type", sobj);
	dict = scheme_false;
	mutable = scheme_true;
	str = (PyStringObject*) scheme_malloc_eternal(sizeof(PyStringObject) + len * sizeof(char));
	str->type = type->type; /* STRUCT */
	str->stype = SCHEME_STRUCT_TYPE(type); /* PYTHON-NODE */
    sapply2("set-python-node-type!", (PyObject*) str,
            sapply1("python-node-type", sobj));
    sapply2("set-python-node-mutable?!", (PyObject*) str, scheme_true);
    spy_copy_dictionary_from((PyObject*) str, sobj);
    strcpy(str->ob_sval, SCHEME_STR_VAL(argv[0]));
    str->ob_size = strlen(SCHEME_STR_VAL(argv[0]));
    return (PyObject*) str;
}


/* TODO: use this in make_py_string */
void spy_init_obj(PyObject* obj, PyTypeObject* py_type)
{
  /*Scheme_Object* sobj = spy_ext_new_instance(py_type);*/
  obj->type = SPY_GLOBALS_SCHEME_STRUCT(); // sobj->type; /* STRUCT */
  obj->stype = SPY_GLOBALS_PYTHON_NODE(); //SCHEME_STRUCT_TYPE(sobj); /* PYTHON-NODE */
/*  sapply2("set-python-node-type!", obj,
          sapply1("python-node-type", sobj)); */
  sapply2("set-python-node-type!", obj, (PyObject*) py_type);
  sapply2("set-python-node-mutable?!", obj, scheme_true);
  sapply2("set-python-node-dict!", obj, seval("(make-hash-table)"));
/*  spy_copy_dictionary_from(obj, sobj);*/
}

// get-py-string: PyString -> string
Scheme_Object* get_py_string(int argc, Scheme_Object* argv[])
{
    return scheme_make_string( ((PyStringObject*) argv[0])->ob_sval );
}

PyObject* spy_ext_call_fn_unary(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  printf("spy_ext_call_fn_unary: calling\n");
  return (*((unaryfunc) SCHEME_CPTR_VAL(fn)))(argv[1]);
}

PyObject* spy_ext_call_fn_binary(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  printf("spy_ext_call_fn_binary: calling\n");
  return (*((binaryfunc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2]);
}

PyObject* spy_ext_call_fn_ternary(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  printf("spy_ext_call_fn_ternary: calling\n");
  return (*((ternaryfunc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2], argv[3]);
}

PyObject* spy_ext_call_fn_coercion(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* rhs = argv[1];
  PyObject* lhs = argv[2];
  int ret;
  printf("spy_ext_call_fn_coercion: calling\n");
  ret = (*((coercion) SCHEME_CPTR_VAL(fn)))(&rhs, &lhs);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_inquiry(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  printf("spy_ext_call_fn_inquiry: calling\n");
  ret = (*((inquiry) SCHEME_CPTR_VAL(fn)))(argv[1]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_intarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  printf("spy_ext_call_fn_intarg: calling\n");
  return (*((intargfunc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]));
}

PyObject* spy_ext_call_fn_intintarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  printf("spy_ext_call_fn_intintarg: calling\n");
  return (*((intintargfunc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]), PyInt_AsInt(argv[3]));
}

PyObject* spy_ext_call_fn_intobjarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  printf("spy_ext_call_fn_intobjarg: calling\n");
  ret = (*((intobjargproc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]), argv[3]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_intintobjarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  printf("spy_ext_call_fn_intintobjarg: calling\n");
  ret = (*((intintobjargproc) SCHEME_CPTR_VAL(fn)))(argv[1], PyInt_AsInt(argv[2]), PyInt_AsInt(argv[3]), argv[4]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_objobj(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  printf("spy_ext_call_fn_objobj: calling\n");
  ret = (*((objobjproc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2]);
  return PyInt_FromInt(ret);
}

PyObject* spy_ext_call_fn_objobjarg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  int ret;
  printf("spy_ext_call_fn_objobjarg: calling\n");
  ret = (*((objobjargproc) SCHEME_CPTR_VAL(fn)))(argv[1], argv[2], argv[3]);
  return PyInt_FromInt(ret);
}


PyObject* spy_ext_call_fn(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  argv++;
  printf("spy_ext_call_fn: calling Spy extension function\n");
  return (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(NULL, scheme_argv_to_python_tuple(argc - 1, argv));
}

PyObject* spy_ext_call_bound_varargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  argv++;
  argv++;
  ASSERT_PN(self);
  printf("spy_ext_call_bound_varargs\n");
  PyObject* ret = (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(self, scheme_argv_to_python_tuple(argc - 2, argv));
  ASSERT_PN(ret);
  printf("spy_ext_call_bound_varargs: returning\n");
  return ret;
}

PyObject* spy_ext_call_bound_noargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  ASSERT_PN(self);
  printf("spy_ext_call_bound_noargs\n");
  PyObject* ret = (*((PyNoArgsFunction) SCHEME_CPTR_VAL(fn)))(self);
  ASSERT_PN(ret);
  printf("spy_ext_call_bound_noargs: returning\n");
  return ret;
}

PyObject* spy_ext_call_bound_onearg(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  PyObject* arg = argv[2];
  ASSERT_PN(self);
  ASSERT_PN(arg);
  printf("spy_ext_call_bound_onearg\n");
  PyObject* ret = (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(self, arg);
  ASSERT_PN(ret);
  printf("spy_ext_call_bound_onearg: returning\n");
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
  printf("spy_ext_call_bound_kwargs\n");
  tuple = scheme_argv_to_python_tuple(argc - 3, argv);
  printf("spy_ext_call_bound_kwargs: converted scheme argv to pytuple\n");
  // TODO: implement keyword arguments here
  PyObject* ret = (*((PyCFunctionWithKeywords) SCHEME_CPTR_VAL(fn)))(self, tuple, kwargs);
  ASSERT_PN(ret);
  printf("spy_ext_call_bound_kwargs: returning\n");
  return ret;
}


PyObject* wrap_ext_function(PyCFunction f, const char* name)
{
  return sapply2("python-wrap-ext-function", scheme_make_cptr((void*) f, "python-ext-function"), sym(name));
}

static PyObject* wrap_ext_method_varargs(PyCFunction m, const char* name)
{
  printf("wrap_ext_method_varargs: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-varargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_method_varargs: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_noargs(PyNoArgsFunction m, const char* name)
{
  printf("wrap_ext_method_noargs: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-noargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_method_noargs: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_onearg(PyCFunction m, const char* name)
{
  printf("wrap_ext_method_onearg: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-onearg", scheme_make_cptr((void*) m, "python-ext-method-onearg"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_method_onearg done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_kwargs(PyCFunctionWithKeywords m, const char* name)
{
  printf("wrap_ext_method_kwargs: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-kwargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_method_kwargs: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_unary(unaryfunc f, const char* name)
{
  printf("wrap_ext_function_unary: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-unary",
                              scheme_make_cptr((void*) f, "python-ext-function-unary"),
                              sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_function_unary: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_binary(binaryfunc f, const char* name)
{
  printf("wrap_ext_function_binary: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-binary",
                              scheme_make_cptr((void*) f, "python-ext-function-binary"),
                              sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_function_binary: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_ternary(ternaryfunc f, const char* name)
{
  printf("wrap_ext_function_ternary: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-ternary",
                              scheme_make_cptr((void*) f, "python-ext-function-ternary"),
                              sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_function_ternary: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_inquiry(inquiry f, const char* name)
{
  printf("wrap_ext_function_inquiry: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-inquiry",
                              scheme_make_cptr((void*) f, "python-ext-function-inquiry"),
                              sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_function_inquiry: done with: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_function_coercion(coercion f, const char* name)
{
  printf("wrap_ext_function_coercion: name: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-function-coercion",
                              scheme_make_cptr((void*) f, "python-ext-function-coercion"),
                              sym(name));
  ASSERT_PN(wrapped);
  printf("wrap_ext_function_coercion: done with: %s\n", name);
  return wrapped;
}


static void addmethod_wrapped(PyTypeObject* type, char* meth_name, PyObject* wrapped)
{
  printf("addmethod_wrapped: setting attribute: %s\n", meth_name);
  sapply3("python-set-attribute!", type, sym(meth_name), wrapped);
  ASSERT_PN(type);
  ASSERT_PN(scheme_hash_get((Scheme_Hash_Table*) sapply1("python-node-dict", type), sym(meth_name)));
  printf("addmethod_wrapped: set attribute: %s\n", meth_name);
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
/*  printf("setting attribute: %s\n", meth_name);
  sapply3("python-set-attribute!", type, sym(meth_name), wrapped);
  ASSERT_PN(type);
  ASSERT_PN(scheme_hash_get((Scheme_Hash_Table*) sapply1("python-node-dict", type), sym(meth_name)));
  printf("set attribute: %s\n", meth_name);*/
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
    printf("wrap_ext_function_" #arity_name ": name: %s\n", name); \
    PyObject* wrapped = sapply2("python-wrap-ext-function-" #arity_name, \
                                scheme_make_cptr((void*) f, "python-ext-function-" #arity_name), \
                                sym(name)); \
    ASSERT_PN(wrapped); \
    printf("wrap_ext_function_" #arity_name ": done with: %s\n", name); \
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


  printf("INIT SPY EXT METHOD TABLE\n");

  printf("adding methods for type %s\n", type->tp_name);

  if ( type->tp_new )
    addmethod(type, "__new__", type->tp_new, METH_KEYWORDS, "");

  if ( type->tp_str )
    addmethod(type, "__str__", type->tp_str, METH_NOARGS, "");

  if ( type->tp_repr )
    addmethod(type, "__repr__", type->tp_repr, METH_NOARGS, "");

  if ( !methods )
   {
   printf("Type method table is null.\n");
   exit(1);
   }

  for ( i = 0; methods[i].ml_name; i++ )
    {
    printf("adding method: %s\n", methods[i].ml_name);
    addmethod( type, methods[i].ml_name, methods[i].ml_meth, methods[i].ml_flags, methods[i].ml_doc );
    printf("added method: %s\n", methods[i].ml_name);
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

  printf("INIT-SPY-EXT-METHOD-TABLE: added %d methods.\n", i);
  return NULL;
}

//#define DEBUG_SEOTS

#ifdef DEBUG_SEOTS // SpyExtObjToString :P
 #define SEOTSF(fmt, args...) printf(fmt, ##args)
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
	//printf("spy_ext_object_to_string: raw access: %s\n", ((PyStringObject*) ext_obj)->ob_sval);
	//printf("spy_ext_object_to_string: selected: %s (len: %d)\n", str, len);
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
  else
    PyString_AsStringAndSize((PyStringObject*)PyObject_Str(ext_obj), &str, &len);
  ret = scheme_make_sized_string(str, len, 1);
  SEOTSF("spy_ext_object_to_string: made the scheme string, returning\n");
//  free(str);
  //printf("spy_ext_object_to_string: deleted the C string.  returning from spy_ext_object_to_string...\n");
  return ret;
}


PyObject* PyModule_GetDict(PyObject* module)
{
  return NULL;
}

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
//  printf( "about to apply %s with one argument\n", func_name );
  return sapply( func, 1, args );
}

void SPY_SET_ATTR(PyObject* obj, const char* attr, PyObject* value)
{
	sapply3("python-set-attribute", obj, sym(attr), value);
}

PyObject* SPY_GET_ATTR(PyObject* obj, const char* attr)
{
	return sapply2("python-get-attribute", obj, sym(attr));
}



PyObject* Py_InitModule(const char* name, PyMethodDef methods[])
{
  //PyObject* m;
/*  int i;
  method_def_array = methods;
  for ( i = 0; methods[i].ml_meth; i++ )
    {
    printf( "about to add extension method %s\n", methods[i].ml_name );
    sapply1( "python-add-extension-method", scheme_make_string(methods[i].ml_name) );
    printf( "added extension method\n" );
//    scheme_add_global( methods[i].ml_name,
//                       scheme_make_prim_w_arity(methods[i].ml_meth, methods[i].ml_name, 0, 99),
//                       scheme_get_env(scheme_config) );

    } */

 return sapply1("py-ext-init-module", sym(name));
}


PyObject* PyErr_NewException(char *name, PyObject *base, PyObject *dict)
{
  THREE_ARGS(args, scheme_make_string(name), base, dict);
  return sapply( slookup("python-new-exception"), 3, args );
}

int PyDict_SetItem(PyObject *p, PyObject *key, PyObject *val)
{
  // (python-method-call p __setitem__ (list key val))
  THREE_ARGS(args, p, sym("__setitem__"), scheme_make_pair(key, scheme_make_pair(val, scheme_null)));
  return sapply( slookup("python-method-call"), 3, args ) != scheme_false;
}

int PyDict_SetItemString(PyObject *p, const char *key, PyObject *val)
{
  return PyDict_SetItem(p, PyString_FromString(key), val);
}
/*
PyObject* PyString_FromString(const char *str)
{
  Scheme_Object* args[1] = { scheme_make_string(str) };
  return sapply( slookup("string->py-string%"), 1, args );
}
*/

void
Py_FatalError (char * message)
{
  scheme_signal_error (message);
}

PyObject *
PyTuple_New (int size)
{
  printf( "PyTuple_New: size: %d\n", size );
  PyObject* tuple = sapply2("python-create-object", slookup("py-tuple%"), scheme_make_integer(size));
  //printf( "PyTuple_New: created tuple\n" );
  return tuple;
}

int
PyTuple_Check (PyObject* tuple)
{
  Scheme_Object* args[2];
  args[0]= tuple;
  args[1] = slookup("py-tuple%");

  return scheme_apply(slookup("py-is-a?"), 2, args) != scheme_false;
}



PyObject* PyTuple_GetItem(PyObject* tuple, int index)
{
  return smethod1(tuple, "__getitem__", PyInt_FromInt(index));
}

#ifndef SPY_LIST

PyObject *
PyList_New (int size)
{
  return sapply1("python-create-object", slookup("py-list%"));
}

int PyList_Append(PyObject *lst, PyObject *app)
{
  smethod1(lst, "append", app);
  return 0;
}


PyObject* PyList_GetItem(PyObject* tuple, int index)
{
	return PyTuple_GetItem(tuple, index);
}

int
PyList_SetItem (PyObject * tuple, int index, PyObject * new_item)
{
  Scheme_Object * args [3];
  args [0] = tuple;
  args [1] = scheme_make_integer_value (index);
  args [2] = new_item;
  scheme_apply (slookup("simple-set-item"), 3, args);
  return 0;
}

int PyList_GetSize(PyObject *o)
{
 return Py/*Sequence_*/Size(o);
}

#endif // SPY_LIST

// FIXME: return 0??? what??
int
PyTuple_SetItem (PyObject * tuple, int index, PyObject * new_item)
{
  smethod2(tuple, "__setitem__",
                  sapply1("number->py-number%", scheme_make_integer(index)),
                  new_item);
  return 0;
}


PyObject *
PyInt_FromLong (long value)
{
  Scheme_Object * args [1];
  args[0] = scheme_make_integer_value (value);
  return scheme_apply (slookup("number->py-number%"), 1, args);
}

long PyInt_AsLong(PyObject *obj)
{
  long ret;
  Scheme_Object* num = sapply1("py-number%->number", obj);
  scheme_get_int_val(num, &ret);
  return ret;
}

int PyInt_AsInt(PyObject *obj)
{
  int ret;
  Scheme_Object* num = sapply1("py-number%->number", obj);
  scheme_get_int_val(num, &ret);
  return ret;
}

double PyFloat_AsDouble(PyObject *obj)
{
  Scheme_Object* num = sapply1("py-number%->number", obj);
  return scheme_real_to_double(num);
}


PyObject *
PyInt_FromInt (int value)
{
  Scheme_Object* args[1] = { scheme_make_integer_value(value) };
  return scheme_apply (slookup("number->py-number%"), 1, args);
}

// FIXME
PyObject * PyExc_OverflowError = 0;
PyObject * PyExc_ValueError = 0;
PyObject * PyExc_TypeError = 0;

PyObject * PyErr_Format(PyObject *o, const char *format, ...)
			//Py_GCC_ATTRIBUTE((format(printf, 2, 3)))
{
return 0;
}

int PyErr_BadArgument(void)
{
return 0;
}

PyObject * PyErr_NoMemory(void)
{
	return seval("py-error%");
}

void PyErr_SetString(PyObject * o, const char * c)
{
}


PyObject *
PyFloat_FromDouble (double value)
{
  Scheme_Object * args [1];
  args[0] = scheme_make_double (value);
  return scheme_apply (slookup("number->py-number%"), 1, args);
}

PyObject* _PyObject_GC_New(PyTypeObject* type)
{
  return spy_ext_new_instance(type);
  //return sapply1("python-new-object", type);
}

void PyErr_SetObject(PyObject *cls, PyObject *obj)
{
}

void PyObject_GC_UnTrack(void *obj)
{
}

/*
PyObject *
PyString_FromStringAndSize (const char * value, int size)
{
  char *str = strdup(value);
  PyObject* py_str = scheme_make_sized_string(str, size, 1);
  free(str);
  return py_str;
}
*/

/*
char* PyString_AsString(PyObject* py_str)
{
   Scheme_Object* str = sapply1( "py-string%->string", py_str );
   Scheme_Object* argv[1] = { str };
   scheme_printf( "STR: ~a~n", 10, 1, argv );
   char* cstr = (char*) scheme_malloc_atomic( sizeof(char) * (SCHEME_STRLEN_VAL(str) + 1) );
   strncpy( cstr, SCHEME_STR_VAL(str), SCHEME_STRLEN_VAL(str) );
   cstr[SCHEME_STRLEN_VAL(str)] = NULL;
   return cstr;
}
*/

int PyDict_DelItem(PyObject *mp, PyObject *key)
{
 smethod1(mp, "del_item", key);
 return 0;
}

void _PyErr_BadInternalCall(char *filename, int lineno)
{
    fprintf(stderr, "Spy: bad internal call in file %s at line %d\n", filename, lineno);
    exit(1);
}


PyObject * PyErr_Occurred(void)
{
  fprintf(stderr, "Spy: a py-err has occurred.\n");
  exit(1);
  return 0;
}


PyObject * PyObject_Str(PyObject *obj)
{
  PyObject* spy_str = smethod0(obj, "__str__");
  Scheme_Object* argv[1];
  argv[0] = sapply1("py-string%->string", spy_str);
  return make_py_string(1, argv);
}



int PyObject_AsCharBuffer(PyObject *obj,
					  const char **buffer,
					  int *buffer_len)
{
  // yeah, right
  return 1;
}

PyObject * PyObject_Init(PyObject *obj, PyTypeObject *type)
{
  //fooo
}

PyObject * PySequence_Fast(PyObject *o, const char* m)
{
  // incorrect implementation, but this is a stupid fn anyway
  return o;
}

int PySequence_Size(PyObject *o)
{
  return PyInt_AsInt(smethod0(o, "__len__"));
}

PyObject* PySequence_GetItem(PyObject *seq, int index)
{
  return smethod1(seq, "__getitem__", PyInt_FromInt(index));
}

int PyTuple_GetSize(PyObject *o)
{
 return PySequence_Size(o);
}

int PyDict_Size(PyObject *o)
{
  return PySequence_Size(smethod0(o, "keys"));
}

PyObject* PyDict_GetItemString(	PyObject *p, const char *key)
{
  return PyDict_GetItem(p, PyString_FromString(key));
}

int PyDict_Next(PyObject *d, int *ppos, PyObject **pkey, PyObject **pvalue)
{
  PyObject *keys;
  if (*ppos >= PyDict_Size(d))
    return 0;
  *ppos++;
  keys = smethod0(d, "keys");
  *pkey = PyList_GetItem(keys, *ppos);
  *pvalue = PyDict_GetItem(d, *pkey);
  return 1;
}


unsigned long PyInt_AsUnsignedLongMask(	PyObject *io)
{
  return (unsigned long) PyInt_AsLong(io);
}

unsigned long PyLong_AsUnsignedLongMask(PyObject *io)
{
  return (unsigned long) PyLong_AsLong(io);
}

// fixme...
int PyErr_ExceptionMatches(PyObject *obj)
{
 return 0;
}

// fixme...
int _PyEval_SliceIndex(PyObject *o, int *i)
{
 return 0;
}

PyObject * PyBool_FromLong(long lng)
{
  return sapply1("number->py-number%", scheme_make_integer(lng));
}

PyObject* alloc_py_type(PYTYPEOBJECT* type, int size)
{
  return sapply1("python-new-object", type);
}

void PyMem_Free(void* obj)
{
}

void PyObject_Free(void* obj)
{
}

PyObject * PyObject_GetItem(PyObject *o, PyObject *key)
{
  return smethod1(o, "__getitem__", key);
}

PyObject * PyDict_GetItem(PyObject *o, PyObject *key)
{
  PyObject* ret = smethod1(o, "__getitem__", key);
  return ret == scheme_false ? NULL : ret;
}

void PyErr_Clear(void)
{
}

PyObject * PyDict_New(void)
{
  //return sapply1("python-new-object", seval("py-dict%"));
  return sapply1("assoc-list->py-dict%", scheme_null);
}

PyObject * PyDict_Keys(PyObject *mp)
{
  return smethod0(mp, "keys");
}

PyObject * PyObject_GenericGetAttr(PyObject *obj, PyObject *attr)
{
  return sapply2("python-get-attribute", obj, attr);
}

void PyDict_Clear(PyObject *mp)
{
 KABOOM();
}

void PY_INCREF(PyObject* o)
{
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
 return 1;
}

void _PyTrash_destroy_chain(void)
{
}

void _PyTrash_deposit_object(PyObject* obj)
{
}

int Py_ReprEnter(PyObject * obj) { return 0; }
void Py_ReprLeave(PyObject * obj) {}

int PyObject_Print(PyObject *o, FILE *fp, int flags)
{
  sapply2("py-print", scheme_false, cons(o, scheme_null));
}

int PyObject_RichCompareBool(PyObject *lhs, PyObject *rhs, int COMP_TYPE)
{
  sapply1("py-object%->bool", PyObject_RichCompare(lhs, rhs, COMP_TYPE)) != scheme_false;
}

int PyObject_Size(PyObject *o)
{
  return PyInt_AsInt(smethod0(o, "__len__"));
}

PyObject * PyObject_Call(PyObject *callable_object, PyObject *args, PyObject *kw)
{
  return sapply3("py-call", callable_object, args, kw);
}

PyObject * PyObject_RichCompare(PyObject * lhs, PyObject *rhs, int COMP_TYPE)
{
 if ( COMP_TYPE == Py_EQ )
   return sapply2("==", lhs, rhs);
 if ( COMP_TYPE == Py_LT )
   return sapply2("py<", lhs, rhs);
 if ( COMP_TYPE == Py_GT )
   return sapply2("py>", lhs, rhs);
 else return seval("py-none");
}

PyObject * PyObject_GetIter(PyObject *obj) { return NULL; }

int PySequence_Check(PyObject *o)
{
 return PyList_Check(o) || PyTuple_Check(o);
}

int PyFloat_Check(PyObject *o)
{
  return sapply2("py-is-a?", o, seval("py-number%")) != scheme_false;
}

PyObject * PyIter_Next(PyObject *iter)
{
  return NULL;
}

int PySlice_GetIndicesEx(PySliceObject *r, int length,
                                    int *start, int *stop,
                                    int *step, int *slicelength)
{
  return 0;
}


PyObject * PyType_GenericAlloc(PyTypeObject *type, int size)
{
  PyObject* obj = PyMem_Malloc(size ? size : 1);
  PRINTF("PyType_GenericAlloc ------\n");
  spy_init_obj(obj, type);
  return obj;
}


void PyObject_GC_Del(void *obj) {}

PyObject * PyType_GenericNew(PyTypeObject *type,
					       PyObject *args, PyObject *kw)
{
  //return sapply1("python-new-object", type);
  PRINTF("PyType_GenericNew ---------\n");
  return spy_ext_new_instance(type);
}

PyObject * PyObject_SelfIter(PyObject *o) { return NULL; }

void spy_copy_s_obj(PyObject* dest, PyObject* src)
{
  dest->type = src->type;
  dest->stype = SCHEME_STRUCT_TYPE(src);
  sapply2("set-python-node-type!", dest, sapply1("python-node-type", src));
  sapply2("set-python-node-mutable?!", dest, scheme_false);
  spy_copy_dictionary_from(dest, src);
}

#ifndef MIN
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#endif

void* PyMem_Realloc(void* o, size_t new_size)
{
  int amt_to_copy;
  void *new_buffer;
  printf("PyMem_Realloc\n");
  if ( new_size == 0 )
    {
    printf("PyMem_Realloc: size = 0, returning free()\n");
    return NULL; /* no free() in a GC environment */
    }
  if ( !o )
   {
   printf("PyMem_Realloc: orig is null, returning malloc(%d)\n", new_size);
   return PyMem_MALLOC(new_size);
   }
  amt_to_copy = new_size; // how can I get the old size? should be min(new_size, old_size)
  printf("PyMem_Realloc: allocating %d bytes\n", new_size);
  new_buffer = PyMem_Malloc(new_size ? new_size : sizeof(PyVarObject));
  printf("PyMem_Realloc: allocated new object\n");
  printf("PyMem_Realloc: copying %d bytes\n", amt_to_copy);
  if ( amt_to_copy )
    memcpy(new_buffer, o, amt_to_copy);
  return new_buffer;
}
