#include "Python.h"

PyObject * PyExc_SystemError = 0;
PyObject * PyExc_IndexError = 0;
PyObject * PyExc_FutureWarning = 0;
PyObject _Py_NotImplementedStruct; /* Don't use this directly */
PyObject _Py_TrueStruct;
PyObject _Py_ZeroStruct;
PyTypeObject PyInt_Type;
PyTypeObject PyList_Type;
PyTypeObject PyDict_Type;
PyTypeObject PyBaseObject_Type;

PyObject* sapply3(const char* func_name, PyObject* arg1, PyObject* arg2, PyObject* arg3);
PyObject* sapply2(const char* func_name, PyObject* arg1, PyObject* arg2);
PyObject* sapply1(const char* func_name, PyObject* arg);


Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] );
PyObject* make_py_string(int argc, Scheme_Object* argv[]);
Scheme_Object* get_py_string(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_fn(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_varargs(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_noargs(int argc, Scheme_Object* argv[]);
PyObject* spy_ext_call_bound_kwargs(int argc, Scheme_Object* argv[]);
Scheme_Object* init_spy_ext_method_table(int argc, Scheme_Object* argv[]);
Scheme_Object* spy_ext_object_to_string(int argc, Scheme_Object* argv[]);

//extern PyMethodDef string_methods[];

static PyMethodDef module_methods[] = {
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
  sapply1("python-new-object", (PyObject*) type);
}

#define Scheme_Struct_Type void

typedef struct Scheme_Structure
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Struct_Type *stype;
  Scheme_Object *slots[3];
} Scheme_Structure;

#define SCHEME_STRUCT_TYPE(o) (((Scheme_Structure *)o)->stype)

Scheme_Env* spy_global_env;
extern Scheme_Env* spy_global_env;

Scheme_Object* scheme_initialize(Scheme_Env* env)
{
  spy_global_env = env;
  PyObject* m;
  PyObject* pystr_type = (PyObject*) &PyString_Type;
  Scheme_Object* pstring_class = spy_ext_new_class("pstring");

  PyExc_IndexError = slookup("py-index-error%");
  PyExc_SystemError = slookup("py-system-error%");
  PyExc_FutureWarning = slookup("py-future-warning%");

  // BLAH!
  PyString_Type.type = pstring_class->type;
  PyString_Type.stype = SCHEME_STRUCT_TYPE(pstring_class);
//  memcpy(&PyString_Type, pstring_class, sizeof(PyObject));
  sapply2("set-python-node-type!", (PyObject*) &PyString_Type,
          sapply1("python-node-type", pstring_class));
  sapply2("set-python-node-mutable?!", (PyObject*) &PyString_Type, scheme_false);
  spy_copy_dictionary_from((PyObject*) &PyString_Type, pstring_class);

  /*  register global Spy variables that contain pointers to Scheme values */
  scheme_register_extension_global((void*) &PyString_Type, sizeof(PyString_Type));
  scheme_register_extension_global((void*) spy_global_env, sizeof(spy_global_env));

  scheme_add_global( "scheme-python-dispatch-method",
                     scheme_make_prim_w_arity(scheme_python_dispatch_method, "scheme-python-dispatch-method", 1, 99),
                     env );

  scheme_add_global( "make-py-string",
                     scheme_make_prim_w_arity(make_py_string, "make-py-string", 1, 1),
                     env );

  scheme_add_global( "get-py-string",
                     scheme_make_prim_w_arity(get_py_string, "get-py-string", 1, 1),
                     env );

  scheme_add_global( "spy-ext-call-fn",
                     scheme_make_prim_w_arity(spy_ext_call_fn, "spy-ext-call-fn", 1, -1),
                     env );

  scheme_add_global( "spy-ext-call-bound-varargs",
                     scheme_make_prim_w_arity(spy_ext_call_bound_varargs, "spy-ext-call-bound-varargs", 2, -1),
                     env );

  scheme_add_global( "spy-ext-call-bound-noargs",
                     scheme_make_prim_w_arity(spy_ext_call_bound_noargs, "spy-ext-call-bound-noargs", 2, 2),
                     env );

  scheme_add_global( "spy-ext-call-bound-kwargs",
                     scheme_make_prim_w_arity(spy_ext_call_bound_kwargs, "spy-ext-call-bound-kwargs", 3, -1),
                     env );

  scheme_add_global( "init-spy-ext-method-table",
                     scheme_make_prim_w_arity(init_spy_ext_method_table, "init-spy-ext-method-table", 3, 3),
                     env );

  scheme_add_global( "spy-ext-object->string",
                     scheme_make_prim_w_arity(spy_ext_object_to_string, "spy-ext-object->string", 1, 1),
                     env );

  //initspam();
  m = Py_InitModule("pstring", module_methods);
  PyModule_AddObject(m, "PString", pystr_type);
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

PyObject* scheme_argv_to_python_tuple( int argc, Scheme_Object* argv[] )
{
  PyObject* tuple = PyTuple_New(argc);
  int i;

  printf( "entered scheme_argv_to_python_tuple, argc: %d\n", argc );
  for ( i = 0; i < argc; i++ )
    PyTuple_SetItem(tuple, i, argv[i]);
  printf( "finished setting up tuple\n" );
  return tuple;
}

Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] )
{
  int i;
  char* method_name = SCHEME_STR_VAL(argv[0]);
  PyCFunction method;

  printf( "entered scheme_python_dispatch_method, looking for %s\n", method_name );
  printf( "the second argument is %s\n", PyString_AsString(argv[1]));

  for ( i = 0; method_def_array[i].ml_meth; i++ )
    if ( !strcmp( method_def_array[i].ml_name, method_name ) )
      {
      printf( "found method\n" );
      method = method_def_array[i].ml_meth;
      break;
      }
  printf( "finished searching\n" );
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
/*    PyStringObject* str = (PyStringObject*) scheme_malloc(sizeof(PyStringObject)); */
    Scheme_Object* sobj = spy_ext_new_instance(&PyString_Type);
	int len = strlen(SCHEME_STR_VAL(argv[0]));

/*    memcpy(str, sobj, sizeof(Scheme_Object));
    sapply2("set-python-node-type!", (PyObject*) str,
            sapply1("python-node-type", sobj));
    sapply2("set-python-node-mutable?!", (PyObject*) str, scheme_true);
    spy_copy_dictionary_from((PyObject*) str, sobj); */
    type = sapply1("python-node-type", sobj);
	dict = scheme_false;
	mutable = scheme_true;
	str = (PyStringObject*) scheme_malloc_eternal(sizeof(PyStringObject) + len * sizeof(char));
	str->type = type->type; /* STRUCT */
	str->stype = SCHEME_STRUCT_TYPE(type); /* PYTHON-NODE */
	/*scheme_make_struct_instance_plus(SCHEME_STRUCT_TYPE(type), 3, fields, sizeof(PyStringObject) + len * sizeof(char));*/
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
  Scheme_Object* sobj = spy_ext_new_instance(py_type);
  obj->type = sobj->type; /* STRUCT */
  obj->stype = SCHEME_STRUCT_TYPE(sobj); /* PYTHON-NODE */
  sapply2("set-python-node-type!", obj,
          sapply1("python-node-type", sobj));
  sapply2("set-python-node-mutable?!", obj, scheme_true);
  spy_copy_dictionary_from(obj, sobj);
}

// get-py-string: PyString -> string
Scheme_Object* get_py_string(int argc, Scheme_Object* argv[])
{
    return scheme_make_string( ((PyStringObject*) argv[0])->ob_sval );
}

PyObject* spy_ext_call_fn(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  argv++;
  printf("calling Spy extension function\n");
  return (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(NULL, scheme_argv_to_python_tuple(argc - 1, argv));
}

PyObject* spy_ext_call_bound_varargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  argv++;
  argv++;
  ASSERT_PN(self);
  printf("calling Spy bound method (with varargs)\n");
  PyObject* ret = (*((PyCFunction) SCHEME_CPTR_VAL(fn)))(self, scheme_argv_to_python_tuple(argc - 2, argv));
  ASSERT_PN(ret);
  printf("returning from spy-ext-call-bound-varargs\n");
  return ret;
}

PyObject* spy_ext_call_bound_noargs(int argc, Scheme_Object* argv[])
{
  Scheme_Object* fn = argv[0];
  PyObject* self = argv[1];
  ASSERT_PN(self);
  printf("calling Spy bound method (with no args)\n");
  PyObject* ret = (*((PyNoArgsFunction) SCHEME_CPTR_VAL(fn)))(self);
  ASSERT_PN(ret);
  printf("returning from spy-ext-call-bound-noargs\n");
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
  printf("calling Spy bound method (with kw args)\n");
  tuple = scheme_argv_to_python_tuple(argc - 3, argv);
  printf("converted scheme argv to pytuple\n");
  // TODO: implement keyword arguments here
  PyObject* ret = (*((PyCFunctionWithKeywords) SCHEME_CPTR_VAL(fn)))(self, tuple, kwargs);
  ASSERT_PN(ret);
  printf("returning from spy-ext-call-bound-kwargs\n");
  return ret;
}


PyObject* wrap_ext_function(PyCFunction f, const char* name)
{
  return sapply2("python-wrap-ext-function", scheme_make_cptr((void*) f, "python-ext-function"), sym(name));
}

static PyObject* wrap_ext_method_varargs(PyCFunction m, const char* name)
{
  printf("wrapping method: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-varargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrapped method: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_noargs(PyNoArgsFunction m, const char* name)
{
  printf("wrapping method: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-noargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrapped method: %s\n", name);
  return wrapped;
}

static PyObject* wrap_ext_method_kwargs(PyCFunctionWithKeywords m, const char* name)
{
  printf("wrapping method: %s\n", name);
  PyObject* wrapped = sapply2("python-wrap-ext-method-kwargs", scheme_make_cptr((void*) m, "python-ext-method"), sym(name));
  ASSERT_PN(wrapped);
  printf("wrapped method: %s\n", name);
  return wrapped;
}


static void addmethod(PyTypeObject* type, char* meth_name, PyCFunction method, int flags, char* doc)
{
  PyObject* wrapped;
  if ( flags & METH_NOARGS )
    wrapped = wrap_ext_method_noargs((PyNoArgsFunction)method, meth_name);
  else if ( flags & METH_KEYWORDS )
    wrapped = wrap_ext_method_kwargs((PyCFunctionWithKeywords)method, meth_name);
  else
    wrapped = wrap_ext_method_varargs(method, meth_name);
  printf("setting attribute: %s\n", meth_name);
  sapply3("python-set-attribute!", type, sym(meth_name), wrapped);
  ASSERT_PN(type);
  ASSERT_PN(scheme_hash_get((Scheme_Hash_Table*) sapply1("python-node-dict", type), sym(meth_name)));
  printf("set attribute: %s\n", meth_name);
}

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

  if ( type->tp_new )
    addmethod(type, "__new__", type->tp_new, METH_KEYWORDS, "");

  if ( type->tp_str )
    addmethod(type, "__str__", type->tp_str, METH_NOARGS, "");

  if ( type->tp_repr )
    addmethod(type, "__repr__", type->tp_repr, METH_NOARGS, "");

  for ( i = 0; methods[i].ml_name; i++ )
    {
    printf("adding method: %s\n", methods[i].ml_name);
    addmethod( type, methods[i].ml_name, methods[i].ml_meth, methods[i].ml_flags, methods[i].ml_doc );
    printf("added method: %s\n", methods[i].ml_name);
	}
/*
  if ( type->tp_as_number )
    {
	PyNumberMethods* num_meths = type->tp_as_number;
	if ( num_meths->nb_add )
	  addmethod(type, "__add__", num_methos->nb_add);
	if ( num_meths->nb_subtract )
	  addmethod(type, "__sub__", num_methos->nb_subtract);
	if ( num_meths->nb_multiply )
	  addmethod(type, "__mul__", num_methos->nb_multiply);
	if ( num_meths->nb_divide )
	  addmethod(type, "__div__", num_methos->nb_divide);
	if ( num_meths->nb_remainder )
	  addmethod(type, "__mod__", num_methos->nb_remainder);
	} */

  printf("INIT-SPY-EXT-METHOD-TABLE: added %d methods.\n", i);
  return NULL;
}

Scheme_Object* spy_ext_object_to_string(int argc, Scheme_Object* argv[])
{
  PyObject* ext_obj = argv[0];
  char *str;
  int len = -3;

  if ( PyString_CheckExact(ext_obj) )
    {
	//printf("spy_ext_object_to_string: obj is a PyString\n");
    if ( PyString_AsStringAndSize((PyStringObject*)ext_obj, &str, &len) )
	  {
	  fprintf(stderr, "spy_ext_object_to_string: string extraction failed!\n");
	  exit(1);
	  }
	//printf("spy_ext_object_to_string: raw access: %s\n", ((PyStringObject*) ext_obj)->ob_sval);
	//printf("spy_ext_object_to_string: selected: %s (len: %d)\n", str, len);
	}
  else
    PyString_AsStringAndSize((PyStringObject*)PyObject_Str(ext_obj), &str, &len);
  return scheme_make_sized_string(str, len, 1);
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
  printf( "about to create a new tuple of size %d\n", size );
  PyObject* tuple = sapply2("python-create-object", slookup("py-tuple%"), scheme_make_integer(size));
  printf( "created tuple\n" );
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


PyObject *
PyList_New (int size)
{
  return sapply1("python-create-object", slookup("py-list%"));
}

PyObject* PyTuple_GetItem(PyObject* tuple, int index)
{
  return smethod1(tuple, "__getitem__", PyInt_FromInt(index));
}

PyObject* PyList_GetItem(PyObject* tuple, int index)
{
	return PyTuple_GetItem(tuple, index);
}

// FIXME: return 0??? what??
int
PyTuple_SetItem (PyObject * tuple, int index, PyObject * new_item)
{
  smethod2(tuple, "__setitem__",
                  sapply1("number->py-number%", scheme_make_integer(index)),
                  new_item);
  return 0;
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

int PyList_Append(PyObject *lst, PyObject *app)
{
  smethod1(lst, "append", app);
  return 0;
}


int PyObject_AsCharBuffer(PyObject *obj,
					  const char **buffer,
					  int *buffer_len)
{
  // yeah, right
  return 1;
}


PyObject * PySequence_Fast(PyObject *o, const char* m)
{
  // incorrect implementation, but this is a stupid fn anyway
  return o;
}

int PySequence_Size(PyObject *o)
{
  return PyInt_AsInt(smethod0(o, "size"));
}

int PyTuple_GetSize(PyObject *o)
{
 return PySequence_Size(o);
}

int PyList_GetSize(PyObject *o)
{
 return PySequence_Size(o);
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
  return smethod1(o, "__getitem__", key);
}

void PyErr_Clear(void)
{
}

PyObject * PyDict_New(void)
{
  return sapply1("python-new-object", seval("py-dict%"));
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
 return smethod0(obj, "repr");
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

