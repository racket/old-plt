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

Scheme_Object* scheme_python_dispatch_method( int argc, Scheme_Object* argv[] );
PyObject* make_py_string(int argc, Scheme_Object* argv[]);

//extern PyMethodDef string_methods[];

static PyMethodDef module_methods[] = {
    {NULL}  /* Sentinel */
};

Scheme_Object* scheme_initialize(Scheme_Env* env)
{
  PyObject* m;
  PyObject* pystr_type = seval("py-string%");

  PyExc_IndexError = seval("#csIndexError");
  PyExc_SystemError = seval("#csSystemError");
  PyExc_FutureWarning = seval("#csFutureWarning");

  // BLAH!
  //memcpy(&PyString_Type, seval("py-string%"), sizeof(Scheme_Object));

  scheme_add_global( "scheme-python-dispatch-method",
                     scheme_make_prim_w_arity(scheme_python_dispatch_method, "scheme-python-dispatch-method", 1, 99),
                     scheme_get_env(scheme_config) );

  scheme_add_global( "make-py-string",
                     scheme_make_prim_w_arity(make_py_string, "make-py-string", 1, 1),
                     scheme_get_env(scheme_config) );

  //initspam();
  m = Py_InitModule("pstring", module_methods);
  PyModule_AddObject(m, "String", pystr_type);
  return scheme_void;
}

int PyModule_AddObject(PyObject* module, char* name, PyObject* obj)
{
  sapply3("py-ext-module-add-object", module, scheme_make_string(name), obj);
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

PyObject* make_py_string(int argc, Scheme_Object* argv[])
{
    PyStringObject* str = (PyStringObject*) scheme_malloc(sizeof(PyStringObject));
    Scheme_Object* sobj = seval("(string->py-string% \"\")");
    memcpy(str, sobj, sizeof(Scheme_Object));
    str->ob_sval = SCHEME_STR_VAL(argv[0]);
    return str;
}


PyObject* PyModule_GetDict(PyObject* module)
{
  return NULL;
}

__inline__ PyObject* smethod0(PyObject* obj, const char* method_name)
{
  TWO_ARGS(args, obj, scheme_intern_symbol(method_name));
  return sapply( seval("python-method-call"), 2, args );
}

__inline__ PyObject* smethod1(PyObject* obj, const char* method_name, PyObject* arg)
{
  THREE_ARGS(args, obj, scheme_intern_symbol(method_name), scheme_make_pair(arg, scheme_null));
  return sapply( seval("python-method-call"), 3, args );
}

__inline__ PyObject* smethod2(PyObject* obj, const char* method_name, PyObject* arg1, PyObject* arg2)
{
  THREE_ARGS(args, obj, scheme_intern_symbol(method_name), cons(arg1, cons(arg2, scheme_null)));
  return sapply( seval("python-method-call"), 3, args );
}

PyObject* sapply3(const char* func_name, PyObject* arg1, PyObject* arg2, PyObject* arg3)
{
  Scheme_Object* args[3] = { arg1, arg2, arg3 };
  return sapply( seval(func_name), 3, args );
}


PyObject* sapply2(const char* func_name, PyObject* arg1, PyObject* arg2)
{
  Scheme_Object* args[2] = { arg1, arg2 };
  return sapply( seval(func_name), 2, args );
}

PyObject* sapply1(const char* func_name, PyObject* arg)
{
  Scheme_Object* args[1] = { arg };
  Scheme_Object* func = seval(func_name);
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

 return sapply1("py-ext-init-module", scheme_make_string(name));
}


PyObject* PyErr_NewException(char *name, PyObject *base, PyObject *dict)
{
  THREE_ARGS(args, scheme_make_string(name), base, dict);
  return sapply( seval("python-new-exception"), 3, args );
}

int PyDict_SetItem(PyObject *p, PyObject *key, PyObject *val)
{
  // (python-method-call p __setitem__ (list key val))
  THREE_ARGS(args, p, scheme_intern_symbol("__setitem__"), scheme_make_pair(key, scheme_make_pair(val, scheme_null)));
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
  printf( "about to create a new tuple\n" );
  PyObject* tuple = sapply2("python-create-object", seval("py-tuple%"), scheme_make_integer(size));
  printf( "created tuple\n" );
  return tuple;
}

int
PyTuple_Check (PyObject* tuple)
{
  Scheme_Object* args[2];
  args[0]= tuple;
  args[1] = scheme_eval_string ("py-tuple%", scheme_get_env(scheme_config));

  return scheme_apply(scheme_eval_string("py-is-a?", scheme_get_env(scheme_config)), 2, args) != scheme_false;
}


PyObject *
PyList_New (int size)
{
  return scheme_eval_string ("(python-create-object py-list%)", scheme_get_env (scheme_config));
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
  scheme_apply (scheme_eval_string ("simple-set-item", scheme_get_env (scheme_config)), 3, args);
  return 0;
}

PyObject *
PyInt_FromLong (long value)
{
  Scheme_Object * args [1];
  args[0] = scheme_make_integer_value (value);
  return scheme_apply (scheme_eval_string ("number->py-number%", scheme_get_env (scheme_config)), 1, args);
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
  return scheme_apply (scheme_eval_string ("number->py-number%", scheme_get_env (scheme_config)), 1, args);
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
  return scheme_apply (scheme_eval_string ("number->py-number%", scheme_get_env (scheme_config)), 1, args);
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
    return smethod0(obj, "str");
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
  return sapply1("new-python-object", type);
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
  return sapply1("new-python-object", seval("py-dict%"));
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

