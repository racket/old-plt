
/* New getargs implementation */

#include "Python.h"

#include <ctype.h>


int PyArg_ParseTuple(PyObject* arg_tuple, char* format, ...)
{
  int i;
  int format_len = strlen(format);
  va_list va;

  va_start(va, format);
  for ( i = 0; i < format_len; i++ )
    if ( format[i] == 's' )
      {
      char** dest = va_arg(va, char**);
      *dest = PyString_AsString( PyTuple_GetItem(arg_tuple, i) );
      }
    else
      {
      // blow up
      fprintf( stderr, "Argument type not supported yet: %c\n", format[i] );
      exit(1);
      }
  va_end(va);
  return 1;  // success
}

PyObject* Py_BuildValue(char* format, ...)
{
  int i;
  int format_len = strlen(format);
  va_list va;

  if ( format_len == 1 ) // one value, not a tuple
    {
    if ( format[0] == 'i' )
      {
      int arg;
      va_start(va, format);
      arg = va_arg(va, int);
      va_end(va);
      return PyInt_FromInt(arg);
      }
    else
      {
      fprintf( stderr, "Cannot build this type of value yet: %c\n", format[0] );
      exit(1);
      }
    }
  else
    {
    PyObject* tuple = PyTuple_New(format_len);

    va_start(va, format);
    for ( i = 0; i < format_len; i++ )
      if ( format[i] == 'i' )
        {
        int arg = va_arg(va, int);
        PyTuple_SetItem(tuple, i, PyInt_FromInt(arg));
        }
      else
        {
        fprintf( stderr, "Cannot build this type of value yet: %c\n", format[i] );
        exit(1);
        }
    va_end(va);
    return tuple;
    }
}


// taken from Python 2.2 CVS
PyArg_UnpackTuple(PyObject *args, char *name, int min, int max, ...)
{
        int i, l;
        PyObject **o;
        va_list vargs;

#ifdef HAVE_STDARG_PROTOTYPES
        va_start(vargs, max);
#else
        va_start(vargs);
#endif

        assert(min >= 0);
        assert(min <= max);
        if (!PyTuple_Check(args)) {
                PyErr_SetString(PyExc_SystemError,
                    "PyArg_UnpackTuple() argument list is not a tuple");
                return 0;
        }
        l = PyTuple_GET_SIZE(args);
        if (l < min) {
                if (name != NULL)
                        PyErr_Format(
                            PyExc_TypeError,
                            "%s expected %s%d arguments, got %d",
                            name, (min == max ? "" : "at least "), min, l);
                else
                        PyErr_Format(
                            PyExc_TypeError,
                            "unpacked tuple should have %s%d elements,"
                            " but has %d",
                            (min == max ? "" : "at least "), min, l);
                va_end(vargs);
                return 0;
        }
        if (l > max) {
                if (name != NULL)
                        PyErr_Format(
                            PyExc_TypeError,
                            "%s expected %s%d arguments, got %d",
                            name, (min == max ? "" : "at most "), max, l);
                else
                        PyErr_Format(
                            PyExc_TypeError,
                            "unpacked tuple should have %s%d elements,"
                            " but has %d",
                            (min == max ? "" : "at most "), max, l);
                va_end(vargs);
                return 0;
        }
        for (i = 0; i < l; i++) {
                o = va_arg(vargs, PyObject **);
                *o = PyTuple_GET_ITEM(args, i);
        }
        va_end(vargs);
        return 1;
}

// fixme...
int PyArg_ParseTupleAndKeywords(PyObject *args, PyObject *kw, char *format, char *keywords[], ...)
{
  if ( !strcmp(format, "|O:str") )
  {
  va_list va;
  va_start(va, keywords);
      {
      PyObject** dest = va_arg(va, PyObject**);
      *dest = PyTuple_GET_ITEM(args, 0);
	  }
  va_end(va);
  }

 return 1;
}

int PyArg_Parse(PyObject* args, char* format, ...)
{
  return 1;
}


int PyType_IsSubtype(PyTypeObject *t, PyTypeObject *st)
{
  return sapply2("py-is-a?", t, st) != scheme_false;
}

