#include "Python.h"

/* from modsupport */

#ifdef MPW /* MPW pushes 'extended' for float and double types with varargs */
typedef extended va_double;
#else
typedef double va_double;
#endif


static PyObject *
do_mkdict(char **p_format, va_list *p_va, int endchar, int n)
{
        PyObject *d;
        int i;
        if (n < 0)
                return NULL;
        if ((d = PyDict_New()) == NULL)
                return NULL;
        for (i = 0; i < n; i+= 2) {
                PyObject *k, *v;
                int err;
                k = do_mkvalue(p_format, p_va);
                if (k == NULL) {
                        Py_DECREF(d);
                        return NULL;
                }
                v = do_mkvalue(p_format, p_va);
                if (v == NULL) {
                        Py_DECREF(k);
                        Py_DECREF(d);
                        return NULL;
                }
                err = PyDict_SetItem(d, k, v);
                Py_DECREF(k);
                Py_DECREF(v);
                if (err < 0) {
                        Py_DECREF(d);
                        return NULL;
                }
        }
        if (d != NULL && **p_format != endchar) {
                Py_DECREF(d);
                d = NULL;
                PyErr_SetString(PyExc_SystemError,
                                "Unmatched paren in format");
        }
        else if (endchar)
                ++*p_format;
        return d;
}


do_mklist(char **p_format, va_list *p_va, int endchar, int n)
{
        PyObject *v;
        int i;
        if (n < 0)
                return NULL;
        if ((v = PyList_New(n)) == NULL)
                return NULL;
        for (i = 0; i < n; i++) {
                PyObject *w = do_mkvalue(p_format, p_va);
                if (w == NULL) {
                        Py_DECREF(v);
                        return NULL;
                }
                PyList_SetItem(v, i, w);
        }
        if (v != NULL && **p_format != endchar) {
                Py_DECREF(v);
                v = NULL;
                PyErr_SetString(PyExc_SystemError,
                                "Unmatched paren in format");
        }
        else if (endchar)
                ++*p_format;
        return v;
}


do_mkvalue(char **p_format, va_list *p_va)
{
        for (;;) {
                switch (*(*p_format)++) {
                case '(':
                        return do_mktuple(p_format, p_va, ')',
                                          countformat(*p_format, ')'));

                case '[':
                        return do_mklist(p_format, p_va, ']',
                                         countformat(*p_format, ']'));

                case '{':
                        return do_mkdict(p_format, p_va, '}',
                                         countformat(*p_format, '}'));

                case 'b':
                case 'B':
                case 'h':
                case 'i':
                        return PyInt_FromLong((long)va_arg(*p_va, int));

                case 'H':
                        return PyInt_FromLong((long)va_arg(*p_va, unsigned int));

                case 'l':
                        return PyInt_FromLong((long)va_arg(*p_va, long));

                case 'k':
                        return PyInt_FromLong((long)va_arg(*p_va, unsigned long));

#ifdef HAVE_LONG_LONG
                case 'L':
                        return PyLong_FromLongLong((PY_LONG_LONG)va_arg(*p_va, PY_LONG_LONG));

                case 'K':
                        return PyLong_FromLongLong((PY_LONG_LONG)va_arg(*p_va, unsigned PY_LONG_LONG));
#endif
#ifdef Py_USING_UNICODE
                case 'u':
                {
                        PyObject *v;
                        Py_UNICODE *u = va_arg(*p_va, Py_UNICODE *);
                        int n;
                        if (**p_format == '#') {
                                ++*p_format;
                                n = va_arg(*p_va, int);
                        }
                        else
                                n = -1;
                        if (u == NULL) {
                                v = Py_None;
                                Py_INCREF(v);
                        }
                        else {
                                if (n < 0)
                                        n = _ustrlen(u);
                                v = PyUnicode_FromUnicode(u, n);
                        }
                        return v;
                }
#endif
                case 'f':
                case 'd':
                        return PyFloat_FromDouble(
                                (double)va_arg(*p_va, va_double));

#ifndef WITHOUT_COMPLEX
                case 'D':
                        return PyComplex_FromCComplex(
                                *((Py_complex *)va_arg(*p_va, Py_complex *)));
#endif /* WITHOUT_COMPLEX */

                case 'c':
                {
                        char p[1];
                        p[0] = va_arg(*p_va, int);
                        return PyString_FromStringAndSize(p, 1);
                }

                case 's':
                case 'z':
                {
                        PyObject *v;
                        char *str = va_arg(*p_va, char *);
                        int n;
                        if (**p_format == '#') {
                                ++*p_format;
                                n = va_arg(*p_va, int);
                        }
                        else
                                n = -1;
                        if (str == NULL) {
                                v = Py_None;
                                Py_INCREF(v);
                        }
                        else {
                                if (n < 0) {
                                        size_t m = strlen(str);
                                        if (m > INT_MAX) {
                                                PyErr_SetString(PyExc_OverflowError,
                                                        "string too long for Python string");
                                                return NULL;
                                        }
                                        n = (int)m;
                                }
                                v = PyString_FromStringAndSize(str, n);
                        }
                        return v;
                }
                case 'N':
                case 'S':
                case 'O':
                if (**p_format == '&') {
                        typedef PyObject *(*converter)(void *);
                        converter func = va_arg(*p_va, converter);
                        void *arg = va_arg(*p_va, void *);
                        ++*p_format;
                        return (*func)(arg);
                }
                else {
                        PyObject *v;
                        v = va_arg(*p_va, PyObject *);
                        if (v != NULL) {
                                if (*(*p_format - 1) != 'N')
                                        Py_INCREF(v);
                        }
                        else if (!PyErr_Occurred())
                                /* If a NULL was passed
                                 * because a call that should
                                 * have constructed a value
                                 * failed, that's OK, and we
                                 * pass the error on; but if
                                 * no error occurred it's not
                                 * clear that the caller knew
                                 * what she was doing. */
                                PyErr_SetString(PyExc_SystemError,
                                        "NULL object passed to Py_BuildValue");
                        return v;
                }

                case ':':
                case ',':
                case ' ':
                case '\t':
                        break;


                default:
                        PyErr_SetString(PyExc_SystemError,
                                "bad format char passed to Py_BuildValue");
                        return NULL;

                }
        }
}




do_mktuple(char **p_format, va_list *p_va, int endchar, int n)
{
        PyObject *v;
        int i;
        if (n < 0)
                return NULL;
        if ((v = PyTuple_New(n)) == NULL)
                return NULL;
        for (i = 0; i < n; i++) {
                PyObject *w = do_mkvalue(p_format, p_va);
                if (w == NULL) {
                        Py_DECREF(v);
                        return NULL;
                }
                PyTuple_SetItem(v, i, w);
        }
        if (v != NULL && **p_format != endchar) {
                Py_DECREF(v);
                v = NULL;
                PyErr_SetString(PyExc_SystemError,
                                "Unmatched paren in format");
        }
        else if (endchar)
                ++*p_format;
        return v;
}



PyObject *
Py_VaBuildValue(char *format, va_list va)
{
        char *f = format;
        int n = countformat(f, '\0');
        va_list lva;

#ifdef VA_LIST_IS_ARRAY
        memcpy(lva, va, sizeof(va_list));
#else
#ifdef __va_copy
        __va_copy(lva, va);
#else
        lva = va;
#endif
#endif

        if (n < 0)
                return NULL;
        if (n == 0) {
                Py_INCREF(Py_None);
                return Py_None;
        }
        if (n == 1)
                return do_mkvalue(&f, &lva);
        return do_mktuple(&f, &lva, '\0', n);
}

/* Helper for mkvalue() to scan the length of a format */

int
countformat(char *format, int endchar)
{
        int count = 0;
        int level = 0;
        while (level > 0 || *format != endchar) {
                switch (*format) {
                case '\0':
                        /* Premature end */
                        PyErr_SetString(PyExc_SystemError,
                                        "unmatched paren in format");
                        return -1;
                case '(':
                case '[':
                case '{':
                        if (level == 0)
                                count++;
                        level++;
                        break;
                case ')':
                case ']':
                case '}':
                        level--;
                        break;
                case '#':
                case '&':
                case ',':
                case ':':
                case ' ':
                case '\t':
                        break;
                default:
                        if (level == 0)
                                count++;
                }
                format++;
        }
        return count;
}

