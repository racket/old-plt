/** Most of this code comes from CPython's  Python/compile.c  **/

#include "Python.h"

#define OFF(x) offsetof(PyCodeObject, x)


static PyMemberDef code_memberlist[] = {
        {"co_argcount", T_INT,          OFF(co_argcount),       READONLY},
        {"co_nlocals",  T_INT,          OFF(co_nlocals),        READONLY},
        {"co_stacksize",T_INT,          OFF(co_stacksize),      READONLY},
        {"co_flags",    T_INT,          OFF(co_flags),          READONLY},
        {"co_code",     T_OBJECT,       OFF(co_code),           READONLY},
        {"co_consts",   T_OBJECT,       OFF(co_consts),         READONLY},
        {"co_names",    T_OBJECT,       OFF(co_names),          READONLY},
        {"co_varnames", T_OBJECT,       OFF(co_varnames),       READONLY},
        {"co_freevars", T_OBJECT,       OFF(co_freevars),       READONLY},
        {"co_cellvars", T_OBJECT,       OFF(co_cellvars),       READONLY},
        {"co_filename", T_OBJECT,       OFF(co_filename),       READONLY},
        {"co_name",     T_OBJECT,       OFF(co_name),           READONLY},
        {"co_firstlineno", T_INT,       OFF(co_firstlineno),    READONLY},
        {"co_lnotab",   T_OBJECT,       OFF(co_lnotab),         READONLY},
        {NULL}  /* Sentinel */
};


PyDoc_STRVAR(code_doc,
"code(argcount, nlocals, stacksize, flags, codestring, constants, names,\n\
      varnames, filename, name, firstlineno, lnotab[, freevars[, cellvars]])\n\
\n\
Create a code object.  Not for the faint of heart.");

static PyObject *
code_new(PyTypeObject *type, PyObject *args, PyObject *kw)
{
  PyErr_SetString(PyExc_TypeError,
                  "Sorry, Spy does not support code.__new__");
  return NULL;
}


static PyObject *
code_repr(PyCodeObject *co)
{
        char buf[500];
        int lineno = -1;
        char *filename = "???";
        char *name = "???";

        if (co->co_firstlineno != 0)
                lineno = co->co_firstlineno;
        if (co->co_filename && PyString_Check(co->co_filename))
                filename = PyString_AS_STRING(co->co_filename);
        if (co->co_name && PyString_Check(co->co_name))
                name = PyString_AS_STRING(co->co_name);
        PyOS_snprintf(buf, sizeof(buf),
                      "<code object %.100s at %p, file \"%.300s\", line %d>",
                      name, co, filename, lineno);
        return PyString_FromString(buf);
}


static int
code_compare(PyCodeObject *co, PyCodeObject *cp)
{
    /* daniel-edit: cannot compare two lambdas */
    return co != cp;
/*
        int cmp;
        cmp = PyObject_Compare(co->co_name, cp->co_name);
        if (cmp) return cmp;
        cmp = co->co_argcount - cp->co_argcount;
        if (cmp) return (cmp<0)?-1:1;
        cmp = co->co_nlocals - cp->co_nlocals;
        if (cmp) return (cmp<0)?-1:1;
        cmp = co->co_flags - cp->co_flags;
        if (cmp) return (cmp<0)?-1:1;
        cmp = PyObject_Compare(co->co_code, cp->co_code);
        if (cmp) return cmp;
        cmp = PyObject_Compare(co->co_consts, cp->co_consts);
        if (cmp) return cmp;
        cmp = PyObject_Compare(co->co_names, cp->co_names);
        if (cmp) return cmp;
        cmp = PyObject_Compare(co->co_varnames, cp->co_varnames);
        if (cmp) return cmp;
        cmp = PyObject_Compare(co->co_freevars, cp->co_freevars);
        if (cmp) return cmp;
        cmp = PyObject_Compare(co->co_cellvars, cp->co_cellvars);
        return cmp;
*/

}

static long
code_hash(PyCodeObject *co)
{
        long h, h0, h1, h2, h3, h4, h5, h6;
        h0 = PyObject_Hash(co->co_name);
        if (h0 == -1) return -1;
        h1 = PyObject_Hash(co->co_code);
        if (h1 == -1) return -1;
        h2 = PyObject_Hash(co->co_consts);
        if (h2 == -1) return -1;
        h3 = PyObject_Hash(co->co_names);
        if (h3 == -1) return -1;
        h4 = PyObject_Hash(co->co_varnames);
        if (h4 == -1) return -1;
        h5 = PyObject_Hash(co->co_freevars);
        if (h5 == -1) return -1;
        h6 = PyObject_Hash(co->co_cellvars);
        if (h6 == -1) return -1;
        h = h0 ^ h1 ^ h2 ^ h3 ^ h4 ^ h5 ^ h6 ^
                co->co_argcount ^ co->co_nlocals ^ co->co_flags;
        if (h == -1) h = -2;
        return h;
}



PyTypeObject PyCode_Type = {
        PyObject_HEAD_INIT(&PyType_Type)
        0,
        "code",
        sizeof(PyCodeObject),
        0,
        /*(destructor)code_dealloc*/0,       /* tp_dealloc */
        0,                              /* tp_print */
        0,                              /* tp_getattr */
        0,                              /* tp_setattr */
        (cmpfunc)code_compare,          /* tp_compare */
        (reprfunc)code_repr,            /* tp_repr */
        0,                              /* tp_as_number */
        0,                              /* tp_as_sequence */
        0,                              /* tp_as_mapping */
        (hashfunc)code_hash,            /* tp_hash */
        0,                              /* tp_call */
        0,                              /* tp_str */
        PyObject_GenericGetAttr,        /* tp_getattro */
        0,                              /* tp_setattro */
        0,                              /* tp_as_buffer */
        Py_TPFLAGS_DEFAULT,             /* tp_flags */
        code_doc,                       /* tp_doc */
        0,                              /* tp_traverse */
        0,                              /* tp_clear */
        0,                              /* tp_richcompare */
        0,                              /* tp_weaklistoffset */
        0,                              /* tp_iter */
        0,                              /* tp_iternext */
        0,                              /* tp_methods */
        code_memberlist,                /* tp_members */
        0,                              /* tp_getset */
        0,                              /* tp_base */
        0,                              /* tp_dict */
        0,                              /* tp_descr_get */
        0,                              /* tp_descr_set */
        0,                              /* tp_dictoffset */
        0,                              /* tp_init */
        0,                              /* tp_alloc */
        code_new,                       /* tp_new */
};

