#ifndef _MODSUPPORT_H_

PyAPI_FUNC(int) PyArg_Parse(PyObject *, char *, ...);
PyAPI_FUNC(int) PyArg_ParseTuple(PyObject *, char *, ...);
PyAPI_FUNC(int) PyArg_ParseTupleAndKeywords(PyObject *, PyObject *,
                                                  char *, char **, ...);
PyAPI_FUNC(int) PyArg_UnpackTuple(PyObject *, char *, int, int, ...);
PyAPI_FUNC(PyObject *) Py_BuildValue(char *, ...);

PyAPI_FUNC(int) PyArg_VaParse(PyObject *, char *, va_list);
PyAPI_FUNC(PyObject *) Py_VaBuildValue(char *, va_list);

PyAPI_FUNC(int) PyModule_AddObject(PyObject *, char *, PyObject *);
PyAPI_FUNC(int) PyModule_AddIntConstant(PyObject *, char *, long);
PyAPI_FUNC(int) PyModule_AddStringConstant(PyObject *, char *, char *);

PyAPI_FUNC(PyObject*) Py_InitModule(const char* name, PyMethodDef methods[]);


Scheme_Object* scheme_initialize(Scheme_Env* env);
Scheme_Object* scheme_reload(Scheme_Env* env);
Scheme_Object* scheme_module_name();

#endif // _MODSUPPORT_H_
