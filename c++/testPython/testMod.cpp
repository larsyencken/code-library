//--------------------------------------------------------------------------//
// testMod.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Tue 18 Dec 2007 06:42:11 CET
//--------------------------------------------------------------------------//

#include <python2.5/Python.h>

//--------------------------------------------------------------------------//

PyObject* getString(PyObject* pSelf, PyObject* pArgs)
{
    const wchar_t example[] = {0x6f22, 0x5b57, 0x304c, 0x8aad, 0x3081, 0x307e, 0x3059, 0x3002, L'\0'};
    return PyUnicode_FromUnicode((Py_UNICODE*) example, wcslen(example));
}

static PyObject* _module = NULL;

static struct PyMethodDef _exportedMethods[] = {
    {"getString", getString, METH_VARARGS,
            "Fetch a unicode string."},
    {0, 0, 0, 0}
};

void inittestMod(void)
{
    _module = Py_InitModule("testMod", _exportedMethods);
}
