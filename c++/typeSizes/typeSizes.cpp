//--------------------------------------------------------------------------//
// typeSizes.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat 15 Dec 2007 09:44:57 CET
//--------------------------------------------------------------------------//

#include <iostream>
#include <python2.5/Python.h>

using namespace std;

typedef struct composite
{
    char p;
    char* p_ptr;
} Composite;

typedef struct __attribute__ ((__packed__)) packed
{
    char p;
    char* p_ptr;
} Packed;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    cout << "short\t\t" << sizeof(short) << endl;
    cout << "int\t\t" << sizeof(int) << endl;
    cout << "long\t\t" << sizeof(long) << endl;
    cout << "long long\t" << sizeof(long long) << endl;
    cout << "void*\t\t" << sizeof(void*) << endl;
    cout << "char\t\t" << sizeof(char) << endl;
    cout << "wchar_t\t\t" << sizeof(wchar_t) << endl;
    cout << "Py_UNICODE\t" << sizeof(Py_UNICODE) << endl;
    cout << "Composite\t" << sizeof(Composite) << endl;
    cout << "Packed\t\t" << sizeof(Packed) << endl;
    return 0;
}

