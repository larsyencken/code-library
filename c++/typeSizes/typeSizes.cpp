//--------------------------------------------------------------------------//
// typeSizes.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat 15 Dec 2007 09:44:57 CET
//--------------------------------------------------------------------------//

#include <iostream>
#include <python2.5/Python.h>

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    cout << "wchar_t\t\t" << sizeof(wchar_t) << endl;
    cout << "Py_UNICODE\t" << sizeof(Py_UNICODE) << endl;
    return 0;
}

