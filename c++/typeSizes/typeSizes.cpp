//--------------------------------------------------------------------------//
// typeSizes.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sat 15 Dec 2007 09:44:57 CET
//--------------------------------------------------------------------------//

#include <iostream>
#include <python2.5/Python.h>
#include <string>

using namespace std;

struct Composite {
    char p;
    char* p_ptr;
};

struct __attribute__ ((__packed__)) Packed {
    char p;
    char* p_ptr;
};

struct Terrible {
    char a;
    double b;
    int c;
};
struct LessTerrible {
    double b;
    char a;
    int c;
};
struct __attribute__ ((__packed__)) TerriblePacked {
    char a;
    double b;
    int c;
};

struct TaraA {
    short a;
    short b;
    std::string c;
};

struct TaraB {
    short a;
    std::string c;
    short b;
};

struct __attribute__ ((__packed__)) TaraAPacked {
    short a;
    short b;
    std::string c;
};

struct __attribute__ ((__packed__)) TaraBPacked {
    short a;
    std::string c;
    short b;
};

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
    cout << "TaraA\t\t" << sizeof(TaraA) << endl;
    cout << "TaraAPacked\t\t" << sizeof(TaraAPacked) << endl;
    cout << "TaraB\t\t" << sizeof(TaraB) << endl;
    cout << "TaraBPacked\t\t" << sizeof(TaraBPacked) << endl;
    cout << "Terrible\t\t" << sizeof(Terrible) << endl;
    cout << "TerriblePacked\t\t" << sizeof(TerriblePacked) << endl;
    cout << "LessTerrible\t\t" << sizeof(LessTerrible) << endl;
    return 0;
}

