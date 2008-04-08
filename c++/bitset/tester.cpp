//--------------------------------------------------------------------------//
// tester.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed Sep  5 22:23:50 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>

#include "bitset.hpp"

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    cout << "Initializing bitset(100)" << endl;
    bitset b(100);

    cout << "Testing initial state" << endl;
    for (int i = 0; i < 100; i++) {
        if (b.test(i)) {
            cerr << "Initialization failure: " << i << " set" << endl;
            return 1;
        }
    }

    cout << "Adding 10 and 20" << endl;
    b.add(10);
    b.add(20);

    cout << "Testing all set members:" << endl;
    for (int i = 0; i < 100; i++) {
        if (b.test(i)) {
            cout << i << endl;
        }
    }

    return 0;
}

