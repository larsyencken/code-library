//--------------------------------------------------------------------------//
// boolTest.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Sun Oct  7 20:50:28 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    cout << "1 is " << (1 ? "true" : "false") << endl;
    cout << "0 is " << (0 ? "true" : "false") << endl;
    return 0;
}

