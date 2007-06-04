//--------------------------------------------------------------------------//
// mod.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Mon Nov 27 11:13:46 JST 2006
//--------------------------------------------------------------------------//

#include <iostream>

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    int i;
    int modVal = 30;

    i = 0;
    cout << i << " % " << modVal << " = " << i % modVal << endl;

    i = 30;
    cout << i << " % " << modVal << " = " << i % modVal << endl;

    i = 40;
    cout << i << " % " << modVal << " = " << i % modVal << endl;

    i = -10; 
    cout << i << " % " << modVal << " = " << i % modVal << endl;

    i = -40;
    cout << i << " % " << modVal << " = " << i % modVal << endl;

    i = 121;
    cout << i << " % " << modVal << " = " << i % modVal << endl;

    return 0;
}

