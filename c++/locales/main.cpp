//--------------------------------------------------------------------------//
// main.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed  5 Dec 2007 20:30:08 CET
//--------------------------------------------------------------------------//

#include <iostream>
#include <locale.h>

using namespace std;

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    setlocale(LC_CTYPE, "en_US.UTF-8");
    //wcout.imbue(locale("en_US.UTF-8"));
    wcout << L"漢字が読めます。" << endl;
    return 0;
}

