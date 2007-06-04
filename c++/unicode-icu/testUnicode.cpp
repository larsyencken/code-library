//--------------------------------------------------------------------------//
// testUnicode.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue Mar 21 18:08:34 EST 2006
//--------------------------------------------------------------------------//

#include <iostream>
#include <unistr.h>

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    UnicodeString u("私はラースです。");
    std::cout << u << std::endl;
    std::cout << u.length() << " mora" << std::endl;
    return 0;
}

