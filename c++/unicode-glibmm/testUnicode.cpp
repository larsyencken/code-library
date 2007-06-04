//--------------------------------------------------------------------------//
// testUnicode.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue Mar 21 18:08:34 EST 2006
//--------------------------------------------------------------------------//

#include <iostream>
#include <glibmm.h>

using namespace Glib;

#include "functional.hpp"

//---------------------------------------------------------------------------//

int ord(float c)
{
    return (int) c;
}

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    Glib::ustring u("私はラースです。");
    std::cout << u.c_str() << std::endl;
    std::cout << u.length() << " mora" << std::endl;

    vector<float> input;
    input.push_back(0.1);
    input.push_back(3.1);
    input.push_back(-20.1);

    vector<int> output;

    mapvector<float, int>(&ord, input, output);

    return 0;
}

