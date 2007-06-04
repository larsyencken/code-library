//--------------------------------------------------------------------------//
// doNothing.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Tue May 16 13:08:56 EST 2006
//--------------------------------------------------------------------------//

#include <iostream>

using namespace std;

#include <xercesc/util/PlatformUtils.hpp>
XERCES_CPP_NAMESPACE_USE

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    cout << "Initializing Xerces" << endl;
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& toCatch)
    {
        cerr << "Encountered an error!" << endl;
        return 1;
    }

    cout << "Terminating Xerces" << endl;
    XMLPlatformUtils::Terminate();

    cout << "Exitted without errors" << endl;
    return 0;
}

