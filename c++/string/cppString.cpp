//--------------------------------------------------------------------------//
// cppString.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Mon Apr 24 11:43:46 2006
//
//--------------------------------------------------------------------------//

#include <boost/python.hpp>
#include <string>

using namespace boost::python;
using namespace std;

//--------------------------------------------------------------------------//

void cppQuadraticTest(const int size)
{
    const char* beginningData = "Here's our sample test string!";

    string s;
    for (int i = 0; i < size; ++i)
    {
        s = s + beginningData;
    }

    return;
}

//--------------------------------------------------------------------------//

void cppConcatTest(const int size)
{
    string data("Here's our sample test string!");
    string s("Lets start with another small string.");

    for (int i = 0; i < size; ++i)
    {
        string p(data + s);
        string q(p + data);
    }

    return;
}

//--------------------------------------------------------------------------//

string interfaceTest(string s)
{
    string data("Here's our sample test string!");

    return data + s;
}

//--------------------------------------------------------------------------//

BOOST_PYTHON_MODULE(cppString)
{
    def("cppQuadraticTest", &cppQuadraticTest);
    def("cppConcatTest", &cppConcatTest);
    def("interfaceTest", &interfaceTest);
}

//--------------------------------------------------------------------------//

