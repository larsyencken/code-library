//--------------------------------------------------------------------------//
// pyString.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Mon Apr 24 11:50:42 2006
//
//--------------------------------------------------------------------------//

#include <boost/python.hpp>

using namespace boost::python;

//--------------------------------------------------------------------------//

void cppQuadraticTest(const int size)
{
    str beginningData("Here's our sample test string!");

    str s;
    for (int i = 0; i < size; ++i)
    {
        s = extract<str>(s + beginningData);
    }

    return;
}

//--------------------------------------------------------------------------//

void cppConcatTest(const int size)
{
    str data("Here's our sample test string!");
    str s("Lets start with another small string.");

    for (int i = 0; i < size; ++i)
    {
        str p(data + s);
        str q(p + data);
    }

    return;
}

//--------------------------------------------------------------------------//

str interfaceTest(str s)
{
    str data("Here's our sample test string!");

    return extract<str>(data + s);
}

//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//

BOOST_PYTHON_MODULE(pyString)
{
    def("cppQuadraticTest", &cppQuadraticTest);
    def("cppConcatTest", &cppConcatTest);
    def("interfaceTest", &interfaceTest);
}

//--------------------------------------------------------------------------//

