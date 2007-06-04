//--------------------------------------------------------------------------//
// signature.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed Feb  7 14:28:15 JST 2007
//--------------------------------------------------------------------------//

#include "signature.hpp"
#include <boost/python.hpp>
using namespace boost::python;

//--------------------------------------------------------------------------//

bool Signature::operator<(const Signature & right) const
{
    return score < (right.score);
}

//--------------------------------------------------------------------------//

void
export_Signature()
{
    class_<Signature>("Signature")
        .def_readwrite("id", &Signature::id)
        .def_readwrite("score", &Signature::score)
        .def_readwrite("width", &Signature::width)
        .def_readwrite("height", &Signature::height)
        .def_readonly("sig1", &Signature::sig1)
        .def_readonly("sig2", &Signature::sig2)
        .def_readonly("sig3", &Signature::sig3)
        .def_readonly("avgl", &Signature::avgl)
        ;
    return;
}

//--------------------------------------------------------------------------//
