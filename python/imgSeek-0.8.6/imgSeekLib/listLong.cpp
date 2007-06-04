//--------------------------------------------------------------------------//
// listLong.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Wed Feb  7 12:07:29 JST 2007
//--------------------------------------------------------------------------//

#include "listLong.hpp"

#include <boost/python.hpp>

using namespace boost::python;

//--------------------------------------------------------------------------//

int ListLong__size(ListLong& li)
{
    return li.size();
}

//--------------------------------------------------------------------------//

long int ListLong__pop_front(ListLong& li)
{
    long int a = li.front();
    li.pop_front();
    return a;
}

//--------------------------------------------------------------------------//

void ListLong__push_front(ListLong& li, long int i)
{
    li.push_front(i);
}

//--------------------------------------------------------------------------//

void ListLong__push_back(ListLong& li, long int i)
{
    li.push_back(i);
}

//--------------------------------------------------------------------------//

int ListListLong__size(ListListLong& li)
{
    return li.size();
}

//--------------------------------------------------------------------------//

ListLong ListListLong__pop_front(ListListLong& li)
{
    ListLong a = li.front();
    li.pop_front();
    return a;
}

//--------------------------------------------------------------------------//

void ListListLong__push_front(ListListLong& li, const ListLong& i)
{
    li.push_front(i);
}

//--------------------------------------------------------------------------//

void ListListLong__push_back(ListListLong& li, const ListLong& i)
{
    li.push_back(i);
}

//--------------------------------------------------------------------------//

void export_ListLong()
{
    class_<ListLong>("ListLong")
        .def("__iter__", iterator<ListLong>())
        .def("push_back", ListLong__push_back)
        .def("push_front", ListLong__push_front)
        .def("pop_front", ListLong__pop_front)
        .def("__len__", ListLong__size)
        ;

    class_<ListListLong>("ListListLong")
        .def("__iter__", iterator<ListListLong>())
        .def("push_back", ListListLong__push_back)
        .def("push_front", ListListLong__push_front)
        .def("pop_front", ListListLong__pop_front)
        .def("__len__", ListListLong__size)
        ;

    return;
}

