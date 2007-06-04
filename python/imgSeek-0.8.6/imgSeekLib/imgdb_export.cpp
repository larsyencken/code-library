//--------------------------------------------------------------------------//
// imgdb_export.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Tue Feb  6 16:49:58 2007
//
//--------------------------------------------------------------------------//

#include "imageDatabase.hpp"
#include "signature.hpp"
#include "listLong.hpp"

#include <boost/python.hpp>
using namespace boost::python;

//--------------------------------------------------------------------------//

BOOST_PYTHON_MODULE(imgdb)
{
    export_ImageDatabase();
    export_Signature();
    export_ListLong();
}

//--------------------------------------------------------------------------//
