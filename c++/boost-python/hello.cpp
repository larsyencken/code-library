//  Copyright Joel de Guzman 2002-2004. Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt 
//  or copy at http://www.boost.org/LICENSE_1_0.txt)
//  Hello World Example from the tutorial
//  [Joel de Guzman 10/9/2002]

#include <glibmm.h>
#include <string>

using namespace Glib;

typedef std::wstring py_ustring;

py_ustring greet()
{
   ustring orig("どうぞよろしくおねがいします。");

   py_ustring ws(orig.begin(), orig.end());

   return ws;
}


#include <boost/python/module.hpp>
#include <boost/python/def.hpp>
using namespace boost::python;

BOOST_PYTHON_MODULE(hello)
{
    def("greet", greet);
}

