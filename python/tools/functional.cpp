//--------------------------------------------------------------------------//
// functional.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 expandtab:
// Sat Mar 25 15:32:32 EST 2006
//--------------------------------------------------------------------------//

#include "functional.hpp"

//---------------------------------------------------------------------------//

tuple separate(object& method, list& inputList)
{
    list trueList;
    list falseList;

    const int listLen = len(inputList);
    for (int i = 0; i < listLen; ++i)
    {
        if (method(object(inputList[i])))
        {
            trueList.append(inputList[i]);
        }
        else
        {
            falseList.append(inputList[i]);
        }
    }

    return make_tuple(trueList, falseList);
}

const char* separate__doc__ =
    "separate(filterFn, itemSeq) -> (trueItems, falseItems)\n\
    \n\
    Separates the item sequence into two lists based on whether the result\n\
    of applying the filterFn to the item was true or false. In this way,\n\
    it's similar to filter, but it also returns the subsequence of items\n\
    which filter removes.\n\
    \n\
    @param filterFn: The fuction whose result is used to categorise each\n\
        item.\n\
    @param itemSeq: A sequence of objects.";

//---------------------------------------------------------------------------//

BOOST_PYTHON_MODULE(cFunctional)
{
    def("separate", &separate, separate__doc__);
}

//---------------------------------------------------------------------------//

