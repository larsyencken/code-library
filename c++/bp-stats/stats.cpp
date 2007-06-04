//--------------------------------------------------------------------------//
// stats.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Mon Mar 27 15:49:05 2006
//
//--------------------------------------------------------------------------//

#include <boost/python/module.hpp>
#include <boost/python/def.hpp>
#include <boost/python/list.hpp>
#include <boost/python/tuple.hpp>
#include <boost/python/extract.hpp>
#include <vector>

#define len(X)    extract<int>(X.attr("__len__")())

using namespace boost::python;
using namespace std;

typedef vector<object> objectVector;

//---------------------------------------------------------------------------//

void copyList(list input, objectVector& result)
{
    result.clear();
    const int inputLen = len(input);
    result.reserve(inputLen);

    for (int i = 0; i < inputLen; ++i)
    {
        result[i] = input[i];
    }

    return;
}

//---------------------------------------------------------------------------//

//--------------------------------------------------------------------------//
// combinations(inputPotentials):
// 
list combinations(list inputPotentials)
{
    // Copy the input to vectors
    const int inputVectorLen = len(inputPotentials);
    vector<objectVector> inputVector;
    {
        for (int i = 0; i < inputVectorLen; ++i)
        {
            inputVector.push_back(objectVector());
            list inputCopy = extract<list>(object(inputPotentials[i]));
            const int inputLen = len(inputCopy);

            for (int j = 0; j < inputLen; ++j)
            {
                inputVector[i].push_back(object(inputCopy[j]));
            }
        }
    }

    // Use the vectors to calculate the output
    list combos;
    list nextCombos;
    combos.append(make_tuple());
    int comboLen = 1;

    for (vector<objectVector>::iterator iter = inputVector.begin();
            iter != inputVector.end(); ++iter)
    {
        nextCombos = list();
        const int itemListLen = iter->size();
        for (int i = 0; i < comboLen; ++i)
        {
            const tuple& partialCombo = extract<tuple>(combos[i]);
            for (int n = 0; n < itemListLen; n++)
            {
                nextCombos.append(partialCombo + make_tuple((*iter)[n]));
            }
        }
        comboLen *= itemListLen;
        combos = nextCombos;
    }

    return combos;
}

//---------------------------------------------------------------------------//

//{
//    // Construct a reversed copy
//    list inputCopy;
//    {
//        const int inputLen = len(inputPotentials);
//        for (int i = 0; i < inputLen; ++i)
//        {
//            inputCopy.append(inputPotentials[inputLen-i-1]);
//        }
//    }
//
//    list combos;
//    {
//        list firstList = extract<list>(inputCopy.pop());
//        const int firstListLen = len(firstList);
//        for (int i = 0; i < firstListLen; ++i)
//        {
//            combos.append(make_tuple(firstList[i]));
//        }
//    }
//
//    while (inputCopy)
//    {
//        list nextCombos;
//        const list itemList = extract<list>(inputCopy.pop());
//
//        const int combosLen = len(combos);
//        const int itemListLen = len(itemList);
//        for (int i = 0; i < combosLen; ++i)
//        {
//            for (int n = 0; n < itemListLen; ++n)
//            {
//                nextCombos.append(combos[i] + make_tuple(itemList[n]));
//            }
//        }
//
//        combos = nextCombos;
//    }
//
//    return combos;
//}
//{
//    const int listLen = len(inputPotentials);
//
//    vector<int> inputLengths(listLen);
//    int numPotentials = 1;
//    for (int i = 0; i < listLen; ++i)
//    {
//        inputLengths[i] = len(inputPotentials[i]);
//        numPotentials *= inputLengths[i];
//    }
//
//    list output;
//    for (int i = 0; i < numPotentials; ++i)
//    {
//        tuple thisCase = make_tuple();
//        int index = i;
//        int subIndex = 0;
//        for (int j = 0; j < listLen; ++j)
//        {
//            subIndex = index % inputLengths[j];
//            index /= inputLengths[j];
//            thisCase += make_tuple(inputPotentials[j][subIndex]);
//        }
//
//        output.append(thisCase);
//    }
//
//    return output;
//}

//--------------------------------------------------------------------------//

BOOST_PYTHON_MODULE(stats)
{
    def("combinations", &combinations);
}

//--------------------------------------------------------------------------//

