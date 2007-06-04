//--------------------------------------------------------------------------//
// stats.cpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Mon Mar 27 15:49:05 2006
//
//--------------------------------------------------------------------------//

#include "stats.hpp"

#include <math.h>

//--------------------------------------------------------------------------//

FreqDist::FreqDist()
    :
_nSamples(0)
{
}

const char* FreqDist__init__doc__ =
    "__init__(self) -> instance\n\
    \n\
    Create a new instance, with zero counts to start with.";

//---------------------------------------------------------------------------//

int FreqDist::frequency(object key)
{
    return _frequency[key];
}

const char* frequency__doc__ =
    "frequency(self, key) -> n\n\
    \n\
    Returns the the number of times the given item has occurred. If the\n\
    item has not been seen before, it will simply have a count of zero.";

//---------------------------------------------------------------------------//

double FreqDist::probability(object key)
{
    return ((double) _frequency[key]) / (double) _nSamples;
}

const char* probability__doc__ =
    "probability(self, key) -> p\n\
    \n\
    Returns the MLE probability of the given key, based on current counts.";

//---------------------------------------------------------------------------//

int FreqDist::numSamples()
{
    return _nSamples;
}

const char* numSamples__doc__ =
    "numSamples(self, key) -> n\n\
    \n\
    Returns the total number of samples counted by this instance.";

//---------------------------------------------------------------------------//

int FreqDist::numUniqueSamples()
{
    return _frequency.size();
}

const char* numUniqueSamples__doc__ =
    "numUniqueSamples(self) -> n\n\
    \n\
    Returns the number of unique samples counted by this instance.";

//---------------------------------------------------------------------------//

list FreqDist::keys()
{
    list outputKeys;

    for (map<object, int>::iterator iter = _frequency.begin();
            iter != _frequency.end(); ++iter)
    {
        outputKeys.append(iter->first);
    }

    return outputKeys;
}

const char* keys__doc__ =
    "keys(self) -> list(key)\n\
    \n\
    Returns a list of all unique items counted.";

//---------------------------------------------------------------------------//

void FreqDist::count(object key)
{
    ++_frequency[key];
    ++_nSamples;
    return;
}

const char* count__doc__ =
    "count(self, key) -> None\n\
    \n\
    Increements the number of times this item has been seen.";

//---------------------------------------------------------------------------//

void FreqDist::countSeq(object keySeq)
{
    const int keySeqLen = len(keySeq);

    for (int i = 0; i < keySeqLen; ++i)
    {
        _frequency[keySeq[i]] += 1;
    }
    _nSamples += keySeqLen;

    return;
}

const char* countSeq__doc__ =
    "countSeq(keySeq) -> None\n\
    \n\
    Count every item in the given sequence.";

//--------------------------------------------------------------------------//
//---------------------------------------------------------------------------//

double mean(object values)
{
    const int numValues = len(values);

    double sum = 0;
    for (int i = 0; i < numValues; ++i)
    {
        sum += extract<double>(values[i]);
    }

    return sum / (double) numValues;
}

const char* mean__doc__ = 
    "mean(seq) -> value \n\
    \n\
    Returns the mean of the given sequence.";

//---------------------------------------------------------------------------//

double stddev(object values)
{
    const int numValues = len(values);

    if (numValues < 2)
    {
        throw "Can't calculate stddev of less than 2 values.";
    }

    double sum = 0;
    double sumSquared = 0;
    for (int i = 0; i < numValues; ++i)
    {
        double val = extract<double>(values[i]);
        sum += val;
        sumSquared += val*val;
    }

    return sqrt((sumSquared - sum*sum/(double)(numValues))/
            (double)(numValues-1));
}

const char* stddev__doc__ = 
    "stddev(seq) -> value \n\
    \n\
    Calculates the standard deviation of the given sequence.";

//---------------------------------------------------------------------------//

tuple basicStats(object values)
{
    const int numValues = len(values);

    double sum = 0;
    double sumSquared = 0;
    for (int i = 0; i < numValues; ++i)
    {
        double val = extract<double>(values[i]);
        sum += val;
        sumSquared += val*val;
    }

    double stddev = sqrt((sumSquared - sum*sum/(double)(numValues)) / 
            (double)(numValues-1));
    double mean = sum / (double) numValues;
    return make_tuple(mean, stddev);
}

const char* basicStats__doc__ = 
    "basicStats(seq) -> (mean, stddev)\n\
    \n\
    Calculates the mean and the standard deviation of a given sequence,\n\
    returning them as a tuple.""";

//---------------------------------------------------------------------------//

BOOST_PYTHON_MODULE(cStats)
{
    class_<FreqDist>("FreqDist", FreqDist__init__doc__)
        .def("keys", &FreqDist::keys, keys__doc__)
        .def("countSeq", &FreqDist::countSeq, countSeq__doc__)
        .def("count", &FreqDist::count, count__doc__) 
        .def("numSamples", &FreqDist::numSamples, numSamples__doc__)
        .def("numUniqueSamples", &FreqDist::numUniqueSamples,
                numUniqueSamples__doc__)
        .def("frequency", &FreqDist::frequency, frequency__doc__)
        .def("probability", &FreqDist::probability, probability__doc__);
    
    def("mean", &mean, mean__doc__);
    def("stddev", &stddev, stddev__doc__);
    def("basicStats", &basicStats, basicStats__doc__);
}

//--------------------------------------------------------------------------//

