//--------------------------------------------------------------------------//
// stats.hpp
// Lars Yencken <lljy@csse.unimelb.edu.au>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Mon Mar 27 15:49:05 2006
//
//--------------------------------------------------------------------------//

#include "jptools.hpp"

#include <map>
using namespace std;

//---------------------------------------------------------------------------//

class FreqDist
{
    private:
        int _nSamples;
        map<object, int> _frequency;

    public:
        // CONSTRUCTORS
        FreqDist();

        // ACCESSORS

        // Determine the frequency of an object's occurrence.
        int frequency(object key);

        // Determine the MLE probability of an object's occurrence.
        double probability(object key);

        // Determine the number of samples.
        int numSamples();

        // Determines the number of unique cases.
        int numUniqueSamples();

        // Provides all the unique keys.
        list keys();

        // MANIPULATORS

        // Add to the count of the given object.
        void count(object key);

        // As with count(), but takes a list of items to count.
        void countSeq(object keyList);

};

//--------------------------------------------------------------------------//
//---------------------------------------------------------------------------//

double mean(object values);

//---------------------------------------------------------------------------//

double stddev(object values);

//---------------------------------------------------------------------------//

tuple basicStats(object values);

//---------------------------------------------------------------------------//

