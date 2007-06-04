//--------------------------------------------------------------------------//
// signature.hpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 et tw=78:
// Wed Feb  7 11:30:01 2007
//
//--------------------------------------------------------------------------//

#ifndef SIGNATURE_HPP
#define SIGNATURE_HPP

#include "haar.hpp"

#include <map>
#include <queue>

//--------------------------------------------------------------------------//

/**
 * A comparison function between Signature objects ids. 
 */
struct cmpf {
    bool operator()(const long int s1, const long int s2) const
    {
        return s1 < s2;
    }
};

//--------------------------------------------------------------------------//

/*
 * An image signature, consisting of the positions of largest magnitude in
 * each Wavelet-transformed channel, and a unique identifier.
 */
struct Signature {
    /**
     * Define an ordering on signature objects by their score.
     */
    bool operator<(const Signature& right) const;

    /**
     * A unique identifier for the image.
     */
    long int id;

    /**
     * The Y positions with largest magnitude.
     */
    Index sig1[NUM_COEFS];

    /**
     * The I positions with the largest magnitude.
     */
    Index sig2[NUM_COEFS];

    /**
     * The Q positions with the largest magnitude.
     */
    Index sig3[NUM_COEFS];

    /**
     * The YIQ values for position [0,0].
     */
    double avgl[3];

    /**
     * The score in the most recent query.
     */
    double score;

    /**
     * The width of the image in pixels.
     */
    int width;

    /**
     * The height of the image in pixels.
     */
    int height;
};

/**
 * Derivative classes.
 */
typedef std::map < const long int, Signature *, cmpf >::iterator sigIterator;
typedef std::map < const long int, Signature *, cmpf > SignatureMap;
typedef std::priority_queue < Signature > SignatureQueue;

/**
 * Export the definitions for this class to Boost Python.
 */
void export_Signature();

//--------------------------------------------------------------------------//

#endif
