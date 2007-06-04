/**
 * imgSeek ::  This file is the haar 2d transform implemented in C/C++ to
 * speed things up
 *                           -------------------
 *  begin                : Fri Jan 17 2003
 *  email                : nieder|at|mail.ru
 *  Time-stamp:            <03/05/09 21:29:35 rnc>
 *
 *  Copyright (C) 2003 Ricardo Niederberger Cabral
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef HAAR_HPP
#define HAAR_HPP

//--------------------------------------------------------------------------//
// INCLUDES
//--------------------------------------------------------------------------//

#include <queue>

//--------------------------------------------------------------------------//
// CONSTANTS AND MACROS
//--------------------------------------------------------------------------//

/**
 * Number of pixels on one side of image; required to be a power of 2.
 */
#define	NUM_PIXELS		128

/**
 * Totals pixels in a square image.
 */
#define NUM_PIXELS_SQUARED	(NUM_PIXELS * NUM_PIXELS)

/**
 * Number of Haar coeffients we retain as signature for an image.
 */ 
#define NUM_COEFS		40

#define UNIT_IS_DOUBLE

#undef ABS
#ifdef UNIT_IS_DOUBLE
#define ABS(x)	fabs(x)
typedef double Unit;
#else
#define UNIT_IS_INT
#define ABS(x)	abs(x)
typedef int Unit;
#endif

//--------------------------------------------------------------------------//
// TYPES
//--------------------------------------------------------------------------//

/**
 * An index into a signature structure.
 */
typedef int Index;

/* signature structure */
class SignatureIndex {
  public:
    /* ATTRIBUTES */
    Unit d;                     /* [f]abs(a[i]) */
    int i;                      /* index i of a[i] */

    /* PUBLIC METHODS */

    // Define an ordering on these values.
    bool operator<(const SignatureIndex & right) const {
        return d > right.d;
    }
};

typedef std::priority_queue < SignatureIndex > SignatureIndexQueue;

//--------------------------------------------------------------------------//
// METHODS
//--------------------------------------------------------------------------//

/**
 * Do the Haar tensorial 2d transform itself. Here input is RGB data
 * [0..255] in Unit arrays. Results are available in a, b, and c. Fully
 * inplace calculation; order of result is interleaved though, but we
 * don't care about that.
 */
void transform(Unit* a, Unit* b, Unit* c);

/**
 * Do the Haar tensorial 2d transform itself. Here input RGB data is in
 * unsigned char arrays ([0..255]). Results are stored into a, b, and c.
 */
void transformChar(unsigned char* channel1, unsigned char* channel2,
                   unsigned char* channels3, Unit* a, Unit* b, Unit* c);

/**
 * Determines a total of NUM_COEFS positions in the image that have the
 * largest magnitude (absolute value) in color value. Returns linearized
 * coordinates in sig1, sig2, and sig3. avgl are the [0,0] values.
 * The order of occurrence of the coordinates in sig doesn't matter.
 * Complexity is 3 x NUM_PIXELS^2 x 2log(NUM_COEFS).
 *
 * @param channel1 The first channel's data.
 * @param channel2 The second channel's data.
 * @param channel3 The third channel's data.
 */
int calcHaar(Unit* channel1, Unit* channel2, Unit* channel3,
             Index* sig1, Index* sig2, Index* sig3, double *avgl);

/**
 * Constructs a new array of doubles, of the given size. Used for exporting to
 * the Python interface.
 */
double* new_darray(int size);

/**
 * Constructs a new array of ints, of the given size. Used for exporting to the
 * Python interface.
 */
int* new_iarray(int size);

#endif
