/***************************************************************************
    imgSeek ::  Haar 2d transform implemented in C/C++ to speed things up
                             -------------------
    begin                : Fri Jan 17 2003
    email                : nieder|at|mail.ru
    Time-stamp:            <05/01/30 19:58:56 rnc>
    ***************************************************************************
    *    Wavelet algorithms, metric and query ideas based on the paper        *
    *    Fast Multiresolution Image Querying                                  *
    *    by Charles E. Jacobs, Adam Finkelstein and David H. Salesin.         *
    *    <http://www.cs.washington.edu/homes/salesin/abstracts.html>          *
    ***************************************************************************

    Copyright (C) 2003 Ricardo Niederberger Cabral

    Clean-up and speed-ups by Geert Janssen <geert at ieee.org>, Jan 2006:
    - introduced names for various `magic' numbers
    - made coding style suitable for Emacs c-mode
    - expressly doing constant propagation by hand (combined scalings)
    - preferring pointer access over indexed access of arrays
    - introduced local variables to avoid expression re-evaluations
    - took out all dynamic allocations
    - completely rewrote calcHaar and eliminated truncq()
    - better scheme of introducing sqrt(0.5) factors borrowed from
      FXT package: author Joerg Arndt, email: arndt@jjj.de,
      http://www.jjj.de/
    - separate processing per array: better cache behavior
    - do away with all scaling; not needed except for DC component

    To do:
    - the whole Haar transform should be done using fixpoints

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

//--------------------------------------------------------------------------//

/* imgSeek Includes */
#include "haar.hpp"

/* C Includes */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

//--------------------------------------------------------------------------//

double* new_darray(int size)
{
    return (double*) malloc(size * sizeof(double));
}

//--------------------------------------------------------------------------//

int* new_iarray(int size)
{
    return (int*) malloc(size * sizeof(int));
}

//--------------------------------------------------------------------------//

// RGB -> YIQ colorspace conversion; Y luminance, I,Q chrominance.
// If RGB in [0..255] then Y in [0..255] and I,Q in [-127..127].
#define RGB_2_YIQ(a, b, c) \
  do { \
    int i; \
    \
    for (i = 0; i < NUM_PIXELS_SQUARED; i++) { \
      Unit Y, I, Q; \
      \
      Y = 0.299 * a[i] + 0.587 * b[i] + 0.114 * c[i]; \
      I = 0.596 * a[i] - 0.275 * b[i] - 0.321 * c[i]; \
      Q = 0.212 * a[i] - 0.523 * b[i] + 0.311 * c[i]; \
      a[i] = Y; \
      b[i] = I; \
      c[i] = Q; \
    } \
  } while(0)

//--------------------------------------------------------------------------//

/**
 * Do the Haar tensorial 2d transform itself. Here input is RGB data [0..255]
 * in Unit arrays Computation is (almost) in-situ.
 */
static void haar2D(Unit a[])
{
    int i;
    Unit t[NUM_PIXELS >> 1];

    // scale by 1/sqrt(128) = 0.08838834764831843:
    /*
       for (i = 0; i < NUM_PIXELS_SQUARED; i++)
       a[i] *= 0.08838834764831843;
     */

    // Decompose rows:
    for (i = 0; i < NUM_PIXELS_SQUARED; i += NUM_PIXELS) {
        int h, h1;
        Unit C = 1;

        for (h = NUM_PIXELS; h > 1; h = h1) {
            int j1, j2, k;

            h1 = h >> 1;        // h = 2*h1
            C *= 0.7071;        // 1/sqrt(2)
            for (k = 0, j1 = j2 = i; k < h1; k++, j1++, j2 += 2) {
                int j21 = j2 + 1;

                t[k] = (a[j2] - a[j21]) * C;
                a[j1] = (a[j2] + a[j21]);
            }
            // Write back subtraction results:
            memcpy(a + i + h1, t, h1 * sizeof(a[0]));
        }
        // Fix first element of each row:
        a[i] *= C;              // C = 1/sqrt(NUM_PIXELS)
    }

    // scale by 1/sqrt(128) = 0.08838834764831843:
    /*
       for (i = 0; i < NUM_PIXELS_SQUARED; i++)
       a[i] *= 0.08838834764831843;
     */

    // Decompose columns:
    for (i = 0; i < NUM_PIXELS; i++) {
        Unit C = 1;
        int h, h1;

        for (h = NUM_PIXELS; h > 1; h = h1) {
            int j1, j2, k;

            h1 = h >> 1;
            C *= 0.7071;        // 1/sqrt(2) = 0.7071
            for (k = 0, j1 = j2 = i; k < h1;
                 k++, j1 += NUM_PIXELS, j2 += 2 * NUM_PIXELS) {
                int j21 = j2 + NUM_PIXELS;

                t[k] = (a[j2] - a[j21]) * C;
                a[j1] = (a[j2] + a[j21]);
            }
            // Write back subtraction results:
            for (k = 0, j1 = i + h1 * NUM_PIXELS; k < h1;
                 k++, j1 += NUM_PIXELS)
                a[j1] = t[k];
        }
        // Fix first element of each column:
        a[i] *= C;
    }

    return;
}

//--------------------------------------------------------------------------//

void transform(Unit a[], Unit b[], Unit c[])
{
    RGB_2_YIQ(a, b, c);

    haar2D(a);
    haar2D(b);
    haar2D(c);

    /* Reintroduce the skipped scaling factors: */
    a[0] /= 256 * 128;
    b[0] /= 256 * 128;
    c[0] /= 256 * 128;
}

//--------------------------------------------------------------------------//

void
transformChar(unsigned char* channel1, unsigned char* channel2,
              unsigned char* channel3, Unit* a, Unit* b, Unit* c)
{
    // Copy from unsigned arrays to Unit arrays.
    Unit *p = a;
    Unit *q = b;
    Unit *r = c;

    for (int i = 0; i < NUM_PIXELS_SQUARED; i++) {
        *p++ = *channel1++;
        *q++ = *channel2++;
        *r++ = *channel3++;
    }

    // Transform as usual.
    transform(a, b, c);
}

//--------------------------------------------------------------------------//

/**
 * Find the NUM_COEFS largest numbers in channelData[] (in magnitude that is)
 * and store their indices in sigIndex[].
 */
inline static void get_m_largest(Unit* channelData, Index* sigIndex)
{
    int count, i;
    SignatureIndex value;
    SignatureIndexQueue valueQueue; // dynamic priority queue

    // Could skip i=0: goes into separate avgl

    // Fill up the bounded queue. (Assuming NUM_PIXELS_SQUARED > NUM_COEFS)
    for (i = 1; i < NUM_COEFS + 1; i++) {
        value.i = i;
        value.d = ABS(channelData[i]);
        valueQueue.push(value);
    }
    // Queue is full (size is NUM_COEFS)

    for ( /*i = NUM_COEFS+1 */ ; i < NUM_PIXELS_SQUARED; i++) {
        value.d = ABS(channelData[i]);

        if (value.d > valueQueue.top().d) {
            // Make room by dropping smallest entry:
            valueQueue.pop();
            // Insert val as new entry:
            value.i = i;
            valueQueue.push(value);
        }
        // else discard: do nothing
    }

    // Empty the (non-empty) queue and fill-in sigIndex:
    count = 0;
    do {
        int t;
        value = valueQueue.top();
        t = (channelData[value.i] <= 0);    /* t = 0 if pos else 1 */
        /* i - 0 ^ 0 = i; i - 1 ^ 0b111..1111 = 2-compl(i) = -i */
        sigIndex[count++] = (value.i - t) ^ -t; // never 0
        valueQueue.pop();
    } while (!valueQueue.empty());

    assert(count == NUM_COEFS);
}

//--------------------------------------------------------------------------//

int
calcHaar(Unit * channelData1, Unit * channelData2, Unit * channelData3,
         Index * sig1, Index * sig2, Index * sig3, double *avgl)
{
    avgl[0] = channelData1[0];
    avgl[1] = channelData2[0];
    avgl[2] = channelData3[0];

    // Color channel 1:
    get_m_largest(channelData1, sig1);

    // Color channel 2:
    get_m_largest(channelData2, sig2);

    // Color channel 3:
    get_m_largest(channelData3, sig3);

    return 1;
}

//--------------------------------------------------------------------------//
