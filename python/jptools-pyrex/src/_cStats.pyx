# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# _cStats.pyx
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Sat Jul 28 09:19:19 2007
#
#----------------------------------------------------------------------------#

"""An extension module with c versions of common statistical functions."""

#----------------------------------------------------------------------------#

from _statsExceptions import InsufficientData

#----------------------------------------------------------------------------#

cdef extern from "math.h":
    double sqrt(double x)

cdef extern from "Python.h":
    object PyTuple_New(int size)
    object PyTuple_GetItem(object l, int pos)
    object PyTuple_GET_ITEM(object l, int pos)
    int PyTuple_SetItem(object tuple, int pos, object o)
    int PyTuple_SET_ITEM(object tuple, int pos, object o)
    int PyTuple_Size(object tuple)

    object PyList_New(int size)
    object PyList_GetItem(object l, int pos)
    object PyList_GET_ITEM(object l, int pos)
    int PyList_SetItem(object l, int pos, object i)
    int PyList_SET_ITEM(object l, int pos, object i)
    int PyList_Size(object l)

    void Py_INCREF(object o)
    void Py_DECREF(object o)

#----------------------------------------------------------------------------#

def basicStats(seq):
    """Returns the mean and standard deviation of a sequence as a tuple."""
    cdef double total
    cdef double squaredTotal
    cdef int n
    cdef double meanVal
    cdef double stddevVal

    n = 0
    total = 0.0
    squaredTotal = 0.0

    for item in seq:
        total = total + item
        squaredTotal = squaredTotal + item*item
        n = n + 1

    if n < 2:
        raise InsufficientData

    meanVal = total / <double>n
    stddevVal = sqrt(
            (squaredTotal - total*total / <double>n) / (<double>n - 1)
        )

    return meanVal, stddevVal

#----------------------------------------------------------------------------#

def mean(seq):
    cdef double total
    cdef int n

    n = 0
    total = 0.0

    for item in seq:
        total = total + item
        n = n + 1

    if n < 1:
        raise InsufficientData
    
    return total / <double>n

#----------------------------------------------------------------------------#

def stddev(seq):
    """Returns the standard deviation of the numeric sequence."""
    cdef double total
    cdef double squaredTotal
    cdef int n
    cdef double meanVal
    cdef double stddevVal

    n = 0
    total = 0.0
    squaredTotal = 0.0

    for item in seq:
        total = total + item
        squaredTotal = squaredTotal + item*item
        n = n + 1

    if n < 2:
        raise InsufficientData

    stddevVal = sqrt(
            (squaredTotal - total*total / <double>n) / (<double>n - 1)
        )

    return stddevVal

#----------------------------------------------------------------------------#

def combinations(*combinationLists):
    """
    Generates a list of all possible combinations of one element from the
    first item in combinationList, one from the second, etc.

    >>> combinations([[1, 2], ['dog'], ['a', 'b']])
    [(1, 'dog', 'a'), (2, 'dog', 'a'), (1, 'dog', 'b'), (2, 'dog', 'b')]
    """
    cdef int i, j, k, l, offset, nCombs, itemLen
    cdef int lengths[100]
    cdef object o, results, item

    combinationLists = list(combinationLists)
    itemLen = len(combinationLists)

    if itemLen > 100:
        raise Exception, "Sorry, too many input lists."

    # Determine the number of combinations we'll need.
    nCombs = 1
    for i from 0 <= i < itemLen:
        o = combinationLists[i] 
        if type(o) != list:
            raise TypeError
        l = PyList_Size(o)
        lengths[i] = l
        nCombs = nCombs * l

    # results = []
    results = PyList_New(nCombs)
    for i from 0 <= i < nCombs:
        # item = ()
        item = PyTuple_New(itemLen)
        k = i
        for j from 0 <= j < itemLen:
            l = lengths[j]
            offset = k % l
            k = k / l
            #o = combinationLists[j][offset]
            o = PyList_GET_ITEM(combinationLists, j)
            Py_INCREF(o)
            o = PyList_GET_ITEM(o, offset)
            Py_INCREF(o)
            # item = item + (o,)
            PyTuple_SET_ITEM(item, j, o)

        #results.append(item)
        Py_INCREF(item)
        PyList_SET_ITEM(results, i, item)

    return results

#----------------------------------------------------------------------------#

cdef class icombinations:
    cdef int nextIndex, nCombs, itemLen
    cdef int lengths[100]
    cdef object sourceLists

    def __new__(self, *combinationLists):
        cdef int i
        cdef object o

        self.sourceLists = list(combinationLists)
        self.itemLen = len(combinationLists)
        self.nCombs = 1

        if self.itemLen > 100:
            raise ValueError, "Sorry, too many input lists."

        for i from 0 <= i < self.itemLen:
            o = PyList_GET_ITEM(self.sourceLists, i)
            Py_INCREF(o)
            if type(o) != list:
                raise TypeError
            l = PyList_Size(o)
            self.lengths[i] = l
            self.nCombs = self.nCombs * l

        self.nextIndex = 0

        return

    def __iter__(self):
        return self

    def __next__(self):
        cdef int i, l, index
        cdef object item
        cdef object o

        if self.nextIndex >= self.nCombs:
            raise StopIteration

        item = PyTuple_New(self.itemLen)

        index = self.nextIndex
        for i from 0 <= i < self.itemLen:
            l = self.lengths[i]
            offset = index % l
            index = index / l
            o = PyList_GET_ITEM(self.sourceLists, i)
            Py_INCREF(o)
            o = PyList_GET_ITEM(o, offset)
            Py_INCREF(o)
            PyTuple_SET_ITEM(item, i, o)

        self.nextIndex = self.nextIndex + 1

        return item

#----------------------------------------------------------------------------#
