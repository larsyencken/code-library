# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# cStats.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Mon May 16 14:18:59 EST 2005
#
#----------------------------------------------------------------------------#

"""
This module is responsible for any general combinatoric methods, in
particular determining possible combinations of input.
"""

#----------------------------------------------------------------------------#

from itertools import izip, ifilter
from math import sqrt

from _statsExceptions import InsufficientData
from _cStats import *

#----------------------------------------------------------------------------#

_eps = 1e-8

#----------------------------------------------------------------------------#
# PUBLIC METHODS
#----------------------------------------------------------------------------#

def binsByData(data, n):
    """
    Puts the data into n sorted bins. Where n does not divide the length
    of the data directly, distributes the remainder as evenly as possible.
    Returns an iterator over the bins.

    @param data: A sequence of data. 
    """
    data.sort()

    assert n <= len(data), "Can't split a group more ways than its length"

    itemsPerGroup, remainder = divmod(len(data), n)

    startAt = 0
    for i in xrange(n):
        endAt = startAt + itemsPerGroup

        if remainder > 0:
            endAt += 1
            remainder -= 1

        yield (startAt, endAt), data[startAt:endAt]

        startAt = endAt

    return

#----------------------------------------------------------------------------#

def binsByIncrement(data, inc, keyMethod=lambda x: x[0]):
    """
    Calculates bins by range increment. Assumes data is a sequence of
    tuples, where the first tuple is the one whose range is divided up.
    """
    data = list(data)
    data.sort()

    # add _eps to the end of the range to ensure we capture that object
    startRange = keyMethod(data[0])
    endRange = keyMethod(data[-1]) + _eps

    for binStart in frange(startRange, endRange, inc):
        binEnd = binStart + inc

        binData = [x for x in data if keyMethod(x) >= binStart and \
                keyMethod(x) < binEnd]

        yield (binStart, binEnd), binData

    return

#----------------------------------------------------------------------------#

def binsByRange(data, n, keyMethod=lambda x: x[0]):
    """
    Calculates bins by range. Assumes data is a sequence of tuples, where
    the first tuple is the one whose range is divided up.
    """
    data = list(data)
    data.sort()

    startRange = keyMethod(data[0])
    endRange = keyMethod(data[-1])
    binSize = (endRange - startRange)/float(n)

    for i in xrange(n):
        binStart = startRange + i*binSize
        binEnd = startRange + (i+1)*binSize

        # add eps to the size of the last bin to ensure we capture that
        # object
        if i == (n-1):
            useBinEnd = binEnd + _eps
        else:
            useBinEnd = binEnd

        binData = [x for x in data if keyMethod(x) >= binStart and \
                keyMethod(x) < useBinEnd]

        yield (binStart, binEnd), binData

    return

#----------------------------------------------------------------------------#

def uniqueTuples(inputList, n=2):
    """
    Generates all subsets of the given list with n items, returned as a list.
    Instead of repeating subsets, and ordering on the items is used to
    generate only unique combinations.

    >>> uniqueTuples([1, 2, 3])
    [(1, 2), (2, 3), (1, 3)]
    """
    def filterFn(x):
        for i in xrange(n-1):
            if x[i] >= x[i+1]:
                return False
        else:
            return True

    return filter(filterFn, combinations(*[inputList]*n))

#----------------------------------------------------------------------------#

def iuniqueTuples(inputList, n=2):
    """An iterator version of uniqueTuples."""
    def filterFn(x):
        for i in xrange(n-1):
            if x[i] >= x[i+1]:
                return False
        else:
            return True

    return ifilter(filterFn, icombinations(*[inputList]*n))

#----------------------------------------------------------------------------#

def combinationSeqs(*combinationList):
    """
    As with combinations() above, except that each potential item is
    assumed to already be in sequence form.

    >>> combinationSeqs([ [(1, 2), (3, 4)], [('dog',), ('cat',)] ])
    [(1, 2, 'dog'), (3, 4, 'dog'), (1, 2, 'cat'), (3, 4, 'cat')]
    """
    return list(icombinationSeqs(*combinationList))

#----------------------------------------------------------------------------#

def icombinationSeqs(*combinationList):
    """
    As for combinations(), but returns an iterator.

        >>> list(icombinationSeqs([ [(1, 2), (3, 4)], [('dog',), ('cat',)] ]))
        [(1, 2, 'dog'), (3, 4, 'dog'), (1, 2, 'cat'), (3, 4, 'cat')]
    """
    for seqCombs in icombinations(*combinationList):
        result = []
        for seq in seqCombs:
            result.extend(seq)
        yield tuple(result)
    return

#----------------------------------------------------------------------------#

def segmentCombinations(gString):    
    """
    Determines the possible segment combinations based on the grapheme
    string alone, in particular due to kanji placement. For example::

        >>> segmentCombinations('ab')
        [('a', 'b'), ('ab',)]
    
    """
    # start out with just the first character
    segmentations = [[gString[0]]]

    # add remaining characters one by one
    for char in gString[1:]: 
        nextSegmentationRound = []
        for segment in segmentations:
            # the new char in its own segment
            nextSegmentationRound.append(segment + [char])

            # the new char as part of the previous segment
            segment[-1] += char
            nextSegmentationRound.append(segment)

        segmentations = nextSegmentationRound
    
    segmentations = map(tuple, segmentations)

    return segmentations

#----------------------------------------------------------------------------#

def isegmentCombinations(gString):
    """
    As for segmentCombinations(), but returns an iterator.

        >>> list(sorted(isegmentCombinations('ab')))
        [('a', 'b'), ('ab',)]

    Note that the order may be different.
    """
    if not gString:
        return

    gStringSize = len(gString)
    nCombs = 2**(gStringSize-1)

    for i in xrange(nCombs):
        currentComb = [gString[0]]
        for j in xrange(1,gStringSize):
            i, hasBoundary = divmod(i, 2)
            if hasBoundary:
                currentComb.append(gString[j])
            else:
                currentComb[-1] += gString[j]
        yield tuple(currentComb)

    return

#----------------------------------------------------------------------------#

def kappa(responsesA, responsesB, potentialResponses=None):
    """
    Assuming matched list of response values for each rater, determine
    their kappa value using Cohen's method.
    """
    from nltk_lite.probability import FreqDist

    if not responsesA or not responsesB:
        raise InsufficientData, "Need at least one response to calculate kappa"

    if len(responsesA) != len(responsesB):
        raise ValueError, "Response vectors are different lengths"
    
    # Build rater bias distributions.
    biasA = FreqDist()
    for sample in responsesA:
        biasA.inc(sample)

    biasB = FreqDist()
    for sample in responsesB:
        biasB.inc(sample)

    if potentialResponses is None:
        # Detect the sample range.
        potentialResponses = set(biasA.samples()).union(biasB.samples())
    
    # calculate pAgreement: the actual frequency of agreement
    nAgreements = 0
    nQuestions = 0
    for responseA, responseB in izip(responsesA, responsesB):
        if responseA == responseB:
            # they agreed 
            nAgreements += 1

        nQuestions += 1

    assert nQuestions > 0
    pAgreement = nAgreements / float(nQuestions)

    assert 0 <= pAgreement <= 1, "P(Agreement) should be a defined probability"

    # calculate pExpected: the agreement expected by chance
    pExpected = 0.0
    for response in potentialResponses:
        pExpected += biasA.freq(response) * biasB.freq(response)
    
    assert 0 <= pExpected <= 1, \
        "P(Expected) should be bewteeen 0 and 1, not %.2f" % pExpected

    # calculate kappa
    kappa = (pAgreement - pExpected)/(1 - pExpected)

    return kappa

#----------------------------------------------------------------------------#

def frange(start, end=None, inc=None):
    """
    A range function, that does accept float increments...
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/66472

        >>> frange(1.0, 3.0, 0.5)
        [1.0, 1.5, 2.0, 2.5]
    """ 

    if end == None:
        end = start + 0.0
        start = 0.0

    if inc == None:
        inc = 1.0

    L = []
    while 1:
        next = start + len(L) * inc
        if inc > 0 and next >= end:
            break
        elif inc < 0 and next <= end:
            break
        L.append(next)
        
    return L

#----------------------------------------------------------------------------#

def inclusionCombinations(sequence):
    """
    Returns a list of all combinations of inclusion/exclusion of the
    elements of the given sequence.

        >>> inclusionCombinations([])
        [[]]
        >>> inclusionCombinations([1, 2])
        [[], [1], [2], [1, 2]]
    """
    currentCombs = [[]]
    for element in sequence:
        nextSet = []
        for comb in currentCombs:
            nextSet.append(comb + [element])

        currentCombs += nextSet

    return currentCombs

#----------------------------------------------------------------------------#
