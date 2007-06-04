# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# functional.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Mon Aug 22 16:18:39 EST 2005
#
#----------------------------------------------------------------------------#

""" This module provides methods for useful and efficient functional
    implementations. 
"""

#----------------------------------------------------------------------------#

#from _functional import *

from itertools import izip, chain

#----------------------------------------------------------------------------#

def chainDicts(*dictionaries):
    """ Say you have more than one dictionary, and the range of the first is
        the domain of the second. You can create a new dictionary with the
        domain of the first but range of the second using this method.
    """
    baseDict = dictionaries[0].copy()

    for dictionary in dictionaries[1:]:
        mapDictInPlace(dictionary.__getitem__, baseDict)

    return baseDict

#----------------------------------------------------------------------------#

def invertDict(dictionary):
    """ Inverts a dictionary with a 1 -> many mapping from key to value to a
        new dictionary with a 1 -> many mapping from value to key.
    """
    invertedDict = {}
    for key, values in dictionary.iteritems():
        for value in values:
            invertedDict.setdefault(value, []).append(key)

    return invertedDict

#----------------------------------------------------------------------------#

def invertDictInjective(dictionary):
    """ Inverts a dictionary with a 1 -> 1 mapping from key to value, into a
        new dictionary with a 1 -> 1 mapping from value to key.
    """
    invertedDict = {}
    for key, value in dictionary.iteritems():
        assert not invertedDict.has_key(value), "Mapping is not 1-1"
        invertedDict[value] = key

    return invertedDict

#----------------------------------------------------------------------------#

def mapDict(method, dictionary):
    """ Applies the method to every value in the dictionary, ignoring keys.

        @param method: The method to apply.
        @param dictionary: The dictionary whose values to apply the method to.
        @return: A new dictionary with the updated values.
    """
    return dict((k, method(v)) for (k, v) in dictionary.iteritems())

#----------------------------------------------------------------------------#

def mapDictInPlace(method, dictionary):
    """ Applies the method to every value in the dictionary, ignoring keys.
        Performs the operations in-place, modifying the existing dictionary.

        @param method: The method to apply.
        @param dictionary: The dictionary whose values to apply the method to.
        @return: None
    """
    for key, value in dictionary.iteritems():
        dictionary[key] = method(value)

    return

#----------------------------------------------------------------------------#

def zipWith(method, listA, listB):
    """ Applies a two-argument method to successive pairs of items taken from
        the two input lists. Returns the list of resulting items.

        @param method: The two-argument method to run over element pairs.
        @param listA: The first providing the first tuple element for each
            pair.
        @param listB: The first providing the second tuple element for each
            pair.
    """
    result = []
    for itemA, itemB in izip(listA, listB):
        result.append(method(itemA, itemB))
    
    return result

#----------------------------------------------------------------------------#

def izipWith(method, listA, listB):
    """ As with zipWith(), but provides an iterator. 
    """
    for itemA, itemB in izip(listA, listB):
        yield method(itemA, itemB)
    
    return

#----------------------------------------------------------------------------#

def partialMap(method, objectList, multiArg=False):
    """ Like map, but filters out all objects with an non-true result, and
        returns them in a separate items list. I.e. any item for which the map
        resulted in a non-true value is provided in a "reject" list.
    """
    listA = []
    listB = []

    for item in objectList:
        result = apply(method, (item,))
        if result:
            listA.append(result)
        else:
            listB.append(item)
    
    return listA, listB

#----------------------------------------------------------------------------#

def filteredMap(method, objectList):
    return filter(None, map(method, objectList))

#----------------------------------------------------------------------------#

def flatten(objectSeq):
    """ Flattens all levels of lists from the input, preserving original order
        of items.
    """
    result = []

    objectSeq = iter(objectSeq)

    while True:
        try:
            item = objectSeq.next()

            if type(item) in (tuple, list):
                objectSeq = chain(item, objectSeq)
            else:
                result.append(item)
        except StopIteration:
            break

    return result

#----------------------------------------------------------------------------#

def iflatten(objectSeq):
    """ Flattens all levels of lists from the input, preserving original order
        of items.
    """
    objectSeq = iter(objectSeq)

    while True:
        try:
            item = objectSeq.next()

            if type(item) in (tuple, list):
                objectSeq = chain(item, objectSeq)
            else:
                yield item
        except StopIteration:
            break

    return

#----------------------------------------------------------------------------#

def unzip(pairList):
    """ The reverse of the zip() method. Given a sequence of tuples of the
        same size, extracts two or more lists. For example::

            > unzip([(1,2), (3,4), (5,6)])
            ([1, 3, 5], [2, 4, 6])

        @param pairList: The sequence of tuples to draw the input from.
    """
    assert pairList
    numLists = len(pairList[0])

    newLists = []
    for i in range(numLists):
        newLists.append([])

    for pair in pairList:
        for i in range(numLists):
            newLists[i].append(pair[i])
    
    return tuple(newLists)

#----------------------------------------------------------------------------#

def thread(inputList, tupleSize=2):
    """ Turns a list of items into a list of tuples by simply placing
        sequential items into tuples::

            > thread([1,2,3,4,5])
            [(1,2), (3,4)]

        Notice that the last element might be discarded, in a similar manner
        as when using zip.
    """
    outputList = []

    finalElement = tupleSize-1

    i = 0
    currentTuple = ()
    while i < len(inputList):
        if i % tupleSize == finalElement:
            outputList.append(currentTuple + (inputList[i],))
            currentTuple = ()
        else:
            currentTuple += (inputList[i],)
        i += 1

    return outputList

#----------------------------------------------------------------------------#

def unthread(inputTuples):
    """ Turns a list of tuples into a flat list::

            > unthread([(1,2),(3,4)])
            [1, 2, 3, 4]
    """
    outputList = []

    for item in inputTuples:
        outputList.extend(item)
    
    return outputList

#----------------------------------------------------------------------------#

def threadSeqIter(inputList, n=2):
    """ Takes a list and creates tuples from sequential items. Note the
        difference between this method and the earlier thread() method.
        For example::

        > list(threadSeqIter([1,2,3,4]))
        [(1,2), (2,3), (3,4)]

        @param inputList: The list which gets threaded.
    """
    if n < 1:
        raise Exception, "Need n >= 1 for a valid thread sequence"
    for i in xrange(len(inputList)-(n-1)):
        yield tuple(inputList[i:i+n])

    return

#----------------------------------------------------------------------------#

def repeat(n, item):
    """ Creates an iterator which provides n references to item.
    """
    for i in xrange(n):
        yield item
    return

#----------------------------------------------------------------------------#

def succession(itemList):
    """ Returns an iterator which builds up the item list point by point.
        
        For example::

            > succession([1,2,3])
            [[1], [1,2], [1,2,3]]
    """
    for i in xrange(1, len(itemList)):
        yield itemlist[:i]
    
    return

#----------------------------------------------------------------------------#

def window(itemList, windowSize=2):
    """ Returns a sliding window iterator over the given list.

        For example::

            > window([1,2,3,4,5])
            [(1,2), (2,3), (3,4), (4,5)]

            > window([1,2,3,4,5], 3)
            [(1,2,3), (2,3,4), (3,4,5)]

        @param itemList: The list to iterate over.
        @param windowSize: The number of elements in each window frame.
    """
    for endWindow in range(windowSize, len(itemList)+1):
        yield tuple(itemList[endWindow-windowSize:endWindow])

    return 

#----------------------------------------------------------------------------#

def windowWithPreBlanks(itemList, windowSize=2):
    """ Returns a sliding window iterator over the given list, but with blanks
        at the beginning and end.

        For example::

            > window([1,2,3,4,5])
            [(None, 1), (1,2), (2,3), (3,4), (4,5)]

        @param itemList: The list to iterate over.
        @param windowSize: The number of elements in each window frame.
    """
    if windowSize <= 1:
        raise Exception, "Need a windowSize >= 2"

    # Pad the list with blanks.
    realList = (windowSize-1)*[None] + list(itemList)

    return window(realList, windowSize)

#----------------------------------------------------------------------------#

def groupByLambda(func, items):
    """ Performs a grouping of the items by lambda value.
    """
    groups = {}
    for item in items:
        keyValue = func(item)

        itemList = groups.get(keyValue, [])
        itemList.append(item)
        groups[keyValue] = itemList
    
    return groups

#----------------------------------------------------------------------------#

def multiDict(inputPairs):
    """ Similar to casting pairs to a dictionary, except that repeated pairs
        are allowed. To show the difference::
        
            > dict( [('a', 1), ('b', 2), ('a', 3)] )
            {'a': 3, 'b': 2}

            > multiDict( [('a', 1), ('b', 2), ('a', 3)] )
            {'a': [1, 3], 'b': [2]}

        @param inputPairs: A list of (key, value) pairs.
        @return: A dictionary mapping keys to lists of values.
    """
    outputDict = {}

    for key, value in inputPairs:
        existingValues = outputDict.get(key, [])
        existingValues.append(value)
        outputDict[key] = existingValues
    
    return outputDict

#----------------------------------------------------------------------------#

def procmap(procedure, itemList):
    """ Like map(), but where the method being applied has no return value. In
        other words, the procedure is called on every item in the list
        sequentially, but since each call has no return value, the call to
        procmap() also has no return value.

        @param procedure: The procedure to call each time.
        @param itemList: The list of items to apply the procedure to.
        @return: None
    """
    for item in itemList:
        method(item)
    
    return

#----------------------------------------------------------------------------#

def addFunc(a, b):
    """ The result of a+b.
    """
    return a + b

#----------------------------------------------------------------------------#

def timesFunc(a, b):
    """ The result of a*b.
    """
    return a * b;

#----------------------------------------------------------------------------#

def orFunc(a, b):
    """ The boolean result of a OR b.
    """
    return a or b

#----------------------------------------------------------------------------#

def andFunc(a, b):
    """ The boolean result of a AND b.
    """
    return a and b

#----------------------------------------------------------------------------#

def tailEnumerate(sequence):
    """ Same as enumerate, but yields tuples of (item, index), so that you can
        sort and otherwise use them nicely.
    """
    i = 0
    for item in sequence:
        yield (item, i)
        i += 1

    return

#----------------------------------------------------------------------------#

class RecursionError(Exception):
    """ An exception to be thrown when a recursion depth limit is exceeded. 
    """
    pass

_recursionCounter = {}
def limitRecursion(function, limit):
    """ Create a depth-limited version of the function.
    """
    global _recursionCounter

    def wrapper(*args, **kwargs):
        currentVal = _recursionCounter.setdefault(function, 0)
        print 'Currentval', currentVal + 1
        if currentVal + 1 > limit:
            raise RecursionError, "Recursion exceeded depth limit."
        _recursionCounter[function] = currentVal + 1

        try:
            value = apply(function, args, kwargs)
        except:
            _recursionCounter[function] -= 1
            raise

        _recursionCounter[function] -= 1

        return value

    wrapper.__doc__ = function.__doc__

    return wrapper

#----------------------------------------------------------------------------#

def mergeDicts(*args):
    """ Merges a number of dictionaries together into one. Assumes the
        dictionary maps to a set of hashable items. The result for each key is
        the union of all the values in the provided dictionaries.
    """
    unifiedDict = {}
    for key, items in apply(chain, [d.iteritems() for d in args]):
        if unifiedDict.has_key(key):
            unifiedDict[key].update(items)
        else:
            unifiedDict[key] = set(items)

    return unifiedDict

#----------------------------------------------------------------------------#

def classify(boolMethod, sequence):
    failures = []
    successes = []
    for item in sequence:
        if boolMethod(item):
            successes.append(item)
        else:
            failures.append(item)

    return successes, failures
#----------------------------------------------------------------------------#

