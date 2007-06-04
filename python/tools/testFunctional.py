# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testFunctional.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Mon Jan 23 14:36:06 EST 2006
#
#----------------------------------------------------------------------------#

import sys, unittest
from itertools import izip

from functional import *

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(MiscTestCases),
            unittest.makeSuite(PartialMapTestCase),
            unittest.makeSuite(MultiDictTestCase),
            unittest.makeSuite(ThreadingTestCase),
            unittest.makeSuite(ZipWithTestCase)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class MiscTestCases(unittest.TestCase):
    def testClassify(self):
        """ Basic use of separate()
        """
        def isEven(i):
            return i % 2 == 0

        initList = range(10)
        even, odd = classify(isEven, initList)
        
        assert even == [0,2,4,6,8]
        assert odd == [1,3,5,7,9]

        return

    def testChainDicts(self):
        """ Tests chaining together dictionaries.
        """
        dataA = {1: 'a', 2: 'b', 3: 'c'}
        dataB = {'a': 1.5, 'b': 3.5, 'c': 1.5}

        self.assertEqual(chainDicts(dataA, dataB), {1:1.5, 2:3.5, 3:1.5})

        return

    def testInvertDict(self):
        """ Tests inverting a dictionary.
        """
        data = {1: [1,2,3], 2: [1,3]}
        newDict = invertDict(data)
        self.assertEqual(newDict, {1:[1,2], 2:[1], 3:[1,2]})

        data2 = {1: 'a', 2: 'b', 3: 'c'}
        newDict2 = invertDictInjective(data2)
        self.assertEqual(newDict2, {'a': 1, 'b': 2, 'c': 3})

        return

    def testFlatten(self):
        data = [[1,2],3,[[4,[5,[]]]],[]]
        self.assertEqual(flatten(data), [1,2,3,4,5])

        return

    def testMapDict(self):
        """ Tests map as applied to dictionaries.
        """
        data = {'a': 1, 'b': 2, 'c': 3}
        method = lambda x: x*x
        newData = mapDict(method, data)

        self.assertEqual(newData, {'a': 1, 'b': 4, 'c': 9})
        mapDictInPlace(method, data)
        self.assertEqual(data, newData)

        return

    def testUnzip(self):
        """ Tests the unzip method.
        """
        data = zip(range(0, 5), range(4,9))
        unzippedData = unzip(data)
        self.assertEqual(unzippedData[0], range(0,5))
        self.assertEqual(unzippedData[1], range(4,9))

        return

    def testMergeDicts(self):
        """ Tests merging multiple dictionaries.
        """
        dictA = {
                    1: set('cat'),
                    2: set('dog'),
                }
        dictB = {
                    1: 'canine',
                    3: 'bird',
                }

        result = mergeDicts(dictA, dictB)

        self.assertEqual(result[1], set('catine'))
        self.assertEqual(result[2], set('dog'))
        self.assertEqual(result[3], set('bird'))

        self.assertEqual(dictA, mergeDicts(dictA, dictA, dictA))

        return

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

class PartialMapTestCase(unittest.TestCase):
    def testToNone(self):
        """ Everything maps to None
        """
        def toNone(a): return None

        initList = range(10)
        okList, badList = partialMap(toNone, initList)

        assert okList == []
        assert badList == range(10)

        return
    
    def testAllOk(self):
        """ Everthing maps to a true value
        """
        def id(a): return a

        initList = range(1,11)
        okList, badList = partialMap(id, initList)

        assert okList == initList, "Expected: %s, got: %s" % \
                (`initList`, `okList`)
        assert badList == []

        return

    def testPartial(self):
        """ Realistic case, part goes either way
        """
        def killEven(x):
            if x % 2 == 0:
                return None
            else:
                return x*10

        initList = range(10, 20)
        okList, badList = partialMap(killEven, initList)
        
        assert okList == [110, 130, 150, 170, 190]
        assert badList == [10, 12, 14, 16, 18]

        return

#----------------------------------------------------------------------------#

class MultiDictTestCase(unittest.TestCase):
    def testMultiDict(self):
        """ A simple multiDict test case.
        """
        inputPairs = [('a', 2), ('b', 3), ('a', 4)]
        self.assertEqual(multiDict(inputPairs), {'a': [2,4], 'b': [3]})

        self.assertEqual(multiDict([]), {})

        return

#----------------------------------------------------------------------------#

class ThreadingTestCase(unittest.TestCase):
    def testThread(self):
        """ Tests the variety of thread/unthread methods.
        """
        inputPairs = range(6)
        threadedPairs = thread(inputPairs)

        self.assertEqual(threadedPairs, [(0,1), (2,3), (4,5)])
        self.assertEqual(unthread(threadedPairs), inputPairs)
        self.assertEqual(list(threadSeqIter(inputPairs)),
                [(0,1), (1,2), (2,3), (3,4), (4,5)])

        return

#----------------------------------------------------------------------------#

class ZipWithTestCase(unittest.TestCase):
    def testBasic(self):
        """ Tests basic functioning of the zipWith method.
        """
        inputA = [1,-5, 0]
        inputB = [2, 5, 1]
        method = lambda x, y: x+y

        self.assertEqual(zipWith(method, inputA, inputB), [3, 0, 1])
        self.assertEqual(list(izipWith(method, inputA, inputB)), [3, 0, 1])
        return

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())
