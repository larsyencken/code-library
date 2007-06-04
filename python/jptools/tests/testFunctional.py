# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testFunctional.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Mon Jan 23 14:36:06 EST 2006
#
#----------------------------------------------------------------------------#

import sys
sys.path.append('..')

import unittest
from jptools.functional import *

#----------------------------------------------------------------------------#

def suite():
	testSuite = unittest.TestSuite((
			unittest.makeSuite(SeparateTestCase),
			unittest.makeSuite(PartialMapTestCase),
			unittest.makeSuite(MultiDictTestCase),
			unittest.makeSuite(ThreadingTestCase)

		))
	return testSuite

#----------------------------------------------------------------------------#

class SeparateTestCase(unittest.TestCase):
	def testSimple(self):
		""" Basic use of separate()
		"""
		def isEven(i):
			return i % 2 == 0

		initList = range(10)
		even, odd = separate(isEven, initList)
		
		assert even == [0,2,4,6,8]
		assert odd == [1,3,5,7,9]

		return

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

if __name__ == "__main__":
	unittest.TextTestRunner(verbosity=1).run(suite())
