# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testStats.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Mon Feb 20 18:01:32 EST 2006
#
#----------------------------------------------------------------------------#

import sys
sys.path.append('..')

import unittest
from jptools.stats import *

#----------------------------------------------------------------------------#

def suite():
	testSuite = unittest.TestSuite((
			unittest.makeSuite(SpearmanTest),
			unittest.makeSuite(CorrelationTest),
			unittest.makeSuite(KappaTest),
			unittest.makeSuite(CombinationTest)
		))
	return testSuite

#----------------------------------------------------------------------------#

class SpearmanTest(unittest.TestCase):
	def setUp(self):
		self.dataA = [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
		self.dataB = [('a', -1000), ('b', 0), ('c', 10), ('d', 1000000)]
		self.dataC = [('a', 4), ('b', 3), ('c', 2), ('d', 1)]

	def testPerfectCorrelation(self):
		""" Perfect positive correlation
		"""
		self.assertEqual(1, rankCorrelation(self.dataA, self.dataB))
		return
	
	def testNegPerfectCorrelation(self):
		""" Perfect negative correlation 
		"""
		self.assertEqual(-1, rankCorrelation(self.dataA, self.dataC))
		self.assertEqual(-1, rankCorrelation(self.dataB, self.dataC))
	
	def tearDown(self):
		pass

#----------------------------------------------------------------------------#

class CorrelationTest(unittest.TestCase):
	def setUp(self):
		self.dataA = [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
		self.dataC = [('a', -1), ('b', -2), ('c', -3), ('d', -4)]

	def testPerfectCorrelation(self):
		""" Perfect positive correlation
		"""
		self.assertEqual(1, correlation(self.dataA, self.dataA))
		return
	
	def testNegPerfectCorrelation(self):
		""" Perfect negative correlation 
		"""
		self.assertEqual(-1, rankCorrelation(self.dataA, self.dataC))
	
	def tearDown(self):
		pass

#----------------------------------------------------------------------------#

class KappaTest(unittest.TestCase):
	def setUp(self):
		self.dataA = [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
		self.dataB = [('a', 4), ('b', 3), ('c', 2), ('d', 1)]
		self.dataC = [('a', -1), ('b', -2), ('c', -3), ('d', -4)]

	def testHighKappa(self):
		""" Tests a high kappa value
		"""
		kappaVal =  kappa(self.dataA, self.dataA)
		assert kappaVal > 0.5, 'low kappa of %.2f' % kappaVal 
		return
	
	def testZeroKappa(self):
		""" Tests the no agreement case.
		"""
		kappaVal =  kappa(self.dataA, self.dataC)
		self.assertEqual(kappaVal, 0.0)
		return

	def testLowKappa(self):
		""" Tests the worse-than-chance agreement case
		"""
		kappaVal =  kappa(self.dataA, self.dataB)
		assert kappaVal < 0
		return
	
	def tearDown(self):
		pass

#----------------------------------------------------------------------------#

class CombinationTest(unittest.TestCase):
	def setUp(self):
		self.dataA = [1,2,3]

	def testICombinations(self):
		self.assertEqual(list(icombinations([self.dataA, self.dataA])),
				combinations([self.dataA, self.dataA]))

		self.assertEqual(list(icombinations([self.dataA, self.dataA])),
				[[1,1],[2,1],[3,1],[1,2],[2,2],[3,2],[1,3],[2,3],[3,3]])

		return

	def testUniqueTuples(self):
		self.assertEqual(uniqueTuples([1,2]), [(1,2)])
		self.assertEqual(uniqueTuples([1,2,3]), [(1,2), (1,3), (2,3)])

		return

#----------------------------------------------------------------------------#

if __name__ == "__main__":
	unittest.TextTestRunner(verbosity=1).run(suite())
