# -*- coding: utf-8 -*-
#
#  test_bitset.py
#  unknown project
# 
#  Created by Lars Yencken on 14-08-2009.
#  Copyright 2009 Lars Yencken. All rights reserved.
#

import unittest
import doctest
#import bitset
import bitset_cython as bitset
import random
import cPickle as pickle

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(BitsetTestCase),
#            doctest.DocTestSuite(bitset)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class BitsetTestCase(unittest.TestCase):
    """
    This class tests the bitset module. 
    """
    def setUp(self):
        pass

    def test_simple(self):
        s = bitset.Bitset(20)
        self.assertEqual(list(s), [])

        s.add(5)
        assert 5 in s
        assert list(s) == [5]

        s.add(7)
        assert 7 in s
        assert list(s) == [5, 7]

    def test_build_big(self):
        s = bitset.Bitset(1000000)
        self.assertEqual(list(s), [])

    def test_remove(self):
        s = bitset.Bitset(100)
        self.assertEqual(list(s), [])
        for i in xrange(100):
            s.add(i)
        
        assert list(s) == range(100)
        
        s.remove(57)
        assert 57 not in s
        assert 56 in s
        assert 58 in s
        data = range(100)
        data.pop(data.index(57))

        assert list(s) == data
    
    def test_pickle(self):
        s = bitset.Bitset(100)
        s.add(57)
        s.add(17)
        s.add(0)
        s.add(100)
        assert s.max_size == 100
        assert 0 in s
        assert 1 not in s
        assert 57 in s
        assert 17 in s
        assert 100 in s
        assert 99 not in s
        assert list(s) == [0, 17, 57, 100]

        s = bitset.from_data(s.get_data())
        assert s.max_size >= 100
        assert 0 in s
        assert 1 not in s
        assert 57 in s
        assert 17 in s
        assert 100 in s
        assert 99 not in s
        assert list(s) == [0, 17, 57, 100]
        
    def tearDown(self):
        pass

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())

#----------------------------------------------------------------------------#

# vim: ts=4 sw=4 sts=4 et tw=78:

