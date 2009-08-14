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
import bitset
import random

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(BitsetTestCase),
            doctest.DocTestSuite(bitset)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class BitsetTestCase(unittest.TestCase):
    """
    This class tests the bitset module. 
    """
    def setUp(self):
        pass

    def test_remove(self):
        s = bitset.Bitset(100)
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
        
    def tearDown(self):
        pass

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())

#----------------------------------------------------------------------------#

# vim: ts=4 sw=4 sts=4 et tw=78:

