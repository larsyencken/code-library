# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testReadOnly.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Fri Jan  4 16:30:39 2008
#
#----------------------------------------------------------------------------# 

import unittest
import doctest
import readOnly

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(ReadOnlyTestCase),
            doctest.DocTestSuite(readOnly)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class ReadOnlyTestCase(unittest.TestCase):
    """
    This class tests the ReadOnly class. 
    """
    def setUp(self):
        self.listObj = [1, 2, 'sheep'] 
        self.readObj = readOnly.ReadOnlyListView(self.listObj)
        pass

    def testList(self):
        self.assertEqual(self.listObj, self.readObj)
        self.assertEqual(3, len(self.readObj))
        self.assertEqual([1, 2, 'sheep', 'dog'], self.readObj + ['dog'])
    
    def tearDown(self):
        pass

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())

#----------------------------------------------------------------------------#

# vim: ts=4 sw=4 sts=4 et tw=78:

