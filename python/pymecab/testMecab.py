# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testMecab.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Oct 23 00:50:14 2007
#
#----------------------------------------------------------------------------# 

import unittest
import doctest
import mecab

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(MecabTestCase),
            doctest.DocTestSuite(mecab)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class MecabTestCase(unittest.TestCase):
    """
    This class tests the Mecab class. 
    """
    def setUp(self):
        self.tagger = mecab.Tagger()
        pass

    def testBasic(self):
        example = u'私は漢字を読めたいです。'
        result = self.tagger.parse(example)
        assert result
    
    def tearDown(self):
        pass

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())

#----------------------------------------------------------------------------#

# vim: ts=4 sw=4 sts=4 et tw=78:
