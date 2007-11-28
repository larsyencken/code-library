# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testChasen.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Mon Oct 29 20:17:32 2007
#
#----------------------------------------------------------------------------# 

import unittest
import doctest
import chasen

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(ChasenTestCase),
            doctest.DocTestSuite(chasen)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class ChasenTestCase(unittest.TestCase):
    """
    This class tests the Chasen class. 
    """
    def setUp(self):
        pass

    def testBadRc(self):
        try:
            c = chasen.Chasen('dogAteMyRc')
        except:
            return
        
        assert False, "No error on bad rc file"

    def testParse(self):
        c = chasen.Chasen()
        input = u'寿司が好きです。'
        sentence = c.parse(input)
        self.assertEqual(5, len(sentence))
        self.assertEqual(sentence[0].surface, u'寿司')
        self.assertEqual(sentence[1].surface, u'が')
        self.assertEqual(sentence[2].surface, u'好き')
        self.assertEqual(sentence[3].surface, u'です')
        self.assertEqual(sentence[4].surface, u'。')
    
    def tearDown(self):
        pass

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())

#----------------------------------------------------------------------------#

# vim: ts=4 sw=4 sts=4 et tw=78:

