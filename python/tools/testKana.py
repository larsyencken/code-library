# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testKana.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Sat Apr 22 13:00:11 2006
#
#----------------------------------------------------------------------------# 

import sys, unittest
from itertools import izip

from kana import *

#----------------------------------------------------------------------------#

def suite():
    testSuite = unittest.TestSuite((
            unittest.makeSuite(KanaTestCase)
        ))
    return testSuite

#----------------------------------------------------------------------------#

class KanaTestCase(unittest.TestCase):
    """ This class tests the Kana class. 
    """
    def setUp(self):
        self.testScript = u'AＡあア亜'
        pass

    def testFetchScripts(self):
        """ Test fetching of hiragana and katakana, and converting between
            them.
        """
        hiragana = u'ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをんゔゕゖ'
        self.assertEqual(getHiragana(), hiragana)
        katakana = u'ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ'
        self.assertEqual(getKatakana(), katakana)

        self.assertEqual(toHiragana(katakana), hiragana)
        self.assertEqual(toKatakana(hiragana), katakana)

        return

    def testScriptType(self):
        """ Tests the scriptType() method.
        """
        self.assertEqual(scriptType(self.testScript), Script.Ascii)
        self.assertEqual(scriptType(self.testScript[1]), Script.FullAscii)
        self.assertEqual(scriptType(self.testScript[2]), Script.Hiragana)
        self.assertEqual(scriptType(self.testScript[3]), Script.Katakana)
        self.assertEqual(scriptType(self.testScript[4]), Script.Kanji)

        return

    def testContainsScript(self):
        """ Tests the containsScript() method.
        """
        assert containsScript(Script.Hiragana, self.testScript)
        assert containsScript(Script.Kanji, self.testScript)
        assert containsScript(Script.Ascii, self.testScript)
        assert containsScript(Script.Katakana, self.testScript)

        assert not containsScript(Script.Ascii, self.testScript[1:])
        assert not containsScript(Script.Kanji, self.testScript[:-1])

        return

    def testCompareKana(self):
        """ Tests the compareKana() method.
        """
        assert compareKana(u'Aあア亜', u'Aアあ亜')
        assert not compareKana(u'あAア亜', u'アあ亜A')
        return

    def testCanonicalForm(self):
        """ Tests correct detection of candidate canonical forms. 
        """
        # Tests that only the first character is considered, and that unvoiced
        # segments are ok.
        self.assertEqual(
                set(canonicalForms((u'たべ', u'ほう', u'だい'))),
                set([(u'たべ', u'ほう', u'だい'), (u'たべ', u'ほう', u'たい')])
            )

        # Tests that the first segment is ignored, and that ひ voicings
        # are ok.
        self.assertEqual(
                set(canonicalForms((u'ぼり', u'ぽり'))),
                set([(u'ぼり', u'ほり'), (u'ぼり', u'ぽり')])
            )
        return

        self.assertEqual(
                set(canonicalForms((u'せっ', u'かつ'))),
                set([
                        (u'せっ', u'かつ'),
                        (u'せり', u'かつ'),
                        (u'せち', u'かつ'),
                        (u'せい', u'かつ'),
                    ])
            )

    def testNormalizeAscii(self):
        """ Tests that ascii characters are normalized correctly.
        """
        fullWidthString = u'ｋｌｉｎｇｏｎｓ　ｏｎ　ｔｈｅ　'\
            u'ｓｔａｒｂｏａｒｄ　ｂｏｗ！＠＃＆＊（）？，。；＋＝"'
        halfWidthString = u'klingons on the starboard bow!@#&*()?,.;+="'

        self.assertEqual(len(fullWidthString), len(halfWidthString))

        for fullChar, halfChar in zip(fullWidthString, halfWidthString):
            self.assertEqual(normalizeAscii(fullChar), halfChar)

        return

    def testVowelLine(self):
        """ Tests correct vowel line detection.
        """
        self.assertEqual(vowelLine(u'ご'), u'お')
        self.assertEqual(vowelLine(u'も'), u'お')
        self.assertEqual(vowelLine(u'さ'), u'あ')
        self.assertEqual(vowelLine(u'あ'), u'あ')
        self.assertEqual(vowelLine(u'ぎ'), u'い')

        return

    def tearDown(self):
        pass

#----------------------------------------------------------------------------#

if __name__ == "__main__":
    unittest.TextTestRunner(verbosity=1).run(suite())

#----------------------------------------------------------------------------#

# vim: ts=4 sw=4 sts=4 et tw=78:

