# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# __init__.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Sun May 15 23:12:04 EST 2005
#
#----------------------------------------------------------------------------#

""" This module is responsible for Japanese script/encoding specific methods,
    especially determining the script type of an entry. It is thus the only
    module which requires a utf8 encoding for the additional Japanese
    characters.
"""

#----------------------------------------------------------------------------#

from stats import combinations
from functional import invertDict

# Import C++ methods.
#from _kana import *

#----------------------------------------------------------------------------#
# CONSTANTS
#----------------------------------------------------------------------------#

smallKana = u'ぁぃぅぇぉっょゅゃ'

nKana = u'ん'

#----------------------------------------------------------------------------#

kanaTable = {
    u'あ': u'あいうえお',
    u'か': u'かきくけこ',
    u'が': u'がぎぐげご',
    u'さ': u'さしすせそ',
    u'ざ': u'ざじずぜぞ',
    u'た': u'たちつてと',
    u'だ': u'だぢづでど',
    u'ま': u'まみむめも',
    u'は': u'はひふへほ',
    u'ば': u'ばびぶべぼ',
    u'ぱ': u'ぱぴぷぺぽ',
    u'な': u'なにぬねの',
    u'ら': u'らりるれろ'
}

def _createTableLines():
    newDict = {}
    for key, line in kanaTable.iteritems():
        for char in line:
            newDict[char] = key
    
    return newDict

kanaLine = _createTableLines()

def toLine(kanaA, vowelLine):
    index = kanaTable[u'あ'].index(vowelLine)
    line = kanaLine[kanaA]
    return kanaTable[line][index]

def isLine(kanaA, vowelLine):
    return kanaA == toLine(kanaA, vowelLine)

def vowelLine(kanaA):
    """ Returns the "vowel line" of the kana, e.g. か returns あ, そ return お.
    """
    line = kanaLine[kanaA]
    return u'あいうえお'[kanaTable[line].index(kanaA)]

#----------------------------------------------------------------------------#
# DETECTING SCRIPT TYPE
#----------------------------------------------------------------------------#

def scriptBoundaries(jString):
    """ Determines where the script boundaries are in the given string.
        
        @param jString: The string of Japanese to segment.
        @type jString: string
        @return: A tuple of script-contiguous blocks
    """
    segments = ()
    currentSegType = scriptType(jString[0])
    currentSeg = jString[0]
    for char in jString[1:]:
        if scriptType(char) == currentSegType or char == u'ー':
            currentSeg += char
        else:
            segments += currentSeg,

            currentSegType = scriptType(char)
            currentSeg = char
    else:
        if currentSeg:
            segments += currentSeg,
    
    return segments

#----------------------------------------------------------------------------#

def scriptTypes(jString):
    """ Returns a set of the script types in the given string.
    """
    return set(map(scriptType, jString))

#----------------------------------------------------------------------------#
# SEQUENTIAL VOICING AND SOUND EUPHONY
#----------------------------------------------------------------------------#

def canonicalForms(kanaSegments):
    """ When given a sequence of segments, determine all possible canonical
        forms for the sequence. We define the canonical form to be the
        underlying form, before sequential voicing and sound euphony are
        applied.

        @param kanaSegments: Reading segments in their surface form.
    """

    numSegments = len(kanaSegments)

    candidateSets = []
    for i, segment in enumerate(kanaSegments):
        variants = [segment]

        if i < numSegments - 1 and len(segment) > 1 and \
                    segment.endswith(u'っ'):
            # Can restore onbin cases.
            variants.extend([segment[:-1] + c for c in u'いちりきつく'])

        if i > 0 and isVoiced(segment[0]):
            # Can devoice.
            variants.extend([fromVoiced[v[0]] + v[1:] for v in variants])

        candidateSets.append(variants)

    return combinations(candidateSets)

#----------------------------------------------------------------------------#

def surfaceForms(readingSegments):
    """ The counterpart of canonicalForms(). Takes a correct reading, and
        determines how it could be erroneously modified into various surface
        forms.
    """
    candidateSets = []
    candidateSets.append(onbinVariants(readingSegments[0]))
    candidateSets.extend(
            map(rendakuVariants, readingSegments[1:])
        )

    return combinations(candidateSets)

#----------------------------------------------------------------------------#

def rendakuVariants(kanaSegment):
    """ Determine the possible variants of a single kana segment.
    """
    variants = set([kanaSegment])
    for kana in toVoiced[kanaSegment[0]]:
        variants.add(kana + kanaSegment[1:])
    return variants

#----------------------------------------------------------------------------#

def onbinVariants(kanaSegment):
    """ Determine the sound euphony variants of a kana segment.
    """
    variants = set([kanaSegment])
    if len(kanaSegment) > 1:
        variants.add(kanaSegment[:-1] + u'っ')

    return variants

#----------------------------------------------------------------------------#

def _createVoicingMap():
    """ Constructs map from kana to their voiced alternatives.
    """
    voicedLine = kanaTable[u'か'] + kanaTable[u'さ'] + kanaTable[u'た']
    doubleVoicedLine = kanaTable[u'は']

    voicingMap = {}
    for kana in getHiragana():
        ordKana = ord(kana)

        if kana in voicedLine:
            voicingMap[kana] = [unichr(ordKana+1)]
        elif kana in doubleVoicedLine:
            voicingMap[kana] = [unichr(ordKana+1), unichr(ordKana+2)]
        else:
            voicingMap[kana] = []

    return voicingMap

toVoiced = _createVoicingMap()
fromVoiced = invertDict(toVoiced)
fromVoiced = dict((k, v[0]) for (k, v) in fromVoiced.iteritems())

#----------------------------------------------------------------------------#

def insertDuplicateKanji(kanjiString):
    """ Inserts full kanji for characters where a shorthand is used.
    """
    loc = kanjiString.find(u'々')
    while loc > 0:
        dup = kanjiString[loc-1]
        kanjiString = kanjiString[:loc] + dup + kanjiString[loc+1:]
        loc = kanjiString.find(u'々')

    return kanjiString

#----------------------------------------------------------------------------#

def isVoiced(char):
    """ Returns True if the character is a kana character which is voiced.
    """
    char = toHiragana(char)

    line = kanaLine.get(char)
    if not line:
        # Not a kana character.
        return False

    return line in u'がだざばぱ'

#----------------------------------------------------------------------------#

def expandLongVowels(kanaString):
    """ Expands whatever long vowels are possible to expand.
    """
    notFound = -1
    kanaString = toHiragana(kanaString)

    i = kanaString.find(u'ー', 1)
    while i != notFound:
        previousChar = kanaString[i-1]
        previousScript = scriptType(previousChar)
        if previousScript == Script.Hiragana:
            # Ok, we can correct this one.
            vowel = vowelLine(previousChar)
            kanaString = kanaString[:i] + vowel + kanaString[i+1:]

        i = kanaString.find(u'ー', i+1)

    return kanaString

#----------------------------------------------------------------------------#
