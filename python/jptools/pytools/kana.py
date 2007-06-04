# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# jptools.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Sun May 15 23:12:04 EST 2005
#
#----------------------------------------------------------------------------#

""" This module is responsible for Japanese script/encoding specific methods,
	especially determining the script type of an entry. It is thus the only
	module which requires a utf8 encoding for the additional Japanese
	characters.
"""

#----------------------------------------------------------------------------#

from sets import Set
import string
import enum

#----------------------------------------------------------------------------#
# CONSTANTS
#

hiragana = u'ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをんゔゕゖ'
katakana = u'ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ'

smallKana = u'ぁぃぅぇぉっょゅゃ'
smallKanaSet = Set(smallKana)

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

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# CONVERTING BETWEEN KANA FORMS
#

def toKatakana(data):
	""" Converts a string from hiragana to katakana, leaving any non-hiragana
		characters as they are.
	"""
	encodingDiff = ord(u'ア') - ord(u'あ')

	lord = ord
	lunichr = unichr

	outputData = []
	for c in data:
		if c >= u'ぁ' and c <= u'ゖ':
			outputData.append(lunichr(lord(c) + encodingDiff))
		else:
			outputData.append(c)

	return ''.join(outputData)

#----------------------------------------------------------------------------#

def toHiragana(data):
	""" Converts a string from katakana to hiragana, leaving any non-hiragana
		characters as they are.
	"""
	encodingDiff = ord(u'あ') - ord(u'ア')

	outputData = ''
	for c in data:
		if c >= u'ァ' and c <= u'ヶ':
			outputData += unichr(ord(c) + encodingDiff)
		else:
			outputData += c

	return outputData

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# DETECTING SCRIPT TYPE
#

Script = enum.Enum('kanji', 'hiragana', 'katakana', 'ascii', 'unknown')
 
#----------------------------------------------------------------------------#

def scriptType(segment):
	""" Determine what type of script a character is. A whole word can be
		passed in, but only the first character is looked at. Furthermore, any
		non-kana script is classified as kanji.
	"""
	char = segment[0]

	if (char >= u'ぁ' and char <= u'ん') or char == u'ー':
		return Script.hiragana
	elif char >= u'ァ' and char <= u'ヶ':
		return Script.katakana
	elif char >= u'一' and char <= u'龥':
		return Script.kanji
	elif ord(char) < 256:
		return Script.ascii
	else:
		return Script.unknown
	
#----------------------------------------------------------------------------#

def hasKanji(jString):
	return Script.kanji in map(scriptType, jString)

#----------------------------------------------------------------------------#

def isAscii(char):
	""" Returns True if the character passed in is an ascii, False otherwise.
	"""
	return ord(char) < 256

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

#----------------------------------------------------------------------------#
# SEQUENTIAL VOICING AND SOUND EUPHONY
#

def rendakuVariants(kanaString):
	variants = []
	for kana in toVoiced[kanaString[0]]:
		variants.append(kana + kanaString[1:])
	return variants

#----------------------------------------------------------------------------#

def onbinVariants(kanaString):
	if len(kanaString) > 1:
		return [kanaString[:-1] + u'っ']
	else:
		return []

#----------------------------------------------------------------------------#

def _createVoicingMap():
	voicedLine = kanaTable[u'か'] + kanaTable[u'さ'] + kanaTable[u'た']
	doubleVoicedLine = kanaTable[u'は']

	voicingMap = {}
	for kana in hiragana:
		ordKana = ord(kana)

		if kana in voicedLine:
			voicingMap[kana] = [unichr(ordKana+1)]
		elif kana in doubleVoicedLine:
			voicingMap[kana] = [unichr(ordKana+1), unichr(ordKana+2)]
		else:
			voicingMap[kana] = []

	return voicingMap

toVoiced = _createVoicingMap()

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

def compareKana(kanaA, kanaB):
	return toKatakana(kanaA) == toKatakana(kanaB)

#----------------------------------------------------------------------------#

def containsRoman(word):
	""" Determines whether or not a word contains any non-script characters.
	"""
	for char in word:
		if scriptType(char) == Script.ascii:
			return True
	else:
		return False

#----------------------------------------------------------------------------#

def uniqueKanji(data):
	""" Returns a sorted string of each unique Kanji occuring in the input.
	"""
	data = filter(lambda x: not isAscii(x), data)
	data = filter(lambda x: scriptType(x) == Script.kanji, data)

	charSet = list(Set(data))
	charSet.sort()

	return charSet

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

class KanjiFilter:
	def __init__(self):
		self.__accepted = Set()
		self.__rejected = Set()
		return

	def filterKana(self, mixedString):
		""" Filter all katakana in a strnig.
		"""
		accepted = ''
		for char in mixedString:
			if scriptType(char) == Script.kanji:
				self.__accepted.add(char)
				accepted += char
			else:
				self.__rejected.add(char)

		return accepted

	def accepted(self):
		return self.__accepted
	
	def rejected(self):
		return self.__rejected

#----------------------------------------------------------------------------#

