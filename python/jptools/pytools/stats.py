# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# stats.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Mon May 16 14:18:59 EST 2005
#
#----------------------------------------------------------------------------#

""" This module is responsible for any general combinatoric methods, in
	particular determining possible combinations of input.
"""

#----------------------------------------------------------------------------#

from sets import Set
from itertools import izip
import Scientific.Statistics
from nltk_lite.probability import FreqDist

#----------------------------------------------------------------------------#
# PUBLIC METHODS
#

def combinations(combinationList):
	""" Generates a list of all possible combinations of one element from the
		first item in combinationList, one from the second, etc. For example::

			> combinations([[1, 2], ['dog'], ['a', 'b']])
			[(1, 'dog', 'a'), (2, 'dog', 'a'), (1, 'dog', 'b'), 
			(2, 'dog', 'b')]
	"""
	combinationList = combinationList[:]
	combinationList.reverse()

	firstList = combinationList.pop()
	combos = map(lambda x: (x,), firstList)

	while combinationList:
		nextLevelCombos = []
		for itemToAdd in combinationList.pop():
			# add this item to the end of every existing combo 
			for existingCombo in combos:
				nextLevelCombos.append(existingCombo + (itemToAdd,))

		combos = nextLevelCombos

	return combos

#----------------------------------------------------------------------------#

def uniqueTuples(inputList, n=2):
	""" Similar to combinations, but selects from the same list.
	"""
	def filterFn(x):
		for i in xrange(n-1):
			if x[i] >= x[i+1]:
				return False
		else:
			return True

	return filter(filterFn, combinations(n*[inputList]))

#----------------------------------------------------------------------------#

def icombinations(combinationLists):
	""" As for combinations(), but returns an iterator.
	"""
	combinationLists = map(list, combinationLists)
	lengths = map(len, combinationLists)
	combined = zip(combinationLists, lengths)
	nCombs = sum(lengths)

	for i in xrange(nCombs):
		item = ()
		for itemList, listLength in combined:
			i, offset = divmod(i, listLength)
			item += (itemList[offset],)
		yield item

	return

#----------------------------------------------------------------------------#

def combinationSeqs(combinationList):
	""" As with combinations() above, except that each potential item is
		assumed to already be in sequence form.

			> combinationSeqs([ [(1, 2), (3, 4)], [('dog',), ('cat',)] ])
			[(1, 2, 'dog'), (3, 4, 'dog'), (1, 2, 'cat'), (3, 4, 'cat')]
	"""
	combinationList = combinationList[:]
	combinationList.reverse()

	combos = combinationList.pop()

	while combinationList:
		nextLevelCombos = []
		for itemToAdd in combinationList.pop():
			# add this item to the end of every existing combo 
			for existingCombo in combos:
				nextLevelCombos.append(existingCombo + itemToAdd)

		combos = nextLevelCombos

	return combos

#----------------------------------------------------------------------------#

def icombinationSeqs(combinationLists):
	""" As for combinations(), but returns an iterator.
	"""
	combinationLists = map(list, combinationLists)
	lengths = map(len, combinationLists)
	combined = zip(combinationLists, lengths)
	nCombs = sum(lengths)

	firstList, firstListLen = combined.pop(0)

	for i in xrange(nCombs):
		i, offset = divmod(i, firstListLen)
		itemStart = firstList[offset]

		for itemList, listLength in combined:
			i, offset = divmod(i, listLength)
			item += itemList[offset]
		yield item

	return

#----------------------------------------------------------------------------#
def segmentCombinations(gString):	
	""" Determines the possible segment combinations based on the grapheme
		string alone, in particular due to kanji placement. For example::

			> segmentCombinations('ab')
			[('a','b'),('ab',)]
		
	"""
	# start out with just the first character
	segmentations = [[gString[0]]]

	# add remaining characters one by one
	for char in gString[1:]: 
		nextSegmentationRound = []
		for segment in segmentations:
			# the new char in its own segment
			nextSegmentationRound.append(segment + [char])

			# the new char as part of the previous segment
			segment[-1] += char
			nextSegmentationRound.append(segment)

		segmentations = nextSegmentationRound
	
	segmentations = map(tuple, segmentations)

	return segmentations

#----------------------------------------------------------------------------#

def isegmentCombinations(gString):
	""" As for segmentCombinations(), but returns an iterator.
	"""
	if not gString:
		return

	gStringSize = len(gString)
	nCombs = 2**(gStringSize-1)

	for i in xrange(nCombs):
		currentComb = [gString[0]]
		for j in xrange(1,gStringSize):
			i, hasBoundary = divmod(i, 2)
			if hasBoundary:
				currentComb.append(gString[j])
			else:
				currentComb[-1] += gString[j]
		yield tuple(currentComb)

	return

#----------------------------------------------------------------------------#

def correlation(responsesA, responsesB):
	""" Calculates Pearson's correlation coefficient for the response data.
	"""
	responsesA.sort()
	responsesB.sort()
	
	return Scientific.Statistics.correlation(
			[x[1] for x in responsesA],
			[x[1] for x in responsesB]
		)

#----------------------------------------------------------------------------#

def rankCorrelation(responsesA, responsesB):
	""" Calculates Spearman's rank correlation between the response sets. The
		value returned will be a correlation between -1 and 1.
		
		@param responsesA: The response set for rater A.
		@type responsesA: [(stimulus, response)]
		@param responsesB: The response set for rater B.
		@type responsesB: [(stimulus, response)]
		@return: The correlation between the two sets.
	"""
	if len(responsesA) != len(responsesB):
		raise Exception, "Different numbers of responses between raters"
	
	# determine ranks
	rankedA = _determineRank(responsesA)
	rankedB = _determineRank(responsesB)

	# sort by stimulus
	rankedA.sort()
	rankedB.sort()
	
	# Using formula:
	# Rs = 1 - (6*sigmaDSquared / (n^3 - n))
	sumDSquared = 0.0
	for ((stimulusA, rankA), (stimulusB, rankB)) in izip(rankedA, rankedB):

		if stimulusA != stimulusB:
			raise Exception, "Different stimulus between raters"

		dSquared = (rankA - rankB)**2
		sumDSquared += dSquared
	
	n = len(rankedA)

	return 1 - (6*sumDSquared/(n**3 - n))

#----------------------------------------------------------------------------#

def kappa(responsesA, responsesB):
	""" Assuming list of (question, response) pairs for each rater, determine
		their kappa value using Cohen's method.
	"""
	if len(responsesA) != len(responsesB):
		raise Exception, "Raters have responded to different items"
	
	if not responsesA:
		raise Exception, "Need at least one response to calculate kappa"
	
	# firstly determine rater biases
	biasA = FreqDist()
	for _questionId, response in responsesA:
		biasA.inc(response)

	biasB = FreqDist()
	for _questionId, response in responsesB:
		biasB.inc(response)

	potentialResponses = set( biasA.samples() ).intersection(
			set( biasB.samples() ))
	
	# calculate pAgreement: the actual frequency of agreement
	nAgreements = 0
	nQuestions = 0
	responsesB = dict(responsesB)
	for question, responseA in responsesA:
		if not responsesB.has_key(question):
			raise Exception, "Rater B missing response for %s" % `question`

		if responseA == responsesB[question]:
			# they agreed on this question
			nAgreements += 1

		nQuestions += 1

	assert nQuestions > 0
	pAgreement = nAgreements / float(nQuestions)

	# calculate pExpected: the agreement expected by chance
	pExpected = 0.0
	for response in potentialResponses:
		pExpected += biasA.freq(response) * biasB.freq(response)

	# calculate kappa
	kappa = (pAgreement - pExpected)/(1 - pExpected)

	return kappa

#----------------------------------------------------------------------------#
# PRIVATE METHODS
#

def _determineRank(responseList):
	""" Maps a list of (stimulus, response) pairs to (stimulus, rank) pairs.
	"""
	# create the comparison operator for sorting; will reverse sort by
	# response
	cmpByResponse = lambda x, y: cmp(y[1], x[1])

	responseList = list(responseList)
	responseList.sort(cmpByResponse)

	rankedResponses = []
	stimulus, response = responseList[0]
	lastRank = 1
	lastResponse = response
	rankedResponses.append( (stimulus, 1) )

	currentRank = 2
	for stimulus, response in responseList[1:]:
		if lastResponse == response:
			# this case is tied with the last one for rank
			rankedResponses.append( (stimulus, lastRank) )
		else:
			# this is ranked lower than the last one
			rankedResponses.append( (stimulus, currentRank))
			lastRank = currentRank

		currentRank += 1

	return rankedResponses

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
