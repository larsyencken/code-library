# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# functional.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Mon Aug 22 16:18:39 EST 2005
#
#----------------------------------------------------------------------------#

""" This module provides methods for useful and efficient functional
	implementations. 
"""

#----------------------------------------------------------------------------#

from exceptions import StopIteration
from sets import Set

#----------------------------------------------------------------------------#

def separate(method, objectList):
	""" Like filter, but returns a tuple of lists (accepted, rejected) instead
		of just the list of accepted items.

		@param method: The method to apply as a test.
		@type method: A function taking one argument.
		@param objectList: The list of items to apply the method to.
		@type objectList: A list.
		@return The pair (acceptedList, rejectedList)
	"""
	listA = []
	listB = []
	
	for item in objectList:
		if apply(method, (item,)):
			listA.append(item)
		else:
			listB.append(item)
	
	return listA, listB

#----------------------------------------------------------------------------#

def partialMap(method, objectList, multiArg=False):
	""" Like map, but filters out all objects with an non-true result, and
		returns them in a separate items list. I.e. any item for which the map
		resulted in a non-true value is provided in a "reject" list.
	"""
	listA = []
	listB = []

	for item in objectList:
		result = apply(method, (item,))
		if result:
			listA.append(result)
		else:
			listB.append(item)
	
	return listA, listB

#----------------------------------------------------------------------------#

def filteredMap(method, objectList):
	return filter(None, map(method, objectList))

#----------------------------------------------------------------------------#

def flatten(objectList):
	if not type(objectList) == list:
		return objectList

	result = []

	queue = objectList
	for item in queue:
		if type(item) == list:
			queue.extend(item)
		else:
			result.append(item)
	
	return result

#----------------------------------------------------------------------------#

def unzip(pairList):
	assert pairList
	numLists = len(pairList[0])

	newLists = []
	for i in range(numLists):
		newLists.append([])

	for pair in pairList:
		for i in range(numLists):
			newLists[i].append(pair[i])
	
	return tuple(newLists)

#----------------------------------------------------------------------------#

def frange(start, end=None, inc=None):
	""" A range function, that does accept float increments...
		http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/66472
	""" 

	if end == None:
		end = start + 0.0
		start = 0.0

	if inc == None:
		inc = 1.0

	L = []
	while 1:
		next = start + len(L) * inc
		if inc > 0 and next >= end:
			break
		elif inc < 0 and next <= end:
			break
		L.append(next)
		
	return L

#----------------------------------------------------------------------------#

def thread(inputList, tupleSize=2):
	""" Turns a list of items into a list of tuples by simply placing
		sequential items into tuples::

			> thread([1,2,3,4,5])
			[(1,2), (3,4)]

		Notice that the last element might be discarded, in a similar manner
		as when using zip.
	"""
	outputList = []

	finalElement = tupleSize-1

	i = 0
	currentTuple = ()
	while i < len(inputList):
		if i % tupleSize == finalElement:
			outputList.append(currentTuple + (inputList[i],))
			currentTuple = ()
		else:
			currentTuple += (inputList[i],)
		i += 1

	return outputList

#----------------------------------------------------------------------------#

def unthread(inputTuples):
	""" Turns a list of tuples into a flat list::

			> unthread([(1,2),(3,4)])
			[1, 2, 3, 4]
	"""
	outputList = []

	for item in inputTuples:
		outputList.extend(item)
	
	return outputList

#----------------------------------------------------------------------------#

def threadSeqIter(inputList, n=2):
	""" Takes a list and creates tuples from sequential items. Note the
		difference between this method and the earlier thread() method.
		For example::

		> list(threadSeqIter([1,2,3,4]))
		[(1,2), (2,3), (3,4)]

		@param inputList: The list which gets threaded.
	"""
	if n < 1:
		raise Exception, "Need n >= 1 for a valid thread sequence"
	for i in xrange(len(inputList)-(n-1)):
		yield tuple(inputList[i:i+n])

	return

#----------------------------------------------------------------------------#

def repeat(n, item):
	""" Creates an iterator which provides n references to item.
	"""
	for i in xrange(n):
		yield item
	return

#----------------------------------------------------------------------------#

def succession(itemList):
	""" Returns an iterator which builds up the item list point by point.
		
		For example::

			> succession([1,2,3])
			[[1], [1,2], [1,2,3]]
	"""
	for i in xrange(1, len(itemList)):
		yield itemlist[:i]
	
	return

#----------------------------------------------------------------------------#

def window(itemList, windowSize=2):
	""" Returns a sliding window iterator over the given list.

		For example::

			> window([1,2,3,4,5])
			[(1,2), (2,3), (3,4), (4,5)]

			> window([1,2,3,4,5], 3)
			[(1,2,3), (2,3,4), (3,4,5)]

		@param itemList: The list to iterate over.
		@param windowSize: The number of elements in each window frame.
	"""
	for endWindow in range(windowSize, len(itemList)+1):
		yield tuple(itemList[endWindow-windowSize:endWindow])

	return 

#----------------------------------------------------------------------------#

def groupByLambda(func, items):
	""" Performs a grouping of the items by lambda value.
	"""
	groups = {}
	for item in items:
		keyValue = func(item)

		itemList = groups.get(keyValue, [])
		itemList.append(item)
		groups[keyValue] = itemList
	
	return groups

#----------------------------------------------------------------------------#

def multiDict(inputPairs):
	""" Similar to casting pairs to a dictionary, except that repeated pairs
		are allowed. To show the difference::
		
			> dict( [('a', 1), ('b', 2), ('a', 3)] )
			{'a': 3, 'b': 2}

			> multiDict( [('a', 1), ('b', 2), ('a', 3)] )
			{'a': [1, 3], 'b': [2]}

		@param inputPairs: A list of (key, value) pairs.
		@return: A dictionary mapping keys to lists of values.
	"""
	outputDict = {}

	for key, value in inputPairs:
		existingValues = outputDict.get(key, [])
		existingValues.append(value)
		outputDict[key] = existingValues
	
	return outputDict

#----------------------------------------------------------------------------#

