# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# smartCache.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Fri Sep  9 15:17:56 EST 2005
#
#----------------------------------------------------------------------------#

""" This module implements a smart caching function, with dependencies.
"""

#----------------------------------------------------------------------------#

import cPickle as pickle
import time
from os import path
import codecs
import bz2, gzip
import sys

#----------------------------------------------------------------------------#

def smartOpen(filename, mode='rb', encoding=None):
	""" Transparently uses compression on the given file based on file
		extension.
	"""
	readMode = 'r' in mode
	if readMode and 'w' in mode:
		raise Exception, "Must be either read mode or write, but not both"

	if filename.endswith('.bz2'):
		stream = bz2.BZ2File(filename, mode)
	elif filename.endswith('.gz'):
		stream = gzip.GzipFile(filename, mode)
	elif filename == '-':
		if readMode:
			stream = sys.stdin
		else:
			stream = sys.stdout
	else:
		stream = open(filename, mode)
	
	if encoding is not None:
		if readMode:
			return codecs.getreader('utf8')(stream)
		else:
			return codecs.getwriter('utf8')(stream)
	
	return stream

#----------------------------------------------------------------------------#

def cachedCall(filename, factoryMethod, factoryArgs, dependencies,
		**parameters):
	""" The one-stop-shop for caching methods. Transparently caches the result
		of the call it encapsulates.

		@param filename: The file to cache to.
		@param factoryMethod: The method used to create new objects.
		@param factoryArgs: The args to use in creating new objects.
		@param dependencies: Any additional variable dependencies can be added
			as arguments.

		@return: The result of calling apply(factoryMethod, factoryArgs).
	"""
	
	# tricky footwork to allow factory parameters and dependency parameters
	factoryParams = {}
	if parameters.has_key('factoryParams'):
		factoryParams = parameters['factoryParams']
		del parameters['factoryParams']
	
	# is there a valid object in the cache?
	object = apply(useCache, (filename,), parameters)
	if object is not None:
		# yes! return it
		return object
	
	# no, create a new object
	object = apply(factoryMethod, factoryArgs, factoryParams)

	# cache it for the next run
	apply(recache, [object, filename, dependencies], parameters)

	return object

#----------------------------------------------------------------------------#

def useCache(filename, **parameters):
	""" Determines whether the cached object is still fresh (if one exists),
		and if so returns that object. Otherwise returns None.
	"""
	if not path.exists(filename):
		return None

	iStream = smartOpen(filename)
	dateStamp = pickle.load(iStream)
	storedParams = pickle.load(iStream)

	if dateStamp.isCurrent() and storedParams == parameters:
		object = pickle.load(iStream)
		iStream.close()
		return object
	else:
		iStream.close()
		return None

#----------------------------------------------------------------------------#

def recache(object, filename, dependencies, **parameters):
	""" Creates a smart cache object in the file.
	"""
	dateStamp = DateStamp(dependencies)

	oStream = smartOpen(filename, 'w')
	pickle.dump(dateStamp, oStream)
	pickle.dump(parameters, oStream)
	pickle.dump(object, oStream)
	oStream.close()
	
	return

#----------------------------------------------------------------------------#

class DateStamp:
	""" This class takes a snapshot of when a number of files were last
		accessed, and can be used to determine whether any haves since
		changed.
	"""
	#------------------------------------------------------------------------#
	# PUBLIC METHODS
	#

	def __init__(self, filenames):
		"""
		"""
		self._filenames = filenames
		self._dates = self._getDates()

		return

	#------------------------------------------------------------------------#

	def isCurrent(self):
		""" Determines whether any of the files have changed since the last
			update.
		"""
		currentDates = self._getDates()

		return currentDates == self._dates

	#------------------------------------------------------------------------#

	def update(self):
		self._dates = self._getDates()
		return

	#------------------------------------------------------------------------#
	# PRIVATE METHODS
	#

	def _getDates(self):
		""" Gets the time each file was modified.
		"""
		dates = []

		for filename in self._filenames:
			if not path.exists(filename):
				raise Exception, "Can't find file dependency %s" % `filename`
			else:
				dates.append(path.getmtime(filename))

		return dates
	
	#------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
