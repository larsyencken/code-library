#!/usr/bin/python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# runTests.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Mon Feb 20 23:07:51 EST 2006
#
#----------------------------------------------------------------------------#

import os, sys
execDir = os.path.dirname(sys.argv[0])
sys.path.append(os.path.join(execDir, '..'))
import unittest

import testSql
import testFunctional
import testSmartCache
import testStats
import testTable

#----------------------------------------------------------------------------#

suite = unittest.TestSuite((
		testSql.suite(),
		testFunctional.suite(),
		testSmartCache.suite(),
		testStats.suite(),
		testTable.suite()
	))

unittest.TextTestRunner(verbosity=1).run(suite)

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 noet tw=78:

