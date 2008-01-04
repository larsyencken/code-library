#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# timeModules.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Thu Jul 26 13:12:18 2007
#
#----------------------------------------------------------------------------#

""" 
"""

#----------------------------------------------------------------------------#

from time import time
from jptools.sequences import head
import random

#----------------------------------------------------------------------------#

def randomSeq():
    """An infinite random sequence."""
    while True:
        yield random.random()

    return

#----------------------------------------------------------------------------#

def timeStatement(statement, globals={}, locals={}):
    t = time()
    eval(statement, globals, locals)
    return time() - t

#----------------------------------------------------------------------------#

class StatsTests:
    def __init__(self, modules):
        self.modules = modules

    def profileMean(self):
        nRepeats = 100
        dataSize = 20000

        data = []
        for i in xrange(nRepeats):
            data.append(head(dataSize, randomSeq()))

        results = []
        for module in self.modules:
            t = 0.0
            for i in xrange(nRepeats):
                t += timeStatement('mean(data)',
                            {'mean': module.mean, 'data': data[i]})

            results.append(t)

        return tuple(results)

    def profileStddev(self):
        nRepeats = 100
        dataSize = 20000
        data = []
        for i in xrange(nRepeats):
            data.append(head(dataSize, randomSeq()))

        results = []
        for module in self.modules:
            t = 0.0
            for i in xrange(nRepeats):
                t += timeStatement('mean(data)',
                            {'mean': module.mean, 'data': data[i]})

            results.append(t)

        return tuple(results)

    def profileCombinations(self):
        nRepeats = 1
        nCombs = 9
        nChoices = 4

        data = []
        for i in xrange(nCombs):
            data.append(list(head(nChoices, randomSeq())))

        results = []
        for module in self.modules:
            t = 0.0
            for i in xrange(nRepeats):
                t += timeStatement('combinations(*data)',
                            {'combinations': module.combinations,
                            'data': data})

            results.append(t)

        return tuple(results)

    def profileICombinations(self):
        nRepeats = 1
        nCombs = 9
        nChoices = 4

        data = []
        for i in xrange(nCombs):
            data.append(list(head(nChoices, randomSeq())))

        results = []
        for module in self.modules:
            t = 0.0
            for i in xrange(nRepeats):
                t += timeStatement('list(icombinations(*data))',
                            {'icombinations': module.icombinations,
                            'data': data})

            results.append(t)

        return tuple(results)


#----------------------------------------------------------------------------#

from jptools import stats, cStats

statsTests = StatsTests([stats, cStats])

if __name__ == '__main__':
    print '       %20s %20s' % tuple([m.__name__ for m in statsTests.modules])
    print 'mean   %20f %20f' % statsTests.profileMean()
    print 'stddev %20f %20f' % statsTests.profileStddev()
    print 'combs  %20f %20f' % statsTests.profileCombinations()
    print 'icombs %20f %20f' % statsTests.profileICombinations()

