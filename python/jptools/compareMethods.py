#!/usr/bin/python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# compareMethods.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Thu May  4 14:21:34 EST 2006
#
#----------------------------------------------------------------------------#

import os, sys, optparse, codecs, time

from jptools.smartCache import smartOpen 
from jptools.functional import thread

#----------------------------------------------------------------------------#
# PUBLIC METHODS
#----------------------------------------------------------------------------#

def compareMethods(method):
    if not methodComparisons.has_key(method):
        print >> sys.stderr, "No comparison written for method %s" % `method`
        return

    methodComparisons[method]()

#----------------------------------------------------------------------------#

def compareUnzip():
    print 'Comparing unzip'
    print '--> Using edict as input'
    data = smartOpen('/home/lars/Documents/Research/data/dict/edict.gz').read()
    data = data.split()

    splitIndex = len(data) /2
    data = zip(data[:splitIndex], data)

    n = 100

    print '--> Timing python'
    from pytools.functional import unzip as unzip_py
    total_py = 0
    for i in xrange(100):
        result_py, time_py = timeCall(unzip_py, [data])
        total_py += time_py
    time_py = total_py
    print '----> %.2f seconds' % time_py

    print '--> Timing cpp'
    from jptools.functional import unzip as unzip_cpp
    total_cpp = 0
    for i in xrange(100):
        result_cpp, time_cpp = timeCall(unzip_cpp, [data])
        total_cpp += time_cpp
    time_cpp = total_cpp
    print '----> %.2f seconds' % time_cpp

    if time_py < time_cpp:
        print '--> Python is %.0f%% faster than C++' % (100*time_cpp/float(time_py))
    else:
        print '--> C++ is %.0f%% faster than Python' % (100*time_py/float(time_cpp))

    if result_py == result_cpp:
        print '--> Results were matching'
    else:
        print '--> Results diverged'

    return

def timeCall(method, methodArgs=[], dictArgs={}, nIterations=1):
    startTime = time.time()
    for i in xrange(nIterations):
        result = apply(method, methodArgs, dictArgs)
    timeTaken = time.time() - startTime

    return result, timeTaken

#----------------------------------------------------------------------------#

methodComparisons = {
        'unzip':    compareUnzip, 
    }

#----------------------------------------------------------------------------#
# PRIVATE METHODS
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# MODULE EPILOGUE
#----------------------------------------------------------------------------#

def _createOptionParser():
    """ Creates an option parser instance to handle command-line options.
    """
    usage = \
"""%prog [options] method1 [method2 [...]] 

Runs comparison profiles for the methods suggested."""

    parser = optparse.OptionParser(usage)

    parser.add_option('--debug', action='store_true', dest='debug',
            default=False, help='Enables debugging mode [False]')

    return parser

#----------------------------------------------------------------------------#

def main(argv):
    """ The main method for this module.
    """
    parser = _createOptionParser()
    (options, args) = parser.parse_args(argv)

    if not args:
        parser.print_help()
        sys.exit(1)

    if not options.debug:
        # we don't want psyco in debugging mode, since it merges together
        # stack frames
        try:
            import psyco
            psyco.profile()
        except:
            pass

    for method in args:
        compareMethods(method)

    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:

