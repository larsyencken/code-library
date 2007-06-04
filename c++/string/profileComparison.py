#!/usr/bin/python

import time, sys

import cppString
import pyString

nQuadraticAdds = 30000
nConcats = 10000000
nInterface = 1000000

for module in cppString, pyString:
    print 'Testing %s' % `module`

    print '--> %12s: ' % 'Quadratic', ; sys.stdout.flush()
    startTime = time.time()
    module.cppQuadraticTest(nQuadraticAdds)
    print '%.2f seconds' % (time.time() - startTime)

    print '--> %12s: ' % 'Concat', ; sys.stdout.flush()
    startTime = time.time()
    module.cppConcatTest(nConcats)
    print '%.2f seconds' % (time.time() - startTime)

    print '--> %12s: ' % 'Interface', ; sys.stdout.flush()
    startTime = time.time()
    for i in xrange(nInterface):
        x = module.interfaceTest('something short and sweet')
    print '%.2f seconds' % (time.time() - startTime)

    print
    print '----------'
    print

