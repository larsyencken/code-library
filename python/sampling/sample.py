# -*- coding: utf-8 -*-
#
#  sample.py
#  unknown project
# 
#  Created by Lars Yencken on 12-03-2010.
#  Copyright 2010 Lars Yencken. All rights reserved.
#

"""
"""

import random

from simplestats import FreqDist
from consoleLog import withProgress

def test_sample(m, n, method, iterations=1000):
    dist = FreqDist()
    for i in withProgress(xrange(iterations)):
        for s in method(m, n):
            dist.inc(s)
    
    min_prob, min_v = min((dist.prob(k), k) for k in dist.iterkeys())
    max_prob, max_v = max((dist.prob(k), k) for k in dist.iterkeys())

    print "Min:", min_prob, min_v
    print "Max:", max_prob, max_v
    print "Diff:", abs(max_prob - min_prob)

def knuth_sample(m, n):
    "Choose m samples from a pool of n with uniform probability."
    select = m
    remaining = n
    for i in xrange(n):
        if random.randint(0, remaining - 1) < select:
            yield i
            select -= 1

            if select == 0:
                break

        remaining -= 1

if __name__ == "__main__":
    import sample_cython
    sample_cython.test_sample(13, 30)
#    test_sample(1000, 30000, knuth_sample)

# vim: ts=4 sw=4 sts=4 et tw=78:

