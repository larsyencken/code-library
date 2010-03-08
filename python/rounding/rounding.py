#!/usr/bin/env python
# -*- coding: utf-8 -*-
# 
#  rounding.py
#  code examples
#  
#  Created by Lars Yencken on 2010-03-08.
#  Copyright 2010 Lars Yencken. All rights reserved.
# 

"""
A basic implementation of the d'Hondt method of stratifying a sample.
"""

from simplestats import FreqDist

def print_dist(dist):
    for (key, count) in sorted(dist.items(), key=lambda p: -p[1]):
        print "\t%s ---> %d (%5.02f%%)" % (key, count,
                100 * count / float(dist.total))

input_dist = FreqDist()
for label, count in [
            ('Fred', 20),
            ('Sally', 5),
            ('Sam', 100),
            ('James', 1),
        ]:
    input_dist.inc(label, count)

print "INPUT:"
print_dist(input_dist)

def sample(n, dist):
    alloc_dist = FreqDist()
    i = 0
    while i < n:
        quotient, label = max(
                (c / (1.0 + alloc_dist.count(l)), l) \
                for (l, c) in dist.iteritems()
            )
        alloc_dist.inc(label)
        i += 1
    return alloc_dist

print

for sample_size in (10, 20, 35):
    print "SAMPLE OF %d" % sample_size
    sample_dist = sample(sample_size, input_dist)
    print_dist(sample_dist)
    print