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

cdef extern from "stdlib.h":
    int rand()

def test_sample(int m, int n, int iterations=100000):
    cdef int i
    cdef int dist[100000]
    cdef int j
    cdef int select
    cdef int remaining
    for i in range(n):
        dist[i] = 0
    r = random.randint
    for i in range(iterations):
        select = m
        remaining = n
        for j in range(n):
            if rand() % remaining < select:
                dist[j] += 1
                select -= 1

                if select == 0:
                    break

            remaining -= 1

    total = 100000 * m

    min_k = 0
    min_v = dist[0]
    max_v = dist[0]
    max_k = 0
    for i in range(n):
        if dist[i] < min_v:
            min_v = dist[i]
            min_k = i

        if dist[i] > max_v:
            max_v = dist[i]
            max_k = i

    max_prob = max_v / float(total)
    min_prob = min_v / float(total)

    print "Min:", min_prob, min_v
    print "Max:", max_prob, max_v
    print "Diff:", abs(max_prob - min_prob)

# vim: ts=4 sw=4 sts=4 et tw=78:

