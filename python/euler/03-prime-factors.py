# -*- coding: utf-8 -*-
#
#  03-prime-factors.py
#  code
#
#  Created by Lars Yencken on 2012-01-15.
#  Copyright 2012 Lars Yencken. All rights reserved.
#

"""
Calculate the largest prime factor of 600851475143.
"""

def factorize(x):
    if x <= 1:
        return set()

    factors = set()
    for i in xrange(2, x + 1):
        if x % i == 0:
            factors.add(i)
            return factors.union(factorize(x / i))

    return factors

print max(factorize(600851475143))
