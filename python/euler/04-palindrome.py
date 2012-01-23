#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  04-palindrome.py
#  code
#
#  Created by Lars Yencken on 2012-01-24.
#  Copyright 2012 Lars Yencken. All rights reserved.
#

"""
Find the largest palindrome made from the product of two 3-digit numbers.
"""

def is_palindrome(x):
    x = str(x)
    return x == x[::-1]

def iter_palindromes():
    for i in xrange(100, 1000):
        for j in xrange(100, 1000):
            x = i * j
            if is_palindrome(x):
                yield x

print max(iter_palindromes())
