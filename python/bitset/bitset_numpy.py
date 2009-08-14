# -*- coding: utf-8 -*-
#
#  bitset_numpy.py
#  unknown project
# 
#  Created by Lars Yencken on 14-08-2009.
#  Copyright 2009 Lars Yencken. All rights reserved.
#

"""
"""

import numpy

class Bitset(object):
    def __init__(self, max_size):
        self.max_size = max_size
        self._array = numpy.zeros(max_size + 1, dtype=bool)
    
    def __contains__(self, key):
        return self._array[key]
    
    def add(self, key):
        self._array[key] = True
    
    def remove(self, key):
        self._array[key] = False
    
    def __iter__(self):
        for i in xrange(max_size + 1):
            if self._array[i]:
                yield i

if __name__ == '__main__':
    b = Bitset(10000000)
    for j in xrange(3):
        for i in xrange(10000000):
            b.add(i)

# vim: ts=4 sw=4 sts=4 et tw=78:

