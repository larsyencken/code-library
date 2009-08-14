# -*- coding: utf-8 -*-
# 
#  bitset.py
#  bitset
#  
#  Created by Lars Yencken on 2008-06-19.
#  Copyright 2008-06-19 Lars Yencken. All rights reserved.
# 

import ctypes

class Bitset(object):
    """
    A boolean vector using minimal memory.
    
    >>> b = Bitset(10)
    >>> b.add(5)
    >>> 5 in b
    True
    >>> 6 in b
    False
    >>> list(b)
    [5]
    >>> b.remove(5)
    >>> 5 in b
    False
    >>> list(b)
    []
    """
    def __init__(self, max_size):
        self.max_size = max_size
        if max_size <= 1:
            raise ValueError('need to be able to hold at least one value')
        self.data = ctypes.create_string_buffer(1 + max_size/8)
    
    def add(self, key):
        if not (0 <= key <= self.max_size):
            raise KeyError("key out of range")
        self.data[key/8] = chr(ord(self.data[key/8]) | (1 << (key % 8)))
    
    def remove(self, key):
        if not (0 <= key <= self.max_size):
            raise KeyError("key out of range")
        self.data[key/8] = chr(ord(self.data[key/8]) & (0xff ^ (1 << (key % 8))))   
        
    def __contains__(self, key):
        if not (0 <= key <= self.max_size):
            raise KeyError("key out of range")
        return bool((ord(self.data[key/8]) >> (key % 8)) & 1)
    
    def __iter__(self):
        """Iterates over all the keys present in the set."""
        for key in xrange(self.max_size + 1):
            if key in self:
                yield key


if __name__ == '__main__':
    b = Bitset(10000000)
    for j in xrange(3):
        for i in xrange(10000000):
            b.add(i)
    
