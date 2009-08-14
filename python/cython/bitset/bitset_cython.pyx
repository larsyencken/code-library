# -*- coding: utf-8 -*-
# 
#  bitset_cython.py
#  bitset
#  
#  Created by Lars Yencken on 2008-06-19.
#  Copyright 2008-06-19 Lars Yencken. All rights reserved.
# 

cdef extern from "stdio.h":
    void* malloc(size_t size)
    void free(void* ptr)

cdef class Bitset:
    """
    A boolean vector using minimal memory.
    """
    cdef readonly int max_size
    cdef readonly int data_size
    cdef char* _data

    def __cinit__(self, int max_size):
        self.max_size = max_size
        if max_size <= 1:
            raise ValueError('need to be able to hold at least one value')
        self.data_size = 1 + max_size / 8
        self._data = <char*>malloc(sizeof(char) * self.data_size)
        if self._data == NULL:
            raise Exception('out of memory')
            
        cdef int i
        for i in range(self.data_size):
            self._data[i] = 0
    
    def add(self, int key):
        if not (0 <= key <= self.max_size):
            raise KeyError("key out of range")
        
        cdef char c = self._data[key / 8]
        cdef char i = key % 8
        c = c | (1 << i)
        self._data[key/8] = c
    
    def remove(self, int key):
        if not (0 <= key <= self.max_size):
            raise KeyError("key out of range")
        cdef char c = self._data[key/8]
        c = c & (0xff ^ (1 << (key % 8)))
        self._data[key/8] = c
        
    def __contains__(self, key):
        if not (0 <= key <= self.max_size):
            raise KeyError("key out of range")
        
        return bool((self._data[key/8] >> (key % 8)) & 1)
    
    def __iter__(self):
        """Iterates over all the keys present in the set."""
        cdef int key
        result = []
        for key in range(self.max_size + 1):
            if key in self:
                result.append(key)
        
        return iter(result)

    def __dealloc__(self):
        if self._data != NULL:
            free(<void*>self._data)
    
    def get_data(self):
        cdef int i
        data = []
        for i in range(self.data_size):
            data.append(chr(self._data[i]))
        result = ''.join(data)
        return result
    
    def set_data(self, data):
        cdef int l = len(data)
        if l != self.data_size:
            raise ValueError('data is the wrong size')
        
        cdef int i
        cdef char c
        for i in range(l):
            c = ord(data[i])
            self._data[i] = c
    
def from_data(data):
    max_size = len(data) * 8 - 1
    s = Bitset(max_size)
    s.set_data(data)
    return s