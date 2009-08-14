# -*- coding: utf-8 -*-
# 
#  dummy.py
#  bitset
#  
#  Created by Lars Yencken on 2008-06-19.
#  Copyright 2008-06-19 Lars Yencken. All rights reserved.
# 

"""
Simple script designed to test memory usage of the interpreter.
"""

if __name__ == '__main__':
    x = set()
    for j in xrange(3):
        for i in xrange(10000000):
            x.add(i)
