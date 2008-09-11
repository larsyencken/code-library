# -*- coding: utf-8 -*-
# 
#  subset.py
#  subset_sum
#  
#  Created by Lars Yencken on 2008-06-20.
#  Copyright 2008-06-20 Lars Yencken. All rights reserved.
# 

"""
The set bisection problem as asked by Raul in the interview.
"""

import bisect

def sorted_contains(seq, target):
    return seq[bisect.bisect_left(seq, target)] == target

def subset_sum_n2(num_list, target):
    """
    Determines if any two numbers in the list add up to the target. Does this
    in O(n^2) time.
    
    >>> subset_sum_n2([2, 10, 3, -20, 5, 1], -15)
    True
    >>> subset_sum_n2([2, 10, 3, -20, 5, 1], -100)
    False
    """
    for item_a in num_list:
        for item_b in num_list:
            if item_a + item_b == target:
                return True
    return False

def subset_sum(num_list, target):
    """
    Determines if any two numbers in the list add up to the target. Does this
    in O(nlogn) time.
    
    >>> subset_sum([2, 10, 3, -20, 5, 1], -15)
    True
    >>> subset_sum([2, 10, 3, -20, 5, 1], -100)
    False
    """
    num_list.sort()
    if sorted_contains(num_list, target):
        return True

    for item in num_list:
        diff = target - item
        if sorted_contains(num_list, diff):
            return True
    
    return False
