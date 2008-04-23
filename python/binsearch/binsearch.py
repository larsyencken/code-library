# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# binsearch.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Aug 28 12:36:40 2007
#
#----------------------------------------------------------------------------#

""" 
"""

#----------------------------------------------------------------------------#

def binsearch(x, items):
    """
    >>> binsearch(2, [1, 2, 3, 4])
    1
    >>> binsearch(5, [1, 2, 3, 4])
    >>> import random; x = random.sample(xrange(10000000), 1000); x.sort()
    >>> binsearch(x[591], x) == 591
    True
    """
    assert type(items) == list
    l = 0
    r = len(items) - 1
    while l <= r:
        p = (l + r) / 2
        c = cmp(items[p], x)
        if c > 0:
            # Greater than x
            r = p - 1
        elif c < 0:
            l = p + 1
        else:
            return p

    else:
        return None

#----------------------------------------------------------------------------#

