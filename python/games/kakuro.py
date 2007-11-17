# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# kakuro.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Fri Nov 16 23:53:44 2007
#
#----------------------------------------------------------------------------#

"""Karuko helper."""

#----------------------------------------------------------------------------#

def solvable(tSum, k, minN=1):
    """
    Returns True if the problem is solvable.

    @param tSum The target sum desired.
    @param k The number of slots to use.
    @param minN The smallest number still available.
    """
    if minN + (k-1) > 9:
        return False

    minSum = sum(range(minN, minN + k))
    return minSum <= tSum

def _options(tSum, k, minN=1):
    """
    Returns all valid options for the list.

    >>> list(_options(4, 2))
    [[1, 3]]

    >>> list(_options(3, 2))
    [[1, 2]]

    >>> list(_options(3, 1))
    [[3]]
    """
    if k == 1:
        if tSum <= 9:
            yield [tSum]
        return

    i = minN
    for i in range(minN, 10):
        if solvable(tSum - i, k - 1, i + 1):
            for choice in _options(tSum - i, k - 1, i + 1):
                yield [i] + choice
    return

#----------------------------------------------------------------------------#

def _remainder(x, y, existingSet=[]):
    results = []
    existingSet = set(existingSet)
    for choice in _options(x, y):
        if existingSet.issubset(choice):
            results.append(list(sorted(set(choice).difference(existingSet))))

    return results

def intersect(*setDescs):
    results = _optionSet(_remainder(*setDescs[0]))
    for setDesc in setDescs[1:]:
        results = results.intersection(_optionSet(_remainder(*setDesc)))

    return list(sorted(results))

i = intersect

def _optionSet(optionLists):
    baseSet = set()
    for optionList in optionLists:
        baseSet.update(optionList)

    return baseSet

#----------------------------------------------------------------------------#

