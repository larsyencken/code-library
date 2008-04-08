# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# readOnly.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Fri Jan  4 16:21:19 2008
#
#----------------------------------------------------------------------------#

"""Read-only views for basic Python data structures."""

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

class ReadOnlyExceptoin(Exception):
    pass

class ReadOnlyListView(object):
    def __init__(self, backingList):
        self._backingList = backingList

    def __len__(self):
        return len(self._backingList)

    def __cmp__(self, rhs):
        return cmp(self._backingList, rhs)

    def __getitem__(self, key):
        return self._backingList[key]

    def __getslice__(self, slice):
        return self._backlingList.__getslice__(slice)

    def __add__(self, rhs):
        return self._backingList + rhs

    def __str__(self):
        return self._backingList.__str__()

    def __reduce__(self, f):
        return self._backingList.__reduce__(f)

    def __mul__(self, rhs):
        return self._backingList.__mul__(rhs)

    def count(self, key):
        return self._backingList.count(key)
