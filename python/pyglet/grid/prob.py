# -*- coding: utf-8 -*-
# 
#  prob.py
#  grid
#  
#  Created by Lars Yencken on 2009-10-25.
#  Copyright 2009 Lars Yencken. All rights reserved.
# 

from __future__ import division

import random

class ProbDist(dict):
    def __init__(self, *args, **kwargs):
        dict.__init__(self, *args, **kwargs)
        self.normalise()
        self._refresh_cdf()

    @classmethod
    def from_query_set(cls, query_set):
        dist = cls()
        for row in query_set.values('symbol', 'pdf'):
            dist[row['symbol']] = row['pdf']

        dist.normalise()
        return dist

    def copy(self):
        new_dist = ProbDist(self)
        new_dist._cdf = self._cdf[:]
        return new_dist

    def __eq__(self, rhs):
        return set(self.items()) == set(rhs.items())

    def normalise(self):
        total = sum(self.itervalues())
        for key in self:
            self[key] /= total
        self._refresh_cdf()

    def save_to(self, manager, **kwargs):
        manager.filter(**kwargs).delete()
        cdf = 0.0
        for symbol, pdf in self.iteritems():
            row_kwargs = {}
            row_kwargs.update(kwargs)
            cdf += pdf
            row_kwargs['cdf'] = cdf
            row_kwargs['pdf'] = pdf
            row_kwargs['symbol'] = symbol
            manager.create(**row_kwargs)

        return

    def sample(self):
        target_cdf = random.random()
        for cdf, symbol in self._cdf:
            if cdf >= target_cdf:
                return symbol
        raise RuntimeError("couldn't sample successfully")

    def sample_n(self, n, exclude_set=None):
        exclude_set = exclude_set or set()

        include_set = set(self.keys()).difference(exclude_set)
        if n > len(include_set):
            raise ValueError("don't have %d unique values" % n)

        elif n == len(include_set):
            result = list(include_set)
            random.shuffle(result)
            return result

        tmp_dist = self.copy()
        for symbol in tmp_dist.keys():
            if symbol not in include_set:
                del tmp_dist[symbol]
        tmp_dist.normalise()

        result = set()
        while len(result) < n:
            symbol = tmp_dist.sample()
            result.add(symbol)
            del tmp_dist[symbol]
            tmp_dist.normalise()

        result = list(result)
        random.shuffle(result)
        return result

    def _refresh_cdf(self):
        cdf_seq = []
        cdf = 0.0
        for symbol, pdf in self.iteritems():
            cdf += pdf
            cdf_seq.append((cdf, symbol))

        self._cdf = cdf_seq
