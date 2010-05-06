#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  compare_em_cache.py
#  unknown project
# 
#  Created by Lars Yencken on 06-05-2010.
#  Copyright 2010 Lars Yencken. All rights reserved.
#

"""
"""

from __future__ import division

import os, sys, optparse

from simplestats import comb
from simplestats import FreqDist

def compare_em_cache(seq, alphabet, alpha=1, normalize=True):
    alphabet_size = len(set(alphabet))
    em_probs = []
    bayes_probs = []
    segment_combs = comb.segment_combinations(seq)
    for segments in segment_combs:
        em_prob = _calc_em_prob(segments, alphabet_size)
        em_probs.append(em_prob)
        
        bayes_prob = _calc_bayes_prob(segments, alphabet_size, alpha=alpha)
        bayes_probs.append(bayes_prob)

    if normalize:
        em_mass = sum(em_probs)
        em_probs = [p / em_mass for p in em_probs]

        bayes_mass = sum(bayes_probs)
        bayes_probs = [p / bayes_mass for p in bayes_probs]

    slen = len(seq) * 2 - 1
    print ('%%%ds    %%-13s    %%-13s' % slen) % ('SEGMENTS', 'EM', 'BAYES')
    max_em = max(em_probs)
    max_bayes = max(bayes_probs)

    for segments, em_prob, bayes_prob in zip(segment_combs, em_probs, 
                bayes_probs):
        is_em_max = '(*)' if em_prob == max_em else '   '
        is_bayes_max = '(*)' if bayes_prob == max_bayes else '   '

        print ('%%%ds %%s%%-13g %%s%%-13g' % slen) % ('-'.join(segments), is_em_max,
                em_prob, is_bayes_max, bayes_prob)

def _calc_em_prob(segments, alphabet_size):
    p = 1.0
    segments = list(segments)
    while segments:
        segments.pop()
        p *= 1 / alphabet_size # uniform probability
        if segments:
            p *= 0.99
        else:
            p *= 0.01

    return p

def _calc_bayes_prob(segments, alphabet_size, alpha=1):
    dist = FreqDist()
    p = 1.0
    for h, word in enumerate(segments):
        p *= (alpha * _p0_word(word, alphabet_size) + dist.count(word)) \
                / (alpha + h)
        dist.inc(word)
        h += 1
        if h == len(segments):
            p *= 0.01
        else:
            p *= 0.99

    return p

def _p0_word(word, alphabet_size):
    p = 1.0
    chars = list(word)
    while chars:
        c = chars.pop()
        p *= 0.5
        p *= 1 / alphabet_size

    return p

#----------------------------------------------------------------------------#

def _create_option_parser():
    usage = \
"""%prog [options] seq alphabet

Compares EM and Bayesian Inference for Chinese segmentation. You provide it
with a short character sequence, and then the full alphabet, and it will print
the matching probability tables.

E.g.

    %prog aaba abc"""

    parser = optparse.OptionParser(usage)

    parser.add_option('-a', '--alpha', action='store', dest='alpha',
            type='float', default=1, help='Set the alpha value to use [1]')

    parser.add_option('-n', '--no-normalization', action='store_false',
            dest='normalize', default=True,
            help='Avoid normalizing probability values')

    return parser

def main(argv):
    parser = _create_option_parser()
    (options, args) = parser.parse_args(argv)

    if len(args) != 2:
        parser.print_help()
        sys.exit(1)

    seq, alphabet = args
    compare_em_cache(seq, alphabet, normalize=options.normalize,
            alpha=options.alpha)

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

# vim: ts=4 sw=4 sts=4 et tw=78:
