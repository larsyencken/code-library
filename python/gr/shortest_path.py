#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  shortest_path.py
#  code
#
#  Created by Lars Yencken on 2010-11-19.
#  Copyright 2010 NICTA. All rights reserved.
#

"""
Finds the shortest path between vocabulary-listed pairs within a GR parse
tree.
"""

import sys
import optparse
import collections
import itertools

Graph = collections.namedtuple('Graph', 'parent conj selected')
Path = collections.namedtuple('Path', 'start end path')
Edge = collections.namedtuple('Edge', 'label source')

def shortest_path(vocab_file, gr_file):
    vocab = set(l.rstrip() for l in open(vocab_file))
    graph = _load_graph(gr_file, vocab)
    for sp in _find_shortest_paths(graph):
        if len(sp.path) <= 9:
            _print_path(sp)

def _print_path(p):
    print '1', ' '.join(['@'] + p.path + ['@'])
    print '1 %s %s' % (p.start, p.end)

def _load_graph(gr_file, vocab):
    parent = {}
    conj = {}
    selected = set()
    for line in open(gr_file):
        if not (line.startswith('(') and line.endswith(')\n')):
            raise ValueError(line)
        label, v_from, v_to = _parse_edge(line[1:-2])

        if v_to.rsplit('_')[0] in vocab:
            selected.add(v_to)

        if label.startswith('conj'):
            assert v_to not in conj
            conj[v_to] = Edge(label, v_from)
        else:
            assert v_to not in parent
            parent[v_to] = Edge(label, v_from)

    return Graph(parent, conj, selected)

def _parse_edge(gr_edge):
    parts = gr_edge.split()
    if len(parts) == 3:
        return parts
    elif len(parts) == 4:
        label = parts[0]
        if label.endswith('subj'):
            return parts[:3]
        elif label.endswith('mod') or label.endswith('comp'):
            del parts[1]
            return parts

    raise ValueError(gr_edge)

def _find_shortest_paths(graph):
    parent, conj, selected = graph
    for v_a, v_b in itertools.product(selected, selected):
        if _nl(v_a) == _nl(v_b):
            continue

        if v_a in conj and conj.get(v_a) == graph.conj.get(v_b):
            label, v_conj = conj.get(v_a)
            path = ['[%s]' % _rev(label), _nl(v_conj), '[%s]' % label]
            yield Path(_nl(v_a), _nl(v_b), path)

        # XXX find least common subsumer
        path_a = []
        e = Edge(None, v_a)
        while e.source in parent:
            e = parent[e.source]
            path_a.append(e)

        path_b = []
        e = Edge(None, v_b)
        while e.source in parent:
            e = parent[e.source]
            path_b.append(e)

        # trim the end
        while len(path_a) > 1 and len(path_b) > 1 and path_a[-1] == path_b[-1]:
            path_a.pop()
            path_b.pop()

        path = []
        for e in path_a:
            path.append("[%s']" % e.label)
            path.append(_nl(e.source))

        first = True
        for e in reversed(path_b):
            if not first:
                path.append(_nl(e.source))
            path.append("[%s]" % e.label)
            first = False

        yield Path(_nl(v_a), _nl(v_b), path)

def _nl(n):
    return n.rsplit('_', 1)[0]

def _rev(l):
    return "%s'" % l

#----------------------------------------------------------------------------#

def _create_option_parser():
    usage = \
"""%prog [options] input.vocab input.grparse

Calculates and prints out the shortest path between all in-vocabulary nodes
in the given GR parse tree."""

    parser = optparse.OptionParser(usage)

    return parser

def main(argv):
    parser = _create_option_parser()
    (options, args) = parser.parse_args(argv)

    if len(args) != 2:
        parser.print_help()
        sys.exit(1)

    shortest_path(*args)

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

