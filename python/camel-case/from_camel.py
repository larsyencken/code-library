#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  from_camel.py
#  camel-case
# 
#  Created by Lars Yencken on 06-10-2008.
#  Copyright 2008 Lars Yencken. All rights reserved.
#

"""

"""

import sys
import codecs
import re

def from_camel(filename):
    """Converts all variable names in camel case."""
    i_stream = codecs.open(filename, 'r', 'utf8')
    o_stream = codecs.open(filename + '.bak', 'w', 'utf8')
    for line in i_stream:
        o_stream.write(_line_from_camel(line))
    i_stream.close()
    o_stream.close()

_camel_pattern = re.compile(r'_?[a-z]+[A-Z][a-zA-Z]+', re.UNICODE)

def _line_from_camel(line):
    """
    >>> _line_from_camel('dogEatDog')
    'dog_eat_dog'
    >>> _line_from_camel("def eat_dog(a, b): return franken_muffin")
    'def eat_dog(a, b): return franken_muffin'
    """
    match = _camel_pattern.search(line)
    while match:
        start, end = match.span()
        first_piece = line[:start]
        end_piece = line[end:]
        line = first_piece + _word_from_camel(line[start:end]) + end_piece
        match = _camel_pattern.search(line)
    return line

def _word_from_camel(word):
    """
    >>> _word_from_camel('dogEatDog')
    'dog_eat_dog'
    >>> _word_from_camel('eatAPI')
    'eat_a_p_i'
    """
    pieces = []
    for char in word:
        if char.isupper():
            pieces.append('_' + char.lower())
        else:
            pieces.append(char)
    return ''.join(pieces)

def main():
    filename = None
    try:
        (filename,) = sys.argv[1:]
    except ValueError:
        print >> sys.stderr, "Usage: from_camel.py filename.py"
    
    from_camel(filename)

if __name__ == '__main__':
    main()

# vim: ts=4 sw=4 sts=4 et tw=78:
