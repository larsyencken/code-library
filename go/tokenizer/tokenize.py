#!/usr/bin/env python

"""
A python tokenizer for comparison.
"""

import sys
import re

pattern = re.compile('[a-zA-Z]+|[0-9]+|[^A-Za-z \t]')
for line in sys.stdin:
    for tok in pattern.findall(line.rstrip()):
        print tok,
    print
