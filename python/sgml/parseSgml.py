#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# parseSgml.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Oct 23 04:39:44 EST 2007
#
#----------------------------------------------------------------------------#

import os, sys, optparse
import re

from cjktools.common import sopen

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#
 
def parseSgml(inputFile, outputFile):
    """
    """
    iStream = sopen(inputFile, 'r')
    oStream = sopen(outputFile, 'w')

    startSentence = '<s>'
    endSentence = '</s>'
    pat = re.compile(r'<s>(.+?)</s>', re.MULTILINE | re.DOTALL | re.UNICODE) 

    blockSize = 1024*1024
    block = iStream.read(blockSize)
    while block:
        for match in pat.finditer(block):
            print >> oStream, match.group(1).replace('\n', ' ').strip(u'　 ')
        block = iStream.read(blockSize)

    oStream.close()
    iStream.close()
    return

#----------------------------------------------------------------------------#
# PRIVATE
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# MODULE EPILOGUE
#----------------------------------------------------------------------------#

def _createOptionParser():
    """
    Creates an option parser instance to handle command-line options.
    """
    usage = \
"""%prog [options] inputFile outputFile

Extracts just the sentences from an SGML file."""

    parser = optparse.OptionParser(usage)

    parser.add_option('--debug', action='store_true', dest='debug',
            default=False, help='Enables debugging mode [False]')

    return parser

#----------------------------------------------------------------------------#

def main(argv):
    """ The main method for this module.
    """
    parser = _createOptionParser()
    (options, args) = parser.parse_args(argv)

    try:
        [inputFile, outputFile] = args
    except:
        parser.print_help()
        sys.exit(1)

    # Avoid psyco in debugging mode, since it merges stack frames.
    if not options.debug:
        try:
            import psyco
            psyco.profile()
        except:
            pass

    parseSgml(inputFile, outputFile)
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:

