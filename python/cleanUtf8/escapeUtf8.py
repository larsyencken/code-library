#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# escapeUtf8.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Wed Nov 14 18:29:24 CET 2007
#
#----------------------------------------------------------------------------#

import os, sys, optparse

from cjktools.common import sopen

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#
 
def escapeUtf8(inputFile, outputFile):
    """
    """
    raise Exception, "Not yet implemented"

#----------------------------------------------------------------------------#

def unescapeUtf8(inputFile, outputFile):
    iStream = open(inputFile, 'r')
    oStream = sopen(outputFile, 'w', 'utf8')

    for line in iStream:
        line = unicode(line.replace('\\N', '\\\\N'), 'unicode-escape')
        oStream.write(line)

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
    """Creates an option parser instance to handle command-line options."""
    usage = \
"""%prog [options] inputFile outputFile

Escape (or unescape) non-ASCII UTF-8 characters in the input file."""

    parser = optparse.OptionParser(usage)

    parser.add_option('--debug', action='store_true', dest='debug',
            default=False, help='Enables debugging mode [False]')

    parser.add_option('-u', '--unescape', action='store_false',
            dest='escape', default=True, help="Unescape the input.")

    return parser

#----------------------------------------------------------------------------#

def main(argv):
    """The main method for this module."""
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

    if options.escape:
        escapeUtf8(inputFile, outputFile)
    else:
        unescapeUtf8(inputFile, outputFile)
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:
