#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# cleanUtf8.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Wed Nov  7 12:18:22 CET 2007
#
#----------------------------------------------------------------------------#

import os, sys, optparse
import codecs
import shutil

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#
 
def cleanUtf8(filename):
    """
    Cleans the input file by normalizing newlines and removing the standard
    utf8 BOM.
    """
    sys.stdout.write('%s:' % filename)
    sys.stdout.flush()
    iStream = codecs.open(filename, 'r', 'utf8')
    try:
        inputLines = iStream.readlines()
    except:
        print ' not utf8'
        iStream.close()
        return

    iStream.close()

    hasDifference = False
    outputLines = []
    for line in inputLines:
        cleanedLine = _cleanLine(line)
        if cleanedLine != line:
            hasDifference = True
        outputLines.append(cleanedLine)

    if hasDifference:
        print ' fixing'
        shutil.move(filename, filename + '.bak')
        oStream = sopen(filename, 'wb', 'utf8')
        for line in outputLines:
            oStream.write(line)
        oStream.close()
    else:
        print ' ok'

    return

#----------------------------------------------------------------------------#
# PRIVATE
#----------------------------------------------------------------------------#

def _cleanLine(line):
    return line.replace('\r', '').replace(u'\ufeff', '')

#----------------------------------------------------------------------------#
# MODULE EPILOGUE
#----------------------------------------------------------------------------#

def _createOptionParser():
    """
    Creates an option parser instance to handle command-line options.
    """
    usage = \
"""%prog [options] file1 [file2 [...]] 

Cleans all UTF-8 text files specified as arguments."""

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

    if not args:
        parser.print_help()
        sys.exit(1)

    # Avoid psyco in debugging mode, since it merges stack frames.
    if not options.debug:
        try:
            import psyco
            psyco.profile()
        except:
            pass

    for filename in args:
        cleanUtf8(filename)
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:

