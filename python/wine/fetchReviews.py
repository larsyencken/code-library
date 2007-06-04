#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# fetchReviews.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Thu May 17 16:58:44 EST 2007
#
#----------------------------------------------------------------------------#

import os, sys, optparse
import re
import urllib

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#

_startRange = 1
_stopRange = 3370
_baseUrl = 'http://www.winestate.com.au/5star/5star.asp?wineno=%.04d'
_pattern = re.compile('<td width="66%" class="Standard12ptGrey">(.*?)</td>.*Review :(.*?)</td>', re.MULTILINE)

def fetchReviews(outputFile):
    """ Fetches wine reviews and dumps them to the given file, one per line.
    """
    print 'Fetching reviews...'
    oStream = open(outputFile, 'w')
    for i in xrange(_startRange, _stopRange + 1):
        iStream = urllib.urlopen(_baseUrl % i)
        data = iStream.read()
        data = data.replace('\r\n', ' ').replace('\t', ' ')
        data = re.sub('[ ]+', ' ', data)
        iStream.close()

        try:
            [(title, review)] = _pattern.findall(data)
            print '%d. %s' % (i, title)

            print >> oStream, '%s|%s' % (title, review)
            oStream.flush()
        except (TypeError, ValueError):
            pass
    oStream.close()
    print 'Done'

    return

#----------------------------------------------------------------------------#
# PRIVATE
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# MODULE EPILOGUE
#----------------------------------------------------------------------------#

def _createOptionParser():
    """ Creates an option parser instance to handle command-line options.
    """
    usage = \
"""%prog [options] outputFile

Mines a large number of concise wine reviews from an online web site, and dumps
them to the given filename."""

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
        [outputFile] = args
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

    fetchReviews(outputFile)
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:

