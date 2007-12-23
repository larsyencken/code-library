#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# fetchMovies.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Sun 23 Dec 2007 13:52:27 EST
#
#----------------------------------------------------------------------------#

import os, sys, optparse
import re
import urllib2

from cjktools.common import sopen

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#
 
def fetchMovies(name):
    """
    """
    links = getEpisodeLinks(name)

    for link in links:
        fetchEpisode(link)

    return

def fetchEpisode(link):
    sys.stdout.write('Fetching %s' % \
            re.search('[a-z]+[0-9]+.rmvb$', link).group(0))
    sys.stdout.flush()

    iStream = urllib2.urlopen(link)
    page = iStream.read()
    iStream.close()
    
    match = re.search(r'http://(?P<server>[a-z]+)\.realitylapse.com/dl/[0-9a-f/]*/fulleps/[a-z-]+/[a-z-]+[0-9]+\.rmvb', page)
    if match is None:
        print '\n--> Error'
        import pdb; pdb.set_trace()
    else:
        print ' (%s)' % match.group('server')
        os.system('wget %s' % match.group(0))

def getEpisodeLinks(name):
    assert ' ' not in name
    link = 'http://realitylapse.com/videos/%s.php' % name
    iStream = urllib2.urlopen(link)
    page = iStream.read()
    iStream.close()

    links = re.findall('/downloads/videos/%s/%s[0-9]*.rmvb' % (name, name), page)
    links = ['http://realitylapse.com' + l for l in links]
    return links

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
"""%prog [options] series-name

Fetches all the episodes available in the given series."""

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
        [name] = args
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

    fetchMovies(name)
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:
