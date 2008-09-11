#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# fetchEpisodes.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Sun 23 Dec 2007 13:52:27 EST
#
#----------------------------------------------------------------------------#

"A script which fetches anime episodes from the Reality Lapse web site."

import os, sys, optparse
import re
import urllib2

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#
 
def fetchEpisodes(seriesName, startFromEpisode, dryRun=False):
    links = getEpisodeLinks(seriesName)

    if not links:
        print 'No episodes found for series: %s' % seriesName
        return

    print 'Found %d episodes' % len(links)
    print 'Beginning to download'

    for link in links:
        episodeNumber = int(re.search('([0-9]+).rmvb$', link).group(1))
        if episodeNumber < startFromEpisode:
            print 'Skipping episode %d' % episodeNumber
            continue

        fetchEpisode(link, dryRun)

    return

def fetchEpisode(link, dryRun=False):
    """Fetch an episode given the link to its download page."""
    filename = re.search('[a-z]+[0-9]+.rmvb$', link).group(0)
    print 'Fetching %s' % filename

    iStream = urllib2.urlopen(link)
    page = iStream.read()
    iStream.close()
    
    match = re.search(r'http://(?P<server>[a-z]+)\.realitylapse.com/dl/[0-9a-f/]*/fulleps/[a-z-]+/[a-z-]+[0-9]+\.rmvb', page)
    if match is None:
        print '--> Error (maybe page format has changed)'
        sys.exit(1)

    realLink = match.group(0)
    print '--> from %s server' % match.group('server')

    if not dryRun:
        os.system('wget %s' % realLink)

    return

def getEpisodeLinks(name):
    """Fetch all the download links for episodes matching this series name."""
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

Fetches all the episodes available in the given series. The series needs to
be specified by its short name, which you have to determine by going to
reality lapse and looking at the URL for the series.

Note that the --from option can be used if you already have some of the
episodes.
"""

    parser = optparse.OptionParser(usage)

    parser.add_option('--debug', action='store_true', dest='debug',
            default=False, help='Enables debugging mode [False]')

    parser.add_option('--from', action='store', dest='startFromEpisode',
            type='int', default=1,
            help="Don't download any episodes before this one.")

    parser.add_option('-d', '--dryrun', action='store_true', dest='dryRun',
            default=False,
            help="Only perform a dry run. Don't actually fetch anything.")

    return parser

#----------------------------------------------------------------------------#

def main(argv):
    """ The main method for this module.
    """
    parser = _createOptionParser()
    (options, args) = parser.parse_args(argv)

    if not os.path.exists('/usr/bin/wget'):
        print "Can't find wget -- please install it and try again"
        return

    try:
        [seriesName] = args
    except:
        parser.print_help()
        sys.exit(1)

    try:
        fetchEpisodes(seriesName, options.startFromEpisode,
                dryRun=options.dryRun)

    except KeyboardInterrupt:
        pass

    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:
