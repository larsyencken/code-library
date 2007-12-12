#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# mineAsahi.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Wed 12 Dec 2007 14:24:11 CET
#
#----------------------------------------------------------------------------#

import os, sys, optparse
import re
import time
from urllib2 import urlopen

from cjktools.common import sopen
import worker

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#

_visitedLinks = set()
_lastArticle = 0

_threadPool = worker.ThreadPool(5)

def mineAsahi(startLink):
    _visitedLinks.add(startLink)
    requests = worker.makeRequests(_fetchPage, [startLink], _parsePage)
    _threadPool.putRequest(requests[0])

    while 1:
        try:
            _threadPool.poll()
            time.sleep(0.5)
        except (KeyboardInterrupt, worker.NoResultsPending):
            break
    return

#----------------------------------------------------------------------------#
# PRIVATE
#----------------------------------------------------------------------------#

def _fetchPage(url):
    data = unicode(urlopen(url).read(), 'euc-jp')
    return data

def _parsePage(request, data):
    article = _parseArticle(data)
    _dumpArticle(article)

    links = _parseLinks(data)
    unvisitedLinks = [link for link in links if link not in _visitedLinks]

    newRequests = worker.makeRequests(_fetchPage, unvisitedLinks, _parsePage)
    for req in newRequests:
        _threadPool.putRequest(req)
    return

def _dumpArticle(text):
    global _lastArticle
    filename = 'article%.04d.txt' % _lastArticle
    print filename
    oStream = sopen(filename, 'w', 'utf8')
    oStream.write(text)
    oStream.close()
    _lastArticle += 1
    return

def _compilePattern(pattern):
    return re.compile(pattern, re.UNICODE | re.MULTILINE | re.DOTALL)

_articlePattern = _compilePattern(r'<h1 id="cap">(?P<title>.*?)</h1>.*?<div class="kiji">(?P<text>.*?)</div>')

def _parseArticle(data):
    match = _articlePattern.search(data)

    if not match:
        return None

    groupDict = match.groupdict()
    title = groupDict['title']
    text = groupDict['text']
    text = text.replace('<p>', '').replace('</p>', '')
    return title + '\n\n' + text

_linkPattern = _compilePattern(r'/[a-z]+/update/[0-9]+/[A-Z0-9]+.html')

example = 'http://www.asahi.com/national/update/1212/JJT200712120008.html'

def _parseLinks(data):
    links = _linkPattern.findall(data)
    qualifiedLinks = []
    for link in links:
        if not link.startswith('http://www.asahi.com'):
            qualifiedLinks.append('http://www.asahi.com' + link)
        else:
            qualifiedLinks.append(link)

    return qualifiedLinks

#----------------------------------------------------------------------------#
# MODULE EPILOGUE
#----------------------------------------------------------------------------#

def _createOptionParser():
    """
    Creates an option parser instance to handle command-line options.
    """
    usage = \
"""%prog [options] inputFile outputFile

Default usage message."""

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
        [startLink] = args
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

    mineAsahi(startLink)
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:
