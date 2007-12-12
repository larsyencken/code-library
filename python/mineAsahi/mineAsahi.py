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
from urllib2 import urlopen, HTTPError

from cjktools.common import sopen
import worker

seedLinks = [
    'http://www.asahi.com/politics/update/1212/TKY200712120454.html',
    'http://www.asahi.com/national/update/1212/OSK200712120090.html',
    'http://www.asahi.com/sports/update/1212/OSK200712120088.html',
]

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#

_visitedLinks = set()
_lastArticle = 0

_threadPool = worker.ThreadPool(5)

def mineAsahi():
    for link in seedLinks:
        _visitedLinks.add(link)
    requests = worker.makeRequests(_fetchPage, seedLinks, _parsePage)
    for req in requests:
        _threadPool.putRequest(req)

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
    print url
    try:
        data = unicode(urlopen(url).read(), 'euc-jp')
    except HTTPError:
        print "Bad url: " + url
        return None

    return data

def _parsePage(request, data):
    if not data:
        return
    article = _parseArticle(data)
    if article:
        _dumpArticle(article)

    links = _parseLinks(data)
    unvisitedLinks = [link for link in links if link not in _visitedLinks]
    for link in unvisitedLinks:
        _visitedLinks.add(link)

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

_tagPattern = _compilePattern(r'</?.*?>')

def _parseArticle(data):
    match = _articlePattern.search(data)

    if not match:
        return None

    groupDict = match.groupdict()
    title = groupDict['title']
    text = groupDict['text']
    text = _tagPattern.sub('', text)
    return title + '\n\n' + text

_linkPattern = _compilePattern(r'href="/[a-zA-Z0-9/]+.html"')

example = 'http://www.asahi.com/national/update/1212/JJT200712120008.html'

def _trimLink(link):
    return link[len('href="'):-1]

def _parseLinks(data):
    links = _linkPattern.findall(data)
    qualifiedLinks = []
    for link in links:
        link = 'http://www.asahi.com' + _trimLink(link)
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

    if args:
        parser.print_help()
        sys.exit(1)

    # Avoid psyco in debugging mode, since it merges stack frames.
    if not options.debug:
        try:
            import psyco
            psyco.profile()
        except:
            pass

    mineAsahi()
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:
