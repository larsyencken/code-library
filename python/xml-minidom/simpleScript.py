# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# simpleScript.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue May 16 16:06:44 2006
#
#----------------------------------------------------------------------------#

""" Tests the python xml.dom.minidom library with xml.
"""

#----------------------------------------------------------------------------#

from xml.dom import minidom

#----------------------------------------------------------------------------#
filename = u'textFile.xml'

dom1 = minidom.parse(filename)

dom2 = minidom.parse(open(filename))

dom3 = minidom.parseString(open(filename).read())

#----------------------------------------------------------------------------#

for example in dom1, dom2, dom3:
    print 'Testing ', `example`
    for element in example.documentElement.getElementsByTagName(u'entry'):
        print element.firstChild.nodeValue.encode('utf8')
