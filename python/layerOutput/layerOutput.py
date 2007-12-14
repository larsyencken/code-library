# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# layerOutput.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Sat Dec 15 00:19:55 2007
#
#----------------------------------------------------------------------------#

"""
"""

#----------------------------------------------------------------------------#

import sys

#----------------------------------------------------------------------------#

_currentLayer = None
_openLayers = set()

#----------------------------------------------------------------------------#

def reset():
    _currentLayer = None
    _openLayers = set()

def startLayer(text, last=False):
    global _currentLayer, _openLayers
    if _currentLayer is None:
        _currentLayer = 0
    log(text, last)

    if not last:
        _openLayers.add(_currentLayer)
    _currentLayer += 1

def log(text, last=False):
    global _currentLayer, _openLayers

    for layer in range(1, _currentLayer):
        if layer in _openLayers:
            sys.stdout.write('|  ')
        else:
            sys.stdout.write('   ')

    if _currentLayer > 0:
        if not last:
            print '├─', text
        else:
            print '└─', text
    else:
        print text
    
    if _currentLayer in _openLayers:
        _openLayers.remove(_currentLayer)

def endLayer():
    global _currentLayer
    _currentLayer -= 1
    if _currentLayer < 0:
        _currentLayer = 0

if __name__ == '__main__':
    startLayer('Building place names')
    startLayer('Constructing place hierarchy')
    log('Eating children')
    startLayer('Mining town')
    log('Mining something')
    log('Chopping wood', last=True)
    endLayer()
    startLayer('Fishing town', last=True)
    log('Fish for trout')
    log('Fish for salmon', last=True)
    endLayer()
    endLayer()
    log('Storing places to database')
    log('Storing aliases to database', last=True)
    endLayer()
