# -*- coding: utf-8 -*-
#
#  keyboard.py
#  pyglet examples
# 
#  Created by Lars Yencken on 25-10-2009.
#  Copyright 2009 Lars Yencken. All rights reserved.
#

import sys

import pyglet
from pyglet.window import key

window = pyglet.window.Window()

@window.event
def on_key_press(symbol, modifiers):
    if symbol == key.Q:
        print 'Exitting'
        sys.exit(0)
    elif symbol == key.LEFT:
        print 'Left arrow'
    elif symbol == key.RIGHT:
        print 'Right arrow'
    else:
        print "Symbol: %s" % symbol

@window.event
def on_draw():
    window.clear()

pyglet.app.run()

# vim: ts=4 sw=4 sts=4 et tw=78:

