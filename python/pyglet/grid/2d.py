# -*- coding: utf-8 -*-
#
#  2d.py
#  unknown project
# 
#  Created by Lars Yencken on 25-10-2009.
#  Copyright 2009 Lars Yencken. All rights reserved.
#

"""
"""

from __future__ import division
import pyglet

from pyglet.window import key
from math import *
import sys

platform = pyglet.window.get_platform()
display = platform.get_default_display()
screen = display.get_default_screen()

template = pyglet.gl.Config(sample_buffers=1, samples=4)
config = screen.get_best_config(template)
context = config.create_context(None)
window = pyglet.window.Window(context=context)

grid = None

@window.event
def on_draw():
    global grid
    window.clear()

    if grid is None:
        grid = HexagonalGrid()
    grid.draw()

@window.event
def on_text_motion(motion):
    if motion == key.MOTION_LEFT:
        grid.origin_x -= 10
    elif motion == key.MOTION_RIGHT:
        grid.origin_x += 10
    elif motion == key.MOTION_UP:
        grid.origin_y += 10
    elif motion == key.MOTION_DOWN:
        grid.origin_y -= 10


@window.event
def on_key_press(symbol, modifiers):
    if symbol == key.Q:
        sys.exit(0)
    elif symbol == key.PLUS:
        grid.scale *= 1.5
    elif symbol == key.MINUS:
        grid.scale /= 1.5

class HexagonalGrid(object):
    matrix = [
            (1, 0), (3, 0), (5, 0),
            (0, 1), (2, 1), (4, 1),
            (3, 2), (5, 2),
        ]
    scale = 20
    origin_x = 300.0
    origin_y = 300.0

    @property
    def eps(self):
        return (1/20) * self.scale

    @property
    def x_delta(self):
        return (sqrt(3)/2) * (self.scale + self.eps)

    @property
    def y_delta(self):
        return (3/2) * (self.scale + self.eps)

    def draw_at(self, x, y):
        new_x = self.origin_x + x * self.x_delta
        new_y = self.origin_y + y * self.y_delta
        pyglet.graphics.draw(6, pyglet.gl.GL_POLYGON,
                ('v2f', gen_hexagon(new_x, new_y, self.scale)),
            )

    def draw(self):
        for (x, y) in self.matrix:
            self.draw_at(x, y)

def gen_hexagon(x, y, l):
    cl = l * cos(pi / 6)
    sl = l / 2

    return (
            x - cl,     y - sl, 
            x - cl,     y + sl,
            x,          y + l,
            x + cl,     y + sl,
            x + cl,     y - sl,
            x,          y - l,
        )

pyglet.app.run()

# vim: ts=4 sw=4 sts=4 et tw=78:

