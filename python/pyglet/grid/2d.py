# -*- coding: utf-8 -*-
#
#  2d.py
#  grid
# 
#  Created by Lars Yencken on 25-10-2009.
#  Copyright 2009 Lars Yencken. All rights reserved.
#

"""
"""

from __future__ import division

import random
from math import sqrt, sin, cos, log, pi
import sys

import pyglet
from pyglet.window import key, mouse, Window
from simplestats import mean

import prob

DEBUG = True

#window.push_handlers(pyglet.window.event.WindowEventLogger())

def is_valid_loc(x, y):
    return (x % 2 == 0 and y % 2 == 0)

class Region(object):
    def __init__(self, locations, color=None):
        if color is None:
            color = (
                    random.randint(0, 256),
                    random.randint(0, 256),
                    random.randint(0, 256),
                )
        self.color = color
        self.locations = locations
    
    @classmethod
    def grow(cls, active_set, border_set):
        locations = set()
        if not border_set and not active_set:
            seed_loc = (0, 0)
        else:
            seed_loc = random.choice(list(border_set))
        
        locations.add(seed_loc)
        size = random.randint(8, 15)
        for i in xrange(size):
            potential_locations = list(cls.expand(locations, active_set))
            if not potential_locations:
                return None

            next_loc = cls.sample_by_centroid(locations,
                    potential_locations, use_max=True)
            next_loc = random.choice(potential_locations)
            locations.add(next_loc)
        
        if len(list(locations)) < 5:
            return None
        
        return cls(list(locations))
    
    @classmethod
    def sample_by_centroid(cls, locations, potential_locations, 
            use_max=False):
        mean_x = mean(x for (x, y) in locations)
        mean_y = mean(y for (x, y) in locations)
        parts = []
        n_locations = len(potential_locations)
        for loc in potential_locations:
            x, y = loc
            dist = sqrt((x - mean_x)**2 + (y - mean_y)**2)
            if dist > 0:
                score = 1 / dist**2 
            else:
                score = 1
            parts.append((loc, score))
        
        dist = prob.ProbDist(parts)
        
        if use_max:
            p, l = max((p, l) for (l, p) in dist.iteritems())
            return l
        
        return dist.sample()
    
    @classmethod
    def expand(cls, locations, active_set):
        potential_set = set()
        exclusion_set = active_set.union(locations)
        for location in locations:
            potential_set.update(expand_loc(location, exclusion_set))
        return potential_set
    
    def __iter__(self):
        return iter(self.locations)
    
class GameWindow(Window):
    def __init__(self, *args, **kwargs):
        super(GameWindow, self).__init__(*args, **kwargs)
        self.map = Map.grow()
        self.calculate_scale()

    def calculate_scale(self):
        eps = 2
        min_x = min(min(l[0] for l in r) for r in self.map) - eps
        min_y = min(min(l[1] for l in r) for r in self.map) - eps

        for region in self.map:
            new_locs = [(x - min_x, y - min_y) for (x, y) in region]
            region.locations = new_locs

        max_x = max(max(l[0] for l in r) for r in self.map) + eps
        max_y = max(max(l[1] for l in r) for r in self.map) + eps

        x_win_size, y_win_size = self.get_size()
        print x_win_size, y_win_size
        x_scale = x_win_size / max_x
        y_scale = y_win_size / max_y
        self.scale = min(x_scale, y_scale)
        print "Scale: %s" % self.scale

        self.origin_x = 0
        self.origin_y = 0

    def rescale(self, multiplier):
        x_win_size, y_win_size = self.get_size()

        # Get the current center in our coordinates
        # x_center = self.origin_x + x_win_size / 2
        # y_center = self.origin_y + y_win_size / 2
        self.scale *= multiplier

    def draw(self):
        self.map.draw(self.origin_x, self.origin_y, self.scale)

    def on_draw(self):
        self.clear()
        self.draw()

    def on_text_motion(self, motion):
        if motion == key.MOTION_LEFT:
            self.origin_x -= 10
        elif motion == key.MOTION_RIGHT:
            self.origin_x += 10
        elif motion == key.MOTION_UP:
            self.origin_y += 10
        elif motion == key.MOTION_DOWN:
            self.origin_y -= 10

    def on_mouse_drag(self, x, y, dx, dy, button, modifiers):
        if button == mouse.LEFT:
            self.origin_x += dx
            self.origin_y += dy

    def on_mouse_scroll(self, x, y, dx, dy):
        self.rescale(1.2 ** dy)

    def on_key_press(self, symbol, modifiers):
        if symbol == key.Q:
            sys.exit(0)
        if symbol == key.R:
            self.map = Map.grow()
            self.calculate_scale()
        elif symbol == key.EQUAL:
            self.rescale(1.2)
        elif symbol == key.MINUS:
            self.rescale(1/1.2)

class Map(object):
    @classmethod
    def grow(cls):
        active_set = set()
        border_set = set()
        regions = []
        n_regions = random.randint(10, 25)
        while len(regions) < n_regions:
            maybe_region = Region.grow(active_set, border_set)
            if maybe_region:
                regions.append(maybe_region)
                active_set.update(maybe_region.locations)
                border_set = cls.determine_borders(active_set)
        return cls(regions)
    
    @classmethod
    def determine_borders(cls, active_set):
        borders = set()
        for loc in active_set:
            for neighbour in expand_loc(loc, active_set):
                borders.add(neighbour)
        return borders
    
    def __init__(self, regions):
        self.regions = regions
        self.eps = 0.0
        self.tile_size = 1.0

    def __iter__(self):
        return iter(self.regions)

    def x_delta(self, scale):
        return (sqrt(3)/2) * (scale + self.eps)

    def y_delta(self, scale):
        return (3/2) * (scale + self.eps)

    def draw_at(self, x, y, origin_x, origin_y, scale, color=(255, 255, 0)):
        new_x = origin_x + x * self.x_delta(scale)
        new_y = origin_y + y * self.y_delta(scale)
        pyglet.graphics.draw(6, pyglet.gl.GL_POLYGON,
                ('v2f', gen_hexagon(new_x, new_y, self.tile_size * scale)),
                ('c3B', color*6),
            )

    def draw(self, origin_x, origin_y, scale):
        for region in self.regions:
            for (x, y) in region.locations:
                self.draw_at(x, y, origin_x, origin_y, scale, 
                        color=region.color)

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

def expand_loc(seed, exclusion_set):
    x, y = seed
    for loc in [(x-2, y), (x-1, y-1), (x-1, y+1),
            (x+1, y-1), (x+1, y+1), (x+2, y)]:
        if loc not in exclusion_set:
            yield loc

if __name__ == '__main__':
    platform = pyglet.window.get_platform()
    display = platform.get_default_display()
    screen = display.get_default_screen()
    game_window = GameWindow(fullscreen=True, screen=screen)
    pyglet.app.run()

# vim: ts=4 sw=4 sts=4 et tw=78:

