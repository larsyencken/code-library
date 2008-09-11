# -*- coding: utf-8 -*-
# 
#  pixels.py
#  pixel_flip
#  
#  Created by Lars Yencken on 2008-06-20.
#  Copyright 2008-06-20 Lars Yencken. All rights reserved.
# 

"""
The code to a Google interview problem, for drawing lines on a bitmap
display (one bit per pixel).
"""

import ctypes

_height = 600
_width = 800
_data = ctypes.create_string_buffer(_width*_height/8 + 1)

def draw_line(x, y, length):
    """
    Draws a horizontal line starting at (x, y) and with the given length.
    """
    start_pos = y*_width + x
    end_pos = start_pos + length
    if end_pos >= (y+1)*_width:
        end_pos = (y+1)*_width - 1
    
    # Draw up to the first boundary
    for pos in xrange(start_pos, end_pos + 1):
        if pos % 8 == 0:
            break
        _data[pos/8] |= 1 << (pos % 8)
    else:
        # No boundary found
        return
    
    # Write the full bytes
    num_full_bytes = (end_pos - pos)/8
    while num_full_bytes > 0:
        _data[pos/8] = 0xff
        pos += 8
        num_full_bytes -= 1
    
    for pos in xrange(pos, endpos + 1):
        _data[pos/8] |= 1 << (pos % 8)
        
    return