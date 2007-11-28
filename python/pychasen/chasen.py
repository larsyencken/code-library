# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# chasen.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Mon Oct 29 19:50:03 2007
#
#----------------------------------------------------------------------------#

"""A python interface to Chasen."""

#----------------------------------------------------------------------------#

from ctypes import *
from ctypes.util import find_library
import os

#----------------------------------------------------------------------------#
# C methods
#----------------------------------------------------------------------------#

_libchasenFile = find_library('chasen') or find_library('libchasen.dll')
if not _libchasenFile:
    raise Exception, "Can't find libchasen to load."

_libchasen = cdll.LoadLibrary(_libchasenFile)

_chasen_sparse_tostr = _libchasen['chasen_sparse_tostr']
_chasen_sparse_tostr.argtypes = [c_char_p]
_chasen_sparse_tostr.restype = c_char_p

class File(Structure):
    pass

_chasen_getopt_argv = _libchasen['chasen_getopt_argv']
_chasen_getopt_argv.argtypes = [POINTER(c_char_p), POINTER(File)]

#----------------------------------------------------------------------------#
# Python interface
#----------------------------------------------------------------------------#

class Token(object):
    __slots__ = ('surface', 'reading', 'infinitive', 'pos', 'inflection',
        'form')

    def __init__(self, surface, reading, infinitive, pos, inflection, form):
        self.surface = surface
        self.reading = reading
        self.infinitive = infinitive
        self.pos = pos.split('-')
        self.inflection = inflection
        self.form = form

    def unicode(self):
        return self.surface

class Chasen(object):
    """A Python interface to libchasen."""

    def __init__(self, rcFile=None):
        """Initializes libchasen, optionally with a non-default rc file."""

        if rcFile:
            if not os.path.exists(rcFile):
                raise Exception, "No chasen rc file exists at path %s" % rcFile
            CharArray6 = c_char_p*6
            argv = CharArray6()
            argv[0] = "chasen"
            argv[1] = "-i"
            argv[2] = "w"
            argv[3] = "-r"
            argv[4] = rcFile
            argv[5] = None
            _chasen_getopt_argv(argv, None)
        else:
            CharArray4 = c_char_p*4
            argv = CharArray4()
            argv[0] = "chasen"
            argv[1] = "-i"
            argv[2] = "w"
            argv[3] = None
            _chasen_getopt_argv(argv, None)

        self.rcFile = rcFile
        return

    def parse(self, sentence):
        if type(sentence) == unicode:
            sentence = sentence.encode('utf8')
        sentence = sentence.replace('\n', '')
        result = unicode(_chasen_sparse_tostr(sentence), 'utf8')
        lines = filter(None, result.split('\n'))
        sentence = []
        for i, line in enumerate(lines):
            if line != ('EOS'):
                segments = line.split('\t')
                token = Token(*segments)
                sentence.append(token)
            else:
                if i != len(lines) - 1:
                    raise Exception, "unexpected EOS in chasen output"

        return sentence

#----------------------------------------------------------------------------#

