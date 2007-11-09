# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# mecab.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Oct 23 00:00:32 2007
#
#----------------------------------------------------------------------------#

""" 
"""

#----------------------------------------------------------------------------#

from ctypes import *

#----------------------------------------------------------------------------#

class Mecab_t(Structure):
    pass

class MecabDictInfo_t(Structure):
    pass

MecabDictInfo_t._fields_ = [
            ("filename",    c_char_p),
            ("charset",     c_char_p),
            ("size",        c_uint),
            ("type",        c_int),
            ("lsize",       c_uint),
            ("rsize",       c_uint),
            ("version",     c_ushort),
            ("next",        POINTER(MecabDictInfo_t)),
        ]

class MecabNode_t(Structure):
    pass

class MecabPath_t(Structure):
    pass

class MecabLearnerNode_t(Structure):
    pass

class MecabLearnerPath_t(Structure):
    pass

MecabNode_t._fields_ = [
        ("prev",        POINTER(MecabNode_t)),
        ("next",        POINTER(MecabNode_t)),
        ("enext",       POINTER(MecabNode_t)),
        ("bnext",       POINTER(MecabNode_t)),
        ("rpath",       POINTER(MecabPath_t)),
        ("lpath",       POINTER(MecabPath_t)),
        ("begin_node_list", POINTER(POINTER(MecabNode_t))),
        ("end_node_list",   POINTER(POINTER(MecabNode_t))),
        ("surface",     c_char_p),
        ("feature",     c_char_p),
        ("id",          c_uint),
        ("length",      c_ushort),
        ("rlength",     c_ushort),
        ("rcAttr",      c_ushort),
        ("lcAttr",      c_ushort),
        ("posid",       c_ushort),
        ("char_type",   c_ubyte),
        ("isbest",      c_ubyte),
        ("sentence_length", c_uint),
        ("alpha",       c_float),
        ("beta",        c_float),
        ("prob",        c_float),
        ("wcost",       c_short),
        ("cost",        c_long),
    ]

MecabPath_t._fields_ = [
        ("rnode",       POINTER(MecabNode_t)),
        ("rnext",       POINTER(MecabPath_t)),
        ("lnode",       POINTER(MecabNode_t)),
        ("lnext",       POINTER(MecabPath_t)),
        ("cost",        c_int),
        ("prob",        c_float),
    ]

MecabLearnerPath_t._fields_ = [
        ("rnode",       POINTER(MecabNode_t)),
        ("rnext",       POINTER(MecabPath_t)),
        ("lnode",       POINTER(MecabNode_t)),
        ("lnext",       POINTER(MecabPath_t)),
        ("cost",        c_double),
        ("fvector",     POINTER(c_int)),
    ]

MecabLearnerNode_t._fields_ = [
        ("prev",        POINTER(MecabNode_t)),
        ("next",        POINTER(MecabNode_t)),
        ("enext",       POINTER(MecabNode_t)),
        ("bnext",       POINTER(MecabNode_t)),
        ("rpath",       POINTER(MecabPath_t)),
        ("lpath",       POINTER(MecabPath_t)),
        ("anext",       POINTER(MecabNode_t)),
        ("surface",     c_char_p),
        ("feature",     c_char_p),
        ("id",          c_uint),
        ("length",      c_ushort),
        ("rlength",     c_ushort),
        ("rcAttr",      c_ushort),
        ("lcAttr",      c_ushort),
        ("posid",       c_ushort),
        ("char_type",   c_ubyte),
        ("stat",        c_ubyte),
        ("isbest",      c_ubyte),
        ("sentence_length", c_uint),
        ("alpha",       c_float),
        ("beta",        c_float),
        ("wcost2",      c_float),
        ("wcost",       c_short),
        ("cost",        c_long),
        ("fvector",     POINTER(c_int)),
    ]

MECAB_NOR_NODE = 0
MECAB_UNK_NODE = 1
MECAB_BOS_NODE = 2
MECAB_EOS_NODE = 3

MECAB_SYS_DIC = 0
MECAB_USR_DIC = 1
MECAB_UNK_DIC = 2

_mecab = cdll.LoadLibrary("libmecab.so")

mecab_new = _mecab['mecab_new']
mecab_new.argtypes = [c_int, POINTER(c_char_p)]
mecab_new.restype = POINTER(Mecab_t)

mecab_destroy = _mecab['mecab_destroy']
mecab_destroy.argtypes = [POINTER(Mecab_t)]
mecab_destroy.restype = None

mecab_sparse_tostr = _mecab['mecab_sparse_tostr']
mecab_sparse_tostr.argtypes = [POINTER(Mecab_t), c_char_p]
mecab_sparse_tostr.restype = c_char_p

mecab_strerror = _mecab['mecab_strerror']
mecab_strerror.argtypes = [POINTER(Mecab_t)]
mecab_strerror.restype = c_char_p

example = u'漢字を読めたいです。'

#----------------------------------------------------------------------------#

class Tagger(object):
    def __init__(self):
        self.cobj = mecab_new(0, None)
        if self.cobj == None:
            raise Exception, "Couldn't create tagger object"

    def parse(self, str):
        if type(str) == unicode:
            str = str.encode('euc-jp')
        
        result = mecab_sparse_tostr(self.cobj, str)

        error = mecab_strerror(self.cobj)
        if error:
            raise Exception, error

        return unicode(result, 'euc-jp')

#----------------------------------------------------------------------------#

