#!/usr/bin/python

import kana_py
import kana_cpp
import codecs
from jptools.progressBar import withProgress
import time

import psyco
psyco.full()

print 'Reading data'
data = codecs.open('edict', 'r', 'utf8').read()

nIterations = 50

def testKatakana(module):
    startTime = time.time()
    for i in xrange(nIterations):
        newData = module.toKatakana(data)
    timeTaken = time.time() - startTime
    print '%-10s %4.2f s' % (module.__name__, timeTaken)

def testUniqueKanji(module):
    startTime = time.time()
    for i in xrange(nIterations/4):
        newData = module.uniqueKanji(data)
    timeTaken = time.time() - startTime
    print '%-10s %4.2f s' % (module.__name__, timeTaken)

print '***** KATAKANA TEST *****'
testKatakana(kana_cpp)
testKatakana(kana_py)

print '***** UNIQUE KANJI TEST *****'
testUniqueKanji(kana_cpp)
testUniqueKanji(kana_py)
