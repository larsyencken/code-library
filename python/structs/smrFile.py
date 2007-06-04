# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# smrFile.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Thu Aug 24 16:54:11 2006
#
#----------------------------------------------------------------------------#

"""
"""

#----------------------------------------------------------------------------#

from dataStruct import DataStruct

#----------------------------------------------------------------------------#
# DATA STRUCTURES
#----------------------------------------------------------------------------#

generalHeader = DataStruct((
        ('marker',              '8s'),
        ('filename',            '14s'),
        ('fileSize',            'L'),
        ('timeCreated',         '8s'),
        ('dateCreated',         '8s'),
        ('numChannels',         'H'),
        ('numVariables',        'H'),
        ('numDataVariables',    'H'),
        ('fileHeaderSize',      'H'),
        ('dataHeaderSize',      'H'),
        ('lastDataOffset',      'L'),
        ('numDataSections',     'H'),
        ('blockSizeRounding',   'H'),
        ('comment',             '74s'),
        ('pointerTableOffset',  'L'),
        ('futureExpansion',     '40x'),
    ))

#----------------------------------------------------------------------------#

class SmrFile:
    """ A wrapper that provides access to the data within a .smr file.
    """

    #------------------------------------------------------------------------#
    # PUBLIC METHODS
    #------------------------------------------------------------------------#

    def __init__(self, filename):
        """ Constructor.
        """
        self.filename = filename
        self._inputStream = open(filename, 'r')

        self.header = generalHeader.read(self._inputStream)

        return

    #------------------------------------------------------------------------#

    #------------------------------------------------------------------------#
    # PRIVATE METHODS
    #------------------------------------------------------------------------#

    #------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

x = SmrFile('woof.smr')

