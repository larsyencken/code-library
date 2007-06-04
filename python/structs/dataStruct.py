# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# dataStruct.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Thu Aug 24 16:57:19 2006
#
#----------------------------------------------------------------------------#

""" Defines an abstract struct data type.
"""

#----------------------------------------------------------------------------#

import struct
from jptools.functional import unzip

#----------------------------------------------------------------------------#


class DataStruct(object):
    """ An abstract data type represented by a struct. You provide the
        fieldnames and their types in order, and we provide dictionary output
        and input.
    """

    #------------------------------------------------------------------------#
    # PUBLIC METHODS
    #------------------------------------------------------------------------#

    def __init__(self, fieldSpec):
        """ Constructor.
        """
        fieldNames, fieldTypes = unzip(fieldSpec)
        self.structSpec = '@' + ''.join(fieldTypes)
        self.fieldNames = fieldNames

        return

    #------------------------------------------------------------------------#

    def unpack(self, packedData):
        """ Unpacks the data structure into a dictionary mapping field name to
            value.
        """
        return dict(zip(
                self.fieldNames, 
                struct.unpack(self.structSpec, packedData),
            ))

    #------------------------------------------------------------------------#

    def pack(self, dataDict):
        """ Takes a dictionary of field values and packs it into a data
            structure.
        """
        fieldValues = map(dataDict.__getitem__, self.fieldNames)
        return struct.pack(self.structSpec, fieldValues)

    #------------------------------------------------------------------------#

    def read(self, inputStream):
        """ Read and return one object of this type from the inputStream.
        """
        return self.unpack(inputStream.read(self.size()))

    #------------------------------------------------------------------------#

    def size(self):
        """ Returns the size of this field element.
        """
        return struct.calcsize(self.structSpec)

    #------------------------------------------------------------------------#
    # PRIVATE METHODS
    #------------------------------------------------------------------------#
    
    #------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

