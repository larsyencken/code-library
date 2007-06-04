# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# test.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Aug 15 17:29:38 2006
#
#----------------------------------------------------------------------------#

""" Tests the behaviour of managed attributes and inheritance.
"""

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

class X(object):
    """
    """

    #------------------------------------------------------------------------#
    # PUBLIC METHODS
    #------------------------------------------------------------------------#

    def __init__(self):
        """ Constructor.
        """
        self._cheese = 'cheddar'

        return

    #------------------------------------------------------------------------#

    def getCheese(self):
        return self._cheese.title()

    def setCheese(self, cheese):
        self._cheese = cheese.lower()
        return

    cheese = property(getCheese, setCheese)

    #------------------------------------------------------------------------#
    # PRIVATE METHODS
    #------------------------------------------------------------------------#
    
    #------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

class Y(X):
    def setCheese(self, cheese):
        self._cheese = 'fetta'

        return

    cheese = property(X.getCheese, setCheese)

#----------------------------------------------------------------------------#

def testInheritance():
    mainObj = X()

    assert mainObj.cheese == 'Cheddar'
    mainObj.cheese = 'EATME'
    assert mainObj._cheese == 'eatme'
    assert mainObj.cheese == 'Eatme'

    print 'Managed attribute ok'

    secObj = Y()
    assert secObj.cheese == 'Cheddar'
    secObj.cheese = 'asdfsfffasdafs'

    if secObj.cheese == 'Fetta':
        print 'Inheritance works too!'
    else:
        print 'Failed:'
        print secObj.cheese

if __name__ == '__main__':
    testInheritance()
