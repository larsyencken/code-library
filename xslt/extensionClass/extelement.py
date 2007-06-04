# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# extelement.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Fri Jul 28 11:52:48 2006
#
#----------------------------------------------------------------------------#

""" Example XSLT extension for 4XSLT. Sourced from:
    http://uche.ogbuji.net/tech/akara/nodes/2003-01-01/xslt-ext-elems
"""

#----------------------------------------------------------------------------#

import os
from Ft.Xml.Xslt import XSL_NAMESPACE, XsltElement, XsltException, Error
from Ft.Xml.Xslt import ContentInfo, AttributeInfo

#----------------------------------------------------------------------------#

EXT_NAMESPACE = 'http://foo.org/namespaces/ext-xslt'

#----------------------------------------------------------------------------#

class SystemElement(XsltElement):
    """ Execute an arbitrary operating system command.
        Because of the security issues, use at your risk  :-)
    """
    content = ContentInfo.Empty   #Specify that this must be an empty element
    legalAttrs = {
        'command': AttributeInfo.StringAvt(
            description='The command to be executed'
        ),
    }

    def instantiate(self, context, processor):
        """ Actually perform the processing.""
        """
        command = self._command.evaluate(context)
        os.system(command)
        return (context,)

#----------------------------------------------------------------------------#

#The global dictionary that must be present in all extensions
ExtElements = {
        (EXT_NAMESPACE, 'system'): SystemElement,
    }

#----------------------------------------------------------------------------#

#And optional dictionary, purely for documentation purposes,
#Which gives the prefix to use for extension namespaces in documentation
ExtNamespaces = {
        EXT_NAMESPACE : 'e',
    }

#----------------------------------------------------------------------------#

