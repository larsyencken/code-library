# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# useext.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Fri Jul 28 11:55:12 2006
#
#----------------------------------------------------------------------------#

""" Example file using an 4XSLT extension. Sourced from:
    http://uche.ogbuji.net/tech/akara/nodes/2003-01-01/xslt-ext-elems
"""

#----------------------------------------------------------------------------#

TRANSFORM = """<?xml version="1.0"?>
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ext="http://foo.org/namespaces/ext-xslt"
  extension-element-prefixes="ext"
  version="1.0">

  <xsl:template match="execute-command">
    <ext:system command="{@cmd}"/>
  </xsl:template>

</xsl:stylesheet>
"""

SOURCE = """<execute-command cmd="dir"/>"""

from Ft.Xml.Xslt import Processor
processor = Processor.Processor()
#Register the extension element
processor.registerExtensionModules(['extelement'])
from Ft.Xml import InputSource
transform = InputSource.DefaultFactory.fromString(TRANSFORM, "http://foo.com")
source = InputSource.DefaultFactory.fromString(SOURCE, "http://foo.com")
processor.appendStylesheet(transform)
result = processor.run(source)
print result

