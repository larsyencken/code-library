# useextfunc.py

TRANSFORM = """<?xml version="1.0"?>
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:s="http://spam.com"
  version="1.0">

  <xsl:template match="/">
    <xsl:value-of select="s:get-current-time()"/>
  </xsl:template>

</xsl:stylesheet>
"""

SOURCE = """<dummy/>"""

from Ft.Xml.Xslt import Processor
processor = Processor.Processor()

# Register the extension function using method (3)
processor.registerExtensionModules(['demo'])
from Ft.Xml import InputSource
transform = InputSource.DefaultFactory.fromString(TRANSFORM, "http://foo.com")
source = InputSource.DefaultFactory.fromString(SOURCE, "http://foo.com")
processor.appendStylesheet(transform)
result = processor.run(source)
print result
