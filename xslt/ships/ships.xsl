<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<xsl:template match="/">
<html>
  <head>
    <title>Ships of the
           <xsl:apply-templates mode="head" /></title>
  </head>
  <body>
    <xsl:apply-templates />
  </body>
</html>
</xsl:template>

<xsl:template match="shiptypes" mode="head">
<xsl:value-of select="@name" />
</xsl:template>

<xsl:template match="shiptypes">
<table border="1">
  <tr><th>Ship</th>
      <th>Class</th>
      <th>Registration</th>
      <th>Captain</th>
    </tr>
  <xsl:apply-templates />
</table>
</xsl:template>

<xsl:template match="ship">
  <tr><td><xsl:value-of select="@name" /></td>
      <td><xsl:value-of select="class" /></td>
      <td><xsl:value-of select="registry-code" /></td>
      <td><xsl:value-of select="captain" /></td>
    </tr>
</xsl:template>

</xsl:stylesheet>
