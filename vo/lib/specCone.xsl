<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" 
    xmlns:spec="http://voservices.net/spectrum"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" />


<xsl:template match="/*">
<xsl:text>#Name,Ra,Dec,Redshift,Class,SpecClass,Id
</xsl:text>
 <xsl:apply-templates select="spec:Sed" />
</xsl:template>

<xsl:template match="spec:Target/spec:Name">
  <xsl:value-of select="@value" /><xsl:text>,</xsl:text>
</xsl:template>
<xsl:template match="spec:Target/spec:Pos">
  <xsl:value-of select="translate(@value,' ',',')" /><xsl:text>,</xsl:text>
</xsl:template>
<xsl:template match="spec:Target/spec:Redshift/spec:Value">
  <xsl:value-of select="@value" /><xsl:text>,</xsl:text>
</xsl:template>
<xsl:template match="spec:Target/spec:Class">
  <xsl:value-of select="@value" /><xsl:text>,</xsl:text>
</xsl:template>
<xsl:template match="spec:Target/spec:SpectralClass">
  <xsl:value-of select="@value" /><xsl:text>,</xsl:text>
</xsl:template>

<xsl:template match="spec:Sed">
  <xsl:apply-templates select="spec:Target/spec:Name" />
  <xsl:apply-templates select="spec:Target/spec:Pos" />
  <xsl:apply-templates select="spec:Target/spec:Redshift/spec:Value" />
  <xsl:apply-templates select="spec:Target/spec:Class" />
  <xsl:apply-templates select="spec:Target/spec:SpectralClass" />

  <xsl:for-each select="spec:Segments">
    <xsl:for-each select="spec:Segment/spec:DataId/spec:DatasetId">
     <xsl:value-of select="@value" /><xsl:text>
</xsl:text>
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
