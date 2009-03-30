<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!--
	cspx2cspm.xsl - generate FDR compatible CSP from compiler model output
	Copyright (C) 2009 Fred Barnes  <frmb@kent.ac.uk>
-->
<xsl:output method="text" encoding="UTF-8" media-type="text/plain" />
<!--{{{  TEMPLATE tag-->
<xsl:template match="tag"><xsl:value-of select="@name" /></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE tagset-->
<xsl:template match="tagset">
datatype <xsl:value-of select="@name" /> = <xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()"> | </xsl:if></xsl:for-each>
</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE treeref-->
<xsl:template match="treeref"><xsl:value-of select="@name" /></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE event-->
<xsl:template match="event"><xsl:value-of select="@name" /></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE eventset-->
<xsl:template match="eventset"><xsl:for-each select="child::*"><value-of select="@name" /></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE skip-->
<xsl:template match="skip">SKIP</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE stop-->
<xsl:template match="stop">STOP</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE output-->
<xsl:template match="output">
<xsl:choose><xsl:when test="child::*[2] and (name(child::*[2]) = 'nullnode')"><xsl:apply-templates select="child::*[1]" /></xsl:when>
<xsl:when test="child::*[2]"><xsl:apply-templates select="child::*[1]" />.<xsl:apply-templates select="child::*[2]" /></xsl:when>
<xsl:otherwise><xsl:apply-templates select="child::*[1]" /></xsl:otherwise></xsl:choose>
</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE input-->
<xsl:template match="input">
<xsl:choose><xsl:when test="child::*[2] and (name(child::*[2]) = 'nullnode')"><xsl:apply-templates select="child::*[1]" /></xsl:when>
<xsl:when test="child::*[2]"><xsl:apply-templates select="child::*[1]" />.<xsl:apply-templates select="child::*[2]" /></xsl:when>
<xsl:otherwise><xsl:apply-templates select="child::*[1]" /></xsl:otherwise></xsl:choose>
</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-paritems-->
<xsl:template name="cspx-paritems"><xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()"> ||| </xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE par-->
<xsl:template match="par">(<xsl:call-template name="cspx-paritems" />)</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-seqitems-->
<xsl:template name="cspx-seqitems"><xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()"><xsl:text>;
	</xsl:text></xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE seq-->
<xsl:template match="seq">(<xsl:call-template name="cspx-seqitems" />)</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-detitems-->
<xsl:template name="cspx-detitems"><xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()"><xsl:text> []
	</xsl:text></xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE det-->
<xsl:template match="det">(<xsl:call-template name="cspx-detitems" />)</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-ndetitems-->
<xsl:template name="cspx-ndetitems"><xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()"><xsl:text> |~|
	</xsl:text></xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE ndet-->
<xsl:template match="ndet">(<xsl:call-template name="cspx-ndetitems" />)</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-thenitems-->
<xsl:template name="cspx-thenitems"><xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()"> -> </xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE then-->
<xsl:template match="then">(<xsl:call-template name="cspx-thenitems" />)</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE apar-->
<xsl:template match="apar">(<xsl:apply-templates select="child::*[2]" />)<xsl:text>
	</xsl:text>[| {| <xsl:call-template name="cspx-fparamset" select="child::*[1]" /> |} |] (<xsl:apply-templates select="child::*[3]" />)</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE hiding-->
<xsl:template match="hiding"><xsl:apply-templates select="child::*[2]" /> \ {| <xsl:call-template name="cspx-fparamset" select="child::*[1]" /> |}</xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-aparamset-->
<xsl:template name="cspx-aparamset"><xsl:for-each select="child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()">,</xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE instance-->
<xsl:template match="instance"><xsl:value-of select="@name" /><xsl:if test="count (child::*) > 0">(<xsl:call-template name="cspx-aparamset" select="." />)</xsl:if>
</xsl:template>
<!--}}}-->
<xsl:template match="chaos">CHAOS (<xsl:if test="count (child::*) > 0">{| <xsl:call-template name="cspx-aparamset" select="." /> |}</xsl:if>)</xsl:template><!--{{{-->
<!--}}}-->
<!--{{{  TEMPLATE atom-->
<xsl:template match="atom"><xsl:value-of select="@id" /></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE verb-->
<xsl:template match="verb"><xsl:text>
</xsl:text><xsl:value-of select="@string" /><xsl:text>
</xsl:text></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE events/event-->
<xsl:template match="events/event"><xsl:value-of select="@name" /></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE events/eventset-->
<xsl:template match="events/eventset"><xsl:for-each select="child::*"><xsl:value-of select="@name" /><xsl:if test="position() != last()">,</xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE cspx-fparamset-->
<xsl:template name="cspx-fparamset"><xsl:for-each select="events/child::*"><xsl:apply-templates select="." /><xsl:if test="position() != last()">,</xsl:if></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE proc-->
<xsl:template match="proc">
<xsl:text>

</xsl:text><xsl:value-of select="@name" /><xsl:if test="count (child::*[1]/*) > 0">(<xsl:call-template name="cspx-fparamset" select="child::*[1]" />)</xsl:if> = <xsl:text>
	</xsl:text><xsl:apply-templates select="child::*[2]" /><xsl:text>
</xsl:text></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE globalevents-->
<xsl:template match="globalevents"><xsl:text>
</xsl:text><xsl:for-each select="child::*">channel <xsl:value-of select="@name" /><xsl:if test="@type"> : <xsl:value-of select="@type" /></xsl:if><xsl:text>
</xsl:text></xsl:for-each></xsl:template>
<!--}}}-->
<!--{{{  TEMPLATE program-->
<xsl:template match="program">
<xsl:text>-- automatically generated by cspx2cspm.xsl from </xsl:text><xsl:value-of select="@name" /><xsl:text>

</xsl:text>
<xsl:apply-templates select="child::*" />
<xsl:text>

</xsl:text>
</xsl:template>
<!--}}}-->

</xsl:stylesheet>

