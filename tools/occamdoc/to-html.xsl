<?xml version="1.0" encoding="UTF-8"?>
<!--
 Convert OccamDoc to HTML
 Adam Sampson <ats@offog.org>

 Copyright 2006 University of Kent, Canterbury, UK

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA 02110-1301, USA.
 -->
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output
	method="html"
	encoding="ISO-8859-1"
	standalone="yes"
	doctype-public="-//W3C//DTD HTML 4.01//EN"
	doctype-system="http://www.w3.org/TR/html4/strict.dtd"
/>

<!-- Parameters -->

<xsl:param name="moduleid" select="''" />
<xsl:param name="mode" select="'page'" />

<!-- Keys -->

<xsl:key name="name" match="declaration" use="@name" />

<!-- The root of the output -->

<xsl:template match="/">
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="/occamDoc">
	<xsl:choose>
		<xsl:when test="$mode='index' or $mode='frames-index'">
			<xsl:apply-templates select="." mode="index" />
		</xsl:when>
		<xsl:when test="$mode='page'">
			<xsl:apply-templates select="." mode="page" />
		</xsl:when>
	</xsl:choose>
</xsl:template>

<xsl:template name="head">
	<xsl:param name="title" />
	<head>
		<title><xsl:value-of select="$title" /></title>
		<link rel="stylesheet" href="occamdoc.css" type="text/css" />
	</head>
</xsl:template>

<xsl:template match="/occamDoc" mode="index">
	<xsl:variable name="title">OccamDoc index</xsl:variable>
	<html>
		<xsl:call-template name="head">
			<xsl:with-param name="title" select="$title" />
		</xsl:call-template>
		<body>
			<xsl:if test="$mode='index'">
			<h1><xsl:value-of select="$title" /></h1>
			<p>
				<a href="frames.html" target="_top">Show frames</a>
			</p>
			</xsl:if>

			<xsl:if test="$mode='frames-index'">
			<p>
				<a href="index.html" target="page">Full index</a>
				<br />
				<a href="index.html" target="_top">Hide frames</a>
			</p>
			</xsl:if>

			<div id="{$mode}">
				<h2>Modules</h2>
				<ul>
					<xsl:apply-templates select="declaration" mode="index">
						<xsl:sort order="ascending" select="@name" />
					</xsl:apply-templates>
				</ul>
				<h2>Declarations</h2>
				<ul>
				<xsl:apply-templates select="declaration/children/declaration" mode="index">
					<xsl:sort order="ascending" select="@name" />
				</xsl:apply-templates>
				</ul>
			</div>
		</body>
	</html>
</xsl:template>

<xsl:template match="/occamDoc" mode="page">
	<xsl:variable name="decls" select="declaration[@name=$moduleid]" />
	<html>
		<xsl:call-template name="head">
			<xsl:with-param name="title" select="$decls/summary" />
		</xsl:call-template>
		<body>
			<div id="content">
				<xsl:apply-templates select="$decls" />
			</div>
		</body>
	</html>
</xsl:template>

<!-- OccamDoc for declarations -->

<xsl:template name="type-string">
	<xsl:choose>
		<xsl:when test="@type = 'module'">
			<xsl:text>Module</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'proctype'">
			<xsl:text>Process type</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'proc'">
			<xsl:text>Process</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'function'">
			<xsl:text>Function</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'operator'">
			<xsl:text>Operator</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'record'">
			<xsl:text>Record</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'chantype'">
			<xsl:text>Channel type</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'protocol'">
			<xsl:text>Protocol</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'datatype'">
			<xsl:text>Data type</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'abbreviation'">
			<xsl:text>Abbreviation</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'constant'">
			<xsl:text>Constant</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'variable'">
			<xsl:text>Variable</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'tag'">
			<xsl:text>Tag</xsl:text>
		</xsl:when>
		<xsl:when test="@type = 'group'">
			<xsl:text>Group</xsl:text>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>Something else</xsl:text>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template name="decl-set">
	<ul>
	<xsl:for-each select="declaration">
		<xsl:sort order="ascending" select="@name" />
		<xsl:apply-templates select="." mode="index" />
	</xsl:for-each>
	</ul>
</xsl:template>

<xsl:template match="declaration" mode="index">
	<li>
		<a class="{@type}">
			<xsl:if test="$mode='frames-index'">
			<xsl:attribute name="target">page</xsl:attribute>
			</xsl:if>
			<xsl:choose>
				<xsl:when test="@type='module'">
					<xsl:attribute name="href"><xsl:value-of select="@name" />.html</xsl:attribute>
				</xsl:when>
				<xsl:otherwise>
					<xsl:attribute name="href"><xsl:value-of select="ancestor::declaration[@type='module']/@name" />.html#name-<xsl:value-of select="@name" /></xsl:attribute>
				</xsl:otherwise>
			</xsl:choose>

			<xsl:if test="$mode!='frames-index'">
			<xsl:call-template name="type-string" />
			<xsl:text> </xsl:text>
			</xsl:if>
			<xsl:call-template name="decl-name-list" />
		</a>
		<xsl:if test="summary and $mode!='frames-index'">
		<xsl:text> - </xsl:text>
		<xsl:value-of select="summary" />
		</xsl:if>

		<xsl:if test="children and @type!='module'">
		<br />
		<xsl:for-each select="children">
			<xsl:call-template name="decl-set" />
		</xsl:for-each>
		</xsl:if>
	</li>
</xsl:template>

<xsl:template name="decl-name-list">
	<xsl:for-each select="name">
		<code><xsl:value-of select="." /></code>
		<xsl:if test="position()!=last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
</xsl:template>

<xsl:template name="decl-param-items">
	<tr>
	<td class="param-type">
		<code><xsl:apply-templates select="definition" /></code>
	</td>
	<xsl:choose>
		<xsl:when test="item/@name">
			<td class="param-name">
			<xsl:for-each select="item">
				<code><xsl:value-of select="@name" /></code>
				<xsl:if test="position()!=last()"><xsl:text>, </xsl:text></xsl:if>
			</xsl:for-each>
			</td>
		</xsl:when>
		<xsl:otherwise>
			<td class="param-name" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:if test="description">
	<td class="param-desc">
		<xsl:apply-templates select="description" mode="inline" />
	</td>
	</xsl:if>
	</tr>
</xsl:template>

<xsl:template name="decl-params">
	<div class="description">
		<xsl:apply-templates select="description" />
	</div>

	<xsl:if test="deprecated">
	<p class="deprecated">
		Deprecated: do not use in new code.
	</p>
	</xsl:if>

	<xsl:if test="since">
	<h4>Available since:</h4>
	<div class="since">
		<xsl:apply-templates select="since" />
	</div>
	</xsl:if>

	<xsl:if test="params/description or items/description">
	<h4>Parameters:</h4>
	<div class="params">
		<table>
			<xsl:for-each select="params|items">
				<xsl:call-template name="decl-param-items" />
			</xsl:for-each>
		</table>
	</div>
	</xsl:if>

	<xsl:if test="returns/description">
	<h4>Returns:</h4>
	<div class="returns">
		<table>
			<xsl:for-each select="returns">
				<xsl:call-template name="decl-param-items" />
			</xsl:for-each>
		</table>
	</div>
	</xsl:if>

	<xsl:if test="maintainer">
	<h4>Maintainer:</h4>
	<div class="maintainer">
		<xsl:apply-templates select="maintainer" />
	</div>
	</xsl:if>
</xsl:template>

<xsl:template match="declaration[@type='module']">
	<div id="module-{@name}" class="module">
		<h1>
			<xsl:text>Module </xsl:text>
			<code><xsl:value-of select="name" /></code>
			<xsl:if test="summary">
				<xsl:text> - </xsl:text>
				<xsl:value-of select="summary" />
			</xsl:if>
		</h1>

		<xsl:if test="description">
		<div class="header">
			<xsl:call-template name="decl-params" />
		</div>
		</xsl:if>

		<h2>Index</h2>
		<xsl:for-each select="children">
		<xsl:call-template name="decl-set" />
		</xsl:for-each>

		<h2>Declarations</h2>
		<xsl:apply-templates select="children" />
	</div>
</xsl:template>

<xsl:template match="declaration">
	<div id="name-{@name}" class="declaration {@type}">
		<h3>
			<xsl:if test="lineNumber">
				<span class="location">
				<code><xsl:value-of select="filename" /></code>
				<xsl:text>:</xsl:text>
				<xsl:value-of select="lineNumber" />
				</span>
			</xsl:if>
			<xsl:call-template name="type-string" />
			<xsl:text> </xsl:text>
			<xsl:call-template name="decl-name-list" />
		</h3>

		<p class="definition">
			<code><xsl:value-of select="definition" /></code>
		</p>

		<xsl:call-template name="decl-params" />

		<xsl:if test="children">
		<div class="children">
			<xsl:apply-templates select="children" />
		</div>
		</xsl:if>
	</div>
</xsl:template>

<!-- Free text -->

<xsl:template match="link">
	<a href="{@href}">
		<xsl:apply-templates />
	</a>
</xsl:template>

<xsl:template match="ref">
	<xsl:choose>
		<xsl:when test="key('name',@href)/@type='module'">
			<a href="{key('name',@href)/@name}.html">
				<code><xsl:value-of select="key('name',@href)/name" /></code>
			</a>
		</xsl:when>
		<xsl:otherwise>
			<a href="{key('name',@href)/ancestor::declaration[@type='module']/@name}.html#name-{key('name',@href)/@name}">
				<code><xsl:value-of select="key('name',@href)/name" /></code>
			</a>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="para">
	<p><xsl:apply-templates /></p>
</xsl:template>

<xsl:template match="para" mode="inline">
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="code">
	<code><xsl:apply-templates /></code>
</xsl:template>

<xsl:template match="text">
	<code><xsl:apply-templates /></code>
</xsl:template>

<!-- These two rules use the pre element to render paragraphs containing only
     code blocks (and only whitespace in text elements). -->

<xsl:template match="para[normalize-space(.) = normalize-space(code)]">
	<pre><code><xsl:value-of select="code" /></code></pre>
</xsl:template>

<xsl:template match="para[normalize-space(.) = normalize-space(text)]">
	<pre><xsl:value-of select="text" /></pre>
</xsl:template>

<xsl:template match="em">
	<em><xsl:apply-templates /></em>
</xsl:template>

<xsl:template match="list">
	<ul><xsl:apply-templates /></ul>
</xsl:template>

<xsl:template match="listitem">
	<li><xsl:apply-templates /></li>
</xsl:template>

</xsl:stylesheet>
