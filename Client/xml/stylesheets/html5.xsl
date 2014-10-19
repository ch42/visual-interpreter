<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	
	<xsl:template name="insertDot">
		<xsl:param name="dotNeeded"  />
		
		<xsl:if test="$dotNeeded">
			<div class="symbol" data-dot="obligatory" />	
		</xsl:if>
	</xsl:template>	
	
	
	<xsl:template match="/">
		<xsl:apply-templates select="//strct" />
	</xsl:template>	

	<xsl:template match="strct">
		<div class="expr">
			<xsl:apply-templates />
		</div>	
	</xsl:template>
	
	<xsl:template match="wrapper">
		<xsl:param name="dot"  />
		<xsl:param name="brackets"  />
		
		<xsl:call-template name="insertDot">
			<xsl:with-param name="dotNeeded" select="$dot" />
		</xsl:call-template>
		
		<xsl:if test="attachment/keyword">
			
			<div class="keyword">	
				<div class="name">
					<xsl:value-of select="./attachment/keyword/text()" />
				</div>
				
				<div class="expr">	
					<xsl:apply-templates select="./content/bvar | ./content/fvar | ./content/wrapper | ./content/abstr | ./content/app">
						<xsl:with-param name="brackets" select="'optional'" />
					</xsl:apply-templates>	
				</div>
			</div>	
		</xsl:if>
		
		<xsl:if test="attachment/annotation">
			
			<div data-annotation="{./attachment/annotation/text()}">
				<xsl:apply-templates select="./content/bvar | ./content/fvar">
					<xsl:with-param name="brackets" select="'optional'" />
				</xsl:apply-templates>	
						
				<xsl:apply-templates select="./content/wrapper | ./content/abstr | ./content/app">
					<xsl:with-param name="brackets" select="$brackets" />
				</xsl:apply-templates>	 
			</div>
		</xsl:if>
		
		<xsl:if test="attachment/predecessor"> 
							
			<div class="substitution">
												
				<div class="substitute">
					<!-- no brackets (yet), wrapper might need brackets -->
					<xsl:apply-templates select="./content/wrapper | ./content/bvar | ./content/fvar"  />
						
					<!-- obligatory brackets -->
					<xsl:apply-templates select="./content/abstr">
						<xsl:with-param name="brackets" select="'obligatory'" />	
					</xsl:apply-templates>
						
					<!-- optional brackets -->
					<xsl:apply-templates select="./content/app">
						<xsl:with-param name="brackets" select="'optional'" />	
					</xsl:apply-templates>	
				</div>
				
				<div class="predecessor">
					<xsl:apply-templates select="./attachment/predecessor" />		
				</div>
			</div>	
		</xsl:if>

	</xsl:template>	
	
	<xsl:template match="left">
		<div class="left">
			<!-- no brackets (yet), wrapper might need brackets -->
			<xsl:apply-templates select="wrapper | bvar | fvar"  />
			
			<!-- obligatory brackets -->
			<xsl:apply-templates select="abstr">
				<xsl:with-param name="brackets" select="'obligatory'" />	
			</xsl:apply-templates>
			
			<!-- optional brackets -->
			<xsl:apply-templates select="app">
				<xsl:with-param name="brackets" select="'optional'" />	
			</xsl:apply-templates>
		</div>	
	</xsl:template>
	
	<xsl:template match="right">
		<div class="right">
			<!-- no brackets -->
			<xsl:apply-templates select="bvar | fvar" />
				
			<!-- obligatory brackets, (if wrapper content is a variable, no brackets needed -> check required) -->
			<xsl:apply-templates select="wrapper | abstr | app">
				<xsl:with-param name="brackets" select="'obligatory'" />		
			</xsl:apply-templates>
		</div>	
	</xsl:template>

	<xsl:template match="app">
		<xsl:param name="brackets" select="'optional'" />
		<xsl:param name="dot"  />
		<xsl:variable name="id" select="./@id" />
		
		<xsl:call-template name="insertDot">
			<xsl:with-param name="dotNeeded" select="$dot" />
		</xsl:call-template>
				
		<xsl:choose>
			<xsl:when test="@id = //meta/redexes/app/@id">
				<div class="app redex" data-id="{$id}" data-brackets="{$brackets}" data-dropdown="dropdown-redex">
					<xsl:apply-templates />	
				</div>		
			</xsl:when>

		  	<xsl:otherwise>
				<div class="app" data-id="{$id}" data-brackets="{$brackets}"> 
					<xsl:apply-templates />	
				</div>
		  	</xsl:otherwise>
		</xsl:choose>
	</xsl:template>	

	<xsl:template match="abstr">
		<!--- params -->
		<xsl:param name="brackets" select="'optional'" />
		<xsl:param name="lambda" select="'obligatory'" />
		
		<!-- span -->
		<div class="abstr" data-brackets="{$brackets}">
			<div class="abstrHead" data-lambda="{$lambda}">
				
				<xsl:variable name="id" select="./@bvar" />
				<div class="bvar" data-id="{$id}" data-dropdown="dropdown-bvar">
					<xsl:value-of select="//meta/bvars/bvar[@id = current()/@bvar]/@name"/>
				</div>	
				
			</div>

			<div class="abstrBody">
				<xsl:apply-templates select="wrapper | bvar | fvar | app">
					<xsl:with-param name="dot" select="'obligatory'" />
				</xsl:apply-templates>	
				
				<!-- nested abstraction -->
				<xsl:apply-templates select="abstr">
					<xsl:with-param name="lambda" select="'optional'" />		
				</xsl:apply-templates>
			</div>
		</div>	
	</xsl:template>

	<xsl:template match="bvar">
		<xsl:param name="lambda" />
		<xsl:param name="dot" />
		
		<xsl:call-template name="insertDot">
			<xsl:with-param name="dotNeeded" select="$dot" />
		</xsl:call-template>	
				
		<div class="bvar" data-id="{./@id}" data-dropdown="dropdown-bvar">
			<xsl:value-of select="//meta/bvars/bvar[@id = current()/@id]/@name"/>
		</div>	
	</xsl:template>

	<xsl:template match="fvar">
		<xsl:param name="dot" />
		
		<xsl:call-template name="insertDot">
			<xsl:with-param name="dotNeeded" select="$dot" />
		</xsl:call-template>
				
		<div class="fvar" data-id="{./@id}" data-dropdown="dropdown-fvar">	
			<xsl:value-of select="//meta/fvars/fvar[@id = current()/@id]/@name"/>
		</div>	
	</xsl:template>

</xsl:stylesheet>
