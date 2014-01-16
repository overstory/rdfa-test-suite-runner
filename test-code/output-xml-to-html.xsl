<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="xs"
    version="2.0">
    

    
    <xsl:template match="output">
        <xsl:variable name="pass">
            <xsl:variable name="pass-number" select="count(//test-result[contains(lower-case(.),'passed')])"/>
            <xsl:choose>
                <xsl:when test="$pass-number=0">
                    <xsl:text>0</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$pass-number"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
        <xsl:variable name="fail">
            <xsl:variable name="fail-number" select="count(//test-result[contains(lower-case(.),'failed')])"/>
            <xsl:choose>
                <xsl:when test="$fail-number=0">
                    <xsl:text>0</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$fail-number"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
        <xsl:variable name="data">
            data: 
            [
                ['Pass', <xsl:value-of select="$pass"/> ],
                ['Fail', <xsl:value-of select="$fail"/> ]
            ]
        </xsl:variable>
        
        
        <html xmlns="http://www.w3.org/1999/xhtml">
            <head>
                <title>RDFa Test Suite Result</title>
                <link rel="stylesheet" type="text/css" href="/css/std.css"/>
                <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"></script>
                <script src="http://code.highcharts.com/highcharts.js"></script>
                <script>
                    $(function () { 
                    $('#graph-container').highcharts({
                    chart: {
                    type: 'pie',
                    borderColor: '#66aaaa',
                    borderWidth: 1,
                    backgroundColor: ''
                    },
                    title: {
                    text: 'Results'
                    },
                    
                    plotOptions: {
                    pie: {
                    allowPointSelect: true,
                    cursor: 'pointer',
                    dataLabels: {
                    enabled: true,
                    color: '#000000',
                    connectorColor: '#000000',
                    format: '<b>{point.name}</b>: {point.percentage:.1f} %'
                    }
                    }
                    },
                    
                    
                    series: [{
                    type: 'pie',
                    name: 'Result',
                    <xsl:value-of select="$data"/>
                    }]
                    });
                    });
                </script>
            </head>
            <body style="height: 95%;">
                <img src="/images/logo-overstory-co-uk-white-bg-x200.png" style="float: right; padding: 10px;" width="150" height="114"/>
                <div style="padding-top: 4.0em;">
                    <h1>Results</h1>
                </div>
                <p>
                    <div id="graph-container" style="width:80%; height:0px;"></div>
                </p>
            </body>
            
        </html>
        
    </xsl:template>
    
</xsl:stylesheet>