xquery version '1.0-ml';

declare namespace json="http://marklogic.com/xdmp/json";

import module namespace sem = "http://marklogic.com/semantics" at "/MarkLogic/semantics.xqy";

import module namespace rdfa="http://marklogic.com/ns/rdfa-impl#" at "rdfa.xqy";

declare variable $q := '
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX mf: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
PREFIX qt: <http://www.w3.org/2001/sw/DataAccess/tests/test-query#>
PREFIX test: <http://www.w3.org/2006/03/test-description#>

SELECT DISTINCT ?test ?name ?comment ?query ?data ?expected
  WHERE
  {
    ?test a mf:QueryEvaluationTest.
    ?test mf:name ?name.
    ?test rdfs:comment ?comment.
    ?test mf:action ?action.
      ?action qt:query ?query.
      ?action qt:data ?data.
    ?test mf:result ?expected.
  }
  ORDER BY ASC(?test)
';

declare variable $xml-options :=
	<options xmlns="xdmp:document-get">
           <format>xml</format>
           <encoding>UTF-8</encoding>
       </options>;

declare variable $text-options :=
	<options xmlns="xdmp:document-get">
           <format>text</format>
           <encoding>UTF-8</encoding>
       </options>;

declare variable $manifest-url := 'http://rdfa.info/test-suite/rdfa1.1/xml/manifest';

declare variable $manifest-graph-uri := 'http://overstory.co.uk/semantics#manifest-graph';

declare variable $rdfa-info-url-root := "http://rdfa.info";

declare variable $rdfa-info-url-full := 'http://rdfa.info/test-suite/test-cases/rdfa1.1/xml/';

declare variable $DATA-MESH-URI-SPACE-ROOT := "http://data.overstory.co.uk/resources/";

(: declare variable $pass-fail-map := map:map(); :)


declare function local:load-doc-from-uri (
	$uri as xs:string,
	$doc-uri as xs:string
) as node()?
{
	let $node :=
		try {
			xdmp:document-get ($uri, $xml-options)/node()
		} catch ($e) {
			try {
				xdmp:document-get ($uri, $text-options)/node()
			} catch ($e) {
				()
			}
		}

	return if (fn:exists ($node)) then xdmp:document-insert ($doc-uri, $node) else ()
};

declare function local:get-doc (
	$uri as xs:string
) as node()?
{
        let $doc-uri := fn:substring-after ($uri, $rdfa-info-url-root)
        let $node := fn:doc ($doc-uri)/node()

	return
	if (fn:exists ($node)) then $node else local:load-doc-from-uri ($uri, $doc-uri)
};

declare function local:count-result (
	$map as map:map,
	$result as xs:string
) as empty-sequence()
{
	let $current := (map:get ($map, $result), 0)[1]
	return map:put ($map, $result, $current + 1)
};

declare function local:run-tests (
	$sparql as xs:string
) as element(test)*
{
	for $result in sem:sparql ($sparql)
        let $xml-uri := fn:string (map:get ($result, "data"))
        let $xml := local:get-doc ($xml-uri)
        let $sparql-uri := fn:string (map:get ($result, "query"))
        let $sparql := local:get-doc ($sparql-uri)
        let $test-id := fn:substring-after (fn:string(map:get ($result, "test")), "#")
        let $expected-result := xs:boolean (map:get ($result, "expected"))

        return
        <test>
            <xml>{ $xml }</xml>
            <sparql>{ $sparql }</sparql>
            <test-xml-uri>{ $xml-uri }</test-xml-uri>
            <test-number>{ $test-id }</test-number>
            <expected>{$expected-result}</expected>
            <result>
            {
		    try {
			let $output-rdf := rdfa:parse_rdfa ($xml, $xml-uri)
			let $ml-triples := sem:rdf-parse ($output-rdf, "rdfxml")
			let $sparql-result as xs:boolean := sem:sparql-triples ($sparql, $ml-triples)
			let $result := $sparql-result = $expected-result
			let $text-result := if ($result) then "Pass" else "Fail"
			(: let $_ := local:count-result ($pass-fail-map, $text-result) :)
			return
			(
			    <sparql-result>{ $sparql-result }</sparql-result>,
			    <test-result>{ $text-result }</test-result>
			)
		    } catch ($e) {
		    	(
		    		(: local:count-result ($pass-fail-map, "Error"), :)
				<sparql-result>false - exception</sparql-result>,
				<test-result>Error</test-result>,
				<test-error>{ $e }</test-error>
		    	)
		    }
            }
            </result>
        </test>
};

declare function local:inject-counts (
	$tests as element(test)*
) as xs:string
{
	xdmp:to-json (
		json:array (
			<json:array>{
				for $r in ("Pass", "Fail", "Error")
				return
				<json:array>
					<json:value xsi:type="xs:string">{ $r }</json:value>
					<json:value xsi:type="xs:integer">{ fn:count ($tests//test-result[. = $r]) }</json:value>
				</json:array>
			}</json:array>
		)
	)
};

declare function local:render-tests (
	$tests as element(test)*
) as element()
{
	<html xmlns="http://www.w3.org/1999/xhtml">
	    <head>
		<title>OverStroy RDFa Test Suite Result</title>
		<link rel="stylesheet" type="text/css" href="/css/std.css"/>
		<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"></script>
		<script src="http://code.highcharts.com/highcharts.js"></script>
		<script src="/js/test-result-chart.js"></script>
	    </head>
	    <body style="height: 95%;">
		<img src="/images/logo-overstory-co-uk-white-bg-x200.png" style="float: right; padding: 10px;" width="150" height="114"/>
		<div style="padding-top: 4.0em;">
		    <h1>RDFa Test Results</h1>
		</div>
		<p>
		    <div id="graph-container" style="width:80%; height:0px;"></div>
		</p>

		<script>
			$(function(){{
                                var chart = $('#graph-container').highcharts();
                                chart.series[0].setData ({ local:inject-counts ($tests) });
			}});
		</script>

	    </body>
	</html>
};

local:render-tests (local:run-tests ($q))

