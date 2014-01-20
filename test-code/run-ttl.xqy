xquery version '1.0-ml';

declare namespace json="http://marklogic.com/xdmp/json";
declare namespace xhtml="http://www.w3.org/1999/xhtml";

import module namespace sem = "http://marklogic.com/semantics" at "/MarkLogic/semantics.xqy";

import module namespace rdfa="http://marklogic.com/ns/rdfa-impl#" at "rdfa.xqy";
import module namespace rdfa-ttl="urn:overstory:rdf:rdf-ttl" at "rdfa-ttl.xqy";

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

declare variable $hide-good-tests := xdmp:get-request-field ("hide-good", "false") cast as xs:boolean;

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
	let $name := fn:string (map:get ($result, "name"))
	let $comment := fn:string (map:get ($result, "comment"))
        let $xml-uri := fn:string (map:get ($result, "data"))
        let $xml := local:get-doc ($xml-uri)
        let $sparql-uri := fn:string (map:get ($result, "query"))
        let $sparql := local:get-doc ($sparql-uri)
        let $test-id := fn:substring-after (fn:string(map:get ($result, "test")), "#")
        let $expected-result := xs:boolean (map:get ($result, "expected"))

        return
        <test>
            <name>{ $name }</name>
            <comment>{ $comment }</comment>
            <input-xml>{ $xml }</input-xml>
            <sparql>{ $sparql }</sparql>
            <test-xml-uri>{ $xml-uri }</test-xml-uri>
            <test-number>{ $test-id }</test-number>
            <expected>{ $expected-result }</expected>
            <result>
            {
		    try {
			let $output-rdf := rdfa-ttl:rdfa-to-ttl ($xml, $xml-uri)
			let $ml-triples := sem:rdf-parse ($output-rdf, "turtle")
			let $sparql-result as xs:boolean := sem:sparql-triples ($sparql, $ml-triples)
			let $result := $sparql-result = $expected-result
			let $text-result := if ($result) then "Pass" else "Fail"
			return
			(
			    <actual-result>{ $sparql-result }</actual-result>,
			    <test-result>{ $text-result }</test-result>,
			    <output-rdf>{ $output-rdf }</output-rdf>,
			    <output-sem-triples>{ $ml-triples }</output-sem-triples>
			)
		    } catch ($e) {
		        let $output-rdf :=
		        	try {
		        		(: In case the problem is with sem:rdf-parse() :)
		        		rdfa-ttl:rdfa-to-ttl ($xml, $xml-uri)
		        	} catch ($e) {
		        		()
		        	}

		        return
		    	(
				<actual-result>exception</actual-result>,
				<test-result>Error</test-result>,
			        if (fn:exists ($output-rdf)) then <output-rdf>{ $output-rdf }</output-rdf> else (),
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

declare function local:format-test-result (
	$test as element(test)
) as element(xhtml:div)
{
	let $name := $test/name
	let $comment := $test/comment
	let $test-number := $test/test-number
	let $test-result := $test/result/test-result
	let $expected := $test/expected
	let $actual-result := $test/result/actual-result
	let $input-xml := $test/input-xml/node()
	let $output-rdf := ($test/result/output-rdf/node(), <no-rdf-xml-output/>)[1]
	let $sparql := $test/sparql/node()
	let $output-sem-triples := ($test/result/output-sem-triples, $test/result/test-error)[1]
	let $error-msg := $test/result/test-error/error:error/error:format-string/fn:string()
	let $quote-options := <options xmlns="xdmp:quote"><indent>yes</indent><indent-untyped>yes</indent-untyped></options>

	return
	<div class="test-detail" xmlns="http://www.w3.org/1999/xhtml">
		<h3 class="{ $test-result }">{ $name } ({ $comment })</h3>
		<p class="test-result">{ $test-number } Expected: { $expected }, Actual: { $actual-result } &nbsp; { if (fn:exists ($error-msg)) then <span class="error-msg">{ $error-msg }</span> else () }</p>
		<div class="xml-output"><p><pre class="brush: xml">{ xdmp:quote ($input-xml, $quote-options) }</pre></p></div>
		<div class="xml-output"><p ><pre>{ $output-rdf }</pre></p></div>
		<div class="xml-output"><p ><pre>{ $sparql }</pre></p></div>
		{ if (fn:exists ($output-sem-triples)) then <div class="xml-output"><p><pre class="brush: xml">{ xdmp:quote ($output-sem-triples, $quote-options) }</pre></p></div> else () }
	</div>
};

declare function local:render-tests (
	$tests as element(test)*
) as element()
{
	<html xmlns="http://www.w3.org/1999/xhtml">
		<head>
			<title>OverStroy RDFa Test Suite Result</title>
			<link href="/css/shCore.css" rel="stylesheet" type="text/css"/>
			<link href="/css/shThemeDefault.css" rel="stylesheet" type="text/css"/>
			<link rel="stylesheet" type="text/css" href="/css/std.css"/>
			<link rel="stylesheet" type="text/css" href="/css/tests.css"/>
			<script type="text/javascript" src="/js/shCore.js"></script>
			<script type="text/javascript" src="/js/shBrushXml.js"></script>
			<script type="text/javascript" src="/js/shBrushPlain.js"></script>
			<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"></script>
			<script type="text/javascript" src="http://code.highcharts.com/highcharts.js"></script>
			<script type="text/javascript" src="/js/test-result-chart.js"></script>
		</head>
		<body style="height: 95%;">
			<img src="/images/logo-overstory-co-uk-white-bg-x200.png" style="float: right; padding: 10px;" width="150" height="114"/>
			<div style="padding-top: 4.0em;">
				<h1>RDFa Test Results</h1>
			</div>

			<div id="graph-container"></div>

			<div class="run-box">
				<p>
					<a href="?hide-good=true">Run and list only Error/Fail results</a>
				</p>
				<p>
					<a href="?hide-good=false">Run and list all test results</a>
				</p>
			</div>

			<div class="test-results">{
				for $test in $tests[fn:not ($hide-good-tests) or (*:result/*:test-result ne "Pass")]
				order by $test/*:result/*:test-result, $test/*:test-number
				return local:format-test-result ($test)
			}</div>
		</body>
		<script>
			$(function(){{
				var chart = $('#graph-container').highcharts();
				chart.series[0].setData ({ local:inject-counts ($tests) });
			}});
		</script>
		<script type="text/javascript">
		     SyntaxHighlighter.all()
		</script>
	</html>
};

local:render-tests (local:run-tests ($q))

