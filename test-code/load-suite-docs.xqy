xquery version '1.0-ml';

import module namespace sem = "http://marklogic.com/semantics" at "/MarkLogic/semantics.xqy";

declare variable $q := '
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX mf: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
PREFIX qt: <http://www.w3.org/2001/sw/DataAccess/tests/test-query#>
PREFIX test: <http://www.w3.org/2006/03/test-description#>

SELECT ?name ?comment ?specref ?query ?data ?expected
  WHERE
  {
    ?test a mf:QueryEvaluationTest.
    ?test mf:name ?name.
    ?test rdfs:comment ?comment.
    ?test mf:action ?action.
    ?test test:specificationReference ?specref.
      ?action qt:query ?query.
      ?action qt:data ?data.
    ?test mf:result ?expected.
  }
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

declare function local:get-data (
	$url as xs:string
) as node()?
{
	try {
		xdmp:document-get ($url, $xml-options)/node()
	} catch ($e) {
		try {
			xdmp:document-get ($url, $text-options)/node()
		} catch ($e) {
			()
		}
	}
};

declare variable $rdfa-info-url-root := "http://rdfa.info";


    for $result in sem:sparql ($q)
    let $xml-uri := fn:string (map:get ($result, "data"))
    let $xml-doc-uri := fn:substring-after ($xml-uri, $rdfa-info-url-root)
    let $xml := local:get-data ($xml-uri)
    let $sparql-uri := fn:string (map:get ($result, "query"))
    let $sparql-doc-uri := fn:substring-after ($sparql-uri, $rdfa-info-url-root)
    let $sparql := xdmp:document-get ($sparql-uri, $text-options)/text()
    return (
    	xdmp:document-insert ($xml-doc-uri, $xml), $xml-doc-uri,
    	xdmp:document-insert ($sparql-doc-uri, $sparql), $sparql-doc-uri
    )
