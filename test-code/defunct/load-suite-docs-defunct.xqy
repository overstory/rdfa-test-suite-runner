xquery version '1.0-ml';

import module namespace sem = "http://marklogic.com/semantics" at "/MarkLogic/semantics.xqy";

import module namespace rdfa="http://marklogic.com/ns/rdfa-impl#" at "rdfa.xqy";

declare variable $q := '
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX mf: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
PREFIX qt: <http://www.w3.org/2001/sw/DataAccess/tests/test-query#>
PREFIX test: <http://www.w3.org/2006/03/test-description#>

SELECT DISTINCT ?name ?comment ?query ?data ?expected
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
  ORDER BY ASC(?data)
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

declare variable $output-to-html-xslt := xdmp:document-get("/output-xml-to-html.xsl");


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

declare function local:load-manifest(
    $url as xs:string
)
{
    try {
        sem:rdf-load($url, 'turtle', (), (), $manifest-graph-uri)
        } catch ($e)
        {
            ()
        }
};


(: load manifest on first run :)

(:local:load-manifest($manifest-url),:)

(: run sparql on the loaded manifest :)

declare variable $output := 
<output>
{
for $result in sem:sparql ($q) 
let $xml-uri := fn:string (map:get ($result, "data"))
let $xml-doc-uri := fn:substring-after ($xml-uri, $rdfa-info-url-root)
let $xml := local:get-data ($xml-uri)
let $sparql-uri := fn:string (map:get ($result, "query"))
let $sparql-doc-uri := fn:substring-after ($sparql-uri, $rdfa-info-url-root)
let $sparql := xdmp:document-get ($sparql-uri, $text-options)/text()
let $current-test-number := fn:substring-before(fn:substring-after($xml-uri, $rdfa-info-url-full), '.xml')
let $expected-result := fn:string(map:get ($result, "expected"))


return (
<entry>
   
    <test-number>{$current-test-number}</test-number>
    <expected>{$expected-result}</expected>
    <result>
    {
    try {
        let $output-rdf := rdfa:parse_rdfa($xml, $xml-uri)
        let $ml-triples := sem:rdf-parse($output-rdf, "rdfxml")
        let $sparql-result := sem:sparql-triples($sparql, $ml-triples)
        return
        (
            (: REMOVE DEBUG WHEN DONE :)
            (:<sparql>{$sparql}</sparql>,
            <input-xml>{$xml}</input-xml>,
            <rdf>{$output-rdf}</rdf>,
            <triples>{$ml-triples}</triples>,:)
            <sparql-result>
            {
                $sparql-result
             }
            </sparql-result>
            ,
            <test-result>
            {
                if (xs:boolean($expected-result)=$sparql-result)
                    then( "PASSED" )
                    else( "FAILED" )
            }
            </test-result>
            
            
        )
    } catch($e)
    {
        <sparql-result>{"false - exception"}</sparql-result>,
        <test-result>{
            if (xs:boolean($expected-result)=fn:false()) 
                then ( "PASSED" )
                else(
                (: REMOVE WHEN DONE :)
                (:$e:)
                "FAILED"
                )
            }
        </test-result>

    }
    }
    </result>
</entry>
) 
}
</output>;

xdmp:xslt-invoke('/output-xml-to-html.xsl', $output)

