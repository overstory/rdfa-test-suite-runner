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

declare variable $manifest-xml-url := 'http://rdfa.info/test-suite/rdfa1.1/xml/manifest';

declare variable $manifest-graph := 'http://overstory.co.uk/semantics#manifest-graph';

declare variable $tests-collection := 'uri:overstory:co:uk:test:suite';

declare variable $test-files-collection := 'uri:overstory:co:uk:test:files';

declare variable $test-queries-collection := 'uri:overstory:co:uk:test:queries';
       
(: Load manifest ttl into ML triplestore, graph: http://overstory.co.uk/semantics#manifest-graph :)       
   
sem:rdf-load($manifest-xml-url, 'turtle', (), (), $manifest-graph),

(: Run sparql query to retrieve all XML and all SPARQL required for tests and load them to the db :)

let $test-map := sem:query-results-serialize(sem:sparql($sparql-manifest))
return 
(
    (: load all tests :)
    
    load-tests($test-map),
    
    (: load all queries for tests :)
    
    load-queries($test-map)
    
    (:
    for each file in collection uri:overstory:co:uk:test:files
    
    1. transform with rdfa.xquery
    2. run equivalent sparql number on the result
    3. get the expected result from return-result-for-test-number()
    4. compare with what we get from running sparql 
    5. save information in the output file
    
    :)
    
)


declare function load-tests (
	$test-map as element()
)
{
    let $test-files := distinct-values($test-map/res:results/res:result/res:binding[@name='data']/res:uri/string())
    return
    (
        for $xml-test in $test-files 
        return
        (
          let $directory-uri := substring-after($xml-test, 'http://rdfa.info')
          return
          ( 
          xdmp:document-load($xml-test,
          <options xmlns="xdmp:document-load">
             <uri>{$insert-uri}</uri>
             <permissions>{xdmp:default-permissions()}</permissions>
             <collections>
                <collection>{$tests-collection}</collection>
                <collection>{$test-files-collection}</collection>
             </collections>
             <repair>full</repair>
          </options> )
          )
        )
    )
	
};

declare function load-queries (
	$test-map as element()
)
{
    let $test-queries := distinct-values($test-map/res:results/res:result/res:binding[@name='query']/res:uri/string())
    return
    (
        for $xml-test in $test-queries 
        return
        (
          let $directory-uri := substring-after($xml-test, 'http://rdfa.info')
          return
          ( 
          xdmp:document-load($xml-test,
          <options xmlns="xdmp:document-load">
             <uri>{$insert-uri}</uri>
             <permissions>{xdmp:default-permissions()}</permissions>
             <collections>
                <collection>{$tests-collection}</collection>
                <collection>{$test-queries-collection}</collection>
             </collections>
             <repair>full</repair>
          </options> )
          )
        )
    )
};

declare function return-result-for-test-number (
    $test-map as element(),
    $test-number as xs:string
)
{
    let $target-resource := $test-map/res:results/res:result[contains(res:binding[@name='data']/res:uri, $test-number)]
    return
    (
        $target-resource/res:binding[@name='expected']/res:literal
    )
};



(:
for $result in sem:sparql ($q)[23]
return xdmp:document-get (fn:string (map:get ($result, "data")), $get-options)
:)