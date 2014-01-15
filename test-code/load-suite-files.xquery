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

declare variable $get-options :=
        <options xmlns="xdmp:document-get">
           <format>xml</format>
           <encoding>UTF-8</encoding>
       </options>;

for $result in sem:sparql ($q)[23]
return xdmp:document-get (fn:string (map:get ($result, "data")), $get-options)