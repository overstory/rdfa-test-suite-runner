xquery version "1.0-ml";

(:
 : Copyright (c) 2008 Mark Logic Corporation. All rights reserved.
 :
 : Licensed under the Apache License, Version 2.0 (the "License");
 : you may not use this file except in compliance with the License.
 : You may obtain a copy of the License at
 :
 :     http://www.apache.org/licenses/LICENSE-2.0
 :
 : Unless required by applicable law or agreed to in writing, software
 : distributed under the License is distributed on an "AS IS" BASIS,
 : WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 : See the License for the specific language governing permissions and
 : limitations under the License.
 :)

(: default element namespace="http://www.w3.org/1999/xhtml" :)

module namespace ml = "http://marklogic.com/ns/rdfa-impl#";
declare default function namespace "http://www.w3.org/2005/xpath-functions";
declare namespace html = "http://www.w3.org/1999/xhtml";

declare namespace rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#";

declare variable $dfvocab := "http://www.w3.org/1999/xhtml/vocab#";
declare variable $default-base := ("http://BASE.URI");
declare variable $htmlrels := ( "alternate",
                                "appendix",
                                "bookmark",
                                "cite",
                                "chapter",
                                "contents",
                                "copyright",
                                "glossary",
                                "help",
                                "icon",
                                "index",
                                "last",
                                "license",
                                "meta",
                                "next",
                                "p3pv1",
                                "prev",
                                "role",
                                "section",
                                "start",
                                "stylesheet",
                                "subsection",
                                "up" );

(: Preloaded with default vocabulary prefixes.  See: http://www.w3.org/2011/rdfa-context/rdfa-1.1 :)
declare variable $prefixes-map := map:map(
<map:map xmlns:map="http://marklogic.com/xdmp/map" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <map:entry key="grddl"><map:value xsi:type="xs:string">http://www.w3.org/2003/g/data-view#</map:value></map:entry>
    <map:entry key="ma"><map:value xsi:type="xs:string">http://www.w3.org/ns/ma-ont#</map:value></map:entry>
    <map:entry key="owl"><map:value xsi:type="xs:string">http://www.w3.org/2002/07/owl#</map:value></map:entry>
    <map:entry key="prov"><map:value xsi:type="xs:string">http://www.w3.org/ns/prov#</map:value></map:entry>
    <map:entry key="rdf"><map:value xsi:type="xs:string">http://www.w3.org/1999/02/22-rdf-syntax-ns#</map:value></map:entry>
    <map:entry key="rdfa"><map:value xsi:type="xs:string">http://www.w3.org/ns/rdfa#</map:value></map:entry>
    <map:entry key="rdfs"><map:value xsi:type="xs:string">http://www.w3.org/2000/01/rdf-schema#</map:value></map:entry>
    <map:entry key="rif"><map:value xsi:type="xs:string">http://www.w3.org/2007/rif#</map:value></map:entry>
    <map:entry key="rr"><map:value xsi:type="xs:string">http://www.w3.org/ns/r2rml#</map:value></map:entry>
    <map:entry key="sd"><map:value xsi:type="xs:string">http://www.w3.org/ns/sparql-service-description#</map:value></map:entry>
    <map:entry key="skos"><map:value xsi:type="xs:string">http://www.w3.org/2004/02/skos/core#</map:value></map:entry>
    <map:entry key="skosxl"><map:value xsi:type="xs:string">http://www.w3.org/2008/05/skos-xl#</map:value></map:entry>
    <map:entry key="wdr"><map:value xsi:type="xs:string">http://www.w3.org/2007/05/powder#</map:value></map:entry>
    <map:entry key="void"><map:value xsi:type="xs:string">http://rdfs.org/ns/void#</map:value></map:entry>
    <map:entry key="wdrs"><map:value xsi:type="xs:string">http://www.w3.org/2007/05/powder-s#</map:value></map:entry>
    <map:entry key="xhv"><map:value xsi:type="xs:string">http://www.w3.org/1999/xhtml/vocab#</map:value></map:entry>
    <map:entry key="xml"><map:value xsi:type="xs:string">http://www.w3.org/XML/1998/namespace</map:value></map:entry>
    <map:entry key="xsd"><map:value xsi:type="xs:string">http://www.w3.org/2001/XMLSchema#</map:value></map:entry>

    <map:entry key="cc"><map:value xsi:type="xs:string">http://creativecommons.org/ns#</map:value></map:entry>
    <map:entry key="ctag"><map:value xsi:type="xs:string">http://commontag.org/ns#</map:value></map:entry>
    <map:entry key="dc"><map:value xsi:type="xs:string">http://purl.org/dc/terms/</map:value></map:entry>
    <map:entry key="dcterms"><map:value xsi:type="xs:string">http://purl.org/dc/terms/</map:value></map:entry>
    <map:entry key="dc11"><map:value xsi:type="xs:string">http://purl.org/dc/elements/1.1/</map:value></map:entry>
    <map:entry key="foaf"><map:value xsi:type="xs:string">http://xmlns.com/foaf/0.1/</map:value></map:entry>
    <map:entry key="gr"><map:value xsi:type="xs:string">http://purl.org/goodrelations/v1#</map:value></map:entry>
    <map:entry key="ical"><map:value xsi:type="xs:string">http://www.w3.org/2002/12/cal/icaltzd#</map:value></map:entry>
    <map:entry key="og"><map:value xsi:type="xs:string">http://ogp.me/ns#</map:value></map:entry>
    <map:entry key="rev"><map:value xsi:type="xs:string">http://purl.org/stuff/rev#</map:value></map:entry>
    <map:entry key="sioc"><map:value xsi:type="xs:string">http://rdfs.org/sioc/ns#</map:value></map:entry>
    <map:entry key="v"><map:value xsi:type="xs:string">http://rdf.data-vocabulary.org/#</map:value></map:entry>
    <map:entry key="vcard"><map:value xsi:type="xs:string">http://www.w3.org/2006/vcard/ns#</map:value></map:entry>
    <map:entry key="schema"><map:value xsi:type="xs:string">http://schema.org/</map:value></map:entry>

    <map:entry key="describedby"><map:value xsi:type="xs:string">http://www.w3.org/2007/05/powder-s#describedby</map:value></map:entry>
    <map:entry key="license"><map:value xsi:type="xs:string">http://www.w3.org/1999/xhtml/vocab#license</map:value></map:entry>
    <map:entry key="role"><map:value xsi:type="xs:string">http://www.w3.org/1999/xhtml/vocab#role</map:value></map:entry>
</map:map>
);

(: we don't have a guarantee that base-uri will be set in non-database docs
, so we have to explicitly pass it in :)
declare function ml:parse_rdfa (
	$doc as node(),
	$url as xs:string
) as element(rdf:RDF)
{
		(: ToDo: change calls to namespace-uri-for-prefix to a local funciton that will look in the map
		:)

	ml:load-namespace-map-from-prefixes ($doc/@prefix/fn:string()),

	<rdf:RDF>{
		ml:namespaces-from-prefix-map ($prefixes-map),
		$doc/namespace::*,

	    let $base := if ($doc//html:head/html:base/@href)
			 then $doc//html:head/html:base/@href
			 else if ($url)
			      then $url
			      else $default-base
	    for $node in $doc//*
	    return (
	    
		if ($node/@property)
		then ml:property($node, string($node/@property), $base)
		else (),

		if ($node/@rel)
		then ml:relrev($node, string($node/@rel), "rel", $base)
		else (),

		if ($node/@rev)
		then ml:relrev($node, string($node/@rev), "rev", $base)
		else (),

		if ($node/@typeof)
		then ml:typeof($node, string($node/@typeof), $base)
		else ()

	    )
	}</rdf:RDF>
};

declare function ml:load-namespace-map-from-prefixes (
	$prefixes-str as xs:string
) as empty-sequence()
{
	let $tokens := tokenize ($prefixes-str, "\s+")
	let $_ :=
		for $token at $idx in $tokens
		return
		if (($idx mod 2) = 1)
		then (
			map:put ($prefixes-map, fn:substring-before ($token, ":"), $tokens[$idx + 1])
		) else ()
	return ()
};

declare function ml:namespaces-from-prefix-map (
	$prefix-map as map:map
) as node()*
{
	for $prefix in map:keys ($prefix-map)
	return namespace { $prefix } { map:get ($prefix-map, $prefix) }
};

declare function ml:namespace-uri-for-prefix (
	$prefix as xs:string,
	$node as node()
) as xs:string?
{
	let $uri := map:get ($prefixes-map, $prefix)
	return
	if (fn:exists ($uri))
	then $uri
	else fn:namespace-uri-for-prefix ($prefix, $node)
};

(: for triples created from this very $node, what is the subject? :)
declare function ml:subject($node as node(), $base as xs:string) {
    if ($node/@about)
    then ml:safe-resolve-uri-or-curie($node/@about, $node, $base)
    else if ($node/@src)
         then ml:safe-resolve-uri($node/@src, $base)
         else if (local-name($node) = ("head", "body"))
              then $base
               (: Marcin change 
               else if ($node/@typeof)
               then ml:generate-bnode-id($node, "typeof")
               else ml:subject-ancestor($node/.., $base)
               
               if element has @typeof and does not contain @about then it's a blank node
               :)
              else if ($node/@typeof and $node/@about)
                   then ml:generate-bnode-id($node, "typeof")
                   else ml:subject-ancestor($node/.., $base)
};

(: looking down the ancestor chain, what is the subject? :)
declare function ml:subject-ancestor($node as node(), $base as xs:string) {
    if ($node/@resource)
    then ml:safe-resolve-uri-or-curie($node/@resource, $node, $base)
    else if ($node/@href)
         then ml:safe-resolve-uri($node/@href, $base)
         else if ($node/(@rel | @rev))
              then ml:generate-bnode-id($node)
              else if ($node/@about)
                  then ml:safe-resolve-uri-or-curie($node/@about, $node, $base)
                  else if ($node/@src)
                       then ml:safe-resolve-uri($node/@src, $base)
                        else if ($node/@typeof)
                             then ml:generate-bnode-id($node, "typeof")
                             else if ($node/..)
                                  then ml:subject-ancestor($node/.., $base)
                                  else $base
};

declare function ml:property($node as node(), $val as xs:string, $base as xs:string) as element()* {
    for $prop in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
    let $prefix := substring-before($prop, ":")
    let $nsuri := ml:namespace-uri-for-prefix($prefix, $node)
    (: 
    Marcin: the second bit is incorrect, a node can have nested XML elements and not be a XMLLiteral
    let $isXML := ($node/@datatype and ml:expand-curie($node/@datatype, $node) eq "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral" or
                  (not($node/@datatype) and $node/node() and $node/(node() except text()) ))
    :)
    let $isXML := ($node/@datatype and ml:expand-curie($node/@datatype, $node) eq "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")
    
    let $effective-dt := if ($node/@datatype and $node/@datatype ne "")
                         then
                             if ($isXML)
                             then false() (: "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral" :)
                             else ml:expand-curie(($node/@datatype,"")[1], $node)
                         else false()
    let $lang := ($node/ancestor-or-self::*/@xml:lang)[position() eq last()]
    let $subj := ml:subject($node, $base)
    let $bnode-ref := ml:generate-bnode-id($node, "typeof")

    where 1
    return
        if ($subj)
        then
            
            <rdf:Description>
            {
                (: 
                Marcin: if bnode then nodeID should be used for local referencing
                :)
                if (starts-with($subj, "_"))
                then attribute rdf:nodeID { $subj }
                else attribute rdf:about { $subj },

                element { ml:curie-to-qname($prop, $node) }
                {

                    
                    (: attribute ns { $prefix }, :)
                    if ($effective-dt)
                    then attribute rdf:datatype {$effective-dt}
                    else (),

                    $lang,
                    
                    
                    (:"testsubj: ", $subj,
                    "test effective-dt: ", $effective-dt,
                    "test isXML: ", $isXML,
                    "test prop: ", $prop,:)
                    


                    (: proper XML Literal?? :)
                    if ($isXML)
                    then (attribute rdf:parseType { "Literal" } , for $n in $node/node() return ml:deep-copy($n) )
                        else if ($node/@typeof and not($node/@about))
                        then 
                        ( 
                            attribute rdf:nodeID {string($bnode-ref)}
                        )
                        
                        else string(if ($node/@content) then $node/@content else $node)

                }
            }
            </rdf:Description>
        else ()
};

declare function ml:relrev($node as node(), $val as xs:string, $relorrev, $base as xs:string) as element()* {
    if ($node/@resource or $node/@href)
    then ml:relrev-immed($node, $val, $relorrev, $base)
    else (ml:relrev-hanging($node, $val, $relorrev, $base), ml:relrev-hanging-bnode($node, $val, $relorrev, $base))
};

(: Generate an immediate triple (or several, if @rel or @ref is a space-sep list) :)
declare function ml:relrev-immed($node as node(), $val as xs:string, $relorrev, $base as xs:string) as element()* {
    for $relv in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
    let $prefix := substring-before($relv, ":")
    let $locobj := if ($node/@resource)
                   then ml:safe-resolve-uri-or-curie($node/@resource, $node, $base)
                   else  ml:safe-resolve-uri($node/@href, $base)

    let $locsbj := ml:subject($node, $base)
    let $effective-sbj := if ($relorrev eq "rel") then $locsbj else $locobj
    let $effective-obj := if ($relorrev eq "rel") then $locobj else $locsbj
    where ml:curie-is-valid($relv, $node)
    return
        if ($locsbj and $locobj)
        then
            <rdf:Description>
            {
                if (starts-with($effective-sbj, "_"))
                then attribute rdf:nodeID { $effective-sbj }
                else attribute rdf:about { $effective-sbj },

                element { ml:curie-to-qname($relv, $node) }
                {
                    (: attribute ns { $prefix }, :)
                    if (starts-with($effective-obj, "_"))
                    then attribute rdf:nodeID { $effective-obj }
                    else attribute rdf:resource { $effective-obj }
                }
            }
            </rdf:Description>
        else ()
};

(: Generate potentially multiple, hanging triples :)
declare function ml:relrev-hanging($node as node(), $val as xs:string, $relorrev, $base as xs:string) as element()* {
    for $relv in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
    let $prefix := substring-before($relv, ":")
    let $locsbj := ml:subject($node, $base)
    for $tpl in ml:hanging-descendants($node)
    let $locobj := if ($tpl/@about)
                   then ml:safe-resolve-uri-or-curie($tpl/@about, $tpl, $base)
                   else if ($tpl/@src)
                        then ml:safe-resolve-uri($tpl/@src, $base)
                        else if ($tpl/@typeof)
                             then ml:generate-bnode-id($tpl, "typeof")
                             else if ($tpl/(@rel | @rev))
                                  then ()
                                  else if ($tpl/@resource)
                                       then ml:safe-resolve-uri-or-curie($tpl/@resource, $tpl, $base)
                                       else if ($tpl/@href)
                                            then ml:safe-resolve-uri($tpl/@href, $base)
                                            else ml:generate-bnode-id($tpl)
    let $effective-sbj := if ($relorrev eq "rel") then $locsbj else $locobj
    let $effective-obj := if ($relorrev eq "rel") then $locobj else $locsbj
    where ml:curie-is-valid($relv, $node)
    return
        if ($locsbj and $locobj)
        then
            <rdf:Description>
            {
                if (starts-with($effective-sbj, "_"))
                then attribute rdf:nodeID { $effective-sbj }
                else attribute rdf:about { $effective-sbj },

                element { ml:curie-to-qname($relv, $node) }
                {
                    (: attribute ns { $prefix }, :)
                    if (starts-with($effective-obj, "_"))
                    then attribute rdf:nodeID { $effective-obj }
                    else attribute rdf:resource { $effective-obj }
                }
            }
            </rdf:Description>
        else ()
};

(: this is to generate the 1 (or none) bnode reference that all @rel/@rev/@property completers share :)
declare function ml:relrev-hanging-bnode($node as node(), $val as xs:string, $relorrev, $base as xs:string) as element()* {
    for $relv in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
    let $locsbj := ml:subject($node, $base)
    let $locobj := ml:generate-bnode-id($node)
    let $effective-sbj := if ($relorrev eq "rel") then $locsbj else $locobj
    let $effective-obj := if ($relorrev eq "rel") then $locobj else $locsbj
    where ml:curie-is-valid($relv, $node)
    return
        if (ml:hanging-bnode($node))
        then <rdf:Description>
             {
                 if (starts-with($effective-sbj, "_"))
                 then attribute rdf:nodeID { $effective-sbj }
                 else attribute rdf:about { $effective-sbj },

                 element { ml:curie-to-qname($relv, $node) }
                 {

                     if (starts-with($effective-obj, "_"))
                     then attribute rdf:nodeID { $effective-obj }
                     else attribute rdf:resource { $effective-obj }
                 }
             }
             </rdf:Description>
        else ()
};

declare function ml:hanging-descendants($node as node()) as node()* {
    (: find all descendant nodes with hanging-triple-completing-via-new-node attributes... :)
    $node//*[@src or @about or @typeof or @href or @resource]
      [count(($node//* intersect ./ancestor::*)/(@src | @about | @typeof | @href | @resource)) eq 0]
      (: but exclude stuff we've already seen, and stuff more than one level deep
         (the deeper stuff is "yet to be seen") :)
};

declare function ml:hanging-bnode($node as node()) as node()* {
    (: find all descendant nodes with hanging-triple-completing-via-the-same-bnode attributes... :)
    $node//*[@rel or @rev or @property]
      [count(($node//* intersect ./ancestor::*)/(@rel | @rev | @property)) eq 0]
      (: but exclude stuff we've already seen, and stuff more than one level deep
         (the deeper stuff is "yet to be seen") :)
};

declare function ml:typeof($node as node(), $val as xs:string, $base as xs:string) as element()* {
    for $type in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
    let $locsbj := if ($node/@about)
                   then ml:safe-resolve-uri-or-curie($node/@about, $node, $base)
                   else if ($node/@src)
                        then ml:safe-resolve-uri($node/@src, $base)
                        else if (local-name($node) = ("head", "body"))
                             then $base
                             else if ($node/@resource and not($node/(@rel | @rev)))
                                  then ml:safe-resolve-uri-or-curie($node/@resource, $node, $base)
                                  else if ($node/@href and not($node/(@rel | @rev)))
                                       then ml:safe-resolve-uri($node/@href, $base)
                                       else ml:generate-bnode-id($node, "typeof")
    let $rsc := ml:expand-curie($type, $node)
    return
        if ($locsbj and $rsc)
        then
            <rdf:Description>
            {
                if (starts-with($locsbj, "_"))
                then attribute rdf:nodeID { $locsbj }
                else attribute rdf:about { $locsbj }
            }
                <rdf:type rdf:resource="{$rsc}"/>
            </rdf:Description>
        else ()
};

(: compensate for the lack of generate-id function (XSLT has it good here!) :)
declare function ml:generate-id($node as element()) as xs:string {
    concat("node", count($node/preceding::*), local-name($node), count($node/ancestor::*) )
};

declare function ml:generate-bnode-id($node as element()) as xs:string {
    ml:generate-bnode-id($node, "")
};

declare function ml:generate-bnode-id($node as element(), $extra as xs:string) as xs:string {
    concat("_:b", ml:generate-id($node), $extra)
};

(: curie parts: 1:prefix, 2:suffix, 3:uri  3 will be missing for invalid CURIEs :)
declare function ml:curie-parse($curie as xs:string, $context as element()) as xs:string* {
    let $prefix := substring-before($curie, ":")
    let $nsuri  := if ($prefix eq "") then $dfvocab else ml:namespace-uri-for-prefix($prefix, $context)
    let $suffix := if ($nsuri eq $dfvocab)
                   then if (starts-with($curie, ":"))
                        then substring-after($curie, ":")
                        else $curie
                   else substring-after($curie, ":")
    return ($prefix, $suffix, $nsuri)
};

declare function ml:curie-is-valid($curie as xs:string, $context as element()) as xs:boolean {
    let $parts := ml:curie-parse($curie, $context)
    return ($parts[1] eq "_") or ($parts[3] eq $dfvocab and $parts[2] = $htmlrels) or ($parts[1] ne "" and $parts[3] ne "")
};

declare function ml:expand-curie($curie as xs:string, $context as element()) as xs:string {
    let $parts := ml:curie-parse($curie, $context)
    return if ($parts[1] eq "_") then $curie else concat($parts[3], $parts[2])
};

(: RDF serialization requires a qname, which might not match exactly with a CURIE :)
declare function ml:curie-to-qname($curie as xs:string, $context as element()) as xs:QName? {
    let $expanded := ml:expand-curie($curie, $context)
    let $elem-part := replace ($expanded , '^.*[#|/]','')
    let $ns-part := replace($expanded, concat('^(.*)', $elem-part,'.*'),'$1')
    return fn:QName($ns-part, $elem-part)
};

(: there is some spec ambiguity on how fn:resolve-uri() should behave with a
zero-length input. We sidestep it by handling it explicitly here :)
declare function ml:safe-resolve-uri($rel as xs:string, $base as xs:string) as xs:string {
    if ($rel eq "")
    then $base
    else if (starts-with($rel, "#"))
         then concat($base, $rel)
         else resolve-uri($rel, $base)
};

declare function ml:safe-resolve-uri-or-curie($val as xs:string, $context as element(), $base as xs:string) as xs:string? {
	let $curie :=
		if (starts-with ($val, "[") and ends-with ($val, "]"))
        	then substring-after(substring-before($val, "]"), "[")
        	else $val
        return
	if (ml:curie-is-valid ($curie, $context))
        then ml:expand-curie ($curie, $context)
	else ml:safe-resolve-uri($val, $base)
(:

    if (starts-with($val, "[") and ends-with($val, "]"))
    then let $curie := substring-after(substring-before($val, "]"), "[")
         return if (ml:curie-is-valid($curie, $context))
                then ml:expand-curie($curie, $context)
                else ()
    else ml:safe-resolve-uri($val, $base)
 :)
};

(: return a deep copy of the node and all children :)
declare function ml:deep-copy($node as node()) as node() {

    typeswitch($node)
    case element() return
        element { node-name($node) }
        {
            $node/@*,
            for $child in $node/node()
            return
                if ($child instance of element())
                then ml:deep-copy($child)
                else $child
          }
    case attribute() return $node
    case text() return $node
    default return $node
};
