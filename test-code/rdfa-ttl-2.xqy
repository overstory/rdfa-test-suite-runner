xquery version "1.0-ml";

module namespace rdfa-ttl = "urn:overstory:rdf:rdf-ttl";

declare namespace xsi="http://www.w3.org/2001/XMLSchema-instance";
declare namespace xhtml = "http://www.w3.org/1999/xhtml";
declare namespace rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#";

declare variable $dfvocab := "http://www.w3.org/1999/xhtml/vocab#";
declare variable $rdf-XMLLiteral := "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral";
declare variable $default-base-uri := $dfvocab;

declare variable $htmlrels :=
	( "alternate", "appendix", "bookmark", "cite", "chapter", "contents",
	"copyright", "glossary", "help", "icon", "index", "last", "license",
	"meta", "next", "p3pv1", "prev", "role", "section", "start",
	"stylesheet", "subsection", "up" );

declare variable $vocabulary-terms := ( "describedby", "license", "role" );

declare private variable $minimum-default-prefixes := map:map(
	<map:map xmlns:map="http://marklogic.com/xdmp/map" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
		<map:entry key="owl"><map:value xsi:type="xs:string">http://www.w3.org/2002/07/owl#</map:value></map:entry>
		<map:entry key="rdf"><map:value xsi:type="xs:string">http://www.w3.org/1999/02/22-rdf-syntax-ns#</map:value></map:entry>
		<map:entry key="rdfa"><map:value xsi:type="xs:string">http://www.w3.org/ns/rdfa#</map:value></map:entry>
		<map:entry key="rdfs"><map:value xsi:type="xs:string">http://www.w3.org/2000/01/rdf-schema#</map:value></map:entry>
		<map:entry key="dc"><map:value xsi:type="xs:string">http://purl.org/dc/terms/</map:value></map:entry>
		<map:entry key="dcterms"><map:value xsi:type="xs:string">http://purl.org/dc/terms/</map:value></map:entry>
		<map:entry key="dc11"><map:value xsi:type="xs:string">http://purl.org/dc/elements/1.1/</map:value></map:entry>
		<map:entry key="foaf"><map:value xsi:type="xs:string">http://xmlns.com/foaf/0.1/</map:value></map:entry>
	</map:map>
);

declare variable $additional-default-prefixes := map:map (
	<map:map xmlns:map="http://marklogic.com/xdmp/map" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
		<map:entry key="grddl"><map:value xsi:type="xs:string">http://www.w3.org/2003/g/data-view#</map:value></map:entry>
		<map:entry key="ma"><map:value xsi:type="xs:string">http://www.w3.org/ns/ma-ont#</map:value></map:entry>
		<map:entry key="prov"><map:value xsi:type="xs:string">http://www.w3.org/ns/prov#</map:value></map:entry>
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

(: Preloaded with default vocabulary prefixes.  See: http://www.w3.org/2011/rdfa-context/rdfa-1.1 :)
declare variable $default-prefixes-map := $minimum-default-prefixes + $additional-default-prefixes;


(:
declare private variable $referenced-prefixes := map:map();
 :)

declare private variable $default-bnode-ref := "_:";
declare private variable $bnode-map := map:map();

(: ---------------------------------------------------------------------- :)

(: The public entry point to the module :)

declare function rdfa-to-ttl (
        $doc as node()
) as xs:string?
{
	rdfa-to-ttl ($doc, ())
};

declare function rdfa-to-ttl (
        $doc-node as node(),
        $uri as xs:string?
) as xs:string?
{
	let $root := if ($doc-node instance of document-node()) then $doc-node/* else $doc-node
	let $base-uri := ($root/xhtml:head/xhtml:base/@href, $root/head/base/@href, $root/@xml:base, $uri, $default-base-uri)[1]
	let $ec := initial-eval-context ($base-uri)
	let $_ :=
		if ($root instance of element())
		then evaluate-node ($ec, $root)
		else add-triple ($ec, wrap-uri ($base-uri), "rdf:type", "rdfa:Error")

	return render-ttl ($ec)
};

(: ----------------------------------------------------------------- :)

declare variable $EC-BASE := "base";
declare variable $EC-ROOT-BASE-URI := "root-base-uri";
declare variable $EC-PARENT-SUBJECT := "parent-subject";
declare variable $EC-PARENT-OBJECT := "parent-object";
declare variable $EC-SUBJECT := "subject";
declare variable $EC-OBJECT := "object";
declare variable $EC-INCOMPLETE_TRIPLES := "incomplete-triples";
declare variable $EC-LIST-MAPPINGS := "list-mappings";
declare variable $EC-IRI-MAPPINGS := "iri-mappings";
declare variable $EC-TERM-MAPPINGS := "term-mappings";
declare variable $EC-REFERENCED-PREFIXES := "referenced-prefixes";
declare variable $EC-DEFINED-PREFIXES := "defined-prefixes";
declare variable $EC-TRIPLES := "triples";
declare variable $EC-PARENT-CONTEXT := "parent-context";
declare variable $EC-VOCABULARY := "vocabulary";
declare variable $EC-DEFAULT-VOCABULARY := "default-vocabulary";
declare variable $EC-LANGUAGE := "language";
declare variable $EC-TYPED-RESOURCE := "typed-resource";
declare variable $EC-CURRENT-OBJECT-RESOURCE := "current-object-resource";

(:
http://www.w3.org/TR/rdfa-syntax/#s_sequence
 At the beginning of processing, an initial evaluation context is created, as follows:

    the base is set to the IRI of the document (or another value specified in a language specific manner such as the HTML base element);
    the parent subject is set to the base value;
    the parent object is set to null;
    the list of incomplete triples is empty;
    the list mapping is empty;
    the language is set to null.
    the list of IRI mappings is empty (or a list defined in the initial context of the Host Language).
    the term mappings is set to null (or a list defined in the initial context of the Host Language).
    the default vocabulary is set to null (or an IRI defined in the initial context of the Host Language).

:)
declare private function initial-eval-context (
	$base as xs:string
) as map:map
{
	let $ec := empty-eval-context()
	let $_ := map:put ($ec, $EC-BASE, $base)
	let $_ := map:put ($ec, $EC-ROOT-BASE-URI, $base)
	let $_ := map:put ($ec, $EC-PARENT-SUBJECT, $base)

	return $ec
};

declare private function child-eval-context (
	$parent-ec as map:map
) as map:map
{
	let $ec := $parent-ec + map:map()
	let $_ := map:put ($ec, $EC-PARENT-CONTEXT, $parent-ec)
	let $_ := map:put ($ec, $EC-INCOMPLETE_TRIPLES, map-copy (map:get ($parent-ec, $EC-INCOMPLETE_TRIPLES)))
	let $_ := map:put ($ec, $EC-LIST-MAPPINGS, map-copy (map:get ($parent-ec, $EC-LIST-MAPPINGS)))
	let $_ := map:put ($ec, $EC-TERM-MAPPINGS, map-copy (map:get ($parent-ec, $EC-TERM-MAPPINGS)))
	let $_ := map:put ($ec, $EC-IRI-MAPPINGS, map-copy (map:get ($parent-ec, $EC-IRI-MAPPINGS)))
	let $_ := map:put ($ec, $EC-DEFINED-PREFIXES, map-copy (map:get ($parent-ec, $EC-DEFINED-PREFIXES)))
	let $_ := map:put ($ec, $EC-PARENT-SUBJECT, map:get ($parent-ec, $EC-SUBJECT))
	let $_ := map:put ($ec, $EC-PARENT-OBJECT, map:get ($parent-ec, $EC-OBJECT))
	(: referenced prefixes and generated triples maps are not copied, they are shared from parent to child :)
	return $ec
};

declare private function empty-eval-context (
) as map:map
{
	let $ec := map:map()
	let $_ := map:put ($ec, $EC-INCOMPLETE_TRIPLES, map:map())
	let $_ := map:put ($ec, $EC-LIST-MAPPINGS, map:map())
	let $_ := map:put ($ec, $EC-IRI-MAPPINGS, map:map())
	let $_ := map:put ($ec, $EC-TERM-MAPPINGS, map:map())
	let $_ := map:put ($ec, $EC-DEFINED-PREFIXES, map:map() + $default-prefixes-map)
	let $_ := map:put ($ec, $EC-REFERENCED-PREFIXES, map:map() + $minimum-default-prefixes)
	let $_ := map:put ($ec, $EC-TRIPLES, map:map())

	return $ec
};

declare private function map-copy (
	$map as map:map?
) as map:map
{
	if (fn:exists ($map))
	then map:map() + $map
	else map:map()
};

(: ----------------------------------------------------------------- :)

declare private function add-triple (
	$ec as map:map,
	$subject as xs:string,
	$predicate as xs:string,
	$object as xs:string
) as empty-sequence()
{
	let $triples := map:get ($ec, $EC-TRIPLES)
	let $_ := map:put ($triples, fn:string (xdmp:random()),
		<triple>
			<subject>{ $subject }</subject>
			<predicate>{ $predicate }</predicate>
			<object>{ $object }</object>
		</triple>)
	return ()
};

declare private function add-incomplete-triple (
	$ec as map:map,
	$subject as xs:string,
	$predicate as xs:string,
	$object as xs:string
) as empty-sequence()
{
	let $triples := map:get ($ec, $EC-INCOMPLETE_TRIPLES)
	let $_ := map:put ($triples, fn:string (xdmp:random()),
		<triple>
			<subject>{ $subject }</subject>
			<predicate>{ $predicate }</predicate>
			<object>{ $object }</object>
		</triple>)
	return ()
};

(: FixMe: to be deleted :)
declare private function gen-error-triple (
	$subject as xs:string,
	$object as xs:string
) as element(triple)
{

	<triple>
		<subject>{ $subject }</subject>
		<predicate>{ "rdf:type" }</predicate>
		<object>{ $object }</object>
	</triple>
};

(: ----------------------------------------------------------------- :)

declare private function render-ttl (
	$ec as map:map
) as xs:string?
{
	(:
	 emit prefixes
	 for distinct subjects
	 	output prefixed subject
	 		output predicate and object
	 :)

	fn:string-join (
		(
			emit-prefixes ($ec), "",
			emit-triples ($ec)
		), "&#x0a;"
	)
};

declare private function emit-prefixes (
	$ec as map:map
) as xs:string*
{
	(
 		let $prefix-map := map:get ($ec, $EC-REFERENCED-PREFIXES)
		for $prefix in map:keys ($prefix-map)
		let $ns-uri := map:get ($prefix-map, $prefix)

		order by $prefix
		return fn:concat ("@prefix ", $prefix, ": ", wrap-uri ($ns-uri), " .")
	)
};

declare private function emit-triples (
	$ec as map:map
) as xs:string*
{
	let $triples := map:get ($ec, $EC-TRIPLES)
	for $triple-key in map:keys ($triples)
	return emit-triple ($ec, map:get ($triples, $triple-key))
};

declare private function emit-triple (
	$ec as map:map,
	$triple as element(triple)
) as xs:string
{
	fn:concat (
		emit-tuple ($ec, $triple/subject),
		"&#x0a;	", emit-tuple ($ec, $triple/predicate), " ",
		emit-tuple ($ec, $triple/object), " ."
	)
};

declare private function emit-tuple (
	$ec as map:map,
	$object as element()
) as xs:string
{
	fn:concat (
		if ($object/@rdf:parseType = 'Literal')
		then quote-xml ($object)
		else compact-uri ($ec, $object),

		if ($object/(@datatype|@rdf:datatype))
		then fn:concat ("^^", ($object/@datatype, $object/@rdf:datatype)[1])
		else
			if ($object/@xml:lang)
			then fn:concat ("@", $object/@xml:lang/fn:string())
			else ()

	)
};

declare private function quote-xml (
	$node as element()
) as xs:string
{
	fn:concat ('"""', xdmp:quote ($node/node()), '"""^^rdf:XMLLiteral')
};

declare private function compact-uri (
	$ec as map:map,
	$uri as xs:string
) as xs:string
{
	if (fn:not (fn:starts-with ($uri, "<") and fn:ends-with ($uri, ">")))
	then $uri
	else
		let $referenced-prefixes := map:get ($ec, $EC-REFERENCED-PREFIXES)
		let $bare-uri := fn:substring-after (fn:substring-before ($uri, ">"), "<")
		let $prefix := find-prefix-for ($referenced-prefixes, $bare-uri)
		let $ns-uri := map:get ($referenced-prefixes, $prefix)
		let $local-part := fn:substring-after ($bare-uri, $ns-uri)

		return
		if ($prefix and $local-part and fn:not (fn:matches ($local-part, "[^\w\d\-_.]")))
		then fn:concat ($prefix, ":", $local-part)
		else $uri
};

(: FixMe: Make an inverse map rather than doing it this way :)
declare private function find-prefix-for (
	$referenced-prefixes as map:map,
	$uri as xs:string
) as xs:string?
{
	(
		for $prefix in map:keys ($referenced-prefixes)
		let $ns-uri := (map:get ($referenced-prefixes, $prefix)	)[1]
		return
		if (fn:starts-with ($uri, $ns-uri))
		then $prefix
		else ()
	)[1]
};

(: ----------------------------------------------------------------- :)

(:
 The processing rules are:

    First, the local values are initialized, as follows:
        the skip element flag is set to 'false';
        new subject is set to null;
        current object resource is set to null;
        typed resource is set to null;
        the local list of IRI mappings is set to the list of IRI mappings from the evaluation context;
        the local list of incomplete triples is set to null;
        the list mapping is set to (a reference of) the list mapping from the evaluation context;
        the current language value is set to the language value from the evaluation context.
        the local term mappings is set to the term mappings from the evaluation context.
        the local default vocabulary is set to the default vocabulary from the evaluation context.
    Note that some of the local variables are temporary containers for values that will be passed to descendant elements via an evaluation context. In some cases the containers will have the same name, so to make it clear which is being acted upon in the following steps, the local version of an item will generally be referred to as such.
    Note that the local term mappings is always reset to a global value, provided by the initial context. Future versions of this specification may introduce a mechanism whereby the local term mappings can be set dynamically, in which case the local term mappings would inherit from the parent's values.

Next the current element is examined for any change to the default vocabulary via @vocab. If @vocab is present and contains a value, the local default vocabulary is updated according to the section on CURIE and IRI Processing. If the value is empty, then the local default vocabulary MUST be reset to the Host Language defined default (if any).
The value of @vocab is used to generate a triple as follows:
subject base
predicate http://www.w3.org/ns/rdfa#usesVocabulary
object value from @vocab
:)

declare private function evaluate-node (
	$ec as map:map,
	$node as element()
) as element(triple)*
{
	let $_ := set-base-uri ($ec, $node)
	let $_ := set-vocabulary ($ec, $node)
	let $_ := set-language ($ec, $node)
	let $_ := add-prefixes ($ec, $node/@prefix)
	let $_ :=
		if (fn:empty ($node/(@rel|@rev)))
		then step5 ($ec, $node)
		else step6 ($ec, $node)
	let $_ := step7 ($ec, $node)
	let $_ := step8 ($ec, $node)
	let $_ :=
		if (fn:exists (map:get ($ec, $EC-CURRENT-OBJECT-RESOURCE)))
		then step9 ($ec, $node)
		else step10 ($ec, $node)

	let $_ := (
		(: Function mapping is in play here, it prevents functions being called when the attribute is not present :)
(:
		gen-property ($node/@property/fn:normalize-space(.), $node, $parent-node, $base-uri, $prefix-map),
		gen-rel ($node/@rel/fn:normalize-space(.), $node, $parent-node, $base-uri, $prefix-map),
		gen-rev ($node/@rev/fn:normalize-space(.), $node, $parent-node, $base-uri, $prefix-map),
		gen-typeof ($node/@typeof/fn:normalize-space(.), $node, $parent-node, $base-uri, $prefix-map)
 :)
	)
	return evaluate-node (child-eval-context ($ec), $node/*)    (: function mapping here :)
};

declare private function set-base-uri (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $root-base-uri := map:get ($ec, $EC-ROOT-BASE-URI)
	let $base-uri :=
		if ($node/ancestor::*[@xml:base = ''])
		then ($root-base-uri)
		else if ($node/self::*[@xml:base]/string(@xml:base) or $node/ancestor::*[@xml:base]/string(@xml:base))
		then ($node/self::*[@xml:base]/string(@xml:base), $node/ancestor::*[@xml:base]/string(@xml:base))[1]
		else ($root-base-uri)
	return map:put ($ec, $EC-BASE, $base-uri)
};

declare private function set-vocabulary (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $base := map:get ($ec, $EC-BASE)
	let $vocab := fn:data ($node/@vocab)
	let $object := resolve-uri-or-curie ($ec, $vocab, $node)
	let $_ :=
		if (fn:empty ($vocab))
		then ()
		else
			if ($vocab = "")
			then map:put ($ec, $EC-VOCABULARY, map:get ($ec, $EC-DEFAULT-VOCABULARY))
			else
				if ($object)
				then (
					map:put ($ec, $EC-VOCABULARY, $vocab),
					add-triple ($ec, wrap-uri ($base), "rdfa:usesVocabulary", wrap-uri ($vocab))
				) else (
					map:put ($ec, $EC-VOCABULARY, map:get ($ec, $EC-DEFAULT-VOCABULARY)),
					add-triple ($ec, wrap-uri ($base), "rdfa:UnresolvedCURIE", "{$vocab}")
				)
	return ()
};

declare private function set-language (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $lang := ($node/@xml:lang/fn:string(), $node/@lang/fn:string())[1]

	return if ($lang) then map:put ($ec, $EC-LANGUAGE, $lang) else ()
};

(:
5: If the current element contains no @rel or @rev attribute, then the next step is to establish a value for new subject. This step has two possible alternatives.

    If the current element contains the @property attribute, but does not contain either the @content or @datatype attributes, then
    new subject is set to the resource obtained from the first match from the following rule:
        by using the resource from @about, if present, obtained according to the section on CURIE and IRI Processing;
        otherwise, if the element is the root element of the document, then act as if there is an empty @about present, and process it according to the rule for @about, above;
        otherwise, if parent object is present, new subject is set to the value of parent object.

    If @typeof is present then typed resource is set to the resource obtained from the first match from the following rules:
        by using the resource from @about, if present, obtained according to the section on CURIE and IRI Processing;
        otherwise, if the element is the root element of the document, then act as if there is an empty @about present and process it according to the previous rule;
        otherwise,
            by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
            otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
            otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing;
            otherwise, the value of typed resource is set to a newly created bnode.
            The value of the current object resource is then set to the value of typed resource.
    otherwise:
        If the element contains an @about, @href, @src, or @resource attribute, new subject is set to the resource obtained as follows:
            by using the resource from @about, if present, obtained according to the section on CURIE and IRI Processing;
            otherwise, by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
            otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
            otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing.
        otherwise, if no resource is provided by a resource attribute, then the first match from the following rules will apply:
            if the element is the root element of the document, then act as if there is an empty @about present, and process it according to the rule for @about, above;
            otherwise, if @typeof is present, then new subject is set to be a newly created bnode;
            otherwise, if parent object is present, new subject is set to the value of parent object. Additionally, if @property is not present then the skip element flag is set to 'true'.

        Finally, if @typeof is present, set the typed resource to the value of new subject.
:)
declare private function step5 (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	if (fn:exists ($node/@property) and fn:empty ($node/(@content|@datatype)))
	then
		let $about-subj :=
			(
				resolve-uri-or-curie ($ec, tok ($node/@about), $node),
				if ($node is $node/root()) then resolve-uri-or-curie ($ec, "", $node) else ()
			)[1]
		let $new-subject :=
			(
				$about-subj,
				map:get ($ec, $EC-PARENT-OBJECT)
			)[1]
		let $_ := map:put ($ec, $EC-SUBJECT, $new-subject)
		let $typed-resource :=
			if (fn:exists ($node/@typeof))
			then (
				$about-subj,
				resolve-uri-or-curie ($ec, tok (($node/(@resource | @href | @src))[1]), $node),
				gen-blank-node-uri ($node)
			)[1] else ()
		let $_ := map:put ($ec, $EC-TYPED-RESOURCE, $new-subject)
		return ()
	else
		let $new-subject :=
			if (fn:exists ($node/(@about | @href | @src | @resource)))
			then resolve-uri-or-curie ($ec, tok (($node/(@about | @resource | @href | @src))[1]), $node)
			else (
				resolve-uri-or-curie ($ec, tok ($node/@resource), $node),
				if ($node is $node/root()) then resolve-uri-or-curie ($ec, "", $node) else (),
				if (fn:exists ($node/@typeof)) then gen-blank-node-uri ($node) else (),
				map:get ($ec, $EC-PARENT-OBJECT)
				(: ToDo: Need to handle skip element condition if no @property :)
			)[1]
		let $_ := if (fn:exists ($node/@typeof)) then map:put ($ec, $EC-TYPED-RESOURCE, $new-subject) else ()
		let $_ := map:put ($ec, $EC-SUBJECT, $new-subject)
		return ()
};

(:
6: If the current element does contain a @rel or @rev attribute, then the next step is to establish both a value for new subject and a value for current object resource:
new subject is set to the resource obtained from the first match from the following rules:

    by using the resource from @about, if present, obtained according to the section on CURIE and IRI Processing;

if the @typeof attribute is present, set typed resource to new subject.

If no resource is provided then the first match from the following rules will apply:

    if the element is the root element of the document then act as if there is an empty @about present, and process it according to the rule for @about, above;
    otherwise, if parent object is present, new subject is set to that.

Then the current object resource is set to the resource obtained from the first match from the following rules:

    by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
    otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
    otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing;
    otherwise, if @typeof is present and @about is not, use a newly created bnode.

If @typeof is present and @about is not, set typed resource to current object resource.

Note that final value of the current object resource will either be null (from initialization) or a full IRI or bnode.
:)
declare private function step6 (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $new-subject := resolve-uri-or-curie ($ec, tok ($node/@about), $node)
	let $new-subject :=
		if (fn:exists ($new-subject))
		then
			if (fn:exists ($node/@typeof))
			then map:put ($ec, $EC-TYPED-RESOURCE, $new-subject)
			else ()
		else
			if ($node is $node/root())
			then resolve-uri-or-curie ($ec, "", $node)
			else map:get ($ec, $EC-PARENT-OBJECT)
	let $current-object-resource := resolve-uri-or-curie ($ec, ($node/(@resource | @href | @src))[1], $node)
	let $current-object-resource := if (fn:exists ($node/@typeof) and fn:empty ($node/@about)) then gen-blank-node-uri ($node) else ()
	let $_ := map:put ($ec, $EC-SUBJECT, $new-subject)
	let $_ := map:put ($ec, $EC-CURRENT-OBJECT-RESOURCE, $current-object-resource)
	let $_ := if (fn:exists ($node/@typeof) and fn:empty ($node/@about)) then map:put ($ec, $EC-TYPED-RESOURCE, $current-object-resource) else ()
	return ()
};

(:
If in any of the previous steps a typed resource was set to a non-null value, it is now used to provide a subject for type values;
One or more 'types' for the typed resource can be set by using @typeof. If present, the attribute may contain one or more IRIs, obtained according to the section on CURIE and IRI Processing, each of which is used to generate a triple as follows:

subject
    typed resource
predicate
    http://www.w3.org/1999/02/22-rdf-syntax-ns#type
object
    current full IRI of 'type' from typed resource

:)
declare private function step7 (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $typed-resource := map:get ($ec, $EC-TYPED-RESOURCE)
	let $_ :=
		if (fn:exists ($typed-resource))
		then add-triple ($ec, $typed-resource, "rdf:type", resolve-uri-or-curie ($ec, tok ($node/@typeof), $node))
		else ()
	return ()
};

(:
 If in any of the previous steps a new subject was set to a non-null value different from the parent object;
The list mapping taken from the evaluation context is set to a new, empty mapping.
:)
declare private function step8 (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $new-subject := map:get ($ec, $EC-SUBJECT)
	let $parent-object := map:get ($ec, $EC-PARENT-OBJECT)
	let $_ :=
		if (fn:exists ($new-subject) and ($new-subject ne $parent-object))
		then map:put ($ec, $EC-LIST-MAPPINGS, map:map())
		else ()
	return ()
};

(:
If in any of the previous steps a current object resource was set to a non-null value, it is now used to generate triples and add entries to the local list mapping:
If the element contains both the @inlist and the @rel attributes the @rel may contain one or more resources, obtained according to the section on CURIE and IRI Processing each of which is used to add an entry to the list mapping as follows:

    if the local list mapping does not contain a list associated with the IRI, instantiate a new list and add to local list mappings
    add the current object resource to the list associated with the resource in the local list mapping

Predicates for the current object resource can be set by using one or both of the @rel and the @rev attributes but, in case of the @rel attribute, only if the @inlist is not present:

    If present, @rel may contain one or more resources, obtained according to the section on CURIE and IRI Processing each of which is used to generate a triple as follows:

    subject
        new subject
    predicate
        full IRI
    object
        current object resource

    If present, @rev may contain one or more resources, obtained according to the section on CURIE and IRI Processing each of which is used to generate a triple as follows:

    subject
        current object resource
    predicate
        full IRI
    object
        new subject

:)
declare private function step9 (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $new-subject := map:get ($ec, $EC-SUBJECT)
	let $current-object-resource := map:get ($ec, $EC-CURRENT-OBJECT-RESOURCE)
	let $has-inlist := fn:exists ($node/@inlist)
	let $rel := $node/@rel/fn:string()
	let $rev := $node/@rev/fn:string()
	let $_ :=
		if ($has-inlist and $rel)
		then add-to-list ($ec, tok ($rel), $current-object-resource)
		else add-triple ($ec, $new-subject, resolve-uri-or-curie ($ec, tok ($rel), $node), $current-object-resource)
	let $_ := add-triple ($ec, $current-object-resource, resolve-uri-or-curie ($ec, tok ($rev), $node), $new-subject)

	return ()
};

(:
If however current object resource was set to null, but there are predicates present, then they must be stored as incomplete triples, pending the discovery of a subject that can be used as the object. Also, current object resource should be set to a newly created bnode (so that the incomplete triples have a subject to connect to if they are ultimately turned into triples);
Predicates for incomplete triples can be set by using one or both of the @rel and @rev attributes:

    If present, @rel must contain one or more resources, obtained according to the section on CURIE and IRI Processing each of which is added to the local list of incomplete triples as follows:
        If the element contains the @inlist attribute, then
            if the local list mapping does not contain a list associated with the IRI, instantiate a new list and add to local list mappings.
            Add:

            list
                list from local list mapping for this IRI
            direction
                none

        Otherwise add:

            predicate
                full IRI
            direction
                forward

    If present, @rev must contain one or more resources, obtained according to the section on CURIE and IRI Processing, each of which is added to the local list of incomplete triples as follows:

    predicate
        full IRI
    direction
        reverse

:)
declare private function step10 (
	$ec as map:map,
	$node as element()
) as empty-sequence()
{
	let $subj := gen-blank-node-uri ($node)
	let $_ := map:put ($ec, $EC-CURRENT-OBJECT-RESOURCE, $subj)
	let $has-inlist := fn:exists ($node/@inlist)
	let $rel := $node/@rel/fn:string()
	let $rev := $node/@rev/fn:string()
	let $_ :=
		if ($has-inlist and $rel)
		then add-to-list ($ec, tok ($rel), ">>NONE<<")
		else add-incomplete-triple ($ec, $subj, resolve-uri-or-curie ($ec, tok ($rel), $node), ">>FORWARD<<")
	let $_ := add-incomplete-triple ($ec, ">>REVERSE<<", resolve-uri-or-curie ($ec, tok ($rev), $node), $subj)

	return ()
};

(: ----------------------------------------------------------------- :)

declare private function tok (
	$value as xs:string
) as xs:string*
{
	tokenize ($value, "\s+")
};

declare private function add-to-list (
	$ec as map:map,
	$iri as xs:string,
	$resource as xs:string
) as empty-sequence()
{
	let $mappings := map:get ($ec, $EC-LIST-MAPPINGS)
	let $list := map:get ($mappings, $iri)
	return map:put ($mappings, $iri, ($list, $resource))
};

(: ----------------------------------------------------------------- :)

(:
declare private function subject (
	$node as element(),
	$parent-node as element()?,
	$base-uri as xs:string,
	$prefix-map as map:map
) as xs:string?
{
	if (has-about ($node/@about))
	then resolve-uri-or-curie ($node/@about, $node, $base-uri, $prefix-map)
	else
		if ($node/@href and ($node/@property) and (($node/@content or $node/@datatype)))
		then resolve-uri-or-curie ($node/@href, $node, $base-uri, $prefix-map)
		else
			if ($node/@src and ($node/@property) and (($node/@content or $node/@datatype)))
			then resolve-uri-or-curie ($node/@src, $node, $base-uri, $prefix-map)
			else
			    if ( $node/@typeof and not($node/parent::*) and not(has-about($node/@about)) )
			    then wrap-uri ($base-uri)
				else if ($node/@typeof and not(has-about($node/ancestor::*)))
				then gen-blank-node-uri($node)
				else
					if (fn:exists ($parent-node))
					then subject-parent ($parent-node, $base-uri, $prefix-map)
					else wrap-uri ($base-uri)
};

declare private function subject-parent (
    $node as element(),
	$base-uri as xs:string,
	$prefix-map as map:map
) as xs:string?
{
    if (has-resource ($node/@resource))
    then resolve-uri-or-curie ($node/@resource, $node, $base-uri, $prefix-map)
        else if ( $node/@rev or $node/@rel)
        then gen-blank-node-uri ($node)
            else if (has-about ($node/@about))
            then resolve-uri-or-curie ($node/@about, $node, $base-uri, $prefix-map)
                else if ( $node/@typeof and not($node/parent::*) and not(has-about($node/@about)) )
			    then wrap-uri ($base-uri)
                    else if ($node/@typeof)
                    then gen-blank-node-uri ($node)
                        else if ($node/parent::*)
                        then subject-parent($node/parent::*, $base-uri, $prefix-map)
                             else wrap-uri ($base-uri)

};

declare private function object (
	$node as element(),
	$is-xml as xs:boolean,
	$base-uri as xs:string,
	$prefix-map as map:map
) as item()*
{
	if ($is-xml)
	then ( $node/node() )
	else if (has-resource ($node/@resource) and fn:not ($node/@rel) and fn:not ($node/@rev))
	then resolve-uri-or-curie ($node/@resource, $node, $base-uri, $prefix-map)
	else if (has-href ($node/@href) and fn:not ($node/@content) and fn:not ($node/@datatype))
	then resolve-uri-or-curie ($node/@href, $node, $base-uri, $prefix-map)
	else if (has-src ($node/@src) and fn:not ($node/@content) and fn:not ($node/@datatype))
	then resolve-uri-or-curie ($node/@src, $node, $base-uri, $prefix-map)
	 :)
(: unless @about is '[]' the new object is set if @typeof is present :)(:

	else if ($node/@typeof and fn:not (has-about ($node/@about)) and fn:not ($node/@about='[]') and fn:not($node/@about=''))
	then gen-blank-node-uri ($node)

	else quoted-string (($node/@content, fn:string ($node), "")[1])
};
 :)

declare private function quoted-string (
	$s as xs:string
) as xs:string
{
	if (fn:matches ($s, "[^ -~'""]"))
	then fn:concat ('"""', $s, '"""')
	else fn:concat ('"', $s, '"')
};

(: ----------------------------------------------------------------- :)

(:
declare private function gen-property (
	$props as xs:string,
	$node as element(),
	$parent-node as element()?,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element(triple)*
{
	for $prop in tokenize ($props, "\s+")
	let $vocab := ancestor-vocab ($node)
	let $predicate :=
	   if ( fn:not ( fn:contains($prop, ':') ) and ( $vocab != '' ) )
	   then ( wrap-uri ( fn:concat ($vocab, $prop) ) )
	   else ( resolve-uri-or-curie ($prop, $node, $base-uri, $prefix-map) )
	let $is-xml as xs:boolean := is-xml ($node, $base-uri, $prefix-map)
	let $datatype := if ($is-xml) then () else effective-datatype ($node, $prefix-map)
	let $lang := effective-lang ($node, $parent-node)	 :)
(: ToDo: use ancestor-or-self:: instead? :)(:

	let $parse-type := if ($is-xml) then "Literal" else ()
	let $subject := subject ($node, $parent-node, $base-uri, $prefix-map)
	let $object := object ($node, $is-xml, $base-uri, $prefix-map)
	return
	if (fn:starts-with ($predicate, "_:"))
	then ()
	 :)
(: subjects without curies should not be processed if @vocab=''
       <root>
        <head>
          <title>Test 0318</title>
        </head>
        <body>
          <div vocab="http://xmlns.com/foaf/0.1/">
            <div about="#me">
              <p property="name">Ivan Herman</p>
              <meta vocab="" property="foaf:prop" content="value"/>
            </div>
          </div>
        </body>
      </root>

      foaf:prop should display

      <root>
        <head>
          <title>Test 0318</title>
        </head>
        <body>
          <div vocab="http://xmlns.com/foaf/0.1/">
            <div about="#me">
              <p property="name">Ivan Herman</p>
              <meta vocab="" property="prop" content="value"/>
            </div>
          </div>
        </body>
      </root>

      prop should be ignored
    :)(:

	else if ( $vocab = '' and not(contains($prop, ':')) )
	then ()
	else
	if ($predicate)
	then
		<triple>
			<subject>{ $subject }</subject>
			<predicate>{ $predicate }</predicate>
			<object>{
				$lang,
				if ($datatype) then attribute rdf:datatype { $datatype } else (),
				if ($parse-type) then attribute rdf:parseType { $parse-type } else (),
				$object
			}</object>
		</triple>
	else gen-error-triple (wrap-uri ($base-uri), "rdfa:UnresolvedTerm")
};

declare private function gen-rel (
	$val as xs:string,
	$node as element(),
	$parent-node as element()?,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element(triple)*
{
	if (has-resource ($node/@resource) or has-href ($node/@href) or has-src ($node/@src) )
	then gen-relrev-immediate ($node, $parent-node, $val, "rel", $base-uri, $prefix-map)
	else (
		relrev-hanging ($node, $parent-node, $val, 'rel', $base-uri, $prefix-map),
		relrev-hanging-bnode ($node, $val, 'rel', $base-uri, $prefix-map)
	)
};

declare private function gen-rev (
	$val as xs:string,
	$node as element(),
	$parent-node as element()?,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element(triple)*
{
	if (has-resource ($node/@resource) or has-href ($node/@href) or has-src ($node/@src) )
	then gen-relrev-immediate ($node, $parent-node, $val, "rev", $base-uri, $prefix-map)
	else
	(
	   relrev-hanging ($node, $parent-node, $val, 'rev', $base-uri, $prefix-map),
	   relrev-hanging-bnode ($node, $val, 'rev', $base-uri, $prefix-map)
	)
};
 :)

(:
declare private function gen-vocab (
    $vocab as xs:string,
    $node as element(),
    $base-uri as xs:string,
    $prefix-map as map:map
) as element(triple)*
{
   if ( fn:not($vocab eq '') )
   then
        <triple>
            <subject>{ wrap-uri ( $base-uri ) }</subject>
            <predicate>{string('rdfa:usesVocabulary')}</predicate>
            <object>{ wrap-uri ( $vocab ) }</object>
        </triple>
   else ()
};
:)

(:
declare function gen-relrev-immediate (
	$node as node(),
	$parent-node as element()?,
	$val as xs:string,
	$relorrev as xs:string,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element(triple)*
{
	for $relv in tokenize($val, "\s+")
	let $prefix := fn:substring-before ($relv, ":")
	let $locobj :=
		if (has-resource ($node/@resource))
		then resolve-uri-or-curie ($node/@resource, $node, $base-uri, $prefix-map)
		else if (has-resource ($node/@href))
		then resolve-uri-or-curie ($node/@href, $node, $base-uri, $prefix-map)
		else if (has-resource ($node/@src))
		then resolve-uri-or-curie ($node/@src, $node, $base-uri, $prefix-map)
		   else resolve-uri ($node/@href, $base-uri)
	let $locsbj := subject ($node, $parent-node, $base-uri, $prefix-map)
	let $effective-sbj := if ($relorrev eq "rel") then $locsbj else $locobj
	let $effective-obj := if ($relorrev eq "rel") then $locobj else $locsbj

	return
       	<triple>
       		<subject>{ $effective-sbj }</subject>
       		<predicate>{ resolve-uri-or-curie ($relv, $node, $base-uri, $prefix-map) }</predicate>
       		<object>{ $effective-obj }</object>
       	</triple>
};

declare function hanging-descendants (
	$node as node()
) as node()*
{
     :)
(: find all descendant nodes with hanging-triple-completing-via-new-node attributes... :)(:

    $node//*[@src or has-about (@about) or @typeof or @href or has-resource (@resource)][count(($node//* intersect ./ancestor::*)/(@src | @about | @typeof | @href | @resource)) eq 0]
       :)
(: but exclude stuff we've already seen, and stuff more than one level deep
         (the deeper stuff is "yet to be seen") :)(:

};

declare function hanging-bnode (
	$node as node()
) as node()*
{
	 :)
(: find all descendant nodes with hanging-triple-completing-via-the-same-bnode attributes... :)(:

	$node//*[@rel or @rev or @property][count(($node//* intersect ./ancestor::*)/(@rel | @rev | @property)) eq 0]
	 :)
(: but exclude stuff we've already seen, and stuff more than one level deep
	 (the deeper stuff is "yet to be seen") :)(:

};

declare function relrev-hanging (
	$node as node(),
	$parent-node as element()?,
	$val as xs:string,
	$relorrev as xs:string,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element()*
{

	for $rel-rev in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
	let $prefix := substring-before($rel-rev, ":")
	let $local-subject := subject($node, (), $base-uri, $prefix-map)
	for $hang-desc in hanging-descendants($node)
	let $local-object :=
            if ($hang-desc/@about[fn:starts-with(., '[_:')])
            then ()
            else if (has-about ($hang-desc/@about))
            then resolve-uri-or-curie ($hang-desc/@about, $hang-desc, $base-uri, $prefix-map)
            else if ($hang-desc/@src)
            then resolve-uri-or-curie ($hang-desc/@src, $hang-desc, $base-uri, $prefix-map)
            else if ($hang-desc/@typeof)
            then gen-blank-node-uri ($hang-desc)
            else if ($hang-desc/(@rel | @rev))
            then ()
            else if (has-resource ($hang-desc/@resource))
            then resolve-uri-or-curie ($hang-desc/@resource, $hang-desc, $base-uri, $prefix-map)
            else if ($hang-desc/@href)
            then resolve-uri-or-curie ($hang-desc/@href, $hang-desc, $base-uri, $prefix-map)

                else gen-blank-node-uri ($hang-desc)
	let $subject := if ($relorrev eq "rel") then $local-subject else $local-object
	let $object := if ($relorrev eq "rel") then $local-object else $local-subject

	return
	if ($local-subject and $local-object)
	then
		<triple>
			<subject>{ $subject }</subject>
			<predicate>{ resolve-uri-or-curie ($rel-rev, $node, $base-uri, $prefix-map) }</predicate>
			<object>{ $object }</object>
		</triple>
	else ()
};

declare function relrev-hanging-bnode(
	$node as node(),
	$val as xs:string,
	$relorrev as xs:string,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element()*
{
	for $rel-rev in if (normalize-space($val) eq "") then () else tokenize($val, "\s+")
	let $local-subject := subject($node, (), $base-uri, $prefix-map)
	let $local-object := gen-blank-node-uri($node)
	let $subject := if ($relorrev eq "rel") then $local-subject else $local-object
	let $object := if ($relorrev eq "rel") then $local-object else $local-subject

	return
	if (hanging-bnode($node))
	then
		<triple>
			<subject>{ $subject }</subject>
			<predicate>{ resolve-uri-or-curie ($rel-rev, $node, $base-uri, $prefix-map) }</predicate>
			<object>{ $object }</object>
		</triple>
	else ()
};



declare private function gen-typeof (
	$props as xs:string,
	$node as element(),
	$parent-node as element()?,
	$base-uri as xs:string,
	$prefix-map as map:map
) as element(triple)*
{
	for $type in tokenize ($props, "\s+")
	 :)
(: todo: make function subject-typeof :)(:

	let $local-subject :=
	           if (has-about ($node/@about))
                   then resolve-uri-or-curie($node/@about, $node, $base-uri, $prefix-map)
                   else if ($node/@src)
                        then resolve-uri-or-curie($node/@src, $node, $base-uri, $prefix-map)
                        else if (local-name($node) = ("head", "body"))
                             then wrap-uri($base-uri)
                             else if (has-resource ($node/@resource) and not($node/(@rel | @rev)))
                                  then resolve-uri-or-curie($node/@resource, $node, $base-uri, $prefix-map)
                                  else if ($node/@href and not($node/(@rel | @rev)))
                                       then resolve-uri-or-curie($node/@href, $node, $base-uri, $prefix-map)
                                       else if (fn:not ($node/parent::*) and fn:not(has-about($node/@about)))
                                       then wrap-uri($base-uri)
                                           else if ($node/@about = '')
                                           then (wrap-uri ($base-uri))
                                                else gen-blank-node-uri($node)

    let $vocab := ancestor-vocab ($node)
	let $object :=
	   if ( fn:not ( fn:contains($type, ':') ) and ( $vocab != '' ) )
	   then ( wrap-uri ( fn:concat ($vocab, $type) ) )
	   else ( resolve-uri-or-curie ($type, $node, $base-uri, $prefix-map) )

	return
	<triple>
		<subject>{ $local-subject }</subject>
		<predicate>rdf:type</predicate>
		<object>{ $object }</object>
	</triple>
};
 :)

(: ----------------------------------------------------------------- :)

(: ToDo: load up namespace definitions as well :)
declare private function add-prefixes (
	$ec as map:map,
	$prefix-def as xs:string?
) as map:map
{
	if (fn:empty ($prefix-def))
	then map:get ($ec, $EC-DEFINED-PREFIXES)
	else (
		let $map := map:get ($ec, $EC-DEFINED-PREFIXES)
		let $local-prefix-map := map-from-prefixes ($ec, $prefix-def)
		let $_ :=
			for $key in map:keys ($local-prefix-map)
			return map:put ($map, $key, map:get ($local-prefix-map, $key))
		return $map
	)
};

(:
declare private function context-prefix-map (
	$current-map as map:map,
	$prefix-def as xs:string?
) as map:map
{
	if (fn:empty ($prefix-def))
	then $current-map
	else (
		let $new-map := $current-map + map:map()
		let $local-prefix-map := map-from-prefixes ($prefix-def)
		let $_ :=
			for $key in map:keys ($local-prefix-map)
			return map:put ($new-map, $key, map:get ($local-prefix-map, $key))
		return $new-map
	)
};
 :)

declare function map-from-prefixes (
	$ec as map:map,
        $prefixes-str as xs:string
) as map:map
{
        let $tokens := tok ($prefixes-str)
        let $map := map:map()
        let $_ :=
                for $token at $idx in $tokens
                return
                if ((($idx mod 2) = 1) and ($token ne "_:") and fn:ends-with ($token, ":") and (fn:substring-before ($token, ":") castable as xs:NCName))
                then map:put ($map, fn:substring-before ($token, ":"), $tokens[$idx + 1])
                else ()
        let $_ :=
        	for $key in map:keys ($map)
        	return referenced-prefix ($ec, $key, map:get ($map, $key))
        return $map
};

declare private function referenced-prefix (
	$ec as map:map,
	$prefix as xs:string,
	$ns-uri as xs:string
) as empty-sequence()
{
	map:put (map:get ($ec, $EC-REFERENCED-PREFIXES), $prefix, $ns-uri)
};

declare private function namespace-uri-for-prefix (
	$ec as map:map,
        $prefix as xs:string,
        $node as node()
) as xs:string?
{
	let $defined-prefixes := map:get ($ec, $EC-DEFINED-PREFIXES)
	let $referenced-prefixes := map:get ($ec, $EC-REFERENCED-PREFIXES)
        let $uri as xs:string? := map:get ($defined-prefixes, $prefix)
        let $uri as xs:string? := if (fn:exists ($uri)) then $uri else map:get ($default-prefixes-map, $prefix)
        let $uri as xs:string? := if (fn:exists ($uri) or ($prefix = "_")) then $uri else fn:namespace-uri-for-prefix ($prefix, $node)
        (:
        needs something like this, however better to add this when prefix map is generated.
        let $test as xs:string? :=
            if (fn:exists ($uri) and not(fn:starts-with($uri, 'http') or fn:starts-with($uri, 'uri')))
            then
            concat($base-uri, $uri)
            else $uri
            :)
	    let $_ := if (fn:exists ($uri)) then map:put ($referenced-prefixes, $prefix, $uri) else ()

        return $uri
};

(: ----------------------------------------------------------------- :)

(:
declare function effective-datatype (
	$node as element(),
	$prefix-map as map:map
) as xs:string?
{
	if ($node/@datatype)
	then ( resolve-curie ($node/@datatype, $node, (), $prefix-map) )
	else if ($node/@rdf:datatype)
	then ( resolve-curie ($node/@rdf:datatype, $node, (), $prefix-map) )
	else ()

};

declare function effective-lang (
	$node as element(),
	$parent-node as element()
) as attribute()?
{
	if ($node/(@xml:lang|@lang))
	then attribute xml:lang { (($node/@xml:lang/fn:string()), $node/@lang/fn:string())[1] }
	else (effective-lang ($parent-node, $node/parent::*))

	 :)
(: alternative: :)(:

	 :)
(: ($node/ancestor-or-self::*/@xml:lang)[position() eq last()] :)(:

};
 :)

(: ----------------------------------------------------------------- :)

declare private variable $default-bnode-id := "_:defbnode";

declare private function resolve-curie (
	$ec as map:map,
	$val as xs:string,
	$node as element()
) as xs:string?
{
	let $base-uri := map:get ($ec, $EC-BASE)
	let $vocab := map:get ($ec, $EC-VOCABULARY)
	let $prefix-map := map:get ($ec, $EC-DEFINED-PREFIXES)		(: ToDo: Should this be $EC-IRI-MAPPINGS? :)
	let $curie :=
		if (fn:starts-with ($val, "[") and fn:ends-with ($val, "]"))
		then fn:substring-after (fn:substring-before ($val, "]"), "[")
		else $val
	let $prefix := if ($curie = $vocabulary-terms) then $curie else fn:substring-before ($curie, ":")
	let $ns-uri :=
		if (fn:not ($prefix) or (fn:starts-with ($curie, ":")))
		then ($vocab, $dfvocab)[1]
		else
			if ($curie = $vocabulary-terms)
			then map:get ($default-prefixes-map, $curie)
			else namespace-uri-for-prefix ($ec, $prefix, $node)
	let $suffix := fn:substring-after ($curie, ":")
	let $result :=
		if ($prefix = "_")
		then if ($curie = $default-bnode-ref) then (map:get ($bnode-map, $default-bnode-ref), $default-bnode-id)[1] else $curie
		else if ($prefix = "" and fn:starts-with ($curie, ':'))
		then fn:concat ("<", $dfvocab, $suffix, ">")
		else

		    if ((($ns-uri eq $dfvocab) and ($suffix = $htmlrels)) or ($prefix and $ns-uri))
			then fn:concat ("<", $ns-uri, $suffix, ">")

			else ()

	return $result
};

declare function resolve-uri (
	$ec as map:map,
	$val as xs:string
) as xs:string
{
	let $base-uri := map:get ($ec, $EC-BASE)
	let $vocab := map:get ($ec, $EC-VOCABULARY)
	let $uri :=
		if (fn:starts-with ($val, ":") or fn:ends-with ($val, ":"))
		then $base-uri
		else
			if (fn:starts-with ($val, "#"))
			then fn:concat ($base-uri, $val)
			else
				if ($val = $vocabulary-terms)
				then map:get ($default-prefixes-map, $val)
				else
					if (fn:exists ($vocab) and fn:not (fn:contains ($val, ":")))
					then fn:concat ($vocab, $val)
					else fn:resolve-uri ($val, $base-uri)
	return wrap-uri ($uri)
};

(:
declare private function resolve-uri-or-curie (
	$val as xs:string,
	$node as element(),
	$base-uri as xs:string,
	$prefix-map as map:map
) as xs:string
{
	(resolve-curie ($val, $node, $base-uri, $prefix-map), resolve-uri ($val, $base-uri))[1]
};
 :)

declare private function resolve-uri-or-curie (
	$ec as map:map,
	$val as xs:string,
	$node as element()
) as xs:string
{
	let $base-uri := map:get ($ec, $EC-BASE)
	let $prefix-map := map:get ($ec, $EC-DEFINED-PREFIXES)		(: ToDo: Should this be $EC-IRI-MAPPINGS? :)
	return (resolve-uri ($ec, $val), resolve-curie ($ec, $val, $node))[1]
};

declare function wrap-uri (
	$uri as xs:string
) as xs:string
{
	fn:concat ("<", $uri, ">")
};

declare function unwrap-uri (
	$uri as xs:string
) as xs:string
{
	let $string-length as xs:int := fn:string-length ($uri) - 2

	return fn:substring ($uri, 2, $string-length)
};

(: ----------------------------------------------------------------- :)

declare function gen-blank-node-uri (
	$node as element()
) as xs:string
{
	let $node-id := concat ("_:b", $node/fn:generate-id ($node))
	let $_ := if (map:get ($bnode-map, $default-bnode-ref)) then () else map:put ($bnode-map, $default-bnode-ref, $node-id)
	let $_ := map:put ($bnode-map, $node-id, $node-id)

	return $node-id
};


(:
declare private function is-xml (
	$node as element(),
	$base-uri as xs:string,
	$prefix-map as map:map
) as xs:boolean
{
	$node/@datatype and (unwrap-uri (resolve-curie ($node/@datatype, $node, (), $prefix-map)) = $rdf-XMLLiteral)
};
 :)

(: returns first ancestor @vocab :)
declare private function ancestor-vocab (
    $node as element()
) as xs:string*
{
    $node/ancestor-or-self::*[@vocab][1]/@vocab
};

(: function mapping will cause empty sequence result if $about is empty, which evaluates as false :)
declare private function has-about (
	$about as xs:string
) as xs:boolean
{
	$about and fn:not ($about = ("[]"))
};

(: function mapping will cause empty sequence result if $about is empty, which evaluates as false :)
declare private function has-resource (
	$resource as xs:string
) as xs:boolean
{
	$resource and fn:not ($resource = ("[]"))
};

declare private function has-href (
	$href as xs:string
) as xs:boolean
{
	$href and fn:not ($href = ("[]"))
};

declare private function has-src (
	$src as xs:string
) as xs:boolean
{
	$src and fn:not ($src = ("[]"))
};

(: ----------------------------------------------------------------- :)
(: ----------------------------------------------------------------- :)
(: ----------------------------------------------------------------- :)



(:
	let $curie :=
		if (fn:starts-with ($val, "[") and fn:ends-with ($val, "]"))
		then fn:substring-after (fn:substring-before ($val, "]"), "[")
		else $val
	let $p := fn:substring-before ($curie, ":")
	let $n := if ($p) then namespace-uri-for-prefix ($p, $prefix-map, $node) else ()
	let $prefix := if (fn:exists ($n)) then $p else ()
	let $ns-uri :=
		if ($n)
		then $n
		else
			if (fn:starts-with ($curie, ":"))
			then $dfvocab
			else if ($curie castable as xs:NCName) then $base-uri else ()
	let $suffix :=
		if ($prefix and ($ns-uri = $dfvocab))
		then ()
		else
			if ($prefix or starts-with ($curie, ":"))
			then fn:substring-after ($curie, ":")
			else $curie
	let $uri := fn:concat ($ns-uri, $suffix)

	return
	<parsed-curie>
		<curie>{ $curie }</curie>
		<prefix>{ $prefix }</prefix>
		<suffix>{ $suffix }</suffix>
		<ns-uri>{ $ns-uri }</ns-uri>
		<expanded-uri>{ $uri }</expanded-uri>
		<uri>{ if (fn:exists ($prefix)) then $curie else $uri }</uri>
		<ttl>{ if (fn:exists ($prefix)) then $curie else fn:concat ("<", $uri, ">") }</ttl>
	</parsed-curie>
:)

(:
declare private function rdfa-property-value (
	$val as xs:string,
	$node as element(),
	$base-uri as xs:string,
	$prefix-map as map:map
) as element(attr-value)
{
	let $curie :=
		if (fn:starts-with ($val, "[") and fn:ends-with ($val, "]"))
		then fn:substring-after (fn:substring-before ($val, "]"), "[")
		else $val
	let $p := fn:substring-before ($curie, ":")
	let $n := namespace-uri-for-prefix ($p, $prefix-map, $node)
	let $prefix := if (fn:exists ($n)) then $p else ()
	let $ns-uri :=
		if (fn:exists ($n))
		then $n
		else
			if (fn:starts-with ($curie, ":"))  (: CheckMe :)
			then $dfvocab
			else $base-uri
	let $tokens := if (fn:exists ($prefix)) then fn:tokenize ($curie, ":") else ()
	let $local-name := if (fn:count ($tokens) = 0) then $curie else $tokens[fn:last()]
	let $full-value := if (fn:empty ($prefix) and fn:empty ($ns-uri)) then $curie else fn:concat ($ns-uri, $local-name)

	return
	<attr-value>
		<prefix>{ $prefix }</prefix>
		<ns-uri>{ $ns-uri }</ns-uri>
		<local-name>{ $local-name }</local-name>
		<curie>{ $curie }</curie>
		<full-value>{ $full-value }</full-value>
	</attr-value>
};

declare private function expanded-curie (
	$attr-value as element(attr-value),
	$prefix-map as map:map
) as xs:string
{
	if (map:get ($prefix-map, $attr-value/prefix/fn:string()))
	then $attr-value/curie/fn:string()
	else $attr-value/full-value/fn:string()
};

declare private function expanded-curie-attr (
	$attr-value as xs:string,
	$node as element(),
	$base-uri as xs:string,
	$prefix-map as map:map
) as xs:string
{
	expanded-curie (rdfa-property-value ($attr-value, $node, $base-uri, $prefix-map), $prefix-map)
};
:)

(:
declare private function expand-curie (
	$curie as xs:string,
	$node as element(),
	$base-uri as xs:string,
	$prefix-map as map:map
) as xs:string
{
	let $parts := parse-curie ($curie, $node, $base-uri, $prefix-map)
	return
	if ($parts/prefix = "_")
	then $curie
	else $parts/uri/fn:string()
};
:)

(: ----------------------------------------------------------------- :)
