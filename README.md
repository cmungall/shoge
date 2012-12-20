# Shoge

Shoge (Serial Homology Ontology Generation Engine) is a tool for
generating ontologies from a grammar.

## Examples

See the examples in this repository

 * grammars/ *example grammars*
     * limbs_simple.shg *basic limb segment generation*
     * limbs.shg *more complex variant of above*
 * sample-output/ *examples of ontologies generated from these grammars*

## Synopsis

Example grammar file:

    grammar(limbs_simple).

    limb_segment *--> stylopod | zeugopod | autopod.
    
    stylopod *--> in_limb, @stylopod.
    zeugopod *--> in_limb, @zeugopod.
    autopod *--> in_limb, @autopod.
    
    in_limb *--> part_of some limb.
    limb *--> laterality, anterioposterior, @limb.
    
    laterality *--> @left.
    laterality *--> @right.
    anterioposterior *--> @anterior.
    anterioposterior *--> @posterior.

Save this as "limbs_simple.shg". Then generate phrases (on command line):

==
    $ shoge  --grammar limbs_simple --generate-phrases limb_segment
    [@left,@anterior,@limb,@stylopod]
    [@left,@posterior,@limb,@stylopod]
    [@right,@anterior,@limb,@stylopod]
    ...
    [@right,@posterior,@limb,@autopod]
==

List generated OWL axioms:

==
    $ shoge  --grammar limbs_simple --generate-ontology limb_segment --list-axioms
    stylopod and part_of some (anterior and limb and  and left) == 'left anterior limb stylopod'.
    ...
==

Generate ontology:

==
    $ shoge  --grammar limbs_simple --generate-ontology limb_segment --to owl --out limb_ontology.owl
==

The 10 rules in the grammar generates 24 classes and 43 axioms.

See
[limbs_simple.omn](https://github.com/cmungall/shoge/blob/master/sample-output/limbs_simple.omn)
in the sample-output directory

## Details

Shoge is a tool for generating ontologies based on simple grammatical
rules. The principle can be loosely explained in terms of serial
homology - the repetition of core structures of patterns, sometimes
with variation, within an organism.

For example, the developmental program for creating a vertebral
element is repeated multiple times along a vertebral column in an
individual. The number varies depending on species, and in some
species there is fusion between particular vertebral elements.

It can be useful to enumerate the serial homologs in an individual
organism. The FMA is one ontology where repeated structures are
individually named, including for example "Articular cartilage of
proximal epiphysis of middle phalanx of left little toe" (FMA:230787).

Generating exhaustive or semi-exhaustive catalogs or ontologies of
repeated structures is often done manually, which is tedious and error
prone. Automated Reasoners can help with validation and
classification, but not with ontology generation.

### Grammars

Generation rules are expressed as extended Definite Clause Grammars
(DCGs). These simultaneously generate the human-readable textual
phrases together with the description logic (DL) equivalence axioms.

The basic form is:

    x *--> y1, y2, ..., yn.

This rule states that terms of type x are written textually as the
sequence of terms y1..yn, and that the class expression for the
corresponding term is the intersection of the class expressions y1..yn.

(this can also be thought of in terms of an anatomical structure "x"
that is repeated in the body, where y1, y2 etc are state matrix
characters).

The symbols y1..yn may either be non-terminal or terminal. A
non-terminal symbol must be accompanied by a separate grammar rule
with the symbol on the left side. There are two forms of terminal
symbol:

 * [Word] - a logically silent symbol, wrapped inside square brackets
generates a phrase but not an expression. This is purely a grammatical
convenience, it does not affect the logic.

 * @Atom - an ontology symbol, denoted using the @ functor, generates
      an ontology term. This can be thought of as an atomic unit.

A symbol can also be of the form Relation some Term

For example, the following rule:

    autopod *--> @autopod, [of], part_of some limb.

Generates phrases such as "autopod of left hindlimb", and class
expressions such as "autopod and part_of some (hindlimb and
left)". The [of] terminal generates part of the phrase but not the
class expression. The "@autopod" indicates that there is a core
generic ontology class by this name.

#### Compilation to DCGs

Grammar rules are compiled down prolog DCGs. Arguments indicating
class expressions are automatically added. For example

    x *--> y1, y2, ..., yn.

is compiled to:

    x(Y1 and Y2 and ... and Yn) --> y1(Y1), y2(Y2), ..., yn(YN).

    autopod *--> @autopod, [of], part_of some limb_of.

is compiled to:

    autopod(@autopod and part_of some X) *--> [@autopod],[of],limb(X).


### Examples

#### Example 2: naming

A grammar file can contain rules for contracting names such as "anterior limb autopod" to "hand":

    'hand' is_name_of anterior limb autopod.
    'foot' is_name_of posterior limb autopod.

#### Example 3: ordinal series

Iterative homology is the repeated generation of similar structures
along some axis - e.g. digits or vertebrae.

Include the "ordinal series" grammar if you want to use the built in naming and generation rules:

    :- include(ordinal_series).
    anatomical_digit *--> part_of some limb_of, @anatomical_digit, in_ordinal_series(1-5).

This can generate terms such as "left anterior limb autopod digit 2",
together with an OWL expression:

    autopod and part_of some (limb and left and anterior) and has_order value 2.

#### Example 4: handling variation

Not every structure is repeated identically. For example, humans have
3 phalanges for every digit other than digit 1.


    phalanx *--> ?proximality,@phalanx,[of],part_of some anatomical_digit_of.
    proximality *--> @proximal.
    proximality *--> @distal.
    proximality *--> @medial.
    
    exclude(phalanx and medial and part_of some (anatomical_digit and has_order value 1)).


This encodes the rule that thumbs of a human do not have medial
phalanges. This term and any related terms will be excluded during
ontology generation.

#### Example 5a: consecutive ordinals


    interdigital_region *--> 
        @interdigital_region,[between],
            adjacent_to some anatomical_digit, [and],
            adjacent_to some anatomical_digit.
    % TODO


#### Example 5b: consecutive ordinals

Interdigital regions are associated with pairs of digits rather than "favoring" any one digit:


    interdigital_region *--> 
       @interdigital_region,[between],
       part_of some autopod, [digit], consecutive_number_pair.

    consecutive_number_pair(left_ordinal value X and right_ordinal value Y) --> whole_number(X,1-5),[and],{Y is X+1},whole_number(Y).


#### Example 6: additional axiom generation rules


    v_level *--> @cervical,in_ordinal_series(1-7).
    v_level *--> @thoracic,in_ordinal_series(1-12).
    v_level *--> @lumbar,in_ordinal_series(1-5).
    v_level *--> @sacral,in_ordinal_series(1-5).
    v_level *--> @coccygeal,in_ordinal_series(1-4).
    
    v_element *--> vertebra.
    v_element *--> vertebra_cartilage_condensation.
    v_element *--> vertebra_pre_cartilage_condensation.
    
    vertebra *--> @vertebra, v_level.
    vertebra_cartilage_condensation *--> @vertebra_cartilage_condensation, v_level.
    vertebra_pre_cartilage_condensation *--> @vertebra_pre_cartilage_condensation, v_level.
    
    about *-->
            {develops_from(S2,S1)},
            v_element(S2 and X),[develops,from],v_element(S1 and X)
            ::
            S2 and X < develops_from some S2 and X.
    
    develops_from(vertebra, vertebra_cartilage_condensation).
    develops_from(vertebra_cartilage_condensation, vertebra_pre_cartilage_condensation).


#### Example 7: enhancing existing ontologies

Shoge can be used to enhance existing ontologies

    shoge ontologies/limb_segment_core.owl --grammar limbs_simple --generate-ontology limb_segment --to owl > ontologies/limb_segment_all.owl

Here the core ontology already has classes called 'limb', 'limb segment', 'zeugopod' - but not specific class such as anterior limb left zeugopod

#### Example 8: Crosscutting subdivisions

A digestive tract can be subdivided on the proximal-distal axis into
segments, and on a perpendicular axis into layers. The core partonomy
can be compactly encoded using hierarchy/3 as follows:

    hierarchy(digestive_tract,
              segment,
              [esophagus,stomach,intestine=[
                                            small_intestine=[duodenum,jejunum,ileum],
                                            large_intestine=[cecum,colon=[
                                                                          ascending_colon,
                                                                          transverse_colon,
                                                                          sigmoid_colon
                                                                          ],
                                                             rectum,anal_canal]]]).
    hierarchy(wall,
              layer,
              [mucosa=[epithelium,lamina_propria,muscularis_mucosa],
               submucosa,
               muscularis_propria=[circular_muscle,longitudinal_muscle],
               serosa]).

The following grammar generates the cross-product of all segments and
all layers:

    structure *--> layer_of_digestive_tract | segment | layer.
    
    layer_of_digestive_tract *--> layer, [of], segment/maximal_part_of.
     
    segment(@X) --> {hierarchy_member_type(X,digestive_tract)},[@X].
    layer(@X) --> {hierarchy_member_type(X,wall)},[@X].

The following rules generate additional axioms connecting continuous
segments in the proximo-distal axis and adjacent layers:

    about *-->
            layer_of_digestive_tract(X),{X =::= ( (@LT1) and maximal_part_of some (@S))},
            { adjacent_to( LT1, LT2) },
            layer_of_digestive_tract(Y),{Y =::= ( (@LT2) and maximal_part_of some (@S))}
            :: X < adjacent_to some Y.
    
    about *-->
            layer_of_digestive_tract(X),{X =::= ( (@Layer) and maximal_part_of some (@SX))},
            { continuous_with( SX, SY) },
            layer_of_digestive_tract(Y),{Y =::= ( (@Layer) and maximal_part_of some (@SY))}
            :: X < continuous_with some Y.
    
    about *-->
            layer_of_digestive_tract(X),{X =::= ( (@LT1) and maximal_part_of some (@S))},
            { part_of( LT1, LT2) },
            layer_of_digestive_tract(Y),{Y =::= ( (@LT2) and maximal_part_of some (@S))}
            :: X < part_of some Y.
    
    about *-->
            layer_of_digestive_tract(X),{X =::= ( (@Layer) and maximal_part_of some (@SX))},
            { part_of( SX, SY) },
            layer_of_digestive_tract(Y),{Y =::= ( (@Layer) and maximal_part_of some (@SY))}
            :: X < part_of some Y.


    adjacent_to(X,Y) :- hierarchy(wall,L),hierarchy_pair(X,Y,L).
    part_of(X,Y) :- hierarchy(T,L),hierarchy_parent(X,Y,[T=L]).
    
    continuous_with(X,Y) :- hierarchy(digestive_tract,L),hierarchy_pair(X,Y,L).

See
[digestive_tract.obo](https://github.com/cmungall/shoge/blob/master/sample-output/digestive_tract.obo)
in the sample-output directory


## Grammar design

Often there are multiple grammars that can generate the same structures. For example:

    limb *--> laterality, anterioposterior, @limb.
    anterioposterior *--> @anterior | @posterior.

is the same as:

    limb *--> hindlimb | forelimb.
    hindlimb *--> laterality, @hindlimb.
    forelimb *--> laterality, @forelimb.

plus the following supplemental axioms:

    forelimb == limb and anterior.
    forelimb == limb and posterior.

The first grammar is more concise, but leads to more generic phrases
that might need re-phrased (e.g. "left anterior limb" to "left
forelimb"). The second form combines the naming conventions with the
grammar, but care must be taken that the named structures are also
fully defined. The second form is often preferable when working on
conjunction with an existing ontologies that names structures like
forelimb and hindlimb.

If we think in developmental terms, then the first might be preferred,
as we are using "@limb" as the core genetic program that is repeated. 

Another way would be to write:

    limb *--> laterality, (@hindlimb | @forelimb).

Which is concise, but has the disadvantage of lacking "hindlimb" and
"forelimb" as non-terminals.

One possible future extension is automatic generation of grammars from
existing ontologies.

## TODO

* Allow exclusion rules to be phrases as well as expressions
* Integration with ACE
* taxon syntax
* integrate with prism, probabilistic grammars
