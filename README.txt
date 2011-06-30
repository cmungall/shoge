---+ Shoogl

Shoogl is a tool for generating ontologies based on simple grammatical
rules.

---++ SYNOPSIS

Example grammar file:

==
  limb_segment *--> stylopod | zeugopod | autopod.
  
  stylopod *--> in_limb, @stylopod.
  zeugopod *--> in_limb, @zeugopod.
  autopod *--> in_limb, @autopod.
  
  in_limb *--> limb/part_of.
  limb *--> laterality, anterioposterior, @limb.
  
  laterality *--> @left.
  laterality *--> @right.
  anterioposterior *--> @anterior.
  anterioposterior *--> @posterior.
==

Save this as "limbs.shg". Then generate phrases (on command line):

==
  $ shoogle  --debug shoogl --grammar limbs --generate-phrases limb_segment
  [@left,@anterior,@limb,@stylopod]
  [@left,@posterior,@limb,@stylopod]
  [@right,@anterior,@limb,@stylopod]
  ...
  [@right,@posterior,@limb,@autopod]
==

List generated OWL axioms:

==
  $ shoogle  --debug shoogl --grammar limbs --generate-ontology limb_segment --list-axioms
  stylopod and part_of some (anterior and limb and  and left) == 'left anterior limb stylopod'.
  ...
==

Generate ontology:

==
  $ shoogle  --debug shoogl --grammar limbs --generate-ontology limb_segment --to owl -o limb_ontology.owl
==

---++ ABOUT

Shoogl is a tool for generating ontologies based on simple grammatical
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

---++ GRAMMARS

Generation rules are expressed as extended Definite Clause Grammars
(DCGs). These simultaneously generate the human-readable textual
phrases together with the description logic (DL) class expressions.

The basic form is:

==
x *--> y1, y2, ..., yn.
==

This rule states that terms of type x are written textually as the
sequence of terms y1..yn, and that the class expression for the
corresponding term is the intersection of the class expressions y1..yn.

The symbols y1..yn may either be non-terminal or terminal. A
non-terminal symbol must be accompanied by a separate grammar rule
with the symbol on the left side. There are two forms of terminal
symbol: a silent symbol, wrapped inside square brackets generates a
phrase but not an expression, and an ontology symbol, denoted using
the @ functor, generates an ontology term.

A symbol can also be of the form Term/Relation.

For example, the following rule:

==
  autopod *--> @autopod,[of],limb/part_of.
==

Generates phrases such as "autopod of left hindlimb", and class
expressions such as "autopod and part_of some (hindlimb and
left)". The [of] terminal generates part of the phrase but not the
class expression. The "@autopod" indicates that there is a core
generic ontology class by this name.

Grammar rules are compiled down prolog DCGs. Arguments indicating
class expressions are automatically added. For example

==
x *--> y1, y2, ..., yn.
==

is compiled to:

==
x(Y1 and Y2 and ... and Yn) --> y1(Y1), y2(Y2), ..., yn(YN).
==

==
  autopod *--> @autopod,[of],limb/part_of.
==

is compiled to:

==
  autopod(@autopod and part_of some X) *--> [@autopod],[of],limb(X).
==


---++ EXAMPLES

Example 2: naming

==
  'hand' is_name_of anterior limb autopod.
  'foot' is_name_of posterior limb autopod.
==

Example 3: ordinal series

==
  anatomical_digit *--> limb/part_of, @anatomical_digit, in_ordinal_series(1-5).
==

This can generate terms such as "left anterior limb autopod digit 2",
together with an OWL expression 

Example 4: handling variation

==
  phalanx *--> ?proximality,@phalanx,[of],anatomical_digit/part_of.
  proximality *--> @proximal.
  proximality *--> @distal.
  proximality *--> @medial.
  
  exclude(phalanx and medial and part_of some (anatomical_digit and has_order value 1)).
==

This encodes the rule that thumbs of a human do not have medial
phalanges. This term and any related terms will be excluded during
ontology generation

Example 5a: consecutive ordinals

==
  interdigital_region *--> @interdigital_region,[between],anatomical_digit/adjacent_to,[and],anatomical_digit/adjacent_to.
  % TODO
==

Example 5b: consecutive ordinals

==
  interdigital_region *--> @interdigital_region,[between],autopod/part_of, [digit], consecutive_number_pair.
  consecutive_number_pair(left_ordinal value X and right_ordinal value Y) --> whole_number(X,1-5),[and],{Y is X+1},whole_number(Y).
==

Example 6: additional axiom generation rules

==
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
==

---++ GRAMMAR DESIGN

Often there are multiple grammars that can generate the same structures. For example:

==
  limb *--> laterality, anterioposterior, @limb.
  anterioposterior *--> @anterior | @posterior.
==

is the same as:

==
  limb *--> hindlimb | forelimb.
  hindlimb *--> laterality, @hindlimb.
  forelimb *--> laterality, @forelimb.
==

plus the following supplemental axioms:

==
  forelimb == limb and anterior.
  forelimb == limb and posterior.
==

The first grammar is more concise, but leads to more generic phrases
that might need re-phrased (e.g. "left anterior limb" to "left
forelimb"). The second form combines the naming conventions with the
grammar, but care must be taken that the named structures are also
fully defined. The second form is often preferable when working on
conjunction with an existing ontologies that names structures like
forelimb and hindlimb.

Another way would be to write:

==
  limb *--> laterality, (@hindlimb | @forelimb).
==

Which is concise, but has the disadvantage of lacking "hindlimb" and
"forelimb" as non-terminals.

One possible future extension is automatic generation of grammars from
existing ontologies.

---++ TODO

* Allow exclusion rules to be phrases as well as expressions
* Integration with ACE
* taxon syntax
