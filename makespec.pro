grammar_seed(digestive_tract,structure).
grammar_seed(limbs_simple,limb_segment).
grammar_seed(limbs2,limb_segment).

grammar_seed(limbs,limb_unit).

all <-- Deps,
 {findall(t(['all-',X]),
          grammar_seed(X,_),
          Deps)}.

'all-%' <-- ['sample-output/%.obo', 'sample-output/%.omn'].

'sample-output/$Grammar.owl' <-- 'grammars/$Grammar.shg',
   {grammar_seed(Grammar,Seed)},
   'shoge --grammar $Grammar --generate-ontology $Seed --to owl --out $@.tmp && mv $@.tmp $@'.


'%.obo' <-- '%.owl',
   'owltools $< -o -f obo $@'.

'%.omn' <-- '%.owl',
   'owltools $< -o -f manchester file://`pwd`/$@'.
