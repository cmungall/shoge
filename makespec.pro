% This is a plmake spec. See https://github.com/cmungall/plmake

grammar_seed(digestive_tract,structure).
grammar_seed(limbs_simple,limb_segment).
grammar_seed(limbs2,limb_segment).

grammar_seed(limbs,limb_unit).
grammar_seed(somites,somitic_element).
grammar_seed(vertebra_development,v_element).

all <-- Deps,
 {findall(t(['all-',X]),
          grammar_seed(X,_),
          Deps)}.

'all-%' <-- ['sample-output/%.obo', 'sample-output/%.omn'].

'sample-output/$Grammar.owl' <-- 'grammars/$Grammar.shg',
   {grammar_seed(Grammar,Seed)},
   'shoge --grammar $Grammar --generate-ontology $Seed --to owl --out $@.tmp && mv $@.tmp $@'.



'sample-output/digestive_tract.png' <-- 'sample-output/digestive_tract.obo',
  'blip ontol-subset -i $< -query "class(ID)" -rel sublayer_of -cr sublayer_of -rel inner_to -rel distally_continuous_with -rel wall_subsegment_of -cr wall_subsegment_of -to png > $@'.

%  'blip ontol-subset -i $< -query "class(ID)" -rel subsegment_of -cr subsegment_of -rel distally_continuous_with -to png > $@'.

'matches/match-$Ont-$G.txt' <-- 'sample-output/$G.obo',
       'blip-findall -r $Ont -i $< -u metadata_nlp  -goal index_entity_pair_label_match "entity_pair_label_reciprocal_best_intermatch(X,Y,S),class(X),class(Y)" -select "m(X,Y,S)" -use_tabs -label -no_pred > $@.tmp && sort -u $@.tmp > $@'.
'matches/match-$Ont-$G.pro' <-- 'matches/match-$Ont-$G.txt',
   'tbl2p -p m $< > $@'.

'matches/missing-$Ont-$G.txt' <-- 'matches/match-$Ont-$G.pro',
     'blip-findall -i $< -i sample-output/$G.obo "class(ID),\\+m(ID,_,_,_,_,_)" -select ID'.

'matches/missing-$Ont-$G-new.obo' <-- 'matches/match-$Ont-$G.pro',
     'blip ontol-query -i $< -i sample-output/$G.obo -query "class(ID),\\+m(ID,_,_,_,_,_)" -to obo > $@'.



'%.obo' <-- '%.owl',
   'owltools $< -o -f obo $@'.

'%.omn' <-- '%.owl',
   'owltools $< -o -f manchester file://`pwd`/$@'.


