/* -*- Mode: Prolog -*- */

:- module(shoge,
          [load_grammar/1,
           list_generated_axioms/1,
           list_generated_phrases/1,
           list_generated_expressions/1,
           list_terminals_from_ontology/3,
           list_term_parses/2,
           generate_ontologies/1,
           generate_ontology/1,
           generate_ontology/2,
           generate_and_save_ontology/2,
           generate_and_save_ontology/3
          ]).

% REQUIREMENT: thea
:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_io)).
:- use_module(library(thea2/owl2_plsyn)).
:- use_module(library(thea2/owl2_visitor)).
:- use_module(library(thea2/owl2_popl)).
:- use_module(library(thea2/owl2_util)).
:- use_module(library(thea2/owl2_reasoner)).

% REQUIREMENT: http, semweb
:- use_module(library(http/dcg_basics)).
:- use_module(library(semweb/rdf_db)).

% Shoge files are prolog files
user:prolog_file_type(shg,prolog).

:- multifile axiomlist/1.
:- multifile exclude/1.
:- multifile rephrase/1.
:- multifile on_completion/1.
:- multifile about//1.
:- multifile (+)/1.

:- dynamic organism_type/1.

% ----------------------------------------
% GRAMMAR SYNTAX RULES
% ----------------------------------------

% OPERATORS
:- op(1200,xfx,*-->).
:- op(1150,xfx,::).
:- op(500,fx,?).
:- op(700,xfy,is_name_of).
:- op(1000,fx,rephrase).
:- op(700,xfy,as).

:- discontiguous
	system:term_expansion/2.

:- dynamic
	include_code/1.

including :-
	include_code(X), !,
	X == true.
including.

if_expansion((:- switch(_)), []) :- true.


if_expansion((:- if(G)), []) :-
        throw(error(foo)),
	(   including
	->  (   catch(G, E, (print_message(error, E), fail))
	    ->  asserta(include_code(true))
	    ;   asserta(include_code(false))
	    )
	;   asserta(include_code(else_false))
	).
if_expansion((:- else), []) :-
	(   retract(include_code(X))
	->  (   X == true
	    ->  X2 = false 
	    ;   X == false
	    ->	X2 = true
	    ;	X2 = X
	    ),
	    asserta(include_code(X2))
	;   throw_error(context_error(no_if),_)
	).
if_expansion((:- endif), []) :-
	retract(include_code(_)), !.

if_expansion(_, []) :-
	\+ including.

system:term_expansion(In, Out) :-
	%prolog_load_context(module, plunit),
	if_expansion(In, Out).

% EXPANSION
%  TODO: treat basic *--> patterns as SubClassOf
system:term_expansion((H*-->B :: X),Goal) :-
        !,
        H =.. [P|Args],
        H2 =.. [P,X|Args],
        dcg_translate_rule( (H2-->B), Goal).
system:term_expansion(H*-->B,[Goal|AxiomGoals]) :-
        axioms_from_production(H,B,AxiomGoals),
        % we add an argument such that e.g. limb_segment *--> ... becomes limb_segment(X and Y and ...)
        add_arg(B,B2,Args),
        add_arg(H,H2,Args),
        dcg_translate_rule( (H2-->B2), Goal).

% todo - normalization in DCG expansion
argi(A1,A2,A) :- nonvar(A1),A1=thing,!,A=A2.
argi(A1,A2,A) :- nonvar(A2),A2=thing,!,A=A1.
argi(A1,A2,A1 and A2).  % head of each DCG rule is an OWL class expression wich is by default a conjunction

add_arg((X,Y),(X2,Y2),A) :-
        !,
        add_arg(X,X2,A1),
        add_arg(Y,Y2,A2),
        argi(A1,A2,A).
add_arg((X;Y),(X2;Y2),A) :-
        !,
        add_arg(X,X2,A),
        add_arg(Y,Y2,A).
add_arg((X|Y),(X2,{A=A1}|Y2,{A=A2}),A) :-
        !,
        add_arg(X,X2,A1),
        add_arg(Y,Y2,A2).
add_arg(@(X),[@X],@X) :- !.            % TODO
add_arg(X,X,thing) :- is_list(X),!.
add_arg(X,Y,A) :- atom(X),!,Y =.. [X,A].
%add_arg(X,Y,A) :- X =.. [P|L],!,add_argl(L,L2,A),Y =.. [P|L2]. % REMOVED - strictly only replaced args..
add_arg(?(X),(X2,{Z=A};[],{Z=thing}),Z) :- !,add_arg(X,X2,A). % better to rewrite entire goal
add_arg(X/P,Y,P some A) :- !,Y =.. [X,A].  % e.g. limb/part_of, @digit
add_arg(P some X,Y,P some A) :- !,Y =.. [X,A].  % e.g. limb/part_of, @digit
add_arg(X,Y,A) :- nonvar(X),X =.. [P|L], !, Y =.. [P,A|L].
add_arg(X,X,_).

add_argl([],[],_).
add_argl([H|T],[H2|T2],A) :-
        add_arg(H,H2,A),
        add_argl(T,T2,A).

% ----------------------------------------
% AXIOMS FROM PRODUCTIONS
% ---------------------------------------- 
% any rule H --> A generates A SubClassOf H.
% e.g limb_segment --> autopod
axioms_from_production(H, (A|B), G) :-
        atom(H),
        !,
        axioms_from_production(H, A, GA),
        axioms_from_production(H, B, GB),
        append(GA,GB,G).
axioms_from_production(H, A, [+(subClassOf(@A,@H))]) :-
        atom(H),
        atom(A),
        !.
axioms_from_production(H, @A, [+(subClassOf(@A,@H))]) :-
        atom(H),
        atom(A).
/*
axioms_from_production(H, A, [+(Ax)]) :-
        atom(H),
        atom(A),
        !,
        internal_to_owl( subClassOf(@A,@H), Ax).
axioms_from_production(H, @A, [+(Ax)]) :-
        atom(H),
        atom(A),
        !,
        internal_to_owl( subClassOf(@A,@H), Ax).
*/
axioms_from_production(_,_,[]).


% ----------------------------------------
% NORMALIZATION
% ----------------------------------------


% ----------------------------------------
% I/O
% ----------------------------------------

load_grammar(Path) :-
        absolute_file_name(Path,[extensions([shg,pl])],File),
        exists_file(File),
        !,
        consult(Path).
load_grammar(Path) :-
        consult(grammars/Path).

% ----------------------------------------
% PHRASE REWRITING
% ----------------------------------------
% user can control rewriting using rephrase/1 
rewrite_phrase(L1,L2) :-
        rewrite_phrase_1(L1,Lx),
        !,
        rewrite_phrase(Lx,L2).
rewrite_phrase(L,L).

rewrite_phrase_1(L1,L2) :-
        rephrase(Toks as Toks2),
        append(Toks,L1T,L1),
        !,
        (   is_list(Toks2)
        ->  append(Toks2,L1T,L2)
        ;   append([Toks2],L1T,L2)).
rewrite_phrase_1([H|L1],[H|L2]) :-
        rewrite_phrase_1(L1,L2).
        

        

% ----------------------------------------
% GENERATION
% ----------------------------------------

expression_phrase(X,P,_Opts) :-
        phrase(X,P1),
        rewrite_phrase(P1,P).


%% list_generated_phrases(+Unit)
%
% lists all tokenlists generated from Unit
% (Unit should be a non-terminal in the grammar)
list_generated_phrases(P) :-
        G =.. [P,_],
        forall(expression_phrase(G,X,[]),
               writeln(X)).

%% list_generated_expressions(+Unit)
%
% lists all class expressions generated from Unit
% (Unit should be a non-terminal in the grammar)
list_generated_expressions(P) :-
        G =.. [P,X],
        forall(expression_phrase(G,_,[]),
               writeln(X)).

list_generated_ontology(P) :-
        forall(generate_axiom(P,Axiom),
               format('~q.~n',[Axiom])).

list_generated_axioms(P) :-
        forall(generate_plaxiom(P,Axiom),
               writeln(Axiom)).

list_term_parses(Unit,T) :-
        forall(parse_term_to_expression(Unit,T,Expr,[]),
               writeln(Expr)).

generate_ontologies(Ps) :-
        maplist(generate_ontology,Ps).

%% generate_ontology(+Unit)
%% generate_ontology(+Unit,+Opts:list)
%
% generate an OWL ontology in-memory using Unit as a base
%
% Opts:
%  on_completion(POPL_Pattern)
generate_ontology(P,Opts) :-
        option(ontology(O),Opts),
        assert_axiom(ontology(O)),

        % assert mini-ontology included in grammar files
        % (important for declaring data properties etc)
        forall(axiomlist(AL),
               forall((member(A,AL),internal_to_owl(A,A2)),
                      assert_axiom(A2,O))),
        forall((+(A),internal_to_owl(A,A2)),
               assert_axiom(A2,O)),
        
        forall(generate_axiom(P,A),
               assert_axiom(A,O)),
        forall(generate_axiom(about,A),
               assert_axiom(A,O)),
        remove_unsatisfiable(O,Opts),
        name_unnamed_entities(O),
        % TODO - proper re-syncing of reasoner
        forall(on_completion(X),
               popl_translate(X,[syntax(plsyn),translate(labels),ontology(O)])),
        simplify_all_axioms(O).

generate_ontology(P) :-        
        generate_ontology(P,[ontology('http://x.org')]).

remove_unsatisfiable(O,Opts) :-
        owl_nothing(Nothing),
        forall(exclude(X),
               (   debug(shoge,'Excluding: ~w',[X]),
                   internal_to_owl(X < Nothing, Axiom),
                   assert_axiom(Axiom, O))),
        assume_entity_declarations,
        initialize_reasoner(pellet,RE,Opts),
        findall(C,reasoner_ask(RE,unsatisfiable(C)),UCs),
        maplist(retract_class_and_axioms,UCs).

retract_class_and_axioms(C) :-
        debug(shoge,'Retracting: ~w',[C]),
        retract_axiom(class(C)),
        forall(axiom_about(A,C),
               retract_axiom(A)).

name_unnamed_entities(O):-
        class(X),
        \+ builtin_class(X),
        \+labelAnnotation_value(X,_),
        add_default_label(X,O),
        fail.
name_unnamed_entities(O):-
        objectProperty(X),
        \+labelAnnotation_value(X,_),
        add_default_label(X,O),
        fail.
name_unnamed_entities(_).

add_default_label(X,O) :-
        atomic_list_concat([_,N],'#',X),
        !,
        assert_axiom(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(lang(en,N))),O),
        debug(shoge,'Default: ~w',[N]).
add_default_label(X,_) :-
        print_message(error,no_label(X)).

%% generate_and_save_ontology(+Unit, +File, +Opts:list)
%
% combines generate_ontology/2 and save_axioms/2.
% default format is OWL (RDF/XML) unless overridden with
% format(Fmt) in Opts list
generate_and_save_ontology(P,File,Opts) :-
        generate_ontology(P),
        option(format(Fmt),Opts,owl),
        save_axioms(File,Fmt).
generate_and_save_ontology(P,File) :-
        generate_and_save_ontology(P,File,[]).

% creates a goal that can be used in phrase/2
unit_goal(P,G) :- G =.. [P,_].

%% unit_goal_structure(+P, ?Goal, ?ClassExpr)
%
% generates a goal that can be used in phrase/2 to generate
% an OWL class expression.
%
% E.g. Goal = limb_segment(ClassExpr)
unit_goal_structure(P,G,X) :- G =.. [P,X].

% generates an axiom using P as base.
% Axiom will be a prolog term conforming to owl2_model
generate_axiom(P,Axiom) :-
        generate_plaxiom(P,PlAxiom),
        internal_to_owl(PlAxiom,Axiom).

% generates a plsyn axiom using P as base.
% this is encoded conforming to owl2_plsyn, and
% will have classes represented as @Name
generate_plaxiom(Ps,Axiom) :-
        member(P,Ps),
        generate_plaxiom(P,Axiom).
generate_plaxiom(P,Axiom) :-
        P\=about,
        atom(P),
        unit_goal_structure(P,G,ClassExpr),
        expression_phrase(G,Toks,[]),
        tokens_label(Toks,Label),
        structure_toks_iri(ClassExpr,Toks,IRI),
        rewrite_class_expression_with_IRIs(ClassExpr,ClassExpr_2),
        member(Axiom,
               [ class IRI,
                 equivalentClasses([ClassExpr_2,IRI]),
                 IRI label Label
               ]).
generate_plaxiom(about,Axiom) :-
        unit_goal_structure(about,G,Axiom),
        phrase(G,_).

internal_to_owl(PlAxiom,Axiom) :-
        plsyn_owl(PlAxiom,RawAxiom),
        replace_labels(RawAxiom,Axiom).

tokens_label(Toks,N) :-
        maplist(token_label,Toks,Labels),
        concat_tokens(Labels,' ',N).

structure_toks_iri(_,[@Tok],IRI) :-
        % reuse existing entity IRI
        labelAnnotation_value(IRI,Tok),
        !.
structure_toks_iri(_,Toks,IRI) :-
        maplist(token_iri_component,Toks,Frags),
        concat_tokens(Frags,'-',N),
        atom_concat('http://x.org#',N,IRI).

rewrite_class_expression_with_IRIs(X,X) :- atom(X),!.
rewrite_class_expression_with_IRIs(@X,Y) :- structure_toks_iri(_,[@X],Y),!.
rewrite_class_expression_with_IRIs(X,Y) :- X =.. ArgsX,maplist(rewrite_class_expression_with_IRIs,ArgsX,ArgsY),Y =.. ArgsY.


%token_to_term(@X,X) :- !. % TODO - use ontology for generation of syns
token_to_term(X,X).

token_label(@X,X) :- !.
token_label(X,X) :- !.

token_iri_component(@X,X) :- !.
token_iri_component(X,X) :- !.


% the native token list representation may include @X terms that
% correspond to vocabulary elements - generate the full string for
% these
concat_tokens(L,D,A) :-
        map_list_concat(token_to_term,L,D,A).

map_list_concat(P,L,D,A) :-
        maplist(P,L,L2),
        atomic_list_concat(L2,D,A).

replace_labels(literal(X),literal(X)) :- !.
replace_labels(NS:Local,IRI) :-
        rdf_global_id(NS:Local,IRI), % temp?
        !.
replace_labels(@X,Y) :-
        symbol_to_iri(X,Y),
        !.
replace_labels(@X,Y) :-
        !,
        replace_labels(X,Y).
replace_labels(X,Y) :-
        atom(X),
        !,
        replace_label(X,Y).
replace_labels(X,Y) :-
        is_list(X),
        !,
        maplist(replace_labels,X,Y).
replace_labels(X,Y) :-
        X =.. [P|Args],
        maplist(replace_labels,Args,Args2),
        Y =.. [P|Args2].

replace_label(X,Y) :- \+ atom_concat(http,_,X),!,atom_concat('http://x.org#',X,Y).
replace_label(X,X).


symbol_to_iri(S,X) :- labelAnnotation_value(X,S),!.
symbol_to_iri(S,X) :-
        atomic_list_concat(Toks,'_',S),
        atomic_list_concat(Toks,' ',S2),
        labelAnnotation_value(X,S2),
        !.


% ----------------------------------------
% PARSING - TODO
% ----------------------------------------

parse_term_to_expression(Unit,Term,Expr,Opts) :-
        atom(Term),
        !,
        tokenize_atom(Term,Toks), % nlp
        % inefficient - tries all combos
        maplist(token_to_symbol,Toks,Symbols),
        writeln(Symbols),
        parse_term_to_expression(Unit,Symbols,Expr,Opts).
parse_term_to_expression(Unit,Toks,Expr,_) :-
        Expr =.. [Unit,_],
        phrase(Expr,Toks).

token_to_symbol(T,@T) :-
%        Head =.. [T,_,_,_],
%        clause(Head,_),
        !.
token_to_symbol(T,T).

list_terminals_from_ontology(Symbol,Class,Opts) :-
        forall(reasoner_ask(subClassOf(D,Class)),
               list_terminal_for_class(Symbol,D,Opts)).
list_terminal_for_class(Symbol,D,_Opts) :-
        labelAnnotation_value(D,Term),
        tokenize_atom(Term,Toks),
        Rule = (Symbol --> Toks),
        format('~q.~n',[Rule]).



% ----------------------------------------
% SIMPLIFICATION
% ----------------------------------------
% replace class expressions with named classes

simplify_all_axioms(O) :-
        equiv_mappings(Map),
        repeat,
        (   axiom(A1),
            simplify_axiom(A1,A2,Map)
        ->  retract_axiom(A1),
            assert_axiom(A2,O),
            fail
        ;   !).

        
simplify_axiom(A1,A2,Map) :-
        simplify_axiom_refl(A1,A2,Map),
        A1\=A2,
        debug(simplify,'~w ==> ~w',[A1,A2]).

simplify_axiom_refl(A1,A2,Map) :-
        simplify_axiom_1(A1,Ax,Map),
        Ax\=A1,
        !,
        simplify_axiom_refl(Ax,A2,Map).
simplify_axiom_refl(A,A,_).

simplify_axiom_1(equivalentClasses(L1),equivalentClasses(L2),Map) :-
        simplify_expressions(L1,L2,false,Map).
simplify_axiom_1(subClassOf(A,B),subClassOf(A2,B2),Map) :-
        simplify_expression(A,A2,true,Map),
        simplify_expression(B,B2,true,Map).

simplify_expression(X,X,_,_) :- atom(X),!.
simplify_expression(X,E,true,Map) :-
        \+ atom(X),
        member(E-X,Map),        % exact
        !.
simplify_expression(X,E,true,Map) :-
        \+ atom(X),
        member(E-X2,Map),
        structurally_equivalent(X,X2),
        !.
simplify_expression(In,Out,_,Map) :-
        !,
        In =.. [P|Args],
        simplify_expressions(Args,Args2,true,Map),
        Out =.. [P|Args2].

simplify_expressions([],[],_,_) :- !.
simplify_expressions([X1|L1],[X2|L2],S,Map) :-
        simplify_expression(X1,X2,S,Map),
        simplify_expressions(L1,L2,S,Map).

equiv_mappings(L) :-
        findall(X-Y,
                (   equivalent_to(X,Y),
                    atom(X),
                    \+ atom(Y)),
                L).

% ----------------------------------------
% CONTEXTS
% ----------------------------------------
%in_context(_) :- \+ organism_type(_),!.
in_context(X) :- organism_type(X).
        
        

% ----------------------------------------
% COMMAND LINE HOOKS (THEA)
% ----------------------------------------


:- multifile user:parse_arg_hook/3.
user:parse_arg_hook(['--set-context',G|L],L,null) :- retractall(organism_type(_)),asserta(organism_type(G)).
user:parse_arg_hook(['--grammar',G|L],L,null) :- load_grammar(G).
user:parse_arg_hook(['--generate-ontology',GA|L],L,goal(generate_ontologies(Gs))) :-
        atomic_list_concat(Gs,',',GA).
user:parse_arg_hook(['--generate-phrases',G|L],L,goal(list_generated_phrases(G))).
user:parse_arg_hook(['--generate-expressions',G|L],L,goal(list_generated_expressions(G))).
user:parse_arg_hook(['--make-terminals',G,C|L],L,goal(list_terminals_from_ontology(G,C,[]))).
user:parse_arg_hook(['--list-axioms'|L],['--list-eq-axioms','--list-sc-axioms'|L],null).
user:parse_arg_hook(['--list-eq-axioms'|L],['--query','equivalentClasses(X)','--save-opts','plsyn,labels'|L],null).
user:parse_arg_hook(['--list-sc-axioms'|L],['--query','subClassOf(X,Y)','--save-opts','plsyn,labels'|L],null).
user:parse_arg_hook(['--parse-term',P,T|L],L,goal(list_term_parses(P,T))).


