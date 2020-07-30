% PROBLEM 9.3: FOWARD CHAINING
%======================================================================
% This file contains the forward-chaining program and a sample
% set of rules at the end. Consult this file and try the following
% queries to find the facts that can be deduced from the given facts
% by using the given set of rules (the program need not be consulted).
%   ?- forward_chain([e,f],NewFacts).
%   ?- forward_chain([b,s],NewFacts).
%   ?- forward_chain([c,s],NewFacts).
%   ?- forward_chain([a,b],NewFacts).
%
% Note: Some (compiled) Prolog systems require every predicate to have
% a clause at the time of evaluation. To make this program work with
% those non-standard systems (as well as with the standard ones), we have
% added a dummy fact "fact('#','#')"  at the end of this program.
%======================================================================
    forward_chain(GFacts,FFacts) :-
        reset,
        collect_facts(GFacts,IFacts),
        forward(IFacts,[],FFacts),!.

    forward([F|Fs],UFs,Facts) :-
        match_rules(F,Fs,UFs,NFs),
        forward(NFs,[F|UFs],Facts).
    forward([],Facts,Facts).

    match_rules(F,Fs,UFs,NFs) :-
        rule(Rule),
        match_rule(F,Rule,Fs,UFs,IFs),
        match_rules(F,IFs,UFs,NFs).
    match_rules(F,NFs,UFs,NFs).

    match_rule(F,(A :- B),Fs,UFs,IFs) :-
        copy((A,B),(A1,B1)),
        select(F,B1,B2),not(member(non(F),B2)),
        new_rule(F,(A,B),(A1,B2),Fs,UFs,IFs).

    new_rule(F,(A,B),(A1,[]),Fs,UFs,IFs) :- !,
	new_fact(A1,Fs,UFs,IFs),
        replace_rule(F,(A,B),A1),
        remove_rules(A1).
    new_rule(F,(A,B),(A1,B1),Fs,_,Fs) :-
	replace_rule(F,(A,B),A1),
        asserta(rule((A1 :- B1))).

    new_fact(A,Fs,UFs,[A|Fs]) :-
        not(member(A,Fs)),not(member(A,UFs)),!.
    new_fact(_,Fs,_,Fs).

    replace_rule(F,(A,B),A1) :-
        retract(rule((A :- B))),
        (instance_of(A,A1),!;
         assert(rule((A :- ([non(F)|B]))))).

    remove_rules(A1) :-
        rule((A :- B)),instance_of(A,A1),
        retract(rule((A :- B))),
        fail; true.

    collect_facts(Fs,IFs) :-
        setof(F,fact(F),L),!,
        append(Fs,L,IFs).
    collect_facts(Fs,Fs).

    select(X,[X|T],T).
    select(X,[Y|T],[Y|R]) :- select(X,T,R).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    fact('#','#').
%--------------------------------------------------------------
% The sample program
%==============================================================
    reset :-
        abolish(rule/1),
        programs(1,Rules),
        store_program(Rules).

    store_program([]) :- !.
    store_program([Rule|Rest]) :- !,
        assert(Rule),
        store_program(Rest).

    programs(1,[rule((a :- [b])),
                rule((a :- [c])),
                rule((b :- [e,f])),
                rule((c :- [e,g])),
                rule((p(1) :- [b])),
                rule((q(X) :- [p(X)])),
                rule((r :- [a,s]))]).


%---------------------------------------------------------------
% META-PROGRAMMING TOOLS
% ==============================================================
    instance_of(A,B) :- not(not(instanc(A,B))).

    instanc(A,B) :- var_list(A,L),bind_var(L,1),A = B.

    bind_var([],_) :- !.
    bind_var(['@var'(N)|Xs],N) :- N1 is N+1,bind_var(Xs,N1).

    var_list(A,[]) :- ground(A),!.
    var_list(A,L) :-  collect_var(A,Q-[]),
        setof(X,member(X,Q),L).

    collect_var(A,Q-Q) :- constant(A),!.
    collect_var(A,[A|Q]-Q) :- var(A), !.
    collect_var(A,Q) :- A =.. [P|Args],collect_vars(Args,Q).

    collect_vars([],Q-Q) :- !.
    collect_vars([A|As],Q-Qt) :-
        collect_var(A,Q-Qs),collect_vars(As,Qs-Qt).

    constant(A) :- atom(A); integer(A); float(A).

    member(X,[X|T]).
    member(X,[Y|T]) :- member(X,T).

    ground(A) :- copy(A,B), A == B.
    copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

% ============================================================




