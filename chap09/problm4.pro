% PROBLEM 9.4: TWO-WAY CHAINING
% =======================================================================
% This file contains the two-way chaining program and a sample
% set of rules at the end. Consult this file and try the following
% queries to find the facts that can be deduced from the given facts
% by using the given set of rules (the program need not be reconsulted)
%   ?- twoway_chain([e,f],NewFacts).
%   ?- twoway_chain([b,s],NewFacts).
%   ?- twoway_chain([c,s],NewFacts).
%   ?- twoway_chain([a,b],NewFacts).
%
% Note: Some (compiled) Prolog systems require every predicate to have a
% clause at the time of evaluation. To make this program work with those
% non-standard systems (as well as with the standard ones), we have added
% the dummy facts "fact('#','#')" and "saved_fact('#','#')" at the end of
% this program.
% =======================================================================
twoway_chain(GFacts,FFacts) :-
    store_facts(GFacts),
    twoway(FFacts),!.

twoway(Facts) :-
    fact(F),
    match_rules(F),
    twoway(Facts).
twoway(Facts) :-
    setof(F,saved_fact(F),Facts).

match_rules(F) :-
    rule(Rule),
    match_rule(F,Rule),
    fail.
match_rules(F) :-
    retract(fact(F)),
    assert(saved_fact(F)).

match_rule(F,(A :- B)) :-
    member(F,B),
    prove(A),
    (not(fact(A)),not(saved_fact(A)),!,
     asserta(fact(A));true).

prove(A) :- fact(A); saved_fact(A).
prove(A) :- rule((A :- B)),prove(B).
prove([A|B]) :- prove(A),prove(B).
prove([]).

store_facts([F|Fs]) :-
    store_facts(Fs),asserta(fact(F)).
store_facts([]) :-
    abolish(saved_fact/1).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

select(X,[X|T],T).
select(X,[Y|T],[Y|R]) :- select(X,T,R).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

fact('#','#').
saved_fact('#','#').

% -------------------------------------------------------------
% The sample program
% -------------------------------------------------------------
rule((a :- [b])).
rule((a :- [c])).
rule((b :- [e,f])).
rule((c :- [e,g])).
rule((p(1) :- [b])).
rule((q(X) :- [p(X)])).
rule((r :- [a,s])).
