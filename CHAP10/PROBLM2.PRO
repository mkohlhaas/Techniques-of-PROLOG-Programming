% PROBLEM 10.2: Converion of a DCG into a Prolog-based parser
%--------------------------------------------------------------------
% This file contains a procedure that converts a DCG into a Prolog
% program that can be used as a parser for the defined grammar.
% Consult this file and try the following queries, then inpect the
% answers to check the results of conversion:
%   ?- sample(1,Rule),convert_rule(Rule,Clause).
%   ?- sample(2,Rule),convert_rule(Rule,Clause).
%   ?- sample(3,Rule),convert_rule(Rule,Clause).
%   ?- sample(4,Rule),convert_rule(Rule,Clause).
%   ?- sample(5,Rule),convert_rule(Rule,Clause).
%--------------------------------------------------------------------
    consult_parser(Filename) :-
        see(Filename),
        read_rules,
        seen.

    read_rules :-
        read(Rule),
        convert_rule(Rule,Clause),
        assert(Clause),
        read_rules.
    read_rules.

    convert_rule((A --> B),(A1)) :-
        append(B,Y,X),!,
        add_arguments(A,[X,Y],A1).
    convert_rule((A --> B),(A1 :- B1)) :-
        add_arguments(A,[X,Y],A1),
        convert_goals(B,[X,Y],B1).

    convert_goals((A,As),[X,Y],B) :-
        append(As,Y,Z),!,
        add_arguments(A,[X,Z],B).
    convert_goals((A,As),[X,Y],Bs) :-
        append(A,Z,X),!,
        convert_goals(As,[Z,Y],Bs).
    convert_goals((A,As),[X,Y],(B,Bs)) :- !,
        add_arguments(A,[X,Z],B),
        convert_goals(As,[Z,Y],Bs).
    convert_goals(A,[X,Y],B) :-
        add_arguments(A,[X,Y],B).

    add_arguments({A},[X,X],A) :- !.
    add_arguments(A,[X,Y],B) :-
        A =.. [P|Xs], append(Xs,[X,Y],Ys),
        B =.. [P|Ys].

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

% Sample rules for conversion
%--------------------------------------------------------------------
    sample(1,(noun_phrase(P,G) --> noun_part(P,G),qualify_part)).
    sample(2,(noun(p,m) --> [jardins])).
    sample(3,(determiner(s,m) --> [W],{member(W,[le,mon,un])})).
    sample(4,(question --> phrase,['?'])).
    sample(5,(product --> ['('],polynom,[')','*','('],polynom,[')'])).

%--------------------------------------------------------------------




