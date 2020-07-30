% FIGURE 10.5: A definite clause grammar that generates logical forms
%--------------------------------------------------------------------
% consult this file and try the following queries to parse sentences
% and to obtain the logical formulas representing the sentences:
%   ?- sentence(S,[which,agents,supply,most,items],[]).
%   ?- sentence(S,[few,agents,supply,all,items],[]).
%   ?- sentence(S,[most,agents,supply,few,items],[]).
%   ?- sentence(most(item:Y,
%      supply(agent:'Adams & Sons',item:Y)),Ans,[]).
%--------------------------------------------------------------------
:- op(500,xfy,:).

    sentence(S) -->
        noun_phrase(P,S,E,VP),verb_phrase(P,E,VP).

    noun_phrase(P,NP,E,F) -->
        determiner(P,NP,E,F),noun(P,E).
    noun_phrase(P,F,E,F) -->
        noun(P,E),proper_noun(P,E).

    verb_phrase(P,X,VP) -->
        verb(P,F,X,Y),noun_phrase(_,VP,Y,F).
    verb_phrase(P,X,VP) -->
        neg_verb(P,F,X,Y),noun_phrase(_,VP,Y,F).

    determiner(P,all(E,F),E,F) -->
        [W],{once(member(W:P,
        [all:plural,every:sgular,each:sgular]))}.
    determiner(P,exist(E,F),E,F) -->
        [W],{member(W,[some,which])}.
    determiner(P,no(E,F),E,F) --> [no].
    determiner(plural,A,E,F) -->
        [W],{member(W,[most,few]), A =.. [W,E,F]}.

    noun(sgular,T:X) -->
        [T],{member(T,[agent,item])}.
    noun(plural,agent:X) --> [agents].
    noun(plural,item:X) -->  [items].

    proper_noun(sgular,T:X) -->
        [X],{proper_name(T:X)}.

    verb(sgular,supply(X,Y),X,Y) --> [supplies].
    verb(plural,supply(X,Y),X,Y) --> [supply].
    neg_verb(P,non(F),X,Y) -->
        negation(P),verb(plural,F,X,Y).

    negation(sgular) --> [does,not].
    negation(plural) --> [do,not].

    proper_name(N) :- constsymbol(N).

    once(P) :- P,!.

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    constsymbol(agent:'Adams & Sons').

%--------------------------------------------------------------------



