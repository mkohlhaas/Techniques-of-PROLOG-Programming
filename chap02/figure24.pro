% FIGURE 2.24: Programs setequal and permutation
% ---------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- subset(S,[a,b,c]),setequal(S,[c,a]).
%   ?- permutation([1,2,3,4,5],L).
% ---------------------------------------------------
setequal(L,L1) :- permutation(L,L1).

permutation([],[]).
permutation(L,[H|T]) :-
    select(H,L,R),permutation(R,T).

select(H,[H|T],T).
select(X,[H|T],[H|T1]) :- select(X,T,T1).

subset(S,[H|T]) :-
    subset(R,T),
    (S = R; S = [H|R]).
subset([],[]).
