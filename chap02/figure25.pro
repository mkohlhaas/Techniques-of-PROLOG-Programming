% FIGURE 2.25: Program permutation-sort
% -------------------------------------------------
% consult this program and try the following query:
%   ?- permutation_sort([4,5,2,3,1],L).
% -------------------------------------------------
permutation_sort(L,L1) :-
    permutation(L,L1), ordered(L1).

ordered([]).
ordered([_]).
ordered([X,Y|R]) :- X =< Y, ordered([Y|R]).

permutation([],[]).
permutation(L,[H|T]) :-
    select(H,L,R),permutation(R,T).

select(H,[H|T],T).
select(X,[H|T],[H|T1]) :- select(X,T,T1).
