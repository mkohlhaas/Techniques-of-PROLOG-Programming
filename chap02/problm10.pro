% PROBLEM 2.10: Declarative program for the next higher permutation
% -----------------------------------------------------------------
% consult this program and try the following query:
%   ?- next_higher_perm([1,2,3,5,4,2],N).
%   ?- next_higher_perm([7,1,0,4,1,0],N).
%   ?- next_higher_perm([6,5,2,9,7,5,3],N).
% -----------------------------------------------------------------
next_higher_perm(L,L1) :-
    higher_perm(L,L1),
    not(far_higher(L1,L)).

higher_perm(L,L1) :-
    permutation(L,L1),
    higher(L1,L).

higher([H1|_],[H|_]) :- H1 > H.
higher([H|T1], [H|T]) :- higher(T1,T).

far_higher(L1,L) :-
    higher_perm(L,L2),
    higher(L1,L2).

permutation([],[]).
permutation(L,[H|T]) :-
    select(H,L,R),permutation(R,T).

select(H,[H|T],T).
select(X,[H|T],[H|T1]) :- select(X,T,T1).
