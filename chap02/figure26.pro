% FIGURE 2.26: Program selection-sort (version 1)
% -------------------------------------------------
% consult this program and try the following query:
%   ?- selection_sort([4,5,2,3,1],L).
% -------------------------------------------------
selection_sort([],[]).
selection_sort(L,[H|T]) :-
    least(H,L,R), selection_sort(R,T).

least(X,L,R) :-
    select(X,L,R), smaller(X,R).

smaller(_,[]).
smaller(X,[H|T]) :-
    X =< H, smaller(X,T).

select(H,[H|T],T).
select(X,[H|T],[H|T1]) :- select(X,T,T1).
