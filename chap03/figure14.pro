% FIGURE 3.14: Program insertion-sort
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- insertion_sort([5,4,3,1,2],L).
%   ?- insertion_sort([7,3,5,1,8,6,2,4],L).
%   ?- insertion_sort([1],L).
%   ?- insertion_sort([],L).
% ---------------------------------------------------
insertion_sort([],[]).
insertion_sort([H|T],L1) :-
    insertion_sort(T,L2),
    insert(H,L2,L1).

insert(X,[],[X]).
insert(X,[H|T],[X,H|T]) :- X =< H.
insert(X,[H|T],[H|T1]) :-
    X > H, insert(X,T,T1).
