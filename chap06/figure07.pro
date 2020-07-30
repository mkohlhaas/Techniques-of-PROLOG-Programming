% FIGURE 6.7: Intersection and difference of sets
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- intersection([1,2,3,4],[2,4,6],L).
%   ?- intersection([5,a,6,b],[1,2,a,6],L).
%   ?- intersection([1,2,3],[4,5,6],L).
%   ?- difference([1,2,3,4],[2,4,6],L).
%   ?- difference([2,4,6],[1,2,3,4],L).
%   ?- difference([2,4,6],[1,2,3,4,6],L).
% ---------------------------------------------------
intersection(L1,L2,L) :-
     setof(X,(member(X,L1),member(X,L2)),L),!.
intersection(_,_,[]).

difference(L1,L2,L) :-
     setof(X,(member(X,L1), not(member(X,L2))),L),!.
difference(_,_,[]).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
