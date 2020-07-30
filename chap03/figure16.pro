% FIGURE 3.16: Program quick-sort
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- sort_list([5,4,3,1,2],L).
%   ?- sort_list([7,3,5,1,8,6,2,4],L).
%   ?- sort_list([1],L).
%   ?- sort_list([],L).
% ---------------------------------------------------
sort_list(L,L1) :- quick_sort(L,[],L1).

quick_sort([],R,R).
quick_sort([H|T],R,L1) :-
    partition(T,Lt,H,Gt),
    quick_sort(Gt,R,GtsR),
    quick_sort(Lt,[H|GtsR],L1).

partition([],[],_,[]).
partition([H|T],[H|TLt],X,Gt) :-
    H =< X,partition(T,TLt,X,Gt).
partition([H|T],Lt,X,[H|TGt]) :-
    H > X, partition(T,Lt,X,TGt).
