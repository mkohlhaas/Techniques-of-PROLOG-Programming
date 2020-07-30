% FIGURE 3.15: Program bubble-sort (version 1)
% --------------------------------------------
% consult this program and try the following queries:
%   ?- bubble_sort([5,4,3,1,2],L).
%   ?- bubble_sort([7,3,5,1,8,6,2,4],L).
%   ?- bubble_sort([1],L).
%   ?- bubble_sort([],L).
% ---------------------------------------------------
bubble_sort(L,L1) :-
    swap(L,L2,0), bubble_sort(L2,L1).
bubble_sort(L,L).

swap([X,Y|R],[X|T],S) :- X =< Y,swap([Y|R],T,S).
swap([X,Y|R],[Y|T],_) :- X > Y, swap([X|R],T,1).
swap([X],[X],1).
