% FIGURE 4.4: The program bubble-sort using cut
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- bubble_sort([5,4,3,1,2],L).
%   ?- bubble_sort([7,3,5,1,8,6,2,4],L).
%   ?- bubble_sort([1],L).
%   ?- bubble_sort([],L).
% ---------------------------------------------------
bubble_sort(L,L1) :- swap(L,L2),!,
                     bubble_sort(L2,L1).
bubble_sort(L,L).

swap([X,Y|R],[X|T]) :-
    X =< Y,!,swap([Y|R],T).
swap([X,Y|R],[Y|T]) :-
    X > Y,
    (swap([X|R],T),!; T = [X|R]).
