% FIGURE 2.27: Efficient selection-sort (version 2)
% -------------------------------------------------
% consult this program and try the following query:
%   ?- selection_sort([4,5,2,3,1],L).
% -------------------------------------------------
selection_sort([],[]).
selection_sort(L,[H|T]) :-
    least(H,L,R), selection_sort(R,T).

least(X,[X],[]).
least(X,[H|T],R) :-
    least(Y,T,S),
    (H =< Y,(X,R) = (H,T);
     H > Y, (X,R) = (Y,[H|S])).
