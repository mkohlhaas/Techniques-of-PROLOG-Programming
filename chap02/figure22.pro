% FIGURE 2.22: Program sublist
% ---------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- sublist([b,c],[a,b,c,d]).
%   ?- sublist(X,[a,b,c,d]).
% ---------------------------------------------------
sublist(X,L) :-
    append(L1,_,L),
    append(_,X,L1).

append([],L,L).
append([H|T1],L2,[H|T]) :-
    append(T1,L2,T).
