% PROBLEM 3.2: Reverse a list
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- reverse([s,e,v,e,r],[],L).
%   ?- reverse([t,a,e,3,2,1],[],L).
% ---------------------------------------------------
reverse([],R,R).
reverse([H|T],R,L1) :-
    reverse(T,[H|R],L1).
