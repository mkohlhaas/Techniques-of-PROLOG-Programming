% PROBLEM 3.4: Procedural program for the next-higher permutation
% ---------------------------------------------------------------
% consult this program and try the following query:
%   ?- next_higher_perm([1,2,3,5,4,2],N).
%   ?- next_higher_perm([7,1,0,4,1,0],N).
%   ?- next_higher_perm([6,5,2,9,7,5,3,1],N).
% ---------------------------------------------------------------
next_higher_perm(L,L1) :-
    reverse(L,[],L2),
    append(A,[X,Y|B],L2),X > Y,
    append(A,[X],C),
    append(A1,[U|B1],C), U > Y,
    append(A1,[Y|B1],B2),
    reverse([U|B],B2,L1).

reverse([],L,L).
reverse([H|T],L,R) :-
    reverse(T,[H|L],R).

append([],L,L).
append([H|T],L,[H|R]) :-
    append(T,L,R).
