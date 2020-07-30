% PROBLEM 4.1: Another bubble-sort
% ---------------------------------------------------
% consult this program and enter the following query:
%   ?- bubble([3,2,1],L).
% use ; to test if the program terminates properly.
% Now, remove the cut from the program and repeat
% the above query, using ; to request backtracking.
% Observe and explain the results.
% ---------------------------------------------------
bubble(L,L1) :-
    append(A,[X,Y|B],L), X > Y,!,
    append(A,[Y,X|B],L2),write(L2),nl,
    bubble(L2,L1).
bubble(L,L).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).
