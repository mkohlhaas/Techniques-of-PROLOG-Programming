% PROBLEM 2.4: A tail-recursive program for the factorial function
%-----------------------------------------------------------------
% This file contains a tail-recursive program for factorial.
% Consult this program and try the following queries:
%   ?- factorial(7,X).
%   ?- factorial(15,X).
%   ?- factorial(0,X).
%   ?- factorial(1,X).
%-----------------------------------------------------------------
    factorial(N,F) :- fact(N,1,F).

    fact(0,T,T).
    fact(N,T,F) :-
        N > 0,
        N1 is N-1,
        T1 is N*T,
        fact(N1,T1,F).


