% PROBLEM 2.5: A program for the Fibonacci function (version 1)
% -------------------------------------------------------------
% consult this program and try the following queries:
% (the last two queries may cause stack-overflow)
%   ?- fibonacci(6,F).
%   ?- fibonacci(14,F).
%   ?- fibonacci(20,F).
%   ?- fibonacci(40,F).
% -------------------------------------------------------------
fibonacci(0,0).
fibonacci(1,1).
fibonacci(N,F) :-
    N > 1,
    N1 is N-1, N2 is N-2,
    fibonacci(N1,F1),
    fibonacci(N2,F2),
    F is F1+F2.

% PROBLEM 2.5: An efficient program for Fibonacci function (version 2)
%---------------------------------------------------------------------
% consult this program and try the following queries, observing
% the difference in execution time and space in comparison with
% the above program:
%   ?- fibonacci(6,_,F).
%   ?- fibonacci(14,_,F).
%   ?- fibonacci(20,_,F).
%   ?- fibonacci(40,_,F).
%---------------------------------------------------------------------
fibonacci(0,_,0).
fibonacci(1,0,1).
fibonacci(N,F1,F) :-
    N > 1, N1 is N-1,
    fibonacci(N1,F2,F1),
    F is F1+F2.
