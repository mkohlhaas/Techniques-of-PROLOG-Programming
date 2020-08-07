% FIGURE 2.5: Program for the factorial function
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- factorial(3,X).
%   ?- factorial(4,X).
%   ?- factorial(5,X).
%   ?- factorial(6,X).
%   ?- factorial(7,X).
% ---------------------------------------------------
factorial(0,1).
factorial(N,F) :-
    N > 0, N1 is N-1,
    factorial(N1,F1),
    F is N*F1.
