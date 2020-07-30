% FIGURE 2.6: Program for the factorial relation
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- factorial(7,F).
%   ?- factorial(N,5040).
% The following query will trap the system in an
% infinite loop; press <Ctrl-Break> to escape:
%   ?- factorial(N,5).
% (consult program \chap4\figure6.pro for remedy)
% ---------------------------------------------------
factorial(0,1).
factorial(N,F) :-
    factorial(N1,F1),
    N is N1+1,
    F is N*F1.
