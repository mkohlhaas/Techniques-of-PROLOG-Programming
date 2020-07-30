% FIGURE 3.11: Effect of clauses order
% ---------------------------------------------------------
% Consult this program and try the following query:
%   ?- integer_num(20).
% Now, exchange the clauses then try the above query again.
% Observe the effect (press <Ctrl-Break> to escape).
% ---------------------------------------------------------
integer_num(0).
integer_num(N) :-
    integer_num(M),
    N is M+1.
