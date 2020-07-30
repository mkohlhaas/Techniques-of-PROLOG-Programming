% FIGURE 6.14: The invertible procedure sum
%----------------------------------------------------------------
% consult this program and try the following queries:
%   ?- sum(4,8,S).
%   ?- sum(X,8,12).
%   ?- sum(4,Y,12).
%   ?- sum(X,Y,12).
% for the last query above, use ; to request alternative answers.
%----------------------------------------------------------------
    sum(X,Y,S) :-
        var(S), S is X+Y;
        nonvar(S),add(X,Y,S).

    add(0,S,S).
    add(X,Y,S) :-
        add(X1,Y1,S),
        X is X1+1, Y is Y1-1.



