% PROBLEM 2.3: Greatest Common Divisor
%----------------------------------------------------
% consult this program and try the following queries:
%   ?- gcd(6,15,Z).
%   ?- gcd(15,40,Z).
%   ?- gcd(675,3195,Z).
%   ?- gcd(1160,1015,Z).
%   ?- gcd(0,5,Z).
%   ?- gcd(0,0,Z).
%----------------------------------------------------
    gcd(X,0,X) :- X > 0.
    gcd(X,Y,Z) :- X < Y, gcd(Y,X,Z).
    gcd(X,Y,Z) :- X >= Y, Y > 0, X1 is X mod Y,
                  gcd(Y,X1,Z).



