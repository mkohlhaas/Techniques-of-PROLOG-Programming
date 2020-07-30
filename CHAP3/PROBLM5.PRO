% PROBLEM 3.5: The cryptarithmetic problem
%----------------------------------------------------
% consult this program and try the following queries:
%   ?- solve([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]).
%   ?- solve([S,E,M,I],[F,A,C,E],[F,A,M,E,D]).
%   ?- solve([G,A,R,Y],[L,I,Z,A],[L,I,R,A,S]).
%----------------------------------------------------
    solve([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]) :-
        M = 1, L = [0,2,3,4,5,6,7,8,9],
        select(S,L,L1), S > 0, (C3 = 0; C3 = 1),
        O is S+M+C3-10*M, select(O,L1,L2),
        select(E,L2,L3), (C2 = 0; C2 = 1),
        N is E+O+C2-10*C3, select(N,L3,L4),
        (C1 = 0; C1 = 1),
        R is E+10*C2-(N+C1), select(R,L4,L5),
        select(D,L5,L6),
        Y is D+E-10*C1, select(Y,L6,_).

    select(X,[X|T],T).
    select(X,[H|T],[H|R]) :- select(X,T,R).



