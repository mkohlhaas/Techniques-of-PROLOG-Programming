% FIGURE 3.2: A program to solve the n-queens problem
%----------------------------------------------------
% consult this program and try the following queries:
%   ?- queens(4,S).
%   ?- queens(8,S).
%----------------------------------------------------
    queens(N,S) :-
        make_list(N,L),
        move_safe(L,[],S).

    make_list(0,[]).
    make_list(N,[N|T]) :-
        N > 0,M is N-1,
        make_list(M,T).

    move_safe([],S,S).
    move_safe(L,Qs,S) :-
        select(Q,L,R),
        safe(Q,1,Qs),
        move_safe(R,[Q|Qs],S).

    select(X,[X|T],T).
    select(X,[H|T],[H|T1]) :-
        select(X,T,T1).

    safe(_,_,[]).
    safe(Q,D,[Q1|Qs]) :-
        Q+D =\= Q1, Q-D =\= Q1,
        D1 is D+1, safe(Q,D1,Qs).



