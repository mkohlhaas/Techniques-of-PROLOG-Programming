% FIGURE 3.4: Print a solution of the N-queens problem
%-----------------------------------------------------
% consult this program and try the following queries:
%   ?- solve_queens(4).
%   ?- solve_queens(8).
%-----------------------------------------------------
    solve_queens(N) :-
        queens(N,S),
        print_sol(S,1,N).

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

    print_sol(S,M,N) :- M > N.
    print_sol(S,M,N) :- M =< N,
        print_row(M,S),
        M1 is M+1,
        print_sol(S,M1,N).

    print_row(_,[]) :- nl.
    print_row(M,[Q|Qs]) :-
        (M = Q, write('Q'); M \= Q,write('.')),
        print_row(M,Qs).



