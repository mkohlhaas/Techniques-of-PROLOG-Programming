% PROBLEM 5.2: The knight-tour
%---------------------------------------------------------------
% consult this program and invoke the system by using the query:
%   ?- knight_tour.
% then enter the number 5. for a 5 by 5 chess-board,
% and position 1+1. to start the kight in the top-left square.
% The system will display the change of the chess-board
% during its search for a solution. Be patient, it may take
% a long while to find a solution. When the search is complete,
% try again with the knight start at position 3+3.
%---------------------------------------------------------------
    knight_tour :-
        read_information(N,X,Y),
        find_knight_tour(N,X,Y,Board),
        print_board(Board).

    read_information(N,X,Y) :-
        nl,write('Enter board size <n.>: '),
        read(N),
        nl,write('Enter knight position <X + Y.> : '),
        read(X+Y).

    find_knight_tour(N,X,Y,Board) :-
        make_array(0,N,Board),
        array_entry(Board,X,Y,1),
        find_moves(1,N,X,Y,Board).

    make_array(N,N,[]) :- !.
    make_array(I,N,[[I1|R]|Rs]) :-
        I < N, I1 is I+1,
        make_row(0,N,R),
        make_array(I1,N,Rs).

    make_row(N,N,[]) :- !.
    make_row(I,N,[(I1,X)|Rest]) :-
        I < N, I1 is I+1,
        make_row(I1,N,Rest).

    array_entry(A,I,J,X) :- !,
        member([I|Row],A),
        member((J,X),Row).

    find_moves(K,N,X,Y,Board) :- K =:= N*N,!.
    find_moves(K,N,X,Y,Board) :-
        print_board(Board),
        K1 is K+1, next_move(X,Y,X1,Y1,N),
        array_entry(Board,X1,Y1,K1),
        find_moves(K1,N,X1,Y1,Board).

    next_move(X,Y,X1,Y1,N) :-
        move(A,B), X1 is X+A, Y1 is Y+B,
        legal(X1,Y1,N).

    legal(X,Y,N) :-
        1 =< X, X =< N, 1 =< Y, Y =< N.

    move(-1,2).            move(-2,1).
    move(-2,-1).           move(-1,-2).
    move(1,-2).            move(2,-1).
    move(2,1).             move(1,2).

% Print the solution
%-----------------------------------------
    print_board([]) :- !,nl.
    print_board([[_|Row]|Rest]) :-
        print_row(Row),
        print_board(Rest).

    print_row([]) :- nl.
    print_row([H|T]) :-
        print_move(H),print_row(T).

    print_move((_,M)) :-
        var(M),!,write('.  ');
        write(M),write(' '),
        (M < 10,write(' '),!;true).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

%----------------------------------------



