% FIGURE 6.21: Program heap-sort
% ------------------------------------------------------
% Consult this program and try the following queries.
% Note: If your Prolog system already has the builtin
% predicate "argrep" which is defined at the end of this
% program, then use comment symbols % to neutralize it.
%   ?- heap_sort(a(5,4,3,1,2),5,L).
%   ?- heap_sort(a(7,3,5,1,8,6,2,4),8,L).
% ------------------------------------------------------
heap_sort(A,N,A1) :-
    build_heap(A,N,N,A2),
    adjust_heap(A2,N,A1).

build_heap(A,I,N,A1) :-
    I > 0,!, adjust(A,I,N,A2),
    I1 is I-1,
    build_heap(A2,I1,N,A1).
build_heap(A,0,_,A).

adjust_heap(A,J,A1) :-
    J > 1,!, swap(A,1,J,A2),
    J1 is J-1, adjust(A2,1,J1,A3),
    adjust_heap(A3,J1,A1).
adjust_heap(A,1,A).

adjust(A,I,J,A1) :-
    find_larger_child(A,I,J,K,Y),
    arg(I,A,X),
    X < Y,!,swap(A,I,K,A2),adjust(A2,K,J,A1).
adjust(A,_,_,A).

find_larger_child(A,I,J,K,Y) :-
    L is 2*I, R is 2*I+1,
    L =< J, arg(L,A,LC),
    (R =< J,arg(R,A,RC), RC > LC,!,
    K is R, Y is RC; K is L, Y is LC).

swap(A,I,J,A1) :-
    arg(I,A,X),arg(J,A,Y),
    argrep(A,I,Y,A2),argrep(A2,J,X,A1).

argrep(Term,N,Value,NewTerm) :-
    Term =.. [F|L],
    replace(N,L,Value,L1),
    NewTerm =.. [F|L1].

replace(1,[_|L],Y,[Y|L]) :- !.
replace(N,[X|L],Y,[X|L1]) :-
    N > 1, N1 is N-1,
    replace(N1,L,Y,L1).
