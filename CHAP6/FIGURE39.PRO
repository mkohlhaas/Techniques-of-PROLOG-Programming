% FIGURE 6.39-40: Deletion of items from a B-tree
%------------------------------------------------------------------
% consult this program and enter the following query to delete the
% keys 8,4,7;6;11;24 in that order from the B-tree given in this
% program:
%   ?- btree(T),delete_btree(T,T1,T2,T3,T4,T5,T6).
% compare the result with the trees shown in Figures 6.38 and 6.41.
%------------------------------------------------------------------
    delete_btree(T,T1,T2,T3,T4,T5,T6) :-
        delete_bt(8,T,T1),
        delete_bt(4,T1,T2),
        delete_bt(7,T2,T3),
        delete_bt(6,T3,T4),
        delete_bt(11,T4,T5),
        delete_bt(24,T5,T6).
%------------------------------------------------------------------
    delete_bt(X,BT,BT1) :-
        leaf(BT),!,
        select(X,BT,BT1).
    delete_bt(X,BT,BT1) :-
        append(A,[L,Y,R|C],BT),(X =< Y,!; C = []),
        (X = Y,!, delete_succ(Z,R,R1),append(A,[L,Z],A1),
                  install2(A1,[R1|C],BT1);
         X < Y,!, delete_bt(X,L,L1),
                  install2(A,[L1,Y,R|C],BT1);
         C = [],  delete_bt(X,R,R1),append(A,[L,Y],A1),
                  install2(A1,[R1],BT1)).

    delete_succ(Z,[Z|R],R) :- atomic(Z),!.
    delete_succ(Z,[L|R],BT) :-
        delete_succ(Z,L,L1),
        install2([],[L1|R],BT).

    install2(A,[ST|B],BT) :-
        underflow(ST),!,
            balance(A,[ST|B],BT);
            append(A,[ST|B],BT).

    underflow(BT) :-
        order(N), length(BT,K),
        (leaf(BT),!, K < N; K < 2*N+1).

    balance(A,[L,X,R|C],BT) :-
        surplus(right,R,Y,Z,_,R2),!,
        balance_right(A,L,X,Y,Z,R2,C,BT).
    balance(A,[R|C],BT) :-
        append(A1,[L,X],A),!,
        (surplus(left,L,Y,Z,L1,L2),!,
            balance_left(A1,L1,L2,Y,Z,X,R,C,BT);
            merge(A1,L,[X|R],C,BT2),adjust(BT2,BT)).
    balance(A,[L,X,R|C],BT) :-
        merge(A,L,[X|R],C,BT2),adjust(BT2,BT).

    balance_right(A,L,X,Y,Z,R,C,BT) :-
        atomic(Y),!,
            merge(A,L,[X],[Y,[Z|R]|C],BT);
            merge(A,L,[X,Y],[Z,R|C],BT).
    balance_left(A,L1,L2,Y,Z,X,R,C,BT) :-
        atomic(Z),!,
            append(A,[L1,Z,[X|R]|C],BT);
            append(A,[L2,Y,[Z,X|R]|C],BT).

    merge(A,L,R,C,BT) :-
        append(L,R,L1),append(A,[L1|C],BT).

    adjust([[X|Y]],[X|Y]) :- !.
    adjust(BT,BT).

    surplus(right,[Y,Z|R],Y,Z,_,R) :-
        have_extra([Y,Z|R]).
    surplus(left,L,Y,Z,L1,L2) :-
        have_extra(L),
        append(L1,[Z],L),append(L2,[Y,Z],L).

    have_extra(BT) :-
        order(N), length(BT,K),
        (leaf(BT),!, K > N; K > 2*N+1).

    order(2).

    leaf([[_|_]|_]) :- !,fail.
    leaf(_).

    select(X,[X|T],T).
    select(X,[H|T],[H|R]) :- select(X,T,R).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

%   length([],0).
%   length([_|T],N) :- length(T,M), N is M+1.

    btree([[[1,2,3],4,[5,6,7,8],9,[10,11,12]],13,
          [[14,15],16,[17,18],19,[20,21],22,[23,24]]]).
%-------------------------------------------------------


