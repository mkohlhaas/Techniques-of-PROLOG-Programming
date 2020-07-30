% FIGURE 6.29-30: Deletion of items from an AVL-tree
%--------------------------------------------------------
% consult this program and enter the following query to
% demolish an AVL-tree by sequentially deleting the nodes
%      9;8;10,11,12;13;5,6,7;1;3,4,2.
%   ?- tree(T), demolish_avl(T,T1,T2,T5,T6,T9,T10,T13).
% compare the result with the trees shown in Figure 6.31.
%--------------------------------------------------------
    demolish_avl(T,T1,T2,T5,T6,T9,T10,T13) :-
        delete_avl(9,T,T1),
        delete_avl(8,T1,T2),
        delete_avl(10,T2,T3),
        delete_avl(11,T3,T4),
        delete_avl(12,T4,T5),
        delete_avl(13,T5,T6),
        delete_avl(5,T6,T7),
        delete_avl(6,T7,T8),
        delete_avl(7,T8,T9),
        delete_avl(1,T9,T10),
        delete_avl(3,T10,T11),
        delete_avl(4,T11,T12),
        delete_avl(2,T12,T13).
%---------------------------------------------------------------
    delete_avl(X,bt(L,X,nil-z)-IX,L) :- !.
    delete_avl(X,bt(nil-z,X,R)-IX,R) :- !.
    delete_avl(X,bt(L,X,R-I)-IX,NewTree) :- !,
        delete_least(Y,R-I,NR-NI),
        balance2(1,IX,NI,I,bt(L,Y,NR-NI),NewTree).

    delete_avl(X,bt(L-I,A,R)-IA,NewTree) :-
        X < A,!,
        delete_avl(X,L-I,NL-NI),
        balance2(2,IA,NI,I,bt(NL-NI,A,R),NewTree).

    delete_avl(X,bt(L,A,R-I)-IA,NewTree) :-
        X > A,
        delete_avl(X,R-I,NR-NI),
        balance2(1,IA,NI,I,bt(L,A,NR-NI),NewTree).

    delete_least(Y,bt(nil-z,Y,R)-IY,R) :- !.
    delete_least(Y,bt(L-I,A,R)-IA,NewTree) :-
        delete_least(Y,L-I,NL-NI),
        balance2(2,IA,NI,I,bt(NL-NI,A,R),NewTree).

    % No rebalance is required
    balance2(S,IA,NI,I,NewTree,NewTree-NIA) :-
        I* (NI-1)* (NI-2) =:= 0,!, NIA = IA;
        IA =:= 0,!, NIA = S;
        IA + S =:= 3,!, NIA = 0.

    % Rebalance by right-rotation
    balance2(1,1,_,_,bt(bt(LB,B,RB)-IB,A,RA),
                     bt(LB,B,bt(RB,A,RA)-NIA)-NIB) :-
        IB =:= 1,!, NIA = 0, NIB = 0;
        IB =:= 0,!, NIA = 1, NIB = 2.

    % Rebalance by left-right rotation
    balance2(1,1,_,_,bt(bt(LB,B,bt(LC,C,RC)-IC)-2,A,RA),
                     bt(bt(LB,B,LC)-IB,C,bt(RC,A,RA)-IA)-0) :-
        IC =:= 0,!, IA = 0, IB = 0;
                    IA is (IC+1) mod 3,
                    IB is (IA+1) mod 3.

    % Rebalance by left-rotation
    balance2(2,2,_,_,bt(LA,A,bt(LB,B,RB)-IB),
                     bt(bt(LA,A,LB)-NIA,B,RB)-NIB) :-
        IB =:= 2,!, NIA = 0, NIB = 0;
        IB =:= 0,!, NIA = 2, NIB = 1.

    % Rebalance by right-left rotation
    balance2(2,2,_,_,bt(LA,A,bt(bt(LC,C,RC)-IC,B,RB)-1),
                     bt(bt(LA,A,LC)-IA,C,bt(RC,B,RB)-IB)-0) :-
        IC =:= 0,!, IA = 0, IB = 0;
                    IB is (IC+1) mod 3,
                    IA is (IB+1) mod 3.

    tree(bt(bt(bt(bt(nil-z,1,nil-z)-0,
                  2,
                  bt(nil-z,3,nil-z)-0)-0,
               4,
               bt(bt(nil-z,5,bt(nil-z,6,nil-z)-0)-2,
                  7,
                  bt(nil-z,8,nil-z)-0)-1)-2,
            9,
            bt(bt(nil-z,10,bt(nil-z,11,nil-z)-0)-2,
               12,
               bt(nil-z,13,nil-z)-0)-1)-1).
%----------------------------------------------------------



