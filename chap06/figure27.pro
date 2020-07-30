% FIGURE 6.27: Insertion of items into an AVL-tree
% -------------------------------------------------------
% consult this program and enter the following query to
% construct an AVL-tree from empty by sequentially
% inserting the nodes 9,8,2;1,7,5;10;3,4;6.
%   ?- construct_avl(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10).
% compare the result with the trees shown in Figure 6.28.
% -------------------------------------------------------
construct_avl(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) :-
    insert_avl(9,nil-z,T1),
    insert_avl(8,T1,T2),
    insert_avl(2,T2,T3),
    insert_avl(1,T3,T4),
    insert_avl(7,T4,T5),
    insert_avl(5,T5,T6),
    insert_avl(10,T6,T7),
    insert_avl(3,T7,T8),
    insert_avl(4,T8,T9),
    insert_avl(6,T9,T10).
% --------------------------------------------------
insert_avl(X,nil-z,bt(nil-z,X,nil-z)-0) :- !.
insert_avl(X,bt(L-I,A,R)-IA,NewTree) :-
    X =< A,!,
    insert_avl(X,L-I,NL-NI),
    balance(1,IA,I,NI,bt(NL-NI,A,R),NewTree).
insert_avl(X,bt(L,A,R-I)-IA,NewTree) :-
    X > A,
    insert_avl(X,R-I,NR-NI),
    balance(2,IA,I,NI,bt(L,A,NR-NI),NewTree).

% No rebalance is required
balance(S,IA,I,NI,NewTree,NewTree-NIA) :-
    NI* (NI-I) =:= 0,!, NIA = IA;
    IA =:= 0,!,         NIA = S;
    IA + S =:= 3,!,     NIA = 0.

% Rebalance by right-rotation
balance(1,1,0,1,bt(bt(LB,B,RB)-1,A,RA),
                bt(LB,B,bt(RB,A,RA)-0)-0).

% Rebalance by left-right rotation
balance(1,1,0,2,bt(bt(LB,B,bt(LC,C,RC)-IC)-2,A,RA),
                bt(bt(LB,B,LC)-IB,C,bt(RC,A,RA)-IA)-0) :-
    IC =:= 0,IA = 0,IB = 0,!;
             IA is (IC+1) mod 3,
             IB is (IA+1) mod 3.

% Rebalance by left-rotation
balance(2,2,0,2,bt(LA,A,bt(LB,B,RB)-2),
                bt(bt(LA,A,LB)-0,B,RB)-0).

% Rebalance by right-left rotation
balance(2,2,0,1,bt(LA,A,bt(bt(LC,C,RC)-IC),B,RB)-1,
                bt(bt(LA,A,LC)-IA,C,bt(RC,B,RB)-IB)-0) :-
    IC =:= 0,IA = 0,IB = 0,!;
             IB is (IC+1) mod 3,
             IA is (IB+1) mod 3.
