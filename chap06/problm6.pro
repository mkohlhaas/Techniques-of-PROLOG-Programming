% PROBLEM 6.6: The common balance procedure for AVL-trees
% -------------------------------------------------------
% No rebalance is required
balance(Change,S,IA,I,NI,NewTree,NewTree-NIA) :-
    subtree_not(Change,I,NI),!, NIA = IA;
    IA =:= 0,!,     NIA = S;
    IA + S =:= 3,!, NIA = 0.

% Rebalance by right-rotation
balance(_,1,1,_,_,bt(bt(LB,B,RB)-IB,A,RA),
                  bt(LB,B,bt(RB,A,RA)-NIA)-NIB) :-
    IB =:= 1,!, NIA = 0, NIB = 0;
    IB =:= 0,!, NIA = 1, NIB = 2.

% Rebalance by left-right rotation
balance(_,1,1,_,_,bt(bt(LB,B,bt(LC,C,RC)-IC)-2,A,RA),
                  bt(bt(LB,B,LC)-IB,C,bt(RC,A,RA)-IA)-0) :-
    IC =:= 0, IA = 0, IB = 0,!;
              IA is (IC+1) mod 3,
              IB is (IA+1) mod 3.

% Rebalance by left-rotation
balance(_,2,2,_,_,bt(LA,A,bt(LB,B,RB)-IB),
                  bt(bt(LA,A,LB)-NIA,B,RB)-NIB) :-
    IB =:= 2,!, NIA = 0, NIB = 0;
    IB =:= 0,!, NIA = 2, NIB = 1.

% Rebalance by right-left rotation
balance(_,2,2,_,_,bt(LA,A,bt(bt(LC,C,RC)-IC,B,RB)-1),
                  bt(bt(LA,A,LC)-IA,C,bt(RC,B,RB)-IB)-0) :-
    IC =:= 0, IA = 0,IB = 0,!;
              IB is (IC+1) mod 3,
              IA is (IB+1) mod 3.

subtree_not(grown,I,NI) :-
    NI* (NI-I) =:= 0.
subtree_not(shrunk,NI,I) :-
    I* (NI-1)* (NI-2) =:= 0.
