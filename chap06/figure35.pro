% FIGURE 6.35-36: Insertion of items into a B-tree
% -----------------------------------------------------------
% consult this program and enter the following query to
% construct a B-tree from empty by sequentially inserting
% the keys
%   1,3,5,2,23;9,13,14;10,12,15,16,17;18,19,20;21,22,24.
%   ?- construct_bt(T4,T5,T8,T13,T16,T19).
% compare the result with the trees shown in Figure 6.37.
% -----------------------------------------------------------
construct_bt(T4,T5,T8,T13,T16,T19) :-
    insert_bt(1,[],T1),
    insert_bt(3,T1,T2),
    insert_bt(5,T2,T3),
    insert_bt(2,T3,T4),
    insert_bt(23,T4,T5),
    insert_bt(9,T5,T6),
    insert_bt(13,T6,T7),
    insert_bt(14,T7,T8),
    insert_bt(10,T8,T9),
    insert_bt(12,T9,T10),
    insert_bt(15,T10,T11),
    insert_bt(16,T11,T12),
    insert_bt(17,T12,T13),
    insert_bt(18,T13,T14),
    insert_bt(19,T14,T15),
    insert_bt(20,T15,T16),
    insert_bt(21,T16,T17),
    insert_bt(22,T17,T18),
    insert_bt(24,T18,T19).
% ------------------------------------------------------
insert_bt(X,BT,BT1) :-
    leaf(BT),!,
    insert(X,BT,BT2),
    adjust(BT2,BT1).

insert_bt(X,BT,BT1) :-
    append(A,[L,Y,R|C],BT),(X =< Y,!; C = []),
    (X =< Y,!, insert_bt(X,L,L1),
               install(A,[L1,Y,R|C],BT1);
     C = [],   insert_bt(X,R,R1),append(A,[L,Y],A1),
               install(A1,[R1],BT1)).

install(A,[ST|B],BT1) :-
    grown(ST,L,X,R),!,
               append(A,[L,X,R|B],BT2),
               adjust(BT2,BT1);
               append(A,[ST|B],BT1).

adjust(BT,[L,X,R]) :-
    overflow(BT,N),
    append(L,[X|R],BT),length(L,N),!.
adjust(BT,BT).

overflow(BT,M) :-
    order(N),length(BT,K),
    (leaf(BT),!, K > 2*N; K > 4*N+1),
    M is K // 2.

grown([[X|Y],Z,[U|V]],[X|Y],Z,[U|V]).

order(2).

leaf([[_|_]|_]) :- !,fail.
leaf(_).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

insert(X,[],[X]).
insert(X,[H|T],[X,H|T]) :- X =< H.
insert(X,[H|T],[H|T1]) :-
    X > H, insert(X,T,T1).

% length([],0).
% length([_|T],N) :- length(T,M), N is M+1.
