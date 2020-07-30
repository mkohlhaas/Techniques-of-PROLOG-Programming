% PROBLEM 7.6: The Travelling Salesman problem
%---------------------------------------------------------------
% Consult this program and enter the following query to find a
% travelling plan with minimum cost for the travelling salesman:
%   ?- salesman(Journey,Cost).
% Try different problem by using the following queries:
%   ?- abolish(table/3),consult('c:\chap7\sales1.dat').
%   ?- salesman(Journey,Cost).
% Repeat with the files sales2.dat (9 cities) and sales3.dat
% (10 cities). Beware, the last two runs may take a little long.
%---------------------------------------------------------------
    initial_state((Table,nil,Value),Value) :-
        cost_table(N,T),
        reduce_cost(N,T,Table,Value).

    final_state(([R1,R2],_,_),go(I,J),go(H,K)) :-
        member(e(I,J,0),R1),member(e(H,K,0),R2),
        I \= H, J \= K.

    next_state((T,_,_),(T1,A,V),Path) :-
        best_arc(T,Row,Col),
        branch(T,Row,Col,Path,(T1,A,V)).

    best_arc(T,(I0,CI),(J0,CJ)) :-
        setof((C,(I,M),(J,N)),(table_entry(T,I,J,0),
               min_cost(T,(I,M),(J,N)),C is 10000 - M - N),
               [(_,(I0,CI),(J0,CJ))|_]).

    min_cost(T,(I,M),(J,N)) :-
        setof(C,(K,C)^ (table_entry(T,I,K,C),K \= J),[M|_]),
        setof(C,(H,C)^ (table_entry(T,H,J,C),H \= I),[N|_]).

    table_entry(Table,I,J,C) :-
        member(Row,Table),member(e(I,J,C),Row).

    branch(T,(I,CI),(J,CJ),_,(Table,no(I,J),Cost)) :-
        setof(R1,R^ (member(R,T),exclude((I,CI),(J,CJ),R,R1)),
              Table), Cost is CI + CJ.
    branch(T,(I,_),(J,_),Path,(Table,go(I,J),Cost)) :-
        setof(R1,R^ (member(R,T),include(I,J,Path,R,R1)),
              Table1), length(Table1,N),
              reduce_cost(N,Table1,Table,Cost).

    exclude(Row,Col,R,R1) :-
        setof(e(H,K,C1),C^ (member(e(H,K,C),R),
              adjust(Row,Col,H,K,C,C1)),R1).

    adjust((I,_),(J,_),I,J,_,10000) :- !.
    adjust((I,CI),_,I,J,C,C1) :- !, C1 is C - CI.
    adjust(_,(J,CJ),I,J,C,C1) :- !, C1 is C - CJ.
    adjust(_,_,_,_,C,C).

    include(I,J,Path,R,R1) :-
        setof(e(H,K,C1),C^ (member(e(H,K,C),R),H \= I,K \= J,
              change(Path,I,J,H,K,C,C1)),R1).

    change(Path,I,J,H,K,0,10000) :-
        rearrange(go(H,K),[go(I,J)|Path],_,go(_,H)),!.
    change(_,_,_,_,_,C,C).

    rearrange(go(I,J),L,[go(J,K)|L1],Last) :-
        select(go(J,K),L,L2),!,rearrange(go(J,K),L2,L1,Last).
    rearrange(Last,_,[],Last).

    select(H,[H|T],T).
    select(X,[H|T],[H|T1]) :- select(X,T,T1).

    cost_table(N,Table) :-
        make_table(Table),
        length(Table,N).

    reduce_cost(N,T,Table,Cost) :-
        reduce_rows(T,T1,0,V),transpose(N,T1,T2),
        reduce_rows(T2,T3,V,Cost),transpose(N,T3,Table).

    reduce_rows([R|Rs],[R1|Rs1],V,V1) :-
        (member(e(_,_,0),R),!, R = R1, V = V2;
         setof(C,(I,J)^member(e(I,J,C),R),[C0|_]),
         setof(e(I,J,C1),
           C^ (member(e(I,J,C),R),C1 is C - C0), R1),
         V2 is V + C0),
        reduce_rows(Rs,Rs1,V2,V1).
    reduce_rows([],[],V,V).

    make_table(Table) :-
        setof(Row,
              H^setof(e(I,J,X),(table(H,J,X),I = H),Row),
              Table).

    transpose(N,Table,Table1) :-
        setof(Col,(M,X,Row)^
                  (nth_member(M,X,Row),
                   (M > N,!,fail;
                    setof(X,Row^member(Row,Table),Col))),
             Table1).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    nth_member(1,X,[X|T]).
    nth_member(N,X,[Y|T]) :- nth_member(M,X,T), N is M+1.

%   length([],0).
%   length([H|T],N) :-
%       length(T,M), N is M+1.


% The table of travelling cost between cities
%---------------------------------------------------------
    table(1,1,10000).         table(4,1,21).
    table(1,2,27).            table(4,2,16).
    table(1,3,43).            table(4,3,25).
    table(1,4,16).            table(4,4,10000).
    table(1,5,30).            table(4,5,18).
    table(1,6,26).            table(4,6,18).
    table(2,1,7).             table(5,1,12).
    table(2,2,10000).         table(5,2,46).
    table(2,3,16).            table(5,3,27).
    table(2,4,1).             table(5,4,48).
    table(2,5,30).            table(5,5,10000).
    table(2,6,25).            table(5,6,5).
    table(3,1,20).            table(6,1,23).
    table(3,2,13).            table(6,2,5).
    table(3,3,10000).         table(6,3,5).
    table(3,4,35).            table(6,4,9).
    table(3,5,5).             table(6,5,5).
    table(3,6,0).             table(6,6,10000).


% The program best-cost-search for the Travelling Salesman
%---------------------------------------------------------
    salesman(Journey,Cost) :-
        best_cost_search([Cost|Journey]).

    best_cost_search(AnsPath) :-
        initial_state(Init,V),
        best_cost([[V,Init]],AnsPath).

    best_cost([[C,S|Path]|_],[C|AnsPath]) :-
        final_state(S,A,B),!,
        rearrange(_,[A,B|Path],AnsPath,_).
    best_cost([Path|Rest],AnsPath) :-
        expand(Path,NPaths),
        merge(NPaths,Rest,NewList),
        best_cost(NewList,AnsPath).

    expand([C,S|Path],NPaths) :-
        setof([C1,(T,A,V),A|Path],
              (extend([S|Path],(T,A,V)),
               C1 is C + V), NPaths),!.
    expand(_,[]).

    extend([S|Path],(T,A,V)) :-
        next_state(S,(T,A,V),Path),
        not(member(A,Path)).

    merge([],L,L) :- !.
    merge(L,[],L) :- !.
    merge([X|P],[Y|Q],[X|R]) :-
        less(X,Y),!,merge(P,[Y|Q],R).
    merge(L,[Y|Q],[Y|R]) :-
        merge(L,Q,R).

    less([V1|Path1],[V2|Path2]) :- V1 < V2.

%-----------------------------------------------------------



