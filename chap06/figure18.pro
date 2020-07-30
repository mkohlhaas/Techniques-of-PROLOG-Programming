% FIGURE 6.18: The program of topological sort
% ---------------------------------------------------
% consult this program and enter the following query:
%   ?- graph(G),topo_sort(G,L).
% ---------------------------------------------------
:- op(500,xfy,:).

topo_sort(Graph,List) :-
    createq(Graph,Queue),
    t_sort(Graph,Queue,List).

createq(G,Q-Qt) :-
    setof(X,minimal(X,G),L),
    append(L,Qt,Q).

minimal(X,G) :-
    member(X:_,G),not(member(_:X,G)).

t_sort([],[]-[],[]) :- !.
t_sort(G,[X|Q]-Qt,[X|L]) :-
    find_successors(X,G,G1,Qt-Qs),
    t_sort(G1,Q-Qs,L).

find_successors(X,G,G1,Q-Qt) :-
    select(X:Y,G,G2),!,
    (member(_:Y,G2),!, Q = Q1; Q = [Y|Q1]),
    find_successors(X,G2,G1,Q1-Qt).
find_successors(_,G,G,Q-Q).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

select(X,[X|T],T).
select(X,[H|T],[H|R]) :- select(X,T,R).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

graph([ics:is1,dma:ps2,dma:se1,dsa:ps2,dsa:se1,
   dsa:co1,is1:is2,is1:ps2,is1:se1,co1:co2,is2:pm,
   is2:is3,ps2:is3,co2:os,pm:cp,is3:cp,os:cp]).
