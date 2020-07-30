% PROBLEM 7.8: Program best-first-search without dynamic queues
% -------------------------------------------------------------
% Consult this program and try the following query to obtain a
% route from one place to another in a city that is described
% by the traffic roads map given at the end of this file:
%   ?- best_first_search(AnsPath).
% -------------------------------------------------------------
best_first_search(AnsPath) :-
    initial_state(Init),value(Init,Value),
    assert(store([Value,Init])),
    best_first(AnsPath).

best_first(AnsPath) :-
    pick_best(Path),
    extend(Path,NewState),
    final_state(NewState),
    reverse([NewState|Path],[],AnsPath).

extend([S|Path],S1) :-
    next_state(S,S1),
    not(member_state(S1,[S|Path])),
    value(S1,V1),
    assert(store([V1,S1,S|Path])).

pick_best(Path) :-
    least_value(V),
    retract(store([V|Path])).
pick_best(Path) :-
    store(_),pick_best(Path).

least_value(V0) :-
    setof(V,Path^store([V|Path]),[V0|_]).

member_state(X,[X|_]).
member_state(X,[_|T]) :- member_state(X,T).

reverse([],L,L).
reverse([H|T],L,R) :- reverse(T,[H|L],R).

% -------------------------------------------------------------
% A database of traffic roads
% -------------------------------------------------------------
initial_state('I').
final_state('F').

next_state(S,S1) :-
    next_corners(S,Corners),
    member(S1,Corners).

next_corners('I',['A','B','N','L']).
next_corners('A',['B','I']).
next_corners('B',['A','C','T']).
next_corners('C',['D','E']).
next_corners('D',['C','E']).
next_corners('E',['D','G']).
next_corners('G',['H','K']).
next_corners('H',['G','K']).
next_corners('K',['H','F']).
next_corners('F',['K','W','M']).
next_corners('W',['T','F']).
next_corners('T',['B','S','V','W']).
next_corners('S',['T','R']).
next_corners('R',['S','Q']).
next_corners('Q',['R','P']).
next_corners('P',['Q','N','U']).
next_corners('U',['P','V']).
next_corners('V',['T','U']).
next_corners('N',['P','I']).
next_corners('L',['I','M']).
next_corners('M',['L','F']).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% ----------------------------------------------------
% An evaluation function for traffic roads
% ----------------------------------------------------
value(S,V) :-
    coord(S,X,Y),wait_time(S,W),
    final_state(F),coord(F,X1,Y1),
    V is W + sqrt((X-X1)*(X-X1) + (Y-Y1)*(Y-Y1)).

coord('I',2,17).          coord('T',7,6).
coord('A',0,15).          coord('S',7,10).
coord('B',2,13).          coord('R',9,10).
coord('C',2,9).           coord('Q',9,8).
coord('D',0,7).           coord('P',11,8).
coord('E',2,6).           coord('U',14,8).
coord('G',2,3).           coord('V',14,6).
coord('H',0,1).           coord('N',11,11).
coord('K',2,0).           coord('L',7,17).
coord('F',10,0).          coord('M',18,0).
coord('W',7,3).

wait_time('K',3) :- !.
wait_time(_,1).
