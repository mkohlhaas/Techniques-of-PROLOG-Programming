% FIGURE 7.18: Program hill-climbing search
% ------------------------------------------------------
% consult this program and enter the following query to
% find a route from one place to another in the traffic
% roads map given at the end of this file:
%   ?- hill_climb_search(AnsPath).
% compare the result with that produced by depth-first
% search.
% -------------------------------------------------------
hill_climb_search(AnsPath) :-
    initial_state(Init),
    hill_climb([Init],AnsPath).

hill_climb([S|_],[S]) :-
    final_state(S),!.
hill_climb([S|Path],[S|AnsPath]) :-
    extend([S|Path],S1),
    hill_climb([S1,S|Path],AnsPath).

extend([S|Path],S1) :-
    best_next_state(S,S1),
    not(member_state(S1,[S|Path])).

best_next_state(S,S1) :-
    setof((V,NS),(next_state(S,NS),value(NS,V)),List),
    member((_,S1),List).

member_state(X,[X|_]).
member_state(X,[_|T]) :- member_state(X,T).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% A database of city traffic roads
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
