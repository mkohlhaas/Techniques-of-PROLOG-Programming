% FIGURE 7.12: Program breadth-first search
% ----------------------------------------------------
% consult this program and enter the following query
% to find a route from one place to another in the
% traffic roads map given at the end of this file:
%   ?- breadth_first_search(AnsPath).
% compare the result with that produced by depth-first
% search, using the query:
%   ?- depth_first_search(AnsPath).
% ----------------------------------------------------
breadth_first_search(AnsPath) :-
    initial_state(Init),
    breadth_first([[Init]|Q]-Q,AnsPath).

breadth_first(Q-Qt,AnsPath) :-
    qmember([S|Path],Q-Qt),final_state(S),!,
    reverse([S|Path],[],AnsPath).
breadth_first([Path|Q]-Qt,AnsPath) :-
    expand(Path,Qt-Qs),
    breadth_first(Q-Qs,AnsPath).

expand(Path,Queue) :-
    setof([S|Path],extend(Path,S),List),!,
    list_to_queue(List,Queue).
expand(_,Q-Q).

% extend is given in depth-first search below

qmember(X,[Y|Q]-Qt) :-
    nonvar(Y),
    (X = Y; qmember(X,Q-Qt)).

list_to_queue(L,Q-Qt) :-
    append(L,Qt,Q).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

reverse([],L,L).
reverse([H|T],L,R) :- reverse(T,[H|L],R).

% Program depth-first search
% -----------------------------------------------
depth_first_search(AnsPath) :-
    initial_state(Init),
    depth_first([Init],AnsPath).

depth_first([S|_],[S]) :-
    final_state(S),!.
depth_first([S|Path],[S|AnsPath]) :-
    extend([S|Path],S1),
    depth_first([S1,S|Path],AnsPath).

extend([S|Path],S1) :-
    next_state(S,S1),
    not(member_state(S1,[S|Path])).

member_state(X,[X|_]).
member_state(X,[_|T]) :- member_state(X,T).

% FIGURE 7.13: A database of city traffic roads
% --------------------------------------------------
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
