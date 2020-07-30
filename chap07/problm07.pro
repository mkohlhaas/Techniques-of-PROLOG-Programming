% PROBLEM 7.7: Program breadth-first-search without dynamic queues
% ----------------------------------------------------------------
% Consult this program and try the following query to find a route
% from one place to another in a city described by the road map
% given at the end of this file:
%   ?- breadth_first_search(AnsPath).
% ----------------------------------------------------------------
breadth_first_search(AnsPath) :-
    initial_state(Init),
    assert(queue([Init])),
    breadth_first(AnsPath).

breadth_first(AnsPath) :-
    queue(Path),
    extend(Path,NewState),
    final_state(NewState),
    reverse([NewState|Path],[],AnsPath).
breadth_first(AnsPath) :-
    queue(_),breadth_first(AnsPath).

extend([S|Path],S1) :-
    next_state(S,S1),
    not(member_state(S1,[S|Path])),
    assertz(queue([S1,S|Path])).
extend(Path,_) :-
    retract(queue(Path)),
    fail.

member_state(X,[X|_]).
member_state(X,[_|T]) :- member_state(X,T).

reverse([],L,L).
reverse([H|T],L,R) :- reverse(T,[H|L],R).

% -------------------------------------------------------------
% The sample traffic roads
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
