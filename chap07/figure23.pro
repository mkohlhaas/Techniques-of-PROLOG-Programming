% FIGURE 7.23: Program best-cost-search
% ------------------------------------------------------
% consult this program and enter the following query to
% find a route from one place to another in the traffic
% roads map given at the end of this file:
%   ?- best_cost_search(AnsPath).
% compare the result with that produced by hill-climbing
% search and best-first search.
% -------------------------------------------------------
:- op(500,xfy,:).

best_cost_search(AnsPath) :-
    initial_state(Init),
    best_cost([[0,Init]],AnsPath).

best_cost([[_,S|Path]|_],AnsPath) :-
    final_state(S),!,
    reverse([S|Path],[],AnsPath).
best_cost([Path|Rest],AnsPath) :-
    expand(Path,NPaths),
    merge(NPaths,Rest,NewList),
    best_cost(NewList,AnsPath).

expand([C,S|Path],NPaths) :-
    setof([C1,S1,S|Path],
          (extend([S|Path],S1),
           costsum(S,C,S1,C1)),NPaths),!.
expand(_,[]).

extend([S|Path],S1) :-
    next_state(S,S1),
    not(member_state(S1,[S|Path])).

member_state(X,[X|_]).
member_state(X,[_|T]) :- member_state(X,T).

costsum(S,C,S1,C1) :-
    cost(S,S1,D), C1 is C + D.

reverse([],L,L).
reverse([H|T],L,R) :- reverse(T,[H|L],R).

merge([],L,L) :- !.
merge(L,[],L) :- !.
merge([X|P],[Y|Q],[X|R]) :-
    less(X,Y),!,merge(P,[Y|Q],R).
merge(L,[Y|Q],[Y|R]) :-
    merge(L,Q,R).

less([V1|_],[V2|_]) :- V1 < V2.

% A database of city traffic roads
% -----------------------------------------------
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

% A cost function for the traffic roads
% ----------------------------------------------------
cost(S,S1,D) :-
    distance(S,NextCorners),
    member(S1:D,NextCorners),!.
cost(S,S1,D) :-
    cost(S1,S,D).

distance('I',['A':3,'B':4,'N':12,'L':5]).
distance('A',['B':3]).
distance('B',['C':4,'T':11]).
distance('C',['D':3,'E':3]).
distance('D',['E':2]).
distance('E',['G':3]).
distance('G',['H':3,'K':3]).
distance('H',['K':2]).
distance('K',['F':8]).
distance('F',['W':8,'M':8]).
distance('W',['T':3]).
distance('T',['S':4,'V':7]).
distance('S',['R':2]).
distance('R',['Q':2]).
distance('Q',['P':2]).
distance('P',['N':3,'U':3]).
distance('U',['V':2]).
distance('L',['M':27]).
