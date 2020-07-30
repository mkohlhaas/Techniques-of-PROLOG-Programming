% FIGURE 7.15: Program wave-search
%-------------------------------------------------------
% consult this program and enter the following query to
% find a route from one place to another in the traffic
% roads map given at the end of this file:
%   ?- wave_search(AnsPath).
% compare the result with that produced by breadth-first
% search in Figure 6.12.
%-------------------------------------------------------
    wave_search(AnsPath) :-
        initial_state(Init),
        wave([[Init]],AnsPath).

    wave(Wave,AnsPath) :-
        member([S|Path],Wave),final_state(S),!,
        reverse([S|Path],[],AnsPath).
    wave(Wave,AnsPath) :-
        expand_wave(Wave,NewWave),
        wave(NewWave,AnsPath).

    expand_wave([],[]) :- !.
    expand_wave([Path|Rest],NewWave) :-
        expand(Path,NPaths),
        expand_wave(Rest,RPaths),
        append(NPaths,RPaths,NewWave).

    expand(Path,NPaths) :-
        setof([S|Path],extend(Path,S),NPaths),!.
    expand(_,[]).

    extend([S|Path],S1) :-
        next_state(S,S1),
        not(member_state(S1,[S|Path])).

    member_state(X,[X|_]).
    member_state(X,[_|T]) :- member_state(X,T).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    reverse([],L,L).
    reverse([H|T],L,R) :- reverse(T,[H|L],R).

% A database of city traffic roads
%------------------------------------------------
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

%-----------------------------------------------------


