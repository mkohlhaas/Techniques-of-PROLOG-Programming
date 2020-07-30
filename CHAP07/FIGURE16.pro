% FIGURE 7.16-17: Program inter-wave search
%-------------------------------------------------------
% consult this program and enter the following query to
% find a route from one place to another in the traffic
% roads map given at the end of this file:
%   ?- inter_wave_search(AnsPath).
% compare the result with that produced by breadth-first
% search and wave-search.
%-------------------------------------------------------
    inter_wave_search(AnsPath) :-
        initial_state(Init),
        final_state(Final),
        inter_wave(0,[[Init]],[[Final]],AnsPath).

    inter_wave(Side,Wave1,Wave2,AnsPath) :-
        member([S|Path1],Wave1),
        member([S|Path2],Wave2),!,
        join(Side,Path1,[S|Path2],AnsPath).
    inter_wave(Side,Wave1,Wave2,AnsPath) :-
        expand_wave(Side,Wave1,NewWave1),
        change_side(Side,OppSide),
        inter_wave(OppSide,Wave2,NewWave1,AnsPath).

    join(0,Path1,Path2,AnsPath) :-
        reverse(Path1,Path2,AnsPath).
    join(1,Path1,Path2,AnsPath) :-
        reverse(Path2,Path1,AnsPath).

    change_side(I,J) :- J is 1-I.

    expand_wave(_,[],[]) :- !.
    expand_wave(Side,[Path|Rest],NewWave) :-
        expand(Side,Path,NPaths),
        expand_wave(Side,Rest,RPaths),
        append(NPaths,RPaths,NewWave).

    expand(Side,Path,NPaths) :-
        setof([S|Path],extend(Side,Path,S),NPaths),!.
    expand(Side,Path,[]).

    extend(Side,[S|Path],S1) :-
        (Side = 0,next_state(S,S1);
         Side = 1,next_state(S1,S)),
        not(member(S1,[S|Path])).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    reverse([],L,L).
    reverse([H|T],L,R) :- reverse(T,[H|L],R).


% A database of city traffic roads
%--------------------------------------------------------------
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