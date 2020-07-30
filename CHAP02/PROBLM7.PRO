% PROBLEM 2.7: Connecting arcs and cycle
%--------------------------------------------------------------
% consult this program and try the following queries:
%   ?- rearrange(_,[arc(3,2),arc(4,1),arc(5,4),arc(2,5)],Path).
%   ?- rearrange(_,[arc(4,1),arc(3,2),arc(2,5),arc(5,4)],Path).
%   ?- cycle([arc(3,2),arc(4,1),arc(5,4),arc(2,5),arc(1,3)]).
%--------------------------------------------------------------
    rearrange(_,[],[]).
    rearrange(arc(I,J),L,[arc(J,K)|Tail]) :-
        select(arc(J,K),L,Rest),
        rearrange(arc(J,K),Rest,Tail).

    cycle(L) :-
        rearrange2(_,L,[arc(I,_)|_],arc(_,I)).

    rearrange2(Last,[],[],Last).
    rearrange2(arc(I,J),L,[arc(J,K)|Tail],Last) :-
        select(arc(J,K),L,Rest),
        rearrange2(arc(J,K),Rest,Tail,Last).

    select(X,[X|T],T).
    select(X,[H|T],[H|R]) :- select(X,T,R).


