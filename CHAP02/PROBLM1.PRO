% PROBLEM 2.1: Office allocation
%----------------------------------------------------
% consult this program and enter the following query:
%   ?- office_allocation(S).
%----------------------------------------------------
    office_allocation(S) :-
        divisions(S),
        capacities(A),
        allocate(S,A).

    allocate([(D,N,F)|Rest],A) :-
        assign(N,F,A,A1),
        allocate(Rest,A1).
    allocate([],_).

    assign(N,1,[H|T],[H1|T]) :-
        N =< H, H1 is H-N.
    assign(N,F,[H|T],[H|T1]) :-
        assign(N,F1,T,T1), F is F1+1.

    divisions([(dcr,12,F1),(dhw,10,F2),(dms,22,F3),(dsw,8,F1),
               (eme,14,F5),(epr,11,F6),(esn,9,F3),(mem,9,F8),
               (mes,9,F9),(mex,16,F5)]).

    capacities([50,40,30]).


