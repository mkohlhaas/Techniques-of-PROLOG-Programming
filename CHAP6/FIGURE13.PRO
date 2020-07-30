% FIGURE 6.13: Replacing an argument of a given term
%-------------------------------------------------------------
% If your Prolog system already has a built-in predicate
% argrep, then ignore this program and try the queries;
% otherwise consult the program before trying the queries:
%   ?- argrep(stud_mark('Smith J.R.',65),2,45,NewTerm).
%   ?- argrep(stud_mark('Smith J.R.',65),1,'Ryan R.',NewTerm).
%   ?- Piles = pile([b,c],[a,d]),
%      arg(1,Piles,Pile1),arg(2,Piles,[X|Pile2]),
%      argrep(Piles,1,[X|Pile1],NPiles),
%      argrep(NPiles,2,Pile2,NewPiles).
% What are the effect of the above queries ?
%-------------------------------------------------------------
    argrep(Term,N,Value,NewTerm) :-
        Term =.. [F|L],
        replace(N,L,Value,L1),
        NewTerm =.. [F|L1].

    replace(1,[X|L],Y,[Y|L]) :- !.
    replace(N,[X|L],Y,[X|L1]) :-
        N > 1, N1 is N-1,
        replace(N1,L,Y,L1).



