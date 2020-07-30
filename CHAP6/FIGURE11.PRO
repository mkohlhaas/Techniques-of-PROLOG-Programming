% FIGURE 6.11: The functional procedure apply
%----------------------------------------------------
% consult this program and try the following queries:
%   ?- apply('+',[1,2],X).
%   ?- apply('*',[14,63],X).
%   ?- apply('//',[213,15],X).
%   ?- apply(mod,[213,15],X).
%   ?- apply(gcd,[18,24],X).
%----------------------------------------------------
    apply(F,L,V) :-
        arithmetic(F),!,
        T =.. [F|L], V is T.

    apply(F,L,V) :-
        append(L,[V],L1),
        G =.. [F|L1], G.

    arithmetic(F) :-
        member(F, ['+','-','*','/','//','^',
                    mod, abs, exp, ln, sqrt]).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    gcd(X,0,X) :- X > 0.
    gcd(X,Y,Z) :- X < Y, gcd(Y,X,Z).
    gcd(X,Y,Z) :- X >= Y, Y > 0, X1 is X mod Y,
                  gcd(Y,X1,Z).
%----------------------------------------------------



