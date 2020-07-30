% FIGURE 4.6: The program factorial using once predicate
%-------------------------------------------------------
% consult this program and try the following queries:
%   ?- factorial(7,F).
%   ?- factorial(N,5040).
% use ; to test if the program terminates properly.
%-------------------------------------------------------
    factorial(N,F) :-
        once(fact(N,F)).

    fact(0,1).
    fact(N,F) :-
        fact(N1,F1),
        N is N1+1,
        F is N*F1.

    once(P) :- P,!.



