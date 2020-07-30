% FIGURE 8.4: Program to extract constant and function symbols
% ---------------------------------------------------------------
% Note: There are (compiled) Prolog systems that require every
% predicate to have a clause at the time of evaluation, otherwise
% the program will be aborted. To make this program work with
% those systems (as well as with the standard ones), we add two
% dummy facts "constsymbol('#','#')"  and functsymbol('#') at the
% end of this program.
% Consult this program and try the following queries, observe
% the new constant and function symbols listed by 'listing':
%   ?- extract_symbols(husband(john,sue)),
%      listing([constsymbol/1,functsymbol/2]).
%   ?- extract_symbols(even(0)),
%      listing([constsymbol/1,functsymbol/2]).
%   ?- extract_symbols((even(s(s(X))) :- even(X))),
%      listing([constsymbol/1,functsymbol/2]).
%   ?- extract_symbols(length([],0)),
%      listing([constsymbol/1,functsymbol/2]).
%   ?- extract_symbols((leng([H|T],N) :-
%           leng(T,M),(N is M+1; N < M+1,!,fail))),
%      listing([constsymbol/1,functsymbol/2]).
% ------------------------------------------------------------
extract_symbols((A :- B)) :- !,extract_symbols((A,B)).
extract_symbols((A;B)) :- !,extract_symbols((A,B)).
extract_symbols((A,B)) :- !,extract_symbols(A),
                            extract_symbols(B).
extract_symbols(true) :- !.
extract_symbols(not(A)) :- !,extract_symbols(A).
extract_symbols(non(A)) :- !,extract_symbols(A).
extract_symbols(A) :- A =.. [_|Args],!,extr_symb(Args).
extract_symbols(_).

extr_symb([]) :- !.
extr_symb([A|As]) :- extracts(A),extr_symb(As).

extracts(A) :- var(A),!.
extracts(A) :- constant(A),!,
     (constsymbol(A),!; assert(constsymbol(A))).
extracts(A) :- A =.. [F|Args],functor(A,F,N),
     (functsymbol(F,N),!; assert(functsymbol(F,N))),
    extr_symb(Args).

constant(A) :- atom(A);integer(A);float(A).

constsymbol('#','#').
functsymbol('#').
