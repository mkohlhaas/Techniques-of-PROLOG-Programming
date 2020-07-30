% FIGURE 6.12: Functional procedures mapcar and reduce
% ----------------------------------------------------
% consult this program and try the following queries:
%   ?- mapcar(abs,[-1,2,-3],L).
%   ?- mapcar(sqrt,[4,25,169],L).
%   ?- reduce('+',[1,2,3,4],0,X).
%   ?- reduce('*',[1,2,3,4],1,X).
%   ?- concatenate([[1,2],[3,4],[5,6,7]],L).
%   ?- concatenate([[1,2],[],[a,b]],L).
% ----------------------------------------------------
mapcar(_,[],[]).
mapcar(F,[X|L],[Y|L1]) :-
    apply(F,[X],Y),
    mapcar(F,L,L1).

reduce(F,[X],A,V) :- !,
    apply(F,[X,A],V).
reduce(F,[X|L],A,V) :-
    reduce(F,L,A,V1),
    apply(F,[X,V1],V).

concatenate(Lists,BigList) :-
    reduce(append,Lists,[],BigList).

% Function apply
% ----------------------------------------------------
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
