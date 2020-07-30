% FIGURE 8.2: Program to extract the variables of a formula
%-----------------------------------------------------------------------
% consult this program and try the following queries:
%   ?- var_list(leq(X,max(X,Y)),L).
%   ?- var_list(leq(1,max(1,2)),L).
%   ?- var_list(leq(1,max(1,2)),[]).
%   ?- var_list(p(f(X,Y),g(X,Z),h(Y,Z)),L).
%   ?- var_list(p(f(g(h(X,X,X)))),L).
%
%  Note: If your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
%----------------------------------------------------------

    var_list(A,[]) :- ground(A),!.
    var_list(A,L) :-  collect_var(A,Q-[]),
        setof(X,member(X,Q),L).

    collect_var(A,Q-Q) :- constant(A),!.
    collect_var(A,[A|Q]-Q) :- var(A), !.
    collect_var(A,Q) :- A =.. [P|Args],collect_vars(Args,Q).

    collect_vars([],Q-Q) :- !.
    collect_vars([A|As],Q-Qt) :-
        collect_var(A,Q-Qs),collect_vars(As,Qs-Qt).

    constant(A) :- atom(A); integer(A); float(A).

    ground(A) :- copy(A,B), A == B.
    copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

%-----------------------------------------------------------



