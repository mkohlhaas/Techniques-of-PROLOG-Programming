% FIGURE 8.7: Freeze and melt variables
% ----------------------------------------------------------------------
%  consult this program and try the following queries:
%    ?- freeze(p(f(X,Y),g(Z)),VList,FGoal),
%       melt(FGoal,VList,MGoal).
%    ?- freeze(likes(X,Y),VList,FGoal),
%       melt(FGoal,VList,MGoal).
%  In each example, compare the melted MGoal with the
%  original goal.
%
%  If your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ---------------------------------------------------------------------

freeze(G,L,G1) :-
    var_list(G,L),copy((G,L),(G1,L1)),
    bind_var(L1,1).

bind_var([],_) :- !.
bind_var(['@var'(N)|L],N) :-
    N1 is N+1,bind_var(L,N1).

melt(A,_,A) :- constant(A),!.
melt('@var'(N),L,V) :- !,nth_var(N,L,V).
melt(A,L,A1) :- A =.. [P|Args],
    melt_list(Args,L,Args1),A1 =.. [P|Args1].

melt_list([],_,[]) :- !.
melt_list([A|As],L,[B|Bs]) :-
    melt(A,L,B),melt_list(As,L,Bs).

nth_var(1,[X|_],X) :- !.
nth_var(N,[_|L],X) :- N1 is N-1, nth_var(N1,L,X).

% ---------------------------------------------------------
% The procedure var_list
% ---------------------------------------------------------
var_list(A,[]) :- my_ground(A),!.
var_list(A,L) :-  collect_var(A,Q-[]),
    setof(X,member(X,Q),L).

collect_var(A,Q-Q) :- constant(A),!.
collect_var(A,[A|Q]-Q) :- var(A), !.
collect_var(A,Q) :- A =.. [_|Args],collect_vars(Args,Q).

collect_vars([],Q-Q) :- !.
collect_vars([A|As],Q-Qt) :-
    collect_var(A,Q-Qs),collect_vars(As,Qs-Qt).

constant(A) :- atom(A); integer(A); float(A).

my_ground(A) :- copy(A,B), A == B.
copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
