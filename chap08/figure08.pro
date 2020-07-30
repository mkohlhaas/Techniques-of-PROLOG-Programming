% FIGURE 8.8: Rearrangement of subgoals in a given goal
% ----------------------------------------------------------
% Consult this program and try the following query:
%   ?- rearrange((p(X,Y),q(Z),r(X),s(Z),t(Y)),NewGoal).
% observe the order of subgoals in the NewGoal.
% ----------------------------------------------------------
% Note: LPA-Prolog (version 3.51, Sep 1991) will fail in
% evaluating the above query as it has a bug in its builtin
% predicate "setof". For example, the following queries
% show the inconsistency in this predicate's behaviour:
%   ?- setof(A,member(A,[X,X,X]),List).
%   A = _4B38
%   X = _4AE1
%   List = [_4AE1]

%   ?- setof(A,member(A,[X,Y,X]),List).
%   A = _4B59
%   X = _4AF0
%   Y = _4AF6
%   List = [_4AF0,_4AF6,_4AF0]
%
%  If your Prolog system has difficulty with (or does not provide) the
%  predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occur(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occur(X,[Y|Ys]) :- X == Y; occur(X,Ys).
% --------------------------------------------------------------------

rearrange(Goal,NewGoal) :-
    freeze(Goal,L,FGoal),
    arrange([],FGoal,FGoal1),
    melt(FGoal1,L,NewGoal).

arrange(GL,G,(A,As)) :-
    occurs('@var'(N),GL), subgoal(A,G,G1),
    occurs('@var'(N),A),!,arrange([A|GL],G1,As).
arrange(_,(A,As),(A,Bs)) :- !,arrange([A],As,Bs).
arrange(_,A,A).

subgoal(A,(A,As),As).
subgoal(A,(B,As),(B,Bs)) :- subgoal(A,As,Bs).
subgoal(A,(B,A),(B)) :- A \= (_,_).

occurs(X,X).
occurs(X,[A|As]) :- !,(occurs(X,A); occurs(X,As)).
occurs(X,Term) :- Term =.. [_|Args], Args \= [],
    occurs(X,Args).

% ---------------------------------------------------
% The freeze and melt procedures
% ---------------------------------------------------
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
