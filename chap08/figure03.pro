% FIGURE 8.3: Applications of var_list
% ----------------------------------------------------------------------
%  consult this program and try the following queries:
%    ?- instanc(p(a,f(U)),p(X,Y)).
%    ?- variant(likes(X,Y),likes(U,V)).
%    ?- subterm(f(a,X),[a,b,p(a,f(a,Y))]).
%    ?- instance_of(p(a,f(U)),p(X,Y)).
%  compare the results of the first and the last queries.
%
%  Note: If your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbol %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ------------------------------------------------------

% Test if A is an instance of B:
instanc(A,B) :-
    var_list(A,L), bind_var(L,1), A = B.

bind_var([],_) :- !.
bind_var(['@var'(N)|Xs],N) :-
    N1 is N+1, bind_var(Xs,N1).

% Test if A is a variant of B:
variant(A,B) :-
    not(not(instanc(A,B))),
    not(not(instanc(B,A))).

% Test if A is a subterm of B:
subterm(A,B) :- variant(A,B),!.
subterm(A,[B|Bs]) :- !,
    (subterm(A,B),!; subterm(A,Bs)).
subterm(A,B) :-
    B =.. [_|Bs], Bs \= [], subterm(A,Bs).

instance_of(A,B) :-
    copy((A,B),(A1,B1)), instanc(A1,B1).

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
