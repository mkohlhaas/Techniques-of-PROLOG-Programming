% FIGURE 8.5: Instantiation of a formula
% ----------------------------------------------------------------------
%  Consult this program and try the following queries.
%  Use ; to obtain alternative answers.
%  (observe the constsymbol and functsymbol facts):
%    ?- instantiate([X,Y]).
%    ?- instant_form(likes(X,Y)).
%  Change the facts on constsymbol and functsymbol
%  and repeat the queries with fewer or more variables.
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
% -----------------------------------------------------------------------

instant_form(A) :- var_list(A,L),instantiate(L).

instantiate([X|Xs]) :- get_term(X,Xs).
instantiate([_|Xs]) :- instantiate(Xs).

get_term(X,_) :- constsymbol(X).
get_term(X,_) :- functsymbol(F,N),functor(X,F,N).
get_term(X,Xs) :- get_var(X,Xs).

get_var(X,[X|_]).
get_var(X,[_|Xs]) :- get_var(X,Xs).

% ---------------------------------------------------
% Available constant and function symbols
% ---------------------------------------------------
constsymbol(ann).
functsymbol(mother,1).

% ---------------------------------------------------------
% The procedure var_list
% ---------------------------------------------------------
var_list(A,[]) :- my_ground(A),!.
var_list(A,L) :-  collect_var(A,Q-[]),
    setof(X,member(X,Q),L).

collect_var(A,Q-Q) :- constant(A),!.
collect_var(A,[A|Q]-Q) :- var(A), !.
collect_var(A,Q) :- A =.. [:|Args],collect_vars(Args,Q).

collect_vars([],Q-Q) :- !.
collect_vars([A|As],Q-Qt) :-
    collect_var(A,Q-Qs),collect_vars(As,Qs-Qt).

constant(A) :- atom(A); integer(A); float(A).

my_ground(A) :- copy(A,B), A == B.
copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
