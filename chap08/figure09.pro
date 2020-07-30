% FIGURE 8.9: LnProlog's negation evaluator
% ------------------------------------------------------------------
%  This file contains the negation evaluator and four programs:
%  'home', 'even', 'fault' and 'leng' at the end.
% ------------------------------------------------------------------
%  Note: some Prolog systems may reserve the name "home" as a
%  builtin atom. In which case, the user should change the predicate
%  "home" in this file to "athome", say, before consulting the file.
%  Also, some (compiled) Prolog systems may require every predicate
%  to be defined before being used. To make this program work in
%  those non-standard systems (as well as in the standard ones), we
%  add a dummy fact "functsymbol('#')" at the end of this program.
% ------------------------------------------------------------------
%  Consult this file and perform the following experiments.
%  First inspect the program 'home' and try the following queries,
%  use ; to request alternative answers:
%     ?- home(john).
%     ?- home(X).
%     ?- non(home(X)).
%     ?- non(non(home(X))).
%     ?- not(non(home(X))).
%
%  Now inspect the program 'even' and reset the database, then try
%  the following queries. Use ; to request alternative answers:
%     ?- reset(1).
%
%     ?- odd(s(0)).
%     ?- odd(X).
%
%  Next inspect the program 'fault' and reset the database again, then
%  try the following queries. Use ; to request alternative answers:
%     ?- reset(2).
%
%     ?- fault(a).
%     ?- fault(b).
%     ?- fault(c).
%     ?- fault(X).
%     ?- non(fault(X)).
%     ?- non(non(fault(X))).
%     ?- not(non(fault(X))).
%
%  Finally inspect the program 'leng', reset the database again, and
%  try the following queries. Use ; to request alternative answers:
%     ?- reset(3).
%
%     ?- non(leng(L,0)).
%     ?- non(leng(L,1)).
%  Observe carefully the answers to the last query, and recognize
%  how they are formed.
%
% ----------------------------------------------------------------------
%  Note: If your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ----------------------------------------------------------------------

non(A) :- var_list(A,L),eval_non(A,L).

eval_non(A,_) :- not(A),!.
eval_non(A,L) :- eval(A),uninstantiated(L),!,fail.
eval_non(A,L) :- instantiate(L),
                 re_var_list(L,VL),eval_non(A,VL).

eval(A) :- A,!.
uninstantiated(L) :- var_pure(L),unrestricted(L,0).

var_pure([]) :- !.
var_pure([X|Xs]) :- var(X),var_pure(Xs).

unrestricted([],_) :- !.
unrestricted([N|Xs],N) :- N1 is N+1,unrestricted(Xs,N1).

re_var_list(A,L) :- var_list(A,L1),shift_var(L1,L).

shift_var([],[]) :- !.
shift_var([X|L],L1) :- append(L,[X],L1).

instantiate([X|Xs]) :- get_term(X,Xs).
instantiate([_|Xs]) :- instantiate(Xs).

get_term(X,_) :- constsymbol(X).
get_term(X,_) :- functsymbol(F,N),functor(X,F,N).
get_term(X,Xs) :- get_var(X,Xs).

get_var(X,[X|_]).
get_var(X,[_|Xs]) :- get_var(X,Xs).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

functsymbol('#').

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

% --------------------------------------------------------
% Program home: the world of John and Sue
% --------------------------------------------------------
home(X) :- non(out(X)).
out(sue).
husband(john,sue).

constsymbol(sue).
constsymbol(john).

% ----------------------------------------------------
% Program even: even and odd integers
% ----------------------------------------------------
even(0).
even(s(s(X))) :- even(X).

odd(X) :- non(even(X)).

% ----------------------------------------------------
% Program fault: faulty nodes in an integrated circuit
% ----------------------------------------------------
fault(X) :- non(respond(X,Y)), X \== Y.

respond(a,b).
respond(a,c).
respond(b,a).

% --------------------------------------------------------------
% Program leng: length of lists
% --------------------------------------------------------------
leng([],0).
leng([_|T],N) :-
    leng(T,M),(N is M+1; N < M+1,!,fail).

reset(1) :-
    abolish(constsymbol/1),
    assert(constsymbol(0)),
    assert(functsymbol(s,1)).

reset(2) :-
    abolish(constsymbol/1),abolish(functsymbol/2),
    assert(constsymbol(a)),assert(constsymbol(b)),
    assert(constsymbol(c)).

reset(3) :-
    abolish(constsymbol/1),assert(constsymbol([])),
    assert(constsymbol(0)),assert(constsymbol(1)),
    assert(functsymbol('.',2)),assert(functsymbol('+',2)).
