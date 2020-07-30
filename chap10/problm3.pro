% PROBLEM 10.3: The procedure complement
% -------------------------------------------------------------------
% This file contains the procedure 'complement' of Figure 10.6.
% Consult this procedure and try the following queries:
%    ?- complement(item:Y,0,
%       supply(agent:'Johnny Ltd',item:Y),L1,L2).
%
%    ?- complement(item:Y,0,
%       supply(agent:'Mitre 10',item:Y),L1,L2).
%
%    ?- complement(item:Y,0,
%       supply(agent:'Yoshima Kuru',item:Y),L1,L2).
%
%    ?- complement(item:Y,0,
%       non(supply(agent:'Adams & Sons',item:Y)),L1,L2).
%
%    ?- complement(item:Y,0,
%       non(supply(agent:'Mitre 10',item:Y)),L1,L2).
%
%    ?- complement(item:Y,0,
%       non(supply(agent:'Rossianno Co',item:Y)),L1,L2).
%
%    ?- complement(agent:X,item:Y,(supply(agent:X,item:Y),
%       most(item:Z,0,supply(agent:X,item:Z))),L1,L2).
%
%    ?- complement(agent:X,item:Y,(non(supply(agent:X,item:Y)),
%       few(item:Z,0,supply(agent:X,item:Z))),L1,L2).
%
%    ?- complement(agent:X,item:Y,
%       non(supply(agent:X,item:Y)),L1,L2).
%
%    ?- complement(agent:X,item:Y,(non(supply(agent:X,item:Y)),
%       not(most(item:Z,0,supply(agent:X,item:Z)))),L1,L2).
%
%    ?- complement(agent:X,item:Y,(supply(agent:X,item:Y),
%       not(few(item:Z,0,supply(agent:X,item:Z)))),L1,L2).
%
% -------------------------------------------------------------------
% FIGURE 10.6: Definition of 'most' and 'few' predicates
% -------------------------------------------------------------------
:- discontiguous(non/1).
:- op(500,xfy,:).

most(X,Y,A) :-
    complement(X,Y,A,L1,L2),
    large(L1,L2).

few(X,Y,A) :-
    complement(X,Y,A,L1,L2),
    large(L2,L1).

complement(X,Y,A,L1,L2) :-
    (setof(X,Y^A,L1),!; L1 = []),
    (setof(X,(constsymbol(X),
     not(member(X,L1))),L2),!; L2 = []).

large(L1,L2) :-
    length(L1,N1),length(L2,N2),
    N1 >= 5*N2.

% -------------------------------------------------------------------
% FIGURE 10.9-10: A logical negation evaluator
% -------------------------------------------------------------------
non(non(A)) :- !,A.
non(A) :- tvar_list(A,L),eval_non(A,L).

eval_non(A,_) :- not(A),!.
eval_non(A,L) :- eval(A),uninstantiated(L),!,fail.
eval_non(A,L) :- instantiate(A,L,VL),eval_non(A,VL).

eval(A) :- A,!.
uninstantiated(L) :- tvar_pure(L),unrestricted(L,0).

tvar_pure([]) :- !.
tvar_pure([_:V|TVs]) :- var(V),tvar_pure(TVs).

unrestricted([],_) :- !.
unrestricted([_:N|TVs],N) :-
    N1 is N+1,unrestricted(TVs,N1).

instantiate(A,L,VL) :- domain(A),instant(L,VL).

instant([X|Xs],Xs) :- get_term(X,Xs).
instant([X|Xs],[X|VL]) :- instant(Xs,VL).

get_term(T:V,_) :- constsymbol(T:V).
get_term(X,Xs) :- get_var(X,Xs).

get_var(T:V,[T:V|_]).
get_var(X,[_|Xs]) :- get_var(X,Xs).

tvar_list(A,[]) :- my_ground(A),!.
tvar_list(A,L) :-  A =.. [_|Args],
    setof(T:X,(member(T:X,Args),var(X)),L).

domain(supply(S:_,T:_)) :-
    type(S,agent),type(T,item).

type(T,T).
type(S,T) :- subtype(S,T).

subtype('#').

my_ground(A) :- copy(A,B), A == B.
copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% -------------------------------------------------------------------
% FIGURE 10.15: Portion of a supplier-part database
% -------------------------------------------------------------------
supply(agent:'Adams & Sons',item:b1).
supply(agent:'Adams & Sons',item:b2).
supply(agent:'Adams & Sons',item:b3).
supply(agent:'Adams & Sons',item:b4).
supply(agent:'Adams & Sons',item:b5).
supply(agent:'Adams & Sons',item:b6).
supply(agent:'Johnny Ltd',item:b1).
supply(agent:'Johnny Ltd',item:b2).
supply(agent:'Johnny Ltd',item:b3).
supply(agent:'Johnny Ltd',item:b4).
supply(agent:'Johnny Ltd',item:b5).
supply(agent:'Mitre 10',item:b1).
supply(agent:'Yoshima Kuru',item:_).
non(supply(agent:'Rossianno Co',item:_)).

constsymbol(agent:'Adams & Sons').
constsymbol(agent:'Johnny Ltd').
constsymbol(agent:'Mitre 10').
constsymbol(agent:'Rossianno Co').
constsymbol(agent:'Yoshima Kuru').
constsymbol(item:b1).
constsymbol(item:b2).
constsymbol(item:b3).
constsymbol(item:b4).
constsymbol(item:b5).
constsymbol(item:b6).
