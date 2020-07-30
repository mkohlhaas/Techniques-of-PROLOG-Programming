% FIGURE 8.15-17: A Prolog meta-interpreter
% ----------------------------------------------------------------
% This file contains a Prolog meta-interpreter and a small program
% at the end. Note: the builtin predicate "system" used in this
% program (to test if a predicate is a builtin predicate) may
% correspond to "sys" (or something else) in your Prolog system.
% In which case, change this by using the following command:
%    ?- assert((system(F/N) :- sys(F))).
%
% To perform experiments with this meta-interpreter, consult this
% file and follow the following instructions.
% Inspect the sample program p and enter the following queries to
% produce the proofs, use ; to request alternative answers:
%    ?- prove(p(X),20,Result,Proof).
%    ?- prove(not(a(4)),20,Result,Proof).
%    ?- prove(setof(X,p(X),L),20,Result,Proof).
%
% Now remove the given sample program (by using abolish(p/1), a/1,
% b/1, e/1, f/1, g/1, h/1, l/1, m/1; use listing to ensure they no
% longer exist), and assert the following program clauses to test the
% effect of the cut by using the query below:
%
%    p(X) :- a(X),!,b(X).
%    p(X) :- c(X).
%
%    a(1).          b(X).
%    a(2).          c(4).
%    a(3).
%
%    ?- prove(p(X),20,Result,Proof).
% ----------------------------------------------------------------
:- op(500,xfy,:).

prove(Goal,Depth,Result,Proof) :-
     prove_branch(Goal,Depth,Result,Proof),
     check_result(Goal,Depth,Result,Proof).
prove(Goal,_,no,FailProof) :-
     collect_failbranches(Goal,FailProof).

prove_branch(true,_,yes,fact) :- !.
prove_branch(_,0,lost,toofar) :- !.
prove_branch(not(A),D,NegResult,NegProof) :-
     prove(A,D,Result,Proof),!,
     invert(A,Result,NegResult,Proof,NegProof).
prove_branch(setof(X,G,L),D,Result,(setof(X,G,L) :- Proof)) :-
     !, setof(p(X,D,R,P),prove(G,D,R,P),ListProof),
     filter_proof(ListProof,Result,L,Proof).
prove_branch((A;B),D,Result,Proof) :- !,
     (prove_branch(A,D,Result,Proof);
      prove_branch(B,D,Result,Proof)).
prove_branch((A,B),D,Result,(ProofA,ProofB)) :- !,
     prove_branch(A,D,ResultA,ProofA),
     prove_conj(ResultA,B,D,Result,ProofB).
prove_branch(A,_,Result,(A :- system:Result)) :-
     syst(A),!,(A,!,Result = yes; Result = no).
prove_branch(A,_,no,(A :- noclause)) :-
     not(clause(A,_)),!.
prove_branch(A,D,Result,(A :- Proof)) :-
     clause(A,B),D1 is D-1,
     detect_cut(B,B1,B2,Cut),
     (Cut = yes,prove_branch(B1,D1,R1,Proof1),
       (R1 \= yes,Result = R1,return_proof(Proof1,B2,Proof);
        R1 = yes,!,prove_branch(B2,D1,Result,Proof2),
                   conjunct(Proof1,Proof2,Proof));
      Cut = no,prove_branch(B,D1,Result,Proof)).

detect_cut(B,B1,B2,yes) :- cutin(B,B1,B2),!.
detect_cut(_,_,_,no).

cutin(!,!,true) :- !.
cutin((!,B),!,B) :- !.
cutin((A,!),(A,!),true) :- !.
cutin((A,B),(A,As),Bs) :- cutin(B,As,Bs).

return_proof(Proof1,B,Proof) :-
    B = true,!, Proof1 = Proof;
    conjunct(Proof1,(B : nottried),Proof).

conjunct(A,fact,A) :- !.
conjunct((A,As),L,(A,Bs)) :- !,conjunct(As,L,Bs).
conjunct((A),L,(A,L)).

invert(A,yes,no,SuccProof,(not(A) :- fails(SuccProof))).
invert(A,no,yes,FailProof,(not(A) :- succeeds(FailProof))).

prove_conj(yes,B,D,Result,Proof) :- !,
     prove_branch(B,D,Result,Proof).
prove_conj(RA,B,_,RA,B:nottried).

check_result(G,D,yes,_) :- !,
     (maxdepth(D),!; collect_failbranches(G,_)).
check_result(G,_,_,Proof) :-
     assert(failproof(G:Proof)),!,fail.

collect_failbranches(G,[FailBranch|Rest]) :-
     copy(G,G1),retract(failproof(G1:FailBranch)),!,
     collect_failbranches(G,Rest).
collect_failbranches(_,[]).

syst(!).
syst(A) :- functor(A,F,N),system(F/N).

filter_proof([p(_,_,no,P)],no,[],setnull(P)) :- !.
filter_proof(ListProof,yes,L,setfull(LProof)) :-
     filter(ListProof,[],L,LProof).

filter([],_,[],[]) :- !.
filter([p(X,_,R,_)|Rest],L1,L,LProof) :-
     (R = no; member(X,L1)),!,filter(Rest,L1,L,LProof).
filter([p(X,_,yes,P)|Rest],L1,[X|Xs],[P|Ps]) :-
     filter(Rest,[X|L1],Xs,Ps).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).
maxdepth(20).

% ------------------------------------------------------
% Sample program for proof collection
% ------------------------------------------------------
p(X) :- a(X),b(X).
p(X) :- h(X).

a(X) :- c(X),d(X).
a(X) :- e(X).

b(1).
b(2).

e(1).
e(X) :- f(X).

f(X) :- g(X).
f(2).

g(_) :- g(_).
h(X) :- k(X); l(X).

l(X) :- m(X); n(X).
m(3).
