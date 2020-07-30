% FIGURE 8.18-19: A Prolog debugger
%=================================================================
% This file contains a Prolog debugger and two programs 'gcd' and
% 'least_num' at the end. Note: the builtin predicate "system"
% used in this program (to test if a predicate is a builtin
% predicate) may correspond to "sys" (or something else) in your
% Prolog system. In which case, change this by using the following
% command:
%   ?- assert((system(F/N) :- sys(F))).
% Consult this file and enter the following queries to study how
% errors are detected.
%   ?- gcd(14,35,Z).
%   ?- prove(gcd(14,35,Z),20,Result,Proof),
%      find_error(0,Proof,ErrorClause).
% Inspect the program 'gcd' to check the error clause.
% Now try the next program:
%   ?- least_num(X,[4,2,1,3]).
%   ?- prove(least_num(X,[4,2,1,3]),20,Result,Proof),
%      find_error(1,Proof,ErrorClause).
% Again, inspect the program 'least_num' to ckeck the error clause.
%=================================================================
% Part 1: Detect incorrect success
%-----------------------------------------------------------------
:- op(500,xfy,:).

    find_error(0,(A :- B),Clause) :- !,
        find_errclause(0,(A :- B),Clause).
    find_error(0,((A :- B),Rest),Clause) :- !,
        (find_errclause(0,(A :- B),Clause),!;
         find_error(0,Rest,Clause)).
    find_error(0,succeeds(FailProof),Clause) :- !,
        find_error(1,FailProof,Clause).
    find_error(0,setfull(SetProof),Clause) :-
        convert(SetProof,ConjProof),
        find_error(0,ConjProof,Clause).

    find_errclause(R,(A :- B),Clause) :-
        ask_user(A,R),!,
        (find_error(R,B,Clause),!;
         recollect_body(B,Body),Clause = (A :- Body)).

    ask_user(A,R) :-
        nl,write('Do you expect the goal '),
        write(A),write(' to be true? (y/n): '),
        get0(C),
        (C =:= 110+R*11,!; C =:= 110+11* (1-R),!,fail;
         ask_user(A,R)).

    recollect_body(fact,true) :- !.
    recollect_body(noclause,nofact) :- !.
    recollect_body(toofar,nonterminate) :- !.
    recollect_body((B : nottried),B) :- !.
    recollect_body((B :- C),B) :- !.
    recollect_body(((B :- C),Rest),(B,Bs)) :-
        recollect_body(Rest,Bs).

    convert([A|T],(A,S)) :- convert(T,S).
    convert([A],(A)).


% Part 2: Detect incorrect failure
%-----------------------------------------------------------------
    find_error(1,[Proof|Rest],Clause) :-
        find_error(1,Proof,Clause);
        find_error(1,Rest,Clause).

    find_error(1,(A :- B),Clause) :- !,
        find_errclause(1,(A :- B),Clause).
    find_error(1,Proof,Clause) :-
        convert(ListProof,Proof),
        append(Left,[(A :- B)|C],ListProof),
        (C = [(D : nottried)|_]; C = []),!,
        find_errclause(1,(A :- B),Clause).
    find_error(1,fails(SuccProof),Clause) :- !,
        find_error(0,SuccProof,Clause).
    find_error(1,setnull(SetProof),Clause) :-
        find_error(1,SetProof,Clause).

    append([],L,L).
    append([X|T],L,[X|R]) :- append(T,L,R).


% The Prolog meta-interpreter
%-------------------------------------------------------------
prove(Goal,Depth,Result,Proof) :-
     prove_branch(Goal,Depth,Result,Proof),
     check_result(Goal,Depth,Result,Proof).
prove(Goal,Depth,no,FailProof) :-
     collect_failbranches(Goal,FailProof).

prove_branch(true,D,yes,fact) :- !.
prove_branch(A,0,lost,toofar) :- !.
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
prove_branch(A,D,Result,(A :- system:Result)) :-
     syst(A),!,(A,!,Result = yes; Result = no).
prove_branch(A,D,no,(A :- noclause)) :-
     not(clause(A,B)),!.
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
prove_conj(RA, B,D,RA,B:nottried).

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

filter_proof([p(X,D,no,P)],no,[],setnull(P)) :- !.
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


% The sample program gcd
%---------------------------------------------------------------
    gcd(X,0,X) :- X > 0.
    gcd(X,Y,Z) :- X < Y, gcd(Y,X,Z).
    gcd(X,Y,Y) :- X >= Y, Y > 0, X1 is X mod Y, gcd(Y,X1,Z).

% The sample program least_num
%---------------------------------------------------------------
    least_num(X,[H|T]) :-
        least_num(Y,T),
        (H =< Y, X = H; H > Y, X = Y).

%===============================================================




