% FIGURE 14-18: A meta-interpreter for expert systems with certainty
%======================================================================
%  This file contains the meta-interpreter in ESSLN. It is written in
%  standard Prolog. However, there are two builtin predicates that may
%  vary from system to system as noted below.
%---------------------------------------------------------------------
%  Note: the builtin predicate "system" used in this program (to test
%  if a predicate is a builtin predicate) may correspond to a predicate
%  "sys" (or something else) in your Prolog system.
%  In which case, change this by using the following command:
%      ?- assert((system(F/N) :- sys(F))).
%  Also, the builtin predicate "cls" is used to clear the screen, which
%  may be equivalent to a predicate "clscr" in your Prolog system. In
%  which case, also change this using the command:
%      ?- assert((cls :- clscr)).
%  If your system has no builtin predicate equivalent to "cls", then
%  define it as follows (simply remove the symbols %):
%      cls :- cursor(&:,0,0),clear(23,80).
%
%      clear(0,0) :- !,cursor(&:,0,0).
%      clear(M,0) :- !,nl,M1 is M-1,clear(M1,80).
%      clear(M,N) :- put(32),N1 is N-1,clear(M,N1).
%  Here, the goal cursor(&:,0,0) moves the cursor to row 0 and column 0.
%  If your Prolog system provides no predicates for cursor movement,
%  then remove all goals "cls" from this file before consulting it.
%
%  If your Prolog system has difficulty with (or does not provide) the
%  predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and
%  also replace the goal "setof(T:X,(member(T:X,Args),var(X)),L)" in
%  the procedure "tvar_list" with the goal "elim_dup(Args,[],L)"  and add
%  the following clauses to this program (by removing the symbols %):
%  :- op(500,xfy,:).
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(T:X,_) :- nonvar(X).
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
%
%-----------------------------------------------------------------------
% To do experiment with this interpreter, consult this file and consult
% another file named \chap9\supplier.pro (which should have been copied
% from the diskette into your directory). Add the following facts to
% make the experiment more interesting:
%   ?- assert((p2(agent:'Yoshima Kuru',item:X) # 1)),
%      asserta((non(p2(agent:'Rossianno Co',item:X)) # 1)),
%      assert(constsymbol(agent:'Yoshima Kuru')),
%      assert(constsymbol(agent:'Rossianno Co')).
%
% Then enter the following Prolog queries (the English sentences are
% only for reference; see file \chap9\convertr.pro for conversion).
% Use ; to obtain alternative answers. Here, the goal "clear_workspace"
% is used to clear the workspace before the next query is evaluated.
% Note: a procedure 'print_proof' is provided at the end of this file
% to print the proof in a form easy to read. So, if you add the goal
% print_proof(Proof) to each goal below, the proof will be printed one
% rule at a time. Press <space> to continue printing (Some proofs are
% too long and only part of them is printed) . At the prompt -> press ;
% to obtain alternative answers. Note also that some proofs are too
% long and are only partly displayed.
%
%   Query: which _agent supply _item b2?
%   ?- clear_workspace, prove(p2(S:X,T:b2),20,C,Proof,[]).
%
%   Query: which _agent not supply _item b2?
%   ?- clear_workspace, prove(non(p2(S:X,T:b2)),20,C,Proof,[]).
%
%   Query: which _agent supply all _item?
%   ?- clear_workspace,
%      prove(all(p2(S:X,T:Y),not(non(p2(S:X,T:Z)))),20,C,Proof,[]).
%
%   Query: which _agent supply no _item?
%   ?- clear_workspace,
%      prove(all(non(p2(S:X,T:Y)),not(p2(S:X,T:Z))),20,C,Proof,[]).
%
%   Query: _agent 'Adams & Sons' supply _item b1?
%   ?- clear_workspace,
%      prove(p2(S:'Adams & Sons',T:b1),20,C,Proof,[]).
%
%   Query: _agent 'Rossianno Co' not supply _item b2?
%   ?- clear_workspace,
%      prove(non(p2(S:'Rossianno Co',T:b2)),20,C,Proof,[]).
%
%   Query: _agent 'Johnny Ltd' supply which _item?
%   ?- clear_workspace,
%      prove(p2(S:'Johnny Ltd',T:Y),20,C,Proof,[]).
%
%   Query: _agent 'Johnny Ltd' not supply which _item?
%   ?- clear_workspace,
%      prove(non(p2(S:'Johnny Ltd',T:Y)),20,C,Proof,[]).
%
%   Query: _agent 'Rossianno Co' supply which _item?
%   ?- clear_workspace,
%      prove(p2(S:'Rossianno Co',T:Y),20,C,Proof,[]).
%
%   Query: _agent 'Rossianno Co' not supply which _item?
%   ?- clear_workspace,
%      prove(non(p2(S:'Rossianno Co',T:Y)),20,C,Proof,[]).
%
%   Query: _agent 'Yoshima Kuru' supply all _item?
%   ?- clear_workspace,
%      prove(not(non(p2(S:'Yoshima Kuru',T:Y))),20,C,Proof,[]).
%
%   Query: _agent 'Mitre 10' supply no _item?
%   ?- clear_workspace,
%      prove(not(p2(S:'Mitre 10',T:Y)),20,C,Proof,[]).
%
%   Query: all _agent supply which _item?
%   ?- clear_workspace,
%      prove(all(p2(S:X,T:Y),not(non(p2(S:Z,T:Y)))),20,C,Proof,[]).
%
%   Query: no _agent supply which _item?
%   ?- clear_workspace,
%      prove(all(non(p2(S:X,T:Y)),not(p2(S:Z,T:Y))),20,C,Proof,[]).
%
%   Query: which _agent is a fractional supplier?
%   ?- clear_workspace, prove(p1(S:X),20,C,Proof,[]).
%
%   Query: which _agent is not a fractional supplier?
%   ?- clear_workspace, prove(non(p1(S:X)),20,C,Proof,[]).
%
% To experiment with another knowledge-base, clear the current one by
% entering the following queries before loading the new knowledge-base:
%   ?- clear_workspace,terminate(_).
%
%==========================================================================
:- op(500,xfx,#).
:- op(500,xfy,:).

terminate(_) :-
     abolish('#'/2),
     abolish(predicate/2),
     abolish(domain/1),
     abolish(constsymbol/1).

clear_workspace :-
     abolish(answer/2),assert(answer(null,null)),
     abolish(save_all/2),assert(save_all(null,null)),
     abolish(failproof/1),assert(failproof(null)).

%---------------------------------------------------------------------
% ESSLN META-INTERPRETER
% ====================================================================
prove(Goal,Depth,Certainty,Proof,Rules) :-
     prove_branch(Goal,Depth,Certainty,Proof,Rules),
     check_result(Goal,Depth,Certainty,Proof).
prove(Goal,Depth,0,FailProof,Rules) :-
     collect_failproof(Goal,Depth,FailProof).

prove_branch(true,D,1,fact,Rules) :- !.
prove_branch(A,0,0,nonterminate,Rules) :- !.
prove_branch(call(A),D,C,Proof,Rules) :- !,
     prove_branch(A,D,C,Proof,Rules).
prove_branch(not(A),D,C,(not(A) # C :- Proof),Rules) :- !,
     copy(A,A1),collect_proof(A1,D,Rules,C,List),!,
     convert(List,Proof).
prove_branch(all(A,B),D,C,(all(A,B) # C :- Proof),Rules) :- !,
     prove_all(A,B,D,C,Proof,Rules).
prove_branch((A,B),D,C,(ProofA,ProofB),Rules) :- !,
     prove_branch(A,D,CA,ProofA,Rules),
     prove_conj(CA,B,D,C,ProofB,Rules).
prove_branch(A,D,C,(A # C :- system:R),Rules) :-
     syst(A),!,(A, C = 1, R = true; not(A), C = 0, R = false).
prove_branch(A,D,C,(A # C :- Proof),Rules) :-
     not(find_clause((A # C),B)),!,
     find_out(A,C,Proof,Rules).
prove_branch(A,D,C,(A # C :- Proof),Rules) :-
     threshold(C0),
     find_clause((A # RC),B),D1 is D-1,
     detect_cut(B,B1,B2,Cut),
     (Cut = yes,prove_branch(B1,D1,C1,Proof1,[(A # RC :- B)|Rules]),
        (C1 < C0, C is RC*C1,return_proof(Proof1,B2,Proof);
         C1 >= C0,!,prove_branch(B2,D1,C2,Proof2,[(A # RC :- B)|Rules]),
            C is RC*C1*C2,conjunct(Proof1,Proof2,Proof));
      Cut = no,prove_branch(B,D1,CB,Proof,[(A # RC :- B)|Rules]),
               C is RC*CB).

detect_cut(B,B1,B2,yes) :- cutin(B,B1,B2),!.
detect_cut(_,_,_,no).

cutin(!,!,true) :- !.
cutin((!,B),!,B) :- !.
cutin((A,!),(A,!),true) :- !.
cutin((A,B),(A,As),Bs) :- cutin(B,As,Bs).

return_proof(Proof1,B,Proof) :-
     B = true,!, Proof1 = Proof;
     conjunct(Proof1,(B : nottried),Proof),!.

conjunct(A,fact,A) :- !.
conjunct((A,As),L,(A,Bs)) :- !,conjunct(As,L,Bs).
conjunct((A),L,(A,L)).

collect_proof(A,D,Rules,0,List) :-
     threshold(T),asserta(bag(1,[],D)),
     prove(A,D,C,P,Rules),
       once(retract(bag(C0,L,D))),
       C1 is C0* (1-C),asserta(bag(C1,[P|L],D)),
       C1 < 1-T, retract(bag(_,List,D)),remove(answer(A,D)),!.
collect_proof(A,D,_,C,List) :-
     retract(bag(C,List,D)),remove(answer(A,D)).

convert([P],P) :- !.
convert([P|L],(P,NP)) :-
     convert(L,NP).

prove_all(A,B,D,C,Proof,Rules) :-
     prove(A,D,CA,ProofA,Rules),
     check_all(CA,ProofA,B,D,C,Proof,Rules).
prove_all(A,B,D,_,_,_) :-
     remove(answer(A,D)),remove(save_all(B,D)),fail.

check_all(0,ProofA,B,D,0,ProofA,Rules) :- !.
check_all(_,_,B,D,C,ProofB,Rules) :-
     not((save_all(B1,D),instanc(B,B1))),
     asserta(save_all(B,D)),
     prove_branch(B,D,C,(B # C :- ProofB),Rules).

prove_conj(C,B,D,0,(B : nottried),Rules) :-
     threshold(T), C < T,!.
prove_conj(CA,B,D,C,ProofB,Rules) :-
     prove_branch(B,D,CB,ProofB,Rules),
     C is CA*CB.

check_result(G,D,C,Proof) :-
     threshold(C0), C < C0,!,
     not((failproof((G1,D):_),instanc(G,G1))),
     assert(failproof((G,D):Proof)),fail.
check_result(G,D,C,Proof) :-
     collect_failbranches(G,D,_),
     (answer(G1,D1),D1 < D,retract(answer(G1,D1)),fail;
     not((answer(G1,D),instanc(G,G1))),save_answer(G,D)).

collect_failproof(G,D,FailProof) :-
     collect_failbranches(G,D,FailList),
     not(answer(G,D)),convert(FailList,FailProof).

collect_failbranches(G,D,[FailBranch|Rest]) :-
     copy(G,G1),retract(failproof((G1,D):FailBranch)),!,
     collect_failbranches(G,D,Rest).
collect_failbranches(_,_,[]).

find_out(A,C,usergiven,Rules) :-
     tobe_filled(A) # 1,!,
     (already_asked(A,C),!; ask_user(A,C,Rules)).
find_out(A,0,nofact,_).

find_clause((A # RC),B) :- clause((A # RC),B).

save_answer(Goal,Depth) :-
     trim_answer(Goal,Goal1),
     assert(answer(Goal1,Depth)).

trim_answer(all(A,B),all(_,B)) :- !.
trim_answer((A,B),(A1,B1)) :- !,
     trim_answer(A,A1),trim_answer(B,B1).
trim_answer(A,A).

syst(A) :- functor(A,F,N),
           (system(F/N),!;
            member(F,['!',member,append,var_list,tvar_list,
                     uninstantiated,instantiate])).

copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).
maxdepth(20).
threshold(0.3).

remove(A) :- copy(A,B),retract(B),!,remove(A).
remove(_).

once(P) :- call(P),!.

%---------------------------------------------------------------------
% META-PROGRAMMING TOOLS
% ====================================================================

% BIND VARIABLES
% --------------------------------------------------------------------

ground(A) :- copy(A,B), A == B.

bind_var([],_) :- !.
bind_var(['$@#'(N)|Vs],N) :- N1 is N+1,bind_var(Vs,N1).

instanc(A,B) :-
     ground(A),!,A = B.
instanc(A,B) :-
     functor(A,F,N),functor(B,F,N),
     var_list(A,VL),bind_var(VL,1),A = B.

% -------------------------------------------------------------------
% FIND THE VARIABLES OF A GOAL
% ===================================================================

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


member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

%---------------------------------------------------------------------
% ESSLN LOGICAL NEGATION EVALUATOR
% ====================================================================
    non(A) # 1 :- tvar_list(A,L),eval_non(A,L).

    eval_non(A,L) # 1 :- not(A),!.
    eval_non(A,L) # 1 :- eval(A),uninstantiated(L),!,fail.
    eval_non(A,L) # 1 :- instantiate(A,L,VL),eval_non(A,VL).

    eval(A) # 1 :- A,!.
    uninstantiated(L) :- tvar_pure(L),unrestricted(L,0).

    tvar_pure([]) :- !.
    tvar_pure([T:V|TVs]) :- var(V),tvar_pure(TVs).

    unrestricted([],_) :- !.
    unrestricted([T:N|TVs],N) :- N1 is N+1,unrestricted(TVs,N1).

    instantiate(A,L,VL) :- domain(A),instant(L,VL).

    instant([X|Xs],Xs) :- get_term(X,Xs).
    instant([X|Xs],[X|VL]) :- instant(Xs,VL).

    get_term(T:V,TVs) :- constsymbol(T:V).
    get_term(X,Xs) :- get_var(X,Xs).

    get_var(T:V,[T:V|TVs]).
    get_var(X,[Y|Xs]) :- get_var(X,Xs).

    tvar_list(A,[]) :- ground(A),!.
    tvar_list(A,L) :-  A =.. [P|Args],
        setof(T:X,(member(T:X,Args),var(X)),L).

    type(T,T).
    type(S,T) :- subtype(S,T).

    subtype('@','#').
    already_asked('@','#').

%---------------------------------------------------------------------
% PRINT PROOF
%=====================================================================
  print_proof(Proof) :-
      cls,
      printp(Proof,1),
      get0(C).

  printp((A,B),N) :- !,
      nl,write('<'), printq(A,N), write('>'),
      printp(B,N).
  printp(A,N) :-
      nl,write('<'), printq(A,N), write('>').

  printq((A,B),N) :- !,
      printq(A,N),nl,write(' '),printq(B,N).
  printq((A :- B),N) :- !,
      write_space(N),write(A),write(' :- '),
      get0(C),N1 is N+1,
      nl,write(' '),printq(B,N1).
  printq(A,N) :-
      write_space(N),write(A),
      get0(C).

  write_space(0) :- !.
  write_space(N) :-
      write('  '),N1 is N-1,write_space(N1).

% --------------------------------------------------------------------


