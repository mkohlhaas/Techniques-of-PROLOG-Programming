% PROBLEMS 8.3-4: LnProlog (Version 2: with a tracer and a debugger)
% ===================================================================
% This file contains the complete code of LnProlog with a debugger
% and a tracer, which is written in Arity-Prolog. Another version
% of this system written in standard Prolog is stored in the file
% \chap8\lnprolg2.std.
%
% To run the system in this file, follow the steps below:
%
% 1.If the underlying system is Arity-Prolog 4.0 then do as follows:
%   a) Consult this file.
%   b) Enter the query ?- lnprolog. (a prompt ?: will appear)
%   c) From then on, you will be working with LnProlog.
%
% 2.If the underlying system is Arity-Prolog 5.1 or above, then simply
%   replace the goal 'initialize' in the top-control clause with the goal
%   'introduction', then run the system as described in 1.
%
% 3.Enter (or consult) the following program clauses:
%        ?: p(X) :- a(X),b(X).
%        ?: p(X) :- c(X).
%        ?: a(X) :- d(X).
%        ?: d(1) :- true.
%        ?: d(2) :- true.
%        ?: d(X) :- f(X),g(X).
%        ?: f(3) :- true.
%        ?: f(X) :- h(X).
%        ?: b(1) :- true.
%        ?: b(3) :- true.
%        ?: b(4) :- true.
%        ?: g(3) :- true.
%        ?: g(4) :- true.
%        ?: h(4) :- true.
%        ?: c(5) :- true.
%    Then turn the tracer on and trace the execution of the goal p(X)
%    by entering the following queries (press 'c' at each prompt '>'):
%        ?: traceon.
%        ?: p(X).
%    When the tracer pauses to display an answer and the prompt ->,
%    you may type ; to continue tracing alternative execution, or
%    you may press any key to terminate (You may also press <Ctrl-Break>
%    to exit LnProlog without leaving the underlying Prolog system,
%    but let's continue with the experiment).
%
% 3. Now turn the tracer off and reset the system by using the queries:
%        ?: traceoff.
%        ?: reset.
%    Then remove the above program by using the query ?: clear_objbase
%    if running on Arity-Prolog 4.0, or using abolish(p/1),abolish(a/1),
%    abolish(b/1),abolish(c/1),abolish(d/1),abolish(f/1),abolish(g/1),
%    abolish(h/1), if running on Arity-Prolog 5.1 or higher.
%    Then enter (or consult) the following program clauses:
%        ?: gcd(X,0,X) :- X > 0.
%        ?: gcd(X,Y,Z) :- X < Y, gcd(Y,X,Z).
%        ?: gcd(X,Y,Y) :- X >= Y, Y > 0, X1 is X mod Y, gcd(Y,X1,Z).
%    and turn on the debugger to debug the program by using the
%    following queries. When the answer is displayed, press 'w' to
%    indicate that the answer is wrong, and follow the debugger's
%    instructions to find the erroneous clause.
%        ?: debugon.
%        ?: gcd(14,35,Z).
%        Z = 14 -> w
%
%    Now, remove the above program and reset the system by using the
%    query  ?: abolish(gcd/3),reset.
%    Then enter the following program clauses:
%        ?: least_num(X,[H|T]) :- least_num(Y,T),min(H,Y,X).
%        ?: min(X,Y,X) :- X =< Y.
%        ?: min(X,Y,Y) :- X > Y.
%    Then, try the following query and when the answer is displayed,
%    press 'w' to indicate that it is wrong, and follow the debugger's
%    instructions to find the error in the given program.
%
%    To turn off the tracer and the debugger, enter the queries:
%        ?: traceoff.
%        ?: debugoff.
%
% =========================== LNPROLOG ==============================
% ================ A PROLOG SYSTEM THAT SUPPORTS ====================
% =========== LOGICAL NEGATION AND QUANTIFIED QUERIES ===============
% ============= AND ALSO HAS A DEBUGGER AND A TRACER ================
% ===================================================================
%
% TOP LEVEL CONTROL
% ===================================================================
:-discontiguous(find_error/3).
:- op(500,xfy,:).

lnprolog :-
    initialize,
    repeat,
        receive_query(S,G),
        process_query(S,G),
    fail.

initialize :-
    introduction,
    create_world(objworld),
    code_world(_,objworld).

introduction :-
    cls,nl,
    nl,write('======================LNPROLOG======================'),
    nl,write('==== is standard Prolog plus a logical negator. ===='),
    nl,write('====In LNPROLOG, the negating predicate is "non"===='),
    nl,write('==== and the success of non(A) provides values  ===='),
    nl,write('==== for the variables of A so that A is false. ===='),
    nl,write('====================================================').

receive_query(S,G) :-
    nl,nl,write('?:'),write(' '),
    read_query($ $,S),
    convert_query(S,G).

read_query(S0,S) :-
    read_string(100,S1),concat(S0,S1,S2),nl,
    (end_query(S1),!,S = S2; read_query(S2,S)).

end_query(S) :-
    string_length(S,K),N is K-1,nth_char(N,S,46).

convert_query(S,G) :- string_term(S,G),!.
convert_query(_,_) :- nl,write('syntax error'),!,fail.

% -------------------------------------------------------------------
% PROCESS QUERY
% -------------------------------------------------------------------
process_query(_,G) :- special_query(G),!,G.
process_query(_,(A :- B)) :- !,process_clause((A :- B)).
process_query(S,G) :- G =.. [P|Args],
    (member(P,[consult,reconsult]),!,process_file(Args);
     member(P,[assert,asserta,assertz]),!,process_assert(Args);
     true),
    process_goal(S,G).

process_clause(Clause) :-
    extract_symbols(Clause),
    assert(Clause),
    nl,write(stored).

process_file([Filename]) :-
    see(Filename),
    readfile,
    seen.

readfile :-
    repeat,
        read(Clause),
        (Clause = end_of_file,!;
        extract_symbols(Clause),fail).

process_assert([Clause]) :-
    extract_symbols(Clause).

special_query(G) :-
    member(G,[debugon,debugoff,traceon,traceoff,clear_objbase]),!.

debugon :- assert(debugger(on)),
    nl,write('Display proof ? (y/n): '),
    (get0(121),!,assert(displayproof(on)); true),nl.

traceon :- assert(tracer(on)).

debugoff :- retract(debugger(on)),
    retract(displayproof(on)).

traceoff :- retract(tracer(on)).

process_goal(S,Goal) :-
    var_symbols(S,VS),
    var_list(Goal,VL),
    evaluate(Goal,VS,VL),
    remove(answer),
    remove(failproof).

% -------------------------------------------------------------------
% EVALUATE A GOAL
% -------------------------------------------------------------------
evaluate(Goal,VS,VL) :-
    (debugger(on),!, prove_goal(Goal,VS,VL);
     tracer(on),!,   trace(Goal,0);
                     call_goal(Goal,VS,VL)),!.
evaluate(_,_,_).

prove_goal(Goal,VS,VL) :-
    prove(Goal,20,Result,Proof),
    process_result(VS,VL,Result),
    process_response(Result,Proof),!.

call_goal(Goal,VS,VL) :-
    call(Goal), process_result(VS,VL,yes),
    (VS = [],!; process_response(yes,_)),!.
call_goal(_,_,_) :- nl,write('no').

process_result(_,_,no) :- !,nl,write('no').
process_result([],[],yes) :- !,nl,write('yes').
process_result(_,VL,yes) :- duplicated_answer(VL),!,fail.
process_result(VS,VL,yes) :- print_answer(VS,VL,VL1),
                             assert(answer(VL1)).

print_answer(['_'|Xs],[_|VL],[_|VL1]) :- !,
    print_answer(Xs,VL,VL1).
print_answer([X|Xs],[V|VL],[V|VL1]) :-
    nl,write(X),write(' = '),write(V),
    print_answer(Xs,VL,VL1).
print_answer([],[],[]).

process_response(Result,Proof) :-
    write(' -> '),get0(C),
    (C = 59, !, fail;
     C = 119,!, (debugger(on),!,error_type(Result,R),
                     find_error(R,Proof,ErrorClause),
                     (displayproof(on),!,nl,nl,write(Proof);true),
                     print_error(ErrorClause);
                 nl,write('Debugger is not on !'));
     true).

error_type(yes,0).
error_type(no,1).

print_error(ErrorClause) :-
     nl,nl,write('Error Clause: '),write(ErrorClause).

duplicated_answer(A) :-
    answer(B),instanc(A,B).

instanc(A,B) :-
    var_list(A,L),bind_var(L,1),A = B.

bind_var([],_).
bind_var(['$@#'(N)|Vs],N) :- N1 is N+1,bind_var(Vs,N1).

remove(P) :- A =.. [P,_],retract(A),!,remove(P).
remove(_).

clear_objbase :-
    delete_world(objworld),
    create_world(objworld),
    code_world(_,objworld),!.

reset :-
    abolish(constsymbol/1),
    abolish(functsymbol/2).

% -------------------------------------------------------------------
% EXTRACT CONSTANT AND FUNCTION SYMBOLS
% -------------------------------------------------------------------
extract_symbols((A :- B)) :- !,extract_symbols((A,B)).
extract_symbols((A;B)) :- !,extract_symbols((A,B)).
extract_symbols((A,B)) :- !,extract_symbols(A),
                            extract_symbols(B).
extract_symbols(true) :- !.
extract_symbols(not(A)) :- !,extract_symbols(A).
extract_symbols(non(A)) :- !,extract_symbols(A).
extract_symbols(A) :- A =.. [_|Args],!,extr_symb(Args).
extract_symbols(_).

extr_symb([]) :- !.
extr_symb([A|As]) :- extracts(A),extr_symb(As).

extracts(A) :- var(A),!.
extracts(A) :- constant(A),!,
               (constsymbol(A),!;assert(constsymbol(A))).

extracts(A) :- A =.. [F|Args],functor(A,F,N),
               (functsymbol(F,N),!;assert(functsymbol(F,N))),
               extr_symb(Args).

%--------------------------------------------------------------------
% EXTRACT VARIABLE SYMBOLS
%--------------------------------------------------------------------
var_symbols(S,VS) :- list_text(L,S),
                     list_var_symbols(L,0,[],VS).

list_var_symbols(L,Q,VT,VS) :-
    append(_,[B,C|D],L),    % (string always has a leading space)
    (quote_found(Q,B,Q1),!,
        list_var_symbols([C|D],Q1,VT,VS);
     var_found(Q,B,C),!,
        append(A1,[C1|D1],[C|D]),not(valid(C1)),!,
        name(V,A1),
        (member(V,VT),V \= '_', VS = VS1,!; VS = [V|VS1]),
        list_var_symbols([C1|D1],0,[V|VT],VS1)).
list_var_symbols(_,_,_,[]).

quote_found(0,B,B) :- member(B,[34,36,39]),!.
quote_found(Q,Q,0).   % quotes [ ', ", $]

var_found(0,B,C) :- not(valid(B)),var_start(C).

var_start(C) :- (65 =< C,C =< 90);C = 95.

valid(C) :-
    (65 =< C,C =< 90);   % A - Z
    (97 =< C,C =< 122);  % a - z
    (48 =< C,C =< 57);   % 0 - 9
    C = 95.              % underscore

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% -------------------------------------------------------------------

% LNPROLOG'S LOGICAL NEGATION & QUATIFICATION
% ===================================================================

all(L,G) :- copy((L,G),(L1,G1)), L = L1,
            G1,not(non(G)).
all(G)  :-  G \= (_,_), not(non(G)).

non(non(A)) :- !,A.

non(A) :- var_list(A,L),eval_non(A,L).

eval_non(A,_) :- not(A),!.
eval_non(A,L) :- eval(A),uninstantiated(L),!,fail.
eval_non(A,L) :- instantiate(L),re_var_list(L,VL),eval_non(A,VL).

eval(A) :- A,!.
uninstantiated(L) :- var_pure(L),unrestricted(L,0).

var_pure([]) :- !.
var_pure([X|Xs]) :- var(X),var_pure(Xs).

unrestricted([],_) :- !.
unrestricted([N|Xs],N) :- N1 is N+1,unrestricted(Xs,N1).

instantiate([X|Xs]) :- get_term(X,Xs).
instantiate([_|Xs]) :- instantiate(Xs).

get_term(X,_) :- constsymbol(X).
get_term(X,_) :- functsymbol(F,N),functor(X,F,N).
get_term(X,Xs) :- get_var(X,Xs).

get_var(X,[X|_]).
get_var(X,[_|Xs]) :- get_var(X,Xs).

re_var_list(A,L) :- var_list(A,L1),shift_var(L1,L).

shift_var([],[]) :- !.
shift_var([X|L],L1) :- append(L,[X],L1).

% -------------------------------------------------------------------
% COLLECT THE VARIABLES OF A FORMULA
% -------------------------------------------------------------------
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

append([],L,L).
append([H|T],L,[H|T1]) :- append(T,L,T1).

%--------------------------------------------------------------------
% THE META-INTERPRETER
%--------------------------------------------------------------------
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
prove_branch(setof(X,G,L),D,Result,(setof(X,G,L) :- Proof)) :- !,
     setof(p(X,D,R,P),prove(G,D,R,P),ListProof),
     filter_proof(ListProof,Result,L,Proof).
prove_branch((A;B),D,Result,Proof) :- !,
     (prove_branch(A,D,Result,Proof);
      prove_branch(B,D,Result,Proof)).
prove_branch((A,B),D,Result,(ProofA,ProofB)) :- !,
     prove_branch(A,D,ResultA,ProofA),
     prove_conj(ResultA,B,D,Result,ProofB).
prove_branch(A,_,Result,(A :- system:Result)) :-
     syst(A),!,(A,Result = yes; Result = no).
prove_branch(A,_,no,(A :- noclause)) :-
     not(find_clause(A,_)),!.
prove_branch(A,D,Result,(A :- Proof)) :-
     find_clause(A,B),D1 is D-1,
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

prove_conj(yes,B,D,Result,Proof) :- !,prove_branch(B,D,Result,Proof).
prove_conj(RA,B,_,RA,B:nottried).

check_result(G,D,yes,_) :- !,
     (maxdepth(D),!; collect_failbranches(G,_)).
check_result(G,_,_,Proof) :- assert(failproof(G:Proof)),!,fail.

collect_failbranches(G,[FailBranch|Rest]) :-
     copy(G,G1),retract(failproof(G1:FailBranch)),!,
     collect_failbranches(G,Rest).
collect_failbranches(_,[]).

syst(A) :- functor(A,F,N),
          (system(F/N),!;
           member(F,['!',var_list,eval,uninstantiated,
                     instantiate,re_var_list])).

filter_proof([p(_,_,no,P)],no,[],setnull(P)) :- !.
filter_proof(ListProof,yes,L,setfull(LProof)) :-
     filter(ListProof,[],L,LProof).

filter([],_,[],[]) :- !.
filter([p(X,_,R,_)|Rest],L1,L,LProof) :-
     (R = no; member(X,L1)),!,filter(Rest,L1,L,LProof).
filter([p(X,_,yes,P)|Rest],L1,[X|Xs],[P|Ps]) :-
     filter(Rest,[X|L1],Xs,Ps).

find_clause(A,B) :- clause(A,B).
find_clause(A,B) :-
     code_world(X,api),
     clause(A,B),
     code_world(_,X).

maxdepth(20).

%-------------------------------------------------------------------
% THE META-DEBUGGER
%-------------------------------------------------------------------
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
recollect_body((B :- _),B) :- !.
recollect_body(((B :- _),Rest),(B,Bs)) :-
     recollect_body(Rest,Bs).

convert([A|T],(A,S)) :- convert(T,S).
convert([A],(A)).

find_error(1,[Proof|Rest],Clause) :-
     find_error(1,Proof,Clause);
     find_error(1,Rest,Clause).

find_error(1,(A :- B),Clause) :- !,
     find_errclause(1,(A :- B),Clause).
find_error(1,Proof,Clause) :-
     convert(ListProof,Proof),
     append(_,[(A :- B)|C],ListProof),
     (C = [(_ : nottried)|_]; C = []),!,
     find_errclause(1,(A :- B),Clause).
find_error(1,fails(SuccProof),Clause) :- !,
     find_error(0,SuccProof,Clause).
find_error(1,setnull(SetProof),Clause) :-
     find_error(1,SetProof,Clause).

%-------------------------------------------------------------------
% THE TRACER
%-------------------------------------------------------------------
trace(true,_) :- !.
trace((A;B),D) :- !,(trace(A,D); trace(B,D)).
trace((A,B),D) :- !, trace(A,D), trace(B,D).
trace(A,D) :-
     report(D,'CALL',A),
     trace_goal(A,D),
     report(D,'EXIT',A).
trace(A,D) :-
     report(D,'FAIL',A),fail.

trace_goal(A,_) :- syst(A),!,A.
trace_goal(A,D) :- find_clause(A,B,D),D1 is D+1,
     detect_cut(B,B1,B2,Cut),
     (Cut = yes,trace(B1,D1),!,trace(B2,D1);
      Cut = no, trace(B,D1)).

find_clause(A,B,D) :-
     clause(A,B), report_redo(A,D).
find_clause(A,B,D) :-
     code_world(X,api),
     clause(A,B),
     code_world(_,X),
     report_redo(A,D).

report_redo(_,_).
report_redo(A,D) :- report(D,'REDO',A),fail.

report(0,'EXIT',A) :- !,
     nl,write('[0] EXIT: '),write(A),
     nl,write(A),write(' -> '),
     get0(C), C \= 59.
report(Depth,Port,Goal) :-
     nl,write([Depth]),write(' '),write(Port),write(': '),
     write(Goal), write(' ? >'), (get0(99),!; nl,abort).

%======================= END LNPROLOG ==============================
