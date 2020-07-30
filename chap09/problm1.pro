% PROBLEM 9.1: A COURSE ADVISOR EXPERT SYSTEM
% ==============================================================
% This file contains the main components of a course advisor
% expert system with a limited knowledge on Computer-Science
% courses. Consult this file and enter the following query:
%    ?- course_advisor.
% Then try the queries suggested in the introduction. Type the
% queries exactly as they are shown on the screen. When you are
% asked to supply information, you may give the information
% required (terminated with a period), or you may ask "why."
% When an answer to your query is displayed, you may accept it
% by typing "ok.", or you may demand an explanation by typing
% "how.", or you may request alternative answer by typing "more."
% For an example, follow the lines shown on page 466 (Observe the
% special terms such as 'Computer-Science'.  and remember to use
% the period (followed by <return>) to terminate your response).
% When you complete the experiment, you may extend the knowledge-
% base to give the system more power in advising students.
%
% ---------------------------------------------------------------
%  Note: the builtin predicate "system" used in this program (to
%  test if a predicate is a builtin predicate) may correspond
%  to a predicate "sys" (or something else) in your Prolog system.
%  In which case, change this by using the following command:
%      ?- assert((system(F/N) :- sys(F))).
%  Also, the builtin predicate "cls" is used to clear the screen,
%  which may be equivalent to a predicate "clscr" in your Prolog
%  system. In which case, also change this using the command:
%      ?- assert((cls :- clscr)).
%  If your system has no builtin predicate equivalent to "cls",
%  then define it as follows (simply remove the symbols %):
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
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
%
% ===============================================================
% A COURSE ADVISOR EXPERT SYSTEM FOR COMPUTER-SCIENCE STUDENTS
% ===============================================================
:- discontiguous(tobe_filled/1).
:- op(500,xfy,:).

course_advisor :-
    introduction,
    repeat,
        receive_query(Query),
        convert_query(Query,Goal),
        evaluate(Goal),
    terminate(Goal).

introduction :-
    cls,
    nl,write(' HELLO ! I AM YOUR COURSE ADVISOR'),
    nl,write(' YOU MAY ASK ME ABOUT YOUR ENROLMENT NEXT SEMESTER'),
    nl,write(' FOR EXAMPLE, YOU MAY ASK QUESTIONS LIKE'),
    nl,write('     Which units should I enrol next semester?'),
    nl,write('     Is unit se1 available next semester?'),
    nl,write('     What are the prerequisites for unit se1?'),
    nl,write('     What are the elective units for Computer-Science?'),
    nl,write(' etc.'),nl.

receive_query(Query) :-
    nl,write('Q:> '),
%    read_string(100,S),    % Arity-Prolog built-in predicates that
%    list_text(T,S),	    % can replace the procedure readline
    readline(T),
    scan_text(T,Query).

scan_text([],[]) :- !.
scan_text(T,[W|Ws]) :-
    append(L,[C|T1],T),member(C,[32,46,63]),!,
    name(W,L),scan_text(T1,Ws).

evaluate(Goal) :-
    prove(Goal,20,Result,Proof,[]),
    process_result(Goal,Result,Proof),!.
evaluate(_).

bye.
terminate(bye) :-
    abolish(already_asked/1),
    abolish(failproof/1),
    assert(already_asked('#')),
    assert(failproof('#')).

    already_asked('#').
    failproof('#').
    tobe_filled('#').

readline(L) :-
    get0(C),
    read_chars(C,[],L1),
    reverse(L1,[],L).

read_chars(13,L,L) :- !.
read_chars(8,[_|L],L1) :- !,
    put(32),put(8),
    get0(C1),
    read_chars(C1,L,L1).
read_chars(C,L,L1) :-
    get0(C1),
    read_chars(C1,[C|L],L1).

reverse([],L,L).
reverse([H|T],L,L1) :-
    reverse(T,[H|L],L1).

% =============================================================
% COURSE KNOWLEDGE
% =============================================================
enrol_unit(X) :-
    program_unit(X),
    not(complete(X)),
    meet_prerequisite(X),
    available(X).

program_unit(X) :-
    your_course(C),
    core_units(C,L),
    member(X,L).
program_unit(X) :-
    your_course(C),
    elective_units(C,L),
    your_interest(X,L).

complete(X) :-
    your_complete_units(L),
    member(X,L).

meet_prerequisite(X) :-
    prerequisite_units(X,L1),
    your_complete_units(L),
    included(L1,L).
meet_prerequisite(X) :-
    not(prerequisite_units(X,_)).

available(se1).
available(ics).
available(dsa).
available(cp).
available(cm).
available(os).
available(is2).
available(is3).
available(se2).
available(cau).
available(car).

core_units('Computer-Science',
    [ics,dma,dsa,is1,is2,is3,ps2,se1,os,pm,
     co1,co2,cp,es]).

elective_units('Computer-Science',
    [car,cau,cme,cc,idb,dbs,dc,dcn,es,gst,
     icg,lbs,lc,ai,rtca,st,se2,tc,vlsi]).
elective_units('Mathematics',
    [ma1,ma2,ma3,lp,or,mm,as,hm,la,fp]).

prerequisite_units(se1,[dsa,is1]).
prerequisite_units(dsa,[ics,dma]).
prerequisite_units(is1,[dsa]).
prerequisite_units(is2,[is1]).
prerequisite_units(is3,[is1]).
prerequisite_units(ps2,[is1,lp]).
prerequisite_units(se1,[dsa,is1]).
prerequisite_units(os,[dsa,co2]).
prerequisite_units(pm,[is2]).
prerequisite_units(co2,[co1,dsa]).
prerequisite_units(cp,[is3,os,pm]).
prerequisite_units(cau,[is1,co1]).

included([],_).
included([X|T],L) :- member(X,L),included(T,L).

tobe_filled(your_course(_)).
tobe_filled(your_interest(_,_)).
tobe_filled(your_complete_units(_)).

% =============================================================
% USER QUERIES
% =============================================================
convert_query(['Which',units,should,'I',enrol,next,semester],
    enrol_unit(_)).
convert_query(['Is',unit,X,available,next,semester],
    available(X)).
convert_query(['What',are,the,prerequisites,for,unit,X],
    prerequisite_units(X,_)).
convert_query(['What',are,the,core,units,for,X],
    core_units(X,_)).
convert_query(['What',are,the,elective,units,for,X],
    elective_units(X,_)).
convert_query([bye],bye).

% =============================================================
% SYSTEM QUERYING USER
% =============================================================
display_question(your_course(_)) :- nl,
    write('  > What course are you doing? ').
display_question(your_interest(_,L)) :- nl,
    write('  > What unit of the following that you are interested in?'),
    nl,write('  '),write(L),nl,write('  ').
display_question(your_complete_units(_)) :- nl,
    write('  > What units have you completed? '),
    nl,write('    List the units in this form [ics,dma,...]'),
    nl,write('    ').

% ==============================================================
% SYSTEM EXPLANATIONS
% ==============================================================
display_goal(enrol_unit(X)) :- !,
    write_list(['You should enrol in unit ',X]).
display_goal(program_unit(X)) :- !,
    write_list(['The unit ',X,' is a unit in your program']).
display_goal(not(complete(X))) :- !,
    write_list(['You have not done the unit ',X]).
display_goal(complete(X)) :- !,
    write_list(['You did the unit ',X]).
display_goal(meet_prerequisite(X)) :- !,
    write_list(['You did the prerequisite for the unit ',X]).
display_goal(available(X)) :- !,
    write_list(['Unit ',X,' is available next semester']).
display_goal(your_course(X)) :- !,
    write_list(['You are doing the course ',X]).
display_goal(core_units(C,L)) :- !,
    write_list(['The core units of the course ',C,' are ']),
    (length(L,N),N > 7,!,nl,tab(8); true), write(L).
display_goal(elective_units(C,L)) :- !,
    write_list(['The elective units of the course ',C,' are ']),
    (length(L,N),N > 7,!,nl,tab(8); true), write(L).
display_goal(your_interest(X,L)) :- !,
    write_list(['You chose ',X,' among the units ']),
    (length(L,N),N > 7,!,nl,tab(8); true), write(L).
display_goal(your_complete_units(L)) :- !,
    write_list(['You have completed the units ']),
    (length(L,N),N > 7,!,nl,tab(8); true), write(L).
display_goal(prerequisite_units(X,L)) :- !,
    write_list(['The prerequisite for ',X,' are ',L]).
display_goal(not(prerequisite_units(X,_))) :- !,
    write_list(['Unit ',X,' requires no prerequisites']).
display_goal(included(X,L)) :- !,
    write_list(['The units ',X,' are among ']),
    (length(L,N),N > 7,!,nl,tab(8); true), write(L).
display_goal(member(X,L)) :- !,
    write_list(['Unit ',X,' is among the units ']),
    (length(L,N),N > 7,!,nl,tab(8); true), write(L).
display_goal(succeeds([(complete(X) :- _)|_])) :-
    write_list(['Unit ',X,' is not among your completed units']).
display_goal(succeeds([(prerequisite_units(X,_) :- _)|_])) :-
    write_list(['Unit ',X,' requires no prerequisites']).

display_goal(A) :- syst(A),write(A).

% ====================================================================
% SYSTEM INTERPRETER
% ====================================================================
prove(Goal,Depth,Result,Proof,Rules) :-
     prove_branch(Goal,Depth,Result,Proof,Rules),
     check_result(Goal,Depth,Result,Proof).
prove(Goal,_,no,FailProof,_) :-
     collect_failbranches(Goal,FailProof).

prove_branch(true,_,yes,fact,_) :- !.
prove_branch(_,0,lost,toofar,_) :- !.
prove_branch(not(A),D,NegResult,NegProof,Rules) :-
     prove(A,D,Result,Proof,Rules),!,
     invert(A,Result,NegResult,Proof,NegProof).
prove_branch((A;B),D,Result,Proof,Rules) :- !,
     (prove_branch(A,D,Result,Proof,Rules);
      prove_branch(B,D,Result,Proof,Rules)).
prove_branch((A,B),D,Result,(ProofA,ProofB),Rules) :- !,
     prove_branch(A,D,ResultA,ProofA,Rules),
     prove_conj(ResultA,B,D,Result,ProofB,Rules).
prove_branch(A,_,Result,(A :- system:Result),_) :-
     syst(A),!,(A,!,Result = yes; Result = no).
prove_branch(A,_,Result,(A :- Proof),Rules) :-
     not(clause(A,_)),!,
     find_out(A,Result,Proof,Rules).
prove_branch(A,D,Result,(A :- Proof),Rules) :-
     clause(A,B),D1 is D-1,
     prove_branch(B,D1,Result,Proof,[(A :- B)|Rules]).

invert(A,yes,no,SuccProof,(not(A) :- fails(SuccProof))).
invert(A,no,yes,FailProof,(not(A) :- succeeds(FailProof))).

prove_conj(yes,B,D,Result,Proof,Rules) :- !,
     prove_branch(B,D,Result,Proof,Rules).
prove_conj(RA,B,_,RA,B:nottried,_).

check_result(G,D,yes,_) :- !,
     (maxdepth(D),!; collect_failbranches(G,_)).
check_result(G,_,_,Proof) :- assert(failproof(G:Proof)),!,fail.

collect_failbranches(G,[FailBranch|Rest]) :-
     copy(G,G1),retract(failproof(G1:FailBranch)),!,
     collect_failbranches(G,Rest).
collect_failbranches(_,[]).

syst(A) :- functor(A,F,N),system(F/N).
          % member(F,[member,included])).

maxdepth(20).

find_out(A,yes,usergiven,Rules) :-
     tobe_filled(A),!,
     (already_asked(A),!; ask_user(A,Rules)).
find_out(_,no,noclause,_).

ask_user(Goal,Rules) :-
    display_question(Goal),
    read(Answer),
    process_answer(Answer,Goal,Rules).

% ===============================================================
% SYSTEM EXPLANATION GENERATOR
% ===============================================================
process_answer(why,Goal,[Rule|Rules]) :- !,
    display_rule(Rule),nl,
    ask_user(Goal,Rules).
process_answer(why,Goal,[]) :- !,
    nl,write('  > No more explanations!'),nl,
    ask_user(Goal,[]).
process_answer(X,Goal,_) :-
    var_list(Goal,[V]), V = X,
    assert(already_asked(Goal)).

display_rule((A :- B)) :-
    member(B,[true,usergiven,nofact,system:_]),!,
    nl,write('  > Because'),
    nl,tab(8),display_goal(A),
    nl,write('    is '),write(B).
display_rule((A :- B)) :-
    copy((A,B),(A1,B1)),
    var_list((A1,B1),VL),bind_vars(VL,65),
    nl,write('  > Because'),
    nl,write('    IF'),  display_body(B1),
    nl,write('    THEN'),display_body(A1).

display_body((A,B)) :- !,
    nl,tab(8),display_goal(A),write(','),
    display_body(B).
display_body(A) :-
    nl,tab(8),display_goal(A).

bind_vars([],_) :- !.
bind_vars([X|VL],C) :-
    name(X,[C]),C1 is C+1,bind_vars(VL,C1).

write_list([]) :- !.
write_list([X|T]) :- write(X),write_list(T).

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

append([],L,L).
append([H|T],L,[H|T1]) :- append(T,L,T1).

% ==============================================================
% PROCESS EVALUATION RESULT
% ==============================================================
process_result(bye,_,_) :- !.
process_result(Goal,Result,Proof) :-
     nl,write('A:> '),
     (Result = no,write('No');
      Result = yes, display_goal(Goal)),
     read_response(Response),
     process_response(Response,Proof).

read_response(R) :-
     nl,write(' -> '),
     read(S),
     (member(S,[more,how,ok]),!,R = S;
      write('    Type how., more. or ok.'),
      read_response(R)).

process_response(more,_) :- !,fail.
process_response(ok,_) :- !.
process_response(how,(ProofA,ProofB)) :- !,
    process_response(how,ProofA),
    read_response(Response),
    process_response(Response,ProofB).
process_response(how,(A :- Proof)) :- !,
    recollect_body(Proof,Body),
    display_rule((A :- Body)),
    read_response(Response),
    process_response(Response,Proof).
process_response(how,_) :- !,
    nl,write('    No more explanations for this goal.').

recollect_body(fact,true) :- !.
recollect_body(system:R,system:R) :- !.
recollect_body(usergiven,usergiven) :- !.
recollect_body(noclause,nofact) :- !.
recollect_body((B : nottried),B) :- !.
recollect_body((B :- _),B) :- !.
recollect_body(((B :- _),Rest),(B,Bs)) :- !,
     recollect_body(Rest,Bs).
recollect_body(B,B).
