% PROBLEM 8.2: A Prolog tracer
%---------------------------------------------------------------
% This file contains a Prolog tracer and two sample programs
% 'p' and 'likes' at the end. Note: the builtin predicate
% "system" used in this program (to test if a predicate is a
% builtin predicate) may correspond to "sys" (or something else)
% in your Prolog system. In which case, change this by using the
% following command:
%   ?- assert((system(F/N) :- sys(F))).
% Consult this file and perform the following experiments.
% First enter the following query to trace the execution of
% the goal p(X) (press 'c' or <return> at each prompt '>' of the
% tracer).
%   ?- trace(p(X),0).
% Next enter the following query to trace the execution of the
% goal likes(sue,X). Compare the result with Prolog's display
% in Figure 4.10 and explain their difference in level numbers:
%   ?- trace(likes(sue,X),0).
%--------------------------------------------------------------
    trace(true,D) :- !.
    trace((A;B),D) :- !,(trace(A,D); trace(B,D)).
    trace((A,B),D) :- !, trace(A,D), trace(B,D).

    trace(A,D) :-
        report(D,'CALL',A),
        trace_goal(A,D),
        report(D,'EXIT',A).
    trace(A,D) :-
        report(D,'FAIL',A),fail.

    trace_goal(A,D) :- syst(A),!,A.
    trace_goal(A,D) :- find_clause(A,B,D),D1 is D+1,
        detect_cut(B,B1,B2,Cut),
        (Cut = yes,trace(B1,D1),!,trace(B2,D1);
         Cut = no, trace(B,D1)).

    find_clause(A,B,D) :-
        clause(A,B), report_redo(A,D).

    report_redo(A,D).
    report_redo(A,D) :- report(D,'REDO',A),fail.

    report(Depth,Port,Goal) :-
        nl,write([Depth]),write(' '),write(Port),write(': '),
        write(Goal), write(' ? >'),
        (get0(C),(C = 99; C = 13),!; nl,abort).

    syst(!).
    syst(A) :- functor(A,F,N),system(F/N).

    detect_cut(B,B1,B2,yes) :- cutin(B,B1,B2),!.
    detect_cut(_,_,_,no).

    cutin(!,!,true) :- !.
    cutin((!,B),!,B) :- !.
    cutin((A,!),(A,!),true) :- !.
    cutin((A,B),(A,As),Bs) :- cutin(B,As,Bs).


% Sample program p
%-----------------------------------------------------------
    p(X) :- a(X),b(X).
    p(X) :- c(X).

    a(X) :- d(X).

    d(1).
    d(2).
    d(X) :- f(X),g(X).

    f(3).
    f(X) :- h(X).

    b(1).
    b(3).
    b(4).
    g(3).
    g(4).
    h(4).
    c(5).


% Sample program likes
%-------------------------------------------------------------
    likes(ann,X) :- toy(X),plays(ann,X).
    toy(doll).
    toy(snoopy).
    plays(ann,snoopy).
    likes(sue,Y) :- likes(ann,Y).


%-------------------------------------------------------------




