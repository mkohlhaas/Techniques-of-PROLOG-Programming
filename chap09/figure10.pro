% FIGURE 9.10: A DINER EXPERT SYSTEM
% ==================================================================
% This file contains a complete expert system with limited knowledge
% on restaurants in Paris. The double-underlined procedures are
% general and the single-underlined ones are for the special case
% of diners knowledge. Consult this file and enter the following
% query to invoke the system:
%    ?- diner_expert.
% Then try the queries listed in the introduction and follow the
% lines shown on page 407 (The words must be typed exactly as shown
% on the sreen, including the capital 'W'. But you may use backspaces
% to correct typing errors . It may be convenient to note these
% queries down to be used later).
% If you are asked to supply information, you may provide the
% answer (terminated by a period) or you may demand an explanation
% by typing 'why.' (remember the period!). For example, when asked
% about your favourite dishes, you may answer: [duck,frog].  When
% asked the price you expect, you may answer: 30.  When asked about
% your location, you may ask why. or you may enter 'Paris-18'.
% When you are given an answer, you may accept it by typing 'ok.',
% or you may demand an explanation by typing 'how.' You may keep
% asking 'how.' until the system says no more explanations, or you
% are satisfied with its explanation (you should say 'ok.' then, and
% note after that you may ask 'how.' again for other explanations).
% To leave the system, type 'bye.' at the prompt Q:>.
% When complete with the experiment, you may extend the knowledge-
% base to cover more interesting cases, or you may replace the
% diner expert knowledge with another knowledge and repeat the
% experiment.
% ------------------------------------------------------------------
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
%  Also, if your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ----------------------------------------------------------------------
% TOP LEVEL CONTROL
% ----------------------------------------------------------------------
:- discontiguous(tobe_filled/1).
:- op(500,xfy,:).

diner_expert :-
    introduction,
    repeat,
        receive_query(Query),
        convert_query(Query,Goal),
        evaluate(Goal),
    terminate(Goal).

introduction :- cls,
nl,write('          HELLO !  I AM THE DINER EXPERT IN PARIS'),
nl,write('          YOU MAY  ASK ME ABOUT PARIS RESTAURANTS'),
nl,write('          FOR EXAMPLE, YOU MAY ASK QUESTIONS LIKE'),nl,
nl,write('          Which restaurant suits me best?'),
nl,write('          Which restaurant serves my favourite dishes?'),
nl,write('          Which restaurant charges reasonable price?'),
nl,write('          Which restaurant is conveniently close to me?'),
nl,write('          etc.'),nl.

receive_query(Query) :- nl,write('Q:> '),
    read_string(100,S),    % Arity-Prolog built-in predicates that
    list_text(T,S),        % can replace the procedure readline
    readline(T),
    scan_text(T,Query).

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

scan_text([],[]) :- !.
scan_text(T,[W|Ws]) :- append(L,[C|T1],T),
    member(C,[32,46,63]),!,name(W,L),scan_text(T1,Ws).

convert_query(['Which',restaurant,suits,me,best],
              best_restaurant(_)).
convert_query(['Which',restaurant,serves,my,favourite,dishes],
              serve_favourites(_)).
convert_query(['Which',restaurant,charges,reasonable,price],
              good_price(_)).
convert_query(['Which',restaurant,is,conveniently,close,to,me],
              close_location(_)).
convert_query([bye],bye).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

reverse([],L,L).
reverse([H|T],L,L1) :-
    reverse(T,[H|L],L1).

% --------------------------------------------------------------
% EVALUATION OF GOALS
% --------------------------------------------------------------
evaluate(Goal) :-
    prove(Goal,20,Result,Proof,[]),
    process_result(Goal,Result,Proof),!.
evaluate(_).

process_result(bye,_,_) :- !.
process_result(Goal,Result,Proof) :-
    nl,write('A:> '),
    (Result = no,write('No');
     Result = yes, display_goal(Goal)),
    read_response(Response),
    process_response(Response,Proof).

read_response(R) :-
    nl,write('  > '),
    read(S),
    (member(S,[how,more,ok]),!,R = S;
     write('    Type how., more. or ok.'),
     read_response(R)).

bye.
terminate(bye) :-
    abolish(failproof/1),
    abolish(already_asked/1),
    assert(failproof('#')),
    assert(already_asked('#')).

failproof('#').
already_asked('#').
tobe_filled('#').

% --------------------------------------------------------------
% THE META-INTERPRETER
% --------------------------------------------------------------
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
    syst(A),!,
    (A,!,Result = yes; Result = no).
prove_branch(A,_,Result,(A :- Proof),Rules) :-
    not(clause(A,_)),!,
    find_out(A,Result,Proof,Rules).
prove_branch(A,D,Result,(A :- Proof),Rules) :-
    clause(A,B),D1 is D-1,
    prove_branch(B,D1,Result,Proof,[(A :- B)|Rules]).

find_out(A,yes,usergiven,Rules) :-
    tobe_filled(A),!,
    (already_asked(A),!; ask_user(A,Rules)).
find_out(_,no,noclause,_).

invert(A,yes,no,SuccProof,(not(A) :- fails(SuccProof))).
invert(A,no,yes,FailProof,(not(A) :- succeeds(FailProof))).

prove_conj(yes,B,D,Result,Proof,Rules) :- !,
    prove_branch(B,D,Result,Proof,Rules).
prove_conj(RA,B,_,RA,B:nottried,_).

check_result(G,D,yes,_) :- !,
    (maxdepth(D),!; collect_failbranches(G,_)).
check_result(G,_,_,Proof) :-
    assert(failproof(G:Proof)),!,fail.

collect_failbranches(G,[FailBranch|Rest]) :-
    copy(G,G1),retract(failproof(G1:FailBranch)),!,
    collect_failbranches(G,Rest).
collect_failbranches(_,[]).

syst(A) :- functor(A,F,N),
    (system(F/N); member(F,['!',member,included])).

maxdepth(20).

% --------------------------------------------------------------
% QUERYING THE USER
% --------------------------------------------------------------
ask_user(Goal,Rules) :-
    display_question(Goal),
    read(Answer),
    process_answer(Answer,Goal,Rules).

process_answer(why,Goal,[Rule|Rules]) :- !,
    display_rule(Rule),!,nl,
    ask_user(Goal,Rules).
process_answer(why,Goal,[]) :- !,
    nl,write('  > No more explanations!'),nl,
    ask_user(Goal,[]).
process_answer(X,Goal,_) :-
    var_list(Goal,[V]), V = X,
    assert(already_asked(Goal)).

% --------------------------------------------------------------
% PROCESS THE USER RESPONSE
% --------------------------------------------------------------
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
recollect_body(toofar,nonterminate) :- !.
recollect_body(noclause,nofact) :- !.
recollect_body((B : nottried),B) :- !.
recollect_body((B :- _),B) :- !.
recollect_body(((B :- _),Rest),(B,Bs)) :-
    recollect_body(Rest,Bs).


% --------------------------------------------------------------
% DISPLAYING OF RULES
% --------------------------------------------------------------
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

% --------------------------------------------------------------
% EXTRACT VARIABLES FROM A FORMULA
% --------------------------------------------------------------
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

% --------------------------------------------------------------
% Displaying of goals in diner expert system
% --------------------------------------------------------------
display_goal(best_restaurant(X)) :- !,
    write_list(['restaurant ',X,' suits you best.']).
display_goal(serve_favourites(X)) :- !,
    write_list(['restaurant ',X]),
    write_list([' is famous in your favourite dishes']).
display_goal(good_price(X)) :- !,
    write_list(['restaurant ',X]),
    write_list([' charges reasonable price']).
display_goal(close_location(X)) :- !,
    write_list(['restaurant ',X]),
    write_list([' is conveniently close to you']).
display_goal(your_favourites(X)) :- !,
    write_list(['your favourite dishes are ',X]).
display_goal(specialties(X,Y)) :- !,
    write_list(['restaurant ',X,' is famous in ',Y]).
display_goal(included(X,Y)) :- !,
    write_list(['dishes ',X,' are included in ',Y]).
display_goal(your_price(X)) :- !,
    write_list(['your price limit is ',X]).
display_goal(average_price(X,Y)) :- !,
    write_list(['average price at restaurant ',X]),
    write_list([' is ',Y]).
display_goal(your_location(X)) :- !,
    write_list(['your location is ',X]).
display_goal(restaurant_location(X,Y)) :- !,
    write_list(['the location of restaurant ',X]),
    write_list([' is ',Y]).
display_goal(distance(X,Y,D)) :- !,
    write_list(['the distance from ',X,' to ',Y]),
    write_list([' is ',D]).
display_goal(dist(X,Y,D)) :- !,
    write_list(['from ',X,' to ',Y]),
    write_list([' is ',D,' km']).

display_goal(A) :- syst(A),write(A).

write_list([]) :- !.
write_list([X|T]) :- write(X),write_list(T).

% syst(A) :- functor(A,F,N),
%     (system(F/N); member(F,['!',member,included])).

% --------------------------------------------------------------
% Displaying of questions in diner expert system
% --------------------------------------------------------------
display_question(your_favourites(_)) :- nl,
    write('  > What are your favourites ?'),
    write(' (List your favourite dishes'), nl,
    write('    in the form [lobster,fish,clam,etc]): ').
display_question(your_price(_)) :- nl,
    write('  > What price per head are you willing'),
    write(' to pay ? : ').
display_question(your_location(_)) :- nl,
    write('  > Where do you live ? : ').

% ---------------------------------------------------------------
% The knowledge-base of the diner expert system
% ---------------------------------------------------------------
best_restaurant(X) :-
    serve_favourites(X),
    good_price(X),
    close_location(X).

serve_favourites(X) :-
    your_favourites(Y),
    specialties(X,Z),
    included(Y,Z).

good_price(X) :-
    your_price(Y),
    average_price(X,Z),
    Z =< Y + 5.

close_location(X) :-
    your_location(Y),
    restaurant_location(X,Z),
    distance(Y,Z,D),
    D =< 20.

included([],_) :- !.
included([X|T],L) :- member(X,L),included(T,L).

distance(X,Y,D) :- dist(X,Y,D).
distance(X,Y,D) :- dist(Y,X,D).

specialties('Le Marecage',[chicken,duck,eel,frog]).
specialties('Le Tour d''Ivoire',[duck,pigeon,quail]).
specialties('Le Grand Couteau',[duck,frog,snake,snail]).

average_price('Le Marecage',25).
average_price('Le Tour d''Ivoire',40).
average_price('Le Grand Couteau',34).

restaurant_location('Le Marecage','Clamart').
restaurant_location('Le Tour d''Ivoire','Paris-15').
restaurant_location('Le Grand Couteau','Paris-17').

dist('Paris-15','Paris-18',15).
dist('Paris-18','Paris-17',8).
dist('Paris-18','Clamart',35).

tobe_filled(your_favourites(_)).
tobe_filled(your_price(_)).
tobe_filled(your_location(_)).
