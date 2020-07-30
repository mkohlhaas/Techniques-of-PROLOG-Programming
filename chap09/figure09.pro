% FIGURE 9.9-11: Producing explanation for an answer
% =====================================================================
%  This file contains the procedures 'process_result' and
%  'process_response' and a sample proof of an answer to the user by
%  a diner expert system.
%  Consult this file and enter the following query to study the process
%  of answering a how-question from the user.
%  Type 'how.' at each prompt '>' until the explanation is either
%  "usergiven" or "system:yes" or "is true". For these, type 'ok.'
%  to accept it, then continue to ask 'how.' again till no more
%  explanations.
%     ?- sample(1,Goal,Result,Proof),
%        process_result(Goal,Result,Proof).
%
%  Note: the builtin predicate "system" used in this program (to
%  test if a predicate is a builtin predicate) may correspond
%  to a predicate "sys" (or something else) in your Prolog system.
%  In which case, change this by using the following command:
%      ?- assert((system(F/N) :- sys(F))).
%  Also, if your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ----------------------------------------------------------
:- op(500,xfy,:).

 process_result(bye,_,_) :- !.
 process_result(Goal,Result,Proof) :-
     nl,write('A:> '),
     (Result = no,write('No');
      Result = yes, display_goal(Goal)),
     nl,write('  > '),
     read(Response),
     process_response(Response,Proof).

% ----------------------------------------------------------
% Procedure process_response
% ----------------------------------------------------------
process_response(ok,_) :- !.
process_response(how,(ProofA,ProofB)) :- !,
    process_response(how,ProofA),
    nl,write('  > '),read(Response),
    process_response(Response,ProofB).
process_response(how,(A :- Proof)) :- !,
    recollect_body(Proof,Body),
    display_rule((A :- Body)),
    nl,write('  > '),read(Response),
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
% Displaying of rules
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
% Displaying of goals
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

display_goal(A) :- syst(A),write(A).

write_list([]) :- !.
write_list([X|T]) :- write(X),write_list(T).

syst(A) :- functor(A,F,N),
    (system(F/N); member(F,['!',member,included])).

% --------------------------------------------------------------
% Program to extract the variables of a formula
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

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% A sample proof of an answer
%------------------------------------------------------------
sample(1,best_restaurant('Le Grand Couteau'),yes,
    (best_restaurant('Le Grand Couteau') :-
        (serve_favourites('Le Grand Couteau') :-
            (your_favourites([duck,frog]) :- usergiven),
            (specialties('Le Grand Couteau',
                      [duck,frog,snake,snail]) :- fact),
            (included([duck,frog],[duck,frog,snake,snail])
                      :- system:yes)),
        (good_price('Le Grand Couteau') :-
            (your_price(30) :- usergiven),
            (average_price('Le Grand Couteau',34) :- fact),
            (34 =< 30 + 5 :- system:yes)),
        (close_location('Le Grand Couteau') :-
            (your_location('Paris-18') :- usergiven),
            (restaurant_location('Le Grand Couteau','Paris-17')
                      :- fact),
            (distance('Paris-18','Paris-17',8) :- fact),
            (8 =< 20 :- system:yes)))).
