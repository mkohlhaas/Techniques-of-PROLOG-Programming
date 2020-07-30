% FIGURE 9.4-7: Querying the user and generating explanations
% =================================================================
%  This file contains the procedure 'ask_user' and three sample
%  goals and rules on a diner expert system at the end of the file.
%  Consult this file and enter the following queries to demonstrate
%  the task of querying the user and answering why-questions.
%  For each round, type "why." for the system's first request and
%  give an answer to the second:
%    ?- sample(1,Goal,Rules),
%       ask_user(Goal,Rules).
%
%    ?- sample(2,Goal,Rules),
%       ask_user(Goal,Rules).
%
%    ?- sample(3,Goal,Rules),
%       ask_user(Goal,Rules).
%
%  Note: the builtin predicate "system" used in this program (to
%  test if a predicate is a builtin predicate) may correspond to
%  "sys" (or something else) in your Prolog system. In which case
%  change this by using the following command:
%    ?- assert((system(F/N) :- sys(F))).
%
%  Also, if your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal "setof(X,member(X,Q),L)"
%  in the procedure "var_list" with the goal "elim_dup(Q,[],L)" and add
%  the following clauses to this program (by removing the symbols %):
%      elim_dup([],_,[]).
%      elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
%      elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ----------------------------------------------------------------------
:- op(500,xfy,:).

ask_user(Goal,Rules) :-
    display_question(Goal),
    read(Answer),
    process_answer(Answer,Goal,Rules).

process_answer(why,Goal,[Rule|Rules]) :- !,
    display_rule(Rule),nl,
    ask_user(Goal,Rules).
process_answer(why,Goal,[]) :- !,
    nl,write('  > No more explanations!'),nl,
    ask_user(Goal,[]).
process_answer(X,Goal,_) :-
    var_list(Goal,[V]), V = X,
    assert(already_asked(Goal)).

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
% Displaying of questions
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

% ----------------------------------------------------------
% Three sample goals and rules
% ----------------------------------------------------------
sample(1,your_favourites(Y),
        [(serve_favourites(X) :-
             your_favourites(Y),
             specialties(X,Z),
             included(Y,Z))]).

sample(2,your_price(Y),
        [(good_price(X) :-
             your_price(Y),
             average_price(X,Z),
             Z =< Y + 5)]).

sample(3,your_location(Y),
        [(close_location(X) :-
             your_location(Y),
             restaurant_location(X,Z),
             distance(Y,Z,D),
             D =< 20)]).
