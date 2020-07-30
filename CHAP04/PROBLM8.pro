% PROBLEM 4.8: Obtain the tokens from a list of characters
%----------------------------------------------------------------
% This file contains the procedure 'tokenize' that receives a
% list of characters, and returns the list of tokens contained
% in the text. At the end of this file is a list of character-
% lists designed to facilitate the experiments with this program.
% Consult this file and try the following queries (Here, the goal
% 'write_list' is used to print out the tokens for checking):
%
%   ?- sample(1 ,L),tokenize(L,T),write_list(T).
%   ?- sample(2 ,L),tokenize(L,T),write_list(T).
%      ...
%   ?- sample(30,L),tokenize(L,T),write_list(T).
%
% Observe that here, the procedure 'tokenize' detects the end of
% the list when it encounters the period. Also, to allow the
% program run with standard Prolog, each backquoted character in
% this program is replaced with its ASCII code. For instance,
% `a is replaced with 97 (For the ASCII set, see Appendix D).
%----------------------------------------------------------------

    tokenize([],[]).
    tokenize(CharList,[Token|TList]) :-
        append(_,[C|List],CharList), C \= 32,!,
        get_token([C|List],Token,Rest),
        tokenize(Rest,TList).

    get_token(List,Token,Rest) :-
        get_chars(List,Lchars,Rest),
        name(Token,Lchars).

    get_chars(L,S,L1) :- separator(L,S,L1),!.
    get_chars([C|L],[C|Lc],L1) :-
        check_start(S,C),
        get_word_chars(S,L,Lc,L1).

    get_word_chars(S,L,Lc,L1) :-
        check_end(S,L,Lc,L1).
    get_word_chars(S,[C|L],[C|Lc],L1) :-
        legal_char(S,C),
        get_word_chars(S,L,Lc,L1).

    legal_char(quote,C) :- C \= 39.
    legal_char(num,C)   :- digit(C).
    legal_char(symb,C)  :- valid_char(C).

    check_start(quote,39).
    check_start(num, C)  :- digit(C).
    check_start(symb,C)  :-
        valid_char(C), not(digit(C)).

    check_end(_,[],[],[]) :- !.
    check_end(quote,[39|L],[39],L) :- !.
    check_end(num, [C|L],[],[C|L]) :- not(digit(C)),!.
    check_end(symb,[C|L],[],[C|L]) :- not(valid_char(C)).

  separator([C,D,E|L],[C,D,E],L) :- name(S,[C,D,E]),
    member(S,['=:=','=\=','\==','@=<','@>=','=..','-->']),!.
  separator([C,D|L],[C,D],L) :- name(S,[C,D]),
    member(S,[(':-'),'\+','->','\=','==','@<','@>','=<','>=',
               '//']),!.
  separator([C|L],[C],L) :-
    member(C,[44,40,41,58,91,93,124,43,45,42,47,59,61,60,62,94]).

    valid_char(C) :- letter(C); digit(C); C = 95.
    letter(C) :-  97 =< C, C =< 122; 65 =< C, C =< 90.
    digit(C)  :-  48 =< C, C =< 57.

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

sample(1,"likes(sue, X)").
sample(2,"likes(mother(X,Y), father(X,john))").
sample(3,"greater(max(X,452), glb(fact(X,Y+10)))").
sample(4,"length([X,Y,Z],3)").
sample(5,"length([H|T],N)").
sample(6,"member(15,[3,5,7|T])").
sample(7,"list([a,b,c])").
sample(8,"list([1,2,3|[a,b,c]])").
sample(9,"list([1,2,3|[]])").
sample(10,"supply(agent: 'Adams & Sons', item:b1)").
sample(11,"10+20+30+40").
sample(12,"(10+20+30)*24").
sample(13,"(X+Y)*(Y-1)").
sample(14,"derivative(sin(X+Y),Z)").
sample(15,"not(non(supply(agent:'Adams & Sons',item:X)))").
sample(16,"all(supply(S:X,T:Y),not(non(supply(S:X,T:Z))))").
sample(17,"fault(X)").
sample(18,"non(fault(X))").
sample(19,"all(fault(X))").
sample(20,"all(non(fault(X)))").
sample(21,"all([X],respond(X,_))").
sample(22,"all([X],non(respond(X,_)))").
sample(23,"likes(ann,X) :- toy(X), plays(ann, X)").
sample(24,"fault(X) :- non(respond(X,Y)), X \== Y").
sample(25,"respond(a,b) :- true").
sample(26,"respond(c,X) :- true").
sample(27,"p(X) :- a(X),b(X)").
sample(28,"gcd(X,0,X) :- X > 0").
sample(29,"gcd(X,Y,Z) :- X < Y, gcd(Y,X,Z)").
sample(30,"gcd(X,Y,Y) :- X >= Y, Y > 0, X1 is X mod Y, gcd(Y,X1,Z)").

write_list([]) :- nl.
write_list([H|T]) :- write(H),write_list(T).







