% PROBLEM 10.4: Extension of the query-answering system
% =====================================================================
% This file contains an extension of the DCG given in Figure 10.5
% and an extended supplier-part database at the end of the file.
% Consult this file and enter the following query to invoke the system:
%   ?- dbqas.
% Then try the following queries. For each query, press <Return> to
% obtain alternative answers; press e to terminate the current query.
%   Q:> where is agent 'Adams & Sons' based?
%   Q:> what does agent 'Adams & Sons' supply?
%   Q:> where is agent 'Mitre 10' based?
%   Q:> what does agent 'Mitre 10' supply?
%   Q:> where is agent 'Yoshima Kuru' based?
%   Q:> what does agent 'Yoshima Kuru' supply?
%   Q:> where is agent 'Rossianno Co' based?
%   Q:> what does agent 'Rossianno Co' supply?
%
% To exit the system, enter the query Q:> e.
% ====================================================================
:- discontiguous(non/1).
:- discontiguous(constsymbol/1).
:- discontiguous(noun/4).
:- discontiguous(verb/6).
:- discontiguous(sentence/3).
:- op(500,xfy,:).

dbqas :-
    repeat,
        receive_query(Q),
        process_query(Q,R),
        print_answer(R),
    terminate(Q).

process_query(Q,R) :-
    my_once(sentence(S,Q,[])),
    translate(S,T,A),
    call(T),response(A,R).
process_query([e],[e]) :- !.
process_query(_,[no]) :-
    not(answer([yes])).
process_query(_,_) :-
    abolish(answer/1),fail.

response(confirm,[yes]) :- !,
    assert(answer([yes])).
response(A,R) :-
    transback(A,B),
    my_once(sentence(B,R,[])),
    not(answer(R)),assert(answer(R)).

terminate(Q) :- Q = [e]; (get0(101),nl,dbqas).
my_once(P) :- P,!.

answer('#','#').

% -------------------------------------------------------------------
% Read a query sentence into a list of words
% -------------------------------------------------------------------
receive_query(S) :-
    nl,write('Q:> '),
    read_sentence(S),nl.

read_sentence(L) :- read_sent(32,L).

read_sent(C,L) :- end_sent(C),!,L = [].
read_sent(_,[W|L]) :-
    read_word(W,C1),read_sent(C1,L).

read_word(W,C1) :-
    read_first_char(S,C),
    read_word_chars(S,C,[],L,C1),
    reverse(L,[],L1),name(W,L1).

read_first_char(S,C) :-
    my_get_char(S),start_word(S,C),!;
    read_first_char(S,C).

read_word_chars(S,C,L,L,C1) :- end_word(S,C,C1),!.
read_word_chars(S,8,[_|L],L1,C1) :- !,
    put(32),put(8),
    my_get_char(C2),
    read_word_chars(S,C2,L,L1,C1).
read_word_chars(S,C,L,L1,C1) :-
    legal_char(S,C),
    my_get_char(C2),
    read_word_chars(S,C2,[C|L],L1,C1).

my_get_char(C) :- get0(C),(C = 13,!,nl; true).
valid_char(C) :-
    96 < C, C < 123;  % a-z
    64 < C, C < 91;   % A-Z
    47 < C, C < 58;   % 0-9
    C = 45.        % hyphen.

start_word(C,C)   :- valid_char(C),!.
start_word(39,C)  :- my_get_char(C).
end_word(39,39,C) :- my_get_char(C).  % quotation marks
end_word(Q,C,C)   :- Q \= 39,C \= 8,not(valid_char(C)).
end_sent(C)       :- (C = 46; C = 63). % period or ?

legal_char(39,_) :- !.
legal_char(_,C)  :- valid_char(C).

reverse([],L,L).
reverse([H|T],L,L1) :- reverse(T,[H|L],L1).

% -------------------------------------------------------------------
% Print out the answer
% -------------------------------------------------------------------
print_answer([e]) :- !.
print_answer(R) :-
    write('A:> '),write_list(R).

write_list([]) :- nl.
write_list([X|T]) :-
    write(X),write(' '), write_list(T).

% --------------------------------------------------------------------
% FIGURE 10.5: The definite clause grammar parser
% --------------------------------------------------------------------
sentence(S) -->
    noun_phrase(P,S,E,VP),verb_phrase(P,E,VP).

noun_phrase(P,NP,E,F) -->
    determiner(P,NP,E,F),noun(P,E).
noun_phrase(P,F,E,F) -->
    noun(P,E),proper_noun(P,E).

verb_phrase(P,X,VP) -->
    verb(P,F,X,Y),noun_phrase(_,VP,Y,F).
verb_phrase(P,X,VP) -->
    neg_verb(P,F,X,Y),noun_phrase(_,VP,Y,F).

determiner(P,all(E,F),E,F) -->
    [W],{my_once(member(W:P,
    [all:plural,every:sgular,each:sgular]))}.
determiner(_,exist(E,F),E,F) -->
    [W],{member(W,[some,which])}.
determiner(_,no(E,F),E,F) --> [no].
determiner(plural,A,E,F) -->
    [W],{member(W,[most,few]), A =.. [W,E,F]}.

noun(sgular,T:_) -->
    [T],{member(T,[agent,item])}.
noun(plural,agent:_) --> [agents].
noun(plural,item:_) -->  [items].

proper_noun(sgular,T:X) -->
    [X],{proper_name(T:X)}.

verb(sgular,supply(X,Y),X,Y) --> [supplies].
verb(plural,supply(X,Y),X,Y) --> [supply].
neg_verb(P,non(F),X,Y) -->
    negation(P),verb(plural,F,X,Y).

negation(sgular) --> [does,not].
negation(plural) --> [do,not].

proper_name(N) :- constsymbol(N).

% -------------------------------------------------------------------
% Extension of the DCG to parse database manager commands
% -------------------------------------------------------------------
sentence(S) -->
    command(S,F),fact(F).

command(add(S),S) --> [add].
command(append(S),S) --> [append].
command(delete(S),S) --> [delete].

fact(constsymbol(T:X)) --> noun(_,T:X),[X].
fact(F) --> sentence(F).

% --------------------------------------------------------------------
% Extention of the DCG to parse the queries 'where is' and 'what does'
% --------------------------------------------------------------------
sentence(S) -->
    query_part(M,S,E,F),proposition(M,E,F).

query_part(M,exist(E,F),E,F) -->
    query_term,aux_verb(M).

proposition(M,E,F) -->
    noun_phrase(_,_,N,_),verb(M,F,N,E).

query_term --> [W],{member(W,[where,what])}.

aux_verb(passive) --> [is].
aux_verb(active) --> [does].

verb(passive,base(X,Y),X,Y) --> [based].
verb(active,supply(X,Y),X,Y) --> [supply].
verb(sgular,base(X,Y),X,Y) --> [is,based,in].

noun(sgular,city:_) --> [city].

% -------------------------------------------------------------------
% FIGURE 10.6: Definition of 'most' and 'few' predicates
% -------------------------------------------------------------------
most(X,Y,A) :-
    complement(X,Y,A,L1,L2),
    large(L1,L2).

few(X,Y,A) :-
    complement(X,Y,A,L1,L2),
    large(L2,L1).

complement(X,Y,A,L1,L2) :-
    (setof(X,Y^A,L1),!; L1 = []),
    (setof(X,(constsymbol(X),
     not(member(X,L1))),L2),!; L2 = []).

large(L1,L2) :-
    length(L1,N1),length(L2,N2),
    N1 >= 5*N2.

% -------------------------------------------------------------------
% FIGURE 10.7-10.8: Prolog representation of logical formulas
% -------------------------------------------------------------------
translate(exist(X,exist(Y,A)),A,(X,Y,A)) :- !.
translate(exist(X,all(Y,A)),(A,not(non(A1))),(X,all(Y1,A1))) :- !,
    copy((X,Y,A),(X1,Y1,A1)),X = X1.
translate(exist(X,no(Y,A)),(non(A),not(A1)),(X,no(Y1,A1))) :- !,
    copy((X,Y,A),(X1,Y1,A1)),X = X1.
translate(exist(X,most(Y,A)),(A,most(Y1,0,A1)),(X,most(Y1,A1))) :- !,
    copy((X,Y,A),(X1,Y1,A1)),X = X1.
translate(exist(X,few(Y,A)),(non(A),few(Y1,0,A1)),(X,few(Y1,A1))) :- !,
    copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(exist(X,A),A,(X,A)) :- !.

translate(all(X,exist(_,A)),not((non(A),not(A1))),_) :- !,
    copy((X,A),(X1,A1)), X = X1.
translate(all(_,all(_,A)),not(non(A)),_) :- !.
translate(all(_,no(_,A)),not(A),_) :- !.
translate(all(X,most(Y,A)),not((non(A),not(most(Y1,0,A1)))),_)
    :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(all(X,few(Y,A)),not((A,not(few(Y1,0,A1)))),_) :- !,
    copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(all(_,A),not(non(A)),_) :- !.

translate(no(_,exist(_,A)),not(A),_) :- !.
translate(no(X,all(_,A)),not((A,not(non(A1)))),_) :- !,
    copy((X,A),(X1,A1)), X = X1.
translate(no(X,no(_,A)),not((non(A),not(A1))),_) :- !,
    copy((X,A),(X1,A1)), X = X1.
translate(no(X,most(Y,A)),not((A,most(Y1,0,A1))),_) :- !,
    copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(no(X,few(Y,A)),not((non(A),few(Y1,0,A1))),_) :- !,
    copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(no(_,A),not(A),_) :- !.

translate(most(X,exist(Y,A)),most(X,Y,A),_) :- !.
translate(most(X,all(Y,A)),few(X,Y,non(A)),_) :- !.
translate(most(X,no(Y,A)),few(X,Y,A),_) :- !.
translate(most(X,most(Y,A)),most(X,Y,(A,most(Y1,0,A1))),_)
    :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(most(X,few(Y,A)),most(X,Y,(non(A),few(Y1,0,A1))),_)
    :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(most(X,A),most(X,0,A),_) :- !.

translate(few(X,exist(Y,A)),few(X,Y,A),_) :- !.
translate(few(X,all(Y,A)),most(X,Y,non(A)),_) :- !.
translate(few(X,no(Y,A)),most(X,Y,A),_) :- !.
translate(few(X,most(Y,A)),few(X,Y,(A,most(Y1,0,A1))),_)
    :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(few(X,few(Y,A)),few(X,Y,(non(A),few(Y1,0,A1))),_)
    :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
translate(few(X,A),few(X,0,A),_) :- !.

% -------------------------------------------------------------------
% Extension of the translator to process manager commands
% -------------------------------------------------------------------
translate(add(A),asserta(B),_)    :- !,trans(A,B).
translate(append(A),assertz(B),_) :- !,trans(A,B).
translate(delete(A),retract(B),_) :- !,trans(A,B).

translate(A,A,_).

transback((T:X,A),all(T:X,B)) :- var(X),!,transback(A,B).
transback((_,A),B) :- !,transback(A,B).
transback(A,A).

trans(all(_,A),B) :- !,trans(A,B).
trans(no(_,A),non(B)) :- !,trans(A,B).
trans(A,A).

% -------------------------------------------------------------------
% FIGURE 10.15: Portion of a supplier-part database
% -------------------------------------------------------------------
supply(agent:'Adams & Sons',item:b1).
supply(agent:'Adams & Sons',item:b2).
supply(agent:'Adams & Sons',item:b3).
supply(agent:'Adams & Sons',item:b4).
supply(agent:'Adams & Sons',item:b5).
supply(agent:'Adams & Sons',item:b6).
supply(agent:'Johnny Ltd',item:b1).
supply(agent:'Johnny Ltd',item:b2).
supply(agent:'Johnny Ltd',item:b3).
supply(agent:'Johnny Ltd',item:b4).
supply(agent:'Johnny Ltd',item:b5).
supply(agent:'Mitre 10',item:b1).
supply(agent:'Yoshima Kuru',item:_).
non(supply(agent:'Rossianno Co',item:_)).

constsymbol(agent:'Adams & Sons').
constsymbol(agent:'Johnny Ltd').
constsymbol(agent:'Mitre 10').
constsymbol(agent:'Rossianno Co').
constsymbol(agent:'Yoshima Kuru').
constsymbol(item:b1).
constsymbol(item:b2).
constsymbol(item:b3).
constsymbol(item:b4).
constsymbol(item:b5).
constsymbol(item:b6).

base(agent:'Adams & Sons',city:'London').
base(agent:'Johnny Ltd',city:'New York').
base(agent:'Mitre 10',city:'Sydney').
base(agent:'Rossianno Co',city:'Milan').
base(agent:'Yoshima Kuru',city:'Tokyo').

constsymbol(city:'London').
constsymbol(city:'New York').
constsymbol(city:'Sydney').
constsymbol(city:'Milan').
constsymbol(city:'Tokyo').

% -------------------------------------------------------------------
% FIGURE 10.9-10.10: The logical negation evaluator
% -------------------------------------------------------------------
non(non(A)) :- !,A.
non(A) :- tvar_list(A,L),eval_non(A,L).

eval_non(A,_) :- not(A),!.
eval_non(A,L) :- eval(A),uninstantiated(L),!,fail.
eval_non(A,L) :- instantiate(A,L,VL),eval_non(A,VL).

eval(A) :- A,!.
uninstantiated(L) :- tvar_pure(L),unrestricted(L,0).

tvar_pure([]) :- !.
tvar_pure([_:V|TVs]) :- var(V),tvar_pure(TVs).

unrestricted([],_) :- !.
unrestricted([_:N|TVs],N) :-
    N1 is N+1,unrestricted(TVs,N1).

instantiate(A,L,VL) :- domain(A),instant(L,VL).

instant([X|Xs],Xs) :- get_term(X,Xs).
instant([X|Xs],[X|VL]) :- instant(Xs,VL).

get_term(T:V,_) :- constsymbol(T:V).
get_term(X,Xs) :- get_var(X,Xs).

get_var(T:V,[T:V|_]).
get_var(X,[_|Xs]) :- get_var(X,Xs).

tvar_list(A,[]) :- my_ground(A),!.
tvar_list(A,L) :-  A =.. [_|Args],
    setof(T:X,(member(T:X,Args),var(X)),L).

domain(supply(S:_,T:_)) :-
    type(S,agent),type(T,item).

type(T,T).
type(S,T) :- subtype(S,T).

subtype('#').

my_ground(A) :- copy(A,B), A == B.
copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
