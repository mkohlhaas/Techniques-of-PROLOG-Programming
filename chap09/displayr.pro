% FIGURE 31: ESSLN'S DISPLAYING OF GOALS
% ====================================================================
% This file contains ESSLN's goal displaying program and a sample
% knowledge-base on supplier-part at the end. Consult this file
% and try the following queries:
%   ?- display_goal(p1(agent:'Johnny Ltd'),yes).
%   ?- display_goal(p1(agent:'Adams & Sons'),no).
%   ?- display_goal(p1(agent:X),yes).
%   ?- display_goal(p1(agent:X),no).
%   ?- display_goal(p2(agent:'Mitre 10',item:b1),yes).
%   ?- display_goal(non(p2(agent:'Mitre 10',item:b2)),yes).
%   ?- display_goal(non(p2(agent:'Adams & Sons',item:b2)),no).
%   ?- display_goal(not(p2(agent:'Rossianno Co',T:Y)),yes).
%   ?- display_goal(not(p2(agent:'Rossianno Co',T:Y)),no).
%   ?- display_goal(not(non(p2(agent:'Adams & Sons',T:Y))),yes).
%   ?- display_goal(not(non(p2(agent:'Johnny Ltd',T:Y))),no).
%   ?- display_goal(all(p2(agent:'Adams & Sons',item:X),
%                       not(non(p2(agent:'Adams & Sons',item:Y)))),yes).
%   ?- display_goal(all(p2(S:X,T:Y),not(non(p2(S:X,T:Z)))),yes).
%   ?- display_goal(all(p2(S:X,T:Y),not(non(p2(S:X,T:Z)))),no).
%   ?- display_goal(all(non(p2(agent:'Rossianno Co',item:Y)),
%                       not(p2(agent:'Rossianno Co',item:Z))),yes).
%   ?- display_goal(all(non(p2(S:X,T:Y)),not(p2(S:X,T:Z))),yes).
%   ?- display_goal(all(non(p2(S:X,T:Y)),not(p2(S:X,T:Z))),no).
%
% ---------------------------------------------------------------------
%  Note: If your Prolog system has difficulty with (or does not provide)
%  the predicate "setof", then replace the goal
%  "setof(T:X,(member(T:X,Args),var(X)),L)" in the procedure
%  "tvar_list" with the goal "get_tvars(Args,[],L)"  and add the
%  following clauses to this program (by removing the symbols %):
%  :- op(500,xfy,:).
%      get_tvars([],_,[]).
%      get_tvars([T:X|Args],L,L1) :-
%          (nonvar(X); occurs(T:X,L)),!,get_tvars(Args,L,L1).
%      get_tvars([A|Args],L,[A|L1]) :- get_tvars(Args,[A|L],L1).
%
%      occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).
% ======================================================================
:- discontiguous(predicate/2).
:- op(500,xfx,#).
:- op(500,xfy,:).

display_goal(all(A,not(non(B))),S) :- !,
    univ_quantify(S,Q),univ_quantify(yes,Q1),
    tvar_list(A,VL),bind_vars(VL,Q),
    print_goal(B,Q1).
display_goal(all(non(A),not(B)),S) :- !,
    univ_quantify(S,Q),univ_quantify(no,Q1),
    tvar_list(A,VL),bind_vars(VL,Q),
    print_goal(B,Q1).
display_goal(not(non(A)),S) :- !,
    (S = no,!,univ_quantify(some,Q),print_goal(non(A),Q);
     univ_quantify(yes,Q), print_goal(A,Q)).
display_goal(not(A),S) :- !,
    (S = no,!,univ_quantify(some,Q),print_goal(A,Q);
     my_ground(A),!,print_goal(non(A),_);
     univ_quantify(no,Q), print_goal(A,Q)).
display_goal(non(A),S) :- !,
    (S = no,!,univ_quantify(yes,Q),print_goal(A,Q);
     univ_quantify(S,Q), print_goal(non(A),Q)).
display_goal((A,B),no) :- !,
    (my_ground((A,B)),!,write_word(no); write_word(none)).
display_goal((A,B),yes) :- !,
    display_goal(A,yes),
    nl,tab(4),display_goal(B,yes).
display_goal(A,S) :-
    (S = no,my_ground(A),!,print_goal(non(A),_);
    univ_quantify(S,Q),print_goal(A,Q)).

univ_quantify(yes,Q) :- trans_word(all,Q).
univ_quantify(no,Q) :- trans_word(no,Q).
univ_quantify(some,Q) :- trans_word(some,Q).

tvar_list(A,[]) :- my_ground(A),!.
tvar_list(A,L) :-  A =.. [_|Args],
    setof(T:X,(member(T:X,Args),var(X)),L).

my_ground(A) :- copy(A,B), A == B.
copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

bind_vars([],_) :- !.
bind_vars([_:S|Xs],S) :- bind_vars(Xs,S).

write_word(W) :- trans_word(W,W1),write(W1).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% --------------------------------------------------------------------
% PRINT GOALS
% --------------------------------------------------------------------
print_goal(non(A),Q) :- !,
    domain(A),
    (predicate(PNA,non(A)),!,print_phrase(PNA,Q);
     predicate(PA,A),negate_quantify(Q,Q1,W),
     print_phrase([W|PA],Q1)).
print_goal(A,Q) :-
    domain(A),
    predicate(Phrase,A),
    print_phrase(Phrase,Q).

print_phrase([],_) :- !.
print_phrase([(T,S:X)|Rest],Q) :- !,
    (var(X),!,write_list([Q,' ',S,' ']);
     quantifier(X),!,write_list([X,' ',S,' ']);
     write_list([T,' ',X,' '])),
    print_phrase(Rest,Q).
print_phrase([W|Rest],Q) :-
    write_list([W,' ']),
    print_phrase(Rest,Q).

negate_quantify(Q,Q1,W) :-
    trans_word(deny,W),
    (trans_word(some,Q),trans_word(all,Q1);
     trans_word(all,Q),trans_word(some,Q1)).

quantifier(X) :- 
    trans_word(W,X),member(W,[all,no,some]).
write_list([]).
write_list([X|T]) :- 
    write(X),write_list(T).

% --------------------------------------------------------------------
% ENGLISH DICTIONARY
% --------------------------------------------------------------------
trans_word(X,X).

% --------------------------------------------------------------------
% BUILTIN PREDICATES
% --------------------------------------------------------------------
predicate([Phrase,B],tvar_list(_,B)) :-
    trans_word('using the substitution',Phrase).
predicate([Phrase|PNA],eval_non(A,_)) :-
    domain(A),
    (predicate(PNA,non(A)),!;
     predicate(PA,A),trans_word(deny,W),PNA = [W|PA]),
    trans_word('evaluating negation shows',Phrase).
predicate([Phrase|PA],eval(A)) :-
    domain(A),predicate(PA,A),
    trans_word('evaluating goal shows',Phrase).
predicate([L,Phrase],uninstantiated(L)) :-
    trans_word('uninstantiated',Phrase).
predicate([Phrase,L],instantiate(_,L,_)) :-
    trans_word('using the instantiation',Phrase).
predicate([Find|Phrase],tobe_filled(A)) :-
    domain(A),predicate(Phrase,A),
    trans_word(find,Find).
predicate([Phrase],!) :- trans_word(stop,Phrase).
predicate([Fail],fail) :- trans_word(fail,Fail).
predicate([(T,A),Is,(T,B)],(A = B)) :- trans_word(is,Is).
predicate([(T,A),Op,(T,B)],(A is B)) :- (Op == '=').
predicate([(T,A),Eq,(T,B)],(A =:= B)) :- trans_word(eq,Eq).
predicate([(T,A),Dif,(T,B)],(A =\= B)) :- trans_word(dif,Dif).
predicate([(T,A),Lt,(T,B)],(A < B)) :- trans_word(lt,Lt).
predicate([(T,A),Gt,(T,B)],(A > B)) :- trans_word(gt,Gt).
predicate([(T,A),Le,(T,B)],(A =< B)) :- trans_word(le,Le).
predicate([(T,A),Ge,(T,B)],(A >= B)) :- trans_word(ge,Ge).
predicate([(T,A),Lta,(T,B)],(A @< B)) :- trans_word(lta,Lta).
predicate([(T,A),Gta,(T,B)],(A @> B)) :- trans_word(gta,Gta).
predicate([(T,A),Lea,(T,B)],(A @=< B)) :- trans_word(lea,Lea).
predicate([(T,A),Gea,(T,B)],(A @>= B)) :- trans_word(gea,Gea).
predicate([(_,A),In,(list,B)],member(A,B)) :- trans_word(in,In).

%-------------------------------------------------------------------
% PROLOG KNOWLEDGE-BASE OF SUPPLIER-PART SYSTEM
%===================================================================
p1(A:B) # 1 :- non(p2(A:B,_:_)).

p2(agent:'Adams & Sons',item:b1) # 1.
p2(agent:'Adams & Sons',item:b2) # 1.
p2(agent:'Adams & Sons',item:b3) # 1.
p2(agent:'Johnny Ltd',item:b1) # 1.
p2(agent:'Johnny Ltd',item:b2) # 1.
p2(agent:'Mitre 10',item:b1) # 1.

predicate([(agent,A:B),is,a,fractional,supplier],p1(A:B)).
predicate([(agent,A:B),is,not,a,fractional,supplier],non(p1(A:B))).
predicate([(agent,A:B),supply,(item,C:D)],p2(A:B,C:D)).
predicate([(agent,A:B),not,supply,(item,C:D)],non(p2(A:B,C:D))).

domain(p1(A:_)) :- type(A,agent).
domain(p2(A:_,C:_)) :- type(A,agent),type(C,item).

constsymbol(agent:'Adams & Sons').
constsymbol(agent:'Johnny Ltd').
constsymbol(agent:'Mitre 10').
constsymbol(item:b1).
constsymbol(item:b2).
constsymbol(item:b3).

type(T,T).
type(S,T) :- subtype(S,T).

subtype('@','#').
