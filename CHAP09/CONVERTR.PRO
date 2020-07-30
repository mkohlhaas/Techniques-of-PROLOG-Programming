% FIGURE 9.21-27: ESSLN'S CONVERSION OF QUERIES
%------------------------------------------------------------------------
% This file contains the procedures 'receive_query' and 'convert_query'
% in ESSLN, which are written in Arity-Prolog, taking advantage of the
% builtin predicates provided for string handling in Arity-Prolog. For
% their versions in standard Prolog, see the file \chap9\ESSLN.STD.
% Also, part of an internal representation of a 'supplier-part'
% knowledge-base is given at the end of this file. To perform experiments
% on how ESSLN converts a query in user-language into an internal Prolog
% goal, we define the predicate 'essln_goal' as shown below.
%
% Consult this file and try the following queries. Study the results
% carefully:
%   ?- essln_goal(G).
%   Q:> which _agent supply _item b1?
%
%   ?- essln_goal(G).
%   Q:> which _agent not supply _item b1?
%
%   ?- essln_goal(G).
%   Q:> which _agent supply all _item?
%
%   ?- essln_goal(G).
%   Q:> which _agent supply no _item?
%
%   ?- essln_goal(G).
%   Q:> _agent 'Adams & Sons' supply _item b1?
%
%   ?- essln_goal(G).
%   Q:> _agent 'Adams & Sons' not supply _item b2?
%
%   ?- essln_goal(G).
%   Q:> _agent 'Johnny Ltd' supply which _item?
%
%   ?- essln_goal(G).
%   Q:> _agent 'Johnny Ltd' not supply which _item?
%
%   ?- essln_goal(G).
%   Q:> _agent 'Mitre 10' supply all _item?
%
%   ?- essln_goal(G).
%   Q:> _agent 'Mitre 10' supply no _item?
%
%   ?- essln_goal(G).
%   Q:> all _agent supply which _item?
%
%   ?- essln_goal(G).
%   Q:> no _agent supply which _item?
%
%   ?- essln_goal(G).
%   Q:> which _agent is a fractional supplier?
%
%   ?- essln_goal(G).
%   Q:> all _agent is a fractional supplier?
%
%   ?- essln_goal(G).
%   Q:> no _agent is a fractional supplier?
%
%=====================================================================
    essln_goal(G) :-
        receive_query(Q,63),
        convert_query([],Q,G).
%---------------------------------------------------------------------
% RECEIVE QUERY
%=====================================================================
:- op(500,xfx,#).
receive_query(Q,C) :-
    nl, write('Q:> '),
    readstring($ $,S,C),
    list_text(T,S),
    scan_text(T,Q),!.
receive_query(_,_) :-
    error_query,fail.

readstring(R,S,C) :-
    read_string(100,R1),concat(R,R1,S1),
    (endstring(R1,C1),!, S = S1, C = C1;
     nl,tab(8),readstring(S1,S,C)).

endstring(S,C) :-
    string_length(S,N), M is N-1,
    nth_char(M,S,C), end_char(C).

end_char(C) :- C = 46; C = 63.  % period or ?

scan_text([],[]) :- !.
scan_text(T,[W|L]) :-
    append(_,[C|T1],T), C \= 32,!,  % skip spaces
    get_word([C|T1],W,T2), scan_text(T2,L).

get_word([C|T],C,T) :- member(C,[58,44,46,63]),!. % (: , . ?)
get_word([C|T],[C],T) :- C = 61,!. % (=)
get_word([C|T],[C|W],T2) :- bracket(C,C1),!,get_chars(0,C1,T,W,T2).
get_word([C|T],[C|W],T2) :- valid_start(C),!, get_chars(1,32,T,W,T2).

get_chars(K,C1,[C|T],[C|W],T2) :-
     valid_char(K,C,C1),!,get_chars(K,C1,T,W,T2).
get_chars(0,39,[39|T],[],T) :- !.
get_chars(0,C,[C|T],[C],T) :- (C = 41; C = 93),!. % ) or ]
get_chars(1,C1,[C|T],[],[C|T]) :- member(C,[32,61,58,44,46,63]).

valid_start(C) :- valid(C); C = 37.  % (%)
valid_char(K,C,C1) :- K = 0,!, C \= C1; K = 1, valid(C).

bracket(39,39).  % single quotes
bracket(40,41).  % round brackets ()
bracket(91,93).  % square brackets []

capital(C) :- 65 =< C, C =< 90.
error_query :- nl,write('A:> '),write_word('Illegal query !').
bad_atom(Nt,No,All) :-
     Nt+No+All > 1,
     nl,write('A:> '),
     write_word('Bad clause!'),nl,tab(4),
   write_word('A clause cannot have more than one word "not","no","all".').

unknown_concept :-
     nl,write('A:> '),
     write_word('I dont know this concept. Check your typing'),
     nl,tab(4),write_word('If this is a new concept, please teach me').

% --------------------------------------------------------------------
% CONVERT QUERY INTO PROLOG GOAL
%=====================================================================
convert_query(VS,Q,G) :-
     append(P,[C|Rest],Q),member(C,[44,63]),!,P \= [],
     convert(g,P,VS,VS1,A),      % [ ,  ?]
     (C = 44,!, G = (A,B), convert_query(VS1,Rest,B);
      C = 63,!, G = A).

convert(g,P,VS,VS1,A) :-
     convert_atom(P,VS,VS1,NP,P1,_,P2,[0,0,0,0],QL),
     (predicate(P1,A1),!, quantify(QL,NP,P2,A1,A);
      error_query,!,fail).

convert_atom([Q,[95|W]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V2)|P2],[E,Nt,No,All],QL) :-
     universal(Q),!,name(T,W),
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[E,Nt,No,1],QL).
convert_atom([Q,[95|W]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V2)|P2],[E,Nt,No,All],QL) :-
     neguniversal(Q),!,name(T,W),
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[E,Nt,1,All],QL).
convert_atom([Q,[95|W]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V1)|P2],[E,Nt,No,All],QL) :-
     existential(Q),!,name(T,W),
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[1,Nt,No,All],QL).
convert_atom([Q|Rest],VS,VS1,[Q1|NP],P1,AL,P2,[E,Nt,No,All],QL) :-
     negation(Q,Q1),!,
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[E,1,No,All],QL).
convert_atom([[95|W],[C|L]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V1)|P2],[E,Nt,No,All],QL) :-
     capital(C),!,name(X,[C|L]),name(T,W),
     (member((X,T,S1:V1),VS),!, VS2 = VS; VS2 = [(X,T,S1:V1)|VS]),
     convert_atom(Rest,VS2,VS1,NP,P1,AL,P2,[1,Nt,No,All],QL).
convert_atom([[95|W],A|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:A1)|P1],[T:A1|AL],[(T,S1:A1)|P2],[E,Nt,No,All],QL) :- !,
     aterm(A,VS,E,A1,VS2,E1),name(T,W),
     convert_atom(Rest,VS2,VS1,NP,P1,AL,P2,[E1,Nt,No,All],QL).
convert_atom([W|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:W1)|P1],[T:W1|AL],[(T,S1:W1)|P2],[E,Nt,No,All],QL) :-
     special_term(W,VS,E,T,W1,VS2,E1),
     convert_atom(Rest,VS2,VS1,NP,P1,AL,P2,[E1,Nt,No,All],QL).
convert_atom([W|Rest],VS,VS1,[W1|NP],[W1|P1],AL,[W1|P2],QL0,QL) :-
     !,name(W1,W),convert_atom(Rest,VS,VS1,NP,P1,AL,P2,QL0,QL).
convert_atom([],VS,VS,[],[],[],[],QL,QL).

quantify([_,0,0,0],_,_,A,A) :- !.
quantify([0,0,1,0],_,_,A,not(A)) :- !.
quantify([0,0,0,1],_,_,A,not(non(A))) :- !.
quantify([1,0,0,1],_,P,A,all(A,not(non(B)))) :- !,predicate(P,B).
quantify([1,0,1,0],_,P,A,all(non(A),not(B))) :- !,predicate(P,B).
quantify([_,1,0,0],NP,_,A,non(A)) :- !,
     (predicate(NP,_),!;
      functor(A,F,N),copy_clause(_,NP,VL,_),
      A1 =.. [F|VL],assert(predicate(NP,non(A1)))).
quantify([_,Nt,No,All],_,_,_,_) :-
     (bad_atom(Nt,No,All); unknown_concept),!,fail.


% QUANTIFIERS
% -----------------------------------------------------------------------
universal(Q) :- name(Q1,Q),(trans_word(all,Q1);trans_word(every,Q1)),!.
neguniversal(Q) :- name(Q1,Q),trans_word(no,Q1),!.
existential(Q) :- name(Q1,Q),(trans_word(some,Q1);trans_word(which,Q1)),!.
negation(Q,Q1) :- name(Q1,Q),trans_word(not,Q1),!.
% -----------------------------------------------------------------------

copy_clause([],[],[],[]) :- !.
copy_clause([(T,S:A)|P1],[(T,R:V)|P2],[R:V|VL],[type(R,T)|TL]) :- !,
     copy_clause(P1,P2,VL,TL).
copy_clause([W|P1],[W|P2],VL,TL) :-
     copy_clause(P1,P2,VL,TL).

aterm(W,VS,E,W1,VS,E) :- atomname(W,W1),!.
aterm(W,VS,E,W1,VS2,E1) :- special_term(W,VS,E,_,W1,VS2,E1).

atomname([39|W],W1) :- name(W1,W).
atomname([C|W],W1) :- 96 < C, C < 123,name(W1,[C|W]).

special_term(W,VS,E,T,W1,VS,E) :-   % integer number
    list_text(W,S),int_text(W1,S),!,trans_word(number,T).
special_term([C|W],VS,E,T,W1,VS2,E1) :-
    (C = 40,!,trans_word(number,T); % ( for numeric expression
     C = 91,  trans_word(list,T)),  % [ for list
    list_text([C|W],S),string_term(S,W1),
    list_var_symbols([C|W],0,[],WVS),
    (WVS = [],!, VS = VS2, E = E1;
     var_list(W1,WVL), match_vars(WVS,WVL,VS,VS2), E1 = 1).

match_vars([],[],VS,VS) :- !.
match_vars([X|XL],[V|VL],VS,VS2) :-
    (member((X,T,S:V),VS),!,match_vars(XL,VL,VS,VS2);
     match_vars(XL,VL,[(X,T,S:V)|VS],VS2)).

%--------------------------------------------------------------------
% EXTRACT VARIABLE SYMBOLS FROM A FORMULA
% ===================================================================
list_var_symbols(L,Q,VT,VS) :-
    append(A,[B,C|D],L),
    (quote_found(Q,B,Q1),!,
        list_var_symbols([C|D],Q1,VT,VS);
     var_found(Q,B,C),!,
        append(A1,[C1|D1],[C|D]),not(valid(C1)),!,
        name(V,A1),
        (member(V,VT),V \= '_', VS = VS1,!; VS = [V|VS1]),
        list_var_symbols([C1|D1],0,[V|VT],VS1)).
list_var_symbols(_,_,_,[]).

quote_found(0,B,B) :- member(B,[34,36,39]),!.
quote_found(Q,Q,0).

var_found(0,B,C) :- not(valid(B)),var_start(C).

var_start(C) :- (65 =< C,C =< 90);C = 95.
valid(C) :-  (65 =< C, C =< 90);    % A - Z
             (97 =< C, C =< 122);   % a - z
             (48 =< C, C =< 57);    % 0 - 9
             C = 95; C = 45.  % underscore; hyphen

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

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

ground(A) :- copy(A,B), A == B.
copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

% --------------------------------------------------------------------
% ENGLISH DICTIONARY
% ====================================================================
  trans_word(X,X).
%---------------------------------------------------------------------
% BUILTIN PREDICATES
% ====================================================================
predicate([Phrase,B],tvar_list(A,B)) :-
    trans_word('using the substitution',Phrase).
predicate([Phrase|PNA],eval_non(A,B)) :-
    domain(A),
    (predicate(PNA,non(A)),!;
     predicate(PA,A),trans_word(deny,W),PNA = [W|PA]),
    trans_word('evaluating negation shows',Phrase).
predicate([Phrase|PA],eval(A)) :-
    domain(A),predicate(PA,A),
    trans_word('evaluating goal shows',Phrase).
predicate([L,Phrase],uninstantiated(L)) :-
    trans_word('uninstantiated',Phrase).
predicate([Phrase,L],instantiate(A,L,VL)) :-
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
predicate([(T,A),In,(list,B)],member(A,B)) :- trans_word(in,In).

%-------------------------------------------------------------------
% PROLOG KNOWLEDGE-BASE OF SUPPLIER-PART SYSTEM
%===================================================================
p1(A:B) # 1 :- non(p2(A:B,C:D)).

p2(agent:'Adams & Sons',item:b1) # 1.
p2(agent:'Adams & Sons',item:b2) # 1.
p2(agent:'Adams & Sons',item:b3) # 1.
p2(agent:'Johnny Ltd',item:b1) # 1.
p2(agent:'Johnny Ltd',item:b2) # 1.
p2(agent:'Mitre 10',item:b1) # 1.

predicate([(agent,A:B),is,a,fractional,supplier],p1(A:B)).
predicate([(agent,A:B),supply,(item,C:D)],p2(A:B,C:D)).
predicate([(agent,A:B),not,supply,(item,C:D)],non(p2(A:B,C:D))).

domain(p1(A:B)) :- type(A,agent).
domain(p2(A:B,C:D)) :- type(A,agent),type(C,item).

constsymbol(agent:'Adams & Sons').
constsymbol(agent:'Johnny Ltd').
constsymbol(agent:'Mitre 10').
constsymbol(item:b1).
constsymbol(item:b2).
constsymbol(item:b3).

%==================================================================




