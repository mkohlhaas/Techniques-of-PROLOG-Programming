% FIGURE 10-13: LNPROLOG
% ------------------------------------------------------------------------
% This file contains the complete code of LnProlog (without its debugger),
% written in Arity-Prolog. Another version of LnProlog written in standard
% Prolog is stored in the file \chap8\lnprolog.std. Also LnProlog with a
% tracer and a debugger is stored in the file \chap8\lnprolg2.ari, and
% its standard version is stored in the file \chap8\lnprolg2.std.
%
% To run the system in this file, follow the steps below:
%
% 1.If the underlying system is Arity-Prolog 4.0 then do as follows:
%   a) Consult this file.
%   b) Enter the query ?- lnprolog. (a prompt ?: will appear)
%   c) From then on, you will be working with LnProlog. To start, bring in
%      a sample program. For example, suppose that you already copied the
%      file '\chap8\faulty.pro' from the diskette into your directory (this
%      program is on page 369), then enter the query:
%        ?: consult('c:\chap8\faulty.pro').
%      Now, you can enter positive, negative or quantified queries such
%      as the following (see pages 361, 370, 371):
%
%      ?: fault(a).                   % is node a faulty ?
%      ?: fault(b).                   % is node b faulty ?
%      ?: fault(X).                   % which node is faulty ?
%      ?: non(fault(X)).              % which node is not faulty ?
%      ?: non(non(fault(X))).         % which node is faulty ?
%      ?: all(fault(X)).              % are all nodes faulty ?
%      ?: all(non(fault(X))).         % are all nodes not faulty ?
%      ?: all([X],respond(X,_)).      % which node responds to all nodes?
%      ?: all([X],non(respond(X,_))). % which node dn't rspnd to any node?
%
%      All other usual commands such as 'listing', 'abolish', 'assert', etc.
%      have the same effect as in standard Prolog.
%
%   d) To leave the system, use the query ?: halt. as usual.
%      In order to work with another program, we must use the query
%         ?: clear_objbase.
%      to remove any data left from the previous program.
%
%   e) You can also enter a program directly to LnProlog, by typing the rules
%      at the prompt ?:. Note, however, that any fact must be entered as a
%      rule with a 'true' body to be distinguished from a query.
%      For example, enter the following rules:
%        ?: even(0) :- true.
%        ?: even(s(s(X))) :- even(X).
%        ?: odd(X) :- non(even(X)).
%      Then try the following queries, using ; to obtain alternative
%      answers:
%        ?: odd(s(0)).
%        ?: odd(X).
%
% 2. If the underlying system is Arity-Prolog 5.1 or above, then simply
%    replace the goal 'initialize' in the top-control clause with the goal
%    'introduction', then run the system as described in 1. After finishing
%    the experiment with a program, in order to run another experiment, we
%    must exit LnProlog and clear the database before loading a new program.
%
% 3. If the underlying Prolog system has the built-in predicates equivalent
%    to the seven special predicates of Arity-Prolog 4.0 listed on page 365,
%    then replace them and run LnProlog in the way described above.
%    Otherwise, use the standard version of LnProlog in the file
%    \chap8\lnprolg.std.
%
% =========================== LNPROLOG ==============================
% ================ A PROLOG SYSTEM THAT SUPPORTS ====================
% =========== LOGICAL NEGATION AND QUANTIFIED QUERIES ===============
% ===================================================================
%
% TOP LEVEL CONTROL
% ===================================================================
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

process_goal(S,Goal) :-
    var_symbols(S,VS),
    var_list(Goal,VL),
    evaluate(Goal,VS,VL),
    remove(answer(_)).

% -------------------------------------------------------------------
% EVALUATE A GOAL
% -------------------------------------------------------------------
evaluate(Goal,VS,VL) :-
    prove(Goal,_,_),process_result(VS,VL),!.
evaluate(_,_,_) :- nl,write('no').

prove(Goal,_,_) :- call(Goal).

process_result([],[]) :- !,nl,write('yes').
process_result(_,VL) :- duplicated_answer(VL),!,fail.
process_result(VS,VL) :- print_answer(VS,VL,VL1),
    assert(answer(VL1)), process_response.

print_answer(['_'|Xs],[_|VL],[_|VL1]) :- !,
    print_answer(Xs,VL,VL1).
print_answer([X|Xs],[V|VL],[V|VL1]) :-
    nl,write(X),write(' = '),write(V),
    print_answer(Xs,VL,VL1).
print_answer([],[],[]).

process_response :- write(' ->'),
    (get0(59),!,fail;nl,write('yes')).

duplicated_answer(A) :-
    answer(B),instanc(A,B).

instanc(A,B) :-
    var_list(A,L),bind_var(L,1),A = B.

bind_var([],_).
bind_var(['$@#'(N)|Vs],N) :- N1 is N+1,bind_var(Vs,N1).

remove(A) :-  retract(A),fail; true.

clear_objbase :-
     delete_world(objworld),
     create_world(objworld),
     code_world(_,objworld),!.

% -------------------------------------------------------------------

% EXTRACT CONSTANT AND FUNCTION SYMBOLS
% ===================================================================
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
% ===================================================================
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
% ===================================================================
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

% ========================== END LNPROLOG ===========================
