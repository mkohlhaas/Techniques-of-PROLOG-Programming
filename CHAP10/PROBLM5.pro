% PROBLEM 10.5: DCG definitions of Prolog's terms and rules
%---------------------------------------------------------------------
% This file contains the DCG definitions of Prolog's terms and rules
% which can be used to replace the built-in predicate 'string_term' of
% Arity-Prolog. At the end of this file is a list of token-lists for
% typical terms and rules (terms: samples 1-22; rules: samples 23-30).
% Note: the builtin predicate "system" used in this program (to test
% if a predicate is a builtin predicate) may correspond to "sys" (or
% something else) in your Prolog system. In which case, change this
% by using the following command:
%    ?- assert((system(F/N) :- sys(F))).
%
% Consult this file and try the following queries to see the effect
% (Here, the goal write_list is used to print out the symbols in L for
% checking):
%   ?- sample(1 ,L),write_list(L),term(T,[],V,L,[]).
%   ?- sample(2 ,L),write_list(L),term(T,[],V,L,[]).
%      ...
%   ?- sample(22,L),write_list(L),term(T,[],V,L,[]).
%
%   ?- sample(23,L),write_list(L),rule(R,[],V,L,[]).
%   ?- sample(24,L),write_list(L),rule(R,[],V,L,[]).
%      ...
%   ?- sample(30,L),write_list(L),rule(R,[],V,L,[]).
%---------------------------------------------------------------------
:- op(500,xfy,:).

    term(T,VL,VL1) --> formal_term(T,VL,VL1).
    term(T,VL,VL1) --> infix_term(T,VL,VL1). 

    formal_term(T,VL,VL1) --> variable(T,VL,VL1).
    formal_term(T,VL,VL1) --> constant(T,VL,VL1).
    formal_term(T,VL,VL1) --> list(T,VL,VL1).
    formal_term(T,VL,VL1) --> prefix_term(T,VL,VL1).

    variable(V,VL,VL1) --> 
        [X],{variable_name(X,V,VL,VL1)}.
    constant(T,VL,VL) --> 
        [X],{(constsymbol(X,T);numbers(X,T))}.

    list([],VL,VL) --> ['[',']'].
    list([T|L],VL,VL1) --> ['['],
        term(T,VL,VL2),tail(L,VL2,VL1).

    tail([],VL,VL) --> [']'].
    tail([T|L],VL,VL1) --> 
        [','],term(T,VL,VL2),tail(L,VL2,VL1).
    tail(L,VL,VL1) -->
        ['|'],variable(L,VL,VL1),[']'].
    tail(L,VL,VL1) --> 
        ['|'],list(L,VL,VL1),[']'].
  
    prefix_term(T,VL,VL1) -->
        functer(F),['('],
        arguments(Args,VL,VL1),[')'],
        {T =.. [F|Args]}.              
            
    functer(X) -->  [X],{functsymbol(X)}.
    
    arguments([Arg],VL,VL1) --> 
        term(Arg,VL,VL1).
    arguments([A|Args],VL,VL1) -->
        term(A,VL,VL2),[','],
        arguments(Args,VL2,VL1).

    infix_term(T,VL,VL1) --> rightas_term(T,VL,VL1).
    infix_term(T,VL,VL1) --> bracket_term(T,VL,VL1).

    rightas_term(T,VL,VL1) --> 
        formal_term(T,VL,VL1).
    rightas_term(T,VL,VL1) -->
        formal_term(A,VL,VL2),[F],{operator(F)},
        rightas_term(B,VL2,VL1),
        {T =.. [F,A,B]}.    

    bracket_term(T,VL,VL1) -->
        ['('],rightas_term(T,VL,VL1),[')'].
    bracket_term(T,VL,VL1) -->
        ['('],rightas_term(A,VL,VL2),[')',F],{operator(F)},
        rightas_term(B,VL2,VL1),
        {T =.. [F,A,B]}.
    bracket_term(T,VL,VL1) -->
        ['('],rightas_term(A,VL,VL2),[')',F],{operator(F)},
        bracket_term(B,VL2,VL1),
        {T =.. [F,A,B]}.
    
    variable_name(X,V,VL,VL1) :-
        name(X,[C|L]),
        ((capital(C); C = 95, L \= []),
            (member((X,V),VL),!,VL1 = VL; 
                       VL1 = [(X,V)|VL]);
         C = 95,L = [],VL1 = [(X,V)|VL]).
    
    constsymbol(X,X) :- atom_name(X).
    constsymbol(X,T) :-	char_string(X,T).

    functsymbol(X) :- atom_name(X); system(X/N);
        member(X,[abs,exp,ln,log,sqrt,acos,asin,
                  atan,cos,sin,tan]).

    atom_name(X) :- name(X,[C|L]),97 =< C, C =< 122.
    char_string(X,T) :- name(X,[C|L]),
        C = 39, string(L,R), name(T,R).
       
    capital(C) :- 65 =< C, C =< 90.

    string([39],[]).
    string([H|T],[H|R]) :- string(T,R).

    numbers(X,T) :- name(X,[C|L]),
        (48 =< C, C =< 57),chars_int([C|L],0,T).
    
    chars_int([],N,N).
    chars_int([C|L],M,N) :-
        M1 is 10*M + C - 48,chars_int(L,M1,N).
 
    operator(F) :-
    member(F,[is,':','+','-','*','/','=','<','>','^',mod]);
    member(F,['->','\=','==','@<','@>','=<','>=','//']);
    member(F,['=:=','=\=','\==','@=<','@>=','=..','-->']).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    rule((A :- B),VL,VL1) -->
        head_term(A,VL,VL2),[(':-')],
        conj_term(B,VL2,VL1).
        
    head_term(T,VL,VL1) --> constant(T,VL,VL1).
    head_term(T,VL,VL1) --> prefix_term(T,VL,VL1).

    conj_term(T,VL,VL1) --> term(T,VL,VL1).
    conj_term((A,B),VL,VL1) --> term(A,VL,VL2),[','],
        conj_term(B,VL2,VL1).
    
sample(1,[likes,'(',sue,',','X', ')']).
sample(2,[likes,'(',mother,'(','X',',','Y',')',',',
    father,'(','X',',',john,')',')']).
sample(3,[greater,'(',max,'(','X',',','452',')',',',
    glb,'(',fact,'(','X',',','Y','+','10',
    ')',')',')']).
sample(4,[length,'(','[','X',',','Y',',','Z',']',
    ',','3',')']).
sample(5,[length,'(','[','H','|','T',']',',','N',')']).
sample(6,[member,'(','15',',','[','3',',','5',',','7',
    '|','T',']',')']).
sample(7,[list,'(','[',a,',',b,',',c,']',')']).
sample(8,[list,'(','[','1',',','2',',','3','|','[',
    a,',',b,',',c,']',']',')']).
sample(9,[list,'(','[','1',',','2',',','3','|','[',']',
    ']',')']).
sample(10,[supply,'(',agent,':','''Adams & Sons''',',',
    item,':',b1,')']).
sample(11,['10','+','20','+','30','+','40']).
sample(12,['(','10','+','20','+','30',')','*','24']).
sample(13,['(','X','+','Y',')','*','(','Y','-','1',')']).
sample(14,[derivative,'(',sin,'(','X','+','Y',')',',',
    'Z',')']).
sample(15,[not,'(',non,'(',supply,'(',agent,':',
    '''Adams & Sons''',',',item,':','X',')',')',')']).
sample(16,[all,'(',supply,'(','S',':','X',',','T',':','Y',
    ')',',',not,'(',non,'(',supply,'(','S',':','X',',','T',
    ':','Z',')',')',')',')']).
sample(17,[fault,'(','X',')']).
sample(18,[non,'(',fault,'(','X',')',')']).
sample(19,[all,'(',fault,'(','X',')',')']).
sample(20,[all,'(',non,'(',fault,'(','X',')',')',')']).
sample(21,[all,'(','[','X',']',',',respond,'(','X',',','_',')',')']).
sample(22,[all,'(','[','X',']',',',non,'(',respond,'(','X',',','_',')',
    ')',')']).
sample(23,[likes,'(',ann,',','X',')',(':-'),toy,'(','X',')',',',
    plays,'(',ann,',','X',')']).
sample(24,[fault,'(','X',')',(':-'),non,'(',respond,'(','X',',',
    'Y',')',')',',','X','\==','Y']).
sample(25,[respond,'(',a,',',b,')',(':-'),true]).
sample(26,[respond,'(',c,',','X',')',(':-'),true]).
sample(27,[p,'(','X',')',(':-'),a,'(','X',')',',',b,'(','X',')']).
sample(28,[gcd,'(','X',',','0',',','X',')',(':-'),'X','>','0']).
sample(29,[gcd,'(','X',',','Y',',','Z',')',(':-'),'X','<','Y',',',
    gcd,'(','Y',',','X',',','Z',')']).
sample(30,[gcd,'(','X',',','Y',',','Y',')',(':-'),'X','>=','Y',',','Y',
    '>','0',',','X1',is,'X',mod,'Y',',',gcd,'(','Y',',','X1',',','Z',')']).

write_list([]) :- nl.
write_list([H|T]) :- write(H),write_list(T).



