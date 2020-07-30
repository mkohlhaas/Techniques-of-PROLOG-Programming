% FIGURE 6.6: Operation of a string matching machine
%------------------------------------------------------------
% consult this program and enter the following queries:
%   ?- find_keywords("ushers").
%   ?- find_keywords("she threw herself into the ushers").
% Note: In this program the backquoted characters are
% replaced with their ASCII codes. For example `h is replaced
% with the number 104.
%------------------------------------------------------------
:- op(500,xfy,:).

    find_keywords(L) :-
        activate_machine(0,L,1),
        print_keywords.

    activate_machine(S,[C|L],I) :-
        change_state(S,C,S1),
        check_keywords(I,S1),
        I1 is I+1,
        activate_machine(S1,L,I1).
    activate_machine(_,[],_).

    check_keywords(I,S) :-
        keyword(S,W),
        assert(store(I,W)),fail.
    check_keywords(_,_).

    print_keywords :-
        setof(I:W,store(I,W),L),
        print_word(0,L),
        abolish(store/2).
    print_keywords.

    print_word(_,[]) :- !,nl.
    print_word(I,[J:W|Rest]) :-
        (I < J,!,nl,write(J);true),
        write(' '),write(W),
        print_word(J,Rest).

% FIGURE 6.5: State transition of a string matching machine
%----------------------------------------------------------
    change_state(0,104,1).       change_state(0,115,3).
    change_state(1,101,2).       change_state(1,105,6).
    change_state(1,101,0).       change_state(1,105,0).
    change_state(2,114,8).       change_state(2,114,0).
    change_state(3,104,4).       change_state(3,104,1).
    change_state(4,101,5).       change_state(4,101,2).
    change_state(6,115,7).       change_state(6,115,3).
    change_state(8,115,9).       change_state(8,115,3).
    change_state(0,_,0).

    keyword(2,he).
    keyword(5,he).
    keyword(5,she).
    keyword(7,his).
    keyword(9,hers).
%----------------------------------------------------



