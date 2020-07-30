% PROBLEM 5.3: Eliza
%---------------------------------------------------------------
% consult this program and invoke the system by using the query:
%   ?- eliza.
% then start a conversation by entering the following sentences:
%   User : I feel terrible.
%   User : I failed the exam.
%   User : I come to see you for advices.
%   User : I trust you.
%   User : You are a nice machine.
%   User : I tell you about an incredible lady.
%   User : Do you really want to know.
%   User : She is driving me crazy.
%   User : Can you help?
%   etc.
% Now, extend the list of pairs in the program to make the
% conversation more interesting, and try again.
%---------------------------------------------------------------
:- op(500,xfy,:).

    eliza :-
        repeat,           
            receive_query(Query),
            response(Query,Answer),
            print_answer(Answer),
        Answer = [bye].

    response([bye],[bye]) :- !.
    response(Query,Answer) :-
        pair(Question,Response),
        match(Query,Question,References),
        match(Answer,Response,References),!.

    match([W|L1],[W|L2],Ref) :-
        atom(W),match(L1,L2,Ref).
    match(L1,[N|L2],Ref) :-
        integer(N),
        member(N:Lw,Ref),!,
        append(Lw,Rest,L1),
        match(Rest,L2,Ref).
    match([],[],Ref).

    pair(['I',feel,1], ['Why',do,you,feel,1,?]).
    pair(['I',1,you,2], ['What',makes,you,1,me,2,?]).
    pair([1,you,2], ['Yes','I',2]).
    pair(_, ['I',see,'.', 'Please',continue]).


% Print the answer
%-----------------------------------------------------
    print_answer(Answer) :-
        nl,write('Eliza: '),
        print_line(Answer).

    print_line([]) :- !,nl.
    print_line([H|T]) :-
        write(H),write(' '),
        print_line(T).


% Read an input sentence (Figure 4.9)
%---------------------------------------------
    receive_query(L) :-
        nl,write('User : '),
        read_sentence(L).

    read_sentence(L) :- read_sent(32,L).

    read_sent(C,L) :- end_sent(C),!,L = [].
    read_sent(C,[W|L]) :-
        read_word(W,C1),read_sent(C1,L).

    read_word(W,C1) :-
        read_first_char(S,C),
        read_word_chars(S,C,[],L,C1),
        reverse(L,[],L1),name(W,L1).

    read_first_char(S,C) :-
        get_char(S),start_word(S,C),!;
        read_first_char(S,C).

    read_word_chars(S,C,L,L,C1) :- end_word(S,C,C1),!.
    read_word_chars(S,8,[C|L],L1,C1) :- !,
        put(32),put(8),
        get_char(C2),
        read_word_chars(S,C2,L,L1,C1).
    read_word_chars(S,C,L,L1,C1) :-
        legal_char(S,C),
        get_char(C2),
        read_word_chars(S,C2,[C|L],L1,C1).

    get_char(C) :- get0(C),(C = 13,!,nl,tab(4); true).
    valid_char(C) :-
        96 < C, C < 123;  % a-z
        64 < C, C < 91;   % A-Z
        47 < C, C < 58;   % 0-9
        C = 45.        % hyphen.

    start_word(C,C)   :- valid_char(C),!.
    start_word(39,C)  :- get_char(C).
    end_word(39,39,C) :- get_char(C).  % quotation marks
    end_word(Q,C,C)   :- Q \= 39,C \= 8,not(valid_char(C)).
    end_sent(C)       :- (C = 46; C = 63). % period or ?

    legal_char(39,_) :- !.
    legal_char(_,C)  :- valid_char(C).

    reverse([],L,L).
    reverse([H|T],L,L1) :- reverse(T,[H|L],L1).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

%---------------------------------------------


