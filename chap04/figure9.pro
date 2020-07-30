% FIGURE 4.9: Read a sentence of text on several lines
% -----------------------------------------------------------------
% This file contains the procedure 'read_sentence' that reads a
% sentence which may cover several lines.
% Consult this program and enter the following query:
%    ?- read_sentence(L).
% then type a sentence that may cover several lines and terminated 
% by a period or a question mark.  Following are some examples:
%
%    find student name and test mark
%    where student id is 91-2123 and test id is 91-2123.
%
%    what mark does student 'Smith J.R.' have in mathematics?
%
%    what is the id number of student 'Smith J.R.'?
%
% Repeat the queries using some backspaces. Note that this procedure
% only allows backspaces within a word; once a word is already 
% terminated (either by a space or an illegal letter), the backspace
% has no effect. Also the last word must be followed immediately by
% a period or a question mark. 
%
% ------------------------------------------------------------------
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
