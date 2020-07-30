% FIGURE 4.8: Read a line of text
% -------------------------------------------------------------
% This file contains the procedure 'readline' that reads a line
% of text and stores the characters' ascii codes in a list.
% Consult this file and try the following query, then enter a
% line of text as shown below:
%   ?- readline(L),name(N,L).
% this is a     text line<Return>
%
% Repeat the query and use some backspaces to see how they are
% handled.
% -------------------------------------------------------------
readline(L) :-
    get0(C),
    read_chars(C,[],L1),
    reverse(L1,[],L).

read_chars(13,L,L) :- !.
read_chars(8,[_|L],L1) :- !,
    put(32),put(8),
    get0(C1),
    read_chars(C1,L,L1).
read_chars(C,L,L1) :-
    get0(C1),
    read_chars(C1,[C|L],L1).

reverse([],L,L).
reverse([H|T],L,L1) :-
    reverse(T,[H|L],L1).
