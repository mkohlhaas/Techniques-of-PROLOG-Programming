% FIGURE 4.7: The program length using once predicate
% -------------------------------------------------------
% (If length is already built-in, then change the name of
% the following procedure before consulting it).
% Consult this program and try the following queries:
%   ?- length([a,b,c],N).
%   ?- length(L,3).
% Use ; to test if the program terminates properly.
% -------------------------------------------------------
list_length(L,N) :-
    my_once(len(L,N)).

len([],0).
len([_|T],N) :-
    len(T,M),
    N is M+1.

my_once(P) :- P,!.
