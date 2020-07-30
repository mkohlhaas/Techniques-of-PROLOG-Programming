% FIGURE 2.18: Length of a list
% -----------------------------------------------------
% Note: If your Prolog system already has a built-in
% predicate `length', then rename the following program
% before consulting it.  Try the following queries:
%   ?- length([a,b,c],3).
%   ?- length([a,b,c],N).
%   ?- length(L,3).
% -----------------------------------------------------
list_length([],0).
list_length([_|T],N) :-
    length(T,M),N is M + 1.
