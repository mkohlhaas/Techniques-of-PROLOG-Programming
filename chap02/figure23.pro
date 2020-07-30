% FIGURE 2.23: Program subset
% ---------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- subset([a,c],[a,b,c]).
%   ?- subset(S,[a,b,c]).
% ---------------------------------------------------
subset([],[]).
% subset(S,[H|T]) :-
%     subset(R,T),
%     (S = R; S = [H|R]).
subset([H|L],[H|T]) :- subset(L,T).
subset(L,[_|T])     :- subset(L,T).
