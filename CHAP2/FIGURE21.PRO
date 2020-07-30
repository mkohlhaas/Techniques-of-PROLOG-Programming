% FIGURE 2.21: Program append
%----------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- append([a,b,c],[1,2],[a,b,c,1,2]).
%   ?- append([a,b,c],[1,2],L).
%   ?- append([a,b,c],L2,[a,b,c,1,2]).
%   ?- append(L1,[1,2],[a,b,c,1,2]).
%   ?- append(L1,L2,[a,b,c,1,2]).
%   ?- append([a,b,c],L2,L).
%   ?- append(L1,[1,2],L).
%----------------------------------------------------
    append([],L,L).
    append([H|T1],L2,[H|T]) :-
        append(T1,L2,T).


