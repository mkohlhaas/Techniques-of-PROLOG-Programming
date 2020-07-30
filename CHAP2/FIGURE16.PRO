% FIGURE 2.16: Program select
%----------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers:
%   ?- select(X,[a,b,c],R).
%   ?- select(b,[a,b,c],R).
%   ?- select(b,L,[a,c]).
%----------------------------------------------------
    select(H,[H|T],T).
    select(X,[H|T],[H|T1]) :- select(X,T,T1).


