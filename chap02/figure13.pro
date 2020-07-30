% FIGURE 2.13: Membership of a list
% ---------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers:
%   ?- member(b,[a,b,b]).
%   ?- member(X,[a,b,b]).
%   ?- member(a,L).
% ---------------------------------------------------
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
