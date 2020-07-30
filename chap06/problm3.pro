% PROBLEM 6.3: Sort and count using set
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- sort_set([5,4,3,1,2],L).
%   ?- sort_set([7,3,5,1,8,6,2,4],L).
%   ?- count_answers(pass(X),Number).
% ---------------------------------------------------
sort_set(L,L1) :-
      setof(X,member(X,L),L1).

count_answers(G,N) :-
      bagof(a,G^G,L),length(L,N).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% length([],0).
% length([_|T],N) :- length(T,M), N is M+1.

pass(ID) :- test(ID,Mark), Mark >= 50.

test(78-2123,40).
test(90-3150,56).
test(90-4201,65).
test(91-2123,32).
test(91-3150,52).
test(92-2201,95).
