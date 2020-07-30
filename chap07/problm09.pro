% PROBLEM 7.9: Minimax search
% --------------------------------------------------------
% consult this program and try the following query to find
% the best next state of state 's' for the current player:
%   ?- best_next_state(s,NextState).
% --------------------------------------------------------
best_next_state(S,S1) :-
    depth_of_search(N),
    minimax(N,S,_,S1).

minimax(0,S,V,_) :- !,value(S,V).
minimax(N,S,V,S1) :-
    N1 is N-1,
    setof((W,NS,NV,T),
          (next_state(S,NS),
           minimax(N1,NS,NV,T),
           minimaxorder(N,NV,W)),
          [(_,S1,V,_)|_]).

minimaxorder(N,V,V) :- (N mod 2) =:= 0,!.
minimaxorder(_,V1,V) :- V is 1000 - V1.

% --------------------------------------------
depth_of_search(3).

next_state(s,a).      next_state(c,j).
next_state(s,b).      next_state(d,k).
next_state(a,c).      next_state(d,l).
next_state(a,d).      next_state(e,m).
next_state(b,e).      next_state(e,n).
next_state(b,f).      next_state(f,o).
next_state(b,g).      next_state(f,p).
next_state(c,h).      next_state(g,q).
next_state(c,i).      next_state(g,r).

value(h,5).           value(n,6).
value(i,4).           value(o,10).
value(j,7).           value(p,2).
value(k,8).           value(q,3).
value(l,9).           value(r,8).
value(m,4).
