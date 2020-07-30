% PROBLEM 7.10: Alpha-beta search
%---------------------------------------------------------
% consult this program and try the following query to find
% the best next state of state 's' for the current player:
%   ?- best_next_state(s,NextState).
% use trace and spy to follow the search invoked by the
% above query.
%---------------------------------------------------------
    best_next_state(S,S1) :-
        depth_of_search(N),
        value_bounds(Alpha,Beta),
        alphabeta(N,Alpha,Beta,S,V,S1).

    alphabeta(0,_,_,S,V,_) :- !,value(S,V).
    alphabeta(N,Alpha,Beta,S,V,S1) :-
        setof(NS,next_state(S,NS),List),
        find_best(N,Alpha,Beta,List,V,S1).

    find_best(N,Alpha,Beta,[S|Rest],V1,S1) :-
        N1 is N - 1,
        alphabeta(N1,Alpha,Beta,S,V,T),
        select_better_of(N,Alpha,Beta,S,V,Rest,S1,V1).

    select_better_of(_,_,_,S,V,[],S,V) :- !.
    select_better_of(N,Alpha,Beta,S,V,List,S1,V1) :-
        minimising(N),
        (V < Alpha,!, (S1,V1) = (S,V);
        (V < Beta,!, Beta1 = V; Beta1 = Beta),
        find_best(N,Alpha,Beta1,List,V2,S2),
        (V < V2,!, (S1,V1) = (S,V); (S1,V1) = (S2,V2))).
    select_better_of(N,Alpha,Beta,S,V,List,S1,V1) :-
        maximising(N),
        (V > Beta,!, (S1,V1) = (S,V);
        (V > Alpha,!,Alpha1 = V; Alpha1 = Alpha),
        find_best(N,Alpha1,Beta,List,V2,S2),
        (V > V2,!, (S1,V1) = (S,V); (S1,V1) = (S2,V2))).

    minimising(N) :- (N mod 2) =:= 0.
    maximising(N) :- (N mod 2) =:= 1.

% A sample search tree for the alpha-beta search
%-------------------------------------------------------
    depth_of_search(3).
    value_bounds(-100,100).

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
%----------------------------------------------


