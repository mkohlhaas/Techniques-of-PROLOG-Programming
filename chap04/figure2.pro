% FIGURE 4.2: The deterministic program insert
% -------------------------------------------------
% consult this program and try the following query:
%   ?- insert(4,[2,5],L1),
%      insert(1,L1,L2),
%      insert(3,L2,L3).
% -------------------------------------------------
insert(X,[],[X]) :- !.
insert(X,[H|T],[X,H|T]) :- X =< H,!.
insert(X,[H|T],[H|T1]) :- X > H,!,insert(X,T,T1).
