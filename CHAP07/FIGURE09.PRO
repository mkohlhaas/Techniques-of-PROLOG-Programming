% FIGURE 7.9: Program depth-first search
%---------------------------------------------------
% consult this program and enter the following query
% to find a solution for the farmer problem:
%   ?- depth_first_search(AnsPath).
%---------------------------------------------------
    depth_first_search(AnsPath) :-
        initial_state(Init),
        depth_first([Init],AnsPath).

    depth_first([S|Path],[S]) :-
        final_state(S),!.
    depth_first([S|Path],[S|AnsPath]) :-
        extend([S|Path],S1),
        depth_first([S1,S|Path],AnsPath).

    extend([S|Path],S1) :-
        next_state(S,S1),
        not(member_state(S1,[S|Path])).

    member_state(X,[X|_]).
    member_state(X,[_|T]) :- member_state(X,T).

% The farmer problem
%-----------------------------------------------
    initial_state([n,n,n,n]).
    final_state([s,s,s,s]).

    next_state(S,S1) :- move(S,S1),safe(S1).

    move([F,W,G,C],[F1,W,G,C])  :- cross(F,F1).
    move([F,F,G,C],[F1,F1,G,C]) :- cross(F,F1).
    move([F,W,F,C],[F1,W,F1,C]) :- cross(F,F1).
    move([F,W,G,F],[F1,W,G,F1]) :- cross(F,F1).

    safe([F,W,G,C]) :- F = G,!; F = W, F = C.

    cross(n,s).
    cross(s,n).
%-----------------------------------------------




