% PROBLEM 7.1: A different representation of the farmer problem
%--------------------------------------------------------------
% consult this program and enter the following query to find a
% solution for the farmer problem which is represented below:
%   ?- depth_first_search(AnsPath).
% compare the result with that produced by the representation
% given in Figure 7.10.
%--------------------------------------------------------------
% The farmer problem
%--------------------------------------------------------------
    initial_state(([f,c,g,w],[])).
    final_state(([],_)).

    next_state(([f|N],S),(N1,[f|S1])) :-
        move(N,S,N1,S1),safe(N1).
    next_state((N,[f|S]),([f|N1],S1)) :-
        move(S,N,S1,N1),safe(S1).

    move(A,B,A,B).
    move(A,B,A1,B1) :-
        select(M,A,A1),insert(M,B,B1).

    safe([g]).
    safe(L) :- not(member(g,L)).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    select(H,[H|T],T).
    select(X,[H|T],[H|T1]) :- select(X,T,T1).

    insert(X,[],[X]).
    insert(X,[H|T],[X,H|T]) :- X @=< H.
    insert(X,[H|T],[H|T1]) :-  X @> H, insert(X,T,T1).


% Program depth-first search
%-----------------------------------------------------------
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

%-----------------------------------------------------------





