% PROBLEM 7.3: Get a specified quantity of water
%------------------------------------------------------------
% consult this program and enter the following query to find
% a sequence of actions needed to obtain a glass of 4 units
% of water:
%   ?- breadth_first_search(Answer).
% Now, replace the procedure next_state with the new one
% given at the end of this program (by placing the symbols %
% in front of the old version and removing the symbols % from
% the new version) then try the following query to obtain an
% answer-path produced by depth-first search with heuristics
% which is nicely displayed:
%   ?- depth_first_search(Answer),
%      print_answer(Answer).
% Now change the final state and repeat the process to find
% a glass of 3,2,1 units of water.
%------------------------------------------------------------
:- op(500,xfy,:).

    initial_state((none,[0,0])).
    final_state((_,[4,_])).
    final_state((_,[_,4])).

    capacity(1,7).
    capacity(2,5).

    next_state((_,[U,V]),(fill_glass(I),[X,Y])) :-
        capacity(I,C),
        fill_glass(I,C,U,V,X,Y).
    next_state((_,[U,V]),(empty_glass(I),[X,Y])) :-
        capacity(I,C),
        empty_glass(I,C,U,V,X,Y).
    next_state((_,[U,V]),(pour(I,J),[X,Y])) :-
        capacity(J,C),I is 3-J,
        pour_glass(I,J,C,U,V,X,Y).

    fill_glass(1,C,0,V,C,V).
    fill_glass(2,C,U,0,U,C).

    empty_glass(1,C,C,V,0,V).
    empty_glass(2,C,U,C,U,0).

    pour_glass(1,2,C,U,V,X,Y) :- pour(C,U,V,X,Y).
    pour_glass(2,1,C,U,V,X,Y) :- pour(C,V,U,Y,X).

    pour(C,U,V,X,Y) :-
        U > 0, V < C,
        (U+V < C, X is 0, Y is U+V,!;
         U+V >= C, X is U+V-C, Y is C).


% The procedure next-state using some heuristics
%----------------------------------------------------
%   next_state((_,[U,V]),([A,B],[X,Y])) :-
%       capacity(1,C1),capacity(2,C2),
%       (U = 0,!, A = 1, U1 = C1; A = 0, U1 = U),
%       (V = C2,!,B = 1, V1 = 0;  B = 0, V1 = V),
%       (U1+V1 < C2,!, X = 0, Y is U1+V1;
%                      X is U1+V1-C2, Y = C2).
%
    print_answer([]).
    print_answer([S|Rest]) :-
        prints(S),print_answer(Rest).

    prints((_,[0,0])) :- !,write(0:0),nl.
    prints(([A,B],[X,Y])) :-
        (A = 0,!; write('Fill glass 1, ')),
        (B = 0,!; write('Empty glass 2, ')),
        write('Pour glass 1 into glass 2'),nl,
        write(X:Y),nl.


% Program breadth-first-search
%------------------------------------------------------
    breadth_first_search(AnsPath) :-
        initial_state(Init),
        breadth_first([[Init]|Q]-Q,AnsPath).

    breadth_first(Q-Qt,AnsPath) :-
        qmember([S|Path],Q-Qt),final_state(S),!,
        reverse([S|Path],[],AnsPath).
    breadth_first([Path|Q]-Qt,AnsPath) :-
        expand(Path,Qt-Qs),
        breadth_first(Q-Qs,AnsPath).

    expand(Path,Queue) :-
        setof([S|Path],extend(Path,S),List),!,
        list_to_queue(List,Queue).
    expand(Path,Q-Q).

    % extend is given in depth-first search below

    qmember(X,[Y|Q]-Qt) :-
        nonvar(Y),
        (X = Y; qmember(X,Q-Qt)).

    list_to_queue(L,Q-Qt) :-
        append(L,Qt,Q).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    reverse([],L,L).
    reverse([H|T],L,R) :- reverse(T,[H|L],R).


% Program depth-first search
%------------------------------------------------
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

    member_state((_,X),[(_,X)|_]).
    member_state(X,[_|T]) :- member_state(X,T).

%-------------------------------------------------------




