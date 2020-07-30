% PROBLEM 7.2: The problem of moving blocks
%-------------------------------------------------------------
% Note: If your Prolog system already has the builtin
% predicate "argrep" which is defined at the end of this
% program, then use the comment symbols % to neutralize it.
% Consult this program and enter the following query to find
% a sequence of moves to bring the blocks into the desired
% position (Observe the terribly long answer):
%   ?- depth_first_search(Answer).
% Now, replace the goal choose_indices(I,J) in the procedure
% next_state with choose_indices(Piles,I,J), then re-enter
% the above query to find a better answer.  Compare the result
% with the previous one.
%-------------------------------------------------------------
    initial_state((none,piles([b,c],[a],[]))).
    final_state((_,piles([],[a,b,c],[]))).
    num_of_piles(3).

    next_state((_,Piles),(move(I,J),NewPiles)) :-
        choose_indices(I,J),
        move_block(I,J,Piles,NewPiles).

    choose_indices(I,J) :-
        num_of_piles(N),
        index(I,N),index(J,N),I \= J.

    move_block(I,J,Piles,NewPiles) :-
        arg(I,Piles,[B|IBlocks]),
        arg(J,Piles,JBlocks),
        argrep(Piles,I,IBlocks,IPiles),
        argrep(IPiles,J,[B|JBlocks],NewPiles).

    index(1,N).
    index(I,N) :-
        index(K,N),
        (K >= N,!,fail; I is K+1).

% Procedure to choose blocks to move using some heuristics
%-----------------------------------------------------------
   choose_indices(Piles,I,J) :-
       final_state((_,FPiles)),
       num_of_piles(N),index(K,N),
       arg(K,FPiles,Pile0),arg(K,Piles,Pile1),
       append(P0,[A|Bs],Pile0),
       (append(P1,[B|Bs],Pile1),!; Bs = []),
       ((A \= B,!; P1 \= [],!),
           I is K, index(J,N), J \= I,
           arg(J,Piles,Pile2),not(member(A,Pile2));
        (var(B), C = A,!; append(P00,[C],P0)),
           index(I,N),arg(I,Piles,Pile3),
           member(C,Pile3),
           (Pile3 = [C|_],!,J is K;
            index(J,N), J \= I, J \= K)).
%-----------------------------------------------------------

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

    member_state((_,X),[(_,X)|_]).
    member_state(X,[_|T]) :- member_state(X,T).

%-----------------------------------------------------------
    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    argrep(Term,N,Value,NewTerm) :-
        Term =.. [F|L],
        replace(N,L,Value,L1),
        NewTerm =.. [F|L1].

    replace(1,[X|L],Y,[Y|L]) :- !.
    replace(N,[X|L],Y,[X|L1]) :-
        N > 1, N1 is N-1,
        replace(N1,L,Y,L1).

%================================================





