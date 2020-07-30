% FIGURE 5.12: A program to construct the Fibonacci-trees
%---------------------------------------------------------
% Consult this program and try the following queries.
% Note the goal tmove(X,Y) in procedure print_tree is to
% move the cursor to row X and column Y; also the goal
% cls is used in the queries to clear the screen before
% printing the tree. Check your Prolog system for the
% equivalent predicates, and replace them if necessary.
% For an example of replacement, see the end of this
% instruction.
%
%   ?- fibonacci_tree(3,N,T),
%      cls,print_tree(T,1,6,4),
%      nl,nl.
%
%   ?- fibonacci_tree(4,N,T),
%      cls,print_tree(T,1,14,8),
%      nl,nl.
%
%   ?- fibonacci_tree(5,N,T),
%      cls,print_tree(T,1,30,16),
%      nl,nl,nl,nl.
%
% In LPA-Prolog, the goal cursor(&:,X,Y) should be used in
% place of tmove(X,Y). Also the predicate cls (clear-screen)
% can be defined as follows:
%      cls :- cursor(&:,0,0),clear(23,80).
%
%      clear(0,0) :- !,cursor(&:,0,0).
%      clear(M,0) :- !,nl,M1 is M-1,clear(M1,80).
%      clear(M,N) :- put(32),N1 is N-1,clear(M,N1).
%
%---------------------------------------------------------
    fibonacci_tree(0,0,nil).
    fibonacci_tree(1,1,bt(nil,1,nil)).
    fibonacci_tree(H,N,bt(bt(T2,Y,T3),X,T2A)) :-
         H > 1, H1 is H-1,
         fibonacci_tree(H1,N1,bt(T2,Y,T3)),
         X is N1+1, N is N1+Y,
         augment(T2,X,T2A).

    augment(nil,_,nil).
    augment(bt(L,K,R),X,bt(LA,KA,RA)) :-
         KA is K+X,
         augment(L,X,LA),
         augment(R,X,RA).

    print_tree(nil,_,_,_).
    print_tree(bt(L,K,R),X,Y,D) :-
        tmove(X,Y), write(K),
        D1 is D//2, X1 is X+D1,
        Y1 is Y-D,  Y2 is Y+D,
        print_tree(L,X1,Y1,D1),
        print_tree(R,X1,Y2,D1).

    tree(1,bt(nil,1,nil)).
    tree(2,bt(bt(nil,1,nil),2,nil)).
    tree(3,bt(bt(bt(nil,1,nil),2,nil),3,bt(nil,4,nil))).
    tree(4,bt(bt(bt(bt(nil,1,nil),2,nil),3,bt(nil,4,nil)),
         5,bt(bt(nil,6,nil),7,nil))).





