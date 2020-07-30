% PROBLEM 3.3: Inorder binary tree
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- sort_tree([4,2,3,5,7,1,6],Tree).
%   ?- sort_tree([6,8,7,4,5,9,2,1,3],Tree).
%   ?- sort_tree([1],Tree).
%   ?- sort_tree([],Tree).
% draw the resulting trees, node by node as they are
% inserted into the trees.
% ---------------------------------------------------
sort_tree(L,T) :- insert_tree(L,nil,T).

insert_tree([],T,T).
insert_tree([X|R],S,T) :-
    insert_elemt(X,S,S1),
    insert_tree(R,S1,T).

insert_elemt(X,nil,bt(nil,X,nil)).
insert_elemt(X,bt(Left,Y,Right),bt(Left1,Y,Right)) :-
    X < Y, insert_elemt(X,Left,Left1).
insert_elemt(X,bt(Left,Y,Right),bt(Left,Y,Right1)) :-
    X >= Y,insert_elemt(X,Right,Right1).
