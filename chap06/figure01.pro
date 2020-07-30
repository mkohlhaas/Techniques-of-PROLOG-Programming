% FIGURE 6.1: A non-deterministic program to find the
%             level of a given item in a binary tree.
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- tree(Tree),level(25,Tree,Level).
%   ?- tree(Tree),level(10,Tree,Level).
%   ?- tree(Tree),level(18,Tree,Level).
%   ?- tree(Tree),level(43,Tree,Level).
%   ?- tree(Tree),level(20,Tree,Level).
% draw the tree given in this program and check the
% above results.
% ---------------------------------------------------
level(X,bt(_,X,_),1).
level(X,bt(L,_,_),H) :-
    level(X,L,HL), H is HL + 1.
level(X,bt(_,_,R),H) :-
    level(X,R,HR), H is HR + 1.

tree(bt(bt(bt(bt(nil,51,nil),
              12,
              bt(nil,3,nil)),
           14,
           bt(bt(nil,25,nil),
              16,
              nil)),
        27,
        bt(bt(nil,
              18,
              bt(nil,29,nil)),
           10,
           bt(bt(nil,51,nil),
              12,
              bt(nil,43,nil))))).
