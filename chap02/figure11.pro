% FIGURE 2.11: A doubly-recursive program for sum-tree
% --------------------------------------------------------
% This program adds up the values in the nodes of a binary
% tree. Given at the end of the program is a sample tree
% which can be used for experiment.
% Consult this program and try the following query:
%   ?- sample(Tree),sum_tree(Tree,Sum).
% --------------------------------------------------------
sum_tree(nil,0).
sum_tree(tree(Left,Node,Right),S) :-
    sum_tree(Left,S1),
    sum_tree(Right,S2),
    S is S1+S2+Node.

sample(
  tree(
    tree(
      tree(
        tree(
          nil,          % after "tree(" comes a left tree
          6,
          nil),         % after node with a number (here 6) comes a right tree
        2,
        tree(
          tree(
            nil,
            13,
            nil),
          14,
          nil)),
      15,
      tree(
        tree(
          nil,
          16,
          nil),
        17,
        tree(
          nil,
          18,
          nil))),       % ")" closes a tree
    20,
    tree(
      tree(
        nil,
        19,
        nil),
      24,
      nil))).
