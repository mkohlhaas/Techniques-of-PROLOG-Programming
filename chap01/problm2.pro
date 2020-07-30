% PROBLEM 1.2: Program 1 (using functions)
% -------------------------------------------------
% consult this program and try the following query:
%    ?- woman(X),likes(X,husband(Y)).
% -------------------------------------------------
likes(mother(X),X) :- good(X).
woman(mother(_)).
woman(ann).
good(husband(ann)).

% PROBLEM 1.2: Program 2 (no functions)
% -------------------------------------------------
% consult the following program (after deleting %,
% and removing the previous program), and try the
% following query:
%    ?- woman(X),husband(Y,Z),likes(X,Y).
% now add the following facts:
%
% husband(anns_husband,ann).
% mother(anns_mother_inlaw,anns_husband).
%
% then try the above query again.
% -------------------------------------------------
% likes(X,Y) :- mother(X,Y),good(Y).
% woman(X) :- mother(X,_).
% woman(ann).
% good(X) :- husband(X,ann).
