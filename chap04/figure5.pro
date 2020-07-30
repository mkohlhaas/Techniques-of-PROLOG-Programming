% FIGURE 4.5: A program for mark-grade relation
% -----------------------------------------------------
% Consult this program and try the following queries:
%   ?- grade(98,Grade).
%   ?- grade(65,Grade).
%   ?- grade(45,Grade).
%   ?- grade(Mark,'HD').
%   ?- grade(Mark,'CR').
%   ?- grade(Mark,'P').
%   ?- grade(Mark,Grade).
% -----------------------------------------------------
grade(Mark,'HD') :- range(Mark,95,100).
grade(Mark,'DI') :- range(Mark,80,94).
grade(Mark,'CR') :- range(Mark,65,79).
grade(Mark,'P')  :- range(Mark,50,64).
grade(Mark,'F')  :- range(Mark,0,49).

range(A-B, A,B) :- !.
range(Mark,A,B) :- A =< Mark, Mark =< B.
