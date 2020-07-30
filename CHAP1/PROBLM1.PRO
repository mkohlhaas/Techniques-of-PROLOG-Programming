% PROBLEM 1.1: Smart teacher
%----------------------------------------------------
% consult this program and try the following queries:
%    ?- smart(X).
%    ?- teaches(X,Y).
%----------------------------------------------------
     smart(X) :- teaches(X,Y),computing(Y).
     teaches(john,ma1).
     teaches(wife(john),sa1).
     mathematics(ma1).
     computing(sa1).

