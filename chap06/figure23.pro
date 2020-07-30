% FIGURE 6.23: Program to transpose a matrix
% --------------------------------------------------------
% Consult this program and try the following query.
% Note: If your Prolog system does not have the built-in
% predicate "length", then remove the comment symbols %
% from the definition of "length" given in this program
% before consulting this program.
%   ?- make_table(Table),length(Table,N),
%      transpose(N,Table,Table1).
% --------------------------------------------------------
transpose(N,Table,Table1) :-
    setof(Col,(M,X,Row)^
              (nth_member(M,X,Row),
               (M > N,!,fail;
                setof(X,Row^member(Row,Table),Col))),
         Table1).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

nth_member(1,X,[X|_]).
nth_member(N,X,[_|T]) :- nth_member(M,X,T), N is M+1.

% length([],0).
% length([H|T],N) :-
%     length(T,M), N is M+1.

make_table(Table) :-
    setof(Row,
          H^setof(e(I,J,X),(table(H,J,X),I = H),Row),
          Table).

table(m1,j1,6).
table(m1,j2,5).
table(m1,j3,3).
table(m1,j4,6).
table(m2,j1,5).
table(m2,j2,6).
table(m2,j3,8).
table(m2,j4,12).
table(m3,j1,8).
table(m3,j2,6).
table(m3,j3,8).
table(m3,j4,9).
table(m4,j1,3).
table(m4,j2,6).
table(m4,j3,5).
table(m4,j4,8).
