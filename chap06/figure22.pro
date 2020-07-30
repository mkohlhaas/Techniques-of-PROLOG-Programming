% FIGURE 6.22: Programs that create arrays and tables
% --------------------------------------------------------
% consult this program and try the following queries:
%   ?- make_array2(Array).
%   ?- make_table(Table).
% --------------------------------------------------------
make_array2(Array) :-
    row_indices(IList),
    col_indices(JList),
    setof(Row, I^ (member(I,IList),
               setof(e(I,J,_),member(J,JList),Row)),
               Array).

array_entry(A,I,J,X) :-
    member(Row,A),member(e(I,J,X),Row).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

row_indices([honda,mercedes,ford,citroen]).
col_indices([tokyo,rome,newyork,paris,sydney]).

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
