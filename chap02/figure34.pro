% FIGURE 2.34: A database as lists of records
% ---------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- part_city(shaft,City).
%   ?- part_city(Part,london).
%   ?- part_city(Part,City).
% ---------------------------------------------------
datafile(supplier,
   [s(10,leman,newyork),
    s(22,adams,london),
    s(46,fuji,tokyo)]).

datafile(part,
   [p(k10,cam,6,20),
    p(m15,shaft,14,50),
    p(s21,wheel,25,132)]).

datafile(supplier_part,
   [sp(10,m15,300),
    sp(22,k10,200),
    sp(22,m15,100),
    sp(46,s21,500)]).

arecord(Type,Rec) :-
    datafile(Type,List),
    member(Rec,List).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

part_city(PName,City) :-
    arecord(part,p(PNumber,PName,_,_)),
    arecord(supplier_part,sp(SNumber,PNumber,_)),
    arecord(supplier,s(SNumber,_,City)).
