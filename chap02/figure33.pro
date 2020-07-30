% FIGURE 2.33: A database as a set of key-attributes
% --------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- part_city(shaft,City).
%   ?- part_city(Part,london).
%   ?- part_city(Part,City).
% ---------------------------------------------------
sname(10,leman).
sname(22,adams).
sname(46,fuji).

scity(10,newyork).
scity(22,london).
scity(46,tokyo).

pname(k10,cam).
pname(m15,shaft).
pname(s21,wheel).

supplier_part(10,m15,300).
supplier_part(22,k10,200).
supplier_part(22,m15,100).
supplier_part(46,s21,500).

part_city(PName,City) :-
    pname(PNumber,PName),
    supplier_part(SNumber,PNumber,_),
    scity(SNumber,City).
