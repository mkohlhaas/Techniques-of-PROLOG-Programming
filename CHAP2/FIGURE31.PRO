% FIGURE 2.31: A database as a set of records
%----------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- part_city(shaft,City).
%   ?- part_city(Part,london).
%   ?- part_city(Part,City).
%----------------------------------------------------
    % SUPPLIER(SNUMBER,SNAME,CITY)
    supplier(10,leman,newyork).
    supplier(22,adams,london).
    supplier(46,fuji,tokyo).
    %
    % PART(PNUMBER,PNAME,SIZE,PRICE)
    part(k10,cam,6,20).
    part(m15,shaft,14,50).
    part(s21,wheel,25,132).
    %
    % SUPPLIER-PART(SNUMBER,PNUMBER,QUANTITY)
    supplier_part(10,m15,300).
    supplier_part(22,k10,200).
    supplier_part(22,m15,100).
    supplier_part(46,s21,500).

    part_city(PName,City) :-
        part(PNumber,PName,_,_),
        supplier_part(SNumber,PNumber,_),
        supplier(SNumber,_,City).


