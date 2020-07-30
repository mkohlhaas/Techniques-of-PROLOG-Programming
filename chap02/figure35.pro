% FIGURE 2.35: A database as a collection of sorted binary trees
% --------------------------------------------------------------
% consult this program and try the following queries:
% use ; to request alternative answers.
%   ?- part_city(shaft,City).
%   ?- part_city(Part,london).
%   ?- part_city(Part,City).
% Explain the answer to the last query above. Now, remove the
% inequalities in the procedure tree_member then try the
% following queries and discuss the results:
%   ?- part_city(Part,london).
%   ?- part_city(Part,City).
% --------------------------------------------------------------
datatree(supplier,
    bt(bt(nil,n(10,s(leman,newyork)),nil),
       n(22,s(adams,london)),
       bt(nil,n(46,s(fuji,tokyo)),nil))).

datatree(part,
    bt(bt(nil,n(cam,p(k10,6,20)),nil),
       n(shaft,p(m15,14,50)),
       bt(nil,n(wheel,p(s21,25,132)),nil))).

datatree(supplier_part,
    bt(bt(nil,
          n(k10,sp(22,200)),
          bt(nil,n(m15,sp(22,100)),nil)),
       n(m15,sp(10,300)),
       bt(nil,n(s21,sp(46,500)),nil))).

arecord(Type,Rec) :-
    datatree(Type,Tree),tree_member(Rec,Tree).

tree_member(n(Key,X),bt(_,n(Key,X),_)).
tree_member(n(Key,X),bt(Left,n(Nkey,_),_)) :-
    Key @=< Nkey,tree_member(n(Key,X),Left).
tree_member(n(Key,X),bt(_,n(Nkey,_),Right)) :-
    Key @> Nkey,tree_member(n(Key,X),Right).

part_city(PName,City) :-
    arecord(part,n(PName,p(PNumber,_,_))),
    arecord(supplier_part,n(PNumber,sp(SNumber,_))),
    arecord(supplier,n(SNumber,s(_,City))).
