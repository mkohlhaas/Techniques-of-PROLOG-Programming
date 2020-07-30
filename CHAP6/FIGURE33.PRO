% FIGURE 6.33: Search for a record in a B-tree
%------------------------------------------------------
% consult this program and enter the following query
% to search for the record of key 20 in a B-tree:
%   ?- btree(Tree),search(20,Tree,Record).
%------------------------------------------------------
    search(X,BT,Record) :-
        leaf(BT),!,
        member((X,Record),BT).
    search(X,BT,Record) :-
        append(A,[L,(Y,Rec),R|C],BT),
        (X =< Y,!; C = []),
        (X = Y,!, Record = Rec;
         X < Y,!, search(X,L,Record);
         C = [],  search(X,R,Record)).

    leaf([[_|_]|_]) :- !,fail.
    leaf(_).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    btree([[(5,[5,'Smith A.',40,manager]),
            (7,[7,'Ryan K.',23,ito2]),
            (9,[9,'Adams P.',24,ito3])],
           (10,[10,'Bush L.',50,director]),
           [(15,[15,'Pillo T.',32,aso4]),
            (20,[20,'Quale S.',28,ito2])],
           (25,[25,'Burns F.',25,aso3]),
           [(30,[30,'Fallen S.',30,ito5]),
            (40,[40,'Sammis S.',21,aso1]),
            (50,[50,'Trevor H.',20,ito2]),
            (60,[60,'Dulles M.',32,consultant])]]).
%------------------------------------------------------------



